module GitHubPages where

import           RIO

import           RIO.Process (readProcessStdout_, runProcess_)
import qualified RIO.Text as Text
import           System.Directory (doesDirectoryExist, withCurrentDirectory)
import           System.Process.Typed (proc)

deploy
    :: (FilePath -> IO ())
       -- ^ Site build routine.
       -- Its argument is the target directory where the site must appear.
    -> IO ()
deploy build = do
    assertInRepoRoot
    assertBranchIsMaster
    originUrl <- git ["config", "remote.origin.url"]
    userEmail <- git ["config", "user.email"]
    headId <- git ["rev-parse", "HEAD"]
    withSystemTempDirectory "github-pages-deploy." $ \tmp -> do
        let tmpBS = encodeUtf8 $ Text.pack tmp
        git_
            [ "clone"
            , "--config=user.email=" <> userEmail
            , "--no-checkout"
            , "."
            , tmpBS
            ]
        build tmp
        withCurrentDirectory tmp $ do
            git_ ["add", "--verbose", "."]
            git_ ["commit", "--quiet", "--reuse-message=" <> headId]
            git_ ["push", "--force", originUrl, "master:gh-pages"]
        git_ ["fetch"]
  where

    assertBranchIsMaster = do
        headValue <- readFileBinary ".git/HEAD"
        unless (strip headValue == "ref: refs/heads/master") $
            error ("expected HEAD to be master, but got " ++ show headValue)

    assertInRepoRoot = do
        inRepoRoot <- doesDirectoryExist ".git"
        unless inRepoRoot $ error "must run from git repo root"

    git :: [ByteString] -> IO ByteString
    git args = strip . toStrictBytes <$> readProcessStdout_ (proc "git" args')
      where
        args' = map (Text.unpack . decodeUtf8Lenient) args

    git_ :: [ByteString] -> IO ()
    git_ args = runProcess_ $ proc "git" args' where
        args' = map (Text.unpack . decodeUtf8Lenient) args

    strip :: ByteString -> ByteString
    strip = encodeUtf8 . Text.strip . decodeUtf8Lenient
