module GitHubPages where

import           RIO

import           RIO.FilePath ((</>))
import           RIO.Process (readProcessStdout_, runProcess_)
import qualified RIO.Text as Text
import           System.Directory (copyFile, doesDirectoryExist,
                                   withCurrentDirectory)
import           System.Process.Typed (proc)

deploy ::
  -- | Site root
  FilePath ->
  [FilePath] ->
  IO ()
deploy root files = do
  assertInRepoRoot
  assertBranchIsMaster
  originUrl <- git ["config", "remote.origin.url"]
  userEmail <- git ["config", "user.email"]
  withSystemTempDirectory "github-pages-deploy." \tmp -> do
    let tmpBS = encodeUtf8 $ Text.pack tmp
    git_
      [ "clone"
      , "--config=user.email=" <> userEmail
      , "--no-checkout"
      , "."
      , tmpBS
      ]
    for_ files \file -> copyFile (root </> file) (tmp </> file)
    withCurrentDirectory tmp do
      git_ ["add", "--verbose", "."]
      git_ ["commit", "--quiet", "--message=deploy"]
      git_ ["push", "--force", originUrl, "master:gh-pages"]
    git_ ["fetch"]

assertBranchIsMaster :: IO ()
assertBranchIsMaster = do
  headValue <- readFileBinary ".git/HEAD"
  unless (strip headValue == "ref: refs/heads/master") $
    error ("expected HEAD to be master, but got " ++ show headValue)

assertInRepoRoot :: IO ()
assertInRepoRoot = do
  inRepoRoot <- doesDirectoryExist ".git"
  unless inRepoRoot $ error "must run from git repo root"

git :: MonadIO io => [ByteString] -> io ByteString
git args =
  strip . toStrictBytes <$> readProcessStdout_ (proc "git" args')
  where
    args' = map (Text.unpack . decodeUtf8Lenient) args

git_ :: MonadIO io => [ByteString] -> io ()
git_ args =
  runProcess_ $ proc "git" args'
  where
    args' = map (Text.unpack . decodeUtf8Lenient) args

strip :: ByteString -> ByteString
strip = encodeUtf8 . Text.strip . decodeUtf8Lenient
