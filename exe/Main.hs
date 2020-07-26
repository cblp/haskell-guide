import           RIO

import           Development.Shake (getDirectoryFiles, getDirectoryFilesIO,
                                    need, shakeArgs, shakeOptions, want, (%>),
                                    (~>))
import           Development.Shake.FilePath ((-<.>), (</>))
import           Text.Pandoc.App (convertWithOpts, defaultOpts, optInputFiles,
                                  optOutputFile, optStandalone)

import           GitHubPages (deploy)

main :: IO ()
main = do
  pagesMd <- getDirectoryFilesIO sourceDir ["//*.md"]
  let pagesHtml = map (-<.> "html")  pagesMd
  let filesHtml = map (buildDir </>) pagesHtml

  shakeArgs shakeOptions do

    want filesHtml

    for_ (zip pagesMd pagesHtml) \(pageSource, pageHtml) -> do
      let fileSource = sourceDir </> pageSource
      let fileHtml   = buildDir  </> pageHtml
      fileHtml %> \_ -> do
        need [fileSource, "exe/Main.hs"]
        liftIO $
          convertWithOpts
            defaultOpts
              { optInputFiles = Just [fileSource]
              , optOutputFile = Just fileHtml
              , optStandalone = True
              }

    "deploy" ~> do
      need filesHtml
      liftIO $ deploy buildDir pagesHtml

sourceDir :: FilePath
sourceDir = "pages"

buildDir :: FilePath
buildDir = "_site"
