import           RIO

import           Development.Shake (addTarget, getDirectoryFilesIO, need,
                                    shakeArgs, shakeOptions, want, (%>))
import           Development.Shake.FilePath ((-<.>), (</>))
import           Text.Pandoc.App (convertWithOpts, defaultOpts, optInputFiles,
                                  optOutputFile, optStandalone)

import           GitHubPages (deploy)

main :: IO ()
main = do
  mdPages <- getDirectoryFilesIO "pages" ["//*.md"]
  let
    pageRules =
      [("pages" </> page, "_site" </> page -<.> "html") | page <- mdPages]
  shakeArgs shakeOptions $ do
    want $ map snd pageRules
    for_ pageRules \(sourceFile, htmlFile) ->
      htmlFile %> \_ -> do
        need [sourceFile, "exe/Main.hs"]
        liftIO $
          convertWithOpts
            defaultOpts
              { optInputFiles = Just [sourceFile]
              , optOutputFile = Just htmlFile
              , optStandalone = True
              }
    addTarget "deploy" $ error "deploy"
