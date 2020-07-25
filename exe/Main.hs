import           RIO

import           Development.Shake (getDirectoryFilesIO, shakeArgs,
                                    shakeOptions, want, (%>))
import           Development.Shake.FilePath ((-<.>), (</>))

main :: IO ()
main = do
  mdPages <- getDirectoryFilesIO "pages" ["//*.md"]
  let
    pageRules =
      [("pages" </> page, "_site" </> page -<.> "html") | page <- mdPages]
  shakeArgs shakeOptions $ do
    want $ map snd pageRules
    for_ pageRules \(source, html) -> do
      html %> \_ -> _ source
