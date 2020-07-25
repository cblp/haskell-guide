import           RIO

import           Development.Shake (getDirectoryFiles, need, shakeArgs,
                                    shakeOptions, want, (~>))
import           Development.Shake.FilePath ((-<.>), (</>))

main :: IO ()
main =
  shakeArgs shakeOptions $ do
    want [allPagesAsHtml]
    allPagesAsHtml ~> do
      pages <- getDirectoryFiles "pages" ["//*.md"]
      need ["_site" </> page -<.> "html" | page <- pages]
  where
    allPagesAsHtml = ".allPagesAsHtml"
