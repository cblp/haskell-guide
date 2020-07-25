import           RIO

import           Development.Shake (shakeArgs, shakeOptions)

main :: IO ()
main = shakeArgs shakeOptions $ do
  pure ()
