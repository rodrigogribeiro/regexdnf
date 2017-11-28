import Test.QuickCheck
import Regex

main :: IO ()
main
  = do
      putStrLn "Testing conversion to DNF"
      quickCheck convertOk
