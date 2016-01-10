module InterviewSpec where

import Test.Hspec

-- Import the module we wish to test.
-- import Interview


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Check the entire interview module" $
         it "Test the calculate function." $
           pendingWith "I see no need to test this."
