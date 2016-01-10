{- 
  The main module.
  Calls all the interview questions and runs them.
-}

module Interview
( interview
) where

import AppleStock (getProfit)
import WaterInfrastructure (calculate)

interview :: IO ()
interview = do
  putStrLn $ "The highest apple stock profit from [10, 7, 5, 8, 11, 9] will be: " ++ (show $ getProfit [10, 7, 5, 8, 11, 9] )
  calculate


