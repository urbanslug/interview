{- 
From interviewcake.com (A good place to prepare for interviews).
Question:

Writing coding interview questions hasn't made me rich. Maybe trading Apple stocks will.
Suppose we could access yesterday's stock prices as a list, where:

The indices are the time in minutes past trade opening time, which was 9:30am local time.
The values are the price in dollars of Apple stock at that time.
So if the stock cost $500 at 10:30am, stock_prices_yesterday[60] = 500.

Write an efficient function that takes stock_prices_yesterday and returns the best profit I could have made from 1 purchase and 1 sale of 1 Apple stock yesterday.

Example list: [10, 7, 5, 8, 11, 9]

-}


module AppleStock (
getProfit
)
where

getProfit :: [Int] -> Int
getProfit [] = 0
getProfit [_] = 0
getProfit (x:y:[]) = if x > y
                        then 0
                        else y - x
getProfit (x:y:z:zs)
  | x > y = getProfit (y:z:zs)
  | otherwise =
    if y < z
       then getProfit (x:z:zs)
       else getProfit (x:y:zs)
