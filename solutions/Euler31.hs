coins = [200, 100, 50, 20, 10, 5, 2]


options [] moneyLeft = 1
options coinsLeft moneyLeft = sum . map (options (tail coinsLeft)) . map (moneyLeft -) $ ofThisCoin
  where coin = head coinsLeft
        maxCoin = moneyLeft `div` coin
        ofThisCoin = map (* coin) [0..maxCoin]

main = print $ options coins 200