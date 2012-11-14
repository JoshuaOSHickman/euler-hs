import Data.Time
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.List

 -- correct answer is either 171 or 172, as that's 1200/7. 
beginningDay :: Day
beginningDay = fromGregorian 1901 1 6 -- hint says 1900 Jan 1 is a monday. First day is first sunday in 1901, a year later. 

nextWeek :: Day -> Day
nextWeek = addDays 7
        
inTwentiethCentury :: Day -> Bool
inTwentiethCentury day = year < 2001 && year > 1900
  where (year, _, _) = toGregorian day

allDays :: [Day]
allDays = allDays' beginningDay []
  where allDays' start acc = if inTwentiethCentury start
                             then allDays' (nextWeek start) (start:acc)
                             else acc

main :: IO ()
main = print $ length $ filter ("-01" `isSuffixOf`) $ reverse (map show allDays)