module Poker where
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Function (on)

deal list = do
  let hand_1 = dealHand list 1 2
  let hand_2 = dealHand list 0 1
  let best_Hand = get_Winner hand_1 hand_2
  best_Hand

--Get combo before conversion and converts each seperately
get_Winner hand1 hand2 = do
    let x = highestHand hand1
    let y = highestHand hand2
    let preResults = get_Best_Hand x y
    let results = get_Printing_Hand preResults
    results

--Compares the 2 hands to get winner and sort/transforms them
get_Best_Hand hand1 hand2
  | (fst hand1) > (fst hand2) = getsrtHand1
  | (fst hand1) < (fst hand2) = getsrtHand2
  | otherwise = getsrtHand1
  where srtdHand1 = snd hand1
        srtdHand2 = snd hand2
        getsrtHand1 = sort_Cards_2 (sort_Cards(chng_Crds_TSort srtdHand1))
        getsrtHand2 = sort_Cards_2 (sort_Cards(chng_Crds_TSort srtdHand2))
--Gets the maximum value from both list and compares them to print and transforms
highestHand hand = maximum $ (poker_Rank hand)

--Runs through the entire list converting the elements and evaluating
poker_Rank hand =do
 let x = combinations 5 hand
 let y = map conv_map x
 let z = map getHandVal y
 z


conv_map list = do
  let x = map get_Cardval list
  let y = map get_SuitVal list
  let hand = zip x y
  let handS = sort_Cards hand
  handS

dealHand list rng_1 rng_2 =do
 let x = take 4(list)
 let y = drop 4(list)
 let x1 = get_Two_Cards x rng_1 rng_2
 let hand_1 = x1 ++ y
 hand_1

--Methods for hand evaluating (used on the side)
------------------------------------------------------------------

handEqual xs = all (== head xs) (tail xs)

get_Printing_Hand list = concatMap (\(c,i) -> [show (c) ++ get_PrintSuit(i)]) list

chng_Crds_TSort list = map (\(c,i) -> (get_Int_PrintVal(c),i)) list

get_Int_PrintVal card
 | card == 14 = 1
 | otherwise = card

get_PrintSuit card
 | card == 1 = "C"
 | card == 2 = "D"
 | card == 3 = "H"
 | card == 4 = "S"

get_SuitVal suit
 | suit <= 13 = 1
 | suit <= 26 = 2
 | suit <= 39 = 3
 | suit <= 52 = 4

get_Cardval card
 | card == 1 = 14
 | card <= 14 = card
 | otherwise = get_Cardval (card - 13)


get_Two_Cards :: (Enum a1, Eq a1, Num a1) => [a2] -> a1 -> a1 -> [a2]
get_Two_Cards hx c d = [k | (1,k) <- zip (cycle [c..d]) hx]

 --Use sortBy to first sort the in terms of the first then 2nd
sort_Cards list = do
   let x = sortBy (compare `on` fst) list
   x

sort_Cards_2 list = do
   let x = sortBy (compare `on` snd) list
   x

--Helper Methods with hand value results (main results)
------------------------------------------------------------------

getHandVal hand
    | hStraightFlush hand == True = (pointifyStrFlush(handNoSuit),hand)
    | hFlush hand == True = (pointifyFlush(handNoSuit),hand)
    | hStraight hand  == True = (pointifyStraight(handNoSuit),hand)
    | hFourAKind handNoSuit == True = (pointifyFouroK(handNoSuit),hand)
    | hFullhouse handNoSuit == True = (pointifyFullh(handNoSuit), hand)
    | hThreeAKind handNoSuit == True = (pointifyThreeoKind(handNoSuit),hand)
    | hTwoPair handNoSuit == True = (pointifyTwop(handNoSuit),hand)
    | hOnePair handNoSuit == True = (pointifyOnep(handNoSuit),hand)
    | otherwise = (pointifyHighcrd(handNoSuit),hand)
    where handNoSuit = map fst hand


pointifyStrFlush hand = (retStraightFlush hand) + 8000
pointifyFlush hand = (sum hand) + 5000
pointifyStraight hand = (retStraightFlush hand) + 4000
pointifyFouroK hand = (retFourKindValue hand) + 7000
pointifyFullh hand = (retThreeKindValue hand) * 15 + (retOnePairValue hand) + 6000
pointifyThreeoKind hand = (retThreeKindValue hand) * 15 + (retSingles hand) + 3000
pointifyTwop hand = (fst (retTwoPairValue hand) * 15) + (snd (retTwoPairValue hand) * 15) + (retSingles hand) + 2000
pointifyOnep hand = (retOnePairValue hand) * 15 + (retSingles hand) + 1000
pointifyHighcrd hand = (sum hand)

countRepetions x list = (length.filter(== x)) list

returnRepetition hand = do
        let repetitions = map (\x -> countRepetions x hand) hand
        let newHand = zip hand repetitions
        newHand

retSingles hand = do
        let qList = filter (\x -> snd x == 1) (returnRepetition hand)
        let ret = map fst qList
        sum ret

retOnePairValue hand = do
        let pList = filter (\x -> snd x == 2) (returnRepetition hand)
        fst (head pList)

retTwoPairValue hand = do
        let pList = filter (\x -> snd x == 2) (returnRepetition hand)
        (fst (head pList), fst (last pList))

retThreeKindValue hand = do
         let tList = filter (\x -> snd x == 3) (returnRepetition hand)
         fst (head tList)

retFourKindValue hand = do
         let qList = filter (\x -> snd x == 1) (returnRepetition hand)
         fst (head qList)

retStraightFlush hand = do
        if hand==[2,3,4,5,14] then 5 else last(hand)
----------------------------------------------------------------------------
--Checking
hOnePair hand
 | length pair == 2 = True
 | otherwise = False
  where pair = filter (\x -> snd x == 2) (returnRepetition hand)


hTwoPair hand
 | length pairs == 4 = True
 | otherwise = False
  where pairs = filter (\x -> snd x == 2) (returnRepetition hand)


--if there are 3 equal cards and not a Fullhouse
hThreeAKind hand
 | (any (3==) repetitions) && not (hFullhouse hand) = True
 | otherwise = False
  where repetitions = map (\x -> countRepetions x hand) hand


hFullhouse hand
 | (any (3==) repetitions) && (any (2==) repetitions) = True
 | otherwise = False
 where repetitions = map (\x -> countRepetions x hand) hand


hFourAKind hand
  | any (4==) repetitions = True
  | otherwise = False
  where repetitions = map (\x -> countRepetions x hand) hand


--Checks if it's a straight flush
hStraightFlush hand
 | (hStraight hand) && (hFlush hand) = True
 | otherwise = False


--Checks if its flush
hFlush hand
 | handEqual suits = True
 | otherwise = False
 where suits = map snd hand

--Checks if its straight
hStraight hand
  | hRepeating (cards) = True
  | [2,3,4,5,14] == cards = True
  | otherwise = False
  where cards = map fst hand

hRepeating xs = hRepeating' xs False
      where   hRepeating' [] acc = acc
              hRepeating' (x:[]) acc = acc
              hRepeating' (x:y:ys) acc
               | x + 1 == y = hRepeating' (y:ys) True
               | otherwise = False

--Used to check all variations
combinations 0 _ = [[]]
combinations _ [] = []
combinations n xs@(y:ys)
   | n < 0 = []
   | otherwise = case drop (n-1) xs of
     [ ] -> []
     [_] -> [xs]
     _ -> [y:c | c <- combinations (n-1) ys]
        ++ combinations n ys
