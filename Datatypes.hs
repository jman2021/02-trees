module Datatypes where

import Test.HUnit
import Prelude hiding (Either, Just, Left, Maybe, Nothing, Right)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Eq)

nextWeekday :: Day -> Day
nextWeekday Monday = Tuesday
nextWeekday Tuesday = Wednesday
nextWeekday Wednesday = Thursday
nextWeekday Thursday = Friday
nextWeekday Friday = Monday
nextWeekday Saturday = Monday
nextWeekday Sunday = Monday

twoBusinessDays :: Day -> Day
twoBusinessDays Friday = Monday
twoBusinessDays Saturday = Tuesday
twoBusinessDays Sunday = Wednesday
twoBusinessDays day = nextWeekday (nextWeekday day)

data Shape
  = Circle Double Double Double
  | Rectangle Double Double Double Double
  deriving (Eq, Show)

area :: Shape -> Double
area (Circle x y r) = pi * r * r
area (Rectangle llx lly urx ury) = width * height
  where
    width = urx - llx
    height = ury - lly

data Point = Point {x :: Double, y :: Double}
  deriving (Show, Eq)

point1 = Point {y = 1.0, x = 2.0} -- order doesn't matter

point2 = Point 1.0 20.0 -- Be careful, Haskell will let you leave the field names off
-- but here the order does matter

x1 = x point1

distFromOrigin :: Point -> Double
distFromOrigin Point {x = px, y = py} = sqrt (px * px + py * py)

distFromOrigin' p =
  let px = x p
   in let py = y p
       in sqrt (px * px + py * py)

distFromOrigin'' Point {x = x, y = y} = sqrt (x * x + y * y)

point3 :: Point
point3 = point1 {x = 2.0}

-- point3 is a Point with x component equal to 2.0,
-- and all others (which is only y here) the same as point1

data IntListNE
  = ISingle Int
  | ICons Int IntListNE

oneTwoThree :: IntListNE
oneTwoThree = ICons 1 (ICons 2 (ISingle 3))

oneTwoThree' :: IntListNE
oneTwoThree' = 1 `ICons` (2 `ICons` ISingle 3) -- backticks for infix

safeHead :: IntListNE -> Int
safeHead (ISingle val) = val
safeHead (ICons val _) = val

testHeadIL :: Test
testHeadIL = "headOfIntListNE" ~: safeHead oneTwoThree ~?= 1

sumOfIntListNE :: IntListNE -> Int
sumOfIntListNE (ISingle val) = val
sumOfIntListNE (ICons val tl) = val + sumOfIntListNE tl

testSumIL = "sumOfIntListNE" ~: sumOfIntListNE oneTwoThree ~?= 6

data Maybe a = Nothing | Just a

noInt :: Maybe Int
noInt = Nothing

justTrue :: Maybe Bool
justTrue = Just True

justThree :: Maybe Int
justThree = Just 3

data Either a b = Left a | Right b

safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "You can't divide by zero, silly."
safeDiv x y = Right $ x `div` y

data Tree a
  = Empty -- No data
  | Branch a (Tree a) (Tree a) -- data of type a, left and right subtrees
  deriving (Eq, Show)

exTree :: Tree Int
exTree =
  Branch
    5
    (Branch 2 (Branch 1 Empty Empty) (Branch 4 Empty Empty))
    (Branch 9 Empty (Branch 7 Empty Empty))

-- | increment all integers in the tree
treePlus :: Tree Int -> Int -> Tree Int
treePlus Empty val = Empty
treePlus (Branch node_val lt rt) val = Branch (node_val + val) (treePlus lt val) (treePlus rt val)

testTreePlus = "treePlus" ~: treePlus (Branch 2 Empty Empty) 3 ~?= Branch 5 Empty Empty

infixOrder :: Tree a -> [a]
infixOrder Empty = []
infixOrder (Branch x l r) = infixOrder l ++ (x : infixOrder r)

testInfixOrder :: Test
testInfixOrder = "infixOrder" ~: infixOrder exTree ~?= [1, 2, 4, 5, 9, 7]

prefixOrder :: Tree a -> [a]
prefixOrder Empty = []
prefixOrder (Branch val lt rt) = (val : prefixOrder lt) ++ prefixOrder rt

testPrefixOrder = "prefixOrder" ~: prefixOrder exTree ~?= [5, 2, 1, 4, 9, 7]

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f Empty = Empty
treeMap f (Branch x l r) = Branch (f x) (treeMap f l) (treeMap f r)

treeIncr :: Tree Int -> Tree Int
treeIncr = treeMap (+ 1)

testTreeIncr =
  "treeIncr" ~: treeIncr (Branch 1 (Branch 2 Empty Empty) Empty)
    ~?= Branch 2 (Branch 3 Empty Empty) Empty

main :: IO ()
main = do
  runTestTT $
    TestList
      [ testSumIL,
        testHeadIL,
        testTreePlus,
        testInfixOrder,
        testTreeIncr,
        testPrefixOrder
      ]
  return ()
