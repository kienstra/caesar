module Lib (
    alphabetRot,
    getByIndex,
    indexOf,
    isDigit,
    isUpper,
    isLower,
    lowerAlphabet,
    lowerRot,
    cube,
    listLength,
    square,
    upperRot
    ) where

cube :: Int -> Int
cube x = x * square x

square :: Int -> Int
square x = x * x

type Alphabet = [Char]

digits :: [Int]
digits = [0 .. 9]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

isLower :: Char -> Bool
isLower char = char `elem` lowerAlphabet

isUpper :: Char -> Bool
isUpper char = char `elem` upperAlphabet

isDigit :: Int -> Bool
isDigit char = char `elem` digits

listLength :: [Int] -> Int
listLength [] = 0
listLength (_ : xs) = 1 + listLength xs

indexOf :: Char -> [Char] -> Int
indexOf _ [] = undefined
indexOf ch (x : xs) = if x == ch then 0 else 1 + indexOf ch xs

getByIndex :: Int -> [Char] -> Char
getByIndex _ [] = undefined
getByIndex i (x : xs) = if i == 0 then x else getByIndex (i - 1) xs

upperRot :: Int -> Char -> Char
upperRot n ch = upperAlphabet !! ((indexOf ch upperAlphabet + n) `mod` 26)

lowerRot :: Int -> Char -> Char
lowerRot n ch = lowerAlphabet !! ((indexOf ch lowerAlphabet + n) `mod` 26)

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n ch = alphabet !! ((indexOf ch alphabet + n) `mod` (length alphabet))
