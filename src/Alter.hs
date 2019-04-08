module Alter where

import Data.Maybe as Maybe


sum' :: Num a => [a] -> a
sum' list = foldl (+) 0 list


product' :: Num a => [a] -> a
product' list = foldl (*) 0 list


reverse' :: [a] -> [a]
reverse' list = foldl (\l e -> [e] ++ l) [] list


and' :: [Bool] -> Bool
and' list = foldl (&&) True list


or' :: [Bool] -> Bool
or' list = foldl (||) False list


head' :: [a] -> a
head' list = Maybe.fromJust $ foldl _header Nothing list

_header :: Maybe a -> a -> Maybe a
_header Nothing elem = Just elem        -- Remember first element  
_header acc _ = acc                     -- and discard following ones


last' :: [a] -> a
last' list = Maybe.fromJust $ foldr _laster Nothing list

_laster :: a -> Maybe a -> Maybe a
_laster elem Nothing = Just elem
_laster _ acc = acc

