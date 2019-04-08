module Lib where

import Data.Maybe as Maybe

f :: Maybe Int -> Int
f Nothing = 0
f (Just x) = x


