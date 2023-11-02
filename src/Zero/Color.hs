module Zero.Color (Color (..),clr) where

import Data.Text ( Text, pack )

data Color = Reset | Default | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Bold | Dim | Grey | Italic | Strike | Underline | Reverse | Inverse

code :: Color -> Int
code Reset = 0
code Default = 10
code Black = 30
code Red = 31
code Green = 32
code Yellow = 33
code Blue = 34
code Magenta = 35
code Cyan = 36
code White = 37
code Bold = 1
code Dim = 2
code Grey = 2
code Italic = 3
code Strike = 9
code Underline = 4
code Reverse = 7
code Inverse = 7

instance Show Color where
    show c = "\x1b[" <> (show . code) c <> "m"

clr :: Color -> String -> String
clr c s = show c <> s <> show Reset

clrt :: Color -> Text -> Text
clrt c t = pack (show c) <> t <> pack (show Reset)

