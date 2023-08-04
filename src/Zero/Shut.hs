module Zero.Shut (shut,unit) where

import Prelude hiding ( fail )

import Zero.Color
import Data.List
import System.Exit

data Unit = Fail String String String | Pass String String

instance Show Unit where
   show (Fail d _ _) = clr Red "x " ++ clr Dim d
   show (Pass d _) = clr Green "v " ++ clr Dim d

detail :: Unit -> String
detail u@(Fail _ t v) = intercalate "\n" [
   "" ,
   show u ,
   clr Dim "  " ++ t ,
   clr Dim "v " ++ clr Dim v ]
detail u@(Pass _ t) = intercalate "\n" [
   "" ,
   show u ,
   "  " ++ t ]

unit :: (Eq a,Show a) => String -> a -> a -> Unit
unit d t v = if t /= v then Fail d (show t) (show v) else Pass d (show t)

shut :: [Unit] -> IO ()
shut t = do
   putStrLn $ "\n" ++ (unlines (map show t) ++ result)
   exitWith $ if failed then ExitFailure 1 else ExitSuccess
   where
      result = if failed then clr Red "\n failed\n" ++ showFailed else clr Green "\n all tests passed\n"
      fail (Pass _ _) = False
      fail (Fail _ _ _) = True
      failed = any fail t
      showFailed = unlines . map detail $ filter fail t

{- USAGE:

   shut [
      unit "description test 1" (test 1) expected1
      ,
      unit "description test 2" (test 2) expected2
      -- ...
      ]
-}
