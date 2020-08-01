import Data.List
import Data.Char
import System.IO

charDiff :: Char -> Char -> Int
charDiff x y = ord y - ord x

charRept :: Int -> Char -> [Char]
charRept n c = go_charRept n c []
    where
        go_charRept :: Int -> Char -> [Char] -> [Char]
        go_charRept n c acc
            | n > 0 = go_charRept (n-1) c (c:acc)
            | n == 0 = acc ++ "o"

charDir :: Int -> Char
charDir n
    | n > 0 = 'p'
    | n < 0 = 'm'

bridge :: [Char] -> [Char]
bridge (x:x':xs) = go_bridge (x:x':xs) []
    where
        go_bridge :: [Char] -> [Char] -> [Char]
        go_bridge (_:[]) acc = acc
        go_bridge (x:x':xs) acc
            | x == x' = go_bridge (x':xs) (acc ++ "o")
            | otherwise = go_bridge (x':xs) (acc ++ newStr)
            where
                newStr = charRept (abs(charDiff x x')) (charDir (charDiff x x'))

-- FUNC CALL FOR INTERACTIVE TERMINAL
heckin :: [Char] -> [Char]
heckin (x:x':xs) = bridge ((chr 0):x:x':xs)

--------------------------------------------

crawler :: [Char] -> [Char]
crawler (x:xs) = go_crawler (x:xs) 0 []
    where
        go_crawler :: [Char] -> Int -> [Char] -> [Char]
        go_crawler [] _ acc_Str = acc_Str
        go_crawler (x:xs) acc_Reg acc_Str
            | x == 'p' = go_crawler xs (acc_Reg + 1) acc_Str
            | x == 'm' = go_crawler xs (acc_Reg - 1) acc_Str
            | x == 'o' = go_crawler xs acc_Reg (acc_Str ++ ((chr acc_Reg):[]))

-- FUNC CALL FOR INTERACTIVE TERMINAL
brain = crawler