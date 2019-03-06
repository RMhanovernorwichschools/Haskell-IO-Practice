{-
    This program will build on dictionary.hs and wordsToPhone from a previous
    assignment. You can copy your wordsToPhone source code here or you can simply
    include the line:
    
    import PTfuncsyntax
    
    and run this program in the same directory with your PFfuncsyntax.hs file.
    
    This program will ask the user to enter a 4-digit number. It will then list 
    off all of the english words that can be formed from that number on a standard 
    telephone keypad.
    
    Example of use:
    
    *Main> main
    Type a four-digit number:
    2376
    "Afro"
    "Bern"
    "berm"
    *Main> 

-}

import PTfuncsyntax
import Data.Char

numtoList x = 
    let a = div x 1000
        b = div (x-(a*1000)) 100
        c = div ((x-(a*1000))-(b*100)) 10
        d = x-((a*1000)+(b*100)+(c*10))
    in [a,b,c,d]

findOptions :: (Eq a, Num a) => [a] -> [[Char]]
findOptions [] = [""]
findOptions (x:[]) = idOpSub x
findOptions (x:xs) = [ a++b | a<-(idOpSub x), b<-(findOptions xs)]

idOpSub :: (Eq a, Num a) => a -> [[Char]]
idOpSub x
    | x==2 = ["a","b","c"]
    | x==3 = ["d","e","f"]
    | x==4 = ["g","h","i"]
    | x==5 = ["j","k","l"]
    | x==6 = ["m","n","o"]
    | x==7 = ["p","q","r","s"]
    | x==8 = ["t","u","v"]
    | x==9 = ["w","x","y","z"]
    |otherwise = []

checkHead x
    | x==2 = "abc"
    | x==3 = "def"
    | x==4 = "ghi"
    | x==5 = "jkl"
    | x==6 = "mno"
    | x==7 = "pqrs"
    | x==8 = "tuv"
    | x==9 = "wxyz"
    |otherwise = []
    
repSpace :: [Char] -> [Char]
repSpace [] = []
repSpace ('\n':xs) = ' ':(repSpace xs)
repSpace (x:xs) = (toLower x):(repSpace xs)
    
finalize x xs=
    [a | a<-x, elem a xs]

main = do
    wordList <- readFile "/usr/share/dict/american-english"
    putStrLn "Type a four-digit number:"
    numb <- readLn
    let valids = filter (\x -> length x ==4 && elem (head x) ((checkHead . head . numtoList) numb)) ((words . repSpace) wordList)
    let ansList = finalize ((findOptions . numtoList) numb) valids
    mapM_ putStrLn ansList
