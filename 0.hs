import Data.List.Split -- fn: splitOn
import Prelude hiding (getLine, readFile, writeFile)
import System.IO
import Control.Monad


-- http://stackoverflow.com/questions/33444796/read-file-with-utf-8-in-haskell-as-io-string
main = do
    putStrLn "enter file name: "
    sourceFileName <- getLine
    handleS <- openFile sourceFileName ReadMode
    hSetEncoding handleS utf8
    contentsS <- hGetContents handleS
    names <- getNames
    let result = fn1 names contentsS
    let outputFileName = (((splitOn ".txt" sourceFileName) !! 0) ++ "_.txt")
    outputHandle <- openFile outputFileName WriteMode
    hSetEncoding outputHandle utf8
    hPutStr outputHandle result
    hClose outputHandle
    putStrLn $ "File saved: " ++ outputFileName


-- http://stackoverflow.com/questions/14668217/the-haskell-way-to-accept-user-input-a-user-inputted-number-of-times
getNames = do
    putStrLn "채팅방에 몇 명이 있나요?: " 
    numOfMember <- getLine
    putStrLn "이름들을 입력해주세요: "
    names <- replicateM (read numOfMember) getLine
    return names


fn1 names content = foldl fn0 content names
  where  
    fn0 :: String -> String -> String
    -- "<name>, ["
    fn0 content name = temp ++ (mergeListWithNewline (map (fn04.fn03.fn02.fn01) list1))
      where list0 = splitOn (name ++ ", [") content -- :: [String]
            temp = list0 !! 0
            list1 = drop 1 list0
            
            fn01 = ((drop 1).(mySplitOn "\n"))
            fn02 = deleteLastLineIfThatIsEmpty
            fn03 = mergeListWithNewline
            fn04 = ((name ++ ": ") ++ )

mySplitOn token str = [ x |  x <- splitOn token str, x /= "" ]


mergeListWithNewline [] = []
mergeListWithNewline (x:[]) = x
mergeListWithNewline (x:xs) = (x++"\n") ++ mergeListWithNewline xs

deleteLastLineIfThatIsEmpty :: [String] -> [String]
deleteLastLineIfThatIsEmpty str = if length str < 2 then str else
    if last str == "" then dropFrontAndBackside 0 1 str else str

dropFrontAndBackside m n list = reverse $ drop n $ reverse $ drop m list
