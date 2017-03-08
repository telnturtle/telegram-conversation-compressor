-- 긴 텔레그램 대화를 긁어서 보관할때 부피를 줄여주는 프로그램
-- by turtle
-- 2017-02-10부터 만듦

-- 사용법: PC 텔레그램에서 대화를 긁어 복사해서 텍스트파일로 붙여넣는다.
-- 폴더에 두고 실행한다.
-- 대화 참여자의 인원수와 이름을 적어주면 텍스트파일 이름 뒤에 _가 붙은
-- 파일이 생긴다. 
-- 끝.

import Data.List.Split -- fn: splitOn
import Prelude hiding (getLine, readFile, writeFile)
import System.IO
import Control.Monad


-- http://stackoverflow.com/questions/33444796/read-file-with-utf-8-in-haskell-as-io-string
main = do
    -- let ?enc = UTF8
    putStrLn "enter file name: "
    sourceFileName <- getLine
    handleS <- openFile sourceFileName ReadMode
    hSetEncoding handleS utf8
    contentsS <- hGetContents handleS
    names <- getNames
    let result = fn1 names contentsS
    -- putStr result
    let outputFileName = (((splitOn ".txt" sourceFileName) !! 0) ++ "_.txt")
    outputHandle <- openFile outputFileName WriteMode
    hSetEncoding outputHandle utf8
    hPutStr outputHandle result
    -- hPutStr outputHandle (fn1 names contentsS)
    hClose outputHandle
    putStrLn $ "File saved: " ++ outputFileName




-- http://stackoverflow.com/questions/14668217/the-haskell-way-to-accept-user-input-a-user-inputted-number-of-times
getNames = do
    putStrLn "채팅방에 몇 명이 있나요?: " 
    numOfMember <- getLine
    putStrLn "이름들을 입력해주세요: "
    names <- replicateM (read numOfMember) getLine
    return names


fn1 names content = myfn fn0 content names
  where  
    fn0 :: String -> String -> String
    -- "<name>, ["
    fn0 content name = temp ++ (mergeListWithNewline ((fn01.fn02.fn03.fn04)list1))
      where list0 = splitOn (name ++ ", [") content -- [String]
            temp = list01 !! 0
            list1 = drop 1 list0
            
            -- list00 = map ((drop 1).(splitOn "\n")) list0--[[String]]
            -- list000 = map deleteLastLineIfThatIsEmpty list00
            -- list10 = map mergeListWithNewline list000 -- [String]
            -- list1 = map ((name ++ ": " )++ ) list10 -- [String]
            
            fn01 = map ((drop 1).(splitOn "\n"))
            fn02 = map deleteLastLineIfThatIsEmpty
            fn03 = map mergeListWithNewline
            fn04 = map ((name ++ ": " )++ )


mergeListWithNewline (x:[]) = x
mergeListWithNewline (x:xs) = (x++"\n") ++ mergeListWithNewline xs

myfn fn content []   = content
myfn fn content (a:as) = myfn fn (fn content a) as

deleteLastLineIfThatIsEmpty :: [String] -> [String]
deleteLastLineIfThatIsEmpty str = if last str == "" then 
    dropFrontAndBackside 0 1 str else str

dropFrontAndBackside list m n = reverse $ drop n $ reverse $ drop m list


-- -- -- -- -- -- -- -- -- -- 쓸데 없는 imports (haskell 버전의 차이인듯?) .-- 
-- import System.IO.Encoding
-- import System.Environment (getArgs)
-- import Data.Encoding.UTF8
-- import qualified Data.Text as T




-- 성공함. 첫째 토큰은 보통 쓸데없는 토큰임. 예를 들면
-- splitOn "." ".1.2.3." = ["", "1", "2", "3", "4"]
-- 임. 첫째는 의미가 없음. 이름0, 이름1로 순차적으로 처리하는 이 프로그램에선
-- 마찬가지로 첫번째 토큰은 의미가 없음. 
-- 예를 두 가지 들 수 있는데 대화가 name0으로 시작하지않는경우와 시작하는 경우임. 예를들면

-- "
-- name3, [time]
-- Blah blah

-- name0, [time]
-- Blah blah
-- " 를 name0으로 자르면

-- ["name3, [time]\nBlah blah\n", "time]\nBlah blah\n"]
-- 똑같진 않지만 대충 이런 모양이 됨. 여기서 첫번째토큰은 name0에게 의미가없음.
-- 따라서 건드리지않고 냅두는게맞는거임.


--  또 다른 예로 
-- "
-- name0, [time]
-- Blah blah

-- name0, [time]
-- Blah blah
-- " 를 name0으로 자르면

-- ["", "time]\n~~~~~~~~~"] 
-- 따위가 됨. 여기서도 첫째 토큰은 의미가 없음. 따라서 첫째 토큰은 내버려두는게 맞음.

-- 나만 알아볼수 있는 설명인듯
-- 몇달뒤에 보면 나도 못알아볼 수도 있음
