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
    putStrLn "채팅방에 몇 명이 있나요?: "
    -- numOfMembers0 <- getLine
    -- let (numOfMembers :: Int) = ord numOfMembers0
    -- names0 <- getNames
    -- let names = splitOn "\n" names0
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
    numOfMember <- getLine
    putStrLn "이름들을 입력해주세요: "
    names <- replicateM (read numOfMember) getLine
    return names


fn1 names content = myfn fn0 content names
  where  
    fn0 :: String -> String -> String
    -- "<name>, ["
    fn0 content name = temp ++ (mergeListWithNewline list1)
      where list01 = splitOn (name ++ ", [") content -- [String]
            temp = list01 !! 0
            list0 = drop 1 list01
            list00 = map ((drop 1).(splitOn "\n")) list0--[[String]] 
            {-list000 = (list00 !! 0) : 
              map deleteLastLineIfThatIsEmpty (drop 1 list00)-} --[[String]]
            list000 = map deleteLastLineIfThatIsEmpty list00
            list10 = map mergeListWithNewline list000 -- [String]
            list1 = map ((name ++ ": " )++ ) list10 -- [String]

mergeListWithNewline (x:[]) = x
mergeListWithNewline (x:xs) = (x++"\n") ++ mergeListWithNewline xs

myfn fn content []   = content
myfn fn content (a:as) = myfn fn (fn content a) as

deleteLastLineIfThatIsEmpty :: [String] -> [String]
deleteLastLineIfThatIsEmpty str = if last str == "" then 
    reverse $ drop 1 $ reverse str else str



-- -- -- -- -- -- -- -- -- -- 아래는 뻘짓의 결과물이다.-- -- -- -- -- -- -- -- 



-- import System.IO.Encoding
-- import System.Environment (getArgs)
-- import Data.Encoding.UTF8
-- import qualified Data.Text as T
-- 주석 처리된 import 들은 뻘짓의 결과물
-- 아, main에서 주석 처리된 것들도임




-- 이 getNames 함수들은 내가 만든건데 일단은 쓸모없음.

-- getNames :: Int -> IO String
-- 내 생각에 이건 type signiture 부터 잘못되었음. 
-- getNames n = go n ""
--     where go n contents = do
--         line0 <- getLine
--         if n == 1
--             then return $ contents ++ line0
--             else go (n-1) (contents ++ line0 ++ "\n")

-- getNames n = go n []
--     where go n list = do
--         line <- getLine
--         if n == 1
--             then return $ line:list
--             else go (n-1) (line:list)









-- 한사람이 여러줄에걸쳐서 말하면 데이터가 날아가는 문제가있음.. 는해결함


-- names의 0번째 이름으로 시작하지 않는 전체텍스트에 대해 맨 위 말이 짤리는 문제가 있음
-- 왜냐하면 맨처음에 이름0으로 스플릿하고나서 토큰들에 대해 다들 첫째줄을 지우게 하는데
-- 이러면 맨윗줄은 맨처음토큰이므로 이름1을 포함한 줄이 지워짐
-- 그럼 다음 이름1로 토크나이징할땐 이름1이 지워진 데이터를 쓰는거임
-- 이러면안되징..
-- 토큰들의 첫번째는 첫째줄을안날리게바꿨는데 그래도 안되네
-- 이름을 달때 첫째토큰은 남겨두고 하자. 그것은 ""이거나 다른 이름일 테니까.




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
-- 따라서 건드리지않고 냅두는게맞는거임. 또 다른 예로 
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

