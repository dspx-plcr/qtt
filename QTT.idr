module QTT

import Control.Linear.LIO
import Data.Fin
import Data.List
import Data.List1
import Data.String
import Decidable.Equality
import System.Clock
import System.File

Score : Type
Score = Fin 6

parseScore : String -> Maybe Score
parseScore score = do
  score <- parsePositive score
  natToFin score 6

minEF : Double
minEF = 1.3

startEF : Double
startEF = 2.5

record EF where
  constructor MkEF
  value : Double
  efproof : (QTT.minEF <= value) = True

newEF : EF
newEF = MkEF startEF Refl

record Card where
  constructor MkCard
  front : String
  back : String
  tested : Clock UTC
  i : Nat
  n : Nat
  ef : EF

Show Card where
  show (MkCard f b t i n e) = f ++ "\n---\n" ++ b ++ "\n===\n" ++
    (show (toNano t)) ++ "," ++ (show i) ++ "," ++ (show n) ++ "," ++
    (show e.value)

newCard : String -> String -> Card
newCard front back = MkCard front back (MkClock 0 0) 0 0 newEF

updateCard : Card -> Clock UTC -> Score -> Card
updateCard c t s =
  let (i, n) : (Nat, Nat) =
        case s >= 3 of
          False => (1, 0)
          True => let i = case c.n of
                            0 => 1
                            1 => 6
                            n => integerToNat $ cast (cast c.i * c.ef.value)
                  in (i, c.n + 1)
      ef : EF =
        let q : Double = cast $ the Integer 5 - finToInteger s
            v = c.ef.value + (0.1 - q * (0.08 + q * 0.02))
        in case decEq (minEF <= v) True of
             Yes pf => MkEF v pf
             _ => MkEF minEF Refl
  in { tested := t, i := i, n := n, ef := ef } c

record DB where
  constructor MkDB
  path : String
  cards : List Card

data DBError : Type where
  ParseError : DBError

parseCards : String -> Maybe (List Card)
parseCards = helper [] . lines
  where
    headTail : List String -> Maybe (String, List String)
    headTail [] = Nothing
    headTail (l::ls) = Just (l, ls)

    unlines : List String -> String
    unlines xs = concat $ intersperse "\n" xs

    getBack : List String -> List String -> (String, List String, Bool)
    getBack ls res =
      case headTail ls of
        Nothing => (unlines $ reverse res, [], False)
        Just ("===", ls) => (unlines $ reverse res, ls, True)
        Just ("", ls) =>
          case headTail ls of
            Nothing => (unlines $ reverse (""::res), [], False)
            Just ("", ls) => (unlines $ reverse res, ls, False)
            Just (l, ls) => getBack ls (l::""::res)
        Just (l, ls) => getBack ls (l::res)

    helper : List Card -> List String -> Maybe (List Card)
    helper xs [] = Just xs
    helper xs ls = do
      let (front, ls) = span (\s => not $ s == "---") ls
      guard $ not . isNil $ front
      let front = unlines front
      guard $ not . null $ front
      ls <- tail' ls
      let (back, ls, hasStats) = getBack ls []
      guard $ not . null $ back
      case hasStats of
        True => do
          (stats, ls) <- headTail ls
          let stats = map pack . forget $ splitOn ',' (unpack stats)
          (tested, stats) <- headTail stats
          (i, stats) <- headTail stats
          (n, stats) <- headTail stats
          (ef, stats) <- headTail stats
          guard $ null stats
          tested <- parseInteger tested
          let tested = fromNano tested
          i <- parsePositive i
          n <- parsePositive n
          ef <- parseDouble ef
          ef <- case decEq (minEF <= ef) True of
                  Yes pf => Just (MkEF ef pf)
                  _ => Nothing
          let ls = case tail' ls >>= tail' of
                     Nothing => []
                     Just ls => ls
          helper ((MkCard front back tested i n ef)::xs) ls
        False => helper ((newCard front back)::xs) ls

newDB : String -> String -> L {use=1} IO (Res Bool (\res => case res of
                                                              True => DB
                                                              False => DBError))
newDB path cards =
  pure1 $ case parseCards cards of
    Nothing => False # ParseError
    Just c => True # MkDB path c

makeDB : String -> List Card -> L {use=1} IO DB
makeDB path cards = pure1 $ MkDB path cards

getPath : (1 _ : DB) -> (String, L {use=1} IO DB)
getPath (MkDB p c) = (p, pure1 $ MkDB p c)

getCards : (1 _ : DB) -> (List Card, L {use=1} IO DB)
getCards (MkDB p c) = (c, pure1 $ MkDB p c)

closeDB : (1 _ : DB) -> L IO ()
closeDB (MkDB p c) = pure ()

writeDB : (1 _ : DB) -> L {use=1} IO (Res Bool (\res => case res of
                                                          True => DB
                                                          False => FileError))
writeDB db = do
  let (p, db) = getPath db
  db <- db
  let (c, db) = getCards db
  db <- db
  let str = case c of
              [] => ""
              c::cs => foldl1 (++) $ intersperse "\n\n\n" $ map show (c::cs)
  handle <- openFile p WriteTruncate
  case handle of
    Left e => closeDB db >> (pure1 $ False # e)
    Right handle => fPutStr handle str >>= (\res =>
                      case res of
                        Left e => closeDB db >> (pure1 $ False # e)
                        Right _ => pure1 $ True # db)

test : Card -> IO Card
test card = do
  putStrLn ""
  putStrLn card.front
  _ <- getLine
  putStrLn $ "The correct answer is\n\t" ++ card.back ++ "\nHow did you do?"
  putStrLn scoreExplanation
  score <- getValidScore
  time <- clockTime UTC
  pure $ updateCard card time score
  where
    getValidScore : IO Score
    getValidScore = do
      score <- getLine
      case parseScore score of
        Just score => pure score
        Nothing => getValidScore
    scoreExplanation = """
      0: Complete failure to recall the information
      1: Incorrect, but upon seeing the answer, it seemed familiar
      2: Incorrect, but upon seeing the answer, it seemed easy
      3: Correct, but after significant effort
      4: Correct, after some hesitation
      5: Correct with perfect recall
      """

testAll : (1 _ : DB) -> L {use=1} IO DB
testAll db = do
  let (cs, db) = getCards db
  db <- db
  let (p, db) = getPath db
  db <- db
  closeDB db
  cs <- traverse (liftIO . testIf) cs
  makeDB p cs
  where
    shouldTest : Card -> Clock UTC -> Bool
    shouldTest c t =
      let diff = timeDifference t c.tested
          diff = (fromInteger (toNano diff)) / (1000*1000*1000*60*60*24)
      in diff >= (fromInteger $ natToInteger c.i)
    where
      fromInteger : Integer -> Double
      fromInteger = Prelude.fromInteger
    testIf c = do
      t <- clockTime UTC
      if shouldTest c t then test c else pure c

main : IO ()
main = do
  let path = "db.txt"
  d <- readFile path
  case d of
    Left e => putStrLn $ "coudln't open file " ++ path ++ ": " ++ (show e)
    Right d =>
      run $ do
        True # db <- newDB path d
          | False # ParseError => putStrLn "problem when parsing the DB"
        db <- testAll db
        True # db <- writeDB db
          | False # (GenericFileError e) => putStrLn $ "problem writing out DB: " ++ (show (GenericFileError e))
          | False # FileReadError => putStrLn $ "problem writing out DB: " ++ (show FileReadError)
          | False # FileWriteError => putStrLn $ "problem writing out DB: " ++ (show FileWriteError)
          | False # FileNotFound => putStrLn $ "problem writing out DB: " ++ (show FileNotFound)
          | False # PermissionDenied => putStrLn $ "problem writing out DB: " ++ (show PermissionDenied)
          | False # FileExists => putStrLn $ "problem writing out DB: " ++ (show FileExists)
        closeDB db
