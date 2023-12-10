module PresetParser where

import Control.Applicative
import Control.Monad (forever, void, when)
import Data.HashMap.Strict qualified as H
import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)

type CordHashMap = H.HashMap (Int, Int) String

-- parser designed to impliment Conway Run-length encoding strings

data PreSet = PreSet
  { cords :: (Int, Int), -- parse board cords
    mapV :: (CordHashMap, String, (Int, Int)) -- cordmap parsing
  }
  deriving (Show)

instance Read PreSet where
  readsPrec _ input =
    case parse presSetParser "" input of
      Left err -> []
      Right result -> [(result, "")]

-- parser skips space
skipSpaces :: Parser ()
skipSpaces = void (Control.Applicative.many space)

-- parses an Int value (for parsing coords, can fail because well-formatted
-- string assumption)
parseInt :: Parser Int
parseInt = read <$> some digit

-- parses an Int or returns nothing (for parsing RLE substrings)
parseIntMaybe :: Parsec String () (Maybe Int)
parseIntMaybe = do
  digits <- Control.Applicative.many digit
  return (readMaybe digits)

-- parses PreSet type first and then kicks off parseSubstring
presSetParser :: Parser PreSet
presSetParser = do
  satisfy ('x' ==)
  x <- parseInt
  satisfy ('y' ==)
  y <- parseInt
  skipSpaces
  mapV <- parseSubstring H.empty (0, 0)
  pure $ PreSet (x, y) mapV

-- handles parsing Conway RLE substrings after coords are parsed
parseSubstring :: CordHashMap -> (Int, Int) -> Parser (CordHashMap, String, (Int, Int))
parseSubstring hashMap (x, y) = do
  num <- parseIntMaybe
  char <- anyChar
  case char of
    -- terminates at !
    '!' -> pure (hashMap, "", (x, y))
    -- newline at $
    '$' -> do
      (hashMap', str, (x', y')) <- parseSubstring hashMap (x + 1, 0)
      pure (hashMap', str, (x', y'))
    c -> do
      let (hashMap', (a, b)) = checkInt hashMap c num (x, y)
      (hashMap'', str, (x', y')) <- parseSubstring hashMap' (a, b)
      pure (hashMap'', str, (x', y'))

-- unwraps maybe Int, if none does 1 cell (following RLE parsing), if Just x
-- then do x cells
checkInt :: CordHashMap -> Char -> Maybe Int -> (Int, Int) -> (CordHashMap, (Int, Int))
checkInt cm c mi (x, y) =
  case mi of
    Nothing -> applyXCells cm c 0 (x, y)
    Just i -> applyXCells cm c (i - 1) (x, y)

-- applies either dead or alive cell to next i cells following parser
applyXCells :: CordHashMap -> Char -> Int -> (Int, Int) -> (CordHashMap, (Int, Int))
applyXCells cm c i (x, y) =
  case c of
    'b' -> (cm, (x, y + i + 1)) -- move next i cells
    'o' -> (newmap, (x, y + i + 1)) -- populate i cells in a row
  where
    newmap = foldl (\acc coord -> H.insert coord "x" acc) cm [(i, x) | i <- [y .. (y + i)]]

-- given string value, runs parser and unwraps result for
-- hashmap functionality
parserHandler :: String -> ((Int, Int), CordHashMap)
parserHandler g = do
  case parse presSetParser "" g of
    Left c -> error "Preset Parsing Error"
    Right (PreSet cords (q, _, _)) -> (cords, q)