{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
module Code128 where

-- You may add, remove, or edit imports as you wish. Note the use of
-- qualified imports to avoid collisions between common names. For
-- example, Prelude and Data.Map both define `map`, and Data.List
-- and Data.Map both define `insert`.

import           Data.Char
import           Data.List
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           System.Environment
import           System.Exit



--------------------------------------------------------------------------------
-- Data Representation

data BCString =            -- "Barcode String"
  BCString [Symbol]        --   Start, data, checksum, and stop symbols.
                           --   The final bar ("11") is implicit.
                           --   No quiet zones.

type Symbol   = [Module]   -- Always length 11
type Module   = Bool       -- True/False means part of bar/space

type BC =                  -- "Barcode" (internal representation)
  ( SymbolId               --   Start symbol
  , [SymbolId]             --   Encoded data
  , SymbolId               --   Checksum
  , SymbolId               --   Stop symbol
  )                        --   Final bar is implicit

type SymbolId = Int
type BValue   = Either Char String
type CValue   = Either (Char, Char) String

data TheCodes =
  TheCodes
    { startB       :: SymbolId
    , startC       :: SymbolId
    , stop         :: SymbolId
    , idsToSymbols :: Map SymbolId Symbol
    , symbolsToIds :: Map Symbol SymbolId
    , bEncodings   :: Map Char SymbolId
    , cEncodings   :: Map (Char, Char) SymbolId
    , bDecodings   :: Map SymbolId BValue
    , cDecodings   :: Map SymbolId CValue
    } deriving (Show)

type Error = String


--------------------------------------------------------------------------------
-- 1. Data Loading

loadTheCodes :: IO TheCodes
loadTheCodes = do
  rows <- map (splitOn ',') <$> lines <$> readFile "code128.csv"
  return $ rowsToCodes $ dropFirstAndLast rows

type LookupTables =
  ( Map SymbolId Symbol        -- idsToSymbols
  , Map Symbol SymbolId        -- symbolsToIds
  , Map Char SymbolId          -- bEncodings
  , Map (Char, Char) SymbolId  -- cEncodings
  , Map SymbolId BValue        -- bDecodings
  , Map SymbolId CValue        -- cDecodings
  )

rowsToCodes :: [[String]] -> TheCodes
rowsToCodes rows =

  -- Perfect opportunity for NamedFieldPuns. Try it!
  TheCodes
    { startB = 104, startC = 105, stop = 106
    , idsToSymbols = idsToSymbols
    , symbolsToIds = symbolsToIds
    , bEncodings = bEncodings
    , cEncodings = cEncodings
    , bDecodings = bDecodings
    , cDecodings = cDecodings
    }

  where
    (idsToSymbols, symbolsToIds, bEncodings, cEncodings, bDecodings, cDecodings) =
      foldr processRow
        (Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
        rows

    processRow :: [String] -> LookupTables -> LookupTables
    processRow row accumulators =
      accumulators' where

        [_id, _pattern, _, _, _bValue, _cValue] =
          row

        (idsToSymbols, symbolsToIds, bEncodings, cEncodings, bDecodings, cDecodings) =
          accumulators

        accumulators' :: LookupTables
        accumulators' =
          (idsToSymbols', symbolsToIds', bEncodings', cEncodings', bDecodings', cDecodings')

        symbolId :: SymbolId
        symbolId = read _id

        idsToSymbols' :: Map SymbolId Symbol
        idsToSymbols' = Map.insert symbolId (readSymbol _pattern) idsToSymbols

        symbolsToIds' :: Map Symbol SymbolId
        symbolsToIds' = Map.insert (readSymbol _pattern) symbolId symbolsToIds

        bEncodings' :: Map Char SymbolId
        bEncodings' =
          case _bValue of
            "comma" -> Map.insert ',' symbolId bEncodings
            "space" -> Map.insert ' ' symbolId bEncodings
            _ -> Map.insert (head _bValue) symbolId bEncodings

        cEncodings' :: Map (Char, Char) SymbolId
        cEncodings' = Map.insert (head _cValue, last _cValue) symbolId cEncodings

        bDecodings' :: Map SymbolId BValue
        bDecodings' = Map.insert symbolId (readBValue _bValue) bDecodings

        cDecodings' :: Map SymbolId CValue
        cDecodings' = Map.insert symbolId (readCValue _cValue) cDecodings


splitOn :: Char -> String -> [String]
splitOn splitChar [] = [[]]
splitOn splitChar (headChar : restChars)
  | splitChar == headChar = [] : splitOn splitChar restChars
  | otherwise = (headChar : currentWord) : restWords
  where
    currentWord : restWords = splitOn splitChar restChars


dropFirstAndLast :: [a] -> [a]
dropFirstAndLast xs =
  tail $ reverse $ tail xs


-- NOTE: otherwise case is ommited because any non 1,0 char in the string
-- should cause system to crash (would add die command but no IO for this function)
readSymbol :: String -> Symbol
readSymbol str = readSymbol' str []
  where
    readSymbol' "" lst= lst
    readSymbol' (x:xs) lst
      | x=='1' = readSymbol' xs lst ++ [True]
      | x=='0' = readSymbol' xs lst ++ [False]


-- NOTE: conisdered omitting otherwise case because any str not matching the casese
-- should cause system to crash (would add die command but no IO for this function)
-- But added to go to space for parsing P5 ed post (not clear if \n)
-- are valid for this assingment 
readBValue :: String -> BValue
readBValue str
  | head str==',' = Right "comma"
  | head str == last str = Left (head str)
  | otherwise = Left ' '  

-- head and last usage is safe because of char pair framework for CValue
readCValue :: String -> CValue
readCValue str = Left (head str, last str)



--------------------------------------------------------------------------------
-- 2. Basic Encodings

encodeB :: TheCodes -> String -> Either Error BC
encodeB theCodes str = encodeB' str []
  where
    encodeB' "" lst =
      Right (startB theCodes, lst, computeChecksum (startB theCodes) lst, stop theCodes)
    encodeB' (x:xs) lst =
        if not (isPrintable x) then Left "encodeB: unsupported characters" else
        case Map.lookup x (bEncodings theCodes) of
          Nothing     -> Left "encodeB: unsupported characters"
          Just value  -> encodeB' xs (lst ++ [value])



encodeC :: TheCodes -> String -> Either Error BC
encodeC theCodes str =
  if null (adjacentPairs str)
  then Left "encodeC: odd number of characters"
  else encodeC' str []
  where
    encodeC' "" lst =
      Right (startC theCodes, lst, computeChecksum (startC theCodes) lst, stop theCodes)
    encodeC' (x:xx:xs) lst =
        case Map.lookup (x,xx) (cEncodings theCodes) of
          Nothing     -> Left "encodeC: unsupported characters"
          Just value  -> encodeC' xs (lst ++ [value])


computeChecksum :: SymbolId -> [SymbolId] -> Int
computeChecksum symbol symbols =
  (symbol + sum (zipWith (*) symbols [1..length symbols])) `mod` 103


isPrintable :: Char -> Bool
isPrintable c = (31 < ord c) && (ord c < 127)



-- returning [] here is handled as an error case in encodeC, as encodeC 
-- has non empty entry
adjacentPairs :: [a] -> [(a, a)]
adjacentPairs helmets = adjacentPairs' helmets []
  where
    adjacentPairs' [] build = build
    adjacentPairs' [x] build = []
    adjacentPairs' (x:xx:xs) build = adjacentPairs' xs (build ++ [(x,xx)])


-- Note this is unused in implimentation
sequenceMaybes :: [Maybe a] -> Maybe [a]
sequenceMaybes lst = sequenceMaybes' lst []
  where
    sequenceMaybes' [] build = Just build
    sequenceMaybes' (x:xs) build =
      case x of
        Nothing -> Nothing
        Just val -> sequenceMaybes' xs (build ++ [val])


--------------------------------------------------------------------------------
-- 3. Basic Decodings

decodeB :: TheCodes -> BC -> Either Error String
decodeB theCodes (start_, data_, checksum_, stop_)
  | start_/= 104 = 
    Left ("decodeB: bad start: "++ show start_)
  | stop_/= 106 = 
    Left ("decodeB: bad end: "++ show stop_)
  | checksum_/= computeChecksum start_ data_  = 
    Left ("decodeB: bad checksum_: "++ show checksum_)
  | otherwise = decodeB' data_ ""
    where
      decodeB' [] lst = Right lst
      decodeB' (x:xs) lst =
          case Map.lookup x (bDecodings theCodes) of
            Nothing           -> Left "decodeB: unsupported characters"
            Just (Left c)     -> decodeB' xs (lst ++ [c])
            Just (Right str)  -> decodeB' xs (lst ++ str)


decodeC :: TheCodes -> BC -> Either Error String
decodeC theCodes (start_, data_, checksum_, stop_)
  | start_/= 105 = 
    Left ("decodeC: bad start: "++ show start_)
  | stop_/= 106 = 
    Left ("decodeC: bad end: "++ show stop_)
  | checksum_/= computeChecksum start_ data_  = 
    Left ("decodeC: bad checksum_: "++ show checksum_)
  | otherwise = decodeC' data_ ""
    where
      decodeC' [] lst = Right lst
      decodeC' (x:xs) lst =
          case Map.lookup x (cDecodings theCodes) of
            Nothing             -> Left "decodeC: unsupported characters"
            Just (Left (c1,c2)) -> decodeC' xs (lst ++ [c1,c2])
            Just (Right str)    -> decodeC' xs (lst ++ str)


--------------------------------------------------------------------------------
-- 4. Barcode String Manipulation

finalBar     = "11"
symbolLength =  11


-- added helper function for show: given a symbol, converts modules to a string of
-- 1 and 0. Note otherwise case is excluded because 
-- well formed symbol must be list of bools 
symboltostring :: Symbol -> String
symboltostring lst = symboltostring' lst ""
  where
    symboltostring' [] str = str
    symboltostring' (x:xs) str
      | x     = symboltostring' xs str ++ ['1']
      | not x = symboltostring' xs str ++ ['0']


instance Show BCString where
  show :: BCString -> String
  show (BCString symbols) =
    foldl (\acc symbol -> acc ++ symboltostring symbol) "" symbols ++ "11"


instance Read BCString where
  readsPrec :: Int -> String -> [(BCString, String)]
  readsPrec _ str =
    case maybeReadBCString str of
      Just bcString -> [(bcString, "")]
      Nothing       -> []


maybeReadBCString :: String -> Maybe BCString
maybeReadBCString "" = Nothing
maybeReadBCString str = maybeReadBCString' str [] []
  where
    maybeReadBCString' "" lst final
      | lst == [True,True] = Just (BCString (reverse final))
      | otherwise          = Nothing
    maybeReadBCString' (x:xs) lst final
      | length lst == 11  = maybeReadBCString' (x:xs) [] (lst : final)
      | x == '1'          = maybeReadBCString' xs ( True : lst) final
      | x == '0'          = maybeReadBCString' xs ( False : lst) final
      | otherwise         = Nothing

--------------------------------------------------------------------------------

run :: (TheCodes -> a -> Either Error b) -> a -> IO (Either Error b)
run f a = do
  theCodes <- loadTheCodes
  pure $ f theCodes a


--------------------------------------------------------------------------------
-- User Interface

bcToBCString :: TheCodes -> BC -> BCString
bcToBCString theCodes (start, data_, checksum, stop) =
  let symbolIds = [start] ++ data_ ++ [checksum, stop] in
  BCString $ map (\i -> (idsToSymbols theCodes) ! i) symbolIds


bcStringToBC :: TheCodes -> BCString -> BC
bcStringToBC theCodes (BCString symbols) =
  (start, data_, checksum, stop)
    where
      list     = map (\symbol -> (symbolsToIds theCodes) ! symbol) symbols
      start    = head list
      stop     = head $ reverse list
      checksum = head $ tail $ reverse list
      data_    = reverse $ tail $ tail $ reverse $ tail list

encodeAndShow
  :: (TheCodes -> String -> Either Error BC) -> TheCodes -> String
  -> Either Error String
encodeAndShow f theCodes str =
  show . bcToBCString theCodes <$> f theCodes str

readAndDecode
  :: (TheCodes -> BC -> Either Error String) -> TheCodes -> String
  -> Either Error String
readAndDecode f theCodes str =
  -- Call `maybeReadBCString` instead of `read` because the latter may crash
  case maybeReadBCString str of
    Nothing       -> Left "no parse"
    Just bcString ->
      let barcode = bcStringToBC theCodes $ bcString in
      f theCodes barcode

runEncoder
  :: (TheCodes -> String -> Either Error BC) -> String
  -> IO ()
runEncoder f str = do
  theCodes <- loadTheCodes
  case encodeAndShow f theCodes str of
    Left error -> die error
    Right s    -> putStrLn s

runDecoder
  :: (TheCodes -> BC -> Either Error String) -> String
  -> IO ()
runDecoder f str = do
  theCodes <- loadTheCodes
  case readAndDecode f theCodes str of
    Left error -> die error
    Right str  -> putStrLn str

main = do
  args <- getArgs
  case args of
    ["encodeB", str] -> runEncoder encodeB str
    ["decodeB", str] -> runDecoder decodeB str
    ["encodeC", str] -> runEncoder encodeC str
    ["decodeC", str] -> runDecoder decodeC str
    _                -> die "Usage: cabal run code128 -- {en,de}code{B,C} string"
                         -- "Usage: ./Code128 {en,de}code{B,C} string"
