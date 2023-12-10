{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Redundant if" #-}
module HipBarcodes where

-- You may add, remove, or edit imports as you wish. Note the use of
-- qualified imports to avoid collisions between common names. For
-- example, Prelude and Data.Map and Graphics.Image all define `map`.

import           Code128
import           Data.Char
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Graphics.Image (Image, Pixel(..), RGB, VU(VU))
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface (MArray)
import qualified Graphics.Image as Image
import           System.Environment
import           System.Exit





--------------------------------------------------------------------------------
-- NOTE: Defining these at top-level rather than changing Code128.TheCodes
-- to include them, like the start and stop symbols.

bToC, cToB :: SymbolId
bToC = 99
cToB = 100


--------------------------------------------------------------------------------
-- 1. General Decoding


-- gets starting code_state char based on the SymbolId
code_state :: SymbolId -> Char
code_state codeid
  | codeid == 104 = 'B'
  | codeid == 105 = 'C'



decode :: TheCodes -> BC -> Either Error String
decode theCodes (start_, data_, checksum_, stop_)
  | (start_ /= 104 && start_ /= 105) || stop_ /= 106  = Left "format error"
  | checksum_ /= computeChecksum start_ data_  = Left ("bad checksum_: "++ show checksum_)
  | otherwise = decode' (code_state start_) data_ ""
      where
      decode' state [] lst = Right lst
      decode' state (x:xs) lst =
          case (x, state) of
            (100, state) ->
              if state=='B' then Left "format error" else  decode' 'B' xs lst
            (99, state) ->
              if state=='C' then Left "format error" else  decode' 'C' xs lst
            (x,'B') -> case Map.lookup x (bDecodings theCodes) of
              Nothing           -> Left "unsupported characters"
              Just (Left c)     -> decode' state xs (lst ++ [c])
              Just (Right str)  -> decode' state xs (lst ++ str)
            (x,'C') -> case Map.lookup x (cDecodings theCodes) of
              Nothing           -> Left "unsupported characters"
              Just (Left (c1,c2)) -> decode' state xs ((lst ++ [c1]) ++ [c2])
              Just (Right str)  -> decode' state xs (lst ++ str)



--------------------------------------------------------------------------------
-- 2. Optimal Encodings

encode :: TheCodes -> String -> Either Error BC
encode theCodes str = recurs str [] (getStartCode str)
  where
    -- abstract out to simplify nothing case 
    processEither e fn = case e of
      Just val -> fn val
      Nothing -> Left "error"

    recurs [] build startCode =
      Right (startCode, build, computeChecksum startCode build, 106)

    recurs curr build startCode
      | not (null charstart) =
        if length abridgedstart >= 4 && even (length abridgedstart) 
          then processEither (processBSAFE theCodes charstart) $ \result -> recurs charend (build ++ result ++ [99]) startCode
        else processEither (processBSAFE theCodes charstart) $ \result ->  recurs charend (build ++ result) startCode

      | (end == "" && length start >= 4) || (length start >= 4 && null build)
       || (length start >= 6 ) ||  (end == "" && null build &&  length start == 2) = processSection start end

      | otherwise = processEither (processBSAFE theCodes start) $ \result -> recurs end (build ++ result) startCode
      where
        (charstart, charend) = break isDigit curr
        (abridgedstart, _) = span isDigit charend
        (start, end) = span isDigit curr
        
        processSection start end  =
          if even (length start) then
            if end == ""
              then processEither (processCSAFE theCodes start) $ \result -> recurs [] (build ++ result) startCode
              else processEither (processCSAFE theCodes start) $ \result -> recurs end (build ++ result ++ [100]) startCode
          else
                    let (prefix, suffix) =
                            if not (null build && end /= "")
                              then splitAt 1 start
                              else (b,a) where (a,b)= splitAt (length start - 1) start
            in
            processEither (processBSAFE theCodes prefix) $ \result ->
              processEither (processCSAFE theCodes suffix) $ \resultC ->
                let output
                      | end == "" && (length start >= 4) = build ++ result ++ [99] ++ resultC
                      | length start >= 4 && null build = build ++ resultC ++ [100] ++ result
                      | otherwise = build ++ result ++ [99] ++ resultC ++ [100]
                in
                recurs end output startCode


-- Gets optimal startcode based on string   
getStartCode :: String -> Int
getStartCode str=
  let (start,end) = span isDigit str
      ls=length start  in
  if (ls >= 4 || end == "" && ls == 2) && even ls || (ls >= 4 && end /= "") && odd ls
    then 105 else 104


-- process all of a string in B with safe lookup 
processBSAFE :: TheCodes -> String -> Maybe [SymbolId]
processBSAFE theCodes str= processBSAFE' str []
  where
    processBSAFE' [] build= Just build
    processBSAFE' (x:xs) build=
        case Map.lookup x (bEncodings theCodes) of
          Nothing     -> Nothing
          Just value  -> processBSAFE' xs (build ++ [value])


-- process all of a string in C with safe lookup 
processCSAFE :: TheCodes -> String -> Maybe [SymbolId]
processCSAFE theCodes str= processCSAFE' str []
  where
    processCSAFE' [] build= Just build
    processCSAFE' (x:xx:xs) build=
        case Map.lookup (x,xx) (cEncodings theCodes) of
          Nothing     -> Nothing
          Just value  -> processCSAFE' xs (build ++ [value])


--------------------------------------------------------------------------------
-- 3. Making Barcodes

makeBarcode :: FilePath -> Int -> Int -> BCString -> IO ()
makeBarcode filePath imageHeight moduleWidth (BCString symbols) =  do
    let concatsyms = concatMap reverse symbols ++ [True,True]
    let grayscale :: Image.Image Image.VU Image.Y Double = Image.makeImageR Image.VU (imageHeight,moduleWidth * length concatsyms) (\(i, j) -> Image.PixelY 0.0)
    Image.writeImage filePath (barmods grayscale concatsyms (length concatsyms))
    where
        barmods imag [] varlen = imag
        barmods imag (True:xs) varlen = barmods imag xs varlen
        barmods imag (x:xs) varlen =
          let modifiedImage = Image.imap (\(x, y) pixel ->
                  if ((varlen - length xs - 1) * moduleWidth - 1 < y) &&
                    (y < (varlen - length xs) * moduleWidth )
                  then PixelY 1.0
                  else pixel) imag in
          barmods modifiedImage xs varlen


--------------------------------------------------------------------------------
-- 4. Scanning Barcodes

scanBarcode :: FilePath -> IO BCString
scanBarcode filePath = do
  img <- Image.readImageRGB VU filePath
  return (imageToBCString img)

imageToBCString :: (MArray arr cs e, ToY cs e) => Image arr cs e -> BCString
imageToBCString img =
  BCString (bcStringGen (getmoduleLen 0) 0 (Image.cols img) [] [])
  where
    getmoduleLen index=
      if isBlack (Image.index img (1, index))
        then getmoduleLen (index + 1)
        else index `div` 2
    bcStringGen modLen spot picLen lst build
      | modLen * spot >= picLen =
        reverse lst
      | length build == 11 =
        bcStringGen modLen spot picLen (build:lst) []
      | isBlack (Image.index img (1, modLen * spot)) =
        bcStringGen modLen (spot +1 ) picLen lst (True : build)
      | otherwise =
        bcStringGen modLen (spot + 1) picLen lst (False : build)


isBlack :: (ToY cs e) => Pixel cs e -> Bool
isBlack pixel =
  toPixelY pixel == 0.0


getModuleWidth :: [Bool] -> Int
getModuleWidth bools =
  undefined

takeEvery :: Int -> [a] -> [a]
takeEvery n xs =
  undefined


--------------------------------------------------------------------------------
-- 5. Scanning Designed Barcodes

scanDesignedBarcode :: FilePath -> IO BCString
scanDesignedBarcode filePath =
  undefined


--------------------------------------------------------------------------------
-- Main

runEncoder
  :: (TheCodes -> String -> Either Error BC) -> FilePath -> Int -> Int -> String
  -> IO ()
runEncoder f filePath height moduleWidth str = do
  theCodes <- loadTheCodes
  let result = bcToBCString theCodes <$> f theCodes str
  either (const (die "encoder failed")) printEncoding result
    where
      printEncoding bcString = do
        putStrLn $ "\nPayload:\n" ++ str
        putStrLn $ "\nEncoding:\n" ++ show bcString
        makeBarcode filePath height moduleWidth bcString

runDecoder
  :: (TheCodes -> BC -> Either Error String) -> String
  -> IO ()
runDecoder f filePath = do
  theCodes <- loadTheCodes
  bcString <- scanBarcode filePath
  let bc = bcStringToBC theCodes bcString
  either (const (die "decoder failed")) printDecoding (f theCodes bc)
    where
      printDecoding str = do
        putStrLn $ "\nDecoding:\n" ++ str
        putStrLn ""

main :: IO ()
main =
  getArgs >>= processArgs
  where
    processArgs ["encode", filePath, imageHeight, moduleWidth, str] =
      HipBarcodes.runEncoder
        encode filePath (read imageHeight) (read moduleWidth) str
    processArgs ["decode", file] =
      HipBarcodes.runDecoder decode file
    processArgs _ =
      die $ "\nUsage:\n\n"
        ++ "  cabal run hip-barcodes -- encode imageFilePath imageHeight moduleWidth string\n"
        ++ "  cabal run hip-barcodes -- decode imageFilePath\n"
