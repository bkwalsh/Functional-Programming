{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Tester where

import Control.Monad
import Code128
import Data.List

data Test a b = -- data, instead of type, for better error messages
  Test
    { funcName :: String
    , func     :: a -> Either Error b
    , input    :: a
    , output   :: Either () b -- () because we ignore the actual Error strings
    }

main :: IO ()
main = do
  theCodes <- loadTheCodes
  runTests $ tests_encodeB theCodes
  runTests $ tests_encodeC theCodes
  runTests $ tests_decodeB theCodes
  runTests $ tests_decodeC theCodes
  runTests $ tests_encodeAndShow theCodes
  runTests $ tests_readAndDecode theCodes

runTests :: (Show a, Eq b) => [Test a b] -> IO ()
runTests tests = do
  putStrLn ""
  forM_ tests $ \(Test funcName func input output) ->
    let                -------------------------- shadowing the record selectors
      strInput =
        show input
      strInput'
        | length strInput < 40 = strInput
        | otherwise            = take 40 strInput ++ "..."
      success b =
        putStrLn $ displayColor (if b then Green else Red) $
          intercalate " " [if b then "PASS" else "FAIL", funcName, strInput']
    in
    case (func input, output) of
      (Left _, Left ())   -> success True
      (Right b, Right b') -> if b == b' then success True else success False
      _                   -> success False

--------------------------------------------------------------------------------

data Color = Red | Green deriving (Eq, Ord)

displayColor :: Color -> String -> String
displayColor color str =
  let
    -- https://en.wikipedia.org/wiki/ANSI_escape_code
    -- https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
    green  = "\ESC[32m"
    red    = "\ESC[31m"
    reset  = "\ESC[0m"
  in
    case color of
      Red   -> red ++ str ++ reset
      Green -> green ++ str ++ reset

--------------------------------------------------------------------------------

piString = "3141592653"

tests_encodeB :: TheCodes -> [Test String BC]
tests_encodeB theCodes =
  map (uncurry $ Test "encodeB theCodes" $ encodeB theCodes)
    [ ( "Haskell!"
      , Right (104,[40,65,83,75,69,76,76,1],1,106)
      )
    , ( "CMSC 22300? HOT Programming in Haskell!"
      , Right (104,[35,45,51,35,0,18,18,19,16,16,31,0,40,47,52,0,48,82,79,71,82,65,77,77,73,78,71,0,73,78,0,40,65,83,75,69,76,76,1],72,106)
      )
    , ( "Separation of ⛪ and 🏛"
      , Left ()
      )
    , ( "Wikipedia"
      , Right (104,[55,73,75,73,80,69,68,73,65],88,106)
      )
    , ( " , "
      , Right (104,[0,12,0],25,106)
      )
    , ( "~"
      , Right (104,[94],95,106)
      )
    , ( " "
      , Right (104,[0],1,106)
      )          
    , ( piString
      , Right (104,[19,17,20,17,21,25,18,22,21,19],88,106)
      )
    ]

tests_encodeC :: TheCodes -> [Test String BC]
tests_encodeC theCodes =
  map (uncurry $ Test "encodeC theCodes" $ encodeC theCodes)
    [ ( "Haskell!"
      , Left ()
      )
    , ( piString
      , Right (105,[31,41,59,26,53],43,106)
      )
    , ( "22300"
      , Left ()
      )
    , ( "223000"
      , Right (105,[22,30,0],84,106)
      )
    , ( "223000!"
      , Left ()
      )
    , ( "223000!!!"
      , Left ()
      )
    , ( "2"
      , Left ()
      )      
    ]

tests_decodeB :: TheCodes -> [Test BC String]
tests_decodeB theCodes =
  map (uncurry $ Test "decodeB theCodes" $ decodeB theCodes)
    [ ( (104,[40,65,83,75,69,76,76,1],1,106)
      , Right "Haskell!"
      )
    , ( (104,[40,65,83,75,69,76,76,1,1],10,106)
      , Right "Haskell!!"
      )
    , ( (104,[40,65,83,75,69,76,76,1,1],10,1)
      , Left ()
      )      
    , ( (105,[40,65,83,75,69,76,76,1,1],10,106)
      , Left ()
      )
    , ( (104,[19,17,20,17,21,25,18,22,21,19],88,106)
      , Right "3141592653"
      )
    , ( (103,[19,17,20,17,21,25,18,22,21,19],88,106)
      , Left ()
      )
    , ( (104,[55,73,75,73,80,69,68,73,65],88,106)
      , Right "Wikipedia"
      )
    , ( (104,[55,55,73,75,73,80,69,68,73,65],88,106)
      , Left ()
      )
    , ( (104,[19,17,20,17,21,25,18,22,21,19],20,106) -- bad checksum
      , Left ()
      )
    ]

tests_decodeC :: TheCodes -> [Test BC String]
tests_decodeC theCodes =
  map (uncurry $ Test "decodeC theCodes" $ decodeC theCodes)
    [ ( (105,[31,41,59,26,53],43,106)
      , Right "3141592653"
      )
    , ( (105,[31,41,59,26,53,11],6,1)
      , Left ()
      )        
    , ( (105,[31,41,59,26,53,11],6,106)
      , Right "314159265311"
      )  
    , ( (105,[31,41,59,26,53,11],6,105)
      , Left ()
      ) 
    , ( (103,[31,41,59,26,53,11],6,106)
      , Left ()
      )   
    , ( (104,[31,41,59,26,53,11],6,106)
      , Left ()
      )                
    , ( (105,[31,41,59,26,53],42,106) -- bad checksum
      , Left ()
      )
    , ( (0, [], 0, 0)
      , Left ()
      )
    ]

tests_encodeAndShow :: TheCodes -> [Test String String]
tests_encodeAndShow theCodes =
  map (uncurry $ Test "encodeAndShow encodeB" $ encodeAndShow encodeB theCodes)
    [ ( "Wikipedia"
      , Right "11010010000111010001101000011010011000010010100001101001010011110010110010000100001001101000011010010010110000111100100101100011101011"
      )
    , ( "Hello!"
      , Right "11010010000110001010001011001000011001010000110010100001000111101011001101100100100111101100011101011"
      )
    , ( "\"Hello!\""
        -- NOTE: To `cabal run` this test, need to escape as follows:
        -- % cabal run code128 -- encodeB \""Hello\!"\"
      , Right "110100100001100110011011000101000101100100001100101000011001010000100011110101100110110011001100110111001100101100011101011"
      )
    , ( "a"
      , Right "1101001000010010110000100100001101100011101011"
      )
    , ( "aaaaaa"
      , Right "11010010000100101100001001011000010010110000100101100001001011000010010110000111011001001100011101011"
      )
    , ( "ohboy"
      , Right "110100100001000111101010011000010100100001101000111101011011011110110001011101100011101011"
      )              
    ,( "3141592653"
      , Right "1101001000011001011100100111001101100100111010011100110110111001001110010110011001110010110011101001101110010011001011100111100100101100011101011"
      )    
    ,( "⛪ and 🏛"
      , Left ()
      )          
    ]

tests_readAndDecode :: TheCodes -> [Test String String]
tests_readAndDecode theCodes =
  map (uncurry $ Test "readAndDecode decodeB" $ readAndDecode decodeB theCodes)
    [
     ( "110100100001110100011010000110100110000100101000011010010100111100101100100001000010011010000110100100101100001111001001011000111010" -- missing final bar
      , Left ()
      )
    , ( ""
      , Left ()
      ),
     ( "110100100001001011000010010000110110001110101"
      , Left ()
      ),  
    ( "11010010000111010001101000011010011000010010100001101001010011110010110010000100001001101000011010010010110000111100100101100011101011"
     , Right "Wikipedia"
      ),
     ( "1101001000010010110000100100001101100011101011"
      , Right "a"
      ),
     ( "11010010000100101100001001011000010010110000100101100001001011000010010110000111011001001100011101011"
      , Right "aaaaaa"
      )
    , ( "110100100001000111101010011000010100100001101000111101011011011110110001011101100011101011"
      , Right "ohboy"
      )          
    ]
