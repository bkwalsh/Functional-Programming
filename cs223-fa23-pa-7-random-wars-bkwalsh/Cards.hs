module Cards where

data CardValue
  = PipCard Int -- From 1 to 10    
  | Jack
  | Queen
  | King
      deriving (Show,Eq,Ord)


data Suit
  = Clubs 
  | Diamonds 
  | Hearts
  | Spades
      deriving (Show,Eq,Ord)


data AcesHigh = AcesHigh
data AcesLow  = AcesLow


data Card (a :: *) =
  Card CardValue Suit
    deriving (Eq)


instance Show (Card a) where
  show (Card value suit) = showValue value ++ showSuit suit
    where
      showValue (PipCard 1)  = "A"
      showValue (PipCard n)  = show n
      showValue Jack         = "J"
      showValue Queen        = "Q"
      showValue King         = "K"

      showSuit Clubs         = "♣"
      showSuit Diamonds      = "♦"
      showSuit Hearts        = "♥" -- "\x1B[31m♥\x1B[0m" -- red in terminal
      showSuit Spades        = "♠" -- "\x1B[31m♠\x1B[0m"

   {- Uncomment if you don't have Unicode:

      showSuit Clubs         = "C"
      showSuit Diamonds      = "D"
      showSuit Hearts        = "H"
      showSuit Spades        = "S"
   -}


--------------------------------------------------------------------------------
instance Ord (Card AcesHigh) where 
  (Card (PipCard 1) suita) <= (Card (PipCard 1) suitb) = suita <= suitb
  _ <= (Card (PipCard 1) _) = True 
  (Card (PipCard 1) _) <= _ = False  
  (Card a suita) <= (Card b suitb) = (a < b) || ((suita <= suitb) && (a == b))


instance Ord (Card AcesLow) where
  (Card a suita) <= (Card b suitb) = (a < b) || ((suita <= suitb) && (a == b))


--------------------------------------------------------------------------------

fullDeckAcesHigh :: [Card AcesHigh]
fullDeckAcesLow  :: [Card AcesLow]

fullDeckAcesHigh = fullDeck True
fullDeckAcesLow  = fullDeck False

-- helper function, if given a cardvalue return a list of that card in every suit, ordered
addSuits :: CardValue -> [Card a]
addSuits v = [Card v Clubs, Card v Diamonds, Card v Hearts, Card v Spades]

-- All 52 cards in ascending order, treating aces high (True) or low (False).
fullDeck :: Bool -> [Card a]
fullDeck flag = 
  if flag then fullDeck' flag (PipCard 1) [] else fullDeck' flag (PipCard 1) (addSuits (PipCard 1))
  where
    fullDeck' True King build = build ++ addSuits (PipCard 1)
    fullDeck' _ King build = build
    fullDeck' _ Queen build = fullDeck' flag King (build ++ addSuits King)
    fullDeck' _ Jack build = fullDeck' flag Queen (build ++ addSuits Queen)
    fullDeck' _ (PipCard 10) build = fullDeck' flag Jack (build ++ addSuits Jack)
    fullDeck' _ (PipCard x) build = fullDeck' flag (PipCard (x+1)) (build ++ addSuits (PipCard (x+1)))