----------------------------------------------------------------------------
-- | Klondike Solitaire — a miso application
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso hiding ((!!))     -- Miso redefines (!!) as a JS array accessor
import qualified Miso.Html as H
import           Miso.CSS hiding (ms)   -- 'ms' would shadow Miso.String.ms
import           Miso.FFI.QQ (js)
import           Miso.Lens
import           Miso.Reload
import           Miso.Random (replicateRM)
import           Data.List   (sortBy, (!!))
import           Data.Maybe  (mapMaybe, listToMaybe)
import           Data.Ord    (comparing)
----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Show, Eq, Ord, Enum, Bounded)

data Rank
  = Ace | Two | Three | Four | Five | Six | Seven
  | Eight | Nine | Ten | Jack | Queen | King
  deriving (Show, Eq, Ord, Enum, Bounded)

data Card = Card
  { cardRank :: Rank
  , cardSuit :: Suit
  } deriving (Show, Eq)

-- | A card in the tableau – face-up (visible) or face-down (hidden)
data FaceCard
  = FaceUp   Card
  | FaceDown Card
  deriving (Show, Eq)

-- | Where a selected group of cards came from
data Src
  = SrcWaste
  | SrcFoundation Int
  | SrcTableau    Int
  deriving (Show, Eq)

-- | Application state
--   All pile lists: head = top of pile (most recently placed / accessible)
data Model = Model
  { _stock       :: [Card]         -- draw pile (face-down)
  , _waste       :: [Card]         -- discard pile; head = visible top
  , _foundations :: [[Card]]       -- 4 foundation piles
  , _tableau     :: [[FaceCard]]   -- 7 tableau columns
  , _selected    :: Maybe (Src, [Card])  -- currently held cards
  , _gameWon     :: Bool
  , _moves       :: Int
  } deriving (Show, Eq)
----------------------------------------------------------------------------
-- Lenses
----------------------------------------------------------------------------
stockL :: Lens Model [Card]
stockL = lens _stock (\m x -> m { _stock = x })

wasteL :: Lens Model [Card]
wasteL = lens _waste (\m x -> m { _waste = x })

foundationsL :: Lens Model [[Card]]
foundationsL = lens _foundations (\m x -> m { _foundations = x })

tableauL :: Lens Model [[FaceCard]]
tableauL = lens _tableau (\m x -> m { _tableau = x })

selectedL :: Lens Model (Maybe (Src, [Card]))
selectedL = lens _selected (\m x -> m { _selected = x })

gameWonL :: Lens Model Bool
gameWonL = lens _gameWon (\m x -> m { _gameWon = x })

movesL :: Lens Model Int
movesL = lens _moves (\m x -> m { _moves = x })
----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------
data Action
  = Init                       -- start / restart game
  | Shuffled [Card]            -- shuffled deck delivered by IO
  | ClickStock                 -- draw from stock (or reset if empty)
  | ClickWaste                 -- select top of waste
  | ClickFoundation Int        -- click foundation pile i
  | ClickTableauCard Int Int   -- click tableau col i, card j from top (0=top)
  | ClickTableauCol  Int       -- click empty area / face-down card in col i
  | AutoComplete               -- move one card to a foundation (recursive)
  | ToggleFullscreen           -- toggle browser fullscreen
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- Entry point
----------------------------------------------------------------------------
main :: IO ()
#ifdef INTERACTIVE
main = live defaultEvents app
#else
main = startApp defaultEvents app
#endif
----------------------------------------------------------------------------
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
----------------------------------------------------------------------------
-- | Global CSS injected into <head> – handles pseudo-classes and keyframes
globalCSS :: MisoString
globalCSS = mconcat
  [ ".sol-card { transition: box-shadow 0.2s ease, transform 0.15s ease; } "
  , ".sol-card:hover { transform: translateY(-3px); } "
  , ".sol-col { transition: height 0.3s ease; } "
  , "@keyframes sol-win-in {"
  , "  from { opacity:0; transform: scale(0.85) translateY(20px); }"
  , "  to   { opacity:1; transform: scale(1) translateY(0); }"
  , "} "
  , ".sol-win { animation: sol-win-in 0.35s ease; } "
  , "* { -webkit-tap-highlight-color: transparent; } "
  , "button, .sol-card { touch-action: manipulation; } "
  , ".sol-board { transform-origin: top left; } "
  , "@media (max-width: 639px) { .sol-board { zoom: 0.85; } } "
  , "@media (max-width: 499px) { .sol-board { zoom: 0.73; } } "
  , "@media (max-width: 430px) { .sol-board { zoom: 0.64; } } "
  , "@media (max-width: 375px) { .sol-board { zoom: 0.58; } } "
  , "@media (max-width: 340px) { .sol-board { zoom: 0.52; } } "
  ]
----------------------------------------------------------------------------
app :: App Model Action
app = (component emptyModel updateModel viewModel)
        { mount = Just Init
        , styles = [Style globalCSS]
        }

emptyModel :: Model
emptyModel = Model [] [] (replicate 4 []) (replicate 7 []) Nothing False 0
----------------------------------------------------------------------------
-- Deck helpers
----------------------------------------------------------------------------
fullDeck :: [Card]
fullDeck = [ Card r s | s <- [minBound..maxBound], r <- [minBound..maxBound] ]

shuffleWith :: [Double] -> [Card] -> [Card]
shuffleWith rs = map snd . sortBy (comparing fst) . zip rs

isRed :: Card -> Bool
isRed c = cardSuit c == Hearts || cardSuit c == Diamonds

rankVal :: Rank -> Int
rankVal = fromEnum  -- Ace=0 .. King=12

getCard :: FaceCard -> Card
getCard (FaceUp   c) = c
getCard (FaceDown c) = c

isFaceUp :: FaceCard -> Bool
isFaceUp (FaceUp _) = True
isFaceUp _          = False

-- | Replace element at index i using a function
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f = zipWith (\j x -> if j == i then f x else x) [0..]

-- | After removing cards from a tableau column, flip a face-down top if needed
flipTop :: [FaceCard] -> [FaceCard]
flipTop (FaceDown c : rest) = FaceUp c : rest
flipTop col                 = col

-- | Split a deck into groups of increasing size: [1], [2], …, [n]
splitGroups :: [a] -> [[a]]
splitGroups xs = take 7 (go 1 xs)
  where
    go _ [] = []
    go n ys = take n ys : go (n + 1) (drop n ys)

-- | Build one tableau column; last card is face-up, rest face-down.
--   head = top = face-up card.
dealCol :: [Card] -> [FaceCard]
dealCol []  = []
dealCol [c] = [FaceUp c]
dealCol cs  = FaceUp (last cs) : map FaceDown (reverse (init cs))

-- | Deal a shuffled deck into the initial game state.
dealInitial :: [Card] -> Model
dealInitial deck = emptyModel
  { _stock   = drop 28 deck
  , _tableau = map dealCol (splitGroups deck)
  }
----------------------------------------------------------------------------
-- Game rules
----------------------------------------------------------------------------
-- | Can we place a run of cards on top of a tableau column?
canPlaceTableau :: [Card] -> [FaceCard] -> Bool
canPlaceTableau []    _                = True
canPlaceTableau cards []               = cardRank (last cards) == King
canPlaceTableau cards (FaceDown _ : _) = False
canPlaceTableau cards (FaceUp t   : _) =
  isRed (last cards) /= isRed t &&
  rankVal (cardRank (last cards)) + 1 == rankVal (cardRank t)

-- | Can we place a single card on a foundation pile?
canPlaceFoundation :: Card -> [Card] -> Bool
canPlaceFoundation c []    = cardRank c == Ace
canPlaceFoundation c (t:_) =
  cardSuit c == cardSuit t &&
  rankVal (cardRank c) == rankVal (cardRank t) + 1

-- | Remove n cards from the source pile; flip new tableau top if face-down.
removeFrom :: Src -> Int -> Model -> Model
removeFrom src n m = case src of
  SrcWaste        -> m { _waste       = drop n (_waste m) }
  SrcFoundation i -> m { _foundations = updateAt i (drop 1) (_foundations m) }
  SrcTableau    i -> m { _tableau     = updateAt i (flipTop . drop n) (_tableau m) }

-- | Place a single card on foundation i.
placeOnFoundation :: Int -> Card -> Model -> Model
placeOnFoundation i c m =
  let newF = updateAt i (c :) (_foundations m)
  in m { _foundations = newF
       , _moves       = _moves m + 1
       , _gameWon     = all ((== 13) . length) newF
       }

-- | Place a run of cards (face-up) on tableau column i.
placeOnTableau :: Int -> [Card] -> Model -> Model
placeOnTableau i cards m =
  m { _tableau = updateAt i (map FaceUp cards ++) (_tableau m)
    , _moves   = _moves m + 1
    }

-- | Find which foundation pile a card can be placed on.
findFoundationFor :: Card -> [[Card]] -> Maybe Int
findFoundationFor c founds =
  listToMaybe [ i | (i, f) <- zip [0..] founds, canPlaceFoundation c f ]

-- | Try to move one card automatically to a foundation (waste or tableau tops).
tryAutoMove :: Model -> Maybe Model
tryAutoMove m = listToMaybe $ mapMaybe id $
  tryFrom SrcWaste (take 1 (_waste m))
  : [ tryFrom (SrcTableau i) (faceUpTop (_tableau m !! i)) | i <- [0..6] ]
  where
    faceUpTop (FaceUp c : _) = [c]
    faceUpTop _              = []
    tryFrom src [c] =
      case findFoundationFor c (_foundations m) of
        Nothing -> Nothing
        Just fi ->
          Just (placeOnFoundation fi c (removeFrom src 1 m))
            { _selected = Nothing }
    tryFrom _ _ = Nothing
----------------------------------------------------------------------------
-- Update
----------------------------------------------------------------------------
updateModel :: Action -> Effect parent props Model Action
updateModel = \case

  Init -> io $ do
    rs <- replicateRM 52
    pure (Shuffled (shuffleWith rs fullDeck))

  Shuffled deck ->
    put (dealInitial deck)

  ClickStock -> do
    m <- get
    let m0 = m { _selected = Nothing }
    case _stock m0 of
      []     -> put m0 { _stock = reverse (_waste m0), _waste = [] }
      (c:cs) -> put m0 { _stock = cs, _waste = c : _waste m0 }

  ClickWaste -> do
    m <- get
    case _waste m of
      []    -> pure ()
      (c:_) -> case _selected m of
        Just (SrcWaste, _) -> put m { _selected = Nothing }
        _                  -> put m { _selected = Just (SrcWaste, [c]) }

  ClickFoundation fi -> do
    m <- get
    let found = _foundations m !! fi
    case _selected m of
      Just (SrcFoundation si, _) | si == fi ->
        put m { _selected = Nothing }
      Just (src, [c]) | canPlaceFoundation c found -> do
        let m' = removeFrom src 1 m
        put (placeOnFoundation fi c m') { _selected = Nothing }
      Just _ ->
        case found of
          []    -> put m { _selected = Nothing }
          (c:_) -> put m { _selected = Just (SrcFoundation fi, [c]) }
      Nothing ->
        case found of
          []    -> pure ()
          (c:_) -> put m { _selected = Just (SrcFoundation fi, [c]) }

  ClickTableauCard ci cardFromTop -> do
    m <- get
    let col = _tableau m !! ci
    case _selected m of
      Just (SrcTableau si, _) | si == ci ->
        selectRun m ci col cardFromTop
      Just (src, cards) | canPlaceTableau cards col -> do
        let m' = removeFrom src (length cards) m
        put (placeOnTableau ci cards m') { _selected = Nothing }
      _ ->
        selectRun m ci col cardFromTop

  ClickTableauCol ci -> do
    m <- get
    let col = _tableau m !! ci
    case _selected m of
      Just (SrcTableau si, _) | si == ci ->
        put m { _selected = Nothing }
      Just (src, cards) | canPlaceTableau cards col -> do
        let m' = removeFrom src (length cards) m
        put (placeOnTableau ci cards m') { _selected = Nothing }
      _ ->
        put m { _selected = Nothing }

  AutoComplete -> do
    m <- get
    case tryAutoMove m of
      Nothing -> pure ()
      Just m' -> put m' >> issue AutoComplete

  ToggleFullscreen -> io_
    [js| document.fullscreenElement ? document.exitFullscreen() : document.documentElement.requestFullscreen() |]

-- | Try to select the face-up run starting at cardFromTop in col ci.
selectRun :: Model -> Int -> [FaceCard] -> Int -> Effect parent props Model Action
selectRun m ci col cardFromTop =
  let slice = take (cardFromTop + 1) col
  in if not (null slice) && all isFaceUp slice
     then put m { _selected = Just (SrcTableau ci, map getCard slice) }
     else put m { _selected = Nothing }
----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------
viewModel :: props -> Model -> View Model Action
viewModel _ m =
  H.div_
    [ style_
        [ backgroundColor (hex "076324")
        , minHeight (vh 100.0)
        , fontFamily "Arial,Helvetica,sans-serif"
        , "user-select" =: "none"
        ]
    ]
    [ H.div_
        [ classList ["sol-board"]
        , style_
            [ padding (px 20)
            , boxSizing "border-box"
            ]
        ]
        [ viewHeader m
        , viewTopRow m
        , viewTableau m
        ]
    , if _gameWon m then viewWinOverlay else vfrag []
    ]

-- | Header: title, buttons, move counter
viewHeader :: Model -> View Model Action
viewHeader m =
  H.div_
    [ style_
        [ display "flex"
        , alignItems "center"
        , "flex-wrap" =: "wrap"
        , gap (px 12)
        , marginBottom (px 16)
        ]
    ]
    [ H.h2_
        [ style_ [ color white, margin "0", fontSize (px 20) ] ]
        [ text "\x2660 Solitaire" ]
    , actionBtn "New Game"      Init
    , actionBtn "Auto Complete" AutoComplete
    , actionBtn "\x26F6"        ToggleFullscreen
    , H.span_
        [ style_
            [ color (hex "c6f6d5")
            , fontSize (px 14)
            , marginLeft (px 8)
            ]
        ]
        [ text ("Moves: " <> ms (_moves m)) ]
    ]
  where
    actionBtn lbl act =
      H.button_
        [ H.onClick act
        , style_
            [ padding "6px 14px"
            , backgroundColor (hex "1a5c33")
            , color white
            , border "1px solid #2a8a55"
            , borderRadius (px 5)
            , cursor "pointer"
            , fontSize (px 13)
            ]
        ]
        [ text lbl ]

-- | Top row: stock | waste | spacer | 4 foundations
viewTopRow :: Model -> View Model Action
viewTopRow m =
  H.div_
    [ style_ [ display "flex", gap (px 10), marginBottom (px 16) ] ]
    [ viewStock m
    , viewWaste m
    , emptySlot []
    , viewFoundation 0 m
    , viewFoundation 1 m
    , viewFoundation 2 m
    , viewFoundation 3 m
    ]

-- | Stock pile
viewStock :: Model -> View Model Action
viewStock m = case _stock m of
  [] ->
    emptySlot
      [ H.onClick ClickStock
      , style_ [ cursor "pointer" ]
      ]
  _ ->
    H.div_
      [ H.onClick ClickStock
      , style_ (cardBackStyles ++ [ cursor "pointer" ])
      ]
      [ H.div_
          [ style_
              [ color (RGBA 255 255 255 0.55)
              , fontSize (px 11)
              , textAlign "center"
              , paddingTop (px 42)
              ]
          ]
          [ text (ms (length (_stock m))) ]
      ]

-- | Waste pile – fans up to 3 cards horizontally.
--   Oldest card sits at left=0; the newest (top/playable) card is at the right.
--   Only the top card has a click handler and the selection highlight.
viewWaste :: Model -> View Model Action
viewWaste m =
  let isSel   = isSrcSelected SrcWaste m
      visible = take 3 (_waste m)          -- [newest, 2nd, 3rd] (head = top)
      n       = length visible
  in H.div_
       [ style_ [ position "relative", width (px wasteW), height (px cardH) ] ]
       $ case visible of
           [] -> [ emptySlot [] ]
           _  ->
             [ let leftOff = (n - 1 - idx) * wasteOverlap
                   isTop   = idx == 0
                   selFlag = isTop && isSel
                   evts    = [ H.onClick ClickWaste | isTop ]
               in H.div_
                    ( classList ["sol-card"]
                    : style_ ( faceUpStyles c selFlag
                             ++ [ position "absolute"
                                , left (px leftOff)
                                , zIndex (ms (n - idx))
                                ] )
                    : evts
                    )
                    (cardContent c selFlag)
             | (idx, c) <- zip [0..] visible
             ]

-- | One foundation pile
viewFoundation :: Int -> Model -> View Model Action
viewFoundation fi m =
  let found = _foundations m !! fi
      isSel = isSrcSelected (SrcFoundation fi) m
  in case found of
    [] ->
      H.div_
        [ H.onClick (ClickFoundation fi)
        , style_ (emptySlotStyles ++ [ cursor "pointer" ])
        ]
        [ H.div_
            [ style_
                [ color (hex "4a9e6a")
                , fontSize (px 18)
                , fontWeight "bold"
                , textAlign "center"
                , lineHeight (px 96)
                ]
            ]
            [ text "A" ]
        ]
    (c:_) -> viewFaceCard c isSel [ H.onClick (ClickFoundation fi) ]

-- | Seven tableau columns
viewTableau :: Model -> View Model Action
viewTableau m =
  H.div_
    [ style_ [ display "flex", gap (px 10), alignItems "flex-start" ] ]
    [ viewTableauCol ci m | ci <- [0..6] ]

-- | One tableau column with absolutely-positioned stacked cards
viewTableauCol :: Int -> Model -> View Model Action
viewTableauCol ci m =
  let col     = _tableau m !! ci
      revCol  = reverse col          -- bottom card rendered first
      offsets = scanl (+) 0 (map faceOffset revCol)
      colH    = if null col then slotH else last offsets + cardH
  in H.div_
      [ classList ["sol-col"]
      , style_ [ position "relative", width (px cardW), height (px colH) ]
      ]
      $  emptySlot [ H.onClick (ClickTableauCol ci) ]
      :  [ renderTabCard ci m revCol topOff ri fc
         | (topOff, ri, fc) <- zip3 offsets [0..] revCol
         ]
  where
    faceOffset (FaceDown _) = 20
    faceOffset (FaceUp _)   = 28

-- | Render one tableau card at a given top offset
renderTabCard :: Int -> Model -> [FaceCard] -> Int -> Int -> FaceCard -> View Model Action
renderTabCard ci m revCol topOff ri fc =
  let cardFromTop = length revCol - 1 - ri
      isSel       = isTabSelected ci cardFromTop m
  in case fc of
    FaceDown _ ->
      H.div_
        [ H.onClick (ClickTableauCol ci)
        , style_ (cardBackStyles ++ [ position "absolute", top (px topOff) ])
        ]
        []
    FaceUp c ->
      H.div_
        [ classList ["sol-card"]
        , H.onClick (ClickTableauCard ci cardFromTop)
        , style_ (faceUpStyles c isSel ++ [ position "absolute", top (px topOff) ])
        ]
        (cardContent c isSel)

-- | Is a tableau card within the currently selected run?
isTabSelected :: Int -> Int -> Model -> Bool
isTabSelected ci cardFromTop m = case _selected m of
  Just (SrcTableau si, cards) -> si == ci && cardFromTop < length cards
  _                           -> False

-- | Is a given source the current selection origin?
isSrcSelected :: Src -> Model -> Bool
isSrcSelected src m = case _selected m of
  Just (s, _) -> s == src
  Nothing     -> False
----------------------------------------------------------------------------
-- Card rendering
----------------------------------------------------------------------------
cardW, cardH, slotH :: Int
cardW = 70
cardH = 100
slotH = 120

-- | Horizontal overlap between fanned waste cards
wasteOverlap :: Int
wasteOverlap = 16

-- | Fixed width of the waste area (room for up to 3 fanned cards)
wasteW :: Int
wasteW = cardW + 2 * wasteOverlap   -- 102 px

-- | Render a face-up card with optional selection highlight
viewFaceCard :: Card -> Bool -> [Attribute Action] -> View Model Action
viewFaceCard c isSel attrs =
  H.div_ (classList ["sol-card"] : style_ (faceUpStyles c isSel) : attrs) (cardContent c isSel)

-- | Inner content of a face-up card (top-left corner, centre, bottom-right)
cardContent :: Card -> Bool -> [View Model Action]
cardContent c _ =
  let col = if isRed c then hex "dc2626" else hex "111827"
  in [ H.div_
         [ style_
             [ position "absolute"
             , top (px 4), left (px 6)
             , fontSize (px 12), fontWeight "bold"
             , color col, lineHeight "1.3"
             ]
         ]
         [ text (rankStr (cardRank c)), H.br_ [], text (suitStr (cardSuit c)) ]
     , H.div_
         [ style_
             [ position "absolute"
             , top (pct 50.0), left (pct 50.0)
             , transform "translate(-50%,-50%)"
             , fontSize (px 26), color col
             ]
         ]
         [ text (suitStr (cardSuit c)) ]
     , H.div_
         [ style_
             [ position "absolute"
             , bottom (px 4), right (px 6)
             , fontSize (px 12), fontWeight "bold"
             , color col, lineHeight "1.3"
             , transform "rotate(180deg)"
             ]
         ]
         [ text (rankStr (cardRank c)), H.br_ [], text (suitStr (cardSuit c)) ]
     ]

-- | Styles for a face-up card (selected or not)
faceUpStyles :: Card -> Bool -> [Style]
faceUpStyles _ isSel =
  [ width (px cardW), height (px cardH)
  , borderRadius (px 8)
  , backgroundColor white
  , border "2px solid #ccc"
  , position "relative"
  , cursor "pointer"
  , transition "box-shadow 0.2s ease, transform 0.15s ease"
  , boxShadow (if isSel
      then "0 0 0 3px #fde047, 0 0 0 6px #f59e0b"
      else "0 2px 4px rgba(0,0,0,0.3)")
  ]

-- | Styles for a face-down card
cardBackStyles :: [Style]
cardBackStyles =
  [ width (px cardW), height (px cardH)
  , borderRadius (px 8)
  , border "2px solid #1e3a8a"
  , background "repeating-linear-gradient(45deg,#1e40af,#1e40af 5px,#2563eb 5px,#2563eb 10px)"
  , position "relative"
  ]

-- | Styles for an empty placeholder slot
emptySlotStyles :: [Style]
emptySlotStyles =
  [ width (px cardW), height (px cardH)
  , borderRadius (px 8)
  , border "2px dashed #4a9e6a"
  , background "rgba(0,0,0,0.12)"
  , position "relative"
  ]

-- | An empty placeholder slot element
emptySlot :: [Attribute Action] -> View Model Action
emptySlot attrs = H.div_ (style_ emptySlotStyles : attrs) []

-- | Rank as display string
rankStr :: Rank -> MisoString
rankStr = \case
  Ace   -> "A";  Two   -> "2";  Three -> "3"
  Four  -> "4";  Five  -> "5";  Six   -> "6"
  Seven -> "7";  Eight -> "8";  Nine  -> "9"
  Ten   -> "10"; Jack  -> "J";  Queen -> "Q"; King -> "K"

-- | Suit unicode symbol
suitStr :: Suit -> MisoString
suitStr = \case
  Clubs    -> "\x2663"    -- ♣
  Diamonds -> "\x2666"    -- ♦
  Hearts   -> "\x2665"    -- ♥
  Spades   -> "\x2660"    -- ♠

-- | Win overlay
viewWinOverlay :: View Model Action
viewWinOverlay =
  H.div_
    [ style_
        [ position "fixed"
        , top (px 0), left (px 0)
        , width (pct 100.0), height (pct 100.0)
        , background "rgba(0,0,0,0.75)"
        , display "flex"
        , alignItems "center"
        , justifyContent "center"
        , zIndex "999"
        ]
    ]
    [ H.div_
        [ classList ["sol-win"]
        , style_
            [ backgroundColor white
            , padding "48px 56px"
            , borderRadius (px 16)
            , textAlign "center"
            , boxShadow "0 8px 32px rgba(0,0,0,0.4)"
            ]
        ]
        [ H.h1_
            [ style_
                [ color (hex "076324")
                , margin "0 0 12px"
                , fontSize (px 32)
                ]
            ]
            [ text "\x1F389 You Win!" ]
        , H.p_
            [ style_ [ color (hex "555555"), margin "0 0 24px" ] ]
            [ text "Congratulations! You completed the game." ]
        , H.button_
            [ H.onClick Init
            , style_
                [ padding "12px 28px"
                , backgroundColor (hex "076324")
                , color white
                , border "none"
                , borderRadius (px 8)
                , cursor "pointer"
                , fontSize (px 16)
                , fontWeight "bold"
                ]
            ]
            [ text "Play Again" ]
        ]
    ]
----------------------------------------------------------------------------
