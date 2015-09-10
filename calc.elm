import Signal
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Text exposing (fromString)
import Color exposing (Color, rgb, black, white)

type Keys
  = Number Float
  | Add
  | Subtract
  | Multiply
  | Divide
  | Negate
  | Percent
  | Decimal
  | Equal
  | ClearEntry
  | AllClear

keys : Signal.Mailbox Keys
keys = Signal.mailbox AllClear

-- MODEL

type alias Model =
  { function : Maybe (Float -> Float -> Float)
  , previousOperation : Maybe (Float -> Float)
  , memory : Float
  , current : Float
  , currentKey : Maybe Keys
  }

init : Model
init = 
  { function = Nothing
  , previousOperation = Nothing
  , memory = 0
  , current = 0
  , currentKey = Nothing
  }

-- VIEW

orange : Color
orange =
  rgb 255 153 0

lightGray : Color
lightGray =
  rgb 249 249 249

darkerGray : Color
darkerGray =
  rgb 220 220 220

calcButton : Keys -> String -> Element
calcButton key label =
  let
    text = Text.fromString label
      |> Text.typeface ["Helvetica Neue"]
      |> Text.height 20
  in
    customButton (Signal.message keys.address key)
      (container 50 50 middle (container 49 49 middle (centered text) |> color lightGray)
        |> color black)
      (container 50 50 middle (container 49 49 middle (centered text) |> color lightGray)
        |> color black)
      (container 50 50 middle (container 49 49 middle (centered text) |> color darkerGray)
        |> color black)

screen : Float -> Element
screen value =
  rightAligned (Text.color white (fromString (toString value)))
    |> width 200
    |> height 50
    |> color black

calculator : Model -> Element
calculator model =
  flow down
    [ screen model.current
    , flow right
        [ if model.current == 0
            then calcButton AllClear   "AC"
            else calcButton ClearEntry "C"
        , calcButton  Negate  "+/-"
        , calcButton  Percent "%"
        , calcButton  Divide  "/"
        ]
    , flow right
        [ calcButton (Number 7) "7"
        , calcButton (Number 8) "8"
        , calcButton (Number 9) "9"
        , calcButton  Multiply  "X"
        ]
    , flow right
        [ calcButton (Number 4) "4"
        , calcButton (Number 5) "5"
        , calcButton (Number 6) "6"
        , calcButton  Subtract  "-"
        ]
    , flow right
        [ calcButton (Number 1) "1"
        , calcButton (Number 2) "2"
        , calcButton (Number 3) "3"
        , calcButton  Add       "+"
        ]
    , flow right
        [ calcButton (Number 0) "0"
            |> width 100
        , calcButton  Decimal   "."
        , calcButton  Equal     "="
        ]
    --, show model
    --    |> width 200
    ]

-- UPDATE

update : Keys -> Model -> Model
update key model =
  case key of
    Number num ->
      { model | current <- num }
    Add ->
      { model | memory <- model.current, function <- Just (+) }
    Subtract ->
      { model | memory <- model.current, function <- Just (-) }
    Multiply ->
      { model | memory <- model.current, function <- Just (*) }
    Divide ->
      { model | memory <- model.current, function <- Just (/) }
    Negate ->
      { model | current <- negate model.current }
    Percent ->
      { model | current <- model.current / 100 }
    AllClear ->
      { model | current <- 0, memory <- 0, function <- Nothing }
    ClearEntry ->
      { model | current <- 0 }
    Equal ->
      case model.function of
        Just fn ->
          { model | current <- (fn model.current model.memory) 
                  , function <- Nothing
                  , previousOperation <- Just (fn model.current)
          }
        Nothing ->
          case model.previousOperation of
            Just fn ->
              { model | current <- (fn model.current) }
            Nothing ->
              model
    _ -> model

-- SIGNAL

main : Signal Element
main =
  Signal.map
    calculator
    (Signal.foldp update init keys.signal)
