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
  rgb 245 146 62

darkerOrange : Color
darkerOrange =
  rgb 219 107 11

lightGray : Color
lightGray =
  rgb 249 249 249

darkerGray : Color
darkerGray =
  rgb 220 220 220

type alias ButtonStyle =
  { height : Int
  , width : Int
  , backgroundColor : Color
  , textColor : Color
  , downBackgroundColor : Color
  , downTextColor : Color
  }

lightButtonStyle : ButtonStyle
lightButtonStyle =
  { height = 50
  , width = 50
  , backgroundColor = lightGray
  , textColor = black
  , downBackgroundColor = darkerGray
  , downTextColor = black
  }

orangeButtonStyle : ButtonStyle
orangeButtonStyle =
  { height = 50
  , width = 50
  , backgroundColor = orange
  , textColor = white
  , downBackgroundColor = darkerOrange
  , downTextColor = black
  }

calcButton : Keys -> String -> ButtonStyle -> Element
calcButton key label style =
  let
    text = Text.fromString label
      |> Text.typeface ["Helvetica Neue"]
      |> Text.height 20
      |> Text.color style.textColor
    border c = container style.width style.height middle c |> color black
    content = container (style.width - 1) (style.height - 1) middle (centered text)
  in
    customButton (Signal.message keys.address key)
      (border (content |> color style.backgroundColor))
      (border (content |> color style.backgroundColor))
      (border (content |> color style.downBackgroundColor))

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
            then calcButton AllClear   "AC" lightButtonStyle
            else calcButton ClearEntry "C" lightButtonStyle
        , calcButton  Negate  "+/-" lightButtonStyle
        , calcButton  Percent "%" lightButtonStyle
        , calcButton  Divide  "÷" orangeButtonStyle
        ]
    , flow right
        [ calcButton (Number 7) "7" lightButtonStyle
        , calcButton (Number 8) "8" lightButtonStyle
        , calcButton (Number 9) "9" lightButtonStyle
        , calcButton  Multiply  "×" orangeButtonStyle
        ]
    , flow right
        [ calcButton (Number 4) "4" lightButtonStyle
        , calcButton (Number 5) "5" lightButtonStyle
        , calcButton (Number 6) "6" lightButtonStyle
        , calcButton  Subtract  "−" orangeButtonStyle
        ]
    , flow right
        [ calcButton (Number 1) "1" lightButtonStyle
        , calcButton (Number 2) "2" lightButtonStyle
        , calcButton (Number 3) "3" lightButtonStyle
        , calcButton  Add       "+" orangeButtonStyle
        ]
    , flow right
        [ calcButton (Number 0) "0" { lightButtonStyle | width <- 100 }
        , calcButton  Decimal   "." lightButtonStyle
        , calcButton  Equal     "=" orangeButtonStyle
        ]
    --, show model
    --    |> width 200
    ]

-- UPDATE

update : Keys -> Model -> Model
update key model =
  case key of
    Number num ->
      { model | current <- (model.current * 10 + num) }
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
