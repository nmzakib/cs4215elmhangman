import Html exposing (..)
import Basics exposing (..)
import Html.Attributes exposing (..)
import Char
import Html.Events exposing (onClick)
import Graphics.Element exposing (..)
import Keyboard
import String
import Signal exposing (Address)
import StartApp.Simple exposing (start)
import Task exposing (..)
import Random exposing (..)
import Date exposing (..)
import Time exposing (..)
import Json.Decode as Json
import List
import Maybe exposing (..)
import Now

{--}

main = 
   StartApp.Simple.start
    { model = init 10 (initialSeed (round Now.loadTime))
    , update = update
    , view = view
    }

--}

{--

main : Signal Element
main =
  Signal.map display2 Keyboard.presses


display2 : Int -> Element
display2 keyCode =
  show <|
    "The last key you pressed was: "
    ++ toString (Char.fromCode keyCode)

--}

display : Int -> String
display keyCode =
  toString (Char.fromCode keyCode)


-- MODEL

type alias Model = 
  { answer : String
  , shown : String
  , attempted : String
  , attempts : Int
  , maxAttempts : Int
  , state : GameState
  , seed : Seed
  }

type GameState = Playing | Over


init : Int -> Seed -> Model
init maxAttempts seed =
  { answer = getWord seed
  , shown = toUnderscores (getWord (seed))
  , attempted = ""
  , attempts = 0
  , maxAttempts = maxAttempts
  , state = Playing
  , seed = seed
  }
  
toUnderscores : String -> String
toUnderscores string =
  String.map (\c -> '_') string
  
words : List String
words =
  [ "abacus"
  , "banana"
  , "candle"
  , "dinosaur"
  , "ethereal"
  , "friends"
  , "gurgle"
  , "heightened"
  , "interior"
  , "jumbled"
  , "kickstart"
  , "longitudinal"
  , "memory"
  , "navigate"
  , "operation"
  , "pasta"
  , "quivering"
  , "roster"
  , "sophisticated"
  , "truncate"
  , "underline"
  , "viridian"
  , "warlord"
  , "xylophone"
  , "yonder"
  , "zombie"
  ]

get n xs = List.head (List.drop n xs)

{--

getWord : String
getWord = 
  Http.getString "http://randomword.setgetgo.com/get.php" |> toString

getWord1 = 
  Http.get decodeUrl ("http://randomword.setgetgo.com/get.php") |> toString

getRandomGif : Effects Action
getRandomGif  =
  Http.get decodeUrl ("http://randomword.setgetgo.com/get.php")
    |> Task.toMaybe
    |> Task.map NewWord
    |> Effects.task

decodeUrl : Json.Decoder String
decodeUrl =
  Json.at ["data"] Json.string

--}


getWord : Seed -> String
getWord seed = 
  case generate (int 0 25) seed of
  (x, y) -> 
    case get x words of
      Just string -> string
      Nothing -> ""


{--
timeType : Int
timeType = round time

rand = generate (int 0 25) (initialSeed timeType)
--}


-- UPDATE

type Action
    = Guess String | NewGame

update : Action -> Model -> Model
update action model = 
  case action of
    Guess letter ->
    if model.state == Playing
    then 
      if String.contains letter model.answer
      then
          if replace letter model.answer model.shown == model.answer
          then
            { model | shown = replace letter model.answer model.shown,
              state = Over
            }
          else
            { model | shown = replace letter model.answer model.shown }
      else
          if String.contains letter model.attempted
          then
            model
          else
              if model.attempts + 1 == model.maxAttempts
              then
              { model |
                attempts = model.attempts + 1,
                attempted = model.attempted ++ letter,
                state = Over
              }
              else
              { model |
                attempts = model.attempts + 1,
                attempted = model.attempted ++ letter
              }
    else model
    NewGame -> init 10 (snd (generate (int 0 25) model.seed))

        
message : Model -> String
message model = 
  case model.state of
    Playing -> "You have " ++ toString (model.maxAttempts - model.attempts) ++ " attempts left"
    Over -> "Game Over!!! The answer is " ++ model.answer
    
replace : String -> String -> String -> String
replace letter answer shown = 
  let helper letter answer shown acc = 
    case (String.uncons answer, String.uncons shown) of 
      (Just (head, tail), Just (shead, stail)) -> if ((String.fromChar head) == letter) then
        helper letter tail stail (acc ++ String.fromChar head) else
        helper letter tail stail (acc ++ String.fromChar shead)
      (_, _) -> acc
  in helper letter answer shown ""

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ div [] [text (model.shown)]
    --, div [] [text (model.answer)]
    , div [] [text ("Attempted letters: " ++ model.attempted)]
    , div [] [text (message model)]
--    , div [] [text (toString model.attempts)]
--    , div [] [text (toString model.maxAttempts)]
--    , button [onClick address (Guess (toString (Signal.map display Keyboard.presses)))] []
    , button [onClick address (Guess "q")] [text "q"]
    , button [onClick address (Guess "w")] [text "w"]
    , button [onClick address (Guess "e")] [text "e"]
    , button [onClick address (Guess "r")] [text "r"]
    , button [onClick address (Guess "t")] [text "t"]
    , button [onClick address (Guess "y")] [text "y"]
    , button [onClick address (Guess "u")] [text "u"]
    , button [onClick address (Guess "i")] [text "i"]
    , button [onClick address (Guess "o")] [text "o"]
    , button [onClick address (Guess "p")] [text "p"]
    , div [] []
    , button [onClick address (Guess "a")] [text "a"]
    , button [onClick address (Guess "s")] [text "s"]
    , button [onClick address (Guess "d")] [text "d"]
    , button [onClick address (Guess "f")] [text "f"]
    , button [onClick address (Guess "g")] [text "g"]
    , button [onClick address (Guess "h")] [text "h"]
    , button [onClick address (Guess "j")] [text "j"]
    , button [onClick address (Guess "k")] [text "k"]
    , button [onClick address (Guess "l")] [text "l"]
    , div [] []
    , button [onClick address (Guess "z")] [text "z"]
    , button [onClick address (Guess "x")] [text "x"]
    , button [onClick address (Guess "c")] [text "c"]
    , button [onClick address (Guess "v")] [text "v"]
    , button [onClick address (Guess "b")] [text "b"]
    , button [onClick address (Guess "n")] [text "n"]
    , button [onClick address (Guess "m")] [text "m"]
    , div [] []
    , button [onClick address (NewGame)] [text "New Game"]
    --, div [] [text (toString model.seed)]
    ]
  
