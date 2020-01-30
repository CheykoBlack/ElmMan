module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode

main =
  Browser.element { 
    init = init
    , update = update
    , view = view 
    , subscriptions = subscriptions
    }

type Msg = UpdateGrid | CharacterKey Char | ControlKey String

type alias Grid = List (List Bool)

type alias Model =
  {
    grid: Grid,
    pacManLocation: (Int, Int)
  }

gridSize : Int
gridSize = 5

init : () -> (Model, Cmd Msg)
init _ =
  ({
    grid = List.repeat gridSize (List.repeat gridSize False),
    pacManLocation = (0,0)
  }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateGrid -> 
        ({ model | grid = model.grid }, Cmd.none)
    
    CharacterKey 's' ->
        ({ model | pacManLocation = calcNewPosition model.pacManLocation (1, 0) }, Cmd.none)

    CharacterKey 'd' ->
        ({ model | pacManLocation = calcNewPosition model.pacManLocation (0, 1) }, Cmd.none)
    
    CharacterKey 'w' ->
        ({ model | pacManLocation = calcNewPosition model.pacManLocation (-1, 0) }, Cmd.none)
    
    CharacterKey 'a' ->
        ({ model | pacManLocation = calcNewPosition model.pacManLocation (0, -1) }, Cmd.none)

    _ ->
        (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown keyDecoder ]

calcNewPosition : (Int, Int) -> (Int, Int) -> (Int, Int)
calcNewPosition (pacX, pacY) (x, y) = (if isInBounds pacX x then pacX + x else pacX, if isInBounds pacY y then pacY + y else pacY)

isInBounds : Int -> Int -> Bool
isInBounds pacManLocation incrementer =
    pacManLocation + incrementer >= 0 && pacManLocation + incrementer < gridSize

view : Model -> Html Msg
view model =
  div []
    [ renderList model ]
    
renderList : Model -> Html msg
renderList model =
  model.grid
    |> List.indexedMap (\rowIndex -> \row -> renderDot model row rowIndex |> div [] )
    |> div []

renderDot : Model -> List Bool -> Int -> List (Html msg)
renderDot model row rowIndex = List.indexedMap (\colIndex -> \col -> span [if isPacMan model.pacManLocation rowIndex colIndex then class "pac-man" else class ""] [ text (toDisplay col) ]) row

toDisplay : Bool -> String
toDisplay isDisplayed = if isDisplayed then " " else "."

isPacMan : (Int, Int) -> Int -> Int -> Bool
isPacMan (pacX, pacY) x y = pacX == x && pacY == y

keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)

toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char

        _ ->
            ControlKey keyValue