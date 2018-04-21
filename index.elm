import Html exposing (beginnerProgram, button, div, Html, hr, text)
import Html.Events exposing (onClick)


main: Program Never Model Msg
main =
  beginnerProgram { model = model, view = view, update = update }
  

-- MODEL

type alias Model = List Int

model : Model
model = [0, 1, 2]


-- UPDATE

type Msg = Increment Int | Decrement Int

addAtIndex : Int -> Int -> List Int -> List Int
addAtIndex addend index =
  List.indexedMap
     (\i value -> if i == index then value + addend else value ) 

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment index ->
      addAtIndex (1) index model

    Decrement index ->
      addAtIndex (-1) index model
      

-- VIEW
elementView : Int -> Int -> Html.Html Msg
elementView index model =
  div []
    [ div []
        [ button [ onClick (Decrement index) ] [ text "-" ]
        , div [] [ text (toString model) ]
        , button [ onClick (Increment index) ] [ text "+" ]
        ]
    , hr [] []
    ]

view : Model -> Html.Html Msg 
view model =
  div [] (List.indexedMap elementView model)