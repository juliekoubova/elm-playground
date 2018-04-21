module Main exposing (..)

import Html exposing (Html, beginnerProgram, button, div, hr, text)
import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    List Int


model : Model
model =
    [ 0, 1, 2 ]



-- UPDATE


type Msg
    = Modify ( Int, Int )
    | AddNextCounter


update : Msg -> Model -> Model
update msg model =
    case msg of
        Modify ( index, amount ) ->
            List.indexedMap
                (\i value ->
                    if i == index then
                        value + amount
                    else
                        value
                )
                model

        AddNextCounter ->
            0 :: model


-- VIEW


elementView : Int -> Int -> Html.Html Msg
elementView index model =
    div []
        [ div []
            [ button [ onClick (Modify ( index, -1 )) ] [ text "-" ]
            , div [] [ text (toString model) ]
            , button [ onClick (Modify ( index, 1 )) ] [ text "+" ]
            ]
        , hr [] []
        ]


view : Model -> Html.Html Msg
view model =
    div []
        [ div [] (List.indexedMap elementView model)
        , button [ onClick AddNextCounter ] [ text "Add counter" ]
        ]
