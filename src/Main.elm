module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import KMeans exposing (Point3D)


type alias Model =
    { count : Int }


initialModel : Model
initialModel =
    let
        points =
            [ Point3D 0 0 0
            , Point3D 1 0 0
            , Point3D 1 1 1
            , Point3D 10.5 10.2 10.1
            , Point3D 10 12 12
            , Point3D 9 8 9
            ]

        clusters =
            KMeans.run [ Point3D -1 -1 -1, Point3D 5 42 42 ] points

        _ =
            Debug.log "clusters" clusters
    in
    { count = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ text "-1" ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
