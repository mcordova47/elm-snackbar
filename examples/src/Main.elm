module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Snackbar


-- MODEL


type alias Model =
    { snackbar : Snackbar.Model }


init : ( Model, Cmd Msg )
init =
    ( { snackbar = Snackbar.init }
    , Cmd.none
    )



-- UPDATE


type Msg
    = CreateSnackbar
    | SnackbarMsg Snackbar.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateSnackbar ->
            let
                snackbar =
                    Snackbar.singleton "Button was clicked"
            in
                ( model
                , Cmd.map SnackbarMsg (Snackbar.add snackbar)
                )

        SnackbarMsg snackbarMsg ->
            let
                ( snackbar, cmd ) =
                    Snackbar.update snackbarMsg model.snackbar
            in
                { model | snackbar = snackbar }
                    ! [ Cmd.map SnackbarMsg cmd ]



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        []
        [ button "create alert" CreateSnackbar
        , Snackbar.view model.snackbar
        ]


button : String -> Msg -> Html Msg
button text msg =
    Html.div
        [ Attributes.class "button"
        , Events.onClick msg
        ]
        [ Html.text text ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map
        SnackbarMsg
        (Snackbar.subscription model.snackbar)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
