module Snackbar exposing
    ( Model
    , Snackbar
    , Msg(..)
    , init
    , empty
    , singleton
    , fromError
    , update
    , view
    , remove
    , add
    , subscription
    )

import Time
import Process
import Task
import Html exposing (Html)
import Html.Attributes as Attributes
import Styles
import Animation exposing (px)


-- MODEL


type alias Model =
    { current : Maybe Snackbar
    , pending : List Snackbar
    , style : Animation.State
    }


type alias Snackbar =
    { message : String }


init : Model
init =
    { current = Nothing
    , pending = []
    , style =
        Animation.style -- With
{-             (Animation.spring
                { stiffness = 400
                , damping = 23 }
            ) -}
            [ Animation.opacity 0.0
            , Animation.bottom (px -50.0)
            ]
    }


empty : Maybe Snackbar
empty =
    Nothing


isEmpty : Maybe Snackbar -> Bool
isEmpty maybeSnackbar =
    case maybeSnackbar of
        Nothing ->
            True

        Just _ ->
            False


singleton : String -> Maybe Snackbar
singleton =
    Just << Snackbar


fromError : Result String a -> Maybe Snackbar
fromError result =
    case result of
        Ok _ ->
            empty

        Err msg ->
            singleton msg



-- UPDATE


type Msg
    = AddSnackbar Snackbar
    | RemoveSnackbar
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddSnackbar snackbar ->
            case model.current of
                Just _ ->
                    { model | pending = model.pending ++ [ snackbar ] }
                        ! []

                Nothing ->
                    let
                        style = show model.style
                    in
                        { model | current = Just snackbar, style = style }
                            ! [ remove ]

        RemoveSnackbar ->
            case model.pending of
                snackbar :: rest ->
                    { model
                        | current = empty
                        , pending = rest
                        , style = hide model.style
                    }
                        ! [ add (Just snackbar) ]

                [] ->
                    { init | style = hide model.style } ! []

        Animate animMsg ->
            let
                style =
                    Animation.update animMsg model.style
            in
                { model | style = style } ! []


show style =
    Animation.queue
        [ Animation.to
            [ Animation.opacity 1.0
            , Animation.bottom (px 0.0)
            ]
        ]
        style

hide style =
    Animation.queue
        [ Animation.to
            [ Animation.opacity 0.0
            , Animation.bottom (px -50.0)
            ]
        ]
        style


add : Maybe Snackbar -> Cmd Msg
add maybeSnackbar =
    case maybeSnackbar of
        Just snackbar ->
            Task.succeed snackbar
                |> Task.perform AddSnackbar

        Nothing ->
            Cmd.none


remove : Cmd Msg
remove =
    Process.sleep (3 * Time.second)
        |> Task.perform (\_ -> RemoveSnackbar)



-- VIEW


view : Model -> Html msg
view model =
    let
        message =
            model.current
                |> Maybe.map .message
                |> Maybe.withDefault ""

        defaultStyle =
            Attributes.style Styles.default

        animatedStyle =
            Animation.render model.style
    in
        Html.div
            (defaultStyle :: animatedStyle)
            [ Html.text message ]



-- SUBSCRIPTIONS


subscription : Model -> Sub Msg
subscription model =
    Animation.subscription Animate [ model.style ]
