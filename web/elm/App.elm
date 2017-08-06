module Main exposing (..)

import ChannelKeys exposing (..)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Navigation
import Phoenix.Channel
import Phoenix.Push
import Phoenix.Socket


type alias Flags =
    { userId : String
    }



-- main


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


userParams : String -> JE.Value
userParams userId =
    JE.object [ ( "user_id", JE.string userId ) ]



-- router


type Route
    = LobbyRoute
    | NotFoundRoute



-- model


type alias Model =
    { phxSocket : Phoenix.Socket.Socket Msg
    , userId : String
    , history : List Navigation.Location
    }



-- init


initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init keySocketServer
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on keyGameOffer keyGameAssignerLobby GameOffer
        |> Phoenix.Socket.on keyGameReject keyGameAssignerLobby GameReject


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    ( Model initPhxSocket flags.userId [ location ]
    , Cmd.none
    )



-- actions


type alias GameScopeContext =
    { scope : String
    }


type Msg
    = PhoenixMsg (Phoenix.Socket.Msg Msg)
    | JoinChannel
    | GameOffer JE.Value
    | GameReject JE.Value
    | RequestGame GameScopeContext
    | UrlChange Navigation.Location



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        x =
            Debug.log "update(msg, model)" ( msg, model )
    in
    case msg of
        JoinChannel ->
            let
                channel =
                    Phoenix.Channel.init keyGameAssignerLobby
                        |> Phoenix.Channel.withPayload (userParams model.userId)

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.join channel model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
              -- what does this do? --
            )

        RequestGame game_scope_context ->
            let
                payload =
                    JE.object
                        [ ( "user_id", JE.string model.userId )
                        , ( "scope", JE.string game_scope_context.scope )
                        ]

                push_ =
                    Phoenix.Push.init "request_game" keyGameAssignerLobby
                        |> Phoenix.Push.withPayload payload

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push_ model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )

        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )

        GameOffer raw ->
            -- raw ==
            -- {
            --   questions = {
            --     0 = "how are you",
            --     1 = "what's up",
            --     2 = "where am i"
            --   },
            --   players = {
            --     0 = {
            --       player_id = "1", join_time = 1501915946
            --     },
            --     1 = {
            --       player_id = "2", join_time = 1501915948
            --     }
            --   },
            --   game_id = "2e7b7885-7708-490b-acf0-b5cbb8fbd633"
            -- }
            let
                x =
                    Debug.log "GameOffer!" raw
            in
            ( model, Cmd.none )

        GameReject raw ->
            let
                x =
                    Debug.log "GameReject!" raw
            in
            ( model, Cmd.none )

        UrlChange location ->
            ( { model | history = location :: model.history }
            , Cmd.none
            )



-- subs


subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.Socket.listen model.phxSocket PhoenixMsg



-- view


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ button [ onClick JoinChannel ] [ text "Join channel" ]
        , button [ onClick (RequestGame (newGameScope "world")) ] [ text "Join world" ]
        , li [] [ a [ href ("#" ++ "abc") ] [ text "abc" ] ]
        ]


newGameScope : String -> GameScopeContext
newGameScope scope =
    GameScopeContext scope
