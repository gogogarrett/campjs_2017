module App exposing (..)

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
import Routing exposing (..)


-- main


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- model


type alias Model =
    { phxSocket : Phoenix.Socket.Socket Msg
    , userId : String
    , history : List Navigation.Location
    , route : Route
    , players : List PlayerScore
    }



-- init


initPhxSocket : String -> Phoenix.Socket.Socket Msg
initPhxSocket userId =
    Phoenix.Socket.init (keySocketServer userId)
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on keyGameOffer keyGameAssignerLobby GameOffer
        |> Phoenix.Socket.on keyGameReject keyGameAssignerLobby GameReject


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        currentRoute =
            Routing.parseLocation location
    in
    ( Model (initPhxSocket flags.userId) flags.userId [ location ] currentRoute []
    , Cmd.none
    )


type alias Flags =
    { userId : String
    }



-- actions


type alias GameScopeContext =
    { scope : String
    }


type alias GameContext =
    { game_id : String
    }


type alias ChannelContext =
    { channel_topic : String
    , payload : String
    }


type Msg
    = PhoenixMsg (Phoenix.Socket.Msg Msg)
    | JoinChannel ChannelContext
    | GameOffer JE.Value
    | GameReject JE.Value
    | RequestGame GameScopeContext
    | UrlChange Navigation.Location
    | AnswerQuestion JE.Value
    | UpdateGameScore JE.Value



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        x =
            Debug.log "update(msg, model)" ( msg, model )
    in
    case msg of
        JoinChannel channelContext ->
            let
                channel =
                    Phoenix.Channel.init channelContext.channel_topic
                        |> Phoenix.Channel.withPayload (userParams model.userId)

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.join channel model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
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
            let
                input_string =
                    JE.encode 0 raw

                game_id =
                    JD.decodeString decodeGameContext input_string

                y =
                    Result.withDefault (GameContext "0") game_id

                true_id =
                    y.game_id

                game_channel_topic =
                    keyGameChannel true_id

                answer_question_action =
                    AnswerQuestion (buildGameContext true_id)

                aa =
                    model.phxSocket
                        |> Phoenix.Socket.withDebug
                        |> Phoenix.Socket.on keySubmitAnswer game_channel_topic AnswerQuestion
                        |> Phoenix.Socket.on keyGameStateUpdated game_channel_topic UpdateGameScore
            in
            ( { model
                | route = GameRoute true_id
                , phxSocket = aa
              }
            , Navigation.newUrl ("#games/" ++ true_id)
            )

        GameReject raw ->
            ( model, Cmd.none )

        AnswerQuestion raw ->
            let
                input_string =
                    JE.encode 0 raw

                game_id =
                    JD.decodeString decodeGameContext input_string

                y =
                    Result.withDefault (GameContext "0") game_id

                true_id =
                    y.game_id

                payload =
                    JE.int 1

                push_ =
                    Phoenix.Push.init keySubmitAnswer (keyGameChannel true_id)
                        |> Phoenix.Push.withPayload payload

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push_ model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )

        UpdateGameScore raw ->
            let
                input_string =
                    JE.encode 0 raw

                x =
                    Debug.log "input string" input_string

                players =
                    case JD.decodeString decodeGameState input_string of
                        Ok s ->
                            s

                        Err msg ->
                            [ PlayerScore "0" 1 ]
            in
            ( { model | players = players }, Cmd.none )

        UrlChange location ->
            let
                currentRoute =
                    Routing.parseLocation location
            in
            ( { model
                | history = location :: model.history
                , route = currentRoute
              }
            , Cmd.none
            )



-- subs


subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.Socket.listen model.phxSocket PhoenixMsg



-- view


view : Model -> Html Msg
view model =
    div []
        [ page model ]


page : Model -> Html Msg
page model =
    case model.route of
        LobbyRoute ->
            div [ class "container" ]
                [ h3 [] [ text ("Hello User " ++ model.userId ++ "!") ]
                , button [ onClick (JoinChannel (buildChannelContext keyGameAssignerLobby "123")) ] [ text "Join channel" ]
                , button [ onClick (RequestGame (newGameScope "world")) ] [ text "Join world" ]
                , ul []
                    [ li [] [ a [ href ("#" ++ "lobby") ] [ text "lobby" ] ]
                    ]
                ]

        GameRoute gameId ->
            div [ class "container" ]
                [ h3 [] [ text ("Hello User " ++ model.userId ++ "!") ]
                , h6 [] [ text ("Game" ++ gameId) ]
                , button [ onClick (JoinChannel (buildChannelContext (keyGameChannel gameId) "123")) ] [ text "Join channel" ]
                , button [ onClick (AnswerQuestion (buildGameContext gameId)) ] [ text "Answer correctly" ]
                , ul []
                    [ li [] [ a [ href ("#" ++ "lobby") ] [ text "lobby" ] ]
                    ]
                , ul [] (renderPlayers model.players)
                ]

        NotFoundRoute ->
            div [ class "abc" ] [ text "no page found" ]


renderPlayers : List PlayerScore -> List (Html Msg)
renderPlayers players =
    List.map renderPlayer players


renderPlayer : PlayerScore -> Html Msg
renderPlayer player =
    li []
        [ p []
            [ text ("Player" ++ player.id)
                span
                []
                [ text (toString player.score) ]
            ]
        ]


newGameScope : String -> GameScopeContext
newGameScope scope =
    GameScopeContext scope


userParams : String -> JE.Value
userParams userId =
    JE.object [ ( "user_id", JE.string userId ) ]


decodeGameContext : JD.Decoder GameContext
decodeGameContext =
    JD.map GameContext
        (JD.field "game_id" JD.string)


type alias PlayerScore =
    { id : String
    , score : Int
    }


decodeGameState : JD.Decoder (List PlayerScore)
decodeGameState =
    JD.field "players" <|
        JD.list <|
            JD.map2 PlayerScore
                (JD.field "id" JD.string)
                (JD.field "score" JD.int)


buildGameContext : String -> JE.Value
buildGameContext gameId =
    JE.object [ ( "game_id", JE.string gameId ) ]


buildChannelContext : String -> String -> ChannelContext
buildChannelContext channelId payLoad =
    ChannelContext channelId payLoad
