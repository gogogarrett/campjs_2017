module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)


type Route
    = LobbyRoute
    | GameRoute String
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map LobbyRoute top
        , map LobbyRoute (s "lobby")
        , map GameRoute (s "games" </> string)
        ]


parseLocation : Location -> Route
parseLocation location =
    case parseHash matchers location of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
