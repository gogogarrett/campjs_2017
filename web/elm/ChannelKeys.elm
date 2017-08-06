module ChannelKeys exposing (..)


keySocketServer : String -> String
keySocketServer userId =
    "ws://localhost:4001/socket/websocket?user_id=" ++ userId


keyGameAssignerLobby : String
keyGameAssignerLobby =
    "lobby:game_assigner"


keyGameOffer : String
keyGameOffer =
    "game_offer"


keyGameReject : String
keyGameReject =
    "game_reject"


keyGameChannel : String -> String
keyGameChannel gameId =
    "game:" ++ gameId


keySubmitAnswer : String
keySubmitAnswer =
    "submit_answer"


keyGameStateUpdated : String
keyGameStateUpdated =
    "game_state_updated"
