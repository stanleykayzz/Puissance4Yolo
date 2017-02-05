import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Http
import Array
import Time
import Maybe exposing (..)
import Json.Decode exposing (..)


main =
  Html.program
    { init = initModel ""
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL
-- type alias ModelBeforeGame_=
--   {
--     name : String
--   }
-- type alias ModelBeforeGame = ModelBeforeGame_
--
-- type alias ModelInGame_ =
--   {
--     name : String,
--     gameKey : String,
--     gameStatus : String
--   }
-- type alias ModelInGame = ModelInGame_
-- type Model = ModelInGame | ModelBeforeGame
type alias Game =
  {
    key : String,
    status : String,
    player1Name : String,
    player2Name : String,
    -- grid : Grid
    gridOrientation : String
    ,gridCells : List String
  }

-- type alias Grid =
--   {
--     orientation : String,
--     cells : List String
--   }
type alias ApiCreateResponse =
  {
    playerKey : String,
    gameKey : String
  }

type alias Action =
  {
    actionType : String,
    actionInfo : String
  }

type alias Model =
  {
    name : String,
    playerKey : String,
    inGame : Bool,
    game : Game
  }

initGame : String -> Game
initGame gameKey=
    Game gameKey "WAITING" "" "" "horizontal" [ "no", "no", "no", "no", "no", "no","no", "no", "no", "no", "no", "no","no", "no", "no", "no", "no", "no","no", "no", "no", "no", "no", "no","no", "no", "no", "no", "no", "no","no", "no", "no", "no", "no", "no","no", "no", "no", "no", "no", "no" ]

initModel : String -> (Model, Cmd Msg)
initModel name=
  (  Model name "" False (initGame "")
  , Cmd.none
  )


-- UPDATE

type Msg
  = ChangeName String
  | ChangeGameKey String
  | GameCreate
  | GameJoin
  | GameInfo Time.Time
  | GameAction Action
  | GameCreateDone (Result Http.Error ApiCreateResponse)
  | GameJoinDone (Result Http.Error String)
  | GameInfoDone (Result Http.Error Game)
  | GameActionDone (Result Http.Error Game)
  | ReturnToHome


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- Other
    ReturnToHome ->
      initModel model.name
    -- on form input Change
    ChangeName newName ->
      (Model newName "" False (initGame model.game.key), Cmd.none) --init model

    ChangeGameKey newGameKey ->
      (Model model.name "" False (initGame newGameKey), Cmd.none) --init model
    -- API calls
    GameCreate ->
      (model, apiCreateGame model.name)

    GameJoin ->
      (model, apiJoinGame model.name model.game.key)

    GameInfo notUsed ->
      if model.inGame == True then
        (model, apiInfoGame model.game.key)
        -- (model, Cmd.none)
      else
        (model, Cmd.none)

    GameAction action ->
      -- (model, Cmd.none)
      (model, (apiActionGame model.playerKey action.actionType action.actionInfo))
    -- on receive API response
    GameCreateDone (Ok apiCreateResponse) ->
      (Model model.name apiCreateResponse.playerKey True (initGame apiCreateResponse.gameKey), Cmd.none)

    GameCreateDone (Err _) ->
      (model, Cmd.none)

    GameJoinDone (Ok playerKey) ->
      (Model model.name playerKey True (initGame model.game.key), Cmd.none)

    GameJoinDone (Err _) ->
      (model, Cmd.none)

    GameInfoDone (Ok game) ->
      (Model model.name model.playerKey True game, Cmd.none)

    GameInfoDone (Err _) ->
      (model, Cmd.none)

    GameActionDone (Ok game) ->
      (Model model.name model.playerKey True game, Cmd.none)

    GameActionDone (Err _) ->
      (model, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.second GameInfo

-- HTTP
-- -- Create
apiCreateGame : String -> Cmd Msg
apiCreateGame name =
  let
    url =
      "http://localhost:8083/elm/api/create.php?playerName=" ++ name
  in
    Http.send GameCreateDone (Http.get url decodeGameCreate)

decodeGameCreate : Decoder ApiCreateResponse
decodeGameCreate =
  Json.Decode.map2 ApiCreateResponse
    (Json.Decode.at ["player1Key"] Json.Decode.string)
    (Json.Decode.at ["gameKey"] Json.Decode.string)

-- -- Create
apiJoinGame : String -> String -> Cmd Msg
apiJoinGame name gameKey =
  let
    url =
      "http://localhost:8083/elm/api/join.php?playerName=" ++ name ++ "&gameKey=" ++ gameKey
  in
    Http.send GameJoinDone (Http.get url decodeGameJoin)

decodeGameJoin : Decoder String
decodeGameJoin =
  Json.Decode.at ["player2Key"] Json.Decode.string

-- -- Info
apiInfoGame : String -> Cmd Msg
apiInfoGame gameKey =
  let
    url =
      "http://localhost:8083/elm/api/info.php?gameKey=" ++ gameKey
  in
    Http.send GameInfoDone (Http.get url decodeGameInfo)

decodeGameInfo : Decoder Game
decodeGameInfo =
  map6 Game
    (Json.Decode.at ["key"] Json.Decode.string)
    (Json.Decode.at ["status"] Json.Decode.string)
    (Json.Decode.at ["player1Name"] Json.Decode.string)
    (Json.Decode.at ["player2Name"] Json.Decode.string)
    (Json.Decode.at ["gridOrientation"] Json.Decode.string)
    (Json.Decode.at ["gridCells"] (Json.Decode.list string) )

  -- -- Action
apiActionGame : String -> String -> String -> Cmd Msg
apiActionGame playerKey actionType actionInfo =
  let
    url =
      "http://localhost:8083/elm/api/action.php?playerKey=" ++ playerKey ++ "&actionType=" ++ actionType ++ "&actionInfo=" ++ actionInfo
  in
    Http.send GameInfoDone (Http.get url decodeGameInfo)

-- VIEW

view : Model -> Html Msg
view model =
    div [] [
      stylesheet,
      if model.inGame == False then
        div []
          [
            input [ placeholder "Your name", onInput ChangeName, Html.Attributes.value
 model.name ] []
            , button [ onClick GameCreate ] [ text "create" ]
            , input [ placeholder "Game Key", onInput ChangeGameKey ] []
            , button [ onClick GameJoin ] [ text "join" ]
          ]
      else
        div [ classList [("grid", True),(model.game.gridOrientation, True)]] [
        gameInfoBar model
        , if model.game.gridOrientation == "horizontal" then
          gridHorizontal model (Array.fromList model.game.gridCells)
        else
          gridVertical model (Array.fromList model.game.gridCells)
        ]
    ]

-- Templates
stylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "http://localhost:8383/tourni4.css"
            ]
        children = []
    in
        Html.node tag attrs children

grid : Model -> Html Msg
grid model =
  div [ classList [("grid", True),(model.game.gridOrientation, True)]] [
  gameInfoBar model
  , gridHorizontal model (Array.fromList model.game.gridCells) ]

gameInfoBar : Model -> Html Msg
gameInfoBar model =
  div [classList [("gameInfoBar",True)]] [
    case model.game.status of
    "WAITING" ->
        text ("Second player must use this key : " ++ model.game.key)

    "TURN_PLAYER_1" ->
      text (model.game.player1Name ++ "'s turn")

    "TURN_PLAYER_2" ->
      text (model.game.player2Name ++ "'s turn")

    "PLAYER_1_WON" ->
      div [] [text (model.game.player1Name ++ " won"), button [ onClick ReturnToHome ] [text "play another game"]]

    "PLAYER_2_WON" ->
      div [] [text (model.game.player2Name ++ " won"), button [ onClick ReturnToHome ] [text "play another game"]]

    _ ->
      text "invalid game status"
  ]

gameActionBar : Model -> Html Msg
gameActionBar model =
  div [classList [("gameActionBar",True) ] ] [
    if model.game.status == "TURN_PLAYER_1" && model.name == model.game.player1Name then
      gameActionBar_ model.game.gridOrientation
    else if model.game.status == "TURN_PLAYER_2" && model.name == model.game.player2Name then
      gameActionBar_ model.game.gridOrientation
    else
      text ""
  ]

gameActionBar_ : String -> Html Msg
gameActionBar_ gridOrientation =
  div [] [
    div [classList [("line",True),("header",True)] ]
      [
        -- button [ onClick (GameAction "token" "0") ] [ text "+" ]
        div [ classList [("cell",True),("left",True)] ] [div [ classList [("vertical-aligner", True)] ] [], button [ onClick (GameAction (Action "turn" "left") ) ] [ text "↶" ]]
        ,div [ classList [("cell",True),("right",True)] ] [div [ classList [("vertical-aligner", True)] ] [], button [ onClick (GameAction (Action "turn" "right") ) ] [ text "↷" ]]
      ]
    , div [classList [("line",True),("header",True)] ]
      (gameActionBarChildren gridOrientation)
  ]

gridCell : Array.Array String -> Int -> Html Msg
gridCell cells index =
  div [ classList [("cell",True),
    (withDefault "no" (Array.get index cells),True)
  ] ]
  [div [ classList [("vertical-aligner", True)] ] [], div [classList [("token", True)] ] []]

gridLine : Array.Array String -> Int -> Int-> Html Msg
gridLine cells index remainingCells =
  div [classList [("line",True)] ]
    (gridLineChildren cells index remainingCells [])

gridLineChildren : Array.Array String -> Int -> Int -> List (Html Msg) -> List (Html Msg)
gridLineChildren cells index remainingCells currentList =
  if remainingCells > 0 then
    List.concat [currentList, [ gridCell cells index], gridLineChildren cells (index+1) (remainingCells-1) currentList ]
  else
    List.concat [currentList, [ gridCell cells index] ]

gameActionBarColumnButton : String -> Html Msg
gameActionBarColumnButton index =
  div [ classList [("cell",True)] ] [div [ classList [("vertical-aligner", True)] ] [], button [ onClick (GameAction (Action "token" index) ) ] [ text "↓" ]]

gameActionBarChildren : String -> List (Html Msg)
gameActionBarChildren gridOrientation =
  if gridOrientation == "horizontal" then
    (gameActionBarChildren_ 0 7 [])
  else
    (gameActionBarChildren_ 0 6 [])

gameActionBarChildren_ : Int -> Int -> List (Html Msg) -> List (Html Msg)
gameActionBarChildren_ index remaining currentList =
  if remaining > 0 then
    List.concat [currentList, [ gameActionBarColumnButton (toString index)], gameActionBarChildren_ (index+1) (remaining-1) currentList ]
  else
    List.concat [currentList, [ gameActionBarColumnButton (toString index)] ]

gridHorizontal : Model -> Array.Array String -> Html Msg
gridHorizontal model cells =
  div [] [
    gameActionBar model
    ,(gridLine cells 0 7)
    ,(gridLine cells 7 7)
    ,(gridLine cells 14 7)
    ,(gridLine cells 21 7)
    ,(gridLine cells 28 7)
    ,(gridLine cells 35 7)
    ]

gridVertical : Model -> Array.Array String -> Html Msg
gridVertical model cells =
  div [] [
    gameActionBar model
    ,(gridLine cells 0 6)
    ,(gridLine cells 6 6)
    ,(gridLine cells 12 6)
    ,(gridLine cells 18 6)
    ,(gridLine cells 24 6)
    ,(gridLine cells 30 6)
    ,(gridLine cells 36 6)
    ]
