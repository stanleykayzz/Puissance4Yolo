module prog

open Suave
open Suave.Successful
open Suave.Filters
open Suave.Operators
open Suave.Writers
open Suave.RequestErrors
//on import le fsharp data pour le json
open FSharp.Data
open FSharp.Data.TypeProviders
open Newtonsoft.Json.Serialization
open Chiron
//on importe la classe qu'on a créé
open game

//http://localhost:8083/elm/api/create.php?playerName=" ++ name
//http://localhost:8083/elm/apiPHP/info.php?gameKey
//http://localhost:8083/elm/api/action.php?playerKey="++ playerKey ++ "&actionType=" ++ actionType ++ "&actionInfo=" ++ actionInfo
//http://localhost:8083/elm/apiPHP/join.php?playerName=" ++ name ++ "&gameKey=" ++ gameKey

let g:game.Game = new game.Game()
let mutable id = 0
let php =
    g.setId(id)
    request (fun r ->        
        match r.queryParam "playerName" with
        | Choice1Of2 name -> OK(g.setFirstPlayerAndReturnJson(name))
        | Choice2Of2 msg -> BAD_REQUEST msg)
        
let php2 =
    request (fun r ->
        match r.queryParam "gameKey" with
        | Choice1Of2 key -> OK (g.infoGameAndReturnJsonString())
        | Choice2Of2 msg -> BAD_REQUEST msg)       

let php3 =
    request (fun r ->
        match r.queryParam "playerKey" with
           | Choice1Of2 playerKey -> 
               match r.queryParam "actionType" with
               | Choice1Of2 actionType -> 
                   match r.queryParam "actionInfo" with
                      | Choice1Of2 actionInfo -> OK (g.actionGameReturnJsonString(playerKey,actionType,actionInfo))//(sprintf "playerKey: %s actionType: %s actionInfo: %s" playerKey actionType actionInfo)
                      | Choice2Of2 msg -> BAD_REQUEST msg
               | Choice2Of2 msg -> BAD_REQUEST msg
           | Choice2Of2 msg -> BAD_REQUEST msg)    

let php4 =
    request (fun r ->
        match r.queryParam "playerName" with
           | Choice1Of2 playerName -> 
               match r.queryParam "gameKey" with
               | Choice1Of2 gameKey -> OK (g.joinPlayer1OnGameAndReturnJsonString(playerName,gameKey)) //(sprintf "playerKey: %s actionType: %s" playerName gameKey)
               | Choice2Of2 msg -> BAD_REQUEST msg
           | Choice2Of2 msg -> BAD_REQUEST msg)  

let MimeJSON = Writers.setMimeType "application/json"

let webPart = 
    choose [
        path "/" >=> (OK "Home")
        path "/elm/api/create.php" >=> php >=> MimeJSON >=> setHeader "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS" >=> setHeader "Access-Control-Allow-Credentials" "true" >=> setHeader "Access-Control-Allow-Headers" "Content-Type" >=> setHeader "Access-Control-Allow-Origin" "*"
        path "/elm/api/join.php" >=> php4 >=> MimeJSON >=> setHeader "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS" >=> setHeader "Access-Control-Allow-Credentials" "true" >=> setHeader "Access-Control-Allow-Headers" "Content-Type" >=> setHeader "Access-Control-Allow-Origin" "*"
        path "/elm/api/info.php" >=> php2 >=> MimeJSON >=> setHeader "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS" >=> setHeader "Access-Control-Allow-Credentials" "true" >=> setHeader "Access-Control-Allow-Headers" "Content-Type" >=> setHeader "Access-Control-Allow-Origin" "*"
        path "/elm/api/action.php" >=> php3 >=> MimeJSON >=> setHeader "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS" >=> setHeader "Access-Control-Allow-Credentials" "true" >=> setHeader "Access-Control-Allow-Headers" "Content-Type" >=> setHeader "Access-Control-Allow-Origin" "*"
    ]

startWebServer defaultConfig webPart