module prog

open Suave
open Suave.Successful
open Suave.Filters
open Suave.Operators
open Suave.RequestErrors
//on import le fsharp data pour le json
open FSharp.Data
//on importe la classe qu'on a créé
open game


//http://localhost:8083/elm/api/create.php?playerName=" ++ name
//http://localhost/elm/apiPHP/info.php?gameKey
//http://localhost:8083/elm/api/action.php?playerKey="++ playerKey ++ "&actionType=" ++ actionType ++ "&actionInfo=" ++ actionInfo
//http://localhost:8083/elm/apiPHP/join.php?playerName=" ++ name ++ "&gameKey=" ++ gameKey

let php =
    request (fun r ->
        match r.queryParam "playerName" with
        | Choice1Of2 name -> OK (sprintf "playerName: %s" name)
        | Choice2Of2 msg -> BAD_REQUEST msg)    

//on crée le fichier json "create"
(*let create = JsonValue.Parse(""" 
    { "playerName1": "Tomas", "gameKey": "1dz9dayzgdya8OA53" }""")*)

let php2 =
    request (fun r ->
        match r.queryParam "gameKey" with
        | Choice1Of2 key -> OK (sprintf "gameKey: %s" key)
        | Choice2Of2 msg -> BAD_REQUEST msg)       

//on crée le fichier json "join"
(*let join = JsonValue.Parse(""" 
    { "playerName2": "Mat", "gameKey": "1dz9dayzgdya8OA53" }""")*)

let php3 =
    request (fun r ->
        match r.queryParam "playerKey" with
           | Choice1Of2 playerKey -> 
               match r.queryParam "actionType" with
               | Choice1Of2 actionType -> 
                   match r.queryParam "actionInfo" with
                      | Choice1Of2 actionInfo -> OK (sprintf "playerKey: %s actionType: %s actionInfo: %s" playerKey actionType actionInfo)
                      | Choice2Of2 msg -> BAD_REQUEST msg
               | Choice2Of2 msg -> BAD_REQUEST msg
           | Choice2Of2 msg -> BAD_REQUEST msg)    

let php4 =
    request (fun r ->
        match r.queryParam "playerName" with
           | Choice1Of2 playerName -> 
               match r.queryParam "gameKey" with
               | Choice1Of2 gameKey -> OK (sprintf "playerKey: %s actionType: %s" playerName gameKey)
               | Choice2Of2 msg -> BAD_REQUEST msg
           | Choice2Of2 msg -> BAD_REQUEST msg)  

//on crée le fichier json "info"
(*let info = JsonValue.Parse(""" 
    {
  "gameKey" : "1337gameKey123",
  "status" : "PLAYER_1_WON",
  "playerName1":"Kavlo",
  "playerName2":"Arkkun",
  "gridOrientation":"horizontal",
  "gridCells":[
   "no","no","no","no","no","no","no"
  ,"no","p2","no","no","no","no","no"
  ,"no","p2","p1","no","no","no","no"
  ,"no","p2","p1","p1","p2","no","no"
  ,"p1","p1","p2","p2","p1","no","no"
  ,"p2","p1","p2","p1","p1","p2","no"]
}""")*)

let webPart = 
    choose [
        path "/" >=> (OK "Home")
        //path "/elm/api/create/browse" >=> browse
        path "/elm/api/create.php" >=> php
        path "/elm/api/info.php" >=> php2
        path "/elm/api/action.php" >=> php3
        path "/elm/api/join.php" >=> php4
        pathScan "/store/details/%d" (fun id -> OK (sprintf "Details %d" id))
    ]
//let php =
  //  request (fun r ->
        ///match r.queryParam "playerName" with
      //  | Choice1Of2 name -> OK (sprintf "Nom de la partie: %s" name)
    //    | Choice2Of2 msg -> BAD_REQUEST msg)

//let webPart = 
  //  choose [
        //path "/" >=> (OK "Home")        
        //path "localhost:8083/front/main.html" >=> (OK "Home")
        //path "localhost:8083/front/main.html" >=> (OK "Home")
        //path "/elm/api/create/php?playerName=%s" >=> php
        //path "/elm/api/create.php?browse" >=> browse
        
       // pathScan "/elm/api/create.php?playerName=%s" (fun name -> OK (sprintf "Nom de la partie %s" name))
       // path "/store" >=> (OK "Store")
       // path "/store/browse" >=> browse
          //pathScan "localhost:8383/elm/apiPHP/create.php?playerName=%s" (fun name -> OK (sprintf "Details %s" name))
          //pathScan "http://localhost:8383/elm/apiPHP/create.php?playerName=%s" (fun id -> OK (sprintf "Details %s" name))
          //pathScan "http://localhost:8383/elm/apiPHP/create.php?playerName=%s" ++ name
          //path "http://localhost:8383/elm/apiPHP/join.php?playerName=" ++ name ++ "&gameKey=" ++ gameKey
          //path "http://localhost:8383/elm/apiPHP/action.php?playerKey=" ++ playerKey ++ "&actionType=" ++ actionType ++ "&actionInfo=" ++ actionInfo
          //path "http://localhost:8383/elm/apiPHP/info.php?gameKey=" ++ gameKey

       // pathScan "/store/details/%d" (fun id -> OK (sprintf "Details %d" id))
   // ]

startWebServer defaultConfig webPart