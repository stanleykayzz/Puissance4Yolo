<?php

header('Cache-Control: no-cache, must-revalidate');
header('Expires: Mon, 26 Jul 1997 05:00:00 GMT');
header('Content-type: application/json');

header('Access-Control-Allow-Credentials:true');
header('Access-Control-Allow-Headers:Content-Type, Accept');
header('Access-Control-Allow-Methods:GET, POST, PUT, DELETE, OPTIONS');
header('Access-Control-Allow-Origin:*');

/*
Access-Control-Allow-Credentials:true
Access-Control-Allow-Headers:Content-Type, Accept
Access-Control-Allow-Methods:GET, POST, PUT, DELETE, OPTIONS
Access-Control-Allow-Origin:*
*/

 ?>


{
  "key" : "1337gameKey123",
  "status" : "PLAYER_1_WON",
  "player1Name":"Kavlo",
  "player2Name":"Arkkun",
  "gridOrientation":"horizontal",
  "gridCells":[
   "no","no","no","no","no","no","no"
  ,"no","p2","no","no","no","no","no"
  ,"no","p2","p1","no","no","no","no"
  ,"no","p2","p1","p1","p2","no","no"
  ,"p1","p1","p2","p2","p1","no","no"
  ,"p2","p1","p2","p1","p1","p2","no"]
}

 <?php /*
{
  "key" : "1337gameKey123",
  "status" : "PLAYER_1_WON",
  "player1Name":"Kavlo",
  "player2Name":"Arkkun",
  "gridOrientation":"vertical",
  "gridCells":[
   "no","no","no","no","no","no"
  ,"no","no","no","no","no","p2"
  ,"no","no","no","no","p1","p1"
  ,"no","no","no","p2","p2","p1"
  ,"no","no","no","p1","p2","p2"
  ,"no","no","p1","p1","p1","p1"
  ,"no","p2","p2","p2","p1","p"]
}
 */?>
