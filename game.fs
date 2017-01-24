module game

//objet métier game
//int id 
//string key 
//Stringstatus (WAITING:Attente second joueur, 
//TURN_PLAYER_1:tour joueur1, TURN_PLAYER_2: tour joueur2, PLAYER_1_WON:joueur 1 gagne,PLAYER_2_WON:joueur 2 gagne )
//String gridOrientation “vertical” ou “horizontal” array [42] 
//string gridCells,
//les string valent “no” si vide, 
//“p1” si jeton du joueur1, “p2” si jeton du joueur 2 

type Game = class
   val mutable gameId : int
   val mutable gameStatus : string
   //gameStatus=[|"WAITING"; "TURN_PLAYER_1"; "TURN_PLAYER_2";"PLAYER_1_WON"; "PLAYER_2_WON";|];
   val mutable gridOrientation : string
   val mutable gridCells : string[]
   val mutable playerName1 : string
   val mutable playerName2 : string
   val mutable gameKey : string

   //les constructeurs , par défaut et surchargés
   new() as this =
      { gameId = (-1); gameStatus = "?"; gridOrientation = "?"; gridCells = Array.create 41 "no";playerName1="?";playerName2 ="?"; gameKey="?"}

   new(id, statu,  orientation,p1,key) as this =
      { gameId = id; gameStatus = statu; gridOrientation = orientation;gridCells = Array.create 41 "no"; playerName1 = p1;playerName2 ="?"; gameKey = key;}
      then
         printfn " Objet créé avec: {(gameid : %i, player1 :%s), ( gameKey : %s , orientation grid :%s)}"
            this.gameId this.playerName1 this.gameKey this.gridOrientation 

   //Methode Join, permet a un joueur 2 de rejoindre une partie
   // Vérifier que nom different du createur (!) 
   //ajoute nom du joueur 2 à l’objet game. choisis qui commence et met l’état correspondant 
   member this.joinGame (p2:string, key:string) :unit= 
      if not(p2.Equals( this.playerName1)) then
          this.playerName2 <- p2
      else
          printf "Les deux joueurs ne doivent pas avoir le même pseudo"
   //Methode gravité, qui va faire descendre un pion au dernier utilisé


    //Methode turnToLeft, qui tourne le tableau et applique la gravité

    //Methode turnToRight, qui tourne le tableau et applique la gravité

    //Methode recursive checkGrid, verifie si il y a une ligne de 4

end