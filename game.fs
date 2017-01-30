module game
//on crée la classe métier 
type Game = class
   val mutable gameId : int
   val mutable gameStatus : string
   val mutable gridOrientation : string
   val mutable gridCells : string[]
   val mutable playerName1 : string
   val mutable playerName2 : string
   val mutable gameKey : string
    //les constructeurs , par défaut et surchargés
   new() as this =
      { gameId = (-1); gameStatus = "?"; gridOrientation = "?"; gridCells = Array.create 42 "no";playerName1="?";playerName2 ="?"; gameKey="?"}

   new(id, statu,  p1,key) as this =
      { gameId = id; gameStatus = statu; gridOrientation = "Verticale";gridCells = Array.create 42 "no"; playerName1 = p1;playerName2 ="?"; gameKey = key;}
      then
         printfn " Objet créé avec: {(gameid : %i, player1 :%s), ( gameKey : %s , orientation grid :%s)}"
            this.gameId this.playerName1 this.gameKey this.gridOrientation
     //Methode Join, permet a un joueur 2 de rejoindre une partie
   // Vérifier que nom different du createur (!) 
   //ajoute nom du joueur 2 à l’objet game. choisis qui commence et met l’état correspondant 
   member this.joinGame (p2:string, key:string) :unit= 
      if not(p2 = this.playerName1) then
          this.playerName2 <- p2
      else
          printf "Les deux joueurs ne doivent pas avoir le même pseudo"    
   //Methode Action, qui permet d'ajouter un pion dans le tableau
   //member this.action (colIndex:int)
   member this.playerAction(playerName:string, c: int ):unit =
       printfn " on est dans player action"
       let mutable col = c
       if(this.gridOrientation.Equals("Horizontale")) then
           printfn "le tableau est Horizontale , on doit passer une valeur de %s à la col %i"  playerName col
           if (col <=  6 ) then
               if(playerName = (this.playerName1)) then
                   printfn " token prends la valeur p1"
                   this.gravityOnAddedToken(col,"p1")
               elif(playerName = (this.playerName2)) then                   
                   printfn " token prends la valeur p2"
                   this.gravityOnAddedToken(col,"p2") 
       elif(this.gridOrientation = ("Verticale")) then
           printfn "le tableau est Verticale , on doit passer une valeur de %s à la col %i"  playerName col
           if (col <=  5 ) then
               if(playerName = (this.playerName1)) then
                   printfn " token prends la valeur p1"
                   this.gravityOnAddedToken(col,"p1")
               elif(playerName = (this.playerName2)) then                   
                   printfn " token prends la valeur p2"
                   this.gravityOnAddedToken(col,"p2")  

end