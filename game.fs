module game

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

   //Methode Action, qui permet d'ajouter un pion dans le tableau
   //member this.action (colIndex:int)
   member this.playerAction(playerName:string, col: int ref):unit =
        if(this.gridOrientation.Equals("horizontale")) then
            while (col <= ref 6 ) do
                if(playerName.Equals(this.playerName1)) then
                    this.gravity(col,"p1")
                else
                    this.gravity(col,"p2")
        
   //Methode gravité, qui va faire descendre un pion au dernier utilisé
    // 0 1 2 3 4 5      0 1 2 3 4 5 6 
    // X X X X X X      X X X X X X X
    // X X X X X X      X X X X X X X
    // X X X X X X      X X X X X X X
    // X X X X X X      X X X X X X X
    // X X X X X X      X X X X X X X
    // X X X X X X      X X X X X X X
    // X X X X X X 
    member this.gravity( colIndex: int byref , tokenType:string):unit =
        if(this.gridOrientation.Equals("Horizontale") && colIndex <6) then
            while(this.gridCells.[colIndex].Equals("no") && this.gridCells.[colIndex+6].Equals("no")) do
                colIndex <- colIndex + 6
            this.gridCells.[colIndex] <- tokenType
       else
            if(colIndex <7) then
                while(this.gridCells.[colIndex].Equals("no") && this.gridCells.[colIndex+7].Equals("no")) do
                    colIndex <- colIndex + 7
                this.gridCells.[colIndex] <- tokenType

    //Methode turnToLeft, qui tourne le tableau et applique la gravité

    //Methode turnToRight, qui tourne le tableau et applique la gravité

    //Methode recursive checkGrid, verifie si il y a une ligne de 4

end