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

    //Méthode récursive appelé pour l'ajout des jetons
   member this.WhileRecurMethod ( c: int, t:string,colI:int):unit =
        printfn " on est dans la fonction recursive et on a col à %i" c
        let mutable colIndex = c
        let mutable tokenType = t

        if(colIndex+colI < this.gridCells.Length && this.gridCells.[colIndex+colI]="no" && this.gridCells.[colIndex]="no") then
            colIndex <- colIndex + colI
            this.WhileRecurMethod(colIndex,tokenType,colI)
        else
            this.gridCells.[colIndex] <- tokenType
    
    //Methode gravité, qui va faire descendre un pion au dernier utilisé
   member this.gravityOnAddedToken( cI: int , tokenType:string):unit =
        let mutable colIndex = cI
        if(this.gridOrientation = "Verticale") then
            this.WhileRecurMethod(cI,tokenType,6)
        else
            this.WhileRecurMethod(cI,tokenType,7)

     //Lorsqu'on a retourné un tableau on l'appel pour y appliquer la gravité
   member this.applyGravityOnTurnedGrid():unit =       
       if(this.gridOrientation.Equals("Horizontale")) then
           let mutable colIndexH = 6
           let mutable lineIndex = 5
           let mutable counter = 41
           while(counter >= 0) do
               if(colIndexH < 0)then
                   colIndexH <- 5
                   lineIndex <- lineIndex - 1
               counter <- counter - 1
       elif(this.gridOrientation.Equals("Verticale") ) then
           let mutable tmp = this.gridCells
           Array.fill this.gridCells 0 42 "no"

           let mutable colIndexV = 5
           let mutable lineIndex = 6
           let mutable counter = 41
           let mutable listVal:string[] = Array.create 42 "no"
           let mutable listIndex:int[] = Array.create 42 0 
           this.recursiveAddInfoToTuple(counter,tmp,listIndex,listVal,colIndexV,lineIndex)
           let mutable a = 0
           this.applyGravityOnEachTokenOfTurnedGrid(a,listIndex,listVal)

    member this.recursiveTurnToLeft(temporaryTab: string[],c: int,orientationTab: int[]):unit =
        let mutable count = c
        let mutable tmp: string[] = temporaryTab
        if(count < 42) then
            printfn "a %i on met la val %s qui correspond a %i avec i = %i" count temporaryTab.[orientationTab.[41-count]] orientationTab.[41-count] (41-count)
            this.gridCells.[count] <- tmp.[orientationTab.[41-count]]
            count <- count + 1
            this.recursiveTurnToLeft(tmp,count,orientationTab)

end