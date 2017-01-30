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

     //Methode turnToLeft, qui tourne le tableau à 90° vers la gauche et applique la gravité
    member this.turnGridToLeft(playerName:string):unit =
        //on crée un tableau temporaire
        let mutable tmp = this.gridCells
        Array.fill this.gridCells 0 42 "no"
        //this.gridCells = Array.create 41 "no"
        let tv = [|36;30;24;18;12;6;0;37;31;25;19;13;7;1;38;32;26;20;14;8;2;39;33;27;21;15;9;3;40;34;28;22;16;10;4;41;35;29;23;17;11;5|]
        let th = [|35;28;21;14;7;0;36;29;22;15;8;1;37;30;23;16;9;2;38;31;24;17;10;3;39;32;25;18;11;4;40;33;26;19;12;5;41;34;27;20;13;6|]

        //if horizontal ==> vertical
        if(this.gridOrientation.Equals("Horizontale")) then
            this.gridOrientation <- "Verticale"
            
        elif(this.gridOrientation.Equals("Verticale")) then
            this.gridOrientation <- "Horizontale"
            let mutable counter = 0
            this.gridCells <- Array.create 42 "no"
            //On retourne d'abord le tableau, on le passe 6colx7line en 7col x 6line
            this.recursiveTurnToLeft(tmp,counter,tv)
            //ensuite on applique la gravité à chaque pions
            this.applyGravityOnTurnedGrid()
    
    //on l'appel pour remplir la liste de valeur a appliquer
    // callagain methode
    member this.recursiveAddInfoToTuple(count:int, gridToSaveOnTuple:string[], loi:int[],lov:string[],c:int,l:int):unit =
        let mutable aCounter = count
        let mutable listOfColumns = loi
        let mutable listOfValues = lov
        let mutable col = c
        let mutable line =l
        if(aCounter >= 0) then
            printfn " à la valeur %i de line %i et de colonne %i on a  %s" aCounter line col gridToSaveOnTuple.[aCounter]
            Array.set listOfColumns aCounter col
            Array.set listOfValues aCounter gridToSaveOnTuple.[aCounter]
            //list.push([c,t[i]])
            col <- col - 1
            if(aCounter <0) then
                aCounter <- 5
                line <- line - 1
            aCounter <- aCounter - 1
            this.recursiveAddInfoToTuple(aCounter,gridToSaveOnTuple,listOfColumns,listOfValues,col,line)


end