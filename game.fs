module game
open Newtonsoft.Json
open customTypes
//on crée la classe métier 
type Game = class
   val mutable gameId : int
   val mutable gameStatus : string
   val mutable gridOrientation : string
   val mutable gridCells : string[]
   val mutable playerName1 : string
   val mutable playerName2 : string
   val mutable player1Key : string
   val mutable player2Key : string
   val mutable gameKey : string
    //les constructeurs , par défaut et surchargés
   new() as this =
      { gameId = (-1); gameStatus = "?"; gridOrientation = "?"; gridCells = Array.create 42 "no";player1Key="?";player2Key ="?";playerName1="?";playerName2 ="?"; gameKey="?"}

   new(id, p1) as this =
      { gameId = id + 1; gameStatus = "WAITING"; gridOrientation = "Verticale";gridCells = Array.create 42 "no";player1Key= this.generateRandomKey() ;player2Key ="?"; playerName1 = p1;playerName2 ="?"; gameKey = this.generateRandomKey()}
      then
         printfn " Objet créé avec: {(gameid : %i, player1 :%s), ( gameKey : %s , orientation grid :%s)}"
            this.gameId this.playerName1 this.gameKey this.gridOrientation
     //Methode Join, permet a un joueur 2 de rejoindre une partie
   // Vérifier que nom different du createur (!) 
   //ajoute nom du joueur 2 à l’objet game. choisis qui commence et met l’état correspondant 
   member this.joinGame (p2:string, key:string) :unit= 
          this.playerName2 <- p2
          this.player2Key <- this.generateRandomKey()

   //on ajoute un player 1
   member this.setFirstPlayer(p1:string):unit =
       this.gameKey <- this.generateRandomKey()
       this.gameStatus <- "WAITING"
       this.playerName1 <- p1
       this.player1Key <- this.generateRandomKey()
   //on ajoute un id 
   member this.setId(id:int):unit =
      this.gameId <- id + 1
   member this.generateRandomKey():string =
      let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
      let random = new System.Random()
      seq {
          for i in {0..7} do
              yield chars.[random.Next(chars.Length)]
      } |> Seq.toArray |> (fun x -> new string(x))

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
           printfn "GRAVITE SUR TURNED GRID HORIZONTALE"
           let mutable tmp = Array.copy this.gridCells
           Array.fill this.gridCells 0 42 "no"
           let mutable colIndexH = 6
           let mutable countCol = 6
           let mutable lineIndex = 5
           let mutable counter = 41
           let mutable listVal:string[] = Array.create 42 "no"
           let mutable listIndex:int[] = Array.create 42 0 
           this.recursiveAddInfoToTuple(counter,tmp,listIndex,listVal,colIndexH,lineIndex,countCol)
           let mutable a = 0
           this.applyGravityOnEachTokenOfTurnedGrid(a,listIndex,listVal)
       elif(this.gridOrientation.Equals("Verticale") ) then
           printfn "GRAVITE SUR TURNED GRID VERTICALE"
           //let mutable tmp = this.gridCells
           let mutable tmp = Array.copy this.gridCells
           Array.fill this.gridCells 0 42 "no"
           let mutable colIndexV = 5
           let mutable countCol = 5
           let mutable lineIndex = 6
           let mutable counter = 41
           let mutable listVal:string[] = Array.create 42 "no"
           let mutable listIndex:int[] = Array.create 42 0 
           this.recursiveAddInfoToTuple(counter,tmp,listIndex,listVal,colIndexV,lineIndex,countCol)
           let mutable a = 0
           this.applyGravityOnEachTokenOfTurnedGrid(a,listIndex,listVal)

    member this.recursiveTurnToLeft(temporaryTab: string[],c: int,orientationTab: int[]):unit =        
        let mutable count = c
        let mutable tmp: string[] = temporaryTab
        if(count < 42) then
            printfn "a %i on met la val %s qui correspond a %i avec i = %i" count tmp.[orientationTab.[41-count]] orientationTab.[41-count] (41-count)
            this.gridCells.[count] <- tmp.[orientationTab.[41-count]]
            count <- count + 1
            this.recursiveTurnToLeft(tmp,count,orientationTab)

    member this.recursiveTurnToRight(temporaryTab: string[],c: int,orientationTab: int[]):unit =        
        let mutable count = c
        let mutable tmp: string[] = temporaryTab
        if(count < 42) then
            printfn "a %i on met la val %s qui correspond a %i avec i = %i" count tmp.[orientationTab.[41-count]] orientationTab.[41-count] (41-count)
            this.gridCells.[count] <- tmp.[orientationTab.[count]]
            count <- count + 1
            this.recursiveTurnToRight(tmp,count,orientationTab)

     //Methode turnToLeft, qui tourne le tableau à 90° vers la gauche et applique la gravité
    member this.turnGridToLeft(playerName:string):unit =
        //on crée un tableau temporaire
        let mutable tmp = Array.create 42 "no"
        this.copyThisGridInto(tmp,0)
        printfn " on vide le tableau this.gridcells"
        Array.fill this.gridCells 0 42 "no"        
        let tv = [|36;30;24;18;12;6;0;37;31;25;19;13;7;1;38;32;26;20;14;8;2;39;33;27;21;15;9;3;40;34;28;22;16;10;4;41;35;29;23;17;11;5|]
        let th = [|35;28;21;14;7;0;36;29;22;15;8;1;37;30;23;16;9;2;38;31;24;17;10;3;39;32;25;18;11;4;40;33;26;19;12;5;41;34;27;20;13;6|]
        if(this.gridOrientation.Equals("Horizontale")) then
            this.gridOrientation <- "Verticale"
            let mutable counter = 0
           // this.gridCells <- Array.create 42 "no"
            //On retourne d'abord le tableau, on le passe 6colx7line en 7col x 6line
            this.recursiveTurnToLeft(tmp,counter,th)
            //ensuite on applique la gravité à chaque pions
            this.applyGravityOnTurnedGrid()   
            printfn " ON RENTRE UNE FOIS DANS HORIZONTALE"
        elif(this.gridOrientation.Equals("Verticale")) then            
            printfn " ON RENTRE UNE FOIS DANS VERTICALE"
            this.gridOrientation <- "Horizontale"
            let mutable counter = 0
           // this.gridCells <- Array.create 42 "no"
            //On retourne d'abord le tableau, on le passe 6colx7line en 7col x 6line
            this.recursiveTurnToLeft(tmp,counter,tv)
            //ensuite on applique la gravité à chaque pions
            this.applyGravityOnTurnedGrid()
    //on l'appel pour remplir la liste de valeur a appliquer
    // callagain methode
    member this.recursiveAddInfoToTuple(count:int, gridToSaveOnTuple:string[], loi:int[],lov:string[],c:int,l:int,countCol:int):unit =
        let mutable aCounter = count
        let mutable listOfColumns = loi
        let mutable listOfValues = lov
        let mutable col = c
        printfn " ADD TO TUUUUPLE ON A col a %i" col
        let mutable line =l
        if(aCounter >= 0) then
            printfn " à la valeur %i de line %i et de colonne %i on a  %s" aCounter line col gridToSaveOnTuple.[aCounter]
            Array.set listOfColumns aCounter col
            Array.set listOfValues aCounter gridToSaveOnTuple.[aCounter]
            //list.push([c,t[i]])
            col <- col - 1
            if(col <0) then
                //aCounter <- col
                col <- countCol
                line <- line - 1
            aCounter <- aCounter - 1
            this.recursiveAddInfoToTuple(aCounter,gridToSaveOnTuple,listOfColumns,listOfValues,col,line,countCol)

    //callA
    //on appel la methoe qu va permettre d'appliqué la gravité sur chaque pion du tableau retourné
    member this.applyGravityOnEachTokenOfTurnedGrid(newC, listCols:int[],listVals:string[]) :unit =
        let mutable newCounter = newC
        let mutable listOfColumns = listCols
        let mutable listOfValues = listVals

        if( newCounter < listOfColumns.Length) then
            this.gravityOnAddedToken(listOfColumns.[newCounter],listOfValues.[newCounter])
            newCounter <- newCounter + 1
            this.applyGravityOnEachTokenOfTurnedGrid(newCounter, listOfColumns,listOfValues)

    //Methode qui permet de mettre uniquement les valeur du grid dans un autre tableau de string
    member this.copyThisGridInto(c:string[],zeroValue:int):unit =
        let mutable zero = zeroValue
        let mutable copy = c
        if(zero < this.gridCells.Length) then
            copy.[zero] <- this.gridCells.[zero]
            zero <- zero + 1
            this.copyThisGridInto(copy,zero)
    
    //Methode turnToRight, qui tourne le tableau à 90° vers la droite et applique la gravité
    member this.turnGridToRight(playerName:string):unit =
       //on crée un tableau temporaire
       let mutable tmp = Array.create 42 "no"
       this.copyThisGridInto(tmp,0)
       printfn " on vide le tableau this.gridcells"
       Array.fill this.gridCells 0 42 "no"        
       let tv = [|36;30;24;18;12;6;0;37;31;25;19;13;7;1;38;32;26;20;14;8;2;39;33;27;21;15;9;3;40;34;28;22;16;10;4;41;35;29;23;17;11;5|]
       let th = [|35;28;21;14;7;0;36;29;22;15;8;1;37;30;23;16;9;2;38;31;24;17;10;3;39;32;25;18;11;4;40;33;26;19;12;5;41;34;27;20;13;6|]

       //if horizontal ==> vertical
       if(this.gridOrientation.Equals("Horizontale")) then
           this.gridOrientation <- "Verticale"
           printfn " HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH"
           let mutable counter = 0
          // this.gridCells <- Array.create 42 "no"
           //On retourne d'abord le tableau, on le passe 6colx7line en 7col x 6line
           this.recursiveTurnToRight(tmp,counter,th)
           //ensuite on applique la gravité à chaque pions
           this.applyGravityOnTurnedGrid()
       elif(this.gridOrientation.Equals("Verticale")) then
           this.gridOrientation <- "Horizontale"
           printfn " VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV"
           let mutable counter = 0
          // this.gridCells <- Array.create 42 "no"
           //On retourne d'abord le tableau, on le passe 6colx7line en 7col x 6line
           this.recursiveTurnToRight(tmp,counter,tv)
           //ensuite on applique la gravité à chaque pions
           this.applyGravityOnTurnedGrid()

    member this.setFirstPlayerAndReturnJson( player1:string):string =
       this.setFirstPlayer(player1)
       let createJson:create = { player1Key = this.playerName1; gameKey = this.gameKey }
       let json:string = Newtonsoft.Json.JsonConvert.SerializeObject(createJson)
       (json)

    member this.joinPlayer1OnGameAndReturnJsonString( player2:string,gameKey:string):string =
       this.joinGame(player2,gameKey)   
       this.gameStatus <- "TURN_PLAYER_1"    
       let joinJson:join = { player2Key = this.player2Key}
       let json:string = Newtonsoft.Json.JsonConvert.SerializeObject(joinJson)
       (json)

    member this.infoGameAndReturnJsonString():string =
       let infoGameJson:information = { key = this.gameKey; status = this.gameStatus; player1Name=this.playerName1;player2Name=this.playerName2; gridOrientation= this.gridOrientation; gridCells = this.gridCells}
       let json:string = Newtonsoft.Json.JsonConvert.SerializeObject(infoGameJson)
       (json)
    //Methode recursive checkGrid, verifie si il y a une ligne de 4
    
end

let g = new Game(1,"Alvin")

printfn " player 1 : %s" g.playerName1
printfn " player 2 : %s" g.playerName2
g.joinGame("seiya","azyeyazyeyaz")
printfn " player 2 : %s" g.playerName2
printfn " on lance une action du p1 à la col 5"
g.playerAction(g.playerName1, 0)

//ok
g.playerAction(g.playerName1, 4)
g.playerAction(g.playerName1, 3)
g.playerAction(g.playerName1, 1)
g.playerAction(g.playerName1, 2)
g.playerAction(g.playerName1, 5)

for c in g.gridCells do
   printfn "%s" c

//tourner la grille 90° vers la gauche ok ok horizontale et verticale
g.turnGridToLeft(g.playerName2)
//tourner la grille 90° vers la gauche ok ok horizontale et verticale
g.turnGridToRight(g.playerName2)
   
printfn "%i" g.gridCells.Length
printfn "%s" g.gridOrientation
printfn "%s" g.gridCells.[41]
printfn "%s" g.gridCells.[40]
printfn "%s" g.gridCells.[39]
printfn "%s" g.gridCells.[38]
printfn "%s" g.gridCells.[37]


printfn "%b" (g.gridOrientation = "Verticale")