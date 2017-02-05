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
   val mutable lastPlayer : string
    //les constructeurs , par défaut et surchargés
   new() as this =
      { gameId = (-1); gameStatus = "?"; gridOrientation = "?"; gridCells = Array.create 42 "no";player1Key="?";player2Key ="?";playerName1="?";playerName2 ="?"; gameKey="?";lastPlayer="?"}

   new(id, p1) as this =
      { gameId = id + 1; gameStatus = "WAITING"; gridOrientation = "vertical";gridCells = Array.create 42 "no";player1Key= this.generateRandomKey() ;player2Key ="?"; playerName1 = p1;playerName2 ="?"; gameKey = this.generateRandomKey(); lastPlayer=""}
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
       this.gridOrientation <- "vertical"

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
       let mutable col = c
       if(this.gridOrientation.Equals("horizontal")) then
           if (col <=  6 ) then
               if(playerName = (this.playerName1)) then
                   this.gravityOnAddedToken(col,"p1")
                   this.nextPlayerToSetAction()
               elif(playerName = (this.playerName2)) then                   
                   this.gravityOnAddedToken(col,"p2")
                   this.nextPlayerToSetAction()
       elif(this.gridOrientation = ("vertical")) then
           if (col <=  5 ) then
               if(playerName = (this.playerName1)) then
                   this.gravityOnAddedToken(col,"p1")
                   this.nextPlayerToSetAction()
               elif(playerName = (this.playerName2)) then                   
                   this.gravityOnAddedToken(col,"p2")  
                   this.nextPlayerToSetAction()

    //Méthode récursive appelé pour l'ajout des jetons
   member this.WhileRecurMethod ( c: int, t:string,colI:int):unit =
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
        if(this.gridOrientation = "vertical") then
            this.WhileRecurMethod(cI,tokenType,6)
        else
            this.WhileRecurMethod(cI,tokenType,7)

   //Lorsqu'on a retourné un tableau on l'appel pour y appliquer la gravité
   member this.applyGravityOnTurnedGrid():unit =       
       if(this.gridOrientation.Equals("horizontal")) then
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
       elif(this.gridOrientation.Equals("vertical") ) then
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
            this.gridCells.[count] <- tmp.[orientationTab.[41-count]]
            count <- count + 1
            this.recursiveTurnToLeft(tmp,count,orientationTab)

    member this.recursiveTurnToRight(temporaryTab: string[],c: int,orientationTab: int[]):unit =        
        let mutable count = c
        let mutable tmp: string[] = temporaryTab
        if(count < 42) then
            this.gridCells.[count] <- tmp.[orientationTab.[count]]
            count <- count + 1
            this.recursiveTurnToRight(tmp,count,orientationTab)

     //Methode turnToLeft, qui tourne le tableau à 90° vers la gauche et applique la gravité
    member this.turnGridToLeft(playerName:string):unit =
        //on crée un tableau temporaire
        let mutable tmp = Array.create 42 "no"
        this.copyThisGridInto(tmp,0)
        Array.fill this.gridCells 0 42 "no"        
        let tv = [|36;30;24;18;12;6;0;37;31;25;19;13;7;1;38;32;26;20;14;8;2;39;33;27;21;15;9;3;40;34;28;22;16;10;4;41;35;29;23;17;11;5|]
        let th = [|35;28;21;14;7;0;36;29;22;15;8;1;37;30;23;16;9;2;38;31;24;17;10;3;39;32;25;18;11;4;40;33;26;19;12;5;41;34;27;20;13;6|]
        if(this.gridOrientation.Equals("horizontal")) then
            this.gridOrientation <- "vertical"
            let mutable counter = 0
            //On retourne d'abord le tableau, on le passe 6colx7line en 7col x 6line
            this.recursiveTurnToLeft(tmp,counter,th)
            //ensuite on applique la gravité à chaque pions
            this.applyGravityOnTurnedGrid()
            this.nextPlayerToSetAction()
        elif(this.gridOrientation.Equals("vertical")) then            
            this.gridOrientation <- "horizontal"
            let mutable counter = 0
            //On retourne d'abord le tableau, on le passe 6colx7line en 7col x 6line
            this.recursiveTurnToLeft(tmp,counter,tv)
            //ensuite on applique la gravité à chaque pions
            this.applyGravityOnTurnedGrid()
            this.nextPlayerToSetAction()

    //on l'appel pour remplir la liste de valeur a appliquer
    // callagain methode
    member this.recursiveAddInfoToTuple(count:int, gridToSaveOnTuple:string[], loi:int[],lov:string[],c:int,l:int,countCol:int):unit =
        let mutable aCounter = count
        let mutable listOfColumns = loi
        let mutable listOfValues = lov
        let mutable col = c
        let mutable line =l
        if(aCounter >= 0) then
            Array.set listOfColumns aCounter col
            Array.set listOfValues aCounter gridToSaveOnTuple.[aCounter]
            col <- col - 1
            if(col <0) then
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
            this.gravityOnAddedToken(listOfColumns.[41-newCounter],listOfValues.[41-newCounter])
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
       Array.fill this.gridCells 0 42 "no"        
       let tv = [|36;30;24;18;12;6;0;37;31;25;19;13;7;1;38;32;26;20;14;8;2;39;33;27;21;15;9;3;40;34;28;22;16;10;4;41;35;29;23;17;11;5|]
       let th = [|35;28;21;14;7;0;36;29;22;15;8;1;37;30;23;16;9;2;38;31;24;17;10;3;39;32;25;18;11;4;40;33;26;19;12;5;41;34;27;20;13;6|]

       //if horizontal ==> vertical
       if(this.gridOrientation.Equals("horizontal")) then
           this.gridOrientation <- "vertical"
           let mutable counter = 0
           //On retourne d'abord le tableau, on le passe 6colx7line en 7col x 6line
           this.recursiveTurnToRight(tmp,counter,th)
           //ensuite on applique la gravité à chaque pions
           this.applyGravityOnTurnedGrid()
           this.nextPlayerToSetAction()
       elif(this.gridOrientation.Equals("vertical")) then
           this.gridOrientation <- "horizontal"
           let mutable counter = 0
           //On retourne d'abord le tableau, on le passe 6colx7line en 7col x 6line
           this.recursiveTurnToRight(tmp,counter,tv)
           //ensuite on applique la gravité à chaque pions
           this.applyGravityOnTurnedGrid()
           this.nextPlayerToSetAction()

    member this.setFirstPlayerAndReturnJson( player1:string):string =
       this.setFirstPlayer(player1)
       let createJson:create = { player1Key = this.playerName1; gameKey = this.gameKey }
       let json:string = Newtonsoft.Json.JsonConvert.SerializeObject(createJson)
       (json)

    member this.joinPlayer1OnGameAndReturnJsonString( player2:string,gameKey:string):string =
       this.joinGame(player2,gameKey)   
       this.gameStatus <- "TURN_PLAYER_1"
       this.lastPlayer <- this.playerName1
       let joinJson:join = { player2Key = this.player2Key}
       let json:string = Newtonsoft.Json.JsonConvert.SerializeObject(joinJson)
       (json)

    member this.infoGameAndReturnJsonString():string =
       let infoGameJson:information = { key = this.gameKey; status = this.gameStatus; player1Name=this.playerName1;player2Name=this.playerName2; gridOrientation= this.gridOrientation; gridCells = this.gridCells}
       let json:string = Newtonsoft.Json.JsonConvert.SerializeObject(infoGameJson)
       (json)
 
    member this.actionGameReturnJsonString(playerKey:string,actionType:string,actionInfo:string):string =
       if(playerKey = this.playerName1)then
           if(actionType = "token")then
               this.playerAction(this.playerName1,System.Int32.Parse(actionInfo))
           elif(actionType = "turn")then
               if(actionInfo = "right")then
                   this.turnGridToRight(this.playerName1)
               elif(actionInfo = "left")then
                   this.turnGridToLeft(this.playerName1)
       elif(playerKey = this.playerName2 || playerKey = this.player2Key)then
           if(actionType = "token")then
               this.playerAction(this.playerName2,System.Int32.Parse(actionInfo))
           elif(actionType = "turn")then
               if(actionInfo = "right")then
                   this.turnGridToRight(this.playerName2)
               elif(actionInfo = "left")then
                   this.turnGridToLeft(this.playerName2)
       //on check si il y a une victoire après qu'un joueur ai fini son tour
       let mutable counterL = 41
       this.recursiveCheckGridCells(counterL)
       let actionGameJson:action = { key = this.gameKey; status = this.gameStatus; player1Name=this.playerName1;player2Name=this.playerName2; gridOrientation= this.gridOrientation; gridCells = this.gridCells}
       let json:string = Newtonsoft.Json.JsonConvert.SerializeObject(actionGameJson)
       (json)

    member this.recursiveCheckGridCells(count:int):unit =
        let mutable lastCounter = count
        if(lastCounter >= 0)then
            this.checkGrid(lastCounter)
            lastCounter <- lastCounter - 1
            this.recursiveCheckGridCells(lastCounter)

    member this.nextPlayerToSetAction():unit =
       if(this.lastPlayer = this.playerName1) then
           this.gameStatus <- "TURN_PLAYER_2"
           this.lastPlayer <- this.playerName2
       elif(this.lastPlayer = this.playerName2)then
           this.gameStatus <- "TURN_PLAYER_1"
           this.lastPlayer <- this.playerName1

    member this.checkNext4Cells(x:int,cardinalValue:int,t:string[],c:int,countCheck:int) =
        let mutable countChecker = countCheck
        let mutable count = c
        let mutable x = x
        if(x+cardinalValue < 42 && x+cardinalValue >= 0 ) then
            if(t.[x+cardinalValue] = t.[x] && not(t.[x] = "no") ) then
                count <- count+1
            elif(not(t.[x+cardinalValue] = t.[x] ))then
                countChecker<-80
        if(count = 4) then
            if(t.[x]="p1") then
                this.gameStatus <- "PLAYER_1_WON"
                printfn "on a un gagnant au %i" cardinalValue
            elif(t.[x]="p2") then
                this.gameStatus <- "PLAYER_2_WON"
                printfn "on a un gagnant au %i" cardinalValue
        elif(count < 4 && (x+cardinalValue) < 42  && (x+cardinalValue) >= 0  && countChecker <= 3) then
            x <- (x + cardinalValue)
            countChecker <- countChecker + 1
            this.checkNext4Cells(x,cardinalValue,t,count,countCheck)

    //Methode recursive checkGrid, verifie si il y a une ligne de 4
    member this.checkGrid(ind:int):unit =
        let mutable index = ind        
        let mutable cs=1
        let mutable cne=1
        let mutable cnw=1
        let mutable cse=1
        let mutable csw=1
        let mutable ce=1
        let mutable cw=1
        let mutable cn=1
        let t = this.gridCells

        if(this.gridOrientation="vertical")then
            let mutable northValue:int = (-6)
            let mutable northEastValue=(-5)
            let mutable northWestValue=(-7)
            let mutable southEastValue=7
            let mutable southWestValue=5
            let mutable southValue=6
            let mutable eastValue=1
            let mutable westValue=(-1)

            //Verification OUEST              
            if(index+westValue>41 || index=0 || index=6 || index=12 || index=18 || index=24 || index=30 || index=36 || (t.[index] ="no"))then
                printfn "On ne peut pas verifier sur la gauche "
            elif(index+westValue< 42 && index+westValue >= 0)then
                if(index+(3*westValue) < 42 && index+(3*westValue) >= 0 )then
                    this.checkNext4Cells(index,westValue,t,cw,0)
            //Verification NORD OUEST 
            if(index+northWestValue<0 || index=0 || index=1 || index=2 || index=3 || index=4 || index=5 || index=6 || index=12 || index=18 || index=24 || index=30 || index=36 || (t.[index] ="no"))then
                printfn "On ne peut pas verifier sur le HAUT GAUCHE "
            elif( index+northWestValue >= 0)then
                if( index+(3*northWestValue) >= 0)then
                    this.checkNext4Cells(index,northWestValue,t,cnw,0)
            //Verification NORD EST 
            if(index+northEastValue<0 || index=41 || index=0|| index=1|| index=2  || index=3 || index=4 || index=5 || index=11|| index=17 || index=23 || index=29 || index=35|| index=41 ||(t.[index] ="no"))then
                printfn "On ne peut pas verifier sur la HAUT DROITE "
            elif(index+northEastValue >= 0)then
                if( index+(3*northEastValue) >= 0)then
                    this.checkNext4Cells(index,northEastValue,t,cne,0)
            //Verification NORD 
            if(index+northValue<0 || index=0|| index=1|| index=2  || index=3 || index=4 || index=5 || (t.[index] ="no"))then
                printfn "On ne peut pas verifier sur la HAUT "
            elif(index+northValue >= 0)then
                printfn "On  peut verifier sur lE HAUT "
                if( index+(3*northValue) >= 0)then
                    this.checkNext4Cells(index,northValue,t,cne,0)
        
        if(this.gridOrientation="horizontal")then
            let mutable northValue:int = (-7)
            let mutable northEastValue=(-6)
            let mutable northWestValue=(-8)
            let mutable southEastValue=8
            let mutable southWestValue=6
            let mutable southValue=7
            let mutable eastValue=1
            let mutable westValue=(-1)

            //Verification OUEST              
            if(index+westValue>41 || index=0 || index=7 || index=14 || index=21 || index=28 || index=35 || (t.[index] ="no") )then
                printfn "On ne peut pas verifier sur la gauche "
            elif(index+westValue< 42 && index+westValue >= 0)then
                if(index+(3*westValue) < 42 && index+(3*westValue) >= 0 )then
                    this.checkNext4Cells(index,westValue,t,cw,0)
            //Verification NORD OUEST 
            if(index+northWestValue<0 || index=0 || index=1 || index=2 || index=3 || index=4 || index=5 || index=6 || index=7 || index=14 || index=21 || index=28 || index=35 )then
                printfn "On ne peut pas verifier sur le HAUT GAUCHE "
            elif( index+northWestValue >= 0)then
                if(not(t.[index] ="no") && index+(3*northWestValue) >= 0)then
                    this.checkNext4Cells(index,northWestValue,t,cnw,0)
            //Verification NORD EST 
            if(index+northEastValue<0 || index=0|| index=1|| index=2  || index=3 || index=4 || index=5 || index=6|| index=13 || index=20 || index=27 || index=34|| index=41 )then
                printfn "On ne peut pas verifier sur la HAUT DROITE "
            elif(index+northEastValue >= 0)then
                if( not(t.[index] ="no") && index+(3*northEastValue) >= 0)then
                    this.checkNext4Cells(index,northEastValue,t,cne,0)
            //Verification NORD 
            if(index+northValue<0 || index=6 || index=0|| index=1|| index=2  || index=3 || index=4 || index=5 )then
                printfn "On ne peut pas verifier sur la HAUT "
            elif(index+northValue >= 0)then
                if( not(t.[index] ="no") && index+(3*northValue) >= 0)then
                    this.checkNext4Cells(index,northValue,t,cne,0)                 
end