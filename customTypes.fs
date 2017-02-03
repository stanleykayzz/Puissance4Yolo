module customTypes

type create = {
    player1Key : string
    gameKey: string
}

type join = {
   player2Key: string
}

type information = {
  key : string
  status : string
  player1Name:string
  player2Name:string
  gridOrientation:string
  gridCells : string[]
}

type action = {
  key : string
  status : string
  player1Name:string
  player2Name:string
  gridOrientation:string
  gridCells : string[]
}


