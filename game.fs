module game

 // Définition classe Personne 
//type Personne = class
    // attributs     val private nom : string
 //   val private prénom : string
  //  val private âge : int
    // surcharges constructeurs et valeurs par défaut
   // new(n) = {nom=n; prénom="?"; âge=(-1)} //valeurs par défaut
   // new(n,p) = {nom=n; prénom=p; âge=(-1)} //valeur par défaut
    //new(n,p,â) = {nom=n; prénom=p; âge=â}
    //new(n,p,dateN) = {nom=n; prénom=p;âge=calculÂge(dateN)}
    // accesseurs
 //   member this.Nom = this.nom
   // member this.Prénom = this.prénom
    //member this.Âge = this.âge
//end


type Game = class
   val private playerName1 : string
   val private playerName2 : string
   val private gameKey : string
   //val private verticalH : bool

   // surcharges constructeurs et valeurs par défaut
    new() = {playerName1="?"; playerName2="?"; gameKey="?"} //valeurs par défaut
    new(j1:string, key:string) = {playerName1=j1; playerName2="?"; gameKey=key} //valeurs par défaut

end
