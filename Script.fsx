// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open tennis
type todo=unit
let todo=()
type Score=
|Zero
|Fifteen
|Thirty
|Fourty

type StartData=Score*Score
type PlayersData=Score*Score
type Player=
|A
|B
type States=
|Start of StartData
|Adv of PlayersData
|Bdv of PlayersData
|Win of Player

// transitions 
let transFromWin (win:Player)=
    Win win
let transFromStart score nextScoreB nextScoreA (start:StartData)=
    let scored=score ()
    if scored=A then 
        start|>nextScoreA|>Adv
    else
        start|>nextScoreB|>Bdv
        
let transFromAdv score isWin nextScoreA nextScoreB aadvByOne (adv:PlayersData):States=
    let scored=score ()
    if scored=A then
        if isWin adv then
            A|>Win
        else
            adv|>nextScoreA|>Adv
    else
        if aadvByOne adv then
            adv|>nextScoreB|>Start
        else
            adv|>nextScoreB|>Bdv
        
let transFromBdv score isWin nextScoreB badvByOne nextScoreA (bdv:PlayersData):States=
    let scored=score ()
    if scored=B then
        if isWin bdv then
            B|>Win
        else
            bdv|>nextScoreB|>Adv
    else
        if badvByOne bdv then
            bdv|>nextScoreA|>Start
        else
            bdv|>nextScoreA|>Bdv
let score ()=
    let r=System.Random()
    if r.Next(0,100) <50 then
        printfn"A"
        A
    else 
        printfn"B"
        B
let nextScore playerScore =
   match playerScore with
   |Zero->Fifteen
   |Fifteen->Thirty
   |Thirty->Fourty
   |Fourty->Fourty
let nextScoreA (adv:PlayersData)=
    let Ascore,Bscore= adv   
    Ascore|>nextScore ,Bscore
let nextScoreB (adv:PlayersData)=
    let Ascore,Bscore= adv  
    Ascore, Bscore|>nextScore 
let isWinA(players:PlayersData) =
   match players with
   |Fourty,_->true
   |_->false
let isWinB(players:PlayersData) =
   match players with
   |_,Fourty->true
   |_->false
let aadvByOne (players:PlayersData)=
    match players with
    |Fourty,Thirty->true
    |Thirty,Fifteen->true
    |Fifteen,Zero->true
    |_->false
let badvByOne (players:PlayersData)=
    match players with
    |Thirty,Fourty->true
    |Fifteen,Thirty->true
    |Zero,Fifteen
    |_->false

let startState=Start (Zero,Zero)

let transFromStart'=transFromStart score nextScoreB nextScoreA 
let transFromAdv' =transFromAdv score isWinA nextScoreA nextScoreB aadvByOne 
let transFromBdv'= transFromBdv score isWinB nextScoreB badvByOne nextScoreA

let x=transFromStart' (Zero,Zero)
let trans state=
    match state with
    |Start s->transFromStart' s
    |Adv a->transFromAdv' a
    |Bdv b->transFromBdv' b
    |Win n->transFromWin n

