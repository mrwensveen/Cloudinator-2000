#load "Cloudinator-2000.Game/GameState.fs"
#load "Cloudinator-2000.Game/Parser.fs"
#load "Cloudinator-2000.Game/Collections.fs"
#load "Cloudinator-2000.Game/Mover.fs"

open Cloudinator_2000.Game.GameStateModule
open Cloudinator_2000.Game.ParserModule
open Cloudinator_2000.Game.MoverModule

let str =
    """
    |r̃|ñ|b̃|q̃|k̃|b̃|ñ|r̃|
    |p̃|p̃|p̃|p̃|p̃|p̃|p̃|p̃|
    | | | | | | | | |
    | | | | | | | | |
    | | | | | | | | |
    | | | | | | | | |
    |P̃|P̃|P̃|P̃|P̃|P̃|P̃|P̃|
    |R̃|Ñ|B̃|Q̃|K̃|B̃|Ñ|R̃|
    turn=WHITE
    """

// let str =
//     """
//     |r̃|ñ|b̃|q̃|k̃|b̃|ñ|r̃|
//     |p̃|p̃|p̃|p̃|p̃|p̃|p̃|p̃|
//     | | | | | | | | |
//     | | | | | | | | |
//     | |P| | | | | | |
//     | |↑| | | | | | |
//     |P̃| |P̃|P̃|P̃|P̃|P̃|P̃|
//     |R̃|Ñ|B̃|Q̃|K̃|B̃|Ñ|R̃|
//     turn=WHITE
//     """

let game = Game str

// printfn "%A" game
// printfn "%A" (Seq.toList game.Board.Keys)

// let moves = getPossibleMoves game true |> List.ofSeq
// moves |> Seq.map (snd >> string) |> Seq.iter (printfn "%s")

let mover = RandomPieceMover() :> IMover
mover.Move game |> Option.map (fst >> string >> printfn "%s")
