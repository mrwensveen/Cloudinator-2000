#load "Cloudinator-2000.Game/GameState.fs"
#load "Cloudinator-2000.Game/Parser.fs"

open Cloudinator_2000.Game.GameStateModule
open Cloudinator_2000.Game.ParserModule

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

let moves = getPossibleMoves game true |> List.ofSeq
// moves |> Seq.iter (printfn "%A")
moves |> Seq.map (snd >> string) |> Seq.iter (printfn "%s")

// snd moves[1] |> string |> printfn "%s"
