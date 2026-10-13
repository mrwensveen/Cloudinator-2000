#load "GameState.fs"
#load "Parser.fs"

open Cloudinator_2000.Game.GameStateModule
open Cloudinator_2000.Game.ParserModule
open System.Text.RegularExpressions

// let str =
//     """
//     |r̃|ñ|b̃|q̃|k̃|b̃|ñ|r̃|
//     |p̃|p̃|p̃|p̃|p̃|p̃|p̃|p̃|
//     | | | | | | | | |
//     | | | | | | | | |
//     | | | | | | | | |
//     | | | | | | | | |
//     |P̃|P̃|P̃|P̃|P̃|P̃|P̃|P̃|
//     |R̃|Ñ|B̃|Q̃|K̃|B̃|Ñ|R̃|
//     turn=WHITE
//     """

let str =
    """
    |r̃|ñ|b̃|q̃|k̃|b̃|ñ|r̃|
    |p̃|p̃|p̃|p̃|p̃|p̃|p̃|p̃|
    | | | | | | | | |
    | | | | | | | | |
    | |P| | | | | | |
    | |↑| | | | | | |
    |P̃| |P̃|P̃|P̃|P̃|P̃|P̃|
    |R̃|Ñ|B̃|Q̃|K̃|B̃|Ñ|R̃|
    turn=WHITE
    """

let game = Game str

printfn "%A" game
