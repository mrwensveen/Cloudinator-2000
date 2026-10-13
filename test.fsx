#load "GameState.fs"
#load "Parser.fs"

open Cloudinator_2000.Game.GameStateModule
open Cloudinator_2000.Game.ParserModule
open System.Text.RegularExpressions

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

let parseLine (y: int) (board: Map<Square, Piece>) (line: string) =
    // Replace unmoved pieces ('P' + \u0303 = 'P̃') by a single character chr(ord('P') + 0x0303)
    // (this is not a valid renderable character), while leaving moved pieces untouched.

    let encodeUnmoved (m: Match) =
        int m.Value[0] + 0x0303 |> char |> string

    let decode (enc: int) (moved: bool) =
        (if moved then enc else enc - 0x0303) |> char |> string

    let encoded = Regex.Replace(line.Trim(), ".\u0303", encodeUnmoved)

    let pieces =
        encoded
        |> Seq.mapi (fun x c ->
            let enc = int c
            let moved = enc <= 0x0303
            let kind = decode enc moved
            (x + 1, 8 - y), Pc kind moved)

    let addPiece (b: Map<Square, Piece>) (square: Square) (piece: Piece option) =
        piece |> Option.fold (fun b p -> b |> Map.add square p) b

    pieces |> Seq.fold (fun b (square, piece) -> addPiece b square piece) board

let lines = str.Trim().Replace("|", "").Split "\n" |> Array.map (fun s -> s.Trim())

let board: Map<Square, Piece> =
    lines[0..7]
    |> Array.fold (fun (y, board) line -> y + 1, line |> parseLine y board) (0, Map.empty)
    |> snd
