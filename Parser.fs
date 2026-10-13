namespace Cloudinator_2000.Game

open GameStateModule
open System.Text.RegularExpressions

module ParserModule =

    let Pc (str: string) (moved: bool) : Piece option =
        let upper = str.ToUpperInvariant()

        let kind =
            match upper with
            | "P" -> Some Pawn
            | "R" -> Some Rook
            | "N" -> Some Knight
            | "B" -> Some Bishop
            | "Q" -> Some Queen
            | "K" -> Some King
            | _ -> None

        kind
        |> Option.map (fun kind ->
            {
                Kind = kind
                Color = if str = upper then White else Black
                Moved = moved
            })

    let private parseLine (y: int) (board: Map<Square, Piece>) (line: string) =
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

    let Game (str: string) =
        let lines = str.Trim().Replace("|", "").Split "\n" |> Array.map (fun s -> s.Trim())

        let turn =
            if lines.Length > 8 && lines[8].Contains "turn=BLACK" then
                Black
            else
                White

        let board: Map<Square, Piece> =
            lines[0..7]
            |> Array.fold (fun (y, board) line -> y + 1, line |> parseLine y board) (0, Map.empty)
            |> snd

        {
            Board = board
            Turn = turn
            EnPassant = None // TODO
            Previous = None
        }
