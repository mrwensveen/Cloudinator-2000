namespace Cloudinator_2000.Game

module GameStateModule =

    type Square = int * int

    type PieceKind =
        | Pawn
        | Rook
        | Knight
        | Bishop
        | Queen
        | King

    type PieceColor =
        | White
        | Black

        member this.Flipped() =
            match this with
            | White -> Black
            | Black -> White

    type Piece =
        {
            Color: PieceColor
            Kind: PieceKind
            Moved: bool
        }

        member this.Promote(kind: PieceKind) = { this with Kind = kind }

        member this.Direction =
            match this.Kind with
            | Pawn
            | King ->
                match this.Color with
                | White -> 1
                | Black -> -1
            | _ -> 0

    type GameState =
        {
            Board: Map<Square, Piece>
            Turn: PieceColor
            EnPassant: (Square * Square) option
            Previous: GameState option
        }

        member this.Move (piece: Piece) (src: Square) (dst: Square) =
            let enPassant =
                if piece.Kind = Pawn && not piece.Moved && snd dst = snd src + 2 * piece.Direction then
                    Some((fst src, snd src + piece.Direction), dst)
                else
                    None

            let board =
                this.Board
                // Delete the piece from src
                |> Map.remove src
                // Delete the opponent's pawn if it left an en-passant possibility and the current
                // player's pawn is moved to that spot
                |> match this.EnPassant with
                   | Some(s, d) when dst = s -> Map.remove d
                   | _ -> id
                // Add the piece to the new position
                |> Map.add dst { piece with Moved = true }

            { this with
                Board = board
                Turn = this.Turn.Flipped()
                EnPassant = enPassant
                Previous = Some this
            }


    type Move = Square * GameState

    let getOccupant (game: GameState) (position: Square option) : Piece option =
        match position with
        | None -> None
        | Some p -> game.Board.TryFind p

    let getPawnMoves (game: GameState) (position: Square) : GameState seq =
        let getPawnMoves' (piece: Piece) : GameState seq =
            let x, y = position

            let direction = piece.Direction

            let positions: Square option array =
                [|
                    Some(x, y + direction)
                    if not piece.Moved then Some(x, y + 2 * direction) else None
                    if x >= 2 then Some(x - 1, y + direction) else None
                    if x <= 7 then Some(x + 1, y + direction) else None
                |]

            let occupants = positions |> Array.map (getOccupant game)

            let canCapture i =
                match occupants[i], game.EnPassant with
                | Some occupant, _ when occupant.Color <> piece.Color -> true
                | _, Some(square, _) when Some square = positions[i] -> true
                | _ -> false

            let possible =
                [|
                    if occupants[0].IsNone then positions[0] else None
                    match occupants[0], occupants[1] with
                    | None, None -> positions[1]
                    | _ -> None
                    if canCapture 2 then positions[2] else None
                    if canCapture 3 then positions[3] else None
                |]

            // Promotions
            let promotionRow =
                match piece.Color with
                | White -> 8
                | Black -> 1

            let promotions ((_, row): Square) =
                if row = promotionRow then
                    seq {
                        Queen
                        Rook
                        Bishop
                        Knight
                    }
                    |> Seq.map piece.Promote
                else
                    Seq.singleton piece

            possible
            |> Seq.choose id
            |> Seq.collect (fun dst -> dst |> promotions |> Seq.map (fun piece -> game.Move piece position dst))

        match getOccupant game (Some position) with
        | Some piece -> getPawnMoves' piece
        | _ -> Seq.empty

    let getKnightMoves (game: GameState) (position: Square) : GameState seq =
        let getKnightMoves' (piece: Piece) : GameState seq =
            let x, y = position

            let positions: Square option array =
                [|
                    if x <= 7 && y <= 6 then Some(x + 1, y + 2) else None
                    if x <= 6 && y <= 7 then Some(x + 2, y + 1) else None
                    if x <= 6 && y >= 2 then Some(x + 2, y - 1) else None
                    if x <= 7 && y >= 3 then Some(x + 1, y - 2) else None
                    if x >= 2 && y >= 3 then Some(x - 1, y - 2) else None
                    if x >= 3 && y >= 2 then Some(x - 2, y - 1) else None
                    if x >= 3 && y <= 7 then Some(x - 2, y + 1) else None
                    if x >= 2 && y <= 6 then Some(x - 1, y + 2) else None
                |]

            let possible =
                positions
                |> Array.map (fun p ->
                    let occupant = getOccupant game p

                    match occupant with
                    | None -> p
                    | Some { Color = c } when c <> piece.Color -> p
                    | _ -> None)

            possible |> Seq.choose id |> Seq.map (game.Move piece position)

        match getOccupant game (Some position) with
        | Some piece -> getKnightMoves' piece
        | _ -> Seq.empty


    let getPieceMoves (game: GameState) (position: Square) (piece: Piece) (checkCheck: bool) : Move seq =
        let pieceMoves =
            match piece.Kind with
            | Pawn -> getPawnMoves game position
            | Knight -> getKnightMoves game position
            | _ -> Seq.empty

        pieceMoves |> Seq.map (fun move -> position, move)
