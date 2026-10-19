namespace Cloudinator_2000.Game

module GameStateModule =

    type Square = int * int

    let private step ((x, y): Square) ((dx, dy): (int * int)) : Square option =
        let nx, ny = x + dx, y + dy

        if nx >= 1 && nx <= 8 && ny >= 1 && ny <= 8 then
            Some(nx, ny)
        else
            None

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

        override this.ToString() : string =
            let s =
                if this.Kind = Knight then
                    "N"
                else
                    this.Kind.ToString()[..0]

            let p = if this.Color = Black then s.ToLowerInvariant() else s
            if this.Moved then p else $"\u0303{p}"

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

        member this.FlipTurn() =
            { this with Turn = this.Turn.Flipped() }

        override this.ToString() : string =
            let board =
                [ 8..-1..1 ]
                |> List.map (fun y ->
                    let line =
                        [ 1..8 ]
                        |> List.map (fun x ->
                            let occupant = this.Board.TryFind(x, y) // aka getOccupant

                            match occupant with
                            | Some o -> o.ToString()
                            | None ->
                                match this.EnPassant with
                                | Some((xSrc, ySrc), (_, yDst)) when x = xSrc && y = ySrc ->
                                    if yDst = ySrc + 1 then "↑" else "↓"
                                | _ -> " ")
                        |> String.concat "|"

                    $"|{line}|")
                |> String.concat "\n"

            $"{board}\nturn={this.Turn.ToString().ToUpperInvariant()}"

    type Move = Square * GameState

    let getOccupant (game: GameState) (position: Square option) : Piece option = Option.bind game.Board.TryFind position

    let getPieces (game: GameState) (color: PieceColor) (kind: PieceKind) : Map<Square, Piece> =
        game.Board
        |> Map.filter (fun _ { Color = c; Kind = k } -> color = c && kind = k)

    let getPawnMoves (game: GameState) (position: Square) : GameState seq =
        let x, y = position

        let getPawnMoves' (piece: Piece) : GameState seq =

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
        let x, y = position

        let getKnightMoves' (piece: Piece) : GameState seq =

            let positions: Square array =
                [|
                    if x <= 7 && y <= 6 then
                        yield x + 1, y + 2
                    if x <= 6 && y <= 7 then
                        yield x + 2, y + 1
                    if x <= 6 && y >= 2 then
                        yield x + 2, y - 1
                    if x <= 7 && y >= 3 then
                        yield x + 1, y - 2
                    if x >= 2 && y >= 3 then
                        yield x - 1, y - 2
                    if x >= 3 && y >= 2 then
                        yield x - 2, y - 1
                    if x >= 3 && y <= 7 then
                        yield x - 2, y + 1
                    if x >= 2 && y <= 6 then
                        yield x - 1, y + 2
                |]

            let occupants = positions |> Array.map (fun p -> p, getOccupant game (Some p))

            let possible =
                occupants
                |> Array.map (fun occupant ->
                    match occupant with
                    | pos, None -> Some pos
                    | pos, Some { Color = c } when c <> piece.Color -> Some pos
                    | _ -> None)

            possible |> Seq.choose id |> Seq.map (game.Move piece position)

        match getOccupant game (Some position) with
        | Some piece -> getKnightMoves' piece
        | _ -> Seq.empty

    let private getSlideDirections =
        function
        | Queen -> [ 0, 1; 1, 1; 1, 0; 1, -1; 0, -1; -1, -1; -1, 0; -1, 1 ]
        | Rook -> [ 0, 1; 1, 0; 0, -1; -1, 0 ]
        | Bishop -> [ 1, 1; 1, -1; -1, -1; -1, 1 ]
        | _ -> List.empty

    let getSlideMoves (game: GameState) (position: Square) : GameState seq =
        let getSlideMoves' (piece: Piece) : GameState seq =
            let rec slide (position: Square) (direction: (int * int)) : Square seq =
                let next = step position direction
                let occupant = getOccupant game next

                match next, occupant with
                | None, _ -> Seq.empty
                | Some p, None ->
                    seq {
                        yield p
                        yield! slide p direction
                    }
                | Some p, Some o ->
                    seq {
                        if o.Color <> piece.Color then
                            yield p
                    }

            getSlideDirections piece.Kind
            |> Seq.collect (fun d -> slide position d)
            |> Seq.map (fun dst -> game.Move piece position dst)

        match getOccupant game (Some position) with
        | Some piece when List.contains piece.Kind [ Queen; Rook; Bishop ] -> getSlideMoves' piece
        | _ -> Seq.empty

    let rec getKingMoves (game: GameState) (position: Square) (isCastlingAllowed: bool) : GameState seq =
        let x, y = position

        let getStepMoves (piece: Piece) : GameState seq =

            let positions: Square array =
                [|
                    if x <= 7 then
                        yield x + 1, y
                    if x >= 2 then
                        yield x - 1, y
                    if y <= 7 then
                        yield x, y + 1
                    if y >= 2 then
                        yield x, y - 1
                    if x <= 7 && y <= 7 then
                        yield x + 1, y + 1
                    if x <= 7 && y >= 2 then
                        yield x + 1, y - 1
                    if x >= 2 && y <= 7 then
                        yield x - 1, y + 1
                    if x >= 2 && y >= 2 then
                        yield x - 1, y - 1
                |]

            let occupants = positions |> Array.map (fun p -> p, getOccupant game (Some p))

            let possible =
                occupants
                |> Array.map (fun occupant ->
                    match occupant with
                    | pos, None -> Some pos
                    | pos, Some { Color = c } when c <> piece.Color -> Some pos
                    | _ -> None)

            possible |> Seq.choose id |> Seq.map (game.Move piece position)

        let getCastlingMoves (piece: Piece) : GameState seq =
            let getCastlingRook ((x, y): Square) : Piece option =
                let checkFiles = if x = 1 then [| 2; 3; 4 |] else [| 6; 7 |]
                let rook = getOccupant game (Some(x, y))

                rook
                |> Option.filter (fun r ->
                    r.Kind = Rook
                    && r.Color = game.Turn
                    && not r.Moved
                    && checkFiles
                       |> Array.forall (fun x -> getOccupant game (Some(x, y)) |> Option.isNone))

            let _isCheck = isCheck game
            let qsRook = if _isCheck then None else getCastlingRook (1, y)
            let ksRook = if _isCheck then None else getCastlingRook (8, y)

            let castlings =
                [|
                    qsRook
                    |> Option.map (fun rook -> (game.Move piece position (x - 2, y)).FlipTurn().Move rook (1, y) (4, y))
                    ksRook
                    |> Option.map (fun rook -> (game.Move piece position (x + 2, y)).FlipTurn().Move rook (8, y) (6, y))
                |]

            castlings |> Seq.choose id

        let getKingMoves' (piece: Piece) : GameState seq =
            seq {
                yield! getStepMoves piece

                if isCastlingAllowed && not piece.Moved then
                    yield! getCastlingMoves piece
            }

        match getOccupant game (Some position) with
        | Some piece -> getKingMoves' piece
        | _ -> Seq.empty

    and getPieceMoves (game: GameState) (position: Square) (piece: Piece) (checkCheck: bool) : Move seq =
        let pieceMoves =
            match piece.Kind with
            | Pawn -> getPawnMoves game position
            | Knight -> getKnightMoves game position
            | King -> getKingMoves game position checkCheck
            | _ -> Seq.empty

        if not checkCheck then
            pieceMoves |> Seq.map (fun move -> position, move)
        else
            pieceMoves
            |> Seq.filter (fun move -> not (isCheck move))
            |> Seq.map (fun move -> position, move)

    and getPossibleMoves (game: GameState) (checkCheck: bool) : Move seq =
        game.Board
        |> Map.toSeq
        |> Seq.filter (fun (_, piece) -> piece.Color = game.Turn)
        |> Seq.collect (fun (position, piece) -> getPieceMoves game position piece checkCheck)

    and getAttackers (game: GameState) (position: Square) : Square seq =
        let getAttackers' () =
            let allMoves: Move seq = getPossibleMoves game true

            allMoves
            |> Seq.choose (fun (origin, move) ->
                let occupant = getOccupant move (Some position)

                occupant
                |> Option.bind (fun o -> if o.Color = game.Turn then Some origin else None))

        // If the current player already occupies the position, there are no attackers
        match getOccupant game (Some position) with
        | Some _ -> Seq.empty
        | None -> getAttackers' ()

    and private isCheck (game: GameState) : bool =
        let kingPositions = getPieces game (game.Turn.Flipped()) King |> Map.keys

        kingPositions
        |> Seq.exists (fun pos -> not (getAttackers game pos |> Seq.isEmpty))
