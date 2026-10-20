namespace Cloudinator_2000.Game

open GameStateModule

module MoverModule =

    type IMover =
        abstract member Move: GameState -> (GameState * float) option

    let private tieOrMove (moveFn: Move seq -> (GameState * float) option) (game: GameState) =
        if game.Board.Count <= 2 then
            None
        else
            let moves = getPossibleMoves game true

            if Seq.isEmpty moves then None else moveFn moves

    type RandomMover() =
        interface IMover with
            member this.Move(game: GameState) : (GameState * float) option =
                game |> tieOrMove (fun moves -> Some(moves |> Seq.randomChoice |> snd, 0.0))

    type FirstMover() =
        interface IMover with
            member this.Move(game: GameState) : (GameState * float) option =
                game
                |> tieOrMove (fun moves -> moves |> Seq.tryHead |> Option.map (fun m -> snd m, 0.0))

    type RandomPieceMover() =
        interface IMover with
            member this.Move(game: GameState) : (GameState * float) option =
                game
                |> tieOrMove (fun moves ->
                    let origins = moves |> SeqEx.groupBy2 fst snd
                    Some(origins |> Seq.randomChoice |> snd |> Seq.randomChoice, 0.0))
