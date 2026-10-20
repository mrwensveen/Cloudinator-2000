namespace Cloudinator_2000.Game

module SeqEx =
    let groupBy2 keyProjection elementProjection source =
        source
        |> Seq.groupBy keyProjection
        |> Seq.map (fun (k, v) -> k, v |> Seq.map elementProjection)
