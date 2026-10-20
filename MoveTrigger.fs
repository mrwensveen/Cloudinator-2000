namespace Cloudinator_2000.Function

open System.Net
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks
open Microsoft.Azure.Functions.Worker
open Microsoft.Azure.Functions.Worker.Http
open Microsoft.Extensions.Logging
open Cloudinator_2000.Game.GameStateModule
open Cloudinator_2000.Game.MoverModule

module MoveTrigger =

    let private jsonOptions =
        JsonFSharpOptions.Default().WithUnionUnwrapFieldlessTags().ToJsonSerializerOptions()

    [<Function("MoveTrigger")>]
    let run
        ([<HttpTrigger(AuthorizationLevel.Function, "post", Route = null)>] req: HttpRequestData)
        (context: FunctionContext)
        : Task<HttpResponseData> =
        task {
            let logger = context.GetLogger "MoveTrigger"

            let! state =
                task {
                    try
                        let! s = JsonSerializer.DeserializeAsync<GameState>(req.Body, jsonOptions)

                        return
                            match s with
                            | null -> None
                            | s -> Some s
                    with :? JsonException as ex ->
                        logger.LogWarning(ex, "Invalid GameState JSON")
                        return None
                }

            match state with
            | None ->
                let response = req.CreateResponse(HttpStatusCode.BadRequest)
                do! response.WriteStringAsync "Request body must be a valid GameState JSON object."
                return response
            | Some state ->
                let mover: IMover = RandomPieceMover()

                let result = mover.Move state

                let response = req.CreateResponse(HttpStatusCode.OK)
                response.Headers.Add("Content-Type", "application/json; charset=utf-8")
                do! response.WriteStringAsync(JsonSerializer.Serialize(result, jsonOptions))
                return response
        }
