namespace Cloudinator_2000.Function

open System.Net
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks
open Microsoft.Azure.Functions.Worker
open Microsoft.Azure.Functions.Worker.Http
open Microsoft.Extensions.Logging
open Cloudinator_2000.Game
open Cloudinator_2000.Game.GameStateModule
open Cloudinator_2000.Game.MoverModule

module MoveTrigger =

    let private jsonOptions =
        JsonFSharpOptions.Default().WithUnionUnwrapFieldlessTags().ToJsonSerializerOptions()

    let private mover: IMover = RandomPieceMover()

    let private respond (req: HttpRequestData) (status: HttpStatusCode) (contentType: string) (body: string) =
        task {
            let response = req.CreateResponse(status)
            response.Headers.Add("Content-Type", contentType)
            do! response.WriteStringAsync body
            return response
        }

    let private handleJson (req: HttpRequestData) (logger: ILogger) =
        task {
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
                return!
                    respond
                        req
                        HttpStatusCode.BadRequest
                        "text/plain; charset=utf-8"
                        "Request body must be a valid GameState JSON object."
            | Some state ->
                let result = mover.Move state

                return!
                    respond
                        req
                        HttpStatusCode.OK
                        "application/json; charset=utf-8"
                        (JsonSerializer.Serialize(result, jsonOptions))
        }

    // Body is the board text format from GameState.ToString(); decoding covers both `curl -d` and `--data-urlencode`.
    let private handleForm (req: HttpRequestData) =
        task {
            let! body = req.ReadAsStringAsync()

            let decoded =
                match WebUtility.UrlDecode body with
                | null -> ""
                | s -> s

            let state = ParserModule.Game decoded

            let text =
                match mover.Move state with
                | Some(next, _) -> next.ToString()
                | None -> ""

            return! respond req HttpStatusCode.OK "text/plain; charset=utf-8" text
        }

    [<Function("MoveTrigger")>]
    let run
        ([<HttpTrigger(AuthorizationLevel.Function, "post", Route = null)>] req: HttpRequestData)
        (context: FunctionContext)
        : Task<HttpResponseData> =
        let logger = context.GetLogger "MoveTrigger"

        let contentType =
            match req.Headers.TryGetValues "Content-Type" with
            | true, NonNull values -> Seq.head values
            | _ -> ""

        if contentType.StartsWith("application/x-www-form-urlencoded", System.StringComparison.OrdinalIgnoreCase) then
            handleForm req
        else
            handleJson req logger
