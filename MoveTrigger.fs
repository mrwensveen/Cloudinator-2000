namespace Cloudinator_2000.Function

open System.Net
open System.Threading.Tasks
open Microsoft.Azure.Functions.Worker
open Microsoft.Azure.Functions.Worker.Http
open Microsoft.Extensions.Logging

module MoveTrigger =

    [<Function("MoveTrigger")>]
    let run
        ([<HttpTrigger(AuthorizationLevel.Function, "get", "post", Route = null)>] req: HttpRequestData)
        (context: FunctionContext)
        : Task<HttpResponseData> =
        task {
            let logger = context.GetLogger "MoveTrigger"
            logger.LogInformation "F# HTTP trigger function processed a request"

            let response = req.CreateResponse(HttpStatusCode.OK)
            response.Headers.Add("Content-Type", "text/plain; charset=utf-8")

            do! response.WriteStringAsync "Welcome to Azure Functions!"

            return response
        }
