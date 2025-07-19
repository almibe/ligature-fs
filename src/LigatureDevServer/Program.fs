// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.DevServer

open Wander.Main
open Wander.Fns
open Wander.Model
open System
open Ligature.Model
open Ligature.InMemoryStore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Microsoft.AspNetCore.Http
open System.IO
open Giraffe.ViewEngine
open Wander.Fns.Html

//let path = Environment.GetEnvironmentVariable "LIGATURE_HOME" + "/store"
let store = InMemoryStore() //new LigatureStore(Some path)

let httpRootPath = Environment.GetEnvironmentVariable "LIGATURE_HTTP_ROOT"

let rec allFiles dirs =
    if Seq.isEmpty dirs then
        Seq.empty
    else
        seq {
            yield! dirs |> Seq.collect Directory.EnumerateFiles
            yield! dirs |> Seq.collect Directory.EnumerateDirectories |> allFiles
        }

let rec handleSeq (seq: List<Expression>) (nodes: List<Application>) : List<Application> =
    let newNodes: List<Application> =
        List.collect
            (fun value ->
                match value with
                | Expression.NodeLiteral node -> [ node ]
                | Expression.Seq seq -> handleSeq seq nodes
                | _ -> failwith "TODO")
            seq

    List.append nodes newNodes

let wanderHandler: HttpHandler =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        if HttpMethods.IsGet ctx.Request.Method then
            task {
                let path = ctx.Request.Path
                let file = httpRootPath + path

                if not (file.Contains "..") && file.EndsWith ".wander" then
                    let! script = System.IO.File.ReadAllTextAsync file

                    return!
                        match run (Wander.Library.stdFns store) Map.empty script with
                        | Ok(Expression.NodeLiteral result) -> ctx.WriteHtmlStringAsync(generateHtml result)
                        | Ok(Expression.Seq seq) ->
                            let nodes: Option<Application list> = Some(handleSeq seq [])

                            match nodes with
                            | Some []
                            | None -> ctx.WriteTextAsync(printExpression (Expression.Seq seq))
                            | Some nodes -> ctx.WriteHtmlStringAsync(generateHtmlSeq nodes)
                        | Ok result -> ctx.WriteTextAsync(printExpression result)
                        | Error err -> ctx.WriteTextAsync err.UserMessage
                else
                    return! ctx.WriteTextAsync "Unexpected value."
            }
        else
            failwith "TODO"

let indexHandler: HttpHandler =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        if HttpMethods.IsGet ctx.Request.Method then
            task {
                let filesList =
                    allFiles [ httpRootPath ]
                    |> List.ofSeq
                    |> List.filter (fun file -> file.EndsWith ".wander")
                    |> List.map (fun file ->
                        let name = (file.Substring httpRootPath.Length).Replace("\\", "/")
                        li [] [ a [ attr "href" name ] [ str name ] ])

                let indexView =
                    html
                        []
                        [ head [] [ title [] [ str "LigatureDevServer" ] ]
                          body [] [ h1 [] [ str "Files:" ]; ul [] filesList ] ]

                return! ctx.WriteHtmlStringAsync(RenderView.AsString.htmlDocument indexView)
            }
        else
            failwith "TODO"

let createEndpoints (store: ILigatureStore) =
    choose [ GET >=> route "/" >=> indexHandler; GET >=> wanderHandler ]

let wapp = WebApplication.Create()


type Startup() =
    member __.ConfigureServices(services: IServiceCollection) =
        // Register default Giraffe dependencies
        services.AddGiraffe() |> ignore

    member __.Configure (app: IApplicationBuilder) (env: IHostEnvironment) (loggerFactory: ILoggerFactory) =
        // Add Giraffe to the ASP.NET Core pipeline
        app.UseGiraffe(createEndpoints (InMemoryStore()))

[<EntryPoint>]
let main _ =
    Host
        .CreateDefaultBuilder()
        .ConfigureWebHostDefaults(fun webHostBuilder -> webHostBuilder.UseStartup<Startup>() |> ignore)
        .Build()
        .Run()

    0
