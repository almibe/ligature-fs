﻿// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.ViewEngine
open System.IO

let datasetsPage = html [] [
        head [] [
            title [] [ str "Ligature Lab" ]
        ]
        body [] [
            h1 [] [ str "Ligature Lab" ]
            p [ _class "some-css-class"; _id "someId" ] [
                str "Datasets:"
            ]
        ]
    ]

let webApp =
    choose [
        route "/" >=> htmlView datasetsPage ]

let configureApp (app : IApplicationBuilder) =
    app.UseStaticFiles()
    app.UseDefaultFiles()
    app.UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    services.AddGiraffe() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    Host.CreateDefaultBuilder()
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseContentRoot(contentRoot)
                    .UseWebRoot(webRoot)
                    .Configure(configureApp)
                    .ConfigureServices(configureServices)
                    |> ignore)
        .Build()
        .Run()
    0