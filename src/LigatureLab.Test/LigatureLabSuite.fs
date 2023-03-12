// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Lab.Test

open Expecto
open Ligature.Lab.Main
open System.Net
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.TestHost
open Microsoft.AspNetCore.Hosting
open System.Net.Http

let getTestHost() =
    WebHostBuilder()
        .UseTestServer()
        .Configure(configureApp)
        .ConfigureServices(configureServices)

let testRequest (request : HttpRequestMessage) =
    let resp = task {
        use server = new TestServer(getTestHost())
        use client = server.CreateClient()
        let! response = request |> client.SendAsync
        return response
    }
    resp.Result

[<Tests>]
let tests =
    testList "Ligature Http Suite" [
        testCase "start with zero Datasets" <| fun _ ->
            let request = new HttpRequestMessage(HttpMethod.Get, "/datasets")
            let result = testRequest request
            Expect.equal result.StatusCode HttpStatusCode.OK "Status should be 200"
            Expect.isEmpty (result.Content.ReadAsStringAsync().Result) ""

        testCase "add Datasets" <| fun _ ->
            let request = new HttpRequestMessage(HttpMethod.Post, "/datasets/test")
            let result = testRequest request
            Expect.equal result.StatusCode HttpStatusCode.OK "Status should be 200"
            
            let request = new HttpRequestMessage(HttpMethod.Get, "/datasets")
            let result = testRequest request
            Expect.equal result.StatusCode HttpStatusCode.OK "Status should be 200"
            Expect.equal (result.Content.ReadAsStringAsync().Result) "test" "Datasets should only contain test."

        testCase "remove Datasets" <| fun _ ->
            let request = new HttpRequestMessage(HttpMethod.Post, "/datasets/test")
            let result = testRequest request
            Expect.equal result.StatusCode HttpStatusCode.OK "Status should be 200"

            let request = new HttpRequestMessage(HttpMethod.Post, "/datasets/test2")
            let result = testRequest request
            Expect.equal result.StatusCode HttpStatusCode.OK "Status should be 200"

            let request = new HttpRequestMessage(HttpMethod.Get, "/datasets")
            let result = testRequest request
            Expect.equal result.StatusCode HttpStatusCode.OK "Status should be 200"
            Expect.equal (result.Content.ReadAsStringAsync().Result) "test\ntest2" "Datasets should contain test & test2."

            let request = new HttpRequestMessage(HttpMethod.Delete, "/datasets/test")
            let result = testRequest request
            Expect.equal result.StatusCode HttpStatusCode.OK "Status should be 200"
            
            let request = new HttpRequestMessage(HttpMethod.Get, "/datasets")
            let result = testRequest request
            Expect.equal result.StatusCode HttpStatusCode.OK "Status should be 200"
            Expect.equal (result.Content.ReadAsStringAsync().Result) "test2" "Datasets should only contain test2."
    ]
