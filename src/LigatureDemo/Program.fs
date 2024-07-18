// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Ligature.Main
open Ligature.Wander.Main
open Ligature.Wander.Model
open Ligature.Wander.NewInterpreter
open System

printfn "Hello, type :q to quit."

let words =
    Map.ofList
        [ "Stack.pop", { Eval = fun environment -> Ok(List.tail environment.Stack) }
          "Stack.count",
          { Eval = fun environment -> Ok((WanderValue.Int environment.Stack.Length) :: environment.Stack) }
          "Stack.apply",
          { Eval =
              fun environment ->
                  match List.tryHead environment.Stack with
                  | Some(WanderValue.Quote(quote)) ->
                      evalValues
                          { environment with
                              Stack = List.tail environment.Stack }
                          quote
                  | _ -> error "Could not call apply." None } ]

let rec readLine prompt stack =
    printf prompt

    match Console.ReadLine() with
    | ":q" -> printfn "Quitting."
    | value ->
        match newRun value { Words = words; Stack = stack } with
        | Ok(result) ->
            printfn "%A" result
            readLine prompt result
        | Error(message) ->
            printfn $"Error: {message.UserMessage}"
            readLine prompt stack

readLine "> " List.empty
