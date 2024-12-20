// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Ligature.Model
open Fable.Core.JsInterop
open Wander.Main
open Wander.Commands
open Wander.Model
open Wander.Lib
open System.Collections.Generic

let encodeQuote (quote: Quote) = failwith "TODO"

let encodeNetwork (network: Network) =
    Set.map
        (fun (e, a, v) ->
            let e =
                match e with
                | ElementPattern.Element(Element e) ->
                    let obj = createEmpty
                    obj?``type`` <- "element"
                    obj?value <- e
                    obj
                | ElementPattern.Variable(Variable v) ->
                    let obj = createEmpty
                    obj?``type`` <- "variable"
                    obj?value <- v
                    obj

            let a =
                match a with
                | ElementPattern.Element(Element e) ->
                    let obj = createEmpty
                    obj?``type`` <- "element"
                    obj?value <- e
                    obj
                | ElementPattern.Variable(Variable v) ->
                    let obj = createEmpty
                    obj?``type`` <- "variable"
                    obj?value <- v
                    obj

            let v =
                match v with
                | Value.Element(Element e) ->
                    let obj = createEmpty
                    obj?``type`` <- "element"
                    obj?value <- e
                    obj
                | Value.Variable(Variable v) ->
                    let obj = createEmpty
                    obj?``type`` <- "variable"
                    obj?value <- v
                    obj
                | Value.Literal l ->
                    let obj = createEmpty
                    obj?``type`` <- "literal"
                    obj?value <- l
                    obj

            [| e; a; v |])
        network
    |> Array.ofSeq

let processArguments (args: Arguments) =
    List.map
        (fun arg ->
            match arg with
            | Any.Element(Element e) ->
                let obj = createEmpty
                obj?``type`` <- "element"
                obj?value <- e
                obj
            | Any.Literal l ->
                let obj = createEmpty
                obj?``type`` <- "literal"
                obj?value <- l
                obj
            | Any.Variable(Variable v) ->
                let obj = createEmpty
                obj?``type`` <- "variable"
                obj?value <- v
                obj
            | Any.Quote q ->
                let obj = createEmpty
                obj?``quote`` <- "quote"
                obj?value <- encodeQuote q
                obj
            | Any.Network n ->
                let obj = createEmpty
                obj?``type`` <- "network"
                obj?value <- encodeNetwork n
                obj
            | Any.ResultSet(_) -> failwith "Not Implemented"
            | Any.Pipe -> failwith "Not Implemented")
        args
    |> Array.ofList

let createCommand obj : Command =
    { Name = Element obj?name
      Doc = obj?doc
      Eval =
        fun commands variables arguments ->
            obj?action (processArguments arguments)
            Ok(Some(Any.ResultSet Set.empty)) }

let runScript (script: string) =
    match run stdCommands (emptyVariables ()) script with
    | Ok(Some res) -> prettyPrint res
    | Ok _ -> "{}"
    | _ -> failwith "TODO"

let runScriptResult (script: string) (commands: Command array) =
    let mutable stdCommands = stdCommands
    Array.iter (fun command -> stdCommands <- Map.add command.Name command stdCommands) commands

    match run stdCommands (emptyVariables ()) script with
    | Ok(Some(Any.ResultSet resultSet)) ->
        let mutable result = [||]

        Set.iter
            (fun res ->
                let dict = new Dictionary<string, string>()

                Map.iter
                    (fun k v ->
                        match k, v with
                        | Variable v, Value.Element(Element e) -> dict.Add(v, e)
                        | Variable v, Value.Literal l -> dict.Add(v, l)
                        | Variable v, Value.Variable(Variable variable) -> dict.Add(v, variable))
                    res

                result <- Array.append result [| dict |])
            resultSet

        result
    | res -> failwith $"Script must return a ResultSet when you call runScriptResult.\nRecieved {res}"
