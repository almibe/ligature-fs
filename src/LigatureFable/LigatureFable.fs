// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Browser
open Ligature.Model
open Fable.Core.JsInterop
open Wander.Main
open Wander.Commands
open Wander.Model
open Wander.Lib
open System.Collections.Generic
open Wander.Interpreter

let encodeQuote (quote: Quote) = failwith "TODO - in encodeQuote"

let encodeResultSet (resultSet: ResultSet) =
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

let encodeAny (any: Any) =
    match any with
    | Any.Network n ->
        let res = createEmpty
        res?``type`` <- "network"
        res?value <- encodeNetwork n
        res
    | Any.Quote q ->
        let res = createEmpty
        res?``type`` <- "quote"
        res?value <- encodeQuote q
        res
    | Any.Literal l ->
        let res = createEmpty
        res?``type`` <- "literal"
        res?value <- l
        res
    | Any.Variable v ->
        let res = createEmpty
        res?``type`` <- "variable"
        res?value <- v
        res
    | Any.Element(Element e) ->
        let res = createEmpty
        res?``type`` <- "element"
        res?value <- e
        res
    | Any.ResultSet rs ->
        let res = createEmpty
        res?``type`` <- "resultset"
        res?value <- encodeResultSet rs
        res
    | Any.Pipe -> failwith "Not Implemented"

let encodeResult (result: Result<Any option, LigatureError>) =
    match result with
    | Ok(Some res) -> encodeAny res
    | Ok None -> createEmpty
    | Error error ->
        let res = createEmpty
        res?``type`` <- "error"
        res?value <- error.UserMessage
        res

let processArguments commands variables (args: Arguments) =
    List.map
        (fun arg ->
            match arg with
            | Any.Quote q ->
                match evalQuote commands variables q with
                | Ok(Some res) -> encodeAny res
                | _ -> failwith "TODO"
            | _ -> encodeAny arg)
        args
    |> Array.ofList

let createCommand obj : Command =
    { Name = Element obj?name
      Doc = obj?doc
      Eval =
        fun commands variables arguments ->
            obj?action (processArguments commands variables arguments)
            Ok(Some(Any.ResultSet Set.empty)) }

let runScript (script: string) commands =
    let mutable stdCommands = stdCommands

    Array.iter
        (fun command ->
            let command = createCommand command
            stdCommands <- Map.add command.Name command stdCommands)
        commands

    run stdCommands emptyVariables script |> encodeResult
