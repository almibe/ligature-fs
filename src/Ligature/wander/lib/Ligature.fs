// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Ligature

open Ligature.Wander.Model
open Ligature.Main

let patternStatementToStatement (pattern: Statement) : Statement option =
    match pattern with
    | { Entity = PatternIdentifier.Id(entity)
        Attribute = PatternIdentifier.Id(attribute) } -> failwith "TODO"
    | _ -> failwith "TODO"

let applyFunction =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Network(pattern); WanderValue.Namespace(data) ] ->
                    let res =
                        data
                        |> Map.toSeq
                        |> Seq.map (fun (k, v) ->
                            match slot (Some k) with
                            | Ok slot ->
                                let v =
                                    match v with
                                    | WanderValue.Identifier i -> Value.Identifier i
                                    | WanderValue.Int i -> Value.Int i
                                    | WanderValue.String s -> Value.String s
                                    | WanderValue.Bytes b -> Value.Bytes b
                                    | _ -> failwith "Error"

                                (slot, v)
                            | _ -> failwith "Error")
                        |> Map.ofSeq

                    Ok(WanderValue.Network(pattern.Apply res))
                | value -> error $"Unexpected value passed to Pattern.apply - {value}." None)
        )
    )

let countFunction =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Network(pattern) ] -> Ok(WanderValue.Int(bigint (pattern.Count())))
                | value -> error $"Unexpected value - {value}." None)
        )
    )

let extractFunction =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Network(pattern); WanderValue.Network(network) ] ->
                    network.Extract pattern
                    |> List.map (fun res ->
                        res
                        |> Map.toSeq
                        |> Seq.map (fun (k, v) ->
                            (k.Name,
                             match v with
                             | Value.Int value -> WanderValue.Int value
                             | Value.Bytes value -> WanderValue.Bytes value
                             | Value.Identifier value -> WanderValue.Identifier value
                             | Value.String value -> WanderValue.String value))
                        |> Map.ofSeq
                        |> WanderValue.Namespace)
                    |> Array.ofList
                    |> WanderValue.Array
                    |> Ok
                | value -> error $"Unexpected value passed to Pattern.extract - {value}." None)
        )
    )

let ligatureLib =
    WanderValue.Namespace(
        Map
            [ ("count", countFunction)
              ("extract", extractFunction)
              ("apply", applyFunction)
              //merge
              //minus
              //query
              //trans
              ]
    )
