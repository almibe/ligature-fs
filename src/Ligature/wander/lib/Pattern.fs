// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Pattern

open Ligature.Wander.Model
open Ligature.Main

let patternStatementToStatement (pattern: Statement) : Statement option =
    match pattern with
    | { Entity = PatternIdentifier.Id(entity)
        Attribute = PatternIdentifier.Id(attribute) } -> failwith "TODO"
    | _ -> failwith "TODO"

// let networkToPattern (network: Network) : IPattern =
//     failwith "TODO"
// let res = network.all()
// let l: Statement list =
//     List.map
//         (fun item ->
//             { Entity = (PatternIdentifier.Id item.Entity);
//                 Attribute = (PatternIdentifier.Id item.Attribute);
//                 Value = Value.Value item.Value }) res
// Network(Set.ofList l)

// let applyFunction =
//     WanderValue.Function(
//         Function.HostFunction(
//             new HostFunction(fun args _ ->
//                 match args with
//                 | [ WanderValue.Network(pattern); WanderValue.Namespace(data) ] ->
//                     let res =
//                         data
//                         |> Map.toSeq
//                         |> Seq.map (fun (k, v) ->
//                             match slot (Some k) with
//                             | Ok slot ->
//                                 let v =
//                                     match v with
//                                     | WanderValue.Identifier i -> Value.Identifier i
//                                     | WanderValue.Int i -> Value.Int i
//                                     | WanderValue.String s -> Value.String s
//                                     | WanderValue.Bytes b -> Value.Bytes b
//                                     | _ -> failwith "Error"

//                                 (slot, v)
//                             | _ -> failwith "Error")
//                         |> Map.ofSeq

//                     match pattern.Apply res with
//                     | Some res -> Ok(WanderValue.Network(networkToPattern res))
//                     | None -> failwith ""
//                 | value -> error $"Unexpected value passed to Pattern.apply - {value}." None)
//         )
//     )

// let singleRootFunction =
//     WanderValue.Function(
//         Function.HostFunction(
//             new HostFunction(fun args _ ->
//                 match args with
//                 | [ WanderValue.Network(pattern) ] -> Ok(WanderValue.Bool(pattern.SingleRoot))
//                 | value -> error $"Unexpected value - {value}." None)
//         )
//     )

let countFunction =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Network(pattern) ] -> Ok(WanderValue.Int(bigint (pattern.Count())))
                | value -> error $"Unexpected value - {value}." None)
        )
    )

let isDatasetFunction =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Network(statements) ] ->
                    let result =
                        Seq.forall
                            (fun (statement: Statement) ->
                                match (statement.Entity, statement.Attribute, statement.Value) with
                                | (PatternIdentifier.Sl(_), _, _) -> false
                                | (_, PatternIdentifier.Sl(_), _) -> false
                                | (_, _, Value.Slot(_)) -> false
                                | _ -> true)
                            (statements.AllStatements())

                    Ok(WanderValue.Bool(result))
                | value -> error $"Unexpected value passed to Pattern.isDataset - {value}." None)
        )
    )

let extractsFunction =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Network(statements) ] ->
                    let result =
                        Seq.exists
                            (fun (statement: Statement) ->
                                let entity =
                                    match statement.Entity with
                                    | PatternIdentifier.Sl(slot) -> slot.Named
                                    | _ -> false

                                let attribute =
                                    match statement.Attribute with
                                    | PatternIdentifier.Sl(slot) -> slot.Named
                                    | _ -> false

                                let value =
                                    match statement.Value with
                                    | Value.Slot(slot) -> slot.Named
                                    | _ -> false

                                entity || attribute || value)
                            (statements.AllStatements())

                    Ok(WanderValue.Bool(result))
                | value -> error $"Unexpected value passed to Pattern.extracts - {value}." None)
        )
    )

// let patternToNetwork (pattern: IPattern) : Network =
//     match pattern.ToNetwork with
//     | Some dataset -> dataset
//     | _ -> failwith "TODO"

let extract network pattern : Map<Slot, Value> list = failwith "TODO"

// let extractFunction =
//     WanderValue.Function(
//         Function.HostFunction(
//             new HostFunction(fun args _ ->
//                 match args with
//                 | [ WanderValue.Network(pattern); WanderValue.Network(dataset) ] ->
//                     //(patternToDataset dataset).Extract pattern
//                     extract (patternToNetwork dataset) pattern
//                     |> List.map (fun res ->
//                         res
//                         |> Map.toSeq
//                         |> Seq.map (fun (k, v) ->
//                             (k.Name,
//                              match v with
//                              | Value.Int value -> WanderValue.Int value
//                              | Value.Bytes value -> WanderValue.Bytes value
//                              | Value.Identifier value -> WanderValue.Identifier value
//                              | Value.String value -> WanderValue.String value))
//                         |> Map.ofSeq
//                         |> WanderValue.Namespace)
//                     |> Array.ofList
//                     |> WanderValue.Array
//                     |> Ok
//                 | value -> error $"Unexpected value passed to Pattern.extract - {value}." None)
//         )
//     )

let patternLib =
    WanderValue.Namespace(
        Map
            [ //("apply", applyFunction)
              ("count", countFunction)
              //  ("extract", extractFunction)
              ("extracts", extractsFunction)
              ("isDataset", isDatasetFunction)
              //("singleRoot", singleRootFunction)
              ]
    )
