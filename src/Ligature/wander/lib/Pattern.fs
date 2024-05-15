// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Pattern

open Ligature.Wander.Model
open Ligature.Main
open Ligature.InMemory.Pattern

let patternStatementToStatement (pattern: PatternStatement) : Statement option =
    match pattern with
    | { Entity = PatternIdentifier.Identifier(entity)
        Attribute = PatternIdentifier.Identifier(attribute) } -> failwith "TODO"
    | _ -> failwith "TODO"

let applyFunction =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Pattern(pattern); WanderValue.Namespace(data) ] ->
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

                    match pattern.Apply res with
                    | Some res -> Ok(WanderValue.Pattern(unsafeDatasetToPattern res))
                    | None -> failwith ""
                | value -> error $"Unexpected value passed to Pattern.apply - {value}." None)
        )
    )

let singleRootFunction =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Pattern(pattern) ] -> Ok(WanderValue.Bool(pattern.SingleRoot))
                | value -> error $"Unexpected value - {value}." None)
        )
    )

let countFunction =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Pattern(pattern) ] -> Ok(WanderValue.Int(bigint (Set.count pattern.PatternStatements)))
                | value -> error $"Unexpected value - {value}." None)
        )
    )

let isDatasetFunction =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Pattern(statements) ] ->
                    let result =
                        Set.forall
                            (fun (statement: PatternStatement) ->
                                match (statement.Entity, statement.Attribute, statement.Value) with
                                | (PatternIdentifier.Slot(_), _, _) -> false
                                | (_, PatternIdentifier.Slot(_), _) -> false
                                | (_, _, PatternValue.Slot(_)) -> false
                                | _ -> true)
                            statements.PatternStatements

                    Ok(WanderValue.Bool(result))
                | value -> error $"Unexpected value passed to Pattern.isDataset - {value}." None)
        )
    )

let extractsFunction =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Pattern(statements) ] ->
                    let result =
                        Set.exists
                            (fun (statement: PatternStatement) ->
                                let entity =
                                    match statement.Entity with
                                    | PatternIdentifier.Slot(slot) -> slot.Named
                                    | _ -> false

                                let attribute =
                                    match statement.Attribute with
                                    | PatternIdentifier.Slot(slot) -> slot.Named
                                    | _ -> false

                                let value =
                                    match statement.Value with
                                    | PatternValue.Slot(slot) -> slot.Named
                                    | _ -> false

                                entity || attribute || value)
                            statements.PatternStatements

                    Ok(WanderValue.Bool(result))
                | value -> error $"Unexpected value passed to Pattern.extracts - {value}." None)
        )
    )

let patternToDataset (pattern: IPattern) : INetwork =
    match pattern.Dataset with
    | Some dataset -> dataset
    | _ -> failwith "TODO"

let extractFunction =
    failwith "TODO"
    // WanderValue.Function(
    //     Function.HostFunction(
    //         new HostFunction(fun args _ ->
    //             match args with
    //             | [ WanderValue.Pattern(pattern); WanderValue.Pattern(dataset) ] ->
    //                 (patternToDataset dataset).Extract pattern
    //                 |> List.map (fun res ->
    //                     res
    //                     |> Map.toSeq
    //                     |> Seq.map (fun (k, v) ->
    //                         (k.Name,
    //                          match v with
    //                          | Value.Int value -> WanderValue.Int value
    //                          | Value.Bytes value -> WanderValue.Bytes value
    //                          | Value.Identifier value -> WanderValue.Identifier value
    //                          | Value.String value -> WanderValue.String value))
    //                     |> Map.ofSeq
    //                     |> WanderValue.Namespace)
    //                 |> Array.ofList
    //                 |> WanderValue.Array
    //                 |> Ok
    //             | value -> error $"Unexpected value passed to Pattern.extract - {value}." None)
    //     )
    // )

let patternLib =
    WanderValue.Namespace(
        Map
            [ ("apply", applyFunction)
              ("count", countFunction)
              ("extract", extractFunction)
              ("extracts", extractsFunction)
              ("isDataset", isDatasetFunction)
              ("singleRoot", singleRootFunction) ]
    )
