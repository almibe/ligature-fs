// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Pattern

open Ligature.Wander.Model
open Ligature
open Ligature.Wander.InMemoryDataset
open Ligature.Wander.Pattern

let patternStatementToStatement (pattern: PatternStatement) : Statement option =
    match pattern with
    | { Entity = PatternIdentifier.Identifier(entity)
        Attribute = PatternIdentifier.Identifier(attribute) } -> failwith "TODO"
    | _ -> failwith "TODO"

let applyFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Pattern(pattern); WanderValue.Record(data) ] ->
                    Ok(
                        WanderValue.Pattern(
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

                                    (slot, PatternValue.Value v)
                                | _ -> failwith "Error")
                            |> Map.ofSeq
                            |> pattern.Apply
                        )
                    )
                | value -> error $"Unexpected value passed to Pattern.isDataset - {value}." None)
        )
    )

let extractFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Pattern(pattern); WanderValue.Pattern(data) ] ->
                    pattern.Extract data
                    |> Array.map (fun res ->
                        res
                        |> Map.toSeq
                        |> Seq.map (fun (k, v) ->
                            (k.Name,
                             match v with
                             | PatternValue.Slot s -> failwith "TODO"
                             | PatternValue.Value(Value.Int value) -> WanderValue.Int value
                             | PatternValue.Value(Value.Bytes value) -> WanderValue.Bytes value
                             | PatternValue.Value(Value.Identifier value) -> WanderValue.Identifier value
                             | PatternValue.Value(Value.String value) -> WanderValue.String value))
                        |> Map.ofSeq
                        |> WanderValue.Record)
                    |> WanderValue.Array
                    |> Ok
                | value -> error $"Unexpected value passed to Pattern.isDataset - {value}." None)
        )
    )

let countFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Pattern(pattern) ] -> Ok(WanderValue.Int(Set.count pattern.Statements))
                | value -> error $"Unexpected value - {value}." None)
        )
    )

let isDatasetFunction<'t> =
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
                            statements.Statements

                    Ok(WanderValue.Bool(result))
                | value -> error $"Unexpected value passed to Pattern.isDataset - {value}." None)
        )
    )

let extractsFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Pattern(statements) ] ->
                    let result =
                        Set.exists (fun (statement: PatternStatement) -> 
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
                            entity || attribute || value) statements.Statements
                    Ok(WanderValue.Bool(result))
                | value -> error $"Unexpected value passed to Pattern.extracts - {value}." None)
        )
    )

let patternLib<'t> =
    WanderValue.Record(
        Map
            [ ("apply", applyFunction)
              ("count", countFunction)
              ("extract", extractFunction)
              ("extracts", extractsFunction)
              ("isDataset", isDatasetFunction) ]
    )
