// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Pattern

open Ligature.Wander.Model
open Ligature
open Ligature.Wander.InMemoryDataset

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
                | [ WanderValue.Pattern(pattern); WanderValue.Record(data) ] -> failwith "TODO"
                    // Ok(
                    //     WanderValue.Dataset(
                    //         Set.map
                    //             (fun (statement: PatternStatement) ->
                    //                 match statement with
                    //                 | { Entity = PatternIdentifier.Identifier(_)
                    //                     Attribute = PatternIdentifier.Identifier(_)
                    //                     Value = PatternValue.Value(_) } -> 
                    //                         statement
                    //                 | _ -> 
                    //                     let entity =
                    //                         match statement.Entity with
                    //                         | PatternIdentifier.Identifier(identifier) -> PatternIdentifier.Identifier(identifier)
                    //                         | PatternIdentifier.Slot(slot) ->
                    //                             let name = readSlot slot
                    //                             if name <> "" then
                    //                                 match data.TryFind name with
                    //                                 | Some value -> 
                    //                                     match value with
                    //                                     | WanderValue.Identifier identifier -> PatternIdentifier.Identifier identifier
                    //                                     | _ -> failwith "Error"
                    //                                 | None -> failwith "Error"
                    //                             else
                    //                                 failwith "Error"

                    //                     let attribute =
                    //                         match statement.Attribute with
                    //                         | PatternIdentifier.Identifier(identifier) -> PatternIdentifier.Identifier(identifier)
                    //                         | PatternIdentifier.Slot(slot) ->
                    //                             failwith "TODO"

                    //                     let value =
                    //                         match statement.Value with
                    //                         | PatternValue.Value(v) -> PatternValue.Value(v)
                    //                         | PatternValue.Slot(slot) ->
                    //                             failwith "TODO"
                    //                     { Entity = entity; Attribute = attribute; Value = value })
                    //             pattern.AllStatements
                    //     )
                    // )
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

// let isDatasetFunction<'t> =
//     WanderValue.Function(
//         Function.HostFunction(
//             new HostFunction(fun args _ ->
//                 match args with
//                 | [ WanderValue.Pattern(statements) ] ->
//                     let result =
//                         Set.forall
//                             (fun (statement: PatternStatement) ->
//                                 match (statement.Entity, statement.Attribute, statement.Value) with
//                                 | (PatternIdentifier.Slot(_), _, _) -> false
//                                 | (_, PatternIdentifier.Slot(_), _) -> false
//                                 | (_, _, PatternValue.Slot(_)) -> false
//                                 | _ -> true)
//                             statements.AllStatements

//                     Ok(WanderValue.Bool(result))
//                 | value -> error $"Unexpected value passed to Pattern.isDataset - {value}." None)
//         )
//     )

let extractsFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Pattern(statements) ] ->
                    let result =
                        Set.exists (fun (statement: PatternStatement) -> failwith "TODO") statements.Statements

                    Ok(WanderValue.Bool(result))
                | value -> error $"Unexpected value passed to Pattern.extracts - {value}." None)
        )
    )

let patternLib<'t> =
    WanderValue.Record(
        Map
            [ ("apply", applyFunction)
              ("count", countFunction)
              ("extracts", extractsFunction)
//              ("isDataset", isDatasetFunction) ]
    ])
