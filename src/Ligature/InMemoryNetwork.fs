// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemoryNetwork

open Main

// let isLiteralPattern (pattern: PatternStatement) =
//     match pattern with
//     | { Entity = PatternIdentifier.Slot(_) } -> false
//     | { Attribute = PatternIdentifier.Slot(_) } -> false
//     | { Value = PatternValue.Slot(_) } -> false
//     | _ -> true

// let literalPatternToStatement (pattern: PatternStatement) : Statement =
//     let (entity, attribute) =
//         match pattern with
//         | { Entity = PatternIdentifier.Identifier(entity)
//             Attribute = PatternIdentifier.Identifier(attribute) } -> (entity, attribute)
//         | _ -> failwith "Error"

//     let value =
//         match pattern.Value with
//         | PatternValue.Value value -> value
//         | _ -> failwith "Error"

//     { Entity = entity
//       Attribute = attribute
//       Value = value }

type InMemoryNetwork =
    val statements: Set<Statement>
    new(statementsArg: Set<Statement>) = { statements = statementsArg }

    override this.Equals(other) =
        match other with
        | :? InMemoryNetwork as ds -> (this.statements) = (ds.statements)
        | _ -> failwith "TODO"

    override this.GetHashCode() = this.statements.GetHashCode()

    interface INetwork with
        member _.all() =
            failwith "TODO"
        member _.find entity attribute value =
            failwith "TDOO"

        // member this.RunQuery query =
        // let mutable result = Set.empty
        // Set.iter (fun pattern ->
        //     if isLiteralPattern pattern then
        //         let statement = literalPatternToStatement pattern
        //         result <- Set.add statement result
        //     else
        //         failwith "TODO"
        //     ) query
        // failwith "TODO"
        // let results =
        //     match entity with
        //     | Some(entity) -> Set.filter (fun statement -> statement.Entity = entity) this.statements
        //     | None -> this.statements

        // let results =
        //     match attribute with
        //     | Some(attribute) -> Set.filter (fun statement -> statement.Attribute = attribute) results
        //     | None -> results

        // let results =
        //     match value with
        //     | Some(value) -> Set.filter (fun statement -> statement.Value = value) results
        //     | None -> results

        // Ok(new InMemoryNetwork(results))

        // member this.AllStatements() : Result<Statement list, LigatureError> = Ok(List.ofSeq this.statements)
        // member this.Contains(arg1: IPattern) : Result<bool, LigatureError> = failwith "Not Implemented"
        // member this.Count(arg1: IPattern) : Result<int64, LigatureError> = failwith "Not Implemented"
        // member this.Extract(arg1: IPattern) : Map<Slot, Value> list = failwith "Not Implemented"

// let emptyInMemoryNetwork = new InMemoryNetwork(Set.empty)

// type InMemoryNetwork(statements: Set<Statement>) =
//     interface INetwork with

let emptyNetwork = InMemoryNetwork(Set.empty)
