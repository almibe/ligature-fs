// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.InMemoryDataset

open Ligature

let isLiteralPattern (pattern: PatternStatement) =
    match pattern with
    | { Entity = PatternIdentifier.Slot(_) } -> false
    | { Attribute = PatternIdentifier.Slot(_) } -> false
    | { Value = PatternValue.Slot(_) } -> false
    | _ -> true

let literalPatternToStatement (pattern: PatternStatement) : Statement =
    let (entity, attribute) =
        match pattern with
        | { Entity = PatternIdentifier.Identifier(entity)
            Attribute = PatternIdentifier.Identifier(attribute) } -> (entity, attribute)
        | _ -> failwith "Error"

    let value =
        match pattern.Value with
        | PatternValue.Value value -> value
        | _ -> failwith "Error"

    { Entity = entity
      Attribute = attribute
      Value = value }

type InMemoryDataset =
    val statements: Set<Statement>
    new(statementsArg) = { statements = statementsArg }

    override this.Equals(other) =
        match other with
        | :? InMemoryDataset as ds -> (this.statements) = (ds.statements)
        | _ -> failwith "TODO"

    override this.GetHashCode() = this.statements.GetHashCode()

    interface IDataset with
        member this.RunQuery query =
            // let mutable result = Set.empty
            // Set.iter (fun pattern ->
            //     if isLiteralPattern pattern then
            //         let statement = literalPatternToStatement pattern
            //         result <- Set.add statement result
            //     else
            //         failwith "TODO"
            //     ) query
            failwith "TODO"
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

        // Ok(new InMemoryDataset(results))

        member this.AllStatements() : Result<Statement list, LigatureError> = Ok(List.ofSeq this.statements)
        member this.Contains(arg1: Pattern) : Result<bool, LigatureError> = failwith "Not Implemented"
        member this.Count(arg1: Pattern) : Result<int64, LigatureError> = failwith "Not Implemented"

let emptyInMemoryDataset = new InMemoryDataset(Set.empty)
