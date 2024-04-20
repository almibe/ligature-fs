// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.InMemoryDataset

open Ligature

type InMemoryDataset =
    val statements: Set<Statement>
    new(statementsArg) = { statements = statementsArg }

    override this.Equals(other) =
        match other with
        | :? InMemoryDataset as ds -> (this.statements) = (ds.statements)
        | _ -> failwith "TODO"

    override this.GetHashCode() = this.statements.GetHashCode()

    interface IDataset with
        member this.MatchStatements entity attribute value =
            let results =
                match entity with
                | Some(entity) -> Set.filter (fun statement -> statement.Entity = entity) this.statements
                | None -> this.statements

            let results =
                match attribute with
                | Some(attribute) -> Set.filter (fun statement -> statement.Attribute = attribute) results
                | None -> results

            let results =
                match value with
                | Some(value) -> Set.filter (fun statement -> statement.Value = value) results
                | None -> results

            Ok(new InMemoryDataset(results))

        member this.AllStatements() : Result<Statement list, LigatureError> = Ok(List.ofSeq this.statements)

let emptyInMemoryDataset = new InMemoryDataset(Set.empty)
