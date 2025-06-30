// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Store

open Ligature.Model
open LiteDB
open System.Collections.Concurrent
open System.Collections.Generic

type LigatureStore(db: LiteDatabase) =
    interface ILigatureStore with
        member this.AddKB(name: Term) : unit =
            let kbs = db.GetCollection "kb"
            ignore <| kbs.Insert(new BsonDocument(dict [ "name", new BsonValue "test" ]))

        member this.AssertKB (arg1: Term) (arg2: Assertions) : unit = failwith "Not Implemented"
        member this.DefineKB (arg1: Term) (arg2: Definitions) : unit = failwith "Not Implemented"

        member this.KBs() : Term seq =
            let kbs = db.GetCollection "kb"
            []

        member this.ReadAssertsKB(arg1: Term) : Result<Assertions, LigatureError> =
            let asserts = db.GetCollection "asserts"
            failwith "Not Implemented"

        member this.ReadDefinitionsKB(arg1: Term) : Result<Definitions, LigatureError> = failwith "Not Implemented"

        member this.RemoveKB(name: Term) : unit =
            let kbs = db.GetCollection "kb"

            kbs.DeleteMany(fun value -> false)
            //value.Contains(KeyValuePair("name", new BsonValue "test")))
            |> ignore

        member this.UnassertKB (arg1: Term) (arg2: Assertions) : unit = failwith "Not Implemented"
        member this.UndefineKB (arg1: Term) (arg2: Definitions) : unit = failwith "Not Implemented"
        member this.IsConsistent(arg1: Term) : Result<bool, LigatureError> = failwith "Not Implemented"

        member this.IsEquivalent (arg1: Term) (arg2: ConceptExpr) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.IsInstance (arg1: Term) (arg2: Instance) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.IsSatisfiable (arg1: Term) (arg2: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.IsSubsumedBy (arg1: Term) (arg2: ConceptExpr) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.Query (arg1: Term) (arg2: ConceptExpr) : Result<Instance, LigatureError> =
            failwith "Not Implemented"

        member this.ReadKB(arg1: Term) : Result<KnowledgeBase, LigatureError> = failwith "Not Implemented"
        member this.TableauModels(arg1: Term) : Result<Set<Assertions>, LigatureError> = failwith "Not Implemented"
