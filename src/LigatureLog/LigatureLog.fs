// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Log

open Ligature.Model
open Ligature.InMemoryStore
open FASTER.core
open System.IO

type LogStore(path: Option<string>) =

    let store =
        match path with
        | None ->
            let tempDir = Directory.CreateTempSubdirectory()
            let settings = new FasterLogSettings(tempDir.FullName, true)
            new FasterLog(settings)
        | Some path ->
            let settings = new FasterLogSettings(path)
            new FasterLog(settings)

    let inmem =
        match path with
        | Some _ ->
            let inmem = new InMemoryStore()
            let iter = store.Scan(store.BeginAddress, store.TailAddress)
            let mutable cont = true

            while cont do
                cont <-
                    iter.TryConsumeNext
                        { new ILogEntryConsumer with
                            member _.Consume(e, _, _) = failwith "TODO" }

            inmem
        | None -> new InMemoryStore()

    interface ILigatureStore with
        member this.KBs() : Term seq = failwith "TODO"

        member this.AddKB(name: Term) : unit = failwith "TODO"

        member this.RemoveKB(name: Term) : unit = failwith "TODO"

        member this.AssertKB (name: Term) (newAssertions: Assertions) : unit = failwith "TODO"

        member this.UnassertKB (name: Term) (network: Assertions) : unit = failwith "TODO"

        member this.ReadAssertsKB(name: Term) : Result<Assertions, LigatureError> = failwith "TODO"

        member this.ReadDefinitionsKB(name: Term) : Result<Definitions, LigatureError> = failwith "TODO"

        member this.DefineKB (name: Term) (newDefinitions: Definitions) : unit = failwith "TODO"

        member this.UndefineKB (name: Term) (toRemove: Definitions) : unit = failwith "TODO"

        member this.ReadKB(arg: Term) : Result<KnowledgeBase, LigatureError> = failwith "TODO"

        member this.IsConsistent(name: Term) : Result<bool, LigatureError> = failwith "TODO"

        member this.IsEquivalent (arg1: Term) (arg2: ConceptExpr) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.IsInstance (arg1: Term) (arg2: Element) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.IsSatisfiable (arg1: Term) (arg2: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.IsSubsumedBy (arg1: Term) (arg2: ConceptExpr) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.Query (arg1: Term) (arg2: ConceptExpr) : Result<Element seq, LigatureError> =
            failwith "Not Implemented"

        member this.TableauModels(arg1: Term) : Result<Set<Assertions>, LigatureError> = failwith "Not Implemented"
