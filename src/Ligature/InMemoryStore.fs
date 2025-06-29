// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemoryStore

open Ligature.Model

type InMemoryStore() =
    let mutable store: Map<Term, Assertions> = Map.empty

    interface ILigatureStore with
        member this.KBs() : Term seq = store.Keys |> seq

        member this.AddKB(name: Term) : unit =
            match store.TryFind name with
            | None -> store <- Map.add name Set.empty store
            | _ -> ()

        member this.RemoveKB(name: Term) : unit = store <- store.Remove name

        member this.AssertKB (name: Term) (network: Assertions) : unit =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb ->
                let newAsserts = Set.union kb network
                store <- Map.add name newAsserts store

        member this.UnassertKB (name: Term) (network: Assertions) : unit =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb ->
                let newAsserts = Set.difference kb network
                store <- Map.add name newAsserts store

        member this.ReadAssertsKB(name: Term) : Result<Assertions, LigatureError> =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb -> Ok kb

        member this.ReadDefinitionsKB(arg: Term) : Result<Definitions, LigatureError> =
            raise (System.NotImplementedException())

        member this.DefineKB (arg: Term) (arg_1: Definitions) : unit =
            raise (System.NotImplementedException())

        member this.UndefineKB (arg: Term) (arg_1: Definitions) : unit =
            raise (System.NotImplementedException())

        member this.ReadKB(arg: Term) : Result<KnowledgeBase, LigatureError> =
            raise (System.NotImplementedException())

        member this.IsConsistent(arg: Term) : Result<bool, LigatureError> =
            raise (System.NotImplementedException())

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

        member this.TableauModels(arg1: Term) : Result<Set<Assertions>, LigatureError> = failwith "Not Implemented"
