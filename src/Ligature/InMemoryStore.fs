// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemoryStore

open Ligature.Model
open Interpreter

type InMemoryStore() =
    let mutable store: Map<Term, Assertions * Definitions> = Map.empty

    interface ILigatureStore with
        member this.KBs() : Term seq = store.Keys |> seq

        member this.AddKB(name: Term) : unit =
            match store.TryFind name with
            | None -> store <- Map.add name (Set.empty, Set.empty) store
            | _ -> ()

        member this.RemoveKB(name: Term) : unit = store <- store.Remove name

        member this.AssertKB (name: Term) (newAssertions: Assertions) : unit =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some(assertions, defintions) ->
                let newAsserts = Set.union assertions newAssertions
                store <- Map.add name (newAsserts, defintions) store

        member this.UnassertKB (name: Term) (network: Assertions) : unit =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some(assertions, definitions) ->
                let newAsserts = Set.difference assertions network
                store <- Map.add name (newAsserts, definitions) store

        member this.ReadAssertsKB(name: Term) : Result<Assertions, LigatureError> =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some(assertions, _) -> Ok assertions

        member this.ReadDefinitionsKB(name: Term) : Result<Definitions, LigatureError> =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some(_, definitions) -> Ok definitions

        member this.DefineKB (name: Term) (newDefinitions: Definitions) : unit =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some(assertions, definitions) ->
                let newDefs = Set.union definitions newDefinitions
                store <- Map.add name (assertions, newDefs) store

        member this.UndefineKB (name: Term) (toRemove: Definitions) : unit =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some(assertions, definitions) ->
                let newDefs = Set.difference definitions toRemove
                store <- Map.add name (assertions, newDefs) store

        member this.ReadKB(arg: Term) : Result<KnowledgeBase, LigatureError> =
            raise (System.NotImplementedException())

        member this.IsConsistent(name: Term) : Result<bool, LigatureError> =
            match store.TryFind name with
            | Some(assertions, definitions) -> isConsistent definitions assertions
            | _ -> error $"Knowledge Base {name} doesn't exist." None

        member this.IsEquivalent (arg1: Term) (arg2: ConceptExpr) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.IsInstance (arg1: Term) (arg2: Element) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.IsSatisfiable (arg1: Term) (arg2: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.IsSubsumedBy (arg1: Term) (arg2: ConceptExpr) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.Instances (arg: Term) (arg_1: ConceptExpr) : Result<Element seq, LigatureError> =
            raise (System.NotImplementedException())

        member this.Query (arg1: Term) (arg2: Pattern) : Result<Map<Slot, Element>, LigatureError> =
            failwith "Not Implemented"

        member this.TableauModels(arg1: Term) : Result<Set<Assertions>, LigatureError> = failwith "Not Implemented"
