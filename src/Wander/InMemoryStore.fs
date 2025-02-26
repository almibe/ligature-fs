// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.InMemoryStore

open Ligature.Model
open Wander.Fns.TinyDL

type KB = { aBox: Network; tBox: Network }

type InMemoryStore() =
    let mutable store: Map<string, KB> = Map.empty

    interface ILigatureStore with
        member this.KnowledgeBases() : string seq = store.Keys |> seq

        member this.AddKnowledgeBase(name: string) : unit =
            match store.TryFind name with
            | None -> store <- Map.add name { aBox = Set.empty; tBox = Set.empty } store
            | _ -> ()

        member this.RemoveKnowledgeBase(name: string) : unit = store <- store.Remove name

        member this.AssertKnowledgeBase (name: string) (network: Network) : unit =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb ->
                let newAsserts = Set.union kb.aBox network
                store <- Map.add name { aBox = newAsserts; tBox = kb.tBox } store

        member this.DefineKnowledgeBase (name: string) (network: Network) : unit =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb ->
                let newDefs = Set.union kb.tBox network
                store <- Map.add name { aBox = kb.aBox; tBox = newDefs } store

        member this.UnassertKnowledgeBase (name: string) (network: Network) : unit =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb ->
                let newAsserts = Set.difference kb.aBox network
                store <- Map.add name { aBox = newAsserts; tBox = kb.tBox } store

        member this.UndefineKnowledgeBase (name: string) (network: Network) : unit =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb ->
                let newDefs = Set.difference kb.tBox network
                store <- Map.add name { aBox = kb.aBox; tBox = newDefs } store

        member this.ReadAsserts(name: string) : Result<Network, LigatureError> =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb -> Ok kb.aBox

        member this.ReadDefinitions(name: string) : Result<Network, LigatureError> =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb -> Ok kb.tBox

        member this.Read(name: string) : Result<Network, LigatureError> =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb -> infer kb.tBox kb.aBox
