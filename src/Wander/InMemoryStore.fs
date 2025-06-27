// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.InMemoryStore

open Ligature.Model

type InMemoryStore() =
    let mutable store: Map<string, Assertions> = Map.empty

    interface ILigatureStore with
        member this.KBs() : string seq = store.Keys |> seq

        member this.AddKB(name: string) : unit =
            match store.TryFind name with
            | None -> store <- Map.add name Set.empty store
            | _ -> ()

        member this.RemoveKB(name: string) : unit = store <- store.Remove name

        member this.AssertKB (name: string) (network: Assertions) : unit =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb ->
                let newAsserts = Set.union kb network
                store <- Map.add name newAsserts store

        member this.UnassertKB (name: string) (network: Assertions) : unit =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb ->
                let newAsserts = Set.difference kb network
                store <- Map.add name newAsserts store

        member this.ReadAssertsKB(name: string) : Result<Assertions, LigatureError> =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb -> Ok kb

        member this.ReadDefinitionsKB(arg: string) : Result<Definitions, LigatureError> =
            raise (System.NotImplementedException())

        member this.DefineKB (arg: string) (arg_1: Definitions) : unit =
            raise (System.NotImplementedException())

        member this.UndefineKB (arg: string) (arg_1: Definitions) : unit =
            raise (System.NotImplementedException())
