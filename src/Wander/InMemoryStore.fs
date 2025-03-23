// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.InMemoryStore

open Ligature.Model

type InMemoryStore() =
    let mutable store: Map<string, Network> = Map.empty

    interface ILigatureStore with
        member this.Stores() : string seq = store.Keys |> seq

        member this.AddStore(name: string) : unit =
            match store.TryFind name with
            | None -> store <- Map.add name Set.empty store
            | _ -> ()

        member this.RemoveStore(name: string) : unit = store <- store.Remove name

        member this.AssertStore (name: string) (network: Network) : unit =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb ->
                let newAsserts = Set.union kb network
                store <- Map.add name newAsserts store

        member this.UnassertStore (name: string) (network: Network) : unit =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb ->
                let newAsserts = Set.difference kb network
                store <- Map.add name newAsserts store

        member this.ReadAsserts(name: string) : Result<Network, LigatureError> =
            match store.TryFind name with
            | None -> failwith "Not found"
            | Some kb -> Ok kb
