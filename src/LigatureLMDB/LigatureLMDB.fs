// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Lmdb

open Model
open Wander.Main
open Wander.Library
open Wander.Model
open LightningDB

type IStore =
    abstract Networks: unit -> string seq
    abstract AddNetwork: string -> unit
    abstract RemoveNetwork: string -> unit
    abstract Merge: string -> Network -> unit
    abstract Remove: string -> Network -> unit
    abstract Read: string -> Network

type InMemoryStore(store: Ref<Map<string, Network>>) =

    interface IStore with
        member _.AddNetwork(name: string) : unit =
            match store.contents.TryFind name with
            | Some _ -> ()
            | None -> store.Value <- Map.add name Set.empty store.contents

        member this.Merge (name: string) (network: Network) : unit =
            match store.contents.TryFind name with
            | Some currentNetwork -> store.Value <- Map.add name (Set.union currentNetwork network) store.contents
            | _ -> failwith "Not Implemented"

        member this.Networks() : string seq = store.contents.Keys

        member this.Read(name: string) : Network =
            match store.contents.TryFind name with
            | Some currentNetwork -> currentNetwork
            | _ -> failwith "Not Implemented"

        member this.Remove (name: string) (network: Network) : unit =
            match store.contents.TryFind name with
            | Some currentNetwork -> store.Value <- Map.add name (Set.difference currentNetwork network) store.contents
            | _ -> failwith "Not Implemented"

        member this.RemoveNetwork(name: string) : unit =
            store.Value <- Map.remove name store.contents

let createInMemoryStore () = InMemoryStore(ref Map.empty)

let createStoreActions (store: IStore) (baseActions: Actions) : Actions =
    baseActions.Add(
        Element "merge",
        Action.Stack(
            { doc = "Reads a Network and Name off the Stack and merges that Network into the target Network."
              examples = [ "{a b c} \"test\" merge" ]
              pre = "Network"
              post = "" },
            fun stack ->
                match stack with
                | Any.Literal networkName :: Any.Network network :: tail ->
                    store.Merge networkName network
                    Ok tail
                | _ -> failwith "TODO"
        )
    )
    |> Map.add
        (Element "delete")
        (Action.Stack(
            { doc =
                "Reads a Network off the Stack and removes all of the Triples in that Network from the target Network."
              examples = []
              pre = "Network"
              post = "" },
            fun stack -> failwith "TODO"
        ))
    |> Map.add
        (Element "read")
        (Action.Stack(
            { doc = "Push the target Network on to the Stack."
              examples = [ "read" ]
              pre = ""
              post = "Network" },
            fun stack -> failwith "TODO" //Ok(Any.Network(store.Read networkName) :: stack)
        ))

// let createStore (location: string): IStore =
//     let env = LightningEnvironment(location)
//     env.Open()
