// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemoryNetwork

open Ligature.Main
open System
open System.Collections.Generic

type InMemoryNetwork(network: Set<Triple>) =
    interface Network with
        member this.Write() = network
        member this.Count() = Set.count network

        member this.Merge other =
            InMemoryNetwork(Set.union network (other.Write()))

        member this.Minus other =
            InMemoryNetwork(Set.difference network (other.Write()))

        member this.Apply(values: Map<Slot, Value>) = failwith "TODO"
        member this.Educe network : Set<Map<Slot, Value>> = failwith "TODO"
        member this.Query pattern trans : Network = failwith "TODO"
        member this.Infer pattern trans : Network = failwith "TODO"

let empty () = InMemoryNetwork(Set [])
