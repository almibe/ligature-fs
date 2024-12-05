// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LMDB

open Ligature.Main

type LigatureLMDB() =
    interface LigatureStore with
        member _.AddNetwork networkName = failwith "TODO"

        member _.RemoveNetwork networkName = failwith "TODO"

        member _.Networks() =
                failwith "TODO"

        member _.Add name network = failwith "TODO"

        member _.Remove name network = failwith "TODO"

        member _.ClearNetwork networkName : Result<unit, LigatureError> = failwith "TODO"

        member _.Read(networkName: NetworkName) : Result<Set<Entry>, LigatureError> = 
            failwith "TODO"

        member this.Set name network : Result<unit, LigatureError> = 
            failwith "TODO"

        member _.Filter (networkName: NetworkName) (query: Network) : Result<Set<Entry>, LigatureError> =
            failwith "TODO"

let openStore (path: string) : LigatureStore =
    failwith "TODO"
