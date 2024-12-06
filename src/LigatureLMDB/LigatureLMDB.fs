// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LMDB

open Ligature.Main
open LightningDB

type LigatureLMDB(env: LightningEnvironment) =
    let idsDB = "ids"
    let networkToIdDB = "networkToId"
    let idToNetworkDB = "idToNetwork"
    let elementToIdDB = "elementToId"
    let idToElementDB = "idToElement"
    let fstDB = "fst"
    let ftsDB = "fts"
    let sftDB = "sft"
    let stfDB = "stf"
    let tfsDB = "tfs"
    let tsfDB = "tsf"
    let dbConfig = 
        let config = DatabaseConfiguration()
        config.Flags <- DatabaseOpenFlags.Create
        config

    // Look up a Network by name, if it exists return its Id as bytes otherwise create a new network
    // and return the new id.
    let checkOrCreateNetwork (tx: LightningTransaction) (networkName: string): Result<byte[], LigatureError> =
        use networkToId = tx.OpenDatabase(networkToIdDB, dbConfig)
        let networkName = System.Text.Encoding.UTF8.GetBytes networkName
        let struct (resultCode, key, value) = tx.Get(networkToId, networkName)
        if resultCode = MDBResultCode.Success then
            Ok(value.CopyToNewArray())
        else
            use ids = tx.OpenDatabase(idsDB, dbConfig)
            let struct (resultCode, key, value) = tx.Get(ids, System.Text.Encoding.UTF8.GetBytes "id")
            let nextId = 
                if resultCode = MDBResultCode.Success then
                    System.BitConverter.ToUInt64 (value.CopyToNewArray()) + 1UL |> System.BitConverter.GetBytes
                else
                    0UL |> System.BitConverter.GetBytes

            tx.Put(ids, System.Text.Encoding.UTF8.GetBytes "id", nextId)

            use idToNetwork = tx.OpenDatabase(idToNetworkDB, dbConfig)
            tx.Put(networkToId, networkName, nextId)
            tx.Put(idToNetwork, nextId, networkName)
            tx.Commit ()
            Ok nextId

    let clearNetworkDB tx networkId db = 
        use db = tx.OpenDatabase(db, dbConfig)
        use cursor = tx.CreateCursor(db)
        cursor.SetRange(networkId)
        let (result, key, value) = cursor.GetCurrent()
        if result = MDBResultCode.Success then
            failwith "TODO"
        else
            failwith "TODO"

    let clearNetwork tx networkId = 
        clearNetworkDB tx networkId fstDB
        clearNetworkDB tx networkId ftsDB
        clearNetworkDB tx networkId sftDB
        clearNetworkDB tx networkId stfDB
        clearNetworkDB tx networkId tfsDB
        clearNetworkDB tx networkId tsfDB

    let setNetwork tx networkId network = failwith "TODO"

    interface LigatureStore with
        member _.AddNetwork networkName = failwith "TODO"

        member _.RemoveNetwork networkName = failwith "TODO"

        member _.Networks() =
            let mutable results = Set.empty
            use tx = env.BeginTransaction()
            use db = tx.OpenDatabase(networkToIdDB, dbConfig)
            use cursor = tx.CreateCursor(db)
            let mutable cont = true
            while cont do
                let struct (found, key, _) = cursor.Next()
                if found = MDBResultCode.NotFound then
                    cont <- false
                else
                    let name = key.CopyToNewArray() |> System.Text.Encoding.UTF8.GetString
                    results <- Set.add (NetworkName(name)) results
            Ok results

        member _.Add name network = failwith "TODO"

        member _.Remove name network = failwith "TODO"

        member _.ClearNetwork networkName : Result<unit, LigatureError> = failwith "TODO"

        member _.Read(networkName: NetworkName) : Result<Set<Entry>, LigatureError> =
            use tx = env.BeginTransaction()
            use networkToId = tx.OpenDatabase(networkToIdDB, dbConfig)
            let networkName = System.Text.Encoding.UTF8.GetBytes networkName
            let struct (result, key, value) = tx.Get(networkToId, networkName)
            if result = MDBResultCode.NotFound then
                failwith "TODO"
            else
                failwith "TODO"

        member _.Set name network : Result<unit, LigatureError> = 
            use tx = env.BeginTransaction ()
            match checkOrCreateNetwork tx name with
            | Ok networkId ->
                clearNetwork tx networkId
                setNetwork tx networkId network
                Ok ()
            | Error _ -> failwith "TODO"

        member _.Filter (networkName: NetworkName) (query: Network) : Result<Set<Entry>, LigatureError> =
            failwith "TODO"

let openStore (path: string) : LigatureStore =
    let envConfig = new EnvironmentConfiguration()
    envConfig.MaxDatabases <- 20
    let env = new LightningEnvironment(path, envConfig)
    env.Open()
    LigatureLMDB(env)
