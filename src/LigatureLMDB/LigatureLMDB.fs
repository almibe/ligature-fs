// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LMDB

open Ligature.Main
open LightningDB
open InMemoryEngine

type LigatureLMDB(env: LightningEnvironment) =
    let idsDB = "ids"
    let networkToIdDB = "networkToId"
    let idToNetworkDB = "idToNetwork"
    let elementToIdDB = "elementToId"
    let idToElementDB = "idToElement"
    let entryDB = "entry"
    let store = newInMemoryEngine ()

    let dbConfig =
        let config = DatabaseConfiguration()
        config.Flags <- DatabaseOpenFlags.Create
        config

    // Look up a Network by name, if it exists return its Id as bytes otherwise create a new network
    // and return the new id.
    let checkOrCreateNetwork (tx: LightningTransaction) (networkName: string) : Result<byte[], LigatureError> =
        use networkToId = tx.OpenDatabase(networkToIdDB, dbConfig)
        let networkName = System.Text.Encoding.UTF8.GetBytes networkName
        let struct (resultCode, key, value) = tx.Get(networkToId, networkName)

        if resultCode = MDBResultCode.Success then
            Ok(value.CopyToNewArray())
        else
            use ids = tx.OpenDatabase(idsDB, dbConfig)

            let struct (resultCode, key, value) =
                tx.Get(ids, System.Text.Encoding.UTF8.GetBytes "id")

            let nextId =
                if resultCode = MDBResultCode.Success then
                    System.BitConverter.ToUInt64(value.CopyToNewArray()) + 1UL
                    |> System.BitConverter.GetBytes
                else
                    0UL |> System.BitConverter.GetBytes

            tx.Put(ids, System.Text.Encoding.UTF8.GetBytes "id", nextId)

            use idToNetwork = tx.OpenDatabase(idToNetworkDB, dbConfig)
            tx.Put(networkToId, networkName, nextId)
            tx.Put(idToNetwork, nextId, networkName)
            tx.Commit()
            Ok nextId

    let clearNetworkDB (tx: LightningTransaction) (networkId: byte[]) db =
        use db = tx.OpenDatabase(db, dbConfig)
        use cursor = tx.CreateCursor(db)
        cursor.SetRange(networkId)
        let struct (result, key, value) = cursor.GetCurrent()

        if result = MDBResultCode.Success then
            if key.CopyToNewArray()[0 .. networkId.Length] = networkId then
                // if it exists then delete all matching entries
                failwith "TODO"
            else
                ()
        else
            ()

    let removeNetwork (tx: LightningTransaction) (networkName: NetworkName) =
        use networkToId = tx.OpenDatabase(networkToIdDB, dbConfig)
        let networkName = System.Text.Encoding.UTF8.GetBytes networkName
        let struct (resultCode, key, value) = tx.Get(networkToId, networkName)

        if resultCode = MDBResultCode.Success then
            tx.Delete(networkToId, networkName)
            use idToNetwork = tx.OpenDatabase(idToNetworkDB, dbConfig)
            tx.Delete(idToNetwork, value.CopyToNewArray())
            clearNetworkDB tx networkName entryDB
        else
            ()

    let clearNetwork tx (networkId: byte[]) = clearNetworkDB tx networkId entryDB

    let setNetwork (tx: LightningTransaction) (networkId: byte[]) (network: Network) =
        use entryDB = tx.OpenDatabase(entryDB, dbConfig)

        Set.iter
            (fun entry ->
                match entry with
                | Entry.Extends { element = element; concept = concept } -> failwith "TODO"
                | Entry.NotExtends { element = element; concept = concept } -> failwith "Not Implemented"
            // | Entry.Role { first = first
            //                second = second
            //                role = role } ->
            //     let value = Array.concat [ networkId ]
            //     tx.Put(entryDB, value, [||]) |> ignore
            )
            network

    interface System.IDisposable with
        member _.Dispose() : unit = env.Dispose()

    interface LigatureEngine with
        member _.AddNetwork networkName =
            store.AddNetwork networkName
            use tx = env.BeginTransaction()
            checkOrCreateNetwork tx networkName
            Ok()

        member _.RemoveNetwork networkName =
            store.RemoveNetwork networkName
            use tx = env.BeginTransaction()
            removeNetwork tx networkName
            Ok()

        member _.Networks() = store.Networks()

        member _.AddEntries name network =
            store.AddEntries name network
            failwith "TODO"

        member _.RemoveEntries name network =
            store.RemoveEntries name network
            failwith "TODO"

        member _.ReadNetwork(networkName: NetworkName) : Result<Set<Entry>, LigatureError> =
            store.ReadNetwork networkName

        member _.SetNetwork name network : Result<unit, LigatureError> =
            store.SetNetwork name network
            use tx = env.BeginTransaction()

            match checkOrCreateNetwork tx name with
            | Ok networkId ->
                clearNetwork tx networkId
                setNetwork tx networkId network
                Ok()
            | Error _ -> failwith "TODO"

        member _.FilterEntries (networkName: NetworkName) (query: Network) : Result<Set<Entry>, LigatureError> =
            store.FilterEntries networkName query


let openStore (path: string) : LigatureEngine =
    let envConfig = new EnvironmentConfiguration()
    envConfig.MaxDatabases <- 6
    let env = new LightningEnvironment(path, envConfig)
    env.Open()
    LigatureLMDB(env)
