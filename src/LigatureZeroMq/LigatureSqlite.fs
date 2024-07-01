// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LigatureSqlite

open Ligature.Main
open Ligature.LigatureStore
open System
open System.Collections.Generic
open System.Data
open Microsoft.Data.Sqlite
open Donald
open Donald.IDataReaderExtensions
open Ligature.Serialization
open Ligature.Wander.Main
open Ligature.Wander.Model

type Event =
    { Network: string
      Type: string
      Ligature: string option }

module Event =
    let ofDataReader (rd: IDataReader) : Event =
        { Network = rd.ReadString "network"
          Type = rd.ReadString "type"
          Ligature = rd.ReadStringOption "ligature" }

type LigatureSqlite(path: string) =
    let store = failwith "TODO" //InMemoryStore.empty ()
    let conn = new SqliteConnection($"Data Source={path}")

    member this.initialize() = failwith "TODO"

    interface LigatureStore with
        member this.addNetwork networkName = failwith "TODO"
        // store.addNetwork (networkName)

        // conn
        // |> Db.newCommand "insert into events(type, network) values('AN', @network)"
        // |> Db.setParams [ "network", SqlType.String networkName ]
        // |> Db.exec

        member this.removeNetwork networkName = failwith "TODO"
        // store.removeNetwork (networkName)

        // conn
        // |> Db.newCommand "insert into events(type, network) values('RN', @network)"
        // |> Db.setParams [ "network", SqlType.String networkName ]
        // |> Db.exec

        member this.networks() = failwith "TODO" //store.networks ()

        member this.add networkName network = failwith "TODO"
        // store.add networkName network |> ignore
        // let ligature = writeLigature network

        // conn
        // |> Db.newCommand "insert into events(type, network, ligature) values('AS', @network, @ligature)"
        // |> Db.setParams [ "network", SqlType.String networkName; "ligature", SqlType.String ligature ]
        // |> Db.exec

        // Ok(())

        member this.remove networkName network = failwith "TODO"
        // store.remove networkName network |> ignore
        // let ligature = writeLigature network

        // conn
        // |> Db.newCommand "insert into events(type, network, ligature) values('RS', @network, @ligature)"
        // |> Db.setParams [ "network", SqlType.String networkName ]
        // |> Db.setParams [ "ligature", SqlType.String ligature ]
        // |> Db.exec

        // Ok(())

        member this.read name = failwith "TODO" //store.read (name)
