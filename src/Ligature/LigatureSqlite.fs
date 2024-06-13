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
    let store = InMemoryStore.empty ()
    let conn = new SqliteConnection($"Data Source={path}")

    member this.initialize() =
        conn.Open()

        conn
        |> Db.newCommand
            "create table if not exists events (
	        id int PRIMARY KEY,
   	        type text NOT NULL,
            network text not null,
            ligature text
            )"
        |> Db.exec

        conn
        |> Db.newCommand "select * from events"
        |> Db.query (fun rd ->
            let event = Event.ofDataReader rd

            match event.Type with
            | "AN" -> store.addNetwork event.Network
            | "RN" -> store.removeNetwork event.Network
            | "AS" ->
                let script =
                    match event.Ligature with
                    | Some(script) -> script
                    | _ -> failwith "Error"

                match run script (newBindings ()) with
                | Ok(WanderValue.Network(value)) -> store.add event.Network value |> ignore
                | _ -> failwith "Error"
            | "RS" -> 
                let script =
                    match event.Ligature with
                    | Some(script) -> script
                    | _ -> failwith "Error"

                match run script (newBindings ()) with
                | Ok(WanderValue.Network(value)) -> store.remove event.Network value |> ignore
                | _ -> failwith "Error"
            | _ -> failwith "Error")

    interface LigatureStore with
        member this.addNetwork networkName =
            store.addNetwork (networkName)

            conn
            |> Db.newCommand "insert into events(type, network) values('AN', @network)"
            |> Db.setParams [ "network", SqlType.String networkName ]
            |> Db.exec

        member this.removeNetwork networkName =
            store.removeNetwork (networkName)

            conn
            |> Db.newCommand "insert into events(type, network) values('RN', @network)"
            |> Db.setParams [ "network", SqlType.String networkName ]
            |> Db.exec

        member this.networks() = store.networks ()

        member this.add networkName network =
            store.add networkName network
            let ligature = writeLigature network

            conn
            |> Db.newCommand "insert into events(type, network, ligature) values('AS', @network, @ligature)"
            |> Db.setParams [ "network", SqlType.String networkName; "ligature", SqlType.String ligature ]
            |> Db.exec

            Ok(())

        member this.remove networkName network =
            store.remove networkName network
            let ligature = writeLigature network

            conn
            |> Db.newCommand "insert into events(type, network, ligature) values('RS', @network, @ligature)"
            |> Db.setParams [ "network", SqlType.String networkName ]
            |> Db.setParams [ "ligature", SqlType.String ligature ]
            |> Db.exec

            Ok(())

        member this.read name = store.read (name)
