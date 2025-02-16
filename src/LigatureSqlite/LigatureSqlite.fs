// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LigatureSqlite

open System.Data
open Microsoft.Data.Sqlite
open Donald
open Wander.Model
open Ligature.Model

// type Event =
//     { Network: string
//       Type: string
//       Ligature: string option }

// module Event =
//     let ofDataReader (rd: IDataReader) : Event =
//         { Network = rd.ReadString "network"
//           Type = rd.ReadString "type"
//           Ligature = rd.ReadStringOption "ligature" }

type LigatureSqlite(path: string) =
    let conn = new SqliteConnection($"Data Source={path}")

    member this.initialize() =
        conn.Open()

        conn
        |> Db.newCommand
            "create table if not exists terms (
   	        term text NOT NULL
            )"
        |> Db.exec

        conn
        |> Db.newCommand
            "create table if not exists networks (
   	        name text NOT NULL
            )"
        |> Db.exec

        conn
        |> Db.newCommand
            "create table if not exists triples (
   	        network int NOT NULL,
            entity int NOT NULL,
            attribute int NOT NULL,
            value int NOT NULL
            )"
        |> Db.exec

        // conn
        // |> Db.newCommand "select * from events"
        // |> Db.query (fun rd ->
        //     let event = Event.ofDataReader rd

        //     match event.Type with
        //     | "AN" -> store.addNetwork event.Network
        //     | "RN" -> store.removeNetwork event.Network
        //     | "AS" ->
        //         let script =
        //             match event.Ligature with
        //             | Some(script) -> script
        //             | _ -> failwith "Error"

        //         match run script Map.empty List.empty with
        //         | Ok([ WanderValue.Network(value) ]) -> store.add event.Network value |> ignore
        //         | err -> () //failwith $"Error - {err}"
        //     | "RS" ->
        //         let script =
        //             match event.Ligature with
        //             | Some(script) -> script
        //             | _ -> failwith "Error"

        //         match run script Map.empty List.empty with
        //         | Ok([ WanderValue.Network(value) ]) -> store.remove event.Network value |> ignore
        //         | _ -> failwith "Error"
        //     | _ -> failwith "Error")

    member this.AddNetwork (networkName: string) =
        conn
        |> Db.newCommand "select count(*) as num from networks where name = @name"
        |> Db.setParams ["name", SqlType.String networkName]
        |> Db.query (fun rd ->
            let count = rd.ReadInt64 "num"
            if count = 0 then
                conn
                |> Db.newCommand "insert into networks(name) values(@name)"
                |> Db.setParams [ "name", SqlType.String networkName ]
                |> Db.exec)

    member this.RemoveNetwork networkName =
        conn
        |> Db.newCommand "select rowid from networks where name = @name"
        |> Db.setParams ["name", SqlType.String networkName]
        |> Db.query (fun rd ->
            let id = rd.ReadInt64 "rowid"
            conn
                |> Db.newCommand "delete from networks where rowid = @id"
                |> Db.setParams [ "id", SqlType.Int64 id ]
                |> Db.exec)

    member this.networks(): Quote = 
        conn
        |> Db.newCommand "select * from networks"
        |> Db.query (fun rd ->
            rd.ReadString "name")
        |> List.map (fun name -> Any.Element (Element name))

    member this.fetchOrCreateElement (Element(element)): int64 =
        let res = 
            conn
            |> Db.newCommand "select rowid from elements where name = @name"
            |> Db.setParams ["name", SqlType.String element]
            |> Db.query (fun rd -> rd.ReadInt64 "rowid")
                // if count = 0 then
                //     conn
                //     |> Db.newCommand "insert into networks(name) values(@name)"
                //     |> Db.setParams [ "name", SqlType.String networkName ]
                //     |> Db.exec)
        match res with
        | [] -> failwith "TODO"
        | [ id ] -> id
        | _ -> failwith "TODO"

    member this.add networkName network =
        conn
        |> Db.newCommand "select rowid from networks where name = @name"
        |> Db.setParams ["name", SqlType.String networkName]
        |> Db.query (fun rd ->
            let id = rd.ReadInt64 "rowid"
            Set.iter (fun (e, a, v) ->
                let eid = this.fetchOrCreateElement e
                let aid = this.fetchOrCreateElement a
                let vid = this.fetchOrCreateElement v
                conn
                |> Db.newCommand "select count(*) as num from elements where 
                    network = @network
                    entity = @entity
                    attribute = @attribute
                    value = @value"
                |> Db.setParams [
                    "network", SqlType.Int64 id
                    "entity", SqlType.Int64 eid
                    "attribute", SqlType.Int64 aid
                    "value", SqlType.Int64 vid]
                |> Db.query (fun rd ->
                    let count = rd.ReadInt64 "num"
                    if count = 0 then
                        failwith "TODO"
                    else
                        failwith "TODO")
                // conn
                //     |> Db.newCommand "insert from networks where rowid = @id"
                //     |> Db.setParams [ "id", SqlType.Int64 id ]
                //     |> Db.exec
                ) network
            failwith "TODO")

        //make sure network exists and get network id
        //for each triple 
        failwith "TODO"
        // store.add networkName network
        // let ligature = writeLigature network

        // conn
        // |> Db.newCommand "insert into events(type, network, ligature) values('AS', @network, @ligature)"
        // |> Db.setParams [ "network", SqlType.String networkName; "ligature", SqlType.String ligature ]
        // |> Db.exec

        // Ok(())

    member this.remove networkName network =
        failwith "TODO"
        // store.remove networkName network
        // let ligature = writeLigature network

        // conn
        // |> Db.newCommand "insert into events(type, network, ligature) values('RS', @network, @ligature)"
        // |> Db.setParams [ "network", SqlType.String networkName ]
        // |> Db.setParams [ "ligature", SqlType.String ligature ]
        // |> Db.exec

        // Ok(())

    member this.read name =
        failwith "TODO"

let createStoreActions (store: LigatureSqlite) (baseActions: Actions) : Actions =
    baseActions.Add(
        Element "merge",
        Action.Stack(
            { doc = "Reads a Network and Name off the Stack and merges that Network into the target Network."
              examples = [ "{a b c} \"test\" merge" ]
              pre = "Network"
              post = "" },
            fun stack ->
                match stack with
                | Any.Element networkName :: Any.Network network :: tail ->
                    failwith "TODO"
                    //store.Merge networkName network
                    //Ok tail
                | _ -> failwith "TODO"
        )
    )
    |> Map.add
        (Element "networks")
        (Action.Stack(
            { doc =
                "Returns a quote of all the existing Networks."
              examples = ["networks"]
              pre = ""
              post = "Quote" },
            fun stack -> 
                Ok (Any.Quote (store.networks()) :: stack)
        ))
    |> Map.add
        (Element "add-network")
        (Action.Stack(
            { doc =
                "Reads a Network name and creates a Network in the Store."
              examples = ["\"test\" add-network"]
              pre = "Literal"
              post = "" },
            fun stack -> 
                match stack with
                | Any.Literal name :: tail -> 
                    store.AddNetwork(name)
                    Ok(tail)
                | _ -> failwith "TODO")
        )
    |> Map.add
        (Element "remove-network")
        (Action.Stack(
            { doc =
                "Reads a Network name and removes that Network from the Store."
              examples = ["\"test\" remove-network"]
              pre = "Literal"
              post = "" },
            fun stack -> 
                match stack with
                | Any.Literal name :: tail -> 
                    store.RemoveNetwork(name)
                    Ok(tail)
                | _ -> failwith "TODO")
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


// module Ligature.DuckDB

// open Model
// open Wander.Main
// open Wander.Library
// open Wander.Model
// open Ligature.Model
// open Wander.Model
// open DuckDB.NET.Data

// type LigatureDuckDB(conn: DuckDBConnection) =

//     interface System.IDisposable with
//         member _.Dispose() : unit = conn.Dispose()

//     interface LigatureEngine with
//         member _.AddNetwork networkName =
//             store.AddNetwork networkName
//             addEvent $"add-network {networkName}"
//             Ok()

//         member _.SetNetwork networkName network =
//             store.SetNetwork networkName network
//             addEvent $"set-network {networkName} {printNetwork network}"
//             Ok()

//         member _.RemoveNetwork networkName =
//             store.RemoveNetwork networkName
//             addEvent $"remove-network {networkName}"
//             Ok()

//         member _.Networks() = store.Networks()

//         member _.AddEntries networkName network =
//             store.AddEntries networkName network
//             addEvent $"add-entries {networkName} {printNetwork network}"
//             Ok()

//         member _.RemoveEntries networkName network =
//             store.RemoveEntries networkName network
//             addEvent $"remove-entries {networkName} {printNetwork network}"
//             Ok()

//         member _.ReadNetwork(networkName: NetworkName) : Result<Set<Entry>, LigatureError> =
//             store.ReadNetwork networkName

//         member _.FilterEntries (networkName: NetworkName) (query: Network) : Result<Set<Entry>, LigatureError> =
//             store.FilterEntries networkName query

// let openDefault () : LigatureEngine =
//     let home =
//         System.Environment.GetEnvironmentVariable("LIGATURE_HOME")
//         + System.IO.Path.DirectorySeparatorChar.ToString()
//         + "ligature.duckdb"

//     let conn = new DuckDBConnection($"DataSource={home}")
//     conn.Open()
//     let command = conn.CreateCommand()

//     command.CommandText <-
//         "CREATE SEQUENCE IF NOT EXISTS  seq;
//         CREATE TABLE IF NOT EXISTS event (
//                 id              UBIGINT PRIMARY KEY DEFAULT NEXTVAL('seq'),
//                 script          TEXT,
//                 );"

//     command.ExecuteNonQuery()
//     LigatureDuckDB(conn)

// let inMemoryDuckDBStore () : LigatureEngine =
//     let conn = new DuckDBConnection("DataSource=:memory:")
//     conn.Open()
//     let command = conn.CreateCommand()

//     command.CommandText <-
//         "CREATE SEQUENCE seq;
//         CREATE TABLE event (
//                 id              UBIGINT PRIMARY KEY DEFAULT NEXTVAL('seq'),
//                 script          TEXT,
//                 );"

//     command.ExecuteNonQuery()
//     LigatureDuckDB(conn)

// type IStore =
//     abstract Networks: unit -> string seq
//     abstract AddNetwork: string -> unit
//     abstract RemoveNetwork: string -> unit
//     abstract Merge: string -> Network -> unit
//     abstract Remove: string -> Network -> unit
//     abstract Read: string -> Network

// type InMemoryStore(store: Ref<Map<string, Network>>) =

//     interface IStore with
//         member _.AddNetwork(name: string) : unit =
//             match store.contents.TryFind name with
//             | Some _ -> ()
//             | None -> store.Value <- Map.add name Set.empty store.contents

//         member this.Merge (name: string) (network: Network) : unit =
//             match store.contents.TryFind name with
//             | Some currentNetwork -> store.Value <- Map.add name (Set.union currentNetwork network) store.contents
//             | _ -> failwith "Not Implemented"

//         member this.Networks() : string seq = store.contents.Keys

//         member this.Read(name: string) : Network =
//             match store.contents.TryFind name with
//             | Some currentNetwork -> currentNetwork
//             | _ -> failwith "Not Implemented"

//         member this.Remove (name: string) (network: Network) : unit =
//             match store.contents.TryFind name with
//             | Some currentNetwork -> store.Value <- Map.add name (Set.difference currentNetwork network) store.contents
//             | _ -> failwith "Not Implemented"

//         member this.RemoveNetwork(name: string) : unit =
//             store.Value <- Map.remove name store.contents

// let createInMemoryStore () = InMemoryStore(ref Map.empty)

// let createStore (location: string): IStore =
//     let env = LightningEnvironment(location)
//     env.Open()
