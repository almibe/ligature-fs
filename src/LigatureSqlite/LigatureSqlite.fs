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
            |> Db.newCommand "select rowid from terms where term = @term"
            |> Db.setParams ["term", SqlType.String element]
            |> Db.query (fun rd -> rd.ReadInt64 "rowid")
        match res with
        | [] -> 
            conn
            |> Db.newCommand "insert into terms(term) values(@element)"
            |> Db.setParams [ "element", SqlType.String element ]
            |> Db.exec
            this.fetchOrCreateElement(Element element)
        | [ id ] -> id
        | _ -> failwith "TODO"

    member this.readElementId (id: int64): Element =
        let res = 
            conn
            |> Db.newCommand "select * from terms where rowid = @id"
            |> Db.setParams ["id", SqlType.Int64 id]
            |> Db.query (fun rd -> rd.ReadString "term")
        match res with
        | [] -> failwith "TODO"
        | [ id ] -> Element id
        | _ -> failwith "TODO"

    member this.add (networkName: string) (network: Network): Unit =
        conn
        |> Db.newCommand "select rowid from networks where name = @name"
        |> Db.setParams ["name", SqlType.String networkName]
        |> Db.query (fun rd ->
            let id = rd.ReadInt64 "rowid"
            Set.iter (fun triple ->
                match triple with
                | (ElementPattern.Element e, ElementPattern.Element a, ElementPattern.Element v) ->
                    let eid = this.fetchOrCreateElement e
                    let aid = this.fetchOrCreateElement a
                    let vid = this.fetchOrCreateElement v

                    conn
                    |> Db.newCommand "select count(*) as num from triples where 
                        network = @network
                        and entity = @entity
                        and attribute = @attribute
                        and value = @value"
                    |> Db.setParams [
                        "network", SqlType.Int64 id
                        "entity", SqlType.Int64 eid
                        "attribute", SqlType.Int64 aid
                        "value", SqlType.Int64 vid]
                    |> Db.query (fun rd -> 
                        let count = rd.ReadInt64 "num"
                        if count = 0 then
                            conn
                                |> Db.newCommand "insert into triples(network, entity, attribute, value)
                                    values(@network, @entity, @attribute, @value)"
                                |> Db.setParams [ 
                                    "network", SqlType.Int64 id 
                                    "entity", SqlType.Int64 eid
                                    "attribute", SqlType.Int64 aid
                                    "value", SqlType.Int64 vid]
                                |> Db.exec)
                    ()
                | _ -> failwith "TODO"
            ) network)
        |> ignore

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

    member this.read (name: string): Network =
        let res =
            conn
            |> Db.newCommand "select rowid from networks where name = @name"
            |> Db.setParams ["name", SqlType.String name]
            |> Db.query (fun rd -> rd.ReadInt64 "rowid")
        match res with
        | [] -> failwith "Network not found"
        | [ id ] ->
            conn
            |> Db.newCommand "select * from triples where network = @network"
            |> Db.setParams ["network", SqlType.Int64 id]
            |> Db.query (fun rd ->
                //read all triples
                //read all elements
                let eid = rd.ReadInt64 "entity"
                let aid = rd.ReadInt64 "attribute"
                let vid = rd.ReadInt64 "value"
                let entity = this.readElementId(eid)
                let attribute = this.readElementId(aid)
                let value = this.readElementId(vid)
                
                failwith "TODO")
            failwith "TODO"
        | _ -> failwith "expected state"

let createStoreActions (store: LigatureSqlite) (baseActions: Actions) : Actions =
    baseActions.Add(
        Element "merge",
        Action.Stack(
            { doc = "Reads a Network and Name off the Stack and merges that Network into the target Network."
              examples = [ "{a b c} \"test\" merge" ]
              pre = "Literal Network"
              post = "" },
            fun stack ->
                match stack with
                | Any.Literal networkName :: Any.Network network :: tail ->
                    store.add networkName network
                    Ok tail
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
              pre = "Literal"
              post = "Network" },
            fun stack ->
                match stack with
                | Any.Literal networkName :: tail -> 
                    Ok(Any.Network(store.read networkName) :: tail)
                | _ -> failwith "TODO"
        ))

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
