// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.DuckDB

open Ligature.Main
open DuckDB.NET.Data

type LigatureDuckDB(conn: DuckDBConnection) =
    member _.FetchOrCreateNetwork networkName = 
        use query = conn.CreateCommand ()
        query.CommandText <- "select id from network where name = $name"
        query.Parameters.Add(new DuckDBParameter("name", networkName)) |> ignore
        use reader = query.ExecuteReader ()
        while reader.Read() do
            failwith "TODO"
        
        failwith "TODO"

    interface LigatureStore with
        member _.AddNetwork networkName = failwith "TODO"

        member _.RemoveNetwork networkName = failwith "TODO"

        member _.Networks() =
            let query = conn.CreateCommand()
            query.CommandText <- "select * from network;"
            use reader = query.ExecuteReader()

            while reader.Read() do
                failwith "TODO"

            Ok(Set.empty)

        member _.Add name network = failwith "TODO"

        member _.Remove name network = failwith "TODO"

        member _.ClearNetwork networkName : Result<unit, LigatureError> = failwith "TODO"

        member _.Read(networkName: NetworkName) : Result<Set<Entry>, LigatureError> = 
            let query = conn.CreateCommand()
            query.CommandText <- "SELECT e1.element, e2.element, e3.element from entry
                left join network n on n.id = entry.network
                left join element e1 on e1.id = entry.first
                left join element e2 on e2.id = entry.second
                left join element e3 on e3.id = entry.third
                where n.name = $name;"
            query.Parameters.Add(new DuckDBParameter("name", networkName)) |> ignore
            use reader = query.ExecuteReader()

            while reader.Read() do
                failwith "TODO"

            Ok(Set.empty)

        member this.Set name network : Result<unit, LigatureError> = 
            use tx = conn.BeginTransaction ()
            //make sure that network exists
            use query = conn.CreateCommand ()
            this.FetchOrCreateNetwork name
            failwith "TODO"

        member _.Filter (networkName: NetworkName) (query: Network) : Result<Set<Entry>, LigatureError> =
            failwith "TODO"

let inMemoryDuckDBStore () : LigatureStore =
    let conn = new DuckDBConnection("DataSource=:memory:")
    conn.Open()
    let command = conn.CreateCommand()

    command.CommandText <-
        "CREATE SEQUENCE seq;
              CREATE TABLE element (
                      id              UBIGINT PRIMARY KEY DEFAULT NEXTVAL('seq'),
                      element         TEXT NOT NULL UNIQUE,
                      );
              CREATE TABLE network (
                      id              UBIGINT PRIMARY KEY DEFAULT NEXTVAL('seq'),
                      name            TEXT NOT NULL UNIQUE,
                      );
              CREATE TABLE entry (
                      id              UBIGINT PRIMARY KEY DEFAULT NEXTVAL('seq'),
                      network         UBIGINT REFERENCES network(id),
                      first           UBIGINT REFERENCES element(id),
                      second          UBIGINT REFERENCES element(id),
                      third           UBIGINT REFERENCES element(id),
                      );"

    command.ExecuteNonQuery()
    LigatureDuckDB(conn)
