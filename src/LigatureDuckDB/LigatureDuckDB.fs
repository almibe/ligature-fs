// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.DuckDB

open Ligature.Main
open DuckDB.NET.Data

type LigatureDuckDB(conn: DuckDBConnection) =
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

        member _.Read(networkName: NetworkName) : Result<Set<Entry>, LigatureError> = failwith "TODO"

        member _.Set name network : Result<unit, LigatureError> = failwith "TODO"

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
