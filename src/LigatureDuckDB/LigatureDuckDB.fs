// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.DuckDB

open Ligature.Main
open Ligature.InMemoryEngine
open DuckDB.NET.Data

type LigatureDuckDB(conn: DuckDBConnection) =
    let store = newInMemoryEngine ()

    interface System.IDisposable with
        member _.Dispose() : unit = conn.Dispose()

    interface LigatureEngine with
        member _.AddNetwork networkName =
            store.AddNetwork networkName
            failwith "TODO"

        member _.SetNetwork networkName network =
            store.SetNetwork networkName network
            failwith "TODO"

        member _.RemoveNetwork networkName =
            store.RemoveNetwork networkName
            failwith "TODO"

        member _.Networks() = store.Networks()

        member _.AddEntries name network =
            store.AddEntries name network
            failwith "TODO"

        member _.RemoveEntries name network =
            store.RemoveEntries name network
            failwith "TODO"

        member _.ReadNetwork(networkName: NetworkName) : Result<Set<Entry>, LigatureError> =
            store.ReadNetwork networkName

        member _.FilterEntries (networkName: NetworkName) (query: Network) : Result<Set<Entry>, LigatureError> =
            store.FilterEntries networkName query

let inMemoryDuckDBStore () : LigatureEngine =
    let conn = new DuckDBConnection("DataSource=:memory:")
    conn.Open()
    let command = conn.CreateCommand()

    command.CommandText <-
        "CREATE SEQUENCE seq;
              CREATE TABLE event (
                      id              UBIGINT PRIMARY KEY DEFAULT NEXTVAL('seq'),
                      script          TEXT,
                      );"

    command.ExecuteNonQuery()
    LigatureDuckDB(conn)
