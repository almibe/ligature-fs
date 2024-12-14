// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.DuckDB

open Ligature.Model
open Wander.Model
open Ligature.InMemoryEngine
open DuckDB.NET.Data

type LigatureDuckDB(conn: DuckDBConnection) =
    let store = newInMemoryEngine ()

    let addEvent (eventStr: string) =
        use command = conn.CreateCommand()
        command.CommandText <- "INSERT into event ($script)"
        command.Parameters.Add(new DuckDBParameter("script", eventStr))
        command.ExecuteNonQuery()

    do
        use command = conn.CreateCommand()
        command.CommandText <- "SELECT script FROM event order by id;"

        use reader = command.ExecuteReader()

        while reader.Read() do
            //run each command against the store
            printfn $"Script: {reader.GetString(0)}"
            failwith "TODO"

    interface System.IDisposable with
        member _.Dispose() : unit = conn.Dispose()

    interface LigatureEngine with
        member _.AddNetwork networkName =
            store.AddNetwork networkName
            addEvent $"add-network {networkName}"
            Ok()

        member _.SetNetwork networkName network =
            store.SetNetwork networkName network
            addEvent $"set-network {networkName} {printNetwork network}"
            Ok()

        member _.RemoveNetwork networkName =
            store.RemoveNetwork networkName
            addEvent $"remove-network {networkName}"
            Ok()

        member _.Networks() = store.Networks()

        member _.AddEntries networkName network =
            store.AddEntries networkName network
            addEvent $"add-entries {networkName} {printNetwork network}"
            Ok()

        member _.RemoveEntries networkName network =
            store.RemoveEntries networkName network
            addEvent $"remove-entries {networkName} {printNetwork network}"
            Ok()

        member _.ReadNetwork(networkName: NetworkName) : Result<Set<Entry>, LigatureError> =
            store.ReadNetwork networkName

        member _.FilterEntries (networkName: NetworkName) (query: Network) : Result<Set<Entry>, LigatureError> =
            store.FilterEntries networkName query

let openDefault () : LigatureEngine =
    let home =
        System.Environment.GetEnvironmentVariable("LIGATURE_HOME")
        + System.IO.Path.DirectorySeparatorChar.ToString()
        + "ligature.duckdb"

    let conn = new DuckDBConnection($"DataSource={home}")
    conn.Open()
    let command = conn.CreateCommand()

    command.CommandText <-
        "CREATE SEQUENCE IF NOT EXISTS  seq;
        CREATE TABLE IF NOT EXISTS event (
                id              UBIGINT PRIMARY KEY DEFAULT NEXTVAL('seq'),
                script          TEXT,
                );"

    command.ExecuteNonQuery()
    LigatureDuckDB(conn)

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
