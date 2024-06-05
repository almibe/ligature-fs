// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Environments

open Ligature.Wander
open Lib.Bool
open Lib.Bytes
open Lib.Identifier
open Lib.Ligature
open Lib.Int
open Lib.Wander
open Lib.DateTime
open Lib.LigatureStore
open LightningDB

let bindCoreHostFunctions bindings =
    bindings
    |> Bindings.bind "Array" Lib.Array.arrayLib
    |> Bindings.bind "Assert" Lib.Assert.assertLib
    |> Bindings.bind "Bool" boolLib
    |> Bindings.bind "Bytes" bytesLib
    |> Bindings.bind "DateTime" dateTimeLib
    |> Bindings.bind "Pattern" Lib.Pattern.patternLib
    |> Bindings.bind "Identifier" identifierLib
    |> Bindings.bind "InMemory" inMemoryLib
    |> Bindings.bind "Int" intLib
    |> Bindings.bind "Statement" Lib.Statement.statementLib
    |> Bindings.bind "String" Lib.String.stringLib
    |> Bindings.bind "Ulid" Lib.Ulid.ulidLib
    |> Bindings.bind "Wander" wanderLib

/// Provides an Environment that provides only the core Host Functions.
let coreEnvironment () =
    bindCoreHostFunctions (Bindings.newBindings ())

let bindWanderLibs bindings =
    match System.Environment.GetEnvironmentVariable "WANDER_LIBS" with
    | null -> failwith "WANDER_LIBS not set"
    | value ->
        let mutable bindings = bindings

        System.IO.Directory.GetFiles(value, "*.wander", new System.IO.EnumerationOptions(RecurseSubdirectories = true))
        |> Array.filter (fun file -> not <| file.Contains(".test."))
        |> Array.iter (fun file ->
            let scriptName = (System.IO.Path.GetFileName file).Split(".") |> Array.head
            let script = System.IO.File.ReadAllText file

            match Ligature.Wander.Main.run script (coreEnvironment ()) with
            | Ok res ->
                match Bindings.read scriptName bindings with
                | None -> bindings <- Bindings.bind scriptName res bindings
                | _ -> failwith $"Error {scriptName} is already bound."
            | Error err -> failwith $"Error evaluating script {scriptName} -- {err.UserMessage}")

        bindings

/// Provices an Environment that provides the core Host Functions and loads namespaces from WANDER_LIBS.
let standardEnvironment () =
    bindCoreHostFunctions (Bindings.newBindings ()) |> bindWanderLibs
