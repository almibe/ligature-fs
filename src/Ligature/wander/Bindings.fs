// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Bindings

open Ligature.Wander
open Lib.Bool
open Lib.Bytes
open Lib.Identifier
open Lib.Ligature
open Lib.Int
open Lib.Wander
open Lib.DateTime
open Lib.Store
open Ligature.LigatureStore
open Ligature.Wander.Model
open Ligature.Main

let bindCoreHostFunctions bindings store =
    bindings
    |> bindFunctions Lib.Array.arrayLib
    |> bindFunctions Lib.Assert.assertLib
    |> bindFunctions boolLib
    |> bindFunctions bytesLib
    |> bindFunctions dateTimeLib
    |> bindFunctions Lib.Ligature.ligatureLib
    |> bindFunctions identifierLib
    |> bindFunctions (storeLib store)
    |> bindFunctions intLib
    |> bindFunctions Lib.Statement.statementLib
    |> bindFunctions Lib.String.stringLib
#if !FABLE_COMPILER
    |> bindFunctions Lib.Ulid.ulidLib
#endif
    |> bindFunctions wanderLib

let identifierUnsafe id =
    match identifier id with
    | Ok id -> id
    | _ -> failwith "error"

let wanderTypeToIdentifier (wt: WanderType): Identifier =
    match wt with
    | WanderType.String -> identifierUnsafe "String"
    | WanderType.Int -> identifierUnsafe "Int"
    | WanderType.Bytes -> identifierUnsafe "Bytes"
    | WanderType.Identifier -> identifierUnsafe "Identifier"
    | WanderType.Slot -> identifierUnsafe "Slot"
    | WanderType.Network -> identifierUnsafe "Network"
    | WanderType.Record -> identifierUnsafe "Record"
    | WanderType.Value -> identifierUnsafe "Value"
    | WanderType.Array -> identifierUnsafe "Array"
    | WanderType.Anything -> identifierUnsafe "Anything"
    | WanderType.Nothing -> identifierUnsafe "Nothing"

let createLib (bindings: Bindings): Set<Statement> =
    bindings.Functions
    |> List.map (fun fn -> [
            { 
                Entity = PatternIdentifier.Id (identifierUnsafe fn.Name) 
                Attribute = PatternIdentifier.Id (identifierUnsafe "isa")
                Value = Value.Identifier (identifierUnsafe "HostFunction") 
            }
            {
                Entity = PatternIdentifier.Id (identifierUnsafe fn.Name)
                Attribute = PatternIdentifier.Id (identifierUnsafe "returns")
                Value = Value.Identifier (wanderTypeToIdentifier fn.Returns)
            }
        ])
    |> List.concat
    |> Set.ofList

let bindLib (bindings: Bindings) =
    bindings |> bind "lib" (WanderValue.Network (createLib bindings))

let coreBindings (store: LigatureStore) =
    store |> bindCoreHostFunctions (newBindings ()) |> bindLib
