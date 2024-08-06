// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Model

open Ligature.Main
open System
open System.Text.RegularExpressions
open Ligature
//open Ligature.LigatureStore.InMemoryStore

[<RequireQualifiedAccess>]
type Expression =
    | NetworkName of NetworkName
    | Call of Identifier
    | Network of Network

type Parameter = { name: string; tag: string }

let encodeString string =
#if !FABLE_COMPILER
    System.Web.HttpUtility.JavaScriptStringEncode(string, true)
#else
    failwith "TODO"
#endif

let rec prettyPrint (value: LigatureValue) : string =
    match value with
    | LigatureValue.Identifier(Identifier(value)) -> value
    | LigatureValue.Int i -> i.ToString()
    | LigatureValue.String s -> encodeString s
    | LigatureValue.Slot(Slot(Some(name))) -> $"${(name)}"
    | LigatureValue.Slot(Slot(None)) -> "$"
    | LigatureValue.Pipeline(values) -> $"[{printPipeline values}]" //TODO print names better
    | LigatureValue.Bytes(bytes) -> printBytes bytes
    | LigatureValue.Network n -> printNetwork n
    | LigatureValue.NetworkName n -> $"@{n}"
    | LigatureValue.HostCombinator(combinator) -> $"Combinator({combinator.Name})"

and printNetwork (network: Network) : string =
    (Seq.fold (fun state triple -> state + " " + (printStatement triple) + ", ") "{" (network))
    + "}"

and printBytes bytes =
    bytes
    |> Array.map (fun value -> System.String.Format("{0:X2}", value))
    |> Array.insertAt 0 "0x"
    |> String.concat String.Empty

and printAssocArray values =
    "[ "
    + Map.fold (fun state key value -> state + $"{key} = {prettyPrint value}, ") "" values
    + "]"

and printStatement ((entity, attribute, value): Statement) : string =
    $"{(printPatternIdentifier entity)} {(printPatternIdentifier attribute)} {(prettyPrint value)}"

and printPatternIdentifier (patternIdentifier: PatternIdentifier) =
    match patternIdentifier with
    | PatternIdentifier.Identifier(Identifier(identifier)) -> identifier
    | PatternIdentifier.Slot(Slot(Some(name))) -> $"${name}"
    | PatternIdentifier.Slot(Slot(None)) -> "$"

and printPattern ((entity, attribute, value): Statement) =
    $"{(printPatternIdentifier entity)} {(printPatternIdentifier attribute)} {(prettyPrint value)}"

and printPipeline pipeline =
    (List.fold (fun state value -> state + " " + (prettyPrint value)) "" pipeline)

type Scope = Map<string, LigatureValue>

type WanderType =
    | String
    | Int
    | Bytes
    | Identifier
    | Slot
    | Network
    | NetworkName
    | AssocArray
    | LigatureValue
    | Array
    | Anything
    | Nothing
    | Function

type HostFunction =
    { Module: string
      Name: string
      Parameters: (string * WanderType) list
      Returns: WanderType
      Description: string
      Eval: LigatureValue list -> Bindings -> Result<LigatureValue, LigatureError> }

and Bindings =
    { Functions: HostFunction list
      Current: Scope
      Stack: Scope list
      Network: Network }

let newBindings (network: Network) =
    { Functions = []
      Current = Map.empty
      Stack = []
      Network = network }

let bind (name: string) (value: LigatureValue) (bindings: Bindings) : Bindings =
    let current' = Map.add name value bindings.Current
    { bindings with Current = current' }

let bindFunction (fn: HostFunction) (bindings: Bindings) : Bindings =
    let functions' = fn :: bindings.Functions
    { bindings with Functions = functions' }

let bindFunctions (functions: List<HostFunction>) (bindings: Bindings) : Bindings =
    let functions' = functions @ bindings.Functions
    { bindings with Functions = functions' }

let addScope bindings =
    let current = Map []
    let stack = List.append [ bindings.Current ] bindings.Stack

    { bindings with
        Current = current
        Stack = stack }

let removeScope bindings =
    let current = List.head bindings.Stack
    let stack = List.tail bindings.Stack

    { bindings with
        Current = current
        Stack = stack }

let rec read name bindings =
    if Map.containsKey name bindings.Current then
        Some(Map.find name bindings.Current)
    else if List.isEmpty bindings.Stack then
        None
    else
        read name (removeScope bindings)

let readFunction (name: string) (bindings: Bindings) : HostFunction option =
    List.tryFind (fun fn -> fn.Name = name) bindings.Functions
