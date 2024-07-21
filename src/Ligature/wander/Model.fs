// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Model

open Ligature.Main
open System
open System.Text.RegularExpressions
//open Ligature.LigatureStore.InMemoryStore

[<RequireQualifiedAccess>]
type Expression =
    | Colon
    | Int of bigint
    | Bytes of byte array
    | String of string
    | Identifier of Identifier
    | Slot of Slot
    | Definition of name: string * value: Expression
    | Word of string
    | Quote of Expression list
    | AssocArray of list<string * Expression>
    | Network of NetworkRoot list

and EntityDescription = Identifier * Expression list

and NetworkRoot = Identifier * EntityDescription list

[<RequireQualifiedAccess>]
type WanderValue =
    | Int of bigint
    | String of string
    | Bool of bool
    | Identifier of Identifier
    | Slot of Slot
    | Triple of Ligature.Main.Triple
    | Quote of WanderValue list
    | Network of Network
    | AssocArray of Map<string, WanderValue>
    | Bytes of byte array
    | Nothing
    | Word of string

let toWanderValue (value: Value) : WanderValue =
    match value with
    | Value.Identifier id -> WanderValue.Identifier id
    | Value.Bytes bytes -> WanderValue.Bytes bytes
    | Value.Int i -> WanderValue.Int i
    | Value.String s -> WanderValue.String s
    | Value.Slot s -> WanderValue.Slot s

type Parameter = { name: string; tag: string }

//TODO try to remove this
let rec wanderEquals (left: WanderValue) (right: WanderValue) : bool =
    if
        (left = WanderValue.Quote(List.empty) || left = WanderValue.AssocArray(Map.empty))
        && (right = WanderValue.Quote(List.empty)
            || right = WanderValue.AssocArray(Map.empty))
    then
        true
    else
        match left, right with
        | WanderValue.Quote(left), WanderValue.Quote(right) ->
            if left.Length = right.Length then
                List.forall2 (fun left right -> wanderEquals left right) left right
            else
                false
        | _ -> left = right

let encodeString string =
#if !FABLE_COMPILER
    System.Web.HttpUtility.JavaScriptStringEncode(string, true)
#else
    failwith "TODO"
#endif

let rec prettyPrint (value: WanderValue) : string =
    match value with
    | WanderValue.Int i -> sprintf "%A" i
    | WanderValue.String s -> encodeString s
    | WanderValue.Bool b -> sprintf "%b" b
    | WanderValue.Identifier i -> $"`{(readIdentifier i)}`"
    | WanderValue.Slot(Slot(Some(name))) -> $"${(name)}"
    | WanderValue.Slot(Slot(None)) -> "$"
    | WanderValue.Quote(values) -> $"[{printValues values}]"
    | WanderValue.Triple(triple) -> printTriple triple
    | WanderValue.AssocArray(values) -> printAssocArray values
    | WanderValue.Bytes(bytes) -> printBytes bytes
    | WanderValue.Network(values) -> printNetwork values
    | WanderValue.Nothing -> "Nothing"
    | WanderValue.Word(name) -> name

and printNetwork (network: Network) : string =
    (Seq.fold (fun state triple -> state + " " + (printTriple triple) + ", ") "{" (network.Write()))
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

and printTriple triple =
    $"`{(readPatternIdentifier triple.Entity)}` `{(readPatternIdentifier triple.Attribute)}` {(printLigatureValue triple.Value)}"

and printPatternIdentifier (patternIdentifier: PatternIdentifier) =
    match patternIdentifier with
    | PatternIdentifier.Id(identifier) -> $"`{readIdentifier identifier}`"
    | PatternIdentifier.Sl(Slot(Some(name))) -> $"${name}"
    | PatternIdentifier.Sl(Slot(None)) -> "$"

and printValue (value: Value) =
    match value with
    | Value.Identifier(value) -> $"`{(readIdentifier value)}`"
    | Value.Int(value) -> value.ToString()
    | Value.String(value) -> $"\"{value}\"" //TODO escape properly
    | Value.Bytes(bytes) -> printBytes bytes
    | Value.Slot(Slot(Some(name))) -> $"${name}"
    | Value.Slot(Slot(None)) -> "$"

and printPattern (pattern: Triple) =
    $"{(printPatternIdentifier pattern.Entity)} {(printPatternIdentifier pattern.Attribute)} {(printValue pattern.Value)}"

and printLigatureValue value =
    match value with
    | Value.Identifier(value) -> $"`{(readIdentifier value)}`"
    | Value.Int(value) -> value.ToString()
    | Value.String(value) -> $"\"{value}\"" //TODO escape properly
    | Value.Bytes(bytes) -> printBytes bytes
    | Value.Slot(_) -> failwith "TODO"

and printValues values =
    Seq.fold (fun x y -> x + (prettyPrint y) + " ") "" values

type Scope = Map<string, WanderValue>

type WanderType =
    | String
    | Int
    | Bytes
    | Identifier
    | Slot
    | Network
    | AssocArray
    | Value
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
      Eval: WanderValue list -> Bindings -> Result<WanderValue, LigatureError> }

and Bindings =
    { Functions: HostFunction list
      Current: Scope
      Stack: Scope list }

let newBindings () =
    { Functions = []
      Current = Map.empty
      Stack = [] }

let bind (name: string) (value: WanderValue) (bindings: Bindings) : Bindings =
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
