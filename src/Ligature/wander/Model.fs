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
    | Colon
    | Int of bigint
    | Bytes of byte array
    | String of string
    | Slot of Slot
    | Definition of name: string * value: Expression
    | Word of string
    | Quote of Expression list
    | AssocArray of list<string * Expression>
    | Network of Network

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

let rec prettyPrint (value: Value) : string =
    match value with
    | Value.Int i -> sprintf "%A" i
    | Value.String s -> encodeString s
    | Value.Slot(Slot(Some(name))) -> $"${(name)}"
    | Value.Slot(Slot(None)) -> "$"
    | Value.Quote(values) -> $"[{printQuote values}]"
// | WanderValue.AssocArray(values) -> printAssocArray values
// | WanderValue.Bytes(bytes) -> printBytes bytes
// | WanderValue.Network(values) -> printNetwork values
// | WanderValue.Nothing -> "Nothing"
// | WanderValue.Word(name) -> name

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

and printTriple ((entity, attribute, value): Triple) : string =
    $"{(readPatternWord entity)} {(readPatternWord attribute)} {(printLigatureValue value)}"

and printPatternWord (patternWord: PatternWord) =
    match patternWord with
    | PatternWord.Word(Word(word)) -> word
    | PatternWord.Slot(Slot(Some(name))) -> $"${name}"
    | PatternWord.Slot(Slot(None)) -> "$"

and printValue (value: Value) =
    match value with
    | Value.Word(Word(value)) -> value
    | Value.Int(value) -> value.ToString()
    | Value.String(value) -> $"\"{value}\"" //TODO escape properly
    //    | Value.Bytes(bytes) -> printBytes bytes
    | Value.Slot(Slot(Some(name))) -> $"${name}"
    | Value.Slot(Slot(None)) -> "$"

and printPattern ((entity, attribute, value): Triple) =
    $"{(printPatternWord entity)} {(printPatternWord attribute)} {(printValue value)}"

and printLigatureValue value =
    match value with
    | Value.Word(Word(word)) -> word
    | Value.Int(value) -> value.ToString()
    | Value.String(value) -> $"\"{value}\"" //TODO escape properly
    //    | Value.Bytes(bytes) -> printBytes bytes
    | Value.Slot(_) -> failwith "TODO"

and printQuote quote = failwith "TODO"
//    Seq.fold (fun x y -> x + (prettyPrint y) + " ") "" values

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
      Stack: Scope list
      Network: Network }

let newBindings (network: Network) =
    { Functions = []
      Current = Map.empty
      Stack = []
      Network = network }

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
