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
let rec wanderEquals (left: LigatureValue) (right: LigatureValue) : bool =
    failwith "TODO"
    // if
    //     (left = LigatureValue.Quote(List.empty) || left = LigatureValue.Quote(Map.empty))
    //     && (right = LigatureValue.Quote(List.empty)
    //         || right = LigatureValue.AssocArray(Map.empty))
    // then
    //     true
    // else
    //     match left, right with
    //     | LigatureValue.Quote(left), LigatureValue.Quote(right) ->
    //         if left.Length = right.Length then
    //             List.forall2 (fun left right -> wanderEquals left right) left right
    //         else
    //             false
    //     | _ -> left = right

let encodeString string =
#if !FABLE_COMPILER
    System.Web.HttpUtility.JavaScriptStringEncode(string, true)
#else
    failwith "TODO"
#endif

let rec prettyPrint (value: LigatureValue) : string =
    match value with
    | LigatureValue.Int i -> sprintf "%A" i
    | LigatureValue.String s -> encodeString s
    | LigatureValue.Slot(Slot(Some(name))) -> $"${(name)}"
    | LigatureValue.Slot(Slot(None)) -> "$"
    | LigatureValue.Quote(values) -> $"[{printQuote values}]"
// | LigatureValue.AssocArray(values) -> printAssocArray values
// | LigatureValue.Bytes(bytes) -> printBytes bytes
// | LigatureValue.Network(values) -> printNetwork values
// | LigatureValue.Nothing -> "Nothing"
// | LigatureValue.Word(name) -> name

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
    $"{(readPatternWord entity)} {(readPatternWord attribute)} {(printLigatureValue value)}"

and printPatternWord (patternWord: PatternWord) =
    match patternWord with
    | PatternWord.Word(Word(word)) -> word
    | PatternWord.Slot(Slot(Some(name))) -> $"${name}"
    | PatternWord.Slot(Slot(None)) -> "$"

and printLigatureValue (value: LigatureValue) =
    match value with
    | LigatureValue.Word(Word(value)) -> value
    | LigatureValue.Int(value) -> value.ToString()
    | LigatureValue.String(value) -> $"\"{value}\"" //TODO escape properly
    //    | LigatureValue.Bytes(bytes) -> printBytes bytes
    | LigatureValue.Slot(Slot(Some(name))) -> $"${name}"
    | LigatureValue.Slot(Slot(None)) -> "$"

and printPattern ((entity, attribute, value): Statement) =
    $"{(printPatternWord entity)} {(printPatternWord attribute)} {(printLigatureValue value)}"


// and printLigatureValue value =
//     match value with
//     | LigatureValue.Word(Word(word)) -> word
//     | LigatureValue.Int(value) -> value.ToString()
//     | LigatureValue.String(value) -> $"\"{value}\"" //TODO escape properly
//     | LigatureValue.Quote(quote) -> printQuote quote
//     //    | LigatureValue.Bytes(bytes) -> printBytes bytes
//     | LigatureValue.Slot(_) -> failwith "TODO"

and printQuote quote = "Quote"
//(List.fold(fun state value -> state + " " + (printLigatureValue value)) "[" quote.parameters) + "]"
//    Seq.fold (fun x y -> x + (prettyPrint y) + " ") "" values

type Scope = Map<string, LigatureValue>

type WanderType =
    | String
    | Int
    | Bytes
    | Identifier
    | Slot
    | Network
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
