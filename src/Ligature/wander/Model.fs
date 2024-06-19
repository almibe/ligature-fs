// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Model

open Ligature.Main
open System
open System.Text.RegularExpressions
open Ligature.LigatureStore.InMemoryStore

[<RequireQualifiedAccess>]
type Expression =
    | Colon
    | Int of bigint
    | Bytes of byte array
    | String of string
    | Bool of bool
    | Identifier of Identifier
    | Slot of Slot
    | Let of name: string * value: Expression
    | Name of string
    | Grouping of Expression list
    | Array of Expression list
    | Application of Expression list
    | FunctionCall of name: string * arguments: Expression list
    | Record of list<string * Expression>
    | Pattern of DatasetPatternRoot list

and EntityDescription = Expression * Expression list

and DatasetPatternRoot = Expression * EntityDescription list

[<RequireQualifiedAccess>]
type WanderValue =
    | Int of bigint
    | String of string
    | Bool of bool
    | Identifier of Identifier
    | Slot of Slot
    | Statement of Ligature.Main.Statement
    | Array of WanderValue array
    | Network of Network
    | Namespace of Map<string, WanderValue>
    | Bytes of byte array
    | Nothing

type Parameter = { name: string; tag: string }

//TODO try to remove this
let rec wanderEquals (left: WanderValue) (right: WanderValue) : bool =
    if
        (left = WanderValue.Network(emptyNetwork)
         || left = WanderValue.Namespace(Map.empty))
        && (right = WanderValue.Network(emptyNetwork)
            || right = WanderValue.Namespace(Map.empty))
    then
        true
    else
        match left, right with
        | WanderValue.Array(left), WanderValue.Array(right) ->
            if left.Length = right.Length then
                Array.forall2 (fun left right -> wanderEquals left right) left right
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
    | WanderValue.Slot s -> $"${(s.Name)}"
    | WanderValue.Array(values) -> $"[{printValues values}]"
    | WanderValue.Statement(statement) -> printStatement statement
    | WanderValue.Namespace(values) -> printRecord values
    | WanderValue.Bytes(bytes) -> printBytes bytes
    | WanderValue.Network(values) -> printNetwork values
    | WanderValue.Nothing -> "Nothing"

and printNetwork (network: Network) : string =
    (Seq.fold (fun state statement -> state + " " + (printStatement statement) + ", ") "{" (network))
    + "}"

and printBytes bytes =
    bytes
    |> Array.map (fun value -> System.String.Format("{0:X2}", value))
    |> Array.insertAt 0 "0x"
    |> String.concat String.Empty

and printRecord values =
    "{ "
    + Map.fold (fun state key value -> state + $"{key} = {prettyPrint value}, ") "" values
    + "}"

and printStatement statement =
    $"`{(readPatternIdentifier statement.Entity)}` `{(readPatternIdentifier statement.Attribute)}` {(printLigatureValue statement.Value)}"

and printPatternIdentifier (patternIdentifier: PatternIdentifier) =
    match patternIdentifier with
    | PatternIdentifier.Id(identifier) -> $"`{readIdentifier identifier}`"
    | PatternIdentifier.Sl(slot) -> $"${(slot.Name)}"

and printValue (value: Value) =
    match value with
    | Value.Identifier(value) -> $"`{(readIdentifier value)}`"
    | Value.Int(value) -> value.ToString()
    | Value.String(value) -> $"\"{value}\"" //TODO escape properly
    | Value.Bytes(bytes) -> printBytes bytes
    | Value.Slot(slot) -> $"${(slot.Name)}"

and printPattern (pattern: Statement) =
    $"{(printPatternIdentifier pattern.Entity)} {(printPatternIdentifier pattern.Attribute)} {(printValue pattern.Value)}"

and printLigatureValue value =
    match value with
    | Value.Identifier(value) -> $"`{(readIdentifier value)}`"
    | Value.Int(value) -> value.ToString()
    | Value.String(value) -> $"\"{value}\"" //TODO escape properly
    | Value.Bytes(bytes) -> printBytes bytes
    | Value.Slot(_) -> failwith "TODO"

and printValues values =
    Seq.fold (fun x y -> x + (prettyPrint y) + ", ") "" values

type Scope = Map<string, WanderValue>

type HostFunction(eval: WanderValue list -> Bindings -> Result<WanderValue, LigatureError>) =
    member _.Run args bindings = eval args bindings

and Bindings =
    { functions: Map<string, HostFunction>
      current: Scope
      stack: Scope list }

let newBindings () =
    { functions = Map.empty
      current = Map.empty
      stack = [] }

let bind (name: string) (value: WanderValue) (bindings: Bindings) : Bindings =
    let current' = Map.add name value bindings.current
    { bindings with current = current' }

let bindFunction (name: string) (fn: HostFunction) (bindings: Bindings) : Bindings =
    let functions' = Map.add name fn bindings.functions
    { bindings with functions = functions' }

let bindFunctions (functions: Map<string, HostFunction>) (bindings: Bindings) : Bindings =
    Map.fold (fun state key value -> bindFunction key value state) bindings functions

let addScope bindings =
    let current = Map []
    let stack = List.append [ bindings.current ] bindings.stack

    { bindings with
        current = current
        stack = stack }

let removeScope bindings =
    let current = List.head bindings.stack
    let stack = List.tail bindings.stack

    { bindings with
        current = current
        stack = stack }

let rec read name bindings =
    if Map.containsKey name bindings.current then
        Some(Map.find name bindings.current)
    else if List.isEmpty bindings.stack then
        None
    else
        read name (removeScope bindings)

let readFunction name bindings = Map.tryFind name bindings.functions
