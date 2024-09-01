// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Model

open Ligature.Main
open System
open System.Text.RegularExpressions
open Ligature
//open Ligature.LigatureStore.InMemoryStore

let encodeString string =
#if !FABLE_COMPILER
    System.Web.HttpUtility.JavaScriptStringEncode(string, true)
#else
    Fable.Core.JsInterop.emitJsExpr string "JSON.stringify($0)"
#endif

let rec prettyPrint (value: LigatureValue) : string =
    match value with
    | LigatureValue.Name(Name(value)) -> value
    | LigatureValue.Int i -> i.ToString()
    | LigatureValue.String s -> encodeString s
    | LigatureValue.Slot(Slot(Some(name))) -> $"${(name)}"
    | LigatureValue.Slot(Slot(None)) -> "$"
    | LigatureValue.Quote values -> $"[{printQuote values}]" //TODO print names better
    | LigatureValue.Expression values -> $"[{printExpression values}]" //TODO print names better
    | LigatureValue.Bytes(bytes) -> printBytes bytes
    | LigatureValue.Network n -> printNetwork n
    | LigatureValue.NetworkName(NetworkName(n)) -> $"@{n}"

and printNetwork (network: Network) : string =
    (Seq.fold (fun state triple -> state + " " + (printStatement triple) + ",\n") "{" (network))
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
    $"{(printPatternName entity)} {(printPatternName attribute)} {(prettyPrint value)}"

and printPattern ((entity, attribute, value): Statement) =
    $"{(printPatternName entity)} {(printPatternName attribute)} {(prettyPrint value)}"

//TODO might not be correct
and printQuote quote =
    (List.fold (fun state value -> state + " " + (prettyPrint value)) "" quote)

//TODO might not be correct
and printExpression expression =
    (List.fold (fun state value -> state + " " + (prettyPrint value)) "" expression)

type Scope = Map<string, LigatureValue>

type WanderType =
    | String
    | Int
    | Bytes
    | Name
    | Slot
    | Network
    | NetworkName
    | AssocArray
    | LigatureValue
    | Array
    | Anything
    | Nothing
    | Function
