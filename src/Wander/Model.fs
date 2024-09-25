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

let rec prettyPrint (value: WanderValue) : string =
    match value with
    | WanderValue.Symbol(Symbol(value)) -> value
    // | WanderValue.Slot(Slot(Some(name))) -> $"${(name)}"
    // | WanderValue.Slot(Slot(None)) -> "$"
    | WanderValue.Quote values -> $"[{printQuote values}]" //TODO print names better
    | WanderValue.Expression values -> $"[{printExpression values}]" //TODO print names better
    | WanderValue.Network n -> printNetwork n

and printNetwork (network: Network) : string = failwith "TODO"
// let mutable first = true

// (Seq.fold
//     (fun state triple ->
//         if first then
//             first <- false
//             state + " " + (printStatement triple) + ","
//         else
//             state + "\n  " + (printStatement triple) + ",")
//     "{"
//     (network))
// + " }"

and printBytes bytes =
    bytes
    |> Array.map (fun value -> System.String.Format("{0:X2}", value))
    |> Array.insertAt 0 "0x"
    |> String.concat String.Empty

and printAssocArray values =
    "[ "
    + Map.fold (fun state key value -> state + $"{key} = {prettyPrint value}, ") "" values
    + "]"

// and printStatement ((entity, attribute, value): Statement) : string =
//     $"{(printSymbol entity)} {(printSymbol attribute)} {(printSymbol value)}"

// and printPattern ((entity, attribute, value): Statement) =
//     $"{(printSymbol entity)} {(printSymbol attribute)} {(printSymbol value)}"

//TODO might not be correct
and printQuote quote =
    (List.fold (fun state value -> state + " " + (prettyPrint value)) "" quote)

//TODO might not be correct
and printExpression expression =
    (List.fold (fun state value -> state + " " + (prettyPrint value)) "" expression)

//type Scope = Map<string, Pattern>
