// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Model

open Ligature.Main
open System
open System.Text.RegularExpressions
open Ligature
open Ligature.Main
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

and printNetwork (network: Set<Entry>) : string =
    let mutable first = true

    (Seq.fold
        (fun state triple ->
            if first then
                first <- false
                state + " " + (printEntry triple) + ","
            else
                state + "\n  " + (printEntry triple) + ",")
        "{"
        (network))
    + " }"

and printBytes bytes =
    bytes
    |> Array.map (fun value -> System.String.Format("{0:X2}", value))
    |> Array.insertAt 0 "0x"
    |> String.concat String.Empty

and printAssocArray values =
    "[ "
    + Map.fold (fun state key value -> state + $"{key} = {prettyPrint value}, ") "" values
    + "]"

and printEntry (entry: Entry) : string =
    match entry with
    | Entry.Extension concept -> $"{concept.element} : {concept.concept}"
    | Entry.NonExtension nc -> $"{nc.element} :¬ {nc.concept}"
    | Entry.Role role -> $"{role.first} {role.role} {role.second}"

//TODO might not be correct
and printQuote (quote: Quote) : string =
    (List.fold (fun state (Symbol(value)) -> state + " " + value) "" quote)

//TODO might not be correct
and printExpression expression =
    (List.fold (fun state value -> state + " " + (prettyPrint value)) "" expression)

//type Scope = Map<string, Pattern>
