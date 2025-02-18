// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Model

open Ligature.Model

type Variable = Variable of string

type Quote = Any list

and AnySet = Set<Any>

and [<RequireQualifiedAccess>] Any =
    | Slot of Slot
    | Variable of Variable
    | Quote of Quote
    | Literal of string
    | Term of Term
    | Network of Pattern
    | ValueSet of ValueSet
    | ResultSet of ResultSet
    | Comment of string
    | AnySet of AnySet

type Expression =
    | Application of Any list
    | Assignment of Variable * Any

type Script = Expression list

type Variables = Map<Variable, Any>

and Actions = Map<Term, Action>

and ActionDoc =
    { doc: string
      examples: string list
      pre: string
      post: string }

and [<RequireQualifiedAccess>] Action =
    | Full of ActionDoc * (Actions -> Variables -> Result<Variables, LigatureError>)
    | Stack of ActionDoc * (Variables -> Result<Variables, LigatureError>)

and Slots = Map<Slot, Any>

let emptySlots: Slots = Map.empty

let encodeString string =
#if !FABLE_COMPILER
    System.Web.HttpUtility.JavaScriptStringEncode(string, true)
#else
    Fable.Core.JsInterop.emitJsExpr string "JSON.stringify($0)"
#endif

let rec printAny (value: Any) : string =
    match value with
    | Any.Term(Term(value)) -> encodeString value
    | Any.Literal(value) -> encodeString value
    | Any.Quote quote -> printQuote quote
    | Any.Network n -> printNetwork n
    | Any.Slot(Slot variable) -> variable
    | Any.ResultSet rs -> printResultSet rs
    | Any.ValueSet(_) -> failwith "Not Implemented"
    | Any.Comment(_) -> failwith "Not Implemented"
    | Any.AnySet s -> printAnySet s

and printQuote (quote: Quote) : string =
    (Seq.fold (fun state value -> state + (printAny value) + ", ") "[" quote) + "]"

and printAnySet (set: AnySet) : string =
    (Seq.fold (fun state value -> state + (printAny value) + ", ") "[" set)
    + "] set"

and writeTermPattern (value: TermPattern) =
    match value with
    | TermPattern.Term(Term e) -> e
    | TermPattern.Slot(Slot v) -> v

and printResultSet (rs: ResultSet) =
    let mutable res = "ResultSet("

    Set.iter
        (fun variables ->
            res <- res + "("

            Map.iter
                (fun (Slot var) value -> res <- res + var + " " + (writeTermPattern value) + ", ")
                variables

            res <- res + ")")
        rs

    res <- res + ")"
    res

and printNetwork (network: Pattern) : string =
    let mutable first = true

    (Seq.fold
        (fun state triple ->
            if first then
                first <- false
                state + " " + (printTriple triple) + ","
            else
                state + "\n  " + (printTriple triple) + ",")
        "{"
        (network))
    + " }"

and printTriple ((element, attribute, value): TriplePattern) : string =
    let element =
        match element with
        | TermPattern.Term(Term e) -> e
        | TermPattern.Slot(Slot v) -> v

    let attribute =
        match attribute with
        | TermPattern.Term(Term e) -> e
        | TermPattern.Slot(Slot v) -> v

    let value =
        match value with
        | TermPattern.Term(Term e) -> e
        | TermPattern.Slot(Slot v) -> v

    $"{element} {attribute} {value}"

// let printStack (stack: Variables) : string =
//     if List.isEmpty stack then
//         "--empty stack--"
//     else
//         List.fold (fun state any -> state + " → " + printAny any + "\n") "" stack

// let printStackAsScript (stack: Variables) =
//     List.fold (fun state any -> state + printAny any + "\n") "" stack
