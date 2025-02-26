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
    | Term of Term
    | Pattern of Pattern
    | Network of Network
    | ValueSet of ValueSet
    | ResultSet of ResultSet
    | Comment of string
    | AnySet of AnySet
    | Block of Script
    | Pipe

and [<RequireQualifiedAccess>] Expression =
    | Application of Any list
    | Assignment of Variable * Any

and Script = Expression list

type Arguments = Any list

type Variables = Map<Variable, Any>

and Fns = Map<Term, Fn>

and FnDoc =
    { doc: string
      examples: string list
      pre: string
      post: string }

and Fn = Fn of FnDoc * (Fns -> Variables -> Arguments -> Result<Any, LigatureError>)

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
    | Any.Quote quote -> printQuote quote
    | Any.Network n -> printNetwork n
    | Any.Slot(Slot variable) -> variable
    | Any.ResultSet rs -> printResultSet rs
    | Any.ValueSet(_) -> failwith "Not Implemented"
    | Any.Comment(_) -> failwith "Not Implemented"
    | Any.AnySet s -> printAnySet s
    | Any.Variable(_) -> failwith "Not Implemented"
    | Any.Pattern(_) -> failwith "Not Implemented"
    | Any.Block(_) -> failwith "Not Implemented"

and printQuote (quote: Quote) : string =
    (Seq.fold (fun state value -> state + (printAny value) + ", ") "[" quote) + "]"

and printAnySet (set: AnySet) : string =
    (Seq.fold (fun state value -> state + (printAny value) + ", ") "[" set)
    + "] set"

and writeTermPattern (value: TermPattern) =
    match value with
    | TermPattern.Term(Term e) -> e
    | TermPattern.Slot(Slot v) -> v

and writeTerm (Term t) = t

and printResultSet (rs: ResultSet) =
    let mutable res = "ResultSet("

    Set.iter
        (fun variables ->
            res <- res + "("

            Map.iter (fun (Slot var) value -> res <- res + var + " " + (writeTerm value) + ", ") variables

            res <- res + ")")
        rs

    res <- res + ")"
    res

and printPattern (network: Pattern) : string =
    let mutable first = true

    (Seq.fold
        (fun state triple ->
            if first then
                first <- false
                state + " " + (printTriplePattern triple) + ","
            else
                state + "\n  " + (printTriplePattern triple) + ",")
        "{"
        (network))
    + " }"

and printNetwork (network: Network) : string =
    let mutable first = true

    Seq.fold
        (fun state triple ->
            if first then
                first <- false
                state + " " + printTriple triple + ","
            else
                state + "\n  " + printTriple triple + ",")
        "{"
        network
    + " }"

and printTriplePattern ((element, attribute, value): TriplePattern) : string =
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

    $"{encodeString element} {encodeString attribute} {encodeString value}"

and printTriple ((Term(element), Term(attribute), Term(value)): Triple) : string =
    $"{encodeString element} {encodeString attribute} {encodeString value}"

// let printStack (stack: Variables) : string =
//     if List.isEmpty stack then
//         "--empty stack--"
//     else
//         List.fold (fun state any -> state + " → " + printAny any + "\n") "" stack

// let printStackAsScript (stack: Variables) =
//     List.fold (fun state any -> state + printAny any + "\n") "" stack
