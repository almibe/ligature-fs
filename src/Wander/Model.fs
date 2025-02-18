// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Model

open Ligature.Model

type Quote = Any list

and AnySet = Set<Any>

and [<RequireQualifiedAccess>] Any =
    | Variable of Slot
    | Quote of Quote
    | Literal of string
    | Element of Term
    | Network of Pattern
    | ValueSet of ValueSet
    | ResultSet of ResultSet
    | Comment of string
    | AnySet of AnySet

type Script = Any list

type Stack = Any list

and Actions = Map<Term, Action>

and ActionDoc =
    { doc: string
      examples: string list
      pre: string
      post: string }

and [<RequireQualifiedAccess>] Action =
    | Full of ActionDoc * (Actions -> Stack -> Result<Stack, LigatureError>)
    | Stack of ActionDoc * (Stack -> Result<Stack, LigatureError>)

and Variables = Map<Slot, Any>

let emptyVariables: Variables = Map.empty

let encodeString string =
#if !FABLE_COMPILER
    System.Web.HttpUtility.JavaScriptStringEncode(string, true)
#else
    Fable.Core.JsInterop.emitJsExpr string "JSON.stringify($0)"
#endif

let rec printAny (value: Any) : string =
    match value with
    | Any.Element(Term(value)) -> encodeString value
    | Any.Literal(value) -> encodeString value
    | Any.Quote quote -> printQuote quote
    | Any.Network n -> printNetwork n
    | Any.Variable(Slot variable) -> variable
    | Any.ResultSet rs -> printResultSet rs
    | Any.ValueSet(_) -> failwith "Not Implemented"
    | Any.Comment(_) -> failwith "Not Implemented"
    | Any.AnySet s -> printAnySet s

and printQuote (quote: Quote) : string =
    (Seq.fold (fun state value -> state + (printAny value) + ", ") "[" quote) + "]"

and printAnySet (set: AnySet) : string =
    (Seq.fold (fun state value -> state + (printAny value) + ", ") "[" set)
    + "] set"

and writeElementPattern (value: TermPattern) =
    match value with
    | TermPattern.Term(Term e) -> e
    | TermPattern.Variable(Slot v) -> v

and printResultSet (rs: ResultSet) =
    let mutable res = "ResultSet("

    Set.iter
        (fun variables ->
            res <- res + "("

            Map.iter
                (fun (Slot var) value -> res <- res + var + " " + (writeElementPattern value) + ", ")
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
        | TermPattern.Variable(Slot v) -> v

    let attribute =
        match attribute with
        | TermPattern.Term(Term e) -> e
        | TermPattern.Variable(Slot v) -> v

    let value =
        match value with
        | TermPattern.Term(Term e) -> e
        | TermPattern.Variable(Slot v) -> v

    $"{element} {attribute} {value}"

let printStack (stack: Stack) : string =
    if List.isEmpty stack then
        "--empty stack--"
    else
        List.fold (fun state any -> state + " → " + printAny any + "\n") "" stack

let printStackAsScript (stack: Stack) =
    List.fold (fun state any -> state + printAny any + "\n") "" stack
