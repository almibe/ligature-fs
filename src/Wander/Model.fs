// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Model

open Ligature.Model

type Variable = Variable of string

type Tuple = Any list

and AnySet = Set<Any>

and Record = Map<Any, Any>

and [<RequireQualifiedAccess>] Any =
    | Slot of Slot
    | Tuple of Tuple
    | Term of Term
    | Literal of Literal
    | Variable of Variable
    | Pattern of Pattern
    | Assertion of Assertion
    | Assertions of Assertions
    | ValueSet of ValueSet
    | ResultSet of ResultSet
    | Comment of string
    | AnySet of AnySet
    | Record of Record
    | Application of Application
    | Lambda of Lambda
    | ConceptExpr of ConceptExpr
    | Definition of Definition
    | Definitions of Definitions

and Application = Term * Any list

and Script = Any list

and Lambda = Variable list * Script

type Arguments = Any list

type Bindings = Map<Term, Lambda>

type Variables = Map<Variable, Any>

and Fns = Map<Term, Fn>

and FnDoc =
    { doc: string
      examples: string list
      args: string
      result: string }

and Fn = Fn of FnDoc * (Fns -> Bindings -> Variables -> Arguments -> Result<Any, LigatureError>)

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
    | Any.Term(Term value) -> value
    | Any.Literal(Literal l) -> encodeString l
    | Any.Variable(Variable v) -> v
    | Any.Tuple tuple -> printTuple tuple
    | Any.Assertions n -> printNetwork n
    | Any.Slot(Slot variable) -> variable
    | Any.ResultSet rs -> printResultSet rs
    | Any.ValueSet(_) -> failwith "Not Implemented"
    | Any.Comment(_) -> failwith "Not Implemented"
    | Any.AnySet s -> printAnySet s
    | Any.Pattern(_) -> failwith "Not Implemented"
    | Any.Application(_) -> "-app-"
    | Any.Lambda _ -> failwith "TODO"
    | Any.Record record -> printRecord record
    | Any.Definition def -> printDefinition def
    | Any.Definitions defs -> printDefinitions defs

and printTuple (tuple: Tuple) : string =
    Seq.fold (fun state value -> state + printAny value + " ") "[" tuple + "]"

and printRecord (record: Record) : string =
    Seq.fold (fun state (key, value) -> state + printAny key + " " + printAny value + " ") "{" (Map.toSeq record)
    + "}"

and printAnySet (set: AnySet) : string =
    Seq.fold (fun state value -> state + printAny value + " ") "set [" set + "]"

and printValue (value: Value) : string =
    match value with
    | Value.Literal(Literal l) -> encodeString l
    | Value.Term(Term t) -> t

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

            Map.iter (fun (Slot var) value -> res <- res + var + " " + printValue value + ", ") variables

            res <- res + ")")
        rs

    res <- res + ")"
    res

and printPattern (network: Pattern) : string =
    let mutable first = true

    Seq.fold
        (fun state triple ->
            if first then
                first <- false
                state + " " + printTriplePattern triple + " "
            else
                state + "\n  " + printTriplePattern triple + " ")
        "(pattern "
        network
    + " )"

and printNetwork (network: Assertions) : string =
    let mutable first = true

    Seq.fold
        (fun state triple ->
            if first then
                first <- false
                state + " " + printTriple triple + " "
            else
                state + "\n  " + printTriple triple + " ")
        "(network "
        network
    + " )"

and printTriplePattern (pattern: AssertionPattern) : string = failwith "TODO"
// let element =
//     match element with
//     | TermPattern.Term(Term e) -> e
//     | TermPattern.Slot(Slot v) -> v

// let attribute =
//     match attribute with
//     | TermPattern.Term(Term e) -> e
//     | TermPattern.Slot(Slot v) -> v

// let value =
//     match value with
//     | ValuePattern.Term(Term e) -> e
//     | ValuePattern.Slot(Slot v) -> v
//     | ValuePattern.Literal(Literal l) -> l

// $"[{encodeString element} {encodeString attribute} {encodeString value}]"

and printTriple (assertion: Assertion) : string =
    match assertion with
    | Assertion.Triple(Term element, Term attribute, value) -> $"[{element} {attribute} {printValue value}]"
    | _ -> failwith "TODO"
