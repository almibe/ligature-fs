// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Model

open Ligature.Model
open Ligature.Model

type Variable = Variable of string


type Tuple = Expression list

and Source() =
    member _.next() : Expression option = failwith "TODO"

// with

// interface System.IComparable with
//     member _.CompareTo (obj: obj): int =
//                 raise (System.NotImplementedException())

and [<RequireQualifiedAccess>] WanderExpression =
    | Term of Term
    | Variable of Variable
    | Comment of string
    | Application of Node
    | NodeLiteral of Node
    | Seq of List<WanderExpression>

and [<RequireQualifiedAccess>] Expression =
    | Seq of List<Expression>
    | Term of Term
    | Element of Element
    | Variable of Variable
    | Assertion of Assertion
    | Assertions of Assertions
    | ObjectView of ObjectView
    | Comment of string
    | VariableApplication of VariableApplication
    | Application of Node
    // | NodeLiteral of Node
    | Lambda of Lambda
    | ConceptExpr of ConceptExpr
    | Definition of Definition
    | Definitions of Definitions

and VariableApplication =
    { variable: Variable
      attributes: Map<Term, Expression>
      children: Expression list }

and Node =
    { name: Term
      attributes: Map<Term, Expression>
      children: Expression list }

and Script = (Variable option * Expression) list

and Lambda = Variable list * Script

type Arguments = Expression list

type Variables = Map<Variable, Expression>

and Fns = Map<Term, Fn>

and FnDoc =
    { doc: string
      examples: string list
      args: string
      result: string }

and [<RequireQualifiedAccess>] Fn =
    | Fn of FnDoc * (Fns -> Variables -> Arguments -> Result<Expression, LigatureError>)
    | Macro of FnDoc * (Fns -> Variables -> Arguments -> Result<Expression, LigatureError>)

and Slots = Map<Slot, Expression>

let emptySlots: Slots = Map.empty

let encodeString string =
#if !FABLE_COMPILER
    System.Web.HttpUtility.JavaScriptStringEncode(string, true)
#else
    Fable.Core.JsInterop.emitJsExpr string "JSON.stringify($0)"
#endif

let rec printExpression (value: Expression) : string =
    match value with
    | Expression.Term(Term value) -> value
    | Expression.Element element -> printElement element
    | Expression.Variable(Variable v) -> v
    | Expression.Seq seq -> printSeq seq
    | Expression.Assertions n -> printAssertions n
    | Expression.Comment _ -> failwith "Not Implemented"
    | Expression.Application _ -> failwith "Not Implemented"
    | Expression.VariableApplication _ -> failwith "Not implemented."
    | Expression.Lambda _ -> failwith "TODO"
    | Expression.Definitions defs -> printDefinitions defs
    | Expression.Assertion _ -> "-assertion-"
    | Expression.ConceptExpr c -> printConcept c
    | Expression.Definition d -> printDefinition d
    | Expression.ObjectView view -> printObjectView view

and printObjectView (view: ObjectView) : string =
    //TODO finish printing concept and roles
    printElement view.root + "{}"

and printSeq (tuple: List<Expression>) : string =
    Seq.fold (fun state value -> state + printExpression value + " ") "[" tuple
    + "]"

and printNode
    ({ name = Term name
       attributes = attributes
       children = children }: Node)
    : string =
    let attributes =
        Map.fold (fun state (Term key) value -> $" {state} {key} = {printExpression value}") "" attributes

    let children =
        List.fold (fun state value -> $" {state} {printExpression value}") "" children

    $"{name} {{{attributes} {children}}}"
// Seq.fold (fun state (key, value) -> state + printAny key + " " + printAny value + " ") "{" (Map.toSeq record)
// + "}"

and printElement (element: Element) : string =
    match element with
    | { value = l
        space = Some(Term t)
        langTag = Some langTag } -> $"element({encodeString l} {encodeString t} {encodeString langTag})"
    | { value = l
        space = None
        langTag = None } -> encodeString l
// | Value.Literal { id = l } -> encodeString l
// | Value.Term(Term t) -> t

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

            Map.iter (fun (Slot var) value -> res <- res + var + " " + printElement value + ", ") variables

            res <- res + ")")
        rs

    res <- res + ")"
    res

// and printPattern (network: Pattern) : string =
//     let mutable first = true

//     Seq.fold
//         (fun state triple ->
//             if first then
//                 first <- false
//                 state + " " + printTriplePattern triple + " "
//             else
//                 state + "\n  " + printTriplePattern triple + " ")
//         "(pattern "
//         network
//     + " )"

and printAssertions (aBox: Assertions) : string =
    let mutable first = true

    Seq.fold
        (fun state triple ->
            if first then
                first <- false
                state + " " + printAssertion triple + " "
            else
                state + "\n  " + printAssertion triple + " ")
        "assertions( "
        aBox
    + " )"

// and printTriplePattern (pattern: AssertionPattern) : string = failwith "TODO"
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

and printAssertion (assertion: Assertion) : string =
    match assertion with
    | Assertion.Triple(element, Term role, filler) -> $"[{printElement element} {role} {printElement filler}]"
    | Assertion.Instance(element, c) -> $"instance({printElement element} {printConcept c})"
    | Assertion.Same(l, r) -> failwith "Not Implemented"
    | Assertion.Different(l, r) -> failwith "Not Implemented"
