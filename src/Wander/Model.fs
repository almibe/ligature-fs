// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Model

open Ligature.Model

type Variable = Variable of string

and [<RequireQualifiedAccess>] Expression =
    | Seq of List<Expression>
    | Term of Term
    | Element of Element
    | Variable of Variable
    | Assertion of Assertion
    | Assertions of Assertions
    | Comment of string
    | VariableApplication of VariableApplication
    | Application of Application
    | Lambda of Lambda
    | ConceptExpr of ConceptExpr
    | Definition of Definition
    | Definitions of Definitions

and VariableApplication =
    { variable: Variable
      attributes: Map<Term, Expression>
      children: Expression list }

and Application =
    { name: Term
      attributes: Map<Term, Expression>
      arguments: Expression list }

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
    | Fn of FnDoc * (Fns -> Variables -> Application -> Result<Expression, LigatureError>)
    | Macro of FnDoc * (Fns -> Variables -> Application -> Result<Expression, LigatureError>)

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

and printSeq (tuple: List<Expression>) : string =
    Seq.fold (fun state value -> state + printExpression value + " ") "[" tuple
    + "]"

and printNode
    ({ name = Term name
       attributes = attributes
       arguments = children }: Application)
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
    | { value = Term l
        space = Some(Term t)
        langTag = Some(Term langTag) } -> $"element({encodeString l} {encodeString t} {encodeString langTag})"
    | { value = Term l
        space = None
        langTag = None } -> encodeString l
// | Value.Literal { id = l } -> encodeString l
// | Value.Term(Term t) -> t


and writeTerm (Term t) = t

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

and printAssertion (assertion: Assertion) : string =
    match assertion with
    | Assertion.Triple(element, Term role, filler) -> $"[{printElement element} {role} {printElement filler}]"
    | Assertion.Instance(element, c) -> $"instance({printElement element} {printConcept c})"
    | Assertion.Same(l, r) -> failwith "Not Implemented"
    | Assertion.Different(l, r) -> failwith "Not Implemented"
