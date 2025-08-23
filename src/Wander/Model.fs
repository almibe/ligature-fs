// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Model

open Ligature.Model

type Variable = Variable of string

and [<RequireQualifiedAccess>] Expression =
    | Seq of List<Expression>
    | ResultSet of ResultSet
    | Slot of Slot
    | Term of Term
    | Element of Element
    | Variable of Variable
    | Assertion of Assertion
    | Assertions of Assertions
    | Pattern of Pattern
    | Comment of string
    | VariableApplication of VariableApplication
    | Application of Application
    | Lambda of Lambda
    | ConceptExpr of ConceptExpr
    | Definition of Definition
    | Definitions of Definitions
    | Unit

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

let rec printApplication (application: Application) : string =
    let (Term name) = application.name

    let atts =
        Map.fold (fun state (Term key) value -> state + $"{key} = {printExpression value} ") "" application.attributes

    let args =
        List.fold (fun state value -> state + $"{printExpression value} ") "" application.arguments

    $"{name}({atts}{args})"

and printExpression (value: Expression) : string =
    match value with
    | Expression.Term(Term value) -> value
    | Expression.Element element -> printElement element
    | Expression.Variable(Variable v) -> $"${v}"
    | Expression.Seq seq -> printSeq seq
    | Expression.Assertions n -> printAssertions n
    | Expression.Comment _ -> failwith "Not Implemented"
    | Expression.Application a -> printApplication a
    | Expression.VariableApplication _ -> failwith "Not implemented."
    | Expression.Lambda _ -> failwith "TODO"
    | Expression.Definitions defs -> printDefinitions defs
    | Expression.Assertion _ -> "-assertion-"
    | Expression.ConceptExpr c -> printConcept c
    | Expression.Definition d -> printDefinition d
    | Expression.Unit -> "()"
    | Expression.Slot(Slot s) -> $"?{s}"
    | Expression.ResultSet rs -> printResultSet rs

and printResultSet rs = "result-set()"

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
    | Assertion.Triple(element, Term role, filler) -> $"triple({printElement element} {role} {printElement filler})"
    | Assertion.Instance(element, c) -> $"instance({printElement element} {printConcept c})"
