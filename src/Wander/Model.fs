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

and [<RequireQualifiedAccess>] Expression =
    | Slot of Slot
    | Tuple of Tuple
    | Set of Set<Expression>
    | Term of Term
    | Individual of Individual
    | Variable of Variable
    | Assertion of Assertion
    | ABox of ABox
    | Comment of string
    | Application of Node
    | NodeLiteral of Node
    | Lambda of Lambda
    | ConceptExpr of ConceptExpr
    | TBox of TBox

and Node =
    { name: Term
      attributes: Map<Term, Expression>
      children: Expression list }

and Script = (Variable option * Expression) list

and Lambda = Variable list * Script

type Arguments = Expression list

type Bindings = Map<Term, Lambda>

type Variables = Map<Variable, Expression>

and Fns = Map<Term, Fn>

and FnDoc =
    { doc: string
      examples: string list
      args: string
      result: string }

and Fn = Fn of FnDoc * (Fns -> Bindings -> Variables -> Arguments -> Result<Expression, LigatureError>)

and Slots = Map<Slot, Expression>

let emptySlots: Slots = Map.empty

let encodeString string =
#if !FABLE_COMPILER
    System.Web.HttpUtility.JavaScriptStringEncode(string, true)
#else
    Fable.Core.JsInterop.emitJsExpr string "JSON.stringify($0)"
#endif

let rec printAny (value: Expression) : string =
    match value with
    | Expression.Term(Term value) -> value
    | Expression.Individual { value = l
                              space = Some(Term t)
                              langTag = Some langTag } -> $"(literal {encodeString l} {t} {langTag})"
    | Expression.Individual { value = content } -> encodeString content
    | Expression.Variable(Variable v) -> v
    | Expression.Tuple tuple -> printTuple tuple
    | Expression.ABox n -> printABox n
    | Expression.Slot(Slot variable) -> variable
    | Expression.Comment _ -> failwith "Not Implemented"
    | Expression.NodeLiteral node -> printNode node
    | Expression.Application _ -> failwith "Not Implemented"
    | Expression.Lambda _ -> failwith "TODO"
    | Expression.TBox defs -> printDefinitions defs
    | Expression.Assertion _ -> "-assertion-"
    | Expression.ConceptExpr c -> printConcept c
    | Expression.Set s -> $"{s}"

and printTuple (tuple: Tuple) : string =
    Seq.fold (fun state value -> state + printAny value + " ") "[" tuple + "]"

and printNode
    ({ name = Term name
       attributes = attributes
       children = children }: Node)
    : string =
    let attributes =
        Map.fold (fun state key value -> $" {state} {key} = {value}") "" attributes

    let children = List.fold (fun state value -> $" {state} {value}") "" children

    $"{{{name}{attributes} {children}}}"
// Seq.fold (fun state (key, value) -> state + printAny key + " " + printAny value + " ") "{" (Map.toSeq record)
// + "}"

and printIndividual (individual: Individual) : string =
    match individual with
    | { value = l; space = Some(Term t) } -> if t = "" then encodeString l else encodeString l + "^^" + t
    | { value = l } -> encodeString l
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

            Map.iter (fun (Slot var) value -> res <- res + var + " " + printIndividual value + ", ") variables

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

and printABox (aBox: ABox) : string =
    let mutable first = true

    Seq.fold
        (fun state triple ->
            if first then
                first <- false
                state + " " + printTriple triple + " "
            else
                state + "\n  " + printTriple triple + " ")
        "a-box( "
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

and printTriple (assertion: Assertion) : string =
    match assertion with
    | Assertion.Triple(individual, Term role, filler) -> $"[{individual} {role} {printIndividual filler}]"
    | Assertion.Instance(individual, ConceptExpr.AtomicConcept(Term c)) -> $"[{individual} : {c}]"
    | Assertion.Instance(individual, c) -> $"[{individual} : {c}]"
    | Assertion.Same(_, _) -> failwith "Not Implemented"
    | Assertion.Different(_, _) -> failwith "Not Implemented"
