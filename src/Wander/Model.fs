// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Model

open Ligature.Model

type Variable = Variable of string


type Tuple = Any list

and Source() =
    member _.next() : Any option = failwith "TODO"

// with

// interface System.IComparable with
//     member _.CompareTo (obj: obj): int =
//                 raise (System.NotImplementedException())

and [<RequireQualifiedAccess>] Any =
    | Slot of Slot
    | Tuple of Tuple
    | Term of Term
    | Literal of Literal
    | Variable of Variable
    | Assertion of Assertion
    | ABox of ABox
    | Comment of string
    | NodeLiteral of Node
    | NodeExpression of Node
    | Lambda of Lambda
    | ConceptExpr of ConceptExpr
    | TBox of TBox

and Node =
    { name: Term
      attributes: Map<Term, Any>
      children: Any list }

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
    | Any.Literal { content = l
                    datatype = Some(Term t)
                    langTag = Some langTag } -> $"(literal {encodeString l} {t} {langTag})"
    | Any.Literal { content = content } -> encodeString content
    | Any.Variable(Variable v) -> v
    | Any.Tuple tuple -> printTuple tuple
    | Any.ABox n -> printNetwork n
    | Any.Slot(Slot variable) -> variable
    | Any.Comment _ -> failwith "Not Implemented"
    | Any.NodeExpression _ -> "-app-"
    | Any.Lambda _ -> failwith "TODO"
    | Any.NodeLiteral node -> printNode node
    | Any.TBox defs -> printDefinitions defs
    | Any.Assertion _ -> "-assertion-"
    | Any.ConceptExpr c -> printConcept c

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

and printValue (value: Value) : string =
    match value with
    | Value.Literal { content = l; datatype = Some(Term t) } ->
        if t = "" then encodeString l else encodeString l + "^^" + t
    | Value.Literal { content = l } -> encodeString l
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

and printNetwork (network: ABox) : string =
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
    | Assertion.Triple(Term element, Term attribute, value) -> $"[{element} {attribute} {printValue value}]"
    | Assertion.Instance(Term i, ConceptExpr.AtomicConcept(Term c)) -> $"[{i} : {c}]"
    | Assertion.Instance(Term i, c) -> $"[{i} : {c}]"
