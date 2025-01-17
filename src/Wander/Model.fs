// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Model

open Ligature.Model

type Script = Any list

type Stack = Any list

and Actions = Map<Element, Action>

and Action =
    { Eval: Actions -> Network -> Stack -> Result<Network * Stack, LigatureError> }

and Variables = Map<Variable, Any>

let emptyVariables: Variables = Map.empty

let encodeString string =
    System.Web.HttpUtility.JavaScriptStringEncode(string, true)

let rec prettyPrint (value: Any) : string =
    match value with
    | Any.Element(Element(value)) -> value
    | Any.Quote(values) -> "(" + (values.ToString()) + ")" //TODO print values correctly
    | Any.Network n -> printNetwork n
    | Any.Literal(value) -> encodeString value
    | Any.Variable(Variable variable) -> variable
    | Any.ResultSet rs -> printResultSet rs
    | Any.ValueSet(_) -> failwith "Not Implemented"

and printResultSet (rs: ResultSet) =
    let mutable res = "ResultSet("

    Set.iter
        (fun variables ->
            res <- res + "("
            Map.iter (fun (Variable var) value -> res <- res + var + " " + (writeValue value) + ", ") variables
            res <- res + ")")
        rs

    res <- res + ")"
    res

and printNetwork (network: Network) : string =
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

and writeValue (value: Value) : string =
    match value with
    | Value.Element(Element element) -> element
    | Value.Literal value -> encodeString value
    | Value.Variable(Variable variable) -> variable
    | Value.Quote(quote) -> failwith "Not Implemented"
    | Value.Network(network) -> failwith "Not Implemented"

and printTriple ((element, attribute, value): Triple) : string =
    let element =
        match element with
        | ElementPattern.Element(Element e) -> e
        | ElementPattern.Variable(Variable v) -> v

    let attribute =
        match attribute with
        | ElementPattern.Element(Element e) -> e
        | ElementPattern.Variable(Variable v) -> v

    $"{element} {attribute} {writeValue value}"
