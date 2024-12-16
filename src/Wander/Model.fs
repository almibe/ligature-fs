// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Model

open Ligature.Model

type Command =
    { Name: Element
      Doc: string
      Eval: Commands -> Variables -> Arguments -> Result<Any option, LigatureError> }

and Commands = Map<Element, Command>

and Call = Element * Arguments

and Arguments = Any list

and AnyAssignment = Variable * Any

and CallAssignment = Variable * Call

and [<RequireQualifiedAccess>] Expression =
    | Call of Call
    | AnyAssignment of AnyAssignment
    | CallAssignment of CallAssignment

and Script = Expression list

and Variables = Map<Variable, Any>

let emptyVariables () : Variables = Map.empty

let encodeString string =
#if !FABLE_COMPILER
    System.Web.HttpUtility.JavaScriptStringEncode(string, true)
#else
    Fable.Core.JsInterop.emitJsExpr string "JSON.stringify($0)"
#endif

let rec prettyPrint (value: Any) : string =
    match value with
    | Any.Element(Element(value)) -> value
    | Any.Quote(values) -> "(" + (values.ToString()) + ")" //TODO print values correctly
    | Any.Network n -> printNetwork n
    | Any.Literal(value) -> encodeString value
    | Any.Variable(Variable variable) -> variable
    | Any.ResultSet rs -> printResultSet rs

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

// and printNetwork (network: Network) : string =
//     let mutable first = true

//     (Seq.fold
//         (fun state triple ->
//             if first then
//                 first <- false
//                 state + " " + (printEntry triple) + ","
//             else
//                 state + "\n  " + (printEntry triple) + ",")
//         "{"
//         (network))
//     + " }"

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

and printTriple ((element, attribute, value): Triple) : string =
    $"{element} {attribute} {writeValue value}"
