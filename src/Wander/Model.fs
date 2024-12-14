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
    | Any.Pattern p -> printPattern p
    | Any.Literal(value) -> encodeString value
    | Any.Variable(Variable variable) -> variable

and printNetwork (network: Set<Entry>) : string =
    let mutable first = true

    (Seq.fold
        (fun state triple ->
            if first then
                first <- false
                state + " " + (printEntry triple) + ","
            else
                state + "\n  " + (printEntry triple) + ",")
        "{"
        (network))
    + " }"

and printPattern (pattern: Pattern) : string =
    let mutable first = true

    (Seq.fold
        (fun state triple ->
            if first then
                first <- false
                state + " " + (printEntryPattern triple) + ","
            else
                state + "\n  " + (printEntryPattern triple) + ",")
        "{"
        (pattern))
    + " }"


and writeValue (value: Value) : string =
    match value with
    | Value.Element(Element element) -> element
    | Value.Literal value -> encodeString value

and printEntry (entry: Entry) : string = failwith "TODO"

and printEntryPattern (entry: EntryPattern) : string =
    $"{entry.element} {entry.attribute} {entry.value}" //TODO print values correctly
