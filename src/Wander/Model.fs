// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Model

open Ligature.Main

type Command =
    { Name: Element
      Doc: string
      Eval: Commands -> Variables -> Arguments -> Result<Any option, LigatureError> }

and Commands = Map<Element, Command>

and Call = Element * Arguments

and Arguments = Any list

and Variables = Map<Variable, Any>

let emptyVariables () : Variables = Map.empty

let encodeString string =
#if !FABLE_COMPILER
    System.Web.HttpUtility.JavaScriptStringEncode(string, true)
#else
    Fable.Core.JsInterop.emitJsExpr string "JSON.stringify($0)"
#endif

// let writeStore (store: LigatureEngine) =
//     let mutable result = ""

//     match store.Networks() with
//     | Ok networks ->
//         Set.iter
//             (fun network ->
//                 match store.ReadNetwork network with
//                 | Ok entries ->
//                     result <- result + "let " + network + " {"

//                     Set.iter
//                         (fun entry ->
//                             match entry with
//                             | Entry.Extends { element = Element element
//                                               concept = Element concept } ->
//                                 result <- result + " " + element + " : " + concept + ","
//                             | Entry.NotExtends(_) -> failwith "Not Implemented"
//                             //| Entry.Role(_) -> failwith "Not Implemented"
//                             | Entry.Attribute(_) -> failwith "Not Implemented")
//                         entries

//                     result <- result + "}\n"
//                 | _ -> failwith "TODO")
//             networks
//     | _ -> failwith "TODO"

//     result

let rec prettyPrint (value: Any) : string =
    match value with
    | Any.Element(Element(value)) -> value
    | Any.Quote(values) -> "(" + (values.ToString()) + ")" //TODO print values correctly
    | Any.Network n -> printNetwork n
    | Any.Literal(value) -> encodeString value
    | Any.Variable(_) -> failwith "Not Implemented"

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

and writeValue (value: Value) : string =
    match value with
    | Value.Element(Element element) -> element
    | Value.Literal value -> encodeString value

and printEntry (entry: Entry) : string =
    match entry with
    | Entry.Extends { element = Element element
                      concept = Element concept } -> $"{element} : {concept}"
    | Entry.NotExtends { element = Element element
                         concept = Element concept } -> $"{element} ¬: {concept}"
    | Entry.Attribute { element = Element element
                        attribute = Element attribute
                        value = value } -> $"{element} {attribute} {writeValue value}"
