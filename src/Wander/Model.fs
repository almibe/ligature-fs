// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Model

open Ligature.Main

type Command =
    { Name: Element
      Doc: string
      Eval: Commands -> LigatureEngine -> Arguments -> Result<WanderValue option, LigatureError> }

and [<RequireQualifiedAccess>] WanderValue =
    | Element of Element
    | Call of Call
    | Network of Network

and Call = Element * WanderValue list

and Commands = Map<Element, Command>

and Arguments = WanderValue list

let encodeString string =
#if !FABLE_COMPILER
    System.Web.HttpUtility.JavaScriptStringEncode(string, true)
#else
    Fable.Core.JsInterop.emitJsExpr string "JSON.stringify($0)"
#endif

let writeStore (store: LigatureEngine) =
    let mutable result = ""

    match store.Networks() with
    | Ok networks ->
        Set.iter
            (fun network ->
                match store.ReadNetwork network with
                | Ok entries ->
                    result <- result + "let " + network + " {"

                    Set.iter
                        (fun entry ->
                            match entry with
                            | Entry.Extends { element = Element element
                                              concept = Element concept } ->
                                result <- result + " " + element + " : " + concept + ","
                            | Entry.NotExtends(_) -> failwith "Not Implemented"
                            | Entry.Role(_) -> failwith "Not Implemented"
                            | Entry.Attribute(_) -> failwith "Not Implemented")
                        entries

                    result <- result + "}\n"
                | _ -> failwith "TODO")
            networks
    | _ -> failwith "TODO"

    result

let rec prettyPrint (value: WanderValue) : string =
    match value with
    | WanderValue.Element(Element(value)) -> encodeString value
    | WanderValue.Call(name, values) -> "(" + name.ToString() + (values.ToString()) + ")" //TODO print values correctly
    | WanderValue.Network n -> printNetwork n

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

and printEntry (entry: Entry) : string =
    match entry with
    | Entry.Extends { element = Element element
                      concept = Element concept } -> $"{element} : {concept}"
    | Entry.NotExtends { element = Element element
                         concept = Element concept } -> $"{element} ¬: {concept}"
    | Entry.Role { first = Element first
                   second = Element second
                   role = Element role } -> $"{first} {role} {second}"
    | Entry.Attribute { element = Element element
                        attribute = Element attribute
                        value = Value value } -> $"{element} {attribute} {encodeString value}"
