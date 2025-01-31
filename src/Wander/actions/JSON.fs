// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.JSON

open Ligature.Model
open Wander.Model
open Wander.Interpreter

let quoteToJson (quote: Quote): Result<string, LigatureError> =
    match quote with
    | Any.Element (Element "object") :: tail -> failwith "TODO"
    | Any.Element (Element "array") :: tail -> failwith "TODO"
    | [Any.Element (Element "number"); Any.Literal value] -> failwith "TODO"
    | [Any.Element (Element "boolean"); Any.Literal value] -> failwith "TODO"
    | _ -> error "Invalid quote passed to to-json" None

let toJSONAction: Action =
    Action.Stack({ doc = "Write an encoded Quote to JSON." }, fun stack ->
        match stack with
        | Any.Quote json :: tail -> 
            quoteToJson(json)
            Ok(Any.Literal "TEST" :: tail))
        | Any.Literal literal :: tail ->
            Ok(Any.Literal System.Text.Json.JsonEncodedText.Encode(literal) :: tail)
        | _ -> error "Invalid call to to-json." None
