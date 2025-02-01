// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.JSON

open Ligature.Model
open Wander.Model
open Wander.Interpreter

let rec anyToJson (any: Any): Result<string, LigatureError> =
    match any with
    | Any.Element (Element e) -> Ok (encodeString e)
    | Any.Literal l -> Ok (encodeString l)
    | Any.Quote q -> quoteToJson q
    | _ -> failwith "TODO"

and quoteToJson (quote: Quote): Result<string, LigatureError> =
    match quote with
    | Any.Element (Element "object") :: tail -> 
        let mutable res = "{"
        List.chunkBySize 2 tail
        |> List.iteri (fun i value -> 
            match value with
            | [Any.Literal name; value] ->
                if i > 0 then res <- res + ","
                res <- res + $"{encodeString name}:" + 
                    match anyToJson value with
                    | Ok jsonRes -> jsonRes
                    | _ -> failwith "TODO"
            | _ -> failwith "TODO")
        res <- res + "}"
        Ok(res)
    | Any.Element (Element "array") :: tail -> 
        let mutable res = "["
        List.iteri (fun i value -> 
            match anyToJson value with
            | Ok jsonRes -> 
                if i > 0 then res <- res + ","
                res <- res + jsonRes
            | _ -> failwith "TODO") tail
        res <- res + "]"
        Ok(res)
    | [Any.Element (Element "number"); Any.Literal value] ->
        match System.Double.TryParse(value) with 
        | true, n -> Ok(value)
        | _ -> failwith "TODO"
    | [Any.Element (Element "boolean"); Any.Literal value] ->
        match value with
        | "true" -> Ok(value)
        | "false" -> Ok(value)
        | _ -> failwith "TODO"
    | _ -> error "Invalid quote passed to to-json" None

let toJSONAction: Action =
    Action.Stack({ doc = "Write an encoded Quote to JSON." }, fun stack ->
        match stack with
        | Any.Quote json :: tail -> 
            match quoteToJson(json) with
            | Ok result -> Ok(Any.Literal result :: tail)
            | _ -> failwith "TODO"
        | Any.Literal literal :: tail ->
            Ok(Any.Literal (encodeString literal) :: tail)
        | _ -> error "Invalid call to to-json." None)
