// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.PrettyPrinter

[<RequireQualifiedAccess>]
type Doc =
    | Nil
    | Cons of Doc * Doc
    | Text of string
    | Break
    | Nest of int * Doc list
    | Group of Doc list

[<RequireQualifiedAccess>]
type SDoc =
    | Nil
    | Text of string * SDoc
    | Line of int * SDoc

let rec docToString (doc: SDoc) : string =
    match doc with
    | SDoc.Nil -> ""
    | SDoc.Text(s, d) -> s + docToString d
    | SDoc.Line(i, d) ->
        let prefix = String.replicate i " "
        prefix + docToString d

let read (input: string) : Doc = failwith "TODO"

let rec fits (indentation: int) (max: int) (docs: Doc list) : bool =
    match docs with
    | [] -> true
    | _ -> failwith "TODO"

// let rec format (token: PrintToken) : string =
//     match token with
//     | PrintToken.Text text -> text
//     | PrintToken.Break -> ""
//     | PrintToken.Group group ->
//         List.fold
//             (fun state value ->
//                 if state = "" then
//                     format value
//                 else
//                     state + " " + format value)
//             ""
//             group
//     | PrintToken.Nest(_, _) -> failwith "TODO"
//     | PrintToken.Nil -> failwith "Not Implemented"
//     | PrintToken.Cons(_, _) -> failwith "Not Implemented"
