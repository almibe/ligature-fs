// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Core

open Ligature.Model
open Wander.Model
open Wander.Interpreter


let popFn: Fn =
    Fn(
        { doc = "Remove top Term from Stack."
          examples = []
          pre = "Term"
          post = "" },
        fun actions variables arguments -> failwith "TODO"
    // match stack with
    // | [] -> error "Cannot pop an empty Stack." None
    // | _ :: tail -> Ok(tail)
    )

// let importCommand: Command =
//     { Eval =
//         fun networks local modules arguments ->
//             let mutable local = local

//             match arguments with
//             | [ Any.Term importName ] ->
//                 match modules.TryFind importName with
//                 | Some res ->
//                     Map.iter (fun name command -> local <- Map.add name command local) res
//                     Ok(None, networks, local, modules, variables)
//                 | None -> failwith "TODO"
//             | _ -> error "Illegal call to import." None }


// let readCommand: Command =
//     { Eval =
//         fun networks local modules arguments ->
//             match arguments with
//             | [ Any.Slot(name) ] ->
//                 if variables.ContainsKey name then
//                     Ok((Snetworks, local, modules))
//                 else
//                     error "Could not read variable" None
//             | _ -> error "Illegal call to read." None }

// let evalCommand: Fn =
//     { Eval =
//         fun networks local modules arguments ->
//             match arguments with
//             | [ Any.Quote(quote) ] -> evalQuote networks local modules quote
//             | _ -> error "Illegal call to read." None }

// let foldCommand: Fn =
//     { Eval =
//         fun networks local modules arguments ->
//             match arguments with
//             | [ quote; initialNetwork; resultSet ] ->
//                 let quote =
//                     match quote with
//                     | Any.Quote q -> q
//                     | _ -> failwith "TODO"

//                 let initialNetwork =
//                     match initialNetwork with
//                     | Any.Network n -> n
//                     | _ -> failwith "TODO"

//                 let resultSet =
//                     match resultSet with
//                     | Any.ResultSet rs -> rs
//                     | Any.Quote q ->
//                         match evalQuote networks local modules q with
//                         | Ok(_, _, _) -> failwith "TODO"
//                         | _ -> failwith "TODO"
//                     | _ -> failwith "TODO"

//                 let res =
//                     Set.fold
//                         (fun s x ->
//                             let variables = Map.add (Slot "?_") (Any.ValueSet x)

//                             match evalQuote networks local modules quote with
//                             | Ok(_, _, _) -> failwith "TODO"
//                             | Error err -> failwith $"TODO - error in fold - {err.UserMessage}")
//                         initialNetwork
//                         resultSet

//                 Ok(networks, local, modules)
//             | _ -> error "Illegal call to fold." None }

// let ignoreCommand: Fn =
//     { Eval = fun networks local modules _ -> Ok(networks, local, modules) }

// let printSignature ((arguments, result): WanderType list * WanderType option) : Term =
//     Term($"{arguments} -> {result}")
// List.map
//     (fun t ->
//         match t with
//         | LigatureType.Bytes -> Identifier.Name(Name("Bytes"))
//         | LigatureType.Int -> Identifier.Name(Name("Int"))
//         | LigatureType.Name -> Identifier.Name(Name("Name"))
//         | LigatureType.Network -> Identifier.Name(Name("Network"))
//         | LigatureType.NetworkName -> Identifier.Name(Name("NetworkName"))
//         | LigatureType.Quote -> Identifier.Name(Name("Quote"))
//         | LigatureType.Slot -> Identifier.Name(Name("Slot"))
//         | LigatureType.String -> Identifier.Name(Name("String"))
//         | LigatureType.Expression -> Identifier.Name(Name("Expression"))
//         | LigatureType.Value -> Identifier.Name(Name("Value")))
//     signature

// let docsCommand: Fn =
//     { Eval = fun local modules variables _ -> failwith "TODO" }
// let mutable docs: Network = Set.empty

// Map.toList modules
// |> List.iter (fun (Term moduleName, m) ->
//     Map.toList m
//     |> List.iter (fun (Term name, command) ->
//         docs <-
//             Set.add
//                 (TermPattern.Term(Term $"{moduleName}.{name}"),
//                  TermPattern.Term(Term("docString")),
//                  Value.Literal(command.Doc))
//                 docs

//         ()))

// Ok((Some(Any.Network docs), local, modules, variables)) }

// let containsCommand: Command =
//     { Name = Term "contains"
//       Doc = "Test if one network contains another."
//       Eval =
//         fun _ _ arguments ->
//             match arguments with
//             | [ Any.Network test; Any.Network data ] ->
//                 if contains test data then
//                     Ok(Some(Any.Term(Term "true")))
//                 else
//                     Ok(Some(Any.Term(Term "false")))
//             | _ -> error "Illegal call to contains" None }

// let resultSetCommand: Fn =
//     { Eval =
//         fun actions network ->
//             let mutable resultSet = Set.empty

//             List.iter
//                 (fun arg ->
//                     match arg with
//                     | Any.Quote q ->
//                         let mutable variables = Map.empty

//                         List.iter
//                             (fun chunk ->
//                                 match chunk with
//                                 | [ Any.Slot v; Any.Term e ] ->
//                                     variables <- Map.add v (Value.Term e) variables
//                                 | [ Any.Slot v; Any.Literal l ] ->
//                                     variables <- Map.add v (Value.Literal l) variables
//                                 | _ -> failwith "TODO")
//                             (q |> List.chunkBySize 2)

//                         resultSet <- Set.add variables resultSet
//                     | _ -> failwith "TODO")
//                 arguments

//             Ok(networks, local, modules) }
