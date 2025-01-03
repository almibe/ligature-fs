// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.Core

open Ligature.Model
open Wander.Model
open Wander.Interpreter

let importCommand: Command =
    { Eval =
        fun local modules variables arguments ->
            let mutable local = local

            match arguments with
            | [ Any.Element importName ] ->
                match modules.TryFind importName with
                | Some res ->
                    Map.iter (fun name command -> local <- Map.add name command local) res
                    Ok(None, local, modules, variables)
                | None -> failwith "TODO"
            | _ -> error "Illegal call to import." None }

let idCommand: Command =
    { Eval =
        fun local modules variables arguments ->
            match arguments with
            | [ value ] -> Ok(Some(value), local, modules, variables)
            | _ -> failwith "id requires 1 argument." }

let readCommand: Command =
    { Eval =
        fun local modules variables arguments ->
            match arguments with
            | [ Any.Variable(name) ] ->
                if variables.ContainsKey name then
                    Ok((Some variables[name], local, modules, variables))
                else
                    error "Could not read variable" None
            | _ -> error "Illegal call to read." None }

let evalCommand: Command =
    { Eval =
        fun local modules variables arguments ->
            match arguments with
            | [ Any.Quote(quote) ] -> evalQuote local modules variables quote
            | _ -> error "Illegal call to read." None }

let foldCommand: Command =
    { Eval =
        fun local modules variables arguments ->
            match arguments with
            | [ quote; initialNetwork; resultSet ] ->
                let quote =
                    match quote with
                    | Any.Quote q -> q
                    | _ -> failwith "TODO"

                let initialNetwork =
                    match initialNetwork with
                    | Any.Network n -> n
                    | _ -> failwith "TODO"

                let resultSet =
                    match resultSet with
                    | Any.ResultSet rs -> rs
                    | Any.Quote q ->
                        match evalQuote local modules variables q with
                        | Ok(Some(Any.ResultSet rs), _, _, _) -> rs
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let res =
                    Set.fold
                        (fun s x ->
                            let variables = Map.add (Variable "?_") (Any.ValueSet x) variables

                            match evalQuote local modules variables quote with
                            | Ok(Some(Any.Network res), _, _, _) -> Set.union s res
                            | Ok(None, _, _, _) -> failwith "TODO"
                            | Error err -> failwith $"TODO - error in fold - {err.UserMessage}")
                        initialNetwork
                        resultSet

                Ok((Some(Any.Network res)), local, modules, variables)
            | _ -> error "Illegal call to fold." None }

let ignoreCommand: Command =
    { Eval = fun local modules variables _ -> Ok(None, local, modules, variables) }

// let printSignature ((arguments, result): WanderType list * WanderType option) : Element =
//     Element($"{arguments} -> {result}")
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

let docsCommand: Command =
    { Eval = fun local modules variables _ -> failwith "TODO" }
// let mutable docs: Network = Set.empty

// Map.toList modules
// |> List.iter (fun (Element moduleName, m) ->
//     Map.toList m
//     |> List.iter (fun (Element name, command) ->
//         docs <-
//             Set.add
//                 (ElementPattern.Element(Element $"{moduleName}.{name}"),
//                  ElementPattern.Element(Element("docString")),
//                  Value.Literal(command.Doc))
//                 docs

//         ()))

// Ok((Some(Any.Network docs), local, modules, variables)) }

// let containsCommand: Command =
//     { Name = Element "contains"
//       Doc = "Test if one network contains another."
//       Eval =
//         fun _ _ arguments ->
//             match arguments with
//             | [ Any.Network test; Any.Network data ] ->
//                 if contains test data then
//                     Ok(Some(Any.Element(Element "true")))
//                 else
//                     Ok(Some(Any.Element(Element "false")))
//             | _ -> error "Illegal call to contains" None }

let resultSetCommand: Command =
    { Eval =
        fun local modules variables arguments ->
            let mutable resultSet = Set.empty

            List.iter
                (fun arg ->
                    match arg with
                    | Any.Quote q ->
                        let mutable variables = Map.empty

                        List.iter
                            (fun chunk ->
                                match chunk with
                                | [ Any.Variable v; Any.Element e ] ->
                                    variables <- Map.add v (Value.Element e) variables
                                | [ Any.Variable v; Any.Literal l ] ->
                                    variables <- Map.add v (Value.Literal l) variables
                                | _ -> failwith "TODO")
                            (q |> List.chunkBySize 2)

                        resultSet <- Set.add variables resultSet
                    | _ -> failwith "TODO")
                arguments

            Ok((Some(Any.ResultSet resultSet), local, modules, variables)) }

let coreCommands =
    (Map.ofList
        [ (Element "docs", docsCommand)
          (Element "id", idCommand)
          (Element "ignore", ignoreCommand)
          (Element "read", readCommand)
          (Element "result-set", resultSetCommand)
          (Element "fold", foldCommand)
          // (containsCommand.Name, containsCommand)
          (Element "eval", evalCommand) ])
