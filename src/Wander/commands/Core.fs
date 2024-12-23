// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.Core

open Ligature.Model
open Wander.Model
open Wander.Interpreter

let idCommand: Command =
    { Name = Element("id")
      Doc = "Return the value passed."
      Eval =
        fun commands variables arguments ->
            match arguments with
            | [ value ] -> Ok((Some(value), commands, variables))
            | _ -> failwith "id requires 1 argument." }

let readCommand: Command =
    { Name = Element("read")
      Doc = "Read the value of a given variable."
      Eval =
        fun commands variables arguments ->
            match arguments with
            | [ Any.Variable(name) ] ->
                if variables.ContainsKey name then
                    Ok((Some variables[name], commands, variables))
                else
                    error "Could not read variable" None
            | _ -> error "Illegal call to read." None }

let evalCommand: Command =
    { Name = Element("eval")
      Doc = "Eval a quote."
      Eval =
        fun commands variables arguments ->
            match arguments with
            | [ Any.Quote(quote) ] -> evalQuote commands variables quote
            | _ -> error "Illegal call to read." None }

let foldCommand: Command =
    { Name = Element("fold")
      Doc = "Perform a fold on a ResultSet.\nfold -> quote -> initialNetwork -> resultSet"
      Eval =
        fun commands variables arguments ->
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
                        match evalQuote commands variables q with
                        | Ok(Some(Any.ResultSet rs), _, _) -> rs
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let res =
                    Set.fold
                        (fun s x ->
                            let variables = Map.add (Variable "?_") (Any.ValueSet x) variables

                            match evalQuote commands variables quote with
                            | Ok(Some(Any.Network res), _, _) -> Set.union s res
                            | _ -> failwith "TODO - error in fold")
                        initialNetwork
                        resultSet

                Ok((Some(Any.Network res)), commands, variables)
            | _ -> error "Illegal call to fold." None }

let ignoreCommand: Command =
    { Name = Element("ignore")
      Doc = "Ignore any arguments passed and return working state unchanged."
      Eval = fun commands variables _ -> Ok(None, commands, variables) }

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
    { Name = Element("docs")
      Doc = "Create a network that contains documentation for the available commands."
      Eval =
        fun commands variables _ ->
            let mutable docs: Network = Set.empty

            Map.toList commands
            |> List.iter (fun (name, command) ->
                docs <-
                    Set.add
                        (ElementPattern.Element name,
                         ElementPattern.Element(Element("docString")),
                         Value.Literal(command.Doc))
                        docs

                ())

            Ok((Some(Any.Network docs), commands, variables)) }

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
    { Name = Element "result-set"
      Doc = "Construct a result set value."
      Eval =
        fun commands variables arguments ->
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

            Ok((Some(Any.ResultSet resultSet), commands, variables)) }

let coreCommands =
    (Map.ofList
        [ (docsCommand.Name, docsCommand)
          (idCommand.Name, idCommand)
          (ignoreCommand.Name, ignoreCommand)
          (readCommand.Name, readCommand)
          (resultSetCommand.Name, resultSetCommand)
          (foldCommand.Name, foldCommand)
          // (containsCommand.Name, containsCommand)
          (evalCommand.Name, evalCommand) ])
