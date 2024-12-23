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
        fun _ _ arguments ->
            match arguments with
            | [ value ] -> Ok(SimpleResult(Some(value)))
            | _ -> failwith "id requires 1 argument." }

let readCommand: Command =
    { Name = Element("read")
      Doc = "Read the value of a given variable."
      Eval =
        fun _ variables arguments ->
            match arguments with
            | [ Any.Variable(name) ] ->
                if variables.ContainsKey name then
                    Ok(SimpleResult(Some variables[name]))
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

let ignoreCommand: Command =
    { Name = Element("ignore")
      Doc = "Ignore any arguments passed and return working state unchanged."
      Eval = fun _ networks _ -> Ok(SimpleResult(None)) }

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
        fun commands networks _ ->
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

            Ok(SimpleResult(Some(Any.Network docs))) }

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
        fun _ _ arguments ->
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

            Ok(SimpleResult(Some(Any.ResultSet resultSet))) }

let coreCommands =
    (Map.ofList
        [ (docsCommand.Name, docsCommand)
          (idCommand.Name, idCommand)
          (ignoreCommand.Name, ignoreCommand)
          (readCommand.Name, readCommand)
          (resultSetCommand.Name, resultSetCommand)
          // (containsCommand.Name, containsCommand)
          (evalCommand.Name, evalCommand) ])
