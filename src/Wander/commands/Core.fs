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
            | [ value ] -> Ok(Some(value))
            | _ -> failwith "id requires 1 argument." }

let readCommand: Command =
    { Name = Element("read")
      Doc = "Read the value of a given variable."
      Eval =
        fun _ variables arguments ->
            match arguments with
            | [ Any.Variable(name) ] ->
                if variables.ContainsKey name then
                    Ok(Some variables[name])
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
      Eval = fun _ networks _ -> Ok(None) }

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
            let mutable docs: Set<Entry> = Set.empty

            Map.toList commands
            |> List.iter (fun (name, command) ->
                docs <-
                    Set.add
                        ({ element = name
                           attribute = Element("docString")
                           value = Value.Literal(command.Doc) })
                        docs

                ())

            Ok(Some(Any.Network docs)) }

let coreCommands =
    (Map.ofList
        [ (docsCommand.Name, docsCommand)
          (idCommand.Name, idCommand)
          (ignoreCommand.Name, ignoreCommand)
          (readCommand.Name, readCommand)
          (evalCommand.Name, evalCommand) ])
