// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.Core

open Ligature.Main
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

let letCommand: Command =
    { Name = Element("let")
      Doc = "Set the value of a given named network."
      Eval =
        fun commands store arguments ->
            match processArguments commands store arguments with
            | [ Value.Element(Element(name)); Value.Network(value) ] ->
                store.SetNetwork name value |> ignore
                Ok(None)
                // | [ Value.Element(Element(name)); Value.Quote(call) ] ->
                //     match evalCall commands store call with
                //     | Ok(Some(Value.Network(value))) -> store.SetNetwork name value |> ignore
                //     | _ -> failwith "TODO"

                Ok(None)
            | _ -> error "Illegal call to let." None }

let readCommand: Command =
    { Name = Element("read")
      Doc = "Read the value of a given network."
      Eval =
        fun _ store arguments ->
            match arguments with
            | [ Value.Element(Element(name)) ] ->
                match store.ReadNetwork name with
                | Ok res ->
                    let network = Value.Network(res)
                    Ok(Some(network))
                | _ -> error "Could not read network" None
            | _ -> failwith "TODO" }

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
                        (Entry.Attribute
                            { element = name
                              attribute = Element("docString")
                              value = Value.Value(command.Doc) })
                        docs

                ())

            Ok(Some(Value.Network docs)) }

let coreCommands =
    (Map.ofList
        [ (docsCommand.Name, docsCommand)
          (idCommand.Name, idCommand)
          (ignoreCommand.Name, ignoreCommand)
          (readCommand.Name, readCommand)
          (letCommand.Name, letCommand) ])
