// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Cli

open Wander.Main
open Wander.Library
open Wander.Model
open Ligature.InMemoryStore
open System
open Model
open Spectre.Console

let store = new InMemoryStore()

let writeAssertionsList (assertions: Assertions) =
    let table = new Table()

    table.AddColumn "Element" |> ignore
    table.AddColumn(new TableColumn "Role") |> ignore
    table.AddColumn(new TableColumn "Filler") |> ignore

    Set.iter
        (fun assertion ->
            match assertion with
            | Assertion.Instance(e, c) ->
                table.AddRow(Markup.Escape(printElement e), Markup.Escape(printConcept c))
                |> ignore
            | Assertion.Triple(e, Term r, f) ->
                table.AddRow(Markup.Escape(printElement e), Markup.Escape r, Markup.Escape(printElement f))
                |> ignore)
        assertions

    AnsiConsole.Write table

let stdFns = stdFns store

let cliFns =
    Map.ofList
        [ Term "write",
          Fn.Fn(
              { doc = "Print a value."
                examples = []
                args = ""
                result = "" },
              fun _ _ application ->
                  match application.arguments with
                  | [ Expression.Assertions assertions ] ->
                      writeAssertionsList assertions
                      Ok Expression.Unit
                  | [ e ] ->
                      printfn $"{printExpression e}"
                      Ok Expression.Unit
                  | x -> failwith $"TODO - {x}"
          )
          Term "write-table",
          Fn.Fn(
              { doc = "Print a table view of a set of assertions."
                examples = []
                args = ""
                result = "" },
              fun _ _ application ->
                  match application.arguments with
                  | [ Expression.Assertions assertions ] ->
                      failwith "TODO"
                      Ok Expression.Unit
                  | _ -> error "Invalid call to write-table." None
          )
          Term "read-term",
          Fn.Fn(
              { doc = "Read a term from stdin."
                examples = []
                args = ""
                result = "" },
              fun _ _ application ->
                  match application.arguments with
                  | [] ->
                      AnsiConsole.Markup "[gray]term:[/] "
                      let input = Console.ReadLine()

                      Ok(Expression.Term(Term input))
                  | x -> failwith $"TODO - {x}"
          )
          Term "clear",
          Fn.Fn(
              { doc = "Clear terminal."
                examples = []
                args = ""
                result = "" },
              fun _ _ application ->
                  match application.arguments with
                  | [] ->
                      AnsiConsole.Clear()
                      Ok Expression.Unit
                  | x -> failwith $"TODO - {x}"
          )
          Term "quit",
          Fn.Fn(
              { doc = "Quit this application."
                examples = []
                args = ""
                result = "" },
              fun _ _ _ ->
                  AnsiConsole.Markup "[green]Goodbye[/].\n"
                  Environment.Exit(0)
                  Ok Expression.Unit
          ) ]

let fns = Map.fold (fun state key value -> Map.add key value state) stdFns cliFns


let runRepl () =
    let mutable cont = true

    while cont do
        let script =
            AnsiConsole.Markup "[gray]input:[/] "
            Console.ReadLine()

        match run fns Map.empty script with
        | Ok res -> printfn $"{printExpression res}"
        | Error err -> printfn $"{err.UserMessage}"

    failwith "TODO"

[<EntryPoint>]
let main (args: string[]) =
    let interactive = args.Length = 0

    AnsiConsole.Markup "[green]Hello[/].\n"

    if interactive then
        runRepl ()
    else
        let dir = IO.Directory.GetCurrentDirectory()
        let file = $"{dir}/{args[0]}"
        let script = System.IO.File.ReadAllText file

        match run fns Map.empty script with
        | Ok res -> printfn $"{printExpression res}"
        | Error err -> printfn $"{err.UserMessage}"

    AnsiConsole.Markup "[green]Goodbye[/].\n"
    0
