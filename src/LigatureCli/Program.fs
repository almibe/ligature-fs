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
open Interpreter
open System.Threading
open System.Collections
open Spectre.Console.Rendering

let store = new InMemoryStore()

let assertionsToTableList (assertions: Assertions) : Rows =
    let tripleTable = new Table()

    tripleTable.AddColumn "Element" |> ignore
    tripleTable.AddColumn(new TableColumn "Role") |> ignore
    tripleTable.AddColumn(new TableColumn "Filler") |> ignore

    let instanceTable = new Table()

    instanceTable.AddColumn "Element" |> ignore
    instanceTable.AddColumn(new TableColumn "Concept") |> ignore

    Set.iter
        (fun assertion ->
            match assertion with
            | Assertion.Instance(e, c) ->
                instanceTable.AddRow(Markup.Escape(printElement e), Markup.Escape(printConcept c))
                |> ignore
            | Assertion.Triple(e, Term r, f) ->
                tripleTable.AddRow(Markup.Escape(printElement e), Markup.Escape r, Markup.Escape(printElement f))
                |> ignore)
        assertions

    new Rows(tripleTable, instanceTable)

let printSimpleConcept (concept: SimpleConcept) : string =
    let expr =
        match concept with
        | SimpleConcept.AtomicConcept ac -> ConceptExpr.AtomicConcept ac
        | SimpleConcept.Top -> ConceptExpr.Top
        | SimpleConcept.Bottom -> ConceptExpr.Bottom
        | SimpleConcept.Exists(r, c) -> ConceptExpr.Exists(r, c)
        | SimpleConcept.All(r, c) -> ConceptExpr.All(r, c)
        | SimpleConcept.Func(r, c) -> ConceptExpr.Func(r, c)
        | SimpleConcept.Nominal e -> ConceptExpr.Nominal e
    printConcept expr

let instancesToTableList (instances: Map<Element, Set<SimpleConcept>>) =
    let table = new Table()

    table.AddColumn "Element" |> ignore
    table.AddColumn(new TableColumn "Concept") |> ignore

    Map.iter
        (fun key assertion ->
            Set.iter
                (fun value ->
                    table.AddRow(Markup.Escape(printElement key), Markup.Escape(printSimpleConcept value))
                    |> ignore)
                assertion)
        instances

    table

let triplesToTableList (triples: Set<Element * Term * Element>) =
    let table = new Table()

    table.AddColumn "Element" |> ignore
    table.AddColumn(new TableColumn "Role") |> ignore
    table.AddColumn(new TableColumn "Filler") |> ignore

    Set.iter
        (fun (e, Term r, f) ->
            table.AddRow(Markup.Escape(printElement e), Markup.Escape r, Markup.Escape(printElement f))
            |> ignore)
        triples

    table

let writeAssertionsList (assertions: Assertions) =
    AnsiConsole.Write(assertionsToTableList assertions)

let modelsToPanel (models: PotentialModel list) =
    let models: Generic.IEnumerable<IRenderable> =
        List.map (fun (value: PotentialModel) -> assertionsToTableList value.toProcess) models

    new Rows(models)

let assertionsToPanel (assertions: Assertions list) =
    let models: Generic.IEnumerable<IRenderable> =
        List.map (fun (value: Assertions) -> assertionsToTableList value) assertions

    new Rows(models)

let tableauDebug (result: ModelResult) (layout: Layout) (ctx: LiveDisplayContext) =
    let data = result.debug.Value
    let mutable step = 0
    let totalSteps = data.Length
    let mutable cont = true

    while cont do
        let table: IRenderable =
            if step < totalSteps then
                let table = new Table()
                table.AddColumn "Steps" |> ignore
                table.AddColumn "Current Model" |> ignore
                table.AddColumn "Additional Models" |> ignore
                table.AddColumn "Complete Clash Free" |> ignore
                table.AddColumn "Contains Clash" |> ignore

                let currentModel = data[step].currentModel

                let currentModelPanel =
                    new Rows(
                        new Text "To Process",
                        assertionsToTableList currentModel.toProcess,
                        new Text "Skipped",
                        assertionsToTableList currentModel.skip,
                        new Text "Instances",
                        instancesToTableList currentModel.isA,
                        new Text "Not",
                        instancesToTableList currentModel.isNot,
                        new Text "Triple",
                        triplesToTableList currentModel.triples
                    )

                table.AddRow(
                    new Text $"{step + 1}/{totalSteps}",
                    currentModelPanel,
                    modelsToPanel data[step].additionModels,
                    assertionsToPanel data[step].completedModelsClashFree,
                    assertionsToPanel data[step].completedModelWithClash
                )
            else
                let table = new Table()
                table.AddColumn "Steps" |> ignore
                table.AddColumn "Complete Clash Free" |> ignore
                table.AddColumn "Contains Clash" |> ignore

                table.AddRow(
                    new Text $"Complete",
                    assertionsToPanel result.consistent,
                    assertionsToPanel result.clashed
                )

        layout.Update table |> ignore
        ctx.Refresh()
        let key = Console.ReadKey()

        if key.Key = ConsoleKey.DownArrow then
            if step < totalSteps then
                step <- step + 1
        else if key.Key = ConsoleKey.UpArrow then
            if step > 0 then
                step <- step - 1
        else if key.Key = ConsoleKey.Q then
            cont <- false
        else
            ()

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
          Term "tableau-step",
          Fn.Fn(
              { doc =
                  "Open a step debugger for the tableau algorithm. Press up and down to step throught and Q to quit."
                examples = [ "tableau-step(definitions() assertions())" ]
                args = "Definitions Assertions"
                result = "Set" },
              fun _ _ application ->
                  match application.arguments with
                  | [ Expression.Definitions tBox; Expression.Assertions aBox ] ->
                      let layout: Layout = new Layout()

                      AnsiConsole
                          .Live(layout)
                          .Start(fun ctx ->
                              match tableauModels tBox aBox true with
                              | Ok result ->
                                  tableauDebug result layout ctx
                                  Ok Expression.Unit
                              | Ok _ -> failwith "Should never reach."
                              | Error err -> Error err)
                  | _ -> error "Invalid call to tableau-model." None
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
