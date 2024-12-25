// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.CodeGen.Main

open Wander.Main
open Wander.Lib
open Wander.Model

open Fantomas.Core
open Fabulous.AST
open type Fabulous.AST.Ast
open Wander.Parser

[<EntryPoint>]
let main (args: string[]) =
    let fileName = "./Test.wander"
    let moduleName = "Test"
    let scriptText = System.IO.File.ReadAllText(fileName)

    let ast =
        match parseString scriptText with
        | Ok res -> res
        | _ -> failwith "TODO"

    printfn "%A" ast

    let modules =
        List.map
            (fun (expression) ->
                match expression with
                | Expression.CommandDefinition def -> AnyModuleDecl(ConstantExpr(String(def.ToString())))
                | _ -> failwith "Unexpected type.")
            ast

    Oak() {
        AnonymousModule() {
            Module($"Wander.CodeGen.Gen.{moduleName}") {
                // let rec x (expressions: Script) =
                //     match expressions with
                //     | head :: tail ->
                //         match head with
                //         | Expression.CommandDefinition def ->
                //             Value (def.name.ToString(), "3")
                //             printfn "TEST"

                //         | _ -> failwith "Unsupported type."
                // x ast
                yield! modules
            // Value("x", "6")
            }
        }
    }
    |> Gen.mkOak
    |> CodeFormatter.FormatOakAsync
    |> Async.RunSynchronously
    |> printfn "%s"

    0
