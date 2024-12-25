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

    Oak() { 
        AnonymousModule() {
            Module($"Wander.CodeGen.Gen.{moduleName}") {
                failwith "TODO" 
            }
        }
    }
    |> Gen.mkOak
    |> CodeFormatter.FormatOakAsync
    |> Async.RunSynchronously
    |> printfn "%s"

    0
