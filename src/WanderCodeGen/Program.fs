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
open Ligature.Model

let encodeAny (any: Any) =
    match any with
    | Any.Element (Element e) -> AppExpr("Element", [Constant(e)])
    | Any.Network n -> failwith "TODO"
    | _ -> failwith "TODO"

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
        List.collect
            (fun (expression) ->
                match expression with
                | Expression.CommandDefinition { name = Element name
                                                 args = args
                                                 body = body } ->
                    let quote = List.map encodeAny body

                    [ AnyModuleDecl(Value($"{name}Quote", ListExpr(quote)))
                      AnyModuleDecl(
                          Value(
                              $"{name}Command",
                              RecordExpr(
                                  [ RecordFieldExpr(
                                        "Eval",
                                        LambdaExpr(
                                            [ Constant("local")
                                              Constant("modules")
                                              Constant("variables")
                                              Constant("arguments") ],
                                            AppExpr("evalQuote", [Constant("local"); Constant("modules"); Constant("variables"); Constant($"{name}Quote")])
                                        )
                                    ) ]
                              )
                          )
                      ) ]
                | _ -> failwith "Unexpected type.")
            ast

    Oak() { AnonymousModule() { Module($"Wander.CodeGen.Gen.{moduleName}") { yield! modules } } }
    |> Gen.mkOak
    |> CodeFormatter.FormatOakAsync
    |> Async.RunSynchronously
    |> printfn "%s"

    0
