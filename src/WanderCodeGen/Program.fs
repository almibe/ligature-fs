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
open System.IO

let kababToCamel (input: string) =
    let (res, _) = 
        Seq.fold (fun (state, upperCase) value ->
            if value = '-' then
                state, true
            else
                if upperCase then
                    (state + (value.ToString().ToUpper())), false
                else
                    (state + value.ToString(), false)) ("", false) input
    res

let encodeElementPattern (e: ElementPattern) =
    match e with
    | ElementPattern.Element(Element e) ->
        AppExpr("ElementPattern.Element", [ ParenExpr(AppExpr("Element", [ ConstantExpr(String(e)) ])) ])
    | ElementPattern.Variable(Variable v) ->
        AppExpr("ElementPattern.Variable", [ ParenExpr(AppExpr("Variable", [ ConstantExpr(String(v)) ])) ])

let encodeValue (value: Value) =
    match value with
    | Value.Element(Element e) -> AppExpr("Value.Element", [ ParenExpr(AppExpr("Element", [ ConstantExpr(String(e)) ])) ])
    | Value.Literal l -> AppExpr("Value.Literal",  ConstantExpr(String(l)) )
    | Value.Variable(Variable v) -> AppExpr("Value.Variable", [ ParenExpr(AppExpr("Variable", [ ConstantExpr(String(v)) ])) ])

let encodeTriple (e: ElementPattern, a: ElementPattern, v: Value) =
    let e = encodeElementPattern e
    let a = encodeElementPattern a
    let v = encodeValue v
    TupleExpr([ e; a; v ])

let encodeNetwork (network: Network) =
    if network.IsEmpty then
        AppExpr("Any.Network", ConstantExpr(Constant("Set.empty")))
    else
        let triples = Set.toList network |> List.map (fun triple -> encodeTriple triple)
        AppExpr("Any.Network", ParenExpr(AppExpr("Set.ofList", ListExpr(triples))))

let encodeAny (any: Any) =
    match any with
    | Any.Element(Element e) -> AppExpr("Any.Element", ParenExpr(AppExpr("Element", [ ConstantExpr(String(e)) ])) )
    | Any.Network n -> encodeNetwork n
    | _ -> failwith "TODO"

[<EntryPoint>]
let main (args: string[]) =
    let path = System.Environment.GetEnvironmentVariable("WANDER_LIBS")
    let fileName = path + "/docs.wander"
    let moduleName = "Docs"
    let scriptText = System.IO.File.ReadAllText(fileName)

    let ast =
        match parseString scriptText with
        | Ok res -> res
        | _ -> failwith "TODO"

    // printfn "%A" ast

    let modules =
        List.collect
            (fun (expression) ->
                match expression with
                | Expression.CommandDefinition { name = Element name
                                                 args = args
                                                 body = body } ->
                    let quote = List.map encodeAny body
                    let name = kababToCamel name
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
                                            AppExpr(
                                                "evalQuote",
                                                [ Constant("local")
                                                  Constant("modules")
                                                  Constant("variables")
                                                  Constant($"{name}Quote") ]
                                            )
                                        )
                                    ) ]
                              )
                          )
                      ) ]
                | _ -> failwith "Unexpected type.")
            ast

    let writeFile (moduleName: string) (content: string) =
        File.WriteAllText($"../WanderGen/{moduleName}.fs", content)

    Oak() { Namespace("Wander.CodeGen.Gen") { 
        Open([ "Ligature"; "Model" ])
        Open([ "Wander"; "Model" ])
        Open([ "Wander"; "Interpreter"])

        Module($"{moduleName}") { yield! modules } 
    } }
    |> Gen.mkOak
    |> CodeFormatter.FormatOakAsync
    |> Async.RunSynchronously
    |> writeFile moduleName

    0
