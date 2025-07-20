// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Html

open Ligature.Model
open Wander.Model
open Feliz.ViewEngine

let generateHtml (view: ObjectView) : string =
    let rec innerGen
        { name = Term tag
          attributes = attributes
          arguments = children }
        : ReactElement =
        let mutable properties = []

        Map.iter
            (fun key value ->
                match key, value with
                | Term key, Expression.Element { value = content } ->
                    properties <- List.append [ IReactProperty.KeyValue(key, content) ] properties
                | Term key, _ -> failwith $"Invalid attribute - {key}")
            attributes

        List.iter
            (fun value ->
                match value with
                | Expression.Term(Term t) -> properties <- List.append [ IReactProperty.Text t ] properties
                | Expression.Element { value = Term content } ->
                    properties <- List.append [ IReactProperty.Text content ] properties
                // | Expression.NodeLiteral node ->
                //     properties <- List.append [ IReactProperty.Children [ innerGen node ] ] properties
                | x -> failwith $"ignoring value - {x}")
            children

        Interop.createElement tag properties

    failwith "TODO"
//    innerGen node |> Render.htmlView

let generateHtmlSeq (nodes: ObjectView list) : string =
    List.fold (fun state value -> state + generateHtml value) "" nodes

let printDataColumn (elements: Set<Element>) : string =
    match List.ofSeq elements with
    | [] -> ""
    | [ { value = Term single } ] -> single
    | multi ->
        List.fold
            (fun state { value = Term value } ->
                match state with
                | "" -> value
                | _ -> $"{state}, {value}")
            ""
            multi

let forceDirectedLayoutFn: Fn =
    Fn.Fn(
        { doc = "Generate a force directed layout for the given set of assertions."
          examples = [ "force-directed-layout(assertions(a {rel(b c)}))" ]
          args = "Assertions"
          result = "Node" },
        fun _ _ application ->
            match application.arguments with
            | [ Expression.Assertions assertions ] -> failwith "TODO"
            | _ -> failwith "Invalid call to force-directed-layout."
    )
