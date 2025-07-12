// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Html

open Ligature.Model
open Wander.Model
open Feliz.ViewEngine

let generateHtml (node: Node) : string =
    let rec innerGen
        { name = Term tag
          attributes = attributes
          children = children }
        : ReactElement =
        let mutable properties = []
        //let mutable children = []
        Map.iter
            (fun key value ->
                match key, value with
                | Term key, Expression.Element { value = content } ->
                    properties <- List.append properties [ IReactProperty.KeyValue(key, content) ]
                | Term key, _ -> failwith $"Invalid attribute - {key}")
            attributes

        List.iter
            (fun value ->
                match value with
                | Expression.Element { value = content } ->
                    properties <- List.append properties [ IReactProperty.Text content ]
                //emitJsStatement content "newElement.append($0)"
                | Expression.NodeLiteral node ->
                    properties <- List.append properties [ IReactProperty.Children [ innerGen node ] ]
                // let childElement = createElement node
                // emitJsStatement childElement "newElement.append($0)"
                | x -> printfn $"ignoring value - {x}")
            children

        Interop.createElement tag properties

    innerGen node |> Render.htmlView

let generateHtmlSeq (nodes: Node list) : string =
    List.fold (fun state value -> state + generateHtml value) "" nodes

let generateHtmlFn: Fn =
    Fn(
        { doc = "Convert a node into html."
          examples = [ "generate-html(p { \"Test\" })" ]
          args = "Node"
          result = "Term" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.NodeLiteral node ] -> Ok(Expression.Term(Term(generateHtml node)))
            | [ Expression.Seq nodes ] -> failwith "TODO"
            | _ -> failwith "Invalid call to generate-html."
    )

let assertionsTableFn: Fn =
    Fn(
        { doc = "Write out assertions as an html table."
          examples = [ "assertions-table(assertions(a {rel(b c)}))" ]
          args = "Assertions"
          result = "Node" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.Assertions assertions ] -> failwith "TODO"
            | _ -> failwith "Invalid call to assertions-table."
    )

let forceDirectedLayoutFn: Fn =
    Fn(
        { doc = "Generate a force directed layout for the given set of assertions."
          examples = [ "force-directed-layout(assertions(a {rel(b c)}))" ]
          args = "Assertions"
          result = "Node" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.Assertions assertions ] -> failwith "TODO"
            | _ -> failwith "Invalid call to force-directed-layout."
    )
