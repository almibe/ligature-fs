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

let generateHtmlFn: Fn =
    Fn(
        { doc = "Convert a node into html."
          examples = [ "generate-html(p { \"Test\" })" ]
          args = "Node"
          result = "Term" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.NodeLiteral node ] -> Ok(Expression.Term(Term(generateHtml node)))
            | _ -> failwith "Invalid call to generate-html."
    )
