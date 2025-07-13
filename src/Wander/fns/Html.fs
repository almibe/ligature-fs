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
                | Expression.Element { value = content } ->
                    properties <- List.append [ IReactProperty.Text content ] properties
                | Expression.NodeLiteral node ->
                    properties <- List.append [ IReactProperty.Children [ innerGen node ] ] properties
                | x -> failwith $"ignoring value - {x}")
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

let printDataColumn (elements: Set<Element>) : string =
    match List.ofSeq elements with
    | [] -> ""
    | [ { value = single } ] -> single
    | multi ->
        List.fold
            (fun state { value = value } ->
                match state with
                | "" -> value
                | state -> $"{state}, {value}")
            ""
            multi

let assertionsTableFn: Fn =
    Fn(
        { doc = "Write out assertions as an html table encoded in Wander nodes."
          examples = [ "assertions-table(assertions(rel(a b c)))" ]
          args = "Assertions"
          result = "Node" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.Assertions assertions ] ->
                let mutable headers = Set.empty

                let data: Map<Element, Map<Term, Set<Element>>> =
                    Set.fold
                        (fun state value ->
                            match value with
                            | Assertion.Triple(e, r, f) ->
                                headers <- Set.add r headers

                                match state.TryFind e with
                                | Some values ->
                                    match values.TryFind r with
                                    | Some values -> failwith "TODO"
                                    | None -> failwith "TODO"
                                | None ->
                                    let values = Map.ofList [ r, Set.ofList [ f ] ]
                                    Map.add e values state
                            | Assertion.Instance(e, c) ->
                                match state.TryFind e with
                                | Some values -> failwith "TODO"
                                | None -> failwith "TODO"
                            | Assertion.Same(_, _) -> failwith "Not Implemented"
                            | Assertion.Different(_, _) -> failwith "Not Implemented")
                        Map.empty
                        assertions

                let headers = Term "Element" :: Term "Concepts" :: List.ofSeq headers

                let headerColumns =
                    List.map
                        (fun value ->
                            Expression.NodeLiteral
                                { name = Term "th"
                                  attributes = Map.empty
                                  children = [ Expression.Term value ] })
                        headers

                let headerRow =
                    Expression.NodeLiteral
                        { name = Term "tr"
                          attributes = Map.empty
                          children = headerColumns }

                let tableEntries: Expression list =
                    Map.fold
                        (fun state element (valueMap: Map<Term, Set<Element>>) ->
                            let firstColumn =
                                Expression.NodeLiteral
                                    { name = Term "td"
                                      attributes = Map.empty
                                      children = [ Expression.Element element ] }

                            let dataColumns: Expression list =
                                List.fold
                                    (fun state headerValue ->
                                        match valueMap.TryFind headerValue with
                                        | Some value ->
                                            let node =
                                                Expression.NodeLiteral
                                                    { name = Term "td"
                                                      attributes = Map.empty
                                                      children = [ Expression.Term(Term $"{printDataColumn value}") ] }

                                            List.append state [ node ]
                                        | None ->
                                            let node =
                                                Expression.NodeLiteral
                                                    { name = Term "td"
                                                      attributes = Map.empty
                                                      children = [ Expression.Term(Term "") ] }

                                            List.append state [ node ])
                                    [ firstColumn ]
                                    headers.Tail

                            let node =
                                Expression.NodeLiteral
                                    { name = Term "tr"
                                      attributes = Map.empty
                                      children = dataColumns }

                            List.append state [ node ])
                        [ headerRow ]
                        data

                let table =
                    Expression.NodeLiteral
                        { name = Term "table"
                          attributes = Map.empty
                          children = tableEntries }

                Ok table
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
