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
                | Expression.Element { value = content } ->
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

let generateHtmlFn: Fn =
    Fn.Fn(
        { doc = "Convert a node into html."
          examples = [ "generate-html(p { \"Test\" })" ]
          args = "Node"
          result = "Term" },
        fun _ _ application ->
            match application.arguments with
            // | [ Expression.NodeLiteral node ] -> Ok(Expression.Term(Term(generateHtml node)))
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
                | _ -> $"{state}, {value}")
            ""
            multi

let assertionsTableFn: Fn =
    Fn.Fn(
        { doc = "Write out assertions as an html table encoded in Wander nodes."
          examples = [ "assertions-table(assertions(rel(a b c)))" ]
          args = "Assertions"
          result = "Node" },
        fun _ _ application ->
            match application.arguments with
            | [ Expression.Assertions assertions ] ->
                let mutable headers = Set.empty
                //                let mutable concepts:  = Map.empty

                let data: Map<Element, Map<Term, Set<Element>>> =
                    Set.fold
                        (fun state value ->
                            match value with
                            | Assertion.Triple(e, r, f) ->
                                headers <- Set.add r headers

                                match state.TryFind e with
                                | Some values ->
                                    match values.TryFind r with
                                    | Some innerValues ->
                                        let values = Map.add r (Set.add f innerValues) values
                                        Map.add e values state
                                    | None ->
                                        let values = Map.add r (Set.ofList [ f ]) values
                                        Map.add e values state
                                | None ->
                                    let values = Map.ofList [ r, Set.ofList [ f ] ]
                                    Map.add e values state
                            | Assertion.Instance(e, c) ->
                                //TODO store concepts in a separate map to avoid issues with roles named "Concepts"
                                let concept: Element =
                                    match c with
                                    | ConceptExpr.AtomicConcept(Term c) ->
                                        { value = c
                                          space = None
                                          langTag = None }
                                    | _ -> failwith "TODO"

                                match state.TryFind e with
                                | Some values ->
                                    match values.TryFind(Term "Concepts") with
                                    | Some existingConcepts ->
                                        let values =
                                            Map.add (Term "Concepts") (Set.add concept existingConcepts) values

                                        Map.add e values state
                                    | None ->
                                        let values = Map.add (Term "Concepts") (Set.ofList [ concept ]) values
                                        Map.add e values state
                                | None ->
                                    let values = Map.ofList [ Term "Concepts", Set.ofList [ concept ] ]
                                    Map.add e values state
                            | Assertion.Same(_, _) -> failwith "Not Implemented"
                            | Assertion.Different(_, _) -> failwith "Not Implemented")
                        Map.empty
                        assertions

                let headers = Term "Element" :: Term "Concepts" :: List.ofSeq headers

                let headerColumns =
                    List.map
                        (fun (Term value) ->
                            Expression.ObjectView
                                { root =
                                    { value = "th"
                                      space = None
                                      langTag = None }
                                  concepts = Set.empty
                                  links =
                                    Map.ofList
                                        [ Term "body",

                                          [ { root =
                                                { value = value
                                                  space = None
                                                  langTag = None }
                                              concepts = Set.empty
                                              links = Map.empty } ] ] })
                        headers

                failwith "TODO"
            // let headerRow =
            //     Expression.ObjectView
            //         { root = { value = "tr"; space = None; langTag = None}
            //           concepts = Set.empty
            //           roles = headerColumns }

            // let tableEntries: Expression list =
            //     Map.fold
            //         (fun state element (valueMap: Map<Term, Set<Element>>) ->
            //             let firstColumn =
            //                 Expression.NodeLiteral
            //                     { name = Term "td"
            //                       attributes = Map.empty
            //                       children = [ Expression.Element element ] }

            //             let dataColumns: Expression list =
            //                 List.fold
            //                     (fun state headerValue ->
            //                         match valueMap.TryFind headerValue with
            //                         | Some value ->
            //                             let node =
            //                                 Expression.NodeLiteral
            //                                     { name = Term "td"
            //                                       attributes = Map.empty
            //                                       children = [ Expression.Term(Term $"{printDataColumn value}") ] }

            //                             List.append state [ node ]
            //                         | None ->
            //                             let node =
            //                                 Expression.NodeLiteral
            //                                     { name = Term "td"
            //                                       attributes = Map.empty
            //                                       children = [ Expression.Term(Term "") ] }

            //                             List.append state [ node ])
            //                     [ firstColumn ]
            //                     headers.Tail

            //             let node =
            //                 Expression.NodeLiteral
            //                     { name = Term "tr"
            //                       attributes = Map.empty
            //                       children = dataColumns }

            //             List.append state [ node ])
            //         [ headerRow ]
            //         data

            // let table =
            //     Expression.NodeLiteral
            //         { name = Term "table"
            //           attributes = Map.empty
            //           children = tableEntries }

            // Ok table
            | _ -> failwith "Invalid call to assertions-table."
    )

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
