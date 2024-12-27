namespace Wander.CodeGen.Gen

open Ligature.Model
open Wander.Model
open Wander.Interpreter

module Docs =
    let docDescriptionQuote =
        [ Any.Element (Element "id")
          Any.Network
              (Set.ofList
                  [ ElementPattern.Element (Element "doc.Markdown"),
                    ElementPattern.Element (Element "tdl.subconcept-of"),
                    Value.Element (Element "tdl.Literal")
                    ElementPattern.Element (Element "wander.Command"),
                    ElementPattern.Element (Element ":"),
                    Value.Element (Element "tdl.Concept")
                    ElementPattern.Element (Element "wander.Command"),
                    ElementPattern.Element (Element "tdl.has-role"),
                    Value.Element (Element "wander.CommandName")
                    ElementPattern.Element (Element "wander.Command"),
                    ElementPattern.Element (Element "tdl.has-role"),
                    Value.Element (Element "wander.command-name")
                    ElementPattern.Element (Element "wander.Command"),
                    ElementPattern.Element (Element "tdl.has-role"),
                    Value.Element (Element "wander.doc-string")
                    ElementPattern.Element (Element "wander.CommandName"),
                    ElementPattern.Element (Element "tdl.domain"),
                    Value.Element (Element "dlt.Element")
                    ElementPattern.Element (Element "wander.CommandName"),
                    ElementPattern.Element (Element "tdl.domain"),
                    Value.Element (Element "wander.CommandName")
                    ElementPattern.Element (Element "wander.CommandName"),
                    ElementPattern.Element (Element "tdl.range"),
                    Value.Element (Element "dlt.Element")
                    ElementPattern.Element (Element "wander.CommandName"),
                    ElementPattern.Element (Element "tld.range"),
                    Value.Element (Element "dlt.Element") ]) ]

    let docDescriptionCommand =
        { Eval = fun local modules variables arguments -> evalQuote local modules variables docDescriptionQuote }

    let docsQuote =
        [ Any.Element (Element "id")
          Any.Network
              (Set.ofList
                  [ ElementPattern.Element (Element "wander.command.union"),
                    ElementPattern.Element (Element ":"),
                    Value.Element (Element "wander.Command")
                    ElementPattern.Element (Element "wander.command.union"),
                    ElementPattern.Element (Element "doc-string"),
                    Value.Literal "Combine two networks and return the resulting network."
                    ElementPattern.Element (Element "wander.command.union"),
                    ElementPattern.Element (Element "wander.CommandName"),
                    Value.Element (Element "union") ]) ]

    let docsCommand =
        { Eval = fun local modules variables arguments -> evalQuote local modules variables docsQuote }
