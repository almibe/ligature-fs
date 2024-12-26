module ``Wander.CodeGen.Gen.Docs`` =
    let ``doc-descriptionQuote`` =
        [ Any.Element Element "id"
          Any.Network
              Set.ofList
                  [ ElementPattern.Element Element "doc.Markdown",
                    ElementPattern.Element Element "tdl.subconcept-of",
                    Value.Element Element "tdl.Literal"
                    ElementPattern.Element Element "wander.Command",
                    ElementPattern.Element Element ":",
                    Value.Element Element "tdl.Concept"
                    ElementPattern.Element Element "wander.Command",
                    ElementPattern.Element Element "tdl.has-attribute",
                    Value.Element Element "wander.CommandName"
                    ElementPattern.Element Element "wander.CommandName",
                    ElementPattern.Element Element "tdl.domain",
                    Value.Element Element "dlt.Element"
                    ElementPattern.Element Element "wander.CommandName",
                    ElementPattern.Element Element "tld.range",
                    Value.Element Element "dlt.Element" ] ]

    let ``doc-descriptionCommand`` =
        { Eval = fun local modules variables arguments -> evalQuote local modules variables doc-descriptionQuote }

    let docsQuote =
        [ Any.Element Element "id"
          Any.Network
              Set.ofList
                  [ ElementPattern.Element Element "wander.command.union",
                    ElementPattern.Element Element ":",
                    Value.Element Element "wander.Command"
                    ElementPattern.Element Element "wander.command.union",
                    ElementPattern.Element Element "wander.CommandName",
                    Value.Element Element "union" ] ]

    let docsCommand =
        { Eval = fun local modules variables arguments -> evalQuote local modules variables docsQuote }
