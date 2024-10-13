// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Tokenizer

let whitespaceNibbler = Nibblers.takeWhile (fun c -> c = ' ' || c = '\t')

[<RequireQualifiedAccess>]
type Token =
    | WhiteSpace of string
    | NewLine of string
    | Name of string
    | OpenBrace
    | CloseBrace
    | OpenSquare
    | CloseSquare
    | OpenParen
    | CloseParen
    | Comma
    | Exists
    | All
    | ConceptInclusion
    | ConceptConjunction
    | ConceptDisjunction
    | Negation
    | Dot
    | Top
    | Bottom
    | Definition
    | QuestionMark

let implode (chars: char list) =
    chars |> Array.ofList |> System.String.Concat

let takeAndMap toTake toMap =
    Gaze.map (Nibblers.takeString toTake) (fun _ -> toMap)

let nameNibbler =
    Nibblers.takeAll
        [ (Nibblers.repeat (Nibblers.takeInRange [ ('0', '9'); ('a', 'z'); ('A', 'Z'); ('_', '_'); ('-', '-') ])) ]

let newLineNibbler =
    Nibblers.takeFirst [ (Nibblers.takeString "\r\n"); (Nibblers.takeString "\n") ]

let newLineTokenNibbler =
    Gaze.map (Nibblers.repeat newLineNibbler) (fun text -> text |> List.concat |> implode |> Token.NewLine)

let whiteSpaceNibbler =
    Gaze.map (Nibblers.repeat (Nibblers.take ' ')) (fun ws -> ws |> implode |> Token.WhiteSpace)

let nameOrKeyidentifierTokenNibbler =
    Gaze.map nameNibbler (fun chars -> chars |> List.concat |> implode |> Token.Name)

let tokenNibbler =
    Nibblers.optional (
        Nibblers.repeat (
            Nibblers.takeFirst (
                [ whiteSpaceNibbler
                  nameOrKeyidentifierTokenNibbler
                  newLineTokenNibbler
                  takeAndMap "," Token.Comma
                  takeAndMap "." Token.Dot
                  takeAndMap "∃" Token.Exists
                  takeAndMap "∀" Token.All
                  takeAndMap "⊑" Token.ConceptInclusion
                  takeAndMap "⊔" Token.ConceptDisjunction
                  takeAndMap "≡" Token.Definition
                  takeAndMap "⊤" Token.Top
                  takeAndMap "¬" Token.Negation
                  takeAndMap "⊥" Token.Bottom
                  takeAndMap "⊓" Token.ConceptConjunction
                  takeAndMap "{" Token.OpenBrace
                  takeAndMap "}" Token.CloseBrace
                  takeAndMap "(" Token.OpenParen
                  takeAndMap ")" Token.CloseParen
                  takeAndMap "[" Token.OpenSquare
                  takeAndMap "]" Token.CloseSquare ]
            )
        )
    )

let tokenize script =
    let gaze = Gaze.fromString (script)

    match Gaze.attempt tokenNibbler gaze with
    | Ok res ->
        if Gaze.isComplete gaze then
            Ok res
        else
            failwith "Error tokenizing."
    | Error err -> failwith "Error tokenizing."
