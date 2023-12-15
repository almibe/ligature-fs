// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature

open System.Text.RegularExpressions

type LigatureError =
    { UserMessage: string
      DebugMessage: string option }

let error userMessage debugMessage =
    Error(
        { UserMessage = userMessage
          DebugMessage = debugMessage }
    )

type Graph = Graph of string

let graphName (Graph name) = name

type Label = private Label of string

let invalidLabel (id: string) = error $"Invalid Label, {id}" None

let label =
    let labelPattern =
        Regex(@"^[a-zA-Z0-9-._~:/?#\[\]@!$&'()*+,;%=]+$", RegexOptions.Compiled)

    fun (id: string) ->
        if labelPattern.IsMatch(id) then
            Ok(Label id)
        else
            invalidLabel id

let readLabel (Label label) = label

type Value =
    | Label of Label
    | String of string
    | Integer of int64
//TODO add Bytes

type Edge =
    { Source: Label
      Label: Label
      Target: Value }

let edge source label target =
    {
        Source = source
        Label = label
        Target = target
    }

type IQueryTx =
    abstract member AllEdges: unit -> Result<Edge list, LigatureError>

    abstract member MatchEdges:
        Label option -> Label option -> Value option -> Result<Edge list, LigatureError>
//TODO add MatchStatementsRange

type IWriteTx =
    abstract member NewLabel: unit -> Result<Label, LigatureError>
    abstract member AddEdge: Edge -> Result<unit, LigatureError>
    abstract member RemoveEdge: Edge -> Result<unit, LigatureError>

type Query<'R> = IQueryTx -> Result<'R, LigatureError>

type Write = IWriteTx -> Result<unit, LigatureError>

type ILigature =
    abstract member AllGraphs: unit -> Result<Graph list, LigatureError>
    abstract member GraphExists: Graph -> Result<bool, LigatureError>
    abstract member CreateGraph: Graph -> Result<Unit, LigatureError>
    abstract member RemoveGraph: Graph -> Result<Unit, LigatureError>
    abstract member Query: Graph -> Query<'r> -> Result<'r, LigatureError>
    abstract member Write: Graph -> Write -> Result<unit, LigatureError>
    abstract member Close: unit -> Result<Unit, LigatureError>
