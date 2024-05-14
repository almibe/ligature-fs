// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Model

open Ligature
open System
open Ligature.InMemory.Pattern

type Slot private (name: string option) =
    member _.Named = name.IsSome

    member _.Name =
        match name with
        | Some name -> name
        | None -> ""

    static member New(name: string option) =
        let slotPattern = Regex(@"^[a-zA-Z0-9_]+$", RegexOptions.Compiled)
        let invalidSlot (id: string) = error $"Invalid Slot, {id}" None

        match name with
        | Some name ->
            if slotPattern.IsMatch(name) then
                Ok(Slot(Some name))
            else
                invalidSlot name
        | None -> Ok(Slot(None))

    static member Empty = Slot(None)

    override this.Equals(other) =
        let other = other :?> Slot
        this.Name = other.Name

    interface IComparable with
        member this.CompareTo(other) =
            let other = other :?> Slot
            this.Name.CompareTo(other.Name)

let slot name = Slot.New name


[<RequireQualifiedAccess>]
type PatternIdentifier =
    | Slot of Slot
    | Identifier of Identifier

[<RequireQualifiedAccess>]
type PatternValue =
    | Slot of Slot
    | Value of Value

type PatternStatement =
    { Entity: PatternIdentifier
      Attribute: PatternIdentifier
      Value: PatternValue }

type IPattern =
    abstract member PatternStatements: Set<PatternStatement>
    abstract member Apply: Map<Slot, Value> -> IDataset option
    abstract member Dataset: IDataset option
    abstract member SingleRoot: bool


[<RequireQualifiedAccess>]
type Expression =
    | Colon
    | Int of bigint
    | Bytes of byte array
    | String of string
    | Bool of bool
    | Identifier of Identifier
    | Slot of Slot
    | Let of name: string * value: Expression
    | Name of string
    | Grouping of Expression list
    | Array of Expression list
    | Application of Expression list
    | FunctionCall of name: string * arguments: Expression list
    | Record of list<string * Expression>
    | Pattern of DatasetPatternRoot list
    | Lambda of list<string> * Expression

and EntityDescription = Expression * Expression list

and DatasetPatternRoot = Expression * EntityDescription list

[<RequireQualifiedAccess>]
type WanderValue =
    | Int of bigint
    | String of string
    | Bool of bool
    | Identifier of Identifier
    | Slot of Slot
    | Statement of Ligature.Statement
    | Function of Function
    | Array of WanderValue array
    | Pattern of IPattern
    | Namespace of Map<string, WanderValue>
    | Bytes of byte array

and [<RequireQualifiedAccess>] Function =
    | Lambda of paramters: string list * body: Expression
    | HostFunction of HostFunction

and HostFunction(eval: WanderValue list -> Bindings.Bindings<string, WanderValue> -> Result<WanderValue, LigatureError>)
    =
    member _.Run args bindings = eval args bindings

type Parameter = { name: string; tag: string }

type Bindings = Bindings.Bindings<string, WanderValue>

//TODO try to remove this
let rec wanderEquals (left: WanderValue) (right: WanderValue) : bool =
    if
        (left = WanderValue.Pattern(emptyPattern) || left = WanderValue.Namespace(Map.empty))
        && (right = WanderValue.Pattern(emptyPattern)
            || right = WanderValue.Namespace(Map.empty))
    then
        true
    else
        match left, right with
        | WanderValue.Array(left), WanderValue.Array(right) ->
            if left.Length = right.Length then
                Array.forall2 (fun left right -> wanderEquals left right) left right
            else
                false
        | _ -> left = right

// [<Emit("JSON.stringify($0)")>]
// let encode (input: string): string = jsNative        

let encodeString string =
    #if !FABLE_COMPILER
        System.Web.HttpUtility.JavaScriptStringEncode(string, true)
    #else
        failwith "TODO"
        //encode s
    #endif

let rec prettyPrint (value: WanderValue) : string =
    match value with
    | WanderValue.Int i -> sprintf "%A" i
    | WanderValue.String s -> encodeString s
    | WanderValue.Bool b -> sprintf "%b" b
    | WanderValue.Identifier i -> $"`{(readIdentifier i)}`"
    | WanderValue.Slot s -> $"${(s.Name)}"
    | WanderValue.Array(values) -> $"[{printValues values}]"
    | WanderValue.Statement(statement) -> printStatementLiteral statement
    | WanderValue.Namespace(values) -> printRecord values
    | WanderValue.Function(_) -> "Function"
    | WanderValue.Bytes(bytes) -> printBytes bytes
    | WanderValue.Pattern(values) ->
        (Set.fold (fun state statement -> state + " " + (printPattern statement) + ", ") "{" values.PatternStatements)
        + "}"

and printBytes bytes =
    bytes
    |> Array.map (fun value -> System.String.Format("{0:X2}", value))
    |> Array.insertAt 0 "0x"
    |> String.concat String.Empty

and printRecord values =
    "{ "
    + Map.fold (fun state key value -> state + $"{key} = {prettyPrint value}, ") "" values
    + "}"

and printStatementLiteral statement =
    $"(`{(readIdentifier statement.Entity)}` `{(readIdentifier statement.Attribute)}` {(printLigatureValue statement.Value)})"

and printStatement statement =
    $"`{(readIdentifier statement.Entity)}` `{(readIdentifier statement.Attribute)}` {(printLigatureValue statement.Value)}"

and printPatternIdentifier (patternIdentifier: PatternIdentifier) =
    match patternIdentifier with
    | PatternIdentifier.Identifier(identifier) -> $"`{readIdentifier identifier}`"
    | PatternIdentifier.Slot(slot) -> $"${(slot.Name)}"

and printPatternValue (value: PatternValue) =
    match value with
    | PatternValue.Value value ->
        match value with
        | Value.Identifier(value) -> $"`{(readIdentifier value)}`"
        | Value.Int(value) -> value.ToString()
        | Value.String(value) -> $"\"{value}\"" //TODO escape properly
        | Value.Bytes(bytes) -> printBytes bytes
    | PatternValue.Slot(slot) -> $"${(slot.Name)}"

and printPattern (pattern: PatternStatement) =
    $"{(printPatternIdentifier pattern.Entity)} {(printPatternIdentifier pattern.Attribute)} {(printPatternValue pattern.Value)}"

and printLigatureValue value =
    match value with
    | Value.Identifier(value) -> $"`{(readIdentifier value)}`"
    | Value.Int(value) -> value.ToString()
    | Value.String(value) -> $"\"{value}\"" //TODO escape properly
    | Value.Bytes(bytes) -> printBytes bytes

and printValues values =
    Seq.fold (fun x y -> x + (prettyPrint y) + ", ") "" values
