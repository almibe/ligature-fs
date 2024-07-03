// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Ligature

open Ligature.Wander.Model
open Ligature.Main

let patternTripleToTriple (pattern: Triple) : Triple option =
    match pattern with
    | { Entity = PatternIdentifier.Id(entity)
        Attribute = PatternIdentifier.Id(attribute) } -> failwith "TODO"
    | _ -> failwith "TODO"

let apply (network: Network) (pattern: Map<string, WanderValue>) =
    let res data =
        data
        |> Map.toSeq
        |> Seq.map (fun (k, v) ->
            let k = Slot(Some(k))

            let v =
                match v with
                | WanderValue.Identifier i -> Value.Identifier i
                | WanderValue.Int i -> Value.Int i
                | WanderValue.String s -> Value.String s
                | WanderValue.Bytes b -> Value.Bytes b
                | _ -> failwith "Error"

            (k, v))
        |> Map.ofSeq

    Ok(WanderValue.Network(network.Apply(res pattern)))

let applyFunction =
    { Module = "Ligature"
      Name = "apply"
      Description = "Take a Pattern and set of named values and use the named values to fill Slots in the Pattern."
      //      Examples = []
      Parameters = [ ("pattern", WanderType.Network); ("values", WanderType.Record) ]
      Returns = WanderType.Network
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.Network(pattern); WanderValue.Namespace(data) ] -> apply pattern data
            | value -> error $"Unexpected value passed to Ligature.apply - {value}." None) }

let unionFunction =
    { Module = "Ligature"
      Name = "union"
      Description = "Combine two Networks together."
      Parameters = [ ("left", WanderType.Network); ("right", WanderType.Network) ]
      Returns = WanderType.Network
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.Network(left); WanderValue.Network(right) ] -> Ok(WanderValue.Network(left.Union(right)))
            | _ -> failwith "error") }

let minusFunction =
    { Module = "Ligature"
      Name = "minus"
      Description = "Remove all Triples in the right Network from the left Network."
      Parameters = [ ("left", WanderType.Network); ("right", WanderType.Network) ]
      Returns = WanderType.Network
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.Network(left); WanderValue.Network(right) ] -> Ok(WanderValue.Network(left.Minus(right)))
            | _ -> failwith "error") }

let countFunction =
    { Module = "Ligature"
      Name = "count"
      Description = "Count the number of Triples in a Network."
      Parameters = [ ("network", WanderType.Network) ]
      Returns = WanderType.Int
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.Network(network) ] -> Ok(WanderValue.Int(bigint (network.Count())))
            | value -> error $"Unexpected value - {value}." None) }

let mapEduceResult (res: Set<Map<Slot, Value>>) : Result<WanderValue, LigatureError> =
    res
    |> Array.ofSeq
    |> Array.map (fun (value: Map<Slot, Value>) ->
        Map.fold
            (fun state key value ->
                match key with
                | Slot(None) -> state
                | Slot(Some(name)) -> Map.add name (toWanderValue value) state)
            Map.empty
            value)
    //    |> List.map (fun (value: Map<string, WanderValue>) -> WanderValue.Namespace value)
    |> Array.map (WanderValue.Namespace)
    |> WanderValue.Array
    |> Ok


//|> WanderValue.Array
//|> WanderValue.Namespace
//|> Ok
//    Ok(WanderValue.Namespace(Map []))

let educeFunction =
    { Module = "Ligature"
      Name = "educe"
      Description = "Take a Pattern and Network and extract out the Slots from matching subnetworks."
      Parameters = [ ("network", WanderType.Network); ("pattern", WanderType.Network) ]
      Returns = WanderType.Array
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.Network(network); WanderValue.Network(pattern) ] -> network.Educe pattern |> mapEduceResult
            | value -> error $"Unexpected value passed to Pattern.extract - {value}." None) }

// let matchFunction =
//     { Module = "Ligature"
//       Name = "match"
//       Description = "Take a Pattern and Network and return all Triples that match in a new Network."
//       Parameters = [ ("pattern", WanderType.Network); ("network", WanderType.Network) ]
//       Returns = WanderType.Array
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ WanderValue.Network(pattern); WanderValue.Network(network) ] ->
//                 extract network pattern
//                 |> List.map (fun res ->
//                     res
//                     |> Map.toSeq
//                     |> Seq.map (fun (k, v) ->
//                         (k.Name,
//                         match v with
//                         | Value.Int value -> WanderValue.Int value
//                         | Value.Bytes value -> WanderValue.Bytes value
//                         | Value.Identifier value -> WanderValue.Identifier value
//                         | Value.String value -> WanderValue.String value
//                         | Value.Slot value -> WanderValue.Slot value))
//                     |> Map.ofSeq
//                     |> WanderValue.Namespace)
//                 |> Array.ofList
//                 |> WanderValue.Array
//                 |> Ok
//                 |> failwith "TODO -- this is just copied from extract currently"
//             | value -> error $"Unexpected value passed to Pattern.extract - {value}." None) }

let ligatureLib =
    [ countFunction
      educeFunction
      applyFunction
      unionFunction
      minusFunction
      //matchFunction
      //queryFunction
      //inferFunction
      ]
