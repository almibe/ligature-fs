// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.Quote

// open Ligature.Wander.Model
// open Ligature.Main
// open FsToolkit.ErrorHandling
// open Ligature.Wander.Interpreter

// let distinctFunction =
//     { Name = "distinct"
//       Returns = WanderType.Array
//       Eval =
//         (fun args bindings ->
//             match args with
//             | [ WanderValue.Array(array) ] -> Ok(WanderValue.Array(Array.distinct array))
//             | _ -> error "Improper call to map function." None) }

// let catFunction =
//     { Name = "cat"
//       Returns = WanderType.Array
//       Eval =
//         (fun args bindings ->
//             match args with
//             | [ WanderValue.Array(array) ] ->
//                 let res =
//                     Array.fold
//                         (fun state value ->
//                             match value with
//                             | WanderValue.String(value) -> state + value
//                             | _ -> failwith "TODO")
//                         ""
//                         array

//                 Ok(WanderValue.String(res))
//             | _ -> error "Improper call to map function." None) }

// let insertFirstFunction =
//     { Name = "insert"
//       Returns = WanderType.Array
//       Eval =
//         (fun args bindings ->
//             match args with
//             | [ value; WanderValue.Array(array) ] -> Ok(WanderValue.Array(Array.insertAt 0 value array))
//             | _ -> error "Improper call to Array.insertFirst function." None) }

// let insertLastFunction =
//     { Name = "insertLast"
//       Returns = WanderType.Array
//       Eval =
//         (fun args bindings ->
//             match args with
//             | [ value; WanderValue.Array(array) ] -> Ok(WanderValue.Array(Array.append [| value |] array))
//             | _ -> error "Improper call to Array.insertLast function." None) }

// // let mapFunction =
// //             HostFunction(
// //                 (fun args bindings ->
// //                     match args with
// //                     | [ WanderValue.Function(fn); WanderValue.Array(array) ] ->
// //                         Array.map (fun value -> callFunction fn [ value ] bindings) array
// //                         |> List.ofArray
// //                         |> List.traverseResultM (fun x -> x)
// //                         |> Result.map (fun x -> WanderValue.Array(Array.ofList x))
// //                     | _ -> error "Improper call to map function." None)
// //             )

// // let rmatchFunction =
// //             HostFunction(
// //                 (fun args bindings ->
// //                     match args with
// //                     | [ WanderValue.Function(fn); WanderValue.Array(array) ] ->
// //                         Ok(
// //                             Array.rmatch
// //                                 (fun lValue rValue ->
// //                                     match callFunction fn [ lValue; rValue ] bindings with
// //                                     | Ok(res) -> res
// //                                     | Error(err) -> failwith (err.ToString()))
// //                                 array
// //                         )
// //                     | _ -> error "Improper call to Array.map function." None)
// //             )

// // let foldFunction =
// //             HostFunction(
// //                 (fun args bindings ->
// //                     match args with
// //                     | [ WanderValue.Function(fn); initial; WanderValue.Array(array) ] ->
// //                         Ok(
// //                             Array.fold
// //                                 (fun state value ->
// //                                     match callFunction fn [ state; value ] bindings with
// //                                     | Ok(res) -> res
// //                                     | Error(err) -> failwith (err.ToString()))
// //                                 initial
// //                                 array
// //                         )
// //                     | _ -> error "Improper call to Array.fold function." None)
// //             )

// let lengthFunction =
//     { Name = "length"
//       Returns = WanderType.Int
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ WanderValue.Array(array) ] -> Ok(WanderValue.Int(Array.length array |> bigint))
//             | _ -> error "Improper call to map function." None) }

let arrayLib = []
//     [ catFunction
//       //              ("map", mapFunction)
//       //              ("rmatch", rmatchFunction)
//       //              ("fold", foldFunction)
//       insertFirstFunction
//       insertLastFunction
//       distinctFunction
//       lengthFunction ]
