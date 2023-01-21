// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Nibblers

// /// <summary>Create a Nibbler that take a single literal value.</summary>
// /// <param name="t">The literal to take.</param>
// /// <returns>A Nibbler that takes a single literal.</returns>
// let take t (input: list<'a>) =
//     if input[0] = t then
//         Ok((t, input[1..]))
//     else
//         Error($"Could not match {t}")

/// <summary>Create a Nibbler that take a single literal value.</summary>
/// <param name="t">The literal to take.</param>
/// <returns>A Nibbler that takes a single literal.</returns>
let take t gaze =
    if Gaze.next gaze = Some(t) then
        Some(t)
    else
        None

/// <summary>Create a Nibbler that takes a list of tokens.</summary>
/// <param name="list">The list of tokens to take.</param>
/// <returns>The newly created Nibbler.</returns>
let takeList list gaze =
    let length = List.length(list)
    let mutable index = 0
    let mutable cont = true
    while not (Gaze.isComplete gaze) && cont && index < length do
        let next = Gaze.next(gaze)
        match next with
        | Some value ->
            if value = list.Item(index) then
                index <- index + 1
            else
                cont <- false
        | None ->
            cont <- false
    if cont && index = length then
        Some list
    else
        None

/// <summary>Create a Nibbler that takes a String when working with a Gaze of Chars.
/// This is just a helper function that relies on takeList.</summary>
/// <param name="string">The String to match.</param>
/// <returns> The newly created Nibbler.</returns>
let takeString string =
    takeList(Array.toList(Gaze.explode(string)))

/// <summary>Create a Nibbler that accepts input based on a function that recieves the current token
/// and returns a bool.</summary>
/// <param name="predicate">The function used to decide if a token matches.</param>
/// <returns>A Nibbler that consumes one item as long as the predicate passes.</returns>
let takeCond predicate gaze =
    let next = Gaze.peek(gaze)
    match next with
    | Some(value) when predicate(value) ->
            Gaze.next(gaze) |> ignore
            Some(value)
    | _ -> None

/// <summary>Create a Nibbler that accepts input based on a function that recieves the current token
/// and returns a bool.</summary>
/// <param name="predicate">The function used to decide if a token matches.</param>
/// <returns>A Nibbler that consumes input as long as the predicate passes.</returns>
let takeWhile predicate gaze =
    let mutable cont = true
    let mutable results = []
    while cont do
        let next = Gaze.peek(gaze)
        match next with
        | Some value when predicate value ->
            Gaze.next(gaze) |> ignore
            results <- results @ [value]
        | _ ->
            cont <- false
    if List.length(results) = 0 then
        None
    else
        Some results

/// <summary>Create a Nibbler that accepts input based on a function that recieves the current token 
/// with index starting at 0 and returns a bool.</summary>
/// <param name="predicate">The function used to decide if a token matches.</param>
/// <returns>A Nibbler that consumes input as long as the predicate passes.</returns>
let takeWhileIndex predicate gaze =
    let mutable index = 0
    let mutable cont = true
    let mutable results = []
    while cont do
        let next = Gaze.peek(gaze)
        match next with
        | Some(value) when predicate(value, index) ->
            Gaze.next(gaze) |> ignore
            results <- results @ [value]
            index <- index + 1
        | _ -> cont <- false
    if List.length(results) = 0 then
        None
    else
        Some(results)

/// <summary>Create a Nibbler that consumes input until the given Nibbler succeeds.</summary>
/// <param name="nibbler">The Nibbler used to test.</param>
/// <returns>The newly created Nibbler.</returns>
let takeUntil nibbler gaze =
    let mutable results = []
    let mutable cont = true
    while cont && not (Gaze.isComplete gaze) do
        let res = Gaze.check nibbler gaze
        match res with
        | Some(_) ->
            cont <- false
        | None ->
                let next = Gaze.next(gaze)
                results <- results @ [next.Value] //???
    Some(results)

/// <summary>Create a Nibbler that accepts a start.</summary>
/// <param name="start">The starting token.</param>
/// <param name="content">The Nibbler used to decide the matched content.</param>
/// <param name="end">The ending token.</param>
/// <returns>A Nibbler that consumes a starting and ending token and returns the content that matches in between.</returns>
let between start content last  gaze =
    if Gaze.next(gaze) = Some(start) then
        match Gaze.attempt content gaze with
        | Some(result) ->
            if Gaze.next(gaze) = Some(last) then
                Some(result)
            else
                None
        | None -> None
    else
        None

/// <summary>Creates a Nibbler that wraps another Nibbler and will never fail but will instead return an empty List.</summary>
/// <param name="nibbler">The Nibbler to wrap.</param>
/// <returns>The newly created Nibbler.</returns>
let optional nibbler gaze =
    match Gaze.attempt nibbler gaze with
    | Some(res) -> Some(res)
    | None -> Some([])

let repeat nibbler gaze =
    let mutable cont = true
    let mutable results = []
    while cont do
        match Gaze.attempt nibbler gaze with
        | Some(result) ->
            results <- results @ [result]
        | None -> cont <- false
    if results = [] then
        None
    else
        Some(results)

/// <summary>Create a Nibbler that accepts a List of Nibblers and only succeeds if all of the
/// passed in Nibblers succeed in order.</summary>
/// <param name="nibblers">A List of nibblers.</param>
/// <returns>A List of all of the results from each Nibbler internally grouped in Lists.</returns>
let takeAll nibblers gaze =
    let mutable results = []
    let mutable nibblerIndex = 0
    while nibblerIndex >= 0 && nibblerIndex < List.length(nibblers) do
        let nibbler = nibblers.Item(nibblerIndex)
        match Gaze.attempt nibbler gaze with
        | Some(result) ->
            results <- results @ [result]
            nibblerIndex <- nibblerIndex + 1
        | None ->
            nibblerIndex <- -1
    if results = [] || nibblerIndex = -1 then
        None
    else
        Some(results)

/// <summary>Create a Nibbler that accepts a List of Nibblers and matches on the first that succeeds.
/// If all fail the created Nibbler will fail as well.</summary>
/// <param name="nibblers">A list of Nibblers to check.</param>
/// <returns>The newly created Nibbler.</returns>
let takeFirst nibblers gaze =
    let mutable result = None
    let mutable nibblerIndex = 0
    while nibblerIndex >= 0 && nibblerIndex < List.length(nibblers) do
        let nibbler = nibblers.Item(nibblerIndex)
        match Gaze.attempt nibbler gaze with
        | Some(res) ->
            result <- Some(res)
            nibblerIndex <- -1
        | None ->
            nibblerIndex <- nibblerIndex + 1
    result
