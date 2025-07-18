// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Ulid

open Wander.Model
open Ligature.Model
open System

//Source https://github.com/lucasschejtman/FSharp.Ulid
module internal Extensions =
    type DateTime with

        static member UnixTime =
            Convert.ToInt64(DateTime.UtcNow.Subtract(DateTime(1970, 1, 1)).TotalSeconds)

    type Random with

        member this.GetSequence(min, max) =
            Seq.initInfinite (fun _ -> this.Next(min, max))

module internal Helpers =
    let (|Positive|NotPositive|) num =
        if num > 0 then Positive else NotPositive

[<AutoOpen>]
module Ulid =
    open Extensions
    open Helpers

    /// Universally Unique Lexicographically Sortable Identifier
    type Ulid private (timestamp) as this =
        let encoding = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
        let encodingLength = 32L
        let mutable value = ""

        do
            let timePart = this.EncodeTime timestamp 10
            let randomPart = this.Randoms 16 0 31
            this.Value <- List.append timePart randomPart |> this.ConcatEncoding
            ()

        member this.Value
            with get () = value
            and private set (v) = value <- v

        override this.ToString() = this.Value

        member private this.ConcatEncoding chars =
            List.fold (fun acc char -> acc + (encoding.Chars char).ToString()) "" chars

        member private this.Randoms length min max =
            Random().GetSequence(min, max) |> Seq.take length |> Seq.toList

        member private this.EncodeTime timestamp length =
            let rec loop ts len chars =
                match len with
                | Positive ->
                    let char = ts % encodingLength
                    let acc = (ts - char) / encodingLength
                    loop acc (len - 1) ((Convert.ToInt32 char) :: chars)
                | NotPositive -> chars

            loop timestamp length []

        /// Creates a new `Ulid` from the specified `int64` timestamp
        static member FromTimestamp timestamp = timestamp |> Ulid

        /// Creates a new `Ulid` from the current Unix timestamp
        static member New = DateTime.UnixTime |> Ulid


let ulidFn =
    Fn.Fn(
        { doc = "Generate a ULID with an optional prefix."
          examples = [ "ulid()"; "ulid(prefix)" ]
          args = "Term?"
          result = "Term" },
        fun _ _ application ->
            match application.arguments with
            | [] -> Ok(Expression.Term(Term(Ulid.New.ToString())))
            | [ Expression.Term(Term prefix) ] -> Ok(Expression.Term(Term(prefix + Ulid.New.ToString())))
            | _ -> failwith "TODO"
    )
