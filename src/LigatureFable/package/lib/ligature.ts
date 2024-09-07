import { newEngine as newInMemoryEngine } from "../../output/LigatureFable.js"
import { Set, List, Record } from "immutable"

export interface Int {
    int: BigInt
}

export const Int = Record({
    int: 0n
})

export interface String {
    string: string
}

export const String = Record({
    string: ""
})

export interface Bytes {
    bytes: Uint8Array
}

export const Bytes = Record({
    bytes: new Uint8Array()
})

export interface Slot {
    slot: string
}

export const Slot =  Record({
    slot: ""
})

export interface Name {
    name: string
}

export const Name = Record({
    name: ""
})

export type Value = Int | String | Bytes | Slot | Name | Quote | Expression

export type Quote = List<Value>

export type Expression = List<Value>

export type Statement = {
    entity: Slot | Name,
    attribute: Slot | Name,
    value: Value
}

export const Statement = Record({
    entity: Slot({slot: ""}),
    attribute: Slot({slot: ""}),
    value: Slot({slot: ""})
})

export type Network = Set<Statement>

export interface Engine {
    run: (string) => Value | Network
}

export function newEngine(): Engine {
    return (newInMemoryEngine() as Engine)
}
