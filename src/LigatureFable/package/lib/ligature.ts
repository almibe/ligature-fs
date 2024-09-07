import { newInMemoryEngine } from "../../output/LigatureFable.js"
import { Set, List, Record } from "immutable"

export interface Slot {
    slot: string | null
}

export const Slot = Record<{slot: string | null}>({
    slot: null
})

export interface Symbol {
    symbol: string
}

export const Symbol = Record({
    symbol: ""
})

export type Value = Slot | Symbol | Quote | Expression

export type Quote = List<Value>

export type Expression = List<Value>

export type Statement = {
    entity: Slot | Symbol,
    attribute: Slot | Symbol,
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
