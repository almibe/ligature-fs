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

export type Identifier = Slot | Symbol

export type Quote = List<Identifier>

export type Expression = List<Identifier>

export type Statement = {
    entity: Identifier,
    attribute: Identifier,
    value: Identifier
}

export const Statement = Record({
    entity: Slot({slot: ""}),
    attribute: Slot({slot: ""}),
    value: Slot({slot: ""})
})

export type Network = Set<Statement>

export interface Engine {
    eval: (quote: List<Identifier>) => (Identifier | Network | null)
}

export function newEngine(): Engine {
    //return (newInMemoryEngine() as Engine)
    return {
        eval: (quote) => {return null;}
    }
}
