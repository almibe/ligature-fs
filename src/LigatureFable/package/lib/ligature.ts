import { newInMemoryEngine } from "../../output/LigatureFable.js"
import { Set, Record } from "immutable"

export interface Symbol {
    symbol: string
}

export const Symbol = Record({
    symbol: ""
})

export type Extension = {
    type: "extension",
    element: Symbol,
    concept: Symbol
}

export const Extension = Record({
    element: Symbol({symbol: ""}),
    concept: Symbol({symbol: ""}),
})

export type NonExtension = {
    type: "nonextension",
    element: Symbol,
    concept: Symbol
}

export const NonExtension = Record({
    element: Symbol({symbol: ""}),
    concept: Symbol({symbol: ""}),
})

export type Role = {
    type: "role",
    first: Symbol,
    second: Symbol,
    role: Symbol
}

export const Role = Record({
    first: Symbol({symbol: ""}),
    second: Symbol({symbol: ""}),
    role: Symbol({symbol: ""}),
})

export type Network = Set<Extension | NonExtension | Role>

export function newEngine() {
    return newInMemoryEngine()
}
