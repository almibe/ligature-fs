import { newInMemoryEngine, runScript } from "../../output/LigatureFable.js"
import { Set, Record } from "immutable"
import Graph from "graphology"

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

export type Entry = Extension | NonExtension | Role

export type Network = Set<Entry>

export function run(script: string): Map<string, Entry[]> {
    return runScript(script)
}

export function newEngine() {
    return newInMemoryEngine()
}

export function toGraph(network: Entry[]): Graph {
    const graph = new Graph({ allowSelfLoops: true, multi: true, type: "directed" });
    network.forEach(entry => {
        if (entry.type == "extension") {
            graph.mergeNode(entry.element.symbol)
            graph.mergeNode(entry.concept.symbol)
            graph.addEdge(entry.element.symbol, entry.concept.symbol, { type: 'extension' })
        } else if (entry.type == "nonextension") {
            graph.mergeNode(entry.element.symbol)
            graph.mergeNode(entry.concept.symbol)
            graph.addEdge(entry.element.symbol, entry.concept.symbol, { type: 'nonextension' })
        } else {
            graph.mergeNode(entry.first.symbol)
            graph.mergeNode(entry.second.symbol)
            graph.addEdge(entry.first.symbol, entry.second.symbol, { type: 'role', roleName: entry.role.symbol })
        }
    })
    return graph
}
