import { newInMemoryEngine, runScript } from "../../output/LigatureFable.js"
import Graph from "graphology"

export type Symbol = string

export type Extension = {
    type: "extension",
    element: Symbol,
    concept: Symbol
}

export type NonExtension = {
    type: "nonextension",
    element: Symbol,
    concept: Symbol
}

export type Role = {
    type: "role",
    first: Symbol,
    second: Symbol,
    role: Symbol
}

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
            graph.mergeNode(entry.element)
            graph.mergeNode(entry.concept)
            graph.addEdge(entry.element, entry.concept, { type: 'extension' })
        } else if (entry.type == "nonextension") {
            graph.mergeNode(entry.element)
            graph.mergeNode(entry.concept)
            graph.addEdge(entry.element, entry.concept, { type: 'nonextension' })
        } else {
            graph.mergeNode(entry.first)
            graph.mergeNode(entry.second)
            graph.addEdge(entry.first, entry.second, { type: 'role', roleName: entry.role })
        }
    })
    return graph
}
