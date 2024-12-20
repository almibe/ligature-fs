import { runScript } from "../../output/LigatureFable.js"

export type NoResult = {}
export type Error = {type: "error", value: string}
export type Literal = {type: "literal", value: string}
export type Element = {type: "element", value: string}
export type Variable = {type: "variable", value: string}
export type Quote = {type: "quote", value: Array<Literal | Element | Variable | Quote | Network>}
export type Triple = [Element | Variable, Element | Variable, Element | Variable | Literal]
export type Network = {type: "network", value: Array<Triple>}
export type Result = NoResult | Error | Literal | Element | Variable | Quote | Network

export function run(script: string, commands, resultsEl: HTMLElement): Result {
    return runScript(script, commands, resultsEl)
}
