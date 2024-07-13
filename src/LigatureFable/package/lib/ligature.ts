import { run as _run, coreBindings as _coreBindings } from "../../Ligature.fs.js"
import { printResult as _printResult } from "../../Ligature.fs.js"

export type Identifier = { "identifier": string }

export type Slot = { "slot": string }

export type Value = bigint | Identifier | string | Uint8Array | Slot

export type Triple = [Identifier | Slot, Identifier | Slot, Value]

export type Error = { "error": string }

export const coreBindings = _coreBindings

const processValue = (type: string, value: any): any => {
    if (type == "Network") {
        const triples = value.network
        return triples.map((triple) => {
            const entity = processValue(triple.Entity[1][0], triple.Entity[1])
            const attribute = processValue(triple.Attribute[1][0], triple.Attribute[1])
            const value = processValue(triple.Value[0], triple.Value[1])
            return [entity, attribute, value]
        })
    } else if (type == "Int") {
        return BigInt(value)
    } else if (type == "String") {
        return value
    } else if (type == "Identifier") {
        return {"identifier": value[1]}
    } else if (type == "Slot") {
        return {"slot": value[1]}
    } else {
        throw `Error: could not process ${type} -- ${value}`
    }
}

export let run = (input: string, bindings: any = coreBindings): Triple[] | Value | Error => {
    const res = JSON.parse(JSON.stringify(_run(input, bindings)))
    if (res[0] == 'Ok') {
        const resValue = res[1]
        const type = resValue[0]
        const value = resValue[1]
        return processValue(type, value)
    } else {
        return { "error": "Error" }
    }
}

export let printResult = (result: string): string => _printResult(_run(result))
