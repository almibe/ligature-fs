import { HostFunction, WanderType, WanderValue } from "../../../Ligature/wander/Model.fs.js"
import { singleton, toArray } from "../../fable_modules/fable-library-js.4.19.2/List.js"
import { FSharpResult$2 } from "../../fable_modules/fable-library-js.4.19.2/Result.js"
import { run as _run } from "../../Ligature.fs.js"
import { printResult as _printResult } from "../../Ligature.fs.js"

export type Identifier = { "identifier": string }

export type Slot = { "slot": string }

export type Value = bigint | Identifier | string | Uint8Array | Slot

export type Triple = [Identifier | Slot, Identifier | Slot, Value]

export type Error = { "error": string }

export type WanderFunction = {
    module: string,
    name: string,
    description: string,
    eval: (args: (Triple[] | Value)[]) => Triple[] | Value | Error
}

// export type FunctionDetails = {
//     module: string,
//     name: string,
//     description: string,
// }

// export const bindingsToFunctionDetails = (bindings): FunctionDetails[] => 
//     toArray(bindings.Functions)
//     .map((f: any) => { return {
//         module: f.Module,
//         name: f.Name,
//         description: f.Description
//     }})

function wanderValueToJSValue(value): Value | Triple[] {
    if (value.tag == 0) {
        return BigInt(value.fields[0])
    } else if (value.tag == 1) {
        return value.fields[0]
    } else if (value.tag == 7) {
        return [] //TODO
    }
    throw "TODO"
}

function jsValueToWanderValue(value: Value): WanderValue {
    if (typeof value == "bigint") {
        return new WanderValue(0, [value])
    } else if (typeof value == "string") {
        return new WanderValue(1, [value])
    }
    throw "TODO"
}

// export const bindFunction = (fn: WanderFunction, bindings) => {
//     const hostFn = new HostFunction(
//         fn.module,
//         fn.name, 
//         singleton([]), //TODO
//         new WanderType(1, []), //TODO
//         fn.description, 
//         (args, _arg) => {            
//             const mappedArgs = toArray(args).map((arg) => wanderValueToJSValue(arg))
//             const res = fn.eval(mappedArgs)
//             return new FSharpResult$2(0, [jsValueToWanderValue(res)])
//         }
//     )

//     return _bindFunction(hostFn, bindings)
// }

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

export let run = (input: string, identifiers, stack): Triple[] | Value | Error => {
    const res = JSON.parse(JSON.stringify(_run(input, identifiers, stack)))
    if (res[0] == 'Ok') {
        const resValue = res[1]
        const type = resValue[0]
        const value = resValue[1]
        return processValue(type, value)
    } else {
        return { "error": "Error" }
    }
}

export let printResult = (result: string, identifiers, stack): string => _printResult(_run(result, identifiers, stack))
