import { HostFunction, WanderType, WanderValue } from "../../../Ligature/wander/Model.fs.js"
import { singleton, toArray } from "../../fable_modules/fable-library-js.4.19.2/List.js"
import { FSharpResult$2 } from "../../fable_modules/fable-library-js.4.19.2/Result.js"
import { run as _run } from "../../Ligature.fs.js"
import { printResult as _printResult } from "../../Ligature.fs.js"

export let run = (input: string): any => {
    // const res = JSON.parse(JSON.stringify(_run(input, identifiers, stack)))
    // if (res[0] == 'Ok') {
    //     const resValue = res[1]
    //     const type = resValue[0]
    //     const value = resValue[1]
    //     return processValue(type, value)
    // } else {
    //     return { "error": "Error" }
    // }
    throw "TODO"
}
