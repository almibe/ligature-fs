import { printResult as _printResult } from "../../Ligature.fs.js"
import { newEngine as _newEngine } from "../../Ligature.fs.js"

export let printResult = (input: string): any => {
    return _printResult(input)
}

export let newEngine = () => {
    return _newEngine()
}
