import { printNetwork as _printNetwork } from "../../Ligature.fs.js"
import { newEngine as _newEngine } from "../../Ligature.fs.js"

export let printNetwork = (input: any): any => {
    return _printNetwork(input)
}

export let newEngine = () => {
    return _newEngine()
}
