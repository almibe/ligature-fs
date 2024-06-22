import { run as _run } from "../../Ligature.fs.js"
import { printResult as _printResult } from "../../Ligature.fs.js"

export let run = (input) => JSON.parse(JSON.stringify(_run(input)))

export let printResult = (result) => _printResult(_run(result))
