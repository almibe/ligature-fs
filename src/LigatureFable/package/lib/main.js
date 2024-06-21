import { run as _run } from "../../Ligature.fs.js"

export let run = (input) => JSON.parse(JSON.stringify(_run(input)))
