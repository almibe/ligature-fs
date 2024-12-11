import { runScript, readValue as read } from "../../output/LigatureFable.js"

export function run(script: string): Map<string, any> {
    return runScript(script)
}

export function readValue(input: string): any {
    return read(input)
}