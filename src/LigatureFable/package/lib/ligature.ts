import { runScript, runScriptResult } from "../../output/LigatureFable.js"
import { displayText as dt } from "../../output/Display.js"

export function run(script: string): string {
    return runScript(script)
}

export function runResult(script: string, commands): Array<Map<string, string>> {
    return runScriptResult(script, commands)
}

export function displayText(script: string, elementId: string) {
    dt(script, elementId)
}
