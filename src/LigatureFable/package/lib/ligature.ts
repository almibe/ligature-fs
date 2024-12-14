import { readValue as read, runScript } from "../../output/LigatureFable.js"
import { displayText as dt } from "../../output/Display.js"

export function run(script: string): Map<string, any> {
    return runScript(script)
}

export function readValue(input: string): any {
    return read(input)
}

export function displayText(script: string, elementId: string) {
    dt(script, elementId)
}
