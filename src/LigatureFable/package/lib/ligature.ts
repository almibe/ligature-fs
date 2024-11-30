import { runScript } from "../../output/LigatureFable.js"

export function run(script: string): Map<string, any> {
    return runScript(script)
}
