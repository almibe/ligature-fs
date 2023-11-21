import { readScriptResult } from "../../npmProject/Ligature.js";

let res = readScriptResult("test");
document.querySelector<HTMLDivElement>('#app')!.innerHTML = `<div>${res}</div>`;
