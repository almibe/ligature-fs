import { readScriptResult } from "../../npmProject/Ligature.js";

let res = readScriptResult("test");
console.log(res);
document.querySelector<HTMLDivElement>('#app')!.innerHTML = `<div>${res}</div>`;
