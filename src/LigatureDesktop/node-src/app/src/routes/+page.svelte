<script>
import '@shoelace-style/shoelace/dist/themes/light.css';
import '@shoelace-style/shoelace/dist/components/button/button.js';
import '@shoelace-style/shoelace/dist/components/button-group/button-group.js';
import '@shoelace-style/shoelace/dist/components/icon/icon.js';
import '@shoelace-style/shoelace/dist/components/input/input.js';
import '@shoelace-style/shoelace/dist/components/rating/rating.js';
import '@shoelace-style/shoelace/dist/components/split-panel/split-panel.js'
//import { setBasePath } from '@shoelace-style/shoelace/dist/utilities/base-path.js';

import { runScript, showEditor } from '@ligature/ligature-components'

import '../style.css'
import Files from '$lib/Files.svelte';



// Set the base path to the folder you copied Shoelace's assets to
//setBasePath('/path/to/shoelace/dist');

setTimeout(() => {
    let editor = showEditor(document.querySelector("#editor"), "(docs)")

    document.querySelector("#runButton")?.addEventListener("click", () => {
        document.querySelector("#results").innerHTML = ""
        runScript(editor.getValue(), document.querySelector("#results"))
    })
})

// <sl-button>, <sl-icon>, <sl-input>, and <sl-rating> are ready to use!
</script>


<sl-split-panel vertical style="height: 100%;">
  <div
    slot="start"
    style="height: 100%"
  >

    <sl-button-group label="Alignment">
        <sl-tooltip content="Run">
            <sl-button id="runButton">▷</sl-button>
        </sl-tooltip>

        <sl-tooltip content="Command">
            <sl-button>⁁</sl-button>
        </sl-tooltip>
    </sl-button-group>

    <div id="editor"></div>
  </div>
  <div
    slot="end"
    style="height: 100%; overflow: scroll;"
  >
    <div id="results"></div>  
  </div>
</sl-split-panel>