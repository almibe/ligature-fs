<script>
import '$lib/Files.svelte'
import '@shoelace-style/shoelace/dist/themes/light.css';
import '@shoelace-style/shoelace/dist/components/button/button.js';
import '@shoelace-style/shoelace/dist/components/icon/icon.js';
import '@shoelace-style/shoelace/dist/components/input/input.js';
import '@shoelace-style/shoelace/dist/components/rating/rating.js';
import '@shoelace-style/shoelace/dist/components/split-panel/split-panel.js'
//import { setBasePath } from '@shoelace-style/shoelace/dist/utilities/base-path.js';

import { showEditor } from '@ligature/ligature-components'

import '../style.css'
import Files from '$lib/Files.svelte';



// Set the base path to the folder you copied Shoelace's assets to
//setBasePath('/path/to/shoelace/dist');

setTimeout(() => {
    showEditor(document.querySelector("#editor"), "(docs)")
})

// <sl-button>, <sl-icon>, <sl-input>, and <sl-rating> are ready to use!
</script>

<sl-split-panel id="main-split">
    <div
        slot="start"
        style="height: 100%; ">
        <Files></Files>
    </div>
    <div
        slot="end"
        style="height: 100%; ">
        <div id="editor"></div>
        <div id="results"></div>
    </div>
</sl-split-panel>
