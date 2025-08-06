import { defineConfig } from 'vite'
import path from "path"
import dts from 'vite-plugin-dts'


// https://vitejs.dev/config/
export default defineConfig({
    clearScreen: false,
    server: {
        watch: {
            ignored: [
                "**/*.fs" // Don't watch F# files
            ]
        }
    },
    build: {
        lib: {
            entry: path.resolve(__dirname, 'ligature.ts'),
            name: 'ligature',
            fileName: (format) => `ligature.${format}.js`
        }
    },
    plugins: [dts({ bundleTypes: true })],
})