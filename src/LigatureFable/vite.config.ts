import { defineConfig } from 'vite'
import path from "path"

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
            entry: path.resolve(__dirname, 'Library.fs.js'),
            name: 'ligature',
            fileName: (format) => `ligature.${format}.js`
        }
    }
})