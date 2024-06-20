import { resolve } from 'path'
import { defineConfig } from 'vite'

export default defineConfig({
  build: {
    lib: {
      entry: resolve(__dirname, 'lib/main.js'),
      name: 'Ligature',
      fileName: 'ligature', // the proper extensions will be added
    },
    rollupOptions: {
    },
  },
})
