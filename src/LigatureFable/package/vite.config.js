import { resolve } from 'path'
import { defineConfig } from 'vite'
import dts from 'vite-plugin-dts'

export default defineConfig({
  build: {
    lib: {
      entry: resolve(__dirname, 'lib/ligature.ts'),
      name: 'Ligature',
      fileName: 'ligature', // the proper extensions will be added
    },
    rollupOptions: {
    },
  },
  plugins: [dts()]
})
