// rollup.config.js
import commonjs from 'rollup-plugin-commonjs'
import nodeResolve from 'rollup-plugin-node-resolve'
import { uglify } from "rollup-plugin-uglify"


export default {
  input: 'src/Main.bs.js',
  output: {
    file: 'dist/build.js',
    format: 'cjs',
    exports: 'named'
  },
  plugins: [
    nodeResolve({
      jsnext: false,
      main: true
    }),
    commonjs({
      sourceMap: false,
    }),
    uglify({
      compress: true,
      mangle: {
        toplevel: true,
        //properties: true
      }
    })
  ]
};