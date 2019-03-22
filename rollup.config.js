// rollup.config.js
import commonjs from 'rollup-plugin-commonjs'
import nodeResolve from 'rollup-plugin-node-resolve'
import { uglify } from "rollup-plugin-uglify"


export default {
  input: 'src/Scheme.bs.js',
  output: {
    file: 'dist/Scheme.js',
    format: 'cjs',
    exports: 'named'
  },
  plugins: [
    nodeResolve({
      jsnext: false,
      main: true
    }),

    commonjs({

      // if false then skip sourceMap generation for CommonJS modules
      sourceMap: false,  // Default: true

      // sometimes you have to leave require statements
      // unconverted. Pass an array containing the IDs
      // or a `id => boolean` function. Only use this
      // option if you know what you're doing!
      ignore: [ 'conditional-runtime-dependency' ]
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