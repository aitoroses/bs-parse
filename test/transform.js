const config = {
    babelrc: false,
    presets: [
        "@babel/preset-env"
    ]
  };
  module.exports = require("babel-jest").createTransformer(config);