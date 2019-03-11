// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Combinators$ReasonSuperTinyCompiler = require("./Combinators.bs.js");

var eof = Combinators$ReasonSuperTinyCompiler.label("Expected EOF", Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, Combinators$ReasonSuperTinyCompiler.slice(Combinators$ReasonSuperTinyCompiler.regex("$")), (function (param) {
            return /* () */0;
          })));

var whitespace = Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, Combinators$ReasonSuperTinyCompiler.slice(Combinators$ReasonSuperTinyCompiler.regex("[\\s]*")), (function (param) {
        return /* () */0;
      }));

function spaceAround(bodyP) {
  return Curry._2(Combinators$ReasonSuperTinyCompiler.$great$great$eq, whitespace, (function (param) {
                return Curry._2(Combinators$ReasonSuperTinyCompiler.$great$great$eq, bodyP, (function (result) {
                              return Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, whitespace, (function (param) {
                                            return result;
                                          }));
                            }));
              }));
}

function surround(openP, bodyP, closeP) {
  return Curry._2(Combinators$ReasonSuperTinyCompiler.$great$great$eq, openP, (function (param) {
                return Curry._2(Combinators$ReasonSuperTinyCompiler.$great$great$eq, spaceAround(bodyP), (function (result) {
                              return Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, closeP, (function (param) {
                                            return result;
                                          }));
                            }));
              }));
}

var number = Combinators$ReasonSuperTinyCompiler.slice(Combinators$ReasonSuperTinyCompiler.regex("-?\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?"));

var str = Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, Combinators$ReasonSuperTinyCompiler.regex("\"([^\"]*)\""), (function (matches) {
        return Caml_array.caml_array_get(matches, 1);
      }));

exports.eof = eof;
exports.whitespace = whitespace;
exports.spaceAround = spaceAround;
exports.surround = surround;
exports.number = number;
exports.str = str;
/* eof Not a pure module */