// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Combinators$ReasonSuperTinyCompiler = require("./Combinators.bs.js");

var whitespace = Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, Combinators$ReasonSuperTinyCompiler.slice(Combinators$ReasonSuperTinyCompiler.regex("[\\s]*")), (function (param) {
        return /* () */0;
      }));

var $$undefined = Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, Combinators$ReasonSuperTinyCompiler.string("undefined"), (function (param) {
        return /* JUndefined */0;
      }));

var $$null = Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, Combinators$ReasonSuperTinyCompiler.string("null"), (function (param) {
        return /* JNull */1;
      }));

var trueBool = Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, Combinators$ReasonSuperTinyCompiler.string("true"), (function (param) {
        return /* JBool */Block.__(2, [true]);
      }));

var falseBool = Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, Combinators$ReasonSuperTinyCompiler.string("false"), (function (param) {
        return /* JBool */Block.__(2, [false]);
      }));

var bools = Combinators$ReasonSuperTinyCompiler.orElse(trueBool, falseBool);

var str = Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, Combinators$ReasonSuperTinyCompiler.regex("\"([^\"]*)\""), (function (matches) {
        return Caml_array.caml_array_get(matches, 1);
      }));

var quotedString = Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, str, (function (s) {
        return /* JString */Block.__(1, [s]);
      }));

var number = Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, Combinators$ReasonSuperTinyCompiler.slice(Combinators$ReasonSuperTinyCompiler.regex("[-+]?[0-9]*\\.?[0-9]+")), (function (numberStr) {
        return /* JNumber */Block.__(0, [Caml_format.caml_float_of_string(numberStr)]);
      }));

var literal = Curry._2(Combinators$ReasonSuperTinyCompiler.$less$pipe$great, Curry._2(Combinators$ReasonSuperTinyCompiler.$less$pipe$great, Curry._2(Combinators$ReasonSuperTinyCompiler.$less$pipe$great, Curry._2(Combinators$ReasonSuperTinyCompiler.$less$pipe$great, $$undefined, $$null), bools), quotedString), number);

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

var objectMember = Curry._2(Combinators$ReasonSuperTinyCompiler.$great$great$eq, Combinators$ReasonSuperTinyCompiler.regex("\"([^\"]*)\"\\s*:\\s*"), (function (captured) {
        return Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, literal, (function (lit) {
                      return /* tuple */[
                              Caml_array.caml_array_get(captured, 1),
                              lit
                            ];
                    }));
      }));

var obj = Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, surround(Combinators$ReasonSuperTinyCompiler.string("{"), Combinators$ReasonSuperTinyCompiler.sepBy(",", spaceAround(objectMember)), Combinators$ReasonSuperTinyCompiler.string("}")), (function (res) {
        return /* JObject */Block.__(4, [res]);
      }));

var array = Curry._2(Combinators$ReasonSuperTinyCompiler.$less$$great, surround(Combinators$ReasonSuperTinyCompiler.string("["), Combinators$ReasonSuperTinyCompiler.sepBy(",", spaceAround(literal)), Combinators$ReasonSuperTinyCompiler.string("]")), (function (res) {
        return /* JArray */Block.__(3, [res]);
      }));

var expr = Curry._2(Combinators$ReasonSuperTinyCompiler.$less$pipe$great, Curry._2(Combinators$ReasonSuperTinyCompiler.$less$pipe$great, literal, obj), array);

var Parser = /* module */[
  /* whitespace */whitespace,
  /* undefined */$$undefined,
  /* null */$$null,
  /* trueBool */trueBool,
  /* falseBool */falseBool,
  /* bools */bools,
  /* str */str,
  /* quotedString */quotedString,
  /* number */number,
  /* literal */literal,
  /* spaceAround */spaceAround,
  /* surround */surround,
  /* objectMember */objectMember,
  /* obj */obj,
  /* array */array,
  /* expr */expr
];

exports.Parser = Parser;
/* whitespace Not a pure module */
