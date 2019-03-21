// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");

function $less$less(f, g, x) {
  return Curry._1(f, Curry._1(g, x));
}

function explode(str) {
  var _i = str.length - 1 | 0;
  var _list = /* [] */0;
  while(true) {
    var list = _list;
    var i = _i;
    if (i < 0) {
      return list;
    } else {
      _list = /* :: */[
        Caml_string.get(str, i),
        list
      ];
      _i = i - 1 | 0;
      continue ;
    }
  };
}

function char_to_string(param) {
  return $$String.make(1, param);
}

function char_list_to_string(list) {
  return List.fold_left((function (prim, prim$1) {
                return prim + prim$1;
              }), "", List.map(char_to_string, list));
}

function take(n, list) {
  if (list) {
    var match = n === 0;
    if (match) {
      return /* [] */0;
    } else {
      return /* :: */[
              list[0],
              take(n - 1 | 0, list[1])
            ];
    }
  } else {
    return /* [] */0;
  }
}

exports.$less$less = $less$less;
exports.explode = explode;
exports.char_to_string = char_to_string;
exports.char_list_to_string = char_list_to_string;
exports.take = take;
/* No side effect */
