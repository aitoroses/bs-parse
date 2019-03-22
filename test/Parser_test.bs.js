// Generated by BUCKLESCRIPT VERSION 5.0.0, PLEASE EDIT WITH CARE

import * as Jest from "../node_modules/@glennsl/bs-jest/src/jest.js";
import * as Block from "../node_modules/bs-platform/lib/es6/block.js";
import * as Curry from "../node_modules/bs-platform/lib/es6/curry.js";
import * as ParseError$BsParse from "../src/ParseError.bs.js";
import * as Combinators$BsParse from "../src/Combinators.bs.js";

Jest.describe("Parsers", (function (param) {
        Jest.describe("errors", (function (param) {
                return Jest.test("should be nice", (function (param) {
                              var error = Combinators$BsParse.get_error(Combinators$BsParse.run(Combinators$BsParse.string("abx"), "abra"));
                              return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](ParseError$BsParse.getAllStackTrace(error)), /* array */["Expected: abx at line 1, column 1"]);
                            }));
              }));
        Jest.describe("string", (function (param) {
                Jest.test("success", (function (param) {
                        var result = Combinators$BsParse.run(Combinators$BsParse.string("abr"), "abra");
                        return Jest.Expect[/* toBe */2]("abr", Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(result)));
                      }));
                return Jest.test("failure", (function (param) {
                              return Jest.Expect[/* toThrow */18](Jest.Expect[/* expect */0]((function (param) {
                                                return Combinators$BsParse.get_exn(Combinators$BsParse.run(Combinators$BsParse.string("abx"), "abra"));
                                              })));
                            }));
              }));
        Jest.describe("orElse", (function (param) {
                var p = Combinators$BsParse.orElse(Combinators$BsParse.string("aa"), Block.__(246, [(function (param) {
                            return Combinators$BsParse.string("bb");
                          })]));
                Jest.test("success first branch", (function (param) {
                        var result = Combinators$BsParse.run(p, "aabra");
                        return Jest.Expect[/* toBe */2]("aa", Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(result)));
                      }));
                Jest.test("success second branch", (function (param) {
                        var result = Combinators$BsParse.run(p, "bbra");
                        return Jest.Expect[/* toBe */2]("bb", Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(result)));
                      }));
                return Jest.test("failure", (function (param) {
                              var result = Combinators$BsParse.run(p, "abbra");
                              return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](ParseError$BsParse.getAllStackTrace(Combinators$BsParse.get_error(result))), /* array */[
                                          "Expected: bb at line 1, column 1",
                                          "Expected: aa at line 1, column 1"
                                        ]);
                            }));
              }));
        Jest.describe("flatMap", (function (param) {
                var p = Combinators$BsParse.flatMap(Combinators$BsParse.string("aa"), (function (param) {
                        return Combinators$BsParse.string("bb");
                      }));
                Jest.test("success", (function (param) {
                        var result = Combinators$BsParse.run(p, "aabb");
                        return Jest.Expect[/* toBe */2]("bb", Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(result)));
                      }));
                return Jest.test("failure", (function (param) {
                              var result = Combinators$BsParse.run(p, "aaabb");
                              return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](ParseError$BsParse.getAllStackTrace(Combinators$BsParse.get_error(result))), /* array */["Expected: bb at line 1, column 3"]);
                            }));
              }));
        Jest.test("unit", (function (param) {
                var result = Combinators$BsParse.run(Combinators$BsParse.unit(1), "abra");
                return Curry._2(Jest.Expect[/* Operators */25][/* == */0], Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(result)), 1);
              }));
        Jest.describe("many", (function (param) {
                var p = Combinators$BsParse.many(Combinators$BsParse.string("aa"));
                Jest.test("aaaa", (function (param) {
                        return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(Combinators$BsParse.run(p, "aaaa"))), /* array */[
                                    "aa",
                                    "aa"
                                  ]);
                      }));
                Jest.test("aa", (function (param) {
                        return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(Combinators$BsParse.run(p, "aa"))), /* array */["aa"]);
                      }));
                return Jest.test("no input", (function (param) {
                              return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(Combinators$BsParse.run(p, ""))), /* array */[]);
                            }));
              }));
        Jest.describe("many1", (function (param) {
                var p = Combinators$BsParse.many1(Combinators$BsParse.string("a"));
                Jest.test("success", (function (param) {
                        var result = Combinators$BsParse.run(p, "aabb");
                        return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(result)), /* array */[
                                    "a",
                                    "a"
                                  ]);
                      }));
                return Jest.test("failure", (function (param) {
                              var result = Combinators$BsParse.run(p, "bb");
                              return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](ParseError$BsParse.getAllStackTrace(Combinators$BsParse.get_error(result))), /* array */["Expected at least one repetition for parser at line 1, column 1"]);
                            }));
              }));
        Jest.test("slice", (function (param) {
                var p = Combinators$BsParse.slice(Combinators$BsParse.many1(Combinators$BsParse.string("a")));
                var result = Combinators$BsParse.run(p, "aaabb");
                return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(result)), "aaa");
              }));
        Jest.describe("regex", (function (param) {
                Jest.test("simple", (function (param) {
                        var p = Combinators$BsParse.regex("a(b)c?ra?");
                        var result = Combinators$BsParse.run(p, "abrcaaaaa");
                        return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(result)), /* array */[
                                    "abr",
                                    "b"
                                  ]);
                      }));
                return Jest.test("continuation", (function (param) {
                              var c = Block.__(246, [(function (param) {
                                      return Combinators$BsParse.string("c");
                                    })]);
                              var p = Curry._2(Combinators$BsParse.$great$great, Combinators$BsParse.regex("a(b)c?ra?"), c);
                              var result = Combinators$BsParse.run(p, "abrcaaaaa");
                              return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(result)), /* tuple */[
                                          /* array */[
                                            "abr",
                                            "b"
                                          ],
                                          "c"
                                        ]);
                            }));
              }));
        return Jest.describe("sepBy", (function (param) {
                      Jest.test("no repetition", (function (param) {
                              var result = Combinators$BsParse.run(Combinators$BsParse.sepBy(Combinators$BsParse.string(","), Combinators$BsParse.string("a")), "");
                              return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(result)), /* array */[]);
                            }));
                      Jest.test("one repetition", (function (param) {
                              var result = Combinators$BsParse.run(Combinators$BsParse.sepBy(Combinators$BsParse.string(","), Combinators$BsParse.string("a")), "a");
                              return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(result)), /* array */["a"]);
                            }));
                      Jest.test("sepBy", (function (param) {
                              var result = Combinators$BsParse.run(Combinators$BsParse.sepBy(Combinators$BsParse.string(","), Combinators$BsParse.string("a")), "a,a,a");
                              return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(result)), /* array */[
                                          "a",
                                          "a",
                                          "a"
                                        ]);
                            }));
                      return Jest.test("sepBy with last sep", (function (param) {
                                    var result = Combinators$BsParse.run(Combinators$BsParse.sepBy(Combinators$BsParse.string(","), Combinators$BsParse.string("a")), "a,a,a,");
                                    return Curry._2(Jest.Expect[/* Operators */25][/* = */5], Jest.Expect[/* expect */0](Combinators$BsParse.get_exn(result)), /* array */[
                                                "a",
                                                "a",
                                                "a"
                                              ]);
                                  }));
                    }));
      }));

export {
  
}
/*  Not a pure module */
