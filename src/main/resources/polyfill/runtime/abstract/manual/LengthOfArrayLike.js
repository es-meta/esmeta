var AO__ToLength = require("./ToLength");
var AO__Get = require("./Get");

// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-lengthofarraylike
function AO__LengthOfArrayLike(obj) {
  "use strict";

  // 1. Return ℝ(? ToLength(? Get(obj, "length"))).
  return AO__ToLength(AO__Get(obj, "length"));
}

module.exports = AO__LengthOfArrayLike;
