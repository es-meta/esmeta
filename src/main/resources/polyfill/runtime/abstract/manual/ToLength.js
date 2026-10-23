var AO__ToIntegerOrInfinity = require("./ToIntegerOrInfinity");

// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-tolength
function AO__ToLength(argument) {
  "use strict";

  // 1. Let len be ? ToIntegerOrInfinity(argument).
  var len = AO__ToIntegerOrInfinity(argument);

  // 2. If len ≤ 0, return +0𝔽.
  if (len <= 0) return 0;
  // 3. Return 𝔽(Math.min(len, 2**53 - 1)).
  else return Math.min(len, Math.pow(2, 53) - 1);
}

module.exports = AO__ToLength;
