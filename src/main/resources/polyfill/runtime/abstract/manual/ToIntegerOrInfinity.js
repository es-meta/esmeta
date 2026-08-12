// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-tointegerorinfinity
function AO__ToIntegerOrInfinity(argument) {
  "use strict";

  // 1. Let number be ? ToNumber(argument).
  var number = Number(argument);

  // 2. If number is one of NaN, +0𝔽, or -0𝔽, return 0.
  if (isNaN(number) || number === 0) {
    return 0;
  }

  // 3. If number is +∞𝔽, return +∞.
  // 4. If number is -∞𝔽, return -∞.
  if (!isFinite(number)) {
    return number;
  }

  // 5. Return truncate(ℝ(number)).
  return Math.trunc(number);
}

module.exports = AO__ToIntegerOrInfinity;
