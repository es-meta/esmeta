// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-tostring
function AO__ToString(argument) {
  "use strict";

  if (typeof argument === "symbol") throw new TypeError();

  return String(argument);
}

module.exports = AO__ToString;
