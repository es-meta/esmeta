// 10.4 Built-in Exotic Object Internal Methods and Slots
// https://tc39.es/ecma262/multipage/ordinary-and-exotic-objects-behaviours.html#sec-arraycreate
function AO__ArrayCreate(length) {
  "use strict";

  return Array(length);
}

module.exports = AO__ArrayCreate;
