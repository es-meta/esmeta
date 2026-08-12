// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-set-o-p-v-throw
function AO__Set(O, P, V, Throw) {
  "use strict";

  // 1. Let success be ? O.[[Set]](P, V, O).
  try {
    O[P] = V;
  } catch (error) {
    // 2. If success is false and Throw is true, throw a TypeError exception.
    if (Throw) throw error;
  }
  // 3. Return unused.
}

module.exports = AO__Set;
