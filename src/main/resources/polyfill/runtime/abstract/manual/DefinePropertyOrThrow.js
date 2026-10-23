// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-deletepropertyorthrow
function AO__DeletePropertyOrThrow(O, P, desc) {
  "use strict";

  // 1. Let success be ? O.[[Delete]](P).
  var success = Object.defineProperty(O, P, desc);
  // 2. If success is false, throw a TypeError exception.
  if (!success) throw new TypeError();
  // 3. Return unused.
}

module.exports = AO__DeletePropertyOrThrow;
