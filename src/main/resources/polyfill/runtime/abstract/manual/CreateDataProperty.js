// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-createdataproperty
function AO__CreateDataProperty(O, P, V) {
  "use strict";

  Object.defineProperty(O, P, {
    value: V,
    writable: true,
    enumerable: true,
    configurable: true
  });

  return true;
}

module.exports = AO__CreateDataProperty;
