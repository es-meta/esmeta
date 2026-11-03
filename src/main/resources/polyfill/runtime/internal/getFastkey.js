var UID = "SET_ID_" + Math.random().toString(36).substring(2);
var ID_COUNTER = 0;

// Object ids live in the host's WeakMap when it has one, so that keys are
// never mutated and Proxy keys never see a trap. This module is loaded before
// the polyfill installs its own WeakMap, so `WeakMap` here is the host's.
var ids = typeof WeakMap === "function" ? new WeakMap() : null;
var wmGet = ids && WeakMap.prototype.get;
var wmSet = ids && WeakMap.prototype.set;

/**
 * Generates a fast string key for O(1) lookup.
 * Returns 'FROZEN' if the object cannot be tagged.
 */
module.exports = function (value) {
  var type = typeof value;
  if (type === "string") return "S" + value;
  if (type === "number") return "N" + value;
  if (type === "bigint") return "I" + value;
  if (type === "boolean") return "B" + value;
  if (type === "symbol") return value;
  if (value === null) return "Lnull";
  if (value === undefined) return "Uundefined";

  // Handle Object
  if (ids) {
    var id = wmGet.call(ids, value);
    if (id === undefined) wmSet.call(ids, value, (id = ++ID_COUNTER));
    return "O" + id;
  }
  // Without a host WeakMap, tag the object itself
  if (Object.prototype.hasOwnProperty.call(value, UID)) {
    return "O" + value[UID];
  }
  if (!Object.isExtensible(value)) {
    return "FROZEN";
  }
  Object.defineProperty(value, UID, {
    value: ++ID_COUNTER,
    writable: false,
    enumerable: false,
    configurable: false,
  });
  return "O" + value[UID];
}