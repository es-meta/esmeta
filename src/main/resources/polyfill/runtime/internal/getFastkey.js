var UID = "SET_ID_" + Math.random().toString(36).substring(2);
var ID_COUNTER = 0;

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