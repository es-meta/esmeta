function AO__RequireInternalSlot(O, internalSlot) {
  // 1. If O is not an Object, throw a TypeError exception.
  if (typeof O !== "object") throw new TypeError();
  // 2. If O does not have an internalSlot internal slot, throw a TypeError exception.
  if (!(internalSlot in O)) throw new TypeError();
  // 3. Return unused.
  return "unused";
}

module.exports = AO__RequireInternalSlot;
