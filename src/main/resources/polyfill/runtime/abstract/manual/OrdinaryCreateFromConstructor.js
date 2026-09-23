var AO__GetPrototypeFromConstructor = require("./GetPrototypeFromConstructor");
var AO__OrdinaryObjectCreate = require("./OrdinaryObjectCreate");

// https://tc39.es/ecma262/multipage/ordinary-and-exotic-objects-behaviours.html#sec-ordinarycreatefromconstructor
function AO__OrdinaryCreateFromConstructor(constructor, intrinsicDefaultProto) {
  "use strict";

  var internalSlotsList = arguments.length > 2 ? arguments[2] : undefined;
  var internalSlotsListIsPresent = arguments.length > 2;
  var proto = AO__GetPrototypeFromConstructor(
    constructor,
    intrinsicDefaultProto,
  );
  var slotsList = internalSlotsListIsPresent ? internalSlotsList : [];
  return AO__OrdinaryObjectCreate(proto, slotsList);
}

module.exports = AO__OrdinaryCreateFromConstructor;
