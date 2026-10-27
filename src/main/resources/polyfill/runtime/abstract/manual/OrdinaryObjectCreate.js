var $ObjectCreate = Object.create;
var $ObjectDefineProperty = Object.defineProperty;

function AO__OrdinaryObjectCreate(proto) {
  var additionalInternalSlotsList =
    arguments.length > 1 ? arguments[1] : undefined;
  var additionalInternalSlotsListIsPresent = arguments.length > 1;
  // 1. Let _internalSlotsList_ be « [[Prototype]], [[Extensible]] ».
  var internalSlotsList = ["Prototype", "Extensible"];
  // 1. If _additionalInternalSlotsList_ is present, set _internalSlotsList_ to the list-concatenation of _internalSlotsList_ and _additionalInternalSlotsList_.
  if (additionalInternalSlotsListIsPresent)
    internalSlotsList = internalSlotsList.concat(additionalInternalSlotsList);
  // 1. Let _O_ be MakeBasicObject(_internalSlotsList_).
  var O = $ObjectCreate(proto);
  for (var i = 0; i < internalSlotsList.length; i++) {
    var _x0 = internalSlotsList[i];
    $ObjectDefineProperty(O, _x0, {
      value: undefined,
      writable: true,
      enumerable: false,
      configurable: true,
    });
  }
  // 1. Set _O_.[[Prototype]] to _proto_.
  $ObjectDefineProperty(O, "Prototype", {
    value: proto,
    writable: true,
    enumerable: false,
    configurable: true,
  })
  // 1. Return _O_.
  return O;

  // if (proto !== null && !isObject(proto)) {
  //   throw new TypeError("`proto` must be null or an object");
  // }
  // var additionalInternalSlotsList = arguments.length < 2 ? [] : arguments[1];
  // if (!isArray(additionalInternalSlotsList)) {
  //   throw new TypeError("`additionalInternalSlotsList` must be an Array");
  // }
  // var F = function () { };
  // F.prototype = proto;
  // var obj = new F();
  // for (var i = 0; i < additionalInternalSlotsList.length; i++) {
  //   var slot = additionalInternalSlotsList[i];
  //   obj[slot] = null;
  // }
  // return obj;
}

module.exports = AO__OrdinaryObjectCreate;
