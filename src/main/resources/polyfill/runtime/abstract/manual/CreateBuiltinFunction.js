function AO__CreateBuiltinFunction(behaviour, length, name, additionalInternalSlotsList) {
  var func = behaviour;

  if (additionalInternalSlotsList) {
    for (var i = 0; i < additionalInternalSlotsList.length; i++) {
      func[additionalInternalSlotsList[i]] = undefined;
    }
  }
  try {
    Object.defineProperty(func, "name", {
      value: name,
      configurable: true,
      writable: false,
      enumerable: false
    });
  } catch (err) { }
  try {
    Object.defineProperty(func, "length", {
      value: length,
      configurable: true,
      writable: false,
      enumerable: false
    });
  } catch (err) { }
  return func;
}

module.exports = AO__CreateBuiltinFunction;