var IN__Global = require("./Global");

// A newly created AggregateError object, without its "errors" property: the
// spec defines that property in the next step with DefinePropertyOrThrow.
// `AggregateError` is ES2021, so older hosts get an object that inherits from
// an `AggregateError.prototype`-like object chained to `Error.prototype`.
var fallbackPrototype;

module.exports = function () {
  var Native = IN__Global.AggregateError;
  if (typeof Native === "function") return new Native([]);
  if (!fallbackPrototype) {
    fallbackPrototype = Object.create(Error.prototype);
    Object.defineProperty(fallbackPrototype, "name", {
      value: "AggregateError",
      writable: true,
      enumerable: false,
      configurable: true,
    });
    Object.defineProperty(fallbackPrototype, "message", {
      value: "",
      writable: true,
      enumerable: false,
      configurable: true,
    });
  }
  return Object.create(fallbackPrototype);
};
