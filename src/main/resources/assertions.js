// hidden constructors
var AsyncFunction = Object.getPrototypeOf(async function () {}).constructor;
var AsyncGeneratorFunction = Object.getPrototypeOf(
  async function* () {}
).constructor;
var GeneratorFunction = Object.getPrototypeOf(function* () {}).constructor;
var TypedArray = Object.getPrototypeOf(Uint8Array);
var GeneratorPrototype = Object.getPrototypeOf(function* () {}).prototype;
var AsyncGeneratorPrototype = Object.getPrototypeOf(
  async function* () {}
).prototype;
var AsyncIteratorPrototype = Object.getPrototypeOf(
  Object.getPrototypeOf(async function* () {}).prototype
);
var ArrayIteratorPrototype = Object.getPrototypeOf([][Symbol.iterator]());
var StringIteratorPrototype = Object.getPrototypeOf(
  new String()[Symbol.iterator]()
);
var MapIteratorPrototype = Object.getPrototypeOf(new Map()[Symbol.iterator]());
var SetIteratorPrototype = Object.getPrototypeOf(new Set()[Symbol.iterator]());
var ThrowTypeError = (function () {
  "use strict";
  return Object.getOwnPropertyDescriptor(arguments, "callee").get;
})();
var IteratorHelperPrototype =
  typeof Iterator !== "undefined"
    ? Object.getPrototypeOf(Iterator.from([]).drop(0))
    : undefined;
var WrapForValidIteratorPrototype =
  typeof Iterator !== "undefined"
    ? Object.getPrototypeOf(
      Iterator.from({ [Symbol.iterator]() { return {}; }})
    )
    : undefined;

// logging errors
var $error = (globalThis.console && globalThis.console.log) || globalThis.print;

// algo map
var $algo = new Map();

// null-safe wrapper of Reflect
var $Reflect = {
  ownKeys: function (o) {
    if (o === null || o === undefined) return undefined;
    return Reflect.ownKeys(o);
  },
};

// conversion to string
function $toString(value) {
  if (value === 0 && 1 / value === -Infinity) return "«-0»";
  if (value instanceof Error) return "a " + value.constructor.name;
  if (typeof value === "string") return '"' + value + '"';
  return String(value);
}

function $isSameValue(x, y) {
  if (x === y) return x !== 0 || 1 / x === 1 / y;
  return x !== x && y !== y;
}

// assertion
function $assert(mustBeTrue) {
  if (mustBeTrue === true) return;
  $error("Expected true but got " + $toString(mustBeTrue));
}

// assertion for comparing two thrown values
$assert.sameThrows = function (thrown, expected) {
  var thrownStr = $toString(thrown);
  if (typeof expected !== "function") {
    if (thrown !== expected)
      $error("Expected " + $toString(expected) + " but got " + thrownStr);
  } else if (!(thrown instanceof expected)) {
    $error("Expected a " + expected.name + " but got " + thrownStr);
  }
};

// assertion for thrown values that were expected to be thrown but not thrown
$assert.shouldveThrown = function (expected) {
  $error(
    "Expected a " +
      expected.name +
      " to be thrown but no exception was thrown at all"
  );
};

// assertion for no exception
$assert.notThrows = function (func) {
  if (typeof func !== "function") {
    $error("$assert.notThrows requires a function.");
    return;
  }
  try {
    func();
  } catch (thrown) {
    $error("Expected no exception but " + $toString(thrown) + " is thrown.");
    return;
  }
};

// assertion for same values
$assert.sameValue = function (actual, expected) {
  if ($isSameValue(actual, expected)) return;
  $error(
    "Expected " + $toString(expected) + " but got " + $toString(actual) + "."
  );
};

// assertion for same values
$assert.notSameValue = function (actual, unexpected) {
  if (!$isSameValue(actual, unexpected)) return;
  $error(
    "Not expected " +
      $toString(unexpected) +
      " but got " +
      $toString(actual) +
      "."
  );
};

// assertion for [[Call]]
$assert.isCallable = function (f) {
  return typeof f === "function";
};
$assert.callable = function (f) {
  if (!$assert.isCallable(f))
    $error("Expected " + $toString(f) + " has [[Call]] but does not.");
};
$assert.notCallable = function (f) {
  if ($assert.isCallable(f))
    $error("Expected " + $toString(f) + " does not have [[Call]] but does.");
};

// assertion for [[Construct]]
$assert.isConstructable = function (f) {
  try {
    Reflect.construct(function () {}, [], f);
    return true;
  } catch (e) {
    return false;
  }
};
$assert.constructable = function (f) {
  if (!$assert.isConstructable(f))
    $error("Expected " + $toString(f) + " has [[Construct]] but does not.");
};
$assert.notConstructable = function (f) {
  if ($assert.isConstructable(f))
    $error(
      "Expected " + $toString(f) + " does not have [[Construct]] but does."
    );
};

// assertion to compare arrays
function $compareArray(actual, expected) {
  // NOTE: all expected elements should appear in order, while additional
  // implementation-defined elements (e.g. `stack` of errors)
  // may be interleaved between them
  var i = 0;
  for (var j = 0; j < actual.length && i < expected.length; j++) {
    if ($isSameValue(expected[i], actual[j])) i++;
  }
  return i === expected.length;
}

// assertion to compare arrays considering implementation-defined elements
$assert.compareArray = function (actual, expected, obj) {
  function format(array) {
    if (!Array.isArray(array)) return $toString(array);
    return "[" + array.map($toString).join(", ") + "]";
  }
  function getObjDesc(obj) {
    var algo = $algo.get(obj) || "Nothing";
    var ty = Object.prototype.toString.call(obj);
    ty = ty.substring("[object ".length, ty.length - "]".length);
    return `${algo} for ${ty}`;
  }
  if (Array.isArray(actual) && $compareArray(actual, expected)) return;
  $error(
    "Expected " +
      format(expected) +
      " but got " +
      format(actual) +
      " in " +
      getObjDesc(obj) +
      "."
  );
};

// assertion to compare iterators
$assert.compareIterator = function (iter, validators) {
  var i, result;
  for (i = 0; i < validators.length; i++) {
    result = iter.next();
    $error(
      !result.done,
      "Expected " +
        i +
        " values(s). Instead iterator only produced " +
        (i - 1) +
        " value(s)."
    );
    validators[i](result.value);
  }
  result = iter.next();
  $error(
    result.done,
    "Expected only " + i + " values(s). Instead iterator produced more."
  );
  $assert.sameValue(
    result.value,
    undefined,
    "Expected value of `undefined` when iterator completes."
  );
};

// verify properties
function $verifyProperty(obj, prop, desc) {
  // check property type
  var propType = typeof prop;
  if (propType !== "string" && propType !== "symbol") {
    $error(
      "$verifyProperty requires a string or symbol property but " +
        $toString(prop) +
        " given."
    );
    return;
  }

  var originalDesc = Object.getOwnPropertyDescriptor(obj, prop);

  // Allows checking for undefined descriptor if it's explicitly given.
  if (desc === undefined) {
    $assert.sameValue(originalDesc, undefined);
    return;
  }

  var hasOwnProperty = Object.prototype.hasOwnProperty;
  $assert(hasOwnProperty.call(obj, prop));
  $assert.notSameValue(desc, null);
  $assert.sameValue(typeof desc, "object");

  function check(name) {
    try {
      if (!hasOwnProperty.call(desc, name)) return;
      if ($isSameValue(desc[name], originalDesc[name])) return;
      var message;
      if (name === "value")
        message =
          "descriptor value should be " +
          $toString(desc.value) +
          " but " +
          $toString(originalDesc.value);
      else
        message =
          "descriptor should " + (desc[name] ? "" : "not ") + "be " + name;
      $error(message);
    } catch (e) {}
  }
  check("value");
  check("writable");
  check("enumerable");
  check("configurable");
}

// delay checking assertions
function $delay(f) {
  var DELAY = 100;
  var setTimeout = globalThis.setTimeout;
  import("os")
    .then((os) => {
      // qjs
      if (!setTimeout) setTimeout = os?.setTimeout;
    })
    .catch(() => {})
    .finally(() => {
      setTimeout(f, DELAY);
    });
}
