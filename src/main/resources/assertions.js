// hidden constructors
let AsyncArrowFunction = Object.getPrototypeOf(async () => {}).constructor;
let AsyncFunction = Object.getPrototypeOf(async function () {}).constructor;
let AsyncGeneratorFunction = Object.getPrototypeOf(
  async function* () {},
).constructor;
let GeneratorFunction = Object.getPrototypeOf(function* () {}).constructor;

// logging errors
let $error = (globalThis.console && globalThis.console.log) || globalThis.print;

// conversion to string
let $toString = (value) => {
  if (value === 0 && 1 / value === -Infinity) return "«-0»";
  if (value instanceof Error) return "a " + value.constructor.name;
  if (value === AsyncFunction.prototype) return "the async function prototype";
  if (value === AsyncGeneratorFunction.prototype) {
    return "the async generator function prototype";
  }
  if (value === GeneratorFunction.prototype) {
    return "the generator function prototype";
  }
  if (value === AsyncArrowFunction.prototype) {
    return "the async arrow function prototype";
  }
  if (value === Object.prototype) return "the object prototype";
  if (typeof value === "string") return '"' + value + '"';
  if (typeof value === "function") return "[object Function]";
  if (typeof value === "object") return "[object Object]";
  return String(value);
};

// wrapper of Object.getPrototpyeOf
let $Object_getPrototypeOf = (o) => {
  if (o === null || o === undefined) {
    return undefined;
  }
  return Object.getPrototypeOf(o);
};

// wrapper of Reflect.ownKeys
let $Reflect_ownKeys = (o) => {
  if (o === null || o === undefined) {
    return undefined;
  }
  return Reflect.ownKeys(o);
};

let $isSameValue = (x, y) => {
  if (x === y) return x !== 0 || 1 / x === 1 / y;
  return x !== x && y !== y;
};

// assertion
let $assert = (mustBeTrue) => {
  if (mustBeTrue === true) return;
  $error("Expected true but got " + $toString(mustBeTrue));
};

// assertion for same values
$assert.sameValue = function (actual, expected) {
  if ($isSameValue(actual, expected)) return;
  $error(
    "Expected " + $toString(expected) + " but got " + $toString(actual) + ".",
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
      ".",
  );
};

// assertion for [[Call]]
$assert.isCallable = function (f) {
  return typeof f === "function";
};
$assert.callable = function (f) {
  if (!$assert.isCallable(f)) {
    $error("Expected " + $toString(f) + " has [[Call]] but does not.");
  }
};
$assert.notCallable = function (f) {
  if ($assert.isCallable(f)) {
    $error("Expected " + $toString(f) + " does not have [[Call]] but does.");
  }
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
  if (!$assert.isConstructable(f)) {
    $error("Expected " + $toString(f) + " has [[Construct]] but does not.");
  }
};
$assert.notConstructable = function (f) {
  if ($assert.isConstructable(f)) {
    $error(
      "Expected " + $toString(f) + " does not have [[Construct]] but does.",
    );
  }
};

// assertion to compare arrays
let $compareArray = (a, b) => {
  if (b.length !== a.length) return false;

  let sortedA = a.slice();
  let sortedB = b.slice();
  /* Allow change in order */
  try {
    sortedA.sort();
    sortedB.sort();
  } catch {
    sortedA = a;
    sortedB = b;
  }

  for (var i = 0; i < sortedA.length; i++) {
    if (!$isSameValue(sortedA[i], sortedB[i])) return false;
  }
  return true;
};

$assert.compareArray = function (actual, expected, obj) {
  function format(array) {
    return "[" + array.map($toString).join(", ") + "]";
  }
  function getObjDesc(obj) {
    var ty = Object.prototype.toString.call(obj);
    return ty.substring("[object ".length, ty.length - "]".length);
  }

  if (!Array.isArray(actual)) {
    $error(
      "$assert.compareArray requires an array as the first argument but " +
        $toString(actual) +
        " given.",
    );
    return;
  }
  if (!Array.isArray(expected)) {
    $error(
      "$assert.compareArray requires an array as the second argument but " +
        $toString(expected) +
        " given.",
    );
    return;
  }

  if ($compareArray(actual, expected)) return;
  $error(
    "Expected " +
      format(expected) +
      " but got " +
      format(actual) +
      " for " +
      getObjDesc(obj) +
      ".",
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
        " value(s).",
    );
    validators[i](result.value);
  }
  result = iter.next();
  $error(
    result.done,
    "Expected only " + i + " values(s). Instead iterator produced more.",
  );
  $assert.sameValue(
    result.value,
    undefined,
    "Expected value of `undefined` when iterator completes.",
  );
};

// verify properties
let $verifyProperty = (obj, prop, desc) => {
  // check object
  if (obj === undefined || obj === null) {
    $error(
      "$verifyProperty requires an object but " +
        $toString(obj) +
        " given.",
    );
    return;
  }

  // check property type
  var propType = typeof prop;
  if (propType !== "string" && propType !== "symbol") {
    $error(
      "$verifyProperty requires a string or symbol property but " +
        $toString(prop) +
        " given.",
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
  if (!hasOwnProperty.call(obj, prop)) {
    $error(
      $toString(obj) + " does not have own property named " + $toString(prop),
    );
    return;
  }
  $assert.notSameValue(desc, null);
  $assert.sameValue(typeof desc, "object");

  function check(name) {
    try {
      if (!hasOwnProperty.call(desc, name)) return;
      if ($isSameValue(desc[name], originalDesc[name])) return;
      var message;
      if (name === "value") {
        if ($toString(prop) == "name") return; /* Allow different name */
        message = "descriptor value of " + $toString(prop) + " should be " +
          $toString(desc.value) +
          " but " +
          $toString(originalDesc.value);
      } else {
        message = "descriptor should " + (desc[name] ? "" : "not ") + "be " +
          name;
      }
      $error(message);
    } catch (e) {}
  }
  check("value");
  check("writable");
  check("enumerable");
  check("configurable");
};

// delay checking assertions in JS runtime environment
// (supported: Node, QuickJs)
// or wait for 10 microtasks in JS Engine
// (might be broken if there are more than 10 awaits in program, etc.)
let $delay = (f) => {
  var setTimeout = globalThis.setTimeout;
  import("os")
    .then((os) => {
      // qjs
      setTimeout ??= os?.setTimeout;
    })
    .catch(() => {})
    .then(() => {
      let p;
      if (setTimeout) {
        p = new Promise((resolve) => setTimeout(resolve, 0));
      } else {
        p = Promise.resolve();
        // delay 10 times
        for (var i = 0; i < 10; i++) {
          p = p.then(() => {});
        }
      }
      p.then(f).catch(() =>
        $error("An exception occured while checking assertions")
      );
    });
};
