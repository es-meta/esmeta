// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-toobject
function AO__ToObject(argument) {
  "use strict";

  if (argument === undefined || argument === null) throw new TypeError();

  return Object(argument);

  // TODO: test not passing. why?
  // if (typeof argument === 'boolean') return Boolean(argument);
  // if (typeof argument === 'number') return Number(argument);
  // if (typeof argument === 'string') return String(argument);
  // if (typeof argument === 'symbol') return Symbol(argument);
  // if (typeof argument === 'bigint') return BigInt(argument);
  // return argument;

  // TODO: toReversed test false positive
  // return argument;
}

module.exports = AO__ToObject;
