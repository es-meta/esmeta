// Enum values (e.g. ~empty~) as unique frozen objects, so that they never
// equal an ECMAScript value such as the string "empty".
var sentinels = Object.create(null);

function IN__Enum(name) {
  return sentinels[name] || (sentinels[name] = Object.freeze({ name: name }));
}

module.exports = IN__Enum;
