function AO__IsObject(argument) {
  return typeof argument === "object"
    ? argument !== null
    : typeof argument === "function";
}

module.exports = AO__IsObject;
