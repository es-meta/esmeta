function AO__Construct(F) {
  if (arguments.length > 1) return new F(...arguments[1])
  return new F();
}

module.exports = AO__Construct;
