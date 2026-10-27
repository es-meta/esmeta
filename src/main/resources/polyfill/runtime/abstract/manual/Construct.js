function AO__Construct(F) {
  var argumentsList = arguments.length > 1 ? arguments[1] : [];
  var bound = Function.prototype.bind.apply(F, [null].concat(argumentsList));
  return new bound();
}

module.exports = AO__Construct;
