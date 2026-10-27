function OP__Replace(base, oldElem, newElem) {
  var index = base.indexOf(oldElem);
  if (index !== -1) {
    base[index] = newElem;
  }
  return base;
}

module.exports = OP__Replace;
