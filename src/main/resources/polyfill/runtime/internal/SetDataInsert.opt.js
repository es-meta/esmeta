var getFastKey = require("./getFastkey");

module.exports = function (setData, value) {
  var key = getFastKey(value);

  if (key === "FROZEN") {
    // Slow path for frozen objects (O(N))
    var frozenList = setData.frozen;
    for (var i = 0; i < frozenList.length; i++) {
      if (frozenList[i] === value) return S;
    }
    frozenList.push(value);

    // Create a node for iteration, but it won't be in the fast index
    var node = { value: value, next: null, prev: setData.tail, frozen: true };
    if (setData.tail) {
      setData.tail.next = node;
      setData.tail = node;
    } else {
      setData.head = setData.tail = node;
    }
    setData.size++;
    return;
  }

  // Fast path (O(1))
  if (setData.index[key]) return;

  var node = { value: value, next: null, prev: setData.tail, key: key };
  setData.index[key] = node;

  if (setData.tail) {
    setData.tail.next = node;
    setData.tail = node;
  } else {
    setData.head = setData.tail = node;
  }
  setData.size++;
}
