var getFastKey = require("./getFastkey");

module.exports = function (setData, value) {
  var key = getFastKey(value);

  if (key === "FROZEN") {
    var frozenList = setData.frozen;
    for (var i = 0; i < frozenList.length; i++) {
      if (frozenList[i] === value) {
        var current = setData.head;
        while (current) {
          if (current.value === value) {
            if (current.prev) current.prev.next = current.next;
            else setData.head = current.next;
            if (current.next) current.next.prev = current.prev;
            else setData.tail = current.prev;
            current.removed = true;
            break;
          }
          current = current.next;
        }
        frozenList.splice(i, 1);
        setData.size--;
        return true;
      }
    }
    return false;
  }

  var node = setData.index[key];
  if (!node) return false;

  delete setData.index[key];

  if (node.prev) node.prev.next = node.next;
  else setData.head = node.next;

  if (node.next) node.next.prev = node.prev;
  else setData.tail = node.prev;

  node.removed = true;
  setData.size--;
  return true;
}
