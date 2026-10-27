var getFastKey = require("./getFastkey");

module.exports = function (mapData, key) {
  var fastKey = getFastKey(key);

  if (fastKey === "FROZEN") {
    var frozenList = mapData.frozen;
    for (var i = 0; i < frozenList.length; i++) {
      if (frozenList[i].Key === key) {
        var node = frozenList[i];

        if (node.prev) node.prev.next = node.next;
        else mapData.head = node.next;

        if (node.next) node.next.prev = node.prev;
        else mapData.tail = node.prev;

        node.removed = true;
        frozenList.splice(i, 1);
        mapData.size--;
        return true;
      }
    }
    return false;
  }

  var node = mapData.index[fastKey];
  if (!node) return false;

  delete mapData.index[fastKey];

  if (node.prev) node.prev.next = node.next;
  else mapData.head = node.next;

  if (node.next) node.next.prev = node.prev;
  else mapData.tail = node.prev;

  node.removed = true;
  mapData.size--;
  return true;
}
