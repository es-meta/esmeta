var getFastKey = require("./getFastkey");

module.exports = function (mapData, entry) {
  var key = entry.Key;
  var fastKey = getFastKey(key);

  if (fastKey === "FROZEN") {
    // Slow path for frozen objects
    var frozenList = mapData.frozen;
    for (var i = 0; i < frozenList.length; i++) {
      if (frozenList[i].Key === key) {
        frozenList[i].Value = entry.Value;
        return;
      }
    }

    var node = { Key: key, Value: entry.Value, next: null, prev: mapData.tail, frozen: true };
    frozenList.push(node);

    if (mapData.tail) {
      mapData.tail.next = node;
      mapData.tail = node;
    } else {
      mapData.head = mapData.tail = node;
    }
    mapData.size++;
    return;
  }

  // Fast path
  var node = mapData.index[fastKey];
  if (node) {
    node.Value = entry.Value;
    return;
  }

  node = { Key: key, Value: entry.Value, next: null, prev: mapData.tail, key: fastKey };
  mapData.index[fastKey] = node;

  if (mapData.tail) {
    mapData.tail.next = node;
    mapData.tail = node;
  } else {
    mapData.head = mapData.tail = node;
  }
  mapData.size++;
}
