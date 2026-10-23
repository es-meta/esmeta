// The optimized implementations are emitted under their plain names, so a
// sibling is addressed the same way in either mode.
var SetDataCreate = require("./SetDataCreate");
var SetDataInsert = require("./SetDataInsert");

module.exports = function (setData) {
  var newSetData = SetDataCreate();
  var node = setData.head;
  while (node) {
    SetDataInsert(newSetData, node.value);
    node = node.next;
  }
  return newSetData;
}
