var setAdd = Set.prototype.add;
var counter = 0;

Set.prototype.add = function(value) {
  counter++;
  setAdd.call(this, value);
};

var s = new Set([1, 2]);
