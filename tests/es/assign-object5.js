var noerror = 0;
var propKeyEvaluated = false;
var base = {};
var prop = {
  toString: function() {
    if(propKeyEvaluated) noerror = 1;
    propKeyEvaluated = true;
    return "a";
  }
};
var expr = function() {
  return 1;
};

base[prop] = expr();
