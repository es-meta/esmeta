function f(x, y) {
  return x + y;
}
var g = f.bind(null, 3);
var x = g(5);
var y = g('a');
