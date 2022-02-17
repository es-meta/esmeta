function f(a, b) {
  arguments[1] = 7;
  return b;
}
var x = f(1, 2);
