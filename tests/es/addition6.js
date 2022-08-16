var x, y;
try {
  0 + Symbol.toPrimitive;
} catch (e) {
  x = e instanceof TypeError;
}
try {
  Symbol.toPrimitive + '';
} catch (e) {
  y = e instanceof TypeError;
}
