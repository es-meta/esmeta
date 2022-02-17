function f(a) {
  switch(a) {
    case 1:
    case 'a':
      break;
    case 'b':
      return 2;
    default:
      a = 3;
  }
  return a;
}
var x0 = f(1);
var x1 = f('a');
var x2 = f('b');
var x3 = f('c');
