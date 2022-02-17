let x, y;

var count = 0;
async function * fn() {
  for await ([...[x, y]] of [[null]]) {
      if ( x == null ) count ++;
      if ( y == undefined ) count++;
  }
}

var promise = fn().next();

