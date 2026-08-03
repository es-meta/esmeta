{
  let called = false;
  function get() {
    if (called) throw 0;
    called = true;
    return null;
  }
  let result = get()?.p;
  if (!called || result !== undefined) throw 0;
  result
;}
0
