{
  let called = false;
  function get() {
    if (called) throw 0;
    called = true;
    return null;
  }
  let receiver = get();
  let result =
    receiver === null || receiver === undefined
      ? undefined
      : receiver.p;
  if (!called || result !== undefined) throw 0;
}
