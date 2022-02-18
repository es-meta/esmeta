function f() { return }
var a = f({
  c: function() {
    f()
  }
})
