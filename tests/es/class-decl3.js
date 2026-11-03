class C {
  #v = 1;
  get #x() { return this.#v; }
  set #x(a) { this.#v = a; }
  t() { this.#x = 5; return this.#x; }
}

var x = new C().t();
