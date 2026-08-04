package esmeta.parser

import esmeta.ES_TEST_DIR
import esmeta.util.SystemUtils.*

/** ESTree parser test on the ECMAScript test programs */
class EsTreeSmallTest extends EsTreeTest {
  val name: String = "esTreeParseTest"

  // registration
  def init: Unit = {
    if (!EsTreeTest.canUse) {
      check("Node.js is unavailable")(cancel("Node.js is unavailable"))
    } else {
      // the syntax of the specification, one program per construct
      for ((desc, code) <- EsTreeSmallTest.cases)
        check(desc)(EsTreeTest.sameAst(code))

      // the module goal symbol
      for ((desc, code) <- EsTreeSmallTest.moduleCases)
        check(s"module: $desc")(EsTreeTest.sameModuleAst(code))

      // every ECMAScript test program
      for (file <- walkTree(ES_TEST_DIR)) {
        val filename = file.getName
        if (jsFilter(filename)) check(filename) {
          EsTreeTest.sameAstFile(file.toString)
        }
      }
    }
  }

  init
}
object EsTreeSmallTest {

  /** programs that exercise the corners of the conversion
    *
    * Their derivations are the ones an abstract syntax tree does not record:
    * cover grammars, elisions, trailing commas, and the productions that only a
    * concrete syntax tree distinguishes.
    */
  val cases: List[(String, String)] = List(
    // trailing commas, which ESTree drops
    "array trailing comma" -> "[1, 2,];",
    "array no trailing comma" -> "[1, 2];",
    "object trailing comma" -> "({ a: 1, });",
    "arguments trailing comma" -> "f(a, b,);",
    "parameters trailing comma" -> "function f(a, b,) {}",
    "array pattern trailing comma" -> "var [a, b,] = c;",
    "object pattern trailing comma" -> "var { a, } = b;",
    // elisions, which ESTree records as holes
    "empty array" -> "[];",
    "single elision" -> "[,];",
    "double elision" -> "[,,];",
    "leading elisions" -> "[,,1];",
    "middle elision" -> "[1,,2];",
    "trailing elision" -> "[1,,];",
    "elision with spread" -> "[,,...a];",
    "array pattern elisions" -> "var [,,a,,b,,] = c;",
    "array pattern rest after elision" -> "var [,,...a] = b;",
    // the cover grammar of parenthesized expressions and arrow parameters
    "parenthesized expression" -> "(a);",
    "parenthesized sequence" -> "(a, b);",
    "arrow with one bare parameter" -> "a => a;",
    "arrow with one parenthesized parameter" -> "(a) => a;",
    "arrow with no parameter" -> "() => 0;",
    "arrow with two parameters" -> "(a, b) => a;",
    "arrow with trailing comma" -> "(a, b,) => a;",
    "arrow with rest parameter" -> "(...a) => a;",
    "arrow with parameters and rest" -> "(a, ...b) => a;",
    "arrow with pattern rest" -> "(...[a]) => a;",
    "arrow with default parameter" -> "(a = 1) => a;",
    "arrow with pattern parameter" -> "({ a }) => a;",
    "arrow with pattern default" -> "({ a = 1 }) => a;",
    "arrow with array pattern" -> "([a, ...b]) => a;",
    "arrow with block body" -> "(a) => { return a; };",
    // the cover grammar of calls and async arrow heads
    "async arrow with bare parameter" -> "async a => a;",
    "async arrow with no parameter" -> "async () => 0;",
    "async arrow with parameters" -> "async (a, b) => a;",
    "async arrow with rest" -> "async (...a) => a;",
    "async arrow with block body" -> "async (a) => { await a; };",
    "call of async" -> "async(a, b);",
    // destructuring assignment, which reuses the literal productions
    "array destructuring assignment" -> "[a, b] = c;",
    "object destructuring assignment" -> "({ a } = b);",
    "nested destructuring assignment" -> "[{ a: [b] }] = c;",
    "destructuring with defaults" -> "[a = 1, { b = 2 }] = c;",
    "destructuring with member target" -> "[a.b, c[d]] = e;",
    "for-of destructuring" -> "for ([a, b] of c) ;",
    // member, call, and optional chains
    "static member" -> "a.b;",
    "computed member" -> "a[b];",
    "private member" -> "class C { #x; m() { return this.#x; } }",
    "call then member" -> "f().x;",
    "call then call" -> "f()();",
    "member then call" -> "a.b();",
    "new without arguments" -> "new f;",
    "new with arguments" -> "new f();",
    "new of member" -> "new a.b();",
    "new of new" -> "new new f();",
    "parenthesized new then call" -> "(new f)();",
    "optional member" -> "a?.b;",
    "optional call" -> "a?.();",
    "optional then static" -> "a?.b.c;",
    "optional then optional" -> "a?.b?.c;",
    "optional computed" -> "a?.[b];",
    "optional call then member" -> "a?.().b;",
    "optional then two statics" -> "a?.b.c.d;",
    "optional then three statics" -> "a?.b.c.d.e;",
    "optional then mixed links" -> "a?.b[c].d(e);",
    "optional then call then computed" -> "a?.b()[c];",
    "optional call then calls" -> "a?.()()();",
    "optional of call" -> "f()?.a;",
    "optional private" -> "class C { #x; m(o) { return o?.#x; } }",
    "spread call" -> "f(...a);",
    "spread in the middle" -> "f(a, ...b, c);",
    "super property" -> "class C extends D { m() { return super.x; } }",
    "super call" -> "class C extends D { constructor() { super(); } }",
    "dynamic import" -> "import('a');",
    "dynamic import with options" -> "import('a', {});",
    "dynamic import then member" -> "import('a').then;",
    "call of dynamic import" -> "import('a')();",
    "call of dynamic import with arguments" -> "import('a')(1, 2);",
    "call of dynamic import with spread" -> "import('a')(...[]);",
    "call of dynamic import then member" -> "import('a')().b;",
    "dynamic import then computed member" -> "import('a')['then'];",
    "dynamic import then tagged template" -> "import('a')`b`;",
    "dynamic import then optional" -> "import('a')?.then;",
    "super call then call" ->
    "class C extends D { constructor() { super()(); } }",
    "super call then member" ->
    "class C extends D { constructor() { super().x; } }",
    "new target" -> "function f() { return new.target; }",
    // templates
    "template without substitution" -> "`a`;",
    "template with one substitution" -> "`a${b}c`;",
    "template with many substitutions" -> "`${a}b${c}d${e}`;",
    "empty template" -> "``;",
    "tagged template" -> "f`a${b}`;",
    "tagged template of member" -> "a.b`c`;",
    "tagged template of call" -> "f()`c`;",
    "nested template" -> "`a${`b${c}`}d`;",
    // literals, kept as lexical tokens
    "numeric literals" -> "0; 1.5; .5; 1e3; 0x10; 0b1; 0o7; 1_000; 10n;",
    "string literals" -> "'a'; \"b\"; '\\u0041'; '\\x41';",
    "regular expression" -> "/a/g;",
    "division is not a regexp" -> "a / b / c;",
    "escaped identifier" -> "var \\u0061bc;",
    "escaped keyword as label" -> "\\u0061wait: ;",
    "yield as identifier" -> "var yield;",
    "await as identifier" -> "var await;",
    // classes
    "empty class" -> "class C {}",
    "class with a lone semicolon" -> "class C { ; }",
    "class with many semicolons" -> "class C { ;; m() {} ; }",
    "class field then semicolon" -> "class C { x = 1; ; }",
    "class heritage" -> "class C extends D {}",
    "class expression" -> "(class {});",
    "static field" -> "class C { static x = 1; }",
    "static block" -> "class C { static { x = 1; } }",
    "empty static block" -> "class C { static {} }",
    "accessors" -> "class C { get x() {} set x(v) {} }",
    "static accessors" -> "class C { static get x() {} static set x(v) {} }",
    "generator method" -> "class C { *m() {} }",
    "async method" -> "class C { async m() {} }",
    "async generator method" -> "class C { async *m() {} }",
    "private methods" -> "class C { #m() {} get #g() {} static #s() {} }",
    "computed member name" -> "class C { [x]() {} }",
    // functions
    "function declaration" -> "function f(a, b) { return a; }",
    "function expression" -> "(function () {});",
    "named function expression" -> "(function f() {});",
    "generator declaration" -> "function* f() { yield; }",
    "generator expression" -> "(function* () { yield 1; });",
    "async function" -> "async function f() { await 1; }",
    "async generator" -> "async function* f() { yield await 1; }",
    "yield delegate" -> "function* f() { yield* g(); }",
    "rest parameter" -> "function f(...a) {}",
    "default parameter" -> "function f(a = 1) {}",
    "pattern parameters" -> "function f({ a }, [b]) {}",
    "empty parameters" -> "function f() {}",
    "empty body" -> "function f() {}",
    "object methods" -> "({ m() {}, *g() {}, async a() {}, get x() {}, set x(v) {} });",
    "object shorthand" -> "({ a, b });",
    "object spread" -> "({ ...a });",
    "object computed key" -> "({ [a]: b });",
    "object literal keys" -> "({ 'a': 1, 0: 2, if: 3 });",
    // statements
    "empty statement" -> ";",
    "block" -> "{ }",
    "nested blocks" -> "{ { ; } }",
    "if without else" -> "if (a) ;",
    "if with else" -> "if (a) ; else ;",
    "do-while" -> "do ; while (a);",
    "while" -> "while (a) ;",
    "for with all parts" -> "for (a; b; c) ;",
    "for with no part" -> "for (;;) ;",
    "for with var" -> "for (var a = 0; a; a++) ;",
    "for with let" -> "for (let a = 0; a; a++) ;",
    "for with const" -> "for (const a = 0;;) ;",
    "for-in" -> "for (a in b) ;",
    "for-in with var" -> "for (var a in b) ;",
    "for-in with let" -> "for (let a in b) ;",
    "for-of" -> "for (a of b) ;",
    "for-of with const" -> "for (const a of b) ;",
    "for-of with pattern" -> "for (const [a] of b) ;",
    "for-await-of" -> "async function f() { for await (const a of b) ; }",
    "continue" -> "while (a) continue;",
    "labelled continue" -> "x: while (a) continue x;",
    "break" -> "while (a) break;",
    "labelled break" -> "x: while (a) break x;",
    "return" -> "function f() { return; }",
    "return a value" -> "function f() { return a; }",
    "with" -> "with (a) ;",
    "switch without default" -> "switch (a) { case 1: ; case 2: }",
    "switch with default" -> "switch (a) { case 1: ; default: ; case 2: }",
    "switch with only default" -> "switch (a) { default: }",
    "empty switch" -> "switch (a) { }",
    "labelled statement" -> "x: ;",
    "labelled function" -> "x: function f() {}",
    "throw" -> "throw a;",
    "try-catch" -> "try {} catch (e) {}",
    "try-catch without parameter" -> "try {} catch {}",
    "try-finally" -> "try {} finally {}",
    "try-catch-finally" -> "try {} catch (e) {} finally {}",
    "catch with pattern" -> "try {} catch ({ e }) {}",
    "debugger" -> "debugger;",
    "var declarations" -> "var a, b = 1, [c] = d, { e } = f;",
    "let declarations" -> "let a, b = 1;",
    "const declaration" -> "const a = 1;",
    "directive prologue" -> "'use strict'; a;",
    // operators, one per production of the grammar
    "exponentiation" -> "a ** b ** c;",
    "multiplicative" -> "a * b / c % d;",
    "additive" -> "a + b - c;",
    "shift" -> "a << b >> c >>> d;",
    "relational" -> "a < b > c <= d >= e;",
    "instanceof and in" -> "a instanceof b; a in b;",
    "private in" -> "class C { #x; m(o) { return #x in o; } }",
    "equality" -> "a == b != c === d !== e;",
    "bitwise" -> "a & b ^ c | d;",
    "logical" -> "a && b || c;",
    "coalesce" -> "a ?? b ?? c;",
    "coalesce of logical" -> "(a || b) ?? c;",
    "conditional" -> "a ? b : c;",
    "assignment operators" ->
    "a = b; a += b; a -= b; a *= b; a /= b; a %= b; a **= b;",
    "shift assignments" -> "a <<= b; a >>= b; a >>>= b;",
    "bitwise assignments" -> "a &= b; a ^= b; a |= b;",
    "logical assignments" -> "a &&= b; a ||= b; a ??= b;",
    "unary operators" -> "delete a.b; void a; typeof a; +a; -a; ~a; !a;",
    "update operators" -> "++a; --a; a++; a--;",
    "sequence" -> "a, b, c;",
    "await" -> "async function f() { await a; }",
    "this" -> "this;",
    // automatic semicolon insertion, which changes the source ESMeta parses
    "semicolon insertion" -> "a\nb",
    "semicolon insertion before a brace" -> "function f() { return\n}",
    "semicolon insertion after a postfix" -> "a\n++b",
  )

  /** programs of the `Module` goal symbol */
  val moduleCases: List[(String, String)] = List(
    "empty module" -> "",
    "side-effect import" -> "import 'a';",
    "default import" -> "import a from 'b';",
    "namespace import" -> "import * as a from 'b';",
    "named imports" -> "import { a, b } from 'c';",
    "no named import" -> "import {} from 'a';",
    "named imports trailing comma" -> "import { a, } from 'b';",
    "renamed import" -> "import { a as b } from 'c';",
    "string named import" -> "import { 'a' as b } from 'c';",
    "default and namespace" -> "import a, * as b from 'c';",
    "default and named" -> "import a, { b } from 'c';",
    "default and empty named" -> "import a, {} from 'c';",
    "import attributes" -> "import a from 'b' with { type: 'json' };",
    "empty import attributes" -> "import a from 'b' with {};",
    "side-effect import with attributes" -> "import 'a' with { type: 'json' };",
    "string attribute key" -> "import a from 'b' with { 'type': 'json' };",
    "export all" -> "export * from 'a';",
    "export all as" -> "export * as a from 'b';",
    "export all as string" -> "export * as 'a' from 'b';",
    "export named from" -> "export { a } from 'b';",
    "export named" -> "var a; export { a };",
    "export no name" -> "export {};",
    "export named trailing comma" -> "var a; export { a, };",
    "export renamed" -> "var a; export { a as b };",
    "export renamed to string" -> "var a; export { a as 'b' };",
    "export var" -> "export var a = 1;",
    "export let" -> "export let a = 1;",
    "export const" -> "export const a = 1;",
    "export function" -> "export function f() {}",
    "export generator" -> "export function* f() {}",
    "export async function" -> "export async function f() {}",
    "export class" -> "export class C {}",
    "export default function" -> "export default function f() {}",
    "export default anonymous function" -> "export default function () {}",
    "export default anonymous generator" -> "export default function* () {}",
    "export default async function" -> "export default async function () {}",
    "export default class" -> "export default class C {}",
    "export default anonymous class" -> "export default class {}",
    "export default expression" -> "export default 1 + 2;",
    "export from with attributes" -> "export { a } from 'b' with { type: 'json' };",
    "top-level await" -> "await 1;",
    "import.meta" -> "import.meta;",
    "module with statements" -> "var a = 1; export { a }; a++;",
  )
}
