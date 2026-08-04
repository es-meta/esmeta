// A one-shot ESTree parser: stdin carries a single JSON request and stdout
// answers with a single JSON response.
//
// The request is an object:
//   { "code": <string> }
//   with optional "sourceType": "script" (default) | "module"
//
// The program is passed as text rather than as a path because the consumer
// slices the very same string for the lexical tokens of the tree, and the
// offsets below are indices into it.
//
// The response is an object:
//   { "ok": true, "ast": <ESTree> }
//   { "ok": false, "error": <string> }
//
// The AST keeps `start`/`end` offsets (UTF-16 code units) so that the consumer
// can slice the original source for lexical tokens, and `preserveParens` so
// that the cover grammars of ECMA-262 can be reconstructed.
import { parse } from "./acorn.mjs";

const options = (sourceType) => ({
  ecmaVersion: "latest",
  sourceType: sourceType === "module" ? "module" : "script",
  preserveParens: true,
  allowHashBang: true,
  locations: false,
  ranges: false,
});

// Literal values and cooked template values are dropped: they may contain lone
// surrogates, which cannot survive a UTF-8 pipe, and the consumer recovers the
// token text from the original source anyway. Only the literal *kind* is kept.
const kindOf = (node) => {
  if ("regex" in node) return "regexp";
  if ("bigint" in node) return "bigint";
  if (node.value === null) return "null";
  return typeof node.value;
};

const replacer = (key, value) => {
  if (value === null || typeof value !== "object") return value;
  switch (value.type) {
    case "Literal":
      return {
        type: "Literal",
        start: value.start,
        end: value.end,
        kind: kindOf(value),
      };
    case "TemplateElement":
      return {
        type: "TemplateElement",
        start: value.start,
        end: value.end,
        tail: value.tail,
      };
    default:
      return value;
  }
};

const handle = (input) => {
  try {
    const req = JSON.parse(input);
    const ast = parse(req.code, options(req.sourceType));
    return { ok: true, ast };
  } catch (e) {
    return { ok: false, error: String((e && e.message) || e) };
  }
};

let input = "";
process.stdin.setEncoding("utf8");
process.stdin.on("data", (chunk) => (input += chunk));
process.stdin.on("end", () => {
  process.stdout.write(JSON.stringify(handle(input), replacer) + "\n");
});
