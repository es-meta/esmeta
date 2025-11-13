import type { Program } from "acorn";
import * as walk from "acorn-walk";

export function containsTopLevelThis(ast: Program): boolean {
  let has = false;

  // Tracks scopes that define their own `this`
  // Program scope is the bottom of the stack and has no own `this`.
  type Scope = { hasOwnThis: boolean };
  const scopeStack: Scope[] = [{ hasOwnThis: false }];

  function pushScope(hasOwnThis: boolean) {
    scopeStack.push({ hasOwnThis });
  }
  function popScope() {
    scopeStack.pop();
  }

  walk.ancestor<unknown>(ast, {
    ThisExpression(node, state, ancestors) {

      const containsAtLeastOneScope = ancestors.some((node, index) => {
        return node.type === 'FunctionDeclaration' ||
          node.type === 'FunctionExpression' ||
          node.type === 'ClassDeclaration' ||
          node.type === 'ClassExpression';
        // 'ArrowFunctionExpression' does NOT create own `this`
      })

      if (!containsAtLeastOneScope) {
        has = true;
      }
    },
  });

  return has;
}