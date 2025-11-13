#!/usr/bin/env node
import { parse, type Program } from 'acorn';
import fs from 'fs/promises';
import fsSync from 'fs';
import { fileURLToPath } from 'url';
import { resolveArgv } from './arg.util.js';
import { containsTopLevelThis } from './check.this.js';

type Flag = 'help' | 'version' | 'ast';

function printHelp(): void {
  console.log([
    'Usage: check [options] [file]',
    '',
    'Options:',
    '  -h, --help     Show help',
    // '  -v, --version  Show version',
    // '      --ast      Print AST JSON',
    // '',
    // 'If no file is provided or file is "-", reads from stdin.'
  ].join('\n'));
}

function printVersion(): void {
  console.log('unknown');
}


export default async function main(argv: string[] = process.argv.slice(2)): Promise<void> {
  const flag = resolveArgv(argv);

  switch (flag.action) {
    case 'help':
      printHelp();
      return;
    case 'version':
      printVersion();
      return;
    case 'ast':
      // continue to parsing
      break;
  }

  const target = flag.file;
  let code: string;
  if (target === '-') {
    throw new Error('Reading from stdin is not supported yet.');
  } else {
    code = await fs.readFile(target, 'utf8');
  }

  try {
    const ast = parse(code, { ecmaVersion: 5, sourceType: 'script', locations: true });
    const contains = containsTopLevelThis(ast);
    if (contains) throw new Error('Top-level "this" is not allowed in ES5 scripts.');
    console.log('✔ parsed successfully');
  } catch (err: unknown) {
    const message = err instanceof Error ? err.message : String(err);
    console.error('Parse error:', message);
    process.exitCode = 2;
  }
}

const __filename = fileURLToPath(import.meta.url);
let invokedPath: string | undefined;
try { invokedPath = fsSync.realpathSync(process.argv[1] || ''); } catch {}
let selfPath: string | undefined;
try { selfPath = fsSync.realpathSync(__filename); } catch {}

if (invokedPath && selfPath && invokedPath === selfPath) {
  main().catch(err => { console.error(err); process.exit(1); });
}
