
type Flag = {
  action: 'help' | 'version' | 'ast';
  file: string;
};

export function resolveArgv(argv: string[]): Flag {
const args = [...argv];
  const flags = new Set<Flag['action']>();
  const files: string[] = [];
  for (const a of args) {
    if (a === '--help' || a === '-h') flags.add('help');
    else if (a === '--version' || a === '-v') flags.add('version');
    else if (a === '--ast') flags.add('ast');
    else files.push(a);
  }

  if (flags.has('help')) {
    return { action: 'help', file: files[0] || '-' };
  }

  if (flags.has('version')) {
    return { action: 'version', file: files[0] || '-' };
  }

  return { action: 'ast', file: files[0] || '-' };

};