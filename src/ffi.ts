import { toList } from './gleam.mjs';

export function getArgs() {
  return toList(Deno.args);
}

const escapeCodes = {
  '\\': '\\',
  '"': '"',
  "'": "'",
  n: '\n',
  r: '\r',
  t: '\t',
};

export function unescape(string: string) {
  return string.replace(/\\(.)/g, (_, c) => {
    if (c in escapeCodes) {
      return escapeCodes[c as keyof typeof escapeCodes];
    }
    return `\\${c}`;
  });
}

export function stringify(string: string) {
  return JSON.stringify(string);
}
