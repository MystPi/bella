import { Ok, Error, toList } from './gleam.mjs';

export function readFile(path: string) {
  try {
    return new Ok(Deno.readTextFileSync(path));
  } catch (e) {
    return new Error(e.message);
  }
}

export function writeFile(path: string, content: string) {
  try {
    Deno.writeTextFileSync(path, content);
    return new Ok(path);
  } catch (e) {
    return new Error(e.message);
  }
}

export function createDirectory(path: string) {
  try {
    Deno.mkdirSync(path, { recursive: true });
    return new Ok(path);
  } catch (e) {
    return new Error(e.message);
  }
}

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
