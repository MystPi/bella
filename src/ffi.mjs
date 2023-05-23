import { readFileSync } from 'node:fs';
import { Ok, Error } from './gleam.mjs'

export function readFile(path) {
  try {
    return new Ok(readFileSync(path, 'utf-8'));
  } catch (e) {
    return new Error(e.message);
  }
}

export function getArg(n) {
  const arg = process.argv.slice(2)[n];

  if (arg === undefined) {
    return new Error(undefined);
  }

  return new Ok(arg);
}