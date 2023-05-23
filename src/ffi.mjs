import { readFileSync } from 'node:fs';
import { Ok, Error, toList } from './gleam.mjs'

export function readFile(path) {
  try {
    return new Ok(readFileSync(path, 'utf-8'));
  } catch (e) {
    return new Error(e.message);
  }
}

export function getArgs() {
  return toList(process.argv.slice(2));
}