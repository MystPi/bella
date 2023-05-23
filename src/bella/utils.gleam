pub external fn read_file(String) -> Result(String, String) =
  "../ffi.mjs" "readFile"

pub external fn get_args() -> List(String) =
  "../ffi.mjs" "getArgs"
