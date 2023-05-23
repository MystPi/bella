pub external fn read_file(String) -> Result(String, String) =
  "./ffi.mjs" "readFile"

pub external fn get_arg(Int) -> Result(String, Nil) =
  "./ffi.mjs" "getArg"
