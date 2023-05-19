import gleam

pub type Error {
  InvalidText(String)
  Expected(String)
  Unexpected(String)
  EmptyBlock
  RuntimeError(String)
}

pub fn expected(msg: String) {
  gleam.Error(Expected(msg))
}

pub fn unexpected(msg: String) {
  gleam.Error(Unexpected(msg))
}

pub fn runtime_error(msg: String) {
  gleam.Error(RuntimeError(msg))
}
