import gleam

pub type Error {
  InvalidText(String)
  Expected(String)
  Unexpected(String)
  RuntimeError(String)
}

pub fn invalid_text(msg: String) {
  gleam.Error(InvalidText(msg))
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
