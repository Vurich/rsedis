use std::error::Error;
use std::fmt;
use std::io;
use std::num::{ParseFloatError, ParseIntError};
use std::str::Utf8Error;

#[derive(Debug)]
pub enum OperationError {
    OverflowError,
    NotANumberError,
    ValueError(String),
    UnknownKeyError,
    WrongTypeError,
    OutOfBoundsError,
    IOError(io::Error),
}

impl fmt::Display for OperationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_string().fmt(f)
    }
}

impl Error for OperationError {
    fn description(&self) -> &str {
        match *self {
            OperationError::WrongTypeError => {
                "WRONGTYPE Operation against a key holding the wrong kind of value"
            }
            OperationError::NotANumberError => "ERR resulting score is not a number (NaN)",
            OperationError::ValueError(ref s) => &*s,
            _ => "ERR",
        }
    }
}

impl From<Utf8Error> for OperationError {
    fn from(_: Utf8Error) -> OperationError {
        OperationError::ValueError("ERR value is not a string".to_owned())
    }
}

impl From<ParseIntError> for OperationError {
    fn from(_: ParseIntError) -> OperationError {
        OperationError::ValueError("ERR value is not a integer".to_owned())
    }
}

impl From<ParseFloatError> for OperationError {
    fn from(_: ParseFloatError) -> OperationError {
        OperationError::ValueError("ERR value is not a float".to_owned())
    }
}

impl From<io::Error> for OperationError {
    fn from(e: io::Error) -> OperationError {
        OperationError::IOError(e)
    }
}
