use std::error;
use std::fmt;

pub struct Error {
    pub layer: ErrorLayer,
    pub kind: ErrorKind,
    pub error: Option<Box<error::Error + Send + Sync>>,
}

pub enum ErrorLayer {
    // TODO: Process(Position), Exception(Position)
    Runtime,   // runtime will be aborted
    Process,   // process failed, but runtime will not be aborted
    Exception, // exception handleable
}

pub enum ErrorKind {
    Generic,
    NoMatch,
    InvalidType,
    InvalidValue,
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.error, f)
    }
}

impl Error {
    pub fn simple(layer: ErrorLayer, kind: ErrorKind) -> Error {
        Error {
            layer,
            kind,
            error: None,
        }
    }

    pub fn new<E>(layer: ErrorLayer, kind: ErrorKind, error: E) -> Error
    where
        E: Into<Box<error::Error + Send + Sync>>,
    {
        Error {
            layer,
            kind,
            error: Some(error.into()),
        }
    }

    pub fn runtime<E>(kind: ErrorKind, error: E) -> Error
    where
        E: Into<Box<error::Error + Send + Sync>>,
    {
        Error::new(ErrorLayer::Runtime, kind, error)
    }

    pub fn exception<E>(kind: ErrorKind, error: E) -> Error
    where
        E: Into<Box<error::Error + Send + Sync>>,
    {
        Error::new(ErrorLayer::Exception, kind, error)
    }

    pub fn is_exception(&self) -> bool {
        match self.layer {
            ErrorLayer::Exception => true,
            _ => false,
        }
    }

    pub fn get_ref(&self) -> Option<&(error::Error + Send + Sync + 'static)> {
        match self.error {
            None => None,
            Some(ref e) => Some(&**e),
        }
    }
}
