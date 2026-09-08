use std::fmt;

use crate::assemble::SourceSpan;

/// The severity level of a diagnostic message.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    /// Represents an assembler warning.
    Warning,
    /// Represents an assembler error.
    Error,
}

/// An assembler diagnostic.
#[derive(Clone, Debug)]
pub struct Diagnostic {
    level: Severity,
    span: SourceSpan,
    message: String,
}

impl Diagnostic {
    /// Constructs a new diagnostic from its components.
    pub fn new(level: Severity, span: SourceSpan, message: impl ToString) -> Self {
        Self {
            level,
            span,
            message: message.to_string(),
        }
    }

    /// Creates a new warning diagnostic from span and message.
    pub fn warning(span: SourceSpan, message: impl ToString) -> Self {
        Self::new(Severity::Warning, span, message)
    }

    /// Creates a new error diagnostic from span and message.
    pub fn error(span: SourceSpan, message: impl ToString) -> Self {
        Self::new(Severity::Error, span, message)
    }

    /// Returns an object wrapping a diagnostic and its original
    /// source string with a [`Display`][fmt::Display] implementation.
    pub fn display<'src>(&self, source: &'src str) -> DisplayDiagnostic<'_, 'src> {
        DisplayDiagnostic {
            diagnostic: self,
            source,
        }
    }
}

/// Wraps a diagnostic and its original source string.
///
/// This type implements [`Display`][fmt::Display] for formatting
/// diagnostics into human-readable output.
pub struct DisplayDiagnostic<'a, 'src> {
    diagnostic: &'a Diagnostic,
    source: &'src str,
}

impl fmt::Display for DisplayDiagnostic<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}
