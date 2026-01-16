use std::fmt;
use std::ops::Range;
use std::sync::Arc;

use chumsky::error::Error as ChumskyError;
use chumsky::input::ValueInput;
use chumsky::label::LabelError;
use chumsky::util::MaybeRef;
use chumsky::DefaultExpected;

use itertools::Itertools;
use line_index::{LineCol, LineIndex, TextSize};
use simplicity::hashes::{sha256, Hash, HashEngine};
use simplicity::{elements, Cmr};

use crate::lexer::Token;
use crate::parse::MatchPattern;
use crate::str::{AliasName, FunctionName, Identifier, JetName, ModuleName, WitnessName};
use crate::types::{ResolvedType, UIntType};

pub type Spanned<T> = (T, Span);

/// Area that an object spans inside a file.
///
/// The area cannot be empty.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Span {
    /// Position where the object starts, inclusively.
    pub start: usize,
    /// Position where the object ends, exclusively.
    pub end: usize,
}

impl Span {
    /// A dummy span.
    #[cfg(feature = "arbitrary")]
    pub(crate) const DUMMY: Self = Self::new(0, 0);

    /// Create a new span.
    ///
    /// ## Panics
    ///
    /// Start comes after end.
    pub const fn new(start: usize, end: usize) -> Self {
        assert!(start <= end, "Start cannot come after end");
        Self { start, end }
    }

    /// Return the CMR of the span.
    pub fn cmr(&self) -> Cmr {
        let mut hasher = sha256::HashEngine::default();
        hasher.input(&self.start.to_be_bytes());
        hasher.input(&self.end.to_be_bytes());
        let hash = sha256::Hash::from_engine(hasher);
        Cmr::from_byte_array(hash.to_byte_array())
    }

    /// Return a slice from the given `file` that corresponds to the span.
    pub fn to_slice<'a>(&self, file: &'a str) -> Option<&'a str> {
        file.get(self.start..self.end)
    }
}

impl chumsky::span::Span for Span {
    type Context = ();

    type Offset = usize;

    fn new((): Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {}

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)?;
        Ok(())
    }
}

impl From<chumsky::span::SimpleSpan> for Span {
    fn from(span: chumsky::span::SimpleSpan) -> Self {
        Self {
            start: span.start,
            end: span.end,
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self::new(range.start, range.end)
    }
}

impl From<&str> for Span {
    fn from(s: &str) -> Self {
        if s.is_empty() {
            Span::new(0, 0)
        } else {
            Span::new(0, s.len())
        }
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for Span {
    fn arbitrary(_: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Self::DUMMY)
    }
}

/// Helper trait to convert `Result<T, E>` into `Result<T, RichError>`.
pub trait WithSpan<T> {
    /// Update the result with the affected span.
    fn with_span<S: Into<Span>>(self, span: S) -> Result<T, RichError>;
}

impl<T, E: Into<Error>> WithSpan<T> for Result<T, E> {
    fn with_span<S: Into<Span>>(self, span: S) -> Result<T, RichError> {
        self.map_err(|e| e.into().with_span(span.into()))
    }
}

/// Helper trait to update `Result<A, RichError>` with the affected source file.
pub trait WithFile<T> {
    /// Update the result with the affected source file.
    ///
    /// Enable pretty errors.
    fn with_file<F: Into<Arc<str>>>(self, file: F) -> Result<T, RichError>;
}

impl<T> WithFile<T> for Result<T, RichError> {
    fn with_file<F: Into<Arc<str>>>(self, file: F) -> Result<T, RichError> {
        self.map_err(|e| e.with_file(file.into()))
    }
}

/// An error enriched with context.
///
/// Records _what_ happened and _where_.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct RichError {
    /// The error that occurred.
    error: Error,
    /// Area that the error spans inside the file.
    span: Span,
    /// File in which the error occurred.
    ///
    /// Required to print pretty errors.
    file: Option<Arc<str>>,
}

impl RichError {
    /// Create a new error with context.
    pub fn new(error: Error, span: Span) -> RichError {
        RichError {
            error,
            span,
            file: None,
        }
    }

    /// Add the source file where the error occurred.
    ///
    /// Enable pretty errors.
    pub fn with_file(self, file: Arc<str>) -> Self {
        Self {
            error: self.error,
            span: self.span,
            file: Some(file),
        }
    }

    pub fn error(&self) -> &Error {
        &self.error
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for RichError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.file {
            Some(ref file) if !file.is_empty() => {
                let index = LineIndex::new(file);

                let start_pos = index.line_col(TextSize::from(self.span.start as u32));
                let end_pos = index.line_col(TextSize::from(self.span.end as u32));

                let start_line_index = start_pos.line as usize;
                let end_line_index = end_pos.line as usize;

                let n_spanned_lines = end_line_index.saturating_sub(start_line_index) + 1;
                let line_num_width = (end_line_index + 1).to_string().len();

                writeln!(f, "{:width$} |", " ", width = line_num_width)?;

                let mut lines = file.lines().skip(start_line_index).peekable();
                let start_line_len = lines.peek().map_or(0, |l| l.len());

                for (relative_line_index, line_str) in lines.take(n_spanned_lines).enumerate() {
                    let line_num = start_line_index + relative_line_index + 1;
                    writeln!(f, "{line_num:line_num_width$} | {line_str}")?;
                }

                let line_start_byte = index
                    .offset(LineCol {
                        line: start_pos.line,
                        col: 0,
                    })
                    .map_or(0, |ts| u32::from(ts) as usize);

                let start_col = file[line_start_byte..self.span.start].chars().count() + 1;

                let (underline_start, underline_length) = if start_line_index == end_line_index {
                    let end_col = file[line_start_byte..self.span.end].chars().count() + 1;
                    (start_col, end_col.saturating_sub(start_col))
                } else {
                    (0, start_line_len)
                };

                write!(f, "{:width$} |", " ", width = line_num_width)?;
                write!(f, "{:width$}", " ", width = underline_start)?;
                write!(f, "{:^<width$} ", "", width = underline_length)?;
                write!(f, "{}", self.error)
            }
            _ => write!(f, "{}", self.error),
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct ErrorHandler {
    /// File in which the error occurred.
    file: Arc<str>,

    /// Errors
    errors: Vec<RichError>,
}

impl ErrorHandler {
    pub fn new(file: Arc<str>) -> Self {
        Self {
            file,
            errors: Vec::new(),
        }
    }

    /// Extend existing errors with slice of new errors
    pub fn update(&mut self, errors: &[RichError]) {
        let new_errors = errors
            .iter()
            .map(|err| err.clone().with_file(Arc::clone(&self.file)));

        self.errors.extend(new_errors);
    }

    pub fn get(&self) -> &[RichError] {
        &self.errors
    }
}

impl fmt::Display for ErrorHandler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for err in self.get() {
            writeln!(f, "{err}\n")?;
        }
        Ok(())
    }
}

impl From<ErrorHandler> for String {
    fn from(handler: ErrorHandler) -> Self {
        handler.to_string()
    }
}

impl std::error::Error for RichError {}

impl std::error::Error for ErrorHandler {}

impl From<RichError> for Error {
    fn from(error: RichError) -> Self {
        error.error
    }
}

impl From<RichError> for String {
    fn from(error: RichError) -> Self {
        error.to_string()
    }
}

impl<'src, 'tokens: 'src, I> ChumskyError<'src, I> for RichError
where
    I: ValueInput<'src, Token = Token<'tokens>, Span = Span>,
{
    fn merge(self, other: Self) -> Self {
        match (&self.error, &other.error) {
            (Error::Grammar(_), Error::Grammar(_)) => other,
            (Error::Grammar(_), _) => other,
            (_, Error::Grammar(_)) => self,
            _ => other,
        }
    }
}

impl<'src, 'tokens: 'src, I> LabelError<'src, I, DefaultExpected<'src, Token<'tokens>>>
    for RichError
where
    I: ValueInput<'src, Token = Token<'tokens>, Span = Span>,
{
    fn expected_found<E>(
        expected: E,
        found: Option<MaybeRef<'src, Token<'tokens>>>,
        span: Span,
    ) -> Self
    where
        E: IntoIterator<Item = DefaultExpected<'src, Token<'tokens>>>,
    {
        let expected_tokens: Vec<String> = expected
            .into_iter()
            .map(|t| match t {
                DefaultExpected::Token(maybe) => maybe.to_string(),
                DefaultExpected::Any => "anything".to_string(),
                DefaultExpected::SomethingElse => "something else".to_string(),
                DefaultExpected::EndOfInput => "end of input".to_string(),
                _ => String::new(),
            })
            .collect();

        let found_string = found.map(|t| t.to_string());

        Self {
            error: Error::Syntax {
                expected: expected_tokens,
                label: None,
                found: found_string,
            },
            span,
            file: None,
        }
    }
}

impl<'src, 'tokens: 'src, I> LabelError<'src, I, &'src str> for RichError
where
    I: ValueInput<'src, Token = Token<'tokens>, Span = Span>,
{
    fn expected_found<E>(
        expected: E,
        found: Option<MaybeRef<'src, Token<'tokens>>>,
        span: Span,
    ) -> Self
    where
        E: IntoIterator<Item = &'src str>,
    {
        let expected_strings: Vec<String> = expected.into_iter().map(|s| s.to_string()).collect();
        let found_string = found.map(|t| t.to_string());

        Self {
            error: Error::Syntax {
                expected: expected_strings,
                label: None,
                found: found_string,
            },
            span,
            file: None,
        }
    }

    fn label_with(&mut self, label: &'src str) {
        if let Error::Syntax {
            label: ref mut l, ..
        } = &mut self.error
        {
            *l = Some(label.to_string());
        }
    }
}

/// An individual error.
///
/// Records _what_ happened but not where.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Error {
    ArraySizeNonZero(usize),
    ListBoundPow2(usize),
    BitStringPow2(usize),
    HexStringLen(usize),
    ForWhileWidthPow2(usize),
    CannotParse(String),
    Grammar(String),
    Syntax {
        expected: Vec<String>,
        label: Option<String>,
        found: Option<String>,
    },
    IncompatibleMatchArms(MatchPattern, MatchPattern),
    // TODO: Remove CompileError once SimplicityHL has a type system
    // The SimplicityHL compiler should never produce ill-typed Simplicity code
    // The compiler can only be this precise if it knows a type system at least as expressive as Simplicity's
    CannotCompile(String),
    JetDoesNotExist(JetName),
    InvalidCast(ResolvedType, ResolvedType),
    MainNoInputs,
    MainNoOutput,
    MainRequired,
    FunctionRedefined(FunctionName),
    FunctionUndefined(FunctionName),
    InvalidNumberOfArguments(usize, usize),
    FunctionNotFoldable(FunctionName),
    FunctionNotLoopable(FunctionName),
    ExpressionUnexpectedType(ResolvedType),
    ExpressionTypeMismatch(ResolvedType, ResolvedType),
    ExpressionNotConstant,
    IntegerOutOfBounds(UIntType),
    UndefinedVariable(Identifier),
    UndefinedAlias(AliasName),
    VariableReuseInPattern(Identifier),
    WitnessReused(WitnessName),
    WitnessTypeMismatch(WitnessName, ResolvedType, ResolvedType),
    WitnessReassigned(WitnessName),
    WitnessOutsideMain,
    ModuleRequired(ModuleName),
    ModuleRedefined(ModuleName),
    ArgumentMissing(WitnessName),
    ArgumentTypeMismatch(WitnessName, ResolvedType, ResolvedType),
}

#[rustfmt::skip]
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::ArraySizeNonZero(size) => write!(
                f,
                "Expected a non-negative integer as array size, found {size}"
            ),
            Error::ListBoundPow2(bound) => write!(
                f,
                "Expected a power of two greater than one (2, 4, 8, 16, 32, ...) as list bound, found {bound}"
            ),
            Error::BitStringPow2(len) => write!(
                f,
                "Expected a valid bit string length (1, 2, 4, 8, 16, 32, 64, 128, 256), found {len}"
            ),
            Error::HexStringLen(len) => write!(
                f,
                "Expected an even hex string length (0, 2, 4, 6, 8, ...), found {len}"
            ),
            Error::ForWhileWidthPow2(bit_width) => write!(
                f,
                "Expected a power of two (1, 2, 4, 8, 16, ...) as for-while bit width, found {bit_width}"
            ),
            Error::CannotParse(description) => write!(
                f,
                "Cannot parse: {description}"
            ),
            Error::Grammar(description) => write!(
                f,
                "Grammar error: {description}"
            ),
            Error::Syntax { expected, label, found } => {
                let found_text = found.clone().unwrap_or("end of input".to_string());
                match (label, expected.len()) {
                    (Some(l), _) => write!(f, "Expected {}, found {}", l, found_text),
                    (None, 1) => {
                        let exp_text = expected.first().unwrap();
                        write!(f, "Expected '{}', found '{}'", exp_text, found_text)
                    }
                    (None, 0) => write!(f, "Unexpected {}", found_text),
                    (None, _) => {
                        let exp_text = expected.iter().map(|s| format!("'{}'", s)).join(", ");
                        write!(f, "Expected one of {}, found '{}'", exp_text, found_text)
                    }
                }
            }
            Error::IncompatibleMatchArms(pattern1, pattern2) => write!(
                f,
                "Match arm `{pattern1}` is incompatible with arm `{pattern2}`"
            ),
            Error::CannotCompile(description) => write!(
                f,
                "Failed to compile to Simplicity: {description}"
            ),
            Error::JetDoesNotExist(name) => write!(
                f,
                "Jet `{name}` does not exist"
            ),
            Error::InvalidCast(source, target) => write!(
                f,
                "Cannot cast values of type `{source}` as values of type `{target}`"
            ),
            Error::MainNoInputs => write!(
                f,
                "Main function takes no input parameters"
            ),
            Error::MainNoOutput => write!(
                f,
                "Main function produces no output"
            ),
            Error::MainRequired => write!(
                f,
                "Main function is required"
            ),
            Error::FunctionRedefined(name) => write!(
                f,
                "Function `{name}` was defined multiple times"
            ),
            Error::FunctionUndefined(name) => write!(
                f,
                "Function `{name}` was called but not defined"
            ),
            Error::InvalidNumberOfArguments(expected, found) => write!(
                f,
                "Expected {expected} arguments, found {found} arguments"
            ),
            Error::FunctionNotFoldable(name) => write!(
                f,
                "Expected a signature like `fn {name}(element: E, accumulator: A) -> A` for a fold"
            ),
            Error::FunctionNotLoopable(name) => write!(
                f,
                "Expected a signature like `fn {name}(accumulator: A, context: C, counter u{{1,2,4,8,16}}) -> Either<B, A>` for a for-while loop"
            ),
            Error::ExpressionUnexpectedType(ty) => write!(
                f,
                "Expected expression of type `{ty}`; found something else"
            ),
            Error::ExpressionTypeMismatch(expected, found) => write!(
                f,
                "Expected expression of type `{expected}`, found type `{found}`"
            ),
            Error::ExpressionNotConstant => write!(
                f,
                "Expression cannot be evaluated at compile time"
            ),
            Error::IntegerOutOfBounds(ty) => write!(
                f,
                "Value is out of bounds for type `{ty}`"
            ),
            Error::UndefinedVariable(identifier) => write!(
                f,
                "Variable `{identifier}` is not defined"
            ),
            Error::UndefinedAlias(identifier) => write!(
                f,
                "Type alias `{identifier}` is not defined"
            ),
            Error::VariableReuseInPattern(identifier) => write!(
                f,
                "Variable `{identifier}` is used twice in the pattern"
            ),
            Error::WitnessReused(name) => write!(
                f,
                "Witness `{name}` has been used before somewhere in the program"
            ),
            Error::WitnessTypeMismatch(name, declared, assigned) => write!(
                f,
                "Witness `{name}` was declared with type `{declared}` but its assigned value is of type `{assigned}`"
            ),
            Error::WitnessReassigned(name) => write!(
                f,
                "Witness `{name}` has already been assigned a value"
            ),
            Error::WitnessOutsideMain => write!(
                f,
                "Witness expressions are not allowed outside the `main` function"
            ),
            Error::ModuleRequired(name) => write!(
                f,
                "Required module `{name}` is missing"
            ),
            Error::ModuleRedefined(name) => write!(
                f,
                "Module `{name}` is defined twice"
            ),
            Error::ArgumentMissing(name) => write!(
                f,
                "Parameter `{name}` is missing an argument"
            ),
            Error::ArgumentTypeMismatch(name, declared, assigned) => write!(
                f,
                "Parameter `{name}` was declared with type `{declared}` but its assigned argument is of type `{assigned}`"
            ),
        }
    }
}

impl std::error::Error for Error {}

impl Error {
    /// Update the error with the affected span.
    pub fn with_span(self, span: Span) -> RichError {
        RichError::new(self, span)
    }
}

impl From<elements::hex::Error> for Error {
    fn from(error: elements::hex::Error) -> Self {
        Self::CannotParse(error.to_string())
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(error: std::num::ParseIntError) -> Self {
        Self::CannotParse(error.to_string())
    }
}

impl From<crate::num::ParseIntError> for Error {
    fn from(error: crate::num::ParseIntError) -> Self {
        Self::CannotParse(error.to_string())
    }
}

impl From<simplicity::types::Error> for Error {
    fn from(error: simplicity::types::Error) -> Self {
        Self::CannotCompile(error.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const FILE: &str = "let a1: List<u32, 5> = None;
let x: u32 = Left(
    Right(0)
);";
    const EMPTY_FILE: &str = "";

    #[test]
    fn display_single_line() {
        let error = Error::ListBoundPow2(5)
            .with_span(Span::new(13, 19))
            .with_file(Arc::from(FILE));

        let expected = r#"
  |
1 | let a1: List<u32, 5> = None;
  |              ^^^^^^ Expected a power of two greater than one (2, 4, 8, 16, 32, ...) as list bound, found 5"#;
        assert_eq!(&expected[1..], &error.to_string());
    }

    #[test]
    fn display_multi_line() {
        let error = Error::CannotParse(
            "Expected value of type `u32`, got `Either<Either<_, u32>, _>`".to_string(),
        )
        .with_span(Span::new(41, FILE.len()))
        .with_file(Arc::from(FILE));

        let expected = r#"
  |
2 | let x: u32 = Left(
3 |     Right(0)
4 | );
  | ^^^^^^^^^^^^^^^^^^ Cannot parse: Expected value of type `u32`, got `Either<Either<_, u32>, _>`"#;
        assert_eq!(&expected[1..], &error.to_string());
    }

    #[test]
    fn display_entire_file() {
        let error = Error::CannotParse("This span covers the entire file".to_string())
            .with_span(Span::new(0, FILE.len()))
            .with_file(Arc::from(FILE));

        let expected = r#"
  |
1 | let a1: List<u32, 5> = None;
2 | let x: u32 = Left(
3 |     Right(0)
4 | );
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Cannot parse: This span covers the entire file"#;
        assert_eq!(&expected[1..], &error.to_string());
    }

    #[test]
    fn display_no_file() {
        let error =
            Error::CannotParse("This error has no file".to_string()).with_span(Span::new(0, 0));
        let expected = "Cannot parse: This error has no file";
        assert_eq!(&expected, &error.to_string());

        let error =
            Error::CannotParse("This error has no file".to_string()).with_span(Span::new(5, 10));
        assert_eq!(&expected, &error.to_string());
    }

    #[test]
    fn display_empty_file() {
        let error = Error::CannotParse("This error has an empty file".to_string())
            .with_span(Span::new(0, 0))
            .with_file(Arc::from(EMPTY_FILE));
        let expected = "Cannot parse: This error has an empty file";
        assert_eq!(&expected, &error.to_string());
    }
}
