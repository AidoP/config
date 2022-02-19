use std::{
    borrow::Cow,
    fmt,
    fs::File,
    io::{self, Read, Write},
    ops::Deref,
    path::Path,
};

mod from;
pub use from::FromValue;
pub mod prelude {
    pub use config_macro::Config;
    pub use super::Config;
}

const RED: &'static str = "\x1b[91m";
const BLUE: &'static str = "\x1b[94m";
const ORANGE: &'static str = "\x1b[33m";
const RESET: &'static str = "\x1b[0m";

pub trait Struct {
    fn set<'a>(&mut self, field: &Field<'a>) -> Result<'a, ()>;
}

impl<T> Config for T where T: Struct + Default {}
pub trait Config: Struct + Default {
    /// Load the configuration from all default sources.
    /// 
    /// `name` is used to find files and as a prefix for environment variables and should be one word.
    /// 
    /// `errors` specifies a writer for errors to be pretty-printed to, or stderr if None.
    fn load(name: &str, errors: Option<&mut dyn Write>) -> Self {
        let mut config = Self::default();
        Self::load_into(&mut config, name, errors);
        config
    }
    /// Load the configuration from all default sources into an existing struct
    /// 
    /// See `Config::load()`
    fn load_into(config: &mut Self, name: &str, errors: Option<&mut dyn Write>) {
        let mut stderr = std::io::stderr();
        let errors = if let Some(errors) = errors {
            errors
        } else {
            &mut stderr
        };
        config.load_file(format!("{}.config", name), Some(errors)).ok();
        config.load_env(name, Some(errors));
    }
    fn load_str(&mut self, str: &str, location: &str, errors: Option<&mut dyn Write>) {
        let mut lexer = Lexer::new(str);
        let result = || -> Result<'_, ()> {
            let tokens = lexer.lex()?;
            let mut tokens = Tokens(tokens.as_slice());
            let fields = Fields::parse(&mut tokens)?;
            for field in fields.into_iter() {
                self.set(field)?
            }
            Ok(())
        }();
        if let Err(e) = result {
            if let Some(errors) = errors {
                e.explain(errors, location, &lexer)
            } else {
                e.explain(&mut std::io::stderr(), location, &lexer)
            };
        }
    }
    fn load_file<P: AsRef<Path>>(&mut self, path: P, errors: Option<&mut dyn Write>) -> io::Result<()> {
        let path = path.as_ref();
        let mut file = File::open(path)?;
        let mut source = String::new();
        file.read_to_string(&mut source)?;
        let location = path.display().to_string();
        Ok(self.load_str(&source, &location, errors))
    }
    fn load_env(&mut self, prefix: &str, errors: Option<&mut dyn Write>) {
        let mut stderr = std::io::stderr();
        let errors = if let Some(errors) = errors {
            errors
        } else {
            &mut stderr
        };
        let mut prefix = prefix.to_uppercase();
        // Safety: Only ASCII characters (ie. the same size) are mutated
        unsafe { prefix.as_bytes_mut().iter_mut().for_each(|c| if *c == b' ' { *c = b'_' }) };
        let prefix = prefix + "_";
        fn reconstruct<'a>(key: &'a str, value: &'a str) -> String {
            let mut fields = key.split('_');
            if let Some(field) = fields.next() {
                // TODO: proper string case transformation
                let field = field.to_lowercase();
                let mut input = format!("{}: ", field);
                for field in fields.clone() {
                    let field = field.to_lowercase();
                    input.push_str(&format!("{{ {}: ", field))
                }
                input.push_str(value);
                for _ in fields {
                    input.push_str(" }")
                }
                input
            } else {
                "".into()
            }
        }
        for (key, value) in std::env::vars() {
            if let Some(stripped) = key.strip_prefix(&prefix) {
                let source = reconstruct(stripped, &value);
                let mut lexer = Lexer::new(&source);
                let result = || -> Result<'_, ()> {
                    let tokens = lexer.lex()?;
                    let mut tokens = Tokens(&tokens);
                    self.set(&Field::parse(&mut tokens)?)
                }();
                if let Err(e) = result {
                    e.explain(errors, &format!("environment variable {}", key), &lexer)
                }
            }
        }
    }
}

pub struct Lexer<'a> {
    source: &'a str,
    str: &'a str
}
impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            str: source
        }
    }
    pub fn lex(&mut self) -> Result<'a, Vec<Token<'a>>> {
        let mut tokens = Vec::new();
        while self.has_content() {
            tokens.push(Token::lex(self)?)
        }
        Ok(tokens)
    }
    fn has_content(&mut self) -> bool {
        while self.trim_start() {}
        self.str.len() > 0
    }
    /// Remove whitespace and a comment from the start of str
    /// Returns true if a comment was removed and the function should be recalled
    fn trim_start(&mut self) -> bool {
        self.str = self.str.trim_start();
        let mut chars = self.char_indices();
        if let Some((_, '#')) = chars.next() {
            if let Some((i, c)) = chars.find(|&(_, c)| c == '\n') {
                self.str = &self.str[i+c.len_utf8()..];
            } else {
                self.str = &self.str[self.str.len()..];
            }
            true
        } else {
            false
        }
    }
    /// Get the byte index offset of the tracking string from the source string
    fn offset(&self) -> usize {
        // Safety: str is always derived from source and are always valid as they are references
        unsafe { self.str.as_ptr().offset_from(self.source.as_ptr()) as usize }
    }
    fn char_indices(&self) -> std::str::CharIndices<'a> {
        self.str.char_indices()
    }
    fn split_at(&mut self, i: usize) -> Spanned<'a> {
        let (str, rest) = self.str.split_at(i);
        let starts = self.offset();
        self.str = rest;
        Spanned {
            str,
            starts
        }
    }
    fn here(&self) -> Spanned<'a> {
        Spanned {
            str: &self.str[..1.min(self.str.len())],
            starts: self.offset()
        }
    }
    fn to_end(&self) -> Spanned<'a> {
        Spanned {
            str: self.str,
            starts: self.offset()
        }
    }
    fn end(&self) -> Spanned<'a> {
        Spanned {
            str: &self.source[self.source.len()..],
            starts: self.source.len()
        }
    }
    /// Construct a span that includes the range between the two given spans
    /// 
    /// If either of the spans are not from this lexer the behaviour is unspecified.
    /// 
    /// # Panics
    /// Panics if `from` spans beyond `to`
    fn between(&self, from: Spanned<'a>, to: Spanned<'a>) -> Spanned<'a> {
        let starts = from.starts;
        let ends = to.starts + to.str.len();
        Spanned {
            str: &self.source[starts..ends],
            starts
        }
    }
    pub fn position(&self, span: Spanned<'a>) -> (usize, usize) {
        let (mut col, mut row) = (0, 0);
        let chars = self.source.char_indices().take_while(|&(i, _)| i <= span.starts);
        for (_, char) in chars {
            match char {
                '\n' => {
                    row += 1;
                    col = 0;
                }
                _ => col += 1
            }
        }
        (row + 1, col)
    }
    pub fn lines(&self, span: Spanned<'a>) -> Vec<Spanned<'a>> {
        let mut lines = Vec::new();
        let mut start = self.source[..span.starts].char_indices().rev().find_map(|(i, c)| (c == '\n').then(|| i + c.len_utf8())).unwrap_or(0);
        let mut end = self.source[span.starts..].char_indices().find_map(|(i, c)| (c == '\n').then(|| i + span.starts)).unwrap_or(self.source.len());

        loop {
            lines.push(Spanned {
                str: &self.source[start..end],
                starts: start
            });

            if span.starts + span.len() <= end {
                break lines
            }

            start = end + 1;
            end = self.source[start..].char_indices().find_map(|(i, c)| (c == '\n').then(|| i + start)).unwrap_or(self.source.len());
        }
    }
    pub fn context(&self, span: Spanned<'a>, line_num: usize) -> String {
        let lines = self.lines(span);
        let mut coloured_lines = Vec::with_capacity(lines.len());

        let mut first_line = lines[0].str.to_string();
        first_line.insert_str(span.starts - lines[0].starts, RED);
        if lines.len() == 1 {
            first_line.insert_str(((span.starts + span.str.len()) - lines[0].starts) + RED.len(), RESET);
        }
        coloured_lines.push(first_line);
        if lines.len() > 1 {
            for line in &lines[1..lines.len()-1] {
                let mut line= line.str.to_string();
                line.insert_str(0, RED);
                coloured_lines.push(line)
            }
            let last = lines.last().unwrap();
            let mut last_line = last.str.to_string();
            last_line.insert_str((span.starts + span.str.len()) - last.starts, RESET);
            last_line.insert_str(0, RED);
            coloured_lines.push(last_line.to_string())
        }

        let from_arrow_padding = self.source[lines[0].starts..span.starts].chars().count() + 1;
        let to_arrow_padding = self.source[lines.last().unwrap().starts..span.starts + span.str.len()].chars().count();
        let num_padding = format!("{}", line_num + lines.len()).chars().count();
        let mut context = format!(" {1:<0$} | {3:>2$}", num_padding, ' ', from_arrow_padding, 'v');
        for (num, line) in coloured_lines.into_iter().enumerate() {
            let num = line_num + num;
            context.push_str(&format!("\n {1:<0$} | {2}{3}", num_padding, num, line, RESET))
        }
        context.push_str(&format!("\n {1:<0$} | {3:>2$}", num_padding, ' ', to_arrow_padding, '^'));

        context
    }
}
#[macro_export]
macro_rules! identifier {
    ($identifier:expr) => {
        $crate::Value::Ident($crate::Spanned { str: $identifier, ..})
    };
}
#[derive(Debug, Clone, Copy)]
pub struct Spanned<'a> {
    pub str: &'a str,
    /// Byte index that the string spans from in the parent string
    starts: usize
}
impl<'a> Deref for Spanned<'a> {
    type Target = str;
    fn deref(&self) -> &'a Self::Target {
        &self.str
    }
}
impl<'a> fmt::Display for Spanned<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.str)
    }
}
struct Tokens<'a, 'b>(&'b [Token<'a>]);
impl<'a, 'b> Tokens<'a, 'b> {
    /// Pop a token from the front of the list
    fn pop(&mut self) -> Option<Token<'a>> {
        if let Some((first, rest)) = self.0.split_first() {
            self.0 = rest;
            Some(*first)
        } else {
            None
        }
    }
    /// Combines Tokens::pop() and Token::is()
    fn pop_is(&mut self, ty: TokenType) -> Result<'a, Spanned<'a>> {
        if let Some(token) = self.pop() {
            token.is(ty)
        } else {
            Err(Error::ExpectedToken(None, ty))
        }
    }
    /// Peek the next token
    fn peek(&self) -> Option<Token<'a>> {
        self.0.first().copied()
    }
    /* Requires unstable
    /// Take the next N elements from the front of the list
    fn take<const N: usize>(&mut self) -> Option<[Token<'a>; N]> {
        if self.0.len() < N {
            None
        } else {
            let (array, rest) = self.0.split_array_ref();
            self.0 = rest;
            Some(*array)
        }
    }*/
}
impl<'a, 'b> Deref for Tokens<'a, 'b> {
    type Target = [Token<'a>];
    fn deref(&self) -> &'b Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    span: Spanned<'a>,
    ty: TokenType
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Escaped,
    Ident,
    Assignment,
    Seperator,
    ArrayOpen,
    ArrayClose,
    StructOpen,
    StructClose
}
impl<'a> Token<'a> {
    const ESCAPE: char = '"';
    const ASSIGNMENT: char = ':';
    const SEPERATOR: char = ',';
    const STRUCT_OPEN: char = '{';
    const STRUCT_CLOSE: char = '}';
    const ARRAY_OPEN: char = '[';
    const ARRAY_CLOSE: char = ']';
    const RESERVED: &'static [char] = &[Self::ESCAPE, Self::ASSIGNMENT, Self::SEPERATOR, Self::STRUCT_OPEN, Self::STRUCT_CLOSE, Self::ARRAY_OPEN, Self::ARRAY_CLOSE];
    fn lex(lexer: &mut Lexer<'a>) -> Result<'a, Self> {
        let mut chars = lexer.char_indices().peekable();
        fn is_ident(char: char) -> bool {
            !(Token::RESERVED.contains(&char) || char.is_whitespace())
        }
        match chars.next().ok_or(Error::EndOfFile(lexer.to_end()))? {
            // Ident token
            (mut i, c) if is_ident(c) => {
                i += c.len_utf8();
                if let Some((end, c)) = chars.take_while(|&(_, c)| is_ident(c)).last() {
                    i = end + c.len_utf8();
                }
                Ok(Self {
                    span: lexer.split_at(i),
                    ty: TokenType::Ident
                })
            },
            // Escaped token
            (_, '"') => {
                let mut escaped = false;
                if let Some((end, c)) = chars.find(|&(_, c)| {
                    let ret = c == '"' && !escaped;
                    escaped = c == '\\' && !escaped;
                    ret
                }) {
                    Ok(Self {
                        span: lexer.split_at(end + c.len_utf8()),
                        ty: TokenType::Escaped
                    })
                } else {
                    Err(Error::EndOfFile(lexer.to_end()))
                }
            }
            (i, Self::ASSIGNMENT) => Ok(Self {
                span: lexer.split_at(i + Self::ASSIGNMENT.len_utf8()),
                ty: TokenType::Assignment
            }),
            (i, Self::SEPERATOR) => Ok(Self {
                span: lexer.split_at(i + Self::SEPERATOR.len_utf8()),
                ty: TokenType::Seperator
            }),
            (i, Self::STRUCT_OPEN) => Ok(Self {
                span: lexer.split_at(i + Self::STRUCT_OPEN.len_utf8()),
                ty: TokenType::StructOpen
            }),
            (i, Self::STRUCT_CLOSE) => Ok(Self {
                span: lexer.split_at(i + Self::STRUCT_CLOSE.len_utf8()),
                ty: TokenType::StructClose
            }),
            (i, Self::ARRAY_OPEN) => Ok(Self {
                span: lexer.split_at(i + Self::ARRAY_OPEN.len_utf8()),
                ty: TokenType::ArrayOpen
            }),
            (i, Self::ARRAY_CLOSE) => Ok(Self {
                span: lexer.split_at(i + Self::ARRAY_CLOSE.len_utf8()),
                ty: TokenType::ArrayClose
            }),
            _ => Err(Error::InvalidInput(lexer.here()))
        }
    }
    /// Return the inner spanned or return Error::ExpectedToken if of the wrong token type
    pub fn is(self, ty: TokenType) -> Result<'a, Spanned<'a>> {
        if ty == self.ty {
            Ok(self.span)
        } else {
            Err(Error::ExpectedToken(Some(self), ty))
        }
    }
}
impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident => write!(f, "identifier"),
            Self::Assignment => write!(f, ":"),
            Self::Escaped => write!(f, "string"),
            Self::Seperator => write!(f, ","),
            Self::ArrayOpen => write!(f, "["),
            Self::ArrayClose => write!(f, "]"),
            Self::StructOpen => write!(f, "{{"),
            Self::StructClose => write!(f, "}}")
        }
    }
}

#[derive(Debug, Clone)]
pub struct Field<'a> {
    field: Value<'a>,
    _assignment: Spanned<'a>,
    value: Value<'a>,
    _seperator: Option<Spanned<'a>>
}
impl<'a> Field<'a> {
    fn parse(tokens: &mut Tokens<'a, '_>) -> Result<'a, Self> {
        Ok(Self {
            field: Value::parse(tokens)?,
            _assignment: tokens.pop_is(TokenType::Assignment)?,
            value: Value::parse(tokens)?,
            _seperator: tokens.peek().map(|t| if t.ty == TokenType::Seperator { tokens.pop().map(|t| t.span) } else { None }).flatten()
        })
    }
    pub fn span(&self, lexer: &Lexer<'a>) -> Spanned<'a> {
        lexer.between(self.field.span(lexer), self.value.span(lexer))
    }
    pub fn field(&self) -> &Value<'a> {
        &self.field
    }
    pub fn value(&self) -> &Value<'a> {
        &self.value
    }
}
#[derive(Debug, Clone)]
pub struct Fields<'a>(Vec<Field<'a>>);
impl<'a> Fields<'a> {
    fn parse(tokens: &mut Tokens<'a, '_>) -> Result<'a, Self> {
        let mut fields = Vec::new();
        while tokens.peek().map(|t| t.ty == TokenType::Ident).unwrap_or(false) {
            fields.push(Field::parse(tokens)?)
        }
        Ok(Self(fields))
    }
}
impl<'a> Deref for Fields<'a> {
    type Target = [Field<'a>];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
#[derive(Debug, Clone)]
pub enum Value<'a> {
    /// A string with escaped values
    Escaped(Spanned<'a>),
    Ident(Spanned<'a>),
    Array {
        open: Spanned<'a>,
        values: Values<'a>,
        close: Spanned<'a>
    },
    Struct {
        open: Spanned<'a>,
        fields: Fields<'a>,
        close: Spanned<'a>
    }
}
#[derive(Debug, Clone)]
pub struct SeperatedValue<'a> {
    value: Value<'a>,
    _seperator: Option<Spanned<'a>>
}
impl<'a> SeperatedValue<'a> {
    fn parse(tokens: &mut Tokens<'a, '_>) -> Result<'a, Self> {
        Ok(Self {
            value: Value::parse(tokens)?,
            _seperator: tokens.peek().map(|t| if t.ty == TokenType::Seperator { tokens.pop().map(|t| t.span) } else { None }).flatten()
        })
    }
}
impl<'a> Deref for SeperatedValue<'a> {
    type Target = Value<'a>;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
#[derive(Debug, Clone)]
pub struct Values<'a>(Vec<SeperatedValue<'a>>);
impl<'a> Values<'a> {
    fn parse(tokens: &mut Tokens<'a, '_>) -> Result<'a, Self> {
        let mut values = Vec::new();
        while tokens.peek().map(|t| t.ty != TokenType::ArrayClose).unwrap_or(false) {
            values.push(SeperatedValue::parse(tokens)?)
        }
        Ok(Self(values))
    }
}
impl<'a> Value<'a> {
    fn parse(tokens: &mut Tokens<'a, '_>) -> Result<'a, Self> {
        match tokens.pop().ok_or(Error::ExpectedValue(None))? {
            Token { ty: TokenType::Ident, span } => Ok(Self::Ident(span)),
            Token { ty: TokenType::Escaped, span } => Ok(Self::Escaped(span)),
            Token { ty: TokenType::ArrayOpen, span: open } => {
                Ok(Self::Array {
                    open,
                    values: Values::parse(tokens)?,
                    close: tokens.pop_is(TokenType::ArrayClose)?
                })
            },
            Token { ty: TokenType::StructOpen, span: open } => {
                Ok(Self::Struct {
                    open,
                    fields: Fields::parse(tokens)?,
                    close: tokens.pop_is(TokenType::StructClose)?
                })
            },
            token => Err(Error::ExpectedValue(Some(token)))
        }
    }
    pub fn clean(&self, expected: DataType) -> Result<'a, Cow<'a, str>> {
        match self {
            Self::Ident(Spanned { str, ..}) => Ok(Cow::Borrowed(str)),
            Self::Escaped(Spanned { str, starts }) => {
                let str = &str[1..str.len()-1];
                // Most strings won't have escape characters
                if !str.contains('\\') {
                    Ok(Cow::Borrowed(str))
                } else {
                    // The string will have just slightly less characters than the original
                    let mut string = String::with_capacity(str.len());
                    let mut chars = str.char_indices();
                    while let Some((i, c)) = chars.next() {
                        if c == '\\' {
                            match chars.next() {
                                Some((_, '\\')) => string.push('\\'),
                                Some((_, '"')) => string.push('"'),
                                Some((_, 'n')) => string.push('\n'),
                                Some((_, 'r')) => string.push('\r'),
                                Some((ni, nc)) => return Err(Error::InvalidEscapeSequence(Spanned { str: &str[i..i+1+nc.len_utf8()], starts: starts + ni })),
                                None => return Err(Error::InvalidEscapeSequence(Spanned { str: &str[i..], starts: starts + i + 1 }))
                            }
                        } else {
                            string.push(c)
                        }
                    }
                    Ok(Cow::Owned(string))
                }
            },
            value => Err(Error::InvalidType(value.clone(), expected)),
        }
    }
    fn span(&self, lexer: &Lexer<'a>) -> Spanned<'a> {
        match self {
            Self::Ident(span) => *span,
            Self::Escaped(span) => *span,
            Self::Array {open, close, ..} => lexer.between(*open, *close),
            Self::Struct {open, close, ..} => lexer.between(*open, *close)
        }
    }
    /// Attempts to interpret the number in the form `[+|-][0(x|b|o)]\d*`, splitting into (sign, radix, digits) parts
    fn int_parts(str: &str) -> (bool, u32, &str) {
        match str.as_bytes() {
            [sign @ (b'-' | b'+'), b'0', radix @ (b'x' | b'o' | b'b'), ..] => (
                *sign == b'-',
                match *radix {
                    b'x' => 16,
                    b'o' => 8,
                    b'b' => 2,
                    _ => unreachable!()
                },
                &str[3..]
            ),
            [b'0', radix @ (b'x' | b'o' | b'b'), ..] =>  (
                false,
                match *radix {
                    b'x' => 16,
                    b'o' => 8,
                    b'b' => 2,
                    _ => unreachable!()
                },
                &str[2..]
            ),
            [sign @ (b'-' | b'+'), ..] =>  (*sign == b'-', 10, &str[1..]),
            _ => (false, 10, str)
        }
    }
}
impl<'a> Deref for Values<'a> {
    type Target = [SeperatedValue<'a>];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub enum DataType {
    Wrapper(&'static str, Box<DataType>),
    Named(&'static str),
    Array(usize, Box<DataType>),
    Dictionary(Box<DataType>, Box<DataType>)
}
impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Wrapper(prefix, wrapped) => write!(f, "{colour}{} {}{reset}", prefix, wrapped, colour=BLUE, reset=RESET),
            Self::Named(name) => write!(f, "{colour}{}{reset}", name, colour=BLUE, reset=RESET),
            Self::Array(size, ty) => write!(f, "[{}; {colour}{}{reset}]", ty, size, colour=ORANGE, reset=RESET),
            Self::Dictionary(keys, values) => write!(f, "dictionary with {} {colour}keys{reset} and {} {colour}values{reset}", keys, values, colour=ORANGE, reset=RESET)
        }
    }
}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;
#[derive(Debug)]
pub enum Error<'a> {
    InvalidInput(Spanned<'a>),
    InvalidEscapeSequence(Spanned<'a>),
    EndOfFile(Spanned<'a>),
    ExpectedToken(Option<Token<'a>>, TokenType),
    /// Like expected token, but many tokens are valid
    ExpectedValue(Option<Token<'a>>),
    InvalidType(Value<'a>, DataType),
    NoDefault(Value<'a>, DataType),
    NoField(Field<'a>, DataType),
    MissingField(Value<'a>, &'static str, DataType),
    NoNew(Value<'a>, DataType),
    Unavailable(Value<'a>)
}
impl<'a> Error<'a> {
    // It doesn't matter writing fails here
    #[allow(unused_must_use)]
    pub fn explain(self, writer: &mut dyn Write, location: &str, lexer: &Lexer) {
        let span = self.span(lexer);
        let (line, col) = lexer.position(span);
        writeln!(writer, "error: Configuration invalid\nin {} @ Line {}, Column {}\n{}", location, line, col, lexer.context(span, line));
        match self {
            Self::InvalidInput(_) => writeln!(writer, "No token expects this input"),
            Self::InvalidEscapeSequence(seq) => writeln!(writer, "Escape sequence '{}' is invalid. Perhaps you meant '\\\\'?", seq.str),
            Self::EndOfFile(_) => writeln!(writer, "The token is incomplete"),
            Self::ExpectedToken(Some(got), expected) => writeln!(writer, "Expected '{}', got '{}'", expected, got.ty),
            Self::ExpectedToken(None, expected) => writeln!(writer, "Expected '{}' but input ended", expected),
            Self::ExpectedValue(Some(got)) => writeln!(writer, "Expected a value, got '{}'", got.ty),
            Self::ExpectedValue(None) => writeln!(writer, "Expected a value but input ended"),
            Self::InvalidType(_, expected) => writeln!(writer, "This is not a valid '{}'", expected),
            Self::NoDefault(_, ty) => writeln!(writer, "Expected a value of type '{}' which cannot be emptied", ty),
            Self::NoField(_, structure) => writeln!(writer, "'{}' does not contain this field", structure),
            Self::MissingField(_, field, structure) => writeln!(writer, "'{}' requires field '{}' to be specified to create a new instance", structure, field),
            Self::NoNew(_, ty) => writeln!(writer, "New instances of '{}' cannot be created", ty),
            Self::Unavailable(_) => writeln!(writer, "Field is inaccessible")
        };
        writeln!(writer);
    }
    fn span(&self, lexer: &Lexer<'a>) -> Spanned<'a> {
        match self {
            Self::InvalidInput(span) => *span,
            Self::InvalidEscapeSequence(span) => *span,
            Self::EndOfFile(span) => *span,
            Self::ExpectedToken(Some(t), _) => t.span,
            Self::ExpectedToken(None, _) => lexer.end(),
            Self::ExpectedValue(Some(t)) => t.span,
            Self::ExpectedValue(None) => lexer.end(),
            Self::InvalidType(value, _) => value.span(lexer),
            Self::NoDefault(none, _) => none.span(lexer),
            Self::NoField(field, _) => field.span(lexer),
            Self::MissingField(value, _, _) => value.span(lexer),
            Self::NoNew(value, _) => value.span(lexer),
            Self::Unavailable(value) => value.span(lexer)
        }
    }
}