use std::{fmt, io::{self, Read, Write}, ops::Deref, path::Path, fs::File, str::FromStr, collections::HashMap};

pub mod prelude {
    pub use config_macro::Config;
    pub use super::Config;
}

pub trait Config: Default {
    /// The name of the configuration, used to find files and as the env vars prefix
    const NAME: &'static str;
    fn set<'a>(&mut self, field: &Field<'a>) -> Result<'a, ()>;
    /// Load the configuration from all default sources
    /// 
    /// All configuration sources will be attempted to read from, regardless of previous errors.
    /// If an error occurs while reading a configuration source it will be printed to stderr and Err(Self) will return.
    /// Configuration sources that cannot be found do not result in an error.
    fn load(errors: Option<&mut dyn Write>) -> Self {
        let mut config = Self::default();
        config.load_file(format!("{}.config", Self::NAME), errors).ok();
        
        config
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
        (row + 1, col + 1)
    }
    pub fn line(&self, span: Spanned<'a>) -> (&'a str, usize) {
        let mut line = self.source;
        let mut starts = 0;
        let mut last = (0, ' ');
        for (i, c) in self.source.char_indices() {
            if last.1 == '\n' {
                if i <= span.starts {
                    starts = last.0 + c.len_utf8();
                    line = &self.source[i..]
                } else {
                    return (&line[..last.0-starts], span.starts-starts)
                }
            }
            last = (i, c);
        }
        (line, span.starts-starts)
    }
    pub fn context(&self, span: Spanned<'a>, line_num: usize) -> String {
        let (line, before) = self.line(span);
        let mut underline = String::with_capacity(line.len());
        for _ in 0..before { underline.push(' ') }
        for _ in 0..span.str.len() { underline.push('^') }
        let line_num = format!(" {} ", line_num);
        let padding = " ".repeat(line_num.len());

        format!("{padding}|\n{line_num}| {}\n{padding}| {}", line, underline, padding=padding, line_num=line_num)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Spanned<'a> {
    str: &'a str,
    /// Byte index that the string spans from in the parent string
    starts: usize
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
    Ident,
    Assignment,
    String,
    Number,
    Seperator,
    ArrayOpen,
    ArrayClose,
    StructOpen,
    StructClose
}
impl<'a> Token<'a> {
    const ASSIGNMENT: char = ':';
    const SEPERATOR: char = ',';
    const STRUCT_OPEN: char = '{';
    const STRUCT_CLOSE: char = '}';
    const ARRAY_OPEN: char = '[';
    const ARRAY_CLOSE: char = ']';
    fn lex(lexer: &mut Lexer<'a>) -> Result<'a, Self> {
        let mut chars = lexer.char_indices().peekable();
        match chars.next().ok_or(Error::EndOfFile(lexer.to_end()))? {
            // Ident token
            (mut i, c) if c.is_alphabetic() => {
                i += c.len_utf8();
                if let Some((end, c)) = chars.take_while(|(_, c)| c.is_alphanumeric()).last() {
                    i = end + c.len_utf8();
                }
                Ok(Self {
                    span: lexer.split_at(i),
                    ty: TokenType::Ident
                })
            },
            // String token
            (_, '"') => {
                if let Some((end, c)) = chars.find(|&(_, c)| c == '"') {
                    Ok(Self {
                        span: lexer.split_at(end + c.len_utf8()),
                        ty: TokenType::String
                    })
                } else {
                    Err(Error::EndOfFile(lexer.to_end()))
                }
            }
            // Number token
            (mut i, mut c) if c.is_ascii_digit() || c == '-' || c == '+' => {
                if c == '+' || c == '-' {
                    if let Some((ni, nc)) = chars.next() {
                        i = ni;
                        c = nc;
                    } else {
                        return Err(Error::EndOfFile(lexer.to_end()))
                    }
                }
                if c == '0' {
                    if let Some(&(ni, nc)) = chars.peek() {
                        i = ni;
                        c = nc;
                        chars.next();
                        // Following 0x, 0o and 0b must be a digit
                        if let Some(&(ni, nc)) = chars.peek() {
                            if !nc.is_ascii_digit() {
                                lexer.split_at(ni + nc.len_utf8());
                                return Err(Error::InvalidInput(lexer.here()))
                            }
                        }
                    }
                }
                i += c.len_utf8();
                let mut had_dot = false;
                if let Some((end, c)) = chars.take_while(|&(_, c)| c.is_ascii_digit() ||
                    if !had_dot {
                        if c == '.' {
                            had_dot = true;
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                ).last() {
                    i = end + c.len_utf8();
                }
                Ok(Self {
                    span: lexer.split_at(i),
                    ty: TokenType::Number
                })
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
            Self::String => write!(f, "string"),
            Self::Number => write!(f, "number"),
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
    ident: Spanned<'a>,
    _assignment: Spanned<'a>,
    value: Value<'a>,
    _seperator: Option<Spanned<'a>>
}
impl<'a> Field<'a> {
    fn parse(tokens: &mut Tokens<'a, '_>) -> Result<'a, Self> {
        Ok(Self {
            ident: tokens.pop_is(TokenType::Ident)?,
            _assignment: tokens.pop_is(TokenType::Assignment)?,
            value: Value::parse(tokens)?,
            _seperator: tokens.peek().map(|t| if t.ty == TokenType::Seperator { tokens.pop().map(|t| t.span) } else { None }).flatten()
        })
    }
    pub fn span(&self, lexer: &Lexer<'a>) -> Spanned<'a> {
        lexer.between(self.ident, self.value.span(lexer))
    }
    pub fn name(&self) -> &'a str {
        self.ident.str
    }
    pub fn value(&self) -> &Value<'a> {
        &self.value
    }
    pub fn destructure(self) -> (&'a str, Value<'a>) {
        let Self { ident, value, ..} = self;
        (ident.str, value)
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
    String(Spanned<'a>),
    Number(Spanned<'a>),
    Default(Spanned<'a>),
    None(Spanned<'a>),
    Bool(Spanned<'a>),
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
pub struct Values<'a>(Vec<Value<'a>>);
impl<'a> Values<'a> {
    fn parse(tokens: &mut Tokens<'a, '_>) -> Result<'a, Self> {
        let mut values = Vec::new();
        while tokens.peek().map(|t| Value::is_start(t)).unwrap_or(false) {
            values.push(Value::parse(tokens)?)
        }
        Ok(Self(values))
    }
}
impl<'a> Value<'a> {
    fn parse(tokens: &mut Tokens<'a, '_>) -> Result<'a, Self> {
        match tokens.pop().ok_or(Error::ExpectedValue(None))? {
            Token { ty: TokenType::String, span } => Ok(Self::String(span)),
            Token { ty: TokenType::Number, span } => Ok(Self::Number(span)),
            Token { ty: TokenType::Ident, span: span @ Spanned { str: "default", ..} } => Ok(Self::Default(span)),
            Token { ty: TokenType::Ident, span: span @ Spanned { str: "none", ..} } => Ok(Self::None(span)),
            Token { ty: TokenType::Ident, span: span @ Spanned { str: "true" | "false", ..} } => Ok(Self::Bool(span)),
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
    /// Returns true if the token may start, or be, a new value
    fn is_start(token: Token) -> bool {
        match token {
            Token { ty: TokenType::String, ..} => true,
            Token { ty: TokenType::Number, ..} => true,
            Token { ty: TokenType::Ident, span: Spanned { str: "default", ..} } => true,
            Token { ty: TokenType::Ident, span: Spanned { str: "true" | "false", ..} } => true,
            Token { ty: TokenType::ArrayOpen, ..} => true,
            Token { ty: TokenType::StructOpen, ..} => true,
            _ => false
        }
    }
    fn span(&self, lexer: &Lexer<'a>) -> Spanned<'a> {
        match self {
            Self::String(span) => *span,
            Self::Number(span) => *span,
            Self::Default(span) => *span,
            Self::None(span) => *span,
            Self::Bool(span) => *span,
            Self::Array {open, close, ..} => lexer.between(*open, *close),
            Self::Struct {open, close, ..} => lexer.between(*open, *close)
        }
    }
    pub fn kind(self) -> ValueKind<'a> {
        match self {
            Self::Default(_) => ValueKind::Default,
            value => ValueKind::Value(value)
        }
    }
    pub fn data_type(&self) -> DataType {
        match self {
            Self::String(_) => DataType::String,
            Self::Number(_) => DataType::Number,
            Self::Default(_) => DataType::Any,
            Self::None(_) => DataType::Option(Box::new(DataType::Any)),
            Self::Bool(_) => DataType::Bool,
            Self::Array {..} => DataType::Array(Box::new(DataType::Any)),
            Self::Struct {..} => DataType::Struct("Any")
        }
    }
    /// Parses an integer including radix and sign prefixes
    /// 
    /// # Panics
    /// Panics if the string is not an optional sign followed by an optional radix prefix followed by 1 or more characters
    fn int_parts(str: &str) -> (bool, u32, &str) {
        let mut chars = str.char_indices().peekable();
        let signed = if let Some(&(_, c)) = chars.peek() {
            if c == '-' || c == '+' {
                chars.next();
            }
            c == '-'
        } else {
            false
        };
        let radix = if let Some(&(_, c)) = chars.peek() {
            if c == '0' {
                // It is fine to discard the leading 0 if it is not a radix prefix
                chars.next();
                if let Some(&(_, c)) = chars.peek() {
                    let radix = match c {
                        'b' => 2,
                        'o' => 8,
                        'x' => 16,
                        _ => 10
                    };
                    if radix != 10 {
                        chars.next();
                    }
                    radix
                } else {
                    10
                }
            } else {
                10
            }
        } else {
            10
        };
        let start = chars.next().unwrap().0;
        (signed, radix, &str[start..])
    }
}
impl<'a> Deref for Values<'a> {
    type Target = [Value<'a>];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

macro_rules! from_value_int {
    (unsigned; $name:ident, $ty:ty, $bits:expr) => {
        impl FromValue for $ty {
            fn data_type() -> DataType {
                DataType::Integer { signed: false, bits: $bits}
            }
            fn from_value<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
                match value {
                    Value::Default(_) => Ok(*self = Default::default()),
                    Value::Number(span) => {
                        let (signed, radix, str) = Value::int_parts(span.str);
                        if signed {
                            Err(Error::InvalidType(value.clone(), Self::data_type()))
                        } else {
                            if let Ok(i) = <$ty>::from_str_radix(str, radix) {
                                Ok(*self = i)
                            } else {
                                Err(Error::InvalidNumber(value.clone(), Self::data_type()))
                            }
                        }
                    }
                    _ => Err(Error::InvalidType(value.clone(), Self::data_type()))
                }
            }
        }
    };
    (signed; $name:ident, $ty:ty, $bits:expr) => {
        impl FromValue for $ty {
            fn data_type() -> DataType {
                DataType::Integer { signed: false, bits: $bits}
            }
            fn from_value<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
                match value {
                    Value::Default(_) => Ok(*self = Default::default()),
                    Value::Number(span) => {
                        let (signed, radix, str) = Value::int_parts(span.str);
                        if let Ok(i) = <$ty>::from_str_radix(str, radix) {
                            if signed {
                                if let Some(i) = i.checked_neg() {
                                    Ok(*self = i)
                                } else {
                                    Err(Error::InvalidNumber(value.clone(), Self::data_type()))
                                }
                            } else {
                                Ok(*self = i)
                            }
                        } else {
                            Err(Error::InvalidNumber(value.clone(), Self::data_type()))
                        }
                    }
                    _ => Err(Error::InvalidType(value.clone(), Self::data_type()))
                }
            }
        }
    };
}
pub enum ValueKind<'a> {
    Default,
    Value(Value<'a>)
}
pub trait FromValue: Sized {
    fn data_type() -> DataType;
    fn from_value<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()>;
}
impl<T> FromValue for Option<T> where T: FromValue + Default {
    fn data_type() -> DataType {
        DataType::Array(Box::new(T::data_type()))
    }
    fn from_value<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        match value {
            Value::Default(_) => Ok(*self = Default::default()),
            Value::None(_) => Ok(*self = None),
            value => {
                let mut item: T = Default::default();
                item.from_value(value)?;
                Ok(*self = Some(item))
            }
        }
    }
}
impl<T> FromValue for Vec<T> where T: FromValue + Default {
    fn data_type() -> DataType {
        DataType::Array(Box::new(T::data_type()))
    }
    fn from_value<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        match value {
            Value::Default(_) => Ok(*self = Default::default()),
            Value::Array { values, ..} => {
                self.clear();
                for value in values.iter() {
                    let mut item = Default::default();
                    T::from_value(&mut item, value)?;
                    self.push(item)
                }
                Ok(())
            }
            _ => Err(Error::InvalidType(value.clone(), Self::data_type()))
        }
    }
}
impl<T> FromValue for HashMap<String, T> where T: FromValue + Default {
    fn data_type() -> DataType {
        DataType::Dictionary(Box::new(T::data_type()))
    }
    fn from_value<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        match value {
            Value::Default(_) => Ok(*self = Default::default()),
            Value::Struct { fields, ..} => {
                for field in fields.iter() {
                    let mut item = Default::default();
                    T::from_value(&mut item, field.value())?;
                    self.insert(field.name().into(), item);
                }
                Ok(())
            }
            _ => Err(Error::InvalidType(value.clone(), Self::data_type()))
        }
    }
}
impl<T> FromValue for T where T: Config {
    fn data_type() -> DataType {
        DataType::Struct(T::NAME)
    }
    fn from_value<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        match value {
            Value::Default(_) => Ok(*self = Default::default()),
            Value::Struct { fields, ..} => {
                for field in fields.iter() {
                    self.set(field)?
                }
                Ok(())
            }
            _ => Err(Error::InvalidType(value.clone(), Self::data_type()))
        }
    }
}
impl FromValue for String {
    fn data_type() -> DataType {
        DataType::String
    }
    fn from_value<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        match value {
            Value::Default(_) => Ok(*self = Default::default()),
            Value::String(span) => {
                const SIZE: usize = '"'.len_utf8();
                Ok(*self = (&span.str[SIZE..span.str.len()-SIZE]).to_owned())
            }
            _ => Err(Error::InvalidType(value.clone(), Self::data_type()))
        }
    }
}
impl FromValue for bool {
    fn data_type() -> DataType {
        DataType::Bool
    }
    fn from_value<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        match value {
            Value::Default(_) => Ok(*self = Default::default()),
            Value::Bool(span) => {
                Ok(*self = span.str == "true")
            }
            _ => Err(Error::InvalidType(value.clone(), Self::data_type()))
        }
    }
}
impl FromValue for f32 {
    fn data_type() -> DataType {
        DataType::Float32
    }
    fn from_value<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        match value {
            Value::Default(_) => Ok(*self = Default::default()),
            Value::Number(span) => {
                if let Ok(f) = f32::from_str(span.str) {
                    Ok(*self = f)
                } else {
                    Err(Error::InvalidNumber(value.clone(), Self::data_type()))
                }
            }
            _ => Err(Error::InvalidType(value.clone(), Self::data_type()))
        }
    }
}
impl FromValue for f64 {
    fn data_type() -> DataType {
        DataType::Float64
    }
    fn from_value<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        match value {
            Value::Default(_) => Ok(*self = Default::default()),
            Value::Number(span) => {
                if let Ok(f) = f64::from_str(span.str) {
                    Ok(*self = f)
                } else {
                    Err(Error::InvalidNumber(value.clone(), Self::data_type()))
                }
            }
            _ => Err(Error::InvalidType(value.clone(), Self::data_type()))
        }
    }
}
from_value_int!{unsigned; as_u8, u8, 8}
from_value_int!{signed; as_i8, i8, 8}
from_value_int!{unsigned; as_u16, u16, 16}
from_value_int!{signed; as_i16, i16, 16}
from_value_int!{unsigned; as_u32, u32, 32}
from_value_int!{signed; as_i32, i32, 32}
from_value_int!{unsigned; as_u64, u64, 64}
from_value_int!{signed; as_i64, i64, 64}
from_value_int!{unsigned; as_u128, u128, 128}
from_value_int!{signed; as_i128, i128, 128}
from_value_int!{unsigned; as_usize, usize, usize::BITS as _}
from_value_int!{signed; as_isize, isize, usize::BITS as _}

#[derive(Debug)]
pub enum DataType {
    String,
    Bool,
    Option(Box<DataType>),
    Array(Box<DataType>),
    Dictionary(Box<DataType>),
    Struct(&'static str),
    Integer {
        signed: bool,
        bits: u8
    },
    Float32,
    Float64,
    /// Any number
    Number,
    /// Equivalent to any value
    Any
}
impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String => write!(f, "String"),
            Self::Bool => write!(f, "bool"),
            Self::Option(ty) => write!(f, "Option<{}>", ty),
            Self::Array(ty) => write!(f, "Vec<{}>", ty),
            Self::Dictionary(ty) => write!(f, "HashMap<String, {}>", ty),
            Self::Struct(name) => write!(f, "struct {}", name),
            &Self::Integer { signed, bits } => write!(f, "{}{}", if signed { 'i' } else { 'u' }, bits),
            Self::Float32 => write!(f, "f32"),
            Self::Float64 => write!(f, "f64"),
            Self::Number => write!(f, "<Number>"),
            Self::Any => write!(f, "<Any>")
        }
    }
}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;
#[derive(Debug)]
pub enum Error<'a> {
    InvalidInput(Spanned<'a>),
    EndOfFile(Spanned<'a>),
    ExpectedToken(Option<Token<'a>>, TokenType),
    /// Like expected token, but many tokens are valid
    ExpectedValue(Option<Token<'a>>),
    InvalidType(Value<'a>, DataType),
    InvalidNumber(Value<'a>, DataType),
    NoField(Field<'a>, &'static str)
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
            Self::EndOfFile(_) => writeln!(writer, "The token is incomplete"),
            Self::ExpectedToken(Some(got), expected) => writeln!(writer, "Expected '{}', got '{}'", expected, got.ty),
            Self::ExpectedToken(None, expected) => writeln!(writer, "Expected '{}' but input ended", expected),
            Self::ExpectedValue(Some(got)) => writeln!(writer, "Expected a value, got '{}'", got.ty),
            Self::ExpectedValue(None) => writeln!(writer, "Expected a value but input ended"),
            Self::InvalidType(value, expected) => writeln!(writer, "Expected a value of type '{}', but got '{}'", expected, value.data_type()),
            Self::InvalidNumber(value, expected) => writeln!(writer, "Number '{}' cannot be represented by '{}'", value.span(lexer), expected),
            Self::NoField(field, structure) => writeln!(writer, "Struct '{}' has no field '{}'", structure, field.name()),
        };
    }
    fn span(&self, lexer: &Lexer<'a>) -> Spanned<'a> {
        match self {
            Self::InvalidInput(span) => *span,
            Self::EndOfFile(span) => *span,
            Self::ExpectedToken(Some(t), _) => t.span,
            Self::ExpectedToken(None, _) => lexer.end(),
            Self::ExpectedValue(Some(t)) => t.span,
            Self::ExpectedValue(None) => lexer.end(),
            Self::InvalidType(value, _) => value.span(lexer),
            Self::InvalidNumber(value, _) => value.span(lexer),
            Self::NoField(field, _) => field.span(lexer)
        }
    }
}