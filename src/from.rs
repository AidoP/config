use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr, SocketAddrV4, SocketAddrV6},
    ops::{Deref, DerefMut},
    path::PathBuf,
    rc::Rc,
    str::FromStr,
    sync::{Mutex, RwLock, Arc, mpsc::Sender}
};
use super::{identifier, Result, Error, DataType, Value};

pub trait FromValue: Sized {
    fn data_type() -> DataType;
    fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self>;
    fn from_value_partial<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        *self = Self::from_value_new(value)?;
        Ok(())
    }
}

impl<T> FromValue for Box<T> where T: FromValue {
    fn data_type() -> DataType { T::data_type() }
    fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
        T::from_value_new(value).map(|t| Box::new(t))
    }
    fn from_value_partial<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        self.deref_mut().from_value_partial(value)
    }
}
impl<T> FromValue for Cell<T> where T: FromValue {
    fn data_type() -> DataType { T::data_type() }
    fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
        T::from_value_new(value).map(|t| Self::new(t))
    }
    fn from_value_partial<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        self.get_mut().from_value_partial(value)
    }
}
impl<T> FromValue for RefCell<T> where T: FromValue {
    fn data_type() -> DataType { T::data_type() }
    fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
        T::from_value_new(value).map(|t| Self::new(t))
    }
    fn from_value_partial<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        self.get_mut().from_value_partial(value)
    }
}
impl<T> FromValue for Rc<T> where T: FromValue {
    fn data_type() -> DataType { T::data_type() }
    fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
        T::from_value_new(value).map(|t| Self::new(t))
    }
    fn from_value_partial<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        Self::get_mut(self).ok_or(Error::Unavailable(value.clone()))?.from_value_partial(value)
    }
}
impl<T> FromValue for Arc<T> where T: FromValue {
    fn data_type() -> DataType { T::data_type() }
    fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
        T::from_value_new(value).map(|t| Self::new(t))
    }
    fn from_value_partial<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        Self::get_mut(self).ok_or(Error::Unavailable(value.clone()))?.from_value_partial(value)
    }
}
impl<T> FromValue for Sender<T> where T: FromValue {
    fn data_type() -> DataType { T::data_type() }
    fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
        Err(Error::NoNew(value.clone(), Self::data_type()))
    }
    fn from_value_partial<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        if self.send(T::from_value_new(value)?).is_err() {
            Err(Error::Unavailable(value.clone()))
        } else {
            Ok(())
        }
    }
}
/// # Panics
/// Due to unspecified behaviour, may panic if called on a poisoned mutex
impl<T> FromValue for Mutex<T> where T: FromValue {
    fn data_type() -> DataType { T::data_type() }
    fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
        T::from_value_new(value).map(|t| Self::new(t))
    }
    fn from_value_partial<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        // As we are setting values, we do not care if the value is poisoned and minimially safe
        // Though behaviour is potentially unspecified, it is not undefined
        // Further, we leave the mutex poisoned so that it is still clear that the contents are undefined
        self.get_mut().unwrap_or_else(|e| e.into_inner()).from_value_partial(value)
    }
}
/// # Panics
/// Due to unspecified behaviour, may panic if called on a poisoned RwLock
impl<T> FromValue for RwLock<T> where T: FromValue {
    fn data_type() -> DataType { T::data_type() }
    fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
        T::from_value_new(value).map(|t| Self::new(t))
    }
    fn from_value_partial<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        // As we are setting values, we do not care if the value is poisoned and minimially safe
        // Though behaviour is potentially unspecified, it is not undefined
        // Further, we leave the mutex poisoned so that it is still clear that the contents are undefined
        self.get_mut().unwrap_or_else(|e| e.into_inner()).from_value_partial(value)
    }
}
impl<T> FromValue for Option<T> where T: FromValue {
    fn data_type() -> DataType { DataType::Wrapper("optional", Box::new(T::data_type())) }
    fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
        match value {
            identifier!("none") => Ok(None),
            value => T::from_value_new(value).map(|t| Some(t))
        }
    }
    fn from_value_partial<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        match value {
            identifier!("none") => *self = None,
            value => match self {
                Some(t) => t.from_value_partial(value)?,
                None => *self = Some(T::from_value_new(value)?)
            }
        }
        Ok(())
    }
}
impl<T, const N: usize> FromValue for [T; N] where T: FromValue {
    fn data_type() -> DataType { DataType::Array(N, Box::new(T::data_type())) }
    fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
        match value {
            none @ identifier!("none") => Err(Error::NoDefault(none.clone(), Self::data_type())),
            Value::Array { values, ..} => {
                if values.len() != N {
                    Err(Error::InvalidType(value.clone(), Self::data_type()))
                } else {
                    unsafe {
                        let mut values = values.iter();
                        let mut array: std::mem::MaybeUninit<[T; N]> = std::mem::MaybeUninit::uninit().assume_init();
                        let mut ptr = array.as_mut_ptr() as *mut T;
                        let end = ptr.add(N);
                        while ptr < end {
                            *ptr = T::from_value_new(values.next().unwrap())?;
                            ptr = ptr.add(1)
                        }
                        Ok(array.assume_init())
                    }
                }
            },
            value => Err(Error::InvalidType(value.clone(), Self::data_type()))
        }
    }
}
impl<T> FromValue for Vec<T> where T: FromValue {
    fn data_type() -> DataType { DataType::Wrapper("list of", Box::new(T::data_type())) }
    fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
        match value {
            identifier!("none") => Ok(Vec::new()),
            Value::Array { values, ..} => {
                let mut vec = Vec::with_capacity(values.len());
                for value in values.iter() {
                    vec.push(T::from_value_new(value.deref())?)
                }
                Ok(vec)
            }
            value => Err(Error::InvalidType(value.clone(), Self::data_type()))
        }
    }
    fn from_value_partial<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        match value {
            identifier!("none") => Ok(*self = Vec::new()),
            Value::Array { values, ..} => {
                for value in values.iter() {
                    self.push(T::from_value_new(value.deref())?)
                }
                Ok(())
            }
            value => Err(Error::InvalidType(value.clone(), Self::data_type()))
        }
    }
}
impl<K, V> FromValue for HashMap<K, V> where K: FromValue + Eq + std::hash::Hash, V: FromValue {
    fn data_type() -> DataType { DataType::Dictionary(Box::new(K::data_type()), Box::new(V::data_type())) }
    fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
        match value {
            identifier!("none") => Ok(HashMap::new()),
            Value::Struct { fields, ..} => {
                let mut map = HashMap::with_capacity(fields.len());
                for field in fields.iter() {
                    map.insert(K::from_value_new(field.field())?, V::from_value_new(field.value())?);
                }
                Ok(map)
            }
            value => Err(Error::InvalidType(value.clone(), Self::data_type()))
        }
    }
    fn from_value_partial<'a>(&mut self, value: &Value<'a>) -> Result<'a, ()> {
        match value {
            identifier!("none") => Ok(*self = HashMap::new()),
            Value::Struct { fields, ..} => {
                for field in fields.iter() {
                    self.insert(K::from_value_new(field.field())?, V::from_value_new(field.value())?);
                }
                Ok(())
            }
            value => Err(Error::InvalidType(value.clone(), Self::data_type()))
        }
    }
}
impl FromValue for bool {
    fn data_type() -> DataType { DataType::Named("bool") }
    fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
        match value {
            identifier!("none") => Ok(Default::default()),
            identifier!("true") => Ok(true),
            identifier!("false") => Ok(false),
            value => Err(Error::InvalidType(value.clone(), Self::data_type()))
        }
    }
}
#[macro_export]
macro_rules! from_value {
    ($name:expr, $ty:ty) => {
        impl $crate::FromValue for $ty {
            fn data_type() -> $crate::DataType { $crate::DataType::Named($name) }
            fn from_value_new<'a>(value: &$crate::Value<'a>) -> $crate::Result<'a, Self> {
                match value {
                    $crate::identifier!("none") => ::std::result::Result::Ok(::std::default::Default::default()),
                    value => if let ::std::result::Result::Ok(str) = Self::from_str(value.clean(Self::data_type())?.as_ref()) {
                        ::std::result::Result::Ok(str)
                    } else {
                        ::std::result::Result::Err($crate::Error::InvalidType(value.clone(), Self::data_type()))
                    }
                }
            }
        }
    };
    (!Default; $name:expr, $ty:ty) => {
        impl $crate::FromValue for $ty {
            fn data_type() -> $crate::DataType { $crate::DataType::Named($name) }
            fn from_value_new<'a>(value: &$crate::Value<'a>) -> $crate::Result<'a, Self> {
                match value {
                    value @ $crate::identifier!("none") => ::std::result::Result::Err($crate::Error::NoDefault(value.clone(), Self::data_type())),
                    value => if let ::std::result::Result::Ok(str) = Self::from_str(value.clean(Self::data_type())?.as_ref()) {
                        ::std::result::Result::Ok(str)
                    } else {
                        ::std::result::Result::Err($crate::Error::InvalidType(value.clone(), Self::data_type()))
                    }
                }
            }
        }
    };
}
macro_rules! from_value_int {
    ($ty:ty) => {
        impl FromValue for $ty {
            fn data_type() -> DataType { DataType::Named(stringify!($ty)) }
            fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
                match value {
                    identifier!("none") => Ok(Default::default()),
                    value => {
                        let digits = value.clean(Self::data_type())?;
                        let (sign, radix, digits) = Value::int_parts(digits.as_ref());
                        let digits = format!("{}{}", if sign { '-' } else { '+' }, digits);
                        if let Ok(i) = Self::from_str_radix(&digits, radix) {
                            Ok(i)
                        } else {
                            Err(Error::InvalidType(value.clone(), Self::data_type()))
                        }
                    }
                }
            }
        }
    }
}
macro_rules! from_value_non_zero {
    ($inner:ty, $ty:ty) => {
        impl FromValue for $ty {
            fn data_type() -> DataType { DataType::Named(concat!("non-zero ", stringify!($inner))) }
            fn from_value_new<'a>(value: &Value<'a>) -> Result<'a, Self> {
                match value {
                    value @ identifier!("none") => Err(Error::NoDefault(value.clone(), Self::data_type())),
                    value => <$ty>::new(<$inner as FromValue>::from_value_new(value)?).ok_or(Error::InvalidType(value.clone(), Self::data_type()))
                }
            }
        }
    }
}
from_value_int!{ u8     }
from_value_int!{ i8     }
from_value_int!{ u16    }
from_value_int!{ i16    }
from_value_int!{ u32    }
from_value_int!{ i32    }
from_value_int!{ u64    }
from_value_int!{ i64    }
from_value_int!{ u128   }
from_value_int!{ i128   }
from_value_int!{ usize  }
from_value_int!{ isize  }
from_value_non_zero!{ u8,    std::num::NonZeroU8    }
from_value_non_zero!{ i8,    std::num::NonZeroI8    }
from_value_non_zero!{ u16,   std::num::NonZeroU16   }
from_value_non_zero!{ i16,   std::num::NonZeroI16   }
from_value_non_zero!{ u32,   std::num::NonZeroU32   }
from_value_non_zero!{ i32,   std::num::NonZeroI32   }
from_value_non_zero!{ u64,   std::num::NonZeroU64   }
from_value_non_zero!{ i64,   std::num::NonZeroI64   }
from_value_non_zero!{ u128,  std::num::NonZeroU128  }
from_value_non_zero!{ i128,  std::num::NonZeroI128  }
from_value_non_zero!{ usize, std::num::NonZeroUsize }
from_value_non_zero!{ isize, std::num::NonZeroIsize }

from_value!{ "f32", f32 }
from_value!{ "f64", f64 }

from_value!{ "string",  String              }
from_value!{ "string",  std::ffi::OsString  }
from_value!{ "char",    char                }
from_value!{ "path",    PathBuf             }

from_value!{ !Default; "IP address",             IpAddr       }
from_value!{ !Default; "IPv4 address",           Ipv4Addr     }
from_value!{ !Default; "IPv6 address",           Ipv6Addr     }
from_value!{ !Default; "IP socket address",      SocketAddr   }
from_value!{ !Default; "IPv4 socket address",    SocketAddrV4 }
from_value!{ !Default; "IPv6 socket address",    SocketAddrV6 }