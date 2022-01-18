use std::marker::PhantomData;

pub use async_lock::Mutex;
pub use async_lock::RwLock;
pub use canary;
use canary::Channel;
use compact_str::CompactStr;
pub use srpc_macro::*;

pub trait Peer {
    type Struct: From<Channel>;
}

pub trait IntoClient {
    fn client<T: Peer>(self) -> T::Struct;
}

impl IntoClient for canary::Channel {
    fn client<T: Peer>(self) -> T::Struct {
        self.into()
    }
}

pub mod __private {
    pub use serde::de::DeserializeOwned;
    pub use serde::Deserialize;
    pub use serde::Serialize;
    pub use serde_repr::*;
}

pub struct RpcHandle<T: Peer>(
    CompactStr,
    PhantomData<T>
);

impl<T: Peer> RpcHandle<T> {
    pub fn new(string: impl Into<CompactStr>) -> Self {
        RpcHandle(string.into(), PhantomData)
    }
    pub async fn switch(&self, c: impl Into<Channel>) -> T::Struct {
        let chan: Channel = c.into();
        T::Struct::from(chan)
    }
}

impl<T: Peer> serde::Serialize for RpcHandle<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer {
        self.0.serialize(serializer)
    }
}

impl<'de, T: Peer> serde::Deserialize<'de> for RpcHandle<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de> {
        let string = CompactStr::deserialize(deserializer)?;
        Ok(RpcHandle::new(string))
    }
}
