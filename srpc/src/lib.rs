use canary::Channel;

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

    pub use async_lock::Mutex;
    pub use async_lock::RwLock;
    pub use canary;
}

pub mod prelude {
    pub use crate::IntoClient;
    pub use crate::Peer;
    pub use srpc_macro::*;
}
