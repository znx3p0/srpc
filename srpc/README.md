# SRPC

SRPC is a simple RPC system built on top of [Sia](https://github.com/znx3p0/sia),
designed to be as ergonomic and zero-cost as possible.

A simple example of an SRPC service
```rust
#[srpc::rpc] // other options include rpc(mutex), rpc(none)
#[derive(Default)]
struct DistributedList<T> {
    list: Vec<T>
}

#[srpc::rpc]
impl<T: Clone> DistributedList<T> {
    async fn push(&mut self, value: T) {
        self.list.push(value);
    }
    async fn get(&self, index: usize) -> Option<T> {
        self.list.get(index).and_then(|val| Some(val.clone()))
    }
    async fn remove(&mut self, index: usize) -> T {
        self.list.remove(index)
    }
}
```

```rust
// database
async fn main() -> Result<()> {
    GLOBAL_ROUTE.add_service_at::<DistributedList<i32>>("list", Arc::new(RwLock::new(Default::default())))?;
    let addr = "tcp@127.0.0.1:8080".parse::<Addr>()?;
    // listen in the following address
    addr.bind().await?;
    std::future::pending().await
}

// peer
async fn main() -> Result<()> {
    let addr = "list://tcp@127.0.0.1:8080".parse::<ServiceAddr>()?;
    let connection = addr.connect().await?;
    let mut db = connection.client::<DistributedList<i32>>();
    db.push(1).await?;
    db.push(5).await?;
    db.push(6).await?;

    let value = db.get(1).await?;
    println!("{:?}", value); // index 1 == 5
    Ok(())
}
```
