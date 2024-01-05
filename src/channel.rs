use std::{
    collections::VecDeque,
    sync::{Arc, Mutex},
};

type Channel<T> = Arc<Mutex<VecDeque<T>>>;
pub(crate) fn channel<T>() -> (ChannelSender<T>, ChannelReceiver<T>) {
    let c = Channel::new(Mutex::new(VecDeque::new()));
    (ChannelSender(c.clone()), ChannelReceiver(c))
}
pub(crate) struct ChannelSender<T>(Channel<T>);
impl<T> ChannelSender<T> {
    pub(crate) fn send(&self, msg: T) -> Result<(), String> {
        self.0
            .lock()
            .map_err(|e| format!("mutex panic {e}"))?
            .push_back(msg);
        Ok(())
    }
}
pub(crate) struct ChannelReceiver<T>(Channel<T>);
impl<T> ChannelReceiver<T> {
    fn try_recv(&self) -> Result<Option<T>, String> {
        let mut q = self.0.lock().map_err(|e| format!("mutex panic {e}"))?;
        Ok((*q).pop_front())
    }
    pub(crate) fn try_iter(&self) -> RecvTryIter<T> {
        RecvTryIter(ChannelReceiver(self.0.clone()))
    }
}
pub(crate) struct RecvTryIter<T>(ChannelReceiver<T>);
impl<T> Iterator for RecvTryIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .try_recv()
            .expect("failed to iterate over recv channel")
    }
}
