use std::any::Any;

pub struct AnyResult {
    pub value: Box<dyn Any>,
    pub type_name: &'static str,
}
impl AnyResult {
    pub fn new<T: 'static + Send>(value: T) -> Self {
        Self {
            value: Box::new(value),
            type_name: std::any::type_name::<T>(),
        }
    }
}
pub fn anyr<T: 'static + Send>(value: T) -> AnyResult {
    AnyResult::new(value)
}
