pub enum InferenceError {}

pub type Res<T> = Result<T, InferenceError>;
