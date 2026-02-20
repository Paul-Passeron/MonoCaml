pub type InferenceError = String;

pub type Res<T> = Result<T, InferenceError>;
