use thiserror::Error;

#[derive(Error, Debug)]

pub enum PipelineError {
    #[error("Input type mismatch during stage execution: expected {expected:?}, got {got:?}")]
    StageTypeMismatch {
        expected: &'static str,
        got: &'static str,
    },
    #[error("Input/Output type mismatch between stages: expected {expected:?}, got {got:?}")]
    PipelineIOTypeMismatch {
        expected: &'static str,
        got: &'static str,
    },
    #[error("Pipeline runtime error: {0}")]
    RuntimeError(String),
}
