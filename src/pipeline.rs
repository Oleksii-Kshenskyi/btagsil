use crate::errors::PipelineError;
use std::any::Any;
use std::any::TypeId;
use std::fmt::Display;

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

pub trait Stage: Send {
    fn name(&self) -> &str;
    fn visualize(&self) -> String;
    fn run(&self, input: AnyResult) -> Result<AnyResult, PipelineError>;

    fn input_typeid(&self) -> TypeId;
    fn input_typename(&self) -> &'static str;
    fn output_typeid(&self) -> TypeId;
    fn output_typename(&self) -> &'static str;
}

pub struct StageImpl<I, O, F>
where
    I: 'static + Send,
    O: 'static + Send,
    F: Fn(I) -> O + Send + 'static,
{
    _input: std::marker::PhantomData<I>,
    _output: std::marker::PhantomData<O>,
    func: F,
    name: String,
}

impl<I, O, F> Stage for StageImpl<I, O, F>
where
    I: 'static + Send,
    O: 'static + Send,
    F: Fn(I) -> O + Send + 'static,
{
    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn visualize(&self) -> String {
        format!(
            "Stage `{}`: {{{} => {}}}",
            self.name,
            self.input_typename(),
            self.output_typename()
        )
    }

    fn run(&self, input: AnyResult) -> Result<AnyResult, PipelineError> {
        let extracted =
            input
                .value
                .downcast::<I>()
                .map_err(|_| PipelineError::StageTypeMismatch {
                    expected: std::any::type_name::<I>(),
                    got: input.type_name,
                })?;
        let processed = (self.func)(*extracted);
        Ok(anyr(processed))
    }

    fn input_typeid(&self) -> TypeId {
        TypeId::of::<I>()
    }
    fn input_typename(&self) -> &'static str {
        std::any::type_name::<I>()
    }

    fn output_typeid(&self) -> TypeId {
        TypeId::of::<O>()
    }
    fn output_typename(&self) -> &'static str {
        std::any::type_name::<O>()
    }
}

impl Display for dyn Stage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.visualize())
    }
}

pub fn stage<I, O, F>(name: &str, func: F) -> Box<dyn Stage>
where
    I: 'static + Send,
    O: 'static + Send,
    F: Fn(I) -> O + Send + 'static,
{
    Box::new(StageImpl::<I, O, F> {
        _input: Default::default(),
        _output: Default::default(),
        func,
        name: name.to_owned(),
    })
}

// TODO: create a nice macro wrapper around the pipeline/stages to make it nicer to use
#[derive(Default)]
pub struct Pipeline {
    stages: Vec<Box<dyn Stage>>,
}

impl Pipeline {
    pub fn new() -> Self {
        Self { stages: Vec::new() }
    }

    pub fn push_stage(&mut self, stage: Box<dyn Stage>) -> Result<(), PipelineError> {
        if let Some(last) = self.stages.last() {
            if last.output_typeid() != stage.input_typeid() {
                return Err(PipelineError::PipelineIOTypeMismatch {
                    expected: last.output_typename(),
                    got: stage.input_typename(),
                });
            }
        }

        self.stages.push(stage);
        Ok(())
    }

    pub fn run(&self, input: AnyResult) -> Result<AnyResult, PipelineError> {
        let mut current = input;
        for stage in &self.stages {
            current = stage.run(current)?;
        }

        Ok(current)
    }

    pub fn visualize(&self) -> String {
        let mut buffer: String = String::new();
        for (index, stage) in self.stages.iter().enumerate() {
            buffer.push_str(&stage.visualize());
            if index != self.stages.len() - 1 {
                buffer.push('\n');
            }
        }

        buffer
    }
}
