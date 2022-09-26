use std::collections::HashMap;
use std::error::Error;

use super::lexer::Token;
use crate::data::errors::{
    DoesNotAcceptInfixError, DoesNotTakeDirectObjectError, RootActionUnknownError,
};

pub struct Infix {
    pub infix: String,
    pub param: Vec<String>,
}

#[derive(Clone)]
struct Validator {
    root_action: &'static str,
    direct_object: bool,
    acceptable_infixes: &'static [&'static str],
}

pub struct ActionConfig {
    pub root_action: String,
    pub direct_object: Vec<String>,
    pub infixes: Vec<Infix>,
}
impl ActionConfig {
    pub fn empty() -> Self {
        Self {
            root_action: String::new(),
            direct_object: vec![],
            infixes: vec![],
        }
    }
}

impl Validator {
    pub fn new(
        action: &'static str,
        direct_object: bool,
        infixes: &'static [&'static str],
    ) -> Self {
        Self {
            root_action: action.as_ref(),
            direct_object,
            acceptable_infixes: infixes,
        }
    }

    pub fn accepts_direct_object(&self) -> bool {
        self.direct_object
    }
    pub fn accepts_infix<S: AsRef<str>>(&self, infix: S) -> bool {
        self.acceptable_infixes.contains(&infix.as_ref())
    }
}

pub struct Parser {
    validators: HashMap<&'static str, Validator>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            validators: HashMap::from([
                ("exit", Validator::new("exit", false, &[])),
                ("echo", Validator::new("echo", true, &[])),
            ]),
        }
    }

    pub fn parse(&self, lexed: Vec<Token>) -> Result<ActionConfig, Box<dyn Error>> {
        assert!(
            lexed.len() > 0,
            "OH NO, Parser::parse(): the lexed vec is empty!"
        );
        let root: String;
        if let Token::RootAction(obj) = lexed.get(0).unwrap() {
            root = obj.join(" ").to_owned();
        } else {
            panic!("Parser::parse(): the first lexed element is not a root action! This should be impossible.");
        }
        let root_for_config = root.clone();
        let validator = self
            .validators
            .get(&*root)
            .ok_or(RootActionUnknownError::new(root))?
            .clone();

        let mut the_config = ActionConfig::empty();
        the_config.root_action = root_for_config;
        self.actually_parse(&lexed[1..], validator, the_config)
    }

    fn actually_parse(
        &self,
        lexed: &[Token],
        validator: Validator,
        mut config: ActionConfig,
    ) -> Result<ActionConfig, Box<dyn Error>> {
        if lexed.len() == 0 {
            return Ok(config);
        }

        match lexed[0] {
            Token::RootAction(_v) => panic!("Parser::actually_parse(): encountered the second RootAction token, which should be impossible!"),
            Token::DirectObject(v) => {
                if validator.accepts_direct_object() {
                config.direct_object = v.iter().map(|x| x.to_string()).collect::<Vec<_>>()
                } else {
                    return Err(Box::new(DoesNotTakeDirectObjectError::new(validator.root_action.to_owned(), v.join(" "))));
                }
            },
            Token::Infix(v) => {
                let infix_str = v.join(" ");
                if validator.accepts_infix(&infix_str) {
                    config.infixes.push(Infix { infix: infix_str, param: vec![] });
                } else {
                    return Err(Box::new(DoesNotAcceptInfixError::new(validator.root_action.to_owned(), infix_str)));
                }
            },
            Token::InfixObject(v) => {
                let last_infix = config.infixes.last_mut().expect("Parser::actually_parse(): need to parse an infix object but it's the first infix element somehow?..");
                let owned_vec: Vec<String> = v.iter().map(|x| x.to_string()).collect::<Vec<_>>();
                last_infix.param.append(&mut owned_vec.clone());
            },
        }

        self.actually_parse(&lexed[1..], validator, config)
    }
}
