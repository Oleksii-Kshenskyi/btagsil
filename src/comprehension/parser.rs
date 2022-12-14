use std::collections::HashMap;

use super::lexer::Token;
use crate::data::errors::{ErrorType, ParsingError};

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
            root_action: action,
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
                ("where", Validator::new("where", true, &[])),
                ("who", Validator::new("who", true, &[])),
                ("name", Validator::new("name", true, &[])),
            ]),
        }
    }

    pub fn parse(&self, lexed: Vec<Token>) -> Result<ActionConfig, ErrorType> {
        assert!(
            !lexed.is_empty(),
            "OH NO, Parser::parse(): the lexed vec is empty!"
        );
        let root: String;
        if let Token::RootAction(obj) = lexed.get(0).unwrap() {
            root = obj.join(" ");
        } else {
            panic!("Parser::parse(): the first lexed element is not a root action! This should be impossible.");
        }
        let root_for_config = root.clone();
        let validator = self
            .validators
            .get(&*root)
            .ok_or(ErrorType::Parsing(ParsingError::RootActionUnknown(root)))?
            .clone();

        if validator.accepts_direct_object() && lexed.len() < 2 {
            return Err(ErrorType::Parsing(ParsingError::DirectObjectNotProvided(validator.root_action.to_owned())));
        }

        let mut the_config = ActionConfig::empty();
        the_config.root_action = root_for_config;
        self.actually_parse(&lexed[1..], validator, the_config)
    }

    fn actually_parse(
        &self,
        lexed: &[Token],
        validator: Validator,
        mut config: ActionConfig,
    ) -> Result<ActionConfig, ErrorType> {
        if lexed.is_empty() {
            return Ok(config);
        }

        match lexed[0] {
            Token::RootAction(_v) => panic!("Parser::actually_parse(): encountered the second RootAction token, which should be impossible!"),
            Token::DirectObject(v) => {
                if validator.accepts_direct_object() && v.is_empty() {
                    return Err(ErrorType::Parsing(ParsingError::DirectObjectNotProvided(validator.root_action.to_owned())))
                } 
                else if validator.accepts_direct_object() {
                    config.direct_object = v.iter().map(|x| x.to_string()).collect::<Vec<_>>()
                } else {
                    return Err(ErrorType::Parsing(ParsingError::DoesNotTakeDirectObject(validator.root_action.to_owned(), v.join(" ")))); 
                }
            },
            Token::Infix(v) => {
                let infix_str = v.join(" ");
                if validator.accepts_infix(&infix_str) {
                    config.infixes.push(Infix { infix: infix_str, param: vec![] });
                } else {
                    return Err(ErrorType::Parsing(ParsingError::DoesNotAcceptInfix(validator.root_action.to_owned(), infix_str)));
                }
            },
            Token::InfixObject(v) => {
                let last_infix = config.infixes.last_mut().expect("Parser::actually_parse(): need to parse an infix object but it's the first infix element somehow?..");
                let mut owned_vec: Vec<String> = v.iter().map(|x| x.to_string()).collect::<Vec<_>>();
                last_infix.param.append(&mut owned_vec);
            },
        }

        self.actually_parse(&lexed[1..], validator, config)
    }
}
