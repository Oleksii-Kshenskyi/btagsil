use crate::comprehension::tag_helper::{DOOrInfix, TagHelper};
use std::error::Error;

#[derive(Debug)]
pub enum Token<'a> {
    RootAction(&'a [&'a str]),
    Infix(&'a [&'a str]),
    InfixObject(&'a [&'a str]),
    DirectObject(&'a [&'a str]),
}

pub struct Lexer<'a> {
    helper: TagHelper<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(tags: &'a [&'a str]) -> Self {
        Self {
            helper: TagHelper::new(tags),
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, Box<dyn Error>> {
        assert!(
            self.helper.original_size() > 0,
            "Lexer::lex(): number of tags received by Lexer is 0!"
        );
        let mut lexed: Vec<Token> = vec![];

        lexed.push(Token::RootAction(self.helper.consume(1)));
        if self.helper.empty() {
            return Ok(lexed);
        }

        let (kind, length) = self.helper.direct_object_or_infix();

        let mut next = match kind {
            DOOrInfix::DirectObject => vec![Token::DirectObject(self.helper.consume(length))],
            DOOrInfix::Infix => Lexer::lex_infixes(&mut self.helper),
        };
        if !self.helper.empty() && self.helper.head_at_infix() {
            next.append(&mut Lexer::lex_infixes(&mut self.helper));
        }
        lexed.append(&mut next);

        Ok(lexed)
    }

    fn lex_infixes<'b>(helper: &'a mut TagHelper<'b>) -> Vec<Token<'b>> {
        if helper.empty() {
            return vec![];
        }
        let mut lexed: Vec<Token> = vec![];
        while !helper.empty() {
            while helper.head_at_infix() {
                lexed.push(Token::Infix(helper.consume(1)));
            }
            if !helper.empty() {
                if let (DOOrInfix::DirectObject, param_size) = helper.direct_object_or_infix() {
                    lexed.push(Token::InfixObject(helper.consume(param_size)));
                }
            }
        }

        lexed
    }
}
