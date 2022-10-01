#[derive(Debug)]
pub enum DOOrInfix {
    DirectObject,
    Infix,
}

pub fn infixes() -> Vec<&'static str> {
    vec![
        "in", "at", "on", "for", "up", "out", "from", "into", "before", "after", "of", "about",
        "because",
    ]
}

pub struct TagHelper<'a> {
    tags: &'a [&'a str],
    head: usize,
}

impl<'a> TagHelper<'a> {
    pub fn new(tags: &'a [&'a str]) -> Self {
        Self { tags, head: 0 }
    }

    pub fn original_size(&self) -> usize {
        self.tags.len()
    }
    pub fn empty(&self) -> bool {
        self.tags.len() <= self.head
    }

    pub fn consume(&mut self, how_many: usize) -> &'a [&'a str] {
        assert!(
            self.tags.len() >= how_many,
            "OH NO: TagHelper::consume(): I consumed way too much!!! Consumed {}, but there are only {} :(",
            how_many,
            self.tags.len()
        );
        let slice = &self.tags[self.head..(self.head + how_many)];
        self.head += how_many;
        slice
    }

    pub fn direct_object_or_infix(&self) -> (DOOrInfix, usize) {
        let infixes = infixes();
        let mut size: usize = 0;
        let mut counter = self.head;
        let infix = infixes.contains(&self.tags[counter]);
        while counter < self.tags.len() {
            if !infixes.contains(&self.tags[counter]) {
                size += 1;
                counter += 1;
            } else {
                break;
            }
        }
        if infix {
            (DOOrInfix::Infix, size)
        } else {
            (DOOrInfix::DirectObject, size)
        }
    }

    pub fn head_at_infix(&self) -> bool {
        if !self.empty() {
            infixes().contains(&self.tags[self.head])
        } else {
            false
        }
    }
}
