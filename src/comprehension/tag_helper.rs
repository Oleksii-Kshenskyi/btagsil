pub enum DOOrInfix {
    DirectObject,
    Infix,
}

pub fn infixes() -> Vec<&'static str> {
    vec![
        "in", "at", "up", "out", "from", "into", "before", "after", "of", "about", "because",
    ]
}

pub struct TagHelper<'a> {
    tags: &'a [&'a str],
    head: usize,
}

impl<'a> TagHelper<'a> {
    pub fn new(tags: &'a [&'a str]) -> Self {
        Self {
            tags: tags,
            head: 0,
        }
    }

    pub fn original_size(&self) -> usize {
        self.tags.len()
    }
    pub fn empty(&self) -> bool {
        (&self.tags[self.head..self.tags.len()]).len() == 0
    }

    pub fn tag(&self, index: usize) -> &'a [&'a str] {
        assert!(
            self.tags.len() > index,
            "OH NO: TagHelper::tag(): there are {} tags, but the index is {}.",
            self.tags.len(),
            index
        );
        &self.tags[index..(index + 1)]
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
        let mut size: usize = 1;
        let mut counter = self.head;
        let mut infix = false;
        while counter < self.tags.len() {
            infix = infixes.contains(&self.tags[counter]);
            if !infix {
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
        infixes().contains(&self.tags[self.head])
    }
}
