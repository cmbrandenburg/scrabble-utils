use rand::Rng;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::path::Path;

const BLANK_CHAR: char = '?';
const LETTER_COUNT: usize = 26;
pub const TILE_COUNT: usize = 100;

#[cfg_attr(rustfmt, rustfmt_skip)]
const TILE_DISTRIBUTION: [u32; 128] = [
        0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0, // 0x00
        0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0, // 0x10
        0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0, // 0x20
    //                                                                ?
        0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  2, // 0x30
        0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0, // 0x40
        0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0, // 0x50
    //      A   B   C   D   E   F   G     H   I   J   K   L   M   N   O
        0,  9,  2,  2,  4, 12,  2,  3,    2,  9,  1,  1,  4,  2,  6,  8, // 0x60
    //  P   Q   R   S   T   U   V   W     X   Y   Z
        2,  1,  6,  4,  6,  4,  2,  2,    1,  2,  1,  0,  0,  0,  0,  0, // 0x70
];

#[cfg_attr(rustfmt, rustfmt_skip)]
const TILE_VALUES: [u32; 128] = [
        0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0, // 0x00
        0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0, // 0x10
        0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0, // 0x20
    //                                                                ?
        0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0, // 0x30
        0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0, // 0x40
        0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0, // 0x50
    //      A   B   C   D   E   F   G     H   I   J   K   L   M   N   O
        0,  1,  3,  3,  2,  1,  4,  2,    4,  1,  8,  5,  1,  3,  1,  1, // 0x60
    //  P   Q   R   S   T   U   V   W     X   Y   Z
        3, 10,  1,  1,  1,  1,  4,  4,    8,  4, 10,  0,  0,  0,  0,  0, // 0x70
];

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Letter(u32);

impl Letter {
    pub fn new(mut ch: char) -> Self {
        ch = ch.to_ascii_lowercase();
        assert!('a' <= ch && ch <= 'z');
        Letter(ch as u32)
    }

    pub fn all_letters() -> &'static [Letter] {
        static ALL_LETTERS: [Letter; LETTER_COUNT] = [
            Letter('a' as u32),
            Letter('b' as u32),
            Letter('c' as u32),
            Letter('d' as u32),
            Letter('e' as u32),
            Letter('f' as u32),
            Letter('g' as u32),
            Letter('h' as u32),
            Letter('i' as u32),
            Letter('j' as u32),
            Letter('k' as u32),
            Letter('l' as u32),
            Letter('m' as u32),
            Letter('n' as u32),
            Letter('o' as u32),
            Letter('p' as u32),
            Letter('q' as u32),
            Letter('r' as u32),
            Letter('s' as u32),
            Letter('t' as u32),
            Letter('u' as u32),
            Letter('v' as u32),
            Letter('w' as u32),
            Letter('x' as u32),
            Letter('y' as u32),
            Letter('z' as u32),
        ];
        &ALL_LETTERS
    }

    pub fn is_valid_char(ch: char) -> bool {
        ch.is_ascii_alphabetic()
    }

    pub fn letter_vec_from_string(s: &str) -> Vec<Letter> {
        s.chars().map(|ch| Letter::new(ch)).collect::<Vec<_>>()
    }

    pub fn as_char(&self) -> char {
        self.0 as u8 as char
    }

    pub fn as_tile(&self) -> Tile {
        Tile(self.0)
    }

    pub fn point_value(&self) -> u32 {
        self.as_tile().point_value()
    }
}

impl Debug for Letter {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        f.debug_tuple("Letter").field(&self.as_char()).finish()
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Tile(u32);

impl Tile {
    pub fn blank() -> Self {
        Tile(BLANK_CHAR as u32)
    }

    pub fn new(ch: char) -> Self {
        assert!(ch == BLANK_CHAR || 'a' <= ch && ch <= 'z', "ch is {}", ch);
        Tile(ch as u32)
    }

    pub fn draw_bag<R: Rng>(rng: &mut R) -> Vec<Self> {
        use rand::seq::SliceRandom;

        let mut all_tiles = Vec::new();
        all_tiles.reserve(TILE_COUNT);

        for (i, &freq) in TILE_DISTRIBUTION.iter().enumerate() {
            for _j in 0..freq {
                all_tiles.push(Tile::new(i as u8 as char));
            }
        }
        assert_eq!(all_tiles.len(), TILE_COUNT);

        all_tiles.shuffle(rng);

        all_tiles
    }

    pub fn is_valid_char(ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch == '?'
    }

    pub fn point_value(&self) -> u32 {
        TILE_VALUES[self.0 as usize]
    }

    pub fn as_char(&self) -> char {
        self.0 as u8 as char
    }

    pub fn letter(&self) -> Option<Letter> {
        if self.is_blank() {
            None
        } else {
            Some(Letter(self.0))
        }
    }

    pub fn is_blank(&self) -> bool {
        self.0 == BLANK_CHAR as u32
    }

    pub fn count(&self) -> u32 {
        TILE_DISTRIBUTION[self.0 as u8 as usize]
    }
}

impl Debug for Tile {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        f.debug_tuple("Tile")
            .field(&(self.0 as u8 as char))
            .finish()
    }
}

#[derive(Debug)]
pub struct TileCounter {
    counts: [i32; LETTER_COUNT + 1],
    inversion: i32,
}

impl TileCounter {
    pub fn new() -> Self {
        TileCounter {
            counts: [0; LETTER_COUNT + 1],
            inversion: 1,
        }
    }

    pub fn count_tiles_in_str(&mut self, s: &str) {
        for ch in s.chars() {
            if ch == '?' {
                self.counts[LETTER_COUNT] += self.inversion;
            } else if ch.is_ascii_alphabetic() {
                let index =
                    (ch.to_ascii_lowercase() as u8 - 'a' as u8) as usize;
                self.counts[index] += self.inversion;
            }
        }
    }

    pub fn invert(&mut self) {
        assert_eq!(self.inversion, 1, "cannot invert twice");
        self.inversion = -1;
        for i in 0..LETTER_COUNT {
            let ch = ('a' as u8 + i as u8) as char;
            self.counts[i] = Tile::new(ch).count() as i32 - self.counts[i];
        }
        self.counts[LETTER_COUNT] =
            Tile::new('?').count() as i32 - self.counts[LETTER_COUNT];
    }

    pub fn char_count(&self, ch: char) -> i32 {
        assert!(Tile::is_valid_char(ch));
        if ch == '?' {
            self.counts[LETTER_COUNT]
        } else if ch.is_ascii_alphabetic() {
            let index = (ch.to_ascii_lowercase() as u8 - 'a' as u8) as usize;
            self.counts[index]
        } else {
            unreachable!();
        }
    }

    pub fn tile_counts<'a>(&'a self) -> impl Iterator<Item = (Tile, i32)> + 'a {
        struct Iter<'tile_counter> {
            index: usize,
            tile_counter: &'tile_counter TileCounter,
        }

        impl<'tile_counter> Iterator for Iter<'tile_counter> {
            type Item = (Tile, i32);
            fn next(&mut self) -> Option<Self::Item> {
                if self.index == LETTER_COUNT + 1 {
                    return None;
                }
                let index = self.index;
                self.index += 1;
                if index < LETTER_COUNT {
                    let ch = ('a' as u8 + index as u8) as char;
                    return Some((
                        Tile::new(ch),
                        self.tile_counter.counts[index],
                    ));
                }
                Some((Tile::new('?'), self.tile_counter.counts[LETTER_COUNT]))
            }
        }

        Iter {
            index: 0,
            tile_counter: self,
        }
    }
}

pub struct Dictionary {
    words: HashSet<String>,
    longest_word_len: usize,
}

impl Dictionary {
    pub fn load_from_file(
        dict_filename: &Path,
    ) -> Result<Self, Box<std::error::Error>> {
        let f = std::fs::File::open(dict_filename)?;
        Self::load_from_reader(f)
    }

    pub fn load_from_reader<R>(r: R) -> Result<Self, Box<std::error::Error>>
    where
        R: std::io::Read,
    {
        use std::io::BufRead;

        let mut words = HashSet::new();
        let r = std::io::BufReader::new(r);
        let mut longest_word_len = 0;

        for line_result in r.lines() {
            match line_result {
                Err(e) => return Err(Box::new(e)),
                Ok(ref line) if line.is_empty() => (),
                Ok(line) => {
                    let w = line
                        .chars()
                        .filter(|c| c.is_ascii_alphabetic())
                        .map(|c| c.to_ascii_lowercase())
                        .collect::<String>();
                    longest_word_len = std::cmp::max(longest_word_len, w.len());
                    words.insert(w);
                }
            };
        }

        Ok(Dictionary {
            words,
            longest_word_len,
        })
    }

    pub fn words(&self) -> impl Iterator<Item = &String> {
        self.words.iter()
    }

    pub fn longest_word_len(&self) -> usize {
        self.longest_word_len
    }
}

// Encapsulates a dictionary as a spanning tree. Each word carries a payload of
// type T.
pub struct Span<T> {
    root: SpanNode<T>,
}

struct SpanNode<T> {
    // None -> not a word
    // Some(..) -> is a word, carries the given payload for this word
    payload: Option<T>,
    children: [*mut SpanNode<T>; LETTER_COUNT],
}

impl<T> Span<T>
where
    T: Clone + Copy,
{
    pub fn new(d: &Dictionary) -> Self
    where
        T: Default,
    {
        let mut s = Span {
            root: SpanNode::new(),
        };
        for w in d.words() {
            let mut p = &mut s.root;
            for c in w.chars() {
                let c = c as u32;
                let c_index = (c - 'a' as u32) as usize;
                if p.children[c_index].is_null() {
                    let child = Box::new(SpanNode::new());
                    p.children[c_index] = Box::into_raw(child);
                }
                p = unsafe { &mut *p.children[c_index] };
            }
            debug_assert!(p.payload.is_none());
            p.payload = Some(T::default());
        }
        s
    }

    pub fn walker(&self) -> SpanWalker<T> {
        SpanWalker {
            node: &self.root,
            _lifetime: PhantomData,
        }
    }

    pub fn walker_mut(&mut self) -> SpanWalkerMut<T> {
        SpanWalkerMut {
            node: &mut self.root,
            _lifetime: PhantomData,
        }
    }

    pub fn reset_payloads_to_default(&mut self)
    where
        T: Default,
    {
        fn recurse<T>(node: &mut SpanNode<T>)
        where
            T: Default,
        {
            match node.payload {
                None => {}
                Some(ref mut x) => *x = T::default(),
            }
            for &p in node.children.iter() {
                if p.is_null() {
                    continue;
                }
                recurse(unsafe { &mut *p });
            }
        }

        recurse(&mut self.root);
    }
}

impl<T> SpanNode<T> {
    pub fn new() -> Self {
        SpanNode {
            payload: None,
            children: [std::ptr::null_mut(); LETTER_COUNT],
        }
    }
}

impl<T> Drop for SpanNode<T> {
    fn drop(&mut self) {
        for c in self.children.iter().filter(|p| !p.is_null()) {
            unsafe {
                Box::from_raw(*c); // free the child
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct SpanWalker<'dict, T>
where
    T: Clone + Copy,
{
    node: *const SpanNode<T>,
    _lifetime: PhantomData<&'dict ()>,
}

#[derive(Clone, Copy)]
pub struct SpanWalkerMut<'dict, T>
where
    T: Clone + Copy,
{
    node: *mut SpanNode<T>,
    _lifetime: PhantomData<&'dict ()>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum SpanResult<T> {
    DeadEnd, // no word begins with this sequence of letters
    Prefix,  // one or more words begins with this sequence of letters
    Word(T), // this sequence of letters is a word
}

impl<'dict, T> SpanWalker<'dict, T>
where
    T: Clone + Copy,
{
    pub fn walk(&mut self, letter: Letter) -> SpanResult<&T> {
        let index = (letter.0 - 'a' as u32) as usize;
        if unsafe { (*self.node).children[index].is_null() } {
            SpanResult::DeadEnd
        } else {
            self.node = unsafe { (*self.node).children[index] };
            match unsafe { &(*self.node).payload } {
                &None => SpanResult::Prefix,
                &Some(ref x) => SpanResult::Word(x),
            }
        }
    }
}

impl<'dict, T> SpanWalkerMut<'dict, T>
where
    T: Clone + Copy,
{
    pub fn walk(&mut self, letter: Letter) -> SpanResult<&mut T> {
        let index = (letter.0 - 'a' as u32) as usize;
        if unsafe { (*self.node).children[index].is_null() } {
            SpanResult::DeadEnd
        } else {
            self.node = unsafe { (*self.node).children[index] };
            match unsafe { &mut (*self.node).payload } {
                &mut None => SpanResult::Prefix,
                &mut Some(ref mut x) => SpanResult::Word(x),
            }
        }
    }
}

// Returns whether the tiles begin with at least one word that uses at least
// one blank.
//
// E.g., "dogmat?c" -> true, but "dogmat?z" -> false.
//
fn does_use_blank<T>(span: &Span<T>, tiles: &[Tile]) -> bool
where
    T: Clone + Copy,
{
    fn recurse<T>(mut w: SpanWalker<T>, tiles: &[Tile], using: bool) -> bool
    where
        T: Clone + Copy,
    {
        for (i, t) in tiles.iter().enumerate() {
            match t.letter() {
                Some(lt) => match w.walk(lt) {
                    SpanResult::DeadEnd => return false,
                    SpanResult::Prefix => continue,
                    SpanResult::Word(..) if using => return true,
                    SpanResult::Word(..) => continue,
                },
                None => {
                    let w2 = w;
                    for &lt in Letter::all_letters() {
                        w = w2;
                        match w.walk(lt) {
                            SpanResult::DeadEnd => continue,
                            SpanResult::Word(..) => return true,
                            SpanResult::Prefix => {
                                if recurse(w, &tiles[i + 1..], true) {
                                    return true;
                                }
                            }
                        }
                    }
                    return false;
                }
            }
        }
        false
    }

    recurse(span.walker(), tiles, false)
}

pub struct ContiguousWordFinder {
    run_index: u32,
    span: Span<(u32, u32)>, // (run index, word point value)
}

impl ContiguousWordFinder {
    pub fn new(dict: &Dictionary) -> Self {
        ContiguousWordFinder {
            run_index: 0,
            span: Span::new(dict),
        }
    }

    pub fn find_contiguous_words(
        &mut self,
        tiles: &[Tile],
        allow_dups: bool,
    ) -> u32 {
        if tiles.is_empty() {
            return 0;
        }

        // Check for overflow in the run index.
        let number_of_blanks = tiles.iter().filter(|&t| t.is_blank()).count();
        match (LETTER_COUNT as u32)
            .checked_pow(number_of_blanks as u32)
            .and_then(|n| n.checked_add(1))
        {
            None => {
                self.run_index = 1;
                self.span.reset_payloads_to_default();
            }
            Some(..) => {
                self.run_index += 1;
            }
        }

        // This is an optimization that finds the total point values starting at
        // each tile where none of the words starting at that tile use a blank.
        // These point values are used for all blank combinations, later.
        let non_blank_point_values: Vec<Option<u32>> = {
            let mut v = vec![None; tiles.len()];
            for i in 0..tiles.len() {
                if does_use_blank(&self.span, &tiles[i..]) {
                    continue;
                }
                let mut w = self.span.walker_mut();
                let mut score_at_this_position = 0;
                let mut next_word_score = 0;
                for (_len, &t) in tiles[i..].iter().enumerate() {
                    let lt = match t.letter() {
                        None => break, // no word uses this blank, abort!
                        Some(lt) => lt,
                    };
                    next_word_score += t.point_value();
                    match w.walk(lt) {
                        SpanResult::DeadEnd => break,
                        SpanResult::Prefix => continue,
                        SpanResult::Word(&mut (
                            ref mut n,
                            ref mut prev_best_score,
                        )) => {
                            let index_matches =
                                !allow_dups && *n == self.run_index;
                            if index_matches
                                && *prev_best_score < next_word_score
                            {
                                score_at_this_position +=
                                    next_word_score - *prev_best_score;
                                *prev_best_score = next_word_score;
                            } else if index_matches {
                                // word already counted
                            } else {
                                score_at_this_position += next_word_score;
                                *n = self.run_index;
                                *prev_best_score = next_word_score;
                            }
                        }
                    }
                }
                v[i] = Some(score_at_this_position);
            }
            v
        };

        let no_blank_run_index = self.run_index;

        // Blank combinations. If the `tiles` slice contains, e.g., two blanks, then
        // we iterate through all combinations of letters for those blanks. I.e.,
        // (a, a), (a, b), (a, c) ... (a, z), (b, a), (b, b) ... (z, z). The
        // applied_blanks slice contains the complete tile sequence, including the
        // blank combination.
        let blank_indices = tiles
            .iter()
            .enumerate()
            .filter(|(_i, t)| t.is_blank())
            .map(|(i, _t)| i)
            .collect::<Vec<_>>();
        let mut applied_blanks = tiles
            .iter()
            .map(|&t| if t.is_blank() { Tile::new('a') } else { t })
            .collect::<Vec<_>>();
        debug_assert_eq!(applied_blanks.len(), tiles.len());

        // Now calculate the best point value from all blank combinations.
        let mut best = (0, Vec::new());
        let mut first = true;
        loop {
            // Find the next blank combination.
            if first {
                first = false;
            } else {
                let mut done = true;
                for blank_index_rindex in 0..blank_indices.len() {
                    let n = blank_indices
                        [blank_indices.len() - blank_index_rindex - 1];
                    match applied_blanks[n].as_char() {
                        'z' => applied_blanks[n] = Tile::new('a'),
                        ch => {
                            applied_blanks[n] =
                                Tile::new((ch as u8 + 1) as char);
                            done = false;
                            break;
                        }
                    }
                }
                if done {
                    break;
                }
            }

            // Now find the total point value of the tiles for this blank
            // combination.
            self.run_index += 1;
            let mut total_score = 0;
            for i in 0..applied_blanks.len() {
                if let Some(n) = non_blank_point_values[i] {
                    total_score += n;
                    continue;
                }

                let mut w = self.span.walker_mut();
                let mut score_at_this_position = 0;
                let mut next_word_score = 0;
                for (len, &t) in applied_blanks[i..].iter().enumerate() {
                    next_word_score += tiles[i + len].point_value();
                    match w.walk(t.letter().unwrap()) {
                        SpanResult::DeadEnd => break,
                        SpanResult::Prefix => continue,
                        SpanResult::Word(&mut (
                            ref mut n,
                            ref mut prev_best_score,
                        )) => {
                            let index_matches = !allow_dups
                                && (*n == no_blank_run_index
                                    || *n == self.run_index);
                            if index_matches
                                && *prev_best_score < next_word_score
                            {
                                debug_assert_eq!(*n, self.run_index,);
                                score_at_this_position +=
                                    next_word_score - *prev_best_score;
                                *prev_best_score = next_word_score;
                            } else if index_matches {
                                // word already counted
                            } else {
                                score_at_this_position += next_word_score;
                                *n = self.run_index;
                                *prev_best_score = next_word_score;
                            }
                        }
                    }
                }

                total_score += score_at_this_position;
            }
            if total_score > best.0 {
                best = (total_score, applied_blanks.clone());
            }
        }

        best.0
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn tile_values() {
        use super::*;
        assert_eq!(Tile::new('?').point_value(), 0);
        assert_eq!(Tile::new('a').point_value(), 1);
        assert_eq!(Tile::new('z').point_value(), 10);
    }

    #[test]
    fn dictionary_converts_to_lowercase() {
        use super::*;
        let d = Dictionary::load_from_reader(
            b"alpha\n\
            BRAVO\n\
            charlie"
                .as_ref(),
        )
        .unwrap();
        assert_eq!(
            d.words().map(|s| s.clone()).collect::<HashSet<_>>(),
            vec!["alpha", "bravo", "charlie"]
                .iter()
                .map(|&s| String::from(s))
                .collect::<HashSet<_>>()
        );
    }

    #[test]
    fn dictionary_ignores_empty_lines() {
        use super::*;
        let d = Dictionary::load_from_reader(
            b"alpha\n\
            \n\
            bravo"
                .as_ref(),
        )
        .unwrap();
        assert_eq!(
            d.words().map(|s| s.clone()).collect::<HashSet<_>>(),
            vec!["alpha", "bravo"]
                .iter()
                .map(|&s| String::from(s))
                .collect::<HashSet<_>>()
        );
    }

    #[test]
    fn dictionary_ignores_leading_and_trailing_whitespace() {
        use super::*;
        let d = Dictionary::load_from_reader(
            b" alpha\n bravo \n\
            charlie \n\
            delta "
                .as_ref(),
        )
        .unwrap();
        assert_eq!(
            d.words().map(|s| s.clone()).collect::<HashSet<_>>(),
            vec!["alpha", "bravo", "charlie", "delta"]
                .iter()
                .map(|&s| String::from(s))
                .collect::<HashSet<_>>()
        );
    }

    #[test]
    fn dictionary_maintains_longest_word_len() {
        use super::*;
        let d = Dictionary::load_from_reader(
            b"alpha\n\
            bravo\n\
            charlie\n"
                .as_ref(),
        )
        .unwrap();
        assert_eq!(d.longest_word_len(), 7);
    }

    #[test]
    fn span() {
        use super::*;

        let d = Dictionary::load_from_reader(
            b"dog\n\
            doge\n\
            doges\n\
            dogmatic\n\
            dogs\n"
                .as_ref(),
        )
        .unwrap();

        let mut s = Span::<()>::new(&d);
        let mut w = s.walker();
        assert_eq!(w.walk(Letter::new('c')), SpanResult::DeadEnd);
        assert_eq!(w.walk(Letter::new('d')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('o')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('g')), SpanResult::Word(&()));
        let w2 = w;
        assert_eq!(w.walk(Letter::new('e')), SpanResult::Word(&()));
        assert_eq!(w.walk(Letter::new('s')), SpanResult::Word(&()));
        w = w2;
        assert_eq!(w.walk(Letter::new('s')), SpanResult::Word(&()));
        assert_eq!(w.walk(Letter::new('s')), SpanResult::DeadEnd);
        w = w2;
        assert_eq!(w.walk(Letter::new('m')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('a')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('t')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('i')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('c')), SpanResult::Word(&()));
        assert_eq!(w.walk(Letter::new('z')), SpanResult::DeadEnd);

        // Same test cases as before, but with the mutable walker.

        let mut w = s.walker_mut();
        assert_eq!(w.walk(Letter::new('c')), SpanResult::DeadEnd);
        assert_eq!(w.walk(Letter::new('d')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('o')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('g')), SpanResult::Word(&mut ()));
        let w2 = w;
        assert_eq!(w.walk(Letter::new('e')), SpanResult::Word(&mut ()));
        assert_eq!(w.walk(Letter::new('s')), SpanResult::Word(&mut ()));
        w = w2;
        assert_eq!(w.walk(Letter::new('s')), SpanResult::Word(&mut ()));
        assert_eq!(w.walk(Letter::new('s')), SpanResult::DeadEnd);
        w = w2;
        assert_eq!(w.walk(Letter::new('m')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('a')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('t')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('i')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('c')), SpanResult::Word(&mut ()));
        assert_eq!(w.walk(Letter::new('z')), SpanResult::DeadEnd);

        // Mutated payload persists across walks.

        let mut s = Span::<&'static str>::new(&d);
        let mut w = s.walker_mut();
        assert_eq!(w.walk(Letter::new('d')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('o')), SpanResult::Prefix);
        match w.walk(Letter::new('g')) {
            SpanResult::Word(x) => *x = "alpha",
            _ => unreachable!(),
        }
        let mut w = s.walker_mut();
        assert_eq!(w.walk(Letter::new('d')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('o')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('g')), SpanResult::Word(&mut "alpha"));

        // Mutated payload can be reset.

        s.reset_payloads_to_default();
        let mut w = s.walker_mut();
        assert_eq!(w.walk(Letter::new('d')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('o')), SpanResult::Prefix);
        assert_eq!(w.walk(Letter::new('g')), SpanResult::Word(&mut ""));
    }

    #[test]
    fn does_use_blank() {
        // Testing private function because its implementation is complicated.

        use super::*;

        macro_rules! tc {
            ($dict:expr, $str:expr, $expected:expr) => {
                assert_eq!(
                    does_use_blank(
                        $dict,
                        &$str
                            .chars()
                            .map(|ch| Tile::new(ch))
                            .collect::<Vec<_>>()
                    ),
                    $expected
                );
            };
        }

        let d = Dictionary::load_from_reader(
            b"dog\n\
            doge\n\
            doges\n\
            dogged\n\
            doggedly\n\
            dogleg\n\
            dogma\n\
            dogmatic\n\
            dogs\n"
                .as_ref(),
        )
        .unwrap();
        let d = Span::<()>::new(&d);

        tc!(&d, "", false);
        tc!(&d, "dog", false);
        tc!(&d, "cat", false);
        tc!(&d, "?og", true);
        tc!(&d, "?o", false);
        tc!(&d, "d?g", true);
        tc!(&d, "do?", true);
        tc!(&d, "???", true);
        tc!(&d, "??", false);
        tc!(&d, "dot", false);
        tc!(&d, "d?t", false);
        tc!(&d, "??t", false);
        tc!(&d, "do", false);
        tc!(&d, "d?", false);
        tc!(&d, "doges?", false);
        tc!(&d, "doge?", true);
        tc!(&d, "dog?atic", true);
    }

    #[test]
    fn point_value_of_all_words_without_anagram() {
        use super::*;

        macro_rules! tc {
            ($dict:expr, $str:expr, $expected:expr) => {
                let mut f = ContiguousWordFinder::new($dict);
                assert_eq!(
                    f.find_contiguous_words(
                        &$str
                            .chars()
                            .map(|ch| Tile::new(ch))
                            .collect::<Vec<_>>(),
                        false,
                    ),
                    $expected
                );
            };
        }

        let d = Dictionary::load_from_reader(
            b"dog\n\
            doge\n\
            doges\n\
            dogged\n\
            doggedly\n\
            dogleg\n\
            dogma\n\
            dogmatic\n\
            dogs\n\
            ma\n\
            mama\n\
            mamas\n\
            mar\n\
            mas\n\
            mat\n"
                .as_ref(),
        )
        .unwrap();

        tc!(&d, "", 0);
        tc!(&d, "dog", 5);
        tc!(&d, "dogdog", 5);

        // dog          5
        // dogMa        6
        // Ma           1
        tc!(&d, "dog?a", 12);

        // dog          5
        // dogma        9
        // dogmaTic    13
        // ma           4
        // maT          4
        tc!(&d, "dogma?ic", 35);

        // dog          5
        // dogMa        6
        // dogMaTic    10
        // Ma           1
        // MaT          1
        tc!(&d, "dog?a?ic", 23);

        // ma           4
        // mama         8
        // mamas        9
        // mas          5
        tc!(&d, "mamamamas", 26);

        // ma           4
        // mama         8
        // mamas        9
        // mas          5
        tc!(&d, "?amamamas", 26);

        // ma           4
        // mama         8
        // mamas        9
        // mas          5
        tc!(&d, "??mamamas", 26);

        // ma           4
        // mama         8
        // mamas        9
        // MaR
        // mas          5
        tc!(&d, "?a?amamas", 27);

        // ma           4
        // Mama         5
        // Mamas        6
        // mas          5
        tc!(&d, "?ama?amas", 20);
    }
}
