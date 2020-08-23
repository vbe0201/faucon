//! Rustyline helper that will manage highlighting and command completion.

use ansi_term::Style;
use rustyline::{
    completion::{extract_word, Candidate, Completer},
    highlight::Highlighter,
    hint::Hinter,
    validate::Validator,
};
use rustyline_derive::Helper;
use std::borrow::Cow;

#[derive(Helper)]
pub struct Helper {
    commands: &'static [&'static str],
}

impl Default for Helper {
    fn default() -> Self {
        Self {
            commands: super::Command::variants(),
        }
    }
}

/// Wrapper around a `&'static str` to be used for completion candidates.
pub struct CompletionCandidate {
    display: &'static str,
}

impl Candidate for CompletionCandidate {
    fn display(&self) -> &str {
        self.display
    }

    fn replacement(&self) -> &str {
        self.display
    }
}

impl Completer for Helper {
    type Candidate = CompletionCandidate;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let (idx, word) = extract_word(line, pos, None, &[]);

        let commands = self
            .commands
            .iter()
            .filter(|cmd| cmd.starts_with(word))
            .map(|x| CompletionCandidate { display: x })
            .collect::<Vec<_>>();

        Ok((idx, commands))
    }
}

impl Highlighter for Helper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        let prompt = Style::new().bold().paint(prompt);
        Cow::Owned(prompt.to_string())
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        let hint = Style::new().dimmed().paint(hint);
        Cow::Owned(hint.to_string())
    }
}

impl Hinter for Helper {}
impl Validator for Helper {}
