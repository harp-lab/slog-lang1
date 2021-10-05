"""
auto complete for REPL

Yihao SUn
"""

from prompt_toolkit.completion.fuzzy_completer import FuzzyWordCompleter
from prompt_toolkit.document import Document
from prompt_toolkit.completion import PathCompleter
from prompt_toolkit.completion.base import Completer

class StringPathCompeleter(Completer):
    """ completer for '<file path>' """

    def get_completions(self, document, complete_event):
        text = document.text_before_cursor
        # stripped_len = len(document.text_before_cursor) - len(text)
        if "\"" in text:
            remain_document = Document(
                text[1:],
                cursor_position=document.cursor_position - 1
            )
            yield from PathCompleter().get_completions(remain_document, complete_event)

class PrefixWordCompleter(Completer):
    """ completer for a set world but start with some prefix """

    def __init__(self, prefix, words):
        super().__init__()
        self.prefix = prefix
        self.words = words

    def get_completions(self, document, complete_event):
        text = document.text_before_cursor
        # stripped_len = len(document.text_before_cursor) - len(text)
        if self.prefix in text:
            remain_document = Document(
                text[1:],
                cursor_position=document.cursor_position - 1
            )
            yield from FuzzyWordCompleter(self.words). \
                        get_completions(remain_document, complete_event)

class SequencialCompleter(Completer):
    """ sequencially nest a a list of completer (text sep by ' ') """

    def __init__(self, completers) -> None:
        super().__init__()
        self.completers = completers
        self.counter = 0

    def get_completions(self, document, complete_event):
        text = document.text_before_cursor.lstrip()
        stripped_len = len(document.text_before_cursor) - len(text)
        if self.counter >= len(self.completers):
            return []
        if " " in text:
            first_term = text.split()[0]
            completer = self.completers[self.counter]
            # If we have a sub completer, use this for the completions.
            if completer is not None:
                remaining_text = text[len(first_term) :].lstrip()
                move_cursor = len(text) - len(remaining_text) + stripped_len
                new_document = Document(
                    remaining_text,
                    cursor_position=document.cursor_position - move_cursor,
                )
                yield from completer.get_completions(new_document, complete_event)
                self.counter = self.counter + 1
        else:
            # first one
            yield from self.completers[0].get_completions(document, complete_event)
