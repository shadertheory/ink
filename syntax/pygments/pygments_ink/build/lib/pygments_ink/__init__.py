from __future__ import annotations

import os
import re
from pathlib import Path

from pygments.lexer import RegexLexer, words
from pygments.token import Keyword, Name, Number, Operator, Punctuation, String, Text, Whitespace, Error

__all__ = ["InkLexer"]

DEFAULT_KEYWORDS = [
    "fn",
    "let",
    "mut",
    "if",
    "else",
    "match",
    "select",
    "case",
    "detached",
    "return",
    "dynamic",
    "comptime",
    "spawn",
    "await",
    "try",
    "or",
    "and",
    "xor",
    "not",
    "false",
    "true",
    "trait",
    "concept",
    "in",
    "impl",
    "as",
    "import",
    "from",
    "with",
    "for",
    "struct",
    "record",
    "self",
    "this",
    "type",
    "enum",
    "where",
    "sum",
    "requires",
]

DEFAULT_SYMBOLS = [
    "...",
    "..=",
    "->",
    "=>",
    "==",
    "!=",
    ">=",
    "<=",
    "?.",
    "??",
    "::",
    "..",
    "|>",
    "|",
    "&",
    "?",
    "]",
    "[",
    ")",
    "(",
    "+",
    "*",
    "/",
    "=",
    ":",
    "!",
    "<",
    ">",
    ",",
    ".",
    "#",
    "@",
    "-",
]


def _find_spec_path() -> Path | None:
    env_path = os.getenv("INK_SPEC_PATH")
    if env_path:
        candidate = Path(env_path)
        if candidate.is_file():
            return candidate

    for parent in Path(__file__).resolve().parents:
        candidate = parent / "ink" / "src" / "lang" / "spec.zig"
        if candidate.is_file():
            return candidate
    return None


def _extract_lexemes(text: str, section: str) -> list[str]:
    section_re = re.compile(rf"\b{re.escape(section)}\b")
    text_re = re.compile(r'\.text\s*=\s*"([^"]+)"')
    in_section = False
    items: list[str] = []
    for line in text.splitlines():
        if not in_section:
            if section_re.search(line) and "lexeme" in line and "{" in line:
                in_section = True
            continue
        if line.strip().startswith("};"):
            break
        match = text_re.search(line)
        if match:
            items.append(match.group(1))
    return items


def _load_lexemes() -> tuple[list[str], list[str]]:
    spec_path = _find_spec_path()
    if not spec_path:
        return list(DEFAULT_KEYWORDS), list(DEFAULT_SYMBOLS)

    try:
        text = spec_path.read_text(encoding="utf-8")
    except OSError:
        return list(DEFAULT_KEYWORDS), list(DEFAULT_SYMBOLS)

    keywords = _extract_lexemes(text, "keyword_lexemes")
    symbols = _extract_lexemes(text, "symbol_lexemes")
    if not keywords or not symbols:
        return list(DEFAULT_KEYWORDS), list(DEFAULT_SYMBOLS)

    return keywords, symbols


def _regex_union(items: list[str]) -> str | None:
    if not items:
        return None
    escaped = sorted(items, key=len, reverse=True)
    return "|".join(re.escape(item) for item in escaped)


def _build_tokens() -> dict[str, list[tuple[str, object]]]:
    keywords, symbols = _load_lexemes()

    bool_literals = {kw for kw in keywords if kw in {"true", "false"}}
    keyword_list = [kw for kw in keywords if kw not in bool_literals]

    punctuation = {"(", ")", "[", "]", ",", ":", "."}
    symbol_set = list(dict.fromkeys(symbols))
    operator_list = [sym for sym in symbol_set if sym not in punctuation]
    punct_list = [sym for sym in symbol_set if sym in punctuation]

    operator_re = _regex_union(operator_list)
    punct_re = _regex_union(punct_list)

    root: list[tuple[str, object]] = [
        (r"[ \t\r\n]+", Whitespace),
        (r'"', String, "string"),
    ]

    if bool_literals:
        root.append((words(sorted(bool_literals), suffix=r"\b"), Keyword.Constant))
    if keyword_list:
        root.append((words(keyword_list, suffix=r"\b"), Keyword))

    root.extend(
        [
            (r"\d+(?:\.\d+)?(?:[eE][+-]?\d+)?", Number),
            (r"[A-Za-z_][A-Za-z_0-9]*(?=[ \t]*\()", Name.Function),
            (r"[A-Za-z_][A-Za-z_0-9]*", Name),
        ]
    )

    if operator_re:
        root.append((operator_re, Operator))
    if punct_re:
        root.append((punct_re, Punctuation))

    root.append((r".", Text))

    return {
        "root": root,
        "string": [
            (r"\\.", String.Escape),
            (r'"', String, "#pop"),
            (r"[^\"\\\n]+", String),
            (r"\n", Error, "#pop"),
        ],
    }


class InkLexer(RegexLexer):
    name = "Ink"
    aliases = ["ink"]
    filenames = ["*.ink"]

    tokens = _build_tokens()
