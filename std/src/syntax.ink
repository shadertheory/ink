type symbol = int
type span = int
type token = int
type token_group = int
type token_tree = int
type token_stream = int

enum delimiter
	paren
	bracket
	block

enum token_kind
	new_line
	identifier
	label
	string
	number
	comma
	colon
	dot
	ellipsis
	hash
	at_sign
	function
	constant
	variable
	expr_if
	expr_else
	expr_match
	expr_select
	case
	detached
	stmt_return
	stmt_break
	stmt_continue
	yield
	loop
	while
	until
	repeat
	for
	each
	sleep
	timeout
	deadline
	spawn
	await
	try
	atomic
	auto
	box
	logical_or
	logical_and
	logical_xor
	logical_not
	logical_false
	logical_true
	assign
	plus_assign
	minus_assign
	asterisk_assign
	slash_assign
	percent_assign
	ampersand_assign
	bar_assign
	caret_assign
	shift_left_assign
	shift_right_assign
	plus
	minus
	asterisk
	slash
	percent
	ampersand
	bar
	caret
	shift_left
	shift_right
	bang
	tilde
	less_than
	greater_than
	less_or_equal
	greater_or_equal
	equal
	not_equal
	enum
	type
	arrow
	question
	question_dot
	coalesce
	range
	range_inclusive
	double_colon
	pipe
	in
	trait
	impl
	as
	import
	from
	with
	dynamic
	comptime
	struct
	where
	self
	this
	mut
	ref
	requires
	dyn

enum token_tree_kind
	token
	group

enum token_tree_value
	tree_token(token)
	tree_group(token_group)

#[record]
struct token_stream_cursor
	stream: token_stream
	index: int

#[foreign] fn span_source(value: span) -> int
#[foreign] fn span_start(value: span) -> int
#[foreign] fn span_end(value: span) -> int
#[foreign] fn span_join(left: span, right: span) -> span
#[foreign] fn span_here() -> span

#[foreign] fn token_kind(value: token) -> token_kind
#[foreign] fn token_symbol(value: token) -> ?symbol
#[foreign] fn token_span(value: token) -> span
#[foreign] fn token_new(kind: token_kind, span: span, symbol: ?symbol) -> token

#[foreign] fn token_group_delimiter(value: token_group) -> delimiter
#[foreign] fn token_group_stream(value: token_group) -> token_stream
#[foreign] fn token_group_span(value: token_group) -> span
#[foreign] fn token_group_new(delimiter: delimiter, span: span, stream: token_stream) -> token_group

#[foreign] fn token_tree_kind(value: token_tree) -> token_tree_kind
#[foreign] fn token_tree_token(value: token_tree) -> token
#[foreign] fn token_tree_group(value: token_tree) -> token_group
#[foreign] fn token_tree_span(value: token_tree) -> span
#[foreign] fn token_tree_from_token(value: token) -> token_tree
#[foreign] fn token_tree_from_group(value: token_group) -> token_tree

#[foreign] fn token_stream_len(value: token_stream) -> int
#[foreign] fn token_stream_get(value: token_stream, index: int) -> token_tree
#[foreign] fn token_stream_slice(value: token_stream, start: int, end: int) -> token_stream
#[foreign] fn token_stream_concat(left: token_stream, right: token_stream) -> token_stream
#[foreign] fn token_stream_push(value: token_stream, tree: token_tree) -> token_stream
#[foreign] fn token_stream_empty() -> token_stream

#[foreign] fn error(span: span, message: string)
#[foreign] fn quote(value: string) -> token_stream

fn token_tree_value(value: token_tree) -> token_tree_value
	if token_tree_kind(value) == token_tree_kind::token
		token_tree_value::tree_token(token_tree_token(value))
	else
		token_tree_value::tree_group(token_tree_group(value))

fn token_tree_is_token(value: token_tree) -> bool
	token_tree_kind(value) == token_tree_kind::token

fn token_tree_is_group(value: token_tree) -> bool
	token_tree_kind(value) == token_tree_kind::group

fn token_stream_cursor_new(value: token_stream) -> token_stream_cursor
	token_stream_cursor
		stream = value
		index = 0

fn token_stream_cursor_peek(value: token_stream_cursor) -> token_tree
	token_stream_get(value.stream, value.index)

fn token_stream_cursor_next(value: token_stream_cursor) -> token_stream_cursor
	token_stream_cursor
		stream = value.stream
		index = value.index + 1

fn token_stream_cursor_len(value: token_stream_cursor) -> int
	token_stream_len(value.stream)

fn token_stream_cursor_is_end(value: token_stream_cursor) -> bool
	value.index >= token_stream_len(value.stream)
