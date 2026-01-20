import std

#[attribute]
comptime fn emit_helper(args: token_stream, item: token_stream) -> token_stream
	if std::token_stream_len(args) == 0
		std::error(std::span_here(), "emit_helper expects an argument")
		std::token_stream_empty()
	else
		let arg_head = std::token_stream_get(args, 0)
		if arg_head == 0
			std::error(std::span_here(), "emit_helper expects an argument")
			std::token_stream_empty()
		else
			if std::token_tree_kind(arg_head) != token_tree_kind::token
				std::error(std::token_tree_span(arg_head), "emit_helper expects an identifier argument")
				std::token_stream_empty()
			else
				let arg_token = std::token_tree_token(arg_head)
				if std::token_kind(arg_token) != token_kind::identifier
					std::error(std::token_tree_span(arg_head), "emit_helper expects an identifier argument")
					std::token_stream_empty()
				else
					let line = std::token_new(token_kind::new_line, std::span_here(), none)
					let line_tree = std::token_tree_from_token(line)
					let line_stream = std::token_stream_push(std::token_stream_empty(), line_tree)
					let extra = std::quote("let macro_tag = 73")
					let suffix = std::token_stream_concat(line_stream, extra)
					std::token_stream_concat(item, suffix)

comptime fn token_stream_drop_new_lines(value: token_stream) -> token_stream
	let count = std::token_stream_len(value)
	mut idx = 0
	mut out = std::token_stream_empty()
	while idx < count
		let tree = std::token_stream_get(value, idx)
		if tree != 0
			if std::token_tree_kind(tree) == token_tree_kind::token
				let tok = std::token_tree_token(tree)
				if std::token_kind(tok) == token_kind::new_line
					continue
			out = std::token_stream_push(out, tree)
		idx += 1
	out

comptime fn add_tokens(left: token_stream, right: token_stream) -> token_stream
	let clean_left = token_stream_drop_new_lines(left)
	let clean_right = token_stream_drop_new_lines(right)
	if std::token_stream_len(clean_left) == 0 or std::token_stream_len(clean_right) == 0
		std::error(std::span_here(), "add_tokens expects two token streams")
		std::token_stream_empty()
	else
		mut span = std::span_here()
		let head = std::token_stream_get(clean_left, 0)
		if head == 0
			std::error(std::span_here(), "add_tokens missing head")
			std::token_stream_empty()
		else
			let next_span = std::token_tree_span(head)
			span = next_span
			let plus_token = std::token_new(token_kind::plus, span, none)
			let plus_tree = std::token_tree_from_token(plus_token)
			let plus_stream = std::token_stream_push(std::token_stream_empty(), plus_tree)
			let with_plus = std::token_stream_concat(clean_left, plus_stream)
			std::token_stream_concat(with_plus, clean_right)

comptime fn sum_pair(pair: token_stream) -> token_stream
	if std::token_stream_len(pair) == 0
		std::error(std::span_here(), "sum_pair expects (a, b)")
		std::token_stream_empty()
	else
		let head = std::token_stream_get(pair, 0)
		if head == 0
			std::error(std::span_here(), "sum_pair expects (a, b)")
			std::token_stream_empty()
		else
			if std::token_tree_kind(head) != token_tree_kind::group
				std::error(std::token_tree_span(head), "sum_pair expects (a, b)")
				std::token_stream_empty()
			else
				let group = std::token_tree_group(head)
				if std::token_group_delimiter(group) != delimiter::paren
					std::error(std::token_group_span(group), "sum_pair expects (a, b)")
					std::token_stream_empty()
				else
					let inner = std::token_group_stream(group)
					let inner_len = std::token_stream_len(inner)
					mut comma_index = -1
					mut idx = 0
					while idx < inner_len
						let item = std::token_stream_get(inner, idx)
						if item != 0
							if std::token_tree_kind(item) == token_tree_kind::token
								let tok = std::token_tree_token(item)
								if std::token_kind(tok) == token_kind::comma
									comma_index = idx
									break
						idx += 1
					if comma_index < 0 or comma_index == 0 or comma_index == inner_len - 1
						std::error(std::token_group_span(group), "sum_pair expects (a, b)")
						std::token_stream_empty()
					else
						let left: token_stream = std::token_stream_slice(inner, 0, comma_index)
						let right: token_stream = std::token_stream_slice(inner, comma_index + 1, inner_len)
						if std::token_stream_len(left) == 0 or std::token_stream_len(right) == 0
							std::error(std::token_group_span(group), "sum_pair expects (a, b)")
							std::token_stream_empty()
						else
							add_tokens(left, right)

comptime fn pow_int(base: i64, exp: i64) -> i64
	mut result = 1
	mut idx = 0
	while idx < exp
		result *= base
		idx += 1
	result

comptime fn triangular(n: i64) -> i64
	mut acc = 0
	mut i = 1
	while i <= n
		acc += i
		i += 1
	acc

comptime fn gcd_int(a: i64, b: i64) -> i64
	mut x = a
	mut y = b
	while y != 0
		let tmp = x % y
		x = y
		y = tmp
	x

trait show
	fn show(self) -> string

trait display<T: type>
	requires show<T>

trait numeric<T: type>
	requires display<T>, adder<T>

trait adder<T: type>
	fn add(self: T, other: T) -> T

trait indexable
	fn index<I: int>(self, idx: I) -> i64
	fn index_set<I: int>(self, idx: I, value: i64)

#[record]
struct point
	x: i64
	y: i64

#[record]
struct int2
	ptr: i64

#[repr(i64)]
enum maybe<T: type>
	none
	some(T)

#[emit_helper(tag)]
let macro_anchor = 1

impl show for i64
	fn show(this) -> string
		"i64"

impl adder for i64
	fn add(this, other: i64) -> i64
		this + other

impl adder for point
	fn add(this, other: point) -> point
		point
			x = this.x + other.x
			y = this.y + other.y

impl show for string
	fn show(this) -> string
		this

impl show for point
	fn show(this) -> string
		this

impl indexable for int2
	fn index<I: int>(this, idx: I) -> i64
		std::deref(this.ptr + (idx as i64))
	fn index_set<I: int>(this, idx: I, value: i64)
		std::store(this.ptr + (idx as i64), value)

fn int2_new(a: i64, b: i64) -> int2
	let ptr = std::alloc(2)
	*ptr = 2
	*(ptr + 1) = 3
	int2
		ptr = ptr

fn int2_free(value: int2)
	std::free(value.ptr)

fn label<T: type>(value: T) -> string where T: display
	value.show()

fn add_and_label<T: type>(a: T, b: T) -> string where T: numeric
	let total = a + b
	total.show()

fn main() -> i64
	let p1 = point
		x = 3
		y = 4
	let p2 = point
		x = 10
		y = 2
	let p5 = 10000000
	let p3 = p1 + p2
	let equiv =  (p1 as dyn show).show()
	std::print("point sum label is {add_and_label(p1, p2)}")
	std::print("point sum is ({p3.x}, {p3.y})")
	std::print("int sum label is {add_and_label(20, 2)}")
	std::print("dyn label is { equiv}")
	let dyn_a = 7 as dyn adder
	let dyn_b = 5 as dyn adder
	let dyn_sum = dyn_a + dyn_b
	std::print("dyn add is {dyn_sum}")
	mut n = 42
	n += 5
	n -= 3
	n *= 2
	n /= 3
	n %= 7
	n <<= 1
	n >>= 1
	n |= 8
	n &= 15
	n ^= 1
	
	let neg_n = -n
	let inv_n = ~n
	let mod_n = n % 5
	let shifty = (n << 2) + (n >> 1)
	let cmp_lt = n < 100
	let cmp_le = n <= 100
	let cmp_gt = n > 0
	let cmp_ge = n >= 0
	let cmp_eq = n == mod_n
	let cmp_ne = n != 0
	let logic = (true xor false) or (false and not false)
	let check = ! (n == 0)
	std::print("arith {neg_n} {inv_n} {mod_n} {shifty}")
	std::print("cmp {cmp_lt} {cmp_le} {cmp_gt} {cmp_ge} {cmp_eq} {cmp_ne} {logic} {check}")
	mut pair = int2_new(5, 9)
	pair[0] += 3
	pair[1] = pair[1] * 2
	let pair_sum = pair[0] + pair[1]
	std::print("pair sum is {pair_sum}")
	int2_free(pair)
	let bytes = std::bytes_from_string("AZ")
	let view: []u8 = std::slice_new(std::bytes_ptr(bytes), std::bytes_len(bytes))
	view[0] += (1 as u8)
	let first_byte = view[0]
	std::print("slice[0] is {first_byte}")
	std::bytes_free(bytes)
	let message = label("hello")
	std::print("string label is {message}")
	let pow_val = pow_int(3, 5)
	let tri_val = triangular(10)
	let gcd_val = gcd_int(84, 30)
	std::print("comptime values {pow_val} {tri_val} {gcd_val}")
	let macro_sum_one = add_tokens!
		20
		22
	let macro_sum_two = sum_pair!
		(21, 21)
	std::print("macro sums {macro_sum_one} {macro_sum_two} tag {macro_tag}")
	0
