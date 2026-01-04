import std

trait show
	fn show(self) -> string

trait display<T: type>
	requires show<T>

trait addable<T: type>
	fn add(self: T, other: T) -> T

trait numeric<T: type>
	requires display<T>, addable<T>

trait adder
	fn add(self, other: dyn adder) -> int
	fn to_int(self) -> int

trait indexable
	fn index(self, idx: int) -> int
	fn index_set(self, idx: int, value: int)

#[record]
struct point
	x: int
	y: int

#[record]
struct int2
	ptr: int

#[repr(int)]
enum maybe<T: type>
	none
	some(T)

impl show for int
	fn show(this) -> string
		"int"

impl addable for int
	fn add(this, other: int) -> int
		this + other

impl adder for int
	fn add(this, other: dyn adder) -> int
		this + other.to_int()
	fn to_int(this) -> int
		this

impl show for string
	fn show(this) -> string
		this

impl show for point
	fn show(this) -> string
		"point"

impl addable for point
	fn add(this, other: point) -> point
		point
			x = this.x + other.x
			y = this.y + other.y

impl indexable for int2
	fn index(this, idx: int) -> int
		std::deref(this.ptr + idx)
	fn index_set(this, idx: int, value: int)
		std::store(this.ptr + idx, value)

fn int2_new(a: int, b: int) -> int2
	const ptr = std::alloc(2)
	std::store(ptr, a)
	std::store(ptr + 1, b)
	int2
		ptr = ptr

fn int2_free(value: int2)
	std::free(value.ptr)

fn label<T: type>(value: T) -> string where T: display
	value.show()

fn add_and_label<T: type>(a: T, b: T) -> string where T: numeric
	const total = a + b
	total.show()

fn main() -> int
	const p1 = point
		x = 3
		y = 4
	const p2 = point
		x = 10
		y = 2
	const p3 = p1 + p2
	std::print("point sum label is {add_and_label(p1, p2)}")
	std::print("point sum is ({p3.x}, {p3.y})")
	std::print("int sum label is {add_and_label(20, 2)}")
	std::print("dyn label is {(p1 as dyn show).show()}")
	const dyn_a = 7 as dyn adder
	const dyn_b = 5 as dyn adder
	const dyn_sum = dyn_a + dyn_b
	std::print("dyn add is {dyn_sum}")
	var n = 42
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
	const neg_n = -n
	const inv_n = ~n
	const mod_n = n % 5
	const shifty = (n << 2) + (n >> 1)
	const cmp_lt = n < 100
	const cmp_le = n <= 100
	const cmp_gt = n > 0
	const cmp_ge = n >= 0
	const cmp_eq = n == mod_n
	const cmp_ne = n != 0
	const logic = (true xor false) or (false and not false)
	const check = ! (n == 0)
	std::print("arith {neg_n} {inv_n} {mod_n} {shifty}")
	std::print("cmp {cmp_lt} {cmp_le} {cmp_gt} {cmp_ge} {cmp_eq} {cmp_ne} {logic} {check}")
	var pair = int2_new(5, 9)
	pair[0] += 3
	pair[1] = pair[1] * 2
	const pair_sum = pair[0] + pair[1]
	std::print("pair sum is {pair_sum}")
	int2_free(pair)
	const bytes = std::bytes_from_string("AZ")
	const view: slice<int> = std::slice_new(std::bytes_ptr(bytes), std::bytes_len(bytes))
	view[0] += 1
	const first_byte = view[0]
	std::print("slice[0] is {first_byte}")
	std::bytes_free(bytes)
	const message = label("hello")
	std::print("string label is {message}")
	0
