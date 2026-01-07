import std

trait show
	fn show(self) -> string

trait display<T: type>
	requires show<T>

trait numeric<T: type>
	requires display<T>, adder<T>

trait adder<T: type>
	fn add(self: T, other: T) -> T

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

impl adder for int
	fn add(this, other: int) -> int
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
	fn hello(this) -> string
		this



impl indexable for int2
	fn index(this, idx: int) -> int
		std::deref(this.ptr + idx)
	fn index_set(this, idx: int, value: int)
		std::store(this.ptr + idx, value)

fn int2_new(a: int, b: int) -> int2
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

fn main() -> int
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
	let view: []int = std::slice_new(std::bytes_ptr(bytes), std::bytes_len(bytes))
	view[0] += 1
	let first_byte = view[0]
	std::print("slice[0] is {first_byte}")
	std::bytes_free(bytes)
	let message = label("hello")
	std::print("string label is {message}")
	0
