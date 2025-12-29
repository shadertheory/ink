trait show<T: type>
	type Output = self
	fn show(this) -> Output

concept printable<T: type>
	requires show<T>

sum option<T: type>
	some(T)
	none

enum ordering
	less
	equal
	greater

struct point
	x: int
	y: int
	label: ?string

struct box<T: type>
	value: T

impl show for point
	fn show(this) -> self
		return this

fn negate(x: int) -> int
	-x

fn choose(flag: bool) -> int
	if not flag
		return 0
	else
		return 1

fn invert(flag: bool) -> bool
	!flag

fn classify(n: int) -> ordering
	match n 0 => less
		1 => greater

fn fib<num: type>(n: num) -> num where num: type
	if n < 1
		0
	else if n == 1
		1
	else
		fib(n - 1) + fib(n - 2)

fn nonnegative(x: int) -> bool
	x >= 0

fn small(x: int) -> bool
	x <= 10

fn nonzero(x: int) -> bool
	x != 0

fn half(x: int) -> int
	x / 2

fn scale<N: int>(x: int) -> int
	return x * N

fn box_it<T: type>(x: T) -> box<T>
	return box(x)

fn pi() -> float
	3.14

fn main() -> int
	s < 3.14
	structure<generic>
	choose(true) |> fib(1) |> negate(1)
