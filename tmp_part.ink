import std

trait show
	fn show(self) -> string

concept display<T: type>
	requires show<T>

concept addable<T: type>
	fn add(self: T, other: T) -> T

impl show for int
	fn show(this) -> string
		"int"

impl addable for int
	fn add(this, other: int) -> int
		this + other

fn label<T: type>(value: T) -> string where T: display
	value.show()

fn add_and_label<T: type>(a: T, b: T) -> string where T: addable, T: display
	const sum = a.add(b)
	sum.show()
