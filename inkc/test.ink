trait add<rhs: type = self>
	type output = self
	fn add(this, other: rhs) -> output

trait sub<rhs: type = self>
	type output = self
	fn sub(this, other: rhs) -> output

trait zero
	type output = self
	fn zero() -> output

trait one
	type output = self
	fn one() -> output 

sum option<inner: type>
	some(inner)
	none

enum ordering
	less
	equal
	greater

trait ord
	fn cmp(this, other: self) -> ?ordering 

trait eq
	fn eq(this, other: self) -> bool

concept arithmetic<rhs: type = self>
	requires add<rhs>
	requires sub<rhs>
	requires zero
	requires one
	requires ord

fn fib<num: type>(n: num) -> num where num: arithmetic 
	if n < 1
		0
	else if n == 1
		1
	else
		fib(n - 1) + fib(n - 2)
