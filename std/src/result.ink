fn ok<T: type, E: type>(value: T) -> result<T, E>
	@result_ok(value)

fn err<T: type, E: type>(value: E) -> result<T, E>
	@result_err(value)

fn is_ok<T: type, E: type>(value: result<T, E>) -> bool
	@result_is_ok(value)

fn unwrap<T: type, E: type>(value: result<T, E>) -> T
	@result_unwrap(value)

fn unwrap_err<T: type, E: type>(value: result<T, E>) -> E
	@result_unwrap_err(value)
