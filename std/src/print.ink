trait print_to<T: type>
	fn print_to(writer: int, value: T)

trait print<T: type>
	requires print_to<T>

#[foreign] fn print<ts: print...>(values: ts...)
#[foreign] fn println<ts: print...>(values: ts...)
#[foreign] fn print_int(value: int)
#[foreign] fn print_float(value: float)
#[foreign] fn print_bool(value: bool)
#[foreign] fn print_string(value: string)
#[foreign] fn print_sep()
#[foreign] fn print_line()
