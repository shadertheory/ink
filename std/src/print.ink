trait print_to<T: type>
	fn print_to(writer: i64, value: T)

trait print<T: type>
	requires print_to<T>

#[foreign] #[sandbox(category=io, allowed)] #[sim(category=io, allowed)] fn print<ts: print...>(values: ts...)
#[foreign] #[sandbox(category=io, allowed)] #[sim(category=io, allowed)] fn println<ts: print...>(values: ts...)
#[foreign] #[sandbox(category=io, allowed)] #[sim(category=io, allowed)] fn print_int(value: i64)
#[foreign] #[sandbox(category=io, allowed)] #[sim(category=io, allowed)] fn print_float(value: float)
#[foreign] #[sandbox(category=io, allowed)] #[sim(category=io, allowed)] fn print_bool(value: bool)
#[foreign] #[sandbox(category=io, allowed)] #[sim(category=io, allowed)] fn print_string(value: string)
#[foreign] #[sandbox(category=io, allowed)] #[sim(category=io, allowed)] fn print_sep()
#[foreign] #[sandbox(category=io, allowed)] #[sim(category=io, allowed)] fn print_line()
