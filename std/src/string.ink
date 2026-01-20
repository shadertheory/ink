#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_new(capacity: i64) -> string
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_free(value: string)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_len(value: string) -> i64
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_cap(value: string) -> i64
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_ptr(value: string) -> i64
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_set_len(value: string, len: i64)
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_from_int(value: i64) -> string
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_from_float(value: float) -> string
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_from_bool(value: bool) -> string
#[foreign] #[sandbox(category=mem, allowed)] #[sim(category=mem, allowed)] fn string_concat(left: string, right: string) -> string

fn string_index_int(value: string, idx: i64) -> char
	let len = string_len(value)
	if idx < 0
		0
	else if (idx as i64) >= len
		0
	else
		let src = string_ptr(value)
		let word = *(src + (idx / 8))
		let shift = (idx % 8) * 8
		((word >> shift) & 255) as char

fn string_index_set_int(value: string, idx: i64, ch: char)
	let len = string_len(value)
	if idx < 0
		0
	else if (idx as i64) >= len
		0
	else
		let ptr = string_ptr(value)
		let word_ptr = ptr + (idx / 8)
		let word = *word_ptr
		let shift = (idx % 8) * 8
		let mask = 255 << shift
		let new_word = (word & ~mask) | (((ch as i64) & 255) << shift)
		*word_ptr = new_word

impl string
	fn index<I: int>(this, idx: I) -> char
		string_index_int(this, idx as i64)
	fn index_set<I: int>(this, idx: I, value: char)
		string_index_set_int(this, idx as i64, value)
