fn bytes_new(capacity: i64) -> bytes
	list_new(capacity)

fn bytes_from_string(value: string) -> bytes
	let len = string_len(value)
	let out = list_new(len)
	let src = string_ptr(value)
	let dst = list_ptr(out)
	mut idx = 0
	while idx < len
		let word = *(src + (idx / 8))
		let shift = (idx % 8) * 8
		let byte = (word >> shift) & 255
		*(dst + idx) = byte
		idx += 1
	list_set_len(out, len)
	out

fn bytes_free(value: bytes)
	list_free(value)

fn bytes_len(value: bytes) -> i64
	list_len(value)

fn bytes_cap(value: bytes) -> i64
	list_cap(value)

fn bytes_ptr(value: bytes) -> i64
	list_ptr(value)

fn bytes_set_len(value: bytes, len: i64)
	list_set_len(value, len)

impl indexable for bytes
	fn index<I: int>(this, idx: I) -> i64
		let idx_i = idx as i64
		let len = bytes_len(this)
		if idx_i < 0
			0
		else if idx_i >= len
			0
		else
			let ptr = bytes_ptr(this)
			*(ptr + idx_i)
	fn index_set<I: int>(this, idx: I, value: i64)
		let idx_i = idx as i64
		let len = bytes_len(this)
		if idx_i < 0
			0
		else if idx_i >= len
			0
		else
			let ptr = bytes_ptr(this)
			*(ptr + idx_i) = value & 255
