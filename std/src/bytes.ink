fn bytes_new(capacity: int) -> bytes
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

fn bytes_len(value: bytes) -> int
	list_len(value)

fn bytes_cap(value: bytes) -> int
	list_cap(value)

fn bytes_ptr(value: bytes) -> int
	list_ptr(value)

fn bytes_set_len(value: bytes, len: int)
	list_set_len(value, len)
