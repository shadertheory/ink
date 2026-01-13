fn list_header<T: type>(value: list<T>) -> int
	value as int

fn list_ptr<T: type>(value: list<T>) -> int
	let header = list_header(value)
	if header == 0
		0
	else
		*header

fn list_len<T: type>(value: list<T>) -> int
	let header = list_header(value)
	if header == 0
		0
	else
		*(header + 1)

fn list_cap<T: type>(value: list<T>) -> int
	let header = list_header(value)
	if header == 0
		0
	else
		*(header + 2)

fn list_set_len<T: type>(value: list<T>, len: int)
	let header = list_header(value)
	if header == 0
		0
	else
		let cap = *(header + 2)
		if len <= cap
			*(header + 1) = len

fn list_copy_words(dst: int, src: int, count: int)
	mut idx = 0
	while idx < count
		*(dst + idx) = *(src + idx)
		idx += 1

fn list_store_value<T: type>(dst: int, value: T)
	let src = (&value) as int
	let words = @type_words(value)
	mut idx = 0
	while idx < words
		*(dst + idx) = *(src + idx)
		idx += 1

fn list_new<T: type>(capacity: int) -> list<T>
	let stride = @type_words(T)
	mut data_words = capacity * stride
	if data_words == 0
		data_words = 1
	let data_ptr = alloc(data_words)
	let header = alloc(3)
	*header = data_ptr
	*(header + 1) = 0
	*(header + 2) = capacity
	header as list<T>

fn list_free<T: type>(value: list<T>)
	let header = list_header(value)
	if header == 0
		0
	else
		let data_ptr = *header
		free(data_ptr)
		free(header)

fn list_reserve<T: type>(value: list<T>, capacity: int) -> list<T>
	let header = list_header(value)
	if header == 0
		list_new(capacity)
	else
		let cap = *(header + 2)
		if capacity <= cap
			value
		else
			let stride = @type_words(T)
			mut data_words = capacity * stride
			if data_words == 0
				data_words = 1
			let new_ptr = alloc(data_words)
			let old_ptr = *header
			let len = *(header + 1)
			let count = len * stride
			list_copy_words(new_ptr, old_ptr, count)
			free(old_ptr)
			*header = new_ptr
			*(header + 2) = capacity
			value

fn list_push<T: type>(value: list<T>, item: T) -> list<T>
	let header = list_header(value)
	if header == 0
		list_push(list_new(1), item)
	else
		let len = *(header + 1)
		let cap = *(header + 2)
		mut out = value
		if len >= cap
			let new_cap = if cap == 0
				1
			else
				cap * 2
			out = list_reserve(out, new_cap)
		let out_header = list_header(out)
		let stride = @type_words(item)
		let data_ptr = *out_header
		let dst = data_ptr + (len * stride)
		list_store_value(dst, item)
		*(out_header + 1) = len + 1
		out
