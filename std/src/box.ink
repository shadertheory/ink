fn box_new<T: type>(value: T) -> box<T>
	let words = @type_words(value)
	let ptr = alloc(words)
	let src = (&value) as i64
	mut idx = 0
	while idx < words
		*(ptr + idx) = *(src + idx)
		idx += 1
	ptr as box<T>

fn box_free<T: type>(value: box<T>)
	let ptr = value as i64
	if ptr == 0
		0
	else
		free(ptr)

fn box_ptr<T: type>(value: box<T>) -> i64
	value as i64

fn box_from_ptr<T: type>(ptr: i64) -> box<T>
	ptr as box<T>
