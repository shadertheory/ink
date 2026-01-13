import std

let fd_kind_shift = 60
let fd_kind_fs = 1
let fd_kind_tcp = 2
let fd_raw_mask = (1 << fd_kind_shift) - 1

fn fd_encode(raw: int, kind: int) -> fd
	let value = (raw & fd_raw_mask) | (kind << fd_kind_shift)
	value as fd

fn fd_stdout() -> fd
	fd_encode(1, fd_kind_fs)

fn fd_fake_tcp(raw: int) -> fd
	fd_encode(raw, fd_kind_tcp)

fn print_label_int(label: string, value: int)
	std::print_string(label)
	std::print_sep()
	std::print_int(value)
	std::print_line()

fn write_buf(target: buf, text: string)
	let bytes = std::bytes_from_string(text)
	std::buf_write_bytes(target, bytes)
	std::bytes_free(bytes)

fn write_line(fd: fd, io_buf: buf, text: string)
	std::buf_reset(io_buf)
	write_buf(io_buf, text)
	let res = std::write(fd, io_buf)
	if std::is_ok(res)
		print_label_int("fs write bytes:", std::unwrap(res))
	else
		let err: int = std::unwrap_err(res)
		print_label_int("fs write error:", err)

fn tcp_roundtrip(client: fd, io_buf: buf, read_buf: buf)
	std::buf_reset(io_buf)
	write_buf(io_buf, "tcp write payload")
	let write_res = std::write(client, io_buf)
	if std::is_ok(write_res)
		print_label_int("tcp write bytes:", std::unwrap(write_res))
		std::buf_reset(read_buf)
		let read_res = std::read(client, read_buf)
		if std::is_ok(read_res)
			print_label_int("tcp read bytes:", std::unwrap(read_res))
		else
			let err: int = std::unwrap_err(read_res)
			print_label_int("tcp read error:", err)
	else
		let err: int = std::unwrap_err(write_res)
		print_label_int("tcp write error:", err)

fn handle_client(client: fd) -> int
	let io_buf: buf = std::buf_new(64)
	let read_buf: buf = std::buf_new(64)
	tcp_roundtrip(client, io_buf, read_buf)
	write_line(fd_stdout(), io_buf, "FS-RAW: real fs if enabled\n")
	std::buf_free(io_buf)
	std::buf_free(read_buf)
	0

fn accept_and_spawn(listener: fd) -> int
	let accept_res = std::accept(listener)
	if std::is_ok(accept_res)
		print_label_int("tcp accept ok:", 1)
		let client = std::unwrap(accept_res)
		let child = spawn handle_client(client)
		let _ = await child
	else
		let err: int = std::unwrap_err(accept_res)
		print_label_int("tcp accept error:", err)
	0

fn fault_probe(listener: fd) -> int
	mut ok = 0
	mut err = 0
	mut i = 0
	while i < 32
		let res = std::accept(listener)
		if std::is_ok(res)
			ok += 1
		else
			err += 1
		i += 1
	std::print_string("fault probe accepts ok:")
	std::print_sep()
	std::print_int(ok)
	std::print_sep()
	std::print_string("err:")
	std::print_sep()
	std::print_int(err)
	std::print_line()
	0

fn main() -> int
	std::print_string("sim demo: mock tcp + fs write + fault injection")
	std::print_line()
	let listener = fd_fake_tcp(42)
	let accept_a = spawn accept_and_spawn(listener)
	let accept_b = spawn accept_and_spawn(listener)
	let probe = spawn fault_probe(listener)
	let _ = await accept_a
	let _ = await accept_b
	let _ = await probe
	0
