type io_error = int

trait stream
	fn read(self, out: buf) -> result<int, io_error>
	fn write(self, data: bytes) -> result<int, io_error>

#[foreign] fn sleep(duration: int) -> result<int, io_error>
#[foreign] fn read(fd: int, buf: buf) -> result<int, io_error>
#[foreign] fn write(fd: int, buf: buf) -> result<int, io_error>
#[foreign] fn accept(fd: int) -> result<int, io_error>
