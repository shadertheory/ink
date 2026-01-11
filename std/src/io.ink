type io_error = int

trait stream
	fn read(self, out: buf) -> result<int, io_error>
	fn write(self, data: []u8) -> result<int, io_error>

#[foreign] fn sleep(duration: duration) -> result<int, io_error>
#[foreign] fn sleep_until(deadline: deadline) -> result<int, io_error>
#[foreign] fn read(fd: int, buf: buf) -> result<int, io_error>
#[foreign] fn write(fd: int, buf: buf) -> result<int, io_error>
#[foreign] fn accept(fd: int) -> result<int, io_error>

fn sleep(deadline: deadline) -> result<int, io_error>
	sleep_until(deadline)

fn sleep(instant: instant) -> result<int, io_error>
	sleep_until(deadline(instant))
