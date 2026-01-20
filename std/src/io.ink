type io_error = i64

trait stream
	fn read(self, out: buf) -> result<i64, io_error>
	fn write(self, data: []u8) -> result<i64, io_error>

#[foreign] #[sandbox(category=time, allowed)] #[sim(category=time, allowed)] fn sleep(duration: duration) -> result<i64, io_error>
#[foreign] #[sandbox(category=time, allowed)] #[sim(category=time, allowed)] fn sleep_until(deadline: deadline) -> result<i64, io_error>
#[foreign] #[sandbox(category=io, allowed)] #[sim(category=io, allowed)] fn read(fd: fd, buf: buf) -> result<i64, io_error>
#[foreign] #[sandbox(category=io, allowed)] #[sim(category=io, allowed)] fn write(fd: fd, buf: buf) -> result<i64, io_error>
#[foreign] #[sandbox(category=io, allowed)] #[sim(category=io, allowed)] fn accept(fd: fd) -> result<fd, io_error>

fn sleep(deadline: deadline) -> result<i64, io_error>
	sleep_until(deadline)

fn sleep(instant: instant) -> result<i64, io_error>
	sleep_until(deadline(instant))
