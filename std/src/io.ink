type io_error = int

trait stream
	fn read(self, out: buf) -> result<int, io_error>
	fn write(self, data: []u8) -> result<int, io_error>

#[foreign] #[sandbox(category=time, allowed)] #[sim(category=time, allowed)] fn sleep(duration: duration) -> result<int, io_error>
#[foreign] #[sandbox(category=time, allowed)] #[sim(category=time, allowed)] fn sleep_until(deadline: deadline) -> result<int, io_error>
#[foreign] #[sandbox(category=io, allowed)] #[sim(category=io, allowed)] fn read(fd: fd, buf: buf) -> result<int, io_error>
#[foreign] #[sandbox(category=io, allowed)] #[sim(category=io, allowed)] fn write(fd: fd, buf: buf) -> result<int, io_error>
#[foreign] #[sandbox(category=io, allowed)] #[sim(category=io, allowed)] fn accept(fd: fd) -> result<fd, io_error>

fn sleep(deadline: deadline) -> result<int, io_error>
	sleep_until(deadline)

fn sleep(instant: instant) -> result<int, io_error>
	sleep_until(deadline(instant))
