type timeout = int

#[foreign] fn cancelled() -> bool

fn cancel<T: type>(task: task<T>)
	0

fn sleep_task(duration: int) -> result<int, io_error>
	sleep(duration)

fn timeout<T: type>(duration: int, task: task<T>) -> result<T, timeout>
	const timer = spawn sleep_task(duration)
	select
		case value = await task => @result_ok(value)
		case _ = await timer => @result_err(error::timeout)
