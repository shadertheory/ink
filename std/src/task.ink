type timeout = int

#[foreign] fn cancelled() -> bool

fn cancel<T: type>(task: task<T>)
	0

fn sleep_task(duration: duration) -> result<int, io_error>
	sleep(duration)

fn timeout<T: type>(duration: duration, task: task<T>) -> result<T, timeout>
	let timer = spawn sleep_task(duration)
	select
		case value = await task => @result_ok(value)
		case _ = await timer => @result_err(error::timeout)
