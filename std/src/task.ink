type timeout = int

#[foreign] #[sandbox(category=task, allowed)] #[sim(category=task, allowed)] fn cancelled() -> bool
#[foreign] #[sandbox(category=time, allowed)] #[sim(category=time, allowed)] fn timeout(duration: duration) -> deadline
#[foreign] #[sandbox(category=time, allowed)] #[sim(category=time, allowed)] fn deadline(value: instant) -> deadline

fn deadline(value: deadline) -> deadline
	value

fn deadline(value: duration) -> deadline
	timeout(value)

fn cancel<T: type>(task: task<T>)
	0

fn sleep_task(duration: duration) -> result<int, io_error>
	sleep(duration)

fn timeout<T: type>(duration: duration, task: task<T>) -> result<T, timeout>
	let timer = spawn sleep_task(duration)
	select
		case value = await task => @result_ok(value)
		case _ = await timer => @result_err(error::timeout)
