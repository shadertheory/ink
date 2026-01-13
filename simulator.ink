import sim

fn simulator()
	sim::simulator
		seed = 0
		concurrency = "half"
		foreigns = sim::foreigns
			allow_categories = "mem io time task macro"
		snapshots = sim::snapshots
			steps = 1
			mode = "full+delta"
			compress = "zstd"
		validation = sim::validation
			level = "strict"
		scenario = sim::scenario
			name = "default"
			components = sim::components
				tcp = "mock"
				udp = "mock"
				fs = "real"
				clock = "sim"
				rng = "sim"
				alloc = "sim"
				scheduler = "sim"
			faults = sim::faults
		scenario = sim::scenario
			name = "chaos"
			components = sim::components
				tcp = "mock"
				udp = "mock"
				fs = "real"
				clock = "sim"
				rng = "sim"
				alloc = "sim"
				scheduler = "sim"
			faults = sim::faults
				tcp_drop = 0.01
				udp_drop = 0.01
				io_error = 0.001
				oom = 0.001
