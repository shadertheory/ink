import build

fn package()
	build::package
		name = "ink"
		version = "0.1.0"
		root = "ink"
		prelude = build::prelude
			std_items = "box_new sleep timeout deadline"
			std_scopes = "mem"
		profile = build::profile
			name = "debug"
			target = "vm"
			sandbox = true
		profile = build::profile
			name = "release"
			target = "vm"
		profile = build::profile
			name = "sim"
			target = "vm"
		module = build::module
			name = "ink"
			sources = "src"
