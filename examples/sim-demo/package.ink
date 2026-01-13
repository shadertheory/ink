import build

fn package()
	build::package
		name = "sim-demo"
		version = "0.1.0"
		root = "src"
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
			name = "app"
			sources = "src"
