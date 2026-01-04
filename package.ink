import build

fn package()
	build::package
		name = "ink"
		version = "0.1.0"
		root = "ink"
		module = build::module
			name = "ink"
			sources = "src"
