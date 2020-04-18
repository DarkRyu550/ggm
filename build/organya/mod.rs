
pub fn main() {
	let dir = std::path::Path::new(&std::env::var_os("OUT_DIR").unwrap())
		.join("organya/");
	let out = std::path::Path::new(&dir).join("wavetable.rs");
	let wto = std::path::Path::new(&dir).join("wavetable.dat");
	let wti = std::path::Path::new("build/")
		.join("organya/")
		.join("wavetable.dat")
		.canonicalize().unwrap();

	
	std::fs::create_dir_all(&dir).unwrap();
	use std::fs::File;
	let mut out = File::create(out).unwrap();
	
	use std::io::Write;
	if wti.exists() && !cfg!(feature = "no-organya-builtin-wavetable") {
		std::fs::copy(&wti, &wto).unwrap();
		writeln!(out, "#[allow(dead_code)] \
			pub const WAVETABLE: Option<&'static [u8]> = Some(include_bytes!(\"{}\"));\
		", wto.canonicalize().unwrap().to_str().unwrap()).unwrap();
	} else {
		println!("Not building wavetable because {}", if wti.exists() {
			"wavetable building has been disabled at the feature level".to_owned()
		} else if !cfg!(feature = "no-organya-builtin-wavetable") {
			format!("the file at {} does not exist", wti.to_str().unwrap())
		} else {
			"the feature has been disabled and the file does not exist".to_owned()
		});

		writeln!(out, "#[allow(dead_code)] \
			pub const WAVETABLE: Option<&'static [u8]> = None;\
		").unwrap();
	}

	println!("cargo:rerun-if-changed=build/organya/mod.rs");
	println!("cargo:rerun-if-changed=build/organya/wavetable.dat");
}

