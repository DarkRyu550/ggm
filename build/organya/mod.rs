
pub fn main() {
	builtin_wavetable();
	pixtone_waveforms();
	pixtone_effects();
	println!("cargo:rerun-if-changed=build/organya/mod.rs");
}

fn pixtone_effects() {		
	use std::path::Path;
	let dir = std::path::Path::new("build/")
		.join("organya/")
		.join("fx/");
	println!("cargo:rerun-if-changed=build/organya/fx/");

	use std::env;
	use std::fs::File;
	let out_dir = Path::new(&env::var_os("OUT_DIR").unwrap()).join("organya/");
	let out = Path::new(&out_dir).join("pixtone_effects");
	let mut out = File::create(out).unwrap();

	use std::io::Write;
	if !dir.exists() {
		eprintln!("PixTone directory build/organya/fx/ does not exist");
		write!(out, "None").unwrap();
		return
	}
	if cfg!(feature = "no-organya-builtin-pixtone") {
		eprintln!("Building PixTones into the binary has been disabled");
		write!(out, "None").unwrap();
		return
	}
	let dir = dir.canonicalize().unwrap();

	let mut files = Vec::new();
	for entry in dir.read_dir().unwrap() {
		let entry = entry.unwrap();
		let entry = entry.path();
		
		if entry.is_file() {
			files.push(entry);
		}
	}
	let files = files.into_iter()
		.map(|file| (file.canonicalize(), file.file_stem().and_then(|s| s.to_str().and_then(|s| Some(s.to_owned())))))
		.filter(|(file, stem)| file.is_ok() && match stem {
			&Some(ref s) => s.len() == 2 
				&& s.chars().find(|c| !c.is_ascii_hexdigit()).is_none(),
			&None => false
		})
		.map(|(file, stem)| (file.unwrap().to_str().map(|s| s.to_owned()), u8::from_str_radix(&stem.unwrap(), 16)))
		.filter(|(file, stem)| file.is_some() && stem.is_ok())
		.map(|(file, stem)| (file.unwrap(), stem.unwrap()));

	write!(out, "Some(&[").unwrap();
	for (path, code) in files {
		write!(out, r#"({}, include_bytes!("{}")), "#, code, path).unwrap();
	}
	write!(out, "])").unwrap();
}

fn pixtone_waveforms() {
	const SAMPLE_LENGTH: usize = 0x100;
	const PIXTONE_PI:    f64   = 3.1416;
	/* Pre-generate the base waveforms used by PixTone. Bit-exact with version
	 * 1.0.3. PixTone has six builtin waves which are combined in different
	 * ways to produce any given sound effect.
	 * These builtin waves are:
	 *	<Index>	<Name>
	 *	0		Sine
	 *	1		Triangle
	 *	2		Sawtooth Up
	 *	3		Sawtooth Down
	 *	4		Square
	 *	5		Noise
	 */
	let mut waveforms: [Vec<i8>; 6] = Default::default();
	let mut waves: [i8; 6] = Default::default();
	let mut seed: u32 = 0;
	for i in 0..SAMPLE_LENGTH {
		seed = (seed.wrapping_mul(214013)).wrapping_add(2531011);

		let line = i as f64 / SAMPLE_LENGTH as f64;
		waves[0] = (64.0 * f64::sin(line * PIXTONE_PI * 2.0)) as i8;
		waves[1] = if (i + 0x40) & 0x80 != 0 { 0x7f_i8.wrapping_sub(i as i8) } else { i as i8 };
		waves[2] = -0x40 + (i / 2) as i8;
		waves[3] =  0x40 - (i / 2) as i8;
		waves[4] = if i < SAMPLE_LENGTH / 2 { 0x40 } else { -0x40 };
		waves[5] = (seed >> 16) as i8 / 2;

		waves.iter()
			.zip(waveforms.iter_mut())
			.for_each(|(sample, bank)| {
				bank.push(*sample);
			});
	}

	let string = waveforms.iter()
		.fold(String::new(), |s, wave| {
			format!("{}{}{:?}",
				s,
				if s.len() > 0 { ", " } else { "" }, 
				wave)
		});
	let string = format!(r#"
		 [{}]
	"#, string);

	use std::path::Path;
	use std::env;
	use std::fs::File;

	let dir = Path::new(&env::var_os("OUT_DIR").unwrap()).join("organya/");
	let out = Path::new(&dir).join("pixtone_waveforms");

	std::fs::create_dir_all(&dir).unwrap();
	let mut out = File::create(out).unwrap();

	use std::io::Write;
	writeln!(out, "{}", string).unwrap();
}

fn builtin_wavetable() {
	let dir = std::path::Path::new(&std::env::var_os("OUT_DIR").unwrap())
		.join("organya/");
	let out = std::path::Path::new(&dir).join("wavetable");
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
		writeln!(out, 
			"Some(include_bytes!(\"{}\"))", 
			wto.canonicalize().unwrap().to_str().unwrap()
		).unwrap();
	} else {
		println!("Not building wavetable because {}", if wti.exists() {
			"wavetable building has been disabled at the feature level".to_owned()
		} else if !cfg!(feature = "no-organya-builtin-wavetable") {
			format!("the file at {} does not exist", wti.to_str().unwrap())
		} else {
			"the feature has been disabled and the file does not exist".to_owned()
		});

		writeln!(out, 
			"None"
		).unwrap();
	}

	println!("cargo:rerun-if-changed=build/organya/wavetable.dat");

}

