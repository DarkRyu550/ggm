
#[cfg(feature = "organya")]
mod organya;

fn main() {
	#[cfg(feature = "organya")]
	organya::main();

	println!("cargo:rerun-if-changed=build/mod.rs");
}

