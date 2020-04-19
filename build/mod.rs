
#[cfg(feature = "organya")]
mod organya;

fn main() {
	#[cfg(feature = "organya")]
	organya::main();

}

