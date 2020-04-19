/** Contains the built-in wavetable, if there is one. See `build/organya/mod.rs`
 * for more information on what this means. */
pub const WAVETABLE: Option<&'static [u8]> = 
	include!(concat!(env!("OUT_DIR"), "/organya/wavetable"));

pub const WAVE_LENGTH: usize = 0x100;

pub struct Wavetable<'a>(pub &'a [u8]);
impl<'a> Wavetable<'a> {
	/** Number of sampled waves in the given table. */
	pub fn len(&self) -> usize { self.0.len() / WAVE_LENGTH }
	/** Is the length of the wavetable exactly a multiple of the wave length? */
	pub fn whole(&self) -> bool { self.0.len() % WAVE_LENGTH == 0 }
	/** Get the wave data at the given index. */
	pub fn get(&self, index: usize) -> Option<&[u8]> {
		self.0.get(index * WAVE_LENGTH..(index + 1) * WAVE_LENGTH)
	}
	/** Get the wave data at the given index mod length. */
	pub fn mget(&self, index: usize) -> &[u8] {
		self.get(index % self.len()).unwrap()
	}
	/** Get an instrument mod length in a given octave. */
	pub fn moctave(&'a self, index: usize, octave: u8) -> Octave<'a> {
		if octave >= 8 { panic!("Invalid octave number {}", octave) }
		Octave {
			class: (octave as i8) - 2,
			wave:  self.mget(index)
		}
	}
}

pub fn decode_note(note: u8) -> (u8, Class) {
	let pitch  = note % 12;
	let octave = note / 12;
	(
		octave,
		unsafe { std::mem::transmute(pitch) }
	)
}

#[derive(Debug, Clone)]
pub struct Octave<'a> {
	/** This number indicates by how much the wave signal has to be shrunk or
	 * stretched, as a power of two. Numbers bigger than zero shrink the wave,
	 * producing higher octaves, while numbers smaller than zero stretch it,
	 * producing lower octaves. Zero leaves the wave in the third octave. */
	class: i8,

	/** The data for the current instrument. */
	wave: &'a [u8],
}
impl<'a> Octave<'a> {
	/** Number of distinct sample points in the current octave. */
	pub fn len(&self) -> usize {
		if self.class < 0 { 
			self.wave.len() << -self.class 
		} else {
			self.wave.len() >> self.class
		}
	}
	/** Get a sample from the current wave. */
	pub fn get(&self, index: usize) -> Option<&u8> {
		/* According to http://rnhart.net/orgmaker/pitch.htm, the octaves
		 * in OrgMaker are produced by generating waves in the form: 
		 * <octave>	<procedure>									<points>	
		 *	0		every source point is used 4 times			1024
		 *	1		every source point is used 2 times			512
		 *	2		every other source point is used 2 times	256
		 *	3		every 4th source point is used 2 times		128
		 *	4		every 8th source point is used 2 times		64
		 *	5		every 16th source point is used 2 times		32
		 *	6		every 32nd source point is used 2 times		16
		 *	7		every 64th source point is used 2 times		8
		 *
		 * In Octave<'a>, these octave numbers are encoded in the octave class
		 * value, which ranges from -2 being the lowest octave to 5 being the
		 * highest. The reson for this comes from the fact the source wave is
		 * assumed to already be in the third octave, and the octave class value
		 * only offsets the octave.
		 */
		let index = if self.class < 0 {
			index >> -self.class
		} else {
			(index & !1) << self.class
		};
		self.wave.get(index)
	}
	/** Get a sample in position mod the length of the wave. */
	pub fn mget(&self, index: usize) -> &u8 {
		self.get(index % self.len()).unwrap()
	}
}


#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u8)]
#[allow(dead_code)]
pub enum Class {
	C  = 0,
	CS = 1,
	D  = 2,
	DS = 3,
	E  = 4,
	F  = 5,
	FS = 6,
	G  = 7,
	GS = 8,
	A  = 9,
	AS = 10,
	B  = 11
}
impl Class {
	/** How many samples to play every second in order to produce the desired
	 * pitch class in a given octave. */
	pub fn freq(&self) -> usize {
		match *self {
			Class::C  => 33408,
			Class::CS => 35584,
			Class::D  => 37632,
			Class::DS => 39808,
			Class::E  => 42112,
			Class::F  => 44672,
			Class::FS => 47488,
			Class::G  => 50048,
			Class::GS => 52992,
			Class::A  => 56320,
			Class::AS => 59648,
			Class::B  => 63232
		}
	}

}

