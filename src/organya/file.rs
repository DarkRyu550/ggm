/*! Utility for decoding the structures in the Organya file format. */

pub enum Version {
	Organya2,
	Organya3,
	Unknown
}

use std::convert::TryInto;
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Header([u8; 18]);
impl Header {
	/** Version of the format we're working with. */
	pub fn version(&self) -> Version { 
		match self.0[0..0x06] {
			[0x4f, 0x72, 0x67, 0x2d, 0x30, 0x32] => Version::Organya2,
			[0x4f, 0x72, 0x67, 0x2d, 0x30, 0x33] => Version::Organya3,
			_ => Version::Unknown
		}
	}

	/** Number of milliseconds between beats. */
	pub fn wait(&self) -> u16 { u16::from_le_bytes(self.0[0x06..0x08].try_into().unwrap()) }
	/** Number of beats in a step. */
	pub fn step(&self) -> u8  {  u8::from_le_bytes(self.0[0x09..0x0a].try_into().unwrap()) }
	/** Step number at which the loop should start. */
	pub fn lbeg(&self) -> u32 { u32::from_le_bytes(self.0[0x0a..0x0e].try_into().unwrap()) }
	/** Step number at which the loop should end. */
	pub fn lend(&self) -> u32 { u32::from_le_bytes(self.0[0x0e..0x12].try_into().unwrap()) }
}
use std::io::Read;
impl Header {
	fn read<R: Read>(r: &mut R) -> std::io::Result<Self> {
		let mut buffer = [0; 18];
		r.read_exact(&mut buffer[..])?;
		Ok(Self(buffer))
	}
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Instrument([u8; 6]);
impl Instrument {
	/** Frequency at which the instrument sample is going to be played. */
	pub fn pitch(&self) -> u16 { u16::from_le_bytes(self.0[0x00..0x02].try_into().unwrap()) }
	/** Instrument ID as an index into the wavetable. */
	pub fn instr(&self) -> u8  {  u8::from_le_bytes(self.0[0x02..0x03].try_into().unwrap()) }
	/** If greater than zero, sustaining n-otes is disabled. */
	pub fn sustn(&self) -> u8  {  u8::from_le_bytes(self.0[0x03..0x04].try_into().unwrap()) }
	/** Number of note events for this instrument. */
	pub fn notec(&self) -> u16 { u16::from_le_bytes(self.0[0x04..0x06].try_into().unwrap()) }
}
impl Instrument {
	fn read<R: Read>(r: &mut R) -> std::io::Result<Self> {
		let mut buffer = [0; 6];
		r.read_exact(&mut buffer[..])?;
		Ok(Self(buffer))
	}
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Note {
	/** Position in beats in which this note is to be played. */
	pub position: u32,
	/** Note value, ranges between 0 and 5f where 0 is C on a very low octave
	 * and increment by one corresponds to moving a piano key up. */
	pub value: u8,
	/** Length of time in beats for which this note will be playing. */
	pub length: u8,
	/** Volume at which the note will be played. */
	pub volume: u8,
	/** Panning value, ranging from 0 to c, where 0 is full left pan,
	 * 6 is centered, and c is full right pan. */
	pub panning: u8
}
impl Default for Note {
	fn default() -> Self {
		Self {
			position: 0,
			value:    0xff,
			length:   0xff,
			volume:   0xff,
			panning:  0xff,
		}
	}
}

const INSTRUMENTS: usize = 16;
pub struct Organya {
	pub file_header: Header,
	pub instruments: [Instrument; INSTRUMENTS],
	pub note_events: [Vec<Note>;  INSTRUMENTS]
}

pub fn read<R: Read>(mut r: &mut R) -> std::io::Result<Organya> {
	/* Load header data. */
	let header = Header::read(r)?;

	debug!("Header:");
	debug!("\tMilliseconds per beat:\t{}", header.wait());
	debug!("\tBeats per measure:\t{}", header.step());
	debug!("\tLoop starts at beat:\t{}", header.lbeg());
	debug!("\tLoop ends at beat:\t{}", header.lend());

	/* Load instrument data. */
	let mut instruments = [Instrument([0; 6]); INSTRUMENTS];
	for i in 0..INSTRUMENTS { 
		instruments[i] = Instrument::read(r)?;
		debug!("Instrument #{}:", i);
		debug!("\tPitch:\t{}", instruments[i].pitch());
		debug!("\tWave:\t{}", instruments[i].instr());
		debug!("\tPI Val:\t{}", instruments[i].sustn());
		debug!("\tNotes:\t{}", instruments[i].notec());
	}

	/* Load note data. */
	let mut notes: [Vec<Note>; INSTRUMENTS] = Default::default();
	
	use std::io::{Error, ErrorKind};
	let g_u32 = |r: &mut R| -> std::io::Result<[u8; 4]> 
		{ let mut n = [0; 4]; r.read_exact(&mut n)?; Ok(n) };
	let g_u8  = |r: &mut R| -> std::io::Result<[u8; 1]> 
		{ let mut n = [0; 1]; r.read_exact(&mut n)?; Ok(n) };

	let n_u32 = |r: &mut R| -> std::io::Result<u32>
		{ Ok(u32::from_le_bytes(g_u32(r)?.try_into().unwrap())) };
	let n_u8 = |r: &mut R| -> std::io::Result<u8>
		{ Ok(u8::from_le_bytes(g_u8(r)?.try_into().unwrap())) };

	instruments.iter()
		.zip(notes.iter_mut())
		.map(|(instrument, notes)| {
	
		notes.resize_with(instrument.notec() as usize, Default::default);
		let range = || 0..instrument.notec() as usize;
		for note in range() { notes[note].position = n_u32(&mut r)?; }
		for note in range() { notes[note].value    = n_u8(&mut r)?;  }
		for note in range() { notes[note].length   = n_u8(&mut r)?;  }
		for note in range() { notes[note].volume   = n_u8(&mut r)?;  }
		for note in range() { notes[note].panning  = n_u8(&mut r)?;  }

		Ok(())
	}).collect::<std::io::Result<()>>()?;

	Ok(Organya {
		file_header: header,
		instruments,
		note_events: notes
	})
}

