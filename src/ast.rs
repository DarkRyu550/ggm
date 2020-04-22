
const HEADER_LENGTH: usize = 64;
struct Header([u8; HEADER_LENGTH]);

use std::convert::TryInto;
impl Header {
	/** Check the header against the known constant bits. */
	fn check(&self) -> bool {
		const HEADER_CONSTS: &'static [(usize, &'static [u8])] = &[
			(0x0000, &[0x53, 0x54, 0x52, 0x4d]),
			(0x000e, &[0xff, 0xff]),
		];

		/* Check the integrity of the header. */
		HEADER_CONSTS.into_iter()
			.find(|(offset, data)| {
				let offset = *offset;
				let frag = match self.0.get(offset..offset + data.len()) {
					Some(frag) => frag,
					None => return true
				};
				&frag != data
			}).is_none()
	}
	/** Total size of all the BLCK chunks in bytes. */ 
	fn length(&self) -> u32 { u32::from_be_bytes(self.0[0x0004..0x0008].try_into().unwrap()) }
	/** Number of audio channels in the music file. */
	fn chans(&self) -> u16 { u16::from_be_bytes(self.0[0x000c..0x000e].try_into().unwrap()) }
	/** Number of audio samples to be played in a second */
	fn srate(&self) -> u32 { u32::from_be_bytes(self.0[0x0010..0x0014].try_into().unwrap()) }
	/** Total number of samples accross all BLCK chunks. */
	fn smpls(&self) -> u32 { u32::from_be_bytes(self.0[0x0014..0x0018].try_into().unwrap()) }
	/** The looping part of the song at this sample. */
	fn lpbeg(&self) -> u32 { u32::from_be_bytes(self.0[0x0018..0x001c].try_into().unwrap()) }
}

const BLCK_HEADER_LENGTH: usize = 32;
struct BlckHeader([u8; BLCK_HEADER_LENGTH]);
impl BlckHeader {
	/** Check the header against the known constant bits. */
	fn check(&self) -> bool {
		const HEADER_CONSTS: &'static [(usize, &'static [u8])] = &[
			(0x0000, &[0x42, 0x4C, 0x43, 0x4B]),
		];

		/* Check the integrity of the header. */
		HEADER_CONSTS.into_iter()
			.find(|(offset, data)| {
				let offset = *offset;
				let frag = match self.0.get(offset..offset + data.len()) {
					Some(frag) => frag,
					None => return true
				};
				&frag != data
			}).is_none()
	}
	/** Size of each channel block in this block, in bytes? */
	fn bsize(&self) -> u32 { u32::from_be_bytes(self.0[0x0004..0x0008].try_into().unwrap()) }
}

#[derive(Debug)]
pub enum Error {
	InvalidHeader,
	InvalidFragmentHeader,
	IoError(std::io::Error)
}
impl std::fmt::Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let message = match self { 
			&Self::InvalidHeader => 
				"Invalid header".to_owned(),
			&Self::InvalidFragmentHeader => 
				"Invalid fragment header".to_owned(),
			&Self::IoError(ref what) =>
				format!("I/O Error: {}", what)
		};

		write!(f, "{}", message)
	}
}

pub struct Ast<S> {
	source:  S,
	header:  Header,
	samples: Vec<i16>,
	csample: usize,
}
use std::io::Read;
impl<S: Read> Ast<S> {
	pub fn new(mut source: S) -> Result<Ast<S>, Error> {
		let map_io = |what| Error::IoError(what);

		let mut header = Header([0; HEADER_LENGTH]);
		source.read_exact(&mut header.0[..]).map_err(map_io)?;
		if !header.check() { return Err(Error::InvalidHeader) }

		debug!("length:     {}", header.length());
		debug!("channels:   {}", header.chans());
		debug!("samplerate: {}", header.srate());
		debug!("samples:    {}", header.smpls());
		debug!("loopstart:  {}", header.lpbeg());

		Ok(Ast { 
			source, 
			header,
			samples: Vec::new(),
			csample: 0
		})
	}

	fn _pull_fragment(&mut self) -> Result<(), Error> {
		let map_io = |what| Error::IoError(what);
	
		let mut block = BlckHeader(Default::default());
		self.source.read_exact(&mut block.0[..]).map_err(map_io)?;
		if !block.check() { return Err(Error::InvalidFragmentHeader) }

		/* Reserve space in the buffer to hold all the song data. */
		use std::mem::size_of;

		let scount = block.bsize() as usize;
		self.samples.reserve(scount);

		/* Load the unwoven sample data into a buffer. */
		let mut tmp = Vec::with_capacity(scount);
		for _ in 0..scount {
			let mut data: [u8; size_of::<i16>()] = Default::default();
			self.source.read_exact(&mut data[..]).map_err(map_io)?;

			tmp.push(i16::from_be_bytes(data));
		}
		trace!("Loaded {} more samples", scount);

		/* Weave them in. */
		let scount = scount / self.header.chans() as usize;
		let chans  = self.header.chans() as usize;

		for sample in 0..scount {
			for chan in 0..chans {
				self.samples.push(tmp[scount * chan + sample]);
			}
		}
		
		Ok(())
	}
}

use crate::Song;
use crate::RemainingLength;
impl<S: Read> Song for Ast<S> {
	type Sample = i16;
	type Error  = Error; 

	fn blocksize(&self) -> Option<usize> { 
		None
	}
	fn remaining(&self) -> RemainingLength { 
		RemainingLength::Infinite
	}
	fn channels(&self) -> usize {
		self.header.chans() as usize
	}
	fn samplerate(&self) -> usize {
		self.header.srate() as usize
	}
	fn generate(&mut self, buf: &mut [Self::Sample]) -> Result<usize, Error> {
		/* If we don't have any more samples in the buffer, load more in. */
		if self.samples[self.csample..].len() == 0 { self._pull_fragment()?; }

		/* Just simply copy the samples over. */
		let length = buf.len();
		let target = &mut buf[..length - length % self.channels()];
		let source = &mut self.samples[self.csample..];

		let copies = usize::min(target.len(), source.len());
		for i in 0..copies{ target[i] = source[i] }

		self.csample += copies;
		Ok(copies)
	}
}

