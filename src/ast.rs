
#[derive(Debug)]
pub enum AstError {
	InvalidHeader,
	InvalidFragmentHeader,
	IoError(std::io::Error)
}

use std::io::Cursor;
pub struct Ast<S> {
	source:		S,
	length:		u32,
	channels:	u16,
	samplerate:	u32,
	samples:	u32,
	loopstart:	u32,
	fragment:	Cursor<Vec<u8>>
}
use std::io::Read;
impl<S: Read> Ast<S> {
	pub fn new(mut source: S) -> Result<Ast<S>, AstError> {
		let map_io = |what| AstError::IoError(what);

		/* Constant expected pieces of the header. */
		const HEADER_LENGTH: usize = 64;
		const HEADER_CONSTS: &'static [(usize, &'static [u8])] = &[
			(0x0000, &[0x53, 0x54, 0x52, 0x4d]),
			/*(0x0008, &[0x00, 0x01, 0x00, 0x10]),*/
			(0x000e, &[0xff, 0xff]),
		];

		let mut header = vec![0; HEADER_LENGTH];
		source.read_exact(&mut header[..HEADER_LENGTH]).map_err(map_io)?;

		/* Check the integrity of the header. */
		for (offset, data) in HEADER_CONSTS {
			let offset = *offset;
			let frag = header.get(offset..offset + data.len())
				.ok_or(AstError::InvalidHeader)?;
			if &frag != data { return Err(AstError::InvalidHeader) }
		}

		/* Load some useful information from the header. */
		use std::convert::TryInto;
		let length      = u32::from_be_bytes(header[0x0004..0x0008].try_into().unwrap());
		let channels    = u16::from_be_bytes(header[0x000c..0x000e].try_into().unwrap());
		let samplerate  = u32::from_be_bytes(header[0x0010..0x0014].try_into().unwrap());
		let samples     = u32::from_be_bytes(header[0x0014..0x0018].try_into().unwrap());
		let loopstart   = u32::from_be_bytes(header[0x0018..0x001c].try_into().unwrap());

		eprintln!("length:\t\t{}",		length);
		eprintln!("channels:\t{}",		channels);
		eprintln!("samplerate:\t{}",	samplerate);
		eprintln!("samples:\t{}",		samples);
		eprintln!("loopstart:\t{}",		loopstart);

		Ok(Ast { 
			source, 
			length, 
			channels, 
			samplerate, 
			samples, 
			loopstart, 
			fragment: Cursor::new(Vec::new()) 
		})
	}

	fn _pull_fragment(&mut self) -> Result<(), AstError> {
		let map_io = |what| AstError::IoError(what);
		
		const HEADER_LENGTH: usize = 32;
		const HEADER_CONSTS: &'static [(usize, &'static [u8])] = &[
			(0x0000, &[0x42, 0x4C, 0x43, 0x4B]),
		];

		let mut header = vec![0; HEADER_LENGTH];
		self.source.read_exact(&mut header[..HEADER_LENGTH]).map_err(map_io)?;

		/* Check the integrity of the header. */
		for (offset, data) in HEADER_CONSTS {
			let offset = *offset;
			let frag = header.get(offset..offset + data.len())
				.ok_or(AstError::InvalidFragmentHeader)?;
			if &frag != data { return Err(AstError::InvalidFragmentHeader) }
		}

		/* Load some useful information from the header. */
		use std::convert::TryInto;
		let size = u32::from_be_bytes(header[0x0004..0x0008].try_into().unwrap());

		/* Load the data for the next fragment in. */
		let channels = self.channels as usize;
		let length   = size as usize;

		let mut vec = vec![0; channels * length + 1];
		for i in 0..1 {
			let mut contents = vec![0; length];
			self.source.read_exact(&mut contents[..length]).map_err(map_io)?;

			for (j, sample) in (0..).into_iter().zip(contents.chunks(2)) {
				vec[(channels * j + i) * 2 + 0] = sample[0];
				vec[(channels * j + i) * 2 + 1] = sample[1];
			}
		}
		
		self.fragment = Cursor::new(vec);
		Ok(())
	}
}
impl<S: Read> Read for Ast<S> {
	fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
		let map_ast = |what| match what {
			AstError::IoError(what) => what,
			what @ _ => std::io::Error::new(
				std::io::ErrorKind::Other,
				format!("{:?}", what))
		};

		/* Figure out how much room we still have in the currenf fragment. */
		use std::io::{Seek, SeekFrom};
		let offset = self.fragment.seek(SeekFrom::Current(0))? as usize;
		let length = self.fragment.get_ref().len();
		if offset == length || offset > length {
			/* We need to load in another fragment. */
			self._pull_fragment().map_err(map_ast)?;
		}

		/* Read from the current fragment into the buffer. */
		self.fragment.read(buf)
	}
}

