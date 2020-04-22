/** Utilities for loading PixTone files. */

pub const CHANNELS: usize = 4;

use std::io::BufRead;
/** Parse the contents of a PXT file as a set of key-value pairs, then, make
 * sure those values are valid by checking them against the content dump on
 * the last lines to make sure they are valid and consistent. */
pub fn parse_pxt<B: BufRead>(mut r: B) -> Result<[PxtChannel; CHANNELS], Error> {
	let mut channels: [PxtChannel; CHANNELS] = Default::default();
	let mut dumps = Vec::new();

	/* Utility functions */
	let iomaperr = |what| Error::IoError { error: what };
	let ref_msg  = |mesg: &'static str| Some(Cow::from(mesg));

	/* Parses a single line given a PtxChannel context. */
	let mut feed = |number: usize, line: String, chan: &mut PxtChannel| 
		-> Result<(), Error> {

		/* Ignore whitespace. */
		if line.split_whitespace().count() == 0 { return Ok(()) }
		if line.len() == 0 { return Ok(()) }

		/* In the format, keys and values are split with a collon. */
		let mut tokens = line.split(":");
		let     keys = tokens.next().unwrap();
		let mut keyw = keys.split_whitespace();
		let key = keyw.next()
			.ok_or(Error::ExpectedIdentifier {
				line:    number,
				column:  0,
				message: ref_msg("expected a key value, got nothing")
			})?;
		if let Some(extra) = keyw.next() {
			return Err(Error::IllegalIdentifier {
				got:     extra.to_owned(),
				line:    number,
				column:  key.len(),
				message: ref_msg("keys can only be composed of one word")
			})
		}

		/* All lines that have data and are not key-value pair are presumed to
		 * be lines containing instrument parameter dumps, and are saved. */
		let values = match tokens.next() {
			Some(value) => value,
			None => {
				dumps.push(keys.to_owned());
				return Ok(())
			}
		};
		let value = values.split_whitespace()
			.next()
			.unwrap_or("");

		let parse_usize = || usize::from_str_radix(value, 10)
			.map_err(|what| Error::ExpectedInteger {
				error:  what,
				got:    value.to_owned(),
				line:   number,
				column: keys.len() + 1
			});
		let parse_u8 = || u8::from_str_radix(value, 10)
			.map_err(|what| Error::ExpectedInteger {
				error:  what,
				got:    value.to_owned(),
				line:   number,
				column: keys.len() + 1
			});
		let parse_f64 = || {
			use std::str::FromStr;
			f64::from_str(value)
				.map_err(|what| Error::ExpectedFloat {
					error:  what,
					got:    value.to_owned(),
					line:   number,
					column: keys.len() + 1
				})
		};
		let dupeerror = || Error::IllegalIdentifier {
			line:    number,
			column:  0,
			got:     key.to_owned(),
			message: ref_msg("duplicate identifiers are invalid")
		};
		fn try_none<T>(op: Option<T>, err: impl Fn() -> Error) -> Result<(), Error>{
			match op {
				Some(_) => Err(err()),
				None => Ok(())
			}
		}

		match key {
			"use"			=> { try_none(chan.r#use.replace(parse_u8()?), dupeerror)?; },
			"size"			=> { try_none(chan.size.replace(parse_usize()?), dupeerror)?; },
			"main_model"	=> { try_none(chan.main_model.replace(parse_u8()?), dupeerror)?; },
			"main_freq"		=> { try_none(chan.main_freq.replace(parse_f64()?), dupeerror)?; },
			"main_top"		=> { try_none(chan.main_top.replace(parse_u8()?), dupeerror)?; },
			"main_offset"	=> { try_none(chan.main_offset.replace(parse_u8()?), dupeerror)?; },
			"pitch_model"	=> { try_none(chan.pitch_model.replace(parse_u8()?), dupeerror)?; },
			"pitch_freq"	=> { try_none(chan.pitch_freq.replace(parse_f64()?), dupeerror)?; },
			"pitch_top"		=> { try_none(chan.pitch_top.replace(parse_u8()?), dupeerror)?; },
			"pitch_offset"	=> { try_none(chan.pitch_offset.replace(parse_u8()?), dupeerror)?; },
			"volume_model"	=> { try_none(chan.volume_model.replace(parse_u8()?), dupeerror)?; },
			"volume_freq"	=> { try_none(chan.volume_freq.replace(parse_f64()?), dupeerror)?; },
			"volume_top"	=> { try_none(chan.volume_top.replace(parse_u8()?), dupeerror)?; },
			"volume_offset"	=> { try_none(chan.volume_offset.replace(parse_u8()?), dupeerror)?; },
			"initialY"		=> { try_none(chan.initial_y.replace(parse_u8()?), dupeerror)?; },
			"ax"			=> { try_none(chan.ax.replace(parse_u8()?), dupeerror)?; },
			"ay"			=> { try_none(chan.ay.replace(parse_u8()?), dupeerror)?; },
			"bx"			=> { try_none(chan.bx.replace(parse_u8()?), dupeerror)?; },
			"by"			=> { try_none(chan.by.replace(parse_u8()?), dupeerror)?; },
			"cx"			=> { try_none(chan.cx.replace(parse_u8()?), dupeerror)?; },
			"cy"			=> { try_none(chan.cy.replace(parse_u8()?), dupeerror)?; },
			_ => return Err(Error::IllegalIdentifier {
				line:    number,
				column:  0,
				got:     key.to_owned(),
				message: ref_msg("the given key is not valid")
			}) 
		}
		Ok(())
	};

	/* Feed the line parser with fresh lines, and start filling the next channel
	 * entry as soon as the last one is complete. */
	let mut line = 0_usize;
	let mut data = String::new();

	for i in 0..CHANNELS {
		while !channels[i].is_complete() {
			data.clear();
			if r.read_line(&mut data).map_err(iomaperr)? == 0 {
				return Err(Error::UnexpectedEof {
					message: ref_msg("while trying to get the next line")
				})
			}
			line += 1;

			feed(line, data.clone(), &mut channels[i])?
		}
	}

	/* Make sure the data dumps in the file match the expected values. */
	let dumps = dumps.into_iter().map(|string| {
		string.chars()
			.filter(|c| c.is_ascii_alphanumeric() || c == &',')
			.collect::<String>()
	}).collect::<Vec<_>>();

	for (channel, expected) in channels.iter().zip(dumps.into_iter()) {
		let dump = channel.strdump();
		if dump != expected {
			return Err(Error::ChannelDumpMismatch {
				expected: expected,
				got: dump
			})
		}
	}

	Ok(channels)
}

#[derive(Copy, Clone, Default, PartialEq, PartialOrd)]
pub struct PxtChannel {
	pub r#use:			Option<u8>,
	pub size:			Option<usize>,
	pub main_model: 	Option<u8>,
	pub main_freq:		Option<f64>,
	pub main_top:		Option<u8>,
	pub main_offset:	Option<u8>,
	pub pitch_model:	Option<u8>,
	pub pitch_freq:		Option<f64>,
	pub pitch_top:		Option<u8>,
	pub pitch_offset:	Option<u8>,
	pub volume_model:	Option<u8>,
	pub volume_freq:	Option<f64>,
	pub volume_top:		Option<u8>,
	pub volume_offset:	Option<u8>,
	pub initial_y:		Option<u8>,
	pub ax:				Option<u8>,
	pub ay:				Option<u8>,
	pub bx:				Option<u8>,
	pub by:				Option<u8>,
	pub cx:				Option<u8>,
	pub cy:				Option<u8>,
}
impl PxtChannel {
	fn is_complete(&self) -> bool {
		   self.r#use.is_some()
		&& self.size.is_some()
		&& self.main_model.is_some()
		&& self.main_freq.is_some()
		&& self.main_top.is_some()
		&& self.main_offset.is_some()
		&& self.pitch_model.is_some()
		&& self.pitch_freq.is_some()
		&& self.pitch_top.is_some()
		&& self.pitch_offset.is_some()
		&& self.volume_model.is_some()
		&& self.volume_freq.is_some()
		&& self.volume_top.is_some()
		&& self.volume_offset.is_some()
		&& self.initial_y.is_some()
		&& self.ax.is_some()
		&& self.ay.is_some()
		&& self.bx.is_some()
		&& self.by.is_some()
		&& self.cx.is_some()
		&& self.cy.is_some()
	}
	pub fn strdump(&self) -> String {
		if !self.is_complete() { panic!("Called strdump() on an incomplete parse"); }
		format!("{},{},\
			{},{:.2},{},{},\
			{},{:.2},{},{},\
			{},{:.2},{},{},\
			{},\
			{},{},{},{},{},{}",
			self.r#use.unwrap(),
			self.size.unwrap(),
			self.main_model.unwrap(),
			self.main_freq.unwrap(),
			self.main_top.unwrap(),
			self.main_offset.unwrap(),
			self.pitch_model.unwrap(),
			self.pitch_freq.unwrap(),
			self.pitch_top.unwrap(),
			self.pitch_offset.unwrap(),
			self.volume_model.unwrap(),
			self.volume_freq.unwrap(),
			self.volume_top.unwrap(),
			self.volume_offset.unwrap(),
			self.initial_y.unwrap(),
			self.ax.unwrap(),
			self.ay.unwrap(),
			self.bx.unwrap(),
			self.by.unwrap(),
			self.cx.unwrap(),
			self.cy.unwrap())
	}
}

use std::borrow::Cow;
#[derive(Debug)]
pub enum Error {
	ChannelDumpMismatch {
		expected: String,
		got: String
	},
	IllegalIdentifier {
		got:     String,
		line:    usize,
		column:  usize,
		message: Option<Cow<'static, str>>,
	},
	ExpectedIdentifier {
		line:    usize,
		column:  usize,
		message: Option<Cow<'static, str>>,
	},
	ExpectedFloat {
		got:     String,
		error:   std::num::ParseFloatError,
		line:    usize,
		column:  usize,
	},
	ExpectedInteger {
		got:     String,
		error:   std::num::ParseIntError,
		line:    usize,
		column:  usize,
	},
	UnexpectedEof {
		message: Option<Cow<'static, str>>
	},
	IoError {
		error: std::io::Error,
	}
}
impl std::fmt::Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let (string, message) = match self {
			&Error::ChannelDumpMismatch { ref expected, ref got } =>
				(format!("A channel dump has mismatched, expecting \"{}\",\
					we got \"{}\"", expected, got), &None),
			&Error::IllegalIdentifier { 
				ref got, 
				ref line, 
				ref column, 
				ref message } => (
					format!("Got illegal identifier \"{}\" at {}:{}", 
						got, line, column), 
					message
				),
			&Error::ExpectedIdentifier {
				ref line,
				ref column,
				ref message} => (
					format!("Expected identifier at {}:{}", line, column),
					message
				),
			&Error::ExpectedFloat { 
				ref got, 
				ref line, 
				ref column, 
				ref error } => (
					format!("Expected floating point, got \"{}\" at {}:{}: {}",
						got, line, column, error),
					&None
				),
			&Error::ExpectedInteger { 
				ref got, 
				ref line, 
				ref column, 
				ref error } => (
					format!("Expected integer, got \"{}\" at {}:{}: {}",
						got, line, column, error),
					&None
				),
			&Error::UnexpectedEof { ref message } =>
				(format!("Unexpected end of file"), message),
			&Error::IoError { ref error } =>
				(format!("{}", error), &None)
		};
		write!(f, "{}{}{}", 
			string, 
			if message.is_some() { ": " } else { "" }, 
			message.as_deref().unwrap_or(""))
	}
}

#[cfg(test)]
mod tests {
	#[test]
	fn parse() {
		/* Taken from the official PixTone v1.0.3 package. These are the
		 * contents of the Alarm.pxt file. It is only used here for testing. */
		const SAMPLE: &'static str = r#"
			use  :1
			size :22050
			main_model   :0
			main_freq     :880.00
			main_top     :63
			main_offset  :0
			pitch_model  :4
			pitch_freq    :8.00
			pitch_top    :63
			pitch_offset :0
			volume_model :0
			volume_freq   :0.00
			volume_top   :63
			volume_offset:0
			initialY:63
			ax      :64
			ay      :63
			bx      :128
			by      :63
			cx      :255
			cy      :63

			use  :0
			size :22050
			main_model   :0
			main_freq     :440.00
			main_top     :63
			main_offset  :0
			pitch_model  :0
			pitch_freq    :0.00
			pitch_top    :63
			pitch_offset :0
			volume_model :0
			volume_freq   :0.00
			volume_top   :63
			volume_offset:0
			initialY:63
			ax      :64
			ay      :63
			bx      :128
			by      :63
			cx      :255
			cy      :63

			use  :0
			size :22050
			main_model   :0
			main_freq     :440.00
			main_top     :63
			main_offset  :0
			pitch_model  :0
			pitch_freq    :0.00
			pitch_top    :63
			pitch_offset :0
			volume_model :0
			volume_freq   :0.00
			volume_top   :63
			volume_offset:0
			initialY:63
			ax      :64
			ay      :63
			bx      :128
			by      :63
			cx      :255
			cy      :63

			use  :0
			size :22050
			main_model   :0
			main_freq     :440.00
			main_top     :63
			main_offset  :0
			pitch_model  :0
			pitch_freq    :0.00
			pitch_top    :63
			pitch_offset :0
			volume_model :0
			volume_freq   :0.00
			volume_top   :63
			volume_offset:0
			initialY:63
			ax      :64
			ay      :63
			bx      :128
			by      :63
			cx      :255
			cy      :63

			1,22050,0,880.00,63,0,4,8.00,63,0,0,0.00,63,0,63,64,63,128,63,255,63
			0,22050,0,440.00,63,0,0,0.00,63,0,0,0.00,63,0,63,64,63,128,63,255,63
			0,22050,0,440.00,63,0,0,0.00,63,0,0,0.00,63,0,63,64,63,128,63,255,63
			0,22050,0,440.00,63,0,0,0.00,63,0,0,0.00,63,0,63,64,63,128,63,255,63
		"#;

		use std::io::Cursor;
		let mut cursor = Cursor::new(SAMPLE);

		super::parse_pxt(&mut cursor).unwrap();
	}
}

