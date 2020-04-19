mod file;
mod wave;
mod pixtone;

use std::collections::VecDeque;
#[derive(Debug)]
struct Melodic {
	instr: file::Instrument,
	notes: VecDeque<file::Note>,

	panning:  f64,
	volume:   f64,
	rsamples: usize,
	octave:   u8,
	wavepos:  f64,
	wavespd:  f64,
}
impl Melodic {
	fn new(instr: file::Instrument, mut notes: Vec<file::Note>) -> Self {
		/* Sort the notes by position and put them in a queue. */
		notes.sort_by(|a, b| u32::cmp(&a.position, &b.position));
		
		Self {
			instr,
			notes:    From::from(notes),
			panning:  0.0,
			volume:   1.0,
			rsamples: 0,
			octave:   0,
			wavepos:  0.0,
			wavespd:  0.0,
		}
	}
}

#[derive(Debug)]
struct Percussion {
	instr: file::Instrument,
	notes: VecDeque<file::Note>,

	panning:  f64,
	volume:   f64,
	rsamples: usize,
	wavepos:  f64,
	wavespd:  f64
}
impl Percussion {
	fn new(instr: file::Instrument, mut notes: Vec<file::Note>) -> Self {
		/* Sort the notes by position and put them in a queue. */
		notes.sort_by(|a, b| u32::cmp(&a.position, &b.position));
		
		Self {
			instr,
			notes:    notes.into(),
			panning:  0.0,
			volume:   1.0,
			rsamples: 0,
			wavepos:  0.0,
			wavespd:  0.0,
		}
	}
}

#[derive(Debug)]
enum Track<'a> {
	Melodic(&'a mut Melodic),
	Percussion(&'a mut Percussion)
}
impl<'a> Track<'a> {
	fn volume(&mut self) -> &mut f64 {
		match self {
			&mut Self::Melodic(ref mut a)    => &mut a.volume,
			&mut Self::Percussion(ref mut a) => &mut a.volume,
		}
	}
	fn panning(&mut self) -> &mut f64 {
		match self {
			&mut Self::Melodic(ref mut a)    => &mut a.panning,
			&mut Self::Percussion(ref mut a) => &mut a.panning,
		}
	}
	fn instr(&self) -> &file::Instrument {
		match self {
			&Self::Melodic(ref a)    => &a.instr,
			&Self::Percussion(ref a) => &a.instr,
		}
	}
}
impl<'a> From<&'a mut Melodic> for Track<'a> {
	fn from(mel: &'a mut Melodic) -> Self {
		Self::Melodic(mel)
	}
}
impl<'a> From<&'a mut Percussion> for Track<'a> {
	fn from(mel: &'a mut Percussion) -> Self {
		Self::Percussion(mel)
	}
}

#[derive(Debug)]
pub enum Error {
	MissingWavetable,
	InvalidWavetable,
	InvalidTracks,
	MissingPixtone,
	InvalidPixtone(usize, pixtone::Error),
	IoError(std::io::Error)
}

const DRUM_SAMPLES: usize = 12;

use std::collections::HashMap;
struct DrumMapper<'a> {
	drums: &'a HashMap<usize, Vec<i8>>,
	map: [usize; DRUM_SAMPLES]
}
impl<'a> DrumMapper<'a> {
	fn new(drums: &'a HashMap<usize, Vec<i8>>) -> Self {
		/* This is the default mapping for the drum instruments in Cave Story. */
		let map = [0x96, 0x00, 0x97, 0x00, 0x9a, 0x98, 0x99, 0x00, 0x9b, 0, 0, 0];
		DrumMapper { map, drums }
	}
	/** How many drum samples are mapped? */
	fn len(&self) -> usize { self.map.len() }
	/** Get the drum sample mapped to the given index. */
	fn get(&self, index: usize) -> Option<&[i8]> {
		self.map.get(index)
			.and_then(|id| self.drums.get(id))
			.map(|vec| &vec[..])
	}
	/** Get the drum sample mapped to the given index mod length. */
	fn mget(&self, index: usize) -> Option<&[i8]> {
		self.drums.get(&self.map[index % self.len()])
			.map(|vec| &vec[..])
	}
}

use std::borrow::Cow;

#[derive(Debug)]
pub struct Player {
	wavetbl: Cow<'static, [u8]>,
	drums:   HashMap<usize, Vec<i8>>,
	melodic: Vec<Melodic>,
	percuss: Vec<Percussion>,
	srate:   usize,
	sbeat:   usize,
	cbeat:   usize,
}

use std::io::Read;
impl Player {
	pub fn new_with_builtins<R: Read>(samplerate: usize, org: R)
		-> Result<Self, Error>{

		let wavetable = wave::WAVETABLE.as_ref()
			.map(|table| Cow::from(*table))
			.ok_or(Error::MissingWavetable)?;

		let drums = match pixtone::EFFECTS.as_ref() {
			Some(drums) => drums.iter()
				.map(|(index, res)| (*index, pixtone::Synth::new(*res)))
				.collect::<Vec<_>>(),
			None => Vec::new()
		};

		Self::_new(samplerate, org, wavetable, drums)
	}
	pub fn new<R: Read, S: Read, T: Read, A: AsMut<[(usize, T)]>>(
		samplerate: usize,
		org: R,
		mut wavetable: S,
		mut drums: A) -> Result<Self, Error> {

		let wavetable = {
			let mut vec = Vec::new();
			wavetable.read_to_end(&mut vec)
				.map_err(|what| Error::IoError(what))?;
			Cow::from(vec)
		};

		let drums = drums.as_mut();
		let drums = drums.iter_mut()
			.map(|(index, res)| (*index, pixtone::Synth::new(res)))
			.collect::<Vec<_>>();

		Self::_new(samplerate, org, wavetable, drums)
	}
	pub fn new_with_wavetable<R: Read, S: Read>(
		samplerate: usize,
		org: R,
		mut wavetable: S) -> Result<Self, Error> {

		let wavetable = {
			let mut vec = Vec::new();
			wavetable.read_to_end(&mut vec)
				.map_err(|what| Error::IoError(what))?;
			Cow::from(vec)
		};

		let drums = match pixtone::EFFECTS.as_ref() {
			Some(drums) => drums,
			None => return Err(Error::MissingPixtone)
		};
		let drums = drums.iter()
			.map(|(index, res)| (*index, pixtone::Synth::new(*res)))
			.collect::<Vec<_>>();

		Self::_new(samplerate, org, wavetable, drums)

	}
	pub fn new_with_drums<R: Read, T: Read, A: AsMut<[(usize, T)]>>(
		samplerate: usize,
		org: R,
		mut drums: A) -> Result<Self, Error> {

		let wavetable = wave::WAVETABLE.as_ref()
			.map(|table| Cow::from(*table))
			.ok_or(Error::MissingWavetable)?;

		let drums = drums.as_mut();
		let drums = drums.iter_mut()
			.map(|(index, res)| (*index, pixtone::Synth::new(res)))
			.collect::<Vec<_>>();

		Self::_new(samplerate, org, wavetable, drums)
	}
	
	fn _new<R: Read>(
		samplerate: usize, 
		mut org: R,
		wavetbl: Cow<'static, [u8]>,
		drums: Vec<(usize, Result<pixtone::Synth, pixtone::Error>)>
		) -> Result<Self, Error> {

		let organya = file::read(&mut org).map_err(|what| Error::IoError(what))?;
		let file::Organya {
			file_header: header, 
			instruments: instrs,
			note_events: notes 
		} = organya;

		let melodic = instrs[..8].iter()
			.zip(notes.iter())
			.map(|(instr, notes)| Melodic::new(*instr, notes.to_vec()))
			.collect::<Vec<_>>();
		let percuss = instrs[8..].iter()
			.zip(notes.iter())
			.map(|(instr, notes)| Percussion::new(*instr, notes.to_vec()))
			.collect::<Vec<_>>();
		if melodic.len() + percuss.len() != 16 { 
			return Err(Error::InvalidTracks) 
		}

		let drums = drums.into_iter()
			.map(|(index, result)| match result {
				Ok(synth) => Ok((index, synth)),
				Err(what) => Err((index, what))
			})
			.collect::<Result<Vec<_>, _>>()
			.map_err(|(index, what)| Error::InvalidPixtone(index, what))?
			.into_iter()
			.map(|(index, mut synth)| {
				let mut samples = vec![0; synth.remaining()];
				let generated   = synth.generate(&mut samples[..]);
				samples.resize(generated, 0);

				trace!("PixTone 0x{:02x} has generated {} samples", 
					index, generated);
				(index, samples)
			})
			.collect::<HashMap<_, _>>();


		/* Get some diagnostics on the wavetable. */
		{
			use wave::Wavetable;
			let table = Wavetable(&wavetbl);
			if !table.whole() {
				warn!("The given wave table isn't whole.");
			}
			if table.len() < 100 {
				return Err(Error::InvalidWavetable)
			}
			if table.len() > 100 {
				debug!("The given wave table has {} entries. Expected 100.",
					table.len());
			}
		}

		Ok(Self {
			wavetbl,
			drums,
			melodic,
			percuss,
			srate: samplerate,
			sbeat: (samplerate * usize::from(header.wait())) / 1000,
			cbeat: 0,
		})
	}

	pub fn samples_per_beat_channel(&self) -> usize {
		self.sbeat
	}

	pub fn synth(&mut self, buff: &mut [f64]) -> usize {
		use wave::Wavetable;
		let table = Wavetable(&self.wavetbl);
		let drums = DrumMapper::new(&self.drums);

		let srate = self.srate;
		let sbeat = self.sbeat;	
		/* Process a track event. */
		let process = |event: &file::Note, mut track: Track| {
			if event.volume != 0xff {
				/* The volume value ranges from 0 being no sound to 254
				 * being max volume. Here, we map that to the 0.0 to 1.0
				 * range. */
				let v = f64::from(event.volume) / f64::from(u8::MAX - 1);
				*track.volume() = v;
			}
			if event.panning != 0xff {
				/* The panning value ranges from 0 to 12, with 0 being
				 * full left pan, 6 being center and 12 being full right
				 * pan. Here, we map that to the -1.0 to 1.0 range, with
				 * -1.0 being full left pan, 0.0 being center and 1.0
				 * being full right pan. */
				let p = (f64::from(event.panning) - 6.0) / 6.0;
				*track.panning() = p;
			}
			match (track, event.value) {
				(Track::Melodic(track), v @ _) if v != 0xff => {
					/* In order to figure out how fast to play a given melodic
					 * note, we first need to understand how melodic instruments
					 * in Organya work. For any given instrument wave, there are
					 * eight intermediary waves, each with its own length and
					 * sample disposition. Then, these intermediary samples are
					 * played at a sample rate fixed to the note within the
					 * octave, producing the correct sound. 
					 *
					 * For more details on how exactly these intermediary waves
					 * work, see `wave.rs` */
					let (octave, pitch) = wave::decode_note(event.value);
					let pfreq = pitch.freq() + usize::from(track.instr.pitch()) - 1000;
					track.octave  = octave;
					track.wavepos = 0.0;
					track.wavespd = pfreq as f64 / srate as f64;
					trace!("Playing melodical at octave {} with {} points per \
						sample", octave, track.wavespd);

					/* The number of samples we still have left to play is... */
					track.rsamples = if track.instr.sustn() > 0 {
						1024
					} else {
						usize::from(event.length) * sbeat
					}
				},
				(Track::Percussion(track), v @ _) if v != 0xff => {
					/* Percussion instruments are rather simpler than melodic
					 * ones when it comes to figuring out wave paramenters.
					 * Here, we scale the speed linearly, and the note we
					 * have to play is also linearly scaled from the root note
					 * that lands somewhere between G#2 and A2, since that is
					 * where the note frequency (~220.50Hz) aligns with the
					 * target of 22050Hz of the PixTone synthesizer. */
					let pfreq = f64::from(event.value) / 32.5 * 22050.0;
					track.wavepos  = 0.0;
					track.wavespd  = pfreq / srate as f64;
					trace!("Playing percussion at freq {} with {} points per \
						sample", pfreq, track.wavespd);

					let instrument = usize::from(track.instr.instr());
					track.rsamples = match drums.mget(instrument) {
						Some(wave) => (wave.len() as f64 / track.wavespd) as usize,
						None => {
							/* This wave sample does not exist. */
							debug!("Percussion track requested empty drum {}", instrument);
							0
						}
					};
				},
				(_, _) => { }
			}
			/* There is no separate check for event.length because in the
			 * OrgPlay program the length value of a note only has any
			 * effect when coupled with a note value change. */
		};
		/* Get a sample from a melodic instrument. */
		let melodic_sample = |track: &mut Melodic| {
			if track.rsamples == 0 { return 0.0 }

			let sample = f64::from({
				/* Load the correct intermediary wave for the current octave and
				 * play it back using the point frequency saved in wavespd. */
				let wave = table.moctave(
					usize::from(track.instr.instr()), 
					track.octave
				);
				let sample = *wave.mget(track.wavepos.round() as usize);

				track.wavepos += track.wavespd;
				sample as i8
			});
			
			track.rsamples -= 1;
			sample
		};
		/* Get a sample from a percussion instrument. */
		let percuss_sample = |track: &mut Percussion| {
			if track.rsamples == 0 { return 0.0 }

			let sample = f64::from({
				let instr  = usize::from(track.instr.instr());
				let sample = drums.mget(instr);
				let sample = sample.map(|drum| {
					let sample = drum[track.wavepos.round() as usize % drum.len()];
					sample
				}).unwrap_or(0);

				track.wavepos += track.wavespd;
				sample as i8
			});

			track.rsamples -= 1;
			sample
		};
		/* Generate one beat for the given melodic instrument */
		let melodic_gen = |buff: &mut [f64], beat: usize, track: &mut Melodic| {
			let event = track.notes.front().map(|e| e.position as usize <= beat);
			if let Some(true) = event { 
				let event = track.notes.pop_front().unwrap();
				process(&event, track.into());
			}

			for i in 0..sbeat {
				let sample = melodic_sample(track);
				let sample = sample / f64::from(i8::MAX);
				let l = sample * if 1.0 - track.panning > 1.0 { 1.0 } else { 1.0 - track.panning };
				let r = sample * if 1.0 + track.panning > 1.0 { 1.0 } else { 1.0 + track.panning };

				assert!(-1.0 <= l && l <= 1.0);
				assert!(-1.0 <= r && r <= 1.0);

				buff[i * 2 + 0] += l * track.volume;
				buff[i * 2 + 1] += r * track.volume;
			}
		};
		/* Generate one beat for the given percussion instrument */
		let percuss_gen = |buff: &mut [f64], beat: usize, track: &mut Percussion| {
			let event = track.notes.front().map(|e| e.position as usize <= beat);
			if let Some(true) = event { 
				let event = track.notes.pop_front().unwrap();
				process(&event, track.into());
			}

			for i in 0..sbeat {
				let sample = percuss_sample(track);
				let sample = sample / f64::from(i8::MAX);
				let l = sample * if 1.0 - track.panning > 1.0 { 1.0 } else { 1.0 - track.panning };
				let r = sample * if 1.0 + track.panning > 1.0 { 1.0 } else { 1.0 + track.panning };

				assert!(-1.0 <= l && l <= 1.0);
				assert!(-1.0 <= r && r <= 1.0);

				buff[i * 2 + 0] += l * track.volume;
				buff[i * 2 + 1] += r * track.volume;
			}
		};

		let beats = buff.len() / self.sbeat / 2;
		for beat in 0..beats {
			trace!("Generating beat {}", self.cbeat + beat);

			/* Get a slice comprising of only the current beat and clear it. */ 
			let slice = &mut buff[
				beat * self.sbeat * 2
				..
				(beat + 1) * self.sbeat * 2
			];
			for sample in slice.iter_mut() { *sample = 0.0; }
		
			/* Fill the slice with our generated sound. */
			let cbeat = self.cbeat;
			let melodic = &mut self.melodic[0..0];
			let percuss = &mut self.percuss[3..4];
			for i in melodic.iter_mut() { melodic_gen(slice, cbeat + beat, i); } 
			for i in percuss.iter_mut() { percuss_gen(slice, cbeat + beat, i); } 

			/* Normalize. */
			for sample in slice.iter_mut() { 
				*sample /= (melodic.len() + percuss.len()) as f64
			}
		}
		self.cbeat += beats;

		beats * self.sbeat * 2
	}
}

