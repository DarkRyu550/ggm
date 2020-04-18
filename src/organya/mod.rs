mod file;
mod wave;

use std::collections::VecDeque;
#[derive(Debug)]
struct Track {
	instr: file::Instrument,
	notes: VecDeque<file::Note>,

	panning:  f64,
	volume:   f64,
	rsamples: usize,
	octave:   u8,
	wavepos:  f64,
	wavespd:  f64,
}
impl Track {
	fn new(instr: file::Instrument, mut notes: Vec<file::Note>) -> Self {
		/* Sort the notes by position and put them in a queue. */
		notes.sort_by(|a, b| u32::cmp(&a.position, &b.position));
		
		Track {
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
pub enum Error {
	MissingWavetable,
	InvalidWavetable,
	InvalidTracks,
	IoError(std::io::Error)
}

use std::borrow::Cow;
#[derive(Debug)]
pub struct Player {
	waves: Cow<'static, [u8]>,
	track: Vec<Track>,
	srate: usize,
	sbeat: usize,
	bmsec: f64,
	cbeat: usize,
}

use std::io::Read;
impl Player {
	pub fn new<R: Read>(samplerate: usize, mut org: R, wave: Option<R>) 
		-> Result<Self, Error> {

		let organya = file::read(&mut org).map_err(|what| Error::IoError(what))?;
		let file::Organya {
			file_header: header, 
			instruments: instrs,
			note_events: notes 
		} = organya;

		let track = instrs.iter()
			.zip(notes.iter())
			.map(|(instr, notes)| Track::new(*instr, notes.to_vec()))
			.collect::<Vec<Track>>();
		if track.len() != 16 { return Err(Error::InvalidTracks) }

		let waves = match wave {
			Some(mut stream) => {
				let mut vec = Vec::new();
				stream.read_to_end(&mut vec)
					.map_err(|what| Error::IoError(what))?;
				Cow::from(vec)
			},
			None => wave::WAVETABLE.clone()
				.map(|table| Cow::from(table))
				.ok_or(Error::MissingWavetable)?
		};

		/* Get some diagnostics on the wavetable. */
		{
			use wave::Wavetable;
			let table = Wavetable(&waves);
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
			waves,
			track,
			srate: samplerate,
			sbeat: (samplerate * usize::from(header.wait())) / 1000,
			bmsec: 1.0 / f64::from(header.wait()),
			cbeat: 0,
		})
	}

	pub fn samples_per_beat_channel(&self) -> usize {
		self.sbeat
	}

	pub fn synth(&mut self, buff: &mut [f64]) -> usize {
		use wave::Wavetable;
		let table = Wavetable(&self.waves);

		let srate = self.srate;
		let sbeat = self.sbeat;
		let spsec = self.sbeat as f64 * self.bmsec as f64 * 1000.0;
		let process = |buff: &mut [f64], beat: usize, track: &mut Track| {
			/* Process the track and generate enough samples for one beat. */
			let event = track.notes.front().map(|e| e.position as usize <= beat);
			if let Some(true) = event {
				let event = track.notes.pop_front().unwrap();
				trace!("Got event {:?}", event);
				if event.volume != 0xff {
					/* The volume value ranges from 0 being no sound to 254
					 * being max volume. Here, we map that to the 0.0 to 1.0
					 * range. */
					let v = f64::from(event.volume) / f64::from(u8::MAX - 1);
					track.volume = v;
				}
				if event.panning != 0xff {
					/* The panning value ranges from 0 to 12, with 0 being
					 * full left pan, 6 being center and 12 being full right
					 * pan. Here, we map that to the -1.0 to 1.0 range, with
					 * -1.0 being full left pan, 0.0 being center and 1.0
					 * being full right pan. */
					track.panning = (f64::from(event.panning) - 6.0) / 6.0;
				}
				if event.value != 0xff {
					let (octave, pitch) = wave::decode_note(event.value);
					let pfreq = pitch.freq() + usize::from(track.instr.pitch()) - 1000;
					track.octave  = octave;
					track.wavepos = 0.0;
					track.wavespd = pitch.freq() as f64 / srate as f64;
					trace!("Playing at octave {} with {} points per sample", octave, track.wavespd);

					/* The number of samples we still have left to play is */
					track.rsamples = if track.instr.sustn() > 0 {
						1024 
					} else {
						usize::from(event.length) * sbeat
					}
				}
				/* There is no separate check for event.length because in the
				 * OrgPlay program the length value of a note only has any
				 * effect when coupled with a note value change. */
			}

			for i in 0..usize::min(sbeat, track.rsamples) {
				/*let sample = f64::from({
					let x = track.wavepos;

					/* Apply Lanczos filtering */
					let radius  = 2.0;
					let lanczos = |n: f64| 
						if n == 0.0 { 
							1.0 
						} else if n.abs() > radius {
							0.0
						} else {
							let c  = n * std::f64::consts::PI;
							let cr = c / radius;
							f64::sin(c) * f64::sin(cr) / (c * cr)
						};

					let scale  = if 1.0 / track.wavespd > 1.0 { 1.0 } else { 1.0 / track.wavespd };
					let min    = (-radius / scale + x - 0.5).round();
					let max    = ( radius / scale + x + 0.5).round();

					let table = table.moctave(usize::from(track.instr.instr()), track.octave);
					let (density, sample): (f64, f64) = (min as usize..max as usize)
						.fold((0.0, 0.0), |(density, sample), m| {
							let m = m as f64;
							let factor = lanczos((m as f64 - x + 0.5) * scale);

							let m = if m < 0.0 { 0.0 } else { m };
							let m = *table.mget(m as usize) as i8;
							let m = m as f64;

							(
								density + factor,
								sample + m * factor
							)
						});
					let sample = if density > 0.0 { sample / density } else { sample };

					track.wavepos += track.wavespd;
					track.wavepos %= table.len() as f64;
					sample
				});*/
				let sample = f64::from({
					let wave   = table.moctave(usize::from(track.instr.instr()), track.octave);
					let sample = *wave.mget(track.wavepos as usize);	

					track.wavepos += track.wavespd;
					sample as i8
				});
				let sample = sample / f64::from(i8::MAX);
				let l = sample * if 1.0 - track.panning > 1.0 { 1.0 } else { 1.0 - track.panning };
				let r = sample * if 1.0 + track.panning > 1.0 { 1.0 } else { 1.0 + track.panning };
				
				/*assert!(-1.0 <= l && l <= 1.0);
				assert!(-1.0 <= r && r <= 1.0);*/

				buff[i * 2 + 0] += l * track.volume;
				buff[i * 2 + 1] += r * track.volume;

				if track.rsamples > 0 { track.rsamples -= 1; }
			}
		};
		
		let beats = buff.len() / self.sbeat / 2;
		for beat in 0..beats {
			trace!("Generating beat {}", self.cbeat + beat);
			let slice = &mut buff[beat * self.sbeat * 2..(beat + 1) * self.sbeat * 2];
			
			for sample in slice.iter_mut() {
				*sample = 0.0;
			}
			
			let cbeat   = self.cbeat;
			let track   = &mut self.track[..];
			let tracks  = track.len();
			let samples = track.iter_mut()
				.map(|mut track| {
					let mut slice = vec![0.0; slice.len()];
					process(&mut slice[..], cbeat + beat, track);
					slice
				})
				.collect::<Vec<_>>();

			samples.into_iter()
				.fold(slice, |slice, track| {
					slice.iter_mut()
						.zip(track.into_iter())
						.for_each(|(tgt, src)| *tgt += src / tracks as f64);
					slice	
				});
		}
		self.cbeat += beats;

		beats * self.sbeat * 2
	}
}

