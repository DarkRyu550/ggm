
/** How many samples are remaining in a given song. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[cfg_attr(feature = "use-serde", derive(serde::Serialize, serde::Deserialize))]
pub enum RemainingLength {
	/** The number of remaining samples is finite and defined. */
	Finite(usize),
	/** This song has been set to loop or generate forever. */
	Infinite,
}
impl PartialEq<usize> for RemainingLength {
	fn eq(&self, other: &usize) -> bool {
		match self {
			&Self::Finite(this) => this.eq(other),
			&Self::Infinite     => false
		}
	}
}
impl PartialOrd<usize> for RemainingLength {
	fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
		match self {
			&Self::Finite(this) => Some(this.cmp(other)),
			&Self::Infinite     => Some(std::cmp::Ordering::Greater)
		}
	}
}

/** This trait specifies a generic interface for song providers in this
 * library. Such that every function that deals with the product of the song
 * generation rather than the process can be generic across all providers.
 * Every synthesizee and weaver should implement this trait. */
pub trait Song {
	type Sample;
	type Error;

	/** The number of samples that are produced by one computation. Ideally,
	 * buffers that store the data in this song should be a multiple of this
	 * value, as to not waste space in the buffer. */ 
	fn blocksize(&self) -> Option<usize>;

	/** How many samples are still left to be played, including loops. If the
	 * playback is configured as to loop endlessly, this has no value. */
	fn remaining(&self) -> RemainingLength;

	/** How many playback channels this song has.
	 * Note that samples from different channels are laid out in succession,
	 * so, if you have a sample `Sn` where `n` is the channel number, the data
	 * will look like `S0 S1 S2 S3 ... Sn` for a given number of channels. */
	fn channels(&self) -> usize;

	/** How many samples should be played in a second. This is simply the sample
	 * rate for this song. */
	fn samplerate(&self) -> usize;

	/** Generate some samples and save them into the given buffer. The number of
	 * samples that actually get generate may be smaller than the total size of
	 * the buffer, but it is guaranteed to be between zero and the length of the
	 * buffer, as to assure that no generated sample is ever lost. 
	 * 
	 * If this function ever returns `Ok(0)`, it can either mean that there are
	 * no more samples to be generated or that the buffer is not long enough to
	 * store them. To distinguish between these cases, you can check the number
	 * of remaining samples. */
	fn generate(&mut self, buf: &mut [Self::Sample]) -> Result<usize, Self::Error>;
}

