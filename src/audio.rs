
pub trait Format {
	/** Length in bits of each sample. */
	fn bit_length(&self) -> usize;

	/** Upper bound for signal value, all values above this are truncated */
	fn upper_bound(&self) -> Self;

	/** Lower bound for signal value, all values below this are truncated */
	fn lower_bound(&self) -> Self;
}

pub trait Resample<T: Format>: Format {
	/** Resample the current format into the target format. */
	fn resample(self) -> T;
}

pub trait TryResample<T: Format>: Format {
	type Error;

	/** Try failibly resampling the current format into the tiget format. */
	fn try_resample(self) -> Result<T, Self::Error>;
}

/* Generic implementations. */
macro_rules! impl_int_fmt {
	($t:ty) => { 
		impl Format for $t {
			fn bit_length(&self)  -> usize { std::mem::size_of::<$t>() * 8 }
			fn upper_bound(&self) -> Self  { <$t>::max_value() }
			fn lower_bound(&self) -> Self  { <$t>::min_value() }
		}
	} 
}
macro_rules! impl_float_fmt {
	($t:ty) => {
		impl Format for $t {
			fn bit_length(&self)  -> usize { std::mem::size_of::<$t>() * 8 }
			fn upper_bound(&self) -> Self  {  1.0 }
			fn lower_bound(&self) -> Self  { -1.0 }
		}
	}
}
impl_int_fmt!(i8);   impl_int_fmt!(u8);
impl_int_fmt!(i16);  impl_int_fmt!(u16);
impl_int_fmt!(i32);  impl_int_fmt!(u32);
impl_int_fmt!(i64);  impl_int_fmt!(u64);
impl_int_fmt!(i128); impl_int_fmt!(u128);
impl_float_fmt!(f32); impl_float_fmt!(f64);

macro_rules! impl_iirs {
	($src:ty, $tgt:ty) => {
		impl Resample<$tgt> for $src {
			fn resample(self) -> $tgt {
					
			}
		}
	}
}
macro_rules! impl_iirs_rc {
	($src:ty, $tgt:ty) => {
		impl_iirs!($src, $tgt);
		impl_iirs!($tgt, $src);
	}
}

