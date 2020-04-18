/*! This module contains code for a very simple, not necessarily correct or
 * memory-safe implementation of an ARM7 interpreter, for use with GBA sound
 * emulation. As its main purpose is to emulate sound, making use of a dynamic
 * recompiler for this job is not necessarely desirable, and having perfect CPU
 * emulation accuracy is of low priority.
 * 
 * The actual implementaation of this processor follows the ARMv4 Architecture,
 * as does the original ARM7TDMI processor on the GBA.
 */

/** Processor modes available in ARMv4 */
#[repr(u32)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Mode {
	User			= 16, 
	System			= 17, 
	Supervisor		= 18, 
	Abort			= 19, 
	Undefined		= 23, 
	Interrupt		= 27, 
	FastInterrupt	= 31
}
impl Mode {
	/** Number of modes this processor can be in. */
	pub const VARIANT_COUNT: usize = 7;

	fn has_spsr(&self) -> bool {
		match &self {
			&Mode::User | &Mode::System => false,
			_ => true
		}
	}
}

/** ARMv4 Exception Types. */
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Exception {
	Reset,
	IllegalInstruction,
	SoftwareInterrupt,
	PrefetchAbort,
	DataAbort,
	Interrupt,
	FastInterrupt,
}
impl Exception {
	/** Number of distinct types of exceptions. */
	const VARIANT_COUNT: usize = 7;

	/** Priority level of this exception, 
	 * where lower values mean higher priority. */
	fn priority(&self) -> u32 {
		/* The ARMv4 manual specifies that exceptions generated from both
		 * invalid instructions or SWI have the exact same priority level.
		 * For our purposes, we need to have a total ordering, which means
		 * having the same priority implies being the same exception, which
		 * would obviously cause a few problems. 
		 * 
		 * So, for no particular reason, SWI was chosen to have a lower
		 * priority than an invalid instruction. */
		match *self {
			Self::Reset					=> 1,
			Self::DataAbort				=> 2,
			Self::FastInterrupt			=> 3,
			Self::Interrupt				=> 4,
			Self::PrefetchAbort			=> 5,
			Self::IllegalInstruction	=> 6,
			Self::SoftwareInterrupt		=> 7
		}
	}

	/** After an exception, the processor is instructed to load the routine 
	 * address from an exception vector located at an offset from a base value */
	fn vector_offset(&self) -> u32 {
		match *self {
			Self::Reset					=> 0x00,
			Self::IllegalInstruction	=> 0x04,
			Self::SoftwareInterrupt		=> 0x08,
			Self::PrefetchAbort			=> 0x0c,
			Self::DataAbort				=> 0x10,
			Self::Interrupt				=> 0x18,
			Self::FastInterrupt			=> 0x1c
		}
	}
}
/* Ordering for exceptions is based on priority level, however, having an
 * exception be greater than another means that its priority is higher.
 * Which is the opposite of how the numerical priority values behave. */
use std::cmp::{Ord, PartialOrd, Ordering};
impl PartialOrd for Exception {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.priority().cmp(&other.priority()).reverse())
	}
}
impl Ord for Exception {
	fn cmp(&self, other: &Self) -> Ordering {
		self.priority().cmp(&other.priority()).reverse()
	}
}

/** Values for the instruction condition fields. */
#[repr(u32)]
enum Condition {
	/* <Name and code> */			/* <CPSR Flags>			*/
	Equal					= 0,	/* Z  = 1				*/
	NotEqual				= 1,	/* Z  = 0				*/
	UnsignedGreaterOrEqual	= 2,	/* C  = 1				*/	
	UnsignedLess			= 3,	/* C  = 0				*/
	SignedNegative			= 4,	/* N  = 1				*/
	SignedPositiveOrZero	= 5,	/* N  = 0				*/
	SignedOverflow			= 6,	/* V  = 1				*/
	SignedNoOverflow		= 7,	/* V  = 0				*/
	UnsignedGreater			= 8,	/* C  = 1 and Z  = 1	*/
	UnsignedLessOrEqual		= 9,	/* C  = 0 or  Z  = 1	*/
	SignedGreaterOrEqual	= 10,	/* N  = V				*/
	SignedLess				= 11,	/* N != V				*/
	SignedGreater			= 12,	/* Z  = 0 and N  = V	*/
	SignedLessOrEqual		= 13,	/* Z  = 1 or  N != V	*/
	Always					= 14,	/* Always execute.		*/
	Never					= 15	/* Reserved (never ex.)	*/
}
use std::convert::TryFrom;
impl TryFrom<u32> for Condition {
	type Error = u32;
	fn try_from(c: u32) -> Result<Self, Self::Error> {
		match c {
			0x0 => Ok(Self::Equal),
			0x1 => Ok(Self::NotEqual),
			0x2 => Ok(Self::UnsignedGreaterOrEqual),
			0x3 => Ok(Self::UnsignedLess),
			0x4 => Ok(Self::SignedNegative),
			0x5 => Ok(Self::SignedPositiveOrZero),
			0x6 => Ok(Self::SignedOverflow),
			0x7 => Ok(Self::SignedNoOverflow),
			0x8 => Ok(Self::UnsignedGreater),
			0x9 => Ok(Self::UnsignedLessOrEqual),
			0xa => Ok(Self::SignedGreaterOrEqual),
			0xb => Ok(Self::SignedLess),
			0xc => Ok(Self::SignedGreater),
			0xd => Ok(Self::SignedLessOrEqual),
			0xe => Ok(Self::Always),
			0xf => Ok(Self::Never),
			_ => Err(c)
		}
	}
}

macro_rules! bit_flag {
	($g:ident,$s:ident,$m:expr) => {
		fn $g(&self) -> bool { (self.0 & $m) != 0 }
		fn $s(&mut self, v: bool) { if v { self.0 |= $m } else { self.0 &= !$m } }
	}
}
/** CSPR decoding utility. */ 
#[repr(transparent)]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct ProgramStatus(u32);
impl ProgramStatus {
	fn into_inner(self) -> u32 { <Self as Into<u32>>::into(self) }

	fn mode(&self) -> u32 { self.0 & 0x00_00_00_1f }
	bit_flag!(sign,        set_sign,        0x80_00_00_00);
	bit_flag!(zero,        set_zero,        0x40_00_00_00);
	bit_flag!(carry,       set_carry,       0x20_00_00_00);
	bit_flag!(overflow,    set_overflow,    0x10_00_00_00);
	bit_flag!(irq_disable, set_irq_disable, 0x00_00_01_00);
	bit_flag!(fiq_disable, set_fiq_disable, 0x00_00_00_80);
	bit_flag!(thumb,       set_thumb,       0x00_00_00_40);
}
impl Into<u32> for ProgramStatus {
	fn into(self) -> u32 { self.0 }
}

use super::MemoryMapper;
trait Instruction {
	/** How many cycles are required to complete this instruction. */
	fn cycles(&self) -> u64;

	/** Tries to execute this instruction in full, raising an exception in case
	 * of runtime failure, in the same way ARM would raise an exception. */
	fn execute<M: MemoryMapper>(&self, mem: M, p: &mut Processor) 
		-> Result<(), Exception>;
}

/** Data processing opcodes. */
#[repr(u32)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Opcode {
	AND = 0b0000,	/* Rd <=  Rn &  So			*/
	EOR = 0b0001,	/* Rd <=  Rn ^  So			*/
	SUB = 0b0010,	/* Rd <=  Rn -  So			*/
	RSB = 0b0011,	/* Rd <=  So -  Rn			*/
	ADD = 0b0100,	/* Rd <=  Rn +  So			*/
	ADC = 0b0101,	/* Rd <=  Rn +  So +  Carry	*/
	SBC = 0b0110,	/* Rd <=  Rn -  So - ~Carry	*/
	RSC = 0b0111,	/* Rd <=  So -  Rn - ~Carry	*/
	TST = 0b1000,	/* Update flags after AND	*/
	TEQ = 0b1001,	/* Update flags after EOR	*/
	CMP = 0b1010,	/* Update flags after SUB	*/
	CMN = 0b1011,	/* Update flags after ADD	*/
	ORR = 0b1100,	/* Rd <=  Rn |  So			*/
	MOV = 0b1101,	/* Rd <=  So				*/
	BIC = 0b1110,	/* Rd <=  Rn & ~So			*/
	MVN = 0b1111	/* Rd <= ~So				*/
}
impl Opcode {
	/** Is this opcode meant for status flag updates? */
	fn updates_status(&self) -> bool { (*self as u32 >> 2) & 0b11 == 0b10 }
}
impl TryFrom<u32> for Opcode {
	type Error = u32;
	fn try_from(val: u32) -> Result<Self, Self::Error> {
		match val {
			0b0000 => Ok(Self::AND),
			0b0001 => Ok(Self::EOR),
			0b0010 => Ok(Self::SUB),
			0b0011 => Ok(Self::RSB),
			0b0100 => Ok(Self::ADD),
			0b0101 => Ok(Self::ADC),
			0b0110 => Ok(Self::SBC),
			0b0111 => Ok(Self::RSC),
			0b1000 => Ok(Self::TST),
			0b1001 => Ok(Self::TEQ),
			0b1010 => Ok(Self::CMP),
			0b1011 => Ok(Self::CMN),
			0b1100 => Ok(Self::ORR),
			0b1101 => Ok(Self::MOV),
			0b1110 => Ok(Self::BIC),
			0b1111 => Ok(Self::MVN),
			_ => Err(val)
		}
	}
}

/** Decodes instructions in the data processing space. */
#[repr(transparent)]
struct DataProcessing(u32);
impl DataProcessing {
	/** Contextual condition for the execution of this instruction. */
	fn condition(&self) -> Condition {
		let val = (self.0 >> 28) & 0b1111;
		Condition::try_from(val).unwrap()
	}

	/** ALU operation to be performed on the data. */
	fn opcode(&self) -> Opcode {
		let val = (self.0 >> 21) & 0b1111;
		Opcode::try_from(val).unwrap()
	}

	/** Index of the source register. */
	fn source(&self) -> u32 {
		(self.0 >> 16) & 0b1111
	}

	/** Index of the destination register. */
	fn destination(&self) -> u32 {
		(self.0 >> 12) & 0b1111
	}

	/** Should this operation update status flags? */
	fn updates_status(&self) -> bool {
		(self.0 >> 20) & 1 == 1
	}

	/** Is the shift operator an immediate value? */
	fn immediate(&self) -> bool {
		(self.0 >> 25) & 1 == 1
	}

	/** Is the shift operator shifted by a register? */
	fn register_shift(&self) -> bool {
		(self.0 >> 4) & 1 == 1
	}
}
impl Instruction for DataProcessing {
	fn cycles(&self) -> u64 {
		let p = if self.destination() == 15 { 1 } else { 0 };
		let r = if !self.immediate() && self.register_shift() { 1 } else { 0 };
		unimplemented!()
	}
	fn execute<M: MemoryMapper>(&self, mut mem: M, p: &mut Processor)
		-> Result<(), Exception> {

		let source = *p.reg(self.source());
		let (shifto, shift_carry) = if !self.immediate() {
			/* Base value is given by a register, shifted by either by the value
			 * in a register or an immediate value. With the shift operating
			 * being being one of four distinct operations */
			let base  = *p.reg(self.0 & 0b1111);
			let op    = (self.0 >> 5) & 0b11;
			let carry = ProgramStatus(*p.cpsr()).carry();

			/* Make sure we're not executing in the wrong domain. */
			if self.register_shift() {
				assert_eq!((self.0 >> 7) & 1, 0, "Tried to execute extension \
					space instruction 0x{:X} as a data processing instruction",
					self.0);
				*p.reg((self.0 >> 8) & 0b1111)
			} else { (self.0 >> 7) & 0b11111 };

			match op {
				0 => {
					/* LSL - Logical Shift Left */

					/* Here we trim the shift value to the first byte as this
					 * is the documented behaviour in the ARM architecture. */
					let shift = if self.register_shift() { 
						*p.reg((self.0 >> 8) & 0b1111) 
					} else { (self.0 >> 7) & 0b11111 } & 0xff;
					
					(
						base << shift,
						
						if      shift == 0  { carry }
						else if shift  < 32 { (base >> (32 - shift)) & 1 == 1 }
						else if shift == 32 { base & 1 == 1 }
						else                { false }
					)
				},
				1 => {
					/* LSR - Logical Shift Right */
					let shift = if self.register_shift() {
						*p.reg((self.0 >> 8) & 0b1111) 
					} else {
						/* In LSR, the encoding of the immediate shift value is
						 * slightly different from the encoding in LSL: Here,
						 * the value 0 means a shift right by 32. */
						match (self.0 >> 7) & 0b11111 {
							    0 => 32,
							n @ _ => n
						}
					} & 0xff;

					(
						base >> shift,

						if      shift == 0  { carry }
						else if shift  < 32 { (base >> (shift - 1)) & 1 == 1 }
						else if shift == 32 { base & 1 == 1 }
						else                { false }
					)
				},
				2 => {
					/* ASR - Arithmetic Shift Right */
					let shift = if self.register_shift() {
						*p.reg((self.0 >> 8) & 0b1111) 
					} else {
						/* ASR immediates are encoded just like LSR ones. */
						match (self.0 >> 7) & 0b11111 {
							    0 => 32,
							n @ _ => n
						}
					} & 0xff;

					
					(
						/* In Rust, signed right shifts are arithmetic. */
						(base as i32 >> shift as i32) as u32,

						if      shift == 0  { carry }
						else if shift  < 32 { (base >> (shift - 1)) & 1 == 1 }
						else                { (base >> 31) & 1 == 1          }
					)
				},
				3 if !self.register_shift() && (self.0 >> 7) & 0b11111 == 0 => {
					/* RRX - Rotate Right with Extend */
					let c_mask = if carry { 0x80000000 } else { 0 };
					(
						(base >> 1) | c_mask,
						base & 1 == 1
					)
				},
				3 => {
					/* ROR - Rotate Right */
					let shift = if self.register_shift() {
						*p.reg((self.0 >> 8) & 0b1111) 
					} else { (self.0 >> 7) & 0b11111 } & 0xff;

					(
						base.rotate_right(shift),

						if      shift       == 0 { carry }
						else if shift & 0xf == 0 { (base >> 31) & 1 == 1 }
						else { (base >> ((shift & 0xf) - 1)) & 1 == 1 }
					)
				},
				_ => unreachable!()
			}
		} else {
			/* 8-bit immediate value, rotated right by a 4-bit immediate. */
			let imm = (self.0 >> 0) & 0xff;
			let rot = (self.0 >> 8) & 0b1111;
			let imm = imm.rotate_right(rot * 2);
			(
				imm,
				if rot == 0 { 
					ProgramStatus(*p.cpsr()).carry() 
				} else { (imm >> 31) & 1 == 1 }
			)
		};

		let mut flags = ProgramStatus(*p.cpsr());
		let result = match self.opcode() {
		 	Opcode::AND | Opcode::EOR | Opcode::TST | Opcode::TEQ | 
			Opcode::BIC | Opcode::ORR | Opcode::MOV | Opcode::MVN => {
				/* All bitwise operations set the C flag to the shifter carry, 
				 * while the N and Z flags are set according to the result.
				 * All other flags remain unchanged. */
				let result = match self.opcode() {
					Opcode::AND => source & shifto,
					Opcode::EOR => source ^ shifto,
					Opcode::TST => source & shifto,
					Opcode::TEQ => source ^ shifto,
					Opcode::BIC => source &!shifto,
					Opcode::ORR => source | shifto,
					Opcode::MOV => shifto,
					Opcode::MVN => !shifto,
					_ => unreachable!()
				};
				
				flags.set_sign((result as i32) < 0);
				flags.set_zero(result == 0);
				flags.set_carry(shift_carry);

				result
			},
			Opcode::ADD | Opcode::ADC | Opcode::SUB | Opcode::SBC | 
			Opcode::RSB | Opcode::RSC | Opcode::CMP | Opcode::CMN => {
				/* All arithmetic operations set the C flag according to the
				 * unsigned over or undeflow in the result, while they
				 * update the V flag accorging to signed over and undeflows. 
				 *
				 * The N and Z flags are updated in the same way as bitwise
				 * operations. */
				let (c, d) = if flags.carry() { (1, 0) } else { (0, 1) };
				let ((result, c), (_, v)) = match self.opcode() {
					Opcode::ADD | Opcode::CMN => (
						source.overflowing_add(shifto),
						(source as i32).overflowing_add(shifto as i32)
					),
					Opcode::SUB | Opcode::CMP => (
						source.overflowing_sub(shifto),
						(source as i32).overflowing_sub(shifto as i32)
					),
					Opcode::RSB => (
						shifto.overflowing_sub(source),
						(shifto as i32).overflowing_sub(source as i32)
					),
					Opcode::ADC => 
						({
							let (res, a) = source.overflowing_add(shifto);
							let (res, b) = res.overflowing_add(c);
							(res, a || b)
						},
						{
							let (res, a) = (source as i32).overflowing_add(shifto as i32);
							let (res, b) = res.overflowing_add(c as i32);
							(res, a || b)
						}),
					Opcode::SBC => 
						({
							let (res, a) = source.overflowing_sub(shifto);
							let (res, b) = res.overflowing_sub(d);
							(res, a || b)
						},
						{
							let (res, a) = (source as i32).overflowing_sub(shifto as i32);
							let (res, b) = res.overflowing_sub(d as i32);
							(res, a || b)
						}),
					Opcode::RSC =>
						({
							let (res, a) = shifto.overflowing_sub(source);
							let (res, b) = res.overflowing_add(d);
							(res, a || b)
						},
						{
							let (res, a) = (shifto as i32).overflowing_add(source as i32);
							let (res, b) = res.overflowing_add(d as i32);
							(res, a || b)
						}),
					_ => unreachable!()
				};

				flags.set_sign((result as i32) < 0);
				flags.set_zero(result == 0);
				flags.set_carry(c);
				flags.set_overflow(v);

				result
			}
		};
		
		if self.opcode().updates_status() || self.updates_status() {
			if self.destination() != 15 {
				*p.cpsr_mut() = flags.into_inner();
			} else {
				/* When the destination register is R15, this causes a copy
				 * of the SPSR to the CSPR, instead of updating is like we
				 * normally would. In the case the current mode has no SPSR,
				 * this instruction is per the spec unpredictable. */
				if !p.mode.has_spsr() {
					warn!("Unpredictable instruction 0x{:x} in mode {:?}",
						self.0, p.mode);
				}
				*p.cpsr_mut() = *p.spsr();
			}
		}

		if !self.opcode().updates_status() {
			*p.reg_mut(self.destination()) = result;
		}
		
		Ok(())
	}
}


/** Decodes instructions in the multiply extension space. */
#[repr(transparent)]
struct MultiplyExtension(u32);
impl MultiplyExtension {
	/** Contextual condition for the execution of this instruction. */
	fn condition(&self) -> Condition {
		let val = (self.0 >> 28) & 0b1111;
		Condition::try_from(val).unwrap()
	}

	/** Should unsigned multiplication be done? */
	fn unsigned(&self) -> bool {
		(self.0 >> 22) & 1 == 1
	}

	fn accumulate(&self) -> bool {
		(self.0 >> 21) & 1 == 1
	}

	/** Should this operation update status flags? */
	fn updates_status(&self) -> bool {
		(self.0 >> 20) & 1 == 1
	}

	/** Whether one of the operands is a long value. */
	fn long(&self) -> bool {
		(self.0 >> 23) & 1 == 1
	}
}

/** Decodes instructions in the control extension space that deal specifically
 * with moving data in and out of the PSR using immediates or other registers. */
#[repr(transparent)]
struct StatusRegisterMove(u32);
impl StatusRegisterMove {
	fn condition(&self) -> Condition {
		let val = (self.0 >> 28) & 0b1111;
		Condition::try_from(val).unwrap()
	}

	/** This bit is enabled when the value of the register should be read. */
	fn read(&self) -> bool {
		(self.0 >> 21) & 1 == 1	
	}

	fn immediate(&self) -> bool {
		(self.0 >> 25) & 1 == 1
	}
}



struct LoadAndStoreSingle(u32);
struct LoadAndStoreMultiple(u32);


enum InstructionSet {
	DataProcessing(DataProcessing),
	Multiply(MultiplyExtension)
}
impl InstructionSet {
	fn try_decode(binary: u32) -> Result<Self, u32> {
		/* This here is a pretty brute-force way to handle ARM instruction
		 * decoding. Effectively, this behaves like a static prefix tree. 
		 *
		 * Instead of going for the most efficient route, which would be to
		 * properly uderstand the reasons for these weird fixed bit patterns,
		 * we're going the "modern computers are fast enough for this" route,
		 * and brute forcing the decoding using prefix parsing. 
		 *
		 * Hopefully LLVM can optimize this further. And, for what we're doing,
		 * This ought to be good enough. */
		let nibble = (binary >> 26) & 0b11;
		match nibble {
			0b00 => {
				/* Check for multiply extension. 
				 * [27:24] = 0, [7:4] = 0b1001 */
				if (binary >> 24 & 0b1111) == 0 
					&& (binary >> 4 & 0b1111) == 0b1001 {

					return Ok(Self::Multiply(MultiplyExtension(binary)))
				}
				/* Check for control extension.
				 * [] */
				unimplemented!()
			},
			0b11 => /* Requires further processing	*/ { 
				unimplemented!()
			},
			0b10 => /* Block data transfer			*/ { 
				unimplemented!()
			},
			0b01 => /* Branch						*/ { 
				unimplemented!()
			},
			_ => unreachable!()
		}
	}
}

enum Thumb {

}

/** Represents all possible enbankments of a register, and allows for them to
 * be indexed in an efficient way. */
pub struct BankedRegister {
	bank: [u32; Mode::VARIANT_COUNT],
	mask: [usize; Mode::VARIANT_COUNT]
}
impl BankedRegister {
	fn at(&self, mode: Mode) -> &u32 {
		&self.bank[self.mask[usize::try_from(mode as u32).unwrap()]]
	}

	fn at_mut(&mut self, mode: Mode) -> &mut u32 {
		&mut self.bank[self.mask[usize::try_from(mode as u32).unwrap()]]
	}
}

/** Holds all processor-wide state and provides utilities for interpreting code
 * in a more convenient manner. */
use std::collections::BinaryHeap;
pub struct Processor {
	general:	[BankedRegister; GENERAL_REGISTER_COUNT],
	status:		[BankedRegister; STATUS_REGISTER_COUNT],
	exc_queue:	BinaryHeap<Exception>,
	mode:		Mode
}
impl Processor {
	fn new() -> Self {
		Processor {
			general:	GENERAL_REGISTERS,
			status:		STATUS_REGISTERS,
			exc_queue:	BinaryHeap::with_capacity(Exception::VARIANT_COUNT),
			mode:		Mode::Supervisor
		}
	}

	/** Raise a processor-level exception to be executed in the next cycle. */
	fn raise(&mut self, e: Exception) {
		self.exc_queue.push(e);
	}

	/** Acquires a reference to a register in the current mode. */
	fn reg(&self, i: u32) -> &u32 {
		self.general[usize::try_from(i).unwrap()].at(self.mode)
	}

	/** Acquires a mutable reference to a register in the current mode. */
	fn reg_mut(&mut self, i: u32) -> &mut u32 {
		self.general[usize::try_from(i).unwrap()].at_mut(self.mode)
	}

	/** Current Program Status Register */
	fn cpsr(&self) -> &u32 {
		self.status[0].at(self.mode)
	}

	/** Current Program Status Register */
	fn cpsr_mut(&mut self) -> &mut u32 {
		self.status[0].at_mut(self.mode)
	}

	/* Saved Program Status Register */
	fn spsr(&self) -> &u32 {
		self.status[1].at(self.mode)
	}

	fn run<M: MemoryMapper>(&mut self, cycles: u64, mem: M) {
		let address = *self.reg(15);
	}
}


/* Easily represent a banked register in function of its mask. */
macro_rules! reg {
	($t:expr) => { BankedRegister{ bank: [0; Mode::VARIANT_COUNT], mask: $t } }	
}

/** Number of general purpose registers. */
const GENERAL_REGISTER_COUNT: usize = 16;

/** Layout of our general purpose registers, represented as an array. */
const GENERAL_REGISTERS: [BankedRegister; GENERAL_REGISTER_COUNT] = [
	reg!([0, 0, 0, 0, 0, 0, 0]), /* R0,  unbanked */
	reg!([0, 0, 0, 0, 0, 0, 0]), /* R1,  unbanked */
	reg!([0, 0, 0, 0, 0, 0, 0]), /* R2,  unbanked */
	reg!([0, 0, 0, 0, 0, 0, 0]), /* R3,  unbanked */
	reg!([0, 0, 0, 0, 0, 0, 0]), /* R4,  unbanked */
	reg!([0, 0, 0, 0, 0, 0, 0]), /* R5,  unbanked */
	reg!([0, 0, 0, 0, 0, 0, 0]), /* R6,  unbanked */
	reg!([0, 0, 0, 0, 0, 0, 0]), /* R7,  unbanked */
	reg!([0, 0, 0, 0, 0, 0, 6]), /* R8,  banked for FIQ */
	reg!([0, 0, 0, 0, 0, 0, 6]), /* R9,  banked for FIQ */
	reg!([0, 0, 0, 0, 0, 0, 6]), /* R10, banked for FIQ */
	reg!([0, 0, 0, 0, 0, 0, 6]), /* R11, banked for FIQ */
	reg!([0, 0, 0, 0, 0, 0, 6]), /* R12, banked for FIQ */
	reg!([0, 0, 2, 3, 4, 5, 6]), /* R13, banked for all except System */
	reg!([0, 0, 2, 3, 4, 5, 6]), /* R14, banked for all except System */
	reg!([0, 0, 0, 0, 0, 0, 0]), /* R15, unbanked */
];

/** R14 is considered by PUSH and POP to be the stack pointer. */
const STACK_POINTER: usize = 14;

/** R15 is synonymous with the program counter. */
const PROGRAM_COUNTER: usize = 15;

/** Number of status registers. */
const STATUS_REGISTER_COUNT: usize = 2;

/** Layout of our status registers, represented as an array. */
const STATUS_REGISTERS: [BankedRegister; STATUS_REGISTER_COUNT] = [
	reg!([0, 0, 0, 0, 0, 0, 0]), /* CPSR, unbanked */
	reg!([0, 0, 2, 3, 4, 5, 6]), /* SPSR, unavailable at User and System. */
];

