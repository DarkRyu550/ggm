mod arm7;

use std::io::{Read, Write, Seek};

trait MemoryMapper: Read + Write + Seek { }
impl<A: Read + Write + Seek> MemoryMapper for A { }


