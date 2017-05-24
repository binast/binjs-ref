use std::io::{ Read, Result, Seek, SeekFrom };

pub struct PositionRead<R> where R: Read {
    reader: R,
    position: u64,
}

impl<R> PositionRead<R> where R: Read {
    pub fn new(reader: R) -> Self {
        PositionRead {
            reader,
            position: 0
        }
    }

    pub fn position(&self) -> u64 {
        self.position
    }
}

impl<R> Read for PositionRead<R> where R: Read {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let bytes = self.read(buf)?;
        self.position += bytes as u64;
        Ok(bytes)
    }
}

impl<R> Seek for PositionRead<R> where R: Read + Seek {
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        self.position = self.reader.seek(pos)?;
        Ok(self.position)
    }
}