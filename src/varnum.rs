use std;
use std::io::Write;

pub fn bytes(mut value: u32) -> Vec<u8> {
    let mut result = Vec::with_capacity(4);
    loop {
        let mut byte = ((value & 0x7F) << 1) as u8;
        if value > 0x7F {
            byte |= 1;
        }
        result.push(byte);
        value >>= 7;
        if value == 0 {
            return result
        }
    }
 }

pub trait WriteVarNum {
    fn write_varnum(&mut self, num: u32) -> Result<usize, std::io::Error>;
}


impl<T> WriteVarNum for T where T: Write {
    fn write_varnum(&mut self, num: u32) -> Result<usize, std::io::Error> {
        self.write(&bytes(num))
    }
}