//! Compressing bytes from/to bytes.

use bytes::serialize::*;
use bytes::varnum::*;

use rand::distributions::Distribution;
use rand::distributions::Standard;
use rand::seq::SliceRandom;
use rand::thread_rng;
use rand::Rng;

use std;
use std::collections::HashSet;
use std::io::{Cursor, Read, Write};

const BROTLI_BUFFER_SIZE: usize = 4096;
const BROTLI_QUALITY: u32 = 11;
const BROTLI_LG_WINDOW_SIZE: u32 = 20;
const LZW_MIN_CODE_SIZE: u8 = 8;

/// The compression mechanisms supported by this encoder.
/// They are designed to match HTTP's Accept-Encoding:
/// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Encoding
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Compression {
    /// no compression (`identity;`)
    Identity,
    /// brotly compression (`br;`)
    #[cfg(feature = "brotli")]
    Brotli,
}

impl Distribution<Compression> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Compression {
        use self::Compression::*;
        let choices = [
            Identity,
            #[cfg(feature = "brotli")]
            Brotli,
        ];
        choices.choose(rng).unwrap().clone()
    }
}

#[derive(Clone, Debug)]
pub struct CompressionResult {
    pub before_bytes: usize,
    pub after_bytes: usize,
    pub algorithms: HashSet<Compression>,
}

impl Compression {
    pub fn name(&self) -> &str {
        use self::Compression::*;
        match *self {
            Identity => "Identity",
            #[cfg(feature = "brotli")]
            Brotli => "Brotli",
        }
    }

    pub fn code(&self) -> &str {
        use self::Compression::*;
        match *self {
            Identity => "identity",
            #[cfg(feature = "brotli")]
            Brotli => "br",
        }
    }

    pub fn parse(name: Option<&str>) -> Option<Compression> {
        let result = match name {
            None | Some("identity") => Compression::Identity,
            #[cfg(feature = "brotli")]
            Some("br") => Compression::Brotli,
            Some("random") => thread_rng().gen(),
            Some(_) => {
                return None;
            }
        };
        Some(result)
    }

    pub fn values() -> Box<[Self]> {
        use self::Compression::*;
        Box::new([
            Identity,
            #[cfg(feature = "brotli")]
            Brotli,
        ])
    }

    pub fn is_compressed(&self) -> bool {
        if let Compression::Identity = *self {
            true
        } else {
            false
        }
    }

    // Format:
    // - compression type (string);
    // - compressed byte length (varnum);
    // - data.
    pub fn compress<W: Write>(
        &self,
        data: &[u8],
        out: &mut W,
    ) -> Result<CompressionResult, std::io::Error> {
        let before_bytes = data.len();
        let after_bytes = match *self {
            Compression::Identity => {
                out.write_all(b"identity;")?;
                out.write_varnum(data.len() as u32)?;
                out.write_all(data)?;
                data.len()
            }
            #[cfg(feature = "brotli")]
            Compression::Brotli => {
                out.write_all(b"br;")?;
                // Compress
                let mut buffer = Vec::with_capacity(data.len());
                {
                    let mut encoder = brotli::CompressorWriter::new(
                        &mut buffer,
                        BROTLI_BUFFER_SIZE,
                        BROTLI_QUALITY,
                        BROTLI_LG_WINDOW_SIZE,
                    );
                    encoder.write(data)?;
                }
                // Write
                out.write_varnum(buffer.len() as u32)?;
                out.write_all(&buffer)?;
                buffer.len()
            }
        };
        Ok(CompressionResult {
            before_bytes,
            after_bytes,
            algorithms: [self.clone()].iter().cloned().collect(),
        })
    }

    pub fn decompress<R: Read, T>(
        inp: &mut R,
        deserializer: &T,
    ) -> Result<T::Target, std::io::Error>
    where
        T: Deserializer,
    {
        const MAX_LENGTH: usize = 32;
        let mut header = Vec::with_capacity(MAX_LENGTH);
        let mut found = false;

        // Scan for `;` in the first 32 bytes.
        for _ in 0..MAX_LENGTH {
            let mut buf = [0];
            inp.read_exact(&mut buf)?;
            if buf[0] != b';' {
                header.push(buf[0]);
            } else {
                found = true;
                break;
            }
        }

        if !found {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Invalid compression header",
            ));
        }

        let compression = match &header[..] {
            b"identity" => Compression::Identity,
            #[cfg(feature = "brotli")]
            b"br" => Compression::Brotli,
            _ => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    "Invalid compression header",
                ))
            }
        };

        let mut byte_len = 0;
        inp.read_varnum_to(&mut byte_len)?;

        let mut compressed_bytes = Vec::with_capacity(byte_len as usize);
        unsafe { compressed_bytes.set_len(byte_len as usize) };
        inp.read_exact(&mut compressed_bytes)?;

        let decompressed_bytes = match compression {
            Compression::Identity => compressed_bytes,
            #[cfg(feature = "brotli")]
            Compression::Brotli => {
                let mut decoder =
                    brotli::Decompressor::new(Cursor::new(&compressed_bytes), BROTLI_BUFFER_SIZE);
                let mut buf = Vec::with_capacity(1024);
                decoder.read_to_end(&mut buf)?;
                buf
            }
        };

        let value = deserializer.read(&mut Cursor::new(decompressed_bytes))?;
        Ok(value)
    }
}
