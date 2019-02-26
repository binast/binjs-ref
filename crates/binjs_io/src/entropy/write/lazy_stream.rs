use std::ffi::OsString;
use std::fs;
use std::io::Write;

/// Highest Brotli compression level.
///
/// Higher values provide better file size but slower compression.
// FIXME: Should probably become a compression parameter.
const BROTLI_QUALITY: u32 = 11;

/// An arbitrary window size for Brotli compression.
///
/// Higher values provide better file size but slower compression.
// FIXME: SHould probably become a compression parameter.
const BROTLI_LG_WINDOW_SIZE: u32 = 20;

/// Lazy initialization ensures that we do not create empty
/// sections in a compressed (rather, we skip the section
/// entirely) or empty files.
pub struct LazyStream {
    /// The uncompressed bytes written to this stream.
    buffer: Vec<u8>,

    /// An optional path to which to write an uncompressed copy
    /// of the data. If unspecified, nothing is written.
    dump_path: Option<std::path::PathBuf>,
}

impl LazyStream {
    /// Create a new LazyStream.
    ///
    /// If `dump_path.is_some()`, all data written to this stream
    /// will also be dumped in the file at `dump_path`.
    pub fn new(dump_path: Option<std::path::PathBuf>) -> Self {
        LazyStream {
            dump_path,
            buffer: vec![],
        }
    }

    /// Return the number of bytes written so far.
    pub fn bytes_written(&self) -> usize {
        self.buffer.len()
    }

    /// Get the data that needs to be actually written to disk.
    pub fn done(mut self) -> std::io::Result<Option<Vec<u8>>> {
        if self.buffer.len() == 0 {
            return Ok(None);
        }

        // Actually compress the data.
        // Note that the only way I have found to ensure that the `CompressorWriter` completely
        // flushes, regardless of the data, is to drop the `CompressorWriter`. The simplest way
        // to implement this is to create the compressor in the call to `done()`.
        let mut brotli_compressed = vec![];
        {
            let mut brotli = brotli::CompressorWriter::new(
                &mut brotli_compressed,
                self.buffer.len(),
                BROTLI_QUALITY,
                BROTLI_LG_WINDOW_SIZE,
            );
            brotli.write_all(&self.buffer).unwrap();
        }

        // Now dump to files if necessary.
        if let Some(mut path) = self.dump_path.take() {
            {
                let dir = path.parent().unwrap();
                fs::DirBuilder::new().recursive(true).create(dir)?;
            }

            // Prepare the file for raw dumping.
            fs::write(&path, &self.buffer)?;

            // Prepare the file for brotli-compressed dumping.
            // Create a double-extension ".foo.bro".
            let extension = match path.extension() {
                None => OsString::from("bro"),
                Some(ext) => {
                    let mut as_os_string = ext.to_os_string();
                    as_os_string.push(".bro");
                    as_os_string
                }
            };
            path.set_extension(extension);
            fs::write(path, &brotli_compressed)?;
        }

        Ok(Some(brotli_compressed))
    }
}
impl std::io::Write for LazyStream {
    /// Store the data for a later flush.
    fn write(&mut self, data: &[u8]) -> std::io::Result<usize> {
        self.buffer.extend_from_slice(data);
        Ok(data.len())
    }

    /// Flush all output streams.
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
