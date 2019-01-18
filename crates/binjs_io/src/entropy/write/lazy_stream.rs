use entropy::probabilities::IntoStatistics;
use io::statistics::Bytes;

use std::io::Write;

/// An arbitrary buffer size for Brotli compression.
///
/// This should not impact file size, only compression speed.
const BROTLI_BUFFER_SIZE: usize = 32768;

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

/// A segment of data, initialized lazily. Also supports writing
/// all data to an external file, for
/// forensics purposes.

/// Lazy initialization ensures that we do not create empty
/// sections in a compressed (rather, we skip the section
/// entirely) or empty files.
pub struct LazyStream {
    /// A brotli-compressed buffer with the data.
    ///
    /// Initialize in the first call to `write()`.
    lazy_brotli: Option<brotli::CompressorWriter<Vec<u8>>>,

    /// An optional path to which to write an uncompressed copy
    /// of the data.
    dump_path: Option<std::path::PathBuf>,

    /// The file to the data must be written. Initialized
    /// in the first call to `write()` if `dump_path` is
    /// provided.
    lazy_dump_file: Option<std::fs::File>,

    /// Number of calls to `increment()` so far.
    instances: usize,

    /// Number of bytes received during calls to `write()`. Used
    /// for debugging and statistics.
    bytes_written: usize,
}
impl LazyStream {
    /// Create a new LazyStream.
    ///
    /// If `dump_path.is_some()`, all data written to this stream
    /// will also be dumped in the file at `dump_path`.
    pub fn new(dump_path: Option<std::path::PathBuf>) -> Self {
        LazyStream {
            dump_path,
            lazy_brotli: None,
            lazy_dump_file: None,
            instances: 0,
            bytes_written: 0,
        }
    }

    /// Count one more instance.
    pub fn increment(&mut self) {
        self.instances += 1;
    }

    /// Return the number of calls to `increment()` so far.
    pub fn instances(&self) -> usize {
        self.instances
    }

    /// Return the number of bytes written so far.
    pub fn bytes_written(&self) -> usize {
        self.bytes_written
    }

    /// Get the file for writing, initializing it if necessary.
    ///
    /// Always returns Ok(None) if `dump_path` was specified as `None`.
    fn get_file(&mut self) -> Result<Option<&mut std::fs::File>, std::io::Error> {
        if let Some(ref mut writer) = self.lazy_dump_file {
            return Ok(Some(writer));
        }
        if let Some(ref path) = self.dump_path {
            let dir = path.parent().unwrap();
            std::fs::DirBuilder::new().recursive(true).create(dir)?;
            let file = std::fs::File::create(path)?;
            self.lazy_dump_file = Some(file);
            return Ok(Some(self.lazy_dump_file.as_mut().unwrap()));
        }
        Ok(None)
    }

    /// Get the data that needs to be actually written to disk.
    pub fn data(&self) -> Option<&[u8]> {
        match self.lazy_brotli {
            Some(ref writer) => Some(writer.get_ref().as_slice()),
            None => None,
        }
    }
}
impl std::io::Write for LazyStream {
    /// Compress the data to the Brotli stream, initializing it if necessary.
    /// Also dump the data to `dump_path` if `dump_path` was specified.
    fn write(&mut self, data: &[u8]) -> Result<usize, std::io::Error> {
        // 1. Write to Brotli.
        {
            let brotli = self.lazy_brotli.get_or_insert_with(|| {
                brotli::CompressorWriter::new(
                    Vec::with_capacity(BROTLI_BUFFER_SIZE),
                    BROTLI_BUFFER_SIZE,
                    BROTLI_QUALITY,
                    BROTLI_LG_WINDOW_SIZE,
                )
            });
            brotli.write_all(data)?;
        }

        // 2. Write to file if necessary.
        if let Some(writer) = self.get_file()? {
            writer.write_all(data)?;
        }

        // 3. Done.
        self.bytes_written += data.len();
        Ok(data.len())
    }

    /// Flush all output streams.
    fn flush(&mut self) -> Result<(), std::io::Error> {
        if let Some(ref mut writer) = self.lazy_brotli {
            writer.flush()?;
        }
        if let Some(ref mut writer) = self.lazy_dump_file {
            writer.flush()?;
        }
        Ok(())
    }
}

impl IntoStatistics for LazyStream {
    type AsStatistics = Bytes;
    fn into_statistics(mut self, _description: &str) -> Bytes {
        match self.lazy_brotli {
            None => 0.into(),
            Some(ref mut writer) => {
                writer.flush().unwrap(); // Writing to a `Vec<u8>` cannot fail.
                writer.get_ref().len().into()
            }
        }
    }
}
