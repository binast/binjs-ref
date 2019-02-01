use entropy::probabilities::IntoStatistics;
use io::statistics::Bytes;

use std::ffi::OsString;
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

/// The streams used internally to dump our data, both compressed and uncompressed.
struct Dumps {
    /// The stream used to dump uncompressed data.
    raw: std::fs::File,

    /// The stream used to dump compressed data.
    brotli: brotli::CompressorWriter<std::fs::File>,
}
impl Write for Dumps {
    fn write_all(&mut self, data: &[u8]) -> std::io::Result<()> {
        self.raw.write_all(data)?;
        self.brotli.write_all(data)?;
        Ok(())
    }
    fn write(&mut self, data: &[u8]) -> std::io::Result<usize> {
        self.write_all(data)?;
        Ok(data.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.raw.flush()?;
        self.brotli.flush()?;
        Ok(())
    }
}

/// Lazy initialization ensures that we do not create empty
/// sections in a compressed (rather, we skip the section
/// entirely) or empty files.
pub struct LazyStream {
    /// The uncompressed bytes written to this stream.
    buffer: Vec<u8>,

    /// An optional path to which to write an uncompressed copy
    /// of the data.
    dump_path: Option<std::path::PathBuf>,

    /// The file to the data must be written. Initialized
    /// in the first call to `write()` if `dump_path` is
    /// provided.
    lazy_dumps: Option<Dumps>,

    /// Number of bytes written, after compression.
    compressed_bytes: Option<usize>,

    /// Number of calls to `increment()` so far.
    instances: usize,
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
            lazy_dumps: None,
            instances: 0,
            compressed_bytes: None,
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
        self.buffer.len()
    }

    /// Get the file for writing, initializing it if necessary.
    ///
    /// Always returns Ok(None) if `dump_path` was specified as `None`.
    fn get_file(&mut self) -> std::io::Result<Option<&mut Dumps>> {
        if let Some(ref mut dumps) = self.lazy_dumps {
            return Ok(Some(dumps));
        }
        if let Some(mut path) = self.dump_path.take() {
            {
                let dir = path.parent().unwrap();
                std::fs::DirBuilder::new().recursive(true).create(dir)?;
            }

            let raw = std::fs::File::create(&path)?;

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

            let brotli = brotli::CompressorWriter::new(
                std::fs::File::create(path)?,
                BROTLI_BUFFER_SIZE,
                BROTLI_QUALITY,
                BROTLI_LG_WINDOW_SIZE,
            );

            self.lazy_dumps = Some(Dumps { raw, brotli });
            return Ok(Some(self.lazy_dumps.as_mut().unwrap()));
        }
        Ok(None)
    }

    /// Get the data that needs to be actually written to disk.
    ///
    /// Each call to `data()` causes the data to be compressed.
    pub fn data(&mut self) -> Option<Vec<u8>> {
        if self.buffer.len() == 0 {
            self.compressed_bytes = Some(0);
            return None;
        }

        // Actually compress the data.
        // Note that the only way I have found to ensure that the `CompressorWriter` completely
        // flushes, regardless of the data, is to drop the `CompressorWriter`. The simplest way
        // to implement this is to create the compressor in the call to `data()`.
        let mut result = vec![];
        {
            let mut brotli = brotli::CompressorWriter::new(
                &mut result,
                self.buffer.len(),
                BROTLI_QUALITY,
                BROTLI_LG_WINDOW_SIZE,
            );
            brotli.write_all(&self.buffer).unwrap();
        }
        self.compressed_bytes = Some(result.len());
        Some(result)
    }
}
impl std::io::Write for LazyStream {
    /// Compress the data to the Brotli stream, initializing it if necessary.
    /// Also dump the data to `dump_path` if `dump_path` was specified.
    fn write(&mut self, data: &[u8]) -> std::io::Result<usize> {
        // 1. Write to buffer.
        self.buffer.extend_from_slice(data);

        // 2. Write to file if necessary.
        if let Some(writer) = self.get_file()? {
            writer.write_all(data)?;
        }

        // 3. Done.
        Ok(data.len())
    }

    /// Flush all output streams.
    fn flush(&mut self) -> std::io::Result<()> {
        if let Some(ref mut writer) = self.lazy_dumps {
            writer.flush()?;
        }
        Ok(())
    }
}

impl IntoStatistics for LazyStream {
    type AsStatistics = Bytes;
    fn into_statistics(self, _description: &str) -> Bytes {
        self.compressed_bytes
            .expect(
                "Attempting `into_statistics` *before* computing the number of compressed bytes",
            )
            .into()
    }
}
