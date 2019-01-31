//! Generate a static prediction table from a sample of JS source files.

use ::generic::FromJSON;
use ::io::entropy::dictionary::{Dictionary, DictionaryBuilder, FilesContaining};
use ::io::statistics::Instances;
use ::io::{Path as IOPath, TokenSerializer};
use ::source::{Shift, SourceParser};
use ::specialized::es6::ast::Walker;

use std::io::{Error, ErrorKind};
use std::path::Path;

pub struct Options<'a> {
    pub parser: &'a Shift,
    pub lazification: u32,
    pub quiet: bool,
    pub depth: usize,
    pub width: usize,
    pub threshold: FilesContaining,
}

macro_rules! progress {
    ($quiet:expr, $($args:tt)*) => {
        if !$quiet {
            println!($($args)*);
        }
    }
}

fn handle_path<'a>(
    options: &mut Options<'a>,
    shared_builder: &mut DictionaryBuilder,
    shared_number_of_files: &mut usize,
    source_path: &Path,
    sub_dir: &Path,
) -> Result<(), std::io::Error> {
    progress!(options.quiet, "Treating {:?} ({:?})", source_path, sub_dir);
    let is_dir = std::fs::metadata(source_path).unwrap().is_dir();
    if is_dir {
        let file_name = source_path
            .file_name()
            .ok_or_else(|| Error::new(ErrorKind::NotFound, "Cannot get file name from directory"))?;

        let sub_dir = sub_dir.join(file_name);
        for entry in std::fs::read_dir(source_path)?
            .map(|dir| dir.unwrap())
        {
            handle_path(
                options,
                shared_builder,
                shared_number_of_files,
                &entry.path().as_path(),
                &sub_dir,
            )?;
        }
        return Ok(());
    }
    if let Some(Some("js")) = source_path.extension().map(std::ffi::OsStr::to_str) {
        // Proceed
    } else {
        progress!(options.quiet, "Skipping {:?}", source_path);
        return Ok(());
    }

    progress!(options.quiet, "Parsing.");

    handle_path_or_text(options, shared_builder, shared_number_of_files, source_path)
}

fn handle_path_or_text<'a>(
    options: &mut Options<'a>,
    dictionary_builder: &mut DictionaryBuilder,
    shared_number_of_files: &mut usize,
    source: &Path,
) -> Result<(), std::io::Error> {
    let json = options
        .parser
        .parse_file(source)
        .map_err(|e| Error::new(ErrorKind::Other, format!("Parse error '{:?}'", e)))?;

    let mut ast =
        ::specialized::es6::ast::Script::import(&json)
        .map_err(|e| Error::new(ErrorKind::Other, format!("Import error '{:?}'", e)))?;
    ::specialized::es6::scopes::AnnotationVisitor::new().annotate_script(&mut ast);

    if options.lazification > 0 {
        progress!(options.quiet, "Introducing laziness.");
        let mut path = ::specialized::es6::ast::WalkPath::new();
        let mut visitor = ::specialized::es6::lazy::LazifierVisitor::new(options.lazification);
        ast.walk(&mut path, &mut visitor)
            .map_err(|e| Error::new(ErrorKind::Other, format!("Lazification error '{:?}'", e)))?;
    }

    progress!(options.quiet, "Building dictionary.");

    {
        let mut serializer = ::specialized::es6::io::Serializer::new(dictionary_builder);
        serializer
            .serialize(&ast, &mut IOPath::new())
            .map_err(|e| Error::new(ErrorKind::Other, format!("Dictionary construction '{:?}'", e)))?;
        serializer.done()
            .map_err(|e| Error::new(ErrorKind::Other, format!("Dictionary finalization error '{:?}'", e)))?;
    }

    *shared_number_of_files += 1;
    Ok(())
}

pub fn generate_dictionary(sources: &[std::path::PathBuf], mut options: Options) -> Result<Dictionary<Instances>, std::io::Error> {
    let mut number_of_files = 0;

    let mut builder = DictionaryBuilder::new(options.depth, options.width);

    // Process files.
    for source_path in sources {
        handle_path(
            &mut options,
            &mut builder,
            &mut number_of_files,
            source_path,
            /* local root */ Path::new(""),
        )?;
    }


    Ok(builder.done(options.threshold))
}
