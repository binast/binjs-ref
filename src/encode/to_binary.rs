use atoms::*;
use encode::serialize_tree::*;
use kind::*;
use varnum::*;

use std;
use std::io::*;

pub trait Writable {
    fn write<W>(&self, out: &mut W, atoms: &AtomsTable<String>, kinds: &AtomsTable<Kind>) -> Result<usize> where W: Write;
}

impl Writable for SerializeTree<Kind> {
    fn write<W>(&self, out: &mut W, atoms: &AtomsTable<String>, kinds: &AtomsTable<Kind>) -> Result<usize> where W: Write {
        match *self {
            SerializeTree::Unlabelled(ref tree) => tree.write(out, atoms, kinds),
            SerializeTree::Labelled(ref tree) => tree.write(out, atoms, kinds)
        }
    }
}

impl SerializeTree<Kind> {
    fn write_list<W>(data: &[Self], out: &mut W, include_length: bool, atoms: &AtomsTable<String>, kinds: &AtomsTable<Kind>) -> Result<usize> where W: Write {
       // Prepare contents.
       let mut buf = Vec::with_capacity(1024); // FIXME: Arbitrary length.
       for item in data {
           item.write(&mut buf, atoms, kinds)?;
       }
       let mut bytes = 0;

       if include_length {
           // Write byte length.
           bytes += out.write_varnum(buf.len() as u32)?;

           // Write number of items.
           bytes += out.write_varnum(data.len() as u32)?;
       }

       // Write contents.
       bytes += out.write(&buf)?;

       Ok(bytes)
   }
}

impl Writable for Labelled<Kind> {
    fn write<W>(&self, out: &mut W, atoms: &AtomsTable<String>, kinds: &AtomsTable<Kind>) -> Result<usize> where W: Write {
        let mut bytes = 0;
        bytes += out.write_varnum(kinds.get_key(&self.kind()).unwrap())?;
        bytes += self.tree().write(out, atoms, kinds)?;
        Ok(bytes)
    }
}

impl Writable for Unlabelled<Kind> {
    fn write<W>(&self, out: &mut W, atoms: &AtomsTable<String>, kinds: &AtomsTable<Kind>) -> Result<usize> where W: Write {
        use encode::serialize_tree::Unlabelled::*;
        match *self {
            // Short nodes: do not write byteLength
            Atom(ref string) =>
                out.write_varnum(atoms.get_key(string).unwrap()),
            RawByte(ref byte) => {
                let buf = [*byte];
                out.write(&buf)
            }
            RawFloat(ref float) => {
                assert!(std::mem::size_of_val(float) == std::mem::size_of::<[u8;8]>());
                // FIXME: This makes assumptions on endianness.
                let buf = unsafe { std::mem::transmute::<f64, [u8;8]>(*float) };
                out.write(&buf)
            }
            // Long nodes: write the bytelength
            Tuple(ref list) =>
                SerializeTree::write_list(list, out, false, atoms, kinds),
            List(ref list) =>
                SerializeTree::write_list(list, out, true, atoms, kinds),
        }
    }
}