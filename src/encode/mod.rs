mod env;
mod to_binary;
// mod to_serialize_tree;


use atoms::*;
use varnum::*;
use self::serialize_tree::*;
use self::to_binary::*;

use easter::prog::*;

use std;

/// Write an AST to a BinJS stream.
pub fn write<T>(ast: &Script, out: &mut T) -> Result<usize, std::io::Error> where T: std::io::Write {
    let mut env = env::Env::toplevel();

    // 1. Generate tree.
    // let tree = SerializeTree::Unlabelled(ast.to_naked(&mut env));
    let tree = SerializeTree::Unlabelled(unimplemented!());

    // Total bytes written.
    let mut bytes = 0;

    // Write header.
    bytes += out.write(b"BINJS")?;
    bytes += out.write_varnum(0)?;

    // 2. Collect atoms into an atoms table.
    let mut atoms = AtomsTableInitializer::new();
    tree.walk_unlabelled(&mut |item| {
        if let Unlabelled::Atom(ref s) = *item {
            atoms.add(s.clone())
        }
    });

    // 3. Write atoms table (first the lengths, then the table)
    let atoms = atoms.compile();
    bytes += atoms.write_index(out)?;

    // 4. Collect kinds into an atoms table.
    let mut kinds = AtomsTableInitializer::new();
    tree.walk_labelled(&mut |item| {
        kinds.add(*item.kind());
    });

    // 5. Write kinds table
    let kinds = kinds.compile();
    bytes += kinds.write_index(out)?;

    // 6. Write `tree`, substituting
    // kinds to `kinds` and atoms to `atoms`.
    bytes += tree.write(out, &atoms, &kinds)?;

    // FIXME: Check `bytes`?
    Ok(bytes)
}
