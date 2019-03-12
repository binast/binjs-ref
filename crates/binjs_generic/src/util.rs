/// Return a random item from a slice.
pub fn pick<'a, T: rand::Rng, U>(rng: &mut T, slice: &'a [U]) -> &'a U {
    let index = rng.gen_range(0, slice.len());
    &slice[index]
}
