
pub fn bytes_of_bool(data: Option<bool>) -> [u8; 1] {
    match data {
        None => [2],
        Some(true) => [1],
        Some(false) => [0]
    }
}

pub fn bool_of_bytes(buf: &[u8; 1]) -> Result<Option<bool>, ()> {
    match buf[0] {
        0 => Ok(Some(false)),
        1 => Ok(Some(true)),
        2 => Ok(None),
        _ => Err(())
    }
}

