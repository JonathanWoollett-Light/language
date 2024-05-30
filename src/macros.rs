use std::collections::HashMap;

pub fn resolve_macros(bytes: &mut Vec<u8>) {
    let mut macros = HashMap::new();
    let mut i = 0;
    while i < bytes.len() {
        let n = i + 6;
        if let Some(macro_slice) = bytes.get(i..n) && macro_slice == b"macro " {
            let mut j = 1;
            let key = loop {
                if bytes[n + j] == b' ' {
                    break &bytes[n..n+j];
                }
                j += 1;
            };
            let mut k = 1;
            let value = loop {
                if bytes[n + j + k] == b'\n' {
                    break &bytes[n+j..n+j+k];
                }
                k += 1;
            };
            macros.insert(Vec::from(key), Vec::from(value));
            bytes.splice(i..n+j+k, []).for_each(drop);
        }
        for (key, value) in &macros {
            if let Some(slice) = bytes.get(i..=key.len()) 
                && &slice[..key.len()] == key
                && slice[key.len()] == b' ' {
                bytes.splice(i..key.len(),value.iter().copied()).for_each(drop);
                i += value.len() - key.len();
                continue;
            }
        }
        i += 1;
    } 
}