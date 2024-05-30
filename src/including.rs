pub fn get_includes(source: &mut Vec<u8>) {
    let mut i = 0;
    const INCLUDE: &[u8] = b"include ";
    while i < source.len() - INCLUDE.len() {
        let j = i + INCLUDE.len();
        if &source[i..j] == INCLUDE {
            let mut k = j;
            loop {
                k += 1;
                match source.get(k) {
                    Some(b'\n') => break,
                    Some(_) => continue,
                    None => panic!(),
                }
            }

            // let paths = std::fs::read_dir("./").unwrap();
            // for path in paths {
            //     eprintln!("Name: {}", path.unwrap().path().display())
            // }

            let str = std::str::from_utf8(&source[j..k]).unwrap();
            // eprintln!("str: {str:?}");
            let text = if let Some(b"http") = source.get(j..j + 4) {
                reqwest::blocking::get(str).unwrap().text().unwrap()
            } else {
                std::fs::read_to_string(str).unwrap()
            };
            source.splice(i..k, text.bytes());
        } else {
            i += 1;
        }
    }
}
