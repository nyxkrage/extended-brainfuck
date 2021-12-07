use std::env::args;
use std::io::stdout;
use std::io::Read;
use std::io::Write;

use exbf::parse;
use exbf::run;
use exbf::Flags;
use exbf::Instruction;

fn main() {
    let source = {
        let mut s = String::new();
        match args().skip(1).next() {
            Some(a) => std::fs::File::open(a)
                .unwrap()
                .read_to_string(&mut s)
                .unwrap(),
            None => panic!("Fuck you, pass a file"),
        };
        s
    };

    let input = if !parse(&source).unwrap().contains(&Instruction::In) {
        "".to_owned()
    } else {
        let mut s = String::new();
        if atty::is(atty::Stream::Stdin) {
            std::io::stdin().read_line(&mut s).unwrap()
        } else {
            std::io::stdin().read_to_string(&mut s).unwrap()
        };
        s
    };
    let output = run(&source, &input, Flags::AllowAll).unwrap();
    print!("{}", output);
    stdout().flush().unwrap();
}
