use exbf::include_base_str;
use exbf::run;
use exbf::Flags;

fn main() {
    println!(
        "{}",
        run(
            include_base_str!("bf/cat.bf"),
            "Hello World!",
            Flags::AllowAll,
        )
        .unwrap()
    );
}
