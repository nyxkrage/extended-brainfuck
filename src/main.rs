use systemf::include_base_str;
use systemf::run;
use systemf::Flags;

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
