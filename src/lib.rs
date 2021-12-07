use flagset::{flags, FlagSet};
use syscalls::{syscall0, syscall1, syscall2, syscall3, syscall4, syscall5, syscall6};
use thiserror::Error;

mod syscall;
use syscall::SyscallType;

#[macro_export]
macro_rules! include_base_str {
    ($path:literal) => {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/", $path))
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    Right,
    Left,
    Inc,
    Dec,
    Out,
    In,
    Syscall,
    Open(usize),
    Close(usize),
}

#[derive(Error, Debug)]
pub enum BrainfuckError {
    #[error("Umatched bracket {bracket} at {position}")]
    UnmatchedBracket { bracket: char, position: usize },
    #[error("Memory Pointer increased beyond scope of memory")]
    MemoryPointerOverflow,
    #[error("Memory overflow")]
    MemoryOverflow,
    #[error("Can't output invalid UTF8")]
    InvalidCharacter,
    #[error("Invalid argument type for syscall")]
    InvalidArgumentType,
    #[error("Syscall failed")]
    SyscallFailed(#[from] syscalls::Errno),
}

fn find_matching_bracket(
    position: usize,
    ops: &[char],
    find_open: bool,
) -> Result<usize, BrainfuckError> {
    let mut index = position;
    let mut depth = 1;
    let (open, close) = if find_open { (']', '[') } else { ('[', ']') };

    loop {
        if ops[index] == open {
            depth += 1;
        } else if ops[index] == close {
            depth -= 1;
        }

        if depth == 0 {
            break Ok(index);
        }

        if index == ops.len() {
            break Err(BrainfuckError::UnmatchedBracket {
                bracket: close,
                position,
            });
        }

        if find_open {
            index -= 1;
        } else {
            index += 1;
        };
    }
}

#[repr(u8)]
#[derive(Debug)]
enum ArgumentType {
    Value,
    Pointer,
    CellPointer,
}

impl ArgumentType {
    fn from(v: usize) -> Result<Self, BrainfuckError> {
        if v == 0 {
            Ok(Self::Value)
        } else if v == 1 {
            Ok(Self::Pointer)
        } else if v == 2 {
            Ok(Self::CellPointer)
        } else {
            Err(BrainfuckError::InvalidArgumentType)
        }
    }
}

#[derive(Debug)]
struct SyscallArg {
    arg_type: ArgumentType,
    length: usize,
    contents: usize,
}

pub fn parse(source: &str) -> Result<Vec<Instruction>, BrainfuckError> {
    let ops: Vec<char> = source
        .chars()
        .filter(|c| matches!(*c, '>' | '<' | '+' | '-' | '.' | ',' | '[' | ']' | '%'))
        .collect();
    println!("Source: {}", ops.iter().collect::<String>());
    let ops = ops
        .iter()
        .enumerate()
        .map(|(i, o)| match o {
            '>' => Ok(Instruction::Right),
            '<' => Ok(Instruction::Left),
            '+' => Ok(Instruction::Inc),
            '-' => Ok(Instruction::Dec),
            '.' => Ok(Instruction::Out),
            ',' => Ok(Instruction::In),
            '%' => Ok(Instruction::Syscall),
            '[' => find_matching_bracket(i + 1, &ops, false).map(Instruction::Open),
            ']' => find_matching_bracket(i - 1, &ops, true).map(Instruction::Close),
            _ => unreachable!("Everything noop should be filtered above"),
        })
        .collect();
    println!("{:?}", ops);
    ops
}

flags! {
  pub enum Flags: u8 {
      DisallowAll = 0b00,
      AllowMemOverflow = 0b01,
      AllowMemPtrOverflow = 0b10,
      AllowAll = (Flags::AllowMemOverflow | Flags::AllowMemPtrOverflow).bits(),
  }
}

const MEMORY_SIZE: usize = 65535;

pub fn run(
    source: &str,
    input: &str,
    safety_flags: impl Into<FlagSet<Flags>>,
) -> Result<String, BrainfuckError> {
    let instructions = parse(source)?;
    let mut input_iter = input.chars();
    let flags: FlagSet<Flags> = safety_flags.into();

    let mut memory = [0u8; MEMORY_SIZE];

    let mut instruction_couter = 0;
    let mut memory_counter = 0;

    let mut output = String::new();

    while let Some(instruction) = instructions.get(instruction_couter) {
        match instruction {
            Instruction::Right => {
                if memory_counter + 1 >= MEMORY_SIZE {
                    if flags.contains(Flags::AllowMemPtrOverflow) {
                        memory_counter = 0
                    } else {
                        return Err(BrainfuckError::MemoryPointerOverflow);
                    }
                } else {
                    memory_counter += 1;
                }
            }
            Instruction::Left => {
                if memory_counter == 0 {
                    if flags.contains(Flags::AllowMemPtrOverflow) {
                        memory_counter = MEMORY_SIZE - 1
                    } else {
                        return Err(BrainfuckError::MemoryPointerOverflow);
                    }
                } else {
                    memory_counter -= 1;
                }
            }
            Instruction::Inc => {
                memory[memory_counter] = if flags.contains(Flags::AllowMemOverflow) {
                    memory[memory_counter].wrapping_add(1)
                } else {
                    memory[memory_counter]
                        .checked_add(1)
                        .ok_or(BrainfuckError::MemoryOverflow)?
                }
            }
            Instruction::Dec => {
                memory[memory_counter] = if flags.contains(Flags::AllowMemOverflow) {
                    memory[memory_counter].wrapping_sub(1)
                } else {
                    memory[memory_counter]
                        .checked_sub(1)
                        .ok_or(BrainfuckError::MemoryOverflow)?
                }
            }
            Instruction::Out => {
                output.push(
                    char::from_u32(memory[memory_counter] as u32)
                        .ok_or(BrainfuckError::InvalidCharacter)?,
                );
            }
            Instruction::In => memory[memory_counter] = input_iter.next().unwrap_or('\0') as u8,
            Instruction::Syscall => {
                let mut offset = 1;
                let syscall: SyscallType = if memory[memory_counter] == 255 {
                    offset += 1;
                    memory[memory_counter] as u16 + memory[memory_counter + 1] as u16
                } else {
                    memory[memory_counter] as u16
                }
                .into();
                let argc = memory[memory_counter + offset];
                offset += 1;
                let mut args = Vec::with_capacity(argc as usize);
                for _ in 0..argc {
                    let mut arg = SyscallArg {
                        arg_type: ArgumentType::from(memory[memory_counter + offset] as usize)?,
                        length: memory[memory_counter + 1 + offset] as usize,
                        contents: 0,
                    };
                    arg.contents = match arg.arg_type {
                        ArgumentType::Value => {
                            let c = &memory[memory_counter + 2 + offset
                                ..memory_counter + 2 + offset + arg.length];
                            offset += arg.length + 2;
                            c[0] as usize
                        }
                        ArgumentType::Pointer => {
                            let p = unsafe {
                                memory
                                    .as_ptr()
                                    .offset((memory_counter + 2 + offset) as isize)
                            };
                            offset += 2 + arg.length;
                            p as usize
                        }
                        ArgumentType::CellPointer => {
                            let p = memory[memory_counter + 2 + offset] as usize;
                            offset += 2;
                            memory[p] as usize
                        }
                    };
                    args.push(arg);
                }

                println!("{} : {}->{:?}", syscall, argc, args);

                let res = unsafe {
                    match argc {
                        0 => syscall0((syscall as i32).into()),
                        1 => syscall1((syscall as i32).into(), args[0].contents as usize),
                        2 => syscall2(
                            (syscall as i32).into(),
                            args[0].contents as usize,
                            args[1].contents as usize,
                        ),
                        3 => syscall3(
                            (syscall as i32).into(),
                            args[0].contents as usize,
                            args[1].contents as usize,
                            args[2].contents as usize,
                        ),
                        4 => syscall4(
                            (syscall as i32).into(),
                            args[0].contents as usize,
                            args[1].contents as usize,
                            args[2].contents as usize,
                            args[3].contents as usize,
                        ),
                        5 => syscall5(
                            (syscall as i32).into(),
                            args[0].contents as usize,
                            args[1].contents as usize,
                            args[2].contents as usize,
                            args[3].contents as usize,
                            args[4].contents as usize,
                        ),
                        6 => syscall6(
                            (syscall as i32).into(),
                            args[0].contents as usize,
                            args[1].contents as usize,
                            args[2].contents as usize,
                            args[3].contents as usize,
                            args[4].contents as usize,
                            args[5].contents as usize,
                        ),
                        _ => unimplemented!("fuck you"),
                    }
                }
                .unwrap();
                memory[memory_counter] = res as u8;
            }
            Instruction::Open(i) => {
                if memory[memory_counter] == 0 {
                    instruction_couter = *i;
                }
            }
            Instruction::Close(i) => {
                if memory[memory_counter] != 0 {
                    instruction_couter = *i;
                }
            }
        }
        instruction_couter += 1;
    }

    Ok(output)
}

#[cfg(test)]
mod tests {
    use crate::Instruction;

    use super::parse;
    use super::run;
    use super::Flags;

    #[test]
    fn hello_world() {
        assert_eq!(
            run(include_base_str!("bf/hello.bf"), "", Flags::AllowAll).unwrap(),
            "Hello World!\n"
        );
    }

    #[test]
    fn quine() {
        assert_eq!(
            run(include_base_str!("bf/quine.bf"), "", Flags::AllowAll).unwrap(),
            include_base_str!("bf/quine.bf").trim()
        );
    }

    #[test]
    fn cat() {
        assert_eq!(
            run(include_base_str!("bf/cat.bf"), "Hello", Flags::AllowAll).unwrap(),
            "Hello"
        );
    }

    #[test]
    fn cat_tree() {
        assert_eq!(
            parse(include_base_str!("bf/cat.bf")).unwrap(),
            vec![
                Instruction::In,
                Instruction::Open(4),
                Instruction::Out,
                Instruction::In,
                Instruction::Close(1)
            ]
        );
    }

    #[test]
    fn exit_syscall() {
        run(
            include_base_str!("bf/syscall_hello.bf"),
            "",
            Flags::AllowAll,
        )
        .unwrap();
    }
}
