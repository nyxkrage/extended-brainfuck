# Extended Brainfuck Interpreter

Brainfuck interpreter with support for syscalls with the new % operator, and repeating instructions with \<INS\>\<NUMBER\>, like +12 would execute the + instruction 12 times
The syscalls follow the [Systemf](https://github.com/ajyoon/systemf) implentation.
See [bf/syscall_hello.bf](bf/syscall_hello.bf) for an example.
