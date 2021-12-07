Print to stdout using syscall write

+>           cell  0:  1    Syscall code for write
+++>         cell  1:  3    3 arguments
>            cell  2:  0    argv(0) is a regular argument
+>           cell  3:  1    argv(0) cell length of 1
>            cell  4:  0    file descriptor 0 for stdout
+>           cell  5:  1    argv(1) type 1 = pointer
+++>         cell  6:  3    argv(1) cell length of 3
72+>         cell  7: 72    character 'H'
73+>         cell  8: 73    character 'I'
10+>         cell  9: 10    character '\n'
>            cell 10:  0    argv(2) is a regular argument
+>           cell 11:  1    argv(2) cell length of 1
+++          cell 12:  3    byte count of argument is 3
12<                         move to cell 0
%                           Execute the syscall
