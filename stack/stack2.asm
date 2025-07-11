;
;   Title:   stack2.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   08 Jun 2025
;      HW:   IMSAI 8080esp by TheHighNibble
;      SW:   TASM compiler, CPM 2.2
;
;   Descr:   Overflow the CCP stack - on purpose
;            The available CCP stack is 7 levels deep.
;            BIGSTACK uses 100 levels, easily overflowing it.
;            
;            This program exits with a return statement,
;            but since 0000 was pushed onto the stack it
;            will will warm boot CP/M and reload the CCP.
;            (It should not fail like stack0.asm)

.ORG 100H

   LD   HL,0
   PUSH HL          ; push 0000 onto the stack
   CALL BIGSTACK    ; push 100 words onto the stack
   RET              ; return to CP/M via warm boot



BIGSTACK:
   LD   DE,7676h    ; fillchar 76 = HALT instruction
   LD   B,100       ; use B reg to count pushes
bs1:
   PUSH DE          ; push a word on the stack
   DJNZ bs1         ; and repeat x 100
   LD   B,100       ; reload counter
bs2:
   POP DE           ; pop the word off the stack
   DJNZ bs2         ; and repeat x 100
   RET              ; return back to main program

.END