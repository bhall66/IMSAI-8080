;
;   Title:   stack1.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   08 Jun 2025
;      HW:   IMSAI 8080esp by TheHighNibble
;      SW:   TASM compiler, CPM 2.2
;
;   Descr:   Overflow the CCP stack - on purpose
;            The available CCP stack is 7 levels deep.
;            BIGSTACK uses 100 levels, easily overflowing it.
;            
;            This program exits via a jump to 0000h,
;            will will warm boot CP/M and reload the CCP.
;            (It should not fail like stack0.asm)

.ORG 100H

   CALL BIGSTACK    ; push 100 words onto the stack
   JP   0000        ; return to CP/M with warm boot



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