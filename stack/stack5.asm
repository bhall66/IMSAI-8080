;
;   Title:   stack5.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   08 Jun 2025
;      HW:   IMSAI 8080esp by TheHighNibble
;      SW:   TASM compiler, CPM 2.2
;
;   Descr:   Mix and Match Stack Method ---
;
;            Establishes a local stack (like stack3.asm)
;            returns to CP/M via warm boot (like stack1.asm)
;            Note: if your system uses high memory, set the
;            stack in the TPA rather than at FFFF.


.ORG 100H

   LD   SP,0        ; Establish new stack at FFFF   
   CALL BIGSTACK    ; push/pop 100 words onto stack
   JP   0000        ; return to CP/M via warm boot


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