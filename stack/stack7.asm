;
;   Title:   stack7.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   08 Jun 2025
;      HW:   IMSAI 8080esp by TheHighNibble
;      SW:   TASM compiler, CPM 2.2
;
;   Descr:   Return to CCP without restoring its stack
;
;            If an executed program returns to CP/M 2.2
;            via RET, CP/M will restore its own stack.
;


.ORG 100H

   POP  HL          ; Get CCP return address 
   LD   SP,2000h    ; Establish local stack
   PUSH HL          ; push return address on new stack

   CALL BIGSTACK    ; push/pop 100 words onto stack

   RET              ; return to CP/M

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