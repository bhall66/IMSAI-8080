;
;   Title:   stack4.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   08 Jun 2025
;      HW:   IMSAI 8080esp by TheHighNibble
;      SW:   TASM compiler, CPM 2.2
;
;   Descr:   This program creates a new stack
;            and then restores CP/M's stack on exit;
;
;            Very similar to stack3.asm, except that the 
;            new stack is located at the end of the code.      


.ORG 100H

   LD   HL,0
   ADD  HL,SP       ; save original SP in HL reg
   LD   SP,STACK    ; point to the new stack
   PUSH HL          ; push original SP onto new stack
   
   CALL BIGSTACK    ; push/pop 100 words on new stack

   POP  HL          ; get original SP off new stack
   LD   SP, HL      ; Restore the original (CP/M) stack
   RET              ; ans use it to return to CP/M


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

   .ds  256         ; since bigstack uses 200 bytes,
                    ; we must set stack size > 200.
                    
STACK:              ; notice stack begins here and
                    ; grows towards start of code

.END