;
;   Title:   stack6.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   08 Jun 2025
;      HW:   IMSAI 8080esp by TheHighNibble
;      SW:   TASM compiler, CPM 2.2
;
;   Descr:   Create a local stack just below CCP
;
;            THis stack maximizes stack space while
;            preserving current copy of the CCP.
;          
;            NOTE: Assumes CCP is 1600h below BIOS,
;            which is true for CP/M 2.2 but not
;            for other OS versions/alternatives.


.ORG 100H

   LD   IX,0        
   ADD  IX,SP       ; temp save original SP in IX
   LD   HL,(0001)   ; get warm boot vector = BIOS+3
   LD   BC,1603h     
   SBC  HL,BC       ; calc start of CPP = BIOS-1600h
   LD   SP,HL       ; point to new stack
   PUSH IX          ; push original SP on new stack

   CALL BIGSTACK    ; push/pop 100 words onto stack

   POP  HL          ; get original SP off local stack
   LD   SP,HL       ; restore to orginal CCP stack
   RET              ; and use it to return to CP/M

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