;
;   Title:   showSP.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   08 Jun 2025
;      HW:   IMSAI 8080esp by TheHighNibble
;      SW:   TASM compiler, CPM 2.2
;
;   Descr:   Show the stack pointer at start of a CP/M program
;

.ORG 100H

   LD    HL,st1         
   CALL  printStr       ;"Stack Pointer"
   
   LD    HL,0
   ADD   HL,SP          ;get SP into HL
   CALL  printHex16     ;and display it on console

   LD    HL, st2
   CALL  printStr       ;": value "

   POP   HL             ;get value at stack pointer
   CALL  printHex16     ;and display it on console
   JP    0000           ;return to CP/M

st1: .db "STACK POINTER ",0
st2: .db ": value ",0

;=====================================================
; printCh -  prints ASCII character in reg A
;
printCh:                
    PUSH HL
    LD   C, 2           ; BDOS function 2 = CHARACTER OUTPUT
    LD   E, A           ; load E with character to be displayed
    CALL 0005h          ; call BDOS
    POP  HL
    RET  

;==========================================================
; printStr - Print String
;
printStr:
    LD   A, (HL)        ; get character in the string
    OR   A              ; is it zero?
    RET  Z              ; if it is, we are done.
    CALL printCh
    INC  HL             ; move to next character
    JR   printStr       ; and loop until done

;=====================================================
; printHex16 -  prints value of Reg HL as four-digit 
;               hexadecimal number. 
printHex16:
   LD   A, H
   CALL printHex        ; print upper 2 digits
   LD   A, L
   CALL printHex        ; print lower 2 digits
   RET

;=====================================================
; printHex -  prints value of Reg A as two-digit 
;             hexadecimal number.  
printHex:
   PUSH AF 
   CALL bh1             ; first char = upper nibble
   POP  AF
   CALL bh2             ; second char = lower nibble
   RET
bh1:	                  ; shift right 4 bits
   RRA
   RRA
   RRA
   RRA
bh2:	                  ; convert lower nibble to Hex
   OR	0F0h
   DAA
   ADD  A, 0A0h
   ADC  A, 40h
   CALL printCh          ; print the converted char
   RET

.END