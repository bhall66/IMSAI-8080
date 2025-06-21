;
;   Title:   bank5.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   20 Jun 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   CP/M 2.2
;            TASM compiler using Z80 mnemonics (-80 option)
;
;   Descr:   Loads a payload program into High memory (C000h) 
;
;            Payload should be assembled with org of C000h
;            and amended to this code at 0140h.
;
;            Use DDT to append this code with a 
;            payload offset of 4140h (C000+4140=0140)    

.ORG    100h

SOURCE   .EQU 140h      ; payload address
DEST     .EQU 0C000h    ; payload destination address
CODELEN  .EQU 180h      ; payload code size, in bytes

    LD   HL,st0
    CALL printStr       ; show title   

; Now move primary code into high memory (C000)
; and then execute it.

    LD   HL,SOURCE      ; source addr
    LD   DE,DEST        ; destination addr
    LD   BC,CODELEN     ; code length
    LDIR                ; move it!
    JP   DEST           ; and start execution there

st0:    .db  13,10,"W8BH HiLoader",13,10,0

;==========================================================
; printStr - Print String
;
; Prints a null-terminated string to the console
; Call with HL pointing to the string


printStr:
    LD   A, (HL)        ; get character in the string
    OR   A              ; is it zero?
    RET  Z              ; if it is, we are done.
    OUT (2),A           ; show char on console
    INC  HL             ; move to next character
    JR   printStr       ; and loop until done

.END