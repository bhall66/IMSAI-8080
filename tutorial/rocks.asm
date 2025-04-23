;
;   Title:   rocks.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   15 Apr 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   CP/M 2.2
;            TASM compiler using Z80 mnemonics (-80 option)
;
;   Descr:   Write a string to the console
;            using CP/M BDOS function call.        
; 

.ORG    100h
START:  LD   C,9         ; 9 = BDOS STRING OUTPUT
        LD   DE,STR1     ; POINT TO THE STRING,
        CALL 5           ; PRINT IT,
        RET              ; AND RETURN TO CP/M
STR1:   .DB "This IMSAI 8080 rocks!",13,10,"$"
.END