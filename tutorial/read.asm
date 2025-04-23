;
;   Title:   read.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   15 Apr 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   CP/M 2.2
;            TASM compiler using Z80 mnemonics (-80 option)
;
;   Descr:   Read a string from console and store it in memory
; 

.ORG 100h
START:  LD   A,80       ;buffer size 80 char
        LD   DE,BUF     ;point to buffer
        LD   (DE),A     ;set maximum length
        LD   C,10       ;bdos 10 = console read
        CALL 5          ;get input from user
        RET             ;return to CP/M

BUF:                    ;buffer structure ----
BUFLEN: .DB 0           ;first byte = max length
STRLEN: .DB 0           ;second byte = actual length
BUFDAT: .FILL 80        ;80 bytes of char data
.END