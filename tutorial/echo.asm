;
;   Title:   echo.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   15 Apr 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   CP/M 2.2
;            TASM compiler using Z80 mnemonics (-80 option)
;
;   Descr:   Read characters from the TTY and display them
;            using CP/M BDOS function calls.
;            Exit to CP/M when "~" is typed.          
; 

.ORG    100h
BDOS    .EQU 5           ; call BDOS at 0005H
START:  LD   C,6         ; 6 = CP/M CONSOLE INPUT
        LD   E,0FFh      ; FF= GET INPUT STATUS
        CALL BDOS        ; RETURNS STATUS IN A
        OR   A           ; SET FLAGS
        JR   Z,START     ; LOOP UNTIL CHAR READY
        CP   '~'         ; IS CHAR A TILDE "~"?
        RET  Z           ; YES, EXIT
        LD   E,A         ; NO, GET RDY TO DISPLAY
        LD   C,2         ; 2 = CP/M CONSOLE OUTPUT
        CALL BDOS        ; ECHO CHAR TO SCREEN
        JR   START       ; AND GET NEXT CHAR
.END