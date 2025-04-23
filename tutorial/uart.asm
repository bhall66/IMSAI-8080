;
;   Title:   uart.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   15 Apr 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler using Z80 mnemonics (-80 option)
;
;   Descr:   Read a byte from the TTY and display it.
;            
;            Taken from the IMSAI manual, page 9-36
;            and rewritten using Z80 mnemonics.
; 

.ORG    100H
START:  LD   A,0CAH      ; 7 BITS, NO PARITY, 2 STOP
        OUT  (3),A       ; UART A - SET MODE 
        LD   A,027H      ; ENABLE TX & RX
        OUT  (3),A       ; UART A - SET CONTROL
U1:     IN   A,(3)       ; CHECK STATUS
        AND  2           ; LOOK @ RX READY BIT ONLY
        JR   Z,U1        ; WAIT FOR A CHARACTER
        IN   A,(2)       ; UART A - READ CHAR INTO A REG
        OUT  (2),A       ; AND WRITE THE RESULT
        JR   U1          ; WAIT FOR NEXT CHARACTER
.END