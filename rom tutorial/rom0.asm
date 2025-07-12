;
;   Title:   rom0asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   05 Jul 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler using Z80 mnemonics (-80 option)
;            
;   Descr:   Make a small, "Hello, World" custom ROM
;            

.ORG    0D800H

start:      LD      A,0CAh      ; 7 BITS, NO PARITY, 2 STOP
            OUT     (3),A       ; UART A - SET MODE 
            LD      A,027H      ; ENABLE TX & RX
            OUT     (3),A       ; UART A - SET CONTROL
            LD      HL,str

loop:       LD      A,(HL)      ; load next char in string      
            OR      A           ; is it end-of-string NULL?
            JP      Z,stop      ; if so, we are done
            OUT     (2),A       ; send char to serial port
            INC     HL          ; point to next character in string
            JP      loop        ; and loop until done

stop:       JP      stop        ; loop here until reset

str:        .db "IMSAI 8080 Custom ROM",0

.END