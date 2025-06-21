;
;   Title:   bank2.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   20 Jun 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   CP/M 2.2
;            TASM compiler using Z80 mnemonics (-80 option)
;
;   Descr:   Second attempt at bank switching.     
;            Locate program in common memory instead of TPA.
;
;            To execute it, "DDT bank2.hex", then "gC000"

MMU     .EQU 40H    ; MMU control port

.ORG    0C000h      ; reside in nonbanked RAM

    LD  A, 2        ; select memory bank #2
    OUT (MMU),A     ; make it so.
                    ; do stuff in bank #2
    LD  A, 0        ; return to bank #0  
    OUT (MMU),A     ; make it so.        
    JP  0000        ; restart CP/M. 
.END