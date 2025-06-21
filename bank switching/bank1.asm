;
;   Title:   bank1.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   20 Jun 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   CP/M 2.2
;            TASM compiler using Z80 mnemonics (-80 option)
;
;   Descr:   First attempt at bank switching     
;            Notice that once bank #2 occupies low memory,
;            The program, located in bank #0, is no longer present.

MMU     .EQU 40H    ; MMU control port

.ORG    100h
    LD  A, 2        ; select memory bank #2
    OUT (MMU),A     ; make it so.
                    ; do stuff in bank #2
    LD  A, 0        ; return to bank #0  (NOPE!)
    OUT (MMU),A     ; make it so.        (NOPE!)
    RET             ; return to CP/M     (NOPE!)
.END