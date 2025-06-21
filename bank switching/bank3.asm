;
;   Title:   bank3.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   20 Jun 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   CP/M 2.2
;            TASM compiler using Z80 mnemonics (-80 option)
;
;   Descr:   Bank Switching demonstration.      
;            This code runs in common memory at C000H
;            To run it, load & run via DDT: 
;            "DDT BANK3.HEX", then "gC000"

MMU     .EQU 40h     ; MMU control port
TPA     .EQU 100H
STACK   .EQU 0C800H  ; program stack


.ORG    0C000h
  
    LD   SP,STACK    ; establish local stack
    LD   HL,st0
    CALL printStr    ; display program title   
    CALL here        ; push PC on stack
here:   
    POP  HL          ; retrieve PC into HL
    CALL printHex16  ; show program counter
;
;  Now try some fun bank switching stuff
;  First, put a page of 4's in bank 4
;
    LD   A,4
    OUT  (MMU),A     ; switch to bank 4 
    LD   HL,TPA      ; point to TPA in b4
    LD   A, 44h      ; and fill it with 4's
    CALL FILL        ; fill a page of memory   
;
;  Show memory in banks 0 and 4
;    
    LD   A,0
    OUT  (MMU),A     ; switch to bank 0     
    LD   HL,TPA      ; point to TPA
    CALL DUMP        ; and show it

    LD   A,4
    OUT  (MMU),A     ; switch to bank 4      
    LD   HL,TPA      ; point to TPA
    CALL DUMP        ; and show it
;
;  restore everything back to normal
;
    LD   A,0
    OUT  (MMU),A     ; switch to bank 0 
    JP   0000        ; return to CP/M via warm boot

st0:    .db  13,10,"W8BH BankTest3, PC=",0


;==========================================================
; FILL -  Fill a page of memory  
;
; Call with: 
;  Reg HL pointing to the memory page
;  Reg A  = fill character   
;
; On Exit:  
;  Reg HL points to end of page
;  Reg DE = HL + 1
;  Reg BC = 0

FILL:
    LD   E,L
    LD   D,H         ; DE = HL
    INC  DE          ; now DE = HL+1
    LD   (HL),A      ; fill char 55h
    LD   BC,100h     ; 256 bytes filled
    LDIR             ; fill the page
    RET

;==========================================================
; DUMP - Show a page (256 bytes) in hexadecimal and ASCII
;        as 16 lines of 16 characters
;
; Call with HL pointing to the sector in memory
; Expects current bank ID at location "BANK"
;
; On Exit:  
;  Reg HL points to end of sector
;  Reg B, DE preserved

DUMP:
   CALL  crlf           ; start a new line
   LD    C, 16          ; print 16 lines (256 bytes total)
d01:
   CALL  printHex16     ; show starting addr of this line
   LD    A, ':'
   CALL  printCh
   LD    A, ' '
   CALL  printCh
   PUSH  HL             ; save line address
   CALL  hexLine        ; print line in hex
   POP   HL             ; restore line address
   CALL  asciiLine      ; print line in ASCII       
   CALL  crlf           ; crlf at end of the line
   DEC   C              ; done with all lines?
   JR    NZ, d01        ; no, so go to next line  
   RET

;==========================================================
; hexLine: print a line of 16 hexadecimal characters
;          pointed to by HL.   Uses regs A,B,HL

hexLine:                ; print 16 bytes as HEX characters
   LD    B, 16          ; put 16 bytes on each line
hl1:
   LD    A, (HL)        ; get next memory byte
   CALL  printHex       ; and print it
   LD    A, ' '         ; put space between bytes
   CALL  printCh        
   INC   HL             ; point to next byte
   DJNZ  hl1            ; loop for all bytes on this line
   RET


;==========================================================
; hexLine: print a line of 16 ASCII haracters
;          pointed to by HL.  Uses regs A,B,HL

asciiLine:              ; print 16 bytes as ASCII characters
   LD    B, 16          ; 16 characters per line
d03:
   LD    A, (HL)        ; get next character
   CALL  isPrintable    ; if not printable,
   JR    NC, d04
   LD    A, '.'         ; replace with a dot '.'
d04: 
   CALL  printCh        ; print the character
   INC   HL             ; advance to next character
   DJNZ  d03            ; and loop until done
   RET


;=====================================================
; isPrintable -  If char in A is a printable character,
;                the carry flag is zeroed.
;                (printable values $20-$7E)
;                Carry flag set if non-printable.
;
; On Entry:
;   Reg A contains an ASCII character
;
; On Exit:
;   All registers preserved.
;   If character is printable, carry flag cleared.  

isPrintable:
   CP   20h             ; if control character,
   RET  C               ; set carry & exit.
   CP   7Eh             ; if above ASCII chars,
   CCF                  ; set carry & exit.
   RET                  ; printable: carry=0


;==========================================================
; printCh - Print Character  (BARE METAL VERSION)
;
; Prints a single character to the console.
; Call with character in Reg A

printCh:
    OUT (2),A           ; Bare metal; no BIOS call
    RET  


;==========================================================
; printStr - Print String
;
; Prints a null-terminated string to the console
; Call with HL pointing to the string
;
; On Exit:
;  Reg A is zero.  
;  Reg HL points to the terminating null character
;  Reg BC, DE preserved.

printStr:
    LD   A, (HL)        ; get character in the string
    OR   A              ; is it zero?
    RET  Z              ; if it is, we are done.
    CALL printCh
    INC  HL             ; move to next character
    JR   printStr       ; and loop until done


;==========================================================
; crlf - Console Carriage Return/Line feed
; 
; Used to terminate current console line and start a new one
; 
; On Exit:
;  Reg A is affected
;  Reg BC, DE, HL are preserved.

crlf:
    LD   A, 13
    CALL printCh
    LD   A, 10
    CALL printCh
    RET


;==========================================================
; printLn - Print String with carriage return
;
; Prints a null-terminated string to the console
; Call with HL pointing to the string
;
; On Exit:
;  Reg A is affected.
;  Reg HL points to the terminating null character
;  Reg BC, DE preserved.

printLn:
    CALL printStr
    CALL crlf
    RET


;=====================================================
; printHex16 -  prints value of Reg HL as four-digit 
;               hexadecimal number.  Leading zeroes are 
;               suppressed.
;
; On Entry:
;   Reg HL contains value to be printed ($0000 - $FFFF)
;
; On Exit:
;   A, BC, DE, HL preserved.

printHex16:
   PUSH AF
   LD   A, H
   CALL printHex        ; print upper 2 digits
   LD   A, L
   CALL printHex        ; print lower 2 digits
   POP  AF
   RET


;=====================================================
; printHex -  prints value of Reg A as two-digit 
;             hexadecimal number.  Leading zeroes are 
;             suppressed.
;
; On Entry:
;   Reg A contains value to be printed ($00 - $FF)
;
; On Exit:
;   Reg A contains ASCII value of lower nibble
;   BC, DE, HL preserved.

printHex:
printHex8:
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
   OR	  $F0
   DAA
   ADD  A, $A0
   ADC  A, $40
   CALL printCh         ; print the converted char
   RET

.END