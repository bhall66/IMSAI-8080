;
;   Title:   dump.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   22 Apr 2025
;      HW:   IMSAI 8080esp by TheHighNibble
;      SW:   TASM compiler, CPM 2.2
;
;   Descr:   Memory Dump.
;            Uses CPM calls.


.ORG 100H

start:
   CALL  crlf           ; start on new line
   LD    HL, str1       ; "Memory Address: "
   CALL  printStr  
   LD    HL, buffer     ; point to input string buffer
   CALL  getLn          ; and get string from user  
   LD    DE, buffer
   CALL  str2Hex        ; convert input to hex address
   LD    C, 16          ; print 16 lines (1 memory page)
d01:
   CALL  printHex16     ; print line start address
   LD    A, ':'
   CALL  printCh
   LD    A, ' '
   CALL  printCh
   LD    B, 16          ; put 16 bytes on each line
   PUSH  HL             ; save line address
   CALL  hexLine        ; print line in hex
   POP   HL             ; restore line address
   CALL  asciiLine      ; print line in ASCII       
   CALL  crlf           ; crlf at end of the line
   DEC   C              ; done with all lines?
   JR    NZ, d01        ; no, so go to next line
   JP    0000           ; return to CP/M via warm boot

hexLine:                ; print 16 bytes as HEX characters
   LD    A, (HL)        ; get next memory byte
   CALL  printHex       ; and print it
   LD    A, ' '         ; put space between bytes
   CALL  printCh        
   INC   HL             ; point to next byte
   DJNZ  hexLine        ; loop for all bytes on this line
   RET

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

buffer:
   .FILL 80,0      ; reserve 80 bytes for input string
str1:
   .db " Memory Address: ",0

#INCLUDE "bhUtils.asm"
.END