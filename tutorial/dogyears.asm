;
;   Title:   dogyears.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   15 Apr 2025
;      HW:   IMSAI 8080 emulator
;      SW:   CP/M 2.2
;            TASM compiler using Z80 mnemonics (-80 option)
;
;   Descr:   Get age of dog and calculates dog-year age
;            Uses CP/M for character I/O.

.ORG $0100

start:
   CALL  crlf           ; start on new line
   LD    HL, str1       ; "Enter age of your dog: "
   CALL  printStr
   
   LD    HL, buffer     ; point to input string buffer
   CALL  getStr         ; and get string from user
   
   CALL  crlf           ; start on new line
   LD    HL, str2       ; "Your dog is "
   CALL  printStr

   LD    DE, buffer
   CALL  str2Num        ; put user age into HL
   LD    A, L
   OR    A              ; age 0?
   JR    NZ, ok
   LD    HL, str4       ; unsuccessful conversion
   CALL  printStr  
   JP    0000           ; quit via warm boot

ok:  
   LD    B, H           ; multiply age x 7 as follows:
   LD    C, L           ; copy HL to BC
   ADD   HL, HL         ; HL x 2
   ADD   HL, HL         ; HL x 4
   ADD   HL, HL         ; HL x 8
   SBC   HL, BC         ; HL x 7
   CALL  printNum16
   LD    HL, str3       ; " years old!"
   CALL  printStr   
   JR    start          ; do it again.

#INCLUDE "bhUtils.asm"

buffer:
   .FILL 80,0      ; reserve 80 bytes for input string
str1:
   .db " Enter the age of your dog (1-20 years): ",0
str2:
   .db " Your dog is ",0
str3:
   .db " years old!",13,10,0
str4:
   .db "dead.",13,10,0

.END

