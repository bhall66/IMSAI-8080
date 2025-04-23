;
;   Title:   bhUtils.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   15 Apr 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler, CP/M 2.2 OS
;
;   Descr:   Collection of useful routines for character I/O,
;            math, string conversions, etc.
;            
; Character I/O is through CP/M BDOS functions 1, 2, and 6,
; encapsulated in getCh, printCh, and wait, respectively.
;            
; To use, put "#INCLUDE bhUtils.asm" in your code.
; The following routines are defined in this file:
;
; getCh       - Get Character into Reg A
; printCh     - Print Character in Reg A
; getStr      - Get String into buffer at (HL)
; getStr2     - same as getStr, but uses BDOS fn 10
; getLn       - same as getStr, echoing <CR>
; printStr    - Print String at (HL)
; printLn     - same as printStr, echoing <CR>
; crlf        - Print Carriage Return/Line feed
; delay       - do nothing for 1 second (approx)
; mSdelay     - do nothing for BC milliseconds (approx)
; upCase      - Converts ASCII character to uppercase
; isPrintable - Clears carry flag if Reg A = $20..$7E
; isDigit     - Clears carry flag if Reg A= '0'..'9'
; isAlpha     - Clears carry flag if Reg A= a..z, A..Z
; isHex       - Clears carry flag if Reg A= '0'-9, 'A'-F.
; str2Num     - Convert string of decimal digits at (DE) into HL
; str2Hex     - Convert string of hex digits at (DE) into HL
; printNum    - Print value of A as an unsigned decimal number
; printNum16  - Print value of HL as a unsigned decimal number
; printHex    - Prints value of A as two-digit hexadecimal 
; printHex16  - prints value of HL as four-digit hexadecimal
; wait        - waits for keypress, returns key in A (no echo)


maxLen .equ  80        ; maximum string length
BDOS   .equ  5         ; BDOS entry point @ 0005h 

;==========================================================
; getCh - Get Character
;
; Reads a single character from the console, echoed to screen
;
; On Exit:  
;  Reg A contains the character
;  Reg BC, DE, HL preserved
;

getCh:
	PUSH BC             ; save current registers
    PUSH DE
    PUSH HL
    LD   C, 1           ; BDOS function 1 = CHARACTER INPUT
    CALL BDOS
    POP  HL             ; restore registers after BDOS call
    POP  DE
    POP  BC
    RET


;=====================================================
; wait -        wait for keyboard input 
;               The input is NOT displayed on screen
;
; On Exit:
;   A contains the keyboard character 
;   BC, DE, HL preserved.
;

wait:
   PUSH BC
   PUSH DE
   PUSH HL
wt1:
   LD   C,6            ; CP/M CONSOLE INPUT
   LD   E,0FFh         ; FF= GET INPUT STATUS
   CALL BDOS           ; RETURNS STATUS IN A
   OR   A              ; SET FLAGS
   JR   Z,wt1          ; LOOP UNTIL CHAR READY
   POP  HL
   POP  DE
   POP  BC
   RET

;==========================================================
; printCh - Print Character
;
; Prints a single character to the console
; Call with character in Reg A
;
; On Exit:
;  Reg A is affected.  
;  Reg BC, DE, HL are preserved.
;

printCh:
    PUSH AF
    PUSH BC             ; save current registers
    PUSH DE 
    PUSH HL
    LD   C, 2           ; BDOS function 2 = CHARACTER OUTPUT
    LD   E, A           ; load E with character to be displayed
    CALL BDOS           ; call BDOS
    POP  HL             ; restore registers after BDOS call
    POP  DE
    POP  BC
    POP  AF
    RET  


;==========================================================
; getStr - Get String
;
; Reads characters from console until <enter> key is pressed
; Limit of 80 characters per string.
;
; BDOS function 10 is not used for 2 reasons:
;     1.  it requires a 2 byte prefix on the string
;     2.  it doesn't do null-termination
;
; On Entry:
;  Call with HL pointing to the character buffer
;
; On Exit: 
;  Reg A is zero 
;  Reg B is the string length
;  Reg C, DE preserved
;  Reg HL points to terminating null character
;

getStr:
   LD    B, 0           ; B contains character count
gs0:
   CALL  wait           ; get a character (without echo)
   CP    13             ; if it's <enter> key
   JR    Z, gs1         ; then quit
   CP    8              ; is it <backspace>
   JR    NZ, gs2        ; no, so add it to buffer
   RL    B              
   RR    B              ; is char count 0?
   JR    Z, gs0         ; yes, dont do anything           
   CALL  printCh        ; move cursor back one
   LD    A, ' '         ; and print a space
   CALL  printCh        ; to erase last character
   LD    A, 8           ; now print <backspace> again 
   CALL  printCh        ; to move cursor back
   DEC   HL             ; decrement buffer pointer
   DEC   B              ; decrement character counter
   JR    gs0            ; and get next character
gs2:
   LD    (HL), A        ; save character to buffer
   INC   HL             ; increment buffer pointer
   CALL  printCh        ; echo input to console
   INC   B              ; increment character counter
   LD    A, B
   CP    maxLen-1       ; reached max size yet?
   JR    C, gs0         ; no, so keep going 
gs1:
   XOR   A              ; terminate the string
   LD   (HL), A         ; with null character
   RET                  ; and quit


;==========================================================
; getStr2 - Get String
;
; Reads characters from console until <enter> key is pressed
; Limit of 80 characters per string.
;
; This is an alternative to getStr that uses BDOS fn 10
;
; On Entry:
;  Call with HL pointing to the character buffer
;
; On Exit: 
;  Reg A is zero 
;  Reg BC, DE preserved
;  Reg HL points to terminating null character

getStr2: 
    PUSH BC
    PUSH DE
    PUSH HL           ;Save buffer ptr x 2
    PUSH HL
    PUSH HL           ;copy HL to DE
    POP  DE           
    LD   A,maxLen     ;put buffer size
    LD   (HL),A       ;in first byte in buffer
    LD   C,10         ;BDOS 10: String Input
    CALL BDOS   
    POP  DE           ;DE = buffer ptr      
    POP  HL           ;HL = buffer ptr
    INC  HL           ;point to str length       
    LD   A,(HL)       ;save string length in A
    OR   A            ;0 length?
    JR   Z,gs21        ;yes, were done.
    LD   C,A          ;no, move string
    LD   B,0          ;BC = string length
    INC  HL           ;point to string data
    LDIR              ;move to buffer start
gs21:
    EX   DE,HL        ;point beyond end of str
    LD   (HL),0       ;terminate string with 0
    POP DE
    POP BC
    RET


;==========================================================
; getLn - Get line of String input, echoing carriage return 
;
; Reads characters from console until <enter> key is pressed
; Limit of 80 characters per string.
;
; BDOS function 10 is not used for 2 reasons:
;     1.  it requires a 2 byte prefix on the string
;     2.  it doesn't do null-termination
;
; On Entry:
;  Call with HL pointing to the character buffer
;
; On Exit: 
;  Reg A is affected 
;  Reg B is the string length
;  Reg C, DE preserved
;  Reg HL points to terminating null character
;
getLn:
    CALL getStr
    CALL crlf
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
;

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
;

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
;
printLn:
    CALL printStr
    CALL crlf
    RET


;=====================================================
; upCase  -  Converts ASCII character to uppercase
;
; On Entry:
;   Reg A contains an ASCII character
;
; On Exit:
;   Reg A converted to upper-case ASCII character.
;   For example, 'a' --> 'A'
;   All other registers preserved.

upCase:
   CP   'a'             ; is char <'a'?
   RET  C               ; dont convert
   CP   'z'             ; is char >'z'?
   RET  NC              ; dont convert
   SUB  20h             ; convert to upper case
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
;  

isPrintable:
   CP   20h             ; if control character,
   RET  C               ; set carry & exit.
   CP   7Eh             ; if above ASCII chars,
   CCF                  ; set carry & exit.
   RET                  ; printable: carry=0


;=====================================================
; isDigit -  Clears carry flag if ASCII '0'..'9'
;            Sets carry flag character otherwise.
;
; On Entry:
;   Reg A contains an ASCII character
;
; On Exit:
;   All registers preserved.
;   If character is a hex digit, carry flag cleared.
;  

isDigit:
   CP   '0'             ; is char <'0'?
   RET  C               ; yes, return with carry=1
   CP   '9'+1           ; is char '0'-'9'?
   CCF                  ; yes, return with carry=0
   RET  


;=====================================================
; isAlpha -  Clears carry flag if char is a..z, A..Z
;            Sets carry flag character otherwise.
;
; On Entry:
;   Reg A contains an ASCII character
;
; On Exit:
;   Reg A converted to upper-case ASCII character.
;   All other registers preserved.
;   If character is a hex digit, carry flag cleared.
;  

isAlpha:
   CALL upCase          ; convert lower to upper case
   CP   'A'             ; is char <'A'?
   RET  C               ; yes, return with carry=1
   CP   'Z'+1           ; if char >'Z'?
   CCF                  ; if yes, return with carry=1
   RET      


;=====================================================
; isHex -  Clears carry flag if character is 0-9, A-F.
;          Sets carry flag character is not hex digit.
;
; On Entry:
;   A contains an ASCII character
;
; On Exit:
;   A is converted to upper-case ASCII character.
;   All other registers are preserved.
;   If character is a hex digit, carry flag cleared.
;       

isHex:
   CALL upCase          ; convert a-f to A-F
   CP   '0'             
   RET  C               ; exit if char <'0'
   CP   'F'+1
   CCF
   RET   C              ; exit if char >'F'
   CP   '9'+1
   JR   C, sh1          ; convert '0'-'9'
   CP  'A'              ; if >9 & <'A, C=1
   RET                  ; otherwise, C=0
        

;=====================================================
; str2Num - Convert string of decimal digits
;           to a 16-bit number
;
; On Entry:
;   DE points to null-terminated string
;
; On Exit:
;   HL = 16-bit result
;   A = last character evaluated
;   BC = HL/10
;   DE = pointer to last char evaluated
;
; Any character not '0..9' terminates the conversion.
; For example "123ddf" converts to 123.
;

str2Num:
   LD   HL, 0
gn0:
   LD   A, (DE)         ; get next ASCII charcter
   SUB  30h             ; convert to binary
   CP   10              ; is valid digit <10?
   RET  NC              ; nope, so exit
   INC  DE              ; point to next char

   LD   B, H            ; multiply HL x 10:
   LD   C, L            ; first, copy HL to BC
   ADD  HL, HL          ; HL x 2
   ADD  HL, HL          ; HL x 4
   ADD  HL, BC          ; HL x 5
   ADD  HL, HL          ; HL x 10

   ADD  A, L            ; add current digit to HL
   LD   L, A            ; by adding it to L
   JR   NC, gn0         ; and handling carry
   INC  H               ; into H as needed.
   JR   NC, gn0


;=====================================================
; str2Hex - Convert string of Hexadecimal digits
;           to a 16-bit number
;
; On Entry:
;   DE points to null-terminated hexadecimal string
;
; On Exit:
;   HL = 16-bit value result
;   A = last character evaluated
;   BC preserved
;   DE = pointer to last char evaluated
;
; Any character not 0..9, A-F terminates the conversion.
; For example "F12G" converts to $0F12.
;

str2Hex:
   LD   HL, 0           ; start with 0 result
sh0:
   LD   A, (DE)         ; get next ASCII charcter
   CALL upCase          ; convert a-f to A-F
   CP   '0'             
   RET  C               ; exit if char <'0'
   CP   'F'+1
   RET  NC              ; exit if char >'F'
   CP   '9'+1
   JR   C, sh1          ; convert '0'-'9'
   CP  'A'
   RET  C               ; exit if >9 & <'A'          
   SUB  7               ; for 'A' to 'F': -$37
sh1:                    ; for '0' to '9': -$30
   SUB  30h             ; convert ASCII to binary
   ADD  HL, HL          ; HL x 2
   ADD  HL, HL          ; HL x 4
   ADD  HL, HL          ; HL x 8
   ADD  HL, HL          ; HL x 16
   ADD  A, L            ; add current digit to HL
   LD   L, A
   INC  DE              ; point to next char
   JR   sh0             ; and get next digit


;=====================================================
; printNum - prints value of Reg A as a decimal number
;            leading zeroes are suppressed
;
; On Entry:
;   A contains value to be printed (0-255)
;
; On Exit:
;   A is zeroed.
;   BC, DE, HL preserved.
;

printNum:
printNum8:
   PUSH HL              ; save HL
   LD   L, A            ; copy A into HL
   XOR  A 
   LD   H, A            
   CALL printNum16      ; print number
   POP  HL              ; restore HL
   RET


;=====================================================
; printNum16 - prints value of Reg HL as a decimal 
;              number.  Leading zeroes are suppressed.
;
; On Entry:
;   HL contains value to be printed (0-65535)
;
; On Exit:
;   BC, DE preserved.
;
 
printNum16:
   PUSH BC              ; save BC, DE
   PUSH DE
   LD   D, 0            ; leading zero flag
   LD	BC,-10000       ; count 10000's
   CALL pn1             ; print the digit, if any
   LD	BC,-1000        ; count 1000's
   CALL pn1             ; print the digit, if any
   LD	BC,-100         ; count 100's
   CALL pn1             ; print the digit, if any
   LD   C, -10          ; count tens
   CALL pn1             ; print the digit, if any
   LD   C, B            ; count ones
   CALL pn1             ; print the digit
   POP  DE           
   POP  BC              ; restore BC, DE
   RET

pn1:	
   LD   A,'0'-1         ; A contains digit in ASCII
pn2:	
   INC  A               ; update counter (in ASCII)  
   ADD  HL, BC          ; try adding another unit
   JR   C, pn2          ; no carry yet, so add again
   SBC  HL, BC          ; went to far, so back up 1 unit
   CP   '0'             ; is digit 0?
   JR   NZ, pn3         ; no, so print it right away
   SLA  D               ; check for leading zeroes
   ; was: RL D/RR   D        
   RET  Z               ; don't print a leading 0
pn3:
   INC  D               ; no more leading zeros
   CALL printCh         ; print character
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
;

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
;

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

;=====================================================
; delay  -       do nothing for 1 second
; msDelay -      delay for BC milliseconds 
;
; Assumes CPU running at 4 MHz (0.25uS per Tstate)
;
;   A, DE, HL preserved
;   BC = 0

delay:
   LD   BC,1000
msDelay:
   PUSH AF
   PUSH DE
d1:
   LD   D,200           ; 200*20 = 4K cycles = 1mS @ 4 MHz
d2:                     
   NOP                  ; 4 cycles
   DEC  D               ; + 4 
   JR   NZ,d2           ; + 12 = 20 cycles in inner loop
   DEC  BC              ; count the milliseconds
   LD   A,B             ; DEC BC doesnt set z flag
   OR   C               ; so need LD/OR to do the job
   JR   NZ,d1           ; wait another millisecond
   POP  DE
   POP  AF
   RET
