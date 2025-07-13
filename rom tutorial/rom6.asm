;
;   Title:   rom6.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   13 Jul 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler using Z80 mnemonics (-80 option)
;            
;   Descr:   Displays program counter before and after a local jump.
;            It is used to study ROM Banking and Auto-Run behavior.
;            Run this code as a ROM.
  
            
MCP     .EQU    0F3h            ; MPU-B control (banked ROM) port

.ORG    0D800H

start:  LD      SP,0D780h       ; set up stack 
        CALL    initSIO         ; initialize serial port
        LD      HL,st1          
        CALL    putStr          ; print "Address before jump"
        CALL    locn0           ; push PC on stack like a normal call
locn0:  POP     HL              ; get PC into HL
        CALL    pHex16          ; print the address
        CALL    crlf            
        JP      j01             ; jump to next address
j01:    LD      HL,st2          
        CALL    putStr          ; print "Address after jump"
        CALL    locn1           ; push PC on stack like a normal call
locn1:  POP     HL              ; get PC into HL
        CALL    pHex16          ; print the address
        CALL    crlf            
fin:    JP      fin           


;
; Console support routines follow ------------------------------
;

; wait for a character from the console & retrieve it
;
getCh:  IN      A,(3)          ; check status
        AND     2              ; look @ ready bit only
        JP      Z,getCh        ; wait until ready
        IN      A,(2)          ; read waiting character
        RET 

; send a character to console, quick n dirty style.
; really one should check TxRdy status bit before sending.
;
putCh:      OUT  (2),A          ; send char to output devices
            RET

; write a null-terminated ASCII string, pointed to by HL, 
; to the console
;
putStr:     LD      A,(HL)      ; load next char in string      
            OR      A           ; is it end-of-string NULL?
            RET     Z           ; if so, we are done
            CALL    putCh       ; send char to output device
            INC     HL          ; point to next character in string
            JP      putStr      ; and loop until done

; crlf issues a two byte carriage return/line feed combo to output devices
; 
crlf:       LD   A,0DH
            CALL putCh          ; output CR
            LD   A,0AH
            JP   putCh          ; output LF

; pHex prints the byte in A as a two-digit hexadecimal
; for example, value 0A8h is printed as two ASCII characters "A8"
;
pHex:       PUSH AF             ; save value
            RRCA                ; rotate first digit into lower 4 bits
            RRCA    
            RRCA    
            RRCA    
            CALL j07            ; convert first digit to ASCII
            POP  AF             ; restore value & continue with 2nd digit
j07:        AND  0FH            ; consider only lower 4 bits
            ADD  A,90H          ; convert value to ASCII
            DAA                 ; via clever textbook routine
            ADC  A,40H
            DAA     
            JP   putCh          ; output ASCII character

; pHex16 prints the word in HL as a four-digit hexadecimal, followed by a space
; for example, value 0A8EEh is printed as two ASCII characters "A8EE "
;
pHex16:     LD   A,H            ; get MSB
            CALL pHex           ; and output ASCII for it
            LD   A,L            ; then get LSB
pHexSp:     CALL pHex           ; and output ASCII for it
space:      LD   A,20H           
            JP   putCh          ; print a space

; initialize serial port
;
initSIO:    LD   A,0CAh         ; 7 BITS, NO PARITY, 2 STOP
            OUT  (3),A          ; UART A - SET MODE 
            LD   A,027H         ; ENABLE TX & RX
            OUT  (3),A          ; UART A - SET CONTROL
            RET

st1:     .db "Address before jump: ",0
st2:     .db "Address after jump:  ",0

.END