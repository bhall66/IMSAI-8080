;
;   Title:   rtc1.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   04 Aug 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler using Z80 mnemonics (-80 option)
;            
;   Descr:   Real Time Clock (RTC) Tutorial
;            Learn how to read & display the time under CP/M 2.2
;

CLKCMD      .EQU    41h         ; RTC command port
CLKDAT      .EQU    42h         ; RTC data port

            .ORG    100h        ; Run under CP/M

start:      LD   SP,1000h       ; establish local stack
            CALL getTime        ; query the RTC
            LD   HL,stTime 
            CALL putStr         ; "The time is "
            CALL pTime24        ; print time in 24-hr format
            CALL crlf               
fin:        JP   0000           ; return to CP/M

; pTime24: print the time in 24-hour format
;
pTime24:    LD   A,(hour)
            CALL pBCD           ; print hours
            LD   A,':'
            CALL putCh 
            LD   A,(min) 
            CALL pBCD           ; print minutes
            LD   A,':'
            CALL putCh 
            LD   A,(sec) 
            CALL pBCD           ; print seconds  
            RET 

; getRTC:
; encapsulates I/O port communication with RTC
; call with RTC parameter# in A
; returns with RTC value in A
;
getRTC:     OUT  (CLKCMD),A     ; send RTC command
            IN   A,(CLKDAT)     ; get RTC value
            RET 

; getTime:
; queries the RTC and saves time variables in memory as
; hour, min, sec, days, month, day, & year
;
getTime:    LD   A,0              
            CALL getRTC         ; RTC 0 = seconds
            LD   (sec),A        ; save seconds           
            LD   A,1
            CALL getRTC         ; RTC 1 = minutes
            LD   (min),A        ; save minutes                       
            LD   A,2
            CALL getRTC         ; RTC 2 = hours
            LD   (hour),A       ; save hours  
            LD   A,3 
            CALL getRTC         ; RTC 3 = days/LSB
            LD   (days),A       ; save days LSB        
            LD   A,4
            CALL getRTC         ; RTC 4 = days/MSB
            LD   (days+1),A     ; save days MSB   
            LD   A,5 
            CALL getRTC         ; RTC 5 = day of month
            LD   (day),A        ; save day   
            LD   A,6
            CALL getRTC         ; RTC 6 = month 
            LD   (mon),A        ; save month 
            LD   A,7 
            CALL getRTC         ; RTC 7 = year 
            LD   (year),A       ; save year
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


; send a character to console.
; really one should check TxRdy status bit before sending.
;
putCh:      OUT  (2),A          ; send char to output devices
            RET


; crlf issues a two byte carriage return/line feed combo to output devices
; 
crlf:       LD   A,0DH
            CALL putCh          ; output CR
            LD   A,0AH
            JP   putCh          ; output LF        


; pBCD prints the BCD byte in A as a two-digit value
; for example, value 38h is printed as two ASCII characters "38"
;
pBCD:       PUSH AF             ; save value
            RRCA                ; rotate first digit into lower 4 bits
            RRCA    
            RRCA    
            RRCA    
            CALL j07            ; convert first digit to ASCII
            POP  AF             ; restore value & continue with 2nd digit
j07:        AND  0FH            ; consider only lower 4 bits
            ADD  A,30H          ; convert value to ASCII  
            JP   putCh          ; output ASCII character


sec:        .db 00
min:        .db 00    
hour:       .db 00
days:       .dw 0000
day:        .db 00
mon:        .db 00
year:       .dw 0000
dow:        .db 00
stTime:     .db "The current time is ",0

.END 