;
;   Title:   rtc5.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   10 Aug 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler using Z80 mnemonics (-80 option)
;            
;   Descr:   Real Time Clock (RTC) Tutorial
;            Learn how to handle time & date under CP/M 2.2
;

CLKCMD      .EQU    41h         ; RTC command port
CLKDAT      .EQU    42h         ; RTC data port

            .ORG    100h        ; Run under CP/M

start:      LD   SP,1000h       ; establish local stack  
            CALL getTime        ; hour/min/sec/days, etc
            LD   HL,stTime 
            CALL putStr         ; "The time is "
            CALL pTime12        ; print time in 12-hr format
            CALL printDOW       ; print day of week
            LD   A,' '
            CALL putCh          ; followed by space
            CALL pDate          ; then print the date
            CALL crlf               
fin:        JP   0000           ; return to CP/M


; print current local time in 12-hr (AM/PM) format
; 
pTime12:    LD   HL,stAM        ; assume AM for now
            LD   A,(hour)       ; get the hour (in BCD)
            CP   12h            ; hour < 12?
            JP   C,j08          ; yes, keep hour as AM
            JP   Z,j09          ; if 12PM, don't change hour
            SUB  12h            ; if hour is 13+, subtract 12
            DAA                 ; adjust result for BCD
j09:        LD   HL,stPM        ; use PM, not AM
j08:        PUSH HL             ; save AM/PM pointer
            CALL pBCD           ; print hours in 12hr format
            LD   A,':'
            CALL putCh 
            LD   A,(min) 
            CALL pBCD           ; print minutes
            LD   A,':'
            CALL putCh 
            LD   A,(sec) 
            CALL pBCD           ; print seconds  
            POP  HL 
            CALL putStr         ; print AM or PM 
            RET 


; print the date as month/day/year
;
pDate:      LD   A,(mon)
            INC  A              ; 0-based to 1-based months
            DAA 
            CALL pBCD           ; print month
            LD   A,'/'
            CALL putCh
            LD   A,(day) 
            CALL pBCD           ; print day 
            LD   A,'/'
            CALL putCh 
            LD   A,(year)
            SUB  0A0h           ; change base 1900 to 2000
            CALL pBCD           ; print year
            RET          

; Calculate the day of the week: 0=Sunday, 1=Monday, etc
;
calcDOW:    LD   HL,(days)      ; HL = days since 1/1/78
            DEC  HL             ; dont count current day
            LD   DE,7           ; divisor is 7
            XOR  A              ; clear carry for next op
jDW:        SBC  HL,DE          ; subtract the divisor
            JR   NC,jDW         ; keep subtracting until neg
            ADD  HL,DE          ; undo last subtraction
            LD   A,L            ; remainder now in HL
            LD   (dow),A        ; store remainder as day of week
            RET 

; Print the day of the week
;
printDOW:   LD   A,(dow)        ; A = day of week
            RLCA                ; A x 2 
            RLCA                ; A x 4
            LD   B,0 
            LD   C,A            ; offset BC = dow x 4
            LD   HL,stDow       ; point to days of week
            ADD  HL,BC          ; add in offset
            CALL putStr         ; print day of week
            RET 


; getRTC:
; encapsulates I/O port communication with RTC
; call with RTC parameter# in A
; returns with RTC value in A
;
getRTC:     OUT  (CLKCMD),A     ; send RTC command
            IN   A,(CLKDAT)     ; get RTC value
            RET 


; forceDec - change RTC return values from default to BCD to decimal
; this routine shown for completeness but not needed here.
;
;forceDec:  IN   A,(CLKCMD)     ; read RTC format (0=BCD,1=decimal)
;           OR   A              ; set flags
;           RET  NZ             ; return if already in decimal
;           LD   A,0FFh         ; must be in BCD format, so
;           OUT  (CLKCMD),A     ; toggle format: BCD->decimal  
;           RET 


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


; write a null-terminated ASCII string, pointed to by HL 
;
putStr:     LD      A,(HL)      ; load next char in string      
            OR      A           ; is it end-of-string NULL?
            RET     Z           ; if so, we are done
            CALL    putCh       ; send char to output device
            INC     HL          ; point to next character in string
            JP      putStr      ; and loop until done


; send a character to console.
; (really one should check TxRdy status bit before sending.)
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
            CALL jBC            ; convert first digit to ASCII
            POP  AF             ; restore value & continue with 2nd digit
jBC:        AND  0FH            ; consider only lower 4 bits
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
stDow:      .db "Sun",0,"Mon",0,"Tue",0,"Wed",0,"Thu",0,"Fri",0,"Sat",0 
stAM:       .db " AM, ",0
stPM:       .db " PM, ",0

.END 