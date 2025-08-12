;
;   Title:   rtc4.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   10 Aug 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler using Z80 mnemonics (-80 option)
;            
;   Descr:   Real Time Clock (RTC) Tutorial
;            Learn how to determine day/month/year from day count.
;

CLKCMD      .EQU    41h         ; RTC command port
CLKDAT      .EQU    42h         ; RTC data port

            .ORG    100h        ; Run under CP/M

start:      LD   SP,1000h       ; establish local stack
        
            CALL getTime        ; hour/min/sec/days

            ; days = day+1 since 1/1/1978, so that
            ; 1/2/1978 = 2 days, etc.
            ;    
            ; The 'days' value must be broken down into
            ; years, months and days in order to determine
            ; the current date.  The year is calculated first,
            ; then the month, and finally the day              

            ; Now calculate the year 
            ; when finished, DE = current year
            ; and HL = day within current year
            ;
            LD   HL,(days)      ; get days into HL 
            LD   DE,1977        ; start year 1978 (incr below)        
j02:        INC  DE             ; go to the next year
            LD   BC,365         ; normal year = 365 days
            LD   A,E            ; test the current year
            AND  03h            ; is it a leap year? 
            JP   NZ,j01         ; no
            INC  BC             ; yes, so year has 366 days
j01:        SBC  HL,BC          ; subtract days from total
            JP   Z,j03          ; loop until 0
            JP   NC,j02         ; or until negative (too far)
j03:        ADD  HL,BC          ; add back days in year
            LD   (year),DE      ; save year
            LD   (doy),HL       ; save day of year

            ; To prepare for determining the month, we must
            ; first determine the # of days in Febrary
            ; if its a leap year, feb has 29 days, etc
            ;
            LD   A,E            ; test the current year
            AND  03h            ; is it a leap year? 
            LD   A,28           
            JP   NZ,j06         ; no, use normal daycount
            LD   A,29           ; yes: adj feb for leap-yr
j06:        LD   (dim+1),A      ; store feb daycount

            ; Now calculate the current month and day
            ; by subtracting a month's worth of days
            ; from the day-of-the-year value
            ; until it is exhausted, then add back last month
            ;
            LD   DE,dim         ; point to days-in-month array
            XOR  A              ; reg A = month counter
            LD   B,A            ; zero-out MSB of BC 
j04:        INC  A              ; increment month 
            EX   DE,HL          ; now HL = pointer to month array
            LD   C,(HL)         ; BC = days-in-month
            INC  HL             ; advance days-in-month pointer
            EX   DE,HL          ; now HL = day of year 
            SBC  HL,BC          ; try subtracting days in current month
            JP   Z,j05          ; loop until 0
            JP   NC,j04         ; or until negative (too far)
j05:        ADD  HL,BC          ; add back days
            LD   (mon),A        ; save the month
            LD   A,L    
            LD   (day),A        ; save the day 

            ; calculate day of week = days mod 7
            ; too bad we don't have a modulo function!
            ; so we will divide via serial subtraction
            ; and use the remainder.  Quotient not needed.
            ;
            LD   HL,(days)      ; HL = days since 1/1/78
            DEC  HL             ; dont count current day
            LD   DE,7           ; divisor is 7
            XOR  A              ; clear carry for next op
j07:        SBC  HL,DE          ; subtract the divisor
            JR   NC,j07         ; keep subtracting until neg
            ADD  HL,DE          ; undo last subtraction
            LD   A,L            ; remainder now in HL
            LD   (dow),A        ; store remainder

            LD   HL,stTime 
            CALL putStr         ; "The current time is..."

            ; print current local time in AM/PM format
            ; 
            LD   HL,stAM        ; assume AM for now
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

            ; print the day of the week
            ;
            LD   A,(dow)        ; A = day of week
            RLCA                ; A x 2 
            RLCA                ; A x 4
            LD   B,0 
            LD   C,A            ; offset BC = dow x 4
            LD   HL,stDow       ; point to days of week
            ADD  HL,BC          ; add in offset
            CALL putStr         ; print day of week
            LD   A,' '
            CALL putCh          ; followed by space

            ; print the date as month/day/year
            ;
            LD   A,(mon)
            CALL printNum       ; print month
            LD   A,'/'
            CALL putCh
            LD   A,(day) 
            CALL printNum       ; print day 
            LD   A,'/'
            CALL putCh 
            LD   HL,(year) 
            CALL printNum16     ; print year 
            CALL crlf            

fin:        JP   0000           ; return to CP/M

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
; hour, min, sec, and days.  Month/day/year will be calculated later.
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
printNum:   PUSH HL             ; save HL
            LD   L, A           ; copy A into HL
            XOR  A 
            LD   H, A            
            CALL printNum16     ; print number
            POP  HL             ; restore HL
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
printNum16: PUSH BC             ; save BC, DE
            PUSH DE
            LD   D, 0           ; leading zero flag
            LD	BC,-10000       ; count 10000's
            CALL pn1            ; print the digit, if any
            LD	BC,-1000        ; count 1000's
            CALL pn1            ; print the digit, if any
            LD	BC,-100         ; count 100's
            CALL pn1            ; print the digit, if any
            LD   C, -10         ; count tens
            CALL pn1            ; print the digit, if any
            LD   C, B           ; count ones
            CALL pn1            ; print the digit
            POP  DE           
            POP  BC             ; restore BC, DE
            RET

pn1:	    LD   A,'0'-1        ; A contains digit in ASCII
pn2:	    INC  A              ; update counter (in ASCII)  
            ADD  HL, BC         ; try adding another unit
            JR   C, pn2         ; no carry yet, so add again
            SBC  HL, BC         ; went to far, so back up 1 unit
            CP   '0'            ; is digit 0?
            JR   NZ, pn3        ; no, so print it right away
            SLA  D              ; check for leading zeroes      
            RET  Z              ; don't print a leading 0
pn3:        INC  D              ; no more leading zeros
            CALL putCh          ; print character
            RET

days:       .dw 0000  
year:       .dw 0000
doy:        .dw 0000
dow:        .db 00
day:        .db 00
mon:        .db 00
hour:       .db 00
min:        .db 00 
sec:        .db 00 
dim:        .db 31,28,31,30,31,30,31,31,30,31,30,31,0
stTime:     .db "The current time is ",0
stDow:      .db "Sun",0,"Mon",0,"Tue",0,"Wed",0,"Thu",0,"Fri",0,"Sat",0 
stAM:       .db " AM, ",0
stPM:       .db " PM, ",0

.END 