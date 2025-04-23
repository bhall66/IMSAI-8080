;
;   Title:   talk.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   15 Apr 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler using Z80 mnemonics (-80 option)
;
;   Descr:   Read and write strings to the console
;            using CP/M BDOS function calls.        
; 

BUFLEN  .EQU 80
CPM     .EQU 5

.ORG     100h
START:  LD   DE,STR1
        CALL OUTSTR       ;display STR1
        CALL INSTR        ;get user response
        CALL CRLF
        
        LD   HL,STRLEN    ;point to str length
        LD   A,(HL)       ;get string length
        OR   A            ;is it zero?
        RET  Z            ;if so, done
        
        LD   B,0
        LD   C,A          ;get str Length in BC 
        INC  HL           ;HL points to chars
        ADD  HL,BC        ;point beyond last char     
        LD   (HL),'$'     ;Add $ to string
        
        LD   DE,CHARS
        CALL OUTSTR       ;display response
        LD   DE,STR2
        CALL OUTSTR       ;display Str2
        CALL CRLF         ;skip a line
        JR   START        ;loop
                                                                    

OUTSTR: LD   C,9          ;BDOS 9: String Output
        CALL CPM          
        RET

INSTR:  LD   DE,BUF       ;Get string input
        LD   A,BUFLEN     ;call with buffer size
        LD   (DE),A       ;as first byte in buffer
        LD   C,10         ;BDOS 10: String Input
        CALL CPM          ;actual length is returned
        RET               ;as 2nd byte in buffer

CRLF:   LD   E,13         ;ASCII 13, carriage return 
        LD   C,2          ;BDOS 2 = send character
        CALL CPM
        LD   E,10         ;ASCII 10, line feed
        LD   C,2          ;BDOS 2 = send character
        CALL CPM
        RET


STR1:   .DB "What is your favorite food? $"
STR2:   .DB " is really tasty!",13,10,"$"
BUF:    .DB 0
STRLEN: .DB 0
CHARS   .FILL BUFLEN+1,0
.END