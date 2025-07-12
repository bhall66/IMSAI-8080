;
;   Title:   rom-c.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   12 Jul 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler using Z80 mnemonics (-80 option)
;            
;   Descr:   W8BH Custom Monitor
;            Appears only if there is no disk in Drive A.
;            Supports ROM banking and autoboot.
;
;   Setup:   1. Install hex file of this monitor in SD card
;            2. Create a memory config in system.conf file
;            similar to (or replacing) mpu-a-rom config.
;            
; Commands:  Boot to Disk
;            Call a routine
;            Enter Hex into Memory
;            Input Port
;            Jump to Memory
;            Load Hex file from tape
;            Output Port
;            Save binary to tape
;            Help (show list)

DDA         .EQU    0080h       ; disk descriptor address
MCP         .EQU    0F3h        ; MPU-B control (banked ROM) port
FDC         .EQU    0FDh        ; Floppy disk controller (FIF) port
LED         .EQU    0FFh        ; Output LED port

            .ORG    0D800h      ; ROM location

start:      LD   A,40H           
            OUT  (MCP),A        ; turn off ROM at 0000-07FF
            JP   j00            ; work with 0000 auto-boot.

j00:        LD   SP,0D7F0h      ; establish local stack
            CALL initSIO        ; initialize serial ports 2 & 22h
            CALL readFD         ; try to read disk's boot sector      
            DEC  A              ; success?
            JP   Z,boot         ; disk read OK, so jump to 0000h

            LD   HL,intro      
            CALL putStr         ; print intro message
cmdLoop:    LD   HL,prompt
            CALL putStr         ; print prompt
            CALL waitCh         ; wait for user input 
            PUSH AF 
            CALL crlf           ; start new line
            POP  AF 
            CP   'A'            ; Author cmd?
            JP   Z,goAuthor
            CP   'B'            ; Boot comd?
            JP   Z,goDisk
            CP   'C'            ; Call cmd?
            JP   Z,goCall
            CP   'D'            ; Display cmd?
            JP   Z,goDump 
            CP   'E'            ; Enter cmd?
            JP   Z,goEnter
            CP   'I'            ; IN port cmd?
            JP   Z,goIn
            CP   'J'            ; Jump cmd?
            JP   Z,goJump 
            CP   'L'            ; Load cmd?
            JP   Z,goLoad
            CP   'O'            ; OUT port cmd?
            JP   Z,goOut
            CP   'S'            ; Save cmd?
            JP   Z,goSend
            CP   'W'            ; War cmd?
            JP   Z,goWar
            CP   '?'            ; Help cmd?
            JP   Z,goList
            JP   cmdLoop

boot:       LD   HL,stBoot
            CALL putStr         ; print "Booting from Drive A"
            LD   A,0C0H           
            OUT  (MCP),A        ; turn off ROM at D800-DFFF
            JP   0000h          ; jump to bootstrap code     

goList:     LD   HL,list        ; print command list
            CALL putStr 
            JP   cmdLoop

goAuthor:   LD   HL,stMe 
            CALL putStr         ; show my name
            JP   cmdLoop 

goSend:     LD   HL,stAddr 
            CALL input          ; get starting address
            EX   DE,HL          ; and it in DE
            LD   HL,stCount     
            CALL input          ; get byte count
            EX   DE,HL          ; now HL=addr, DE=count
j16:        LD   A,(HL)         ; get byte from memory
            CALL auxOut         ; and send it to tape
            INC  HL             ; point to next byte
            DEC  DE             ; and reduce count
            LD   A,D 
            OR   E              ; count=0?
            JP   NZ,j16         ; loop until done
            JP   cmdLoop

; Create a 3-byte I/O routine <opcode><port#><RET> & execute it.
; On entry, D has opcode, E has port#, and B has data value.
;
stuffIt:    LD   HL,start       ; stuff bytes in RAM
            DEC  HL             ; in reverse order:
            LD   A,0C9h         ; stuff a RET opcode
            LD   (HL),A         ; at end 
            DEC  HL
            LD   A,E            ; retrieve port#
            LD   (HL),A         ; and stuff it
            DEC  HL 
            LD   A,D            ; retrieve opcode
            LD   (HL),A         ; and stuff it
            LD   A,B            ; put data value in A 
            CALL start-3        ; execute the instruction
            RET 
  
goOut:      LD   HL,stPort      ; "Port:"
            CALL input          ; get port#
            LD   E,L            ; and save it in E
            LD   HL,stData      ; "Data:" 
            CALL input          ; get data value
            LD   B,L            ; and save it in B
            LD   D,0D3h         ; save OUT opcode in D
            CALL stuffIt        ; exec the I/O cmd
            JP   cmdLoop 

goIn:       LD   HL,stPort      ; "Port:"
            CALL input          ; get port#
            LD   E,L            ; and save it in E
            LD   D,0DBh         ; save IN opcode in D
            CALL stuffIt        ; exec the I/O cmd
            LD   B,A            ; save result
            LD   HL,stData
            CALL putStr         ; "Data:"
            LD   A,B            ; retrieve result
            CALL pHex           ; print value in A
            JP   cmdLoop

goCall:     LD   HL,cmdLoop     ; push cmdLoop addr on stack
            PUSH HL             ; so subr RET can go there
goJump:     LD   HL,stAddr      
            CALL input          ; get address to call/jump to
            JP   (HL)           ; and go there.
   
goDisk:     CALL readFD         ; try to read disk's boot sector   
            DEC  A              ; disk read success=1?
            JP   Z,boot         ; yes, so do it!
            LD   HL,noDisk        
            CALL putStr         ; print "No Disk"
            JP   cmdLoop

;  Tape Hex Loader command.  Reads input in Intel Hex format.
;  If a checksum error is found then 'C' is output. 
;  If an invalid character is found then 'T' will be output
;
goLoad:     CALL auxIn          ; get next character from paper tape reader
            SUB  ':'            ; is it a colon ':'?
            JP   NZ,goLoad      ; no, so keep reading until colon found
            LD   D,A            ; start of new line, set checksum to 0
            CALL j13            ; read in next 2 hex digits, convert to number (byte counter)
            JP   Z,cmdLoop      ; if zero, then complete file has been read
            LD   B,E            ; otherwise, result is #byte in this line
            CALL j13            ; read next 2 hex digits, convert to number 
            LD   H,E            ; put result in H
            CALL j13            ; read in 2 hex digits, convert to number 
            LD   L,E            ; put result in L.  Now we have address to store data
            CALL j13            ; read in 2 hex digits, convert to number 
j14:        CALL j13            ; read in 2 hex digits, convert to number (data byte)
            LD   (HL),E         ; put data byte into memory       
            INC  HL             ; go to next memory address
            DEC  B              ; and decrement byte counter       
            JP   NZ,j14         ; loop until entire line is read 
            CALL j13            ; read in 2 hex digits, convert to number (checksum)
            JP   Z,goLoad       ; loop to read next line if checksum is valid
            LD   A,'C'          ; otherwise,
            CALL putCh          ; print 'C' = checksum error
            JP   cmdLoop

; tape loader helper function, which reads in two hex characters
; and converts them to a byte value
;       
j13:        CALL auxIn          ; get next character with processing
            CALL isHex          ; convert ASCII character to hex digit
            JP   C,j15          ; jump if not hex digit
            ADD  A,A            ; first digit obtained. Multiply x 16 = shift left
            ADD  A,A            ; mpy x 4
            ADD  A,A            ; mpy x 8
            ADD  A,A            ; mpy x 16
            LD   E,A            ; store result in E
            CALL auxIn          ; get next character with processing
            CALL isHex          ; convert ASCII character to hex digit
            JP   C,j15          ; jump if not hex digit
            ADD  A,E            ; combine 1st and 2nd digits
            CPL 
            OUT  (LED),A        ; blinkenlights! 
            CPL 
            LD   E,A            ; store result in E
            ADD  A,D            ; add result to reg D
            LD   D,A            ; where a running total kept for checksum
            RET  

j15:        LD   A,'T'
            CALL putCh          ; print error message 'T'
            JP   cmdLoop         

goWar:      LD   HL,noWar       ; no war, please.
            CALL putStr
            JP   cmdLoop

; Allow user to enter hex data to memory, terminated by <enter> or any non-hex character
; 
goEnter:    LD    HL, stAddr    ; "Address: "
            CALL  input         ; get address
            PUSH  HL            ; save starting addr
            EX    DE,HL 
            LD    HL,stData
            CALL  putStr        ; "Data: "
            EX    DE,HL         ; addr in HL 
j02:        EX    DE,HL         ; now addr in DE, value in HL 
            CALL  getHex2       ; get next value
            JP    NZ,j06        ; leave on non-hex input
            PUSH  AF 
            EX    DE,HL         ; now value in DE, addr in HL
            LD    A,E 
            LD    (HL),A        ; put value in memory
            INC   HL
            POP   AF 
            CP    0Dh           ; last char <enter>?
            JP    NZ,j02        ; loop if not
j06:        POP   HL            ; retrieve start addr
            LD    A,L
            AND   0F0h          ; start on 16-byte boundary
            LD    L,A
            CALL  crlf          ; finish the input line
            CALL  dump          ; show memory just changed
            RET 

; similar to getHex, but sets zero flag if input was stopped by either <space> or <Enter>
; Anything else constitues an error and should not be used as data.
;
getHex2:    CALL getHex 
            CP   ' '            ; flag <space> as OK
            RET  Z 
            CP   0Dh            ; flag <enter> as OK
            RET                  

; Display 256-byte memory dump, beginning user-specified address
;
goDump:     CALL  crlf          ; start on new line
            LD    HL,stAddr     ; "Memory Address: "
            CALL  input         ; get hex address
            CALL  crlf          ; new line
dump:       LD    C,16          ; print 16 lines (1 memory page)
d01:        CALL  pHex16        ; print line start address
            LD    B,16          ; put 16 bytes on each line
            PUSH  HL            ; save line address
            CALL  hexLine       ; print line in hex
            POP   HL            ; restore line address
            CALL  asciiLine     ; print line in ASCII       
            CALL  crlf          ; crlf at end of the line
            DEC   C             ; done with all lines?
            JR    NZ,d01        ; no, so go to next line
            JP    cmdLoop         

; print 16 bytes as HEX characters
hexLine:    LD    A, (HL)       ; get next memory byte
            CALL  pHexSp        ; and print it       
            INC   HL            ; point to next byte
            DJNZ  hexLine       ; loop for all bytes on this line
            RET

; print 16 bytes as ASCII characters
asciiLine:              
            LD    B, 16         ; 16 characters per line
d03:        LD    A, (HL)       ; get next character
            CALL  isASCII       ; if not printable,
            JR    NC, d04
            LD    A, '.'        ; replace with a dot '.'
d04:        CALL  putCh         ; print the character
            INC   HL            ; advance to next character
            DJNZ  d03           ; and loop until done
            RET


; Set up a command to read the disk and receive result code 
;
readFD:     LD   HL,DDA         ; HL = disk descriptor address
            LD   DE,cmdStr      ; copy of disk descriptor in scratchpad
            LD   B,7            ; copy 7-byte descriptor to its address               
j03:        EX   DE,HL
            LD   A,(HL)         ; get a byte
            EX   DE,HL
            LD   (HL),A         ; and store it, 8080-style
            INC  HL              
            INC  DE
            DEC  B
            JP   NZ,j03         ; loop until done.

            ; the disk descriptor is now initialized with a command
            ; to read Drive 0/Track 0/Sector 1 to Address 0000.
            ; Now inform controller where that command is located.

            LD   A,10H          ; FDC cmd 10h = set disk descriptor
            OUT  (FDC),A        ; and sent command
            LD   HL,DDA 
            LD   A,L
            OUT  (FDC),A        ; send LSB of disk descriptor
            LD   A,H
            OUT  (FDC),A        ; send MSB of disk descriptor

            ; Tell the FDC to execute the command string

            XOR  A              ; cmd 0 = execute the command string
            OUT  (FDC),A        ; send it

            ; Poll the result byte & return when reponse is received

            LD   HL,DDA+1       ; get disk descriptor address
j04:        LD   A,(HL)         ; look at result code
            OR   A              ; still zero = no response?
            JP   Z,j04          ; wait until response received
            RET  

input:      CALL putStr         ; prompt user
            CALL getHex         ; get response (hex value)
            CALL crlf           ; new line
            RET 

; getHex reads hexadecimal characters from the console and stops when a
; non-hex digit is encountered.  The input could be any number of characters.
; On exit, HL contains the value of the entered value, and A contains non-hex char
; that stopped the routine.  

getHex:     LD   HL,0000h       ; get console input into HL, default 0
j10:        CALL waitCh         ; get next character with processing
            PUSH AF             ; save entered character
            CALL isHex          ; is char a hex digit?
            JP   NC,j09         ; jump if yes
            POP  AF             ; restore entered character in A
            RET                 ; return on non-hex input
j09:        ADD  HL,HL          ; HL x 16 (shift it left one digit)
            ADD  HL,HL 
            ADD  HL,HL 
            ADD  HL,HL
            ADD  A,L             
            LD   L,A            ; add in A
            POP  AF 
            JP   j10 

; isHex returned carry cleared for ASCII characters (A-F,0-9), but set otherwise.
; On exit, A contains value of the character.  For example, reg A=0Ah for "A" input
;
isHex:      SUB  30h            ; is it a control/special char?
            RET  C              ; yes, return with carry set
            CP   0Ah            ; is it a numeric digit?
            JP   C,j08          ; yes, return with carry clear  
            SUB  11h            ; is it ':' thru '@'?
            RET  C              ; yes, return with carry set
            ADD  A,0Ah          ; A->10h; B->11h; etc.
            CP   10h            ; set carry for A-F
j08:        CCF                 ; clears carry for A-F
            RET 

isASCII:    CP   20h            ; if control character,
            RET  C              ; set carry & exit.
            CP   7Eh            ; if above ASCII chars,
            CCF                 ; set carry & exit.
            RET                 ; printable: carry=0

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

; convert ASCII character in A to upper case
;
upCase:     CP   'a'            ; is char <'a'?
            RET  C              ; dont convert
            CP   'z'            ; is char >'z'?
            RET  NC             ; dont convert
            SUB  20h            ; convert to upper case
            RET

; initialize console and paper tape serial ports
;
initSIO:    LD   A,0CAh         ; 7 BITS, NO PARITY, 2 STOP
            OUT  (3),A          ; UART A - SET MODE 
            OUT  (23h),A        ; UART B - SET MODE
            LD   A,027H         ; ENABLE TX & RX
            OUT  (3),A          ; UART A - SET CONTROL
            OUT  (23h),A        ; UART B - SET CONTROL
            RET

; wait for user input and echo to screen, handling end-of-line
;
waitCh:     CALL checkCh        ; char ready?
            JP   Z,waitCh       ; wait until one rec'd in A
            CALL upCase         ; upper case only in monitor
            OUT  (2),A          ; echo to console  
            CP   13h            ; was it <cr>?
            RET  NZ             ; no, leave.
            LD   A,10h          ; yes, also send <lf>
            OUT  (2),A  
            RET 

; check serial port for any user input, handling ctrl-C
;
checkCh:    IN   A,(3)          ; check status
            AND  2              ; look @ ready bit only
            RET  Z              ; leave if nothing
            IN   A,(2)          ; read waiting character
            CP   3              ; if it's a <ctrl> C
            JP   Z,j00          ; then reboot monitor
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

; send a character to the auxillary (paper tape) port
;
auxOut:     PUSH AF             ; save char
j05:        IN   A,(23h)        ; check status
            AND  1              ; look @ TxRdy bit only
            JP   Z,j05          ; wait until ready
            POP  AF             ; retrieve char
            OUT  (22h),A        ; and send to punch
            RET     

; read a character from the auxillary (paper tape) port
;
auxIn:      IN   A,(23h)        ; check status
            AND  2              ; look @ RxRdy bit only
            JP   Z,auxIn        ; wait until char ready
            IN   A,(22h)        ; read the character  
            RET      


intro:      .db 13,10,">> IMSAI 8080 SYSTEM MONITOR 1.0 <<",13,10
            .db 13,10,"Greetings.",13,10
            .db "Shall we play a game? ",13,10,13,10
list:       .db "(B)  Boot to Disk",13,10
            .db "(C)  Call a routine",13,10
            .db "(D)  Display memory",13,10
            .db "(E)  Enter hex values",13,10
            .db "(I)  Input Port",13,10
            .db "(J)  Jump to memory",13,10
            .db "(L)  Load hex file from tape",13,10
            .db "(O)  Output Port",13,10
            .db "(S)  Save binary to tape",13,10
            .db "(W)  Global Thermonuclear War",13,10
            .db "(?)  Show this list again",13,10,0
prompt:     .db 13,10,": ",0
stBoot:     .db 13,10,"Booting from drive A...",13,10,0
noDisk:     .db "No Disk.",0
noWar:      .db "Wouldn't you prefer a good game "
            .db "of chess?",0
stMe        .db "Bruce E. Hall, W8BH",0
stAddr:     .db "Address: ",0
stPort:     .db "Port: ",0
stData:     .db "Data: ",0
stCount:    .db "Size: ",0
cmdStr      .db 21h,00,00,00,01,00,00h

.END