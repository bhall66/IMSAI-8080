;
;   Title:   mpu-a-rom.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   06 Jul 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler using Z80 mnemonics (-80 option)
;            
;   Descr:   This file represents a hand disassembly of the MPU-A-ROM.hex
;            source file, which is found on the z80pack GitHub repository.
;            
;            All labels, equates, and comments are mine.      
;           
;            Assemble using "TASM -80 -o20 mpu-a-rom.asm"
;            to reproduce the source file.
;

CONIO      .EQU  2              ; console I/O
CONSTAT    .EQU  3              ; console status
MMU        .EQU  0F3h           ; memory management unit port
FDC        .EQU  0FDh           ; floppy disk controller port
WP         .EQU  0FEh           ; memory write protect port

FDCMD      .EQU  0              ; offset for floppy disk unit/command byte
FDRES      .EQU  1              ; offset for floppy disk result code
FDTRK      .EQU  3              ; offset for floppy disk track# (byte)
FDSEC      .EQU  4              ; offset for floppy disk sector# (byte)
FDDMA      .EQU  5              ; offset for floppy disk DMA buffer address

MPAGE      .EQU  0A800h         ; base address for memory (variables)
m01        .EQU  MPAGE + 0E3h   ; scrachpad used for ASCII dump & disk descriptor
m02        .EQU  MPAGE + 0F4h   ; Disk controller type (1=DIO 5.25", 2=DIO 8", 4=FIF)
m03        .EQU  MPAGE + 0F5h   ; video driver init stuff
m04        .EQU  MPAGE + 0F6h   ; input devices (1=UART,2=PARALLEL,4=SYSTEM SERIAL)
m05        .EQU  MPAGE + 0F7h   ; output devices (1=UART,2=PARALLEL,4=SYSTEM SERIAL)
m06        .EQU  MPAGE + 0F8h   ; contains address where direct I/O is done
m07        .EQU  MPAGE + 0FAh   ; direct I/O operations done here (2 byte I/O + return)
m08        .EQU  MPAGE + 0FDh   ; contains character to be output, or FDC cmd
m09        .EQU  MPAGE + 0FEh   ; address of FDC disk descriptor (0080h)

VIOPAGE    .EQU  0F800h         ; VIO ROM address
VIOINIT    .EQU  VIOPAGE        ; VIO initialization address
VIOOUT     .EQU  VIOPAGE + 03h  ; VIO character output address
VIOSTART   .EQU  VIOPAGE + 06h  ; VIO entry point
VIOSIG     .EQU  VIOPAGE + 7FDh ; VIO firmware signature "VIO" 

TIMER0     .EQU  0D100h         ; timer0 address
TIMCTRL    .EQU  0D103h         ; timer control register


    .ORG    0D800H

        LD   A,40H
        OUT  (MMU),A            ; turn off ROM at 0000-07FF
        JP   j01                ; jump vector for start
        JP   getCh              ; jump vector for getCh
        JP   putCh              ; jump vector for putCh
        JP   j04                ; jump vector for memory dump (addr in HL, count in BC)
j01:    XOR  A
        LD   (m03),A 
        OUT  (WP),A             ; unprotect page0 memory where DMA lives
        LD   SP,0D0E3h          ; put SP in MPU-B RAM, above timers?
        LD   HL,0080h           ; store default disk descriptor address
        LD   (m09),HL           ; at m09
        CALL diskOK             ; check disk status (1=disk present)
        DEC  A 
        JP   Z,bootFD           ; disk present, so boot with jump to 0000.
        XOR  A                  ; no disk, so continue loading monitor
        LD   (m05),A            ; init m05 = 0
        LD   A,(m03)
        INC  A 
        JP   Z,j07 
        CALL vidOK              ; is video driver present?
        LD   A,10h              ; will set m05 = 10h
        JP   Z,j09              ; yes, initialize the driver.
j07:    LD   A,0FFh             ; no, to try with WP
        OUT  (WP),A 
        CALL vidOK              ; test again: is video driver present?
        JP   NZ,j10             ; no, jump
        LD   A,0FFh             ; yes, so initialize it
        LD   (m03),A            ; set m03 = FFh
        LD   A,20h
j09:    LD   (m05),A            ; set m05 = 20h
        CALL VIOINIT            ; and initialize the driver.
j10:    LD   HL,m07             ; address where direct I/O done
        LD   (m06),HL           ; store this address in m06
        LD   A,0AEh 
        OUT  (CONSTAT),A        ; send UART mode = AE
        LD   A,27h
        OUT  (CONSTAT),A        ; send UART command = 27h
        CALL testIO             ; find input device(s), determine baud rate
        LD   HL,str4         
        CALL putStr             ; print "IMSAI IEEE MONITOR..." message

wboot:  LD   SP,0D0E3h 
j54:    LD   HL,j54             
        PUSH HL                 ; push monitor restart address on stack
        CALL crlf               ; start a new line
        LD   A,3Fh              
        CALL putCh              ; print monitor command prompt "?"
        CALL getCh              ; and get input character
        LD   HL,cmdTbl          ; points table of monitor commands
j17:    CALL getCh2             ; get next character with processing
        CALL lookup             ; is it a command?
        RET  Z                  ; return if command not found
        CALL space              ; print a space                
        LD   B,1
        JP   (HL)               ; handle the command request
cmd2:   LD   HL,cmdTbl2         ; point to start of 2nd command table
        JP   j17                ; main program loop

;  Determine controller type, and verify disk via a read operation
;  Returning result code in reg A (code 1 = read OK, disk available)
;
diskOK: CALL initDD             ; create a disk descriptor
        LD   HL,m02
        LD   A,(m03)
        INC  A 
        JP   Z,j19
        LD   A,(0E7FDh)         ; load signature byte 1
        SUB  44h                ; is it a 'D'?
        JP   NZ,j19             ; jump if no
        LD   A,(0E7FEh)         ; load signature byte 2
        SUB  49h                ; is it an 'I'?
        JP   Z,j25              ; found it, so continue at E00C below
j19:    LD   A,0F8h 
        OUT  (WP),A             ; unprotect F800h memory block
        LD   A,(0F7FDh)         ; load signature byte 1
        SUB  44h                ; is it a 'D'?
        JP   NZ,j20             ; jump if no.
        LD   A,(0F7FEh)         ; load signature byte 2
        SUB  49h                ; is it a 'I'?
        JP   NZ,j20             ; jump if no.
        LD   A,0FFh
        LD   (m03),A 
        CALL 0F00Ch             ; found it, continue at F00C
        JP   j23
j20:    LD   (HL),4             ; neither of above, so type "4"
        JP   fdExec             ; execute the FD command
j25:    CALL 0E00Ch
j23:    LD   (HL),2             ; store controller type "2"
        CALL fdExec             ; execute the FD command
        RET  P
        LD   HL,m02 
        LD   (HL),1             ; store controller type "1"
        JP   fdExec             ; execute the FD command

bootFD: LD   A,0C0h             
        OUT  (MMU),A            ; remove ROM from memory map
        JP   0000h              ; cold boot time!

; testIO will test for the presence of I/O ports, and determine the proper baud rate
; for the serial port.
;
testIO: IN   A,(14h)
j63:    LD   HL,str5
        CALL putStr             ; print "HIT SPACE BAR" message
        LD   HL,m04             ; point to input device list
        XOR  A                  ; 0 = start with empty list
        LD  (HL),A              ; initializes device list

        ; first, test for a specific UART mode

        CALL j26                ; put UART in synchronous mode (?)
        LD   B,0                ; Reg B will track devices as they are found
        IN   A,(12h)            ; look at serial port 12 input
        OR   A                  ; anything there?
        JP   Z,j27              ; yes, skip to next test
        INC  B                  ; wasn't zero, so flag this mode

        ; the loop below waits for input on serial and parallel ports,
        ; marks which are found, and exits if parallel or system serial.

j27:    CALL parIn              ; look for input on parallel port
        JP   NZ,ppFnd           ; if found, mark as parallel & exit
        CALL sysIn              ; try getting input from serial port 2/3
        JP   NZ,j30             ; if only on system port, mark as device 4 & exit
        LD   A,B 
        OR   A 
        JP   Z,j27              ; loop until device is found
        CALL serIn              ; try getting input from serial port 12/13h
        JP   Z,j27              ; loop if nothing there
j86:    INC  A 
        JP   Z,j27              ; loop if FFh received

        ; Serial port 12h:  Automatically determine the baud rate
        ; When a character is ready on the UART, measure long it takes to receive it

        LD   B,0                ; initialize wait-for-character counter
j75:    CALL serIn              ; look at serial port 12/13h
        JP   Z,j75              ; & wait until char is ready
        INC  B                  ; char received, so count until it is received
        OR   A                  ; has the char been received yet?
        JP   Z,j75              ; loop until char is received

        ; B now contains the # of above loops it took to receieve a character
        ; use Baud Table #1 to translate this to an approximate baud rate

        LD   HL,baudT1-1        ; point to baud rate table #1
j99:    INC  HL
        LD   A,(HL)             ; get a timer byte from table
        OR   A                  ; 0= could not find baud rate
        JP   Z,j63              ; so try again from beginnning
        CP   B                  ; compare table value to loop counter
        INC  HL                 ; point to next value in table
        JP   C,j99              ; loop if its less than loop counter
        LD   A,(HL)             ; otherwise get following byte in table
        PUSH AF                 ; and save it as the correspoding baud rate
        CALL setBaud            ; set system baud rate to A (ex: 48 = 4800 baud)
        LD   B,1                ; mark device "1" present
        CALL j92                ; and add it to input & output bitmaps
        POP  AF                 ; restore table value

        ; finally, print the baud rate that was established.

        LD   H,A                ; and save as baud rate
        LD   L,0
        CP   1                  ; is baud rate 100?
        JP   NZ,j96
        LD   L,10h              ; convert baud "100" to "110"
j96:    CALL pHex16             ; print value as baud rate (ex "9600")
        LD   HL,str7
        JP   putStr             ; print "BAUD SERIAL" message

ppFnd:  LD   B,2                ; Parallel port found: mark it as device '2'
        CALL j92                ; and add it to input & output bitmaps
        AND  30h
        JP   Z,j97
        LD   (HL),A 
j97:    LD   HL,str8
        JP   putStr             ; print "PARALLEL" message

j30:    LD   C,A                ; save input in C
        IN   A,(12h)            ; look at input from serial port 12
        CP   C                  ; if they match,
        JP   Z,j86              ; jump back to input loop
        IN   A,(14h)            ; look at input from parallel port 14
        CP   C                  ; does it match character?
        JP   Z,ppFnd            ; yes, so mark it as parallel & leave
        LD   A,C                ; input is from system serial; retrieve it
        AND  7Fh                ; look at only the lower 7 bits
        CP   20h                ; is the character a ' ' (space)?
        JP   NZ,j27             ; not a space, so return to input loop
        LD   B,4                ; space found, so mark as device '4'

; save the I/O device(s) in B to the input and output bitmaps 
j92:    LD   HL,m04             ; get current input device bits
        LD   A,(HL)             ; into A
        OR   B                  ; add device(s) in B
        LD   (HL),A             ; and save bitmap back in m04
        INC  HL                 ; get output device bits
        LD   A,(HL)             ; into A
        OR   B                  ; and device(s) in B
        LD   (HL),A             ; and save bits back in m05
        RET

; this routine puts UART in synchonous mode at a specific speed
;
j26:    LD   HL,0042h           
        CALL setT0              ; send counter value in HL to timer0 
        CALL resetU             ; reset UART
        LD   A,4Ch              ; cmd byte 4Ch
        OUT  (13h),A            ; send reset cmd to UART          
        LD   A,0FFh             
        OUT  (13h),A 
        OUT  (13h),A 
        LD   A,0B7h             ; command B7h =       
        OUT  (13h),A            ; put UART in synchronous mode
        RET

; Set up serial port UART at ports 12/13h with normal settings
;
initU:  CALL resetU             ; soft reset UART
        LD   A,0AEh             ; baud=1/16 clk; 8 bits; no parity; 1.5 stop
        OUT  (13h),A            ; send command AE
        LD   A,37h              ; reset flags, RTS/DTR true; Rx/Tx enabled
        OUT  (13h),A            ; send command 37
        RET

;  Do a software reset of the serial UART at ports 12/13h
;
resetU: XOR  A                  ; zero out A
        OUT  (13h),A            ; prepare reset to serial UART
        OUT  (13h),A 
        OUT  (13h),A 
        LD   A,40h              ; 40h = reset UART command
        OUT  (13h),A            ; do UART software reset
        RET

; Handle the Change Baud Rate "Z" command
;
cmdZ:   CALL getHex             ; get baud rate from user
        LD   A,H 
        OR   L                  ; is it 0 (or no input)?
        JP   Z,j63              ; do 2 terminal procedure 
        LD   A,H                ; get upper 2 digits of baud rate into A
setBaud:LD   HL,baudT2          ; point to the baud table #2
        PUSH BC 
        CALL lookup             ; find the baud rate & timer0 count 
        POP  BC 
        CALL NZ,setT0           ; send counter value in HL to timer0
        JP   NZ,initU           ; initialize UART in usual manner (8N1.5)
        LD   HL,str6            ; could not find baud rate, so  
        JP   putStr             ; print "Invalid Baud" message

; The following routine sets up timer0 with the count value in HL
;
setT0:  LD   DE,TIMCTRL         ; DE = timer mode address
        EX   DE,HL 
        LD   (HL),36h           ; set mode=36h (timer0, Read LSB/MSB, Mode 3, Binary)
        LD   HL,TIMER0          ; D100 = timer0 address
        LD   (HL),E             ; store LSB first
        LD   (HL),D             ; then store MSB
        RET 

; Handle the Jump Memory "J" command
;
cmdJ:   POP  DE                 ; remove return address from stack

; Handle the Call Routine "C" command
;
cmdC:   CALL getHex             ; get address from user
        JP   (HL)               ; and go there!

; Handle the Kill ROM & Jump "K" command
;
cmdK:   CALL getHex             ; get address from user
        LD   A,0C0h
        OUT  (MMU),A            ; remove ROM from memory map
        JP   (HL)               ; and jump to (HL)!

; Handle the Kill ROM & quit to VIO "Q" command
;
cmdQ:   LD   A,(m05)            ; look at output locations
        AND  30h                ; if bits 4 & 5 are zero,
        RET  Z                  ; return
        LD   A,(m03)            ; get contents of m03
        INC  A 
        JP   NZ,j49             ; if it FF?
        LD   A,0FFh             ; if yes, fiddle with WP first
        OUT  (WP),A 
j49:    LD   A,0C0h             
        OUT  (MMU),A            ; remove ROM from memory map 
        JP   VIOSTART           ; and jump to video driver

; Handle the Examine memory "E" command
;
cmdE:   CALL getHex             ; get address from user
j61:    CALL crlf
        CALL pHex16             ; print address + space 
        CALL j62                ; print byte, prompt for new value
        DEC  HL                 ; point to previous address
        CP   0Ah                ; if user pressed <enter>
        RET  Z                  ; then quit
        CP   02Dh               ; if user pressed "-"
        JP   Z,j61              ; then show value at previous addr
        INC  HL                 ; otherwise to go next address
        INC  HL 
        JP   j61                ; and show it

; the following routine shows hex value at HL and allows user to change it.
;
j62:    LD   A,(HL)             ; get memory byte
        LD   E,A                ; save value in E
        CALL pHexSp             ; print it in hex, followed by space
        EX   DE,HL              ; temp save HL
        CALL j45                ; get new value into L
        EX   DE,HL              ; restore HL, new value now in E
        LD   (HL),E             ; put value back in HL
        RET 

; Handle the Display/Dump memory "D" command:
; parameters are start address (HL) and optional end address in DE
; if no end address, just one byte is printed.
; after 24 lines print, it will wait for keypress before continuing


cmdD:   CALL param2             ; get start addr in HL and count in BC
j04:    LD   E,18h              ; E = max line counter
        PUSH HL                 ; start start addr on stack
j81:    LD   D,E                ; reset D to line counter

        ; printing of a line of byte values starts here
        ; first job is to clear scratchpad area for ASCII characters

j80:    PUSH DE                 ; save current line counter on stack
        LD   D,11h              ; set up a counter for 17 bytes   
        LD   HL,m02             ; pointing to space between m01 and m02
        XOR  A                  ; A=0
j60:    DEC  HL                 ; going backwards
        LD   (HL),A             ; clear ASCII char scratchpad area
        DEC  D                  
        JP   NZ,j60             ; after loop done, HL points to m01 scratchpad area
        POP  DE                 ; restore line counter
        EX   (SP),HL            ; swap start address and ASCII store area

        ; the line begins with printing the memory address

        CALL crlf               ; start a new line
        CALL pHex16             ; with address shown
        CALL space              ; followed by a space 

        ; printing of each hex value in the line starts here

j84:    CALL getCh              ; does user want to stop output?
        JP   NZ,j79             ; yep, so quit
        LD   A,(HL)             ; get byte at current address
        CALL pHexSp             ; and print hex digits + space
        LD   A,(HL)             ; reload character
        INC  HL                 ; increment the current address

        ; before going to the next byte, store an ASCII representation of the value 
        ; in the scratchpad area, replacing nonprintable values with '.'

        EX   (SP),HL            ; swap in scrathpad address
        LD   (HL),A             ; store byte value as potential character
        CP   20h                ; is value a control character?
        JP   C,j82              ; yes, so replace with '.'
        CP   7Fh                ; is value above ASCII?
        JP   C,j83              ; if not, it a printable character
j82:    LD   (HL),2Eh           ; replace nonprintable char with '.'
j83:    INC  HL                 ; go to next location in ASCII string
        DEC  BC                 ; decrement byte counter
        LD   A,B 
        OR   C                  ; is it zero (all done)?
        JP   Z,j78              ; exit if done

        ; Evaluate if there are more bytes to show in this line

        EX   (SP),HL            ; swap current addr back in
        LD   A,L                ; look at lower byte
        AND  0Fh                ; are we at xxx0?
        JP   NZ,j84             ; if not loop

        ; All 16 bytes have been printed as hex values, so print the ASCII string

        EX   (SP),HL            ; swap addr back out
j78:    CALL space              ; print a space  
        LD   HL,m01             ; load address of ASCII string
        CALL putStr             ; and print it

        ; Evaluate if there are more lines to print, and loop accordingly

        LD   A,B 
        OR   C                  ; if byte counter is zero,
        JP   Z,j79              ; then quit
        DEC  D                  ; decrement line counter
        JP   NZ,j80             ; do next line if still space on screen
        CALL getCh1             ; have shown 24 lines, so wait for user
        CALL crlf               ; start next line
        JP   j81                ; with reset of line counter
j79:    POP  HL                 ; get rid of start addr pushed on stack
        JP   crlf 

;  Tape Loader: Read input in Intel Hex format.  If a checksum error is found
;  then C will be output.  If an invalid character is found then 'T' will be output
;  and the operation will be terminated.

cmdL:   CALL getCh2             ; get next character with processing
        SUB  3Ah                ; is it a ":"?
        JP   NZ,cmdL            ; no, so keep reading
        LD   D,A                ; start of new line, set checksum counter to 0
        CALL j41                ; read next 2 hex digits, convert to number (byte counter)
        RET  Z                  ; if zero, then complete file had been read
        LD   B,E                ; otherwise, result is #byte in this line
        CALL j41                ; read next 2 hex digits, convert to number 
        LD   H,E                ; put result in H
        CALL j41                ; read in 2 hex digits, convert to number 
        LD   L,E                ; put result in L.  Now we have address to store data
        CALL j41                ; read in 2 hex digits, convert to number
j42:    CALL j41                ; read in 2 hex digits, convert to number (data byte)
        LD   (HL),E             ; put data byte into memory pointed
        INC  HL                 ; go to next memory address
        DEC  B                  ; decrement byte counter
        JP   NZ,j42             ; loop until entire line read
        CALL j41                ; read in 2 hex digits, convert to number (checksum)
        JP   Z,cmdL             ; loop to read next line if checksum is OK
        LD   A,43h              ; otherwise, 
        JP   putCh              ; print "C" = checksum error

j41:    CALL getCh2             ; get next character with processing
        CALL isHex              ; convert ASCII to hex digit
        JP   C,j40              ; jump is not hex
        ADD  A,A                ; first digit, so muliply x 16.
        ADD  A,A 
        ADD  A,A
        ADD  A,A 
        LD   E,A                ; store result in E
        CALL getCh2             ; get next character with processing
        CALL isHex              ; is char a hex digit? (NC=true)
        JP   C,j40              ; if not, jump.
        ADD  A,E                ; combine first and second digits
        LD   E,A                ; store result in E
        ADD  A,D                ; add result to reg D
        LD   D,A                ; where a running total is kept
        RET
j40:    LD   A,54h 
        CALL putCh              ; print "T" = invalid character received
        JP   j54                ; restart monitor

; Handle the Test Memory "T" command
;
cmdT:   CALL param2             ; get start addr in HL and count in BC
        DEC  BC 
j90:    XOR  A                  ; start with byte pattern = 00
        LD   D,(HL)             ; save actual byte in D
j91:    LD   (HL),A             ; store pattern to memory
        CP   (HL)               ; and read it back.  Same?
        JP   NZ,j64             ; if not, show address & contents
        DEC  A                  ; go to next byte pattern
        JP   NZ,j91             ; and loop through all patterns
        LD   (HL),D             ; restore original byte to memory
        CALL getCh              ; check to see if user wants to quit
        RET  NZ                 ; quit now if so.
        INC  HL                 ; go to next memory location
        DEC  BC                 ; and decrement the location counter
        LD   A,B 
        OR   C                  ; finished with all addresses (BC=0)?
        JP   NZ,j90             ; no, so go to next address to test
        RET

j64:    INC  HL 
        LD   E,A                ; save current byte pattern
        CALL showAB             ; display an address (HL-1) and its byte contents
        LD   A,E                ; restore current byte pattern
        JP   j93                ; show expected byte pattern

; Handle the Move Memory "M" command
; call with <start addr>,<end addr>,<destination addr>
;
cmdM:   CALL param3             ; load parameters into HL, DE, and BC
j56:    JP   memcpy

; Handle the Fill Memory "F" command
; call with <start addr>, <end addr>, <fill character>
;
cmdF:   CALL param3             ; load parameters into HL, DE, and BC
        LD   A,E                ; fill character in DE moved to A
        LD   (HL),A             ; load first one to start address
        DEC  BC                 ; count one done.
        LD   D,H                ; DE = HL
        LD   E,L 
        INC  DE                 ; set destination = start + 1
        JP   j56                ; and do memory copy

; Handle the Input Port "I" command
;
cmdI:   DEC  B                  ; falls through to next routine

; Handle the Output Port "O" command
;
cmdO:   CALL getHD              ; get port# in HL, 2nd in DE
        LD   A,B                ; A=0 for IN, 1 for OUT
        RLCA                    ; 3 rotates set up A to contain I/O opcode
        RLCA
        RLCA                    ; now A=0 for IN, 08 for OUT
        XOR  0DBh               ; now A=DB for IN, D3 for OUT
        LD   D,L                ; put port# in D
        LD   HL,(m06)           ; get address for direct I/O
        LD   (HL),A             ; store instruction (IN or OUT)
        CP   (HL)               ; did it stick?
        RET  NZ                 ; nope, so cant use this memory
        PUSH HL                 ; save this address
        INC  HL 
        LD   (HL),D             ; after IN/OUT, add add Port#
        INC  HL 
        LD   (HL),0C9h          ; after port#, add a RET instruction
        LD   SP,j52             ; put return address on stack?
        EX   (SP),HL 
        LD   A,B 
        OR   A 
        LD   A,E 
        JP  (HL)                ; execute the I/O instruction
j52:    JP  Z,pHexSp
        RET 

; Handle the Change direct I/O address "Y" command
;
cmdY:   CALL getHex             ; get address from user into HL
        LD   A,H 
        OR   L                  ; is HL=0?
        JP   NZ,j53             ; no, use the user input
        LD   HL,m07             ; yes, reset to original location m07
j53:    LD   (m06),HL           ; put new address into m06
        RET 

; Handle the Verify memory "V" command. 
; The parameters are start (HL), destination (DE), and count (BC)  
;
cmdV:   CALL param3             ; load parameters into HL, DE, and BC
j74:    LD   A,(DE)             ; A = destination byte
        CP   (HL)               ; is it the same as start byte?
        INC  HL                 ; go to next start byte
        INC  DE                 ; go to next dest byte
        JP   Z,j73              ; if same, continue the verify
        CALL showAB             ; show source address and byte contents
        EX   DE,HL              ; flip to DE
        CALL j47                ; show destination address and its contents
        EX   DE,HL              ; flip back
j73:    DEC  BC                 ; decrement counter
        LD   A,B
        OR   C                  ; is count 0 yet?
        RET  Z                  ; quit if it is
        CALL getCh              ; check for keyboard input
        RET  NZ                 ; allow user to stop execution
        JP   j74                ; otherwise, keep going!

; Handle Search Memory "S" command
; Parameters are start addr (HL), value to find (DE), byte counter (BC), optional mask (SP)
;
cmdS:   CALL param3             ; load parameters into HL, DE, and BC 
        PUSH HL                 ; put start address on stack
        LD   HL,0FFFFh          ; assume full 16-bit compare (mask FFFF)
        CP   0Ah                ; if no 4th parameter 
        CALL NZ,getHex          ; otherwise, get mask into HL 
        EX   (SP),HL            ; swap mask and start addr.
j77:    LD   A,(HL)             ; grab byte at current address
        EX   (SP),HL            ; swap mask into HL
        AND  H                  ; apply mask to current byte
        CP   D                  ; do compare with search value
        EX   (SP),HL            ; swap mask out
        INC  HL                 ; point to next byte to check
        JP   NZ,j76             ; no match, so continue search
        LD   A,(HL)             ; MSB matches, so check LSB now
        EX   (SP),HL            ; swap in mask
        AND  L                  ; apply mask to LSB
        CP   E                  ; and compare it to seach LSB
        EX   (SP),HL            ; swap make back out
        CALL Z,showAW           ; if match, show address (HL-1) and word contents
j76:    DEC  BC                 ; decrement counter
        LD   A,B                ; check the byte counter
        OR   C                  ; is it zero?
        JP   NZ,j77             ; not done yet, grab next byte
        POP  BC                 ; get rid of mask pushed on stack
        RET

; Copy Memory from source (HL) to destination (DE)
; BC = number of bytes to copy
;
memcpy: LD   A,B 
        OR   C                  ; is BC=0 yet?
        RET  Z                  ; if so, we are done.
        LD   A,(HL)             ; get byte from source address
        LD   (DE),A             ; and copy to destination address
        DEC  BC                 ; decrement counter
        INC  HL                 ; go to next source addr
        INC  DE                 ; go to next destination addr
        JP   memcpy             ; loop until done.

; Handle the Boot to Floppy "B" command
;
cmdB:   CALL crlf               
        CALL diskOK             ; check disk, get return code
        DEC  A                  ; status=1 is success.
        JP   Z,bootFD           ; disk found, so boot with jump to 0000.

; Handle a floppy disk error by printing the result code
;
fdErr:  AND  0F0h               ; look at upper 4 bits of disk status 
        LD   HL,st2
        CP   0A0h               ; if status = A1h
        JP   Z,putStr           ; print "NOT READY" message
        LD   HL,st3             ; otherwise,
        CALL putStr             ; print "DISK ERR" message
        LD   HL,(m09)           ; point to disk descriptor
        INC  HL                 ; result code is in 2nd byte
        LD   A,(HL)             ; get the disk error #
        JP   pHex               ; and print it

st2:    .db "NOT READY",0
st3:    .db "DISK ERR- ",0 


; initDD: initializes 9 bytes FDC disc descriptor at A8E3-A8EB.
; first byte contains command (2=read) in upper nibble and
; unit (1=drive0, 2=drive1, 4=drive2, 8=drive3) in lower nibble
;  21 00 00 00 01 00 00 00 00
;
initDD: LD   HL,m01+8            ; zero out a block of 9 bytes         
        LD   B,8
j70:    DEC  HL
        LD   (HL),0
        DEC  B                  
        JP   NZ,j70             
        LD   (HL),21h           ; store 21 (Read Drive #0) as FDCMD
        LD   A,1 
        LD   (m01+FDSEC),A      ; store 01 (Sector 1) as FDSEC
        RET 

; Handle the "N" command
; This command is not described in the VIO manual
; It lets the user change the location of the disc descriptor, normally at 0080h.
;
cmdN:   CALL getHex             ; get address from user 
        LD   A,H 
        OR   L                  ; is it zero?
        JP   NZ,j69             ; no, use it
        LD   L,80h              ; yes, use default value of 0080h.
j69:    LD   (m09),HL           ; and store value at m09
        RET 

; fdExec: copy DD to its destination, then issue FDC commands to recognize DD, 
; execute the disk operation, and receive the result code.
;
fdExec: LD   HL,(m09)           ; HL = disk descriptor address
        LD   DE,m01             ; copy of disk descriptor in scratchpad
        LD   B,7                ; copies descriptor to its address
j85:    EX   DE,HL 
        LD   A,(HL)             ; get a byte
        EX   DE,HL
        LD   (HL),A             ; and store it
        INC  HL 
        INC  DE 
        DEC  B 
        JP   NZ,j85             ; loop until done.
        LD   A,(m03)            ; checks video stuff
        INC  A 
        JP   NZ,j65
        LD   A,0F8h 
        OUT  (WP),A 
j65:    LD   A,10h              ; FDC cmd 10h = set disk descriptor
        CALL fdSend             ; send it
        LD   HL,(m09)           ; get descriptor address (usually 0080)
        LD   A,L                
        CALL fdSend             ; send LSB first
        LD   A,H 
        CALL fdSend             ; thn send MSB
        XOR  A                  ; FDC cmd 0h = disk I/O
        CALL fdSend             ; send it

        ; now that a command has been sent to the controller, poll the result
        ; byte for a response.  Return as soon as a response is received, but
        ; time out after 128K attempts.
        
        LD   DE,0000h           ; DE & B used as time-out counter
        LD   B,2
        LD   HL,(m09)           ; point to disk descriptor
        INC  HL                 ; now point to FDC result code
j88:    LD   A,(HL)             ; get result code from FDC
        OR   A                  ; is it zero? 
        RET  NZ                 ; return when FDC returns a results
        DEC  D 
        JP   NZ,j88             ; wait loop x 256 reads
        DEC  E 
        JP   NZ,j88             ; wait loop x 64K
        DEC  B 
        JP   NZ,j88             ; wait loop x 128K
        RET                     ; don't wait forever!

; Handle Read from Diskette "R" command
;
cmdR:   CALL initDD             ; initialize disc descriptor with Read on Disk 0
        JP   diskIO             ; do it

; Handle Write to Diskette "W" command
; call with <track>,<sector>,<buffer addr>,<Unit#>  (last 2 optional)       
;
cmdW:   CALL initDD             ; initialize disc descriptor at A8E3-A8EB
        LD   (HL),11h           ; change command byte to 11h = Write on Disk 0.

diskIO: CALL getHD              ; get 1st param in HL, 2nd in DE
        LD   (m08),A            ; temp save last char
        LD   A,L 
        OR   E                  ; was it zero/invalid?
        JP   Z,err1             ; return with "INVALID" message
        LD   (m01+FDTRK),HL     ; store first param as track #
        LD   A,E 
        LD   (m01+FDSEC),A      ; store 2nd param as sector #
        LD   A,(m08)            ; retrieve last char
        CP   0Ah                ; was it <enter>?
        JP   Z,j71              ; use default DMA & unit
        CALL getHD              ; otherwise get DMA in HL, Unit in DE
        LD   (m01+FDDMA),HL     ; store DMA buffer address 

        ; The Drive# (0-3) must be converted into a bit pattern.
        ; For drive 0, bit 0 is set, etc.

        LD   A,E                ; get unit #
        AND  03h                ; must be 0-3
        INC  A                  ; convert 0-3 to 1-4
        LD   E,A                ; use this as a rotation counter
        XOR  A 
        SCF                     ; start with A=0 plus carry set
j72:    RLA                     ; rotate carry to bit0 
        DEC  E 
        JP   NZ,j72             ; set drive bit by rotating

        ; The first byte in the disc descriptor contains the disk command & unit info.
        ; the upper 4 bits are the command and the lower 4 bits are the unit.
        ; add the new drive info to the existing command byte 

        LD   E,A                ; copy drive info to E
        LD   HL,m01+FDCMD       ; address for command/unit byte
        LD   A,(HL)             ; get old drive command/unit value
        AND  0F0h               ; mask out unit
        OR   E                  ; add in new unit
        LD  (HL),A              ; and save new command/unit 

        ; Now the disc descriptor contains the command &  user input parameters

j71:    CALL fdExec             ; send command to FC controller & get result
        DEC  A                  ; was result code 1=success?
        RET  Z                  ; return if so with A=0
        JP   fdErr              ; otherwise print the disk error code

; fdSend: send a byte to the Floppy Disk Controller
;
fdSend: LD   (m08),A            ; temp store byte to send
        LD   A,(m02)            ; get controller type
        CP   04h                ; is it type 4?
        JP   Z,j48              ; yes, execute FDC command in m08
        SUB  01h                ; otherwise continue below.
        RLCA 
        RLCA
        PUSH HL                 ; save HL on stack
        PUSH DE                 ; save DE on stack
        LD   D,A 
        LD   A,(m03)
        INC  A 
        RLCA 
        ADD  A,D                ; A provides index into table
        LD   HL,fddat           ; point to FD controller address table
        CALL j89                ; adds A to HL
        LD   E,(HL)             
        INC  HL 
        LD   D,(HL)             ; load controller address into DE
        EX   DE,HL              ; put it back into HL
        POP  DE                 ; restore original DE
        EX   (SP),HL            ; restore original HL, put controller address on stack
        LD   A,(m08)            ; restore byte back into A
        RET                     ; jump to controller address

; Table of floppy disk controller (DIO) entry points
fddat:  .dw 0F009h, 0E009h, 0F006h, 0E006h       

; This is the only call to the FDC port
; There are only two commands:  10h to set up disk descriptor
; and 00h to execute the descriptor
;
j48:    LD   A,(m08)            ; get FDC command in m08
        OUT  (FDC),A            ; and sent it to floppy disk (FIF) controller
        RET 

; This might be a bit of orphaned code, obsoleted by fdSend above.  
; Nothing jumps here.
        CP   02h                
        LD   A,(m08)            ; get the disk command
        JP   Z,0E006h           ; and jump to this controller
        JP   0E009h             ; or this one

err1:   LD   HL,st1 
        JP   putStr             ; print "INVALID" message     
st1:    .db "INVALID",0

cmdD2:  LD   HL,m02             ; show FD type and allow user to change it
        JP   j62

cmdI2:  LD   HL,m04             ; show input device bitmap & allow user to change
        JP   j62

cmdO2:  LD   HL,m05             ; show output device bitmap & allow user to change
        JP   j62 

; Param2 gets <start address> in HL and <end address> in DE
; Then calculates the number of bytes between them as BC = ED - HL + 1
        
param2: CALL j67                ; get up to 2 parameters in HL,DE
        PUSH AF                 ; save A
        LD   A,E                ; do 16-bit math (BC=ED-HL) as two 8-bit ops               
        SUB  L                  
        LD   C,A                ; now C = E - L
        LD   A,D 
        SBC  A,H 
        LD   B,A                ; now B = D - H
        INC  BC                 ; add one
        POP  AF                 ; restore A
        RET

; This is a helper function for param2.  It gets start and end addresses from the user,
; which can be separated by either a comma or space, and puts results into HL and DE.

j67:    CALL getHex             ; get console input into HL 
        LD   D,H 
        LD   E,L                ; save to DE
j68:    CP   0Ah                ; was input just <enter>?
        RET  Z                  ; return if so.
        CP   20h                ; was last char a space ' '?
        JP   Z,j66
        CP   2Ch                ; was last char a comma ','?
        RET  NZ 
j66:    EX   DE,HL              ; if space or comma then 
        CALL getHex             ; get second hex input
        EX   DE,HL              ; 1st input in HL, 2nd input in DE
        RET 

; Param 3 gets three parameters from user: start address, end address, and X.
; At the end, HL contains start address, DE contains X, and BC contains byte count.

param3: CALL param2             ; get start addr in HL and count in BC
        JP   j66                ; gets 3rd parameter in DE


; Next routine gets one parameter into HL, and a second optional parameter in DE.
;
getHD:  CALL getHex             ; get console input into HL 
        LD   DE,0000h 
        JP   j68                ; get optional 2nd parameter

; Check to see if VIO firmware is present.
; The firmware signature is "VIO".  Zero flag set if firmware present.
;
vidOK:  LD   HL,VIOSIG          ; point to VIO signature in firmware
        LD   A,56h         
        CP   (HL)               ; is it a 'V'?
        RET  NZ                 ; return if not
        INC  HL                 ; go to next byte
        LD   A,49h              ; is it an 'I'?
        CP   (HL)               ; return with Z flag set
        RET 

j89:    ADD  A,L                ; small routine to add A to HL
        LD   L,A 
        RET  NC 
        INC  H 
        RET 

; Search the command table
; A = command that is being searched for
; on return, zero flag set if not found
; otherwise, HL contains address of the command

lookup: LD   B,A                ; save command in B
j59:    LD   A,(HL)             ; get next command
        OR   A                  ; zero = end of table
        RET  Z                  ; quit if end of table
        CP   B                  ; command match?
        INC  HL                 ; point to addr LSB
        JP   Z,j58              ; jump if found
        INC  HL                 ; point to next table entry
        INC  HL 
        JP   j59                ; and keep searching 
j58:    LD   B,(HL)             
        INC  HL                 ; point to addr MSB
        LD   H,(HL)             ; load MSB into H
        LD   L,B                ; load LSB into L
        OR   A                  ; clears Z flag 
        RET 

; SERIAL PORT (12/13) INPUT 
serIn:  IN   A,(13h)            ; check status port 13h
        AND  02h                ; look at ready bit
        RET  Z                  ; return if not ready
        IN   A,(12h)            ; ready, so get char from port 12h
        RET                    

; SYSTEM SERIAL PORT (2/3) INPUT 
sysIn:  IN   A,(CONSTAT)        ; check serial uart status
        AND  02h                ; look at Rx-ready bit only
        RET  Z                  ; returns 0 when not ready
        IN   A,(CONIO)          ; otherwise get character into reg A
        RET                     

; PARALLEL PORT (14/15) INPUT
parIn:  IN   A,(15h)            ; get status byte from port 15h
        AND  02h                ; look at only bit 1
        RET  Z                  ; not ready
        IN   A,(14h)            ; read byte from port 14h
        RET 

; SERIAL PORT (12/13) OUTPUT       
serOut: IN   A,(13h)            ; get status byte
        AND  01h                ; look only at bit 0
        JP   Z,serOut
        LD   A,(m08)            ; get character to send
        OUT  (12h),A            ; and output to port 12h
        RET 

; SYSTEM SERIAL PORT (2/3) OUTPUT  
sysOut: IN   A,(CONSTAT)        ; check serial status
        AND  01h                ; look at Tx-ready bit only
        JP   Z,sysOut           ; wait until ready
        LD   A,(m08)            ; get character to transmit
        OUT  (CONIO),A          ; and transmit it
        RET

; PARALLEL IN/ SYSTEM SERIAL OUT
piso:   IN   A,(15h)            ; check status port 15h
        AND  01h                ; and look at ready bit
        JP   Z,sysOut 
        LD   A,(m08)            ; get char to transmit
        OUT  (CONIO),A          ; and transmit it to console
        RET 

getCh1: CALL getCh              ; get character, waiting for input
        JP   Z,getCh1           ; loop until character is received.
        RET 

getCh:  LD   A,(m04)            ; m04 controls where to look for input
        AND  02h                ; if bit 1 set,
        CALL NZ,parIn           ; look for input from parallel port 14/15h
        RET  NZ                 ; return if something found
        LD   A,(m04)            ; otherwise keep looking     
        AND  04h                ; if bit 2 set,     
        CALL NZ,sysIn           ; try getting char from serial port 2/3h
        RET  NZ                 ; return if something found
        LD   A,(m04)            ; otherwise keep looking
        AND  01h                ; if bit 0 set,
        CALL NZ,serIn           ; look for input from serial port 12/13h
        RET                     ; return, even if character not available

        CALL crlf               ; start a new line
putStr: LD   A,(HL)             ; load next char in string
        OR   A                  ; is it a zero?
        RET  Z                  ; if so, we are done
        PUSH BC 
        LD   B,A 
        CALL putCh              ; sent character to console etc
        POP  BC 
        INC  HL                 ; point to next character to send
        JP   putStr             ; and loop until done

putCh:  LD   (m08),A            ; save character to output
        PUSH HL
        LD   HL,m05             ; get output locations       
        LD   A,(HL)
        AND  01h                ; if bit 0 set,
        CALL NZ,serOut          ; output to port 12h
        LD   A,(HL)
        AND  02h                ; if bit 1 set,
        CALL NZ,piso            ; output to console
        LD   A,(HL)
        AND  04h                ; if bit 2 set,
        CALL NZ,sysOut          ; send char to console
        LD   A,(HL) 
        AND  10h                ; if bit 4 set
        LD   A,(m08) 
        CALL NZ,VIOOUT          ; send it to video output
        LD   A,(m05)
        AND  20h                ; if bit 5 set
        LD   A,(m08)
        CALL NZ,j34             ; send it to video output
        POP  HL
        RET 

j34:    LD   A,0FFh 
        OUT  (WP),A             ; adjust mem write protection
        LD   A,(m08)            ; get output char
        JP   VIOOUT             ; and send to video

                                ; Gets a character, processing <enter>,<esc>,lower case.
getCh2: CALL getCh1             ; get char, waiting until received
        AND  7Fh                ; strip off MSB  (7-bit ASCII only)
        CP   0Dh                ; was it <enter>?
        JP   Z,crlf             ; if so, send <cr><lf> 
        CP   03h                ; was it Ctrl-C?
        JP   Z,wboot            ; yes, do a warm boot
        CP   15h                ; was it Ctrl-Z?
        JP   Z,wboot            ; yes, do a warm boot
        CALL putCh              ; echo character to console
        CP   1Bh                ; was it <esc>?
        JP   NZ,j36             ; no, so continue evaluation below
        CALL getCh2             ; yes, so get next character
        JP   getCh2
j36:    CP   61h                ; was it upper-case?
        RET  C                  ; no, so return with it.
        CP   5Bh                ; 
        RET  NC                 ; 
        XOR  20h                ; convert input to upper case.
        RET 

; crlf issues a two byte carriage return/line feed combo to output devices
; 
crlf:   LD   A,0Dh              
        CALL putCh              ; output CR
        LD   A,0Ah              
        JP   putCh              ; output LF

pHex:   PUSH AF                 ; print byte in hexademical
        RRCA                    ; rotate first digit into lower 4 bits
        RRCA
        RRCA
        RRCA 
        CALL j35                ; convert first digit to ASCII
        POP  AF                 ; restore value & continue with 2nd digit
j35:    AND  0Fh                ; consider only lower 4 bits
        ADD  A,90h              ; convert value to ASCII
        DAA                     ; via clever textbook routine
        ADC  A,40h 
        DAA 
        JP   putCh              ; output hex digit

; pHex16 prints the word in HL as a four-digit hexadecimal, followed by a space
; for example, value 0A8EEh is printed as two ASCII characters "A8EE "
;
pHex16: LD   A,H                ; get upper byte 
        CALL pHex               ; and print it          
        LD   A,L                ; get lower byte
pHexSp: CALL pHex               ; print hex digit, followed by a space

space:  LD   A,20h 
        JP   putCh              ; print a space

; getHex reads hexadecimal characters from the console and stops when a
; non-hex digit is encountered.  The input could be any number of characters.
; On exit, HL contains the value of the entered value, and A contains non-hex char
; that stopped the routine.  

getHex: LD   HL,0000h           ; get console input into HL, default 0
j45:    CALL getCh2             ; get next character with processing
        PUSH AF                 ; save entered character
        CALL isHex              ; is char a hex digit?
        JP   NC,j44             ; jump if yes
        POP  AF                 ; restore entered character in A
        RET                     ; return on non-hex input
j44:    ADD  HL,HL              ; HL x 16 (shift it left one digit)
        ADD  HL,HL 
        ADD  HL,HL 
        ADD  HL,HL
        ADD  A,L                ; 
        LD   L,A                ; add in A
        POP  AF 
        JP   j45 

; isHex returned carry cleared for ASCII characters (A-F,0-9), but set otherwise.
; On exit, A contains value of the character.  For example, reg A=0Ah for "A" input
;
isHex:  SUB  30h                ; is it a control/special char?
        RET  C                  ; yes, return with carry set
        CP   0Ah                ; is it a numeric digit?
        JP   C,j39              ; yes, return with carry clear  
        SUB  11h                ; is it ':' thru '@'?
        RET  C                  ; yes, return with carry set
        ADD  A,0Ah              ; A->10h; B->11h; etc.
        CP   10h                ; set carry for A-F
j39:    CCF                     ; clears carry for A-F
        RET 

; showAW displays an address (HL-1) and it word contents
;
showAW: CALL showAB             ; display address (HL-1) & its byte contents
        LD   A,(HL) 
j93:    CALL pHex               ; and also contents at next address
        RET 

; showAB displays an address (HL-1) and it byte contents
;
showAB: CALL crlf               ; new line
j47:    DEC  HL 
        CALL pHex16             ; display an address (HL-1)
        LD   A,(HL)
        CALL pHexSp             ; and its contents
        INC  HL 
        RET 

;  Baud Table #1: timing values and associated baud rates
;  used for automatic baud rate determination
;
baudT1: .db  03h, 96h           ; 9600 baud
        .db  06h, 49h           ; 4900 baud (?)
        .db  0Bh, 24h           ; 2400 baud
        .db  17h, 12h           ; 1200 baud
        .db  2Eh, 06h           ; 600 baud
        .db  5Eh, 03h           ; 300 baud
        .db 0FFh, 01h           ; 110 baud
        .db 00h

; Baud Table #2: rates "96"=9600, etc and their associated timer0 
; count values, calculated as count = 2000000/(16*baud)
;
baudT2: .db 96h \ .dw 000Dh     ; 9600 baud
        .db 01h \ .dw 0470h     ;  110 baud
        .db 03h \ .dw 01A0h     ;  300 baud
        .db 48h \ .dw 001Ah     ; 4800 baud
        .db 24h \ .dw 0034h     ; 2400 baud
        .db 12h \ .dw 0068h     ; 1200 baud
        .db 06h \ .dw 000D0h    ;  600 baud
        .db 00h
         
; Table of Monitor commands and their jump vectors:
;     
cmdTbl: .db 'B' \ .dw cmdB      ; Boot from Floppy Disk
        .db 'C' \ .dw cmdC      ; Call routine in memory
        .db 'D' \ .dw cmdD      ; Display memory
        .db 'E' \ .dw cmdE      ; Examine & modify memory
        .db 'F' \ .dw cmdF      ; Fill memory
        .db 'H' \ .dw cmdL      ; Load Intel Hex tape
        .db 'I' \ .dw cmdI      ; Port Input
        .db 'J' \ .dw cmdJ      ; Jump to Memory
        .db 'K' \ .dw cmdK      ; Kill ROM and Jump
        .db 'M' \ .dw cmdM      ; Copy Memory
        .db 'N' \ .dw cmdN      ; Change disk descriptor address
        .db 'O' \ .dw cmdO      ; Port Output
        .db 'Q' \ .dw cmdQ      ; Switch-off ROM and Jump to VIO monitor
        .db 'R' \ .dw cmdR      ; Read from Diskette
        .db 'S' \ .dw cmdS      ; Search memory
        .db 'T' \ .dw cmdT      ; Test memory
        .db 'V' \ .dw cmdV      ; Verify Memory
        .db 'W' \ .dw cmdW      ; Write to Diskette
        .db 'Y' \ .dw cmdY      ; Change Direct I/O address
        .db 'Z' \ .dw cmdZ      ; Set baud rate
        .db ':' \ .dw cmd2      ; secondary commands follow
cmdTbl2:.db 'D' \ .dw cmdD2     ; change FD drive type
        .db 'I' \ .dw cmdI2     ; change char input device
        .db 'O' \ .dw cmdO2     ; change char output device
        .db 00                  ; end of table marker

        .db "        "
str4:   .db "IMSAI IEEE MONITOR    VERS 1.0",0
str5:   .db "HIT SPACE BAR",13,10,0
str6:   .db "INVALID BAUD",0
str7:   .db "BAUD SERIAL",13,10,0
str8:   .db "PARALLEL",13,10,0
        .fill 38h,1Ah

.END