;
;   Title:   mpu-b-rom.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   07 Jul 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler using Z80 mnemonics (-80 option)
;            
;   Descr:   This file represents a hand disassembly of the MPU-B-ROM.hex
;            source file, which is found on the z80pack GitHub repository.
;            
;            All labels, equates, and comments are mine.      
;           
;            Assemble using "TASM -80 -o20 mpu-b-rom.asm"
;            to reproduce the source file.
;

CTAPE      .EQU  0              ; cassette tape I/O port
CONIO      .EQU  2              ; console I/O port
CONSTAT    .EQU  3              ; console status port
MBC        .EQU  0F3h           ; MPU-B control (ROM banking) port
FDC        .EQU  0FDh           ; floppy disk controller port
WP         .EQU  0FEh           ; memory write protect port

FDCMD      .EQU  0              ; offset for floppy disk unit/command byte
FDRES      .EQU  1              ; offset for floppy disk result code
FDTRK      .EQU  3              ; offset for floppy disk track# (byte)
FDSEC      .EQU  4              ; offset for floppy disk sector# (byte)
FDDMA      .EQU  5              ; offset for floppy disk DMA buffer address

MPAGE      .EQU  0D000h         ; memory (for variables) base address
m02        .EQU  MPAGE + 0E4h   ; scrachpad area, also monitor stack start
m03        .EQU  MPAGE + 0F5h   ; FD driver type (1=DIO MINI, 2=DIO STD, 4=FIF)
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

DIOPAGE    .EQU  0E000h         ; DIO disc controller base address
DIOINIT    .EQU  DIOPAGE + 0Ch  ; DIO initialization address
DIOSIG     .EQU  DIOPAGE + 7FDh ; DIO firmware signature "DIO"
DIOEXEC1   .EQU  DIOPAGE + 06h  ; DIO execution address for std 8" disk
DIOEXEC2   .EQU  DIOPAGE + 09h  ; DIO execution address for mini 5.25" disk

TIMER0     .EQU  0D100h         ; timer0 address
TIMCTRL    .EQU  0D103h         ; timer control register


.ORG    0D800H

        LD      A,40H           
        OUT     (MBC),A         ; turn off ROM at 0000-07FF
        JP      j01             ; jump vector for start
        JP      getCh           ; jump vector for getCh
        JP      putCh           ; jump vector for putCh
        JP      mDump           ; jump vector for memory dump (addr in HL, count in DE)
j01:    LD      SP,m02          ; set up monitor stack 
        LD      HL,0080H        ; store default disk descriptor address
        LD      (m09),HL        ; at m09
        CALL    diskOK          ; check disk status (1=disk present)
        DEC     A
        JP      Z,bootFD        ; disk present, so boot with jump to 0000.
        XOR     A               ; no disk, so continue loading monitor
        LD      (m05),A         ; start finding output devices
        LD      HL,VIOSIG       ; check for video driver
        LD      A,'V'          
        CP      (HL)            ; does video signature start with 'V'? 
        JP      NZ,j07          ; nope
        INC     HL              ; go to next character
        LD      A,'I'           
        CP      (HL)            ; is it an 'I'?
        JP      NZ,j07          ; nope
        LD      A,10H           
        LD      (m05),A         ; mark the video driver present
        CALL    VIOINIT         ; and initialize it
j07:    LD      HL,m07          ; put address where direct I/O is done
        LD      (m06),HL        ; into memory register 06
        LD      A,0AEH
        OUT     (CONSTAT),A     ; set system UART mode = AE
        LD      A,27H
        OUT     (CONSTAT),A     ; set system UART command = 27h
        CALL    testIO          ; find input device(s), determine baud rate
        LD      HL,str4
        CALL    putStr          ; print "IMSAI MPU-B MONITOR" message
wboot:  LD      SP,m02          ; re-establish local stack
j10:    LD      HL,j10          
        PUSH    HL              ; push monitor restart address on stack
        CALL    crlf            ; start a new line
        LD      A,3FH
        CALL    putCh           ; print monitor command prompt "?"
        CALL    getCh           ; and get input character
        LD      HL,cmdTbl       ; points to table of monitor commands
j15:    CALL    getCh2          ; get next character with processing
        CALL    lookup          ; is it a valid command?
        RET     Z               ; no: return if command not found
        CALL    space           ; yes: print a space          
        LD      B,1             ; handy flag for complimentary commands (ex: P/U)
        JP      (HL)            ; handle the command request

cmd2:   LD      HL,cmdTbl2      ; point to start of 2nd command table
        JP      j15             ; return to main program loop

diskOK: CALL    initDD          ; create a disk descriptor
        LD      HL,m03          
        LD      A,(DIOSIG)      ; load signature byte 1
        SUB     44H             ; is it a 'D'?
        JP      NZ,j17          ; jump if no
        LD      A,(DIOSIG+1)    ; load signature byte 2
        SUB     49H             ; is it an 'I'?
        JP      Z,j18           ; found it, eval as DIO controller
j17:    LD      (HL),4          ; no, so type '4' FDC
        JP      fdExec          ; and execute the FD command
j18:    CALL    DIOINIT         ; initialize the FDC
        LD      (HL),2          ; mark it as controller type '2'
        CALL    fdExec          ; and execute the FD command
        RET     P               ; return if successful
        LD      HL,m03
        LD      (HL),1          ; mark it as controller type '1'
        JP      fdExec          ; and execute the FD command

bootFD: LD      A,0C0H          
        OUT     (MBC),A         ; remove ROM from memory map
        JP      0000h           ; and cold boot

; testIO will test for the presence of I/O ports, and determine the proper baud rate
; for the serial port.
;
testIO: IN      A,(14H)
j28:    LD      HL,str5
        CALL    putStr          ; print "HIT SPACE BAR" message
        LD      HL,m05         
        LD      A,10H
        AND     (HL)
        LD      (HL),A
        DEC     HL
        XOR     A
        LD      (HL),A          ; initialize device list

        ; first, test for a specific UART mode

        CALL    j20             ; turn on synchronous mode
        LD      B,0             ; Reg B will track devices as they are found
        IN      A,(12h)         ; look at serial port 12 input
        OR      A               ; anything there?
        JP      Z,j21           ; nope, skip to next test
        INC     B               ; yep, so flag this mode

        ; the loop below waits for input on serial and parallel ports,
        ; marks which are found, and exits if parallel or system serial.

j21:    CALL    parIn           ; look for input on parallel port
        JP      NZ,ppFnd        ; if found, mark as parellel & exit
        CALL    sysIn           ; get getting input from system port 2/3
        JP      NZ,j25          ; if only on system port, mark as device 4 & exit
        LD      A,B
        OR      A
        JP      Z,j21           ; loop until a device is found
        CALL    serIn           ; try getting input from serial port 12/13h
        JP      Z,j21           ; loop if nothing there
j34:    INC     A
        JP      Z,j21           ; loop if FFh received

        ; Serial port 12h:  Automatically determine the baud rate
        ; When a character is ready on the UART, measure long it takes to receive it

        LD      B,0             ; initialize wait-for-character counter
j27:    CALL    serIn           ; look at serial port 12/13h
        JP      Z,j27           ; and wait until char is ready
        INC     B               ; char incoming, count until it is received
        OR      A               ; has it been received yet?
        JP      Z,j27           ; no, keep counting until received

        ; B now contains the # of above loops it took to receieve a character
        ; use Baud Table #1 to translate this to an approximate baud rate

        LD      HL,baudT1-1     ; point to baud rate table #1
j29:    INC     HL
        LD      A,(HL)          ; get timer byte from the table
        OR      A               ; 0 = could not find baud rate
        JP      Z,j28           ; so try again from the beginning
        CP      B               ; compare table balue with counter
        INC     HL              ; point to next value in table
        JP      C,j29           ; loop if its less than counter
        LD      A,(HL)          ; point to next value in table       
        PUSH    AF              ; and save as the corresponding baud rate
        CALL    setBaud         ; set baud rate to A (ex: 48 = 4800 baud)
        LD      B,1             ; mark device '1' present
        CALL    j31             ; add add it to I/O device list
        POP     AF              ; restore table value

     ; finally, print the baud rate that was established.

        LD      H,A             ; and save as baud rate
        LD      L,0           
        CP      1               ; is baud rate "100"?
        JP      NZ,j32 
        LD      L,10H           ; convert baud 100 to 110.
j32:    CALL    pHex16          ; print value as baud rate (ex "9600")
        LD      HL,str7
        JP      putStr          ; print "BAUD SERIAL" message

ppFnd:  LD      B,2             ; parallel port found, mark it as device '2'
        CALL    j31             ; add it to input & output devices  
        RRA     
        RRA     
        RRA     
        AND     B
        XOR     (HL)
        LD      (HL),A
        LD      HL,str8
        JP      putStr          ; print "PARALLEL"

j25:    LD      C,A             ; save input in C
        IN      A,(12h)         ; look at input from serial port 12h       
        CP      C               ; if they match,
        JP      Z,j34           ; jump back to input loop
        IN      A,(14h)         ; look at input from parallel port 14h
        CP      C               ; does it match input char?
        JP      Z,ppFnd         ; yes, so mark it was parallel & leave
        LD      A,C             ; input is from system serial; get it
        AND     7FH             ; look at only the lower 7 bits
        CP      20H             ; is it a ' ' space character?
        JP      NZ,j21          ; not a space, so return to input loop
        LD      B,4             ; space found, so mark as device '4'

; Save the I/O device(s) in B to the input and output bitmaps 
;
j31:    LD      HL,m04          ; get current input devices
        LD      A,(HL)          ; into A      
        OR      B               ; add in current device
        LD      (HL),A          ; and save back to input device list
        INC     HL              ; get output device list
        LD      A,(HL)          ; into A
        OR      B               ; add current device
        LD      (HL),A          ; and save back to output device list
        RET     

; Put UART in synchonous mode at a specific speed
;
j20:    LD      HL,0042H
        CALL    setT0           ; set timer0 counter value with HL
        CALL    resetU          ; reset UART
        LD      A,4CH           ; command byte = 4C
        OUT     (13H),A         ; send reset command
        LD      A,0FFH
        OUT     (13H),A
        OUT     (13H),A
        LD      A,0B7H          ; command B7:
        OUT     (13H),A         ; puts UART in synchronous mode
        RET     

; Set up serial port UART at ports 12/13h with normal settings
;
initU:  CALL    resetU          ; UART soft reset
        LD      A,0AEH          ; AE = baud 1/16th clk, 8 bits, no parity, 1.5 stop
        OUT     (13H),A         ; send mode byte
        LD      A,37H           ; 37 = reset flags, RTS/DTR true, Rx/Tx enabled
        OUT     (13H),A         ; send command byte
        RET     

;  Do a software reset of the serial UART at ports 12/13h
;
resetU: XOR     A       
        OUT     (13H),A         ; prepare reset
        OUT     (13H),A
        OUT     (13H),A
        LD      A,40H           ; 40h = reset UART command
        OUT     (13H),A         ; do UART software reset
        RET     

; Handle the Change Baud Rate "Z" command
;
cmdZ:   CALL    getHex          ; get desired baud rate from user
        LD      A,H
        OR      L               ; is it zero/no input?
        JP      Z,j28           ; do 2 terminal procedure
        LD      A,H

setBaud:LD      HL,baudT2       ; point to baud rate table #2
        PUSH    BC
        CALL    lookup          ; find baud rate & timer count
        POP     BC
        CALL    NZ,setT0        ; set timer0 value
        JP      NZ,initU        ; initialize UART is usual manner
        LD      HL,str6         ; could not find baud rate, so
        JP      putStr          ; print "INVALID BAUD" message

; The following routine sets up timer0 with the count value in HL
;
setT0:  LD      DE,TIMCTRL      ; DE = timer control address
        EX      DE,HL
        LD      (HL),36H        ; 36h = timer0, read lsb/msb, mode 3, binary
        LD      HL,TIMER0       ; load the value into timer0
        LD      (HL),E          ; store LSB first,
        LD      (HL),D          ; then MSB
        RET     

; Handle the Jump Memory "J" and Call Routine "C" commands
;
cmdJ:   POP     DE              ; remove return address from the stack
cmdC:   CALL    getHex          ; get address from user
        JP      (HL)            ; and go there.

; Handle the Kill ROM & Jump "K" command
;
cmdK:   CALL    getHex          ; get address from user
        LD      A,0C0H
        OUT     (MBC),A         ; remove ROM from memory map
        JP      (HL)            ; and jump to the address

; Handle the Quit MPU-B and Jump to VIO monitor "Q" command
;
cmdQ:   LD      A,(m05)         ; is video initialized?
        AND     10H             ; look at bit4 (video) only
        RET     Z               ; continue only if video present
        LD      A,0C0H
        OUT     (MBC),A         ; remove ROM from memory map
        JP      VIOSTART        ; and jump to VIO monitor

; Handle the Examine memory "E" command
;
cmdE:   CALL    getHex          ; get address from user
j41:    CALL    crlf 
        CALL    pHex16          ; print address + space
        CALL    mod8            ; print byte, prompt for new value
        DEC     HL              ; point to previous address
        CP      0AH             ; if user pressed <enter>
        RET     Z               ; then quit
        CP      '-'             ; if user pressed minus '-' key
        JP      Z,j41           ; then show value at previous addr
        INC     HL              ; otherwise advance to next addr
        INC     HL
        JP      j41             ; and show it.

; mod8 shows the byte value at address HL, in hex, and allows user to change it.
;        
mod8:   LD      A,(HL)          ; get byte from HL
        LD      E,A             ; save value in E
        CALL    pHexSp          ; print value, followed by space       
        EX      DE,HL           ; temp save HL
        CALL    j39             ; get new value into L
        EX      DE,HL           ; restore HL, new value now in E
        LD      (HL),E          ; store new value to memory
        RET     

; Handle the Display/Dump memory "D" command:
;
; Parameters are start address (HL) and optional end address in DE
; If no end address, just one byte is printed.
; After 24 lines print, it will wait for a keypress before continuing
;
cmdD:   CALL    param2          ; get start address in HL and count in BC      
mDump:  LD      E,24            ; E = line counter.  Up to 24 lines/screen
        PUSH    HL              ; push start addr on stack
j52:    LD      D,E             ; reset D to line counter
j50:    PUSH    DE              ; save current line counter on stack
        LD      D,17            ; set up a counter for 17 bytes
        LD      HL,m03          ; clear out memory between stack and m03
        XOR     A               ; A=0
j44:    DEC     HL              ; going backwards,
        LD      (HL),A          ; clear memory
        DEC     D               ; one byte at a time,
        JP      NZ,j44          ; until all bytes are cleared
        POP     DE              ; restore line counter
        EX      (SP),HL         ; swap start address and ASCII store area

        ; the line begins with printing the memory address

        CALL    crlf            ; start new line
        CALL    pHex16          ; with address shown
        CALL    space           ; followed by a space

        ; printing of each hex value in the line starts here

j49:    CALL    getCh           ; does user want to stop output?
        JP      NZ,j45          ; yep, so quit
        LD      A,(HL)          ; get byte at current address
        CALL    pHexSp          ; and print it, followed by a space
        LD      A,(HL)          ; reload the byte
        INC     HL              ; go to next address

        ; before going to the next byte, store an ASCII representation of the value 
        ; in the scratchpad area, replacing nonprintable values with '.'
    
        EX      (SP),HL         ; swap scratchpad address back in      
        LD      (HL),A          ; store byte as a potential ASCII char
        CP      20H             ; is the byte a control character?
        JP      C,j46           ; if so, replace with a '.'
        CP      7FH             ; is the value above ASCII range?
        JP      C,j47           ; if not, keep as a printable character
j46:    LD      (HL),'.'        ; replace nonprintable char with '.'
j47:    INC     HL              ; go to next location in ASCII string
        DEC     BC              ; decrement byte counter
        LD      A,B             
        OR      C               ; BC=0 all done?
        JP      Z,j48           ; exit if done

        ; Evaluate if there are more bytes to show in this line
  
        EX      (SP),HL         ; swap current addr back in
        LD      A,L             ; look at lower byte
        AND     0FH             ; are we on a 16 byte boundary (xxx0)?
        JP      NZ,j49          ; loop if not yet

        ; All 16 bytes have been printed as hex values, so print the ASCII string   

        EX      (SP),HL         ; swap addr back out
j48:    CALL    space           ; print a space
        LD      HL,m02          ; point to the ASCII string 
        CALL    putStr          ; and print it

        ; Evaluate if there are more lines to print, and loop accordingly

        LD      A,B
        OR      C               ; if byte counter is zero
        JP      Z,j45           ; then quit
        DEC     D               ; decrement line counter
        JP      NZ,j50          ; do next line if space left on screen
        CALL    getCh1          ; have shown 24 lines, now wait for user
        CALL    crlf            ; start next line
        JP      j52             ; with reset of line counter
j45:    POP     HL              ; get rid of start addr pushed on stack
        JP      crlf 

; Handle Protect "P" and Unprotect "U" memory commands
; The command sent to the write protect port has the form "NNNNNNxx"
; where NNNNNN is the 1K block of memory (0-63 decimal = addr 0000-FC00) 
; and xx is the command (01=protect, 00=unprotect)
;
cmdP:   INC     B               ; protect command=1, unprotect cmd=0
cmdU:   CALL    getHD           ; get start (HD) and end (DE) addresses
        LD      A,D             ; with MSB of destination address
        AND     0FCh            ; mask it to 1K boundary
        OR      B               ; add in P/U command bit (bit0)
        LD      D,A             ; and put result back into D.
        LD      A,H             ; with MSB of start address,
        AND     0FCh            ; mask to 1K boundary
        OR      B               ; and add in P/U command bit
j54:    OUT     (WP),A          ; send command to Write Protect Unit
        CP      D               ; are we at destination 1K block yet?
        RET     Z               ; quit when done
        ADD     A,4             ; otherwise point to next 1K (4x256) block
        JP      j54             ; and loop until done

;  Handle Tape Hex Loader "H" command.  Reads input in Intel Hex format.
;  If a checksum error is found then 'C' is output. 
;  If an invalid character is found then 'T' will be output
;
cmdH:   CALL    getCh2          ; get next character with processing
        SUB     ':'             ; is it a colon ':'?
        JP      NZ,cmdH         ; no, so keep reading until colon found
        LD      D,A             ; start of new line, set checksum to 0
        CALL    j56             ; read in next 2 hex digits, convert to number (byte counter)
        RET     Z               ; if zero, then complete file has been read
        LD      B,E             ; otherwise, result is #byte in this line
        CALL    j56             ; read next 2 hex digits, convert to number 
        LD      H,E             ; put result in H
        CALL    j56             ; read in 2 hex digits, convert to number 
        LD      L,E             ; put result in L.  Now we have address to store data
        CALL    j56             ; read in 2 hex digits, convert to number 
j57:    CALL    j56             ; read in 2 hex digits, convert to number (data byte)
        LD      (HL),E          ; put data byte into memory       
        INC     HL              ; go to next memory address
        DEC     B               ; and decrement byte counter       
        JP      NZ,j57          ; loop until entire line is read 
        CALL    j56             ; read in 2 hex digits, convert to number (checksum)
        JP      Z,cmdH          ; loop to read next line if checksum is valid
        LD      A,'C'           ; otherwise,
        JP      putCh           ; print 'C' = checksum error

; tape loader helper function, which reads in two hex characters
; and converts them to a byte value
;       
j56:    CALL    getCh2          ; get next character with processing
        CALL    isHex           ; convert ASCII character to hex digit
        JP      C,j59           ; jump if not hex digit
        ADD     A,A             ; first digit obtained. Multiply x 16 = shift left
        ADD     A,A             ; mpy x 4
        ADD     A,A             ; mpy x 8
        ADD     A,A             ; mpy x 16
        LD      E,A             ; store result in E
        CALL    getCh2          ; get next character with processing
        CALL    isHex           ; convert ASCII character to hex digit
        JP      C,j59           ; jump if not hex digit
        ADD     A,E             ; combine 1st and 2nd digits
        LD      E,A             ; store result in E
        ADD     A,D             ; add result to reg D
        LD      D,A             ; where a running total kept for checksum
        RET     

; Handle the Test Memory "T" command
;
cmdT:   CALL    param2          ; get start address in HL and count in BC 
        DEC     BC
j62:    XOR     A               ; start with byte pattern = 00
        LD      D,(HL)          ; save actual memory byte in D
j61:    LD      (HL),A          ; store pattern to memory
        CP      (HL)            ; and read it back.  Same?
        JP      NZ,j60          ; if not, show address & contents
        DEC     A               ; go to next byte pattern
        JP      NZ,j61          ; and loop through all patterns
        LD      (HL),D          ; restore original byte to memory 
        CALL    getCh           ; check to see if user wants to quit
        RET     NZ              ; quit now if so
        INC     HL              ; go to next memory location
        DEC     BC              ; and decrement byte counter
        LD      A,B
        OR      C               ; finished (BC=0)?
        JP      NZ,j62          ; no, so go to next address to test
        RET     

j60:    INC     HL              ; helper function for Test Memory command
        LD      E,A             ; save current byte pattern
        CALL    showAB          ; show address HL-1 and its byte contents 
        LD      A,E             ; restore byte pattern
        JP      j64             ; show expected byte pattern

; Handle the Move Memory "M" command
; call with <start addr>,<end addr>,<destination addr>
;
cmdM:   CALL    param3          ; load parameters in HL, DE, and BC
j67:    JP      memcpy          ; and do it.

; Handle the Fill Memory "F" command
; call with <start addr>, <end addr>, <fill character>
;
cmdF:   CALL    param3          ; laod parameters into HL, DE, and BC
        LD      A,E             ; fill character in DE moved to A
        LD      (HL),A          ; store first fill char at start address
        DEC     BC              ; cound one done!
        LD      D,H             ; DE = HL
        LD      E,L
        INC     DE              ; set destination = start + 1
        JP      j67             ; and do a memory copy

; Handle the Input "I" and Output "O" port commands
;      
cmdI:   DEC     B               
cmdO:   CALL    getHD2          ; get port# and value (for output)
        LD      A,B             ; A=0 for IN, 1 for OUT
        RLCA                    ; 3 rotates set up A to contain I/O opcode
        RLCA    
        RLCA                    ; now A=0 for IN, 08 for OUT
        XOR     0DBh            ; now A=DB for IN, D3 for OUT 
        LD      D,L             ; put port# in D
        LD      HL,(m06)        ; get address for I/O
        LD      (HL),A          ; store opcode (IN or OUT)
        CP      (HL)            ; did it stick?
        RET     NZ              ; nope, this memory aint no good.
        PUSH    HL              ; save this address
        INC     HL              
        LD      (HL),D          ; after opcode, add port#
        INC     HL      
        LD      (HL),0C9h       ; after port#, add RET opcode
        LD      HL,j53          ; put return address on stack
        EX      (SP),HL          
        LD      A,B
        OR      A
        LD      A,E
        JP      (HL)            ; execute the I/O instruction
j53:    JP      Z,pHexSp        
        RET     

; Handle the Change direct I/O address "Y" command
;
cmdY:   CALL    getHex          ; get address from user into HL
        LD      A,H
        OR      L               ; 0= no address entered?
        JP      NZ,j69          ; user entered address, so use it
        LD      HL,m07          ; otherwise, reset to original location m07
j69:    LD      (m06),HL        ; put new address in m06
        RET     

; Handle the Verify memory "V" command. 
; The parameters are start (HL), destination (DE), and count (BC)  
;
cmdV:   CALL    param3          ; load parameters into HL, DE, BC
j72:    LD      A,(DE)          ; A = byte at destination addr
        CP      (HL)            ; is it same as byte at source addr?
        INC     HL              ; go to next source byte
        INC     DE              ; go to next dest byte
        JP      Z,j70           ; if same, continue verification process
        CALL    showAB          ; not same: show source addr & contents
        EX      DE,HL           ; flip to DE
        CALL    j71             ; show dest addr & its contents
        EX      DE,HL           ; flip back
j70:    DEC     BC              ; decrement byte counter
        LD      A,B     
        OR      C               ; byte counter = 0?
        RET     Z               ; quit if it is.
        CALL    getCh           ; does user want to quit verification?
        RET     NZ              ; yes, quit
        JP      j72             ; no, keep going.

; Handle Search Memory "S" command
; Parameters are start addr (HL), value to find (DE), byte counter (BC), optional mask (SP)
;
cmdS:   CALL    param3          ; load parameters into HL, DE, and BC 
        PUSH    HL              ; put start address on stack
        LD      HL,0FFFFh       ; assume full 16-bit compare (mask FFFF)       
        CP      0AH             ; if no 4th parameter 
        CALL    NZ,getHex       ; otherwise, get mask into HL 
        EX      (SP),HL         ; swap mask and start addr.
j75:    LD      A,(HL)          ; grab byte at current address
        EX      (SP),HL         ; swap mask into HL
        AND     H               ; apply mask to current byte
        CP      D               ; do compare with search value
        EX      (SP),HL         ; swap mask out
        INC     HL              ; point to next byte to check
        JP      NZ,j73          ; no match, so continue search
        LD      A,(HL)          ; MSB matches, so check LSB now
        EX      (SP),HL         ; swap in mask
        AND     L               ; apply mask to LSB
        CP      E               ; and compare it to seach LSB
        EX      (SP),HL         ; swap mask back out
        CALL    Z,showAW        ; if match, show address (HL-1) and word contents
j73:    DEC     BC              ; decrement byte counter          
        LD      A,B
        OR      C               ; is byte counter 0?
        JP      NZ,j75          ; not done yet, so grab next byte
        POP     BC              ; get rid of mask pushed on stack
        RET     

; Copy Memory from source (HL) to destination (DE)
; BC = number of bytes to copy
;
memcpy: LD      A,B             
        OR      C               ; all done (byte counter BC=0)?
        RET     Z               ; quit if so
        LD      A,(HL)          ; get next byte from source
        LD      (DE),A          ; and store it at destination
        DEC     BC              ; decrement byte counter
        INC     HL              ; increment source address
        INC     DE              ; increment destination address
        JP      memcpy          ; loop until done

; Handle the Execute "X" and Load "L" Cassette File commands
;
cmdX:   DEC     B
cmdL:   PUSH    BC              ; save command X (B=0) vs L (B=1)
        CALL    getHD           ; get start(HL) and end(DE) addresses
        LD      A,D
        OR      E               ; no end address?
        JP      Z,j76           ; will get parameters from file instead
        PUSH    HL              ; save start addr on stack
        PUSH    DE              ; save end addr on stack
        CALL    getHex          ; get execution addr
        PUSH    HL              ; and save it on stack, too.
        CALL    getHdr          ; read in tape header, Z=success
        JP      NZ,j78
        POP     AF              ; discard addresses from header
        POP     AF              ; since user specified them.
        POP     AF
        JP      j78
j76:    CALL    getHdr          ; read in tape header, Z=success
j80:    JP      Z,j78
j59:    LD      A,'T'
j91:    CALL    putCh           ; print error message 'T'
        JP      j10             ; and warm boot monitor

        ; start, end, and exec addresses are now on stack
        ; use them to calculate number of records to be read

j78:    POP     BC              ; BC = exec addr
        POP     HL              ; HL = end addr
        POP     DE              ; DE = start addr
        PUSH    BC              ; save 'em all back to stack
        PUSH    HL
        PUSH    DE
        LD      A,L             ; do 16-bit math (HL = HL-DE)
        SUB     E
        LD      L,A             ; now L = L- E
        LD      A,H 
        SBC     A,D             ; account for carry from previous sub
        LD      H,A             ; now H = H - D
        ADD     HL,HL           ; multiply x 2 since records are 128b not 256b      
        LD      C,H             ; number of 128-byte records between end & start
        INC     C               ; C = number of records to read

        ; read in 128-byte records from cassette tape
        ; checking record type at start and checksum at end of each record

j85:    CALL    readC           ; read record type at start of each record
        CP      81H             ; must be type '81'
        JP      NZ,j80          ; if wrong file type, exit with error
        CALL    readC           ; read next byte as record length
        LD      B,A             ; save it in B as a counter
        LD      HL,0000         ; initialize checksum for this record
j82:    CALL    j81             ; read in data byte & add it to checksum
        LD      (DE),A          ; store byte to destination address
        INC     DE              ; increment address
        DEC     B               ; decrement byte counter
        JP      NZ,j82          ; loop until entire record is read
        PUSH    DE              ; temp save destination address
        EX      DE,HL           ; save actual checksum in DE
        CALL    readHL          ; read expected checksum from cassette
        LD      H,L
        LD      L,A
        ADD     HL,DE           ; add them together
        LD      A,H
        OR      L               ; is HL=0?
        LD      A,'C'
        CALL    NZ,putCh        ; no: print 'C' for checksum error
        JP      NZ,j84
        LD      A,'*'           
        CALL    putCh           ; yes: print '*' for successful record load
j84:    POP     DE              ; restore destination address
        DEC     C               ; decrement record counter
        JP      NZ,j85          ; loop until all records are read

        ; File load complete.
        ; Display the starting, ending, and execution addresses.

        CALL    crlf 
        LD      C,03H           ; three words to display     
j86:    POP     HL              ; get each one off of the stack
        CALL    pHex16          ; and show it
        DEC     C
        JP      NZ,j86          ; loop until done

        ; Load vs Exec

        POP     AF              ; which command (Exec vs Load) are we doing?
        RRA                     ; rotate through carry
        RET     C               ; return if was Load cmd
        JP      (HL)            ; otherwise jump if it was Exec cmd

; Read the cassette file header
;
getHdr: CALL    readS           ; read sync stream
        CALL    readC           ; read byte from cassette
        CP      01H             ; is it 01?
        RET     NZ              ; if not, read failed.
        CALL    readC           ; read 2nd byte from cassette
        LD      C,5             ; next, read filename:
j88:    CALL    readC           ; read 5 bytes from cassette
        CALL    putCh           ; and echo them to console
        DEC     C               
        JP      NZ,j88          ; loop until 5 bytes read
        LD      C,3             ; read in 3 addresses
j89:    CALL    readHL          ; get HL value from cassette
        EX      (SP),HL         ; swap it to stack
        PUSH    HL              
        DEC     C
        JP      NZ,j89          ; loop until done
        CALL    readHL          ; get final value
        XOR     A
        RET     

;  Reads sync stream from cassette
;
readS:  CALL    j90             ; wait until 1st sync byte received
        LD      B,31            ; count 31 more bytes
j92:    CALL    readC           ; read byte from cassette
        CP      0E6H            ; is it sync byte E6?
        LD      A,'I'           
        JP      NZ,j91          ; if not, exit with eror 'I'
        DEC     B
        JP      NZ,j92          ; loop until done
        RET     

; sendC sends a byte to the cassette port
;
sendC:  PUSH    AF              ; save byte to send
j93:    IN      A,(CONSTAT)     ; check system serial status 
        AND     04h             ; look only at bit2 (Tx empty?)
        JP      Z,j93           ; wait until ready
        POP     AF              ; restore byte to send
        OUT     (CTAPE),A       ; send it out cassette port
        RET     

; Handle the Generate Sync Stream "G" command
;
cmdG:   CALL    getHex 
        LD      A,10H
        OUT     (CONSTAT),A
j95:    LD      A,0E6H          ; byte 'E6'
        CALL    sendC           ; send it to cassette
        IN      A,(CONSTAT)     ; check system serial status
        AND     02H             ; look only at bit1 (Rx ready)
        RET     NZ              ; return when ready
        JP      j95             ; keep sending until ready

; readC reads a byte from cassette
;
readC:  CALL    getCh           ; allow user to quit now
        JP      NZ,wboot        ; quit if user typed something
        IN      A,(CONSTAT)     ; check system serial status
        CP      4               ; bit2 (Tx empty)
        JP      Z,readC         ; wait until Tx empty
        IN      A,(CTAPE)       ; read byte from cassette port
        RET     

; reads a byte from the cassette, and adds it value to checksum in HL
;
j81:    CALL    readC           ; get byte from cassette tape
        PUSH    BC              ; temp save BC
        LD      C,A             ; put value into BC
        LD      B,0
        ADD     HL,BC           ; add value to HL
        POP     BC              ; restore BC
        RET     

; Handle Adjust & Align Cassette "A" command
; Not much info on this command.  End by Ctrl-C from user?
;
cmdA:   CALL    getCh2          ; user exit with Ctrl-C
        CALL    j90             ; read until sync stream
j98:    LD      HL,0F000H       ; address to store cassette data
        LD      DE,01E1H        ; counter
j97:    DEC     DE
        LD      A,D
        OR      E               ; DE=0?
        JP      Z,j98
        LD      (HL),7FH        ; store 7Fh to memory
        CALL    readC           ; read a byte from cassette
        LD      (HL),A          ; store it in memory
        INC     HL              ; go to next memory location
        JP      j97             ; and loop

; routine that reads cassette until start of sync stream encountered
;
j90:    LD      A,60H
        OUT     (CONSTAT),A
j99:    CALL    readC           ; read byte from cassette
        CP      0E6H            ; is it sync byte E6?
        JP      NZ,j99          ; if not, keep reading
        LD      A,20H
        OUT     (CONSTAT),A         
        RET     

; Read two bytes from cassette and store them in HL
;
readHL: CALL    readC           ; read a byte from cassette
        LD      L,A             ; save it to L
        CALL    readC           ; read a byte from cassette
        LD      H,A             ; save it to H
        RET     

; Handle the Boot to Floppy "B" command
;
cmdB:   CALL    crlf 
        CALL    diskOK          ; check disk status (1=disk present)
        DEC     A
        JP      Z,bootFD        ; disk present, so boot with jump to 0000.

; Handle a floppy disk error by printing the result code
;
fdErr:  AND     0F0H            ; look at upper 4 bits of disk status
        LD      HL,str1          
        CP      0A0H            ; if status = A0,
        JP      Z,putStr        ; print "NOT READY"
        LD      HL,str2         ; otherwise,
        CALL    putStr          ; print "DISK ERR"
        LD      HL,(m09)        ; point to disk descriptor
        INC     HL              ; result code is in 2nd byte
        LD      A,(HL)          ; get the disk error #
        JP      pHex            ; and print it

str1:  .db "NOT READY",0
str2:  .db "DISK ERR- ",0


; initDD: initializes 9 bytes FDC disc descriptor at m02
; first byte contains command (2=read) in upper nibble and
; unit (1=drive0, 2=drive1, 4=drive2, 8=drive3) in lower nibble
;  21 00 00 00 01 00 00 00 00
;
initDD: LD      HL,m02+8        ; zero out a block of 9 bytes
        LD      B,8             
j03:    DEC     HL
        LD      (HL),00H       
        DEC     B
        JP      NZ,j03          
        LD      (HL),21h        ; store 21h (Read Drive #0) as FDCMD
        LD      A,1
        LD      (m02+FDSEC),A   ; store 01 (Sector 1) as FDSEC
        RET     

; Handle the "N" command
; This command is not described in the VIO manual
; It lets the user change the location of the disc descriptor, normally at 0080h.
;
cmdN:   CALL    getHex          ; get address from user
        LD      A,H
        OR      L               ; is it zero/not entered?
        JP      NZ,j11          ; no, use entered value
        LD      L,80H           ; otherwise use default 0080h location.
j11:    LD      (m09),HL        ; and store value at m09
        RET     

; fdExec: copy DD to its destination, then issue FDC commands to recognize DD, 
; execute the disk operation, and receive the result code.
;
fdExec: LD      HL,(m09)        ; HL = disk descriptor address
        LD      DE,m02          ; copy of disk descriptor in scratchpad
        LD      B,7             ; copies descriptor to its address               
j02:    EX      DE,HL
        LD      A,(HL)          ; get a byte
        EX      DE,HL
        LD      (HL),A          ; and store it
        INC     HL
        INC     DE
        DEC     B
        JP      NZ,j02          ; loop until done.
        LD      A,10H           ; FDC cmd 10h = set disk descriptor
        CALL    fdSend
        LD      HL,(m09)        ; get descriptor address, usually 0080h
        LD      A,L
        CALL    fdSend          ; send LSB first
        LD      A,H
        CALL    fdSend          ; followed by MSB
        XOR     A
        CALL    fdSend          ; send cmd 0h = Disk I/0

        ; now that a command has been sent to the controller, poll the result
        ; byte for a response.  Return as soon as a response is received, but
        ; time out after 128K attempts.
 
        LD      DE,0000         ; DE & B used as time-out counter
        LD      B,2
        LD      HL,(m09)        ; get disk descriptor address
        INC     HL              ; now point to FDC result code
j06:    LD      A,(HL)          ; get result code from FDC
        OR      A               ; is it zero? 
        RET     NZ              ; return when FDC returns a result
        DEC     D
        JP      NZ,j06          ; wait loop x 256 reads
        DEC     E
        JP      NZ,j06          ; wait loop x 64K
        DEC     B
        JP      NZ,j06          ; wait loop x 128K
        RET                     ; don't wait forever!

; Handle Write to Diskette "W" command
; call with <track>,<sector>,<buffer addr>,<Unit#>  (last 2 optional)       
;
cmdR:   CALL    initDD          ; initialize disc descriptor with Read on Disk 0
        JP      diskIO          ; do it

; Handle Write to Diskette "W" command
; call with <track>,<sector>,<buffer addr>,<Unit#>  (last 2 optional)       
;
cmdW:   CALL    initDD          ; initialize disc descriptor with Read on Disk 0
        LD      (HL),11H        ; change command byte to 11h = Write to Disk 0

diskIO: CALL    getHD2          ; get track & sector
        LD      (m08),A         ; temp save last char
        LD      A,L
        OR      E               ; was it zero/invalid?
        JP      Z,j12           ; print "INVALID"
        LD      (m02+FDTRK),HL  ; store first parameter as track #
        LD      A,E             
        LD      (m02+FDSEC),A   ; store 2nd param as sector #
        LD      A,(m08)         ; retrieve last char
        CP      0AH             ; was it <enter>?
        JP      Z,j13           ; use default DMA & unit
        CALL    getHD2          ; otherwise get DMA in HL, unit in DE
        LD      (m02+FDDMA),HL  ; store DMA buffer address

        ; The Drive# (0-3) must be converted into a bit pattern.
        ; For drive 0, bit 0 is set, etc.

        LD      A,E             ; get unit #
        AND     03H             ; mask to 0-3
        INC     A               ; convert 0-3 to 1-4
        LD      E,A             ; use this as a rotation counter
        XOR     A
        SCF                     ; start with A=0 plus carry set
j14:    RLA                     ; rotate carry to bit0 
        DEC     E
        JP      NZ,j14          ; set drive bit by rotating

        ; The first byte in the disc descriptor contains the disk command & unit info.
        ; the upper 4 bits are the command and the lower 4 bits are the unit.
        ; add the new drive info to the existing command byte 

        LD      E,A             ; copy drive info to E
        LD      HL,m02+FDCMD    ; address for command/unit byte
        LD      A,(HL)          ; get old drive command/unit value
        AND     0F0H            ; mask out unit
        OR      E               ; add in new unit
        LD      (HL),A          ; and save new command/unit 

        ; Now the disc descriptor contains the command &  user input parameters

j13:    CALL    fdExec          ; send command to FC controller & get result
        DEC     A               ; was result code 1=success?
        RET     Z               ; return if so with A=0
        JP      fdErr           ; otherwise print the disk error code

; fdSend: send a byte to the Floppy Disk Controller
;
fdSend: LD      (m08),A         ; temp store byte to send
        LD      A,(m03)         ; get controller type
        CP      04H             ; is it type 4?
        JP      NZ,j19          ; if not, jump below
        LD      A,(m08)         ; get FDC command in m08 
        OUT     (FDC),A         ; and sent it to floppy disk (FIF) controller
        RET     

j19:    CP      02H             ; is it type 2 controller?
        LD      A,(m08)         ; get FDC command in m08 
        JP      Z,DIOEXEC1      ; yes, jump to controller addr
        JP      DIOEXEC2        ; no, jump to this one instead

j12:    LD      HL,str3
        JP      putStr          ; print "INVALID"

str3:  .db "INVALID",0

cmdD2:  LD      HL,m03          ; show FD type and allow user to change it
        JP      mod8 

cmdI2:  LD      HL,m04          ; show input device & allow user to change
        JP      mod8 

cmdO2:  LD      HL,m05          ; show output device & allow user to change
        JP      mod8 

; Param2 gets <start address> in HL and <end address> in DE
; Then calculates the number of bytes between them as BC = ED - HL + 1
;        
param2: CALL    getHD           ; get up to 2 parameters in HL, DE
        PUSH    AF              ; save A
        LD      A,E             ; do 16-bit math (BC=ED-HL) as two 8-bit ops
        SUB     L
        LD      C,A             ; now C = E-L
        LD      A,D
        SBC     A,H
        LD      B,A             ; now B = D-H
        INC     BC              ; add one to count
        POP     AF              ; restore A
        RET     

; getHD gets start and end addresses from the user, which can be separated
; by either a comma or space, and puts results into HL and DE.
;
getHD:  CALL    getHex          ; get first parameter in HL
        LD      D,H             ; copy HL to DE
        LD      E,L
j08:    CP      0AH             ; was input just <enter>?
        RET     Z               ; return if so
        CP      20H             ; was last char a space?
        JP      Z,j16           
        CP      ','             ; was last char a comma?
        RET     NZ              
j16:    EX      DE,HL           ; if space or comma then
        CALL    getHex          ; get second hex input
        EX      DE,HL           ; 1st input in HL, 2nd in DE
        RET     

; Param3 gets three parameters from user: start address, end address, and X.
; At the end, HL contains start address, DE contains X, and BC contains byte count.
;
param3: CALL    param2          ; get start addr in HL and count in BC
        JP      j16             ; get 3rd parameter in DE

;getHD2 is like getHD, with second optional parameter in DE.
;
getHD2: CALL    getHex          ; get 1st parameter in HL
        LD      DE,0000         ; init DE
        JP      j08             ; if 2nd parameter given, put in DE


; Lookup searches the command table
; call with A = command that is being searched for
; on return, zero flag set if not found
; otherwise, HL contains the command address
;
lookup: LD      B,A             ; save entered command in B
j36:    LD      A,(HL)          ; get next command from table
        OR      A               ; 0 = end of table
        RET     Z               ; quit if command not found
        CP      B               ; command match?
        INC     HL              ; point to addr LSB
        JP      Z,j35           ; jump if found
        INC     HL              ; point to next table entry
        INC     HL  
        JP      j36             ; and keep searching
j35:    LD      B,(HL)          ; get LSB
        INC     HL              ; point to add MSB
        LD      H,(HL)          ; load MSB into H
        LD      L,B             ; load LSB into L
        OR      A               ; clear Z flag
        RET     

; SERIAL PORT (12/13) INPUT 
serIn:  IN      A,(13H)         ; check status port 13h
        AND     02H             ; look at ready bit
        RET     Z               ; return if not ready
        IN      A,(12H)         ; ready, so get char from port 12h
        RET     

; SYSTEM SERIAL PORT (2/3) INPUT 
sysIn:  IN      A,(CONSTAT)     ; check status port
        AND     02H             ; look only at bit 1
        RET     Z               ; return if not ready
        IN      A,(CONIO)       ; ready, so get char from port 2
        RET     

; PARALLEL PORT (14/15) INPUT
parIn:  IN      A,(15H)         ; get status byte from port 15h
        AND     02h             ; look only at bit 1
        RET     Z               ; not ready
        IN      A,(14H)         ; read byte from port 14h
        RET     

; SERIAL PORT (12/13) OUTPUT 
serOut: IN      A,(13H)         ; get status byte
        AND     01H             ; look only at bit 1
        JP      Z,serOut        ; not ready
        LD      A,(m08)         ; load byte from memory
        OUT     (12H),A         ; and send it to port 12h
        RET     

; SYSTEM SERIAL PORT (2/3) OUTPUT 
sysOut: IN      A,(CONSTAT)     ; check status byte
        AND     01H             ; look only at bit 0
        JP      Z,sysOut        ; not ready
        LD      A,(m08)         ; load byte from memory
        OUT     (CONIO),A       ; and send it to port 2
        RET     

; PARALLEL IN/ SYSTEM SERIAL OUT
piso:   IN      A,(15H)         ; check parallel (kbd) status
        AND     01H             ; look at only bit 0
        JP      Z,piso          ; not ready
        LD      A,(m08)         ; get char to output
        OUT     (CONIO),A       ; and send to port 2
        RET     

; getCh1 waits for a character, looking at all input devices
;
getCh1: CALL    getCh           ; look for character at input(s)
        JP      Z,getCh1        ; wait until char is received
        RET     

; getCh looks for character input from all input devices
; and returns with char in A, if found.  
; Zero flag is set if no devices have input
;
getCh:  LD      A,(m04)         ; m04 contains input device list
        AND     02H             ; if bit 1 set,
        CALL    NZ,parIn        ; look for parallel (kbd) input
        RET     NZ              ; return if char received
        LD      A,(m04)         
        AND     04H             ; if bit 2 set,
        CALL    NZ,sysIn        ; look at system ports 2/3
        RET     NZ              ; return if char received
        LD      A,(m04)
        AND     01H             ; if bit 0 set,
        CALL    NZ,serIn        ; look at serial port 12/13h
        RET                     

        CALL    crlf            ; nice idea before putStr, but never used!

; putStr write a null-terminated ASCII string, pointed to by HL, 
; to all output devices.
;
putStr: LD      A,(HL)          ; load next char in string      
        OR      A               ; is it end-of-string NULL?
        RET     Z               ; if so, we are done
        PUSH    BC              ; save BC (why? video driver?)
        LD      B,A             ; save char in B (video driver?)
        CALL    putCh           ; send char to output devices
        POP     BC              ; restore BC
        INC     HL              ; point to next character in string
        JP      putStr          ; and loop until done

; putCh outputs a character in A to the designated output device(s), according to
; the device list found at m05
;
putCh:  LD      (m08),A         ; save character to send out
        PUSH    HL
        LD      HL,m05              
        LD      A,(HL)          ; get output device list
        AND     01H             ; if bit 0 set, 
        CALL    NZ,serOut       ; send byte to serial port 12h
        LD      A,(HL)
        AND     02H             ; if bit 1 set,
        CALL    NZ,piso         ; send byte to kbd/sys console
        LD      A,(HL)
        AND     04H             ; if bit 2 set,
        CALL    NZ,sysOut       ; send byte to system port 02h
        LD      A,(HL)
        AND     10H             ; if bit 4 set,
        LD      A,(m08)
        CALL    NZ,VIOOUT       ; send byte to video driver
        POP     HL
        RET     

; getCh2 waits for a character, returning its upper-case value in A
; Special characters like <Enter>, <Esc>, Ctrl-C, Ctrl-Z are processed. 
;
getCh2: CALL    getCh1          ; wait for character input
        AND     7FH             ; strip off MSb (7-bit ASCII only)
        CP      0DH             ; is it <Enter>?
        JP      Z,crlf          ; if echo <CR><LF> to output devices
        CP      03H             ; is it Ctrl-C?
        JP      Z,wboot         ; yes, do a warm boot
        CP      15H             ; is it Ctrl-Z?
        JP      Z,wboot         ; yes, do a warm boot
        CALL    putCh           ; echo character to output devices
        CP      1BH             ; is it <Esc>?
        JP      NZ,j23          ; no, continue eval below
        CALL    getCh2          ; get 2nd char in escape sequence
        JP      getCh2 
j23:    CP      61H             ; is character upper case?
        RET     C               ; no, so return with it in A
        CP      7BH             
        RET     NC
        XOR     20H             ; convert input to upper case
        RET     

; crlf issues a two byte carriage return/line feed combo to output devices
; 
crlf:   LD      A,0DH
        CALL    putCh           ; output CR
        LD      A,0AH
        JP      putCh           ; output LF

; pHex prints the byte in A as a two-digit hexadecimal
; for example, value 0A8h is printed as two ASCII characters "A8"
;
pHex:   PUSH    AF              ; save value
        RRCA                    ; rotate first digit into lower 4 bits
        RRCA    
        RRCA    
        RRCA    
        CALL    j24             ; convert first digit to ASCII
        POP     AF              ; restore value & continue with 2nd digit
j24:    AND     0FH             ; consider only lower 4 bits
        ADD     A,90H           ; convert value to ASCII
        DAA                     ; via clever textbook routine
        ADC     A,40H
        DAA     
        JP      putCh           ; output ASCII character

; pHex16 prints the word in HL as a four-digit hexadecimal, followed by a space
; for example, value 0A8EEh is printed as two ASCII characters "A8EE "
;
pHex16: LD      A,H             ; get MSB
        CALL    pHex            ; and output ASCII for it
        LD      A,L             ; then get LSB
pHexSp: CALL    pHex            ; and output ASCII for it
space:  LD      A,20H           
        JP      putCh           ; print a space

; getHex reads hexadecimal characters from the console and stops when a
; non-hex digit is encountered.  The input could be any number of characters.
; On exit, HL contains the value of the entered value, and A contains non-hex char
; that stopped the routine.  
;
getHex: LD      HL,0000         ; get console input into HL, default 0
j39:    CALL    getCh2          ; get next character with processing
        PUSH    AF              ; save entered character
        CALL    isHex           ; is char a hex digit?
        JP      NC,j22          ; process it if yes
        POP     AF              ; restore entered char in A
        RET                     ; return on first non-hex input

j22:    ADD     HL,HL           ; HL x 16 (shift it left one digit)
        ADD     HL,HL           ; x 4
        ADD     HL,HL           ; x 8
        ADD     HL,HL           ; x 16
        ADD     A,L         
        LD      L,A             ; after left-shift, add new digit in A
        POP     AF
        JP      j39             ; continue until non-hex input found

; isHex returned carry cleared for ASCII characters (A-F,0-9), but set otherwise
; On exit, A contains value of the character.  For example, reg A=0Ah for "A" input
;
isHex:  SUB     30H             ; is it a control/special char? 
        RET     C               ; yes, return with carry set
        CP      0AH             ; is it a numeric digit?
        JP      C,j26           ; yes, return with carry clear 
        SUB     11H             ; is it ':' thru '@'?
        RET     C               ; yes, return with carry set
        ADD     A,0AH           ; convert A->10h; B->11h; etc.
        CP      10H             ; set carry for A-F
j26:    CCF                     ; clears carry for A-F
        RET     

; show AW displays an address (HL-1) and its word contents
;
showAW: CALL    showAB          ; display address (HL-1) & its byte contents
        LD      A,(HL)
j64:    CALL    pHex            ; and also contents at next address
        RET     

; showAB displays an address (HL-1) and its byte contents
;
showAB: CALL    crlf            ; new line
j71:    DEC     HL
        CALL    pHex16          ; display an address (HL-1)
        LD      A,(HL)
        CALL    pHexSp          ; and its contents, followed by space
        INC     HL
        RET     

;  Baud Table #1: timing values and associated baud rates
;  used for automatic baud rate determination
;
baudT1: .db  03h, 96h           ; 9600 baud
        .db  06h, 48h           ; 4800 baud
        .db  0Bh, 24h           ; 2400 baud
        .db  17h, 12h           ; 1200 baud
        .db  2Eh, 06h           ; 600 baud
        .db  5Eh, 03h           ; 300 baud
        .db 0FFh, 01h           ; 110 baud
        .db 00h

; Baud Table #2: rates "96"=9600, etc and their associated timer0 
; count values, which are calculated as count = 2000000/(16*baud)
;
baudT2: .db 96h \ .dw 000Dh     ; 9600 baud
        .db 01h \ .dw 0470h     ;  110 baud
        .db 03h \ .dw 01A0h     ;  300 baud
        .db 48h \ .dw 001Ah     ; 4800 baud
        .db 24h \ .dw 0034h     ; 2400 baud
        .db 12h \ .dw 0068h     ; 1200 baud
        .db 06h \ .dw 00D0h     ;  600 baud
        .db 00h  

; Table of Monitor commands and their jump vectors:
;     
cmdTbl: .db 'A' \ .dw cmdA      ; Align cassette recorder
        .db 'B' \ .dw cmdB      ; Boot from Floppy Disk
        .db 'C' \ .dw cmdC      ; Call routine in memory
        .db 'D' \ .dw cmdD      ; Display memory
        .db 'E' \ .dw cmdE      ; Examine & modify memory
        .db 'F' \ .dw cmdF      ; Fill memory
        .db 'G' \ .dw cmdG      ; Generate Sync stream
        .db 'H' \ .dw cmdH      ; Load Intel Hex tape
        .db 'I' \ .dw cmdI      ; Port Input
        .db 'J' \ .dw cmdJ      ; Jump to Memory
        .db 'K' \ .dw cmdK      ; Kill ROM and Jump
        .db 'L' \ .dw cmdL      ; Load Cassette file
        .db 'M' \ .dw cmdM      ; Copy Memory
        .db 'N' \ .dw cmdN      ; Change disk descriptor address
        .db 'O' \ .dw cmdO      ; Port Output
        .db 'P' \ .dw cmdP      ; Protect Memory
        .db 'Q' \ .dw cmdQ      ; Switch-off ROM and Jump to VIO monitor
        .db 'R' \ .dw cmdR      ; Read from Diskette
        .db 'S' \ .dw cmdS      ; Search memory
        .db 'T' \ .dw cmdT      ; Test memory
        .db 'U' \ .dw cmdU      ; Unprotect memory
        .db 'V' \ .dw cmdV      ; Verify Memory
        .db 'W' \ .dw cmdW      ; Write to Diskette
        .db 'X' \ .dw cmdX      ; Execute cassette file
        .db 'Y' \ .dw cmdY      ; Change Direct I/O address
        .db 'Z' \ .dw cmdZ      ; Set baud rate
        .db ':' \ .dw cmd2      ; secondary commands follow
cmdTbl2:.db 'D' \ .dw cmdD2     ; change FD drive type
        .db 'I' \ .dw cmdI2     ; change char input device
        .db 'O' \ .dw cmdO2     ; change char output device
        .db 00                  ; end of table marker   

        .db "(C)1978 "
str4:   .db "IMSAI MPU-B MONITOR    VERS 1.3",0
str5:   .db "HIT SPACE BAR",13,10,0
str6:   .db "INVALID BAUD",0
str7:   .db "BAUD SERIAL",13,10,0
str8:   .db "PARALLEL",13,10,0
        .db 0, 42h, 23h
.END