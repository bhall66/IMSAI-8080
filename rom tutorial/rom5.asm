;
;   Title:   rom5.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   11 Jul 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler using Z80 mnemonics (-80 option)
;            
;   Descr:   Minimal ROM to boot a system disk.  ROM banking supported.
;            Auto-boot supported.
;
;            If no disk in Drive A, tells user to mount a system disk.
;            Code written to work in 8080 & Z80 environments.    
            
DDA         .EQU    0080h       ; disk descriptor address
MCP         .EQU    0F3h        ; MPU-B control (banked ROM) port
FDC         .EQU    0FDh        ; Floppy disk controller (FIF) port

.ORG    0D800H

start:  LD      A,40H           
        OUT     (MCP),A         ; turn off ROM at 0000-07FF
        JP      j01             ; allow auto-boot from 0000
j01:    LD      SP,0D780h       ; set up monitor stack 
        CALL    initSIO         ; initialize serial port
j02:    CALL    readFD          ; read disk (1=success)
        DEC     A
        JP      Z,boot          ; disk was read, so jump to 0000.
        LD      HL,noDisk
        CALL    putStr          ; "Put disk in Drive A..."
        CALL    getCh           ; wait until char is pressed
        JP      j02             ; and try again.

boot:   LD      A,0C0H          
        OUT     (MCP),A         ; remove ROM from memory map
        JP      0000h           ; and boot to CP/M

; Set up a command to read the disk, execute the command, and receive result code 
;
readFD: LD      HL,DDA          ; HL = disk descriptor address
        LD      DE,cmdStr       ; copy of disk descriptor in scratchpad
        LD      B,7             ; copie 7-byte descriptor to its address               
j03:    EX      DE,HL
        LD      A,(HL)          ; get a byte
        EX      DE,HL
        LD      (HL),A          ; and store it, 8080-style
        INC     HL              
        INC     DE
        DEC     B
        JP      NZ,j03          ; loop until done.

        ; the disk descriptor is now initialized with a command
        ; to read Drive 0/Track 0/Sector 1 to Address 0000.
        ; Now inform controller where that command is located.

        LD      A,10H           ; FDC cmd 10h = set disk descriptor
        OUT     (FDC),A         ; and sent command
        LD      HL,DDA 
        LD      A,L
        OUT     (FDC),A         ; send LSB of disk descriptor
        LD      A,H
        OUT     (FDC),A         ; send MSB of disk descriptor

        ; Tell the FDC to execute the command string

        XOR     A               ; cmd 0 = execute the command string
        OUT     (FDC),A         ; send it

        ; Poll the result byte & return when reponse is received

        LD      HL,DDA+1        ; get disk descriptor address
j04:    LD      A,(HL)          ; look at result code
        OR      A               ; still zero = no response?
        JP      Z,j04           ; wait until response received
        RET  

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

; initialize serial port
;
initSIO:    LD   A,0CAh         ; 7 BITS, NO PARITY, 2 STOP
            OUT  (3),A          ; UART A - SET MODE 
            LD   A,027H         ; ENABLE TX & RX
            OUT  (3),A          ; UART A - SET CONTROL
            RET

noDisk:     .db 13,10,"Put a system disk in Drive A and press any key.",13,10,0
cmdStr:     .db 21h,00,00,00,01,00,00h

.END