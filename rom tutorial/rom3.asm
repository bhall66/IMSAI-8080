;
;   Title:   rom3.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   11 Jul 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler using Z80 mnemonics (-80 option)
;            
;   Descr:   ROM exercise - read the CP/M 2.2 bootloader
;            and jump to it.  ROM banking support added.  
;
;            Compatible with any CP/M 2.2 system disk.
;         
            
DDA         .EQU    0080h       ; disk descriptor address
MCP         .EQU    0F3h        ; MPU-B Memory Control port
FDC         .EQU    0FDh        ; Floppy disk controller (FIF) port
LED         .EQU    0FFh        ; front panel LED port

.ORG    0D800H

start:  LD      A,40H           
        OUT     (MCP),A         ; turn off ROM at 0000-07FF
readFD: LD      HL,cmdStr       ; source = command string
        LD      DE,DDA          ; destination = 0080h
        LD      BC,7            ; 7 bytes to copy  
        LDIR                    ; do the copy, Z80 style            

        ; the disk descriptor is now initialized with a command
        ; to read Drive 0/Track 0/Sector 1 to Address 0000.
        ; Now inform controller where that command is located.

        LD      A,10H           ; cmd 10h = set disk descriptor
        OUT     (FDC),A         ; sent it
        LD      HL,DDA 
        LD      A,L
        OUT     (FDC),A         ; send LSB of disk descriptor
        LD      A,H
        OUT     (FDC),A         ; send MSB of disk descriptor

        ; Tell the FDC to execute the command string

        XOR     A               ; cmd 0 = execute the command string
        OUT     (FDC),A         ; send it

        ; Poll the result byte & return when response is received

        LD      HL,DDA+1        ; point to result code
loop:   LD      A,(HL)          ; get result
        OR      A               ; still zero = no response?
        JP      Z,loop          ; wait until response received

        ; Show controller response code on the front panel LEDs
        ; if read was successful, boot to CP/M

        CPL                     ; complement A for LED output
        OUT     (LED),A         ; output code to front panel LEDs
        CPL                     ; restore A
        DEC     A               ; result code 1 = success?
        JP      Z,boot          ; yes, so do the boot!
stop:   JP      stop            ; otherwise stop

boot:   LD      A,0C0H          
        OUT     (MCP),A         ; remove ROM from memory map
        JP      0000h           ; and boot to CP/M

cmdStr:     .db 21h,00,00,00,01,00,00h
.END