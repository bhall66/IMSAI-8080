;
;   Title:   rom04asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   05 Jul 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler using Z80 mnemonics (-80 option)
;            
;   Descr:   Test for disk presence, using low-level calls.
;            if disk found, load cp/m boostrap into 0000-007F,
;            and jump there.
;            ...and... make it all CP/M compatible.
;            

DDA         .EQU    0080h       ; disk descriptor address
DMA         .EQU    0000h       ; disk DMA address
FDC         .EQU    0FDh
LED         .EQU    0FFh

.ORG    0100h

            LD   SP,1000h       ; establish local stack
            CALL initSIO        ; initialize serial ports 2 & 22h
            LD   HL,st0
            CALL putStr         ; print title message
            CALL initDD         ; establish disk descriptor

j02:        LD   HL,st1
            CALL putStr         ; print "Press any key: "
            CALL waitCh         ; wait for user input 
            CP   3              ; was it Ctrl-C?
            JP   Z,cboot        ; yes, so return via cold boot 
            CALL fdExec         ; try a read disk command
            SUB  1              ; disk read success=1?
            JP   NZ,j03         ; nope, so flag & try again
            LD   HL,st3
            CALL putStr         ; print "booting from Drive A"
            JP   0000h          ; jump to bootstrap code!
j03:        LD   HL,st2        
            CALL putStr         ; print "disk not found"
            JP   j02            ; loop

cboot:      LD   HL,(0001)      ; get warm boot address
            LD   BC,3           ; cold boot = wb - 3
            SBC  HL,BC          
            JP   (HL)           ; jump to cold boot


waitCh:     IN   A,(3)          ; check status
            AND  2              ; look @ ready bit only
            JP   Z,waitCh       ; wait for a character
            IN   A,(2)          ; read the character
            RET 


initDD:     LD   HL,cmdStr      ; copy "Read Disk0/Tr0/Sec1" to descriptor
            LD   DE,DDA
            LD   BC,7
            LDIR
            LD   HL,DMA         ; get DMA address
            LD   A,L 
            LD   (DDA+5),A      ; add in LSB of DMA to descriptor
            LD   A,H 
            LD   (DDA+6),A      ; add in MSB of DMA to descriptor

            LD   A,10h          ; command 10h = point to disk descriptor
            OUT  (FDC),A 
            LD   HL,DDA         ; get descriptor address
            LD   A,L            ; address LSB
            OUT  (FDC),A 
            LD   A,H            ; address MSB 
            OUT  (FDC),A 
            RET 

fdExec:     XOR  A              ; command 0 = use descriptor 
            LD   (DDA+1),A      ; erase any previous result
            OUT  (FDC),A        ; execute the command string
j01:        LD   A,(DDA+1)      ; look at result code
            OR   A              ; still zero?
            JP   Z,j01          ; wait until response received
            CPL                 
            OUT  (LED),A        ; show result on FP LEDs.
            CPL                 ; restore value
            RET 


initSIO:    LD   A,0CAh         ; 7 BITS, NO PARITY, 2 STOP
            OUT  (3),A          ; UART A - SET MODE 
            OUT  (23h),A        ; UART B - SET MODE
            LD   A,027H         ; ENABLE TX & RX
            OUT  (3),A          ; UART A - SET CONTROL
            OUT  (23h),A        ; UART B - SET CONTROL
            RET

; putStr write a null-terminated ASCII string, pointed to by HL, 
; to the console
;
putStr:     LD      A,(HL)      ; load next char in string      
            OR      A           ; is it end-of-string NULL?
            RET     Z           ; if so, we are done
            OUT     (2),A       ; send char to output devices
            INC     HL          ; point to next character in string
            JP      putStr      ; and loop until done


st0:        .db "Disk in Drive A?",13,10,13,10,0
st1:        .db "Press any key: ",0
st2:        .db "no disk.",13,10,0
st3:        .db 13,10,"Booting from drive A...",13,10,0
cmdStr      .db 21h,00,00,00,01,00,00h

.END