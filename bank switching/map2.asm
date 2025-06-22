;
;   Title:   map2.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   22 June 2025
;      HW:   IMSAI 8080esp by TheHighNibble
;      SW:   TASM compiler, CPM 2.2
;
;   Descr:   use the MPU-B control byte to configure memory
;            and display the memory map for 4 different settings
;          
;   Usage:   "DDT map2.hex", then "g8000".  System will halt. 


.ORG 8000H              ; must stay in RAM
MBCP    .EQU 0F3h       ; MPU-B Control Port
BOOTROM .EQU 0D800H     ; bootrom address

start:
    LD   SP,8400h
    LD   HL, stTitle    ; "MPU-B Control Byte Test..."
    CALL printStr 

 ; now try 4 different memory configurations
 ; and show the map for each one.  

    LD   A,40h          ; 40h: 0000 RAM, D800 ROM
    OUT  (MBCP),A       ; change memory configuration
    CALL map            ; and show it

    LD   A,80h          ; 80h: 0000 ROM, D800 RAM
    OUT  (MBCP),A       ; change memory configuration
    CALL map            ; and show it

    LD   A,0C0h         ; C0h: 0000 RAM, D800 RAM
    OUT  (MBCP),A       ; change memory configuration
    CALL map            ; and show it

    LD   A,0            ; 00h: 0000 ROM, D800 ROM
    OUT  (MBCP),A       ; change memory configuration  
    CALL map            ; and show it
    
    LD   HL,stReboot    ; OS in memory is trashed, so...
    CALL printStr       ; announce reboot
    JP   BOOTROM        ; via Bootloader in ROM


map:
    PUSH AF             ; temp save value in A
    LD   HL, stPrefix   ; start output line
    CALL printStr
    POP  AF             ; retrieve control byte value
    CALL printHex       ; and display it
    LD   DE, 400H       ; set memory page size = 1K
    LD   B, 64          ; eval 64 pages of memory (64K)
    LD   HL, 0000       ; starting at 0000h
rt01:
    LD   C, (HL)        ; temp save memory contents
    LD   A, $DC         ; use test byte $DC
    LD   (HL), A        ; try saving the byte
    CP   (HL)           ; then read it back & compare 
    LD   (HL), C        ; restore memory contents
    PUSH HL             ; save memory pointer
    JR   Z, foundRAM    ; if same, RAM was written correctly
foundROM:
    LD   HL, stROM      ; ROM found, so print 'R'
    JR   rt03
foundRAM:
    LD   HL, stRAM      ; RAM found, so print 'W'
rt03:
    CALL printStr       ; print result 
    POP  HL             ; restore memory pointer
    ADD  HL, DE         ; point to next memory page
    DJNZ rt01           ; and loop until done
    LD   HL, stPostfix  ; finish output
    CALL printStr
    RET                

printStr:
    LD   A, (HL)        ; get character in the string
    OR   A              ; is it zero?
    RET  Z              ; if it is, we are done.
    OUT  (2),A          ; send character to console
    INC  HL             ; move to next character
    JR   printStr       ; and loop until done

printHex:
    PUSH AF 
    CALL bh1            ; first char = upper nibble
    POP  AF
    CALL bh2            ; second char = lower nibble
    LD   A,' '
    OUT  (2),A      
    OUT  (2),A          ; add two spaces at end.   
    RET
bh1:	                ; shift right 4 bits
    RRA
    RRA
    RRA
    RRA
bh2:	                ; convert lower nibble to Hex
    OR	  $F0
    DAA
    ADD  A, $A0
    ADC  A, $40
    OUT  (2),A          ; send character to console
    RET

stTitle:
    .db 13,10
    .db "  MPU-B Control Byte Test        R=ROM, W=RAM"   ,13,10,13,10
    .db "|      0000111122223333444455556666777788889999AAAABBBBCCCCDDDDEEEEFFFF |",13,10
    .db "| BYTE 048C048C048C048C048C048C048C048C048C048C048C048C048C048C048C048C |",13,10,0
stPrefix:
    .db "|  ",0
stPostfix:
    .db 27,"[39m",27,"[49m"," |",13,10,0   ; restores default FG/BG colors
stReboot:
    .db 13,10,"Rebooting CP/M...",13,10,0
stROM:
    .db 27,"[41m",27,"[97m",'R',0   ; show white 'R' on red background
stRAM:
    .db 27,"[44m",27,"[97m",'W',0   ; show white 'W' on blue background
.END