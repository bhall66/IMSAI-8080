;
;   Title:   map4.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   18 June 2025
;      HW:   IMSAI 8080esp by TheHighNibble
;      SW:   TASM compiler, CPM 2.2
;
;   Descr:   use the MPU-B control byte to configure memory
;            and display the memory map for 4 different settings
;          
;   Usage:   "DDT map4.asm", then "g8000".  System will halt. 


.ORG 8000H              ; must stay in RAM
MBCP .EQU 0F3h          ; MPU-B Control Port

start:
    LD   SP,8400h
    LD   HL, stTitle    ; "MPU-B Control Byte Test..."
    CALL printStr 

 ; now try 4 different memory configurations
 ; and show the map for each one.  

    LD   A,0            ; 00h: 0000 ROM, D800 ROM
    OUT  (MBCP),A       ; change memory configuration  
    CALL map            ; and show it

    LD   A,40h          ; 40h: 0000 RAM, D800 ROM
    OUT  (MBCP),A       ; change memory configuration
    CALL map            ; and show it

    LD   A,80h          ; 80h: 0000 ROM, D800 RAM
    OUT  (MBCP),A       ; change memory configuration
    CALL map            ; and show it

    LD   A,0C0h         ; C0h: 0000 RAM, D800 RAM
    OUT  (MBCP),A       ; change memory configuration
    CALL map            ; and show it

    LD   HL,stHalted    
    CALL printStr
    HALT                ; OS trashed, need cold reset


map:
    LD   HL, stPrefix   ; start output line
    CALL printStr
    LD   DE, 400H       ; page size of 1K
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


stTitle:
    .db 13,10
    .db "  MPU-B Control Byte Test        R=ROM, W=RAM"   ,13,10,13,10
    .db "| 0000111122223333444455556666777788889999AAAABBBBCCCCDDDDEEEEFFFF |",13,10
    .db "| 048C048C048C048C048C048C048C048C048C048C048C048C048C048C048C048C |",13,10,0
stPrefix:
    .db "| ",0
stPostfix:
    .db 27,"[39m",27,"[49m"," |",13,10,0   ; restores default FG/BG colors
stROM:
    .db 27,"[41m",27,"[97m",'R',0   ; show white 'R' on red background
stRAM:
    .db 27,"[44m",27,"[97m",'W',0   ; show white 'W' on blue background
stHalted:
    .db 13,10,"System Halted.",0
.END