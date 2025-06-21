;
;   Title:   map.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   20 June 2025
;      HW:   IMSAI 8080esp by TheHighNibble
;      SW:   TASM compiler, CPM 2.2
;
;   Descr:   Displays a color ROM/RAM memory map
;


.ORG 100H

start:
    LD   HL, stMapper   ; "Memory Mapper..."
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
    LD   HL, stEndmap   ; finish output
    CALL printStr
    RET                 ; return to OS

printStr:
    LD   A, (HL)        ; get character in the string
    OR   A              ; is it zero?
    RET  Z              ; if it is, we are done.
    OUT  (2),A          ; send character to console
    INC  HL             ; move to next character
    JR   printStr       ; and loop until done

stMapper:
    .db 13,10
    .db "  W8BH Memory Mapper 1.0        R=ROM, W=RAM"   ,13,10,13,10
    .db "| 0000111122223333444455556666777788889999AAAABBBBCCCCDDDDEEEEFFFF |",13,10
    .db "| 048C048C048C048C048C048C048C048C048C048C048C048C048C048C048C048C |",13,10
    .db "| ",0
stEndmap:
    .db 27,"[39m",27,"[49m"," |",13,10   ; restores default FG/BG colors
    .db 13,10,0
stROM:
    .db 27,"[41m",27,"[97m",'R',0   ; show white 'R' on red background
stRAM:
    .db 27,"[44m",27,"[97m",'W',0   ; show white 'W' on blue background

.END