;
;   Title:   mapper.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   22 Apr 2025
;      HW:   IMSAI 8080esp by TheHighNibble
;      SW:   TASM compiler, CPM 2.2
;
;   Descr:   Displays a ROM/RAM memory map in color
;            Uses CPM BDOS calls.
;


.ORG 100H

ramTest:
    LD   HL, stMapper   ; "Memory Mapper..."
    CALL printStr      
    LD   DE, $0800      ; page size of 2K
    LD   B, 32          ; eval 32 pages of memory (64K)
    LD   HL, $0000      ; starting at $0000
rt01:
    LD   A, H           ; look at H register
    AND  $1F            ; are we on an 8K boundary?
    JR   NZ, rt02       ; if not, continue
    LD   A, ' '         ; otherwise, print space
    CALL printCh
rt02:
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

printCh:
    PUSH AF
    PUSH BC             ; save current registers
    PUSH DE 
    PUSH HL
    LD   C, 2           ; BDOS function 2 = CHARACTER OUTPUT
    LD   E, A           ; load E with character to be displayed
    CALL 5              ; call BDOS
    POP  HL             ; restore registers after BDOS call
    POP  DE
    POP  BC
    POP  AF
    RET  

printStr:
    LD   A, (HL)        ; get character in the string
    OR   A              ; is it zero?
    RET  Z              ; if it is, we are done.
    CALL printCh
    INC  HL             ; move to next character
    JR   printStr       ; and loop until done


stMapper:
    .db 13,10
    .db "  Z80 Memory Mapper         R=ROM, W=RAM"   ,13,10,13,10
    .db "+-----------------------------------------+",13,10
    .db "| 0    8    16   24   32   40   48   56K  |",13,10
    .db "+-----------------------------------------+",13,10
    .db "|",0
stEndmap:
    .db 27,"[39m",27,"[49m"," |",13,10   ; restores default FG/BG colors
    .db "+-----------------------------------------+",13,10
    .db 13,10,0

stROM:
    .db 27,"[41m",27,"[97m",'R',0   ; show white 'R' on red background
stRAM:
    .db 27,"[44m",27,"[97m",'W',0   ; show white 'W' on blue background

.END

