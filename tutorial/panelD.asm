;
;   Title:   panelD.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   15 Apr 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   Digital Research Assembler (ASM)
;
;   Descr:   Display the state of 8 front panel switches 
;            on the front panel LEDs.
;

ORG 100H
START:  IN   0FFH    
        CMA           
        OUT  0FFH    
        JMP  START  
END