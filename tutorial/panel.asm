;
;   Title:   panel.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   15 Apr 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler using Z80 mnemonics (-80 option)
;
;   Descr:   Display the state of 8 front panel switches 
;            on the front panel LEDs.
;

.ORG $0100 
START:  IN       A,(0FFH)   ; read front panel switches
        CPL                 ; invert bits in register A
        OUT     (0FFH),A    ; Put it on the output LEDs
        JP      START       ; loop forever
.END