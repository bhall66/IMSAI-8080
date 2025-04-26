;
;   Title:   ledsOff.asm
;  Author:   Bruce E. Hall, w8bh
;    Date:   25 Apr 2025
;      HW:   IMSAI8080 emulator by TheHighNibble
;      SW:   TASM compiler, CP/M 2.2 OS
;
;   Descr:   Ever notice that the "Programmed Output" LEDS on
;            the front panel are always on, drawing current?
;
;            This small program turns off those LEDs,
;            thereby reducing current use by about 13 mA.
;
;            Consider adding it to your "profile.sub" file, so
;            that it runs whenever CP/M is booted.
            
.ORG 100H

start:	
   XOR  A               ; start with all LEDs on    
left:	
   OUT  (0FFh),A        ; update output LEDs
   CALL	delay           ; let's animate it for fun!
   SLA	A	            ; shift pattern left 
   SET  0,A             ; turn off leftmost LED
   JP	NC,left         ; do it until all LEDS off
   RET                  ; return to CP/M

delay:
   PUSH AF
   LD   BC,200
d1:
   LD   D,200           ; 200*20 = 4K cycles = 1mS @ 4 MHz
d2:                     
   NOP                  ; 4 cycles
   DEC  D               ; + 4 
   JR   NZ,d2           ; + 12 = 20 cycles in inner loop
   DEC  BC              ; count the milliseconds
   LD   A,B             ; DEC BC doesnt set z flag
   OR   C               ; so need LD/OR to do the job
   JR   NZ,d1           ; wait another millisecond
   POP  AF
   RET

.END 
