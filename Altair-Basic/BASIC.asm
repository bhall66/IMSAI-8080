; ----------------------------------------------------------------------------
; Micro-Soft Altair BASIC 3.2 (4K) - Annotated Disassembly
;   
; Copyright 1975, Bill Gates, Paul Allen, Monte Davidoff
; Source: http://altairbasic.org/ compiled by Reuben Harris
; Additional cleanup, relocation by Charles Mangin, March, 2019
; Comments and IMSAI 8080 port by Bruce Hall, August 2025
; ----------------------------------------------------------------------------

;
; Introductory comments by Bruce Hall
;
; The Version 3.0 printout dated 09/10/75, released by Bill Gates in April 2025, contains
; the following text:
;
;    "Originally written on the PDP-10 from February 9 to April 9.
;     Bill Gates wrote the runtime stuff.
;     Paul Allen wrote the non-runtime stuff.
;     Monte Davidoff wrote the math package."
;
; Bill Gates fondly remembers Altair BASIC as "The coolest code I have ever written".
;
; This code is heavily commented.  Many comments I added come from altairbasic.org. 
; I also added comments, where appropriate, from the original source code which was not
; available at the time of disassembly.  Note that most of the labels, including names of
; routines, are from the disassembly and therefore do not represent labels found in the 
; original source code.  This is not at all bad, as the original labels were limited to 
; 6 characters and are often less descriptive.
;
; The text of the program is stored in memory immediately after BASIC.  PROGRAM_BASE is a 
; pointer to the start of the text.  Register pair HL is commonly used to point to the byte
; of text currently being evaluated.
;
; Each line of a user program is tokenized, with all reserved words and operators converted
; into 1-byte tokens.  Other text, such as variable names and strings, are stored as is.
;
; Each program line is stored in memory as follows:
;   1. a pointer to the next line of text (2 bytes)
;   2. the line number in binary form (2 bytes)
;   3. the tokenized text, followed by a null byte
;
; End-of-program is marked by a double-zero.  The pointer in the last line points to this
; double-zero.  
;
; Program variables are stored after the text, 6 bytes per variable.  The start of variable 
; storage is pointed to by VAR_BASE.  Arrayed variable follow and are pointed to by 
; VAR_ARRAY_BASE. The byte following all variable storage is pointed to by VAR_TOP
;
; A good description of the floating point representation can be found at altairbasic.org.  
; Numbers are generally stored in a 4-bytes floating point format, where the mantissa is 
; represented by 23 bits plus a sign bit, and the exponent is represented by 8 bits.  Numbers 
; are stored in a 4-byte memory location called the floating point accumulator (FAC), on the 
; stack, or in registers BCDE.  
; 

CON  .EQU 02                    ;!!! IMSAI console port
CONS .EQU 03                    ;!!! IMSAI console status port

     .ORG   00

Start   
    DI                          ; disable interrupts
    JMP Init                    ; init I/O, removing unneeded functions
                                ; the jump location will be changed after INIT done.

    .DW 0490h   
    .DW 07F9h

; SyntaxCheck looks at the current character to make sure it is a specific thing
; (contained in the location after the call).  If not it calls the syntax error
; routine.  Otherwise it gobbles the next character and returns (by falling into NextChar)
; HL ends up pointing to the character after the one which was checked      
;
SyntaxCheck                     ;RST 1 @ 0008
    MOV A,M                     ; A=Byte of BASIC program.
    XTHL                        ; HL=return address.
    CMP M                       ; Compare to byte expected.
    INX H                       ; Return address++;
    XTHL    
    JNZ SyntaxError             ; Error if not what was expected.

; NextChar returns the next character in buffer at HL, skipping over space characters. 
; Condition codes are set according to what is in A: 
; Carry C = Numberic (0..9) 
; Zero Z = ":" or null 00h
;
NextChar                        ;RST 2 @ 0010
    INX H                       ; advance pointer to next character
    MOV A,M                     ; retrieve it
    CPI $3A                     ; is it >= ':'?
    RNC                         ; alpha character, so done
    JMP NextChar_tail           ; must eval further 

; OutChar prints the character to terminal, handling terminal width
;
OutChar                         ;RST 3 @ 0018
    PUSH PSW    
    LDA TERMINAL_X  
    JMP OutChar_tail    
    NOP 

; 16-bit compare of HL and DE registers, setting C/Z flags accordingly
; Carry set for HL < DE
; Zero set for HL = DE
;
CompareHLDE                     ;RST 4 @ 0020   
    MOV A,H 
    SUB D   
    RNZ 
    MOV A,L 
    SUB E   
    RET 

; Variables for x and y positions of the terminal output
; 
TERMINAL_Y  .DB 01              ; not used in IMSAI version
TERMINAL_X  .DB 00              ; horizontal character position


; Tests the state of FACCUM, returning code in A.
; A=0 if FAC=0
; A=1 if FAC>0
; A=-1 if FAC<0 
; Condition codes are set according to the value of A. 
;
FTestSign                       ;RST 5 @ 0028   
    LDA FACCUM+3                ; look at exponent first    
    ORA A                       ; if its zero, then FACCUM=0
    JNZ FTestSign_tail          ; otherwise, more to do...
    RET 

; Push the word HL is pointing to on stack, then incr. HL by 2.
; First we write the return address to the JMP instruction at the end of the function; 
; then we read the word at (HL) into BC and push it onto the stack; 
; lastly jumping to the return address.  
;
PushNextWord                    ;RST 6 @ 0030   
    XTHL                        ; get return address into HL
    SHLD L003A+1                ; put it after JMP instruction below
    POP H                       ; get HL back
    MOV C,M                     ; get word stored at (HL)    
    INX H   
    MOV B,M                     ; and move it into BC
    INX H   
    PUSH B                      ; push (HL) on stack
L003A   
    JMP L003A                   ; jump to the return address         

KW_INLINE_FNS                   ; Function Addresses:
    .DW Sgn                     ; for SGN 
    .DW Int                     ; for INT
    .DW Abs                     ; for ABS
    .DW FunctionCallError       ; for FCE 
    .DW Sqr                     ; for SQR 
    .DW Rnd                     ; for RND 
    .DW Sin                     ; for SIN

; THE OPERATOR TABLE
;
; This table contains an operators precedence, followed by the address of the routine
; to perform the operation.  The index into the operator table is made by subtracting
; off the crunch value of the lowest numbered operator.  The order of operators in the
; crunch list and in optable is identical.  The precedences are arbitary, except for
; their comparative sizes.  Note that the precedence for unary operators such as not
; and negation are set up specially without using a table
;
KW_ARITH_OP_FNS 
    .DB 79h
    .DW FAdd                    ; +
    .DB 79h
    .DW FSub                    ; -
    .DB 7Ch 
    .DW FMul                    ; *
    .DB 7Ch 
    .DW FDiv                    ; /


; THE RESERVED WORD OR "CRUNCH" LIST
; 
; When a command or program line is typed in it is stored in buffer.  As soon
; as the whole line has been typed in, CRUNCH is called to convert all reserved words
; to their crunch values.  This reduces the size of the program and speeds up execution
; by allowing table dispatches to perform functions, statements, and operations.  This is
; because all the statement names are stored consecutively in the crunch list.  When a 
; match is found between a string of characters and a word in the crunch list the entire
; text of the matched word is taken out of the input line and a reserved word token is
; put in its place.  A reserved word token is always equal to 0x80 plus the position of
; the matched word in the crunch list
;
KEYWORDS    
    .DB 45h,4Eh,0C4h            ; "END"     80
    .DB 46h,4Fh,0D2h            ; "FOR"     81
    .DB 4Eh,45h,58h,0D4h        ; "NEXT"    82
    .DB 44h,41h,54h,0C1h        ; "DATA"    83
    .DB 49h,4Eh,50h,55h,0D4h    ; "INPUT"   84
    .DB 44h,49h,0CDh            ; "DIM"     85
    .DB 52h,45h,41h,0C4h        ; "READ"    86
    .DB 4Ch,45h,0D4h            ; "LET"     87
    .DB 47h,4Fh,54h,0CFh        ; "GOTO"    88
    .DB 52h,55h,0CEh            ; "RUN"     89
    .DB 49h,0C6h                ; "IF"      8A
    .DB 52h,45h,53h,54h         
    .DB 4Fh,52h,0C5h            ; "RESTORE" 8B
    .DB 47h,4Fh,53h,55h,0C2h    ; "GOSUB"   8C
    .DB 52h,45h,54h,55h,52h,0CEh; "RETURN"  8D
    .DB 52h,45h,0CDh            ; "REM"     8E
    .DB 53h,54h,4Fh,0D0h        ; "STOP"    8F
    .DB 50h,52h,49h,4Eh,0D4h    ; "PRINT"   90
    .DB 4Ch,49h,53h,0D4h        ; "LIST"    91
    .DB 43h,4Ch,45h,41h,0D2h    ; "CLEAR"   92
    .DB 4Eh,45h,0D7h            ; "NEW"     93
    .DB 54h,41h,42h,0A8h        ; "TAB("    94
    .DB 54h,0CFh                ; "TO"      95
    .DB 54h,48h,45h,0CEh        ; "THEN"    96
    .DB 53h,54h,45h,0D0h        ; "STEP"    97
    .DB $AB                     ; "+"       98
    .DB $AD                     ; "-"       99
    .DB $AA                     ; "*"       9A
    .DB $AF                     ; "/"       9B
    .DB $BE                     ; ">"       9C
    .DB $BD                     ; "="       9D
    .DB $BC                     ; "<"       9E
    .DB 53h,47h,0CEh            ; "SGN"     9F
    .DB 49h,4Eh,0D4h            ; "INT"     A0
    .DB 41h,42h,0D3h            ; "ABS"     A1
    .DB 55h,53h,0D2h            ; "USR"     A2
    .DB 53h,51h,0D2h            ; "SQR"     A3
    .DB 52h,4Eh,0C4h            ; "RND"     A4
    .DB 53h,49h,0CEh            ; "SIN"     A5
    .DB $00                     ; End of List       

; STATEMENT DISPATCH ADDRESSES
; 
; When a statement is to be executed, the first character of the statement is examined
; to see if it less than the reserved word token for the lowest numbered statement name.
; If so, the "Let" code is called to treat the statement as an assignment statement.
; Otherwise a check is made to make sure the reserved word number is not too large to be 
; a statement number.  If not, the address to dispatch to is fetched from this table 
; using the reserved word number to calculate an index into the table
;
KW_GENERAL_FNS                  ; Function Addresses for:
    .DW Stop                    ; END
    .DW For                     ; FOR
    .DW Next                    ; NEXT
    .DW FindNextStatement       ; DATA
    .DW Input                   ; INPUT
    .DW Dim                     ; DIM
    .DW Read                    ; READ
    .DW Let                     ; LET
    .DW Goto                    ; GOTO
    .DW Run                     ; RUN
    .DW If                      ; IF
    .DW Restore                 ; RESTORE
    .DW Gosub                   ; GOSUB
    .DW Return                  ; RETURN
    .DW Rem                     ; REM
    .DW Stop                    ; STOP
    .DW Print                   ; PRINT
    .DW List                    ; LIST
    .DW Clear                   ; CLEAR
    .DW New                     ; NEW

; ERROR MESSAGES
;
; When an error condition is detected, Reg E must be set up to indicate which error
; message is appropriate and a branch must be made to error.  The stack will be reset
; and all program content will be lost.  Variable values and the actual program remain
; intact.  Only the value of Reg E is important when the the branch is made to Error.
; E is used as an index into the error table below which gives the two-character error
; meesage that will be printed on the user's terminal
;
ERROR_CODES 
    .DB 4Eh,0C6h                ; "NF"  NEXT without FOR.
    .DB 53h,0CEh                ; "SN"  Syntax Error
    .DB 52h,0C7h                ; "RG"  RETURN without GOSUB.
    .DB 4Fh,0C4h                ; "OD"  Out of Data
    .DB 46h,0C3h                ; "FC"  Illegal Function Call
    .DB 4Fh,0D6h                ; "OV"  Overflow.
    .DB 4Fh,0CDh                ; "OM"  Out of memory.
    .DB 55h,0D3h                ; "US"  Undefined Subroutine
    .DB 42h,0D3h                ; "BS"  Bad Subscript
    .DB 44h,0C4h                ; "DD"  Duplicate Definition
    .DB 2Fh,0B0h                ; "\0"  Division by zero.
    .DB 49h,0C4h                ; "ID"  Invalid in Direct mode.
     
; INTERNAL VARIABLES FOLLOW
;
    .DB ','                     ; A comma, used by INPUT statement since the data pointer
                                ; always starts on a comma or terminator
LINE_BUFFER 
    .DW 0000,0000,0000,0000h    ; 72 character input buffer.  What user types is stored here
    .DW 0000,0000,0000,0000h    ;   Direct statements also execute out of here.
    .DW 0000,0000,0000,0000h    ;   Called "BUF" in original code.
    .DW 0000,0000,0000,0000h    ;
    .DW 0000,0000,0000,0000h    ;
    .DW 0000,0000,0000,0000h    ;
    .DW 0000,0000,0000,0000h    ;
    .DW 0000,0000,0000,0000h    ;
    .DW 0000,0000,0000,0000h    ;       

DIM_OR_EVAL     .DB 00          ; flag
INPUT_OR_READ   .DB 00          ; flag
PROG_PTR_TEMP   .DW 0000        ; temporary variable
TEMP2           .DW 0000        ; temporary variable for formula eval, etc  
CURRENT_LINE    .DW 0000        ; current line #, =FFFF for direct commands
STACK_TOP       .DW 0F1Ah       ; set by init according to memory size 
PROGRAM_BASE    .DW 0000        ; start of user program storage
VAR_BASE        .DW 0000        ; start of variable storage area
VAR_ARRAY_BASE  .DW 0000        ; start of array variable storage
VAR_TOP         .DW 0000        ; end of variable storage area   
DATA_PROG_PTR   .DW 0000        ; pointer to data
FACCUM          .DB 00          ; Lowest order of mantissa, Floating pt accumulator (FAC)
                .DB 00          ; Middle order of mantissa
                .DB 00          ; Highest order of mantissa
                .DB 00          ; Exponent of FAC
FTEMP           .DB 00          
FBUFFER         .DW 0000,0000   ; floating point I/O buffer, used by FOut
                .DW 0000,0000
                .DW 0000,0000
                .DB 00  

szError .DB " ERROR",0          ;" ERROR"    
szIn    .DB " IN ",0            ;" IN"
szOK    .DB 13,"OK",13,0        ;" OK"   

; Find a "FOR" entry on the stack with the variable pointer passed in DE
;
; Whenever a FOR is executed an 18-byte entry is pushed onto the stack.  Before this is done,
; however, a check must be made to see if there are any FOR entries already on the stack
; for the same loop variable.  If so, that FOR entry and all other FOR entries that were made
; after it are eliminated from the stack.  This is so a program that jumps out of the middle
; of a FOR loop and then restarts the loop again and again won't use up 18 bytes of stack 
; every time.  The NEXT code also calls FindFor to search for a FOR entry with the loop
; variable in the NEXT, at whatever point a match is found the stack is reset.  If no match
; is found a "Next without For" error occurs.  GOSUB execution also puts a 6-byte entry on
; stack, when a return is executed FindFor is called with a variable pointer that can't be
; matched...when FindFor has run through all the FOR entries on the stack it returns and the
; return code makes sure the entry that was stopped on is a GOSUB entry.  This assures that
; if you GOSUB to a section of code in which a FOR loop is entered but never exited, the 
; return will still be able to find the most recent GOSUB entry.  The "return" code
; eliminates the GOSUB entry and all FOR entries made after the GOSUB entry.
;
FindFor 
    LXI H,0004h                 ; HL=SP+4 (ie get word
    DAD SP                      ; just past return addr)
    MOV A,M                     ; what type of thing is on the stack?
    INX H   
    CPI $81                     ; is the stack entry a 'FOR'?
    RNZ                         ; Return if not
    RST 6                       ; PushNextWord = PUSH (HL)
    XTHL                        ; POP HL (ie HL=(HL))
    RST 4                       ; CompareHLDE: HL==DE?
    LXI B,000Dh                 ; to wipe out a For entry 
    POP H                       ; Restore HL
    RZ                          ; if for matches, good
    DAD B                       ; HL+=000D
    JMP FindFor+4               ; Try the next one

; Block Transfer Routine
; Make space by shoving everything forward
; HL = destination adddress
; DE = low address to be transferred
; BD = high address to be transferred
;
; A check is made to make sure a reasonable amount of space remains
; between the top of the stack and the highest locatoin
;
CopyMemoryUp    
    CALL CheckEnoughMem         ; Make sure stack won't be overrun
    PUSH B                      ; Exchange BC with HL.
    XTHL    
    POP B   
CopyMemLoop 
    RST 4                       ;HL==DE?
    MOV A,M                     ; get the byte to transfer      
    STAX B                      ; and copy it to dest
    RZ                          ; Exit if DE reached.
    DCX B                       ; back up for next byte
    DCX H   
    JMP CopyMemLoop             ; keep copying

; Called to confirm enough space in memory for the stack
; Must be called by any routine which puts arbitary amount of stuff on the stack
; Also called by routines such as GOSUB and FOR which make permanent entries on stack
;
CheckEnoughVarSpace 
    PUSH H                      ; save HL
    LHLD VAR_TOP    
    MVI B,00h                   ; BC=C*4
    DAD B   
    DAD B   
    CALL CheckEnoughMem;
    POP H                       ; restore HL
    RET 

; Call with HL = some address
; Examined to make sure a certain number of bytes remain between it at stack top.
;
CheckEnoughMem  
    PUSH D  
    XCHG    
    LXI H,$FFDE                 ; HL=-34 (extra 2 bytes for return address)
    DAD SP                      ; HL = stack pointer + offset
    RST 4                       ; is this > entering HL
    XCHG                        ; restore HL from DE
    POP D                       ; get DE back
    RNC                         ; was OK?
                                ; (fall through if not)
OutOfMemory 
    MVI E,0Ch                   ; offset to "OM" in table
    .DB 01                      ; LXI B,... = jump over next 2 bytes
SyntaxError 
    MVI E,02h                   ; offset to "SN" in table
    .DB 01                      ; LXI B,... = jump over next 2 bytes
DivideByZero                    ; offset to "/0" in table
    MVI E,14h   

; print an error message, like "SN ERROR"
; call with Reg E = byte offset to the 2-byte error string
;
Error   
    CALL ResetStack 
    CALL NewLine    
    LXI H,ERROR_CODES           ; point to error code table
    MOV D,A                     ; NewLine set A=0, so D=0            
    MVI A,'?'                   
    RST 3                       ; print '?'
    DAD D                       ; add offset to specific error
    MOV A,M                     ; get 1st char of code
    RST 3                       ; and print it
    RST 2                       ; Get 2nd char of error code
    RST 3                       ; and print it
    LXI H,szError               
    CALL PrintString            ; Print " ERROR".
    LHLD CURRENT_LINE           ; get current line #
    MOV A,H                     ; see if in direct mode
    ANA L   
    INR A                       ; 0 = direct mode 
    CNZ PrintIN                 ; print line # in HL
    .DB 01                      ; LXI over Stop and fall into Main
Stop    
    RNZ                         ; Syntax Error if args.
    POP B                       ; Lose return address.


; Print "OK" and wait for user input.
; This code processes both text that must be executed immediately (like LIST) and
; text that will be stored as part of the user's program.
; Program lines are prefixed with a line number.  If one isn't found, it jumps to Exec
;
Main    
    LXI H,szOK                  ; "OK" prompt
    CALL Init                   ; replaced by INIT to "CALL PrintString"
GetNonBlankLine 
    LXI H,$FFFF 
    SHLD CURRENT_LINE           ; set line# = FFFF (Immediate mode)
    CALL InputLine              ; get input from user
    RST 2                       ; Look at first character
    INR A                       ; see if 0, setting flags
    DCR A    
    JZ GetNonBlankLine          ; blank line, so try again.
    PUSH PSW                    ; save status indicator
    CALL LineNumberFromStr      ; read in line #
    PUSH D                      ; save line #
    CALL Tokenize               ; crunch the line!
    MOV B,A                     ; after crunch, A=0 & BC=char count
    POP D                       ; restore line #
    POP PSW                     ; was there a line #?
    JNC Exec                    ; if not, it was a direct statement

StoreProgramLine    
    PUSH D                      ; Push line number
    PUSH B                      ; Push character count
    RST 2                       ; Get first char of line
    ORA A                       ; Zero set if line is empty (ie removing a line)
    PUSH PSW                    ; Preserve line-empty flag
    CALL FindProgramLine        ; Get nearest program line address in BC.
    PUSH B                      ; Push line address.
    JNC InsertProgramLine       ; If line doesn't exist, insert it.

RemoveProgramLine   
    XCHG                        ; start at DE = Next line address.
    LHLD VAR_BASE               ; end point = where variables start
RemoveLine  
    LDAX D                      ; Move a byte of the program remainder
    STAX B                      ; from DE to BC
    INX B                       ; increment pointers
    INX D   
    RST 4                       ; Loop until DE==VAR_BASE, ie whole
    JNZ RemoveLine              ; program remainder has been moved down
    MOV H,B 
    MOV L,C                     ; Update VAR_BASE from BC (new program end)
    SHLD VAR_BASE   

InsertProgramLine   
    POP D                       ; Retrieve line# (from above) into DE
    POP PSW                     ; Restore line-empty flag 
    JZ UpdateLinkedList         ; If line is empty, no need to insert
    LHLD VAR_BASE   
    XTHL                        ; HL = character count from stack
    POP B                       ; BC = VAR_BASE
    DAD B                       ; HL = VAR_BASE + character count
    PUSH H                      ; temp save updated address for variables
    CALL CopyMemoryUp           ; Make space for the new line.
    POP H                       ; retrieve updated variable address
    SHLD VAR_BASE               ; and save it.
    XCHG                        ; HL=Line address, DE=VAR_BASE
    MOV M,H                     ; not program end, so create a non-zero link
    INX H                       ; Skip over next line ptr (updated below)
    INX H   
    POP D                       ; DE = line number 
    MOV M,E                     ; Write line # to program memory.
    INX H                       
    MOV M,D                      
    INX H   

CopyFromBuffer  
    LXI D,LINE_BUFFER           ; Copy the line into the program.
    LDAX D                      ; get a byte from the input buffer                 
    MOV M,A                     ; and move it to program memory
    INX H                       ; increment pointers
    INX D   
    ORA A                       ; zero marks the end
    JNZ CopyFromBuffer+3        ; loop until done

UpdateLinkedList    
    CALL ResetAll               ; do clear & set up stack
    INX H   
    XCHG    

; CHEAD goes through program storage and fixes up all the links.
; The end of each line is found by searching for the zero at the end.
; A double zero link is used to detect the end of the program
;
CHEAD   
    MOV H,D 
    MOV L,E 
    MOV A,M                     ; If the pointer to the next line is a null
    INX H                       ; word then we've reached the end of the
    ORA M                       ; program, job is done, and we can jump back
    JZ GetNonBlankLine          ; to let the user type in the next line.
    INX H                       ; Skip over line number.
    INX H   
    INX H   
    XRA A                       ; looking for a zero in memory
L0271   
    CMP M                       ; did we find it?
    INX H                       ; bump pointer
    JNZ L0271                   ; no, keep advancing to end of line
    XCHG            
    MOV M,E                     ; do 1st byte of fixup
    INX H                       ; advance pointer
    MOV M,D                     ; do 2nd byte of fixup
    JMP CHEAD                   ; keep chaining until done

; Search the program text for the line whose # is passed in DE
; There are 3 possible returns:
; 1) Z set, carry not set = line not found, BC points to double zero @ end
; 2) Z and carry both set = line found, BC points to line's link field
; 3) non-zero, no carry   = line not found, BC points to line beyond searched line
;
FindProgramLine 
    LHLD PROGRAM_BASE   
    MOV B,H                     ; BC = this line
    MOV C,L 
    MOV A,M                     ; If we've found two consecutive
    INX H                       ; null bytes, then we've reached the end
    ORA M                       ; of the program and so return.
    DCX H   
    RZ                          ; Return not found, reached of program
    PUSH B                      ; Push this line address
    RST 6                       ; Push link
    RST 6                       ; Push line #
    POP H                       ; HL = this line number
    RST 4                       ; Compare line numbers
    POP H                       ; HL = next line address
    POP B                       ; BC = this line address
    CMC 
    RZ                          ; Return carry set if line numbers match.
    CMC
    RNC                         ; Return if we've reached a line number beyond one searched
    JMP FindProgramLine+3       ; keep looking for a match

; The NEW command clears the program text & variable space
;
New 
    RNZ  
    LHLD PROGRAM_BASE    
    XRA A                       ; put a double-zero terminator @ prog start
    MOV M,A 
    INX H   
    MOV M,A 
    INX H    
    SHLD VAR_BASE               ; and locate variable base right after it

Run 
    RNZ 
ResetAll    
    LHLD PROGRAM_BASE   
    DCX H

; Clear initializes the variable and array space
;   
Clear   
    SHLD PROG_PTR_TEMP          ; save HL
    CALL Restore                ; restore data
    LHLD VAR_BASE               ; take the variable start address
    SHLD VAR_ARRAY_BASE         ; and save it as start of array space
    SHLD VAR_TOP                ; and and of variable storage

; Reset the stack pointer, elimanating GOSUB and FOR context.
; A dummy entry is put on the stack, so FindFor will always find a
; "non-FOR" entry at the bottom of the stack.
;
ResetStack  
    POP B                       ; get return address
    LHLD STACK_TOP              ; point HL to top of memory
    SPHL                        ; initialize stack
    XRA A   
    MOV L,A 
    PUSH H                      ; push zero (not-FOR/not-GOSUB token) on stack
    PUSH B                      ; restore return address
    LHLD PROG_PTR_TEMP          ; get saved HL
    RET 

; Prompt user with a '?" and get a line of input
;
InputLineWith   
    MVI A,'?'                   
    RST 3                       ; Print the '?' prompt
    MVI A,' '                   
    RST 3                       ; Print ' '
    CALL InputLine              ; get user input
    INX H   

; All reserved words are translated into single bytes with msb set.  This save space
; and time by allowing for table dispatch during execution.  Therefore all tokens
; appear in the same order as they do in the address table.
;
Tokenize    
    MVI C,05                    ; character count is at least 5.
    LXI D,LINE_BUFFER           ; at start, dest ptr = input ptr
    MOV A,M                     ; get character from buffer
    CPI ' '                     ; is it a space we want to save?
    JZ WriteChar                ; yes, stuff in destination line
    MOV B,A                     ; get character from line
    CPI '"'                     ; is it a quote sign?
    JZ FreeCopy                 ; if so, go to special string handling
    ORA A                       ; end of line?
    JZ Exit                     ; yes, done tokenizing
    PUSH D                      ; save destination ptr.
    MVI B,00                    ; Initialise Keyword ID to 0.
    LXI D,KEYWORDS-1            ; point to list of keywords
    PUSH H                      ; save input ptr.
    .DB 3Eh                     ; LXI over get-next-char
KwCompare   
    RST 2                       ; get next char from buffer
    INX D                       ; bump destination pointer
    LDAX D                      ; Get keyword char to compare with.
    ANI 7Fh                     ; Ignore bit 7 of keyword char.
    JZ NotAKeyword              ; If char 0, then end of keywords reached.
    CMP M                       ; Keyword char matches input char?
    JNZ NextKeyword             ; If not, jump to get next keyword.
    LDAX D                      ; get keyword byte
    ORA A                       ; set condition codes
    JP KwCompare                ; if sign set, keyword has been found
    POP PSW                     ; Remove input ptr from stack. We don't need it.
    MOV A,B                     ; A=Keyword ID
    ORI $80                     ; Set bit 7 (indicates a keyword)
    .DB $F2                     ; JP = skip over next POP H and MOV A,M
NotAKeyword 
    POP H                       ; Restore input ptr
    MOV A,M                     ; and get input char
    POP D                       ; Restore dest ptr
WriteChar   
    INX H                       ; Advance input ptr
    STAX D                      ; Save char to dest
    INX D                       ; bump dest ptr
    INR C                       ; bump dest character count
    SUI 8Eh                     ; Is it the REM keyword (8E)?
    JNZ Tokenize+5              ; loop if not
    MOV B,A                     ; otherwise, just copy rest of line (B now 0)

    ; copy input to output without tokenizing, which is needed for REM comments
    ; and string literals.  Reg B holds the terminating character - when this 
    ; char is reached the freecopy is complete and it jumps back

FreeCopyLoop    
    MOV A,M                     ; get input char in A
    ORA A                       ; set condition codes
    JZ Exit                     ; if A=0 then exit
    CMP B                       ; are we at end of line yet?
    JZ WriteChar                ; yes: go back to tokenizing
FreeCopy    
    INX H                       ; bump 
    STAX D                      ; copy char to destination
    INR C                       ; bump char counter
    INX D                       ; bump dest pointer
    JMP FreeCopyLoop            ; keep copying from input to dest

    ; advance keyword pointer in DE to point to the next keyword in the table
    ; then jump back to KwCompare to see if it matches.

NextKeyword 
    POP H                       ; Restore input ptr
    PUSH H                      ; keep saving it
    INR B                       ; go to next Keyword ID
    XCHG                        ; HL=keyword table ptr
NextKwLoop  
    ORA M                       ; Loop until
    INX H                       ; bit 7 of
    JP NextKwLoop               ; keyword char is set.
    XCHG                        ; DE=keyword ptr, HL=input ptr
    JMP KwCompare+2             ; done, move to next keyword

Exit    
    LXI H,LINE_BUFFER-1         ; restore HL to line buffer start
    STAX D                      ; need three 0's on the end
    INX D                       ; one for end-of-line
    STAX D                      ; and two for a zero link
    INX D                       ; because if this is direct statement
    STAX D                      ; it must look like a program end.
    RET                         ; done tokenizing the line.  Whew!

Backspace   
    DCR B                       ;Char count--;
    DCX H                       ;Input ptr--;
    RST 3                       ;Print backspace char.
    JNZ InputNext   

ResetInput  
    RST 3                       ;RST OutCha
    CALL NewLine    

; Get a line of input from the user, handling any special characters
; allow up to 72 characters in the input buffer
;
InputLine   
    LXI H,LINE_BUFFER           ; point to line buffer
    MVI B,01                    ; character counter
InputNext   
    CALL InputChar              ; get next character
    CPI $0D                     ; is it <cr>?
    JZ TerminateInput           ; done if so
    CPI ' '                     ; If < ' '
    JC InputNext                ; or
    CPI $7D                     ; > '}'
    JNC InputNext               ; then loop back.
    CPI '@'                     ; if its @
    JZ ResetInput               ; then start fresh
    CPI '_'                     ; if its an underscore
    JZ Backspace                ; then do a backspace
    MOV C,A                     ; temp save char in C
    MOV A,B                     ; get character count
    CPI 72                      ; is it 72 yet?
    MVI A,07                    ; 7 = bell character    
    JNC L036A                   ; too many chars; ring the bell
    MOV A,C                     ; restore char               
    MOV M,C                     ; move it into line buffer
    INX H                       ; point to next buffer location
    INR B                       ; increase character count
L036A   
    RST 3                       ; display the character 
    JMP InputNext               ; and get more input

; Prints a character to the terminal, dealing with line width. 
; On entry, the char to be printed is on the stack and A holds TERMINAL_X. 
; If the current line is up to the maximum width then we print a new line and 
; update the terminal position. Then we print the character
;
OutChar_tail                    ; go to next line if necessary:
    CPI 72                      ; 72 is default line width
    CZ NewLine                  ; we've reached end of line
    INR A                       ; not yet, so increment column count
    STA TERMINAL_X              ; and save it

; The following two routines are modified for use on the IMSAI 8080,
; using console I/O ports 2/3
;
WaitTermReady   
    IN CONS                     ; read console status port
    ANI 01                      ; look at Tx Ready bit only
    JZ WaitTermReady            ; loop until ready
    POP PSW                     ; get char to write
    ANI 7Fh                     ; ignore bit7
    OUT CON                     ; and send it           
    RET 

InputChar   
    IN CONS                     ; read console status port 
    ANI 02                      ; look at Rx Ready bit only 
    JZ InputChar                ; wait until ready
    IN CON                      ; read the byte 
    ANI 7Fh                     ; keep only lower 7 bits
    RET

List    
    CALL LineNumberFromStr      ; get line # into DE
    RNZ                         ; must be terminator or error
    POP B                       ; get rid of return address
    CALL FindProgramLine        ; find line# >= DE
    PUSH B                      ; save start pointer
ListNextLine    
    POP H                       ; get pointer to line
    RST 6                       ; push link onto stack
    POP B                       ; take link off temporarily
    MOV A,B                     
    ORA C                       ; is it 0 = end of chain?
    JZ Main                     ; if so, we are done
    CALL TestBreakKey           ; check for ctrl-C
    PUSH B                      ; put link back on stack
    CALL NewLine                ; start new line
    RST 6                       ; push line #
    XTHL                        ; HL = line#, put HL on stack
    CALL PrintInt               ; print line# as integer
    MVI A,' '                   
    POP H                       ; restore pointer to start of text
ListChar    
    RST 3                       ; space after line #    
    MOV A,M                     ; get a character from line
    ORA A                       ; test flags
    INX H                       ; bump text pointer
    JZ ListNextLine             ; 0=end of line, so get start next line
    JP ListChar                 ; normal character, so print it
    SUI 7Fh                     ; its a token, so convert to keyword #
    MOV C,A                     ; save keyword # in C
    PUSH H                      ; save current position
    LXI D,KEYWORDS              ; get pointer to keywords
    PUSH D                      ; and save it
ToNextKeyword   
    LDAX D                      ; get character from keyword list
    INX D                       ; bump pointer
    ORA A                       ; test bits
    JP ToNextKeyword            ; not at end of keyword yet
    DCR C                       ; decrement char
    POP H                       ; pop position here
    JNZ ToNextKeyword-1         ; not at end of keyword list yet
PrintKeyword                    ; here when found right keyword!
    MOV A,M                     ; get char from keyword
    ORA A                       ; set condition codes
    JM ListChar-1   
    RST 3                       ; Out Char  
    INX H                       ; bump keyword pointer
    JMP PrintKeyword            ; print the rest of the keyword


; THE "FOR" STATEMENT
;
; A "FOR" entry on the stack has the following format:
; (1 byte)  the token
; (2 bytes) a pointer to the loop variable
; (1 byte)  a byte reflecting the sign of the increment
; (4 bytes) the step
; (4 bytes) the upper value 
; (2 bytes) the line# for the for statement
; (2 bytes) a text pointer into the for statement
; TOTAL = 16 bytes
;
; This FOR handler only gets called once.  Subsequent iterations of the loop return to the
; line following the FOR statement, not to the FOR statement itself.
;
For 
    CALL Let                    ; read the variable and assign it the
                                ; correct initial value; store a pointer to
                                ; the variable in (temp)
    XTHL                        ; save text ptr on the stack
    CALL FindFor                ; must have variable pointer in DE 
    POP D                       ; DE = text pointer
    JNZ L03E2                   ; if no matching entry, don't eliminate anything
    DAD B                       ; if "For", eliminate the matching entry 
                                ; and everything following
    SPHL                        ; do the eliminatation since matching entry was found
L03E2   
    XCHG                        ; HL = text pointer
    MVI C,08                    
    CALL CheckEnoughVarSpace    ; make sure 16 bytes available on stack
    PUSH H                      ; really save the text pointer
    CALL FindNextStatement      ; get HL that points just beyond terminator
    XTHL                        ; restore HL as text pointer at variable name
    PUSH H                      ; push pointer to end of statement onto the stack
    LHLD CURRENT_LINE           ; HL = current line #
    XTHL                        ; now current line # on stack & HL is text ptr
    RST 1                       ; Syntax Check
    .DB 95h                     ; for keyword "TO"  
    CALL EvalExpression         ; read the value after "TO"
    PUSH H                      ; save text pointer
    CALL FCopyToBCDE            ; get the stuff
    POP H                       ; restore text pointer
    PUSH B                      ; push FP result onto the stack
    PUSH D                       
    LXI B,8100h                 ; initialize step value to 1
    MOV D,C 
    MOV E,D 
    MOV A,M                     ; get terminating character                
    CPI $97                     ; Do we have a "Step"?
    MVI A,01h                   ; set up default sign
    JNZ PushStepValue           ; skip ahead if no step
    CALL EvalExpression+1       ; evaluate the step expression
    PUSH H  
    CALL FCopyToBCDE            ; get step value into regs
    RST 5                       ; FTestSign
    POP H   
PushStepValue   
    PUSH B                      ; push step value onto stack
    PUSH D  
    PUSH PSW                    ; save sign of step
    INX SP                      ; as one-byte entry only
    PUSH H  
    LHLD PROG_PTR_TEMP          ; retrieve ptr to variable    
    XTHL                        ; & put in stack; get back text ptr 
EndOfForHandler 
    MVI B,$81           
    PUSH B                      ; put the "FOR" token on stack
    INX SP                      ; as a one-byte entry
                                ; continue below into "NextExec"

; Having executed one statement, the following code moves to the next program statement
; in the line (or next line if there are no more statements in current line)
;
; Character pointed to by HL is either ":" or EOL.  The address of this routine is left
; on the stack when a statement is executed so that it can do a RET when it is done.
;
; In the original source code, this function was called "NEWSTT", the new statement fetcher.
;
ExecNext    
    CALL TestBreakKey           ; give user a chance to break execution
    MOV A,M                     ; get next character in text
    CPI ':'                     ; a colon ":" is a statement separator
    JZ Exec                     ; go jump to Exec to run it!
    ORA A                       ; no statement separator found, so char should be null byte
    JNZ SyntaxError             ; if not null, thats a syntax error.
    INX H                       
    MOV A,M                     ; get next byte, usually next line address
    INX H   
    ORA M                       ; and 2nd byte of next line address
    INX H                       
    JZ Main                     ; both zero = program end, so return to main
    MOV E,M                     
    INX H   
    MOV D,M                     ; DE = line# of next line
    XCHG                        ; HL now line #, DE = text pointer
    SHLD CURRENT_LINE           ; save current line #
    XCHG                        ; restore text pointer & fall into Exec below


; Execute a BASIC statement pointed to by HL
;
Exec    
    RST 2                       ; get first character of statement  
    LXI D,ExecNext  
    PUSH D                      ; push ExecNext so that we return to it
    RZ                          ; leave if this is an empty statement
    SUI 80h                     ; Check to see if we start with token.
    JC Let                      ; No?  It could be an implied let, like "A=2"
    CPI $14                     ; if it's not a reserved word         
    JNC SyntaxError             ; then its a syntax error
    RLC                         ; BC = A*2
    MOV C,A 
    MVI B,00h   
    XCHG    
    LXI H,KW_GENERAL_FNS        ; point to list of keyword functions
    DAD B                       ; add in offset to desired function
    MOV C,M                     ; get keyword handler address in BC
    INX H               
    MOV B,M                         
    PUSH B                      ; push keyword handler on stack
    XCHG                        ; HL = program pointer
    RST 2                       ; get next character in statement
    RET                         ; return to keyword handler fn

NextChar_tail   
    CPI ' '                     ; is it a space?
    JZ NextChar                 ; ignore spaces
    CPI '0'                        
    CMC                         ; set carry if non-numeric
    INR A                       ; test for null
    DCR A                       ; without affecting carry
    RET


; The "RESTORE" statement
;
; Reset the data pointer to just before the start of the program
;
Restore 
    XCHG    
    LHLD PROGRAM_BASE           ; point to start of program 
    DCX H                       ; decrement pointer    
L046E   
    SHLD DATA_PROG_PTR          ; and save this as data pointer        
    XCHG    
    RET 

; !!! TestBreakKey modified for use with IMSAI 8080
;
TestBreakKey    
    IN  CONS                    ; Check console status
    ANI 02                      ; look at only Rx Ready
    RZ                          ; exit if no key waiting
    CALL InputChar              ; get the key
    CPI 3                       ; is it Ctrl-C?
    JMP Stop    

; if the characters pointed to by HL is A..Z, the carry flag is clear
; carry flag set for anything else
; 
CharIsAlpha 
    MOV A,M                     ; get character
    CPI 'A' 
    RC                          ; carry set if char < 'A'
    CPI 'Z'+1   
    CMC                         ; carry set if char > 'Z'
    RET 

; Get the subscript of an array variable in an expression or DIM declaration
; The subscript is returned as a positive interger in CDE
;
GetSubscript    
    RST 2                       ; point to the next character
    CALL EvalExpression         ; get the subscript     
    RST 5                       ; Check the sign
    JM FunctionCallError        ; FC error if subscript is negative
    LDA FACCUM+3    
    CPI $90                     ; FC error if subscript > 32767
    JC FAsInteger               ; exit to FasInteger if subscript OK

FunctionCallError               ; FC error
    MVI E,08h   
    JMP Error   

; Gets a line number from a string pointer at in HL. 
; The integer result is returned in DE. 
; Leading spaces are skipped over, and it returns on finding the first non-digit. 
; The largest possible line number is 65529
; (Syntax error if the value of the first four digits is more then 65529)
; Returns with Z set if it found a valid number (or the string was empty), 
; or NZ if the string didn't lead with a number.
;
LineNumberFromStr               ; gets number value in buffer into DE
    DCX H                       ; start at beginning of line buffer
    LXI D,0000                  ; initialize result DE = 0 
NextLineNumChar 
    RST 2                       ; get next character in line buffer 
    RNC                         ; exit on 1st non-alphanumeric
    PUSH H                      ; save buffer pointer
    PUSH PSW                    ; save current character
    LXI H,1998h                 ; Decimal 6552
    RST 4   
    JC SyntaxError              ; line number too high (>65529) 
    MOV H,D                     ; copy result DE to HL
    MOV L,E 
    DAD D                       ; HL = result* 2    
    DAD H                       ; HL = result* 4
    DAD D                       ; HL = result* 5
    DAD H                       ; HL = result* 10
    POP PSW                     ; retrieve character
    SUI '0'                     ; convert to a number
    MOV E,A                    
    MVI D,00h                   ; put number in DE
    DAD D                       ; new result = old result + number
    XCHG                        ; put new result in DE             
    POP H                       ; restore buffer pointer             
    JMP NextLineNumChar 


; The "GOSUB" Statement
;
; A gosub entry on the stack has the following format:
; (1 byte)  Gosub Token
; (2 bytes) Line # of the gosub statement
; (2 bytes) Pointer to the text of the gosub
; TOTAL = 5 bytes
;
Gosub   
    MVI C,03h   
    CALL CheckEnoughVarSpace    ; make sure there is room
    POP B                       ; pop off return address
    PUSH H                      ; push text pointer
    PUSH H                      ; save text pointer
    LHLD CURRENT_LINE           ; get current line #
    XTHL                        ; HL = text ptr; Cur.line on stack
    MVI D,$8C   
    PUSH D                      ; put gosub token on stack
    INX SP                      ; as 1-byte entry
    PUSH B                      ; restore return address, fall into GOTO


; the "GOTO" statement
;
; Sets program execution to continue from the line number argument
;
Goto    
    CALL LineNumberFromStr      ; DE = line # from argument
    RNZ                         ; syntax error if argument isn't a number
    CALL FindProgramLine        ; BC = addr of program line with this line#
    MOV H,B                     ; move address to HL
    MOV L,C     
    DCX H   
    RC                          ; successful return if program line exists
    MVI E,0Eh                   ; otherwise,
    JMP Error                   ; Undefined subroutine (US) Error

; RETURN
;
; Return program execution to the statement following the last GOSUB.
; Information on where to return is kept on the stack (see GOSUB)
;
Return  
    RNZ                         ; no arguments allowed for Return stmt
    MVI D,$FF                   ; var pointer in DE never gets matched
    CALL FindFor                ; go past all the "FOR" entries
    SPHL                        ; update the stack
    CPI $8C                     ; did we reach the GOSUB token?
    MVI E,04h   
    JNZ Error                   ; Return without Gosub (RG) Error
    POP H                       ; pop off Gosub line# into HL
    SHLD CURRENT_LINE           ; and store it as current line
    LXI H,ExecNext              ; set return address to ExecNext
    XTHL                        ; Pop program pointer to just after GOSUB stmt.
                                ; Fall into FindNextStatement below 
                                ; which is OK since Return has no arg.

; the "REM" Statement
; 
; FindNextStatement: Finds the end of the statement or the end of the program line
;
FindNextStatement   
    .DB 013Ah                   ; LXI B,..3A, skips over next byte  
Rem 
    .DB 10h                     ; Bug: supposed to be '0Eh' for "MOV C,0"
    NOP 
FindNextStatementLoop   
    MOV A,M                     ; get the character
    ORA A                       ; is it end-of-line null?
    RZ                          ; return if so (done)  
    CMP C                       ; test for the other terminator
    RZ                          ; return if so
    INX H                       ; advance to next character
    JMP FindNextStatementLoop   ; loop until done


; The "LET" Statement
;
; Assigns a value to a variable
;
Let
    CALL GetVar                 ; returns address of variable in DE
    RST 1                       ; Syntax Check: ensure "=" follows the variable name    
    .DB 9Dh                     ; "="
AssignVar                       ; assign result of expression @ HL to variable @ DE
    PUSH D                      ; save variable pointer on stack
    CALL EvalExpression         ; get variable value in floating pt accumulator
    XTHL                        ; HL=pointer to value; text ptr on stack
    SHLD PROG_PTR_TEMP          ; save pointer for "FOR"     
    PUSH H                      ; save ptr to value
    CALL FCopyToMem             ; store FACCUM result to variable memory
    POP D                       ; DE = pointer to value
    POP H                       ; restore text pointer into HL 
    RET 

; The "IF" Statement
;
; (left-hand expression) (comparison operator string) (right-hand expression)
;
; Evaluates a condition.  The comparison operator must one or more of ">","<","=".
; It converts a string of comparison operators to a 3-bit value:
;    set bit 0 if ">" present in the string
;    set bit 1 if "=" present in the string
;    set bit 2 if "<" present in the string
; Therefore ">=" sets bits 0&1 = 3.  ">>>" sets bit 0, so value is 1.  "<=>" value is 7.
;
If  
    CALL EvalExpression         ; get left-hand expression
    MOV A,M                     ; A = first char of operator
    CALL FPush                  ; push left-hand expression on stack
    MVI D,00    
GetCompareOpLoop    
    SUI 9Ch                     ; convert tokens '<' to 0, '=' to 1, '<' to 2   
    JC GotCompareOp             ; jump if not a comparison token           
    CPI $03                     
    JNC GotCompareOp            ; jump if not a comparison token
    CPI $01                     ; set up bits by mapping
    RAL                         ; 0 to 1, 1 to 2, and 2 to 4
    ORA D                       ; OR with earlier bits
    MOV D,A                     ; store new bits
    RST 2                       ; get next character in text    
    JMP GetCompareOpLoop        ; and loop until done
GotCompareOp    
    MOV A,D                     ; retrieve comparison bits
    ORA A                       ; set condition flags
    JZ SyntaxError              ; 0 = no comparison found = error
    PUSH PSW                    ; save comparison bits on stack
    CALL EvalExpression         ; evaluate right-hand expression, rslt in FAC
    RST 1                       ; SyntaxCheck: make sure "THEN" present 
    .DB 96h                     ; KWID_THEN 
    DCX H   
    POP PSW             
    POP B   
    POP D   
    PUSH H                      
    PUSH PSW    
    CALL FCompare               ; compare two sides of relation stmt
    INR A                       ; build relational bits...
    RAL                         ; less=4, equal=2, greater=1
    POP B                       ; pop off what relational operator was
    ANA B                       ; see if we matched
    POP H                       ; pop off text pointer
    JZ Rem                      ; if test failed, skip the rest of line
    RST 2                       ; pick up the first line# character
    JC Goto                     ; if line# found, it means implied "GOTO"
    JMP Exec+5                  ; otherwise interpret remainder as new statement


; The "PRINT" statement
;
;  Print can be an empty line, a single expression, or multiple expressions separated by
;  tabulation directives (comma, semicolon, or tab keyword) 
;
    DCX H   
    RST 2                       ; get another character
Print   
    JZ NewLine                  ; if end of line, exit via NewLine  
    RZ  
    CPI '"'                     ; is char a quote? (start of string literal)
    CZ PrintString-1            ; if so just print the string literal
    JZ Print-2                  ; if end of line reached, then NewLine            
    CPI $94                     ; if char the TAB token,     
    JZ Tab                      ; then do the TAB function
    PUSH H                      ; save the text pointer
    CPI ','                     ; if char is a comma,
    JZ ToNextTabBreak           ; then we have to move to next tab break
    CPI $3B                     ; if char is a semicolon,
    JZ ExitTab                  ; then don't need to do anything special
    POP B                       ; get rid of old text pointer
    CALL EvalExpression         ; evaluate expression to print -> FAC
    PUSH H                      ; save text pointer
    CALL FOut                   ; convert the FAC number to a string
    CALL PrintString            ; and print it.
    MVI A,' '                   ; always end with a space
    RST 3                       ; out char  
    POP H                       ; restore text pointer
    JMP Print-2                 ; and continue with print

; HL points to just beyond the last byte of a line of user input.
; Here we write a null byte to terminate it, reset HL to point to start of input line buffer
; then fall into NewLine
;
TerminateInput  
    MVI M,00h                   ; add null byte to terminate line
    LXI H,LINE_BUFFER-1         ; reset HL to start of line buffer


; Print carriage return + line feed (CRLF)
; !!! modified for IMSAI 8080: block of orginal code commented out
;
NewLine 
    MVI A,0Dh                   ; <CR>
    STA TERMINAL_X  
    RST 3                       ; send it
    MVI A,0Ah                   ; <LF>
    RST 3                       ; send it
    XRA A 
    STA TERMINAL_X              ; reset x posn
    RET                         ; returns with A=0

; the rest of this routine is ALTAIR and/or teletype specific, and not needed
;   LDA TERMINAL_Y  
;PrintNullLoop  
;   DCR A   
;   STA TERMINAL_X  
;   RZ  
;   PUSH PSW    
;   XRA A   
;   RST 3   ;RST OutChar    
;   POP PSW 
;   JMP PrintNullLoop

; Print a string to the terminal, stopping on a null byte (0) or quote char
;
    INX H                       ; sometimes used with PrintString
PrintString 
    MOV A,M                     ; get character in string
    ORA A                       ; is it zero?
    RZ                          ; yes, we are done
    INX H                       ; point to next char
    CPI '"'                     ; is character a quote?
    RZ                          ; if so we are done
    RST 3                       ; output the character  
    CPI $0D                     ; is it <cr>?
    CZ NewLine                  ; send newline if so
    JMP PrintString             ; repeat until done

; Calculate how many spaces are need to get us the next tab-break, then
; jump to PrintSpaces to do it.
;
ToNextTabBreak  
    LDA TERMINAL_X              ; check current tab position
    CPI $38                     ; beyone tab break column = 56?
    CNC NewLine                 ; if so, just NewLine
    JNC ExitTab 
CalcSpaceCount  
    SUI 0Eh                     ; get # spaces to reach next tab break:
    JNC CalcSpaceCount          ; A = (14 - (A%14))-1
    CMA                         
    JMP PrintSpaces             ; and print required # of spaces


; the "TAB" keyword takes an integer argument which indicates
; the column to print spaces up to
;
Tab 
    CALL GetSubscript           ; get positive integer argument in E
    RST 1                       ; Syntax Check  
    .DB 29h                     ; for closing parenthesis ')'   
    DCX H   
    PUSH H                      ; save text pointer
    LDA TERMINAL_X              ; get current position
    CMA                         ; negate it
    ADD E                       ; #spaces needed = tab argument - curpos
    JNC ExitTab                 ; done
PrintSpaces                     
    INR A                       ; print A+1 spaces...
    MOV B,A                     ; counter = A+1
    MVI A,' '                   ; character to print is a space
PrintSpaceLoop  
    RST 3                       ; print the space
    DCR B                       ; and loop
    JNZ PrintSpaceLoop          ; until done
ExitTab 
    POP H                       ; restore the text pointer
    RST 2                       ; get the next character in program 
    JMP Print+3                 ; and return to the PRINT handler


; The "INPUT" Statement
;
; Let user input a number at a "?" prompt
;
Input   
    PUSH H                      ; save text pointer
    LHLD CURRENT_LINE           ; check for direct mode by getting line#
    MVI E,16h                   ; code for illegal direct (ID) error
    INX H                       ; increment line # 
    MOV A,L                     ; is line# now 0?
    ORA H                       ; if so, we are in direct mode
    JZ Error                    ; and this is an ID error    
    CALL InputLineWith          ; print "?" and get user input
    JMP L05FA+1                 ; continue below


; The "READ" Statement
;    
Read    
    PUSH H                      ; save text pointer
    LHLD DATA_PROG_PTR          ; get last data location
L05FA   
    ORI $AF                     ; make flag non-zero (=READ)
    ;XRA A  
    STA INPUT_OR_READ           ; save flag
    XTHL                        ; save data ptr on stack, HL=text pointer      
    .DB 01                      ; LXI B,... trick: skip next syntax check   
ReadNext    
    RST 1                       ; SyntaxCheck   
    .DB 2Ch                     ; for comma ',' 
    CALL GetVar                 ; DE = address for variable value  
    XTHL                        ; HL = data pointer; save text ptr on stack
    PUSH D                      ; save address of variable value
    MOV A,M                     ; get byte of data
    CPI ','                     ; is it a comma separator?
    JZ GotDataItem              ; yes, so can evaluate the data
    ORA A                       ; is it null line terminator?
    JNZ SyntaxError             ; if not, then exit as syntax error
    LDA INPUT_OR_READ           ; reading or input?
    ORA A                       ; test the question!
    INX H   
    JNZ NextDataLine+1          ; reading: jump ahead to get next data item
    MVI A,'?'                   ; inputting: more inputs to process
    RST 3                       ; print unneeded question mark.  Oops!! 
    CALL InputLineWith          ; and get the next input item
GotDataItem 
    POP D                       ; restore variable address
    INX H                       ; advance data ptr to start of next data item
    CALL AssignVar              ; assign the data to the variable
    XTHL                        ; HL = text ptr; push data ptr
    DCX H                       ; undo AssignVar's text ptr advance 
    RST 2                       ; get next character of READ statement  
    JNZ ReadNext                ; if not EOL, get next data item
    POP D                       ; get data pointer
    LDA INPUT_OR_READ           ; and flag
    ORA A                       ; don't update data pointer
    RZ                          ; if this is an INPUT statement
    XCHG                        ; HL = data pointer, DE = text ptr
    JNZ L046E                   ; update data pointer
NextDataLine                    ; loop to find next data line
    POP H                       ; restore data pointer
    RST 6                       ; push next word on stack (and in BC)
    MOV A,C                     ; test BC
    ORA B                       ; is BC=0? (address=0 or end of program)
    MVI E,06h                   ; Out of Data (OD) error
    JZ Error                    ; flag the error
    INX H                       
    RST 2                       ; get next text character   
    CPI $83                     ; Is it the DATA token?  
    JNZ NextDataLine            ; if not, loop to try next program line
    POP B                       ; discard text pointer on stack
    JMP GotDataItem             ; handle the new data


; the "NEXT" Statement
; 
; takes one argument, the name of the FOR variable
;
Next    
    CALL GetVar                 ; DE = address of variable
    SHLD PROG_PTR_TEMP          ; save the text pointer (pointing to end of stmt)
    CALL FindFor                ; access the FOR data structure on stack
    SPHL                        
    PUSH D                      ; save address of variable
    MOV A,M                     ; get 1st char of the FOR data
    INX H   
    PUSH PSW    
    PUSH D                      ; push the FOR variable again
    MVI E,00h                   ; Next without For (NF) error flag  
    JNZ Error                   ; no FOR data found, so error out
    CALL FLoadFromMem           ; load next 4 bytes in FOR data (step) into FAC
    XTHL                        ; HL = variable addr; push FOR data ptr
    PUSH H                      
    CALL FAddMem                ; Add step number to the FOR variable
    POP H                       ;
    CALL FCopyToMem             ; and save result back to FOR variable
    POP H                       ; ie, var = var + step
    CALL FLoadBCDEfromMem       ; HL = ptr to "TO" number, load into BCDE   
    PUSH H  
    CALL FCompare               ; compare updated var to "TO" value. Result in A:
                                ; A will be FF if var<TO, 0 if var=TO, 1 if var>TO 
    POP H   
    POP B                       ; B= step direction (1=forward, FF=backward)
    SUB B                       ; note that FOR loop is done if A-B = 0.  Cool.
    CALL FLoadBCDEfromMem       ; get BC=text pointer, DE=line# of FOR statement    
    JZ ForLoopIsComplete        ; jump ahead if FOR loop is done
    XCHG                        ; HL = points to line #
    SHLD CURRENT_LINE           ; save line#
    MOV L,C                     ; load HL with text pointer (@ end of FOR stmt)
    MOV H,B 
    JMP EndOfForHandler         ; done
ForLoopIsComplete   
    SPHL                        ; remove the FOR data from stack                 
    LHLD PROG_PTR_TEMP          ; restore the text pointer
    JMP ExecNext

; Evaluates an expression (terms & operators), returning the result in FAC.
; 
; The evalator starts with HL pointing to the first char in the expression
; At finish, HL points to the terminator.
;
; It uses the operator table to determine precedence and dispatch addr for each operator
;
; The temporary result on the stack has the following format:
; (2 bytes) Return address on completion of the operator routine
; (4 bytes) Floating point temporary result
; (2 bytes) Address of the operator routine
; (2 bytes) The operator precedence
; TOTAL = 10 bytes
;
EvalExpression  
    DCX H                       ; back up character pointer
    MVI D,00h                   ; initial dummy precedence is 0
    PUSH D                      ; push it 
    MVI C,01h   
    CALL CheckEnoughVarSpace    ; ensure room for recursive calls
    CALL EvalTerm               ; evaluate the term
    SHLD TEMP2                  ; save text pointer
ArithParse  
    LHLD TEMP2                  ; load text pointer
    POP B                       ; pop off precedence
    MOV A,M                     ; get next character
    MVI D,00h                   ; assume no relation operators
    SUI $98                     ; is this 1 relation?
    RC                          ; relations all done
    CPI $04                     ; is it an arithmetic (+-*/) operator?
    RNC                         ; nope, bigger
    MOV E,A                     ; deal with the arithmetic ops...         
    RLC                         ; A x 2
    ADD E                       ; A x 3
    MOV E,A                     ; DE = A x 3 = offset into fn table
    LXI H,KW_ARITH_OP_FNS       ; point to operator function table
    DAD D                       ; add offset.  Now HL = function table entry
    MOV A,B                     ; A = old precedence                
    MOV D,M                     ; D = new precedence
    CMP D                       ; old - new
    RNC                         ; apply old op if it has >= precedence
    INX H                       ; HL now points to function
    PUSH B                      ; save old precedence
    LXI B,ArithParse            ; B = operator return address
    PUSH B                      ; is first entry on stack
    MOV C,D                     
    CALL FPush                  ; 2nd stack entry = FP temp result
    MOV D,C                     ; save precedence in D                        
    RST 6                       ; push address of arithmetic fn
    LHLD TEMP2                  ; restore text pointer
    JMP EvalExpression+3

; Evaluate a term in an expression.  
; This can be a constant, variable, function, or bracketed expression
;
EvalTerm    
    RST 2                       ; get 1st character of the term.    
    JC FIn                      ; if its a digit, then get value into FAC
    CALL CharIsAlpha            ; an alpha char must be a variable
    JNC EvalVarTerm             ; so evaluate it as a variable
    CPI $98                     ; if char is a leading '+', 
    JZ EvalTerm                 ; just ignore it and get next char
    CPI '.'                     ; if char is a leading decimal point,
    JZ FIn                      ; then eval as number, get value into FAC
    CPI $99                     ; if char is a leading '-', 
    JZ EvalMinusTerm            ; then jump ahead to minus term eval
    SUI 9Fh                     ; if char is the ID of an inline function,
    JNC EvalInlineFn            ; then jump ahead to handle it
EvalBracketed                   ; only possibility left is a bracketed expression
    RST 1                       ; Syntax Check 
    .DB 28h                     ; for opening paren '(' 
    CALL EvalExpression         ; if found, do recursive evaluation
    RST 1                       ; Syntax Check  
    .DB 29h                     ; for closing paren ')' 
    RET 
EvalMinusTerm                   ; evaluate a negative term...
    CALL EvalTerm               ; recursive call to evalute
    PUSH H  
    CALL FNegate                ; then negate the result
PopRet 
    POP H   
    RET 
EvalVarTerm                     ; evaluate a variable...
    CALL GetVar                 ; DE = address of variable value        
    PUSH H  
    XCHG                        ; now HL = address of value
    CALL FLoadFromMem           ; get value into FAC
    POP H   
    RET 
EvalInlineFn    
    MVI B,00h   
    RLC                         ; A x 2
    MOV C,A                     ; offset BC = A x 2
    PUSH B                      ; and save offset on stack
    RST 2                       ; get next character
    CALL EvalBracketed          ; evaluate bracketed fn (ex SIN(X+2))
    XTHL                        ; HL=offset, put text pointer on stack
    LXI D,PopRet                ; Set return address to "POP H, RET"  !!! ADDED LABEL            
    PUSH D  
    LXI B,KW_INLINE_FNS         ; point to inline function table
    DAD B                       ; and add offset to get function address
    RST 6                       ; put function adddress on stack
    RET                         ; and return to it


; DIMENSION AND VARIABLE SEARCHING
;
; Space is allocated for variables as they are encountered, this DIM statements must be
; executed to have effect.  Six (6) bytes are allocated for each simple variable.  The
; first two bytes give the name of the variable and the last four give its value. 
; VAR_BASE (VARTAB in original) gives the first location where a simple variable name is
; found and VAR_ARRAY_BASE (ARYTAB in original) gives the location to stop searching for simple
; variables.  A FOR entry has a text pointer and a pointer to a variable value so neither the
; program or the simple variables can be moved while there are active FOR entries on the 
; stack.
;
; Adding a simple variable:
;   1. Adding 6 to VAR_ARRAY_BASE and VAR_TOP
;   2. Block transferring the array variables up by 6
;   3. Making sure VAR_TOP is not too close to the stack
;
; This movement of array variables means that no pointer to an array will stay valid when
; new simple variables can be encountered.  This is why array variables are not allowed
; FOR loop variables.
;
; Setting up a new array variable merely involves building the descriptor, updating VAR_TOP,
; and making sure there is still enough room between VAR_TOP and the stack.
;
; The format of of an array variable is:
;   1.  Second character of array name
;   2.  First character of array name
;   3.  Number of bytes used by the values
;   4.  The values
;
; GetVar (PTRGET in original), the routine which returns a pointer to a variable name, has
; an important flag.  DIM_OR_EVAL (DIMFLG is original) indicates whether DIM
; called the routine or not.  If so, no prior entry for the variable should be found, and the
; index indicates how much space to set aside.  Simple variables can be be dimensioned, but
; the only effect will be to set aside space for the variable if it hasn't been encountered
; yet.

; The "DIM" Statement
;
; Declares in array
;
DimContd    
    DCX H   
    RST 2                       ; get next program character    
    RZ                          ; return if EOL null 
    RST 1                       ; Syntax Check  
    .DB 2Ch ;','                ; for a comma
Dim 
    LXI B,DimContd              ; set return address for DimContd, above    
    PUSH B                      ; push return address on stack
    .DB $F6                     ; ORI AF = set A to nonzero value

; Called with HL pointing to a variable name
; Returns with DE = pointer to the variable's value
; if variable is new, memory is allocated and var is initialized
;
GetVar          
    XRA A                       ; A=0
    STA DIM_OR_EVAL             ; flag 0=GetVar, nonzero=DIM
    MOV B,M                     ; B= 1st char of variable name 
    CALL CharIsAlpha            ; must be alpha
    JC SyntaxError              ; otherwise error out
    XRA A   
    MOV C,A                     ; zero out C
    RST 2                       ; get next character    
    JNC L072E                   ; !!! ADDED LABEL. jump if not alphanumeric
    MOV C,A                     ; C = second char in var name    
    RST 2                       ; look at next character
L072E 
    SUI '('                     ; is it '(' for an array?
    JZ GetArrayVar              ; jump if so.    
    PUSH H                      ; save text ptr on stack
    LHLD VAR_ARRAY_BASE         ; HL= array base
    XCHG                        ; DE = array base
    LHLD VAR_BASE               ; HL = var base
FindVarLoop 
    RST 4                       ; Compare HL and DE
    JZ AllocNewVar              ; if equal, variable not found (is new)
    MOV A,C                     
    SUB M                       ; is this variable the one?
    INX H   
    JNZ NotIt                   ; nope  
    MOV A,B 
    SUB M                       ; try 2nd character matching
NotIt 
    INX H   
    JZ FinPtr                   ; that was it!  
    INX H                       ; skip over that on -- not it.
    INX H   
    INX H   
    INX H   
    JMP FindVarLoop 

; Prepare to allocate a new variable.  First, check the return address to see if it is
; the expression evaluator that called us, and if it is then exit without allocating.
; 
AllocNewVar 
    POP H                       ; HL = text pointer
    XTHL                        ; push text ptr, HL=return addr.
    PUSH D  
    LXI D,EvalVarTerm+3         ; DE = an EvalTerm address !!!Added Label
    RST 4                       ; Compare HL and DE
    POP D   
    JZ AlreadyAllocd            ; called by expr eval, no need to allocate  
    XTHL                        ; put return addr on stack, HL=text ptr.
    PUSH H                      ; text ptr back on stack
    PUSH B                      ; Preserve var name on stack
    LXI B,0006h                 ; going to insert 6 bytes...
    LHLD VAR_TOP                ; current end of var storage
    PUSH H                      ; save it
    DAD B                       ; add on 6 bytes for new variable
    POP B                       ; old end of storage
    PUSH H                      ; save new candidate for VarTop
    CALL CopyMemoryUp           ; move block up by 6 bytes
    POP H                       ; retrieve new VarTop
    SHLD VAR_TOP                ; and save it
    MOV H,B                     ; now update var_array_base
    MOV L,C                     ; to account for 6 byte move
    SHLD VAR_ARRAY_BASE         ; and save it.

; initialize new variable to zero.
; HL is pointing at end of new variable, so zero backwards
; to DE.
;
InitVarLoop 
    DCX H                       ; go backwards from end of variable
    MVI M,00h                   ; write a zero
    RST 4                       ; compare HL and DE
    JNZ InitVarLoop             ; loop until done
    POP D                       ; DE = variable name
    MOV M,E                     ; write 1st byte of var name
    INX H   
    MOV M,D                     ; write 2nd byte of var name
    INX H   
FinPtr 
    XCHG                        ; DE = variable pointer
    POP H                       ; restore text pointer
    RET 

; GetVar exit when called by EvalTerm.  Set FAC to zero.
;
AlreadyAllocd   
    STA FACCUM+3                ; A=0 previously
    POP H   
    RET 

; Accesses or allocates an array variable.  DIM_OR_EVAL flag indicates whether its a 
; declaration (DIM, flag=0xEF) or just array access (Eval, flag=0).
;
GetArrayVar 
    PUSH B                      ; save var name on stack
    LDA DIM_OR_EVAL 
    PUSH PSW                    ; save flag on stack
    CALL GetSubscript           ; get array subscript in CDE
    RST 1                       ; Syntax Check  
    .DB 29h                     ; for closing paren ')' 
    POP PSW                     ; restore flag
    STA DIM_OR_EVAL 
    XTHL                        ; HL=var name
    XCHG                        ; HL=subscript; DE=var name
    DAD H                       ; subscript x 2
    DAD H                       ; offset = subscript x 4
    PUSH H                      ; push offset on stack
    LHLD VAR_ARRAY_BASE         ; HL = start of array storage
    .DB $01 ;LXI B,....         ; LXI trick, skip next 2 instructions
FindArray   
    POP B                       ; BC = length of last variable              
    DAD B                       ; skip over last var by adding its length
    XCHG                        ; DE = current search point
    PUSH H                      ; save variable name
    LHLD VAR_TOP                ; HL = place to store
    RST 4                       ; Compare HL and DE.  Are we there yet?
    XCHG                        ; HL = search point
    POP D                       ; pop variable name    
    JZ AllocArray               ; got to end; couldn't find it
    RST 6                       ; advance to next array in block...push next word
    XTHL                        ; HL = var name we are examining
    RST 4                       ; Compare HL and DE: Is this the variable?
    POP H                       ; pop off search pointer
    RST 6                       ; push length of var being examined
    JNZ FindArray               ; not a match, keep looking
    LDA DIM_OR_EVAL             ; is this an existing var trying to be dimensioned?
    ORA A                       ; test it
    MVI E,12h                   ; Syntax Error DD
    JNZ Error   
L07BF   
    POP D                       ; pop off length of this variable
    DCX D                       ; decrement length so we can use CARRY after compare
    XTHL                        ; trade pointer at var with index into variable
    RST 4                       ; compare HLDE.
    MVI E,10h                   ; is subscript >= number of array elements?
    JNC Error                   ; Bad subscript (BS) Error
    POP D                       ; pop off pointer to variable
    DAD D                       ; add it to index
    POP D                       ; pop off text pointer
    XCHG                        ; HL = text pointer; DE = variable pointer
    RET 

; Allocate space for an array.  
; Here, DE=array name and HL points to where it will be stored.
AllocArray  
    MOV M,E                     ; store array name 1st char
    INX H   
    MOV M,D                     ; store array name 2nd char
    INX H   
    LXI D,002Ch                 ; default size
    LDA DIM_OR_EVAL             ; are we dimensioning?
    ORA A                       ; test it
    JZ NotDim                   ; jump if just evaluating
    POP D                       ; we're DIM, so pop off index
    PUSH D                      ; and put it back on
    INX D                       ; add 4
    INX D   
    INX D   
    INX D   
NotDim  
    PUSH D  
    MOV M,E                     ; save array size LSB
    INX H   
    MOV M,D                     ; save array size MSB
    INX H   
    PUSH H  
    DAD D                       ; add array size to pointer
    CALL CheckEnoughMem         ; make sure dont run into stack
    SHLD VAR_TOP                ; save HL as new storage end
    POP D   
InitElements                    ; init all array elements to 0
    DCX H   
    MVI M,00h                   ; write a zero
    RST 4                       ; compare HL and DE
    JNZ InitElements            ; keep going until all elements are 0
    JMP L07BF   


; ================================================================================
;             FLOATING POINT MATH PACKAGE
; ================================================================================

; Converts integer word in AB to a floating-point number.
; Interestingly, this code is never used.
;
FWordToFloat    
    MOV D,B 
    MVI E,00h   
    MVI B,90h                   ;exponent=2^16
    JMP FCharToFloat+5  

FAddOneHalf                     ; add 0.5 to FAC
    LXI H,ONE_HALF              ; 0.5
FAddMem 
    CALL FLoadBCDEfromMem       ; get argument into registers    
    JMP FAdd+2                  ; do the addition

; Subtraction = addition with second term negated
FSub    
    POP B                       ; Get lhs in BCDE.
    POP D   
    CALL FNegate                ; Negate second argument
    .DB $21                     ; LXI trick, skip to FAdd + 2.


; Addition.  (FAC = ARG + FAC)
; The steps for adding two numbers (lhs,rhs) is
;
;  1. handle special cases where either side is 0
;  2. Denormalize the mantissas
;  3. Align the two numbers
;  4. Add the mantissas
;  5. Normalize the result & store in FAC
;
FAdd    
    POP B                       ; Get lhs in BCDE.
    POP D               
    MOV A,B                     ; If lhs=0 then we don't need
    ORA A                       ; to do anything and can just
    RZ                          ; exit.
    LDA FACCUM+3                ; If rhs=0 then exit via a copy
    ORA A                       ; of lhs to FAC.
    JZ FLoadFromBCDE    
    SUB B                       ; A=rhs.exponent-lhs.exponent.
    JNC L082C                   ; If rhs' exponent >= lhs'exponent, jump ahead.
    CMA                         ; Two's complement the exponent
    INR A                       ; difference, so it's correct.
    XCHG    
    CALL FPush                  ; Push old rhs
    XCHG    ;
    CALL FLoadFromBCDE          ; rhs = old lhs
    POP B                       ; lhs = old rhs.
    POP D   
L082C   
    PUSH PSW                    ; Preserve exponent diff
    CALL FUnpackMantissas   
    MOV H,A                     ; H=sign relationship
    POP PSW                     ; A=exponent diff.
    CALL FMantissaRtMult        ; Shift lhs mantissa right by (exponent diff) places.
    ORA H                       ; A=0 after last call, so this tests
    LXI H,FACCUM                ; the sign relationship.
    JP FSubMantissas            ; Jump ahead if we need to subtract.
    CALL FAddMantissas  
    JNC FRoundUp                ; Jump ahead if that didn't overflow.
    INX H                       ; Flip the sign in FTEMP_SIGN.
    INR M   
    JZ Overflow                 ; Error out if exponent overflowed.
    CALL FMantissaRtOnce        ; Shift mantissa one place right
    JMP FRoundUp                ; Jump ahead.
FSubMantissas   
    XRA A
    SUB B                       ; B=0-B
    MOV B,A 
    MOV A,M                     ; A=(FAC)       
    SBB E                       
    MOV E,A                     ; E=(FAC)-E
    INX H   
    MOV A,M                     ; A=(FAC+1)                   
    SBB D   
    MOV D,A                     ; D=(FAC+1)-D
    INX H   
    MOV A,M                     ; A=(FAC+2)                   
    SBB C   
    MOV C,A                     ; C=(FAC+2)-C

; Normalize the Mantissa in CDEB, rounded up to CDE, and stored in FAC
;
FNormalize  
    CC FNegateInt
     MVI H,00h
    MOV A,C                     ; Test most-significant bit of mantissa
    ORA A                       ; and jump ahead if it's 1.
    JM FRoundUp 

    ; Normalize the mantissa, shifting it left until bit23 is set

NormLoop    
    CPI $E0                     ; If we've shifted 32 times,
    JZ FZero                    ; then the number is 0.
    DCR H                       ; -1, then -2, etc.    
    MOV A,B                     ; Left-shift extra mantissa byte
    ADD A                       ; shift in a zero 
    MOV B,A  
    CALL FMantissaLeft          ; Left-shift mantissa
    MOV A,H                     ; get the shift count
    JP NormLoop                 ; Loop until bit23 set (msb of reg C)

    ; Adjust the exponent by the number of left-shifts done to the mantissa

    LXI H,FACCUM+3              ; get current exponent  
    ADD M                       ; add negative number
    MOV M,A                     ; Since A was a -ve number, that certainly should
    JNC FZero                   ; have carried, hence the extra check for zero.
    RZ                          

; Round result in CDEB and put number in FAC
; Round up or down depending on MSB of B
;
FRoundUp    
    MOV A,B                     ; see if we should round up
    LXI H,FACCUM+3  
    ORA A                       ; If bit 7 of the extra mantissa byte
    CM FMantissaInc             ; is set, then round up the mantissa.
    MOV B,M                     ; B=exponent
    INX H                       ; point to sign
    MOV A,M                     ; get sign in A
    ANI $80                     ; get rid of unwanted bits
    XRA C                       ; pack sign and HO
    MOV C,A                     ; save it in C
    JMP FLoadFromBCDE           ; save number in BCDE to FAC.

; Shift the mantissa in CDE left by one bit
;
FMantissaLeft   
    MOV A,E                     ; do Reg E (lowest order) first
    RAL                         ; shift left with carry
    MOV E,A                     ; save E
    MOV A,D                     ; now do Reg D
    RAL                         ; shift left with carry
    MOV D,A                     ; and save D
    MOV A,C                     ; now do C (Highest Order)
    ADC A                       ; shift left with carry
    MOV C,A                     ; and save C
    RET

; Increment mantissa in CDE and handle overflow 
;
FMantissaInc                    
    INR E                       ; add one to the lower order
    RNZ                         ; all done if it is not zero
    INR D                       ; add one to next highest order
    RNZ                         ; all done if no overflow
    INR C   
    RNZ 
    MVI C,80h                   ; Mantissa overflowed to zero, so set it
    INR M                       ; to 1 and increment the exponent.
    RNZ                         ; And if the exponent overflows...
Overflow    
    MVI E,0Ah   
    JMP Error

; Add mantissa pointed to by HL to the one in CDE
;
FAddMantissas                   
    MOV A,M 
    ADD E                       ; do reg E (LO) first
    MOV E,A 
    INX H   
    MOV A,M 
    ADC D                       ; do reg D, next highest order
    MOV D,A 
    INX H   
    MOV A,M 
    ADC C                       ; and finally reg C, highest order
    MOV C,A 
    RET 

; Negate the 32-bit integer in CDEB by subtracting it from zero.
; It also flips the sign in FTEMP.
; Called by FAsInteger and FAdd.
;
FNegateInt  
    LXI H,FTEMP 
    MOV A,M 
    CMA 
    MOV M,A 
    XRA A   
    MOV L,A 
    SUB B   
    MOV B,A 
    MOV A,L 
    SBB E   
    MOV E,A 
    MOV A,L 
    SBB D   
    MOV D,A 
    MOV A,L 
    SBB C   
    MOV C,A 
    RET 

; Shift the mantissa in CDE by 'A' places.
; Note that lost bits end up in B
;
FMantissaRtMult 
    MVI B,00h                   ; Initialise extra mantissa byte
    INR A   
    MOV L,A 
RtMultLoop  
    XRA A   
    DCR L   
    RZ  
    CALL FMantissaRtOnce    
    JMP RtMultLoop  

; Shift the mantissa in CDE one bit right
;
FMantissaRtOnce 
    MOV A,C 
    RAR 
    MOV C,A 
    MOV A,D 
    RAR 
    MOV D,A 
    MOV A,E 
    RAR 
    MOV E,A 
    MOV A,B                     ; NB: B is the extra
    RAR                         ; mantissa byte.
    MOV B,A 
    RET 

; Multiplication
;
; The steps for multiplying two numbers (lhr,rhs) is:
;  1. Exit if rhs=0
;  2. Add the lhs and rhs exponents
;  3. Initialize result mantissa to 0
;  4. Get rightmost bit of rhs
;  5. If this bit is set then add the lsh mantissa to result mantissa
;  6. Shift result mantissa one bit to the right
;  7. Get next bit of rhs mantissa.  If not done, loop back to #5
;  8. When all 24 bits done, Normalize the result.
;
FMul    
    POP B                       ; Get lhs in BCDE
    POP D   
    RST 5                       ; FTestSign If rhs==0 then exit
    RZ  
    MVI L,00h                   ; L=0 to signify exponent add
    CALL FExponentAdd   
    MOV A,C 
    STA FMulInnerLoop+13    
    XCHG    
    SHLD FMulInnerLoop+8    
    LXI B,0000h 
    MOV D,B 
    MOV E,B 
    LXI H,FNormalize+3  
    PUSH H  
    LXI H,FMulOuterLoop 
    PUSH H  
    PUSH H  
    LXI H,FACCUM    
FMulOuterLoop   
    MOV A,M                     ; A=FAC mantissa byte
    INX H   
    PUSH H                      ; Preserve FAC ptr
    MVI L,08h                   ; 8 bits to do
FMulInnerLoop   
    RAR                         ; Test lowest bit of mantissa byte
    MOV H,A                     ; Preserve mantissa byte
    MOV A,C                     ; A=result mantissa's high byte
    JNC L0919                   ; If that bit of multiplicand was 0, 
                                ; then skip over adding mantissas.
    PUSH H  
    LXI H,0000h 
    DAD D   
    POP D   
    ACI 00                      ; A=result mantissa high byte. This gets back to C
    XCHG                        ; in the call to FMantissaRtOnce+1.
L0919   
    CALL FMantissaRtOnce+1  
    DCR L   
    MOV A,H                     ; Restore mantissa byte and
    JNZ FMulInnerLoop           ; jump back if L is not yet 0.
PopHLandReturn  
    POP H                       ; Restore FAC ptr
    RET 

; Divide by Ten.  Used in FOut to bring the number into range before printing
;
FDivByTen   
    CALL FPush                  ; save number
    LXI B,8420h                 ; BCDE=(float)10;
    LXI D,0000h 
    CALL FLoadFromBCDE                      

FDiv    
    POP B    
    POP D   
    RST 5                       ; check for division by zero 
    JZ DivideByZero             ; not so fast
    MVI L,$FF                   ; subtract exponents, L is flag
    CALL FExponentAdd   
    INR M                       ; add 2 to exponent to correct scaling
    INR M                       
    DCX H                       ; point to highest order of FAC
    MOV A,M                     ; get MSB
    STA FDivA+1                 ; and save it.
    DCX H                       ; point to middle order byte
    MOV A,M 
    STA FDivB+1                 ; put it where nothing will hurt it
    DCX H                       ; point to lowest order byte
    MOV A,M                     
    STA FDivC+1                 ; and save LO

; The numerator will be kept in BHL.  The quotient will be formed in CDE.
; To get a bit of the quotient, we first save BHL on the stack, then
; subtract the denominator that we saved in memory.  The carry indicates
; whether or not BHL was bigger than the denominator.  If BHL was bigger, 
; the next bit of the quotient is a 1.  To get the old BHL off the stack,
; we pop them into the PSW.  If the denominaor was bigger, the next bit of
; the quotient is 9, and we get the old BHL back by popping it off the 
; stack.  We have to keep an extra bit of the quotient in FDIVG+1 in case the
; denominator was bigger, then BHL will get shifted left.  If the MSB of
; B was one, it has to be stored somewhere, so we store in FDIVG+1.   Then 
; the next time through the loop BHL will look bigger because it has an
; extra HO bit in FDIVG+1.  We are done dividing when the MSB of C is a one.
; This occures when we have calculated 24 bits of the quotient.  When we jump
; to Round, the 25tgh bit of the quotient determines whether we round or not.
; It is in the MSB of A.  If initially the denominator is bigger then the 
; numerator, the first bit of the quotient will be zero.  This mean we
; will go through the divide loop 26 times, since it stops on the 25th bit
; after the first non-zero bit of the exponent.  So, this quotient will look
; shifted left one from the quotient of two numbers in which the numerator is
; bigger.  This can occur only on the first time through the loop, so CDE
; are all zero.  So, if we finish the loop in CDE are all zero, then we
; must decrement the exponent to correct for this.

    MOV B,C                     ; get number in BHL
    XCHG    
    XRA A                       ; zero CDE and highest order
    MOV C,A 
    MOV D,A 
    MOV E,A 
    STA FDivG+1 
FDivLoop    
    PUSH H                      ; save LO's of number
    PUSH B                      ; save HO of number
    MOV A,L                     ; subtract number that was in FAC
FDivC:
    SUI 00h                     ; subtract LO
    MOV L,A                     ; and save it
    MOV A,H                     
FDivB:
    SBI 00                      ; subtract MO (Middle Order)
    MOV H,A                     ; and save it
    MOV A,B 
FDivA: 
    SBI 00                      ; subtract HO
    MOV B,A                     ; and save it
FDivG:
    MVI A,00h                   ; get highest order
    SBI 00                      ; subtract the carry from it
    CMC                         ; set carry to correspond to next quotient bit
    JNC FDiv2                   ; get old number bvack if subtracted too much
    STA FDivG+1                 ; update highest order  
    POP PSW                     ; the subtraction was good
    POP PSW                     ; get previous number off stack
    STC                         ; next bit in quotient is a one
    .DB $D2 ;JNC ....           ; jnc around next 2 bytes
FDiv2:
    POP B                       ; we subtracted too much
    POP H                       ; get old number back
    MOV A,C                     ; are we done?
    INR A                       ; set sign flag without affecting carry
    DCR A                       
    RAR                         ; put carry in MSB
    JM FRoundUp+1               ; we are done
    RAL                         ; we arent done, get old carry back
    CALL FMantissaLeft          ; rotate everything left one
    DAD H                       ; rotate a zero into right end of number
    MOV A,B                     ; the HO byte, finally!
    RAL 
    MOV B,A 
    LDA FDivG+1                 ; rotate the highest order  
    RAL 
    STA FDivG+1 
    MOV A,C                     ; +1 to exponent if the 1st subtraction
    ORA D                       ; did not work
    ORA E   
    JNZ FDivLoop                ; this isn't the case
    PUSH H                      ; save part of number
    LXI H,FACCUM+3              ; get pointer to FAC
    DCR M                       ; decrement exponent
    POP H                       ; get number back
    JNZ FDivLoop                ; divide more if no overflow occured
    JMP Overflow                ; oops, overflow!

; This code is called by FMul and FDiv.  It's main job is to add (for FMul) or subtract
; (for FDiv) the binary exponents of the two arguments.
; on entry, L=0 for additon/Multiplication and L=FF for subtraction/Division
;
FExponentAdd    
    MOV A,B                     ; BCDE=0?
    ORA A                       ; test it
    JZ MulDv2                   ; if it is, zero FAC & we are done
    MOV A,L                     ; A=0 for add, FF for subtract.
    LXI H,FACCUM+3              ; get pointer to exponent
    XRA M                       ; get FAC exponent
    ADD B                       ; Add in register exponent
    MOV B,A                     ; save it
    RAR                         ; check for overflow
    XRA B                       ; overflow if sign is same as carry
    MOV A,B                     ; get sum
    JP MulDv1                   ; we have an overflow!
    ADI $80                     ; add exponent bias     
    MOV M,A                     ; and save it in the FAC
    JZ PopHLandReturn           ; we have underflow!
    CALL FUnpackMantissas       ; unpack the arguments
    MOV M,A                     ; save the new sign
    DCX H                       ; point to exponent
    RET                         ; all done, leave HO in A
MulDv1: 
    ORA A                       ; is error overflow or underflow?
MulDv2:
    POP H                       ; get old return addr off stack
    JM Overflow                 ; Overflow! 
                                ; Underflow.. fall into FZero below

; set the floating point accumulator to zero.
; Zero is stored in a special way: the exponent is zero'd without bias
;
FZero   
    XRA A                       ; A=0
    STA FACCUM+3                ; set exponent=0, that means FAC=0.
    RET 

; Multiply FAC by ten, as fast as possible
;
FMulByTen   
    CALL FCopyToBCDE            ; get number into registers
    MOV A,B                     ; get exponent
    ORA A                       ; result zero if arg is zero...
    RZ                          ; and it is
    ADI 02                      ; adding 2 to exponent = multiply by 4
    JC Overflow                 ; oops, overflow
    MOV B,A                     ; restore exponent
    CALL FAdd+2                 ; add in orginal number = multiply by 5    
    LXI H,FACCUM+3              ; point to exponent again
    INR M                       ; +1 exponent = doubling = overall multiply by 10
    RNZ                         ; all done if no overflow
    JMP Overflow                ; overflow

; When FAC is non-zero, RST FTestSign jumps here to get the sign as an integer
; returns 01 = positive, FF=regative
;
FTestSign_tail  
    LDA FACCUM+2                ; look at MSB of mantissas  
    .DB $FE                     ; skip over next instruction
InvSignToInt    
    CMA                         ; inverts sign bit in A
SignToInt   
    RAL                         ; rotate sign bit (bit7) into carry
    SBB A                       ; FF if carry set, 0 otherwise
    RNZ                         ; returns if sign bit was set 
    INR A                       ; change A=0 to A=1, setting flags
    RET 

; Returns an integer that indicates FAC's sign.
; First, call FTestSign which gets answer in A, then fall into
; FCharToFloat to get that answer back into the FAC
;
Sgn 
    RST 5                       ; FTestSign 

; Convert a signed byte in A to a floating-point number in FAC
;
FCharToFloat    
    MVI B,88h                   ; set exponent correctly
    LXI D,0000h                 ; zero DE
    LXI H,FACCUM+3              ; get pointer to FAC
    MOV C,A                     ; registers now have A as unnormalized FP number
    MOV M,B                     ; set FAC exponent to 2^8
    MVI B,00h                   ; zero overflow byte
    INX H                       ; point to FTEMP
    MVI M,80h                   ; set it to x80 in prep for FNormalize
    RAL                         ; put sign flag into carry 
    JMP FNormalize              ; go float the number

; Absolute function:  FAC = |FAC|
;
Abs 
    RST 5                       ; FTestSign 
    RP                          ; if FAC positive, we are done

FNegate                         ; negate sign in FAC
    LXI H,FACCUM+2  
    MOV A,M                     ; get MSB into A
    XRI $80                     ; flip the sign bit
    MOV M,A                     ; and save it
    RET 

; Push the Floating point accumulator on the stack
;
FPush   
    XCHG                        ; save HL   
    LHLD FACCUM                 ; get LO's
    XTHL                        ; swap LO's and return addr
    PUSH H                      ; put ret addr back on stack
    LHLD FACCUM+2               ; get HO's
    XTHL                        ; switch HO's and return addr
    PUSH H                      ; put ret addr back on stack
    XCHG                        ; get old HL back
    RET                         ; all done

; Move number from memory (HL) into FAC
;
FLoadFromMem    
    CALL FLoadBCDEfromMem       ; get number into registers
FLoadFromBCDE   
    XCHG                        ; put DE part of mantissa (LO's) in HL
    SHLD FACCUM                 ; store it in FAC
    MOV H,B                     ; get BV part of mantiissa (HO's) in HL
    MOV L,C                     
    SHLD FACCUM+2               ; store it at FACCUM+2
    XCHG                        ; get old HL back
    RET 

FCopyToBCDE 
    LXI H,FACCUM                ; point to FAC  
FLoadBCDEfromMem    
    MOV E,M                     ; get LO
    INX H                       ; point to MO
    MOV D,M                     ; get MO
    INX H                       ; point to HO
    MOV C,M                     ; get HO
    INX H                       ; point to exponent
    MOV B,M                     ; get exponent
IncHLReturn 
    INX H                       ; point to beginning of next number
    RET                         ; and done

; Move number from FAC to memory (HL)
;
FCopyToMem  
    LXI D,FACCUM                ; get pointer to FAC
    MVI B,04h                   ; count 4 bytes to copy
FCopyLoop   
    LDAX D                      ; get byte from FAC
    MOV M,A                     ; and store in in memory
    INX D                       ; increment pointers
    INX H   
    DCR B                       ; decrement counter
    JNZ FCopyLoop               ; loop until done
    RET 

; The numbers in the BCDE and in the FAC are unpacked, with the assumed '1' in the mantissas
; restored.  A XOR combination of both (register and FAC) sign bits is stored in
; FTEMP_SIGN.  On return, A is positive if the signs were different and negative if the
; signs were the same.
;
FUnpackMantissas    
    LXI H,FACCUM+2              ; point to HO/sign
    MOV A,M                     ; get HO/sign
    RLC                         ; Move FAC's sign to bit 0.
    STC                         ; Set MSB of FAC mantissa,
    RAR                         ; FAC's sign is now in carry.
    MOV M,A 
    CMC                         ; Negate FAC's sign.
    RAR                         ; Bit 7 of A is now FAC's sign.
    INX H                       ; Store negated FAC sign @ FTEMP_SIGN.
    INX H   
    MOV M,A 
    MOV A,C 
    RLC                         ; Set MSB of BCDE mantissa,
    STC                         ; BCDE's sign is now in carry.
    RAR 
    MOV C,A 
    RAR                         ; Bit 7 of A is now BCDE's sign
    XRA M                       ; XORed with FTEMP_SIGN.
    RET 

; Compares FACCUM to BCDE, with the result being returned in A as follows :
; FACCUM > BCDE, A = 0x01.
; FACCUM < BCDE, A = 0xFF.
; FACCUM = BCDE, A = 0.   
;    
FCompare    
    MOV A,B                     
    ORA A                       ; is BCDE is zero (B=0)?        
    JZ FTestSign                ; if so, no need to compare
    LXI H,InvSignToInt       
    PUSH H                      ; set return addr to InvSignToInt
    RST 5                       ; Test FACCUM's sign
    MOV A,C 
    RZ  
    LXI H,FACCUM+2  
    XRA M   
    MOV A,C 
    RM  
    CALL FIsEqual               ; test for equality
    RAR                         ; get carry into A bit 7    
    XRA C   
    RET 

; Test for equality between FAC and BCDE
;
FIsEqual    
    INX H                       ; point to exponent
    MOV A,B                     ; and exponent of argument
    CMP M                       ; compare the two
    RNZ                         ; numbers are different
    DCX H                       ; point to HO
    MOV A,C                     ; get HO of arg
    CMP M                       ; compare with HO of FAC
    RNZ                         ; they are different
    DCX H                       ; point to MO of FAC
    MOV A,D                     ; get MO of arg
    CMP M                       ; compare with MO of FAC
    RNZ                         ; they are different
    DCX H                       ; point to LO of FAC
    MOV A,E                     ; get MO of argument
    SUB M                       ; subtract LO of FAC
    RNZ                         ; numbers are different
    POP H                       ; BCDE=FAC, so drop 1st return addr
    POP H                       ; and second return addr
    RET                         ; Return to caller of FCompare instead

; Return the integer part of FAC in CDE
;
; The hard case is negative non-integers.  To handle this, if the number is negative,
; we regard the 3-byte mantissa as a 3-byte integer and subtract one.  Then all the
; fractional bits are shifted out by shifting the mantissa right.  Then, if the number is
; negative, we add one.  So, if we had a negative integer, all the bits to the right of the
; binary point were zero.  So the net effect is we have the original number in CDE.  If the
; number is a negative non-integer, there is at least one non-zero bit to the right of the
; binary point, so the net effect is that we get the absolute value of int(fac) in CDE.
; CDE is then negated if the orginal number was negative so the result will be signed.
;
FAsInteger  
    MOV B,A                     ; set BCDE = 0
    MOV C,A 
    MOV D,A 
    MOV E,A 
    ORA A                       ; if A=0
    RZ                          ; special case done
    PUSH H                      ; save HL on stack
    CALL FCopyToBCDE            ; copy FAC to BCDE
    CALL FUnpackMantissas       ; restore assumed '1' bit
    XRA M                       ; Get sign back
    MOV H,A                     ; save the sign in H
    CM FMantissaDec             ; subtract 1 from LO if number is negative              
    MVI A,98h                   ; see how many we have to shift to change
    SUB B                       ;   the number to an integer
    CALL FMantissaRtMult        ; shift number to get rid of fractional bits
    MOV A,H                     ; get sign
    RAL                         ; put sign in carry bit so it will not be changed
    CC FMantissaInc             ; if number was negative, add one
    MVI B,00h                   ; forget the bits we shifted out
    CC FNegateInt               ; negate number if it was negative because
                                ; wanted a signed mantissa
    POP H                       ; get old HL back
    RET 

; Decrement the mantissa in CDE
;
FMantissaDec    
    DCX D                       ; start by subtracting 1 from DE
    MOV A,D                     ; we have to subtract one from C if
    ANA E                       ; D and E are both FF...
    INR A                       ; see if they both were FF
    RNZ                         ; they were not, we are done
    DCR C                       ; they were, so subtract one from C
    RET

; Remove the fractional part of FAC
;
Int     
    LXI H,FACCUM+3              ; point to FAC exponent
    MOV A,M                     ; get it    
    CPI $98                     ; is it >= 2^24?
    RNC                         ; too big to hold fraction; already an int
    CALL FAsInteger             ; convert FAC to integer in CDE.  HL points to exponent
    MVI M,98h                   ; change exponent to 2^24
    MOV A,C                     
    RAL 
    JMP FNormalize

; Reads a string and converts it to a floating point number in FAC.
;
; At Entry, HL points to the first character in the text buffer and the first char is
; also in A.  We pack the digits into the FAC as an integer and keep track of where
; the decimal point is.  C=FF if we have not seen a decimal point, C=0 if we have.
; B is the number of digits after the decimal point.  At the end, B and the exponent (in E)
; are used to determine how many times we multiply or divide by ten to get the
; correct number.
;
FIn 
    DCX H   
    CALL FZero                  ; start with FAC=0
    MOV B,A                     ; B=count of fractional digits
    MOV D,A                     ; D=exponent sign (FF for minus, 01 for plus)
    MOV E,A                     ; E=exponent
    CMA                         ; C= decimal point flag ($FF for no, $00 for yes)
    MOV C,A 
FInLoop 
    RST 2                       ; get the next char in string   
    JC ProcessDigit             ; if its a digit, process it below
    CPI '.'                     ; is it a decimal point?
    JZ L0AE4                    ; yes, handle below
    CPI 'E'                     ; only other choice is 'E' for exponent
    JNZ ScaleResult             ; if not that, end of number has been reached
GetExponent 
    RST 2                       ; get first character of exponent (after E) 
    DCR D                       ; set sign of exponent flag
    CPI $99                     ; is it a negative exponent?
    JZ NextExponentDigit        ; yes
    INR D                       ; no, so reset flag
    CPI $98                     ; ignore "+"
    JZ NextExponentDigit    
    DCX H                       ; check if last char was a digit
NextExponentDigit   
    RST 2                       ; get next character    
    JC DoExponentDigit          ; is it a digit?
    INR D                       ; no, so exponent all in
    JNZ ScaleResult             ; set its sign
    XRA A   
    SUB E   
    MOV E,A 
    INR C                       ; make sure C is not FF

    ; here to check if we have seen 2 deciment points and set the 
    ; decimal point flag
L0AE4   
    INR C                       ; decimal points - set flage
    JZ FInLoop                  ; continue scanning characters

    ; here to multiply or divide by 10 the correct number of times
    ; we have already read in all the digits
ScaleResult 
    PUSH H                      ; save pointer for later
    MOV A,E                     ; get exponent
    SUB B                       ; exp = exp - # of decimal places
DecimalLoop 
    CP DecimalShiftUp           ; x10 if exponent is positive
    JP DecimalLoopEnd           ; /10 if exponent is negative
    PUSH PSW                    ; save exponent
    CALL FDivByTen              ; divide number by 10
    POP PSW                     ; get back exponent
    INR A                       ; increment it
DecimalLoopEnd  
    JNZ DecimalLoop             ; loop if not done
    POP H                       ; restore text pointer
    RET 

; Helper function for shifting the result decimally up one place
; We do this only if A (holding exponent) not zero.  Adjust exponent in A.
;
DecimalShiftUp  
    RZ                          ; dont do if exponent is zero
    PUSH PSW                    ; save exponent
    CALL FMulByTen              ; muliply result X 10
    POP PSW                     ; restore A (the exponent)
    DCR A                       ; and decrement it
    RET

; Pack the next digit of the number into the FAC
; We multiply the FAC by ten and add in the digit.
;
ProcessDigit    
    PUSH D                      ; save exponent information
    MOV D,A                     ; protect digit from below
    MOV A,B                     ; increment decimal place count
    ADC C                       ;  if past the decimal point
    MOV B,A 
    PUSH B                      ; save necessary data
    PUSH H  
    PUSH D  
    CALL FMulByTen              ; muliply old number by 10
    POP PSW                     ; get digit to add
    SUI '0'                     ; convert ASCII digit to binary
    CALL FPush                  ; put FAC on stack
    CALL FCharToFloat           ; convert to floating point    
    POP B                       ; recall old FAC into BCDE
    POP D   
    CALL FAdd+2                 ; add in new digit
    POP H                       ; recall data
    POP B   
    POP D   
    JMP FInLoop                 ; get next character

; Handle an exponent digit by multiplying current exponent in E by 10,
; then adding the digit value to it
; 
DoExponentDigit 
    MOV A,E                     ; get exponent
    RLC                         ; exp x 2
    RLC                         ; exp x 4
    ADD E                       ; exp x 5 by adding one
    RLC                         ; exp x 10
    ADD M                       ; add current (ASCII) digit
    SUI '0'                     ; convert ASCII to binary
    MOV E,A                     ; save new exponent value
    JMP NextExponentDigit       ; and continue processing

; Prints "IN " and then falls into PrintInt
; Use by error handling code to print things like "SN ERROR IN 50"
;
PrintIN 
    PUSH H                      ; save line number  
    LXI H,szIn                  ; point to "IN " string
    CALL PrintString            ; print it
    POP H                       ; restore line number

; Prints an Integer
; Promotes the integer in HL to a floating pointer number in FAC, sets the
; return address to PrintSz-1, and falls into FOout.
; The integer starts off occupying the least significant bits of the mantissa in CDE.
; The exponent is set to 24, yielding an unnormalized but valid FP value
;
PrintInt    
    XCHG                        ; DE=integer
    XRA A                       ; A=0 (ends up in C)
    MVI B,98h                   ; B (exponent) = 24
    CALL FCharToFloat+5         ; convert to floating point number
    LXI H,PrintString-1 
    PUSH H                      ; push return address on stack
                                ; and fall into FOut

; FOUT: Print a floating point number to the terminal
;
FOut    
    LXI H,FBUFFER               ; HL points to start of character buffer
    PUSH H                      ; save it
    RST 5                       ; Get sign of number    
    MVI M,' '                   ; print a space if positive
    JP DoZero   
    MVI M,'-'                   ; print a minus sign of negative
DoZero  
    INX H                       ; increment pointer to next char
    MVI M,'0'                   ; put 0 in buffer in case number is zero
    JZ NullTerm-3               ; do it if number is zero
    PUSH H                      ; save buffer pointer
    CM FNegate                  ; negate number if negative

    ; Here we get the FAC in the range 100,000 - 999,999 and round it to an integer.
    ; We keep a count of how many times we multiply or divide by ten so we know what
    ; the exponent will be.  The FAC is then converted to an integer in CDE.  We use
    ; a table of powers of ten to calculate each digit.
    ; This algorithm is used for speed.

    XRA A                       ; zero out exponent counter
    PUSH PSW                    ; put ten's exponent count on stack
    CALL ToUnder1000000         ; see if number is too big or too small
ToOver100000    
    LXI B,9143h                 ; BCDE=(float)100,000.
    LXI D,4FF8h 
    CALL FCompare               ; If FACCUM >= 100,000
    INR A                       ; !!! (conv FF to 0, 0 to 1, 1 to 2)    
    JNZ  PrepareToPrint         ; !!! Number is in range
    ;JPO PrepareToPrint         ; ORIGINAL CODE modified to work in Z80 mode
    POP PSW                     ; restore exponent counter
    CALL DecimalShiftUp+1       ; multipy by ten to get it in range
    PUSH PSW                    ; save exp counter
    JMP ToOver100000            ; see if number is now in range
L0B71   
    CALL FDivByTen              ; number too big so divide by ten
    POP PSW 
    INR A                       ; add one to exponent
    PUSH PSW    
    CALL ToUnder1000000         ; is number less than 1,000,000?

    ; now number is in printing range, ie
    ; all digits to be printed are the integer part

PrepareToPrint  
    CALL FAddOneHalf            ; round number to nearest integer
    INR A                       ; make reg A non-zero, since number is positive and non-zero
                                ; round will exit with the HO in A, so the MSB will always
                                ; be zero and adding one will never cause reg A to be zero
    CALL FAsInteger             ; get integer part in CDE
    CALL FLoadFromBCDE  
    LXI B,0206h                 ; set decimal point count for E notation
    POP PSW                     ; get exponent counter
    ADD C                       ; and add in C=digit count=6
    JM L0B95                    ; print E notation if answer <1
    CPI $07 
    JNC L0B95                   ; print E notation if > 999,999
    INR A   
    MOV B,A                     ; B = decimal point count
    MVI A,01h                   ; set fixed point flag, the exponent is zero
L0B95   
    DCR A   
    POP H                       ; HL=output buffer
    PUSH PSW                    ; Preserve exponent adjustment (and preserve zero flag used to indicate scientific notation wanted).
    LXI D,DECIMAL_POWERS        ; store loc of largest power of ten

    ; This is the outer loop of printing, where each ASCII digit is calculated in turn.  
    ; We start by writing the decimal point, but we only advance HL to keep it if B=0,
    ; which means that the decimal point has been reached

NextDigit   
    DCR B                       ; see if it is time to print a decimal point
    MVI M,'.'                   ; put a decimal point in the buffer
    CZ IncHLReturn              ; increment buffer pointer if it is time
    PUSH B                      ; save flags
    PUSH H                      ; save character pointer
    PUSH D                      ; save power of ten pointer
    CALL FCopyToBCDE            ; get number in CDE
    POP H                       ; get power of ten pointer
    MVI B,'0'-1                 ; B = next digit to be printer

    ; Work out the digit corresponding to the current decimal power. 
    ; We do this by subtracting the decimal power (eg 100) from CDE until it overflows, 
    ; and incrementing the ASCII digit value in B each time. When it overflows, 
    ; we have our digit. And when it overflows, we call FAddMantissas to undo the 
    ; last subtraction which was one step too far.

DigitLoop   
    INR B                       ; add one to digit
    MOV A,E                 
    SUB M                       ; subtract LO    
    MOV E,A 
    INX H                       ; point to next byte of power of ten
    MOV A,D 
    SBB M                       ; subtract MO
    MOV D,A 
    INX H   
    MOV A,C 
    SBB M                       ; subract HO
    MOV C,A 
    DCX H                       ; point to beginning of power of ten
    DCX H   
    JNC DigitLoop               ; subtract again if result was positive
    CALL FAddMantissas          ; it wasn't, add power of ten back in
    INX H                       ; increment pointer to next power of ten
    CALL FLoadFromBCDE          ; save CDE in FAC

    ; Write out the digit.  Loop back if there are more to do

    XCHG                        ; DE = power of ten pointer         
    POP H                       ; HL = output buffer pointer
    MOV M,B                     ; put ASCII digit in the output buffer
    INX H                       ; advance buffer pointer
    POP B                       ; get counters off stack
    DCR C                       ; was that the last digit?
    JNZ NextDigit               ; do more if not
    DCR B                       ; see if decimal point goes after last digit
    JZ L0BDB                    ; it does, we have no zeroes to suppress

    ; suppress the trailing zeroes

L0BCF   
    DCX H                       ; go back to last character
    MOV A,M                     ; get it
    CPI '0'                     ; ignore trailing zeroes
    JZ L0BCF                    ; until all done

    ; suppress decimal point if we have an integer

    CPI '.'                     ; ignore dp before trailing zeroes
    CNZ IncHLReturn             ; if no dp, move ptr to next position
L0BDB   
    POP PSW                     ; get decimal exponent
    JZ NullTerm                 ; return if number was in fixed point format

    ; scientific format: write the exponent part

    MVI M,'E'                   ; put an 'E' in the buffer
    INX H                       ; put sign of exponent in buffer
    MVI M,'+'                   ; Write '+' if exponent if positive
    JP L0BEB    
    MVI M,'-'                   ; Write '-' if it's negative, also
    CMA                         ; negate (two's complement) the decimal exponent
    INR A                       ; so printing it will work.

    ; Work out the 1st digit of exponent in B.
    ; Done by usual method of subtracting 10 until it overflows

L0BEB   
    MVI B,'0'-1                 ; initialize ten's digit count
ExpDigitLoop    
    INR B                       ; increment digit
    SUI 0Ah                     ; subtract 10
    JNC ExpDigitLoop            ; do it again if result was positive
    ADI 3Ah                     ; add back 10 and convert to ASCII
    INX H                       
    MOV M,B                     ; put ten's digit of exponent in buffer
     INX H                      
    MOV M,A                     ; put one's digit of exponent in buffer
    INX H   
NullTerm    
    MOV M,C                     ; put null byte at end of buffer
    POP H                       ; exit with HL pointing to string
    RET                         ;

; Divides the FACCUM by ten until its less than 1M.
ToUnder1000000
    LXI B,9474h 
    LXI D,23F7h                 ; BCDE 947423F7 = (float)1,000,000
    CALL FCompare               ; if FACCUM bigger than 1M? (Z=0, YES=01, NO=FF)
    POP H                       ; retrieve return address
    DCR A                       ; !!!
    JZ  L0B71                   ; !!! remove parity check operation  
    ; JPE L0B71                 ; ORIGINAL CODE number too big, divide by 10 
    PCHL                        ; number OK so return

ONE_HALF    
    .DB $00,$00,$00,$80         ; DD 0.5     
DECIMAL_POWERS  
    .DB $A0,$86,$01             ; DT 100,000     
    .DB $10,$27,$00             ; DT 10,000  
    .DB $E8,$03,$00             ; DT 1000    
    .DB $64,$00,$00             ; DT 100     
    .DB $0A,$00,$00             ; DT 10  
    .DB $01,$00,$00             ; DT 1  


; THE SQUARE ROOT FUNCTION   X = SQR(A)
;
; First we scale the argument to between 0.5 and 2 by looking at the exponent 
; and using sqr(M x 2^(2*N)) = 2^N * SQR(M)
; Then Newton's method is used to compute SQR(M).  The exponent is save to scale
; the result at the end. 
;
; Newton's method for square root:
;    X(0)   = A 
;    X(n+1) = (X(n) + A/X(n))/2
;
Sqr 
    RST 5                       ; FTestSign - check for error condition 
    JM FunctionCallError        ; cant take sqr of negative number
    RZ                          ; sqr(0) = 0
    LXI H,FACCUM+3              ; scale argument between 0.5 andf 2
    MOV A,M                     ; get exponent
    RAR                         ; get exponent of scale factor
                                ; use sqr(M x 2^(2*N)) = 2^N * SQR(M)
    PUSH PSW                    ; save it
    PUSH H                      ; save pointer to exponent
    MVI A,40h                   ; set exponent of scaled down number
    RAL 
    MOV M,A                     ; replace it
    LXI H,FBUFFER               
    CALL FCopyToMem             ; save A
    MVI A,04h                   ; set iteration count
SqrLoop 
    PUSH PSW                    ; save count
    CALL FPush                  ; save X(n)
    LXI H,FBUFFER               
    CALL FLoadBCDEfromMem       ; get A in the registers
    CALL FDiv+2                 ; compute A/X(n)
    POP B   
    POP D   
    CALL FAdd+2                 ; add in X(n)
    LXI B,8000h                 ; set up BCDE with 0.5
    MOV D,C 
    MOV E,C 
    CALL FMul+2                 ; multiply by 0.5, same as divide by 2
    POP PSW                     ; get iteration count
    DCR A                       ; are we done?
    JNZ SqrLoop                 ; no, do more iterations
    POP H                       ; yes, set exponent of answer
    POP PSW                     ; get scale factor
    ADI $C0                     ; convert it to an exponent
    ADD M                       ; add exponent in
    MOV M,A                     ; replace exponent
    RET 


; PSEUDO-RANDOM NUMBER GENERATOR
;
; If argument is zero, it returns the last number generated
; If argument is negative, a new sequence of random number is started using the arg.
; 
; To form the next random number in the sequence, we multiply the previous random
; number by a constant, and add in another constant.  Then the high- and low- order
; bytes are swapped, the exponent is put where it will be shifted in by normal, and the
; exponent in the FAC set so that the result will be less than one.  This is then normalized
; and saved for the next time.  The HO and LO bytes are swapped so we have a random chance
; of setting a number less than or greater than 0.5
;
Rnd 
    RST 5                       ; FTestSign - get sign of the argument  
    JM L0C7C                    ; start new sequence of negative
    LXI H,RND_SEED              ; get last number generated
    CALL FLoadFromMem   
    RZ                          ; return last number if argument is 0
    LXI B,9835h                 ; load BCDE with constant A
    LXI D,447Ah 
    CALL FMul+2                 ; multiply by constant A (x 11,879,546)
    LXI B,$6828                 ; load BCDE with constant B            
    LXI D,$B146 
    CALL FAdd+2                 ; add in constant B (+ 0.0000003927678)
L0C7C   
    CALL FCopyToBCDE            ; switch HO and LO bytes as follows...
    MOV A,E                     ; get LO
    MOV E,C                     ; put HO in LO byte
    MOV C,A                     ; put LO in HO byte
    MVI M,80h                   ; make result positive
    DCX H                       ; get pointer to exponent
    MOV B,M                     ; put exponent in overflow position
    MVI M,80h                   ; set exp so result will be btwn 0 and 1
    CALL FNormalize+3           ; normalize the result
    LXI H,RND_SEED      
    JMP FCopyToMem              ; save result as seed for next time

RND_SEED    .DB 52C74F80h   


; THE SIN FUNCTION:   A = SIN(X)
;
; Idea: use identities to get FAC in Quadrants I or IV.
; The FAC is divided by 2*pi and the integer part is ignored because
; SIN(X + 2*pi) = SIN(X).  Then the argument can be compared with pi/2 by 
; comparing the result of the division with pi/2/(2*pi) = 1/4.
; An approximation polynomial is then used to compute SIN(x).
;
Sin 
    CALL FPush                  ; push argument
    LXI B,8349h                 ; BCDE = 2*pi
    LXI D,0FDBh 
    CALL FLoadFromBCDE         
    POP B   
    POP D   
    CALL FDiv+2                 ; result = x/2pi, now between 0 and 1
    CALL FPush                  ; disregard integer part since SIN is
    CALL Int                    ;   perodic with period 2*pi
    POP B   
    POP D   
    CALL FSub+2 ;=u-INT(u)
    LXI B,7F00h                 ; BCDE = 0.25
    MOV D,C 
    MOV E,C 
    CALL FSub+2                 ; subtract 0.25           
    RST 5                       ; FTestSign to see what quadrant we are in
    STC                         ; set Quadrant I flag
    JP NegateIfPositive         ; Quadrant I, get back original X
    CALL FAddOneHalf            ; Add 0.5
    RST 5   
    ORA A                       ; clear carry
NegateIfPositive    
    PUSH PSW                    ; save quadrant flag
    CP FNegate                  ; negate if in Quadrants I, II, or III
    LXI B,7F00h                 ; BCDE = 0.25
    MOV D,C 
    MOV E,C 
    CALL FAdd+2                 ; In Quadrants II, III so add 0.25
    POP PSW                     ; get Quadrant flag
    CNC FNegate                 ; negate if in Quadrants II, III, or IV
    CALL FPush                  ; save X
    CALL FCopyToBCDE    
    CALL FMul+2                 ; square X (X = X*X)
    CALL FPush                  ; save X^2
    LXI H,TAYLOR_SERIES 
    CALL FLoadFromMem           ; move 1st constant into FAC
    POP B                       ; get X^2
    POP D   
    MVI A,04h                   ; get degree
TaylorLoop  
    PUSH PSW                    ; save #terms remaining
    PUSH D                      ; save x^2
    PUSH B  
    PUSH H                      ; save constant pointer
    CALL FMul+2                 ; multiply by x^2
    POP H                       ; get pointer to constants
    CALL FLoadBCDEfromMem       ; get constant into registers
    PUSH H                      ; save pointer
    CALL FAdd+2                 ; add in constant
    POP H                       ; move pointer to next constant
    POP B                       ; get x^2
    POP D   
    POP PSW                     ; pop #terms remaining into A.
    DCR A                       ; see if we are done
    JNZ TaylorLoop              ; no, do next term
    JMP FMul                    ; multiply by X and we are done

TAYLOR_SERIES   
    .DB $BA,$D7,$1E,$86         ; 39.710670  
    .DB $64,$26,$99,$87         ; -76.574982     
    .DB $58,$34,$23,$87         ; 81.602234  
    .DB $E0,$5D,$A5,$86         ; -41.341675     
    .DB $DA,$0F,$49,$83         ; 6.283185   
L0D17   
    .DB $00,$00,$00,$00,$00
    .DB $00,$00,$00,$00,$00     


; THE INITIALIZATION ROUTINE
;
; The first section of this code sets up I/O configuration for the ALTAIR computer,
; and therefore is not relevant to the IMSAI 8080.  This I/O code has been commented out.
;
; Next, the amount of memory, terminal width, and optional math functions to be retained
; are ascertained from the user.  A zero is put down at the first location not used by
; the math package and PROGRAM_BASE (TXTAB in original code) is set up to point at the next
; location.  This determines where user program storage will start.  Once INIT finishes, the
; memory it uses is reclamed for program storage.  The last thing INIT does is change address
; zero to be a jump to MAIN (Ready in original code) instead of INIT.  Once this is done
; there is no way to restart INIT.

Init    

; IMSAI 8080 I/O port initialzation
;
    LDA  0CAh                   ; 7 BITS, NO PARITY, 2 STOP
    OUT  CONS                   ; UART A - SET MODE 
    LDA  27h                    ; ENABLE TX & RX
    OUT  CONS                   ; UART A - SET CONTROL

;  ALTAIR teletype and terminal port initialization follows
;  None of this is needed for IMSAI 8080 port
;
;   LXI H,$0F1A 
;   SPHL    
;   SHLD STACK_TOP  
;   IN 01   
;   MVI C,$FF   ;
;   LXI D,ConfigIOcode  
;   PUSH D  
;   LDA 0FFFh      
;   MOV B,A 
;   IN $FF
;   RAR 
;   JC L0D42-1  
;   ANI 0Ch 
;   JZ L0D42    
;   MVI B,10h   
;   MOV A,B 
;L0D42  
;    STA L0D8D-1    
;   IN $FF  
;   RAL 
;   RAL 
;   MVI B,20h   
;L0D4B  
;   LXI D,$CA02 
;   RC  
;   RAL 
;   MOV B,E 
;   DCR E   
;   RC  
;   RAL 
;   JC L0D6F    
;   MOV B,E 
;   LXI D,$C280 
;   RAL 
;   RNC 
;   RAL 
;   MVI A,03h                  ; lets send byte 01
;   CALL L0D8B                 ; OUT to port 0
;   DCR A   
;   ADC A   
;   ADD A   
;   ADD A   
;   INR A   
;   CALL L0D8B                 ; and do it again.
;   STC 
;   JMP L0D4B   
;L0D6F  
;   XRA A   
;   CALL L0D8B  
;   CALL L0D87  
;   CALL L0D87  
;   MOV C,E 
;   CMA 
;   CALL L0D87  
;   MVI A,04h   
;   DCR M   
;   CALL L0D8B  
;   DCR M   
;   DCR M   
;   DCR M   
;L0D87  
;    LXI H,L0D8D-1  
;   INR M   
;L0D8B  OUT 00  
;L0D8D  RET 

; Here, the I/O status bits are configured.  It is modifying the I/O routines, shoving values in
; for the ANI bitmasks at address <function>+3
; there is no need for this with the IMSAI.

;ConfigIOcode   
;   MOV H,D 
;   MOV L,B 
;   SHLD InputChar+3    
;   MOV A,H 
;   ANI $C8 
;   MOV H,A 
;   SHLD TestBreakKey+3 
;   XCHG    
;   SHLD WaitTermReady+3    
;   LDA L0D8D-1 
;   STA InputChar+1 
;   STA TestBreakKey+1  
;   INR A   
;   STA InputChar+8 
;   ADD C   
;   STA WaitTermReady+1 
;   INR A   
;   STA InputChar-2 

;
;  Now ask the user how much memory is present, in bytes
;
    LXI H,$FFFF 
    SHLD CURRENT_LINE           ; set immediate mode
    CALL NewLine    
    LXI H,szMemorySize          
    CALL PrintString            ; print "Memory Size?"       
    CALL InputLineWith          ; get user input
    RST 2                       ; look at first char    
    ORA A                       ; user type anything?
    JNZ L0DDE                   ; if so, handle it.

    ; No answer given to the request for memory size, therefore we find the top of memory ourselves. 
    ; This is done by writing alternating 0x37 and 0x36's to progressively higher addresses and 
    ; reading the values back from memory. When the value read is not the value written, 
    ; we know we have written past the top of memory.

    LXI H,UnusedMemory          ; start where BASIC leaves off  
FindMemTopLoop  
    INX H                       ; go to next memory location
    MVI A,37h   
    MOV M,A                     ; write a 37h to memory
    CMP M                       ; did it stick?
    JNZ DoneMemSize             ; nope, we've reached the top
    DCR A                       ; now try with 36h
    MOV M,A                     ; write it to memory
    CMP M                       ; did it stick?
    JZ FindMemTopLoop           ; yes, so keep going
    JMP DoneMemSize             ; nope, we are done.

    ; Memory size has been given in bytes. 
    ; Here we convert the string input to an integer value and 
    ; error if it's 0 or a non-numeric input.

L0DDE   
    LXI H,LINE_BUFFER   
    CALL LineNumberFromStr      ; get number up to 65529 into DE
    ORA A                       ; A=0 for valid number                    
    JNZ SyntaxError             ; user didn't enter valid number
    XCHG                        ; put answer in HL
    DCX H                       ; HL - 1

DoneMemSize 
    DCX H                       
    PUSH H                      ; push result (last word of RAM) on stack

;
; Ask the user how many columns wide their terminal is. 
; This defaults to 72 if empty input is given.
;
GetTerminalWidth    
    LXI H,szTerminalWidth   
    CALL PrintString            ; print "TERMINAL WIDTH?"
    CALL InputLineWith          ; get user input
    RST 2                       ; NextChar  
    ORA A                       ; if no input,
    JZ DoOptionalFns            ; use default 72 & jump next section
    LXI H,LINE_BUFFER   
    CALL LineNumberFromStr      ; convert user input into integer in DE

    ; If user-supplied terminal width is >=256 or <16 then 
    ; that's not in range so jump back to ask again

    MOV A,D 
    ORA A                       ; make sure D=0 (therefore DE<256)
    JNZ GetTerminalWidth        ; answer too high, try again
    MOV A,E                     ; look at LSB of answer (in A)
    CPI $10                     ; less than 16?
    JC GetTerminalWidth         ; answer too low, try again
    STA OutChar_tail+1          ; store user-supplied width

    ; Calculate the column of the last tab-break and write this number 
    ; to the right place in the ToNextTabBreak function. The tab-break size is 14, so 
    ; the last tab-break is calculated as (width - ((width % 14)+14). 
    ; So for 72, the last tab brk is at column 56.
    
CalcTabBrkSize  
    SUI 0Eh 
    JNC CalcTabBrkSize  
    ADI 1Ch 
    CMA 
    INR A   
    ADD E   
    STA ToNextTabBreak+4

    ; Ask the user which optional inline functions they want. 
    ; If they answer Y(es) to any, they do not get an option to turn off support for later ones, 
    ; ie if you say yes to SIN you have implicitly accepted RND and SQR too. 
    ; If functions are turned off, the memory they occupy is reclaimed for program space 
    ; (look for where PROGRAM_BASE gets set and how it is calculated).
    ;
    ; This code will loop up to three times: first for SIN, then RND, then SQR
    ; until the user responds with a 'N'.
    ; Each time, RST 6 is called three times, to advance through the three entries
    ; in OPT_FN descriptor table devoted to the optional function: string, table entry, and function
    ; address

DoOptionalFns   
    LXI H,OPT_FN_DESCS          ; point to first optional fn (SIN)
OptionalFnsLoop 
    RST 6                       ; push next table address on stack
    LXI D,szWantSin             ; DE = address just beyond table
    RST 4                       ; compare HL & DE
    JZ L0E32                    ; if same, we are done

    ; Get the address of the string prompt in HL, print the prompt, get input
    ; put first char of that input in A

    RST 6                       ; push string prompt address on stack
    XTHL                        ; now get it into HL
    CALL PrintString            ; print the prompt "WANT SIN?", etc        
    CALL InputLineWith          ; get user response
    RST 2                       ; look at first char only   
    POP H                       ; restore HL
    CPI 'Y'                     ; user entered Yes?
L0E32   
    POP D                       ; if so, put program stor. start into DE
    JZ InitProgramBase          ; and jump below
    CPI 'N'                     ; if not Y or N, 
    JNZ DoOptionalFns           ; then try again

    ; user selected N=no, so get HL=addr of fun in the inline_fn table

    RST 6                       ; pushes next entry (addr of fn in inline table) on stack                 
    XTHL                        ; now get this back into HL, and put incremented pointer on stack

    ; update the inline function table, inserting "FunctionCallError"
    ; in place of the function's real address 

    LXI D,FunctionCallError 
    MOV M,E 
    INX H   
    MOV M,D 
    POP H                       ; restore HL
    JMP OptionalFnsLoop         ; go back to handle next optional function

; Get here with the Program_Base in Reg DE.  Store the value.
;
InitProgramBase 
    XCHG                        ; put program_base into HL
    MVI M,00h                   ; write a 0 first
    INX H   
    SHLD PROGRAM_BASE           ; store program_base address in variable

    ; The location of ram_end was pushed on stack about a hundred lines of code back
    ; now retrieve into HL, and put program_base on stack.

    XTHL                        ; ram end addr in HL, PROGRAM_BASE on stack
    LXI D,0F1Ah                 ; if ram end < 0F1A then Out of Memory error
    RST 4                       ; Compare HL & DE
    JC OutOfMemory  
    POP D                       ; Get PROGRAM_BASE in DE
    SPHL                        ; Set stack pointer to top of memory
    SHLD STACK_TOP              ; Set STACK_TOP to top of memory

    XCHG                        ; get PROGRAM_BASE in HL, stack top in DE
    CALL CheckEnoughMem         ; base must be 32 bytes away from stack pointer

    ; calculate bytes available as Stack_top - Program_Base - 16:

    MOV A,E 
    SUB L   
    MOV L,A 
    MOV A,D 
    SBB H   
    MOV H,A 
    LXI B,$FFF0                 ; - 16
    DAD B   
    CALL NewLine
    CALL PrintInt               ; print free bytes
    LXI H,szVersionInfo         
    CALL PrintString            ; print version info

    ; change code at start of MAIN to call PrintString instead of restarting

    LXI H,PrintString           
    SHLD Main+4 

    CALL New+1                  ; initialize vars for new program

    ; Change the JMP address at start of BASIC to jump to Main instead of INIT

    LXI H,Main                  ; get pointer to MAIN
    SHLD Start+2                ; and save it as new BASIC start point
    PCHL                        ; Now jump to MAIN  

; the following table is used ONLY for DoOptionalFns routine
;
OPT_FN_DESCS    
    .DW L0D17   
     .DW szWantSin              ; SIN option string
     .DW KW_INLINE_FNS+12       ; SIN table entry @ 0049
     .DW Sin                        ; SIN function    @ 0C8B
     .DW szWantRnd              ; RND option string
     .DW KW_INLINE_FNS+10       ; RND table entry @ 0047
     .DW Rnd                        ; RND function    @ 0C58
     .DW szWantSqr              ; SQR option string 
     .DW KW_INLINE_FNS+8            ; SQR table entry @ 0045
     .DW Sqr                        ; SQR function    @ 0C1A

szWantSin   
    .DB "WANT SIN",0            
szWantRnd   
    .DB "WANT RND",0            
szWantSqr   
    .DB "WANT SQR",0            
szTerminalWidth 
    .DB "TERMINAL WIDTH",0
szVersionInfo   
    .DB " BYTES FREE",13,13
    .DB "BASIC VERSION 3.2",13
    .DB "[4K VERSION]",13,0
szMemorySize    
    .DB "MEMORY SIZE",0
szIMSAI
    .DB "v03 BH",0

UnusedMemory                    ; mark end of ALTAIR BASIC
    .DB 00

.END