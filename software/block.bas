10 REM
20 REM  DISK FILEBLOCK to OFFSET, TRACK, and SECTOR
30 REM  by Bruce E. Hall
40 REM  on 29 May 2025
50 REM
60 REM First, create an array to convert the logical sector#
70 REM into the physical sector#, assume a skew of 6
100 DIM S(26)
110 FOR I=1 TO 26 
120 READ S(I) 
130 NEXT I
140 DATA 1, 7, 13, 19, 25, 5, 11, 17, 23, 3, 9, 15, 21, 2
150 DATA 8, 14, 20, 26, 6, 12, 18, 24, 4, 10, 16, 22
160 INPUT "Block (2-242)" B
170 IF (B<2) OR (B>242) THEN END
180 REM 
190 REM Here is the good stuff!  Each block is 8 sectors,
200 REM so 8B is the number of sectors from start of directory
210 REM Divide by 26 for track, and remainder is logSector #
220 REM Offset = #sectors*128, but numbers get too big.
230 REM so do #sectors*8 instead and add a "0" digit at the end
240 REM
250 T = (8*B)\26 + 2
260 LS = (8*B) MOD 26 + 1
270 PRINT "DISK IMAGE OFFSET  (Track, Sector)"
280 FOR I=0 TO 7
290 ADDR=((T*26)+S(LS)-1)*8
300 PRINT "        ";
310 PRINT HEX$(ADDR);"0:        ";
320 PRINT T;", ";S(LS)
330 LS = LS + 1
340 IF (LS<=26) THEN GOTO 360
350 T = T+1: LS = 1
360 NEXT I
370 PRINT
380 GOTO 160