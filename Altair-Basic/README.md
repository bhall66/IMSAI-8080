# Altair-Basic



Micro-Soft Altair Basic 3.2 \[4K]

Copyright 1975, Bill Gates, Paul Allen, Monte Davidoff



"basic-rom.hex" is a rom image file for running Altair Basic on an emulated IMSAI 8080 by TheHighNibble.



Install it in the ROMS folder of your IMSAI 8080's microSD card, then create a memory map in system.conf like this:



MEMORY 5

ram         0,256

rom         0xd8,16,basic.hex

boot        0xd800



Set your NVS configuration to 8558, 0558, 0D58, or something similar.



"basic-tape.hex" is a paper tape file.  Load it via monitor/paper tape reader and run it at address 0000. 

