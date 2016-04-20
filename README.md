# Monitor Program for XMEGA 2015
This is my final project for my high school that I wrote in 2015. This is a very old project that I'm no longer working on. It was a big rush to do it and it is really a mess. However, to show that I did something for the final project, here it is.

Monitor Program is a program for the AVR microcontroller ATXMEGA128A1 that lets the user control the memory regions. It can do a hexdump of the program and data memories, lets the user input data at a specific RAM region and change some of the I/O peripheral registers. The default USART peripheral is USARTC0 (I don't know how to set it otherwise).

Monitor Program was at one time called Embedded Debugger because it was supposed to really do debugging. However, by a total coincidence and lack of information, Embedded Debugger *("had been"/"has been"/"is") an already used name by Atmel for an integrated circuit that does the exact same thing that I wanted this program to do. However, because of the inability to debug my microcontroller properly, I couldn't get some ASM commands to work properly which is why this isn't really a debugger, but just a research project.

Fortunately, I have a development board with the real Embedded Debugger so that I can start coding Monitor Program from scratch. Not going to promise anything, but I will surely not work on this version.

Monitor Program is programmed in the full version of BASCOM-AVR which means that you will have to buy BASCOM-AVR in order to compile Monitor Program. For now, just flash the microcontroller with the hex file that I've included, open a terminal and connect your USB/USART converter to the USARTC0 pins of your microcontroller.

Here are some commands:
1.	cmd – prints list of commands
Fast usage: cmd 1
Syntax #1: cmd (integer) – prints integer-th 16 built-in commands
Syntax #2: cmd (command name) – prints details about the command
2.	dump – prints contents of memory
Syntax #1: dump data (hex number of first address) (hex number of last address) – prints contents of the data memory from and to the specified addresses
Example of a hex address: „0x28AC“ for hex value „0x28AC“
Syntax #2: dump prog (hex number of first address) (hex number of last address) – prints contents of the program memory from and to the specified address
Syntax #3: dump preview – prints the contents last input (for example: by the command insert)
Syntax #4: dump stack – prints contents of stack
3.	insert – writes data to the data memory
Syntax #1: insert (hex number of the address at which you want to write data) (a sequence of data separated with spaces)
Example of data: „'A'“ for an 8-bit character,
„12345“ or „-3600“ for a 16-bit integer (from -32768 to 32767 for a signed integer and from 0 to 65535 for an unsigned integer),
„123f“ or „3.14159“ for a 32-bit floating point number (must have a decimal dot or the letter „f“ before the number),
„5s“ for a 8-bit integer (from -128 to 127 for a signed integer and from 0 to 255 for an unsigned integer)
4.	cls – clears the text from the terminal
Syntax #1: cls
5.	reset – resets the microcontroller
Syntax #1: reset
6.	io – controls the registers of I/O peripherals
Syntax #1: io (I/O peripheral name) – prints the contents of all registers of this I/O peripheral
Some peripherals: USARTC0, USARTC1, USARTD0, USARTD1, USARTE0, USARTE1, USARTF0, USARTF1, SPIC, SPID, SPIE, SPIF, TCC0, TCC1, TCD0, TCD1, TCE0, TCE1, TCF0, TCF1
Syntax #2: io (I/O peripheral name).(register name) – prints the contents of the said register of the said I/O peripheral
Syntax #3: io (I/O peripheral name).(register name)=(8-bit data or 16-bit for 16-bit registers) – inputs data into the said register of the said I/O peripheral
