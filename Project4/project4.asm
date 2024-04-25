;Project 4
;Nate Bryan
;read data from a file 64 bytes at a time and make a new file with that read out data
;**************************************************************
Mystack SEGMENT STACK

    DW 256 (?)

MyStack ENDS
;**************************************************************
MyData SEGMENT

    ;openFile variables
    fileName DB 100 DUP (0)
    uInteger DB 100 DUP (0)
    uIntLength DW 0
    fileHandle DW 100 DUP (0)

    ;createFile variables
    newName DB 'output.dat', 0
    newFileHandle DW 100 DUP (0)

    ;readFile/formatText Variables
    fileBuffer DB 64 DUP (0)
    formattedBuffer DB 64 DUP (0)
    afterBuffer DB ?
    checkedBuffer DB 64 DUP (' ')
    lastCharacter DW 0
    formattedLength DW 0
    checkedLength DW 0

    ;timer variables
    startingTicks DW 0

    ;Error Messages
    noFile DB "No File found." 
    afterMsg DB ?
    noCreate DB "File could not be made."
    afterCreate DB ?
    cantRead DB "Couldn't read from file."
    afterRead DB ?

MyData ENDS
;**************************************************************
MyCode SEGMENT
    assume ds: MyData, cs: MyCode

    main PROC

        MOV AX, myData                             ;allows us to access the data segment
        MOV DS, AX                                 ;^^^

        CMP BYTE PTR ES:[80h], 0                   ;is there a command line
        JNE weGood                                 ;if yes continue
        MOV AH, 4Ch                                ;else terminate program
        INT 21h                                    ;^^^
weGood:

        MOV AH, 00h                                ;gets amount of ticks at start of program
        INT 1Ah                                    ;^^^
        
        LEA BX, startingTicks                      ;BX is the address of startingTicks
        MOV [BX], DX                               ;put number of starting ticks into startingTicks

        CALL clearScreen                           ;calls proc to clear screen

        CALL openFile                              ;calls proc to open a file

        MOV AX, 0B800h
	MOV ES, AX

        CALL createFile                            ;calls proc to create a new file

        CALL readFile                              ;calls proc to read data from a file

        CALL closeFile

;stuff needed for the test loop **************************************************************
;        MOV CX, 64                                 ;puts 20 into CX to act as a countdown
;        LEA DI, fileBuffer                         ;puts address of first character in fileName into DI
;        MOV SI, 160 * 5                            ;points to 5th line on screen
;
;testLoop:
;        MOV AL, [DI]                               ;move character into AL
;        MOV AH, 00011111b                          ;set color of character to be bright white on black background
;        MOV WORD PTR ES:[SI], AX                   ;put character on screen
;        INC DI                                     ;look at next character
;        INC SI                                     ;move to next position on screen
;        INC SI                                     ;^^^
;        LOOP testLoop                              ;if CX > 0 then loop
;End of test loop **************************************************************

        MOV AH, 4Ch                                ;use a DOS interrupt to terminate program
        INT 21h
        RET

    main ENDP
    

;Proc used to clear the screen
    clearScreen PROC                               
        PUSH AX CX SI ES                            ;Pushing registers on stack
        
        MOV AX, 0B800h
	MOV ES, AX

        MOV CX, 2000                               ;setting counter so that entire screen will be affected
        MOV SI, 0                                  ;set screen pointer to zero so we start at top left of screen
        MOV AX, 0720h                              ;moves an empty character into AX

;loop that clears the screen
cs1:                                   
        MOV ES:[SI],AX                             ;puts empty character on screen

        INC SI                                     ;incriments SI for next iteration of the loop
        INC SI                                     ;^^^

        LOOP cs1                                   ;goes back to top of loop as long as CX > 0

        POP ES SI CX AX                            ;pops registers out of stack
        RET                                        ;Returns value
    clearScreen ENDP

;proc to open file
    openFile PROC
        PUSH AX BX CX DI SI                        ;pushing registers to be used in proc

        MOV SI, 82h                                ;puts the address of the beginning of the command tail into SI
        LEA DI, fileName                           ;puts the address of the filename variable in the data segment into DI
        MOV CL, ES:[80h]                           ;puts the length of the command tail into CL
        DEC CL                                     ;reduces length by one cause length of tail is included in tail
        MOV BX, 0                                  ;counter to track how long uInt is 
        
;loop to get the name of the file to be opened
nameLoop:
        MOV AL, ES:[SI]                            ;moves character into AL
        DEC CL                                     ;reduces length of tail by one
        CMP AL, ' '                                ;is character a space
        JE exitName                                ;if yes we hit the end so exit loop 
        MOV [DI], AL                               ;moves character into fileName
        INC SI                                     ;look at next character
        INC DI                                     ;point to next spot in fileName
        JMP nameLoop                               ;repeat loop

exitName:

        INC SI                                     ;moves past white space to unsigned int part of tail
        LEA DI, uInteger                           ;puts address of uInteger into DI

;loop to put unsigned int into uInteger variable
unsignedLoop:
        MOV AL, ES:[SI]                            ;moves next character in tail into AL
        MOV [DI], AL                               ;moves character into uInteger
        INC SI                                     ;next character
        INC DI                                     ;next spot in uInteger
        INC BX
        LOOP unsignedLoop                          ;loop until make it to end of tail
        
        LEA DI, uIntLength                         ;points to address of uIntLength
        MOV [DI], BX                               ;puts length of int into DI

        LEA DX, fileName                           ;puts the filename into DX
        MOV AL, 0h                                 ;so that the open file can be read
        MOV AH, 3Dh                                ;interrupt that creates the filehandle to be used in other procs
        INT 21h                                    ;calls interrupt
        JC handleErrorOpen                         ;if no file is found print Error Message
        LEA BX, fileHandle                         ;if file is found put filehandle into bx
        MOV [BX], AX                               ;put filehandle into filehandle variable
        JMP endOpen                                ;jump to end of proc

;displays an error message
handleErrorOpen:
        PUSH ES
        MOV AX, 0B800h                             ;^^^
        MOV ES, AX                                 ;^^^

        LEA SI, noFile                             ;puts address of error message into SI
        MOV DI, 160 * 10 + 70                      ;makes DI point to around the middle of the screen

;prints Error Message
printEmessage:
        MOV AL, [SI]                               ;Moves a character from the message into AL
        MOV AH, 00001111b                          ;sets the color of fore and background of the character
        ADD AH, 00000000b                          ;^^^ 
        MOV ES:[DI], AX                            ;puts character on screen
        ADD DI, 2                                  ;moves to next position on screen
        INC SI                                     ;moves to next character in message
        CMP SI, OFFSET afterMsg                    ;checks if si is pointing past the message
        JE printed                                 ;if no continue
        JMP printEmessage                          ;^^^

printed:
        POP ES
endOpen:

        POP SI DI CX BX AX                            ;pops all registers used in proc
        RET
    openFile ENDP


;proc to create a new .dat file to copy the information from the first file to
    createFile PROC
        PUSH AX CX DX SI DI                        ;pushes all registers needed
        
        MOV DX, 0
        LEA DX, newName                            ;puts name of file into DL
        MOV CL, 0                                  ;puts 0 into cl so new file will be read only    
        MOV AH, 3Ch                                ;puts 3Ch into AH for the create file interrupt
        INT 21h                                    ;calls the interrupt
        JC handleErrorCreate                       ;if file was not made print error message
        JMP create1                                ;else put file info into variables for later use

;display Error Message
handleErrorCreate:
        ADD AX, '0'
        LEA BX, newFileHandle                      ;if file is found put filehandle into bx
        MOV [BX], AX                               ;put filehandle into filehandle variable

        LEA SI, noCreate                           ;puts address of error message into SI
        MOV DI, 160 * 11 + 70                      ;makes DI point to around the middle of the screen

;prints Error Message
printEmessageCreate:
        MOV AL, [SI]                               ;Moves a character from the message into AL
        MOV AH, 00001111b                          ;sets the color of fore and background of the character
        ADD AH, 00000000b                          ;^^^ 
        MOV ES:[DI], AX                            ;puts character on screen
        ADD DI, 2                                  ;moves to next position on screen
        INC SI                                     ;moves to next character in message
        CMP SI, OFFSET afterCreate                 ;checks if si is pointing past the message
        JE printedE                                ;if no continue
        JMP printEmessageCreate                    ;^^^

;error message is done printing
printedE:
        JMP endCreate                              ;skips putting anything into newFileHandle

;puts the filehandle for the new file into the variable newFileHandle
create1:
        LEA SI, newFileHandle
        MOV [SI], AX

endCreate:

        POP DI SI DX CX AX                         ;pops the registers that were used in the proc
        RET                                        ;ends proc
    createFile ENDP


;Proc to read information from a file
    readFile PROC
        PUSH AX BX CX DX SI DI                     ;pushes registers we will be using onto the stack
        MOV SI, 0
readLoop:
        
        MOV BX, fileHandle                         ;puts the filehandle for the file we're reading from into BX
        LEA DX, fileBuffer                         ;puts the address of the buffer we will be using to hold the information that is read
        MOV CX, 64                                 ;we will be reading 64 bytes
        MOV AH, 3Fh                                ;code to read the file with interrupt
        INT 21h                                    ;triggers the interrupt
        JC handleReadError                         ;if something went wrong prints out error message

        CMP AX, 64
        JE readLoop1
        MOV SI, 1
readLoop1:

        CALL formatText                            ;proc to format the text gotten from the inital file
        ;CALL checkInt
        CALL timer                                 ;proc to call the timer
        

        MOV BX, newFileHandle                      ;puts the handle of the new file we made into BX
        LEA DX, formattedBuffer                    ;puts the formatted buffer address into DX
        MOV CX, formattedLength                    ;we are going to read the full 64 bytes
        MOV AH, 40h                                ;interrupt for writing to a file
        INT 21h                                    ;^^^
        JC handleReadError                         ;if Error handle it
        CMP SI, 0
        JNE endloop
        JMP readLoop                               ;if no go again
endloop:      
        JMP endRead

;Prints out error information
handleReadError:
        ADD AX, '0'                                ;makes it so we can put the error code on screen
        LEA SI, cantRead                           ;puts address of error message into SI
        MOV DI, 160 * 12 + 70                      ;makes DI point to around the middle of the screen

        MOV AH, 00001111b                          ;sets the color of fore and background of the character
        ADD AH, 00010000b                          ;^^^ 
        MOV ES:[DI], AX                            ;puts character on screen
        ADD DI, 2                                  ;next position on screen

;prints Error Message
handleReadErrorLoop:
        MOV AL, [SI]                               ;Moves a character from the message into AL
        MOV AH, 00001111b                          ;sets the color of fore and background of the character
        ADD AH, 00000000b                          ;^^^ 
        MOV ES:[DI], AX                            ;puts character on screen
        ADD DI, 2                                  ;moves to next position on screen
        INC SI                                     ;moves to next character in message
        CMP SI, OFFSET afterRead                   ;checks if si is pointing past the message
        JE endRead                                 ;if no continue
        JMP handleReadErrorLoop                    ;^^^

endRead:

        POP DI SI DX CX BX AX 
        RET
    readFile ENDP

;PROC to close any open files
    closeFile PROC
        PUSH AX BX                                 ;pushes registers needed for proc

        MOV BX, fileHandle                         ;puts the filehandle of the file we read into BX
        MOV AH, 3Eh                                ;closes file
        INT 21h                                    ;^^^

        MOV BX, newFileHandle                      ;puts filehandle of output.dat into BX
        MOV AH, 3Eh                                ;closes file
        INT 21h                                    ;^^^

        POP BX AX
        RET
    closeFile ENDP
        

;proc to format the text that was taken from the initial file        
    formatText PROC
        PUSH AX BX CX DX SI DI                     ;register pushed that are needed

        MOV formattedBuffer, 0                     ;reset the formatted buffer

        LEA SI, fileBuffer                         ;get address of fileBuffer
        LEA DI, formattedBuffer                    ;get address of formattedBuffer
        
        MOV CX, 64                                 ;will go through loop at most 64 times
        LEA BX, lastCharacter                      ;was last character a space = 0, number = 1, letter = 2
        MOV DX, 0                                  ;counter to keep track of the length of the formatted buffer

;loop to format 
formatLoop:
        CMP CX, 0                                  ;is CX 0?
        JNE contLoop                               ;if not keep going
        JMP endFormatLoop                          ;else end loop

contLoop:
        MOV AL, [SI]                               ;move the character from the buffer into AL
        CMP AL, 26h                                ;is it the end of file
        JNE contLoop2                              ;if not continue
        JMP endFormatLoop                          ;else end loop

contLoop2:
        CMP AL, ' '                                ;is AL a space
        JLE checkSpace                             ;if yes go to checkSpace
        JMP handleInput                            ;else handle the input as appropriate

checkSpace:
        CMP BYTE PTR [BX], 0                       ;was the last character a space
        JE ignoreInput                             ;if yes ignore the input
        CMP BYTE PTR [BX], 0                       ;was the last character a letter?
        JG handleSpace                             ;if yes then handle the input as a space

ignoreInput:
        DEC CX                                     ;CX - 1
        MOV BYTE PTR [SI], ' '
        INC SI                                     ;next position of the buffer
        JMP formatLoop                             ;top of loop

;handles spaces
handleSpace:
        CMP BYTE PTR [BX], 1                       ;was last character a number
        JE handleNextLine                          ;if yes make this space a new line character
        MOV AL, ' '                                ;else make it a space
        MOV AH, 00001111b                          ;color
        ADD AH, 00000000b                          ;^^^
        MOV [DI], AX                               ;Put space into the formatted buffer
        MOV BYTE PTR [BX], 0                       ;set last character to be a space
        MOV BYTE PTR [SI], ' '
        DEC CX                                     ;CX - 1
        INC SI                                     ;next position in buffer
        INC DI                                     ;^^^
        INC DX                                     ;formattedBufferLength + 1
        JMP formatLoop                             ;top of loop

;makes a new line character
handleNextLine:
        MOV AL, 0Ah                                ;puts a new line character into AL
        MOV AH, 00001111b                          ;color
        ADD AH, 00000000b                          ;^^^
        MOV [DI], AX                               ;put character in formattedBuffer
        MOV BYTE PTR [BX], 0                       ;last character = space
        MOV BYTE PTR [SI], ' '
        DEC CX                                     ;CX - 1
        INC SI                                     ;next position in buffer
        INC DI                                     ;^^^
        INC DX                                     ;formattedBufferLength + 1
        JMP formatLoop                             ;top of loop
        
handleInput:
        CMP AL, 40h                                ;is AL a letter?
        JLE handleInt                              ;if not handle it as an integer
        MOV AH, 00001111b                          ;color
        ADD AH, 00000000b                          ;^^^
        MOV [DI], AX                               ;put character into formatted buffer
        MOV BYTE PTR [BX], 2                       ;last character was a letter
        MOV BYTE PTR [SI], ' '
        DEC CX                                     ;CX - 1
        INC SI                                     ;next position in buffer
        INC DI                                     ;^^^
        INC DX                                     ;formattedBufferLength + 1
        JMP formatLoop                             ;top of loop

handleInt:
        MOV AH, 00001111b                          ;color
        ADD AH, 00000000b                          ;^^^
        MOV [DI], AX                               ;Put character into formatted buffer
        MOV BYTE PTR [BX], 1                       ;last character was a number
        MOV BYTE PTR [SI], ' '
        DEC CX                                     ;CX - 1
        INC SI                                     ;next position in buffer
        INC DI                                     ;^^^
        INC DX                                     ;formattedBufferLength + 1
        JMP formatLoop                             ;top of loop

endFormatLoop:

        LEA BX, formattedLength                    ;puts address of formatted length into BX
        MOV [BX], DX                               ;puts length of fromatted buffer into length

        POP DI SI DX CX BX AX                      ;pops registers used
        RET     
    formatText ENDP


;proc to get rid of any names with a number lower than the unsigned int
    checkInt PROC
        PUSH AX BX CX DX SI DI                     ;pushes registers that have been used
                
        LEA BX, formattedBuffer                    ;puts address of formattedBuffer into BX
        LEA DI, checkedBuffer                      ;puts address of checkedBuffer into DI
        MOV DX, 0                                  ;int length counter
        MOV AX, 0                                  ;checkedBuffer length counter

;main loop that everyting comes back to that checks the number and determines whether the name will be added
checkLoop1:

        CMP BX, OFFSET afterBuffer                 ;checks if BX is pointing past formattedBuffer
        JNE contCheck                              ;if no continue
        JMP endCheck                               ;if yes then end proc

contCheck:
        MOV CX, 64                                 ;used for clearing fileBuffer

;clears fileBuffer to be used
clearBuffer:
        LEA SI, fileBuffer                        ;puts address of fileBuffer in SI
        MOV BYTE PTR [SI], ' '                    ;puts space into all positions
        LOOP clearBuffer                          ;loops until buffer is full

;keeps putting characters into fileBuffer
checkLoop2:
        LEA SI, fileBuffer                        ;puts address of fileBuffer into SI
        MOV CX, [BX]                              ;puts character from formatted buffer into CX
        MOV [SI], CX                              ;puts character into fileBuffer
        INC BX                                    ;goes to next position in formattedBuffer
        CMP BYTE PTR [SI], 0Ah                    ;checks if a nextline character was put into SI
        JE handleCheck                            ;if yes time to do the checking
        INC SI                                    ;if not next position and loop till we get there
        JMP checkLoop2                            ;^^^

;handles the check
handleCheck:
        DEC SI                                    ;look back one space
        CMP BYTE PTR [SI], ' '                    ;checks to see if we are looking at the space between the numbers and letter
        JE handleCheck2                           ;if yes next step
        INC DX                                    ;increase integer size by one
        JMP handleCheck                           ;loop until we find it

;next step is to see if length of unsigned int or length of int in file has more characters
handleCheck2:
        INC SI                                    ;go back to numbers
        CMP DX, uIntLength                        ;compare length
        JNE handleCheck3                          ;if not equal go to check which is longer
        JMP handleCheck4                          ;else start checking the specific numbers

;checks if file or uInt is longer
handleCheck3:
        CMP DX, uIntLength                        ;compares length of numbers
        JG addBuffer                              ;if file is longer then add it to the buffer
        JMP checkLoop1                            ;else back to top

;checks specifics and if uintlength is higher then ignores else it goes abck to top
handleCheck4:
        PUSH DI                                   ;push Di to make things easier
        LEA DI, uInteger                          ;puts address of uInteger into DI
        MOV CX, [DI]                              ;puts character into CX
        CMP [SI], CX                              ;compares numbers
        JG addBuffer                              ;if greater add to buffer
        CMP [SI], CX                              ;else check again
        JE handleCheck5                           ;if equal repeat
        POP DI                                    ;pop DI
        JMP checkLoop1                            ;else go back to beginning of loop

;same as 4 except it goes until it finds the solution
handleCheck5:
        INC SI                                    ;go to next position
        INC DI                                    ;^^^
        MOV CX, [DI]                              ;put number into CX
        CMP [SI], CX                              ;compare numbers
        JG addBuffer                              ;if greater add to buffer
        CMP [SI], CX                              ;compare again
        JE handleCheck5                           ;if equal repeat
        POP DI                                    ;pop DI
        JMP checkLoop1                            ;if not back to top

;add stuff to buffer
addBuffer:
        LEA SI, fileBuffer                        ;puts address into SI

;same as add Buffer just dont want to have fileBuffer being reset every time
addBuffer1:
        MOV CX, [SI]                              ;Move character into CX
        MOV [DI], CX                              ;Move character into DI
        INC DI                                    ;next position
        INC SI                                    ;next position
        INC AX                                    ;increase length of buffer by 1
        CMP BYTE PTR [DI], 0Ah                    ;check if next line character has been put in
        JNE addBuffer1                            ;if not keep going
        JMP checkLoop1                            ;else back to top

;ends the proc
endCheck:

        LEA SI, formattedLength                   ;puts the formatted length address into SI
        MOV [SI], AX                              ;puts new length of checkBuffer

        POP DI SI DX CX BX AX                     ;pop all registers used
        RET
    checkInt ENDP


;proc to keep a timer on screen
    timer PROC
        PUSH AX BX CX DX SI DI

        MOV AH, 00h                            ;gets current ticks
        INT 1Ah                                ;^^^

        SUB DX, startingTicks
        MOV AX, 55                             ;multiplies by 55 to get miliseconds
        MUL DX                                 ;^^^

        MOV SI, 160 + 76                       ;puts SI in the middle of the top row of the screen
        MOV BX, 10                             ;puts 10 into BX for dividing
        MOV CX, 2                              ;puts 2 into CX so we can get the 100th and 10th place

        MOV DX, 0                              ;does the 1000th place off screen
        DIV BX                                 ;^^^
        SUB SI, 2                              ;^^^

;prints out delay on top of screen
timerLoop1:
        MOV DX, 0                              ;clears out DX to be used for dividing
        DIV BX                                 ;divides AX by 10
        ADD DL, '0'                            ;adds '0' to get character for number
        MOV DH, 00001111b                      ;changes front color to white
        MOV ES:[SI], DL                        ;puts character on screen
        SUB SI, 2                              ;moves back one position
        LOOP timerLoop1                        ;loop twice

        MOV WORD PTR ES:[SI], 072Eh            ;puts white period on screen

timerLoop2:        
        SUB SI, 2                              ;moves back one more posiiton
        MOV DX, 0                              ;clears out DX
        DIV BX                                 ;divides
        ADD DL, '0'                            ;adds ACII
        MOV ES:[SI], DL                        ;puts character on screen
        CMP AX, 0
        JNE timerLoop2

        POP DI SI DX CX BX AX
        RET
    timer ENDP

myCode ENDS

END main