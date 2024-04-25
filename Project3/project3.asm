;Project 3
;Nate Bryan
;clear screen then draw a box that has a scrolling marquee in it
;**************************************************************
Mystack SEGMENT STACK

    DW 256 (?)

MyStack ENDS
;**************************************************************
MyData SEGMENT

    ;box perameters
    boxCorner DW 160 * 8 + 40                       ;holds the positon of the top left corner of the box
    border DB 1                                     ;determines whether the border is single or double lined
    frontColor DB 00001111b                         ;holds the color used for the lines of the box
    backColor DB 00000000b                          ;holds the color used for the background of the box
    height DB 8                                     ;holds the height of the box with width being this * 2
    bwidth DB 16

    ;box single border ascii codes 
    single DB 0DAh, 0BFh, 0C0h, 0D9h, 0B3h, 0C4h    ;array of single line ascii codes in the order ulCorner, urCorner, llCorner, lrCorner, virticle line, horizontal line 

    ;box double border ASCII codes 
    double DB 0C9h, 0BBh, 0C8h, 0BCh, 0BAh, 0CDh    ;array of double line ascii codes in the order ulCorner, urCorner, llCorner, lrCorner, virticle line, horizontal line 

    ;shortcuts for border codes
    ulCorner EQU 0                                  ;upper left corner
    urCorner EQU 1                                  ;upper right corner
    llCorner EQU 2                                  ;lower left corner
    lrCorner EQU 3                                  ;lower right corner
    virtLine EQU 4                                  ;virticle line
    horzLine EQU 5                                  ;horizontal line

    ;message to be printed in box
    msg DB "Peanut butter and jelly ends the elderly lady     " ;sentence to be printed in box
    afterMsg DB ?                                   ;variable to check for after message
    msgPos DW 0                                     ;variable to hold current position in message

    ;Delay variables
    scrollDelay DW 2
    shiftDelay EQU 4
    startingTicks DW 0
    startingScrollTicks DW 0

    ;pause variable
    paused DB 0

    
MyData ENDS
;**************************************************************
MyCode SEGMENT
    assume ds: MyData, cs: MyCode
    
;main function of the program that calls procs that do the work
    main PROC                                      
            
        MOV AX, 0B800h                             ;put the screen memory address into the AX
        MOV ES, AX                                 ;put AX which contains the memory address into ES

        MOV AX, myData                             ;allows us to access the data segment
        MOV DS, AX                                 ;^^^

        MOV AH, 00h                                ;inturrupt to get current ticks
        INT 1Ah                                    ;^^^

        LEA SI, startingTicks                      ;gets starting ticks for shift
        MOV [SI], DX                               ;^^^

        LEA SI, startingScrollTicks                ;gets starting ticks for scroller
        MOV [SI], DX                               ;^^^

        CALL clearScreen                           ;calls the proc that clears the screen

        CALL drawBox                               ;calls the drawBOX proc to draw the box on the screen

        CALL UpdateScrollDelay

        CALL readKeys                              ;calls the readKeys proc to read any keyboard inputs

        MOV AH, 4Ch                                ;use a DOS interrupt
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

;PROC to draw a box on the screen
    drawBox PROC                                   
        PUSH AX BX CX DX SI DI                     ;Pushes the registers onto the stack

        ;if statement to determine whether to draw single or double line box
        MOV AL, border                             ;moves the value of border which represents whether to do single(0) or double(1)
        CMP AL, 0                                  ;checks if single line
        JNE doubleLine                             ;if not jump to double line
        LEA SI, single                             ;tells SI to look at the address of single
        JMP endDrawIF                              ;jumps over double line
doubleLine:                                        ;jumped to if border != 0
        LEA SI, double                             ;tells SI to look at the address of double

endDrawIF:                                         ;end if statement

        MOV DI, boxCorner                          ;puts the position of the top left corner of the box into DI

        MOV AL, [SI + ulCorner]                    ;puts the symbol for the upper left corner into AL
        MOV AH, frontColor                         ;changes the foreground color of AX into purple
        ADD AH, backColor                          ;changes the background color of AX into white
        MOV ES:[DI], AX                            ;puts the character onto the screen at the position of the top left corner

        MOV CL, bwidth                             ;puts the width into the counter



;Loop that draws top line of box
drawb1:                                
        ADD DI, 2                                  ;goes to next spot on screen

        MOV AL, [SI + horzLine]                    ;puts horizontal into AL
        MOV AH, frontColor                         ;puts purple lines
        ADD AH, backColor                          ;puts grey background
        MOV ES:[DI], AX                            ;puts character onto the screen

        LOOP drawb1                                ;loops until across the screen where top right corner goes

        MOV AL, [SI + urCorner]                    ;Puts top right corner of box into AX
        MOV AH, frontColor                         ;puts purple lettering
        ADD AH, backColor                          ;puts grey as background
        MOV ES:[DI], AX                            ;puts character on screen

        MOV Cl, height                             ;puts the height into the counter

;Loop to draw the right side of the box
drawb2:                                 
        ADD DI, 160                                ;goes to next line
        MOV AL, [SI + virtLine]                    ;puts the ASCII code for the | character
        MOV AH, frontColor                         ;sets the color of the | to
        ADD AH, backColor                          ;sets the background color
        MOV ES:[DI], AX                            ;puts character on screen
        LOOP drawb2                                ;repeats until it makes it to the bottom right

        MOV DI, boxCorner                          ;resets Di to point at top right
            
        MOV Cl, height                             ;puts the height into the counter

;Loop to draw the left side of the box
drawb3:                               
        ADD DI, 160                                ;Goes to next line
        MOV AL, [SI + virtLine]                    ;Puts the | character into AL
        MOV AH, frontColor                         ;sets the character color
        ADD AH, backColor                          ;sets the background color 
        MOV ES:[DI], AX                            ;Puts character onto the screen
        LOOP drawb3                                ;loops until it makes it to the bottem left
            
        MOV AL, [SI + llCorner]                    ;puts the character for the lower left of the box
        MOV AH, frontColor                         ;changes the character color
        ADD AH, backColor                          ;changes the background color
        MOV ES:[DI], AX                            ;puts character onto the screen

        MOV Cl, bwidth                             ;puts the width into the counter

;Loop to draw the bottom of the box
drawb4:                               
        ADD DI, 2                                  ;goes to next character

        MOV AL, [SI + horzLine]                    ;Puts the horizontal line into AX
        MOV AH, frontColor                         ;changes character color
        ADD AH, backColor                          ;changes background color
        MOV ES:[DI], AX                            ;puts character onto the screen

        LOOP drawb4                                ;loops until its made it to the bottom right corner of the box

        MOV AL, [SI + lrCorner]                    ;puts the character for the bottom right corner into AX
        MOV AH, frontColor                         ;changes the color of the character
        ADD AH, backColor                          ;changes the color of the background
        MOV ES:[DI], AX                            ;puts the character onto the screen

        ;stuff needed to fill in the box
        MOV DI, boxCorner                          ;puts the position of the top left corner of the box in DI
        MOV BL, 1                                  ;initiates a counter BL to keep track of the outer loop
        MOV DL, bwidth                             ;makes a variable to hold width - 1
        SUB DL, 1                                  ;^^^
            
;loop to fill in the box with the proper colors
drawb6:
        ADD DI, 160                                ;Sets DI to point at the next line on the screen

        MOV CX, 0                                  ;initialize the counter for the inner loop drawb
        ADD CL, DL                                 ;ADDs width - 1 to the counter so we get accross the whole box

;Loop to draw the background blocks in the box
drawb5:
        ADD DI, 2                                  ;goes to next spot on screen
        MOV AX, ES:[DI]                            ;puts character on screen into AX
        MOV AH, frontColor                         ;puts background
        ADD AH, backColor                          ;puts foreground color
        MOV ES:[DI], AX                            ;puts character onto the screen
        LOOP drawb5                                ;go back to top of loop  
                                    
        ADD CL, DL                                 ;Resets counter so we can do it again to reset DI to the beginning of the line

;Loop to decrement DI until its back at the beginning of the line
drawb7:
        SUB DI, 2                                  ;goes to previous spot on screen

        LOOP drawb7                                ;jumps to top of loop

        INC BL                                     ;incrementing the outer loop counter
        CMP BL, height                             ;have we done all the rows
        JL drawb6                                  ;if not back to top of loop

        POP DI SI DX CX BX AX                      ;Pops the register values
        RET                                        ;returns the PROC
    drawBox ENDP                                   ;ENDS drawBox PROC


;Proc to read the keyboard inputs from a user
    readKeys PROC
        PUSH AX BX CX DX SI                        ;pushes registers needed for proc

    ;main loop that reads keys and then does different things depending on what key is used
keyloop:

        MOV AH, 12h                                ;checks for any extended keys
        INT 16h                                    ;^^^

        TEST AX, 11b                               ;is the shift key held down?
        JZ noShift                                 ;no then skip
        CALL handleShift                           ;else call handleShiftProc

noshift:
        MOV AH, 11h                                ;checks if any key is ready
        INT 16h                                    ;^^^
        JNZ cont1                                  ;if yes then continue
        JMP skip                                   ;rerun loop

cont1:
        MOV AH, 10h                                ;checks what the key in the buffer is
        INT 16h                                    ;^^^

        CMP AL, 27                                 ;is it ESC?
        JNE cont2                                  ;if yes end the keyloop
        JMP endloop                                ;^^^

cont2:

        CMP AX, 3B00h                              ;is it F1
        JNE cont3                                  ;if yes process F1
        JMP processF1                              ;^^^

cont3:
        LEA SI, paused
        CMP BYTE PTR [SI], 0
        JE notPaused
        JMP keyloop

notPaused:
        CMP AX, 4BE0h                              ;is it left arrow?
        JNE cont4                                  ;if yes process left arrow
        JMP processLeft                            ;^^^

cont4:
        CMP AX, 4DE0h                              ;is it right arrow?
        JNE cont5                                  ;if yes process right arrow
        JMP processRight                           ;^^^

cont5:
        CMP AX, 48E0h                              ;is it up arrow?
        JNE cont6                                  ;if yes process up arrow
        JMP processUp                              ;^^^

cont6:
        CMP AX, 50E0h                              ;is it down arrow?
        JNE cont7                                  ;if yes process down arrow
        JMP processDown                            ;^^^

cont7:
        CMP AX, 3062h                              ;is it b?
        JNE cont8                                  ;if yes process b    
        JMP processb                               ;^^^

cont8:
        CMP AX, 2166h                              ;is it f?
        JNE cont9                                  ;if yes process f
        JMP processf                               ;^^^

cont9:
        CMP AX, 9800h                              ;is it ctrl + up
        JNE cont10                                 ;if yes process
        JMP processCup                             ;^^^

cont10:
        CMP AX, 0A000h                             ;is it ctrl + down
        JNE cont11                                 ;if yes process
        JMP processCdown                           ;^^^

cont11:
        JMP keyloop                                ;if key is not one that does anything jump back to top of loop

    ;processes the input of F1 to change the border style
processF1:
        LEA BX, paused
        NEG BYTE PTR [BX]
        ADD BYTE PTR [BX], 1
        JMP looper                                 ;redraw box and go back to top of loop

    ;changes the front color used in the box
processf:
        LEA BX, frontColor                         ;gets the address of frontcolor
        INC BYTE PTR [BX]                          ;adds one to frontcolor changing the color
        AND BYTE PTR [BX],00001111b                ;makes sure it only changes the foreground and does nothing to background
        JMP looper                                 ;redraw box and go back to top of loop

    ;changes background color of box
processb:
        LEA BX, backColor                          ;gets the address of backColor
        ADD BYTE PTR [BX],00010000b                ;adds one to the backColor
        AND BYTE PTR [BX],11110000b                ;makes sure it only changes the background
        JMP looper                                 ;redraw box and go back to top of loop

    ;moves box left 1 space
processLeft:
        LEA BX, boxCorner                          ;gets address of top left corner of box
        MOV DX, boxCorner                          ;puts the value in boxCorner into DX
        CMP DX, 160                                ;is it on the first row
        JGE numbersLeft1                           ;if yes skip numbersLeft1 loop
        JMP skipLeft                               ;^^^

    ;loop to get DX to be equivalent to the first row of the screen
numbersLeft1:
        SUB DX, 160                                ;move DX up one row
        CMP DX, 160                                ;check if it is on top row
        JGE numbersLeft1                           ;if not repeat
skipLeft:
        CMP DX, 0                                  ;is the box touching left side of screen
        JG nextLeft1                               ;if yes ignore input
        JMP keyloop                                ;^^^
nextLeft1:
        SUB WORD PTR [BX], 2                       ;move box left one space
        JMP looper                                 ;redraw box and go back to top of loop

    ;moves box right one space
processRight:
        LEA BX, boxCorner                          ;puts address in BX 
        MOV DX, boxCorner                          ;puts the number associated with boxCorner to DX
        CMP DX, 160                                ;is DX greater than 160
        JGE numbersRight1                          ;if yes go into loop to make it less than or equal to 160
        JMP skipRight1                             ;if no skip loop
    ;makes sure DX is less than or equal to 160
numbersRight1:
        SUB DX, 160                                ;moves up one row
        CMP DX, 160                                ;is it in top row?
        JGE numbersRight1                          ;if not repeat

skipRight1:
        MOV Cl, bwidth                             ;adds the width into the counter

numbersRight2:
        ADD DX, 2                                  ;adds the width to DX
        LOOP numbersRight2

        CMP DX, 158                                ;checks if right side of box is touching edge of screen
        JL nextRight1                              ;if yes ignore input
        JMP keyloop                                ;^^^

nextRight1:
        ADD WORD PTR [BX], 2                       ;move box right one space
        JMP looper                                 ;redraw box and go back to top of loop

    ;moves box up one row
processUp:
        LEA BX, boxCorner                          ;gets address of top left corner of box
        CMP WORD PTR [BX], 320                     ;is box already touching the top of the screen
        JGE nextUp1                                ;if yes ignore input
        JMP keyloop                                ;^^^
nextUp1:
        SUB WORD PTR [BX], 160                     ;move box up one row
        JMP looper                                 ;redraw box and g back to top of loop

    ;moves down one row
processDown:
        LEA BX, boxCorner                          ;gets address of top left corner of box
        MOV CL, height                             ;moves height into the counter
        MOV DX, 3840                               ;moves the position of the begining of the last row into DX

numbersDown1:   
        SUB DX, 160                                ;goes up one row in DX
        LOOP numbersDown1   

        CMP WORD PTR [BX], DX                      ;checks if bottom of box is touching bottom of screen
        JL  nextDown1                              ;if yes ignore input
        JMP keyloop                                ;^^^

nextDown1:
        ADD WORD PTR [BX], 160                     ;move box down one row
        JMP looper                                 ;redraw box and go back to top of loop

;increases speed of text scrolling across screen
processCup:
        LEA SI, scrollDelay                        ;puts address of scroll delay into SI
        CMP WORD PTR [SI], 0                       ;compares scroll delay to 0
        JLE skipCup                                ;is it more than zero
        SUB WORD PTR [SI], 2                       ;if yes sub 2 ticks

skipCup:
        JMP looper

;reduces speed of text scrolling across screen down to once every 3 seconds
processCdown:
        LEA SI, scrollDelay                        ;puts address of scroll delay into SI
        CMP WORD PTR [SI], 54                      ;checks if delay is already at 3 seconds if yes
        JGE skipCdown                              ;skip input
        ADD WORD PTR [SI], 2                       ;^^^

skipCdown:
        JMP looper

;goes back to top of loop
skip:
        LEA SI, paused
        CMP BYTE PTR [SI], 0
        JNE paused1
        CALL updateScroll
paused1:
        JMP keyloop                                ;jump to top of loop

    ;end of loop that redraws box then jumps back to top of loop
looper: 
        CALL clearScreen                           ;clears screen
        CALL drawBox                               ;draws box
        CALL updateScroll
        CALL updateScrollDelay
        JMP keyloop                                ;jumps to top of loop

    ;end the loop and allows proc to end
endloop:

        POP DI DX CX BX AX                         ;pops registers from stack that were used in proc
        RET                                        ;return
    readKeys ENDP                                  ;end of readKeys PROC


;Proc to handle shift inputs
    handleShift PROC
        PUSH AX BX CX DX SI                        ;Pushes registers used in proc

        MOV AH, 00h                                ;Interupt to get number of current ticks
        INT 1Ah                                    ;^^^

        SUB DX, startingTicks                      ;subtracks starting tickls from current ticks
        CMP DX, shiftDelay                         ;checks if enough time has passed in order to let next shift input happen
        JG processShift                            ;if yes process the shift
        JMP skipShift                              ;else dont

;processes the shift input
processShift:

        MOV AH, 12h                                ;checks for any extended keys
        INT 16h                                    ;^^^

        TEST AX, 10b                               ;is it left shift?
        JNZ shrink                                 ;if not it must be right shift
        JMP growBox                                ;^^^

;shrinks box width
shrink:
        LEA SI, bwidth                             ;puts address of bwidth into SI
        CMP BYTE PTR [SI], 3                       ;is the width at the minimum
        JLE endShift                               ;if yes ignore input
        SUB BYTE PTR [SI], 1                       ;else decrease width by 1
        JMP endShift                               ;jumps to end of PROC

;grows box width
growBox:
        LEA BX, boxCorner                          ;puts address of boxCorner into BX
        LEA SI, bwidth                             ;puts address of bwidth into SI
        MOV DX, boxCorner                          ;puts the number associated with boxCorner to DX
        CMP DX, 160                                ;is DX greater than 160
        JGE boxGrow1                               ;if yes go into loop to make it less than or equal to 160
        JMP skipGrow1                              ;if no skip loop

    ;makes sure DX is less than or equal to 160
boxGrow1:
        SUB DX, 160                                ;moves up one row
        CMP DX, 160                                ;is it in top row?
        JGE boxGrow1                               ;if not repeat

skipGrow1:
        MOV Cl, bwidth                             ;adds the width into the counter

boxGrow2:
        ADD DX, 2                                  ;adds the width to DX
        LOOP boxGrow2

        CMP DX, 158                                ;checks if right side of box is touching edge of screen
        JL nextGbox1                               ;if yes ignore input
        JMP endShift                               ;jumps to end of shift proc

nextGbox1:
        CMP BYTE PTR [SI], 40                      ;is the box at max width?
        JGE endShift                               ;if yes skip
        ADD BYTE PTR [SI], 1                       ;if no increases width of the box      

endShift:
        CALL clearScreen                           ;clears screen
        CALL drawBox                               ;redraws screen
        CALL updateScroll                          ;updates the words scrolling in box
        CALL updateScrollDelay

        MOV AH, 00h                                ;interrupt to get new start tick
        INT 1Ah                                    ;^^^

        LEA SI, startingTicks                      ;puts new starting tick amount into startingTicks variable
        MOV [SI], DX                               ;^^^

skipShift:
        POP SI DX CX BX AX                         ;pops registers used in proc
        RET                                        ;return
    handleShift ENDP                               ;ends PROC

;Proc to print scroll delay at top of screen
    updateScrollDelay PROC 
        PUSH AX BX CX DX SI

            MOV BX, scrollDelay                    ;puts scrolldelay in BX
            MOV AX, 55                             ;multiplies by 55 to get miliseconds
            MUL BX                                 ;^^^

            MOV SI, 76                             ;puts SI in the middle of the top row of the screen
            MOV BX, 10                             ;puts 10 into BX for dividing
            MOV CX, 2                              ;puts 2 into CX so we can get the 100th and 10th place

            MOV DX, 0                              ;does the 1000th place off screen
            DIV BX                                 ;^^^
            SUB SI, 2                              ;^^^

;prints out delay on top of screen
printDelay:
            MOV DX, 0                              ;clears out DX to be used for dividing
            DIV BX                                 ;divides AX by 10
            ADD DL, '0'                            ;adds '0' to get character for number
            MOV DH, frontColor                     ;changes front color to white
            MOV ES:[SI], DL                        ;puts character on screen
            SUB SI, 2                              ;moves back one position
            LOOP printDelay                        ;loop twice

            MOV WORD PTR ES:[SI], 072Eh            ;puts white period on screen
            SUB SI, 2                              ;moves back one more posiiton
            MOV DX, 0                              ;clears out DX
            DIV BX                                 ;divides
            ADD DL, '0'                            ;adds ACII
            MOV ES:[SI], DL                        ;puts character on screen

        POP SI DX CX BX AX
        RET
    updateScrollDelay ENDP


;proc to handle the scrolling of the sentence inside the box
    updateScroll PROC
        PUSH AX BX CX DX SI DI                     ;Pushes all the registers used in the proc

        MOV DI, boxCorner                          ;gets the position of the top left corner of the box
        ADD DI, 2                                  ;moves it over one space so that it will be off the left border

        MOV CX, 4                                  ;puts half the hight of the box into CX

;loop to put the position in DI to be the middle row of the box
positionLoop:
        ADD DI, 160                                ;goes to next row
        LOOP positionLoop                          ;loops until its in the middle row

        LEA SI, msg                                ;puts the address of the first character of the message into SI
        ADD SI, msgPos                             ;puts the position of the first character in the box into DL
        
;skips to here if starting at first character
atStart:
        MOV CL, bwidth                             ;puts the box width into the counter
        SUB CL, 1                                  ;subtracts 2 so that the barrier isnt affected

;loop to print message (currently struggling to get text wrapping functioning)
printMessage:
        MOV AL, [SI]                               ;Moves a character from the message into AL
        MOV AH, frontColor                         ;sets the color of fore and background of the character
        ADD AH, backColor                          ;^^^ 
        MOV ES:[DI], AX                            ;puts character on screen

        ADD DI, 2                                  ;moves to next position on screen
        INC SI                                     ;moves to next character in message
        CMP SI, OFFSET afterMsg                    ;checks if si is pointing past the message
        JNE continue                               ;if no continue
        LEA SI, msg                                ;if yes set to beginning of message

continue:
        SUB CL, 1                                  ;subtrack from counter
        CMP CL, 0                                  ;check if counter is done
        JG printMessage                            ;if not repeat for all space in the box

        MOV AH, 00h                                ;interrupt to get current ticks
        INT 1Ah                                    ;^^^

        SUB DX, startingScrollTicks                ;subtracts starting ticks from current ticks
        CMP DX, scrollDelay                        ;checks if enought time has passed for next scroll
        JG processScroll                           ;if yes process scroll
        JMP skipScroll                             ;if not skip

;processes the things needed to scroll the sentence accross the screen
processScroll:
        INC WORD PTR [msgPos]                      ;increments msgPos to start at next character
        MOV DX, [msgPos]                           ;moves msgPos into DL to check if its at the last character
        CMP DX, 50                                 ;is it at the last character?
        JL dontReset                               ;if yes reset msg pointer and tell it to point at the first character
        MOV WORD PTR [msgPos], 0                   ;^^^

;skips setting msgPos to 0 if its not at the lasst position yet
dontReset:
        MOV AH, 00h                                ;gets current ticks
        INT 1Ah                                    ;^^^

        LEA SI, startingScrollTicks                ;puts current ticks into starting ticks for scroller
        MOV [SI], DX                               ;^^^

;skips scrolling if not enough time has passed since last scroll
skipScroll:
        POP DI SI DX CX BX AX                      ;pops all used registers off of stack
        RET                                        ;returns values
    updateScroll ENDP                              ;ends the updateScroll Proc


MyCode ENDS                                        ;end of myCode Segment
END main                                           ;end of main

