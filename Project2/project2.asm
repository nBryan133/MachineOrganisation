;Project 2
;Nate Bryan
;clear screen then draw a box that can be played with
;**************************************************************
Mystack SEGMENT STACK

    DW 256 (?)

MyStack ENDS
;**************************************************************
MyData SEGMENT

    ;box perameters
    boxCorner DW 160 * 8 + 40                       ;holds the positon of the top left corner of the box
    border DB 1                                     ;determines whether the border is single or double lined
    frontColor DB 00001111b                         ;holds the color used for the lines of the box (bright red)
    backColor DB 00000000b                          ;holds the color used for the background of the box (blue)
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
    
MyData ENDS
;**************************************************************
MyCode SEGMENT
    assume ds: MyData, cs: MyCode
    
;main function of the program that calls procs that do the work
    main PROC                                      
            
        MOV AX, 0B800h                             ;put the screen memory address into the AX
        MOV ES, AX                                 ;put AX which contains the memory address into ES

        MOV AX, myData
        MOV DS, AX

        CALL clearScreen                           ;calls the proc that clears the screen

        CALL drawBox                               ;calls the drawBOX proc to draw the box on the screen

        CALL readKeys                              ;calls the readKeys proc to read any keyboard inputs

        MOV AH, 4Ch                                ;use a DOS interrupt
        INT 21h
        RET

    main ENDP 

;Proc used to clear the screen
    clearScreen PROC                               
        PUSH AX CX SI                              ;Pushing registers on stack
        
        MOV CX, 2000                               ;setting counter so that entire screen will be affected
        MOV SI, 0                                  ;set screen pointer to zero so we start at top left of screen
        MOV AX, 0720h                              ;moves an empty character into AX

    ;loop that clears the screen
cs1:                                   
        MOV ES:[SI],AX                             ;puts empty character on screen

        INC SI                                     ;incriments SI for next iteration of the loop
        INC SI                                     ;^^^

        LOOP cs1                                   ;goes back to top of loop as long as CX > 0

        POP SI CX AX                               ;pops registers out of stack
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

        ;debugging stuff
        ;MOV ES:[160*15 + 80], BYTE PTR 'N'        ; assume DH isn't 8
        ;CMP DI, 160 * 8 + 20
        ;JNE debug5                                ; assumption was correct
        ;MOV ES:[160*15 + 80], BYTE PTR 'Y'        ; else correct it
        ;debug5:

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

        MOV Cl, bwidth                             ;puts the height into the counter

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
        CMP AX, 3C00h                              ;is it F2
        JNE cont4                                  ;if yes process F2
        JMP processF2                              ;^^^

cont4:
        CMP AX, 3D00h                              ;is it F3
        JNE cont5                                  ;if yes process F3
        JMP processF3                              ;^^^

cont5:
        CMP AX, 4BE0h                              ;is it left arrow?
        JNE cont6                                  ;if yes process left arrow
        JMP processLeft                            ;^^^

cont6:
        CMP AX, 4DE0h                              ;is it right arrow?
        JNE cont7                                  ;if yes process right arrow
        JMP processRight                           ;^^^

cont7:
        CMP AX, 48E0h                              ;is it up arrow?
        JNE cont8                                  ;if yes process up arrow
        JMP processUp                              ;^^^

cont8:
        CMP AX, 50E0h                              ;is it down arrow?
        JNE cont9                                  ;if yes process down arrow
        JMP processDown                            ;^^^

cont9:
        CMP AX, 9B00h                              ;is it ctrl + left
        JNE cont10                                 ;if yes process ctrl + left
        JMP processCleft                           ;^^^

cont10:
        CMP AX, 9D00h                              ;is it ctrl + right
        JNE cont11                                 ;if yes process ctrl + right
        JMP processCright                          ;^^^
    
cont11:
        CMP AX, 9800h                              ;is it ctrl + UP
        JNE cont12                                 ;if yes process ctrl + Up
        JMP processCup                             ;^^^

cont12:
        CMP AX, 0A000h                              ;is ti ctrl + Down
        JNE cont13                                 ;if yes process ctrl + down
        JMP processCdown                           ;^^^

cont13:
        JMP keyloop                                ;if key is not one that does anything jump back to top of loop

    ;processes the input of F1 to change the border style
processF1:
        LEA BX, border                             ;puts the address of border into BX
        CMP BYTE PTR [BX], 0                       ;checks if border == 0
        JNE doubleSwitch                           ;if no then change border to single line
        INC BYTE PTR [BX]                          ;else changes border to double line from single line
        JMP looper                                 ;redraw box and go back to top of loop
    ;change the box border to single line
doubleSwitch:
        DEC BYTE PTR [BX]                          ;changes border to single line from double line 
        JMP looper                                 ;redraw box and go back to top of loop

    ;changes the front color used in the box
processF2:
        LEA BX, frontColor                         ;gets the address of frontcolor
        INC BYTE PTR [BX]                          ;adds one to frontcolor changing the color
        AND BYTE PTR [BX],00001111b                ;makes sure it only changes the foreground and does nothing to background
        JMP looper                                 ;redraw box and go back to top of loop

    ;changes background color of box
processF3:
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
        CMP WORD PTR [BX], 160                     ;is box already touching the top of the screen
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

    ;process ctrl + left (shrinks the width of the box)
processCleft:
        LEA SI, bwidth                             ;puts address of bwidth into SI
        CMP BYTE PTR [SI], 12                      ;is the width at the minimum
        JLE nextCleft                              ;if yes ignore input
        SUB BYTE PTR [SI], 1                       ;else decrease width by 1

nextCleft:
        JMP looper

    ;processes ctrl + right (increases width of box)
processCright:
        LEA BX, boxCorner                          ;puts address of boxCorner into BX
        LEA SI, bwidth                             ;puts address of bwidth into SI
        MOV DX, boxCorner                          ;puts the number associated with boxCorner to DX
        CMP DX, 160                                ;is DX greater than 160
        JGE numbersCright1                         ;if yes go into loop to make it less than or equal to 160
        JMP skipCright1                            ;if no skip loop

    ;makes sure DX is less than or equal to 160
numbersCright1:
        SUB DX, 160                                ;moves up one row
        CMP DX, 160                                ;is it in top row?
        JGE numbersCright1                         ;if not repeat

skipCright1:
        MOV Cl, bwidth                             ;adds the width into the counter

numbersCright2:
        ADD DX, 2                                  ;adds the width to DX
        LOOP numbersCright2

        CMP DX, 158                                ;checks if right side of box is touching edge of screen
        JL nextCright1                             ;if yes ignore input
        JMP keyloop                                ;^^^

nextCright1:
        ADD BYTE PTR [SI], 1                       ;increases width of the box
        JMP looper                                 ;clears screen and redraws box

    ;processes ctrl + up (increase height of box)
processCup:
        LEA BX, boxCorner
        LEA SI, height
        CMP WORD PTR [BX], 160                     ;is box already touching the top of the screen
        JGE nextCup1                               ;if yes ignore input
        JMP keyloop                                ;^^^

nextCup1:
        SUB WORD PTR [BX], 160                     ;moves top left of box up one row
        ADD BYTE PTR [SI], 1                       ;increases height by one
        JMP looper

    ;processes ctrl + down (reduce height of box)
processCdown:
        LEA BX, boxCorner                          ;puts the address of boxCorner in BX
        LEA SI, height                             ;puts the address of height into SI
        CMP BYTE PTR [SI], 6                       ;is height at min?
        JLE nextCdown                              ;if not reduce size
        ADD WORD PTR [BX], 160                     ;puts top of box one row lower 
        SUB BYTE PTR [SI], 1                       ;reduces height by 1
nextCdown:
        JMP looper                                 ;clears screen and redraws box

    ;incriment character on screen then goes back to top of loop
skip:
        INC BYTE PTR ES:[160 * 20 + 80]            ;incriments a random character on screen
        JMP keyloop                                ;jump to top of loop

    ;end of loop that redraws box then jumps back to top of loop
looper: 
        CALL clearScreen                           ;clears screen
        CALL drawBox                               ;draws box
        JMP keyloop                                ;jumps to top of loop

    ;end the loop and allows proc to end
endloop:

        POP DI DX CX BX AX                         ;pops registers from stack that were used in proc
        RET                                        ;return
    readKeys ENDP                                  ;end of readKeys PROC

MyCode ENDS                                        ;end of myCode Segment
END main                                           ;end of main

