;Project 1
;Nate Bryan
;flip screen and change color of letters
;**************************************************************
Mystack SEGMENT STACK

    DW 256 (?)

MyStack ENDS
;**************************************************************
MyData SEGMENT

MyData ENDS
;**************************************************************
MyCode SEGMENT
    assume ds: MyData, cs: MyCode
    
    main PROC                                      ;main function of the program
            
            MOV AX, 0B800h                         ;put the screen memory address into the AX
            MOV ES, AX                             ;put AX which contains the memory address into ES

            MOV DI, 0                              ;put the address of the first character into DI
            MOV SI, 158                            ;put the address of the last character into SI
            MOV CX, 25                             ;gives CX the ability to be used as a row counter

            looper:                                ;loops the flipper proc in order to flip all text on screen
                CALL flipper                       ;calls the flipper function to flip the current line
                ADD DI, 160                        ;tells DI to look at the next line
                ADD SI, 160                        ;tells Si to look at the next line
                LOOP looper                        ;tells the loop to loop again

            MOV CX, 25                             ;refreshes the loop counter
            MOV DI, 0                              ;refreshes the DI screen pointer
            colorlooper:                           ;loops the flipper proc in order to flip all text on screen
                CALL color                         ;calls the color function to flip the color of the current line
                ADD DI, 160                        ;tells DI to look at the next line
                LOOP colorlooper                   ;tells the loop to loop again
        
        MOV AH, 4Ch                                ;use a DOS interrupt
        INT 21h
        RET

    main ENDP 

    flipper PROC                                   ;function to flip the text on the screen
        PUSH AX BX DI SI CX                        ;pushes registers onto the stack
            MOV CX, 40
            top:                                   ;allows proc to loop for the full line
                MOV AX, ES:[DI]                    ;puts first character in row into AX
                MOV BX, ES:[SI]                    ;puts character at end of line at beginning of line
                
                MOV ES:[SI], AX                    ;puts character taken out of DI into SI                
                MOV ES:[DI], BX                    ;puts character taken out of SI into DI

                INC DI                             ;increments DI to the next position on screen
                INC DI                             ;^^^

                DEC SI                             ;increments SI to the next position on the screen
                DEC SI                             ;^^^

                LOOP top                           ;loops if CX > 0

        POP CX SI DI BX AX                         ;pop the registers that were used
        RET                       
    flipper ENDP
    

    color PROC                                     ;function to change the color of all alphabetical characters
        PUSH AX BX DI CX                           ;pushes registers onto the stack
            MOV CX, 80
            colorTop:                              ;top of loop that checks whether the character on screen is alphabetical
                CMP BYTE PTR ES:[DI], 'A'          ;checks if ACII code of letter is less than or than 'A'
                JL dontColor                       ;if not go to next character

                CMP BYTE PTR ES:[DI], 'Z'          ;is character greater than Z
                JLE doColor                        ;if so color character

                CMP BYTE PTR ES:[DI], 'a'          ;is character less than a
                JL dontColor                       ;if yes dont color

                CMP BYTE PTR ES:[DI], 'z'          ;is character less than z
                JLE doColor                        ;if so do color
                
                JMP dontColor                      ;else dont color


                doColor:
                    MOV AX, ES:[DI]                ;moves character on screen into AX
                    MOV AH, 01111001b              ;changes character to bright blue lettering on white background
                    MOV ES:[DI], AX                ;puts character back on screen with its color changes
                    
                dontColor:
                INC DI                             ;increments DI to the next position on screen
                INC DI                             ;^^^
                
                LOOP colorTop                      ;loops if CX > 0
        
        POP CX DI BX AX                            ;pops the registers that were used
        RET                                        ;return statement at the end
    color ENDP

MyCode ENDS
END main

