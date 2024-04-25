; sample prog 1
; J. George
;
;***********************************************************************************
MyStack SEGMENT STACK				;

	DW 256 DUP	(?)			;
	
MyStack ENDS					;
;***********************************************************************************
MyData SEGMENT				

MyData ENDS						;
;***********************************************************************************
MyCode SEGMENT					
	ASSUME CS:MyCode, DS: MyData		;
	
mainProg PROC					;
	PUSH AX BX CX SI

	MOV AX, 0B800h
	MOV ES, AX
	MOV BL, 0
	MOV CX, 26
	MOV SI, 160 * 18

	top:
		MOV AX, 'a'
		ADD AL, BL
		MOV AH, 00010010b
		MOV ES:[SI], AX

		INC SI
		INC SI

		INC BL 
		LOOP top
	
	POP SI CX BX AX
	
	MOV AH, 4Ch                                ;use a DOS interrupt
    INT 21h
    RET
	
mainProg ENDP					;
MyCode ENDS					;
;****************************************************************************

END mainProg					; 