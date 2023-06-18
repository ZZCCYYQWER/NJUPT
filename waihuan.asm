DATAS SEGMENT
BUF DB 30
    DB ?
    DB 30 DUP(?)    ;BUFFER AREA TO STORE THE MESSAGE
OLD0B DD ?   ;SUBSTITUTE
FLAG DB 0
DATAS ENDS


CODES SEGMENT
    ASSUME CS:CODES,DS:DATAS,SS:STACKS
START:
    MOV AX,DATAS
    MOV DS,AX
    CLI
    MOV AH,0AH
    MOV DX,OFFSET BUF   
    INT 21H           ;INPUT THE MESSAGE
    MOV AH,2
    MOV DL,0AH     
    INT 21H
    MOV CH,0
    MOV CL,BUF+1      ;TO CALCULATE THE LENGTH
    CALL I82501 


    CALL I8259
    CALL READ0B
    CALL WRITE0B
    STI
    MOV BX,OFFSET BUF+2   ;TO GET THE ADDRESS OF THE FIRST LETTER OF THE MESSAGE    
SCAN: 
    MOV DX,2FDH
    IN AL,DX
    TEST AL,20H        ;TO TEST IF THE REGISTER IS DEPLOIED
    JZ SCAN
    MOV DX,2F8H
    MOV AL,[BX]        ;TO SEND THE LETTER
    OUT DX,AL
    INC BX             ;TO TEST IF THE REGISTER GOT THE CORRECT LETTER
    
     PUSH CX
     MOV CX,0FFFH
     DELAY:
     LOOP DELAY
     POP CX
    
    ; MOV AH,2
    ; MOV DL,AL
    ; INT 21H
     ; IN AL,DX
     ; MOV AH,2
     ; MOV DL,AL
     ; INT 21H           ;TO TEST WHETHER THE REGISTER SENT THE CORRECT LETTER
LOOP SCAN
LAST:MOV DX,2FDH         ;TO MAKE SURE IT IS OVER
     IN AL,DX
     TEST AL,40H
     JZ LAST

    
    
SCANT: CMP FLAG,1           ;TO CHECK IF THE PROCESS IF OVER
       JZ SCANT
       CALL RESET
       MOV AH,4CH
       INT 21H
RECEIVE PROC
        PUSH AX
        PUSH DX
        PUSH DS            
        MOV AX,DATAS         ;TAKE BACK THE INITIAL ADDRESS      
        MOV DS,AX
        MOV DX,2F8H   
   
        IN AL,DX
     ;  AND AL,7FH          ;ABANDONED 
        CMP AL,03H           ;TO TEST IF IT IS OVER
        JZ NEXT             
        MOV AH,2             ;TO DISPLAY 
        MOV DL,AL
        INT 21H
        JMP EXIT
NEXT:   MOV FLAG,1
EXIT:   MOV AL,20H
        OUT 20H,AL
        POP DS               
        POP DX
        POP AX
        IRET
RECEIVE ENDP


I82501 PROC
     MOV DX,2FBH
     MOV AL,80H
     OUT DX,AL
     MOV DX,2F9H
     MOV AL,09H;
     OUT DX,AL
     MOV DX,2F8H
     MOV AL,00H;
     OUT DX,AL
     MOV DX,2FBH
     MOV AL,0BH;
     OUT DX,AL
     MOV DX,2F9H
     MOV AL,01H;
     OUT DX,AL
     MOV DX,2FCH
     MOV AL,00011000B;
     OUT DX,AL
     RET
I82501 ENDP

I8259 PROC
     IN AL,21H
     AND AL,11110111B;
     OUT 21H,AL
     RET
I8259 ENDP

READ0B PROC
      MOV AX,350BH
      INT 21H
      MOV WORD PTR OLD0B,BX
      MOV WORD PTR OLD0B+2,ES
      RET
READ0B ENDP

WRITE0B PROC
        PUSH DS
        MOV AX,CODES
        MOV DS,AX
        MOV DX,OFFSET RECEIVE
        MOV AX,250BH
        INT 21H
        POP DS
        RET
WRITE0B ENDP
RESET PROC
        IN AL,21H
        OR AL,00001000B
        OUT 21H,AL
        MOV AX,250BH
        MOV DX,WORD PTR OLD0B
        MOV DS,WORD PTR OLD0B+2
        INT 21H
        RET
RESET ENDP
CODES ENDS
END START






