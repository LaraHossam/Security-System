
; You may customize this and other start-up templates; 
; The location of this template is c:\emu8086\inc\0_com_template.txt



; In the bank, a security lock is used to access some rooms. This lock accepts two inputs: the 
; employee identification number (16 bits) and his/her password (4 bits). If the bank has 20 
; employees, construct their database and store it in the memory. Then write a program to 
; access these rooms.
; The inputs of the program are the employee identification and the password.
; The output is one bit (0/1) that means (denied/allowed). 

  
  

ORG 100h    

; DATA SEGMENT

    .DATA
NEWLINE                 DB      0DH, 0AH, '$'       ; USED TO PRINT A NEW LINE
INPUT_ID                DB      5,?,5 DUP (?)       ; STORE A BYTE WITH 5, THEN AN UNINTIALIZED ARRAY OF 5 VALUES. DUP IS A DIRECTIVE TO DECLARE AN ARRAY OF DATA, DECLARE 5 UNINITIALIZED BYTES STARTING AT LOC INPUT_ID
INPUT_PASSWORD          DB      2,?,2 DUP (?)       ; STORE A BYTE WITH 2, THEN AN UNINTIALIZED ARRAY OF 5 VALUES. DUP IS A DIRECTIVE TO DECLARE AN ARRAY OF DATA, DECLARE 2 UNINITIALIZED BYTES STARTING AT LOC INPUT_ID
WELCOME                 DB      'PROJECT #3 SECURITY LOCK FOR A BANK','$'  
CREDENTIALS             DB      'NAME: LARA HOSSAM   ID: 6853','$'
ID_PROMPT               DB      'ENTER EMPLOYEE IDENTIFICATION NUMBER:','$'
PASSWORD_PROMPT         DB      'ENTER EMPLOYEE PASSWORD:','$'
HEXA_DIGITS             DB      '0123456789ABCDEFabcdef'
ERROR_WRONG_ENTRY       DB      'ERROR: ENTER VALID HEX DIGITS: 0 --> 9, A --> F, a --> f','$'                             ; 
ERROR_WRONG_NO          DB      'ERROR: THE ID NUMBER MUST BE 4-BIT HEX','$' 
INVALID_ID              DB      'THIS ID DOES NOT EXIST. TRY AGAIN.','$'
IDS                     DW      01FDCH,1222H,1AAAH,5678H,2EDCH,0FADCH,5436H,2502H,0DEDCH,0EEEEH,5567H,8888H,5436H,12F3H,8933H,6CFAH,3BEBH,3FACH,6789H,0AEFEH  
PASSWORDS               DB      51H,0F4H,0CFH,6DH,95H,0ADH,0BCH,0A1H,47H,25H
LINE                    DB      '---------------------------------------------------------------','$'
ACCESS_GRANTED_PROMPT   DB      'ACCESS GRANTED. (1) ','$'
ACCESS_DENIED_PROMPT    DB      'ACCESS DENIED (0) ','$'

; END OF DATA SEGMENT





; CODE SEGMENT 

    .CODE
MAIN                PROC   
                    MOV  AX,@DATA                       ; MOVE DATA TO DATA SEGMENT
                    MOV  DS,AX                          ; MOVE AX TO DS, INITIALIZATION OF DS                                                              
                    MOV  ES,AX                          ; Make DS and ES OVERLAPPED BECAUSE ALL OUR STRINGS ARE IN THE DATA SEGMENT, WE WANT TO ACCESS THEM USING DI AND SI
                    CALL WELCOME_MSG                    ; PRINT WELCOME MESSAGE 
START:              CALL GET_ID                         ; GET ID [4 HEXA DIGITS]
                    CALL CHECK_NO_DIGITS                ; CHECK IF USER INPUTED 4 DIGITS        
                    CALL CHECK_ENTRY_ID                 ; CHECK IF ALL DIGITS ARE HEXA
                    MOV  SI, OFFSET INPUT_ID + 2        ; INITIALIZE SI TO POINT TO THE ID INPUTED BY USER IN MEMORY
                    CALL PUT_IN_AX                      ; PUT ID INPUT IN AX
                    CALL CHECK_ID                       ; COMPARE INPUT TO DATABASE OF IDS               
                    CALL GET_PASS                       ; IF ID EXISTS, GET PASSOWRD FROM USER
                    MOV  SI, OFFSET INPUT_PASSWORD + 2  ; INITIALIZE SI TO POINT TO THE PASSWORDS INPUTED BY USER IN MEMORY
                    CALL PUT_PASS_IN_AX                 ; PUT PASSWORD IN AX
                    CALL VALIDATE_CREDENTIALS           ; CHECK IF ID AND PASSOWRD MATCH
                    CALL ACCESS_GRANTED                 ; ACCESS GRANTED IF ALL SUBROUTINES ABOVE ARE EXECUTED
WRONGNODIGITS:      CALL WRONG_NO_DIGITS                ; IF INPUT DOES NOT CONTAIN 4 HEX DIGITS
WRONGENTRY:         CALL WRONG_ENTRY                    ; IF INPUT CONTAINS DIGITS IN THE WRONG RANGE
WRONGID:            CALL WRONG_ID                       ; IF ID DOES NOT EXIST IN THE DATABASE
WRONGPASSWORD:      CALL ACCESS_DENIED                  ; ACCESS DENIED IF ID AND PASSOWRD DON'T MATCH            
                    ENDP                                               

;--------------------------------------------------------------------


; FUNCTIONS USED IN MAIN
       
WELCOME_MSG         PROC   
                    MOV AH,09H              ; DISPLAY A STRING OF CHARACTERS WHOSE OFFSET IS
                                            ; SPECIFIED BY DX, UNTIL A '$' IS FOUND
                    MOV DX,OFFSET WELCOME   ; OFFSET TO SPECIFY THE START OF THE STRING TO BE PRINTED
                    INT 21H                 ; SOFTWARE INTERRUPT 0X21 (33) CAUSING TO DISPLAY CHARACTERS ON THE SCREEN 
                    
                    CALL NEW_LINE 
                    
                    MOV AH,09H         
                    MOV DX,OFFSET CREDENTIALS
                    INT 21H
                    
                    CALL NEW_LINE
                    CALL DASH_LINE
                    CALL NEW_LINE 
                    
                    RET
                    ENDP



NEW_LINE            PROC                    ; SUBROUTINE TO PRINT A NEW LINE WHENEVER IT'S NEEDED 
                    MOV AH,09H         
                    MOV DX,OFFSET NEWLINE
                    INT 21H
                    RET
                    ENDP
                
                
                
DASH_LINE           PROC                    ; SUBROUTINE TO PRINT A DASHED LINE WHENEVER IT'S NEEDED
                    MOV AH,09H         
                    MOV DX,OFFSET LINE
                    INT 21H
                    RET
                    ENDP
                
                
                
GET_ID              PROC 
                    MOV AH,09H              ; DISPLAY MESSAGE ON SCREEN TO PROMPT USER FOR INPUT        
                    MOV DX,OFFSET ID_PROMPT
                    INT 21H   
                    
                    CALL NEW_LINE
                         
                    MOV AH,0AH              ; INT 21H/ AH = 0AH - INPUT OF A STRING TO DS:DX, FIRST BYTE IS BUFFER SIZE
                                            ; SECOND BYTE IS NUMBER OF CHARS READ. IT DOES NOT ADD $ AT THE END.
                    MOV DX,OFFSET INPUT_ID  ; RETURNS BUFFER FILLED WITH USER INPUT - READS FROM STANDARD INPUT 
                                            ; AFTER THE INTERRUPT, BYTE [INPUT_ID + 1] WILL CONTAIN THE NUMBER OF CHARACTERS READ AND THE CHARACTERS THEMSELVES START AT [INPUT_ID +2]
                    INT 21H           
    
                    CALL NEW_LINE
                    RET
                    ENDP                


        
        
CHECK_NO_DIGITS     PROC
                    LEA SI,INPUT_ID+1        ; START READING THE NUMBER OF CHARACTERS READ FROM THE ARRAY INPUT_ID
                    CMP [SI],04H             ; COMPARE THE NUMBER OF CHARACTERS TO 4, IF IT'S EQUAL, CONTINUE EXECUTION
                    JNZ WRONG_NO_DIGITS      ; IF NOT EQUAL 4, THEN IT'S THE WRONG NUMBER OF DIGITS.
                    RET
                    ENDP 




WRONG_NO_DIGITS     PROC
                    MOV AH,09H
                    MOV DX,OFFSET ERROR_WRONG_NO
                    INT 21H        
                    
                    CALL NEW_LINE 
                    CALL DASH_LINE
                    CALL NEW_LINE 
                    
                    JMP START               ; RESTART ID INPUT AGAIN AS THE INPUT WASN'T 4 HEXA DIGITS
                    RET
                    ENDP


 
CHECK_ENTRY_ID      PROC                    ; CHECK IF ALL THE DIGITS ARE VALID HEXA DIGITS
                    MOV AH,4                ; 4 HEXA DIGITS [OUTER COUNTER]
                    LEA SI,INPUT_ID+2       ; LOAD THE ADDRESS OF THE FIRST INPUT BY USER
AGAIN:              LEA DI,HEXA_DIGITS      ; LOAD THE ARRAY CONTAINING ALL THE VALID HEXA DIGITS IN MEMORY
                    MOV CX,22               ; CX IS THE LOOP COUNTER, CHECK EVERY DIGIT IF IT'S ONE OF THE 22 VALID DIGITS 
                    MOV AL,[SI]             ; MOVE THE FIRST DIGIT INTO AL AND REPEATEDLY SCAN IN DI IF IT'S THERE
                    REPNZ SCASB
                    CMP CX,00               ; IF ONE DIGIT IS NOT FOUND IN DI AFTER ALL THE ITERATIONS, GO TO END.
                    JZ  END
                    INC SI                  ; ELSE INCREMENT SI TO POINT TO THE NEXT DIGIT
                    DEC AH                  ; DECREMENT AH SO THAT THE OUTER LOOP DECREASES BY 1
                    JNZ AGAIN               ; IF AH IS NOT 0, AND SI IS NOT DONE, REPEAT.
                    RET
END:                JMP WRONGENTRY       
                    ENDP
                   
                   
                   
                   
                  
                   
WRONG_ENTRY         PROC
                    MOV  AH,09H
                    MOV  DX,OFFSET ERROR_WRONG_ENTRY 
                                              ; ID DIGITS ARE NOT HEXA
                    INT  21H 
                    CALL NEW_LINE
                    CALL DASH_LINE
                    CALL NEW_LINE
                    JMP  START                ; RESTART ID INPUT AGAIN AS THE INPUT WASN'T VALID HEXA DIGITS
                    RET
                    ENDP

   

PUT_IN_AX           PROC                      ; SAVE THE USER INPUT IN INPUT_ID ARRAY IN MEMORY IN AX SO IT CAN BE CHECKED
                    MOV CX,04H                ; AS WE HAVE 4 HEXA DIGITS, THE COUNTER WILL LOOP 4 TIMES
LOOP_DIGITS:        CMP [SI],39H              ; COMPARE THE USER INPUT TO 39H AS IT IS THE ASCII VALUE OF NUMBER 9
                    JZ  NUMBER                ; IF IT'S EQUAL TO 39H OR BELOW THEN IT'S A NUMBER RANGING FROM 0 TO 9
                    JB  NUMBER         
                    JA  LETTER                ; IF IT'S ABOVE 39H THEN IT'S A LETTER
NUMBER:             SUB [SI],30H              ; TO GET THE REAL VALUE OF THE INPUT, SUBTRACT 30H TO GET THE HEX DIGIT FROM THE ASCII CODE
                    JMP CONTINUE                 
LETTER:             CMP [SI],70               ; THE ASCII VALUE FOR CAPITAL F IS 70 DECIMAL, IF IT'S EQUAL TO 70 OR BELOW THEN IT'S CAPITAL  
                    JZ  CAPITAL
                    JB  CAPITAL
                    JA  SMALL                 ; THE DECIMAL ASCII CODE FOR SMALL a IS 97, SO IT'S DEFINITELY SMALL IF IT'S ABOVE 70
CAPITAL:            SUB [SI],55               ; THE ASCII CODE OF A IS 65, IT'S DECIMAL REPRESENTATION IS 10, SO TO GET THE 10 WE HAVE TO SUBTRACT 55
                    JMP CONTINUE 
SMALL:              SUB [SI],87               ; THE ASCII CODE OF a IS 97, SO TO GET THE REPRESENTATION OF 10 WE SUBTRACT 87
                    JMP CONTINUE       
CONTINUE:           INC SI                    ; GO TP THE NEXT DIGIT
                    DEC CX                    ; DECREMENT LOOP COUNTER
                    JNZ LOOP_DIGITS           ; REPEAT
                
                    SUB SI,4                  ; GO TO THE FIRST 2  DIGITS AGAIN
                    MOV AH,[SI]               ; STORE THE FIRST DIGIT IN AH
                    MOV AL,[SI+2]             ; STORE THE THIRD DIGIT IN AL
                    MOV BH,[SI+1]             ; STORE THE SECOND DIGIT IN BH
                    MOV BL,[SI+3]             ; STORE THE THIRD DIGIT IN BL
                    SHL AX,4
                    OR  AX,BX  
                    RET
                    ENDP                      ; VISUALIZATION OF STORING IN AX
                                              ; IF YOU WANT TO STORE  1234H, AH = 01 H  AL = 03 H
                                              ; SHIFT LEFT 4 TIMES SO        AH = 10 H  AL = 03 H
                                              ;                              BH = 02 H  BL = 04 H
                                              ; ORING THEM TOGETHER YILED IN AX = 1234H     


CHECK_ID            PROC
                    MOV CX,21                 ; LOOPING IN THE ARRAY OF 20 ENTRIES
                    LEA DI,IDS                ; DI IS POINTING TO THE START OF THE ARRAY OF IDS
                    CLD                       ; CLEAR DIRECTION FLAG TO AUTO INCREMENT
                    REPNE SCASW               ; CHECK IF THE ID IN AX EXISTS OR NOT
                    CMP CX,0000H              ; IF THE ID DOES NOT EXIST AND THE LOOP IS DONE
                    JZ WRONGID                ; JUMP TO WRONG ID
                    RET        
                    ENDP
                    
                    
                    
WRONG_ID            PROC 
                    MOV AH,09H
                    MOV DX,OFFSET INVALID_ID  ; ID IS NOT IN DATABASE
                    INT 21H     
                    CALL NEW_LINE
                    CALL DASH_LINE
                    CALL NEW_LINE
                    JMP START                 ; RESTART AGAIN
                    RET
                    ENDP



GET_PASS            PROC
                    MOV AH,09H
                    MOV DX,OFFSET PASSWORD_PROMPT 
                                              ; DISPLAY MESSAGE ON SCREEN TO PROMPT USER FOR INPUT
                    INT 21H   
                    CALL NEW_LINE
                    
                    MOV AH,0AH
                    MOV DX,OFFSET INPUT_PASSWORD
                    INT 21H   
                                              ; INT 21H/ AH = 0AH - INPUT OF A STRING TO DS:DX, FIRST BYTE IS BUFFER SIZE
                                              ; SECOND BYTE IS NUMBER OF CHARS READ. IT DOES NOT ADD $ AT THE END.
                                              ; RETURNS BUFFER FILLED WITH USER INPUT - READS FROM STANDARD INPUT 
                                             
                    RET
                    ENDP


PUT_PASS_IN_AX      PROC                     ; SAME FUNCTION AS PUT_ID_IN_AX BUT THE DIFFERENCE IS THAT WE NEED NO LOOP, IT'S JUST ONE HEX DIGIT.
                    CMP [SI],39H            
                    JZ  NUM
                    JB  NUM         
                    JA  LET    
NUM :               SUB [SI],30H
                    JMP CONT         
LET:                CMP [SI],70
                    JZ  CAPITAL1
                    JB  CAPITAL1
                    JA  SMALL1
CAPITAL1:           SUB [SI],55
                    JMP CONT 
SMALL1:             SUB [SI],87
                    JMP CONT       
CONT:               XOR AX,AX               ; MAKE AX ALL ZEROS
                    MOV AX,[SI]             ; STORE THE BYTE IN AX [AL]
                    MOV AH, 00H             ; MAKE AH = 00H SO AX = BYTE INPUT 
                    RET    
                    ENDP
                    
                    
                    
VALIDATE_CREDENTIALS PROC                    
                    LEA BX, IDS             ; LOAD THE EFFECTIVE ADDRESS OF IDS IN BX IN ORDER TO COMPUTE THE BASE ADDRESS 
                    SUB DI, BX              ; GET THE OFFSET TO KNOW WHICH ELEMENT ARE WE TACKLING
                    SHR DI, 1               ; DIVIDE BY 2 [WE START FROM ELEMENT 1]
                    MOV CX, DI              ; SAVE THE OFFSET OF THE ELEMENT
                    ADD DI, 1               ; ADD ONE TO MAKE SURE THAT WE'RE ACCESSING THE  RIGHT BYTE AS WE CAN'T ACCESS 4 BITS FROM MEMORY AT A TIME
                    SHR DI, 1               ; DIVIDE BY 2 TO ENSURE THAT WE'RE CHOOSING THE RIGHT OFFSET IN THE PASSWORD ARRAY
                    AND CX, 1               ; CHECK EVEN OR ODD OFFSET
                    JZ EVEN                                        
ODD:                XOR BX, BX              ; CLEAR BX 
                    MOV BL, PASSWORDS[DI-1] ; JUST FOR TRACING AND TESTINGSHR BL, 4               ; IF THE OFFSET IS ODD
                    SHR BL,4
                    CMP BL,AL               ; CHECK IF THE TWO PASSWORDS ARE CORRECT OR NOT  BY SHIFTING THE BYTE BY 4 TO THE RIGHT, ALL WE CARE ABOUT IS THE HIGHER 4 BITS
                    JNZ ACCESS_DENIED 
                    RET
                    
EVEN:               XOR BX, BX              ; CLEAR BX 
                    MOV BL, PASSWORDS[DI-1] ; JUST FOR TRACING AND TESTINGSHR BL, 4               ; IF THE OFFSET IS EVEN
                    AND BL, 0FH
                    CMP BL,AL               ; CHECK IF THE TWO PASSWORDS ARE CORRECT OR NOT  BY ANDING THE BYTE FROM MEMORY WITH 0F AS ALL WE CARE ABOUT IS THE LOWER 4 BITS
                    JNZ ACCESS_DENIED
                    RET
                    ENDP    
                       
ACCESS_GRANTED      PROC
                    CALL NEW_LINE
                    MOV AH,09H
                    MOV DX,OFFSET ACCESS_GRANTED_PROMPT   ; SUBROUTINE TO PRINT AN ACCEPTANCE MESSAGE IF ACCESS IS GRANTED 
                    INT 21H 
                    CALL NEW_LINE 
                    CALL DASH_LINE
                    CALL NEW_LINE
                    JMP START
                    RET
                    ENDP
                      
        
        
        
ACCESS_DENIED       PROC
                    CALL NEW_LINE
                    MOV AH,09H
                    MOV DX,OFFSET ACCESS_DENIED_PROMPT  ; SUBROUTINE TO PRINT A DECLINE MESSAGE IF ACCESS IS DENIED
                    INT 21H 
                    CALL NEW_LINE 
                    CALL DASH_LINE
                    CALL NEW_LINE 
                    JMP START
                    RET
                    ENDP
                
                    END                                 ; DIRECTIVE TO STOP THE COMPILER


; END OF CODE SEGMENT          