.586
DATA SEGMENT USE16
MESG DB 128
	 DB ?
	 DB 128 DUP(?)
LENS EQU $-MESG
OLD0B DD ?
A1 DB "   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~$"
A2 DB "   *** * *  *   Transfer   *  * * ****$"
A3 DB "   designer:B20030726 zcy $"
A4 DB "   Please input the word:$"
A5 DB "   THE output:$"
A6 DB "   -----------------------------------$"
A7 DB "   $"
A8 DB "   ENTER BAUD(like 01 01 and <=01 80 >=00 0C):$";长度45
A9 DB "   ENTER FRAME FORMAT(<=3F ):$";长度27
;波特率报错
WRONGB DB "   !!! Wrong!ENTER BAUD(like 01 01 and <=01 80 >=00 0C) !!!$"
;帧格式报错
WRONGF DB "   !!! Wrong!PlEASE ENTER FRAME FORMAT(<=3F ) !!!$"
LINE DB 1
STRING DB 6,0,6 DUP(0);6位BUF,用来存放波特率高位和低位(两个16进制数和一个空格)
STRINGF DB 3,0,3 DUP(0);3位，用来存放帧格式
DATA1 DB 2 DUP(0);2位data用于存放转化后的波特率
DATAF DB 1 DUP(0);1位dataf用于存放转化后的帧格式
DATA ENDS


CODE SEGMENT USE16
ASSUME CS:CODE,DS:DATA 
BEG:   
;界面样式
    ;设置外窗口属性
    MOV   AH,7				;功能：屏幕初始化
    MOV   AL,0				;page
    MOV   BH,70H			;白底黑字
    MOV   CH,1				;row左上
    MOV   CL,2				;column左上
    MOV   DH,23				;row右下
    MOV   DL,77				;column右下
    INT   10H				;BIOS显示操作中断
    
    ;设置内窗口属性
    MOV   AH,7				;功能：屏幕初始化
    MOV   AL,0				;page
    MOV   BH,00111111B		;浅青色底白字
    MOV   CH,2				;row左上
    MOV   CL,4				;column左上
    MOV   DH,22				;row右下
    MOV   DL,75				;column右下
    INT   10H				;BIOS显示操作中断
        ;设置光标类型(大小)
    MOV   CH,12				;光标起始行
    MOV   CL,13				;光标结束行
    MOV   AH,1				;设置光标类型
    INT   10H				;DOS中断：显示操作
        ;初始化光标位置
    MOV   DH,2				;屏幕显示行
    MOV   DL,4				;屏幕显示列
    MOV   BH,0				;显示页号
    MOV   AH,2				;置光标位置
    INT   10H				;BIOS显示操作中断
		MOV AX,DATA
		MOV DS,AX
		;MOV AH,0           ;设置显示方式
		;MOV AL,3           ;80*25彩色文本方式
		INT 10H
		;MOV AH,6
		;MOV AL,0
		;MOV CH,0
		;MOV CL,0 
		;MOV DH,80
	    ;MOV DL,80
		;MOV BH,01110000B   ;前四位定义背景色为棕色
		;INT 10H
		;显示“~~~~~~~~~~~~~~~~~~~~~~~~~~~~~”
		ADD LINE,2
		MOV DH,LINE
		MOV DL,1
		CALL GUANGBIAO
		LEA DX,A1
		CALL OUTP
		ADD LINE,2
		;显示“**** * * *  Transfer   * * * ****”
		MOV DH,LINE
		MOV DL,1
		CALL GUANGBIAO
		LEA DX,A2
		CALL OUTP
		ADD LINE,2
		;显示“designer:B20030722 ”
		MOV DH,LINE
		MOV DL,8
		CALL GUANGBIAO
		LEA DX,A3
		CALL OUTP
		ADD LINE,2
		
;输入波特率
;显示绿色底色输入框-波特率(PS,要》=300)
		MOV AH,6 
		MOV AL,0
		MOV CH,LINE
		MOV CL,47         ;当前行，0列到47列(此处为47列)
		MOV DH,LINE
		MOV DL,52          ;当前行，47列到52列(此处为52列)
		MOV BH,00100001B   ;底色显示为绿色
		INT 10H
;波特率设置
		MOV DH,LINE
		MOV DL,1
		CALL GUANGBIAO
		LEA DX,A8
		ADD LINE,1;显示"ENTER BAUD(like 01 01H and >=0060H):$"
        MOV AH,09H
        INT 21H        
        LEA DX,STRING           ;读取STRING字符形式的数据
        MOV AH,0AH
        INT 21H        
;==字符转换成数值
;数字字符与数值的转换（以字符‘1’转化成数值01H为例）：
;字符‘1’的ASCII码为31H，所以只要在其基础上减去30H，即可得到对应的数值。
;==英文字符与数值的转换（以字符‘A’转换成数值0AH为例）：
;字符‘A’的ASCII码为41H，只要在其基础上减去37H，即可得到对应数值0AH。
;上述的代码中，若判断是字母，先减去07H，再同数字字符一样减去30H。当然这里可以设计成两个分支，直接减去37H。
;==对上述代码中逻辑左移部分的解释：上述代码中设计成每次读入并转换
;后的单个数值都存在对应区域的第四位，所以当我们已经读入高四位的数
;据后，想读入低四位的数据时，只需将原有的数据左移4位，再加上低位数据即可。这里也可以设计成乘10H后相加（与左移4位效果相同）。
        MOV BX,05H              ;目标读取的数据个数
        MOV SI,02H              ;用来指向STRING
        MOV DI,00H              ;用来指向DATA1
        MOV CL,04H
TRANS1: MOV DL,STRING[SI]       ;依次读取STRING中的字符
        CMP DL,20H              ;空格的ASCII码为20H 
        JE NEXT1                ;若是空格就跳转到NEXT1
        CMP DL,0DH              ;回车的ASCII码为0DH
        JE NEXT1                ;若是回车就跳转到NEXT1
        CMP DL,3AH              
        JB NEXT0                ;若是字母，则减去07H；若是数字，跳转到NEXT0
        SUB DL,07H        
NEXT0:  SUB DL,30H              ;减去30H后由ASCII转为数值
        SHL DATA1[DI],CL        ;逻辑左移4位后相加
        ADD DATA1[DI],DL
        INC SI
        JMP NEXT2
NEXT1:  INC DI					;遇到空格或是回车表示一个数据转换完成，此时DI加一，指向下一个存储数值的位置
        INC SI
NEXT2:  CMP BX,DI				;判断所有数据是否转换完成
        JNE TRANS1 
		
		
;帧格式显示绿色底色输入框-帧格式
		MOV AH,6 
		MOV AL,0
		MOV CH,LINE
		MOV CL,30          ;当前行，0列到30列(此处为30列)
		MOV DH,LINE
		MOV DL,31          ;当前行，30列到31列(此处为31列)
		MOV BH,00100001B   ;底色显示为绿色
		INT 10H
;显示“ENTER FRAME FORMAT(like 03):$”
INPUTF:
		MOV DH,LINE
		MOV DL,1
		CALL GUANGBIAO
		LEA DX,A9
		ADD LINE,1;显示"ENTER FRAME FORMAT(like 03):$"
        MOV AH,09H
        INT 21H        
        LEA DX,STRINGF           ;读取STRINGF字符形式的数据
        MOV AH,0AH
        INT 21H        
;字符转换成数值
        MOV BX,05H              ;目标读取的数据个数
        MOV SI,02H              ;用来指向STRING
        MOV DI,00H              ;用来指向DATAF
        MOV CL,04H
TRANS1F: MOV DL,STRINGF[SI]       ;依次读取STRINGF中的字符
        CMP DL,20H              ;空格的ASCII码为20H 
        JE NEXT1F               ;若是空格就跳转到NEXT1F
        CMP DL,0DH              ;回车的ASCII码为0DH
        JE NEXT1F                ;若是回车就跳转到NEXT1F
        CMP DL,3AH              
        JB NEXT0F                ;若是字母，则减去07H；若是数字，跳转到NEXT0
        SUB DL,07H        
NEXT0F:  SUB DL,30H              ;减去30H后由ASCII转为数值
        SHL DATAF[DI],CL        ;逻辑左移4位后相加
        ADD DATAF[DI],DL
        INC SI
        JMP NEXT2F
NEXT1F:  INC DI					;遇到空格或是回车表示一个数据转换完成，此时DI加一，指向下一个存储数值的位置
        INC SI
NEXT2F:  CMP BX,DI				;判断所有数据是否转换完成
        JNE TRANS1F            
;帧格式输入检验
;MOV BX,OFFSET DATAF[1]
;CMP WORD PTR [BX],3FH    ;输入的帧格式如果超过3FH就报错
;JA ERRORF
;显示错误提示信息
;ERRORF:;帧格式报错
	;MOV AH,6 
	;MOV AL,0
	;MOV CH,LINE
	;MOV CL,4           ;当前行，23列到30列（输入8位字符）
	;MOV DH,LINE
	;MOV DL,60
	;MOV BH,01000001B   ;底色显示为红色
	;INT 10H
	;MOV DL,1
	;CALL GUANGBIAO
	;LEA DX,WRONGF
	;CALL OUTP
	;INC LINE
	;JMP INPUTF
;EXIT1: MOV AH,4CH
	;INT 21H


;这里要在波特率和帧格式后面了，待修改；已修改
;显示绿色底色输入框-input the word
		MOV AH,6 
		MOV AL,0
		MOV CH,LINE
		MOV CL,26          ;当前行，0列到26列(此处为26列)
		MOV DH,LINE
		MOV DL,75          ;当前行，26列到75列(此处为75列)
		MOV BH,00100001B   ;底色显示为绿色
		INT 10H
;显示“Please input the word:$”
		MOV DH,LINE
		MOV DL,1
		CALL GUANGBIAO
		LEA DX,A4
		CALL OUTP
		ADD LINE,1     
;UI显示结束(待修改)

;主程序内容
	   MOV AX,DATA
	   MOV DS,AX
	   CLI                        ;关中断
	   MOV AH,0AH
	   MOV DX,OFFSET MESG
	   INT 21H
	   MOV AH,2
	   MOV DL,0AH
	   INT 21H
;显示青色底色输出框
	   MOV AH,6 
		MOV AL,0
		MOV CH,LINE
		MOV CL,15     ;当前行，0列到15列(此处为15列)
		MOV DH,LINE
		MOV DL,75       ;当前行，15列到75列(此处为75列)
		MOV BH,11100001B   ;底色显示为黄色
		INT 10H   
;显示“The output”
	   MOV DH,LINE
	   MOV DL,1
	   CALL GUANGBIAO
	   LEA DX,A5
	   CALL OUTP
	   ADD LINE,2 
;主程序内容
	   MOV CH,0
	   CALL I8250                 ;主串口初始化
	   CALL I8259                 ;开放主8259A辅串口中断	   
	   CALL RD0B                ;读中断向量
	   CALL WR0B 				  ;写中断向量
	   STI                        ;开中断	   
	   MOV BX,OFFSET MESG+2
	   MOV CL,MESG+1
 
SCANT: MOV DX,2FDH
	   IN AL,DX
	   TEST AL,20H
	   JZ SCANT	   
	   MOV DX,2F8H
	   MOV AL,[BX]
	   OUT DX,AL
	   INC BX                   ;计数
	   MOV DX,0
TWAIT: DEC DX
	   JNZ TWAIT                ;加循环延时来保证中断接收的时间
	   LOOP SCANT
RETURN:CALL RESET
	   MOV AH,4CH
	   INT 21H             ;返回 DOS
	   
	   
	   
	   
;往下是子程序
;光标的函数GUANGBIAO
	   GUANGBIAO PROC
	   PUSH AX
	   PUSH BX
	   MOV AH,2          ;2号功能调用，预置光标的位置
	   MOV BH,0
	   INT 10H           ;定光标
	   POP BX
	   POP AX
	   RET
	   GUANGBIAO ENDP
;界面样式显示输出的函数OUTP
	   OUTP PROC
	   PUSH AX
	   MOV AH,9          ;9号功能调用，字符串输出
	   INT 21H
	   POP AX
	   RET
	   OUTP ENDP

RECEIVE PROC
	   PUSHA
	   PUSH DS
	   MOV AX,DATA
	   MOV DS,AX
	   MOV DX,2F8H
	   IN AL,DX
	   AND AL,7FH
	   MOV AH, 2
       MOV DL, AL
       INT 21H           ;屏幕显示
	   
EXIT:  MOV AL,20H      ;中断结束命令
	   OUT 20H,AL
	   POP DS           ;恢复现场
	   POPA
	   IRET
RECEIVE ENDP


I8250 PROC              ;主串口初始化子程序
	   MOV DX,2FBH
	   MOV AL,80H
	   OUT DX,AL       ;寻址位置1
	   
	   MOV DX,2F9H		;设置波特率，此处00，60是1200波特
	   MOV AL,DATA1[0]
	   OUT DX,AL	     ;除数高8位
	   MOV DX,2F8H
	   MOV AL,DATA1[1]
	   OUT DX,AL	     ;除数高8位
	   
;帧格式，案例为03H
	   MOV DX,2FBH
	   MOV AL,DATAF[0]
	   OUT DX,AL

	   MOV DX,2F9H
	   MOV AL,01H	    ;允许接收中断
	   OUT DX,AL
	   MOV DX,2FCH
	   MOV AL,18H   	;内环，8250能送出中断请求
	   OUT DX,AL
	   RET
I8250 ENDP
 
I8259 PROC
	   IN AL,21H
	   AND AL,11110111B
	   OUT 21H,AL	    ;置中断屏蔽寄存器
	   RET
I8259 ENDP
 
RD0B PROC  	        ;保存原来系统的0BH 中断向量
	   MOV AX,350BH
	   INT 21H
	   MOV WORD PTR OLD0B,BX
	   MOV WORD PTR OLD0B+2,ES
	   RET
RD0B ENDP
 
WR0B PROC         	;置换0BH型中断向量指向自定义中断服务程序
	   PUSH DS
	   MOV AX,CODE
	   MOV DS,AX
	   MOV DX,OFFSET RECEIVE
	   MOV AX,250BH
	   INT 21H
	   POP DS
	   RET
WR0B ENDP
RESET PROC                ;恢复系统0B中断向量
	   IN AL,21H
	   OR AL,00001000B
	   OUT 21H,AL
	   MOV AX,250BH
	   MOV DX,WORD PTR OLD0B
	   MOV DS,WORD PTR OLD0B+2
	   INT 21H
	   RET
RESET ENDP
CODE ENDS
	END BEG





