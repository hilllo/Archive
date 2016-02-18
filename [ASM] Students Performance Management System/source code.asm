;;;;;;;;;;;;;;;2011051742 网络工程 卢晓山;;;;;;;;;;;;;;;;;;;;;
; ************************************************************
; *********************用户界面的宏定义*********************** 
; ***********Macro Definition of the User Interface***********

SCROLL    MACRO     N,ULR,ULC,LRR,LRC,ATT
          MOV       AH,6                        ;clear screen or roll up
          MOV       AL,N                        ;N=rollup rows number; when N=0, blank
          MOV       CH,ULR                      ;row number of the upper left
          MOV       CL,ULC                      ;column number of the bottom left
          MOV       DH,LRR                      ;row number of the bottom right
          MOV       DL,LRC                      ;column number of the bottom right
          MOV       BH,ATT                      ;property of the rollup row
          INT       10H
          ENDM 

CURSE     MACRO     CURY,CURX    
          MOV       AH,2                        ;position of the curor
          MOV       DH,CURY                     ;row number
          MOV       DL,CURX                     ;column number
          MOV       BH,0                        ;current page
          INT       10H
          ENDM 

MYCLEAN   MACRO 
          SCROLL    0,0,0,24,79,50H             ;open outside window, magenta background, clear screen 
          SCROLL    24,1,0,23,79,2FH            ;open inside windo, green background, white lettering
          CURSE     1, 0
          ENDM

MYSCROLL   MACRO 
          ;SCROLL    0,0,0,24,79,50H             ;open outside window, magenta background, clear screen  
          SCROLL    24,1,0,23,79,2FH            ;open inside windo, green background, white lettering
          CURSE     1, 0
          ENDM 

data segment
    TESTVar DB 'AAA 01 AAA','$'
; ***********************************************************
; *************************文件参数**************************
; ************************Parameters*************************
    F1 DB 'D:\SMS.DAT',0  ; address of the data file
    FILE DW 0                  ;save file number
    HD DB 350 DUP(0)           ;data buffer
    THE_END DB 2 DUP(0)    
    STU     DB 350 DUP (0)

    BlankSTU DB '00'             ;空白数据学生学号
    STUIDLength DW 2             ;学生学号长度
    STULength DW 35              ;学生数据长度
    StaticsticsBuff DW 0         ;统计数据辅助变量
    InputIDBuff DB 3,0,'000'     ;ID输入
    InputScoreBuff DB 4,0,'0000'
    RankSet DW '01','$$'         ;排序辅助变量
    
; ***********************************************************
; ***学生数据:学号,语/数/英成绩/名次,总成绩/名次,平均成绩****
; ******Student Data:ID, Scores, Ranks, Average scores*******
    STUStatement DB 'ID CN  CR MTH MR EN  ER TTL TR AVR','$'
    ;学号只要为00则不显示且不参与任何运算
    ;if ID='00', then that student would not be involved in the calculation
    ;第一次运行时，按照此处数据创建文件
    ;create a data file when the program runs the first time
    STU1    DB '01',' ','089',' ','00',' ','093',' ','00',' ','042',' ','00',' ','000',' ','00',' ','000','$'
    STU2    DB '02',' ','073',' ','00',' ','088',' ','00',' ','085',' ','00',' ','000',' ','00',' ','000','$'
    STU3    DB '00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000','$'        
    STU4    DB '00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000','$'        
    STU5    DB '00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000','$'        
    STU6    DB '00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000','$'         
    STU7    DB '00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000','$'  
    STU8    DB '00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000','$'         
    STU9    DB '00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000','$'         
    STU10   DB '00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000',' ','00',' ','000','$' 
    ;STU1    DB '01',' ','089',' ','04',' ','093',' ','02',' ','042',' ','10',' ','224',' ','09',' ','074','$'
    ;STU2    DB '02',' ','073',' ','10',' ','088',' ','03',' ','085',' ','04',' ','246',' ','06',' ','082','$'    
    ;STU3    DB '03',' ','091',' ','03',' ','082',' ','07',' ','099',' ','01',' ','272',' ','01',' ','090','$'        
    ;STU4    DB '04',' ','079',' ','08',' ','080',' ','08',' ','083',' ','06',' ','242',' ','07',' ','080','$'
    ;STU5    DB '05',' ','085',' ','06',' ','071',' ','10',' ','066',' ','09',' ','222',' ','10',' ','074','$'        
    ;STU6    DB '06',' ','076',' ','09',' ','088',' ','03',' ','085',' ','04',' ','249',' ','05',' ','083','$'        
    ;STU7    DB '07',' ','095',' ','01',' ','072',' ','09',' ','072',' ','08',' ','239',' ','08',' ','079','$' 
    ;STU8    DB '08',' ','092',' ','02',' ','086',' ','05',' ','088',' ','03',' ','266',' ','02',' ','088','$'        
    ;STU9    DB '09',' ','087',' ','05',' ','094',' ','01',' ','079',' ','07',' ','260',' ','03',' ','086','$'        
    ;STU10   DB '10',' ','082',' ','07',' ','086',' ','05',' ','090',' ','02',' ','258',' ','04',' ','086','$'        

; ***********************************************************
; ************************学生成绩组*************************
; **************************Scores***************************
    cn DW 10 dup (0)
    mth DW 10 dup (0)
    en DW 10 dup (0)
    ttl DW 10 dup (0)
    ;cn  DW 89,73,0,0,0,0,0,0,0,0
    ;mth DW 93,88,0,0,0,0,0,0,0,0
    ;en  DW 42,85,0,0,0,0,0,0,0,0
    ;ttl DW 224,246,0,0,0,0,0,0,0,0

; ***********************************************************
; *********************主程序的提示信息**********************
; ************Information of the Main Programe***************
    MainManuTitle DB '****Welcome to the Student Management System****','$'
    MainManuSTUTitle DB 'Current Data:','$'
    MaTag11  DB '1:insert','$'
    MaTag12  DB '2:revise','$'
    MaTag13  DB '3:delete','$'
    MaTag14  DB '4:rank','$'
    MaTag15  DB '5:query','$'
    MaTag16  DB '6:statistics','$'
    MaTag10  DB '0:exit','$'
    MaTag2  DB 'Please chose the operator: ', '$'
    MaTag3  DB 'Error!Press any key to input again...', '$'   
    MaTag4  DB 'Thank you for using!', '$'
    MaTag5 DB 'Done!The system is calculating.Please wait for a minute...','$'
    MaTag6 DB 'Done!Press any key to return...','$'
    pkey db "press any key...$"

; ***********************************************************
; *****************插入/删除/修改提示信息********************
; ***********Message of Insert/Delete/Modification***********
    ManageTagID DB 'ID:','$'
    ManageTagCN DB 'CN:','$'
    ManageTagMTH DB 'MTH:','$'
    ManageTagEN DB 'EN:','$'
    ManageTagTTL DB 'TTL:','$'
    ManageTagInsert DB 'Please input the data you want to insert.','$' 
    ManageTagRevise DB 'Please input the data you want to revise.','$'
    ManageTagDelete DB 'Please input the data you want to delete.','$'
    ManageTagError DB 'Misinput.Press any key to return...','$'

; ***********************************************************
; **********************排序提示信息*************************
;**********************Message of Rank***********************
    RankTagManu DB '1:Cn Rank 2:Mth Rank 3:En Rank 4:Total Rank','$'
    RankTagTitle DB 'RK ID','$'
    RankTagCN DB 'CN Rank:','$'    
    RankTagMTH DB 'MTH Rank:','$'
    RankTagEN DB 'EN Rank:','$'
    RankTagTTL DB 'Total Rank:','$'
; ***********************************************************
; **********************查询提示信息*************************
;*******************Message of Inquiry***********************
    QrSTUTag DB 'Please input the data you want to query.','$'
    QrSTUSel DB '1:Cn 2:Mth 3:En 4:TTL','$'
    QrFailTag DB 'Fail List:','$'
    QrFailSel DB '1:Cn 2:Mth 3:En','$'
    QrManuSel DB '1:Subject 2:Fail','$'
; ***********************************************************
; **********************统计提示信息*************************    
;******************Message of Statistics*********************
    STTManuTag DB 'Please input the type of statistic.','$'
    STTAVRCNBuff DB '000','$'
    STTAVRMTHBuff DB '000','$'
    STTAVRENBuff DB '000','$'
    STTAVRTTLBuff DB '000','$'
    STTAVRCN DB 'CN AVR:','$'
    STTAVRMTH DB 'MTH AVR:','$'
    STTAVREN DB 'EN AVR:','$'
    STTAVRTTL DB 'TTL AVR:','$'
    STTManuSel DB '1:Average 2:Sort','$'    
    SSTOut DB '000','$'
    SST60 DB  '0-60   :','$'
    SST70 DB  '61-70  :','$'
    SST80 DB  '71-80  :','$'
    SST90 DB  '81-90  :','$'
    SST100 DB '91-100:','$'    
    SSTCN60 DW 0
    SSTCN70 DW 0
    SSTCN80 DW 0
    SSTCN90 DW 0
    SSTCN100 DW 0
    SSTMTH60 DW 0
    SSTMTH70 DW 0
    SSTMTH80 DW 0
    SSTMTH90 DW 0
    SSTMTH100 DW 0
    SSTEN60 DW 0
    SSTEN70 DW 0
    SSTEN80 DW 0
    SSTEN90 DW 0
    SSTEN100 DW 0
ends

stack segment
    dw   128  dup(0)
ends

code segment
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax
    SCROLL    0,0,0,24,79,50H             ;开外窗口，品红底,清屏
; ************************************************************
; *******从文件中读入数据(文件不存在时,按设定新建文件)********    
; Read Data from File (If the file does not exsist, create new one)
    MOV DX,OFFSET F1     ;DX指向文件名 DX point to the file name
    MOV AL,02H           ;文件属性 file properties
    MOV AH,3DH                                          
    INT 21H              ;打开文件 open file
    JNC OpenSucc         ;succeed, jump
    CALL CreateData      ;fail, create file and initialize it
    MOV DX,OFFSET F1     ;DX指向文件名
    MOV AL,02H           ;文件属性
    MOV AH,3DH
    INT 21H   
OpenSucc:
    MOV FILE,AX          ;保存文件号，留作备用 save file number
    MOV BX,FILE
    LEA DX,STU
    MOV AH,3FH
    MOV CX,350
    INT 21H
    MOV BX,FILE
    MOV AH,3EH       ;关闭 close
    INT 21H

;[MAIN];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Main:
; ************************************************************
; ***************数据初始化（计算名次、成绩）*****************    
; ********************Data Initialization*********************
    CALL InitiateScore ;初始化成绩组数据
MainCal:
    CALL InitiateRank  ;初始化排名
; ************************************************************
; *********主页提示，并按照用户的选择调用对应的子程序*********
; ********************Message of Main Page********************
MainManu:
    MYCLEAN
    CALL OutMainManu        ; 输出主页的操作提示    
    MOV AH, 01H
    INT 21H
    CALL MyEnter           ; 用户输入请求的操作 input demand
    
    CMP AL,'0'                
    JZ EXT                ; 用户选择exit,跳到EXT程序段,退出程序
MMO1:    
    CMP AL,'1'            ; 用户选择insert,执行插入操作
    JNZ MMO2
    CALL Insert
    JMP NXT123
MMO2:    
    CMP AL,'2'            ; 用户选择revise,执行修改操作
    JNZ MMO3
    CALL Revise
    JMP NXT123

MMO3:    
    CMP AL,'3'            ; 用户选择delete,执行删除操作
    JNZ MMO4
    CALL Delete
    JMP NXT123
    
MMO4:    
    CMP AL,'4'            ; 用户选择rank,执行排序操作
    JNZ MMO5
    CALL RankSub
    JMP NXT

MMO5:
    CMP AL,'5'            ; 用户选择query,执行查询操作
    JNZ MMO6
    CALL QrManu
    JMP NXT
MMO6:    
    CMP AL,'6'            ; 用户选择statistics,执行统计操作
    JNZ MMFail
    CALL SSTManu
    JMP NXT
MMFail:    
    LEA DI,MaTag3         ;输入错误 input error
    CALL OutStr 
    CALL MyEnter
NXT:
    MOV AH, 01H
    INT 21H
    JMP MainManu
NXT123:
    CALL MyEnter
    LEA DI,MaTag5
    CALL OutStr
    JMP MainCal                     
EXT:  
    LEA DI, MaTag4   ;输出感谢信息,退出程序 exit
    CALL OutStr 
    CALL MyEnter

;[MAIN END];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; ************************************************************
; ************************更新文件数据************************ 
; **********************Update File Data**********************
    MOV DX,OFFSET F1     ;DX指向文件名 DX point to the file name
    MOV AL,02H           ;文件属性 file properties
    MOV AH,3DH           ;打开文件 open file
    INT 21H
    MOV FILE,AX            ;保存文件号save file number
    LEA DX,STU
    MOV CX,350
    MOV BX,AX
    MOV AH,40H
    INT 21H
    MOV BX,SI
    MOV AH,3EH           ;关闭close
    INT 21H



    lea dx, pkey
    mov ah, 9
    int 21h        ; output string at ds:dx
    
    ; wait for any key....    
    mov ah, 1
    int 21h
    
    mov ax, 4c00h ; exit to operating system.
    int 21h    
ends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ****************子程序Insert：插入一个成绩******************
; *************Subroutine Insert: Insert a score**************
; ************************************************************
    Insert PROC NEAR
        PUSH SI
        PUSH DI
        PUSH DX
        PUSH BX
        
        LEA SI,BlankSTU        ;找出第一个'00' find out the first '00'
        CALL SearchID
        CMP DX,10
        JZ  InsertFail         ;纠错功能:超过10个ID时不再插入 fail: stop inserting when there are more than 10 students
        PUSH DI
        PUSH DX
        MYCLEAN
        CALL OutALL
        CALL MyEnter
        LEA DI,ManageTagInsert
        CALL OutStr
        CALL MyEnter
        LEA DI,ManageTagID
        CALL OutStr
        LEA SI,InputIDBuff
        CALL InStr
        ADD SI,2
        ;检'00'
        MOV CX,STUIDLength
        LEA DI,BlankSTU
        CALL CMPString
        CMP CX,0
        JZ  InsertFailTemp         ;纠错功能:输入'00'时不插入 fail: do not insert when the input is '00'
        POP DX
        POP DI        
        ;开始插入start inserting
        PUSH DI                ;保留DI(更新数据用) save DI (used to update data)
        PUSH DX                ;保留DX(更新数据用) save DX (used to update data)
        LEA SI,InputIDBuff     ;插入ID insert ID
        ADD SI,2
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL                               
        CALL MyEnter           ;插入CN(三位数) insert Chinese score (three-digit)
        PUSH DI
        LEA DI,ManageTagCN     
        CALL OutStr
        POP DI
        LEA SI,InputScoreBuff
        CALL InStr
        ADD SI,2
        ADD DI,2
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL        
        CALL MyEnter           ;插入MTH(三位数) insert math score (three-digit)
        PUSH DI
        LEA DI,ManageTagMTH     
        CALL OutStr
        POP DI
        LEA SI,InputScoreBuff
        CALL InStr
        ADD SI,2
        ADD DI,5
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL          
        CALL MyEnter           ;插入EN(三位数) insert English score (three-digit)
        PUSH DI
        LEA DI,ManageTagEN     
        CALL OutStr
        POP DI
        LEA SI,InputScoreBuff
        CALL InStr
        ADD SI,2
        ADD DI,5
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL             
        POP DX                  ;更新数据 update data
        POP DI                  ;更新数据 update data
        MOV SI,DI
        LEA DI,cn
        ADD DI,DX
        ADD DI,DX
        CALL UpdateScore
        XCHG DI,SI
        CALL UpdateTtlAvr
        JMP InsertEnd           ;插入完成 finish
        InsertFailTemp:
        POP DX
        POP DI
        InsertFail:             ;插入失败:输入'00'或超过10个ID时不再插入 fail
        CALL MyEnter
        LEA DI,ManageTagError
        CALL OutStr
        POP BX
        POP DX
        POP DI
        POP SI
        JMP NXT
        InsertEnd:
        POP BX
        POP DX
        POP DI
        POP SI
        RET
    Insert ENDP

; ****************子程序Revise：更改一个成绩******************
; ************************************************************
    Revise PROC NEAR
        PUSH SI
        PUSH DI
        PUSH DX
        PUSH BX

        
        MYCLEAN
        CALL OutALL
        CALL MyEnter
        LEA DI,ManageTagRevise
        CALL OutStr
        CALL MyEnter
        LEA DI,ManageTagID
        CALL OutStr
        LEA SI,InputIDBuff
        CALL InStr
        ADD SI,2
        ;检'00'
        MOV CX,STUIDLength
        LEA DI,BlankSTU
        CALL CMPString
        CMP CX,0
        JZ  ReviseFail         ;纠错功能:输入'00'时不插入        
                 
        CALL SearchID          ;找出与输入数相匹配的数
        CMP DX,10
        JZ  ReviseFail         ;纠错功能:找不到时不插入
                
        ;开始插入
        PUSH DI                ;保留DI(更新数据用)
        PUSH DX                ;保留DX(更新数据用)
        LEA SI,InputIDBuff     ;插入ID
        ADD SI,2
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL                               
        CALL MyEnter           ;插入CN(三位数)
        PUSH DI
        LEA DI,ManageTagCN     
        CALL OutStr
        POP DI
        LEA SI,InputScoreBuff
        CALL InStr
        ADD SI,2
        ADD DI,2
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL        
        CALL MyEnter           ;插入MTH(三位数)
        PUSH DI
        LEA DI,ManageTagMTH     
        CALL OutStr
        POP DI
        LEA SI,InputScoreBuff
        CALL InStr
        ADD SI,2
        ADD DI,5
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL          
        CALL MyEnter           ;插入EN(三位数)
        PUSH DI
        LEA DI,ManageTagEN     
        CALL OutStr
        POP DI
        LEA SI,InputScoreBuff
        CALL InStr
        ADD SI,2
        ADD DI,5
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL             
        POP DX                  ;更新数据
        POP DI                  ;更新数据
        MOV SI,DI
        LEA DI,cn
        ADD DI,DX
        ADD DI,DX
        CALL UpdateScore
        XCHG DI,SI
        CALL UpdateTtlAvr
        JMP ReviseEnd           ;插入完成 
        
        ReviseFail:             ;插入失败:输入'00'或找不到ID时不插入
        CALL MyEnter
        LEA DI,ManageTagError
        CALL OutStr
        POP BX
        POP DX
        POP DI
        POP SI
        JMP NXT
        ReviseEnd:
        POP BX
        POP DX
        POP DI
        POP SI
        RET
    Revise ENDP
    
; ****************子程序Delete：删除一个成绩******************
; ************************************************************
    Delete PROC NEAR
        PUSH SI
        PUSH DI
        PUSH DX
        MYCLEAN
        CALL OutALL
        CALL MyEnter
        LEA DI,ManageTagDelete
        CALL OutStr
        CALL MyEnter
        LEA DI,ManageTagID
        CALL OutStr
        LEA SI,InputIDBuff
        CALL InStr
        ADD SI,2
        ;检'00'
        MOV CX,STUIDLength
        LEA DI,BlankSTU
        CALL CMPString
        CMP CX,0
        JZ  DeleteFail
        CALL SearchID
        CMP DX,10
        JZ  DeleteFail
        PUSH DX
        MOV DX,'00'
        MOV [DI],DX
        POP DX                ;更新数据
        MOV SI,DI             
        LEA DI,cn
        ADD DI,DX
        ADD DI,DX
        CALL UpdateScore
        XCHG DI,SI
        CALL UpdateTtlAvr
        
        JMP DeleteEnd
        ;纠错功能:找不到ID时输出错误信息并返回主菜单
        DeleteFail:
        CALL MyEnter
        LEA DI,ManageTagError
        CALL OutStr
        POP DX
        POP DI
        POP SI
        JMP NXT
        
        DeleteEnd:
        POP DX
        POP DI
        POP SI
        RET
    Delete ENDP
; *******************子程序RankSub：排序**********************
; ************************************************************
    RankSub PROC NEAR
        PUSH SI
        PUSH DI
        PUSH DX
        PUSH CX
        PUSH AX        
       
        MyClean        
        LEA DI, RankTagManu
        CALL OutStr
        CALL MyEnter
        LEA DI,MaTag2
        CALL OutStr
        MOV AH,01H
        INT 21H
        CALL MyEnter           ; 用户输入请求的操作。
        CMP AL,'1'
        JZ RS1
        CMP AL,'2'
        JZ RS2
        CMP AL,'3'
        JZ RS3
        CMP AL,'4'
        JZ RS4
        LEA DI,ManageTagError
        CALL OutStr
        CALL MyEnter
        JMP RSEND
         
RS1:        ;语文成绩排名
        MOV DX,'10'
        MOV RankSet,DX
        LEA DI,RankTagCN       
        CALL OutStr
        CALL MyEnter      
        LEA DI,RankTagTitle
        CALL OutStr
        CALL MyEnter
        LEA DI,RankSet
        MOV CX,10
        RankSubStepLoop:
          PUSH CX
          CALL OutStr          
          MOV AL,' '
          CALL OutChar
          LEA SI,STU
          ADD SI,7
          MOV CX,10
          RankSubStep:
            PUSH CX
            MOV CX,2
            CALL CMPString
            CMP CX,0
            JNZ RankSubNext ;不匹配则不输出            
            MOV AL,[SI-7]     ;匹配则输出
            CALL OutChar
            MOV AL,[SI-6]             
            CALL OutChar            
            MOV AL,' '
            CALL OutChar
            RankSubNext:
            ADD SI,35
            POP CX
            LOOP RankSubStep
          RankSubStepLoopNext:
          CALL MyEnter
          POP CX
          CALL INCStr
          LOOP  RankSubStepLoop
          JMP RSEND
        
RS2:        ;数学成绩排名
        MOV DX,'10'
        MOV RankSet,DX
        LEA DI,RankTagMTH       ;;;;
        CALL OutStr
        CALL MyEnter      
        LEA DI,RankTagTitle
        CALL OutStr
        CALL MyEnter
        LEA DI,RankSet
        MOV CX,10
        RankSubStepLoopMTH:     ;;;;
          PUSH CX
          CALL OutStr          
          MOV AL,' '
          CALL OutChar
          LEA SI,STU
          ADD SI,14             ;;;;
          MOV CX,10
          RankSubStepMTH:       ;;;;
            PUSH CX
            MOV CX,2
            CALL CMPString
            CMP CX,0
            JNZ RankSubNextMTH ;;;;不匹配则不输出         
            MOV AL,[SI-14]     ;;;;匹配则输出
            CALL OutChar
            MOV AL,[SI-13]     ;;;;        
            CALL OutChar            
            MOV AL,' '
            CALL OutChar
            RankSubNextMTH:    ;;;;
            ADD SI,35
            POP CX
            LOOP RankSubStepMTH     ;;;;
          RankSubStepLoopNextMTH:   ;;;;
          CALL MyEnter
          POP CX
          CALL INCStr
          LOOP  RankSubStepLoopMTH  ;;;;
          JMP RSEND

RS3:        ;英语成绩排名
        MOV DX,'10'
        MOV RankSet,DX
        LEA DI,RankTagEN       ;;;;
        CALL OutStr
        CALL MyEnter      
        LEA DI,RankTagTitle
        CALL OutStr
        CALL MyEnter
        LEA DI,RankSet
        MOV CX,10
        RankSubStepLoopEN:     ;;;;
          PUSH CX
          CALL OutStr          
          MOV AL,' '
          CALL OutChar
          LEA SI,STU
          ADD SI,21            ;;;;
          MOV CX,10
          RankSubStepEN:       ;;;;
            PUSH CX
            MOV CX,2
            CALL CMPString
            CMP CX,0
            JNZ RankSubNextEN ;;;;不匹配则不输出         
            MOV AL,[SI-21]     ;;;;匹配则输出
            CALL OutChar
            MOV AL,[SI-20]     ;;;;        
            CALL OutChar            
            MOV AL,' '
            CALL OutChar
            RankSubNextEN:    ;;;;
            ADD SI,35
            POP CX
            LOOP RankSubStepEN     ;;;;
          RankSubStepLoopNextEN:   ;;;;
          CALL MyEnter
          POP CX
          CALL INCStr
          LOOP  RankSubStepLoopEN  ;;;;
          JMP RSEND        

RS4:        ;总分成绩排名
        MOV DX,'10'
        MOV RankSet,DX
        LEA DI,RankTagTTL       ;;;;
        CALL OutStr
        CALL MyEnter      
        LEA DI,RankTagTitle
        CALL OutStr
        CALL MyEnter
        LEA DI,RankSet
        MOV CX,10
        RankSubStepLoopTTL:     ;;;;
          PUSH CX
          CALL OutStr          
          MOV AL,' '
          CALL OutChar
          LEA SI,STU
          ADD SI,28             ;;;;
          MOV CX,10
          RankSubStepTTL:       ;;;;
            PUSH CX
            MOV CX,2
            CALL CMPString
            CMP CX,0
            JNZ RankSubNextTTL ;;;;不匹配则不输出         
            MOV AL,[SI-28]     ;;;;匹配则输出
            CALL OutChar
            MOV AL,[SI-27]     ;;;;        
            CALL OutChar            
            MOV AL,' '
            CALL OutChar
            RankSubNextTTL:    ;;;;
            ADD SI,35
            POP CX
            LOOP RankSubStepTTL     ;;;;
          RankSubStepLoopNextTTL:   ;;;;
          CALL MyEnter
          POP CX
          CALL INCStr
          LOOP  RankSubStepLoopTTL  ;;;;
          JMP RSEND

RSEND:  
        LEA DI,MaTag6
        CALL OutStr
        POP AX
        POP CX
        POP DX
        POP DI
        POP SI           
        RET
    RankSub ENDP

; *********************子程序QrManu：查询*********************
; ************************************************************
    QrManu PROC NEAR
        PUSHA
        
        MYCLEAN        
        LEA DI,QrManuSel
        CALL OutStr
        CALL MyEnter
        LEA DI,QrSTUTag
        CALL OutStr
        MOV AH,01H
        INT 21H
        CALL MyEnter           ; 用户输入请求的操作。
        CMP AL,'1'
        JZ QM1
        CMP AL,'2'
        JZ QM2
        JMP QrManuFail
QM1:        
        CALL QrSTU
        JMP QrManuEnd
QM2:        
        CALL QrFail
        JMP QrManuEnd        
        
        QrManuFail:
        CALL MyEnter
        LEA DI,ManageTagError
        CALL OutStr
        POPA
        JMP NXT
        
        QrManuEnd:
        POPA
        RET
    QrManu ENDP
    
; ************子程序QrFail：查询单科不及格学生名单************
; ************************************************************
    QrFail PROC NEAR
        PUSHA
      
        MYCLEAN        
        LEA DI,QrFailSel
        CALL OutStr
        CALL MyEnter
        LEA DI,QrSTUTag
        CALL OutStr
        MOV AH,01H
        INT 21H
        CALL MyEnter           ; 用户输入请求的操作。
        CMP AL,'1'
        JZ QF1
        CMP AL,'2'
        JZ QF2
        CMP AL,'3'
        JZ QF3
        JMP QrFailFail
QF1:  
      LEA SI,cn
      JMP QFGO
QF2:  
      LEA SI,mth
      JMP QFGO
QF3:  
      LEA SI,en
      JMP QFGO      
QFGO:      
      LEA DI,QrFailTag
      CALL OutStr
      CALL MyEnter
      LEA DI,STU
      MOV DX,60
      MOV BX,0
      MOV CX,10
      QrFailStep:
        CMP [SI],BX
        JZ QrFailNext
        CMP [SI],DX
        JAE QrFailNext
        MOV AL,[DI]
        CALL OutChar
        MOV AL,[DI+1]
        CALL OutChar
        MOV AL,' '
        CALL OutChar
        QrFailNext:
        ADD DI,35
        ADD SI,2
        LOOP QrFailStep
      JMP QrFailEnd:  
      
      QrFailFail:
      CALL MyEnter
      CALL MyEnter
      LEA DI,ManageTagError
      CALL OutStr
      POPA
      JMP NXT
      
      QrFailEnd:
      CALL MyEnter
      LEA DI,MaTag6
      CALL OutStr
      POPA
      RET  
    QrFail ENDP

; ****子程序QrSTU：查询一名学生单科成绩及名次;总成绩及名次****
; ************************************************************
    QrSTU PROC NEAR
        PUSH SI
        PUSH DI
        PUSH DX
        PUSH AX
        
        MYCLEAN
        CALL OutALL
        CALL MyEnter
        LEA DI,QrSTUTag
        CALL OutStr
        CALL MyEnter
        LEA DI,ManageTagID
        CALL OutStr
        LEA SI,InputIDBuff
        CALL InStr
        ADD SI,2
        ;检'00'
        MOV CX,STUIDLength
        LEA DI,BlankSTU
        CALL CMPString
        CMP CX,0
        JZ  QrSTUFail
        CALL SearchID
        CMP DX,10
        JZ  QrSTUFail
        
        CALL MyEnter
        PUSH DI             ;;
        LEA DI,QrSTUSel
        CALL OutStr
        CALL MyEnter
        LEA DI,MaTag2
        CALL OutStr
        MOV AH,01H
        INT 21H
        CALL MyEnter           ; 用户输入请求的操作。
        POP DI              ;;
        CMP AL,'1'
        JZ QS1
        CMP AL,'2'
        JZ QS2
        CMP AL,'3'
        JZ QS3
        CMP AL,'4'
        JZ QS4 
        JMP QrSTUFail
        
QS1:    
        PUSH DI             ;;;
        LEA DI,ManageTagCN
        CALL OutStr
        POP DI              ;;;
        
        MOV AL,[DI+3]
        CALL OutChar
        MOV AL,[DI+4]
        CALL OutChar
        MOV AL,[DI+5]
        CALL OutChar
        CALL MyEnter
        
        PUSH DI             ;;;
        LEA DI,RankTagCN
        CALL OutStr
        POP DI              ;;;
        
        MOV AL,[DI+7]
        CALL OutChar
        MOV AL,[DI+8]
        CALL OutChar
        CALL MyEnter
        JMP QrSTUEnd
QS2:        
        
        PUSH DI             ;;;
        LEA DI,ManageTagMTH
        CALL OutStr
        POP DI              ;;;
        
        MOV AL,[DI+10]
        CALL OutChar
        MOV AL,[DI+11]
        CALL OutChar
        MOV AL,[DI+12]
        CALL OutChar
        CALL MyEnter
        
        PUSH DI             ;;;
        LEA DI,RankTagMTH
        CALL OutStr
        POP DI              ;;;
        
        MOV AL,[DI+14]
        CALL OutChar
        MOV AL,[DI+15]
        CALL OutChar
        CALL MyEnter
        JMP QrSTUEnd
        
QS3:                
        PUSH DI             ;;;
        LEA DI,ManageTagEN
        CALL OutStr
        POP DI              ;;;
        
        MOV AL,[DI+17]
        CALL OutChar
        MOV AL,[DI+18]
        CALL OutChar
        MOV AL,[DI+19]
        CALL OutChar
        CALL MyEnter
        
        PUSH DI             ;;;
        LEA DI,RankTagEN
        CALL OutStr
        POP DI              ;;;
        
        MOV AL,[DI+21]
        CALL OutChar
        MOV AL,[DI+22]
        CALL OutChar
        CALL MyEnter
        JMP QrSTUEnd
QS4:                
        PUSH DI             ;;;
        LEA DI,ManageTagTTL
        CALL OutStr
        POP DI              ;;;
        
        MOV AL,[DI+24]
        CALL OutChar
        MOV AL,[DI+25]
        CALL OutChar
        MOV AL,[DI+26]
        CALL OutChar
        CALL MyEnter
        
        PUSH DI             ;;;
        LEA DI,RankTagTTL
        CALL OutStr
        POP DI              ;;;
        
        MOV AL,[DI+28]
        CALL OutChar
        MOV AL,[DI+29]
        CALL OutChar
        CALL MyEnter        
        JMP QrSTUEnd
        
        ;纠错功能:找不到ID时输出错误信息并返回主菜单
        QrSTUFail:
        CALL MyEnter
        LEA DI,ManageTagError
        CALL OutStr
        POP AX
        POP DX
        POP DI
        POP SI
        JMP NXT
        
        QrSTUEnd:
        LEA DI,MaTag6
        CALL OutStr
        POP AX
        POP DX
        POP DI
        POP SI
        RET
    QrSTU ENDP

; *******************子程序SSTManu：统计**********************
; ************************************************************
   SSTManu PROC NEAR
        PUSHA
        MYCLEAN        
        LEA DI,STTManuSel
        CALL OutStr
        CALL MyEnter
        LEA DI,STTManuTag
        CALL OutStr
        MOV AH,01H
        INT 21H
        CALL MyEnter           ; 用户输入请求的操作。
        CMP AL,'1'
        JZ SSTM1
        CMP AL,'2'
        JZ SSTM2
        JMP SSTManuFail
SSTM1:        
        CALL STTAVR
        JMP SSTManuEnd
SSTM2:        
        CALL SSTSort
        JMP SSTManuEnd        
        
        SSTManuFail:
        CALL MyEnter
        LEA DI,ManageTagError
        CALL OutStr
        POPA
        JMP NXT
        
        SSTManuEnd:
        POPA
        RET
   SSTManu ENDP

; ***********子程序SSTSort：统计单科各分数段人数**************
; ************************************************************
   SSTSort PROC NEAR
        PUSHA
        MYCLEAN
        LEA DI, QrFailSel
        CALL OutStr
        CALL MyEnter
        LEA DI,MaTag2
        CALL OutStr
        MOV AH,01H
        INT 21H
        CALL MyEnter           ; 用户输入请求的操作。
        CMP AL,'1'
        JZ SSTS1
        CMP AL,'2'
        JZ SSTS2
        CMP AL,'3'
        JZ SSTS3
        LEA DI,ManageTagError
        CALL OutStr
        CALL MyEnter
        JMP SSTSEND
SSTS1:        
        LEA DI,SSTCN60
        LEA SI,cn
        JMP SSTSStart
SSTS2:        
        LEA DI,SSTMTH60
        LEA SI,mth
        JMP SSTSStart
SSTS3:        
        LEA DI,SSTEN60
        LEA SI,en
        JMP SSTSStart        
SSTSStart:        
        MOV CX,10
        MOV DX,0
        MOV [DI],DX
        MOV [DI+2],DX 
        MOV [DI+4],DX 
        MOV [DI+6],DX
        MOV [DI+8],DX
        MOV BX,1
        SSTSortStep:
          MOV DX,[SI]
          CMP DX,0
          JZ SSTSortNext:
          CMP DX,60
          JBE SSTS60
          CMP DX,70
          JBE SSTS70
          CMP DX,80
          JBE SSTS80
          CMP DX,90
          JBE SSTS90
          CMP DX,100
          JBE SSTS100
          SSTS60:
            ADD [DI],BX
            JMP SSTSortNext 
          SSTS70:
            ADD [DI+2],BX
            JMP SSTSortNext
          SSTS80:
            ADD [DI+4],BX
            JMP SSTSortNext
          SSTS90:
            ADD [DI+6],BX
            JMP SSTSortNext
          SSTS100:
            ADD [DI+8],BX        
          SSTSortNext:
          ADD SI,2
          LOOP SSTSortStep
        
        MOV CX,5
        MOV DX,0
        LEA BX,SST60
        MOV SI,DI
        LEA DI,SSTOut
        SSTOutStep:
          CALL TranslateDigit
          PUSH DI
          MOV DI,BX
          CALL OutStr
          POP DI
          INC DI
          CALL OutStr
          CALL MyEnter
          DEC DI
          ADD SI,2
          ADD BX,8 
          LOOP SSTOutStep
        SSTSEND:
        LEA DI,MaTag6
        CALL OutStr
        POPA
        RET
   SSTSort ENDP       

; ***********子程序STTAVR：统计单科/总分平均分****************
; ************************************************************
    STTAVR PROC NEAR
        PUSHA
        MYCLEAN
        LEA DI,QrSTUSel
        CALL OutStr
        CALL MyEnter
        LEA DI,MaTag2
        CALL OutStr        
        MOV AH,01H
        INT 21H
        CALL MyEnter           ; 用户输入请求的操作。
        CMP AL,'1'
        JZ SA1
        CMP AL,'2'
        JZ SA2
        CMP AL,'3'
        JZ SA3
        CMP AL,'4'
        JZ SA4 
        JMP STTAVRFail        
SA1:    
        LEA SI,cn
        LEA DI,STTAVRCN
        CALL OutStr
        LEA DI,STTAVRCNBuff
        JMP STTAVRStart
SA2:    
        LEA SI,mth
        LEA DI,STTAVRMTH
        CALL OutStr
        LEA DI,STTAVRMTHBuff
        JMP STTAVRStart
SA3:
        LEA SI,en
        LEA DI,STTAVREN
        CALL OutStr
        LEA DI,STTAVRENBuff
        JMP STTAVRStart
SA4:    
        LEA SI,ttl
        LEA DI,STTAVRTTL
        CALL OutStr
        LEA DI,STTAVRTTLBuff
        
        STTAVRStart:
        MOV CX,10
        MOV BX,0  
        MOV AX,0
        MOV DX,0
        STTAVRStep:
          CMP BX,[SI]
          JZ STTAVRNext
          INC DX
          ADD AX,[SI]
          STTAVRNext:          
          ADD SI,2
          LOOP STTAVRStep
        DIV DL
        MOV AH,0
        MOV StaticsticsBuff,AX
        MOV DX,0
        LEA SI,StaticsticsBuff
        CALL TranslateDigit
        CALL OutStr
        CALL MyEnter          
        JMP STTAVREnd        
        ;纠错功能:找不到ID时输出错误信息并返回主菜单
        STTAVRFail:
        LEA DI,ManageTagError
        CALL OutStr
        JMP NXT        
        STTAVREnd:
        LEA DI,MaTag6
        CALL OutStr
        POPA
        RET
    STTAVR ENDP

; ***************子程序InitiateRank：初始化排名***************
; ************************************************************
    InitiateRank PROC NEAR
        PUSH SI
        PUSH DI
        PUSH CX
        LEA DI,STU
        LEA SI,cn
        MOV CX,10
        InitiateRankStep:
          CALL UpdateRank
          ADD DI,35
          ADD SI,2
          LOOP InitiateRankStep
        POP CX
        POP DI
        POP SI
        RET
    InitiateRank ENDP

; *子程序InitiateScore：初始化成绩组数据及总分/平均分计算录入*
; ************************************************************
    InitiateScore PROC NEAR
        PUSH SI
        PUSH DI
        PUSH CX
        LEA SI,STU
        LEA DI,cn
        MOV CX,10
        InitiateScoreStep:
          CALL UpdateScore         ;更新成绩组
          XCHG SI,DI
          CALL UpdateTtlAvr        ;计算总分与平均分
          XCHG SI,DI
          ADD SI,35
          ADD DI,2
          LOOP InitiateScoreStep
        POP CX
        POP DI
        POP SI
        RET
    InitiateScore ENDP

; *******子程序UpdateRank：更新一个学生三科及总分排名*********
;    入口参数: DI:第n个学生数据字符串第一位 SI:第n个成绩组数据
; ************************************************************
   UpdateRank PROC NEAR
        PUSH SI
        PUSH DI
        PUSH DX
        PUSH BX
        PUSH CX
        PUSH AX
        
         ;检查学号是否为'00'
        PUSH SI             ;;;;;;
        LEA SI,BlankSTU
        MOV CX,STUIDLength
        CALL CMPString
        CMP CX,0
        JNZ  UpdateRankStep ;不为'00'则计算
        
        POP SI
        MOV DX,'00'         ;为'00'则所有排名均置为'00'
        MOV [DI+7],DX
        MOV [DI+14],DX
        MOV [DI+21],DX
        MOV [DI+28],DX

        JMP UpdateRankEnd
        
        UpdateRankStep:     ;不为'00'则计算
        POP SI
        MOV DX,'10'         ;先置为'01'
        MOV [DI+7],DX
        MOV [DI+14],DX
        MOV [DI+21],DX
        MOV [DI+28],DX
        
        MOV CX,4
        LEA BX,cn
        UpdateRank4:
        PUSH CX
        PUSH BX
        
        MOV CX,10
        MOV AX,[SI]
        ADD DI,7
        UpdateRankLoop:
          CMP AX,[BX]
          JAE UpdateRankNext ;当前SI>=BX则什么都不做
          CALL INCStr          ;当前SI<BX则排名+1
          UpdateRankNext:
          ADD BX,2
          LOOP UpdateRankLoop
        POP BX
        POP CX
        ADD BX,20
        ADD SI,20
        LOOP UpdateRank4
       

        UpdateRankEnd:
        POP AX
        POP CX
        POP BX
        POP DX
        POP DI
        POP SI
        
        RET
   UpdateRank ENDP
        
; *****子程序UpdateTtlAvr：更新一个学生的总分及平均分*********
;    入口参数: DI:第n个学生数据字符串第一位 SI:第n个成绩组数据
; ************************************************************
     UpdateTtlAvr PROC NEAR
        PUSH DI
        PUSH SI
        PUSH DX
        PUSH AX
        PUSH CX
        PUSH BX
        
        ;检查学号是否为'00'
        PUSH SI
        LEA SI,BlankSTU
        MOV CX,STUIDLength
        CALL CMPString
        CMP CX,0
        JNZ  UpdateTtlAvrStep ;不为'00'则计算
        POP SI
        ;为'00'则置ttl为0
        MOV BX,0
        MOV [SI+60],BX
        JMP UpdateTtlAvrEnd
        
        ;不为'00'则计算
        UpdateTtlAvrStep:       
        ;计算总成绩
        POP SI        
        MOV DX,[SI]            
        ADD DX,[SI+20]
        ADD DX,[SI+40]
        MOV StaticsticsBuff,DX
        MOV DX,0
        PUSH SI                
        LEA SI,StaticsticsBuff
        ADD DI,24
        CALL TranslateDigit
        POP SI
        MOV BX,StaticsticsBuff
        MOV [SI+60],BX
        ;计算平均成绩
        MOV AX,StaticsticsBuff  
        MOV DL,3
        DIV DL
        MOV AH,0
        MOV StaticsticsBuff,AX
        MOV DX,0
        PUSH SI
        LEA SI,StaticsticsBuff
        ADD DI,7
        CALL TranslateDigit
        POP SI
        
        UpdateTtlAvrEnd:
        MOV StaticsticsBuff,0
        POP BX
        POP CX
        POP AX
        POP DX
        POP SI
        POP DI         
        RET
     UpdateTtlAvr ENDP
 
; ***********子程序UpdateScore：更新一个学生成绩**************
;    入口参数: DI:储存单元地址(从cn中起) SI:学号地址
; ************************************************************
    UpdateScore PROC NEAR
       PUSH SI
       PUSH DI
       PUSH CX
       PUSH DX       
       ;检查学号是否为'00'
       PUSH SI
       PUSH DI
       MOV DI,SI
       LEA SI,BlankSTU
       MOV CX,STUIDLength
       CALL CMPString
       CMP CX,0
       JNZ  UpdateScoreStep ;不为零更新
         ;为零置0
         POP DI
         POP SI
         MOV DX,0   
         MOV [DI],DX ;语文成绩
         ADD DI,20   ;数学成绩
         MOV [DI],DX
         ADD DI,20   ;英语成绩
         MOV [DI],DX
         JMP UpdateScoreEnd
         ;不为零更新
         UpdateScoreStep:
         POP DI
         POP SI
         MOV CX,3  ;数字字符串长度
         ADD SI,3  ;语文成绩
         CALL TranslateChar
         ADD SI,7  ;数学成绩
         ADD DI,20
         CALL TranslateChar
         ADD SI,7  ;英语成绩
         ADD DI,20
         CALL TranslateChar
       UpdateScoreEnd:
       POP DX
       POP CX
       POP DI
       POP SI
       RET
    UpdateScore ENDP     
         
; ********子程序TranslateChar：将字符转换成字型数据***********
;    入口参数: DI:储存单元地址 SI:字符地址 CX:字符串长度
; ************************************************************
    TranslateChar PROC NEAR
        PUSH DI
        PUSH SI
        PUSH CX
        PUSH BX
        PUSH AX
        PUSH DX
        MOV AX,0
        TranslateCharStep:
          MOV DX,10
          MOV BX,0
          MOV Bl,[SI]
          SUB Bl,30H
          MUL DX
          ADD AX,BX
          INC SI
          LOOP TranslateCharStep
        MOV [DI],AX
        POP DX
        POP AX
        POP BX
        POP CX
        POP SI
        POP DI
        RET
    TranslateChar ENDP

; *****子程序TranslateDigit：将二进制数据转化为ASCII码********
;    入口参数: DI:字符串地址 SI:数据地址
; ************************************************************
     TranslateDigit PROC NEAR
        PUSH DI
        PUSH SI
        PUSH AX
        PUSH BX
        PUSH DX
        MOV AX,[SI]
        MOV BX,100       ;计算百位
        DIV BX
        ADD AL,30H
        MOV [DI],AL
        INC DI
        MOV AL,DL
        MOV BX,10        ;计算十位
        DIV BL
        ADD AL,30H
        MOV [DI],AL
        INC DI
        ADD AH,30H       ;计算个位
        MOV [DI],AH
        POP DX
        POP BX
        POP AX
        POP SI
        POP DI
        RET
    TranslateDigit ENDP 
     
; ********子程序INCStr：双位数字符自增1(适用于00~19)**********
;    入口参数: DI:双位数字符地址
; ************************************************************
    INCStr PROC NEAR
        PUSH DI
        PUSH DX
        
        MOV DL,[DI+1]
        CMP DL,'9'   ;'09'+1有进位
        JZ INCStrCarry 
        
        INC DL      ;无进位时
        INC DI
        MOV [DI],DL
        JMP INCStrEnd
        INCStrCarry:    ;有进位时
          MOV DX,'01'
          MOV [DI],DX        
        INCStrEnd:
        POP DX
        POP DI
        RET        
    INCStr ENDP      
    
; *************子程序SearchID：找出ID在第几号位***************
;    入口参数: SI:输入的ID数
;    出口参数: DI:该ID地址 DX:该ID是第几个(为10则查找失败)
; ************************************************************
    SearchID PROC NEAR
        PUSH SI
        PUSH DI
        PUSH CX
        PUSH DX         
        ;检'00'，不需要在此子程序中实现
        ;MOV CX,STUIDLength
        ;LEA DI,BlankSTU
        ;CALL CMPString
        ;CMP CX,0
        ;JZ  SearchIDFail        
        LEA DI,STU
        MOV CX,10
        MOV DX,0
        SearchIDStep:
          PUSH CX
          MOV CX,STUIDLength
          CALL CMPString
          CMP CX,0
          JZ SearchIDSucc  ;查找成功
          ADD DI,35
          INC DX
          POP CX
          LOOP SearchIDStep
          JMP SearchIDFail ;查找失败
       SearchIDSucc:       ;查找成功，返回当前DI,DX
        POP CX
        POP CX
        POP CX
        POP SI
        POP SI
        JMP SearchIDEnd
       SearchIDFail:       ;查找失败，返回DI原值，DX为10
        MOV DX,10
        POP CX
        POP CX
        POP DI
        POP SI
        JMP SearchIDEnd
       SearchIDEnd:
       RET
    SearchID ENDP
    
; ***********子程序CMPString：对比两个字符串是否相同**********
;    入口参数: DI:字符串地址 SI:样板地址 CX:字符串长度
;    出口参数：CX:为0则表示匹配，否则表示不匹配
; ************************************************************
   CMPString PROC NEAR
      PUSH DI
      PUSH SI
      PUSH DX
      PUSH BX
      CMPStringStep:
        MOV DL,[SI]
        MOV BL,[DI]
        CMP BL,DL
        JNZ CMPStringEnd
        INC SI
        INC DI
        LOOP CMPStringStep
      CMPStringEnd:
        POP BX
        POP DX
        POP SI
        POP DI
        RET
   CMPString ENDP

; *************子程序OutAll：输出所有学生数据*****************
; ************************************************************
    OutAll PROC NEAR
      PUSH DI
      PUSH SI
      PUSH CX
      LEA DI,MainManuSTUTitle
      CALL OutStr
      CALL MyEnter
      LEA DI,STUStatement
      CALL OutStr
      CALL MyEnter
      LEA DI,STU
      LEA SI,BlankSTU
      MOV CX,10
      OutAllStep:
        PUSH CX
        MOV CX,STUIDLength
        CALL CMPString
        CMP CX,0
        JZ OutAllNext
        CALL OutStr
        CALL MyEnter
        OutAllNext:
          POP CX
          ADD DI,STULength
          LOOP OutAllStep
    POP CX
    POP SI
    POP DI      
    RET
    OutAll ENDP          

; ***********子程序OutMainManu：输出主界面********************
; ************************************************************
OutMainManu PROC NEAR
        PUSH DI
        LEA DI,MainManuTitle
        CALL OutStr
        CALL MyEnter                    
        CALL OutAll   
        LEA DI,MaTag11
        CALL OutStr
        CALL MyEnter
        LEA DI,MaTag12
        CALL OutStr
        CALL MyEnter
        LEA DI,MaTag13
        CALL OutStr
        CALL MyEnter
        LEA DI,MaTag14
        CALL OutStr
        CALL MyEnter
        LEA DI,MaTag15
        CALL OutStr
        CALL MyEnter
        LEA DI,MaTag16
        CALL OutStr
        CALL MyEnter
        LEA DI,MaTag10
        CALL OutStr
        CALL MyEnter
        LEA DI, MaTag2
        CALL OutStr
        POP DI
        RET
OutMainManu ENDP
; ************************************************************
; ***********子程序InStr：输入字符串到SI指向的内存************   
   InStr PROC NEAR
      PUSH AX
      PUSH DX
      PUSH SI 
      MOV DX, SI
      MOV AH, 0AH
      INT 21H 
      MOV AH, 0
      MOV AL, [SI+1]         ; 输入字符串。
      ADD SI, 2
      ADD SI, AX
      MOV [SI], '$'          ; 将字符串最后的回车换为'$'。
      POP SI
      POP DX
      POP AX
      RET
   InStr ENDP
   
; ************************************************************
; **************子程序OutChar：输出AL表示的字符***************  
   OutChar PROC NEAR
      PUSH DX
      MOV DL, AL
      MOV AH, 2
      INT 21H
      POP DX  
      RET
   OutChar ENDP 

; ************************************************************
; **************子程序OutStr：输出DI表示的字符串**************     
   OutStr PROC NEAR
      PUSH AX
      PUSH DX 
      MOV DX, DI       
      MOV AH, 09H
      INT 21H
      POP DX
      POP AX  
      RET
   OutStr ENDP 
   
; ************************************************************
; ***************子程序MyEnter：输出出车换行******************
   MyEnter PROC NEAR                
      PUSH AX
      PUSH DX        
      MOV DL, 0DH
      MOV AH, 2
      INT 21H
      MOV DL, 0AH
      MOV AH, 2
      INT 21H
      POP DX
      POP AX  
      RET
   MyEnter ENDP

; ************************************************************
; ***************子程序CreateData：新建文件*******************
CreateData PROC NEAR
      PUSHA             
      LEA DX,F1
      MOV CX,0
      MOV AH,3CH       ;建立文件
      INT 21H
      MOV SI,AX        ;保存文件号
      LEA DX,STU1
      MOV CX,350
      MOV BX,AX
      MOV AH,40H
      INT 21H
      MOV BX,SI
      MOV AH,3EH       ;关闭
      INT 21H
      POPA
      RET
      CreateData ENDP
end start ; set entry point and stop the assembler.
