;;;;;;;;;;;;;;;2011051742 ���繤�� ¬��ɽ;;;;;;;;;;;;;;;;;;;;;
; ************************************************************
; *********************�û�����ĺ궨��*********************** 
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
; *************************�ļ�����**************************
; ************************Parameters*************************
    F1 DB 'D:\SMS.DAT',0  ; address of the data file
    FILE DW 0                  ;save file number
    HD DB 350 DUP(0)           ;data buffer
    THE_END DB 2 DUP(0)    
    STU     DB 350 DUP (0)

    BlankSTU DB '00'             ;�հ�����ѧ��ѧ��
    STUIDLength DW 2             ;ѧ��ѧ�ų���
    STULength DW 35              ;ѧ�����ݳ���
    StaticsticsBuff DW 0         ;ͳ�����ݸ�������
    InputIDBuff DB 3,0,'000'     ;ID����
    InputScoreBuff DB 4,0,'0000'
    RankSet DW '01','$$'         ;����������
    
; ***********************************************************
; ***ѧ������:ѧ��,��/��/Ӣ�ɼ�/����,�ܳɼ�/����,ƽ���ɼ�****
; ******Student Data:ID, Scores, Ranks, Average scores*******
    STUStatement DB 'ID CN  CR MTH MR EN  ER TTL TR AVR','$'
    ;ѧ��ֻҪΪ00����ʾ�Ҳ������κ�����
    ;if ID='00', then that student would not be involved in the calculation
    ;��һ������ʱ�����մ˴����ݴ����ļ�
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
; ************************ѧ���ɼ���*************************
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
; *********************���������ʾ��Ϣ**********************
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
; *****************����/ɾ��/�޸���ʾ��Ϣ********************
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
; **********************������ʾ��Ϣ*************************
;**********************Message of Rank***********************
    RankTagManu DB '1:Cn Rank 2:Mth Rank 3:En Rank 4:Total Rank','$'
    RankTagTitle DB 'RK ID','$'
    RankTagCN DB 'CN Rank:','$'    
    RankTagMTH DB 'MTH Rank:','$'
    RankTagEN DB 'EN Rank:','$'
    RankTagTTL DB 'Total Rank:','$'
; ***********************************************************
; **********************��ѯ��ʾ��Ϣ*************************
;*******************Message of Inquiry***********************
    QrSTUTag DB 'Please input the data you want to query.','$'
    QrSTUSel DB '1:Cn 2:Mth 3:En 4:TTL','$'
    QrFailTag DB 'Fail List:','$'
    QrFailSel DB '1:Cn 2:Mth 3:En','$'
    QrManuSel DB '1:Subject 2:Fail','$'
; ***********************************************************
; **********************ͳ����ʾ��Ϣ*************************    
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
    SCROLL    0,0,0,24,79,50H             ;���ⴰ�ڣ�Ʒ���,����
; ************************************************************
; *******���ļ��ж�������(�ļ�������ʱ,���趨�½��ļ�)********    
; Read Data from File (If the file does not exsist, create new one)
    MOV DX,OFFSET F1     ;DXָ���ļ��� DX point to the file name
    MOV AL,02H           ;�ļ����� file properties
    MOV AH,3DH                                          
    INT 21H              ;���ļ� open file
    JNC OpenSucc         ;succeed, jump
    CALL CreateData      ;fail, create file and initialize it
    MOV DX,OFFSET F1     ;DXָ���ļ���
    MOV AL,02H           ;�ļ�����
    MOV AH,3DH
    INT 21H   
OpenSucc:
    MOV FILE,AX          ;�����ļ��ţ��������� save file number
    MOV BX,FILE
    LEA DX,STU
    MOV AH,3FH
    MOV CX,350
    INT 21H
    MOV BX,FILE
    MOV AH,3EH       ;�ر� close
    INT 21H

;[MAIN];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Main:
; ************************************************************
; ***************���ݳ�ʼ�����������Ρ��ɼ���*****************    
; ********************Data Initialization*********************
    CALL InitiateScore ;��ʼ���ɼ�������
MainCal:
    CALL InitiateRank  ;��ʼ������
; ************************************************************
; *********��ҳ��ʾ���������û���ѡ����ö�Ӧ���ӳ���*********
; ********************Message of Main Page********************
MainManu:
    MYCLEAN
    CALL OutMainManu        ; �����ҳ�Ĳ�����ʾ    
    MOV AH, 01H
    INT 21H
    CALL MyEnter           ; �û���������Ĳ��� input demand
    
    CMP AL,'0'                
    JZ EXT                ; �û�ѡ��exit,����EXT�����,�˳�����
MMO1:    
    CMP AL,'1'            ; �û�ѡ��insert,ִ�в������
    JNZ MMO2
    CALL Insert
    JMP NXT123
MMO2:    
    CMP AL,'2'            ; �û�ѡ��revise,ִ���޸Ĳ���
    JNZ MMO3
    CALL Revise
    JMP NXT123

MMO3:    
    CMP AL,'3'            ; �û�ѡ��delete,ִ��ɾ������
    JNZ MMO4
    CALL Delete
    JMP NXT123
    
MMO4:    
    CMP AL,'4'            ; �û�ѡ��rank,ִ���������
    JNZ MMO5
    CALL RankSub
    JMP NXT

MMO5:
    CMP AL,'5'            ; �û�ѡ��query,ִ�в�ѯ����
    JNZ MMO6
    CALL QrManu
    JMP NXT
MMO6:    
    CMP AL,'6'            ; �û�ѡ��statistics,ִ��ͳ�Ʋ���
    JNZ MMFail
    CALL SSTManu
    JMP NXT
MMFail:    
    LEA DI,MaTag3         ;������� input error
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
    LEA DI, MaTag4   ;�����л��Ϣ,�˳����� exit
    CALL OutStr 
    CALL MyEnter

;[MAIN END];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; ************************************************************
; ************************�����ļ�����************************ 
; **********************Update File Data**********************
    MOV DX,OFFSET F1     ;DXָ���ļ��� DX point to the file name
    MOV AL,02H           ;�ļ����� file properties
    MOV AH,3DH           ;���ļ� open file
    INT 21H
    MOV FILE,AX            ;�����ļ���save file number
    LEA DX,STU
    MOV CX,350
    MOV BX,AX
    MOV AH,40H
    INT 21H
    MOV BX,SI
    MOV AH,3EH           ;�ر�close
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
; ****************�ӳ���Insert������һ���ɼ�******************
; *************Subroutine Insert: Insert a score**************
; ************************************************************
    Insert PROC NEAR
        PUSH SI
        PUSH DI
        PUSH DX
        PUSH BX
        
        LEA SI,BlankSTU        ;�ҳ���һ��'00' find out the first '00'
        CALL SearchID
        CMP DX,10
        JZ  InsertFail         ;������:����10��IDʱ���ٲ��� fail: stop inserting when there are more than 10 students
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
        ;��'00'
        MOV CX,STUIDLength
        LEA DI,BlankSTU
        CALL CMPString
        CMP CX,0
        JZ  InsertFailTemp         ;������:����'00'ʱ������ fail: do not insert when the input is '00'
        POP DX
        POP DI        
        ;��ʼ����start inserting
        PUSH DI                ;����DI(����������) save DI (used to update data)
        PUSH DX                ;����DX(����������) save DX (used to update data)
        LEA SI,InputIDBuff     ;����ID insert ID
        ADD SI,2
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL                               
        CALL MyEnter           ;����CN(��λ��) insert Chinese score (three-digit)
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
        CALL MyEnter           ;����MTH(��λ��) insert math score (three-digit)
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
        CALL MyEnter           ;����EN(��λ��) insert English score (three-digit)
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
        POP DX                  ;�������� update data
        POP DI                  ;�������� update data
        MOV SI,DI
        LEA DI,cn
        ADD DI,DX
        ADD DI,DX
        CALL UpdateScore
        XCHG DI,SI
        CALL UpdateTtlAvr
        JMP InsertEnd           ;������� finish
        InsertFailTemp:
        POP DX
        POP DI
        InsertFail:             ;����ʧ��:����'00'�򳬹�10��IDʱ���ٲ��� fail
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

; ****************�ӳ���Revise������һ���ɼ�******************
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
        ;��'00'
        MOV CX,STUIDLength
        LEA DI,BlankSTU
        CALL CMPString
        CMP CX,0
        JZ  ReviseFail         ;������:����'00'ʱ������        
                 
        CALL SearchID          ;�ҳ�����������ƥ�����
        CMP DX,10
        JZ  ReviseFail         ;������:�Ҳ���ʱ������
                
        ;��ʼ����
        PUSH DI                ;����DI(����������)
        PUSH DX                ;����DX(����������)
        LEA SI,InputIDBuff     ;����ID
        ADD SI,2
        MOV DL,[SI]
        MOV [DI],DL
        INC DI
        INC SI
        MOV DL,[SI]
        MOV [DI],DL                               
        CALL MyEnter           ;����CN(��λ��)
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
        CALL MyEnter           ;����MTH(��λ��)
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
        CALL MyEnter           ;����EN(��λ��)
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
        POP DX                  ;��������
        POP DI                  ;��������
        MOV SI,DI
        LEA DI,cn
        ADD DI,DX
        ADD DI,DX
        CALL UpdateScore
        XCHG DI,SI
        CALL UpdateTtlAvr
        JMP ReviseEnd           ;������� 
        
        ReviseFail:             ;����ʧ��:����'00'���Ҳ���IDʱ������
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
    
; ****************�ӳ���Delete��ɾ��һ���ɼ�******************
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
        ;��'00'
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
        POP DX                ;��������
        MOV SI,DI             
        LEA DI,cn
        ADD DI,DX
        ADD DI,DX
        CALL UpdateScore
        XCHG DI,SI
        CALL UpdateTtlAvr
        
        JMP DeleteEnd
        ;������:�Ҳ���IDʱ���������Ϣ���������˵�
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
; *******************�ӳ���RankSub������**********************
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
        CALL MyEnter           ; �û���������Ĳ�����
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
         
RS1:        ;���ĳɼ�����
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
            JNZ RankSubNext ;��ƥ�������            
            MOV AL,[SI-7]     ;ƥ�������
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
        
RS2:        ;��ѧ�ɼ�����
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
            JNZ RankSubNextMTH ;;;;��ƥ�������         
            MOV AL,[SI-14]     ;;;;ƥ�������
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

RS3:        ;Ӣ��ɼ�����
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
            JNZ RankSubNextEN ;;;;��ƥ�������         
            MOV AL,[SI-21]     ;;;;ƥ�������
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

RS4:        ;�ֳܷɼ�����
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
            JNZ RankSubNextTTL ;;;;��ƥ�������         
            MOV AL,[SI-28]     ;;;;ƥ�������
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

; *********************�ӳ���QrManu����ѯ*********************
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
        CALL MyEnter           ; �û���������Ĳ�����
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
    
; ************�ӳ���QrFail����ѯ���Ʋ�����ѧ������************
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
        CALL MyEnter           ; �û���������Ĳ�����
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

; ****�ӳ���QrSTU����ѯһ��ѧ�����Ƴɼ�������;�ܳɼ�������****
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
        ;��'00'
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
        CALL MyEnter           ; �û���������Ĳ�����
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
        
        ;������:�Ҳ���IDʱ���������Ϣ���������˵�
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

; *******************�ӳ���SSTManu��ͳ��**********************
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
        CALL MyEnter           ; �û���������Ĳ�����
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

; ***********�ӳ���SSTSort��ͳ�Ƶ��Ƹ�����������**************
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
        CALL MyEnter           ; �û���������Ĳ�����
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

; ***********�ӳ���STTAVR��ͳ�Ƶ���/�ܷ�ƽ����****************
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
        CALL MyEnter           ; �û���������Ĳ�����
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
        ;������:�Ҳ���IDʱ���������Ϣ���������˵�
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

; ***************�ӳ���InitiateRank����ʼ������***************
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

; *�ӳ���InitiateScore����ʼ���ɼ������ݼ��ܷ�/ƽ���ּ���¼��*
; ************************************************************
    InitiateScore PROC NEAR
        PUSH SI
        PUSH DI
        PUSH CX
        LEA SI,STU
        LEA DI,cn
        MOV CX,10
        InitiateScoreStep:
          CALL UpdateScore         ;���³ɼ���
          XCHG SI,DI
          CALL UpdateTtlAvr        ;�����ܷ���ƽ����
          XCHG SI,DI
          ADD SI,35
          ADD DI,2
          LOOP InitiateScoreStep
        POP CX
        POP DI
        POP SI
        RET
    InitiateScore ENDP

; *******�ӳ���UpdateRank������һ��ѧ�����Ƽ��ܷ�����*********
;    ��ڲ���: DI:��n��ѧ�������ַ�����һλ SI:��n���ɼ�������
; ************************************************************
   UpdateRank PROC NEAR
        PUSH SI
        PUSH DI
        PUSH DX
        PUSH BX
        PUSH CX
        PUSH AX
        
         ;���ѧ���Ƿ�Ϊ'00'
        PUSH SI             ;;;;;;
        LEA SI,BlankSTU
        MOV CX,STUIDLength
        CALL CMPString
        CMP CX,0
        JNZ  UpdateRankStep ;��Ϊ'00'�����
        
        POP SI
        MOV DX,'00'         ;Ϊ'00'��������������Ϊ'00'
        MOV [DI+7],DX
        MOV [DI+14],DX
        MOV [DI+21],DX
        MOV [DI+28],DX

        JMP UpdateRankEnd
        
        UpdateRankStep:     ;��Ϊ'00'�����
        POP SI
        MOV DX,'10'         ;����Ϊ'01'
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
          JAE UpdateRankNext ;��ǰSI>=BX��ʲô������
          CALL INCStr          ;��ǰSI<BX������+1
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
        
; *****�ӳ���UpdateTtlAvr������һ��ѧ�����ּܷ�ƽ����*********
;    ��ڲ���: DI:��n��ѧ�������ַ�����һλ SI:��n���ɼ�������
; ************************************************************
     UpdateTtlAvr PROC NEAR
        PUSH DI
        PUSH SI
        PUSH DX
        PUSH AX
        PUSH CX
        PUSH BX
        
        ;���ѧ���Ƿ�Ϊ'00'
        PUSH SI
        LEA SI,BlankSTU
        MOV CX,STUIDLength
        CALL CMPString
        CMP CX,0
        JNZ  UpdateTtlAvrStep ;��Ϊ'00'�����
        POP SI
        ;Ϊ'00'����ttlΪ0
        MOV BX,0
        MOV [SI+60],BX
        JMP UpdateTtlAvrEnd
        
        ;��Ϊ'00'�����
        UpdateTtlAvrStep:       
        ;�����ܳɼ�
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
        ;����ƽ���ɼ�
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
 
; ***********�ӳ���UpdateScore������һ��ѧ���ɼ�**************
;    ��ڲ���: DI:���浥Ԫ��ַ(��cn����) SI:ѧ�ŵ�ַ
; ************************************************************
    UpdateScore PROC NEAR
       PUSH SI
       PUSH DI
       PUSH CX
       PUSH DX       
       ;���ѧ���Ƿ�Ϊ'00'
       PUSH SI
       PUSH DI
       MOV DI,SI
       LEA SI,BlankSTU
       MOV CX,STUIDLength
       CALL CMPString
       CMP CX,0
       JNZ  UpdateScoreStep ;��Ϊ�����
         ;Ϊ����0
         POP DI
         POP SI
         MOV DX,0   
         MOV [DI],DX ;���ĳɼ�
         ADD DI,20   ;��ѧ�ɼ�
         MOV [DI],DX
         ADD DI,20   ;Ӣ��ɼ�
         MOV [DI],DX
         JMP UpdateScoreEnd
         ;��Ϊ�����
         UpdateScoreStep:
         POP DI
         POP SI
         MOV CX,3  ;�����ַ�������
         ADD SI,3  ;���ĳɼ�
         CALL TranslateChar
         ADD SI,7  ;��ѧ�ɼ�
         ADD DI,20
         CALL TranslateChar
         ADD SI,7  ;Ӣ��ɼ�
         ADD DI,20
         CALL TranslateChar
       UpdateScoreEnd:
       POP DX
       POP CX
       POP DI
       POP SI
       RET
    UpdateScore ENDP     
         
; ********�ӳ���TranslateChar�����ַ�ת������������***********
;    ��ڲ���: DI:���浥Ԫ��ַ SI:�ַ���ַ CX:�ַ�������
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

; *****�ӳ���TranslateDigit��������������ת��ΪASCII��********
;    ��ڲ���: DI:�ַ�����ַ SI:���ݵ�ַ
; ************************************************************
     TranslateDigit PROC NEAR
        PUSH DI
        PUSH SI
        PUSH AX
        PUSH BX
        PUSH DX
        MOV AX,[SI]
        MOV BX,100       ;�����λ
        DIV BX
        ADD AL,30H
        MOV [DI],AL
        INC DI
        MOV AL,DL
        MOV BX,10        ;����ʮλ
        DIV BL
        ADD AL,30H
        MOV [DI],AL
        INC DI
        ADD AH,30H       ;�����λ
        MOV [DI],AH
        POP DX
        POP BX
        POP AX
        POP SI
        POP DI
        RET
    TranslateDigit ENDP 
     
; ********�ӳ���INCStr��˫λ���ַ�����1(������00~19)**********
;    ��ڲ���: DI:˫λ���ַ���ַ
; ************************************************************
    INCStr PROC NEAR
        PUSH DI
        PUSH DX
        
        MOV DL,[DI+1]
        CMP DL,'9'   ;'09'+1�н�λ
        JZ INCStrCarry 
        
        INC DL      ;�޽�λʱ
        INC DI
        MOV [DI],DL
        JMP INCStrEnd
        INCStrCarry:    ;�н�λʱ
          MOV DX,'01'
          MOV [DI],DX        
        INCStrEnd:
        POP DX
        POP DI
        RET        
    INCStr ENDP      
    
; *************�ӳ���SearchID���ҳ�ID�ڵڼ���λ***************
;    ��ڲ���: SI:�����ID��
;    ���ڲ���: DI:��ID��ַ DX:��ID�ǵڼ���(Ϊ10�����ʧ��)
; ************************************************************
    SearchID PROC NEAR
        PUSH SI
        PUSH DI
        PUSH CX
        PUSH DX         
        ;��'00'������Ҫ�ڴ��ӳ�����ʵ��
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
          JZ SearchIDSucc  ;���ҳɹ�
          ADD DI,35
          INC DX
          POP CX
          LOOP SearchIDStep
          JMP SearchIDFail ;����ʧ��
       SearchIDSucc:       ;���ҳɹ������ص�ǰDI,DX
        POP CX
        POP CX
        POP CX
        POP SI
        POP SI
        JMP SearchIDEnd
       SearchIDFail:       ;����ʧ�ܣ�����DIԭֵ��DXΪ10
        MOV DX,10
        POP CX
        POP CX
        POP DI
        POP SI
        JMP SearchIDEnd
       SearchIDEnd:
       RET
    SearchID ENDP
    
; ***********�ӳ���CMPString���Ա������ַ����Ƿ���ͬ**********
;    ��ڲ���: DI:�ַ�����ַ SI:�����ַ CX:�ַ�������
;    ���ڲ�����CX:Ϊ0���ʾƥ�䣬�����ʾ��ƥ��
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

; *************�ӳ���OutAll���������ѧ������*****************
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

; ***********�ӳ���OutMainManu�����������********************
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
; ***********�ӳ���InStr�������ַ�����SIָ����ڴ�************   
   InStr PROC NEAR
      PUSH AX
      PUSH DX
      PUSH SI 
      MOV DX, SI
      MOV AH, 0AH
      INT 21H 
      MOV AH, 0
      MOV AL, [SI+1]         ; �����ַ�����
      ADD SI, 2
      ADD SI, AX
      MOV [SI], '$'          ; ���ַ������Ļس���Ϊ'$'��
      POP SI
      POP DX
      POP AX
      RET
   InStr ENDP
   
; ************************************************************
; **************�ӳ���OutChar�����AL��ʾ���ַ�***************  
   OutChar PROC NEAR
      PUSH DX
      MOV DL, AL
      MOV AH, 2
      INT 21H
      POP DX  
      RET
   OutChar ENDP 

; ************************************************************
; **************�ӳ���OutStr�����DI��ʾ���ַ���**************     
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
; ***************�ӳ���MyEnter�������������******************
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
; ***************�ӳ���CreateData���½��ļ�*******************
CreateData PROC NEAR
      PUSHA             
      LEA DX,F1
      MOV CX,0
      MOV AH,3CH       ;�����ļ�
      INT 21H
      MOV SI,AX        ;�����ļ���
      LEA DX,STU1
      MOV CX,350
      MOV BX,AX
      MOV AH,40H
      INT 21H
      MOV BX,SI
      MOV AH,3EH       ;�ر�
      INT 21H
      POPA
      RET
      CreateData ENDP
end start ; set entry point and stop the assembler.
