         GBLA  &OPSYS                                                   00000010
PMRT     TXLIFE70 'PMRDBL     POLICY MASTER RECORD DE-BLOCKER ROUTINE',X00000030
               SYSTEM=00,VERSION=00,LEVEL=00,TYPE=PERM,                X00000040
               DATE=03/15/77                                            00000050
PMRDBL   TXBEGIN PMRDBL                                                 00000060
*********************************************************************** 00000070
*                                                                       00000080
*   THE PURPOSE OF THIS ROUTINE IS TO ALLOW COBOL PROGRAMS TO EXTRACT   00000090
*   INFORMATION FROM THE POLICY MASTER RECORD.                          00000100
*                                                                       00000110
*  PARM 1 (REG 2) ADDRESS OF AN AREA CONTAINING THE POLICY              00000120
*                 MASTER RECORD.                                        00000130
*                                                                       00000140
*  PARM 2 (REG 3) ADDRESS OF AN AREA TO RECEIVE THE REQUESTED TRAILER.  00000150
*                 THIS IS AN AREA LARGE ENOUGH TO HOLD THE LARGEST      00000160
*                 POSSIBLE TRAILER.                                     00000170
*                                                                       00000180
*  PARM 3 (REG 4) THE ADDRESS OF A FOUR BYTE WORK AREA FOR THIS PROGRAM 00000190
*                                                                       00000200
*  PARM 4 (REG 5) THE ADDRESS OF A SIX BYTE AREA FOR REQUESTS           00000210
*                 AND RETURN CODE.      RTPP*X   WHERE R=RECORD         00000220
*                                                      T=TYPE           00000230
*                                                      PP=PHASE         00000240
*                                                      *=NEXT INDICATOR 00000250
*                                                      X=RETURN CODE    00000260
*                                                                       00000270
* DJNA 1997 Y2K                                                         00000271
*********************************************************************** 00000280
         EJECT                                                          00000290
*********************************************************************** 00000300
         SPACE 1                                                        00000310
*        RETURN CODES AND OTHER EQUATES                                 00000320
         SPACE 1                                                        00000330
NOTFND   EQU   C'0'                RETURN CODE NOT FOUND                00000340
FOUND    EQU   C'1'                RETURN CODE FOUND                    00000350
         SPACE 1                                                        00000360
YES      EQU   C'*'                                                     00000370
         SPACE 1                                                        00000380
*********************************************************************** 00000390
         EJECT                                                          00000400
         USING PMRDSECT,R2         SET R2 TO PMR DSECT                  00000410
         USING INPARM,R5           SET R5 TO INPARM DSECT               00000420
         USING TRDSECT,R6                                               00000430
         LM    R2,R5,0(R1)         LOAD CALLERS PARMS                   00000440
         L     R10,0(,R4)          SET FINDREG FOR NEXT                 00000450
         LA    R14,RETURN          SET EXIT                             00000460
         MVC   OPI,REC             MOVE PARMS TO MY WORK AREA           00000470
         CLI   REC,C'Z'            IS THIS A REQUEST FOR Z TRAILER      00000480
         BE    PROCZTRL            YES.. GO PROCESS Z TRAILER           00000490
         CLI   NXT,YES             NO... IS THIS FOR A NEXT             00000500
         BNE   CONT                NO... CONTINUE                       00000510
         LTR   R10,R10             YES.. IS FINDREG BLANK               00000520
         BZ    ABORT               YES.. ABORT JOB                      00000530
CONT     EQU   *                                                        00000540
         LA    R6,RECTAB           SET ADDR FOR SEARCH OF VALID REC COD 00000550
         LA    R7,RECTABNO         SET BRANCH NO.                       00000560
TRYNEXT  EQU   *                                                        00000570
         CLC   REC,RTAB            IS THIS RECORD CODE IN TABLE         00000580
         BE    FNDRCODE            YES..                                00000590
         LA    R6,L'RECTAB(,R6)    NO... INDEX TO NEXT REC CODE IN TAB  00000600
         BCT   R7,TRYNEXT          CONTINUE THRU TABLE                  00000610
         B     ABORT               CANT FIND IT IN TABLE                00000620
FNDRCODE EQU   *                                                        00000630
         CLC   TYP(L'TYP+L'PHS),BLANKS REQUEST FOR RECORD               00000640
         BE    RECORD              YES.. GO PROCESS IT                  00000650
         CLC   PHS,BLANKS          IS REQUEST FOR RECTYP                00000660
         BE    RECTYPE             YES.. GO PROCESS IT                  00000670
         CLC   TYP,BLANKS          REQUEST FOR RECTYPH                  00000680
         BNE   RECTYPH             YES.. GO PROCESS IT                  00000690
         CLI   PPERMIT,YES         MUST BE FOR RECPHS OR RECPH2         00000700
         BE    RECPHS              RECPHS -- GO PROCESS IT              00000710
         CLI   P2PERMIT,YES        PH2 PERMITTED?                       00000720
         BE    RECPH2              YES.. GO PROCESS IT                  00000730
         B     ABORT               NO... INVALID REQUEST ABORT JOB      00000740
         EJECT                                                          00000750
*********************************************************************** 00000760
RECORD   EQU   *                   REQUEST IS FOR RECORD                00000770
*********************************************************************** 00000780
         SPACE 1                                                        00000790
         CLI   NXT,YES             WAS NEXT REQUESTED                   00000800
         BE    RNXT                YES                                  00000810
         TXFINDT OPI,NEXT=RNXT                                          00000820
         SPACE 1                                                        00000830
*********************************************************************** 00000840
RECTYPE  EQU   *                                                        00000850
*********************************************************************** 00000860
         SPACE 1                                                        00000870
         CLI   TPERMIT,YES         IS THIS TYPE PERMITTED FOR REC       00000880
         BNE   ABORT               NO... ABORT JOB                      00000890
         CLI   NXT,YES             WAS NEXT RECORD REQUESTED            00000900
         BE    RTNXT               YES                                  00000910
         TXFINDT OPI,SRCHARG=RECTYP,NEXT=RTNXT                          00000920
         SPACE 1                                                        00000930
*********************************************************************** 00000940
RECPHS   EQU   *                                                        00000950
*********************************************************************** 00000960
         SPACE 1                                                        00000970
         MVC   OPI+1(2),OPI+2      SET OPI FOR PROPER SET UP            00000980
         CLI   NXT,YES             WAS NEXT REQUESTED                   00000990
         BE    RPNXT               YES                                  00001000
         TXFINDT OPI,SRCHARG=RECPHS,NEXT=RPNXT                          00001010
         SPACE 1                                                        00001020
*********************************************************************** 00001030
RECPH2   EQU   *                                                        00001040
*********************************************************************** 00001050
         SPACE 1                                                        00001060
         MVC   OPI+1(2),OPI+2      SET OPI FOR PROPER SET UP            00001070
         CLI   NXT,YES             WAS NEXT REQUESTED                   00001080
         BE    RP2NXT              YES                                  00001090
         TXFINDT OPI,SRCHARG=RECPH2,NEXT=RP2NXT                         00001100
         SPACE 1                                                        00001110
*********************************************************************** 00001120
RECTYPH  EQU   *                                                        00001130
*********************************************************************** 00001140
         SPACE 1                                                        00001150
         CLI   TPERMIT,YES         IS TYPE PERMITTED FOR THIS REC       00001160
         BNE   ABORT               NO... ABORT JOB                      00001170
         CLI   P2PERMIT,YES        IS PHASE PERMITTED FOR THIS REC      00001180
         BNE   ABORT               NO... ABORT JOB                      00001190
         CLI   NXT,YES             WAS NEXT REQUESTED                   00001200
         BE    RTPNXT              YES                                  00001210
         TXFINDT OPI,SRCHARG=RECTYPH,NEXT=RTPNXT                        00001220
         EJECT                                                          00001230
RETURN   EQU   *                                                        00001240
         B     EXIT0               REQUEST NOT FOUND                    00001250
         MVI   RETCD,FOUND         REQUEST FOUND                        00001260
         SR    R6,R6               CLEAR REG                            00001270
         IC    R6,1(,R10)          SET IN LENGTH                        00001280
         BCTR  R6,0                DECREASE BY 1                        00001290
         LR    R7,R3               SET REG FOR MOVE                     00001300
         EX    R6,MOVE             DO MOVE                              00001310
         SPACE 1                                                        00001320
EXIT     EQU   *                                                        00001330
         ST    R10,0(,R4)          SET FINDREG IN CALLER                00001340
         TXEND ,                   RETURN TO CALLER                     00001350
         SPACE 1                                                        00001360
EXIT0    EQU   *                   REQUEST NOT FOUND                    00001370
         MVI   RETCD,NOTFND        SET NOT FOUND                        00001380
         SR    R10,R10             CLEAR FINDREG                        00001390
         B     EXIT                RETURN                               00001400
         EJECT                                                          00001410
*********************************************************************** 00001420
PROCZTRL EQU   *                   PROCESSING Z TRAILER                 00001430
*********************************************************************** 00001440
         SPACE 1                                                        00001450
         DROP  R6                                                       00001460
         BAL   R14,RECORD          GO GET RECORD                        00001470
         B     EXIT0               NOT FOUND                            00001480
         SPACE 1                                                        00001490
*********************************************************************** 00001500
*        BLANK CALLERS WORK AREA                                        00001510
*********************************************************************** 00001520
         SPACE 1                                                        00001530
         L     R6,BLKNO            SET THE NUMBER OF BYTES TO BLANK     00001540
         LR    R1,R3               SET REG OF WORK AREA TO BLANK        00001550
BLANKER  EQU   *                                                        00001560
         MVC   0(1,R1),BLANKS      MOVE IN BLANK                        00001570
         LA    R1,1(,R1)           INDEX TO NEXT BYTE TO BLANK          00001580
         BCT   R6,BLANKER          CONTINUE TO BLANK WORK AREA TILL 0   00001590
         SPACE 1                                                        00001600
         USING ZTRLDSEC,R3         FOUND                                00001610
         MVI   RETCD,FOUND         SET RETURN CODE                      00001620
         MVC   ZCONTRL,0(R10)      MOVE INFO                            00001630
         LA    R10,L'ZCONTRL(,R10)    SET TO NEW INFO                   00001640
         LA    R7,ZTRLN1           SET FOR MOVE TO CALLER               00001650
         ZAP   CONTROL,1(1,R3)     GET CONTROL INFO                     00001660
         BAL   R14,MOVEZ           GO DO MOVE OF Z TRAILER              00001670
         LA    R7,ZTRLN2                                                00001680
         ZAP   CONTROL,2(1,R3)                                          00001690
         BAL   R14,MOVEZ                                                00001700
         LA    R7,ZTRLN3                                                00001710
         ZAP   CONTROL,3(1,R3)                                          00001720
         BAL   R14,MOVEZ                                                00001730
         SR    R10,R10             CLEAR FINDREG NO MORE INFO           00001740
         B     EXIT                COMPLETED                            00001750
         EJECT                                                          00001760
MOVEZ    TXENTERS                                                       00001770
AGAIN    EQU   *                                                        00001780
         CP    CONTROL,=P'0'       CONTROL INFO ZERO                    00001790
         BE    ZEND                YES.. RETURN                         00001800
         SR    R6,R6               NO... CLEAR REG                      00001810
         IC    R6,1(,R10)          GET LENGTH                           00001820
         BCTR  R6,0                DECREASE BY 1                        00001830
         EX    R6,MOVE             DO MOVE                              00001840
         LA    R10,1(R6,R10)       SET TO NEXT INFO                     00001850
         SP    CONTROL,=P'1'       DECREASE CONTROL                     00001860
         LA    R7,L'ZTRLN1(,R7)    SET TO NEXT LINE OF CALLER           00001870
         B     AGAIN               GO DO IT AGAIN                       00001880
ZEND     EQU   *                   WE ARE DONE                          00001890
         TXEXITS ,                 RETURN                               00001900
         SPACE 1                                                        00001910
MOVE     MVC   0(0,R7),0(R10)      THIS IS AN EXECUTE INSTRUCTION       00001911
         EJECT                                                          00001930
         TXVDATA                                                        00001940
FIRST    DC    C'DATA STARTS'                                           00001941
BLKNO    DC    F'800'                                                   00001950
OPI      DS    CL4                                                      00001960
BLANKS   DC    CL10' '                                                  00001970
CONTROL  DS    PL1                                                      00001980
ABORT    DC    H'0'                                                     00001990
RECTAB   DC    CL4'A * '                                                00002000
         DC    CL4'B * '                                                00002010
         DC    CL4'C*  '                                                00002020
         DC    CL4'D*  '                                                00002030
         DC    CL4'E* *'                                                00002040
         DC    CL4'F* *'                                                00002050
         DC    CL4'G*  '                                                00002060
         DC    CL4'M* *'                                                00002070
         DC    CL4'N   '                                                00002080
         DC    CL4'P   '                                                00002090
         DC    CL4'Q*  '                                                00002100
         DC    CL4'R * '                                                00002110
         DC    CL4'S*  '                                                00002120
         DC    CL4'T * '                                                00002130
         DC    CL4'U* *'                                                00002140
         DC    CL4'V * '                                                00002150
         DC    CL4'W * '                                                00002160
RECTABNO EQU   (*-RECTAB)/L'RECTAB                                      00002170
PMRDSECT DSECT                                                          00002180
         DS    CL252                                             CSOY2K 00002181
PMFIRST  EQU   *                                                        00002200
         SPACE 1                                                        00002210
INPARM   DSECT                                                          00002220
REC      DS    CL1                                                      00002230
TYP      DS    CL1                                                      00002240
PHS      DS    CL2                                                      00002250
NXT      DS    CL1                                                      00002260
RETCD    DS    CL1                                                      00002270
         SPACE 1                                                        00002280
TRDSECT  DSECT                                                          00002290
RTAB     DS    CL1                                                      00002300
TPERMIT  DS    CL1                                                      00002310
PPERMIT  DS    CL1                                                      00002320
P2PERMIT DS    CL1                                                      00002330
         SPACE 1                                                        00002340
ZTRLDSEC DSECT                                                          00002350
ZCONTRL  DS    CL6                                                      00002360
ZTRLN1   DS    4CL37                                                    00002370
ZTRLN2   DS    9CL37                                                    00002380
ZTRLN3   DS    9CL37                                                    00002390
LAST     DC    C'DATA ENDS'                                             00002391
         END                                                            00002400
