       IDENTIFICATION DIVISION.                                         00000100
       PROGRAM-ID.    FNB076.                                           00000200
                                                                        00000300
      ***************************************************************** 00000400
061402*  THIS PROGRAM WAS MODIFIED FROM THE MAINFRAME FOR THE LAN AND   00000500
061402*  PROCESSES CSO AP CHECKS (FORM 0031).  IT FORMATS A PRINT LINE  00000600
061402*  AND WRITES IT TO SYS007 (CHECK COPY).  IT THEN SCANS THE       00000700
061402*  INVOICE MESSAGE FOR "CC" (EXTRA CHECK COPY) AND WRITES THESE   00000800
061402*  TO SYS009.                                                     00000900
      ***************************************************************** 00001000
      *          P R O G R A M   M O D I F I C A T I O N S            * 00001100
      ***************************************************************** 00001200
      * CR/IR           DATE       NAME DESCRIPTION                   * 00001300
      * --------------  ---------- ---- ----------------------------- * 00001400
      * CR970170050     06/06/1997 DANA NEW TO PRODUCTION             * 00001500
      * CR2000030100009 04/01/2000 DJNA DRAFT NUMBER EXPANSION
061402* CR2002031800003 06/14/2002 SMVA CHANGED MICR0031 COPYBOOK TO
061402*                           FNMICR - USED FOR ALL FN AP CHECKS
061402*                           ADDED NEWPAGE CC TO PRINT RECORD
061402*                           REMOVED SYS008 ACH LOGIC -
061402*                           CURRENTLY DOES NOT APPLY
061402*                           MOVED PROGRAM TO LAN
082002*                 08/20/2002 SMVA REMOVE FORM '0031' COND AND 
082002*                          ALLOW ALL CHECKS TO PROCESS            00001500
092402*                 09/24/2002 SMVA CHANGE CHECK AMOUNT CONDITIONAL
092402*                          VALUE FROM ZERO TO 0.00 W/LEAD SPACES 
102506* IR2006102400001 10/25/2006 AJRA PRINT INVOICE ON VOIDS WHEN
102506*                           NUMBER OF INVOICES > 15
      ***************************************************************** 00001600
                                                                        00001700
       ENVIRONMENT DIVISION.                                            00001800
       INPUT-OUTPUT SECTION.                                            00001900
       FILE-CONTROL.                                                    00002000
                                                                        00002100
           SELECT MICR-DRAFT-FILE                                       00002200
               ASSIGN TO MICRDRFT
061402         ORGANIZATION IS LINE SEQUENTIAL                          00002300
061402*        ORGANIZATION IS INDEXED                                  00002400
061402*        ACCESS IS SEQUENTIAL                                     00002500
061402*        RECORD KEY IS MICR-KEY                                   00002600
               FILE STATUS IS MICRDRFT-STATUS                           00002700
                              MICRDRFT-STATUS2.                         00002710
                                                                        00002800
           SELECT CSO-COPY                                              00002900
               ASSIGN TO SYS007
061402         ORGANIZATION IS LINE SEQUENTIAL.                         00003000
                                                                        00003100
061402*    SELECT ACH-COPY                                              00003200
061402*        ASSIGN TO SYS008.                                        00003300
                                                                        00003400
           SELECT CC-COPY                                               00003500
               ASSIGN TO SYS009
061402         ORGANIZATION IS LINE SEQUENTIAL.                         00003600
                                                                        00003700
                                                                        00003800
       DATA DIVISION.                                                   00003900
       FILE SECTION.                                                    00004000
                                                                        00004100
       FD  MICR-DRAFT-FILE.                                             00004200
       01  FILLER            PIC X(3000).                               00004410
       01  MICR-RECORD.                                                 00004500
061402     COPY FNMICR.
                                                                        00004700
       FD  CSO-COPY                                                     00004800
           LABEL RECORDS ARE STANDARD                                   00004900
           RECORDING MODE IS F                                          00005000
           RECORD CONTAINS 2848 CHARACTERS                              00005100
           BLOCK CONTAINS 0 RECORDS.                                    00005200
       01  CSO-RECORD            PIC X(2848).                           00005300
                                                                        00005400
061402*FD  ACH-COPY                                                     00005500
061402*    LABEL RECORDS ARE STANDARD                                   00005600
061402*    RECORDING MODE IS F                                          00005700
061402*    RECORD CONTAINS 2848 CHARACTERS                              00005800
061402*    BLOCK CONTAINS 0 RECORDS.                                    00005900
061402*01  ACH-RECORD            PIC X(2848).                           00006000
                                                                        00006100
       FD  CC-COPY                                                      00006200
           LABEL RECORDS ARE STANDARD                                   00006300
           RECORDING MODE IS F                                          00006400
           RECORD CONTAINS 2848 CHARACTERS                              00006500
           BLOCK CONTAINS 0 RECORDS.                                    00006600
       01  CC-RECORD             PIC X(2848).                           00006700
                                                                        00006800
                                                                        00006900
       WORKING-STORAGE SECTION.                                         00007000
                                                                        00007100
       01  FILLER            COMP-3.                                    00007200
           05  SUB1          PIC S9(3)  VALUE +0.                       00007300
           05  SUB2          PIC S9(3)  VALUE +0.                       00007400
           05  CC-COUNT      PIC S9(3)  VALUE +0.                       00007500
                                                                        00007600
       01  FILLER.                                                      00007700
           05  DUMP                PIC X      VALUE SPACE.              00007800
           05  S0C7 REDEFINES DUMP PIC S9     COMP-3.                   00007900
           05  MICRDRFT-STATUS     PIC XX     VALUE ZERO.               00008000
               88  EOF                        VALUE '10'.               00008100
           05  MICRDRFT-STATUS2.                                        00008110
               10  MS-VSAM-RC      PIC S9(4)  BINARY.                   00008120
               10  MS-FUNCTION     PIC S9(4)  BINARY.                   00008121
               10  MS-FEEDBACK     PIC S9(4)  BINARY.                   00008130
           05  WS-VOID-MSG         PIC X(40)                            00008200
               VALUE 'VOID  VOID  VOID  VOID  VOID  VOID  VOID'.        00008300
                                                                        00008400
       01  PRINT-RECORD.
061402     05  PR-CC                PIC X(01)  VALUE '1'.               00008500
           05  PR-CHECK-NO          PIC X(10).                          00008600
           05  PR-CHECK-AMT         PIC ***,***,***.99.                 00008700
           05  PR-CHECK-AMT-2       PIC X(77).                          00008800
           05  PR-CHECK-AMT-2A      PIC X(77).                          00008900
           05  PR-CHECK-DATE        PIC X(10).                          00009000
061402*    05  PR-ENTITY            PIC X(4).
061402     05  FILLER               PIC X(04).                          00009100
CSO        05  PR-NAME              PIC X(46).                          00009110
           05  PR-ADDRESS-1         PIC X(50).                          00009200
           05  PR-ADDRESS-2         PIC X(50).                          00009300
           05  PR-ADDRESS-3         PIC X(50).                          00009400
           05  PR-ADDRESS-4         PIC X(50).                          00009500
           05  PR-VENDOR-ID         PIC X(10).                          00009600
           05  PR-INVOICE-LINE      OCCURS 30 TIMES                     00009700
                                    PIC X(80).                          00009800
                                                                        00009900
       01  WS-WORKAREA.                                                 00010000
           05  WS-TEXT-BYTE OCCURS 80 TIMES                             00010100
                            INDEXED BY INDX                             00010200
                            PICTURE IS X.                               00010300
                                                                        00010400
       01  ISD034-PARMS.                                                00010500
           05  ISD034-AMT         PIC S9(13)V99    COMP-3.              00010600
           05  ISD034-SCRIPT-OPT  PIC X.                                00010700
           05  ISD034-LINE-1      PIC X(77).                            00010800
           05  ISD034-LINE-2      PIC X(77).                            00010900
           05  ISD034-LINE-3      PIC X(77).                            00011000
                                                                        00011100
       01  WS-INVOICE-LINE        VALUE SPACE.                          00011200
           05  WS-INV-DATE        PIC X(10).                            00011300
           05  FILLER             PIC X(5).                             00011400
           05  WS-INV-NO          PIC X(16).                            00011500
           05  FILLER             PIC X(3).                             00011600
           05  WS-INV-TYPE        PIC X(2).                             00011700
           05  FILLER             PIC X(25).                            00011800
           05  WS-INV-AMT         PIC X(14).                            00011900
                                                                        00012000
                                                                        00012100
      *                                                                 00012200
       PROCEDURE DIVISION.                                              00012300
      *                                                                 00012400
           PERFORM 0000-START THRU 0000-EXIT                            00012500
                                                                        00012600
           PERFORM 1000-PRINT-CHECKS THRU 1000-EXIT                     00012700
               UNTIL EOF                                                00012800
                                                                        00012900
           PERFORM 9000-END THRU 9000-EXIT                              00013000
                                                                        00013100
061402     GOBACK.                                                      00013200
                                                                        00013300
      *                                                                 00013600
       1000-PRINT-CHECKS.                                               00013700
      *                                                                 00013800
           READ MICR-DRAFT-FILE
061402         AT END SET EOF TO TRUE
061402     END-READ
                                                                        00013900
           IF EOF                                                       00013901
               GO TO 1000-EXIT                                          00013902
061402     END-IF
                                                                        00013903
           IF MICRDRFT-STATUS NOT = '00'                                00013904
               DISPLAY 'READ ERROR ON MICRDRFT'                         00013905
061402     END-IF                                                       00013907
                                                                        00014200
082002*    IF FNMICR-FORM = '0031'                                      00014300
           PERFORM 2000-FORMAT-PRINT-REC THRU 2000-EXIT                 00014400
082002*    ELSE                                                         00014500
082002*        GO TO 1000-EXIT
082002*    END-IF                                                       00014600
                                                                        00014700
061402*    IF FNMICR-PAY-ENTITY = 'ACHP'                                00014800
061402*        WRITE ACH-RECORD FROM PRINT-RECORD                       00014900
061402*    ELSE
061402     MOVE '1'         TO PR-CC                                    00015000
           WRITE CSO-RECORD FROM PRINT-RECORD                           00015100
                                                                        00015200
           PERFORM 3000-WRITE-CC THRU 3000-EXIT
                                                                        00015300
           .                                                            00015400
       1000-EXIT.                                                       00015500
           EXIT.                                                        00015600
                                                                        00015700
                                                                        00015800
      *                                                                 00015900
       2000-FORMAT-PRINT-REC.                                           00016000
      *                                                                 00016100
092402     IF FNMICR-AMOUNT-PAID = '          0.00'                     00016200
               PERFORM 2100-FORMAT-VOID THRU 2100-EXIT                  00016300
               GO TO 2000-EXIT
           END-IF                                                       00016400
                                                                        00016500
           MOVE SPACES                TO PRINT-RECORD                   00016600
           MOVE FNMICR-DRAFT-NO       TO PR-CHECK-NO                    00016700
           MOVE FNMICR-AMOUNT-PAID    TO PR-CHECK-AMT                   00016800
           MOVE FNMICR-AMOUNT-PAID    TO ISD034-AMT                     00016900
           CALL 'ISD034' USING ISD034-PARMS                             00017000
           MOVE ISD034-LINE-1         TO PR-CHECK-AMT-2                 00017100
           MOVE ISD034-LINE-2         TO PR-CHECK-AMT-2A                00017200
           MOVE FNMICR-CHECK-DATE     TO PR-CHECK-DATE                  00017300
061402*    MOVE FNMICR-PAY-ENTITY     TO PR-ENTITY                      00017400
           MOVE FNMICR-PAYEE-NAME     TO PR-NAME                        00017410
           MOVE FNMICR-PAYEE-ADDRESS1 TO PR-ADDRESS-1                   00017500
           MOVE FNMICR-PAYEE-ADDRESS2 TO PR-ADDRESS-2                   00017600
           MOVE FNMICR-PAYEE-ADDRESS3 TO PR-ADDRESS-3                   00017700
           MOVE FNMICR-PAYEE-ADDRESS4 TO PR-ADDRESS-4                   00017800
           MOVE FNMICR-VENDOR-NO      TO PR-VENDOR-ID                   00017900
           MOVE ZERO                  TO SUB2                           00018000
           PERFORM 2200-FILL-INVOICE THRU 2200-EXIT                     00018100
               VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > +15

           .                                                            00018300
       2000-EXIT.                                                       00018400
           EXIT.                                                        00018500
                                                                        00018600
      *                                                                 00018900
       2100-FORMAT-VOID.                                                00019000
      *                                                                 00019100
           MOVE SPACES         TO  PRINT-RECORD                         00019200
           MOVE FNMICR-DRAFT-NO TO  PR-CHECK-NO                         00019210
           MOVE ZEROS          TO  PR-CHECK-AMT                         00019300
           MOVE WS-VOID-MSG    TO  PR-CHECK-AMT-2                       00019400
           MOVE 'XX/XX/XXXX'   TO  PR-CHECK-DATE                        00019500
           MOVE ALL 'X'  TO  PR-NAME                                    00019600
           MOVE ALL 'X'  TO  PR-ADDRESS-1                               00019700
           MOVE ALL 'X'  TO  PR-ADDRESS-2                               00019800
           MOVE ALL 'X'  TO  PR-ADDRESS-3                               00019900
           MOVE ALL 'X'  TO  PR-VENDOR-ID
102506     IF FNMICR-INV-NO(1) GREATER THAN SPACES 
102506         MOVE ZERO      TO SUB2
102506         PERFORM 2200-FILL-INVOICE THRU 2200-EXIT
102506             VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > +15
102506     END-IF
                                                                        00020000
           .                                                            00020100
       2100-EXIT.                                                       00020200
           EXIT.                                                        00020300
                                                                        00020400
      *                                                                 00020700
       2200-FILL-INVOICE.                                               00020800
      *                                                                 00020900
           MOVE FNMICR-INV-DATE (SUB1) TO WS-INV-DATE                   00021000
           MOVE FNMICR-INV-NO   (SUB1) TO WS-INV-NO                     00021100
           MOVE FNMICR-INV-TYPE (SUB1) TO WS-INV-TYPE                   00021200
           MOVE FNMICR-INV-AMT  (SUB1) TO WS-INV-AMT                    00021300
           ADD +1 TO SUB2                                               00021400
           MOVE WS-INVOICE-LINE        TO PR-INVOICE-LINE (SUB2)        00021500
           ADD +1 TO SUB2                                               00021600
           MOVE FNMICR-INV-MSG (SUB1)  TO PR-INVOICE-LINE (SUB2)
                                                                        00021700
           .                                                            00021800
       2200-EXIT.                                                       00021900
           EXIT.                                                        00022000
                                                                        00022100
      *                                                                 00022400
       3000-WRITE-CC.                                                   00022500
      *                                                                 00022600
           MOVE ZERO TO CC-COUNT                                        00022700
           PERFORM 3100-SCAN-TEXT THRU 3100-EXIT                        00022800
               VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 30                 00022900
           PERFORM CC-COUNT TIMES
061402         MOVE '1'        TO PR-CC                                 00023100
               WRITE CC-RECORD FROM PRINT-RECORD                        00023200
           END-PERFORM.                                                 00023300
                                                                        00023400
       3000-EXIT.                                                       00023500
           EXIT.                                                        00023600
                                                                        00023700
                                                                        00023800
      *                                                                 00023900
       3100-SCAN-TEXT.                                                  00024000
      *                                                                 00024100
      * THIS ROUTINE SCANS FOR ALL OCCURANCES OF "CC "                  00024200
      * WITHIN ALL INVOICE LINES.  CC-COUNT IS SET TO                   00024300
      * THE NUMBER OF "CC " THAT WERE FOUND.                            00024400
      *                                                                 00024500
           IF PR-INVOICE-LINE (SUB2) NOT = SPACE                        00024600
             MOVE PR-INVOICE-LINE (SUB2) TO WS-WORKAREA                 00024700
             PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > 78           00024800
               IF WS-TEXT-BYTE (INDX)     = 'C' AND                     00024900
                  WS-TEXT-BYTE (INDX + 1) = 'C' AND                     00025000
                  WS-TEXT-BYTE (INDX + 2) = ' '                         00025100
                      ADD +1 TO CC-COUNT                                00025200
               END-IF                                                   00025300
             END-PERFORM                                                00025400
           END-IF
                                                                        00025500
           .                                                            00025600
       3100-EXIT.                                                       00025700
           EXIT.                                                        00025800
                                                                        00025900
      *                                                                 00026200
       0000-START.                                                      00026300
      *                                                                 00026400
           OPEN INPUT MICR-DRAFT-FILE                                   00026500
           IF MICRDRFT-STATUS = '00' OR '97'                            00026600
               CONTINUE                                                 00026700
           ELSE                                                         00026800
               DISPLAY 'OPEN ERROR ON MICRDRFT'                         00026900
061402     END-IF                                                       00027100
                                                                        00027200
061402*    OPEN OUTPUT ACH-COPY                                         00027300
061402     OPEN OUTPUT CSO-COPY                                         00027400
                       CC-COPY
                                                                        00027500
           .                                                            00027600
       0000-EXIT.                                                       00027700
           EXIT.                                                        00027800
                                                                        00027900
                                                                        00028000
      *                                                                 00028100
       9000-END.                                                        00028200
      *                                                                 00028300
           CLOSE MICR-DRAFT-FILE                                        00028400
061402*          ACH-COPY                                               00028500
                 CSO-COPY                                               00028600
                 CC-COPY
                                                                        00028700
           .                                                            00028800
       9000-EXIT.                                                       00028900
           EXIT.                                                        00029000
