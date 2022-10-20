       TITLE 'FREEDOM/AP MICR DRAFT FILE UPDATE'                        00010000
                                                                        00020000
       IDENTIFICATION DIVISION.                                         00030000
       PROGRAM-ID.    FNB183.                                           00040000
       AUTHOR.        DAN DRYDEN.                                       00050000
       DATE-WRITTEN.  NOVEMBER, 1998.                                   00060000
                                                                        00070000
      ******************************************************************00080000
      *                                                                *00090000
      *    TRANSFER CSO FREEDOM/AP CHECK RECORDS TO MICR DRAFT FILE    *00100000
      *                                                                *00110000
      ******************************************************************00120000
      *                        H I S T O R Y                           *00130000
      ******************************************************************00140000
      *    DATE    NAME DESCRIPTION                                    *00150005
      * ---------- ---- ---------------------------------------------- *00160005
      * 03/12/1999 DANA FLAG NEGATIVE INVOICE AMOUNTS AS "CR"          *00170005
      * 03/12/1999 DANA FIX MULITPLE INVOICE PROBLEM  IR#1999031000010 *00180005
DAN01 * 03/10/2000 DANA CR#2000022300004 - FORM NUMBER FROM PARM       *00190005
CSODJN* 04/01/2000 DJNA CR#2000030100009 - DRAFT NUMBER EXPANSION      *00200005
DAN02 * 03/02/2001 DANA IR#2001022300005 - SKIP BAD RECORDS            *00201007
061402* 06/14/2002 SMVA CR#2002031800003 - ADD CENSTAT LIFE (FORM CEN3)
061402*                          CHANGE MICR0031 COPYBOOK TO FNMICR
061402*                          ADD SYS008 IN PLACE OF DISPLAY COMMANDS
061402*                          FOR SUMMARY INFORMATION
061402*                          MOVE PROGRAM FROM MAINFRAME TO LAN
092002* 09/20/2002 SMVA                  - MOVE TO UNIX 
092002*                                  - HANDLE FILE STATUS '35' 
092302* 09/23/2002 SMVA                  - REMOVE MICR-STATUS CODE
092402* 09/24/2002 SMVA CR#2002082600004 - ADD FORMS CEN4,CEN5,LPA1,LMB1 
100702* 10/07/2002 SMVA CR#2002100700003 - ADD VOIDS TO PROCESS 
080103* 08/01/2003 SMVA CR#2003052100011 - REPLACE FORM 0031 WITH CSO1
112105* 11/21/2005 AJRA CR#2005111400002 - ADD FORM CSO2
052306* 05/23/2006 AJRA CR#2006051800001 - ADD FORM CSO3
      ******************************************************************00210000
                                                                        00220000
       ENVIRONMENT DIVISION.                                            00230000
       INPUT-OUTPUT SECTION.                                            00240000
       FILE-CONTROL.                                                    00250000
                                                                        00260000
           SELECT CSO-CHECK-FILE                                        00270000
               ASSIGN TO SYS010
               ORGANIZATION IS LINE SEQUENTIAL                          00280000
               FILE STATUS IS SYS010-STATUS.                            00290000
                                                                        00300000
           SELECT MICR-DRAFT-FILE                                       00310000
               ASSIGN TO MICRDRFT
061402         ORGANIZATION IS LINE SEQUENTIAL.

100702     SELECT MICR-TRNSFR-FILE                                      00310000
100702         ASSIGN TO TRNSFR
100702         ORGANIZATION IS LINE SEQUENTIAL.

061402     SELECT PRINTER-OUTPUT
061402         ASSIGN TO SYS008.                                        00350000
                                                                        00370000
                                                                        00380000
       DATA DIVISION.                                                   00390000
       FILE SECTION.                                                    00400000
                                                                        00410000
       FD  CSO-CHECK-FILE                                               00420000
           LABEL RECORDS ARE STANDARD                                   00430000
           RECORDING MODE IS F                                          00440000
           BLOCK CONTAINS 0 RECORDS.                                    00460000
DAN02                           COPY FNC030.                            00471011
                                                                        00480000
       FD  MICR-DRAFT-FILE                                              00490000
100702*    RECORD IS VARYING FROM 1836 TO 3000 CHARACTERS.              00500005
100702     RECORD IS VARYING FROM 430 TO 1858 CHARACTERS.   
100702 01  MICR-DRAFT-RECORD    PIC X(1858).                            00510000
100702*    05  MICR-KEY         PIC X(0019).                            00520005
100702*    05  FILLER           PIC X(1817).                            00530000
100702*01  FILLER               PIC X(3000).                            00540000
                                                                        00550000
100702 FD  MICR-TRNSFR-FILE                                             00490000
100702     RECORD IS VARYING FROM 430 TO 1858 CHARACTERS.    
100702 01  MICR-TRNSFR-RECORD   PIC X(1858).       

061402 FD  PRINTER-OUTPUT
061402                          COPY ELCPRTFD.
                                                                        00570000
       WORKING-STORAGE SECTION.                                         00580000
                                                                        00590000
       01  FILLER                  BINARY.                              00600000
           05  SUB                 PIC S9(4)     VALUE +0.              00610000
                                                                        00620000
       01  FILLER                  COMP-3.                              00630000
           05  INPUT-COUNT         PIC S9(7)     VALUE +0.              00640000
               88  FIRST-RECORD                  VALUE +1.              00650000
           05  INPUT-AMT           PIC S9(9)V99  VALUE +0.              00660000
           05  PRINT-COUNT         PIC S9(7)     VALUE +0.              00670000
           05  PRINT-AMT           PIC S9(9)V99  VALUE +0.              00680000
           05  VOID-COUNT          PIC S9(7)     VALUE +0.              00690000
           05  SEQ-NO              PIC S9(5)     VALUE +0.              00700000
061402     05  WS-AMOUNT-PAID      PIC S9(9)V99  VALUE +0.
061402     05  WS-NO-OF-INVOICES   PIC S9(3)     VALUE +0.

       01  FILLER.                                                      00720000
100702*    05  S0C7                PIC X      VALUE ' '.                00730000
           05  SYS010-STATUS       PIC XX     VALUE '00'.               00750000
               88  EOF                        VALUE '10' '47'.          00760000
092302*    05  MICR-STATUS         PIC XX     VALUE '00'.               00770000
100702     05  WS-FIRST-VOID-SW    PIC X(01)  VALUE 'Y'.
100702         88  FIRST-VOID                 VALUE 'Y'. 
100702         88  VOIDS-PROCESSED            VALUE 'N'. 
           05  PREV-CHECK-NO       PIC X(10)  VALUE SPACE.              00780001
CSODJN     05  FIRST-CHECK         PIC X(10)  JUSTIFIED RIGHT.          00790005
           05  PRT-DATE            PIC X(10).                           00800000
           05  PRT-TIME            PIC X(8).                            00810000
           05  ED-AMT              PIC ZZZ,ZZZ,ZZZ.99-.                 00820000
           05  ED-CNT              PIC ZZ,ZZZ,ZZZ,ZZ9-.                 00830000
           05  VOID-NAME           PIC X(46)  VALUE                     00840000
               'VOID   VOID   VOID   VOID   VOID   VOID   VOID'.        00850000
           05  WS-CHECK-DATE.                                           00860000
100702*        10  SAVE-CHECK-MO   PIC XX.                              00870001
100702         10  WS-CHECK-MO     PIC XX.                              00870001
               10  FILLER          PIC X       VALUE '/'.               00880000
100702*        10  SAVE-CHECK-DA   PIC XX.                              00890001
100702         10  WS-CHECK-DA     PIC XX.                              00890001
               10  FILLER          PIC X       VALUE '/'.               00900000
100702*        10  SAVE-CHECK-YR   PIC XXXX.                            00910001
100702         10  WS-CHECK-YR     PIC XXXX.                            00910001
           05  WS-TIME             PIC 9(8).                            00920000
           05  REDEFINES WS-TIME.                                       00930000
               10  WS-HH           PIC XX.                              00940000
               10  WS-MI           PIC XX.                              00950000
               10  WS-SE           PIC XX.                              00960000
               10  WS-MS           PIC XX.                              00970000
061402     05  WS-DATE             PIC 9(06).                           00980000
                                                                        00980000
           05  REDEFINES WS-DATE.
061402         10  WS-YR           PIC XX.                              01000000
               10  WS-MO           PIC XX.                              01010000
               10  WS-DAY          PIC XX.                              01020000
                                                                        01030000
                                                                        01040000
       01  WS-WORK-RECORD.                                              01050000
061402     COPY FNMICR.                                                 01060004
                                                                        01070000
      /                                                                 01080000
       01  SAVE-DATA                 VALUE SPACES.                      01090001
           05  SAVE-NAME             PIC X(50).                         01100001
           05  SAVE-ADDRESS-1        PIC X(50).                         01110001
           05  SAVE-ADDRESS-2        PIC X(50).                         01120001
           05  SAVE-ADDRESS-3        PIC X(50).                         01130001
           05  SAVE-ADDRESS-4        PIC X(50).                         01140001
           05  SAVE-CHECK-NO         PIC X(10).                         01150001
           05  SAVE-CHECK-DATE.                                         01160001
               10  SAVE-CHECK-YR     PIC X(4).                          01170001
               10  SAVE-CHECK-MO     PIC X(2).                          01180001
               10  SAVE-CHECK-DA     PIC X(2).                          01190001
           05  SAVE-CHECK-AMT        PIC S9(8)V99                       01200001
                                     SIGN TRAILING SEPARATE.


061402 01  WS-PRTCHK-SUMMARY-LINE1.
061402     05  FILLER                PIC X(01)      VALUE '1'.
092402     05  WS-PRTCHK-HEADER      PIC X(60)      VALUE SPACES.

061402 01  WS-PRTCHK-SUMMARY-LINE2.
061402     05  FILLER                PIC X(01)      VALUE ' '.
061402     05  FILLER                PIC X(23)      VALUE
061402         'TOTAL CHECKS PRINTED:  '.
061402     05  FILLER                PIC X(15)      VALUE SPACES.
061402     05  WS-PRTCHK-CNT         PIC ZZ,ZZZ,ZZZ,ZZ9-.

061402 01  WS-PRTCHK-SUMMARY-LINE3.
061402     05  FILLER                PIC X(01)      VALUE ' '.
061402     05  FILLER                PIC X(22)      VALUE
061402         'TOTAL DOLLAR AMOUNT:  '.
061402     05  FILLER                PIC X(16)      VALUE SPACES.
061402     05  WS-PRTCHK-TOTAL-AMT   PIC $$Z,ZZZ,ZZZ.99-.

061402 01  WS-PRTCHK-SUMMARY-LINE4.
061402     05  FILLER                PIC X(01)      VALUE ' '.
061402     05  FILLER                PIC X(16)      VALUE
061402         'CHECKS VOIDED:  '.
061402     05  FILLER                PIC X(22)      VALUE SPACES.
061402     05  WS-PRTCHK-VOID-CNT    PIC ZZ,ZZZ,ZZZ,ZZ9-.

061402 01  WS-PRTCHK-SUMMARY-LINE5.
061402     05  FILLER                PIC X(01)      VALUE ' '.
061402     05  FILLER                PIC X(17)      VALUE
061402         'FIRST CHECK NO:  '.
061402     05  FILLER                PIC X(21)      VALUE SPACES.
061402     05  WS-PRTCHK-FIRST-NO    PIC X(10).

061402 01  WS-PRTCHK-SUMMARY-LINE6.
061402     05  FILLER                PIC X(01)      VALUE ' '.
061402     05  FILLER                PIC X(16)      VALUE
061402         'LAST CHECK NO:  '.
061402     05  FILLER                PIC X(22)      VALUE SPACES.
061402     05  WS-PRTCHK-LAST-NO     PIC X(10).

061402 01  WS-PRTCHK-SUMMARY-LINE7.
061402     05  FILLER                PIC X(01)      VALUE ' '.
061402     05  FILLER                PIC X(20)      VALUE
061402         'CHECKS PRINTED ON:  '.
061402     05  FILLER                PIC X(18)      VALUE SPACES.
061402     05  WS-PRTCHK-DATE        PIC X(10).
061402     05  FILLER                PIC X(04)      VALUE
061402         ' AT '.
061402     05  WS-PRTCHK-TIME        PIC X(08).
                                                                        01240000
                                                                        01250004
       LINKAGE SECTION.                                                 01260004
                                                                        01270004
       01  PARM.                                                        01280004
           05  PARM-LENGTH    PIC S9(4) BINARY.                         01290004
           05  PARM-FORM      PIC X(4).                                 01300004
                                                                        01310004
                                                                        01340004
      *                                                                 01350004
       PROCEDURE DIVISION USING PARM.                                   01360004
      *                                                                 01370000
           PERFORM 0000-INIT THRU 0000-EXIT                             01380000
                                                                       
092002***** Status 35 is file not present
092002     IF SYS010-STATUS NOT = '35'
               PERFORM 1000-PROCESS-FILE THRU 1000-EXIT UNTIL EOF   
               PERFORM 9000-END THRU 9000-EXIT                     
092002     END-IF 
                                                                        01430000
061402     GOBACK.                                                      01440000
                                                                        01450000
                                                                        01470000
       1000-PROCESS-FILE.                                               01480000

100702     INITIALIZE AP-CHECK-RECORD

100702***** End of file is set when SYS010-STATUS is 10 or 47
100702*****   47 occurs when void file is not present
           READ CSO-CHECK-FILE                                          01500000
061402        AT END GO TO 1000-EXIT
061402     END-READ

100702     IF SYS010-STATUS = '47'
100702        MOVE SAVE-CHECK-NO            TO WS-PRTCHK-LAST-NO
100702        GO TO 1000-EXIT
100702     END-IF 

DAN02      IF AP-CHECK-AMT NOT NUMERIC                                  01510109
              DISPLAY 'CHECK-AMT IS ' AP-CHECK-AMT 
061402        GO TO 1000-EXIT
061402     END-IF

100702     IF AP-VOID-DATE = SPACES  OR
100702        AP-VOID-DATE = ZEROS   OR
100702        AP-VOID-DATE = LOW-VALUES
100702        CONTINUE
100702     ELSE
100702        IF FIRST-VOID
100702            MOVE SAVE-CHECK-NO        TO WS-PRTCHK-LAST-NO 
100702            PERFORM 2000-BUILD-CHECK  THRU 2000-EXIT
100702            PERFORM 4000-WRITE-MICR   THRU 4000-EXIT  
100702            WRITE MICR-TRNSFR-RECORD  FROM WS-WORK-RECORD
100702            SET VOIDS-PROCESSED       TO TRUE
100702        END-IF

100702        ADD +1                      TO VOID-COUNT
100702        MOVE 'V'                    TO FNMICR-VOID-SW
100702        MOVE PARM-FORM              TO FNMICR-FORM      
100702        MOVE AP-CHECK-NO            TO FNMICR-DRAFT-NO         
100702        MOVE AP-CHECK-AMT           TO FNMICR-AMOUNT-PAID
100702        MOVE AP-CHECK-YR            TO WS-CHECK-YR        
100702        MOVE AP-CHECK-MO            TO WS-CHECK-MO        
100702        MOVE AP-CHECK-DA            TO WS-CHECK-DA        
100702        MOVE WS-CHECK-DATE          TO FNMICR-CHECK-DATE   
100702        MOVE AP-NAME                TO FNMICR-PAYEE-NAME  
100702        MOVE AP-ADDRESS-1           TO FNMICR-PAYEE-ADDRESS1      
100702        MOVE AP-ADDRESS-2           TO FNMICR-PAYEE-ADDRESS2     
100702        MOVE AP-ADDRESS-3           TO FNMICR-PAYEE-ADDRESS3    
100702        MOVE AP-ADDRESS-4           TO FNMICR-PAYEE-ADDRESS4   
100702        PERFORM 2100-ADJUST-ADDRESS THRU 2100-EXIT
100702        WRITE MICR-TRNSFR-RECORD    FROM WS-WORK-RECORD
100702        MOVE SPACE                  TO FNMICR-VOID-SW
100702        GO TO 1000-EXIT
100702     END-IF 
                                                                 
           ADD +1 TO INPUT-COUNT                                        01520000
           IF AP-CHECK-SGN = '-'                                        01521009
DAN02         SUBTRACT AP-CHECK-AMT FROM INPUT-AMT                      01522011
              DISPLAY 'NEGATIVE AMT: '                                  01523009
                       AP-CHECK-NO '   ' AP-CHECK-AMT ' ' AP-CHECK-SGN  01524009
           ELSE                                                         01531009
              ADD AP-CHECK-AMT TO INPUT-AMT                             01531109
           END-IF                                                       01533009
                                                                        01540000
           IF FIRST-RECORD                                              01550000
              MOVE AP-CHECK-NO TO PREV-CHECK-NO                         01560000
              MOVE AP-CHECK-NO TO FIRST-CHECK                           01570000
              MOVE AP-CHECK-RECORD TO SAVE-DATA                         01580002
              MOVE +0 TO SAVE-CHECK-AMT                                 01590002
              PERFORM 3100-INIT-INVOICE THRU 3100-EXIT                  01600003
           END-IF                                                       01610000

           IF AP-CHECK-NO NOT = PREV-CHECK-NO                           01630000
              PERFORM 2000-BUILD-CHECK  THRU 2000-EXIT
              PERFORM 4000-WRITE-MICR   THRU 4000-EXIT                  01650003
100702        WRITE MICR-TRNSFR-RECORD  FROM WS-WORK-RECORD
              PERFORM 3100-INIT-INVOICE THRU 3100-EXIT                  01660003
              MOVE AP-CHECK-RECORD TO SAVE-DATA                         01670003
              MOVE +0 TO SAVE-CHECK-AMT                                 01680003
           END-IF                                                       01690000
                                                                        01700000
           PERFORM 3000-BUILD-INVOICE THRU 3000-EXIT                    01710000

           MOVE AP-CHECK-NO TO PREV-CHECK-NO                            01730000

           .                                                            01740000
       1000-EXIT.                                                       01750000
           EXIT.                                                        01760000
                                                                        01770001
                                                                        01800000
       2000-BUILD-CHECK.                                                01810000
      *                                                                 01820000
DAN01      MOVE PARM-FORM              TO FNMICR-FORM                   01840004
100702          
           ADD +1                      TO SEQ-NO                        01850001
           MOVE SEQ-NO                 TO FNMICR-DRAFT-ORDER            01860001
           MOVE SAVE-CHECK-NO          TO FNMICR-DRAFT-NO               01880001
           MOVE SAVE-CHECK-AMT         TO FNMICR-AMOUNT-PAID
061402     MOVE SAVE-CHECK-AMT         TO WS-AMOUNT-PAID                01900001
           MOVE +0                     TO SAVE-CHECK-AMT                01910001
100702     MOVE SAVE-CHECK-YR          TO WS-CHECK-YR
100702     MOVE SAVE-CHECK-MO          TO WS-CHECK-MO
100702     MOVE SAVE-CHECK-DA          TO WS-CHECK-DA
           MOVE WS-CHECK-DATE          TO FNMICR-CHECK-DATE             01930001
           MOVE SAVE-NAME              TO FNMICR-PAYEE-NAME             01960001
           MOVE SAVE-ADDRESS-1         TO FNMICR-PAYEE-ADDRESS1         01970001
           MOVE SAVE-ADDRESS-2         TO FNMICR-PAYEE-ADDRESS2         01980001
           MOVE SAVE-ADDRESS-3         TO FNMICR-PAYEE-ADDRESS3         01990001
           MOVE SAVE-ADDRESS-4         TO FNMICR-PAYEE-ADDRESS4         02000001
           PERFORM 2100-ADJUST-ADDRESS THRU 2100-EXIT                   02010000
           .                                                            02020000
       2000-EXIT.                                                       02030000
           EXIT.                                                        02040000
                                                                        02050000
                                                                        02060000
                                                                        02070000
       2100-ADJUST-ADDRESS.                                             02080000
      *                                                                 02090000
      *  THIS ROUTINE SHIFTS THE ADDRESS LINES SO THAT THERE ARE NO   
      *  BLANK LINES BETWEEN ADDRESS LINES.                          
      *                                                             
           IF FNMICR-PAYEE-ADDRESS1 = SPACE                        
               IF FNMICR-PAYEE-ADDRESS2 NOT = SPACE               
                   MOVE FNMICR-PAYEE-ADDRESS2
                                               TO FNMICR-PAYEE-ADDRESS1
                   MOVE SPACES                 TO FNMICR-PAYEE-ADDRESS2
               ELSE                                                   
                   IF FNMICR-PAYEE-ADDRESS3 NOT = SPACE              
                       MOVE FNMICR-PAYEE-ADDRESS3
                                               TO FNMICR-PAYEE-ADDRESS1
                       MOVE SPACES             TO FNMICR-PAYEE-ADDRESS3
                   ELSE                                               
                       IF FNMICR-PAYEE-ADDRESS4 NOT = SPACE          
                           MOVE FNMICR-PAYEE-ADDRESS4
                                               TO FNMICR-PAYEE-ADDRESS1
                           MOVE SPACES         TO FNMICR-PAYEE-ADDRESS4
                       ELSE
061402                     GO TO 2100-EXIT
061402                 END-IF
061402             END-IF
061402         END-IF
061402     END-IF                                                     
                                                                     
           IF FNMICR-PAYEE-ADDRESS2 = SPACE                         
               IF FNMICR-PAYEE-ADDRESS3 NOT = SPACE                
                   MOVE FNMICR-PAYEE-ADDRESS3
                                               TO FNMICR-PAYEE-ADDRESS2
                   MOVE SPACES                 TO FNMICR-PAYEE-ADDRESS3
               ELSE                                                   
                   IF FNMICR-PAYEE-ADDRESS4 NOT = SPACE              
                       MOVE FNMICR-PAYEE-ADDRESS4
                                               TO FNMICR-PAYEE-ADDRESS2
061402                 MOVE SPACES             TO FNMICR-PAYEE-ADDRESS4
061402             END-IF
061402         END-IF
061402     END-IF                                                     
                                                                     
           IF FNMICR-PAYEE-ADDRESS3 = SPACE                         
               IF FNMICR-PAYEE-ADDRESS4 NOT = SPACE                
                   MOVE FNMICR-PAYEE-ADDRESS4
                                               TO FNMICR-PAYEE-ADDRESS3
061402             MOVE SPACES                 TO FNMICR-PAYEE-ADDRESS4
061402         END-IF
061402     END-IF

061402     .                                                          
       2100-EXIT.                                                    
           EXIT.                                                    
                                                                   
                                                                        02450000
                                                                        02460000
       3000-BUILD-INVOICE.                                              02470000
      *                                                                 02480000
           ADD +1 TO SUB                                                02490000
           IF SUB > +15                                                 02500000
              PERFORM 3200-VOID THRU 3200-EXIT                          02510001
              PERFORM 4000-WRITE-MICR THRU 4000-EXIT                    02520000
              PERFORM 3100-INIT-INVOICE THRU 3100-EXIT                  02530003
              MOVE +1 TO SUB                                            02540000
           END-IF                                                       02550000
                                                                        02560000
DAN02      IF AP-CHECK-SGN = '-'                                        02561011
              SUBTRACT AP-CHECK-AMT FROM SAVE-CHECK-AMT                 02562009
           ELSE                                                         02563009
              ADD AP-CHECK-AMT TO SAVE-CHECK-AMT                        02564109
           END-IF

061402     ADD +1 TO WS-NO-OF-INVOICES
061402     MOVE WS-NO-OF-INVOICES TO FNMICR-NO-OF-INVOICES              02565009
                                                                        02590000
           MOVE AP-INVOICE-NO  TO FNMICR-INV-NO  (SUB)                  02600000
           MOVE AP-INVOICE-AMT TO FNMICR-INV-AMT (SUB)
                                                                        02610000
031299     IF AP-INVOICE-SGN = '-'                                      02620009
031299        MOVE 'CR'        TO FNMICR-INV-TYPE (SUB)                 02630000
031299     ELSE                                                         02640000
031299        MOVE '  '        TO FNMICR-INV-TYPE (SUB)                 02650000
031299     END-IF
                                                                        02660000
           MOVE AP-REF         TO FNMICR-INV-MSG (SUB) (1:30)           02670000
           MOVE AP-PO-NO       TO FNMICR-INV-MSG (SUB) (33:24)          02680000
           MOVE AP-VENDOR-ID   TO FNMICR-VENDOR-NO                      02690000

           .                                                            02700000
       3000-EXIT.                                                       02710000
           EXIT.                                                        02720000
                                                                        02730000
                                                                        02740000
                                                                        02750000
       3100-INIT-INVOICE.                                               02760000
                                                                        02770000
061402     MOVE +0 TO WS-NO-OF-INVOICES                                 02780000
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > +15              02790000
              MOVE SPACE TO FNMICR-INVOICE-DATA (SUB)                   02800000
           END-PERFORM                                                  02810000
           MOVE +0 TO SUB                                               02820000
           .                                                            02830000
       3100-EXIT.                                                       02840000
           EXIT.                                                        02850000
                                                                        02860001
                                                                        02870001
                                                                        02880001
       3200-VOID.                                                       02890001
      *                                                                 02900001
      *  THIS ROUTINE VOIDS THE CHECK IF THERE ARE MORE THAN            02910001
      *  15 INVOICES.  THIS IS NECESSARY SO THAT THE REMAINING          02920001
      *  INVOICE LINES CAN PRINT ON THE NEXT CHECK.                     02930001
      *                                                                 02940001
           ADD +1              TO SEQ-NO                                02960003
           MOVE SEQ-NO         TO FNMICR-DRAFT-ORDER                    02970003
           MOVE SAVE-CHECK-NO  TO FNMICR-DRAFT-NO                       02990003
           MOVE +0             TO FNMICR-AMOUNT-PAID                    03010003
           MOVE CORR SAVE-CHECK-DATE TO WS-CHECK-DATE                   03020003
           MOVE WS-CHECK-DATE  TO FNMICR-CHECK-DATE                     03030003
           MOVE VOID-NAME      TO FNMICR-PAYEE-NAME                     03040003
           MOVE ALL '*'        TO FNMICR-PAYEE-ADDRESS1                 03050003
           MOVE ALL '*'        TO FNMICR-PAYEE-ADDRESS2                 03060003
           MOVE ALL '*'        TO FNMICR-PAYEE-ADDRESS3                 03070003
           ADD +1 TO VOID-COUNT                                         03080001
           .                                                            03090001
       3200-EXIT.                                                       03100001
           EXIT.                                                        03110001
                                                                        03120000
                                                                        03130000
                                                                        03140000
       4000-WRITE-MICR.                                                 03150000
      *                                                                 03160000
           WRITE MICR-DRAFT-RECORD FROM WS-WORK-RECORD                  03170000
                                                                        03230000
062104     IF SUB NOT > +15
               ADD +1 TO PRINT-COUNT
061402         ADD WS-AMOUNT-PAID    TO PRINT-AMT    
062104     END-IF

           .                                                            03300000
       4000-EXIT.                                                       03310000
           EXIT.                                                        03320000
                                                                        03330000
                                                                        03350000
       0000-INIT.                                                       03360000
                                                                        03370000
092402     IF PARM-FORM = 'CSO1' OR 'CEN1' OR 'CEN2' OR 'CEN3' OR       03371012
112105         'CEN4' OR 'CEN5' OR 'LPA1' OR 'LMB1' OR 'CSO2'
052306         OR 'CSO3'
DAN02         CONTINUE                                                  03372012
DAN02      ELSE                                                         03372112
DAN02         DISPLAY 'INVALID PARAMETER IN JCL: ' PARM-FORM
061402     END-IF
                                                                        03372212
           OPEN INPUT CSO-CHECK-FILE                                    03380000
092002     IF SYS010-STATUS NOT = '00' AND '35'                         03390000
              DISPLAY 'OPEN ERROR ' SYS010-STATUS ' ON SYS010'
061402     END-IF
                                                                        03420000
061402     OPEN OUTPUT MICR-DRAFT-FILE
100702                 MICR-TRNSFR-FILE
061402                 PRINTER-OUTPUT
                                                                        03430000
           .                                                            03530000
       0000-EXIT.                                                       03540000
           EXIT.                                                        03550000
                                                                        03560000
                                                                        03570000
                                                                        03580000
       9000-END.                                                        03590000
                                                                        03600000
100702     IF VOIDS-PROCESSED
100702        CONTINUE
100702     ELSE
100702         IF INPUT-COUNT > ZERO 
                  PERFORM 2000-BUILD-CHECK THRU 2000-EXIT
                  PERFORM 4000-WRITE-MICR  THRU 4000-EXIT   
100702            WRITE MICR-TRNSFR-RECORD FROM WS-WORK-RECORD
               END-IF
100702     END-IF

061402     ACCEPT WS-DATE FROM DATE
061402     STRING WS-MO '/' WS-DAY '/' WS-YR                            03650000
061402       DELIMITED BY SIZE INTO PRT-DATE                            03660000
061402     END-STRING                                                   03670000

                                                                        03680000
           ACCEPT WS-TIME FROM TIME                                     03690000
           STRING WS-HH ':' WS-MI ':' WS-SE                             03700000
             DELIMITED BY SIZE INTO PRT-TIME                            03710000
           END-STRING                                                   03720000
                                                                        03730000
061402     EVALUATE TRUE                                                03740004
080103         WHEN PARM-FORM = 'CSO1'
080103            MOVE 'CSO AP CHECKS PRINTED - FORM CSO1'
061402                                          TO WS-PRTCHK-HEADER     03750004
DAN01             DISPLAY 'CSO FREEDOM/AP CHECKS PRINTED'
                                                                        03760004
061402         WHEN PARM-FORM = 'CEN1'
061402            MOVE 'CENSTAT SERVICES CHECKS PRINTED - FORM CEN1'
061402                                          TO WS-PRTCHK-HEADER     03770004
DAN01             DISPLAY 'CENSTAT SERVICES, INC CHECKS PRINTED'
                                                                        03780004
061402         WHEN PARM-FORM = 'CEN2'
061402            MOVE
061402            'CENSTAT DEALER SERVICES CHECKS PRINTED - FORM CEN2'
061402                                          TO WS-PRTCHK-HEADER     03790004
DAN01             DISPLAY 'CENSTAT DEALER SERVICES, INC CHECKS PRINTED'

061402         WHEN PARM-FORM = 'CEN3'
061402            MOVE
                  'CENSTAT LIFE ASSURANCE CHECKS PRINTED - FORM CEN3'
061402                                          TO WS-PRTCHK-HEADER     03790004
061402            DISPLAY 'CENSTAT LIFE, INC CHECKS PRINTED'
  
090902         WHEN PARM-FORM = 'CEN4'
090902            MOVE
090902            'CENSTAT FINANCIAL CHECKS PRINTED - FORM CEN4'
090902                                          TO WS-PRTCHK-HEADER     03790004
090902            DISPLAY 'CENSTAT FINANCIAL INC. CHECKS PRINTED'

090902         WHEN PARM-FORM = 'CEN5'
090902            MOVE
090902            'CENSTAT CASUALTY COMPANY CHECKS PRINTED - FORM CEN5'
090902                                          TO WS-PRTCHK-HEADER     03790004
090902            DISPLAY 'CENSTAT CASUALTY COMPANY CHECKS PRINTED'

090902         WHEN PARM-FORM = 'LPA1'
090902            MOVE
090902         'LENDERS PROTECTION ASSURANCE CHECKS PRINTED - FORM LPA1'
090902                                          TO WS-PRTCHK-HEADER     03790004
090902            DISPLAY 'LENDERS PROTECTION ASSURANCE CHECKS PRINTED'
                                                                        03800004
090902         WHEN PARM-FORM = 'LMB1'
090902            MOVE
090902         'LENDERS MUTUAL BENEFIT ASSN. CHECKS PRINTED - FORM LMB1'
090902                                          TO WS-PRTCHK-HEADER     03790004
090902            DISPLAY 'LENDERS MUTUAL BENEFIT ASSN. CHECKS PRINTED'

112105         WHEN PARM-FORM = 'CSO2'
112105            MOVE 'CSI DCC AP CHECKS PRINTED - FORM CSO2'
112105                                          TO WS-PRTCHK-HEADER  
112105            DISPLAY 'CSI DCC/AP CHECKS PRINTED'
112105                                                               
052306         WHEN PARM-FORM = 'CSO3'
052306            MOVE 'DCC LPAC AP CHECKS PRINTED - FORM CSO3'
052306                                          TO WS-PRTCHK-HEADER  
052306            DISPLAY 'DCC LPAC/AP CHECKS PRINTED'
052306
DAN01          WHEN OTHER                                              
DAN01             DISPLAY 'INVALID FORM NUMBER IN JCL: ' PARM-FORM     
DAN01      END-EVALUATE                                                 03830004
                                                                        03840004
061402     DISPLAY ' '                                                  03860000
061402     MOVE PRINT-COUNT TO ED-CNT                                   03870000
061402     DISPLAY 'CHECKS PRINTED: ' ED-CNT                            03880000
061402     MOVE PRINT-AMT   TO ED-AMT                                   03890000
061402     DISPLAY 'AMOUNT PRINTED: ' ED-AMT                            03900000
061402     MOVE VOID-COUNT TO ED-CNT                                    03910000
061402     DISPLAY '   VOID CHECKS: ' ED-CNT                            03920000
061402     DISPLAY 'FIRST CHECK NO:       ' FIRST-CHECK                 03930000
100702     DISPLAY ' LAST CHECK NO:       ' WS-PRTCHK-LAST-NO           03940000
061402     DISPLAY '    PRINT DATE: ' PRT-DATE '  ' PRT-TIME            03950000

061402     WRITE PRT            FROM WS-PRTCHK-SUMMARY-LINE1
061402     MOVE PRINT-COUNT     TO WS-PRTCHK-CNT
061402     WRITE PRT            FROM WS-PRTCHK-SUMMARY-LINE2
061402     MOVE PRINT-AMT       TO WS-PRTCHK-TOTAL-AMT
061402     WRITE PRT            FROM WS-PRTCHK-SUMMARY-LINE3
061402     MOVE VOID-COUNT      TO WS-PRTCHK-VOID-CNT
061402     WRITE PRT            FROM WS-PRTCHK-SUMMARY-LINE4
061402     MOVE FIRST-CHECK     TO WS-PRTCHK-FIRST-NO
061402     WRITE PRT            FROM WS-PRTCHK-SUMMARY-LINE5
061402     WRITE PRT            FROM WS-PRTCHK-SUMMARY-LINE6
061402     MOVE PRT-DATE        TO WS-PRTCHK-DATE
061402     MOVE PRT-TIME        TO WS-PRTCHK-TIME
061402     WRITE PRT            FROM WS-PRTCHK-SUMMARY-LINE7


           CLOSE CSO-CHECK-FILE                                         03970000
                 MICR-DRAFT-FILE
100702           MICR-TRNSFR-FILE
061402           PRINTER-OUTPUT                                         03980000
           .                                                            03990000
       9000-EXIT.                                                       04000000
           EXIT.                                                        04010000
                                                                        04020000
