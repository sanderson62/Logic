       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIAPEXT.
       AUTHOR.     PABLO.
       DATE-COMPILED.
051810******************************************************************
051810*                   C H A N G E   L O G
051810*
051810* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
051810*-----------------------------------------------------------------
051810*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
051810* EFFECTIVE    NUMBER
051810*-----------------------------------------------------------------
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
051810******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERACCT           ASSIGN TO ERACCT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-PRIMARY
                                   FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT ELBENE           ASSIGN TO ELBENE
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS BE-CONTROL-PRIMARY
                                   FILE STATUS IS ELBENE-FILE-STATUS.

           SELECT ELMSTR           ASSIGN TO ELMSTR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CL-CONTROL-PRIMARY
                                   FILE STATUS IS ELMSTR-FILE-STATUS.

           SELECT ELTRLR           ASSIGN TO ELTRLR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AT-CONTROL-PRIMARY
                                   FILE STATUS IS ELTRLR-FILE-STATUS.

           SELECT ELTRLRR          ASSIGN TO ELTRLRR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS ATR-CONTROL-PRIMARY
                                   FILE STATUS IS ELTRLRR-FILE-STATUS.

           SELECT ELTRLR-OUT       ASSIGN TO ELTRLROT
               ORGANIZATION IS LINE SEQUENTIAL.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ERACCT.

           COPY ERCACCT.

       FD  ELBENE.

           COPY ELCBENE.

       FD  ELMSTR.

           COPY ELCMSTR.

       FD  ELTRLR.

           COPY ELCTRLR.
      /
       FD  ELTRLRR.

00018  01  ELTRLR-REC-IN.                                               ELCTRLR
00019      12  FILLER                          PIC XX.                  ELCTRLR
00021                                                                   ELCTRLR
00022      12  ATR-CONTROL-PRIMARY.                                     ELCTRLR
00023          16  ATR-COMPANY-CD               PIC X.                  ELCTRLR
00024          16  ATR-CARRIER                  PIC X.                  ELCTRLR
00025          16  ATR-CLAIM-NO                 PIC X(7).               ELCTRLR
00026          16  ATR-CERT-NO.                                         ELCTRLR
00027              20  ATR-CERT-PRIME           PIC X(10).              ELCTRLR
00028              20  ATR-CERT-SFX             PIC X.                  ELCTRLR
00029          16  ATR-SEQUENCE-NO              PIC S9(4)     COMP.     ELCTRLR
00030              88  ATR-1ST-TRL-AVAIL             VALUE +4095.       ELCTRLR
00031              88  ATR-LAST-TRL-AVAIL            VALUE +100.        ELCTRLR
00032              88  ATR-RESV-EXP-HIST-TRL         VALUE +0.          ELCTRLR
00033              88  ATR-INSURED-ADDR-TRL          VALUE +1 THRU +9.  ELCTRLR
00034              88  ATR-BENEFICIARY-ADDR-TRL      VALUE +11 THRU +19.ELCTRLR
00035              88  ATR-ACCOUNT-ADDR-TRL          VALUE +21 THRU +29.ELCTRLR
00036              88  ATR-PHYSICIAN-ADDR-TRL        VALUE +31 THRU +39.ELCTRLR
00037              88  ATR-EMPLOYERS-ADDR-TRL        VALUE +41 THRU +49.ELCTRLR
00038              88  ATR-OTHER-1-ADDR-TRL          VALUE +51 THRU +59.ELCTRLR
00039              88  ATR-OTHER-2-ADDR-TRL          VALUE +61 THRU +69.ELCTRLR
00040              88  ATR-DIAGNOSIS-TRL             VALUE +90.         ELCTRLR
00041                                                                   ELCTRLR
00042      12  ATR-TRAILER-TYPE                 PIC X.                  ELCTRLR
00043          88  RESERVE-EXPENSE-TR               VALUE '1'.          ELCTRLR
00044          88  PAYMENT-TR                       VALUE '2'.          ELCTRLR
00045          88  AUTO-PAY-TR                      VALUE '3'.          ELCTRLR
00046          88  CORRESPONDENCE-TR                VALUE '4'.          ELCTRLR
00047          88  ADDRESS-TR                       VALUE '5'.          ELCTRLR
00048          88  GENERAL-INFO-TR                  VALUE '6'.          ELCTRLR
00049          88  AUTO-PROMPT-TR                   VALUE '7'.          ELCTRLR
00050          88  DENIAL-TR                        VALUE '8'.          ELCTRLR
00051          88  INCURRED-CHG-TR                  VALUE '9'.          ELCTRLR
00052          88  FORM-CONTROL-TR                  VALUE 'A'.          ELCTRLR
00053                                                                   ELCTRLR
00054      12  ATR-RECORDED-DT                  PIC XX.                 ELCTRLR
00055      12  ATR-RECORDED-BY                  PIC X(4).               ELCTRLR
00056      12  ATR-LAST-MAINT-HHMMSS            PIC S9(6)     COMP-3.   ELCTRLR
00057                                                                   ELCTRLR
00058      12  ATR-TRAILER-BODY                 PIC X(165).             ELCTRLR
00059                                                                   ELCTRLR
00293                                                                   ELCTRLR
00294      12  ATR-ADDRESS-TR  REDEFINES  ATR-TRAILER-BODY.             ELCTRLR
00295          16  ATR-ADDRESS-TYPE             PIC X.                  ELCTRLR
00296              88  INSURED-ADDRESS               VALUE 'I'.         ELCTRLR
00297              88  BENEFICIARY-ADDRESS           VALUE 'B'.         ELCTRLR
00298              88  ACCOUNT-ADDRESS               VALUE 'A'.         ELCTRLR
00299              88  PHYSICIAN-ADDRESS             VALUE 'P'.         ELCTRLR
00300              88  EMPLOYER-ADDRESS              VALUE 'E'.         ELCTRLR
00301              88  OTHER-ADDRESS-1               VALUE 'O'.         ELCTRLR
00302              88  OTHER-ADDRESS-2               VALUE 'Q'.         ELCTRLR
00303          16  ATR-MAIL-TO-NAME             PIC X(30).              ELCTRLR
00304          16  ATR-ADDRESS-LINE-1           PIC X(30).              ELCTRLR
00305          16  ATR-ADDRESS-LINE-2           PIC X(30).              ELCTRLR
00306          16  ATR-CITY-STATE.
                   20  ATR-CITY                 PIC X(28).
                   20  ATR-STATE                PIC XX.
00307          16  ATR-ZIP.                                             ELCTRLR
00308              20  ATR-ZIP-CODE.                                    ELCTRLR
00309                  24  ATR-ZIP-1ST          PIC X.                  ELCTRLR
00311                  24  FILLER              PIC X(4).                ELCTRLR
00312              20  ATR-ZIP-PLUS4            PIC X(4).               ELCTRLR
00317          16  ATR-PHONE-NO                 PIC 9(11)     COMP-3.   ELCTRLR
00318          16  FILLER                      PIC X(23).               ELCTRLR
00319          16  ATR-ADDRESS-LAST-MAINT-DT    PIC XX.                 ELCTRLR
00320          16  ATR-ADDRESS-LAST-UPDATED-BY  PIC X(4).               ELCTRLR
00321                                                                   ELCTRLR

       FD  ELTRLR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  ELTRLR-OUT-REC              PIC X(225).

           EJECT

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIAPEXT  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ELTRLR             VALUE 'Y'.
       77  TRL-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  TRL-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
      /

       01  ELTRLR-DETAIL-RECORD.
           12  EX-CHECK-NO             PIC X(7).
           12  EX-TAB1                 PIC X.
           12  EX-CLAIM-NO             PIC X(7).
           12  EX-TAB2                 PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB3                 PIC X.
           12  EX-CLAIM-LNAME          PIC X(15).
           12  EX-TAB4                 PIC X.
           12  EX-CLAIM-FNAME          PIC X(12).
           12  EX-TAB5                 PIC X.
           12  EX-CLAIM-INIT           PIC X.
           12  EX-TAB6                 PIC X.
           12  EX-PAYEE-NAME           PIC X(30).
           12  EX-TAB7                 PIC X.
           12  EX-PAYEE-ADDR1          PIC X(30).
           12  EX-TAB8                 PIC X.
           12  EX-PAYEE-ADDR2          PIC X(30).
           12  EX-TAB9                 PIC X.
           12  EX-PAYEE-CITY-ST        PIC X(30).
           12  EX-TAB10                PIC X.
           12  EX-PAYEE-ZIP            PIC X(9).
           12  EX-TAB11                PIC X.
           12  EX-CHECK-AMT            PIC -9(7).99.
           12  EX-TAB12                PIC X.
           12  EX-CHECK-DT             PIC X(10).
           12  EX-TAB13                PIC X.
           12  EX-SSN                  PIC X(9).

      ******************************************************************
       01  WS-MISC.
           05  WS-WORK-SEQ             PIC X.
           05  WS-NUM-SEQ REDEFINES WS-WORK-SEQ PIC 9.
           05  WS-SAVE-ELTRLR          PIC X(225) VALUE LOW-VALUES.
           05  ERACCT-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ELBENE-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ELMSTR-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ELTRLRR-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ELTRLR-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.

           EJECT
       PROCEDURE DIVISION.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-ELTRLR THRU 0100-EXIT UNTIL
                 (END-OF-ELTRLR)
PEMTST*          OR (TRL-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' TRLR  RECORDS READ    '  TRL-RECS-IN
           DISPLAY ' TRLR  RECORDS WRITTEN '  TRL-RECS-OUT
           GOBACK
           .

       0100-PROCESS-ELTRLR.

           IF (AT-TRAILER-TYPE = '2')
              AND (AT-VOID-DT = SPACES OR LOW-VALUES)
              AND (AT-CHECK-WRITTEN-DT NOT = SPACES AND LOW-VALUES)
              AND (AT-CHECK-WRITTEN-DT > X'95FF')
              AND (AT-CHECK-WRITTEN-DT < X'9781')
              PERFORM 0105-BUILD-EXTRACT THRU 0105-EXIT
           END-IF

           PERFORM 0200-READ-ELTRLR    THRU 0200-EXIT
           .
       0100-EXIT.
           EXIT.

       0105-BUILD-EXTRACT.

           IF AT-CHECK-NO = '0064953'
              DISPLAY 'FOUND CHECK '
           END-IF

           MOVE AT-CONTROL-PRIMARY     TO CL-CONTROL-PRIMARY

           READ ELMSTR
           IF ELMSTR-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ELMSTR READ ERR ' ELMSTR-FILE-STATUS
           END-IF

           MOVE WS-SAVE-ELTRLR         TO ELTRLR-DETAIL-RECORD
           PERFORM 0110-GET-PAYEE      THRU 0110-EXIT

           MOVE AT-CHECK-NO            TO EX-CHECK-NO
           MOVE AT-CLAIM-NO            TO EX-CLAIM-NO
           MOVE AT-CERT-NO             TO EX-CERT-NO
           MOVE AT-PAYEES-NAME         TO EX-PAYEE-NAME
           MOVE CL-INSURED-LAST-NAME   TO EX-CLAIM-LNAME
           MOVE CL-INSURED-1ST-NAME    TO EX-CLAIM-FNAME
           MOVE CL-INSURED-MID-INIT    TO EX-CLAIM-INIT
           MOVE AT-AMOUNT-PAID         TO EX-CHECK-AMT

           MOVE AT-CHECK-WRITTEN-DT    TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-CHECK-DT
           END-IF


           PERFORM 0300-WRITE-TRLR     THRU 0300-EXIT

           .

       0105-EXIT.
           EXIT.

       0110-GET-PAYEE.

           IF (AT-PAYEE-TYPE = 'A')
              AND (AT-PAYEE-SEQ = '0')
              PERFORM 0120-GET-ERACCT  THRU 0120-EXIT
           ELSE
           IF (AT-PAYEE-TYPE = 'B')
              AND (AT-PAYEE-SEQ = '0')
              PERFORM 0130-GET-ELBENE  THRU 0130-EXIT
           ELSE
              PERFORM 0140-GET-ELTRLRR THRU 0140-EXIT
           END-IF
           END-IF


           .
       0110-EXIT.
           EXIT.

       0120-GET-ERACCT.

           MOVE LOW-VALUES           TO AM-CONTROL-PRIMARY
           MOVE CL-COMPANY-CD        TO AM-COMPANY-CD
           MOVE CL-CERT-CARRIER      TO AM-CARRIER
           MOVE CL-CERT-GROUPING     TO AM-GROUPING
           MOVE CL-CERT-STATE        TO AM-STATE
           MOVE CL-CERT-ACCOUNT      TO AM-ACCOUNT
           MOVE CL-CERT-EFF-DT       TO AM-EXPIRATION-DT

           START ERACCT KEY IS NOT < AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS = '00'
              READ ERACCT NEXT RECORD
              IF ERACCT-FILE-STATUS = '00'
                 MOVE AM-PERSON      TO EX-PAYEE-ADDR1
                 MOVE AM-ADDRS       TO EX-PAYEE-ADDR2
051810           MOVE SPACES         TO EX-PAYEE-CITY-ST
051810           STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810              DELIMITED BY '  ' INTO EX-PAYEE-CITY-ST
051810           END-STRING
                 MOVE AM-ZIP         TO EX-PAYEE-ZIP
              ELSE
                 DISPLAY ' ERACCT READ ERR ' ERACCT-FILE-STATUS
              END-IF
           ELSE
              DISPLAY ' ERACCT START ERR ' ERACCT-FILE-STATUS
           END-IF


           .
       0120-EXIT.
           EXIT.

       0130-GET-ELBENE.

           MOVE AT-COMPANY-CD         TO BE-COMPANY-CD
           MOVE CL-BENEFICIARY        TO BE-BENEFICIARY
           MOVE 'B'                   TO BE-RECORD-TYPE

           READ ELBENE
           IF ELBENE-FILE-STATUS = '00'
              MOVE BE-ADDRESS-LINE-1  TO EX-PAYEE-ADDR1
              MOVE BE-ADDRESS-LINE-2  TO EX-PAYEE-ADDR2
051810        MOVE SPACES             TO EX-PAYEE-CITY-ST
051810        STRING BE-CITY ' ' BE-STATE
051810           DELIMITED BY '  ' INTO EX-PAYEE-CITY-ST
051810        END-STRING
              MOVE BE-ZIP-CODE        TO EX-PAYEE-ZIP
           ELSE
              DISPLAY ' ELBENE READ ERR ' ELBENE-FILE-STATUS
           END-IF


           .
       0130-EXIT.
           EXIT.

       0140-GET-ELTRLRR.

           MOVE AT-CONTROL-PRIMARY     TO ATR-CONTROL-PRIMARY

           MOVE AT-PAYEE-SEQ           TO WS-WORK-SEQ
           IF AT-PAYEE-TYPE = 'I'
              MOVE +0                  TO ATR-SEQUENCE-NO
              ADD WS-NUM-SEQ           TO ATR-SEQUENCE-NO
           ELSE
           IF AT-PAYEE-TYPE = 'B'
              MOVE +10                 TO ATR-SEQUENCE-NO
              ADD WS-NUM-SEQ           TO ATR-SEQUENCE-NO
           ELSE
           IF AT-PAYEE-TYPE = 'A'
              MOVE +20                 TO ATR-SEQUENCE-NO
              ADD WS-NUM-SEQ           TO ATR-SEQUENCE-NO
           ELSE
           IF AT-PAYEE-TYPE = 'O'
              MOVE +50                 TO ATR-SEQUENCE-NO
              ADD WS-NUM-SEQ           TO ATR-SEQUENCE-NO
           ELSE
           IF AT-PAYEE-TYPE = 'Q'
              MOVE +60                 TO ATR-SEQUENCE-NO
              ADD WS-NUM-SEQ           TO ATR-SEQUENCE-NO
           ELSE
           IF AT-PAYEE-TYPE = 'P'
              MOVE +30                 TO ATR-SEQUENCE-NO
              ADD WS-NUM-SEQ           TO ATR-SEQUENCE-NO
           ELSE
           IF AT-PAYEE-TYPE = 'E'
              MOVE +40                 TO ATR-SEQUENCE-NO
              ADD WS-NUM-SEQ           TO ATR-SEQUENCE-NO.

           READ ELTRLRR

           IF ELTRLRR-FILE-STATUS = '00'
              MOVE ATR-ADDRESS-LINE-1   TO EX-PAYEE-ADDR1
              MOVE ATR-ADDRESS-LINE-2   TO EX-PAYEE-ADDR2
051810        MOVE SPACES               TO EX-PAYEE-CITY-ST
051810        STRING ATR-CITY ' ' ATR-STATE
051810           DELIMITED BY '  ' INTO EX-PAYEE-CITY-ST
051810        END-STRING
              MOVE ATR-ZIP              TO EX-PAYEE-ZIP
           ELSE
              DISPLAY ' ELTRLRR READ ERR ' ELTRLRR-FILE-STATUS
           END-IF

           .
       0140-EXIT.
           EXIT.



       0200-READ-ELTRLR.

           READ ELTRLR NEXT RECORD

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              SET END-OF-ELTRLR        TO TRUE
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELTRLR READ NEXT ' ELTRLR-FILE-STATUS
                 SET END-OF-ELTRLR     TO TRUE
              ELSE
                 IF AT-COMPANY-CD > X'04'
                    SET END-OF-ELTRLR TO TRUE
                 END-IF
              END-IF
           END-IF

           IF NOT END-OF-ELTRLR
              ADD 1 TO TRL-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-TRLR.

           WRITE ELTRLR-OUT-REC        FROM ELTRLR-DETAIL-RECORD
           ADD 1 TO TRL-RECS-OUT

           .

       0300-EXIT.
           EXIT.


       0400-OPEN-FILES.

           OPEN INPUT ELTRLR ELTRLRR ELMSTR ERACCT ELBENE
               OUTPUT ELTRLR-OUT

           IF ERACCT-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ERACCT OPEN ERR  ' ERACCT-FILE-STATUS
           END-IF

           IF ELBENE-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ELBENE OPEN ERR  ' ELBENE-FILE-STATUS
           END-IF

           IF ELTRLR-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ELTRLR OPEN ERR  ' ELTRLR-FILE-STATUS
           END-IF

           IF ELTRLRR-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ELTRLRR OPEN ERR  ' ELTRLRR-FILE-STATUS
           END-IF

           IF ELMSTR-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ELMSTR OPEN ERR  ' ELMSTR-FILE-STATUS
           END-IF


           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELTRLR ELTRLR-OUT ELTRLRR ELMSTR ERACCT ELBENE

           .

       0500-EXIT.
           EXIT.

       0550-START-ELTRLR.

           MOVE LOW-VALUES             TO AT-CONTROL-PRIMARY
           MOVE X'04'                  TO AT-COMPANY-CD

           START ELTRLR KEY IS NOT < AT-CONTROL-PRIMARY

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              SET END-OF-ELTRLR        TO TRUE
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELTRLR START     ' ELTRLR-FILE-STATUS
                 SET END-OF-ELTRLR     TO TRUE
              END-IF
           END-IF

           .

       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO ELTRLR-DETAIL-RECORD

           MOVE 'UNKNOWN'              TO EX-PAYEE-ADDR1
                                          EX-PAYEE-ADDR2
                                          EX-PAYEE-CITY-ST
                                          EX-PAYEE-ZIP

           MOVE X'09'                  TO EX-TAB1
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB7
                                          EX-TAB8
                                          EX-TAB9
                                          EX-TAB10
                                          EX-TAB11
                                          EX-TAB12
                                          EX-TAB13

           MOVE ELTRLR-DETAIL-RECORD   TO WS-SAVE-ELTRLR
           PERFORM 0550-START-ELTRLR   THRU 0550-EXIT
           PERFORM 0200-READ-ELTRLR    THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.

