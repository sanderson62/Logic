       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ECS0632.
       AUTHOR.        PABLO.
       DATE-COMPILED.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
010505* 010505  2004110300005    PEMA  NEW PROGRAM FOR SECURE PAY    
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-FILE-IN     ASSIGN TO EXTRIN.

           SELECT ERCOMP           ASSIGN TO ERCOMP
                                   ORGANIZATION INDEXED
                                   ACCESS DYNAMIC
                                   RECORD KEY CO-CONTROL-PRIMARY
                                   FILE STATUS ERCOMP-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT SORT-WORK        ASSIGN TO SYS001.

           SELECT PRNTR            ASSIGN TO SYS008.
           SELECT FICH             ASSIGN TO SYS020.

       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY ECSEXT01.

       FD  ERCOMP.
           COPY ERCCOMP.

       FD  DISK-DATE
           COPY ELCDTEFD.

       SD  SORT-WORK.
       01  SORT-REC.
           12  SORT-KEY             PIC  X(41).
           12  SORT-DETAIL          PIC  X(73).
     
       FD  PRNTR
                                       COPY ELCPRTFD.

       FD  FICH
                                       COPY ELCFCHFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   ECS0632  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  X1                          PIC S999 VALUE +0 COMP-3.
       77  Y1                          PIC S999 VALUE +0 COMP-3.
       77  X                           PIC X   VALUE SPACES.
       77  WS-SAVE-PRT                 PIC X(133)  VALUE SPACES.
       77  WS-BANK-CNT                 PIC S999 VALUE +0 COMP-3.
       01  WS-LINE-COUNT            COMP-3  PIC S9(03) VALUE +56.
       01  WS-LINE-COUNT-MAX        COMP-3  PIC S9(03) VALUE +55.
       01  WS-PAGE                  COMP-3  PIC S9(05) VALUE +0.
       01  WS-RELEASE-SORT-COUNT    COMP-3  PIC S9(05) VALUE +0.
       01  WS-RETURN-SORT-COUNT     COMP-3  PIC S9(05) VALUE +0.
       01  WS-BANK-ISSUE-COUNT      COMP-3  PIC S9(05) VALUE +0.
       01  WS-AGENT-ISSUE-COUNT     COMP-3  PIC S9(05) VALUE +0.
       01  WS-ALL-ISSUE-COUNT       COMP-3  PIC S9(05) VALUE +0.
       01  WS-BANK-CANCEL-COUNT     COMP-3  PIC S9(05) VALUE +0.
       01  WS-AGENT-CANCEL-COUNT    COMP-3  PIC S9(05) VALUE +0.
       01  WS-ALL-CANCEL-COUNT      COMP-3  PIC S9(05) VALUE +0.
       01  WS-BANK-BANK-FEE-TOTAL   COMP-3  PIC S9(07)V99 VALUE +0.
       01  WS-AGENT-BANK-FEE-TOTAL  COMP-3  PIC S9(07)V99 VALUE +0.
       01  WS-ALL-BANK-FEE-TOTAL    COMP-3  PIC S9(07)V99 VALUE +0.
       01  WS-CNC-FACT              COMP-3  PIC S9(03)V9(07) VALUE +0.

       01  WS-EOF-SW                        PIC  X(01) VALUE SPACE.
           88  END-OF-EXTR                             VALUE 'Y'.

       01  WS-EOF-SW2                       PIC  X(01) VALUE SPACE.
           88  END-OF-SORT-FILE                        VALUE 'Y'.

       01  EXTR-RECS-IN                     PIC  9(09) VALUE ZEROS.
       01  SUB1                             PIC S9(05) VALUE +0 COMP-3.
       01  PGM-SUB                   COMP-3 PIC S9(03) VALUE +0.
       01  ERCOMP-STATUS                    PIC  X(02) VALUE SPACES.

       01  WS-ALL-SUB-BANKS.
           05  FILLER OCCURS 8.
               10  WS-SUB-BANK         PIC X(10).
               10  WS-SUB-BANK-LVL     PIC 99.
       01  WS-MAIN-BANK                PIC X(10).
       01  WS-BENEFIT-CODE-SW          PIC  X(01) VALUE SPACE.
           88  AH-BEN-CODE-FOUND                  VALUE 'Y'.

       01  WS-RECORD-FOUND-SW               PIC  X(01) VALUE SPACE.
           88  GOT-MOST-CURRENT                        VALUE 'Y'.

       01  WS-FIRST-TIME-SW                 PIC  X(01) VALUE 'Y'.
           88  FIRST-TIME                              VALUE 'Y'.
           88  NOT-FIRST-TIME                          VALUE 'N'.

       01  WS-CHNG-IN-BANK-SW               PIC  X(01) VALUE 'Y'.
           88  NEW-BANK                                VALUE 'Y'.
           88  SAME-BANK                               VALUE 'N'.

       01  WS-ABEND-FIELDS.
           05  WS-RETURN-CODE               PIC S9(04) VALUE ZERO.
           05  WS-ZERO                      PIC S9(01) VALUE ZERO.
           05  WS-ABEND-MESSAGE             PIC  X(80) VALUE SPACES.
           05  WS-ABEND-FILE-STATUS         PIC  X(02) VALUE ZERO.

      ***** SAVE AREA  *****  
       01  WS-SAVE-SORT-KEY-27              PIC  X(27).
       01  WS-PREV-AGENT                    PIC  X(10).
       01  WS-PREV-BANK                     PIC  X(10).
       01  WS-PREV-REC-TYPE                 PIC  X(02).
       01  WS-PREV-REFUND-OPT-FLG           PIC  X(01).
       01  WS-SAVE-AM-CONTROL-A             PIC  X(19).
       01  WS-SAVE-AM-NAME                  PIC  X(30).


      ***** SORT WORK AREA  *****  
       01  WS-SORT-REC.
           05  WS-SORT-KEY.
               10  WS-SORT-CARRIER          PIC  X(01) VALUE SPACE.
               10  WS-SORT-GROUP            PIC  X(06) VALUE SPACES.
               10  WS-SORT-AGENT            PIC  X(10) VALUE SPACES.
               10  WS-SORT-BANK             PIC  X(10) VALUE SPACES.
               10  WS-SORT-REC-TYPE         PIC  X(02) VALUE SPACES.
                   88  REC-ISSUE                       VALUE '01'.
                   88  REC-CANCEL                      VALUE '02'.
               10  WS-SORT-REFUND-OPT-FLG   PIC  X(01) VALUE SPACE.
                   88  NOT-REFUNDABLE-SECTION          VALUE ' '.
                   88  REFUNDABLE-SECTION              VALUE '*'.
               10  WS-SORT-CERT             PIC  X(11) VALUE SPACES.
           05  WS-SORT-INSURED-NAME.
               10  WS-SORT-LNAME            PIC  X(15) VALUE SPACES.
               10  WS-SORT-FNAME            PIC  X(10) VALUE SPACES.
               10  WS-SORT-INIT             PIC  X(01) VALUE SPACES.
           05  WS-SORT-CERT-EFF-DT          PIC  9(11) VALUE 0.
           05  WS-SORT-AGE                  PIC  9(02) VALUE 0.
           05  WS-SORT-AH-TERM              PIC S9(03) VALUE +0.
           05  WS-SORT-AH-TYPE              PIC  X(03) VALUE SPACES.
           05  WS-SORT-PRM-AMT       COMP-3 PIC S9(9)V99 VALUE +0.
           05  WS-SORT-AH-CAN-DT            PIC  9(11) VALUE 0.
           05  WS-SORT-AH-REFUND     COMP-3 PIC S9(07)V99 VALUE +0.
           05  WS-SORT-BANK-FEE      COMP-3 PIC S9(05)V99 VALUE +0.
           05  WS-SORT-STATE                PIC  X(02) VALUE SPACES.

      ***** REPORT LAYOUT AREA  *****  
       01  WS-DETAIL1A.
           05 FILLER                       PIC  X(01)  VALUE ' '.
           05 FILLER                       PIC  X(02)  VALUE SPACES.
           05 WS-D1A-INSURED-NAME          PIC  X(26)  VALUE SPACES.
           05 FILLER                       PIC  X(04)  VALUE SPACES.
           05 WS-D1A-CERT                  PIC  X(10)  VALUE SPACES.
           05 FILLER                       PIC  X(03)  VALUE SPACES.
           05 WS-D1A-CERT-EFF-DT.
              10  WS-D1A-CERT-EFF-MM       PIC  X(02)  VALUE SPACES.
              10  FILLER                   PIC  X(01)  VALUE '/'.
              10  WS-D1A-CERT-EFF-DD       PIC  X(02)  VALUE SPACES.
              10  FILLER                   PIC  X(01)  VALUE '/'.
              10  WS-D1A-CERT-EFF-CCYY     PIC  X(04)  VALUE SPACES.
           05 FILLER                       PIC  X(03)  VALUE SPACES.
           05 WS-D1A-AGE                   PIC  X(02)  VALUE SPACES.
           05 FILLER                       PIC  X(04)  VALUE SPACES.
           05 WS-D1A-AH-TERM               PIC  ZZ9.
           05 FILLER                       PIC  X(06)  VALUE SPACES.
           05 WS-D1A-AH-TYPE               PIC  X(03)  VALUE SPACES.
           05 FILLER                       PIC  X(02)  VALUE SPACES.
           05 WS-D1A-PRM-AMT               PIC  Z,ZZZ,ZZZ.99-.
           05 FILLER                       PIC  X(08)  VALUE SPACES.
           05 WS-D1A-BANK-FEE              PIC  Z,ZZZ.99-.
           05 FILLER                       PIC  X(24)  VALUE SPACES.
 
       01  WS-DETAIL1B.
           05 FILLER                       PIC  X(01)  VALUE ' '.
           05 FILLER                       PIC  X(02)  VALUE SPACES.
           05 WS-D1B-INSURED-NAME          PIC  X(26)  VALUE SPACES.
           05 FILLER                       PIC  X(04)  VALUE SPACES.
           05 WS-D1B-CERT                  PIC  X(10)  VALUE SPACES.
           05 FILLER                       PIC  X(03)  VALUE SPACES.
           05 WS-D1B-CERT-EFF-DT.
              10  WS-D1B-CERT-EFF-MM       PIC  X(02)  VALUE SPACES.
              10  FILLER                   PIC  X(01)  VALUE '/'.
              10  WS-D1B-CERT-EFF-DD       PIC  X(02)  VALUE SPACES.
              10  FILLER                   PIC  X(01)  VALUE '/'.
              10  WS-D1B-CERT-EFF-CCYY     PIC  X(04)  VALUE SPACES.
           05 FILLER                       PIC  X(03)  VALUE SPACES.
           05 WS-D1B-CERT-CANCEL-DT.
              10  WS-D1B-CERT-CAN-MM       PIC  X(02)  VALUE SPACES.
              10  FILLER                   PIC  X(01)  VALUE '/'.
              10  WS-D1B-CERT-CAN-DD       PIC  X(02)  VALUE SPACES.
              10  FILLER                   PIC  X(01)  VALUE '/'.
              10  WS-D1B-CERT-CAN-CCYY     PIC  X(04)  VALUE SPACES.
           05 FILLER                       PIC  X(03)  VALUE SPACES.
           05 WS-D1B-AH-TYPE               PIC  X(03)  VALUE SPACES.
           05 FILLER                       PIC  X(04)  VALUE SPACES.
102704     05 WS-D1B-PRM-AMT               PIC  Z,ZZZ,ZZZ.99-.
102704     05 FILLER                       PIC  X(04).
           05 WS-D1B-REFUND-AMT            PIC  Z,ZZZ,ZZZ.99-.
           05 FILLER                       PIC  X(04)  VALUE SPACES.
           05 WS-D1B-REFUND-BKFEE          PIC  Z,ZZZ.99-.
           05 FILLER                       PIC  X(28)  VALUE SPACES.

       01  WS-HD1.
           05  FILLER                      PIC  X(01)  VALUE '1'.
           05  FILLER                      PIC  X(50)  VALUE SPACES.
           05  WS-HD1-TITLE                PIC  X(68)  VALUE
               'BANK FEE RECONCILIATION DETAIL'.
           05  FILLER                      PIC  X(07)  VALUE 'ECS0632'.
           05  FILLER                      PIC  X(07)  VALUE SPACES.
     
       01  WS-HD2.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(50)  VALUE SPACES.
           05  WS-HD2-CO                   PIC  X(30)  VALUE SPACES.
           05  FILLER                      PIC  X(38)  VALUE SPACES.
           05  WS-HD2-RUN-DT               PIC  X(08)  VALUE SPACES.
           05  FILLER                      PIC  X(06)  VALUE SPACES.
     
       01  WS-HD3.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(56)  VALUE SPACES.
           05  WS-HD3-ALPHA-DT             PIC  X(18)  VALUE SPACES.
           05  FILLER                      PIC  X(44)  VALUE SPACES.
           05  FILLER                      PIC  X(05)  VALUE 'PAGE'.
           05  WS-HD3-PAGE                 PIC  ZZ,ZZ9.
           05  FILLER                      PIC  X(03)  VALUE SPACES.
     
       01  WS-HD4-1.
           05  FILLER                      PIC  X(01)  VALUE '0'.
           05  FILLER                      PIC  X(24)  VALUE
               '       REPRESENTATIVE - '.
           05  FILLER                      PIC  X(02)  VALUE SPACES.
           05  WS-HD4-1-AGENT-NAME         PIC  X(30)  VALUE SPACES.
           05  FILLER                      PIC  XX     VALUE SPACES.
           05  WS-HD4-1-CARRIER            PIC  X(01)  VALUE SPACES.
           05  WS-HD4-1-GROUP              PIC  X(06)  VALUE SPACES.
           05  FILLER                      PIC  X(01)  VALUE '-'.
           05  WS-HD4-1-AGENT              PIC  X(10)  VALUE SPACES.
           05  FILLER                      PIC  X(50)  VALUE SPACES.
     
       01  WS-HD4-2.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(26)  VALUE SPACES.
           05  WS-HD4-2-AGENT-ADDRESS1     PIC  X(30)  VALUE SPACES.
           05  FILLER                      PIC  X(71)  VALUE SPACES.
     
       01  WS-HD4-3.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(26)  VALUE SPACES.
           05  WS-HD4-3-AGENT-ADDRESS2     PIC  X(30)  VALUE SPACES.
           05  FILLER                      PIC  X(71)  VALUE SPACES.
     
       01  WS-HD4-4.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(26)  VALUE SPACES.
           05  WS-HD4-4-AGENT-ADDRESS3     PIC  X(30)  VALUE SPACES.
           05  FILLER                      PIC  X(71)  VALUE SPACES.
     
       01  WS-HD4-5.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(26)  VALUE SPACES.
           05  WS-HD4-5-AGENT-ADDRESS4     PIC  X(30)  VALUE SPACES.
           05  FILLER                      PIC  X(71)  VALUE SPACES.
     
       01  WS-HD5.
           05  FILLER                      PIC  X(01)  VALUE '0'.
           05  FILLER                      PIC  X(03)  VALUE SPACES.
           05  FILLER                      PIC  X(25)  VALUE
               'FINANCIAL INSTITUTION  - '.
           05  WS-HD5-BANK-NO              PIC  X(10)  VALUE SPACES.
           05  FILLER                      PIC  X(02)  VALUE SPACES.
           05  WS-HD5-BANK-NAME            PIC  X(30)  VALUE SPACES.
           05  FILLER                      PIC  X(74)  VALUE SPACES.

       01  WS-HD7A.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(33)  VALUE SPACES.
           05  FILLER                      PIC  X(08)  VALUE
               'CONTRACT'.
           05  FILLER                      PIC  X(04)  VALUE SPACES.
           05  FILLER                      PIC  X(09)  VALUE
               'EFFECTIVE'. 
           05  FILLER                      PIC  X(78)  VALUE SPACES.
 
       01  WS-HD8A.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(07)  VALUE SPACES.
           05  FILLER                      PIC  X(04)  VALUE
               'NAME'.
           05  FILLER                      PIC  X(23)  VALUE SPACES.
           05  FILLER                      PIC  X(06)  VALUE
               'NUMBER'.
           05  FILLER                      PIC  X(08)  VALUE SPACES.
           05  FILLER                      PIC  X(04)  VALUE
               'DATE'.
           05  FILLER                      PIC  X(06)  VALUE SPACES.
           05  FILLER                      PIC  X(03)  VALUE
               'AGE'.
           05  FILLER                      PIC  X(03)  VALUE SPACES.
           05  FILLER                      PIC  X(04)  VALUE
               'TERM'.
           05  FILLER                      PIC  X(04)  VALUE SPACES.
           05  FILLER                      PIC  X(04)  VALUE
               'TYPE'.
           05  FILLER                      PIC  X(04)  VALUE SPACES.
           05  FILLER                      PIC  X(12)  VALUE
      *        'FACE AMOUNT'.
               'LUMP SUM FEE'.
           05  FILLER                      PIC  X(06)  VALUE SPACES.
           05  FILLER                      PIC  X(12)  VALUE
               'ADDENDUM FEE'.
           05  FILLER                      PIC  X(22)  VALUE SPACES.

       01  WS-AGENT-TOTAL-LINE.
           05  FILLER                      PIC  X(01)  VALUE '-'.
           05  FILLER                      PIC  X(28)  VALUE
               '** REPRESENTATIVE TOTALS ** '.
           05  FILLER                      PIC  X(13)  VALUE
               'ISSUE COUNT: '.
           05  WS-BTOT-ISSUE-COUNT         PIC  ZZZ,ZZ9.
           05  FILLER                      PIC  X(03)  VALUE SPACES.
           05  FILLER                      PIC  X(14)  VALUE
               'CANCEL COUNT: '.
           05  WS-BTOT-CANCEL-COUNT        PIC  ZZZ,ZZ9.
           05  FILLER                      PIC  X(02)  VALUE SPACES.
           05  FILLER                      PIC  X(25)  VALUE
               'NET REPRESENTATIVE FEES: '.
           05  WS-BTOT-NET-BANK-FEES       PIC  Z,ZZZ,ZZ9.99-.
      *    05  FILLER                      PIC  X(8)  VALUE SPACES.

       01  WS-FIN-INST-TOTAL-LINE.
           05  FILLER                      PIC  X(01)  VALUE '-'.
           05  FILLER                      PIC  X(21)  VALUE
               '**     TOTALS    **  '.
           05  FILLER                      PIC  X(13)  VALUE
               'ISSUE COUNT: '.
           05  WS-DTOT-ISSUE-COUNT         PIC  ZZZ,ZZ9.
           05  FILLER                      PIC  X(03)  VALUE SPACES.
           05  FILLER                      PIC  X(14)  VALUE
               'CANCEL COUNT: '.
           05  WS-DTOT-CANCEL-COUNT        PIC  ZZZ,ZZ9.
           05  FILLER                      PIC  X(03)  VALUE SPACES.
           05  FILLER                      PIC  X(32)  VALUE
               'NET FINANCIAL INSTITUTION FEES: '.
           05  WS-DTOT-NET-BANK-FEES       PIC  Z,ZZZ,ZZ9.99-.
           05  FILLER                      PIC  X(8)  VALUE SPACES.


       01  WS-GRAND-TOTAL-LINE.
           05  FILLER                      PIC  X(01)  VALUE '-'.
           05  FILLER                      PIC  X(21)  VALUE
               '*** GRAND  TOTALS ***'.
           05  FILLER                      PIC  X(07)  VALUE SPACES.
           05  FILLER                      PIC  X(13)  VALUE
               'ISSUE COUNT: '.
           05  WS-GTOT-ISSUE-COUNT         PIC  ZZZ,ZZ9.
           05  FILLER                      PIC  X(05)  VALUE SPACES.
           05  FILLER                      PIC  X(14)  VALUE
               'CANCEL COUNT: '.
           05  WS-GTOT-CANCEL-COUNT        PIC  ZZZ,ZZ9.
           05  FILLER                      PIC  X(05)  VALUE SPACES.
           05  FILLER                      PIC  X(32)  VALUE
               'NET FINANCIAL INSTITUTION FEES: '.
           05  WS-GTOT-NET-BANK-FEES       PIC  Z,ZZZ,ZZ9.99-.
           05  FILLER                      PIC  X(8)  VALUE SPACES.


       01  WS-HDR-DASH-LINE.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(44)  VALUE
               '--------------------------------------------'.
           05  FILLER                      PIC  X(44)  VALUE
               '--------------------------------------------'.
           05  FILLER                      PIC  X(44)  VALUE
               '--------------------------------------------'.


       COPY ELCDATE.
       COPY ELCDTECX.
       COPY ELCDTEVR.

       PROCEDURE DIVISION.

      ******************************************************************
      ***        D A T E   C A R D   L O A D   R O U T I N E         ***
      ******************************************************************
     
           COPY ELCDTERX.
      ******************************************************************

           PERFORM 0400-OPEN-FILES       THRU 0400-EXIT
           PERFORM 0600-INITIALIZE       THRU 0600-EXIT

           SORT SORT-WORK ASCENDING KEY  SORT-KEY
               INPUT PROCEDURE 
                   0100-INPUT-ROUTINE    THRU 0100-EXIT
               OUTPUT PROCEDURE 
                   0200-OUTPUT-ROUTINE   THRU 0200-EXIT
      
           IF SORT-RETURN NOT = ZERO
               MOVE 'SORT FAILED'        TO WS-ABEND-MESSAGE
               MOVE SORT-RETURN          TO WS-RETURN-CODE
               PERFORM ABEND-PGM         THRU APS-EXIT
           END-IF

           PERFORM 0500-CLOSE-FILES      THRU 0500-EXIT

           DISPLAY ' EXTR RECORDS READ    '  EXTR-RECS-IN
           DISPLAY ' SORT RECORDS RELEASED '  WS-RELEASE-SORT-COUNT
           DISPLAY ' SORT RECORDS RETURNED '  WS-RETURN-SORT-COUNT
           DISPLAY ' WS LINE COUNT '  WS-LINE-COUNT
         
           GOBACK

           .
       0100-INPUT-ROUTINE.
            
           PERFORM 0110-PROCESS-EXTR     THRU 0110-EXIT
               UNTIL END-OF-EXTR

           .
       0100-EXIT.
           EXIT.


       0110-PROCESS-EXTR.

           IF (DE-REIN NOT = 'R')
              AND (DE-ISSUE OR DE-CANCEL)
              AND (DE-AH-TYPE NOT = '  ' AND '00')
              MOVE DE-AH-TYPE          TO CLAS-LOOK
              MOVE SPACE               TO WS-BENEFIT-CODE-SW
              MOVE CLAS-STARTA         TO CLAS-INDEXA
              PERFORM 0150-FIND-AH-BEN-CODE
                                       THRU 0150-EXIT UNTIL
                 AH-BEN-CODE-FOUND
092705        IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L'
                 MOVE +0               TO WS-BANK-CNT
                 PERFORM VARYING X1 FROM +1 BY +1 UNTIL
                    X1 > +10
                    IF DE-AGT-TYPE (X1) = 'B'
                       ADD +1          TO WS-BANK-CNT
                    END-IF
                 END-PERFORM
                 IF WS-BANK-CNT > +1
                    PERFORM 0111-BUILD-SORT-RECS
                                       THRU 0111-EXIT
                 END-IF
              END-IF
           END-IF

           PERFORM 0120-READ-EXTR      THRU 0120-EXIT
           .
       0110-EXIT.
           EXIT.

       0111-BUILD-SORT-RECS.
       
           PERFORM VARYING X1 FROM +1 BY +1 UNTIL
              DE-AGT-TYPE (X1) = 'B'
           END-PERFORM

           MOVE DE-AGT (X1)            TO WS-MAIN-BANK
           ADD +1                      TO X1
           MOVE +1                     TO Y1
           MOVE SPACES                 TO WS-ALL-SUB-BANKS

           PERFORM VARYING X1 FROM X1 BY +1 UNTIL
              X1 > +10
              IF DE-AGT-TYPE (X1) = 'B'
                 MOVE DE-AGT (X1)      TO WS-SUB-BANK (Y1)
                 MOVE X1               TO WS-SUB-BANK-LVL (Y1)
                 ADD +1                TO Y1
              END-IF
           END-PERFORM

           INITIALIZE                  WS-SORT-REC
           MOVE DE-CARRIER             TO WS-SORT-CARRIER    
           MOVE DE-GROUPING            TO WS-SORT-GROUP
           MOVE WS-MAIN-BANK           TO WS-SORT-BANK
           MOVE DE-CERT                TO WS-SORT-CERT

           MOVE DE-EFF                 TO WS-SORT-CERT-EFF-DT
           MOVE DE-LNAME               TO WS-SORT-LNAME      
           MOVE DE-FNAME               TO WS-SORT-FNAME      
           MOVE DE-INIT                TO WS-SORT-INIT       
           MOVE DE-AGE                 TO WS-SORT-AGE        
           MOVE DE-AH-TERM             TO WS-SORT-AH-TERM
           MOVE DE-AH-TYPE             TO WS-SORT-AH-TYPE
           MOVE DE-STATE               TO WS-SORT-STATE    

           IF DE-ISSUE
              MOVE '01'                TO WS-SORT-REC-TYPE
102704        MOVE DE-AH-PRM           TO WS-SORT-PRM-AMT
           ELSE
              MOVE '02'                TO WS-SORT-REC-TYPE
102704        MOVE DE-AH-PRM           TO WS-SORT-PRM-AMT
              MOVE DE-AH-CANC-DTE      TO WS-SORT-AH-CAN-DT
              MOVE DE-AH-RFND          TO WS-SORT-AH-REFUND
           END-IF

           PERFORM VARYING X1 FROM +1 BY +1 UNTIL
              (X1 > +8)
              OR (WS-SUB-BANK (X1) = SPACES)
              MOVE WS-SUB-BANK-LVL (X1) TO Y1

              MOVE WS-SUB-BANK (X1)    TO WS-SORT-AGENT
              IF DE-ISSUE
                 COMPUTE WS-SORT-BANK-FEE = DE-A-PC (Y1) * 1000
              ELSE
                 COMPUTE WS-CNC-FACT = DE-AH-RFND / DE-AH-PRM
                 COMPUTE WS-SORT-BANK-FEE = DE-A-PC (Y1) *
                    WS-CNC-FACT * 1000
              END-IF
                 
              MOVE SPACES              TO WS-SORT-REFUND-OPT-FLG
              RELEASE SORT-REC         FROM WS-SORT-REC
              ADD +1                   TO WS-RELEASE-SORT-COUNT
           END-PERFORM

           .
       0111-EXIT.
           EXIT.


       0120-READ-EXTR.

           READ EXTR-FILE-IN AT END
              SET END-OF-EXTR          TO TRUE
           END-READ

           IF NOT END-OF-EXTR
              ADD +1                   TO EXTR-RECS-IN
           END-IF

           .
       0120-EXIT.
           EXIT.

       0150-FIND-AH-BEN-CODE.

           IF CLAS-INDEXA GREATER CLAS-MAXA
               DISPLAY 'AH BENEFIT ' CLAS-LOOK ' NOT ON FILE'
               PERFORM ABEND-PGM          THRU APS-EXIT
           END-IF
     
           IF CLAS-I-BEN (CLAS-INDEXA) = CLAS-LOOK
               SET AH-BEN-CODE-FOUND   TO TRUE
           ELSE
               ADD +1                  TO  CLAS-INDEXA
           END-IF

           .
       0150-EXIT.
           EXIT.


       0200-OUTPUT-ROUTINE.

      *    PERFORM 0300-HDR1-TO-HDR5   THRU 0300-EXIT

           PERFORM 0215-RETURN-SORT    THRU 0215-EXIT

           IF WS-RETURN-SORT-COUNT NOT = ZEROS
              MOVE WS-SORT-AGENT       TO WS-PREV-AGENT
              MOVE WS-SORT-BANK        TO WS-PREV-BANK
              MOVE DTE-CLASIC-COMPANY-CD
                                       TO CO-COMPANY-CD
              MOVE WS-SORT-CARRIER     TO CO-CARRIER
              MOVE WS-SORT-GROUP       TO CO-GROUPING
              MOVE WS-SORT-BANK        TO CO-RESP-NO
              MOVE LOW-VALUES          TO CO-ACCOUNT
              MOVE 'B'                 TO CO-TYPE
              PERFORM 0295-GET-BANK-NAMES
                                       THRU 0295-EXIT
              MOVE DTE-CLASIC-COMPANY-CD
                                       TO CO-COMPANY-CD
              MOVE WS-SORT-CARRIER     TO CO-CARRIER
              MOVE WS-SORT-GROUP       TO CO-GROUPING
              MOVE WS-SORT-AGENT       TO CO-RESP-NO
              MOVE LOW-VALUES          TO CO-ACCOUNT
              MOVE 'G'                 TO CO-TYPE
              PERFORM 0290-GET-AGENT-NAMES
                                       THRU 0290-EXIT
              MOVE WS-SORT-CARRIER     TO WS-HD4-1-CARRIER
              MOVE WS-SORT-GROUP       TO WS-HD4-1-GROUP
              MOVE WS-SORT-AGENT       TO WS-HD4-1-AGENT

              PERFORM 0210-PROCESS-SORT THRU 0210-EXIT UNTIL
                 END-OF-SORT-FILE
      *       MOVE +0                  TO WS-LINE-COUNT
              PERFORM 0220-BANK-BREAK  THRU 0220-EXIT
              PERFORM 0230-AGENT-BREAK THRU 0230-EXIT
              PERFORM 0240-FINAL-BREAK THRU 0240-EXIT
           ELSE
              DISPLAY ' NO RECORDS TO PROCESS '
           END-IF

           . 
       0200-EXIT.
           EXIT.


       0210-PROCESS-SORT.

           IF WS-SORT-AGENT NOT = WS-PREV-AGENT
              PERFORM 0220-BANK-BREAK  THRU 0220-EXIT
              PERFORM 0230-AGENT-BREAK THRU 0230-EXIT
           ELSE
              IF WS-SORT-BANK NOT = WS-PREV-BANK
                 PERFORM 0220-BANK-BREAK
                                       THRU 0220-EXIT
                 MOVE WS-HD5           TO PRT
                 PERFORM 8800-PRINT-RTN
                                       THRU 8800-EXIT
              END-IF
           END-IF

           IF REC-ISSUE
              ADD WS-SORT-BANK-FEE     TO WS-BANK-BANK-FEE-TOTAL
              ADD +1                   TO WS-BANK-ISSUE-COUNT
  
              MOVE WS-SORT-INSURED-NAME
                                       TO WS-D1A-INSURED-NAME
              MOVE WS-SORT-CERT        TO WS-D1A-CERT   
              MOVE WS-SORT-CERT-EFF-DT (4:4) 
                                       TO WS-D1A-CERT-EFF-CCYY
              MOVE WS-SORT-CERT-EFF-DT (8:2) 
                                       TO WS-D1A-CERT-EFF-MM
              MOVE WS-SORT-CERT-EFF-DT (10:2) 
                                       TO WS-D1A-CERT-EFF-DD
              MOVE WS-SORT-AGE         TO WS-D1A-AGE
              MOVE WS-SORT-AH-TERM     TO WS-D1A-AH-TERM
              MOVE WS-SORT-AH-TYPE     TO WS-D1A-AH-TYPE
              MOVE WS-SORT-PRM-AMT     TO WS-D1A-PRM-AMT
              MOVE WS-SORT-BANK-FEE    TO WS-D1A-BANK-FEE
           ELSE
              SUBTRACT WS-SORT-BANK-FEE
                                       FROM WS-BANK-BANK-FEE-TOTAL
              ADD +1                   TO WS-BANK-CANCEL-COUNT

              MOVE WS-SORT-INSURED-NAME
                                       TO WS-D1B-INSURED-NAME
              MOVE WS-SORT-CERT        TO WS-D1B-CERT    
              MOVE WS-SORT-CERT-EFF-DT (4:4) 
                                       TO WS-D1B-CERT-EFF-CCYY
              MOVE WS-SORT-CERT-EFF-DT (8:2) 
                                       TO WS-D1B-CERT-EFF-MM
              MOVE WS-SORT-CERT-EFF-DT (10:2) 
                                       TO WS-D1B-CERT-EFF-DD
              MOVE WS-SORT-AH-CAN-DT (4:4) 
                                       TO WS-D1B-CERT-CAN-CCYY
              MOVE WS-SORT-AH-CAN-DT (8:2) 
                                       TO WS-D1B-CERT-CAN-MM
              MOVE WS-SORT-AH-CAN-DT (10:2) 
                                       TO WS-D1B-CERT-CAN-DD
              MOVE WS-SORT-AH-TYPE     TO WS-D1B-AH-TYPE
              MOVE WS-SORT-AH-REFUND   TO WS-D1B-REFUND-AMT
              MOVE WS-SORT-BANK-FEE    TO WS-D1B-REFUND-BKFEE
              MOVE WS-SORT-PRM-AMT     TO WS-D1B-PRM-AMT
           END-IF
 
      *    IF REC-ISSUE
      *       MOVE WS-DETAIL1A         TO PRT
      *    ELSE
      *       MOVE WS-DETAIL1B         TO PRT
      *    END-IF 

      *    PERFORM 8800-PRINT-RTN      THRU 8800-EXIT

           PERFORM 0215-RETURN-SORT    THRU 0215-EXIT

           . 
       0210-EXIT.
           EXIT.

       0215-RETURN-SORT.

           RETURN SORT-WORK            INTO WS-SORT-REC AT END
              SET END-OF-SORT-FILE     TO TRUE
           END-RETURN

           IF NOT END-OF-SORT-FILE
              ADD +1                   TO WS-RETURN-SORT-COUNT
           END-IF

           .
       0215-EXIT.
           EXIT.

       0220-BANK-BREAK.

           MOVE WS-BANK-ISSUE-COUNT  TO WS-DTOT-ISSUE-COUNT
           MOVE WS-BANK-CANCEL-COUNT TO WS-DTOT-CANCEL-COUNT
           MOVE WS-BANK-BANK-FEE-TOTAL
                                       TO WS-DTOT-NET-BANK-FEES

           MOVE WS-FIN-INST-TOTAL-LINE TO PRT
           PERFORM 8800-PRINT-RTN      THRU 8800-EXIT

           ADD WS-BANK-ISSUE-COUNT   TO WS-AGENT-ISSUE-COUNT
           ADD WS-BANK-CANCEL-COUNT  TO WS-AGENT-CANCEL-COUNT
           ADD WS-BANK-BANK-FEE-TOTAL
                                       TO WS-AGENT-BANK-FEE-TOTAL
           MOVE ZEROS                  TO WS-BANK-ISSUE-COUNT
           MOVE ZEROS                  TO WS-BANK-CANCEL-COUNT
           MOVE ZEROS                  TO WS-BANK-BANK-FEE-TOTAL
           MOVE WS-SORT-BANK           TO WS-PREV-BANK
           MOVE WS-SORT-BANK           TO WS-HD5-BANK-NO

           MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD
           MOVE WS-SORT-CARRIER        TO CO-CARRIER
           MOVE WS-SORT-GROUP          TO CO-GROUPING
           MOVE WS-SORT-BANK           TO CO-RESP-NO
           MOVE LOW-VALUES             TO CO-ACCOUNT
           MOVE 'B'                    TO CO-TYPE
           PERFORM 0295-GET-BANK-NAMES THRU 0295-EXIT

           .
       0220-EXIT.
           EXIT.


       0230-AGENT-BREAK.

           MOVE WS-AGENT-ISSUE-COUNT   TO WS-BTOT-ISSUE-COUNT
           MOVE WS-AGENT-CANCEL-COUNT  TO WS-BTOT-CANCEL-COUNT
           MOVE WS-AGENT-BANK-FEE-TOTAL
                                       TO WS-BTOT-NET-BANK-FEES

           MOVE WS-AGENT-TOTAL-LINE    TO PRT
           PERFORM 8800-PRINT-RTN      THRU 8800-EXIT

           ADD WS-AGENT-ISSUE-COUNT    TO WS-ALL-ISSUE-COUNT
           ADD WS-AGENT-CANCEL-COUNT   TO WS-ALL-CANCEL-COUNT
           ADD WS-AGENT-BANK-FEE-TOTAL TO WS-ALL-BANK-FEE-TOTAL
           MOVE ZEROS                  TO WS-AGENT-ISSUE-COUNT
           MOVE ZEROS                  TO WS-AGENT-CANCEL-COUNT
           MOVE ZEROS                  TO WS-AGENT-BANK-FEE-TOTAL

           MOVE WS-SORT-CARRIER        TO WS-HD4-1-CARRIER
           MOVE WS-SORT-GROUP          TO WS-HD4-1-GROUP
           MOVE WS-SORT-AGENT          TO WS-HD4-1-AGENT
                                          WS-PREV-AGENT   
           MOVE WS-SORT-CARRIER        TO CO-CARRIER
           MOVE WS-SORT-GROUP          TO CO-GROUPING
           MOVE WS-SORT-AGENT          TO CO-RESP-NO
           MOVE LOW-VALUES             TO CO-ACCOUNT
           MOVE 'G'                    TO CO-TYPE

           READ ERCOMP 
               
           EVALUATE ERCOMP-STATUS
              WHEN '00' 
                 MOVE CO-ACCT-NAME       TO WS-HD4-1-AGENT-NAME
                 MOVE CO-ADDR-1          TO WS-HD4-2-AGENT-ADDRESS1
                 IF CO-ADDR-2 NOT = SPACES
                    MOVE CO-ADDR-2      TO WS-HD4-3-AGENT-ADDRESS2
051810              MOVE SPACES         TO WS-HD4-4-AGENT-ADDRESS3
051810              STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810                 DELIMITED BY '  ' INTO WS-HD4-4-AGENT-ADDRESS3
051810              END-STRING
                    MOVE CO-ZIP         TO WS-HD4-5-AGENT-ADDRESS4
                 ELSE
051810              MOVE SPACES         TO WS-HD4-3-AGENT-ADDRESS2
051810              STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810                 DELIMITED BY '  ' INTO WS-HD4-3-AGENT-ADDRESS2
051810              END-STRING
                    MOVE CO-ZIP         TO WS-HD4-4-AGENT-ADDRESS3
                    MOVE SPACES         TO WS-HD4-5-AGENT-ADDRESS4
                 END-IF
           
              WHEN '23'
                 DISPLAY 'THIS AGENT NOT ON ERCOMP ' WS-SORT-AGENT

              WHEN OTHER
                 DISPLAY 'ERROR ON ERCOMP READ ' ERCOMP-STATUS
           END-EVALUATE

           MOVE +56                    TO WS-LINE-COUNT

           .
       0230-EXIT.
           EXIT.

       0240-FINAL-BREAK.

           MOVE +0                     TO WS-LINE-COUNT
           ADD +1                      TO WS-PAGE
           MOVE WS-PAGE                TO WS-HD3-PAGE
           MOVE WS-HD1                 TO PRT
           PERFORM 8900-PRINT-A-LINE   THRU 8900-EXIT
           MOVE WS-HD2                 TO PRT
           PERFORM 8900-PRINT-A-LINE   THRU 8900-EXIT
           MOVE WS-HD3                 TO PRT
           PERFORM 8900-PRINT-A-LINE   THRU 8900-EXIT
           MOVE WS-ALL-ISSUE-COUNT     TO WS-GTOT-ISSUE-COUNT
           MOVE WS-ALL-CANCEL-COUNT    TO WS-GTOT-CANCEL-COUNT
           MOVE WS-ALL-BANK-FEE-TOTAL  TO WS-GTOT-NET-BANK-FEES

           MOVE WS-GRAND-TOTAL-LINE    TO PRT
           PERFORM 8800-PRINT-RTN      THRU 8800-EXIT

           .
       0240-EXIT.
           EXIT.
           
       0290-GET-AGENT-NAMES.

           READ ERCOMP

           IF ERCOMP-STATUS = '00'
              MOVE CO-ACCT-NAME       TO WS-HD4-1-AGENT-NAME
              MOVE CO-ADDR-1          TO WS-HD4-2-AGENT-ADDRESS1
              IF CO-ADDR-2 NOT = SPACES
                 MOVE CO-ADDR-2      TO WS-HD4-3-AGENT-ADDRESS2
051810           MOVE SPACES         TO WS-HD4-4-AGENT-ADDRESS3
051810           STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810              DELIMITED BY '  ' INTO WS-HD4-4-AGENT-ADDRESS3
051810           END-STRING
                 MOVE CO-ZIP         TO WS-HD4-5-AGENT-ADDRESS4
              ELSE
051810           MOVE SPACES         TO WS-HD4-3-AGENT-ADDRESS2
051810           STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810              DELIMITED BY '  ' INTO WS-HD4-3-AGENT-ADDRESS2
051810           END-STRING
                 MOVE CO-ZIP         TO WS-HD4-4-AGENT-ADDRESS3
                 MOVE SPACES         TO WS-HD4-5-AGENT-ADDRESS4
              END-IF
           ELSE
              IF ERCOMP-STATUS = '10' OR '23'
                 MOVE 'COMPUTER GENERATED '
                                       TO WS-HD4-1-AGENT-NAME
                                          WS-HD4-2-AGENT-ADDRESS1
                                          WS-HD4-3-AGENT-ADDRESS2
                 MOVE SPACES           TO WS-HD4-4-AGENT-ADDRESS3
                                          WS-HD4-5-AGENT-ADDRESS4
              ELSE
                 DISPLAY 'ERROR ON ERCOMP GET BANK  ' ERCOMP-STATUS
                 DISPLAY 'CO CONTROL PRIMARY ' CO-CONTROL-PRIMARY 
                 PERFORM ABEND-PGM        THRU APS-EXIT
              END-IF
           END-IF

 
           . 
       0290-EXIT.
           EXIT.

       0295-GET-BANK-NAMES.

           READ ERCOMP

           IF ERCOMP-STATUS = '00'
              MOVE CO-ACCT-NAME        TO WS-HD5-BANK-NAME
              MOVE CO-RESP-NO          TO WS-HD5-BANK-NO
           ELSE
              IF ERCOMP-STATUS = '10' OR '23'
                 MOVE 'COMPUTER GENERATED '
                                       TO WS-HD5-BANK-NAME
                 MOVE CO-RESP-NO       TO WS-HD5-BANK-NO
              ELSE
                 DISPLAY 'ERROR ON ERCOMP READ BANK ' ERCOMP-STATUS
                 DISPLAY 'CO CONTROL PRIMARY ' CO-CONTROL-PRIMARY 
                 PERFORM ABEND-PGM        THRU APS-EXIT
              END-IF
           END-IF
           

 
           . 
       0295-EXIT.
           EXIT.


       0300-HDR1-TO-HDR5.

           ADD +1                      TO WS-PAGE
           MOVE WS-PAGE                TO WS-HD3-PAGE
           MOVE WS-HD1                 TO PRT
           PERFORM 8900-PRINT-A-LINE   THRU 8900-EXIT
           MOVE WS-HD2                 TO PRT
           PERFORM 8900-PRINT-A-LINE   THRU 8900-EXIT
           MOVE WS-HD3                 TO PRT
           PERFORM 8900-PRINT-A-LINE   THRU 8900-EXIT
           MOVE WS-HD4-1               TO PRT
           PERFORM 8900-PRINT-A-LINE   THRU 8900-EXIT
           MOVE WS-HD4-2               TO PRT
           PERFORM 8900-PRINT-A-LINE   THRU 8900-EXIT
           MOVE WS-HD4-3               TO PRT
           PERFORM 8900-PRINT-A-LINE   THRU 8900-EXIT
           MOVE WS-HD4-4               TO PRT
           PERFORM 8900-PRINT-A-LINE   THRU 8900-EXIT
           MOVE WS-HD4-5               TO PRT
           PERFORM 8900-PRINT-A-LINE   THRU 8900-EXIT
           MOVE WS-HD5                 TO PRT
           PERFORM 8900-PRINT-A-LINE   THRU 8900-EXIT

           MOVE +12                    TO WS-LINE-COUNT

           . 
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT EXTR-FILE-IN
                      ERCOMP
               OUTPUT PRNTR

           IF ERCOMP-STATUS = '00' OR '97'   
               CONTINUE        
           ELSE               
               DISPLAY 'OPEN ERROR ' ERCOMP-STATUS ' ON ERCOMP' 
               DISPLAY '*** JOB ABORTED ***'
               PERFORM ABEND-PGM     THRU APS-EXIT
           END-IF             

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE EXTR-FILE-IN
                 ERCOMP 
                 PRNTR

           IF ERCOMP-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON CLOSE - ERCOMP ' ERCOMP-STATUS
              PERFORM ABEND-PGM      THRU APS-EXIT  
           END-IF

           .
       0500-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE COMPANY-NAME           TO WS-HD2-CO

           ACCEPT WS-ACCEPT-DATE       FROM DATE
           MOVE WS-AD-MM               TO WS-CD-MM
           MOVE WS-AD-DD               TO WS-CD-DD
           MOVE WS-AD-YY               TO WS-CD-YY
           MOVE WS-CURRENT-DATE        TO WS-HD2-RUN-DT

           MOVE ALPH-DATE              TO WS-HD3-ALPHA-DT
     
           MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD
           
           MOVE ZEROS                  TO WS-DTOT-NET-BANK-FEES
           MOVE ZEROS                  TO WS-BTOT-NET-BANK-FEES
           MOVE ZEROS                  TO WS-GTOT-NET-BANK-FEES
           MOVE +0                     TO WS-PAGE   

           PERFORM 0120-READ-EXTR      THRU 0120-EXIT
           .
       0600-EXIT.
           EXIT.

       8800-PRINT-RTN.

           IF WS-LINE-COUNT > WS-LINE-COUNT-MAX
              MOVE PRT                 TO WS-SAVE-PRT
              PERFORM 0300-HDR1-TO-HDR5
                                       THRU 0300-EXIT
              MOVE WS-SAVE-PRT         TO PRT
           END-IF
           
           PERFORM 8900-PRINT-A-LINE   THRU 8900-EXIT
           ADD +1                      TO WS-LINE-COUNT
           MOVE SPACES                 TO PRT

           .
       8800-EXIT.
           EXIT.
           
       8900-PRINT-A-LINE.

           MOVE P-CTL              TO X.
                                   COPY ELCPRT2.

       8900-EXIT.
           EXIT.

      *8510-DATE-CONVERSION.

      *    CALL 'ELDATCX' USING DATE-CONVERSION-DATA

      *    .

      *8590-EXIT.
      *    EXIT.

       ABEND-PGM SECTION.
                                     COPY ELCABEND.
