       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCFX4.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELCNTL           ASSIGN TO ELCNTL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CF-CONTROL-PRIMARY
                                   FILE STATUS IS ELCNTL-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT ELCNTL-OUT       ASSIGN TO ELCNTLOT
               ORGANIZATION IS LINE SEQUENTIAL.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ELCNTL.

           COPY ELCCNTL.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ELCNTL-OUT
           RECORDING MODE V
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  CF-USER-REC1                PIC X(95).
       01  CF-USER-REC2                PIC X(44).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMCFX4  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-ELCNTL                 VALUE 'Y'.
       77  ELCNTL-RECS-IN              PIC 9(9) VALUE ZEROS.
       77  ELCNTL-RECS-OUT             PIC 9(9) VALUE ZEROS.
       77  S1                          PIC S999  VALUE +0 COMP-3.
       77  S2                          PIC S999  VALUE +0 COMP-3.
       77  M1                          PIC S999  VALUE +0 COMP-3.
       77  PGM-SUB                     PIC S9(5) COMP-3 VALUE +515.
       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WS-EX-INIT-REC1            PIC X(95) VALUE LOW-VALUES.
       01  EX-USER-REC1.
           12  EX1-COMP-ID            PIC XXX.
           12  EX1-TAB1               PIC X.
           12  EX1-USER-ID            PIC X(4).
           12  EX1-TAB2               PIC X.
           12  EX1-USER-PASSWORD      PIC X(11).
           12  EX1-TAB3               PIC X.
           12  EX1-USER-NAME          PIC X(30).
           12  EX1-TAB4               PIC X.
           12  EX1-USER-TITLE         PIC X(26).
           12  EX1-TAB5               PIC X.
           12  EX1-LAST-MAINT-DT      PIC X(10).
           12  EX1-TAB6               PIC X.
           12  EX1-CR-FORCE           PIC X.
           12  EX1-TAB7               PIC X.
           12  EX1-CL-FORCE           PIC X.
           12  EX1-TAB8               PIC X.
           12  EX1-EOR                PIC X.

       01  WS-EX-INIT-REC2            PIC X(44) VALUE LOW-VALUES.
       01  EX-USER-REC2.
           12  F                      PIC X.
           12  EX2-TAB1               PIC X.
           12  F                      PIC X.
           12  EX2-TAB2               PIC X.
           12  F                      PIC X.
           12  EX2-TAB3               PIC X.
           12  EX2-REC-TYPE           PIC X(6).
           12  EX2-TAB4               PIC X.
           12  EX2-SYS-DESC           PIC X(25).
           12  EX2-TAB5               PIC X.
           12  EX2-APP-B              PIC X.
           12  EX2-TAB6               PIC X.
           12  EX2-APP-U              PIC X.
           12  EX2-TAB7               PIC X.
           12  EX2-EOR                PIC X.



       01  CREDIT-DESCRIPT-TABLE.
           05  CR-001 PIC X(25) VALUE 'END USER REPORTING       '.
           05  CR-002 PIC X(25) VALUE 'PROGRAM OPTIONS          '.
           05  CR-003 PIC X(25) VALUE 'TEXT FILE (LETTERS FORMS)'.
           05  CR-004 PIC X(25) VALUE 'ACCOUNT MASTERS          '.
           05  CR-005 PIC X(25) VALUE 'COMPENSATION MASTERS     '.
           05  CR-006 PIC X(25) VALUE 'RATE MASTERS             '.
           05  CR-007 PIC X(25) VALUE 'REINSURANCE MASTERS      '.
           05  CR-008 PIC X(25) VALUE 'COMMISSION TABLES        '.
           05  CR-009 PIC X(25) VALUE 'MORTALITY TABLE CONTROLS '.
           05  CR-010 PIC X(25) VALUE 'LOAN OFFICERS            '.
           05  CR-011 PIC X(25) VALUE 'DATA ENTRY               '.
           05  CR-012 PIC X(25) VALUE 'REVIEW AND CORRECTION    '.
           05  CR-013 PIC X(25) VALUE 'FULL FILE EDIT           '.
           05  CR-014 PIC X(25) VALUE 'CLAIMS AND RESERVES      '.
           05  CR-015 PIC X(25) VALUE 'COMPENSATIONS (PYMT/ADJ) '.
           05  CR-016 PIC X(25) VALUE 'RETRO/REINS   (PYMT/ADJ) '.
           05  CR-017 PIC X(25) VALUE 'CHECK MAINTENANCE(CREDIT)'.
           05  CR-018 PIC X(25) VALUE 'ACCOUNT STATEMENTS       '.
           05  CR-019 PIC X(25) VALUE 'GENERAL AGENT STATEMENTS '.
           05  CR-020 PIC X(25) VALUE 'LOSS RATIO SELECTION     '.
           05  CR-021 PIC X(25) VALUE 'ONLINE STATEMENT PRINTING'.
           05  CR-022 PIC X(25) VALUE 'CHECKS TO PRINT  (CREDIT)'.
           05  CR-023 PIC X(25) VALUE 'CHECK RELEASE    (CREDIT)'.
           05  CR-024 PIC X(25) VALUE 'PRINT RELEASED CHECKS    '.
           05  CR-025 PIC X(25) VALUE 'ACCOUNT NOTEPAD          '.
           05  CR-026 PIC X(25) VALUE 'RETRO MASTER      (EL606)'.
           05  CR-027 PIC X(25) VALUE 'RETRO HISTORY     (EL607)'.
           05  CR-028 PIC X(25) VALUE 'BANK MASTER MAINTENANCE  '.
           05  CR-029 PIC X(25) VALUE 'STATE CONTROLS (EL106A)  '.
           05  CR-030 PIC X(25) VALUE 'REPORT CUSTOMIZATION 604A'.
           05  CR-031 PIC X(25) VALUE 'CERTIFICATE LOOK-UP      '.
           05  CR-032 PIC X(25) VALUE 'CERTIFICATE NOTES        '.
           05  CR-033 PIC X(25) VALUE 'CERTIFICATE CHANGES      '.
           05  CR-034 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-035 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-036 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-037 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-038 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-039 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-040 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-041 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-042 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-043 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-044 PIC X(25) VALUE '****** FUTURE USE  ******'.
       01  FILLER REDEFINES CREDIT-DESCRIPT-TABLE.
           05  FILLER OCCURS 44.
               10  CREDIT-DES          PIC X(25).


       01  CLAIM-DESCRIPT-TABLE.
           05  CL-001 PIC X(25) VALUE 'NEW CLAIM SETUP          '.
           05  CL-002 PIC X(25) VALUE 'RECORD MAIL RECEIVED     '.
           05  CL-003 PIC X(25) VALUE 'CLAIM AUDIT              '.
           05  CL-004 PIC X(25) VALUE 'BENEFICIARY MASTER       '.
           05  CL-005 PIC X(25) VALUE 'CLAIM MAINTENANCE        '.
           05  CL-006 PIC X(25) VALUE 'DENIAL PROCESSING        '.
           05  CL-007 PIC X(25) VALUE 'CLAIMS LETTER WRITER     '.
           05  CL-008 PIC X(25) VALUE 'NOTE / REMINDER RECORDING'.
           05  CL-009 PIC X(25) VALUE 'SETUP AUTOMATIC PAYMENT  '.
           05  CL-010 PIC X(25) VALUE 'PAYMENT PROCESSING       '.
           05  CL-011 PIC X(25) VALUE 'CHECKS TO PRINT  (CLAIMS)'.
           05  CL-012 PIC X(25) VALUE 'CHECK RELEASE    (CLAIMS)'.
           05  CL-013 PIC X(25) VALUE 'PRINT RELEASED CHECKS    '.
           05  CL-014 PIC X(25) VALUE 'CLAIM ADDRESS MAINTENANCE'.
           05  CL-015 PIC X(25) VALUE 'CLAIM TRAILER MAINTENANCE'.
           05  CL-016 PIC X(25) VALUE 'CLAIM STATUS/DISPOSITION '.
           05  CL-017 PIC X(25) VALUE 'SUPERVISOR REQUEST REPORT'.
           05  CL-018 PIC X(25) VALUE 'FILE FOLDER LABEL PRINT  '.
           05  CL-019 PIC X(25) VALUE 'CLAIMS STATUS PRINT      '.
           05  CL-020 PIC X(25) VALUE 'LETTER/ADDR LABEL PRINT  '.
           05  CL-021 PIC X(25) VALUE 'CLAIM LOOK-UP            '.
           05  CL-022 PIC X(25) VALUE 'REVIEW PENDING ACTIVITY  '.
           05  CL-023 PIC X(25) VALUE 'CHECK RECON      (EL146) '.
           05  CL-024 PIC X(25) VALUE 'POLICY FORM MSTR (EL1582)'.
           05  CL-025 PIC X(25) VALUE 'AUTO ACT MAINT   (EL145) '.
           05  CL-026 PIC X(25) VALUE 'INITIAL/PROG FORMS PRINT '.
           05  CL-027 PIC X(25) VALUE 'ONLINE REPORTS           '.
           05  CL-028 PIC X(25) VALUE 'PAYMENT APPROVAL         '.
           05  CL-029 PIC X(25) VALUE 'DENIAL MAINTENANCE       '.
           05  CL-030 PIC X(25) VALUE 'REJECT MAINTENANCE       '.
           05  CL-031 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-032 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-033 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-034 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-035 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-036 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-037 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-038 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-039 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-040 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-041 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-042 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-043 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-044 PIC X(25) VALUE '****** FUTURE USE  ******'.
       01  FILLER REDEFINES CLAIM-DESCRIPT-TABLE.
           05  FILLER OCCURS 44.
               10  CLAIM-DES           PIC X(25).


      ******************************************************************
       01  WS-MISC.
           05  WS-CNTR                 PIC 9(4) VALUE ZEROS.
           05  ELCNTL-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 4000-OPEN-FILES     THRU 4000-EXIT

           PERFORM 5020-INITIALIZE     THRU 5020-EXIT

           PERFORM 0100-PROCESS-ELCNTL THRU 0100-EXIT UNTIL
              (END-OF-ELCNTL)
PEMTST*       OR (CLM-RECS-IN > 1000)

      *    MOVE WS-EX2-INIT            TO EX-USER-REC
      *    MOVE 'TOTAL COUNT '         TO EX2-USER-NAME
      *    MOVE WS-CNTR                TO EX2-CNT
      *    PERFORM 0300-WRITE-ELCNTL   THRU 0300-EXIT

           PERFORM 5000-CLOSE-FILES    THRU 5000-EXIT

           DISPLAY ' ELCNTL RECORDS READ    '  ELCNTL-RECS-IN
           DISPLAY ' ELCNTL RECORDS WRITTEN '  ELCNTL-RECS-OUT
           GOBACK

           .
       0100-PROCESS-ELCNTL.

           EVALUATE CF-RECORD-TYPE
              WHEN '2'
                 PERFORM 0400-BUILD-USER-REC
                                       THRU 0400-EXIT
           END-EVALUATE
                            
           PERFORM 0200-READ-ELCNTL    THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-ELCNTL.

           READ ELCNTL NEXT RECORD

           IF ELCNTL-FILE-STATUS = '10' OR '23'
              SET END-OF-ELCNTL        TO TRUE
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCNTL READ NEXT ' ELCNTL-FILE-STATUS
                 SET END-OF-ELCNTL     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ELCNTL
              ADD 1 TO ELCNTL-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-REC1.

              INSPECT EX-USER-REC1 REPLACING
                 ALL ';'               BY ' '
                 ALL X'00'             BY ' '
                 ALL X'09'             BY ';'

           WRITE CF-USER-REC1          FROM EX-USER-REC1
           ADD 1 TO ELCNTL-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0350-WRITE-REC2.

           WRITE CF-USER-REC2          FROM EX-USER-REC2
           ADD 1 TO ELCNTL-RECS-OUT

           .
       0350-EXIT.
           EXIT.

       0400-BUILD-USER-REC.

           IF CF-SEQUENCE-NO NOT = +0
              GO TO 0400-EXIT
           END-IF

           PERFORM 0450-BUILD-REC1     THRU 0450-EXIT
           PERFORM 0300-WRITE-REC1     THRU 0300-EXIT
           
           IF ACCESS-TO-CREDIT
              MOVE +1               TO S1
              MOVE +33              TO M1
              PERFORM 0500-BUILD    THRU 0500-EXIT
           END-IF

           IF ACCESS-TO-CLAIMS
              MOVE +2               TO S1
              MOVE +30              TO M1
              PERFORM 0500-BUILD    THRU 0500-EXIT
           END-IF

           .
       0400-EXIT.
           EXIT.

       0450-BUILD-REC1.

           MOVE WS-EX-INIT-REC1     TO EX-USER-REC1
           MOVE CF-COMPANY-ID       TO EX1-COMP-ID
           MOVE CF-PROCESSOR        TO EX1-USER-ID
      *    MOVE CF-PROCESSOR-PASSWORD
      *                             TO EX1-USER-PASSWORD
           MOVE CF-PROCESSOR-NAME   TO EX1-USER-NAME
           MOVE CF-PROCESSOR-TITLE  TO EX1-USER-TITLE
           MOVE CF-LAST-MAINT-DT    TO DC-BIN-DATE-1
           MOVE ' '                 TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                    THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT
                                    TO EX1-LAST-MAINT-DT
           END-IF

           MOVE CF-APPLICATION-FORCE (1) TO EX1-CR-FORCE
           MOVE CF-APPLICATION-FORCE (2) TO EX1-CL-FORCE

           .
       0450-EXIT.
           EXIT.


       0500-BUILD.

           PERFORM VARYING S2 FROM +1 BY +1 UNTIL
              S2 > M1
              ADD 1                       TO WS-CNTR
              MOVE WS-EX-INIT-REC2        TO EX-USER-REC2
              IF S1 = +1
                 MOVE 'CREDIT'            TO EX2-REC-TYPE
                 MOVE CREDIT-DES (S2)     TO EX2-SYS-DESC
              ELSE
                 MOVE 'CLAIMS'            TO EX2-REC-TYPE
                 MOVE CLAIM-DES (S2)      TO EX2-SYS-DESC
              END-IF
   
              MOVE CF-BROWSE-APP (S1 S2)  TO EX2-APP-B
              MOVE CF-UPDATE-APP (S1 S2)  TO EX2-APP-U
   
              INSPECT EX-USER-REC2 REPLACING
                 ALL ';'               BY ' '
                 ALL X'00'             BY ' '
                 ALL X'09'             BY ';'

              PERFORM 0350-WRITE-REC2  THRU 0350-EXIT
   
              ADD 1 TO ELCNTL-RECS-OUT
           END-PERFORM

           .
       0500-EXIT.
           EXIT.

       4000-OPEN-FILES.

           OPEN INPUT ELCNTL
               OUTPUT ELCNTL-OUT

           .
       4000-EXIT.
           EXIT.

       5000-CLOSE-FILES.

           CLOSE ELCNTL ELCNTL-OUT

           .
       5000-EXIT.
           EXIT.

       5010-START-ELCNTL.

           MOVE LOW-VALUES             TO CF-CONTROL-PRIMARY
           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '2'                    TO CF-RECORD-TYPE
           START ELCNTL KEY >= CF-CONTROL-PRIMARY

           IF ELCNTL-FILE-STATUS = '10' OR '23'
              SET END-OF-ELCNTL        TO TRUE
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCNTL START     ' ELCNTL-FILE-STATUS
                 SET END-OF-ELCNTL     TO TRUE
              END-IF
           END-IF

           .
       5010-EXIT.
           EXIT.


       5020-INITIALIZE.

           MOVE SPACES                 TO EX-USER-REC1
           MOVE X'09'                  TO EX1-TAB1
                                          EX1-TAB2
                                          EX1-TAB3
                                          EX1-TAB4
                                          EX1-TAB5
                                          EX1-TAB6
                                          EX1-TAB7
                                          EX1-TAB8

           MOVE ' '                    TO EX1-EOR
           MOVE EX-USER-REC1           TO WS-EX-INIT-REC1

           MOVE SPACES                 TO EX-USER-REC2
           MOVE X'09'                  TO EX2-TAB1
                                          EX2-TAB2
                                          EX2-TAB3
                                          EX2-TAB4
                                          EX2-TAB5
                                          EX2-TAB6
                                          EX2-TAB7

           MOVE ' '                    TO EX2-EOR
           MOVE EX-USER-REC2           TO WS-EX-INIT-REC2

           PERFORM 5010-START-ELCNTL   THRU 5010-EXIT
           PERFORM 0200-READ-ELCNTL    THRU 0200-EXIT

           .
       5020-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
