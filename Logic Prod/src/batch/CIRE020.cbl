       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIRE020.
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EL562-FICHE-IN   ASSIGN TO SYS010.

           SELECT DISK-DATE        ASSIGN TO SYS019.
                                                                        
           SELECT ERRPTC           ASSIGN TO ERRPTC
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS RA-CONTROL-PRIMARY
                                   FILE STATUS IS ERRPTC-FILE-STATUS.

           SELECT EL562-FICHE-OUT  ASSIGN TO SYS011.

       DATA DIVISION.
       FILE SECTION.

       FD  EL562-FICHE-IN
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

       01  EL562-IN-REC.
           05  EL562-CTL               PIC X.
           05  EL562-DATA              PIC X(132).

       FD  DISK-DATE                                                    
                                   COPY ELCDTEFD.                       
       EJECT                                                            
       FD  EL562-FICHE-OUT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

       01  EL562-OUT-REC           PIC X(162).

       FD  ERRPTC.

           COPY ERCRPTC.

       WORKING-STORAGE SECTION.

       77  ERRPTC-FILE-STATUS          PIC XX    VALUE '00'.
       77  WS-REPORT-CODE-1            PIC X(10) VALUE SPACES.
       77  WS-REPORT-CODE-2            PIC X(10) VALUE SPACES.
       77  WS-FICHE-RECS-IN            PIC 9(9)  VALUE ZEROS.
       77  WS-FICHE-RECS-OUT           PIC 9(9)  VALUE ZEROS.
       77  WS-ERRPTC-RECS-IN           PIC 9(9)  VALUE ZEROS.
       77  WS-PREV-ERRPTC-KEY          PIC X(28) VALUE LOW-VALUES.
       77  WS-EOF-SW                   PIC X     VALUE ' '.
           88  END-OF-FICHE                      VALUE 'Y'.

       01  WS-ABEND-AREA.
           05  PGM-SUB                 PIC S999 COMP  VALUE +158.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  HEAD-1                      PIC X(133).
       01  HEAD-2                      PIC X(133).
       01  HEAD-3                      PIC X(133).
       01  HEAD-4.
           05  FILLER                  PIC X.
           05  HEAD-4-ACCT-NO          PIC X(10).
           05  FILLER                  PIC X(4).
           05  FICHE-CARRIER           PIC X(1).
           05  FICHE-GROUPING          PIC X(6).
           05  FILLER                  PIC X.
           05  FICHE-ACCOUNT           PIC X(10).
           05  FILLER                  PIC X(56).
           05  FICHE-FIN-RESP          PIC X(10).
           05  FILLER                  PIC X(27).           
       
       01  WS-RECORD-OUT.
           05  WS-REC-RPTCDE1          PIC X(10).
           05  WS-REC-SEQNO            PIC 9(9)   VALUE ZEROS.
           05  WS-REC-RPTCDE2          PIC X(10).
           05  WS-REC-REST             PIC X(133).

       01  WORK.
           05  SEL-SW              PIC X       VALUE SPACE.
           05  RPT2-SW             PIC X       VALUE 'N'.
           05  SYS014-STATUS       PIC X(2)    VALUE SPACE.
           05  S0C7                PIC S9      COMP-3.
           05  SAVE-COMPANY-NAME   PIC X(30)   VALUE SPACE.

                                   COPY ELCDTECX.                       

                                   COPY ELCDTEVR.                       

       PROCEDURE DIVISION.

       0000-MAINLINE.

                                       COPY ELCDTERX.                       

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 1000-PROCESS        THRU 1000-EXIT UNTIL
              END-OF-FICHE
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT

           DISPLAY ' FICHE IN RECS   ' WS-FICHE-RECS-IN
           DISPLAY ' FICHE OUT RECS  ' WS-FICHE-RECS-OUT
           DISPLAY ' REPORT CODES IN ' WS-ERRPTC-RECS-IN
           
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

           OPEN INPUT  EL562-FICHE-IN
                       ERRPTC
                OUTPUT EL562-FICHE-OUT

           IF ERRPTC-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY '*** ERROR OPENING ERRPTC FILE ***'
              DISPLAY '*** STATUS CODE IS ' ERRPTC-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           PERFORM 0300-READ-FICHE     THRU 0300-EXIT

           .
       0200-EXIT.
           EXIT.

       0300-READ-FICHE.
       
           READ EL562-FICHE-IN AT END
              SET END-OF-FICHE         TO TRUE
           END-READ
           
           IF NOT END-OF-FICHE
              ADD 1                    TO WS-FICHE-RECS-IN
           END-IF
           
           .
       0300-EXIT.
           EXIT.
           
       1000-PROCESS.
       
           IF EL562-CTL = '1'
              MOVE EL562-IN-REC        TO HEAD-1
              PERFORM 0300-READ-FICHE  THRU 0300-EXIT
              MOVE EL562-IN-REC        TO HEAD-2
              PERFORM 0300-READ-FICHE  THRU 0300-EXIT
              MOVE EL562-IN-REC        TO HEAD-3
              PERFORM 0300-READ-FICHE  THRU 0300-EXIT
              MOVE EL562-IN-REC        TO HEAD-4
              PERFORM 1400-GET-ERRPTC  THRU 1400-EXIT
              MOVE WS-REPORT-CODE-1    TO WS-REC-RPTCDE1
              MOVE WS-REPORT-CODE-2    TO WS-REC-RPTCDE2
              IF WS-REPORT-CODE-1 NOT = 'ZZZZZZZZZZ' AND '##########'
                 MOVE WS-REPORT-CODE-1 TO HEAD-1 (5:10)
              END-IF
              IF WS-REPORT-CODE-2 NOT = 'ZZZZZZZZZZ' AND '##########'
                 MOVE WS-REPORT-CODE-2 TO HEAD-1 (16:10)
              END-IF
              MOVE HEAD-1              TO WS-REC-REST
              PERFORM 1300-WRITE-REC   THRU 1300-EXIT
              MOVE HEAD-2              TO WS-REC-REST
              PERFORM 1300-WRITE-REC   THRU 1300-EXIT
              MOVE HEAD-3              TO WS-REC-REST
              PERFORM 1300-WRITE-REC   THRU 1300-EXIT
              MOVE HEAD-4              TO WS-REC-REST
              PERFORM 1300-WRITE-REC   THRU 1300-EXIT
           ELSE
              MOVE WS-REPORT-CODE-1    TO WS-REC-RPTCDE1
              MOVE WS-REPORT-CODE-2    TO WS-REC-RPTCDE2
              MOVE EL562-IN-REC        TO WS-REC-REST
              PERFORM 1300-WRITE-REC   THRU 1300-EXIT
           END-IF              
                            
           PERFORM 0300-READ-FICHE  THRU 0300-EXIT

           .
       1000-EXIT.
           EXIT.
           
       1200-READ-ERRPTC.

           READ ERRPTC

           IF (ERRPTC-FILE-STATUS = '10' OR '23')
              OR (RA-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              DISPLAY ' REPORT CODE NOT FOUND '
                           RA-CONTROL-PRIMARY (2:27)
              PERFORM ABEND-PGM
           ELSE
              IF ERRPTC-FILE-STATUS NOT = '00'
                 DISPLAY 'ERRPTC, BAD READ '
                      ERRPTC-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 ADD 1                 TO WS-ERRPTC-RECS-IN
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.

       1300-WRITE-REC.

           WRITE EL562-OUT-REC         FROM WS-RECORD-OUT
           ADD 1                       TO WS-FICHE-RECS-OUT
           ADD 1                       TO WS-REC-SEQNO

           .
       1300-EXIT.
           EXIT.

       1400-GET-ERRPTC.
       
           MOVE DTE-CLASIC-COMPANY-CD  TO RA-COMPANY-CD
           MOVE FICHE-CARRIER          TO RA-CARRIER
           MOVE FICHE-GROUPING         TO RA-GROUPING
           MOVE FICHE-FIN-RESP         TO RA-FIN-RESP
           MOVE FICHE-ACCOUNT          TO RA-ACCOUNT
           IF RA-FIN-RESP = SPACES
              MOVE RA-ACCOUNT          TO RA-FIN-RESP
           END-IF

           IF RA-CONTROL-PRIMARY = WS-PREV-ERRPTC-KEY
              CONTINUE
           ELSE

              READ ERRPTC

              IF ERRPTC-FILE-STATUS    = '00'
                 ADD 1                 TO WS-ERRPTC-RECS-IN
                 MOVE RA-REPORT-CODE-1 TO WS-REPORT-CODE-1
                 MOVE RA-REPORT-CODE-2 TO WS-REPORT-CODE-2
                 MOVE RA-CONTROL-PRIMARY
                                       TO WS-PREV-ERRPTC-KEY
              ELSE
                 IF HEAD-4-ACCT-NO NOT = 'ACCOUNT NO'
                    MOVE 'ZZZZZZZZZZ'  TO WS-REPORT-CODE-1
                                          WS-REPORT-CODE-2
                 ELSE
                    DISPLAY ' BAD READ ON ERRPTC ' ERRPTC-FILE-STATUS
                    DISPLAY ' HEADING 4          ' HEAD-4
                    MOVE '##########'  TO WS-REPORT-CODE-1
                                          WS-REPORT-CODE-2
                 END-IF
              END-IF
           END-IF

           .
       1400-EXIT.
           EXIT.

       3000-CLOSE-FILES.

           CLOSE EL562-FICHE-IN EL562-FICHE-OUT ERRPTC

           IF ERRPTC-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY '*** ERROR CLOSING ERRPTC FILE ***'
              DISPLAY '*** STATUS CODE IS ' ERRPTC-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF


           .
       3000-EXIT.
           EXIT.

       ABEND-PGM.                                                       
                                       COPY ELCABEND.

           
