       PROGRAM-ID.    CIDEPX4.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
052013* 052013 CR2013042200001   PEMA  NEW PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EPECS                ASSIGN TO SYS010.
           SELECT SORT-EPECS           ASSIGN TO SORTWK1.
           SELECT DISK-DATE            ASSIGN TO SYS019.

           SELECT ERACCTT              ASSIGN TO ERACCTT
                                       ACCESS IS SEQUENTIAL
                                       ORGANIZATION IS INDEXED
                                      FILE STATUS IS ERACCT-FILE-STATUS
                                      RECORD KEY IS AM-CONTROL-PRIMARY.

           SELECT EXTRACT              ASSIGN TO SYS011
              ORGANIZATION IS LINE SEQUENTIAL.
                                                                        
              

       DATA DIVISION.
       FILE SECTION.

       FD  EPECS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
                                       COPY ECSEPC01.

       SD  SORT-EPECS.

       01  SORT-RECORD.
           05  SRT-CARRIER             PIC X.
           05  SRT-GROUP               PIC X(6).
           05  SRT-STATE               PIC XX.
           05  SRT-ACCOUNT             PIC X(10).
           05  srt-exp-dt              pic 9(8).
           05  srt-eff-dt              pic 9(8).
           05  SRT-REPORT-CDE1         PIC X(10).
           05  SRT-REPORT-CDE2         PIC X(10).
           05  SRT-REPORT-CDE3         PIC X(10).
           05  SRT-ACCT-NAME           PIC X(30)  VALUE SPACES.

      *  FIRST LEVEL
      *     1 = MTD
      *     2 = MTD-1
      *     3 = MTD-2
      *     4 = MTD-3
      *     5 = MTD-4
      *     6 = MTD-5
      *     7 = MTD-6
      *     8 = MTD-7
      *     9 = MTD-8
      *    10 = MTD-9
      *    11 = MTD-10
      *    12 = MTD-11

           05  SRT-ACCUM-TOTALS.
               10  FILLER OCCURS 24.
                   15  srt-iss-count        pic 9(11)       comp-3.
                   15  SRT-LF-BEN           PIC S9(11)V99   COMP-3.
                   15  SRT-LF-PREM          PIC S9(9)V99    COMP-3.
                   15  SRT-LF-CLMS          PIC S9(9)V99    COMP-3.
                   15  SRT-AH-BEN           PIC S9(11)V99   COMP-3.
                   15  SRT-AH-PREM          PIC S9(9)V99    COMP-3.
                   15  SRT-AH-CLMS          PIC S9(9)V99    COMP-3.
                   15  SRT-TOT-PREM         PIC S9(11)V99   COMP-3.
                   15  SRT-TOT-COMM         PIC S9(9)V99    COMP-3.
                   15  SRT-NET-COUNT        PIC S9(9)       COMP-3.
                   15  SRT-HI-CERT-DT       PIC 9(11)       COMP-3.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ERACCTT.    
                                                                        
                                       COPY ERCACCT.                        

       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

      *01  EXTRACT-HEAD-OUT            PIC X(480).
       01  EXTRACT-RECORD-OUT          PIC X(249).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDEPX4 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EP-CODE                  PIC X  VALUE SPACES.
       77  WS-DISP-AMT                 PIC -ZZZ,ZZZ,999.99.
       77  WS-DISP-AMTA                PIC -ZZZ,ZZZ,999.99.
       77  WS-DISP-AMTB                PIC -ZZZ,ZZZ,999.99.
       77  WS-DISP-DATE                PIC 9(11)  VALUE ZEROS.
       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  EPEC-IN-CNT                 PIC 9(11)  VALUE ZEROS.
       77  EXTR-OUT-CNT                PIC 9(11)  VALUE ZEROS.
       77  WS-RET-RECS                 PIC 9(11)  VALUE ZEROS.
       77  WS-REL-RECS                 PIC 9(11)  VALUE ZEROS.
       77  WS-REL-RECS-skip            PIC 9(11)  VALUE ZEROS.
       77  WS-LAST-MONTH-DT            PIC 9(11)  VALUE ZEROS.
       77  WS-LAST-YEAR-END-DT         PIC 9(11)  VALUE ZEROS.
       77  WS-ONE-YEAR-AGO-DT          PIC 9(11)  VALUE ZEROS.
       77  WS-TWO-YEARS-AGO-DT         PIC 9(11)  VALUE ZEROS.
       77  WS-TWO-YEAR-ENDS-AGO-DT     PIC 9(11)  VALUE ZEROS.
       77  A1                          PIC S9(5)  VALUE +0 COMP-3.
       77  S1                          PIC S9(5)  VALUE +0 COMP-3.
       77  D1                          PIC S999   VALUE +0 COMP-3.
       77  WS-S1                       PIC S999   VALUE +0 COMP-3.
       77  WS-S2                       PIC S999   VALUE +0 COMP-3.
       77  WS-S3                       PIC S999   VALUE +0 COMP-3.
       77  WS-S4                       PIC S999   VALUE +0 COMP-3.
       77  WS-WORK-TAX                 PIC S9(7)V9(5) VALUE +0 COMP-3.
       77  ERACCT-FILE-STATUS          PIC XX VALUE LOW-VALUES.
       77  ERRTBL-FILE-STATUS          PIC XX VALUE LOW-VALUES.
       77  ERACCT-EOF-SW               PIC X  VALUE SPACES.
           88  END-OF-ERACCT                  VALUE 'Y'.
       77  ERRTBL-EOF-SW               PIC X  VALUE SPACES.
           88  END-OF-ERRTBL                  VALUE 'Y'.
       77  WS-ERN-PRM                  PIC S9(9)V99 VALUE +0 COMP-3.
       77  WS-ERN-COMM                 PIC S9(9)V99 VALUE +0 COMP-3.
       77  WS-COMM-PCT                 PIC S9(5)V99    VALUE +0 COMP-3.
       77  WS-WRK-LR-LO         PIC S9(5)V99    VALUE  -999.99 COMP-3.
       77  WS-WRK-LR-HI         PIC S9(5)V99    VALUE  +999.99 COMP-3.
       77  WS-WRK-LR                   PIC S9(5)V99    VALUE +0 COMP-3.
       77  SAVE-BEN-INDEX              PIC S999    VALUE +0 COMP-3.
       01  WS-CURRENT-KEY.
           05  WS-CONTROL              PIC X(19)  VALUE LOW-VALUES.
           05  ws-ck-exp-dt            pic 9(8).

       01  WS-PREVIOUS-KEY.
           05  WS-Pk-carrier           PIC X.
           05  WS-Pk-group             PIC X(6).
           05  WS-Pk-state             PIC XX.
           05  WS-Pk-account           PIC X(10).
           05  ws-pk-exp-dt            pic 9(8).

       01  WS-PREV-ACCT-STUFF.
           05  ws-prev-rptcd1          pic x(10)  value spaces.
           05  ws-prev-rptcd2          pic x(10)  value spaces.
           05  ws-prev-rptcd3          pic x(10)  value spaces.
           05  ws-prev-acct-name       PIC X(30)  VALUE SPACES.
           05  ws-prev-eff-dt          pic 9(8)   value zeros.

       01  WS-CURR-ACCT-STUFF.
           05  ws-curr-rptcd1          pic x(10)  value spaces.
           05  ws-curr-rptcd2          pic x(10)  value spaces.
           05  ws-curr-rptcd3          pic x(10)  value spaces.
           05  ws-curr-acct-name       pic x(30)  value spaces.
           05  ws-curr-eff-dt          pic 9(8)   value zeros.

       01  WS-DISPLAY-AMT              PIC ZZZZZZZZZ.ZZ.

      *  FIRST LEVEL
      *     1 = CURR
      *     2 = CURR - 1
      *     3 = CURR - 2
      *     4 = CURR - 3
      *     5 = CURR - 4
      *     6 = CURR - 5
      *     7 = CURR - 6
      *     8 = CURR - 7
      *     9 = CURR - 8
      *    10 = CURR - 9
      *    11 = CURR - 10
      *    12 = CURR - 11
      *    13 = CURR - 12

       01  FILLER.
           05  WS-DATES.
               10  FILLER OCCURS 25.
                   15  WS-DATE         PIC 9(11).
           05  WS-ACCUM-TOTALS.
               10  FILLER OCCURS 25.
                   15  ws-iss-count    pic 9(11)       comp-3.
                   15  WS-LF-BEN       PIC S9(11)V99   COMP-3.
                   15  WS-LF-PREM      PIC S9(9)V99    COMP-3.
                   15  WS-LF-CLMS      PIC S9(9)V99    COMP-3.
                   15  WS-AH-BEN       PIC S9(11)V99   COMP-3.
                   15  WS-AH-PREM      PIC S9(9)V99    COMP-3.
                   15  WS-AH-CLMS      PIC S9(9)V99    COMP-3.
                   15  WS-TOT-PREM     PIC S9(11)V99   COMP-3.
                   15  WS-TOT-COMM     PIC S9(9)V99    COMP-3.
                   15  WS-NET-COUNT    PIC S9(9)       COMP-3.
                   15  WS-HI-CERT-DT   PIC 9(11)       COMP-3.


       01  WS-INIT-EXTRACT             PIC X(714).
       01  EXTRACT-RECORD.
           05  EXT-CARRIER             PIC X.
           05  EXT-GROUP               PIC X(6).
           05  EXT-STATE               PIC XX.
           05  EXT-ACCOUNT             PIC X(10).
           05  ext-exp-dt              pic x(10).
           05  ext-eff-dt              pic x(10).
           05  EXT-YEAR                PIC x(4).
           05  EXT-MONTH               PIC xx.
           05  EXT-REPORT-CDE1         PIC X(10).
           05  EXT-REPORT-CDE2         PIC X(10).
           05  EXT-REPORT-CDE3         PIC X(10).
           05  EXT-LF-BEN              PIC -----------.99.
           05  EXT-LF-PREM             PIC ---------.99.
           05  EXT-LF-CLMS             PIC ---------.99.
           05  EXT-AH-BEN              PIC -----------.99.
           05  EXT-AH-PREM             PIC ---------.99.
           05  EXT-AH-CLMS             PIC ---------.99.
           05  EXT-TOT-PREM            PIC -----------.99.
           05  EXT-TOT-COMM            PIC ---------.99.
           05  EXT-NET-COUNT           PIC --------9.
           05  ext-account-name        pic x(30).
           05  EXT-HI-CERT-DT          PIC X(10).
           05  EXT-EOR                 PIC X.

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WS-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.

       01  DATE-AREAS.
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYY        PIC X(4).
               10  WS-WORK-CCYY-N REDEFINES WS-WORK-CCYY
                                       PIC 9(4).
               10  WS-WORK-MM          PIC XX.
               10  WS-WORK-DD          PIC XX.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           SORT SORT-EPECS ON ASCENDING KEY
                                    SRT-CARRIER
                                    SRT-GROUP
                                    SRT-STATE
                                    SRT-ACCOUNT
                                    srt-exp-dt

                INPUT PROCEDURE 0002-INPUT THRU 0002-EXIT
                OUTPUT PROCEDURE 0003-OUTPUT THRU 0003-EXIT

           GOBACK

           .
       0002-INPUT.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0075-READ-ERACCT    THRU 0075-EXIT

           MOVE AM-NAME                TO ws-PReV-ACCT-NAME
           move am-report-code-1       to ws-prev-rptcd1
           move am-report-code-2       to ws-prev-rptcd2
           move am-report-code-3       to ws-prev-rptcd3
           move am-effect-dt           to ws-prev-eff-dt

           PERFORM 0060-READ-EPEC      THRU 0060-EXIT
           MOVE WS-CURRENT-KEY         TO WS-PREVIOUS-KEY

           PERFORM 0077-PROCESS-EPEC   THRU 0077-EXIT UNTIL
                 (END-OF-INPUT)
      *          OR (EPEC-IN-CNT > 3000)

           PERFORM 0085-RELEASE-EXTRACT
                                       THRU 0085-EXIT
           DISPLAY ' END OF SORT INPUT '
           .
       0002-EXIT.
           EXIT.


       0003-OUTPUT SECTION.

           DISPLAY ' BEGIN SORT OUTPUT '
           MOVE SPACES                 TO WS-EOF-SW

           PERFORM 0110-RETURN         THRU 0110-EXIT

           MOVE SORT-RECORD (1:27)     TO WS-CURRENT-KEY
                                          WS-PREVIOUS-KEY

           MOVE SRT-ACCT-NAME          TO ws-PReV-ACCT-NAME
           move srt-report-cde1        to ws-prev-rptcd1
           move srt-report-cde2        to ws-prev-rptcd2
           move srt-report-cde3        to ws-prev-rptcd3
           move srt-eff-dt             to ws-prev-eff-dt

           PERFORM 0015-INIT-TABLE     THRU 0015-EXIT

           PERFORM 0100-PROCESS-RETURN THRU 0100-EXIT UNTIL
                END-OF-INPUT

           PERFORM 0190-BUILD-EXTRACT  THRU 0190-EXIT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           .
       0003-EXIT.
           EXIT.

       0010-INITIALIZE.

           MOVE SPACES                 TO EXTRACT-RECORD

           MOVE 'E'                    TO EXT-EOR

           MOVE EXTRACT-RECORD         TO WS-INIT-EXTRACT
           
           PERFORM 0015-INIT-TABLE     THRU 0015-EXIT
           MOVE LOW-VALUES             TO WS-PREVIOUS-KEY
                                          WS-CURRENT-KEY
           
           PERFORM VARYING WS-S2 FROM +1 BY +1 UNTIL WS-S2 > +25
              MOVE ZEROS               TO WS-DATE (WS-S2)
           END-PERFORM

           PERFORM VARYING D1 FROM +1 BY +1 UNTIL D1 > +25
              MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
              MOVE +0                     TO DC-ELAPSED-DAYS
              COMPUTE DC-ELAPSED-MONTHS = (D1 - +1) * -1
              MOVE '6'                    TO DC-OPTION-CODE
              MOVE '1'                    TO DC-END-OF-MONTH
              PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD   TO WS-DATE (D1)
                 display d1 ' dates    ' ws-date (d1)
              ELSE
                 DISPLAY ' ERROR-DATE-ELAPSED ' D1
                 PERFORM ABEND-PGM
              END-IF
           END-PERFORM

           .
       0010-EXIT.
           EXIT.

       0015-INIT-TABLE.

           PERFORM VARYING WS-S2 FROM +1 BY +1 UNTIL
              WS-S2 > +25
              MOVE +0                  TO ws-iss-count  (ws-s2)
                                          WS-LF-BEN     (WS-S2)
                                          WS-LF-PREM    (WS-S2)
                                          WS-LF-CLMS    (WS-S2)
                                          WS-AH-BEN     (WS-S2)
                                          WS-AH-PREM    (WS-S2)
                                          WS-AH-CLMS    (WS-S2)
                                          WS-TOT-PREM   (WS-S2)
                                          WS-TOT-COMM   (WS-S2)
                                          ws-net-count  (ws-s2)
                                          WS-HI-CERT-DT (WS-S2)
           END-PERFORM

           .
       0015-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT EPECS ERACCTT
               OUTPUT EXTRACT

           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR-ERACCTT-OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' EPEC IN RECORDS  ' EPEC-IN-CNT
           DISPLAY ' RECORDS RELEASED ' WS-REL-RECS
      *    display ' records skipped  ' ws-rel-recs-skip
           DISPLAY ' RECORDS RETURNED ' WS-RET-RECS
           DISPLAY ' EXTR OUT RECORDS ' EXTR-OUT-CNT
           CLOSE EPECS EXTRACT ERACCTT

           .
       0030-EXIT.
           EXIT.

       0060-READ-EPEC.

           READ EPECS AT END
               SET END-OF-INPUT        TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              IF EP-REIN = 'R'
                 GO TO 0060-READ-EPEC
              END-IF
              ADD 1                    TO EPEC-IN-CNT
              MOVE EP-Cntrl-1          TO WS-CONTROL
              move ep-exp-dte          to ws-ck-exp-dt 
              PERFORM 0070-MATCH-TO-ERACCT
                                       THRU 0070-EXIT
           END-IF

           .
       0060-EXIT.
           EXIT.

       0070-MATCH-TO-ERACCT.
       
           IF (AM-CONTROL-A = EP-CNTRL-1)
              AND (AM-EXPIRE-DT = EP-EXP-DTE)
              AND (AM-EFFECT-DT = EP-EFF-DTE)
              MOVE AM-REPORT-CODE-1    TO WS-curr-RPTCD1
              MOVE AM-REPORT-CODE-2    TO WS-curr-RPTCD2
              MOVE AM-REPORT-CODE-3    TO WS-curr-RPTCD3
              move am-name             to ws-curr-acct-name
              move ep-eff-dte          to ws-curr-eff-dt 
              GO TO 0070-EXIT
           ELSE
              IF AM-CONTROL-A > EP-CNTRL-1
                 DISPLAY 'EPEC AND ERACCT MESSED UP '
                 DISPLAY ' EPEC ' EP-CNTRL-1
                 DISPLAY ' ACCT ' AM-CONTROL-A
                 PERFORM ABEND-PGM
              ELSE
                 PERFORM 0075-READ-ERACCT 
                                       THRU 0075-EXIT
                 GO TO 0070-MATCH-TO-ERACCT
              END-IF
           END-IF
           
           .
       0070-EXIT.
           EXIT.
           
       0075-READ-ERACCT.
       
           IF NOT END-OF-ERACCT
              READ ERACCTT
           END-IF
           
           IF ERACCT-FILE-STATUS = '10' OR '23'
              SET END-OF-ERACCT        TO TRUE
              MOVE HIGH-VALUES         TO AM-CONTROL-A
           ELSE
              IF ERACCT-FILE-STATUS = '00'
                 CONTINUE
              ELSE
                 DISPLAY ' BAD READ ON ERACCTT ' ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF
           
           .
       0075-EXIT.
           EXIT.
           
       0077-PROCESS-EPEC.

           IF EP-RECORD-ID = 'EP' OR 'EC'
              PERFORM 0080-PROCESS-EPEC
                                       THRU 0080-EXIT
           END-IF

           PERFORM 0060-READ-EPEC      THRU 0060-EXIT

           .
       0077-EXIT.
           EXIT.

       0080-PROCESS-EPEC.

           IF WS-CURRENT-KEY NOT = WS-PREVIOUS-KEY
              PERFORM 0085-RELEASE-EXTRACT
                                       THRU 0085-EXIT
              PERFORM 0015-INIT-TABLE  THRU 0015-EXIT
              MOVE WS-CURRENT-KEY      TO WS-PREVIOUS-KEY
              move ws-curr-rptcd1      to ws-prev-rptcd1
              move ws-curr-rptcd2      to ws-prev-rptcd2
              move ws-curr-rptcd3      to ws-prev-rptcd3
              move ws-curr-acct-name   to ws-prev-acct-name
              move ws-curr-eff-dt      to ws-prev-eff-dt
           END-IF

           IF EP-RECORD-ID = 'EP'
              perform varying ws-s2 from +1 by +1 until ws-s2 > +25
                 IF (EP-RUN-DTE = ws-date (ws-s2))
                             OR
                    ((EP-RUN-DTE < ws-date (ws-s2))
                    AND (EP-PURGE = 'P'))
                    PERFORM 0082-ACCUM-EP-TOTS
                                          THRU 0082-EXIT
                 END-IF
              end-perform
           end-if

           IF EP-RECORD-ID = 'EC'
              perform varying ws-s2 from +1 by +1 until ws-s2 > +25
                 IF (EC-RUN-DTE = ws-date (ws-s2))
                             OR
                    ((EC-RUN-DTE < ws-date (ws-s2))
                    AND (EC-PURGE = 'P'))
                    PERFORM 0083-ACCUM-EC-TOTS
                                          THRU 0083-EXIT
                 END-IF
              end-perform
           end-if

           .
       0080-EXIT.
           EXIT.

       0082-ACCUM-EP-TOTS.

           if ep-rcd-type = 'L'
              COMPUTE WS-LF-BEN (WS-S2) =
                 WS-LF-BEN (WS-S2) + (EP-ISS-BEN - EP-CNC-BEN)
              compute ws-lf-prem (ws-s2) =
                 ws-lf-prem (ws-s2) + (ep-iss-prm - ep-cnc-prm)
              compute ws-lf-clms (ws-s2) = 
                 ws-lf-clms (ws-s2) + ep-clm-amt
           else
              COMPUTE WS-ah-BEN (WS-S2) =
                 WS-ah-BEN (WS-S2) + (EP-ISS-BEN - EP-CNC-BEN)
              compute ws-ah-prem (ws-s2) =
                 ws-ah-prem (ws-s2) + (ep-iss-prm - ep-cnc-prm)
              compute ws-ah-clms (ws-s2) = 
                 ws-ah-clms (ws-s2) + ep-clm-amt
           end-if

           if ep-hi-cert > ws-hi-cert-dt (ws-s2)
              move ep-hi-cert          to ws-hi-cert-dt (ws-s2)
           end-if
           compute ws-iss-count (ws-s2) =
              ws-iss-count (ws-s2) + ep-iss-cnt
           compute ws-tot-prem (ws-s2) =
              ws-tot-prem (ws-s2) + (ep-iss-prm - ep-cnc-prm)
           compute ws-net-count (ws-s2) = 
              ws-net-count (ws-s2) + (ep-iss-cnt - ep-cnc-cnt)

           .
       0082-EXIT.
           EXIT.

       0083-ACCUM-EC-TOTS.

           PERFORM VARYING A1 FROM +1 BY +1 UNTIL
              (A1 > +5)
              IF EC-AGT-TYPE (A1) = 'C' OR 'D'
                 COMPUTE WS-TOT-COMM (WS-S2) =
                 WS-TOT-COMM (WS-S2) +
                    (EC-ISS-COMM (A1) - EC-CNC-COMM (A1))
              END-IF
           END-PERFORM

           .
       0083-EXIT.
           EXIT.

       0085-RELEASE-EXTRACT.

           MOVE WS-Pk-carrier          TO SRT-CARRIER
           MOVE WS-Pk-group            TO SRT-GROUP
           MOVE WS-Pk-state            TO SRT-STATE
           MOVE WS-Pk-account          TO SRT-ACCOUNT
           move ws-pk-exp-dt           to srt-exp-dt

           move ws-prev-eff-dt         to srt-eff-dt
           MOVE WS-PREV-RPTCD1         TO SRT-REPORT-CDE1
           MOVE WS-PREV-RPTCD2         TO SRT-REPORT-CDE2
           MOVE WS-PREV-RPTCD3         TO SRT-REPORT-CDE3
           
           MOVE ws-PReV-ACCT-NAME      TO SRT-ACCT-NAME

           perform 0090-calc-rolling-12 thru 0090-exit
      *    MOVE WS-ACCUM-TOTALS        TO SRT-ACCUM-TOTALS

      *    if srt-exp-dt < ws-date (25)
      *       add 1                    to ws-rel-recs-skip
      *    else
              RELEASE SORT-RECORD
              ADD 1                    TO WS-REL-RECS
      *    end-if

           .
       0085-EXIT.
           EXIT.

       0090-calc-rolling-12.

           perform varying d1 from +1 by +1 until d1 > +24
              move ws-hi-cert-dt (d1)  to srt-hi-cert-dt (d1)
              compute srt-iss-count (d1) = 
                 ws-iss-count (d1) - ws-iss-count (d1 + 1)
              compute srt-lf-ben (d1) =
                 ws-lf-ben (d1) - ws-lf-ben (d1 + 1)
              compute srt-lf-prem (d1) =
                 ws-lf-prem (d1) - ws-lf-prem (d1 + 1)
              compute srt-lf-clms (d1) =
                 ws-lf-clms (d1) - ws-lf-clms (d1 + 1)
              compute srt-ah-ben (d1) =
                 ws-ah-ben (d1) - ws-ah-ben (d1 + 1)
              compute srt-ah-prem (d1) =
                 ws-ah-prem (d1) - ws-ah-prem (d1 + 1)
              compute srt-ah-clms (d1) =
                 ws-ah-clms (d1) - ws-ah-clms (d1 + 1)
              compute srt-tot-prem (d1) =
                 ws-tot-prem (d1) - ws-tot-prem (d1 + 1)
              compute srt-tot-comm (d1) =
                 ws-tot-comm (d1) - ws-tot-comm (d1 + 1)
              compute srt-net-count (d1) =
                 ws-net-count (d1) - ws-net-count (d1 + 1)
              if srt-iss-count (d1) = zeros
                 move zeros            to srt-hi-cert-dt (d1)
              end-if
           end-perform

           .
       0090-exit.
           exit.

       0100-PROCESS-RETURN.

           IF WS-CURRENT-KEY NOT = WS-PREVIOUS-KEY
              PERFORM 0190-BUILD-EXTRACT
                                       THRU 0190-EXIT
              PERFORM 0015-INIT-TABLE  THRU 0015-EXIT
              MOVE WS-CURRENT-KEY      TO WS-PREVIOUS-KEY

              move ws-curr-rptcd1      to ws-prev-rptcd1
              move ws-curr-rptcd2      to ws-prev-rptcd2
              move ws-curr-rptcd3      to ws-prev-rptcd3
              move ws-curr-acct-name   to ws-prev-acct-name
              move ws-curr-eff-dt      to ws-prev-eff-dt
           END-IF

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +24
              add srt-iss-count (s1)   to ws-iss-count (s1)
              ADD SRT-LF-BEN    (S1)   TO WS-LF-BEN    (S1)
              ADD SRT-LF-PREM   (S1)   TO WS-LF-PREM   (S1)
              ADD SRT-LF-CLMS   (S1)   TO WS-LF-CLMS   (S1)
              ADD SRT-AH-BEN    (S1)   TO WS-AH-BEN    (S1)
              ADD SRT-AH-PREM   (S1)   TO WS-AH-PREM   (S1)
              ADD SRT-AH-CLMS   (S1)   TO WS-AH-CLMS   (S1)
              ADD SRT-TOT-PREM  (S1)   TO WS-TOT-PREM  (S1)
              ADD SRT-TOT-COMM  (S1)   TO WS-TOT-COMM  (S1)
              ADD SRT-NET-COUNT (S1)   TO WS-NET-COUNT (S1)
              move srt-hi-cert-dt (s1) to ws-hi-cert-dt (s1)
           END-PERFORM

           PERFORM 0110-RETURN         THRU 0110-EXIT

           .
       0100-EXIT.
           EXIT.

       0110-RETURN.

           RETURN SORT-EPECS    AT END
               SET END-OF-INPUT        TO TRUE
           END-RETURN

           IF NOT END-OF-INPUT
              MOVE SORT-RECORD (1:27)  TO WS-CURRENT-KEY

              move srt-report-cde1     to ws-curr-rptcd1
              move srt-report-cde2     to ws-curr-rptcd2
              move srt-report-cde3     to ws-curr-rptcd3
              move srt-acct-name       to ws-curr-acct-name
              move srt-eff-dt          to ws-curr-eff-dt
              ADD 1                    TO WS-RET-RECS
           END-IF

           .
       0110-EXIT.
           EXIT.

       0190-BUILD-EXTRACT.

           MOVE WS-INIT-EXTRACT        TO EXTRACT-RECORD
           MOVE WS-Pk-carrier          TO EXT-CARRIER
           MOVE WS-Pk-group            TO EXT-GROUP
           MOVE WS-Pk-state            TO EXT-STATE
           MOVE WS-Pk-account          TO EXT-ACCOUNT

           move ws-pk-exp-dt           to ws-work-date
           if WS-WORK-CCYY = '9999'
              move '12/31/9999'        to ext-exp-dt
           else
              string ws-work-mm '/' ws-work-dd '/' ws-work-ccyy
                 delimited by size into ext-exp-dt
              end-string
           end-if

           move ws-prev-eff-dt         to ws-work-date
           string ws-work-mm '/' ws-work-dd '/' ws-work-ccyy
              delimited by size into ext-eff-dt
           end-string

           MOVE WS-PREV-RPTCD1         TO EXT-REPORT-CDE1
           MOVE WS-PREV-RPTCD2         TO EXT-REPORT-CDE2
           if ws-prev-rptcd3 = low-values
              move spaces              to ws-prev-rptcd3
           end-if
           MOVE WS-PREV-RPTCD3         TO EXT-REPORT-CDE3
           MOVE ws-PReV-ACCT-NAME      TO EXT-ACCount-NAME

           PERFORM 0195-MOVE-TABLE     THRU 0195-EXIT
      *    PERFORM 0200-WRITE-EXTRACT  THRU 0200-EXIT

           .
       0190-EXIT.
           EXIT.

       0195-MOVE-TABLE.

           perform varying d1 from +1 by +1 until d1 > +24
              move ws-date (d1)        to ws-work-date
              move ws-work-ccyy        to ext-year
              move ws-work-mm          to ext-month
              if ws-hi-cert-dt (d1) = zeros
                 move spaces           to ext-hi-cert-dt
              else
                 move ws-hi-cert-dt (d1)  to ws-work-date
                 string ws-work-mm '/' ws-work-dd '/' ws-work-ccyy
                    delimited by size into ext-hi-cert-dt
                 end-string
              end-if
              move ws-lf-ben (d1)      to ext-lf-ben
              move ws-lf-prem (d1)     to ext-lf-prem
              move ws-lf-clms (d1)     to ext-lf-clms
              move ws-ah-ben (d1)      to ext-ah-ben
              move ws-ah-prem (d1)     to ext-ah-prem
              move ws-ah-clms (d1)     to ext-ah-clms
              move ws-tot-prem (d1)    to ext-tot-prem
              move ws-tot-comm (d1)    to ext-tot-comm
              move ws-net-count (d1)   to ext-net-count
              PERFORM 0200-WRITE-EXTRACT
                                       THRU 0200-EXIT
           end-perform

           .
       0195-EXIT.
           EXIT.
           
       0200-WRITE-EXTRACT.

           string 
              EXT-CARRIER             ';'
              EXT-GROUP               ';'
              EXT-STATE               ';'
              EXT-ACCOUNT             ';'
              ext-exp-dt              ';'
              ext-eff-dt              ';'
              EXT-YEAR                ';'
              EXT-MONTH               ';'
              EXT-REPORT-CDE1         ';'
              EXT-REPORT-CDE2         ';'
              EXT-REPORT-CDE3         ';'
              EXT-LF-BEN              ';'
              EXT-LF-PREM             ';'
              EXT-LF-CLMS             ';'
              EXT-AH-BEN              ';'
              EXT-AH-PREM             ';'
              EXT-AH-CLMS             ';'
              EXT-TOT-PREM            ';'
              EXT-TOT-COMM            ';'
              EXT-NET-COUNT           ';'
              ext-account-name        ';'
              EXT-HI-CERT-DT          ';'
              EXT-EOR
                 delimited by size into extract-record-out
           end-string

      *    WRITE EXTRACT-RECORD-OUT    FROM EXTRACT-RECORD
           WRITE EXTRACT-RECORD-OUT
           ADD 1                       TO EXTR-OUT-CNT

           .
       0200-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

