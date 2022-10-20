       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDCRT1.
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
020720* 020720   2019112000002   TANA  NEW PROGRAM
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELCRTO       ASSIGN TO ELCRTO
                               ACCESS IS SEQUENTIAL
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELCRTO-FILE-STATUS
                               RECORD KEY IS OC-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.

           SELECT CRTO-OUT     ASSIGN TO CRTOOT
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  CRTO-OUT.

       01  CRTO-OUT-REC                PIC X(404).

       FD  DISK-DATE                   COPY ELCDTEFD.

       FD  ELCRTO.
                                       COPY ELCCRTO.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDCRT1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-ELCRTO              VALUE 'Y'.
       77  S1                          PIC S999   VALUE +0 COMP-3.
       77  ELCRTO-FILE-STATUS          PIC XX     VALUE '00'.

       01  FILLER.
           05  WS-SAVE-CRTO-EXT        PIC X(404)   VALUE SPACES.
           05  WS-ELCRTO-IN            PIC 9(7)    VALUE ZEROS.
           05  WS-ELCRTO-OUT           PIC 9(7)    VALUE ZEROS.

       01  CRTO-EXTR-RECORD.
0          05  CRTO-CARRIER            PIC X.
           05  CRTO-COL1               PIC X.
1          05  CRTO-GROUPING           PIC X(6).
           05  CRTO-COL2               PIC X.
2          05  CRTO-STATE              PIC XX.
           05  CRTO-COL3               PIC X.
3          05  CRTO-ACCOUNT            PIC X(10).
           05  CRTO-COL4               PIC X.
4          05  CRTO-CERT-EFF-DT        PIC X(10).
           05  CRTO-COL4B              PIC X.
5          05  CRTO-CERT-NO            PIC X(11).
           05  CRTO-COL5               PIC X.
6          05  CRTO-RECORD-TYPE        PIC X.
           05  CRTO-COL6               PIC X.
7          05  CRTO-KEY-SEQ-NO         PIC 9(4).
           05  CRTO-COL7               PIC X.
8          05  CRTO-LAST-MAINT-DT      PIC X(10).
           05  CRTO-COL8               PIC X.
9          05  CRTO-LAST-MAINT-BY      PIC X(4).
           05  CRTO-COL9               PIC X.
10         05  CRTO-LAST-MAINT-HHMMSS  PIC 9(6).
           05  CRTO-COL10              PIC X.
11         05  CRTO-ENDORSEMENT-PROCESSED-DT  PIC X(10).
           05  CRTO-COL11              PIC X.
12         05  CRTO-INS-LAST-NAME      PIC X(15).
           05  CRTO-COL12              PIC X.
13         05  CRTO-INS-FIRST-NAME     PIC X(10).
           05  CRTO-COL13              PIC X.
14         05  CRTO-INS-MIDDLE-INIT    PIC X.
           05  CRTO-COL14              PIC X.
15         05  CRTO-INS-AGE            PIC 9(3).
           05  CRTO-COL14B             PIC X.
16         05  CRTO-JNT-LAST-NAME      PIC X(15).
           05  CRTO-COL15              PIC X.
17         05  CRTO-JNT-FIRST-NAME     PIC X(10).
           05  CRTO-COL16              PIC X.
18         05  CRTO-JNT-MIDDLE-INIT    PIC X.
           05  CRTO-COL17              PIC X.
19         05  CRTO-JNT-AGE            PIC 9(3).
           05  CRTO-COL18              PIC X.
20         05  CRTO-LF-BENCD           PIC XX.
           05  CRTO-COL19              PIC X.
21         05  CRTO-LF-TERM            PIC 9(3).
           05  CRTO-COL20              PIC X.
22         05  CRTO-LF-BEN-AMT         PIC -9(9).99.
           05  CRTO-COL21              PIC X.
23         05  CRTO-LF-PRM-AMT         PIC -9(7).99.
           05  CRTO-COL22              PIC X.
24         05  CRTO-LF-ALT-BEN-AMT     PIC -9(9).99.
           05  CRTO-COL23              PIC X.
25         05  CRTO-LF-ALT-PRM-AMT     PIC -9(7).99.
           05  CRTO-COL24              PIC X.
26         05  CRTO-LF-EXP-DT          PIC X(10).
           05  CRTO-COL24B             PIC X.
27         05  CRTO-LF-COMM-PCT        PIC -9.9(5).
           05  CRTO-COL25              PIC X.
28         05  CRTO-LF-CANCEL-DT       PIC X(10).
           05  CRTO-COL26              PIC X.
29         05  CRTO-LF-CANCEL-AMT      PIC -9(7).99.
           05  CRTO-COL27              PIC X.
30         05  CRTO-LF-ITD-CANCEL-AMT  PIC -9(7).99.
           05  CRTO-COL28              PIC X.
31         05  CRTO-AH-BENCD           PIC XX.
           05  CRTO-COL29              PIC X.
32         05  CRTO-AH-TERM            PIC 9(3).
           05  CRTO-COL30              PIC X.
33         05  CRTO-AH-BEN-AMT         PIC -9(9).99.
           05  CRTO-COL31              PIC X.
34         05  CRTO-AH-PRM-AMT         PIC -9(7).99.
           05  CRTO-COL32              PIC X.
35         05  CRTO-AH-EXP-DT          PIC X(10).
           05  CRTO-COL33              PIC X.
36         05  CRTO-AH-COMM-PCT        PIC -9.9(5).
           05  CRTO-COL34              PIC X.
37         05  CRTO-AH-CP              PIC 9(2).
           05  CRTO-COL35              PIC X.
38         05  CRTO-AH-CANCEL-DT       PIC X(10).
           05  CRTO-COL36              PIC X.
39         05  CRTO-AH-CANCEL-AMT      PIC -9(7).99.
           05  CRTO-COL37              PIC X.
40         05  CRTO-AH-ITD-CANCEL-AMT  PIC -9(7).99.
           05  CRTO-COL38              PIC X.
41         05  CRTO-CRED-BENE-NAME     PIC X(25).
           05  CRTO-COL39              PIC X.
42         05  CRTO-1ST-PMT-DT         PIC X(10).
           05  CRTO-COL40              PIC X.
43         05  CRTO-INS-AGE-DEFAULT-FLAG PIC X.
           05  CRTO-COL41              PIC X.
44         05  CRTO-JNT-AGE-DEFAULT-FLAG PIC X.
           05  CRTO-COL42              PIC X.
45         05  CRTO-ISSUE-TRAN-IND     PIC X.
           05  CRTO-COL43              PIC X.
46         05  CRTO-CANCEL-TRAN-IND    PIC X.
           05  CRTO-COL44              PIC X.
47         05  CRTO-EOR                PIC X.


       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WORK-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.


       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

       0000-BEGIN.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT

           PERFORM 0040-PROCESS-FILE   THRU 0040-EXIT UNTIL
              END-OF-ELCRTO

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-ELCRTO-IN
           DISPLAY ' RECORDS OUT   ' WS-ELCRTO-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0010-OPEN-FILES.

           OPEN INPUT  ELCRTO
                OUTPUT CRTO-OUT

           IF ELCRTO-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ELCRTO OPEN ERROR ' ELCRTO-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0010-EXIT.
           EXIT.

       0020-INIT.

           MOVE SPACES                 TO CRTO-EXTR-RECORD
           INITIALIZE CRTO-EXTR-RECORD
           MOVE X'A2'                  TO CRTO-COL1
                                          CRTO-COL2
                                          CRTO-COL3
                                          CRTO-COL4
                                          CRTO-COL4B
                                          CRTO-COL5
                                          CRTO-COL6
                                          CRTO-COL7
                                          CRTO-COL8
                                          CRTO-COL9
                                          CRTO-COL10
                                          CRTO-COL11
                                          CRTO-COL12
                                          CRTO-COL13
                                          CRTO-COL14
                                          CRTO-COL14B
                                          CRTO-COL15
                                          CRTO-COL16
                                          CRTO-COL17
                                          CRTO-COL18
                                          CRTO-COL19
                                          CRTO-COL20
                                          CRTO-COL21
                                          CRTO-COL22
                                          CRTO-COL23
                                          CRTO-COL24
                                          CRTO-COL24B
                                          CRTO-COL25
                                          CRTO-COL26
                                          CRTO-COL27
                                          CRTO-COL28
                                          CRTO-COL29
                                          CRTO-COL30
                                          CRTO-COL31
                                          CRTO-COL32
                                          CRTO-COL33
                                          CRTO-COL34
                                          CRTO-COL35
                                          CRTO-COL36
                                          CRTO-COL37
                                          CRTO-COL38
                                          CRTO-COL39
                                          CRTO-COL40
                                          CRTO-COL41
                                          CRTO-COL42
                                          CRTO-COL43
                                          CRTO-COL44
           MOVE 'E'                    TO CRTO-EOR
           MOVE CRTO-EXTR-RECORD       TO WS-SAVE-CRTO-EXT

           PERFORM 0120-START-ELCRTO   THRU 0120-EXIT
           if not end-of-ELCRTO
              PERFORM 0110-READ-ELCRTO THRU 0110-EXIT
           end-if

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE ELCRTO CRTO-OUT

           IF ELCRTO-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ELCRTO CLOSE ERROR ' ELCRTO-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-PROCESS-FILE.

           PERFORM 0070-BUILD-EXTR     THRU 0070-EXIT

           PERFORM 0110-READ-ELCRTO    THRU 0110-EXIT

           .
       0040-EXIT.
           EXIT.

       0070-BUILD-EXTR.

           MOVE WS-SAVE-CRTO-EXT       TO CRTO-EXTR-RECORD

           MOVE OC-CARRIER             TO CRTO-CARRIER
           MOVE OC-GROUPING            TO CRTO-GROUPING
           MOVE OC-ACCOUNT             TO CRTO-ACCOUNT
           MOVE OC-STATE               TO CRTO-STATE

           MOVE OC-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO CRTO-CERT-EFF-DT
           ELSE
              MOVE '01/01/2000'        TO CRTO-CERT-EFF-DT
           END-IF
           MOVE OC-CERT-NO             TO CRTO-CERT-NO
           MOVE OC-RECORD-TYPE         TO CRTO-RECORD-TYPE
           MOVE OC-KEY-SEQ-NO          TO CRTO-KEY-SEQ-NO

           IF OC-LAST-MAINT-DT  > LOW-VALUES
              MOVE OC-LAST-MAINT-DT       TO DC-BIN-DATE-1
              MOVE ' '                    TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT TO CRTO-LAST-MAINT-DT
              END-IF
           END-IF.

           MOVE OC-LAST-MAINT-BY       TO CRTO-LAST-MAINT-BY
           MOVE OC-LAST-MAINT-HHMMSS   TO CRTO-LAST-MAINT-HHMMSS

           IF OC-ENDORSEMENT-PROCESSED-DT > LOW-VALUES
              MOVE OC-ENDORSEMENT-PROCESSED-DT TO DC-BIN-DATE-1
              PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT TO
                                     CRTO-ENDORSEMENT-PROCESSED-DT
              END-IF
           END-IF.

           IF OC-INS-LAST-NAME > LOW-VALUES
              MOVE OC-INS-LAST-NAME       TO CRTO-INS-LAST-NAME.
           IF OC-INS-FIRST-NAME > LOW-VALUES
              MOVE OC-INS-FIRST-NAME      TO CRTO-INS-FIRST-NAME.
           IF OC-INS-MIDDLE-INIT > LOW-VALUES
              MOVE OC-INS-MIDDLE-INIT     TO CRTO-INS-MIDDLE-INIT.
           IF OC-INS-AGE NUMERIC
              MOVE OC-INS-AGE             TO CRTO-INS-AGE.
           IF OC-JNT-LAST-NAME > LOW-VALUES
              MOVE OC-JNT-LAST-NAME       TO CRTO-JNT-LAST-NAME.
           IF OC-JNT-FIRST-NAME > LOW-VALUES
              MOVE OC-JNT-FIRST-NAME      TO CRTO-JNT-FIRST-NAME.
           IF OC-JNT-MIDDLE-INIT > LOW-VALUES
              MOVE OC-JNT-MIDDLE-INIT     TO CRTO-JNT-MIDDLE-INIT.
           IF OC-JNT-AGE NUMERIC
              MOVE OC-JNT-AGE             TO CRTO-JNT-AGE.

           IF OC-LF-BENCD > LOW-VALUES
              MOVE OC-LF-BENCD            TO CRTO-LF-BENCD.
           IF OC-LF-TERM NUMERIC
              MOVE OC-LF-TERM             TO CRTO-LF-TERM.
           IF OC-LF-BEN-AMT NUMERIC
              MOVE OC-LF-BEN-AMT          TO CRTO-LF-BEN-AMT.
           IF OC-LF-PRM-AMT NUMERIC
              MOVE OC-LF-PRM-AMT          TO CRTO-LF-PRM-AMT.
           IF OC-LF-ALT-BEN-AMT NUMERIC
              MOVE OC-LF-ALT-BEN-AMT      TO CRTO-LF-ALT-BEN-AMT.
           IF OC-LF-ALT-PRM-AMT NUMERIC
              MOVE OC-LF-ALT-PRM-AMT      TO CRTO-LF-ALT-PRM-AMT.
           IF OC-LF-EXP-DT > LOW-VALUES
              MOVE OC-LF-EXP-DT           TO DC-BIN-DATE-1
              PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT TO CRTO-LF-EXP-DT
              END-IF
           END-IF.

           IF OC-LF-COMM-PCT NUMERIC
              MOVE OC-LF-COMM-PCT         TO CRTO-LF-COMM-PCT.
           IF OC-LF-CANCEL-DT > LOW-VALUES
              MOVE OC-LF-CANCEL-DT        TO DC-BIN-DATE-1
              PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT TO CRTO-LF-CANCEL-DT
              END-IF
           END-IF.

           IF OC-LF-CANCEL-AMT NUMERIC
              MOVE OC-LF-CANCEL-AMT       TO CRTO-LF-CANCEL-AMT.
           IF OC-LF-ITD-CANCEL-AMT NUMERIC
              MOVE OC-LF-ITD-CANCEL-AMT   TO CRTO-LF-ITD-CANCEL-AMT.

           IF OC-AH-BENCD  > LOW-VALUES
              MOVE OC-AH-BENCD            TO CRTO-AH-BENCD.
           IF OC-AH-TERM NUMERIC
              MOVE OC-AH-TERM             TO CRTO-AH-TERM.
           IF OC-AH-BEN-AMT NUMERIC
              MOVE OC-AH-BEN-AMT          TO CRTO-AH-BEN-AMT.
           IF OC-AH-PRM-AMT NUMERIC
              MOVE OC-AH-PRM-AMT          TO CRTO-AH-PRM-AMT.
           IF OC-AH-EXP-DT > LOW-VALUES
              MOVE OC-AH-EXP-DT           TO DC-BIN-DATE-1
              PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT TO CRTO-AH-EXP-DT
              END-IF
           END-IF.

           IF OC-AH-COMM-PCT NUMERIC
              MOVE OC-AH-COMM-PCT         TO CRTO-AH-COMM-PCT.
           IF OC-AH-CP NUMERIC
              MOVE OC-AH-CP               TO CRTO-AH-CP.
           IF OC-AH-CANCEL-DT > LOW-VALUES
              MOVE OC-AH-CANCEL-DT        TO DC-BIN-DATE-1
              PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT TO CRTO-AH-CANCEL-DT
              END-IF
           END-IF.

           IF OC-AH-CANCEL-AMT NUMERIC
              MOVE OC-AH-CANCEL-AMT       TO CRTO-AH-CANCEL-AMT
      *       IF OC-AH-CANCEL-AMT > 99999
      *          display 'cert # out: ' OC-CERT-NO
      *          DISPLAY 'AH CANCEL AMT: ' CRTO-AH-CANCEL-AMT
      *       END-IF
           END-IF.

           IF OC-AH-ITD-CANCEL-AMT NUMERIC
              MOVE OC-AH-ITD-CANCEL-AMT   TO CRTO-AH-ITD-CANCEL-AMT.

           MOVE OC-CRED-BENE-NAME      TO CRTO-CRED-BENE-NAME
           IF OC-1ST-PMT-DT > LOW-VALUES
              MOVE OC-1ST-PMT-DT          TO DC-BIN-DATE-1
              PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT TO CRTO-1ST-PMT-DT
              END-IF
           END-IF.

           IF OC-INS-AGE-DEFAULT-FLAG  > LOW-VALUES
              MOVE OC-INS-AGE-DEFAULT-FLAG TO CRTO-INS-AGE-DEFAULT-FLAG.
           IF OC-JNT-AGE-DEFAULT-FLAG  > LOW-VALUES
              MOVE OC-JNT-AGE-DEFAULT-FLAG TO CRTO-JNT-AGE-DEFAULT-FLAG.
           MOVE OC-ISSUE-TRAN-IND      TO CRTO-ISSUE-TRAN-IND
           MOVE OC-CANCEL-TRAN-IND     TO CRTO-CANCEL-TRAN-IND

           PERFORM 0080-WRITE-CRTO-OUT THRU 0080-EXIT

           .
       0070-EXIT.
           EXIT.

       0080-WRITE-CRTO-OUT.
           INSPECT CRTO-EXTR-RECORD REPLACING ALL ';' BY ' '
           INSPECT CRTO-EXTR-RECORD REPLACING ALL X'A2' BY ';'

           WRITE CRTO-OUT-REC          FROM CRTO-EXTR-RECORD
      *    display 'cert # out: ' OC-CERT-NO
           ADD 1                       TO WS-ELCRTO-OUT

           .
       0080-EXIT.
           EXIT.

       0110-READ-ELCRTO.

           READ ELCRTO NEXT RECORD

           IF (ELCRTO-FILE-STATUS = '10' OR '23')
                           OR
              (OC-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              SET END-OF-ELCRTO   TO TRUE
           ELSE
              IF ELCRTO-FILE-STATUS NOT = '00'
                 DISPLAY ' ELCRTO READ ERROR ' ELCRTO-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 ADD 1                 TO WS-ELCRTO-IN
              END-IF
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-START-ELCRTO.

           MOVE LOW-VALUES             TO OC-CONTROL-PRIMARY

           MOVE DTE-CLASIC-COMPANY-CD  TO OC-COMPANY-CD
           START ELCRTO KEY >= OC-CONTROL-PRIMARY

           IF ELCRTO-FILE-STATUS = '10' OR '23'
              SET END-OF-ELCRTO TO TRUE
           ELSE
              IF ELCRTO-FILE-STATUS NOT = '00'
                 DISPLAY ' ELCRTO START ERROR ' ELCRTO-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0120-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

