       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SALAGTL.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SAL-FILE-IN          ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE            ASSIGN TO SORTWK1.
           SELECT CID-FILE-OUT         ASSIGN TO SYS011
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DISK-DATE            ASSIGN TO SYS019.
           SELECT PRINTX               ASSIGN TO SYS008.

       DATA DIVISION.
       FILE SECTION.

       FD  SAL-FILE-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  SAL-INPUT                   PIC X(410).
       01  SAL-ACCT-RECORD.
           05  SAL-STATE               PIC XX.
           05  SAL-ACCT-NO             PIC XXX.
           05  SAL-EFF-DT.
               10  SAL-EFF-MO          PIC XX.
               10  FILLER              PIC X.
               10  SAL-EFF-DA          PIC XX.
               10  FILLER              PIC X.
               10  SAL-EFF-CCYY        PIC XXXX.
           05  SAL-EXP-DT.
               10  SAL-EXP-MO          PIC XX.
               10  FILLER              PIC X.
               10  SAL-EXP-DA          PIC XX.
               10  FILLER              PIC X.
               10  SAL-EXP-CCYY        PIC XXXX.
           05  SAL-ACCT-NAME           PIC X(45).
           05  SAL-ACCT-ADDR1          PIC X(35).
           05  SAL-ACCT-ADDR2          PIC X(35).
           05  SAL-ACCT-CITY           PIC X(30).
           05  SAL-ACCT-STATE          PIC XX.
           05  SAL-ACCT-ZIP            PIC X(10).
           05  SAL-CONTACT             PIC X(40).
           05  SAL-ACCT-PHONE          PIC X(13).
           05  SAL-ACCT-FAX            PIC X(13).
           05  SAL-LVL1-AGT            PIC X(10).
           05  SAL-LVL1-TYP            PIC X.
           05  SAL-LVL1-TBL            PIC X.
           05  FILLER                  PIC X(9).  
           05  SAL-LVL2-AGT            PIC XXX.
           05  FILLER                  PIC X(7). 
           05  SAL-LVL3-AGT            PIC XXX.
           05  FILLER                  PIC X(7).
           05  SAL-LVL2-PCT            PIC X(7).
           05  SAL-LVL3-PCT            PIC X(7).
           05  SAL-LOB                 PIC X.
           05  SAL-IG                  PIC X.
           05  SAL-STATUS              PIC X.
           05  SAL-RETRO               PIC X.
           05  SAL-RETRO-RET           PIC X.
           05  SAL-WA-NO               PIC XXX.
           05  SAL-LIC-EXP-DT          PIC X(10).
           05  SAL-LIC-NO              PIC X(10).
           05  SAL-SALUTAION           PIC XXX.
           05  SAL-FIRST               PIC X(10).
           05  SAL-LAST                PIC X(35).
           05  SAL-LIC-SSN             PIC X(11).
           05  SAL-LIC-STATUS          PIC X.
           05  SAL-PARENT              PIC XXX.
           05  FILLER                  PIC X(9).
           05  FILLER                  PIC X(9).
           05  FILLER                  PIC X(9).
           05  FILLER                  PIC X(9).
           
           
           
           

           EJECT
       SD  SORT-FILE. 

       01  SORT-RECORD.
           05  SORT-KEY                PIC X(10).
           05  FILLER                  PIC X(303).

       FD  CID-FILE-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  CID-RECORD-OUT              PIC X(313).

       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       FD  PRINTX
                                       COPY ELCPRTFD.
           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     SALAGTL  WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  SUB1                        PIC S999   COMP-3 VALUE +0.
       77  SUB2                        PIC S999   COMP-3 VALUE +0.
       77  BUSNDX                      PIC S9(03) COMP-3 VALUE +0.
       77  ACTNDX                      PIC S9(03) COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  WS-INPUT-CNT                PIC 9(9)   VALUE ZEROS.
       77  WS-OUTPUT-CNT               PIC 9(9)   VALUE ZEROS.
       77  WS-INIT-EXTRACT             PIC X(313)  VALUE LOW-VALUES.

       01  AGENT-EXTRACT.
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-TAB1                PIC X.
           05  EXT-LICENSE-NO          PIC X(10).
           05  EXT-TAB2                PIC X.
           05  EXT-LIC-EXP-DT          PIC X(10).
           05  EXT-TAB3                PIC X.
           05  EXT-FNAME               PIC X(10).
           05  EXT-TAB4                PIC X.
           05  EXT-LNAME               PIC X(35).
           05  EXT-TAB5                PIC X.
           05  EXT-LIC-SSN             PIC X(11).
           05  EXT-TAB6                PIC X.
           05  EXT-LIC-STATUS          PIC X.
           05  EXT-TAB7                PIC X.
           05  EXT-AM-NAME             PIC X(30).
           05  EXT-TAB8                PIC X.
           05  EXT-ACCT-ADDR1          PIC X(35).
           05  EXT-TAB9                PIC X.
           05  EXT-ACCT-ADDR2          PIC X(35).
           05  EXT-TAB10               PIC X.
           05  EXT-ACCT-CITY           PIC X(30).
           05  EXT-TAB11               PIC X.
           05  EXT-ACCT-STATE          PIC XX.
           05  EXT-TAB12               PIC X.
           05  EXT-ACCT-ZIP            PIC X(10).
           05  EXT-TAB13               PIC X.
           05  EXT-CONTACT             PIC X(40).
           05  EXT-TAB14               PIC X.
           05  EXT-ACCT-PHONE          PIC X(13).
           05  EXT-TAB15               PIC X.
           05  EXT-ACCT-FAX            PIC X(13).
           05  EXT-TAB16               PIC X.
           05  EXT-LAST                PIC X.

       01  WS-MISC.
           05  WS-WORK-FIELD-IN        PIC X(30).
           05  WS-WORK-FIELD-TBL REDEFINES WS-WORK-FIELD-IN
                             OCCURS 30 PIC X.
           05  WS-WORK-FIELD-OT        PIC X(30).
           05  WS-WORK-FIELD-TBLO REDEFINES WS-WORK-FIELD-OT
                             OCCURS 30 PIC X.
                                       
           05  WS-WORK-DATE            PIC X(8).
           05  WS-WORK-DATE-N REDEFINES WS-WORK-DATE
                                       PIC 9(8).
           05  WS-DISPLAY-DATE         PIC ZZZ9(8).
           05  WS-DISPLAY-TERM         PIC Z99.
           05  WS-DISPLAY-RTERM        PIC Z99.99.
           05  WS-DISPLAY-PRM          PIC ZZZZZZZ.99.
           05  WS-DISPLAY-UEP          PIC ZZZZZZZ.99.

           EJECT

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

           EJECT
                                       COPY ELCDATE.
      /
                                       COPY ELCDTECX.
      /
                                       COPY ELCDTEVR.
      /
       PROCEDURE DIVISION.

       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

           SORT SORT-FILE ON ASCENDING KEY SORT-KEY
                INPUT PROCEDURE 0002-INPUT
                                       THRU 0002-EXIT
                OUTPUT PROCEDURE 0003-OUTPUT
                                       THRU 0003-EXIT

           GOBACK
             .

       0002-INPUT SECTION.

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT

           .
       0002-EXIT.
           EXIT.

       0003-OUTPUT SECTION.

           MOVE ' '                    TO WS-EOF-SW

           PERFORM 0070-RETURN-REC     THRU 0070-EXIT

           PERFORM 0075-PROCESS-RECS THRU 0075-EXIT UNTIL
                END-OF-INPUT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           .
       0003-EXIT.
           EXIT.


       0010-INITIALIZE.
           
           MOVE SPACES                 TO AGENT-EXTRACT
           MOVE X'09'                  TO EXT-TAB1
                                          EXT-TAB2
                                          EXT-TAB3
                                          EXT-TAB4
                                          EXT-TAB5
                                          EXT-TAB6
                                          EXT-TAB7
                                          EXT-TAB8
                                          EXT-TAB9
                                          EXT-TAB10
                                          EXT-TAB11
                                          EXT-TAB12
                                          EXT-TAB13
                                          EXT-TAB14
                                          EXT-TAB15
                                          EXT-TAB16
           MOVE '*'                    TO EXT-LAST

           MOVE AGENT-EXTRACT          TO WS-INIT-EXTRACT                                          
                                          
           .

       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT SAL-FILE-IN 
               OUTPUT CID-FILE-OUT

           .

       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' SALA RECORDS INPUT    ' WS-INPUT-CNT
           DISPLAY ' ACCT RECORDS OUTPUT   ' WS-OUTPUT-CNT

           CLOSE SAL-FILE-IN CID-FILE-OUT

           .

       0030-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

           PERFORM 0080-PROCESS-INPUT  THRU 0080-EXIT UNTIL
                 END-OF-INPUT

           .

       0050-EXIT.
           EXIT.

       0060-READ-INPUT.

           READ SAL-FILE-IN AT END
               SET END-OF-INPUT        TO TRUE
           END-READ

      *    DISPLAY ' SAL ACCT AND PARENT ' SAL-ACCT-NO '  ' SAL-PARENT
           IF NOT END-OF-INPUT
              ADD +1                   TO WS-INPUT-CNT
           END-IF

           .

       0060-EXIT.
           EXIT.

       0070-RETURN-REC.

           RETURN SORT-FILE     AT END
               SET END-OF-INPUT        TO TRUE
           END-RETURN

           .

       0070-EXIT.
           EXIT.

       0075-PROCESS-RECS.

           MOVE SORT-RECORD            TO CID-RECORD-OUT
                                          AGENT-EXTRACT 
      *    IF EXT-AM-NAME NOT = SPACES
              WRITE CID-RECORD-OUT
              ADD 1                    TO WS-OUTPUT-CNT
      *    END-IF

           PERFORM 0070-RETURN-REC THRU 0070-EXIT

           .
       0075-EXIT.
           EXIT.


       0080-PROCESS-INPUT.

           MOVE WS-INIT-EXTRACT        TO AGENT-EXTRACT
           
           MOVE '00010'                TO EXT-ACCOUNT (1:5)
           MOVE SAL-ACCT-NO            TO EXT-ACCOUNT (6:3)
           MOVE '00'                   TO EXT-ACCOUNT (9:2)
           
           MOVE SAL-LIC-EXP-DT (7:4)   TO WS-WORK-DATE (1:4)
           MOVE SAL-LIC-EXP-DT (1:2)   TO WS-WORK-DATE (5:2)
           MOVE SAL-LIC-EXP-DT (4:2)   TO WS-WORK-DATE (7:2)
           MOVE WS-WORK-DATE-N         TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
      *       MOVE WS-WORK-DATE-N      TO EXT-LIC-EXP-DT
              MOVE SAL-LIC-EXP-DT      TO EXT-LIC-EXP-DT
           ELSE
              DISPLAY ' LIC DATE ERROR ON ACCT ' EXT-ACCOUNT
                   '  ' SAL-LIC-EXP-DT
           END-IF
           
           MOVE SAL-ACCT-NAME          TO EXT-AM-NAME
           MOVE SAL-LIC-NO             TO EXT-LICENSE-NO
           MOVE SAL-LIC-STATUS         TO EXT-LIC-STATUS
           MOVE SAL-LAST               TO EXT-LNAME
           MOVE SAL-FIRST              TO EXT-FNAME
           MOVE SAL-LIC-SSN            TO EXT-LIC-SSN
           MOVE SAL-ACCT-ADDR1         TO EXT-ACCT-ADDR1
           MOVE SAL-ACCT-ADDR2         TO EXT-ACCT-ADDR2
           MOVE SAL-ACCT-CITY          TO EXT-ACCT-CITY
           MOVE SAL-ACCT-STATE         TO EXT-ACCT-STATE
           MOVE SAL-ACCT-ZIP           TO EXT-ACCT-ZIP
           MOVE SAL-CONTACT            TO EXT-CONTACT
           MOVE SAL-ACCT-PHONE         TO EXT-ACCT-PHONE
           MOVE SAL-ACCT-FAX           TO EXT-ACCT-FAX
           
           RELEASE SORT-RECORD         FROM AGENT-EXTRACT 

           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.




       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
