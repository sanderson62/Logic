       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDGRM1.
       AUTHOR.     CSO.
       DATE-COMPILED.
      * REMARKS
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      *      THIS PROGRAM MATCHES THE OFF-LINE GAAP FILE WITH THE      *
      *      ACCOUNT MASTER AND CREATES A NEW GAAP FILE FOR THE        *
      *      SUPPLIED VIA PARM REPORT CODE 3.                          *
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      * 121119  CR2019121100001  PEMA  NEW PROGRAM
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT GAAP-FILE-IN     ASSIGN TO SYS010.

           SELECT GAAP-FILE-OUT    ASSIGN TO SYS011.

           SELECT ERACCT           ASSIGN TO ERACCTT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-PRIMARY
                                   FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019. 

       DATA DIVISION.
       FILE SECTION.

       FD  GAAP-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSGAP01.

       FD  GAAP-FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  GAAP-RECORD-OUT             PIC X(365).

       FD  ERACCT.
                                       COPY ERCACCT.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDGRM1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  ERACCT-FILE-STATUS      PIC XX  VALUE LOW-VALUES.
       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       77  RECS-IN                 PIC 9(9) VALUE ZEROS.
       77  RECS-OUT                PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
       77  PGM-SUB                 PIC S999    COMP    VALUE +020.
      ******************************************************************

       01  WS-ABEND-FIELDS.
           12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.
           12  WS-ZERO                 PIC S9          VALUE ZERO.
           12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.
           12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.

                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       LINKAGE SECTION.
      
       01  PARM.
           05  PARM-LENGTH             PIC S9(04) comp VALUE ZEROS.
           05  PARM-REPORT-CODE-3      PIC X(10)  VALUE SPACES.
      
       PROCEDURE DIVISION USING PARM.

                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0050-PROCESS        THRU 0050-EXIT UNTIL
              (END-OF-INPUT)
PEMTST*       OR (RECS-IN > 50000)

           PERFORM 0100-CLOSE-FILES    THRU 0100-EXIT

           DISPLAY ' GAAP RECORDS READ     '  RECS-IN
           DISPLAY ' GAAP RECORDS OUT      '  RECS-OUT
           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT GAAP-FILE-IN ERACCT
               OUTPUT GAAP-FILE-OUT

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT ERROR - OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

            if parm-length > +0
               display ' parm supplied  **' parm-report-code-3 '** '
                  parm-length
            else
               display ' no parm, full copy '
            end-if

           PERFORM 0060-READ-INPUT     THRU 0060-EXIT
           PERFORM 0030-START-ERACCT   THRU 0030-EXIT
           PERFORM 0040-READ-ERACCT    THRU 0040-EXIT

           .
       0020-EXIT.
           EXIT.

       0030-START-ERACCT.
      
           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD
      
           START ERACCT KEY >= AM-CONTROL-PRIMARY
      
           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT ERROR - START ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
      
           .
       0030-EXIT.
           EXIT.

       0040-READ-ERACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
              OR (AM-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              MOVE HIGH-VALUES         TO AM-CONTROL-PRIMARY
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERACCT ERROR - READ ' ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS.
       
           PERFORM 0070-MATCH-TO-ERACCT
                                       THRU 0070-EXIT

           if parm-length > +0
              if am-report-code-3 = parm-report-code-3
                 PERFORM 0080-WRITE-GAAP
                                       THRU 0080-EXIT
              end-if
           else
              PERFORM 0080-WRITE-GAAP  THRU 0080-EXIT
           end-if

           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           .
       0050-EXIT.
           EXIT.
           
       0060-READ-INPUT.

           READ GAAP-FILE-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO RECS-IN
           END-IF

           .
       0060-EXIT.
           EXIT.

       0070-MATCH-TO-ERACCT.

           IF (GR-CONTROL(1:19) > AM-CONTROL-A)
              OR ((GR-CONTROL(1:19) = AM-CONTROL-A)
                 AND (GR-EFF >= AM-EXPIRE-DT))
                 PERFORM 0040-READ-ERACCT
                                       THRU 0040-EXIT
                 GO TO 0070-MATCH-TO-ERACCT
           ELSE
              IF (GR-CONTROL(1:19) < AM-CONTROL-A)
                 OR ((GR-CONTROL(1:19) = AM-CONTROL-A)
                    AND (GR-EFF < AM-EFFECT-DT))
                 DISPLAY ' NO MATCHING ACCOUNT '
                 DISPLAY ' AM CONTROL          ' AM-CONTROL-A
                 DISPLAY ' GR CONTROL          ' GR-CONTROL(1:19)
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0070-EXIT.
           EXIT.

       0080-WRITE-GAAP.

           WRITE GAAP-RECORD-OUT       FROM GAAP-RECORD
           ADD +1 TO RECS-OUT

           .
       0080-EXIT.
           EXIT.

       0100-CLOSE-FILES.

           CLOSE GAAP-FILE-IN GAAP-FILE-OUT
                 ERACCT

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT ERROR - CLOSE ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0100-EXIT.
           EXIT.

       ABEND-PGM SECTION.
                                     COPY ELCABEND.
