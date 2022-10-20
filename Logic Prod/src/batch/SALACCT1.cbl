       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SALACCT.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SAL-FILE-IN          ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE            ASSIGN TO SORTWK1.
           SELECT CID-FILE-OUT         ASSIGN TO SYS011.
           SELECT CID-NOTE-FILE-OUT    ASSIGN TO SYS012.
           SELECT CID-PLAN-FILE-OUT    ASSIGN TO SYS013.
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
           05  FILLER                  PIC XX.
           05  SORT-KEY                PIC X(26).
           05  FILLER                  PIC X(1972).

       FD  CID-FILE-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  CID-RECORD-OUT              PIC X(2000).

       FD  CID-NOTE-FILE-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  CID-NOTE-RECORD-OUT         PIC X(120).

       FD  CID-PLAN-FILE-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  CID-PLAN-RECORD-OUT         PIC X(420).

       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       FD  PRINTX
                                       COPY ELCPRTFD.
           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     SALACCT  WORKING STORAGE   '.
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
       77  WS-NOTE-OUTPUT-CNT          PIC 9(9)   VALUE ZEROS.
       77  WS-PLAN-OUTPUT-CNT          PIC 9(9)   VALUE ZEROS.
       77  WS-INIT-NOTE                PIC X(120).
       77  WS-INIT-PLAN                PIC X(420).
       77  WS-INIT-ACCT                PIC X(2000).
       77  WS-SAVE-ACCT                PIC X(2000).

                                       COPY ERCACCT.

                                       COPY ERCACNT.
                                       
                                       COPY ERCPLAN.

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

           MOVE SPACES                 TO ACCOUNT-MASTER
           MOVE 'AM'                   TO AM-RECORD-ID
           MOVE X'04'                  TO AM-COMPANY-CD
                                          AM-COMPANY-CD-A1
           MOVE '9'                    TO AM-CARRIER
                                          AM-VG-CARRIER
           MOVE '000000'               TO AM-GROUPING
                                          AM-VG-GROUPING
           MOVE 'MN'                   TO AM-STATE
                                          AM-VG-STATE
           MOVE 'CONV'                 TO AM-LAST-MAINT-USER
           MOVE +200000                TO AM-LAST-MAINT-HHMMSS
           MOVE X'9B41'                TO AM-LAST-MAINT-DT
           MOVE ZEROS                  TO AM-TEL-NO
                                          AM-ANNIVERSARY-DATE
                                          AM-HI-CERT-DATE
                                          AM-LO-CERT-DATE
                                          AM-ENTRY-DATE
                                          AM-INACTIVE-DATE
                                          AM-3RD-PARTY-NOTIF-LEVEL
                                          AM-EXPIRE-DT
                                          AM-EFFECT-DT
                                          AM-CAL-TABLE
                                          AM-LF-DEVIATION
                                          AM-AH-DEVIATION
           MOVE 02                     TO AM-GPCD
           MOVE '2'                    TO AM-IG
           MOVE 01                     TO AM-REMIT-TO
           MOVE +0                     TO AM-LF-DEVIATION-PCT
                                          AM-AH-DEVIATION-PCT
                                          AM-LF-OB-RATE
                                          AM-AH-OB-RATE
                                          AM-LF-OB-RATE-JNT
                                          AM-AH-OB-RATE-JNT
                                          AM-LF-PSI-FACTOR
                                          AM-AH-PSI-FACTOR
                                          AM-OVR-SHT-AMT
                                          AM-OVR-SHT-PCT
                                          AM-REI-FEE-LF
                                          AM-REI-FEE-AH
                                          AM-REI-LF-TAX
                                          AM-REI-PR-PCT
                                          AM-REI-78-PCT
                                          AM-REI-AH-TAX
                                          AM-TOL-PREM
                                          AM-TOL-REF
                                          AM-TOL-CLM
                                          AM-LF-RET
                                          AM-AH-RET
                                          AM-RET-MIN-LOSS-L
                                          AM-RET-MIN-LOSS-A
                                          AM-LF-RPT021-EXP-PCT
                                          AM-AH-RPT021-EXP-PCT
                                          AM-MAX-MON-BEN
                                          AM-MAX-TOT-BEN
                                          AM-CANCEL-FEE 
                                          AM-TOL-REF-PCT
                                          AM-TARGET-LOSS-RATIO    
                                          AM-LIFE-IBNR-PCT        
                                          AM-CRDT-MODIFICATION-PCT
                                          AM-EXEC1-DIS-PERCENT  
                                          AM-EXEC1-LIFE-PERCENT 
                                          AM-EXEC2-DIS-PERCENT  
                                          AM-EXEC2-LIFE-PERCENT 
                                          AM-RETRO-QUALIFY-LIMIT
                                          AM-RETRO-RET-PCT-LF (1) 
                                          AM-RETRO-RET-THRU-LF (1)
                                          AM-RETRO-RET-PCT-LF (2) 
                                          AM-RETRO-RET-THRU-LF (2)
                                          AM-RETRO-RET-PCT-LF (3) 
                                          AM-RETRO-RET-THRU-LF (3)

           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +10
              MOVE +0                  TO AM-L-COM (SUB1)
                                          AM-J-COM (SUB1)
                                          AM-A-COM (SUB1)
                                          AM-L-COM-SV (SUB1)
                                          AM-J-COM-SV (SUB1)
                                          AM-A-COM-SV (SUB1)
              MOVE ZEROS               TO AM-AGT (SUB1)
              IF SUB1 < +6
                 MOVE +0               TO AM-ALLOW-BEGIN-RANGE (SUB1)
                                          AM-ALLOW-END-RANGE (SUB1)
                                          AM-ALLOWANCE-AMT (SUB1)
              END-IF
           END-PERFORM

           MOVE ACCOUNT-MASTER         TO WS-INIT-ACCT

           MOVE SPACES                 TO NOTE-FILE
           MOVE 'NT'                   TO NT-FILE-ID
           MOVE X'04'                  TO NT-COMPANY-CD
           MOVE '1'                    TO NT-RECORD-TYPE
           MOVE +1                     TO NT-LINE-SEQUENCE
           MOVE 'SALA'                 TO NT-LAST-MAINT-BY
           MOVE +200000                TO NT-LAST-MAINT-HHMMSS
           MOVE X'9B41'                TO NT-LAST-MAINT-DT
           MOVE 'RECORD CREATED BY SAL CONVERSION 06/30/03'
                                       TO NT-NOTE-LINE
           MOVE NOTE-FILE              TO WS-INIT-NOTE
           
           MOVE SPACES                 TO PLAN-MASTER
           INITIALIZE                  PLAN-MASTER
           MOVE 'PL'                   TO PL-RECORD-ID
           MOVE X'04'                  TO PL-COMPANY-CD
           MOVE X'9B41'                TO PL-LAST-MAINT-DT
           MOVE 'SALA'                 TO PL-LAST-MAINT-USER
           MOVE +200000                TO PL-LAST-MAINT-HHMMSS
           MOVE +71                    TO PL-ATT-AGE
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +8
              MOVE +0                  TO PL-LM-AGE (SUB1)
                                          PL-LM-DUR (SUB1)
                                          PL-LM-MOA (SUB1)
                                          PL-LM-AMT (SUB1)
           END-PERFORM
           
           MOVE +65                    TO PL-LM-AGE (1)
           MOVE +180                   TO PL-LM-DUR (1)
           MOVE +1                     TO PL-LM-MOA (1)
                                          PL-LM-AMT (1)
           MOVE PLAN-MASTER            TO WS-INIT-PLAN                                          
                                          
           .

       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT SAL-FILE-IN 
               OUTPUT CID-FILE-OUT CID-NOTE-FILE-OUT
                  CID-PLAN-FILE-OUT

           .

       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' SALA RECORDS INPUT    ' WS-INPUT-CNT
           DISPLAY ' ACCT RECORDS OUTPUT   ' WS-OUTPUT-CNT
           DISPLAY ' NOTE RECORDS OUTPUT   ' WS-NOTE-OUTPUT-CNT
           DISPLAY ' PLAN RECORDS OUTPUT   ' WS-PLAN-OUTPUT-CNT

           CLOSE SAL-FILE-IN CID-FILE-OUT CID-NOTE-FILE-OUT
                 CID-PLAN-FILE-OUT


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
                                          ACCOUNT-MASTER
           IF AM-NAME NOT = SPACES
              PERFORM 0077-BUILD-ERNOTE
                                       THRU 0077-EXIT
              PERFORM 0078-BUILD-ERPLAN
                                       THRU 0078-EXIT
              WRITE CID-RECORD-OUT
              ADD 1                    TO WS-OUTPUT-CNT
           END-IF

           PERFORM 0070-RETURN-REC THRU 0070-EXIT

           .
       0075-EXIT.
           EXIT.

       0077-BUILD-ERNOTE.
       
           MOVE WS-INIT-NOTE           TO NOTE-FILE
           
           MOVE AM-CONTROL-A           TO NT-ACCT-NOTE-KEY
           WRITE CID-NOTE-RECORD-OUT   FROM NOTE-FILE
           ADD 1                       TO WS-NOTE-OUTPUT-CNT
           
           .
       0077-EXIT.
           EXIT.

       0078-BUILD-ERPLAN.
       
           MOVE WS-INIT-PLAN           TO PLAN-MASTER
           
           MOVE AM-CONTROL-A           TO PL-CONTROL-A
           MOVE 'L'                    TO PL-BENEFIT-TYPE
           MOVE '99'                   TO PL-BENEFIT-CODE
           MOVE '000'                  TO PL-REVISION-NO
           MOVE PL-CONTROL-PRIMARY     TO PL-CONTROL-BY-VAR-GRP
           WRITE CID-PLAN-RECORD-OUT   FROM PLAN-MASTER
           ADD 1                       TO WS-PLAN-OUTPUT-CNT
           MOVE 'A'                    TO PL-BENEFIT-TYPE
           MOVE PL-CONTROL-PRIMARY     TO PL-CONTROL-BY-VAR-GRP
           WRITE CID-PLAN-RECORD-OUT   FROM PLAN-MASTER
           ADD 1                       TO WS-PLAN-OUTPUT-CNT
           
           .
       0078-EXIT.
           EXIT.

       0080-PROCESS-INPUT.

           MOVE WS-INIT-ACCT           TO ACCOUNT-MASTER
           
           MOVE '00010'                TO AM-ACCOUNT (1:5)
           MOVE SAL-ACCT-NO            TO AM-ACCOUNT (6:3)
           MOVE '00'                   TO AM-ACCOUNT (9:2)
           MOVE AM-ACCOUNT             TO AM-VG-ACCOUNT
           IF SAL-ACCT-NO = '105'
              MOVE '09/30/1998'        TO SAL-EFF-DT
           END-IF
           IF SAL-ACCT-NO = '111'
              MOVE '04/23/1999'        TO SAL-EFF-DT
           END-IF
           IF SAL-ACCT-NO = '136'
              MOVE '05/30/2002'        TO SAL-EFF-DT
           END-IF
           IF SAL-ACCT-NO = '335'
              MOVE '01/30/1998'        TO SAL-EFF-DT
           END-IF
           IF SAL-ACCT-NO = '545'
              MOVE '10/01/1991'        TO SAL-EFF-DT
           END-IF
           IF SAL-ACCT-NO = '560'
              MOVE '02/10/1995'        TO SAL-EFF-DT
           END-IF
           IF SAL-ACCT-NO = '564'
              MOVE '12/14/1995'        TO SAL-EFF-DT
           END-IF
           IF SAL-EFF-DT = ZEROS OR SPACES
              MOVE '01/01/1975'        TO SAL-EFF-DT
              DISPLAY ' DEFAULT EFF DATE ON ACCT ' AM-ACCOUNT
           END-IF
           MOVE SAL-EFF-DT (7:4)       TO WS-WORK-DATE (1:4)
           MOVE SAL-EFF-DT (1:2)       TO WS-WORK-DATE (5:2)
           MOVE SAL-EFF-DT (4:2)       TO WS-WORK-DATE (7:2)
           MOVE WS-WORK-DATE-N         TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE WS-WORK-DATE-N      TO AM-EFFECT-DT
                                          AM-PREV-EFF-DT
              MOVE DC-BIN-DATE-1       TO AM-EFFECTIVE-DT
           ELSE
              DISPLAY ' EFF DATE ERROR ON ACCT ' AM-ACCOUNT
                   '  ' SAL-EFF-DT
           END-IF
           
           IF SAL-ACCT-NO = '531'
              MOVE '11/02/1999'        TO SAL-EXP-DT
           END-IF
           IF SAL-ACCT-NO = '532'
              MOVE '04/13/1999'        TO SAL-EXP-DT
           END-IF
           IF SAL-ACCT-NO = '573'
              MOVE '02/15/2000'        TO SAL-EXP-DT
           END-IF
           IF SAL-EXP-DT = SPACES
              MOVE '07/01/2003'        TO SAL-EXP-DT
           END-IF

           MOVE SAL-EXP-DT (7:4)       TO WS-WORK-DATE (1:4)
           MOVE SAL-EXP-DT (1:2)       TO WS-WORK-DATE (5:2)
           MOVE SAL-EXP-DT (4:2)       TO WS-WORK-DATE (7:2)
           MOVE WS-WORK-DATE-N         TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE WS-WORK-DATE-N      TO AM-PREV-EXP-DT
              MOVE LOW-VALUES          TO AM-CNTRL-B
              MOVE DC-BIN-DATE-1       TO AM-EXPIRATION-DT
              MOVE AM-CNTRL-B          TO AM-VG-DATE
           ELSE
              DISPLAY ' EXP DATE ERROR ON ACCT ' AM-ACCOUNT
                  '  ' SAL-EXP-DT
           END-IF
           
           MOVE SAL-ACCT-NAME          TO AM-NAME
           MOVE SAL-ACCT-ADDR1         TO AM-ADDRS
           EVALUATE SAL-ACCT-ADDR1 (1:4)
              WHEN 'MR. '
                 MOVE SAL-ACCT-ADDR1 (5:26)
                                       TO AM-ADDRS
              WHEN 'MS. '
                 MOVE SAL-ACCT-ADDR1 (5:26)
                                       TO AM-ADDRS
           END-EVALUATE
              
           STRING SAL-ACCT-CITY ' ' SAL-ACCT-STATE DELIMITED BY '  '
              INTO AM-CITY
           END-STRING
           
           MOVE AM-ADDRS               TO WS-WORK-FIELD-IN
           MOVE +1                     TO SUB2
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +30
              IF WS-WORK-FIELD-TBL (SUB1) NOT = '.' OR ',' OR ';'
                 MOVE WS-WORK-FIELD-TBL (SUB1)
                                       TO WS-WORK-FIELD-TBLO (SUB2)
                 ADD +1                TO SUB2
              END-IF
           END-PERFORM
           MOVE WS-WORK-FIELD-OT       TO AM-ADDRS           
           
           MOVE SAL-ACCT-ZIP (1:5)     TO AM-ZIP-PRIME
           MOVE SAL-ACCT-ZIP (7:4)     TO AM-ZIP-PLUS4
 
      *    MOVE SAL-CONTACT            TO AM-PERSON
      *    EVALUATE SAL-CONTACT (1:4)
      *       WHEN 'MR. '
      *          MOVE SAL-CONTACT (5:26)
      *                                TO AM-PERSON
      *       WHEN 'MS. '
      *          MOVE SAL-CONTACT (5:26)
      *                                TO AM-PERSON
      *    END-EVALUATE

           MOVE SAL-CONTACT            TO AM-CONTROL-NAME
           EVALUATE SAL-CONTACT (1:4)
              WHEN 'MR. '
                 MOVE SAL-CONTACT (5:26)
                                       TO AM-CONTROL-NAME
              WHEN 'MS. '
                 MOVE SAL-CONTACT (5:26)
                                       TO AM-CONTROL-NAME
           END-EVALUATE

           MOVE SAL-ACCT-PHONE (2:3)   TO AM-AREA-CODE
           MOVE SAL-ACCT-PHONE (6:3)   TO AM-TEL-PRE
           MOVE SAL-ACCT-PHONE (10:4)  TO AM-TEL-NBR
           MOVE 'MN (COMBO$1) 9-01'    TO AM-COMMENT-LINE (1)
           MOVE '30425C 3RD REV MN (430) COMBO 6X6'
                                       TO AM-COMMENT-LINE (2)
           MOVE 01                     TO AM-REMIT-TO
           IF SAL-ACCT-NO = '136'
              MOVE 06                  TO AM-GPCD
           END-IF
           MOVE 'SAL'                  TO AM-REI-TABLE
           MOVE 'Y'                    TO AM-BENEFIT-TABLE-USAGE
           MOVE '99'                   TO AM-BENEFIT-CODE (1)
                                          AM-BENEFIT-CODE (2)
           MOVE 'A'                    TO AM-BENEFIT-TYPE (1)
           MOVE 'L'                    TO AM-BENEFIT-TYPE (2)
           MOVE '000'                  TO AM-BENEFIT-REVISION (1)
                                          AM-BENEFIT-REVISION (2)
           MOVE 'Y'                    TO AM-BENEFIT-RETRO-Y-N (1)
                                          AM-BENEFIT-RETRO-Y-N (2)
           
           MOVE 'NCRO'                 TO AM-REPORT-CODE-1
           IF (AM-NAME (1:7) = 'PEOPLES')
              OR (AM-NAME (1:4) = 'PINE')
              OR (AM-NAME (1:9) = 'ST. CLAIR')
              OR (AM-NAME (1:10) = 'STATE BANK')
              OR (AM-NAME (1:7) = 'TRIUMPH')
              OR (AM-NAME (1:7) = 'WELCOME')
              OR (AM-NAME (1:8) = 'CITIZENS')
              OR (AM-NAME (1:5) = 'FIRST')
              OR (AM-NAME (1:6) = 'CURRIE')
              OR (AM-NAME (1:8) = 'EASTWOOD')
              OR (AM-NAME (1:7) = 'FARMERS')
              OR (AM-NAME (1:8) = 'LANDMARK')
              MOVE 'BNS'               TO AM-REPORT-CODE-2
           END-IF
           IF AM-NAME (1:13) = 'AMERICAN BANK'
              MOVE 'ABN'               TO AM-REPORT-CODE-2
           END-IF
           IF AM-NAME (1:6) = 'BORDER'
              MOVE 'BORDER'            TO AM-REPORT-CODE-2
           END-IF
           IF AM-NAME (1:9) = 'COMMUNITY'
              MOVE 'COMM BANK'         TO AM-REPORT-CODE-2
           END-IF
           IF AM-NAME (1:10) = 'FIRST MINN'
              MOVE '1ST MN BNK'        TO AM-REPORT-CODE-2
           END-IF
           IF SAL-ACCT-NO = '104' OR '105'
              MOVE 'FSB'               TO AM-REPORT-CODE-2
           END-IF
           IF AM-NAME (1:7) = 'MIDWEST'
              MOVE 'MIDWEST'           TO AM-REPORT-CODE-2
           END-IF
           IF AM-NAME (1:7) = 'NORTH A'
              MOVE 'NAB'               TO AM-REPORT-CODE-2
           END-IF
           IF (AM-NAME (1:8) = 'NORTHERN')
              OR (AM-NAME (1:8) = 'SECURITY')
              OR (SAL-ACCT-NO = '511')
              MOVE 'BEITO'             TO AM-REPORT-CODE-2
           END-IF
           IF (AM-NAME (1:6) = 'UNITED')
              MOVE 'UPB'               TO AM-REPORT-CODE-2
           END-IF
           
           IF (SAL-ACCT-NO = '116' OR '292' OR '553' OR '561' OR
              '550' OR '640' OR '637' OR '639' OR '641' OR '101' OR
              '119' OR '803' OR '802' OR '275' OR '546' OR '264' OR 
              '322' OR '126' OR '418' OR '564' OR '385' OR '474' OR
              '527' OR '517' OR '544' OR '523' OR '118')
              MOVE SPACES              TO AM-REPORT-CODE-2
           END-IF

           IF AM-REPORT-CODE-2 NOT = SPACES 
      *       MOVE +0.1700             TO AM-LF-RET
              MOVE +0.1500             TO AM-LF-RET
              MOVE AM-REPORT-CODE-2    TO AM-RET-GRP
              MOVE 'Y'                 TO AM-RET-Y-N
           END-IF
           IF AM-REPORT-CODE-2 (1:3) = 'BNS'
              MOVE 'BNS#1'             TO AM-RET-GRP
           END-IF
           IF (AM-REPORT-CODE-2 (1:5) = 'BEITO')
              OR (AM-REPORT-CODE-2 (1:3) = 'NAB' OR 'FSB')
      *       MOVE +0.1900             TO AM-LF-RET
              MOVE +0.1700             TO AM-LF-RET
           END-IF
           IF AM-REPORT-CODE-2 (1:7) = 'MIDWEST'
      *       MOVE +0.2200             TO AM-LF-RET
              MOVE +0.2000             TO AM-LF-RET
           END-IF
           MOVE AM-LF-RET              TO AM-AH-RET
           MOVE +1                     TO SUB1
           MOVE AM-ACCOUNT             TO AM-AGT (SUB1)
           MOVE 'D'                    TO AM-COM-TYP (SUB1)
           MOVE +.45000                TO AM-L-COM (SUB1)
                                          AM-J-COM (SUB1)
                                          AM-A-COM (SUB1)
           IF SAL-ACCT-NO = '630' OR '536' OR '373' OR '490' OR '546'
              MOVE +.40000             TO AM-L-COM (SUB1)
                                          AM-J-COM (SUB1)
                                          AM-A-COM (SUB1)
           END-IF
           
           IF SAL-LVL2-AGT NOT = SPACES
              ADD +1                   TO SUB1
              MOVE '00010'             TO AM-AGT (SUB1) (1:5)
              MOVE SAL-LVL2-AGT        TO AM-AGT (SUB1) (6:3)
              MOVE '00'                TO AM-AGT (SUB1) (9:2)
              MOVE 'P'                 TO AM-COM-TYP (SUB1)
              MOVE +.03000             TO AM-L-COM (SUB1)
                                          AM-J-COM (SUB1)
                                          AM-A-COM (SUB1)
              IF AM-RET-Y-N = 'Y'
                 MOVE 'A'              TO AM-RETRO-LV-INDIC (SUB1)
              END-IF
              IF SAL-ACCT-NO = '630' OR '536'
                 MOVE +.05000          TO AM-L-COM (SUB1)
                                          AM-J-COM (SUB1)
                                          AM-A-COM (SUB1)
              END-IF
           END-IF

           IF SAL-LVL3-AGT NOT = SPACES
              ADD +1                   TO SUB1
              MOVE '00010'             TO AM-AGT (SUB1) (1:5)
              MOVE SAL-LVL3-AGT        TO AM-AGT (SUB1) (6:3)
              MOVE '00'                TO AM-AGT (SUB1) (9:2)
              MOVE 'P'                 TO AM-COM-TYP (SUB1)
              MOVE +.03000             TO AM-L-COM (SUB1)
                                          AM-J-COM (SUB1)
                                          AM-A-COM (SUB1)
              IF AM-RET-Y-N = 'Y'
                 MOVE 'A'              TO AM-RETRO-LV-INDIC (SUB1)
              END-IF
           END-IF

           ADD +1                      TO SUB1
           MOVE '0000968600'           TO AM-AGT (SUB1)
           MOVE 'P'                    TO AM-COM-TYP (SUB1)
           MOVE +.0                    TO AM-L-COM (SUB1)
                                          AM-J-COM (SUB1)
                                          AM-A-COM (SUB1)

      *       IF SAL-LVL3-PCT = '0.015'
      *          MOVE +.01500          TO AM-L-COM (SUB1)
      *                                   AM-J-COM (SUB1)
      *                                   AM-A-COM (SUB1)
      *       ELSE
      *          IF SAL-LVL3-PCT = ' 0.05'
      *             MOVE +.05000       TO AM-L-COM (SUB1)
      *                                   AM-J-COM (SUB1)
      *                                   AM-A-COM (SUB1)
      *          ELSE
      *             MOVE +.03000       TO AM-L-COM (SUB1)
      *                                   AM-J-COM (SUB1)
      *                                   AM-A-COM (SUB1)
      *          END-IF
      *       END-IF
      *    END-IF

           IF SAL-STATUS = 'A'
              MOVE '0'                 TO AM-STATUS
           ELSE
              MOVE '1'                 TO AM-STATUS
              MOVE SPACES              TO AM-REPORT-CODE-2
              MOVE +0                  TO AM-LF-RET
                                          AM-AH-RET
              MOVE SPACES              TO AM-RET-GRP
                                          AM-RET-Y-N
           END-IF

           
           MOVE ACCOUNT-MASTER         TO WS-SAVE-ACCT
           RELEASE SORT-RECORD         FROM ACCOUNT-MASTER
           IF AM-EXPIRATION-DT = X'9B41'
              MOVE '00S'               TO AM-REI-TABLE
              MOVE HIGH-VALUES         TO AM-EXPIRATION-DT
                                          AM-VG-EXPIRATION-DT
              MOVE 99999999999         TO AM-PREV-EXP-DT
              MOVE X'9B41'             TO AM-EFFECTIVE-DT
              MOVE 20030701            TO AM-EFFECT-DT
                                          AM-PREV-EFF-DT
              IF AM-RET-GRP = 'BNS#1'
                 MOVE 'BNS#2'          TO AM-RET-GRP
              END-IF
              RELEASE SORT-RECORD      FROM ACCOUNT-MASTER
           END-IF

           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.



       0110-READ-INPUT.

           READ SAL-FILE-IN AT END
              SET END-OF-INPUT TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-INPUT-CNT
           END-IF

           .
       0110-EXIT.
           EXIT.


       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.




       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
