       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIBOWCNV.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 032306                   PEMA  ADD BOW LOAN NUMBER
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  BOW-IN              ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  BOW-OUT             ASSIGN TO SYS012.
       DATA DIVISION.

       FILE SECTION.

       FD  BOW-IN
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

       01  BOW-REC1.
           05  BOW-REC1-ID             PIC XXX.
           05  FILLER                  PIC X(477).
       01  BOW-REC2.
           05  BOW-REC2-ID             PIC XXX.
           05  FILLER                  PIC X(477).
       01  BOW-REC3.
           05  BOW-REC3-ID             PIC XXX.
           05  FILLER                  PIC X(477).

       FD  BOW-OUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS BOW-OUT-RECORD.

       01  BOW-OUT-RECORD          PIC X(480).

       WORKING-STORAGE SECTION.                                         00007200
       77  FILLER  PIC X(32) VALUE '********************************'.  00007300
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  00007400
       77  FILLER  PIC X(32) VALUE '********************************'.  00007500
       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-BOW                     VALUE 'Y'.
       77  RECS-IN                     PIC 9(7)  VALUE ZEROS.
       77  RECS-OUT                    PIC 9(7)  VALUE ZEROS.
       77  S1                          PIC S999  VALUE +0 COMP-3.
       77  S2                          PIC S999  VALUE +0 COMP-3.
       77  WS-WORK-CERT                PIC X(10).
       
                                       COPY ERCBOWRL.

       01  BOW-RECORD                  PIC X(450)  VALUE SPACES.

       PROCEDURE DIVISION.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT
           PERFORM 0030-PROCESS-BOW    THRU 0030-EXIT UNTIL
              END-OF-BOW
           PERFORM 0015-CLOSE-FILES    THRU 0015-EXIT
           
           DISPLAY ' RECORDS IN  ' RECS-IN
           DISPLAY ' RECORDS OUT ' RECS-OUT
           
           GOBACK
           .
       0010-OPEN-FILES.

           OPEN INPUT  BOW-IN
                OUTPUT BOW-OUT

           .
       0010-EXIT.
           EXIT.

       0015-CLOSE-FILES.

           CLOSE BOW-IN  BOW-OUT

           .
       0015-EXIT.
           EXIT.

       0020-INIT.
       
           PERFORM 0040-READ-BOW       THRU 0040-EXIT
           
           .
       0020-EXIT.
           EXIT.

       0030-PROCESS-BOW.

           IF BOW-REC2-ID = 'SP1'
              MOVE BOW-REC1            TO BOW-RECORD
           ELSE
              IF BOW-REC2-ID = 'SP3'
                 MOVE BOW-REC3         TO BOW-RECORD
              ELSE
                 IF BOW-REC2-ID = 'SP2'
                    MOVE BOW-REC2      TO BANK-OF-THE-WEST-INPUT
                    PERFORM 0060-BUILD-ACCOUNT
                                       THRU 0060-EXIT
                    PERFORM 0070-FIX-CERT-NO
                                       THRU 0070-EXIT
                 END-IF
              END-IF
           END-IF
           
           PERFORM 0050-WRITE-BOW      THRU 0050-EXIT
           PERFORM 0040-READ-BOW       THRU 0040-EXIT
           
           .
       0030-EXIT.
           EXIT.
                
       0040-READ-BOW.

           READ BOW-IN AT END
                SET END-OF-BOW         TO TRUE
           END-READ

           IF NOT END-OF-BOW
              ADD 1                    TO RECS-IN
           END-IF

           .
       0040-EXIT.
           EXIT.

       0050-WRITE-BOW.
       
           MOVE BOW-RECORD             TO BOW-OUT-RECORD
           WRITE BOW-OUT-RECORD
           ADD 1                       TO RECS-OUT
           
           .
       0050-EXIT.
           EXIT.
           
       0060-BUILD-ACCOUNT.
       
      * FIRST, WE NEED TO SAVE THE BRANCH NO AND LOAN #

032306     STRING BWD-BRANCH-NO '-' BWD-LOAN-NO DELIMITED BY SIZE
032306        INTO BWD-ENROLLER-NAME
032306     END-STRING
      
           EVALUATE BWD-STATE
              WHEN 'NE'
                 MOVE '0000015860'     TO BWD-ACCOUNT-NO
              WHEN 'CO'
                 MOVE '0000017500'     TO BWD-ACCOUNT-NO
              WHEN 'KS'
                 MOVE '0000019580'     TO BWD-ACCOUNT-NO
              WHEN 'IA'
                 MOVE '0000610400'     TO BWD-ACCOUNT-NO
              WHEN 'MO'
                 MOVE '0000610500'     TO BWD-ACCOUNT-NO
              WHEN 'OK'
                 MOVE '0000490700'     TO BWD-ACCOUNT-NO
              WHEN 'AZ'
                 MOVE '0000596100'     TO BWD-ACCOUNT-NO
              WHEN 'IL'
                 MOVE '0000596200'     TO BWD-ACCOUNT-NO
              WHEN 'IN'
                 MOVE '0000596300'     TO BWD-ACCOUNT-NO
              WHEN 'MN'
                 MOVE '0000596400'     TO BWD-ACCOUNT-NO
              WHEN 'SD'
                 MOVE '0000596500'     TO BWD-ACCOUNT-NO
              WHEN 'WY'
                 MOVE '0000596600'     TO BWD-ACCOUNT-NO
              WHEN OTHER
                 DISPLAY 'INVALID STATE COULD NOT BLD ACCT NO '
                 DISPLAY BWD-STATE ' ' BWD-LOAN-NO
           END-EVALUATE

      *    MOVE BANK-OF-THE-WEST-INPUT TO BOW-RECORD

           .
       0060-EXIT.
           EXIT.       

       0070-FIX-CERT-NO.
       
           DISPLAY ' BEFORE CERT NO ' BWD-PRIOR-CERT-NO
           MOVE +1                     TO S2
           MOVE ZEROS                  TO WS-WORK-CERT
           
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (S1 > +10)
              OR (BWD-PRIOR-CERT-NO (S1:1) NOT = ZEROS)
           END-PERFORM
           
           IF S1 > +10
              CONTINUE
           ELSE
              COMPUTE S2 = (10 - S1) + 1
              MOVE BWD-PRIOR-CERT-NO (S1:S2) TO WS-WORK-CERT (1:S2)
              MOVE WS-WORK-CERT        TO BWD-PRIOR-CERT-NO
              DISPLAY ' AFTER CERT NO ' BWD-PRIOR-CERT-NO
           END-IF
              
              
           MOVE BANK-OF-THE-WEST-INPUT TO BOW-RECORD

           .
       0070-EXIT.
           EXIT.       




