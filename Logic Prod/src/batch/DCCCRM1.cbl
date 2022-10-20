       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCCCRM1.
       AUTHOR.     Cowtown.
       DATE-COMPILED.
      * REMARKS
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      *      THIS PROGRAM MATCHES THE OFF-LINE CERT FILE WITH THE      *
      *      ACCOUNT MASTER AND CREATES A NEW CERT MASTER WITH THE     *
      *      CLP STATE IN PLACE OF THE ACCOUNT STATE.                  *
      *      IF YOU WANT TO USE THIS IT WOULD BEHOOVE YOU TO SORT      *
      *      THE OUTPUT OF THIS FILE AFTERWARDS. I just wasn't in      *
      *      the mood to put a sort routine in this program.           *
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      *#################################################################
      *LABEL name=SORT1
      *#################################################################
      *ASSGNDD ddname='SORTIN' filename='filein' disp='i-o'
      *ASSGNDD  ddname='SORTOUT' filename='fileout' disp='o' normal='k'
      *                            abend='d' recsize='1056' recfmt='F'
      *ASSGNDD ddname='SORTWK01' type='TEMP' 
      *ASSGNDD ddname='SORTWK02' type='TEMP' 
      *ASSGNDD ddname='SORTWK03' type='TEMP' 
      *ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
      *RECORD:
      *       KEYS=(4 36 CHAR)
      *ENDSORT:
      *!
      *EXECPGM pgmname='SORT' stepname='SORT1'
      *#################################################################
      *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      * 011714  CR2013053000001  PEMA  NEW PROGRAM
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERT-FILE-IN     ASSIGN TO SYS010.

           SELECT CLP-CERT-OUT     ASSIGN TO SYS011.

           SELECT ERACCT           ASSIGN TO ERACCTT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-PRIMARY
                                   FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019. 

       DATA DIVISION.
       FILE SECTION.

       FD  CERT-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSCRT01.

       FD  CLP-CERT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  CLP-CERT-OUT-RECORD         PIC X(1056).

       FD  ERACCT.
                                       COPY ERCACCT.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   DCCCRM1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  ERACCT-FILE-STATUS      PIC XX  VALUE LOW-VALUES.
       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       77  CRT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  CRT-RECS-OUT            PIC 9(9) VALUE ZEROS.
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

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0050-PROCESS        THRU 0050-EXIT UNTIL
              (END-OF-INPUT)
PEMTST*       OR (CRT-RECS-IN > 50000)

           PERFORM 0100-CLOSE-FILES    THRU 0100-EXIT

           DISPLAY ' CERT RECORDS READ     '  CRT-RECS-IN
           DISPLAY ' CLP CERTS OUT         '  CRT-RECS-OUT
           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT CERT-FILE-IN ERACCT
               OUTPUT CLP-CERT-OUT

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT ERROR - OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

           PERFORM 0060-READ-CERT      THRU 0060-EXIT
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

           IF AM-DCC-CLP-STATE = '  ' OR '00' OR LOW-VALUES
              MOVE AM-STATE            TO AM-DCC-CLP-STATE
           END-IF
           MOVE AM-DCC-CLP-STATE       TO CR-STATE
           PERFORM 0080-WRITE-CLP-CERT THRU 0080-EXIT

           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           .
       0050-EXIT.
           EXIT.
           
       0060-READ-CERT.

           READ CERT-FILE-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO CRT-RECS-IN
           END-IF

           .
       0060-EXIT.
           EXIT.

       0070-MATCH-TO-ERACCT.

           IF (CR-ACCT-CONTROL > AM-CONTROL-A)
              OR ((CR-ACCT-CONTROL = AM-CONTROL-A)
                 AND (CR-DT >= AM-EXPIRE-DT))
                 PERFORM 0040-READ-ERACCT
                                       THRU 0040-EXIT
                 GO TO 0070-MATCH-TO-ERACCT
           ELSE
              IF (CR-ACCT-CONTROL < AM-CONTROL-A)
                 OR ((CR-ACCT-CONTROL = AM-CONTROL-A)
                    AND (CR-DT < AM-EFFECT-DT))
                 DISPLAY ' NO MATCHING ACCOUNT '
                 DISPLAY ' AM CONTROL          ' AM-CONTROL-A
                 DISPLAY ' CR CONTROL          ' CR-ACCT-CONTROL
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0070-EXIT.
           EXIT.

       0080-WRITE-CLP-CERT.

           WRITE CLP-CERT-OUT-RECORD   FROM CERTIFICATE-RECORD
           ADD +1 TO CRT-RECS-OUT

           .
       0080-EXIT.
           EXIT.

       0100-CLOSE-FILES.

           CLOSE CERT-FILE-IN CLP-CERT-OUT
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
