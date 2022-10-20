       IDENTIFICATION DIVISION.
       PROGRAM-ID. fixmicr.
       AUTHOR.     PABLO.
       DATE-COMPILED.
103002******************************************************************
103002*                   C H A N G E   L O G
103002*
103002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
103002*-----------------------------------------------------------------
103002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
103002* EFFECTIVE    NUMBER
103002*-----------------------------------------------------------------
103002* 103002                   PEMA  ADD DCC PROCESSING           
040902******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT MCRDFT           ASSIGN TO MCRDFT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS MC-CONTROL-PRIMARY
                                   FILE STATUS IS MCRDFT-FILE-STATUS.

           SELECT MICR-OUT         ASSIGN TO MICROUT
                                   ORGANIZATION IS LINE SEQUENTIAL.

103002     SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  MCRDFT.
       01  MICR-REC-IN.
           10  MC-CONTROL-PRIMARY.
               16  M420C-FORM          PIC X(4).
               16  M420C-DRAFT-ORDER   PIC 9(5).
               16  M420C-DRAFT         PIC X(10).
           10  M420C-PRINT-COUNT       PIC S9(4) COMP.
           10  M420C-SEQ-NUMBER        PIC S9(4) COMP.
           10  M420C-AMOUNT-PAID       PIC S9(9)V9(2) COMP-3.
           10  M420C-COMPANY-NAME      PIC X(43).
           10  M420C-CSO-ADDRESS       PIC X(33).
           10  M420C-CLAIM-NO          PIC X(7).
           10  M420C-CERT-NO           PIC X(11).
           10  M420C-ACCT-NO           PIC X(10).
           10  M420C-PLAN-CODE         PIC X(4).
           10  M420C-PAID-FROM-DATE    PIC X(8).
           10  M420C-PAID-THRU-DATE    PIC X(8).
           10  M420C-PAYMENT-TYPE      PIC X.
           10  M420C-CC-ACCT           PIC X(7).
           10  M420C-CC-ACCT-NUMBER    PIC X(18).
           10  M420C-TYPE-MESSAGE      PIC X(15).
           10  M420C-FINAL-MESS9       PIC X(17).
           10  M420C-FINAL-MESS10      PIC X(17).
           10  M420C-FINAL-MESS11      PIC X(17).
           10  M420C-MEMBER-NAME       PIC X(30).
           10  M420C-MEMBER-ADDRESS1   PIC X(30).
           10  M420C-MEMBER-ADDRESS2   PIC X(30).
           10  M420C-MEMBER-ADDRESS3   PIC X(30).
           10  M420C-MEMBER-ADDRESS4   PIC X(30).
           10  M420C-MEMBER-ZIP-CODE   PIC X(9).
           10  M420C-3RDADD-NAME       PIC X(30).
           10  M420C-3RDADD-LINE1      PIC X(30).
           10  M420C-3RDADD-LINE2      PIC X(40).
           10  M420C-3RDADD-LINE3      PIC X(40).
           10  M420C-3RDADD-ZIP        PIC X(9).
           10  M420C-CHECK-DATE        PIC X(8).
           10  M420C-DFT-NOTES1        PIC X(40).
           10  M420C-DFT-NOTES2        PIC X(40).
           10  M420C-PAYEE-NAME        PIC X(30).
           10  M420C-PAYEE-ADDRESS1    PIC X(30).
           10  M420C-PAYEE-ADDRESS2    PIC X(30).
           10  M420C-PAYEE-ADDRESS3    PIC X(30).
           10  M420C-PAYEE-ADDRESS4    PIC X(30).
           10  M420C-PAYEE-ZIP-CODE    PIC X(9).
           10  M420C-REPLY-DATE        PIC X(5).
           10  M420C-SIGNATURE         PIC X.
           10  M420C-LOAN-NUMBER       PIC X(25).
           10  M420C-DRAFT-MESSAGES.
               16  M420C-DRAFT-MESSAGE OCCURS 5 TIMES PIC X(70).
      /

       FD  MICR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  MICR-OUT-REC                PIC X(1202).

103002 FD  DISK-DATE                                                    
103002     COPY ELCDTEFD.                                               
103002                                                                  


           EJECT

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMMCX1 WORKING STORAGE      '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-MCRDFT             VALUE 'Y'.
           88  MORE-MCRDFT               VALUE ' '.
       77  MICR-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  MICR-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
      /

       01  MICR-DETAIL-RECORD.
           12  MICR-FORM               PIC X(4).
           12  MICR-DRAFT-ORDER        PIC 99999.
           12  MICR-DRAFT              PIC X(10).
           12  MICR-PRINT-COUNT        PIC -99999999.
           12  MICR-SEQ-NUMBER         PIC -99999999.
           12  MICR-AMOUNT-PAID        PIC -zzzzzzzz9.99.
           12  MICR-COMPANY-NAME       PIC X(43).
           12  MICR-CSO-ADDRESS        PIC X(33).
           12  MICR-CLAIM-NO           PIC X(7).
           12  MICR-CERT-NO            PIC X(11).
           12  MICR-ACCT-NO            PIC X(10).
           12  MICR-PLAN-CODE          PIC X(4).
           12  MICR-PAID-FROM-DATE     PIC X(8).
           12  MICR-PAID-THRU-DATE     PIC X(8).
           12  MICR-PAYMENT-TYPE       PIC X.
           12  MICR-CC-ACCT            PIC X(7).
           12  MICR-CC-ACCT-NUMBER     PIC X(18).
           12  MICR-TYPE-MESSAGE       PIC X(15).
           12  MICR-FINAL-MESS9        PIC X(17).
           12  MICR-FINAL-MESS10       PIC X(17).
           12  MICR-FINAL-MESS11       PIC X(17).
           12  MICR-MEMBER-NAME        PIC X(30).
           12  MICR-MEMBER-ADDRESS1    PIC X(30).
           12  MICR-MEMBER-ADDRESS2    PIC X(30).
           12  MICR-MEMBER-ADDRESS3    PIC X(30).
           12  MICR-MEMBER-ADDRESS4    PIC X(30).
           12  MICR-MEMBER-ZIP-CODE    PIC X(9).
           12  MICR-3RDADD-NAME        PIC X(30).
           12  MICR-3RDADD-LINE1       PIC X(30).
           12  MICR-3RDADD-LINE2       PIC X(40).
           12  MICR-3RDADD-LINE3       PIC X(40).
           12  MICR-3RDADD-ZIP         PIC X(9).
           12  MICR-CHECK-DATE         PIC X(8).
           12  MICR-DFT-NOTES1         PIC X(40).
           12  MICR-DFT-NOTES2         PIC X(40).
           12  MICR-PAYEE-NAME         PIC X(30).
           12  MICR-PAYEE-ADDRESS1     PIC X(30).
           12  MICR-PAYEE-ADDRESS2     PIC X(30).
           12  MICR-PAYEE-ADDRESS3     PIC X(30).
           12  MICR-PAYEE-ADDRESS4     PIC X(30).
           12  MICR-PAYEE-ZIP-CODE     PIC X(9).
           12  MICR-REPLY-DATE         PIC X(5).
           12  MICR-SIGNATURE          PIC X.
           12  MICR-LOAN-NUMBER        PIC X(25).
           12  MICR-DRAFT-MESSAGES.
               16  MICR-DRAFT-MESSAGE OCCURS 5 TIMES   PIC X(70).


      /
       01  WS-MISC.
103002     05  PGM-SUB                     PIC S9(4) COMP VALUE +310.
00360      05  WS-RETURN-CODE        PIC S9(4)   COMP   VALUE +0.       
00361      05  WS-ABEND-MESSAGE      PIC X(80)          VALUE SPACES.   
00362      05  WS-ABEND-FILE-STATUS  PIC XX             VALUE ZEROS.    
00363      05  WS-ZERO               PIC S9      COMP-3 VALUE +0.       
00364                                                                   
00365      05  WS-ABEND-CODE         PIC 9(4).                          
00366      05  ABEND-CODE  REDEFINES  WS-ABEND-CODE.                    
00367          10  ABEND-CODE-1    PIC XX.                              
00368          10  ABEND-CODE-2.                                        
00369              15  AC2-ONE     PIC X.                               
00370              15  AC2-TWO     PIC X.                               
           05  WS-SAVE-MCRFFT          PIC X(1507) VALUE LOW-VALUES.
           05  MCRDFT-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.
103002                                 COPY ELCDTECX.
103002                                 COPY ELCDTEVR.                                               

           EJECT
       PROCEDURE DIVISION.

103002*************************************************************     
103002                                 COPY ELCDTERX.                   
103002*************************************************************     
           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-MICR   THRU 0100-EXIT UNTIL
                 END-OF-MCRDFT

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' MICR RECORDS READ    '  MICR-RECS-IN
           DISPLAY ' MICR RECORDS WRITTEN '  MICR-RECS-OUT
           GOBACK
           .

       0100-PROCESS-MICR.

011603     IF M420C-CHECK-DATE NOT = '01/16/03'
011603         PERFORM 0200-READ-MICR      THRU 0200-EXIT
011603         GO TO 0100-EXIT
011603     ELSE
           MOVE M420C-FORM             TO MICR-FORM

           MOVE M420C-FORM             TO MICR-FORM
           MOVE M420C-DRAFT-ORDER      TO MICR-DRAFT-ORDER
           MOVE M420C-DRAFT            TO MICR-DRAFT
           MOVE M420C-PRINT-COUNT      TO MICR-PRINT-COUNT
           MOVE M420C-SEQ-NUMBER       TO MICR-SEQ-NUMBER
           MOVE M420C-AMOUNT-PAID      TO MICR-AMOUNT-PAID
           MOVE M420C-COMPANY-NAME     TO MICR-COMPANY-NAME
           MOVE M420C-CSO-ADDRESS      TO MICR-CSO-ADDRESS
           MOVE M420C-CLAIM-NO         TO MICR-CLAIM-NO
           MOVE M420C-CERT-NO          TO MICR-CERT-NO
           MOVE M420C-ACCT-NO          TO MICR-ACCT-NO
           MOVE M420C-PLAN-CODE        TO MICR-PLAN-CODE
           MOVE M420C-PAID-FROM-DATE   TO MICR-PAID-FROM-DATE
           MOVE M420C-PAID-THRU-DATE   TO MICR-PAID-THRU-DATE
           MOVE M420C-PAYMENT-TYPE     TO MICR-PAYMENT-TYPE
           MOVE M420C-CC-ACCT          TO MICR-CC-ACCT
           MOVE M420C-CC-ACCT-NUMBER   TO MICR-CC-ACCT-NUMBER
           MOVE M420C-TYPE-MESSAGE     TO MICR-TYPE-MESSAGE
           MOVE M420C-FINAL-MESS9      TO MICR-FINAL-MESS9
           MOVE M420C-FINAL-MESS10     TO MICR-FINAL-MESS10
           MOVE M420C-FINAL-MESS11     TO MICR-FINAL-MESS11
           MOVE M420C-MEMBER-NAME      TO MICR-MEMBER-NAME
           MOVE M420C-MEMBER-ADDRESS1  TO MICR-MEMBER-ADDRESS1
           MOVE M420C-MEMBER-ADDRESS2  TO MICR-MEMBER-ADDRESS2
           MOVE M420C-MEMBER-ADDRESS3  TO MICR-MEMBER-ADDRESS3
           MOVE M420C-MEMBER-ADDRESS4  TO MICR-MEMBER-ADDRESS4
           MOVE M420C-MEMBER-ZIP-CODE  TO MICR-MEMBER-ZIP-CODE
           if micr-member-address4 = spaces
              move micr-member-zip-code to micr-member-address4
              move spaces               to micr-member-zip-code
           end-if
           if micr-member-address3 = spaces
              move micr-member-address4 to micr-member-address3
              move micr-member-zip-code to micr-member-address4
              move spaces               to micr-member-zip-code
           end-if
           if micr-member-address2 = spaces
              move micr-member-address3 to micr-member-address2
              move micr-member-address4 to micr-member-address3
              move micr-member-zip-code to micr-member-address4
              move spaces               to micr-member-zip-code
           end-if
           if micr-member-address1 = spaces
              move micr-member-address2 to micr-member-address1
              move micr-member-address3 to micr-member-address2
              move micr-member-address4 to micr-member-address3
              move micr-member-zip-code to micr-member-address4
              move spaces               to micr-member-zip-code
           end-if
           MOVE M420C-3RDADD-NAME      TO MICR-3RDADD-NAME
           MOVE M420C-3RDADD-LINE1     TO MICR-3RDADD-LINE1
           MOVE M420C-3RDADD-LINE2     TO MICR-3RDADD-LINE2
           MOVE M420C-3RDADD-LINE3     TO MICR-3RDADD-LINE3
           MOVE M420C-3RDADD-ZIP       TO MICR-3RDADD-ZIP
           if micr-3rdadd-line3 = spaces
              move micr-3rdadd-zip      to micr-3rdadd-line3
              move spaces               to micr-3rdadd-zip
           end-if
           if micr-3rdadd-line2 = spaces
              move micr-3rdadd-line3    to micr-3rdadd-line2
              move micr-3rdadd-zip      to micr-3rdadd-line3
              move spaces               to micr-3rdadd-zip
           end-if
           if micr-3rdadd-line1 = spaces
              move micr-3rdadd-line2    to micr-3rdadd-line1
              move micr-3rdadd-line3    to micr-3rdadd-line2
              move micr-3rdadd-zip      to micr-3rdadd-line3
              move spaces               to micr-3rdadd-zip
           end-if
           if micr-3rdadd-name     = spaces
              move micr-3rdadd-line1    to micr-3rdadd-name
              move micr-3rdadd-line2    to micr-3rdadd-line1
              move micr-3rdadd-line3    to micr-3rdadd-line2
              move micr-3rdadd-zip      to micr-3rdadd-line3
              move spaces               to micr-3rdadd-zip
           end-if
           MOVE M420C-CHECK-DATE       TO MICR-CHECK-DATE
           MOVE M420C-DFT-NOTES1       TO MICR-DFT-NOTES1
           MOVE M420C-DFT-NOTES2       TO MICR-DFT-NOTES2
           MOVE M420C-PAYEE-NAME       TO MICR-PAYEE-NAME
           MOVE M420C-PAYEE-ADDRESS1   TO MICR-PAYEE-ADDRESS1
           MOVE M420C-PAYEE-ADDRESS2   TO MICR-PAYEE-ADDRESS2
           MOVE M420C-PAYEE-ADDRESS3   TO MICR-PAYEE-ADDRESS3
           MOVE M420C-PAYEE-ADDRESS4   TO MICR-PAYEE-ADDRESS4
           MOVE M420C-PAYEE-ZIP-CODE   TO MICR-PAYEE-ZIP-CODE
           if micr-payee-address4 = spaces
              move micr-payee-zip-code to micr-payee-address4
              move spaces              to micr-payee-zip-code
           end-if
           if micr-payee-address3 = spaces
              move micr-payee-address4 to micr-payee-address3
              move micr-payee-zip-code to micr-payee-address4
              move spaces              to micr-payee-zip-code
           end-if
           if micr-payee-address2 = spaces
              move micr-payee-address3 to micr-payee-address2
              move micr-payee-address4 to micr-payee-address3
              move micr-payee-zip-code to micr-payee-address4
              move spaces              to micr-payee-zip-code
           end-if
           if micr-payee-address1 = spaces
              move micr-payee-address2 to micr-payee-address1
              move micr-payee-address3 to micr-payee-address2
              move micr-payee-address4 to micr-payee-address3
              move micr-payee-zip-code to micr-payee-address4
              move spaces              to micr-payee-zip-code
           end-if
           MOVE M420C-REPLY-DATE       TO MICR-REPLY-DATE
           MOVE M420C-SIGNATURE        TO MICR-SIGNATURE
           MOVE M420C-LOAN-NUMBER      TO MICR-LOAN-NUMBER
           MOVE M420C-DRAFT-MESSAGES   TO MICR-DRAFT-MESSAGES

103002     IF ((DTE-CLIENT = 'CID')
103002        AND (MICR-FORM = '420C'))
103002                OR
103002        ((DTE-CLIENT = 'DCC')
103002        AND (MICR-FORM = 'DCC1'))
103002        PERFORM 0300-WRITE-MICR  THRU 0300-EXIT
           END-IF

           PERFORM 0200-READ-MICR      THRU 0200-EXIT

      *    IF MICR-RECS-IN > 200
      *       SET END-OF-MCRDFT        TO TRUE
      *    END-IF

           .

       0100-EXIT.
           EXIT.

       0200-READ-MICR.

           READ MCRDFT NEXT RECORD

           IF MCRDFT-FILE-STATUS = '10' OR '23'
              SET END-OF-MCRDFT        TO TRUE
           ELSE
              IF MCRDFT-FILE-STATUS NOT = '00'
                 display ' MICRDRFT FILE IS CORRUPT READ NEXT'
                 DISPLAY ' WERE THE CHECKS RELEASED PROPERLY ?'
                 move ' MICRDRFT FILE IS CORRUPT ' to ws-abend-message
                 move mcrdft-file-status     to ws-abend-file-status
                 PERFORM ABEND-PGM
                 SET END-OF-MCRDFT     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-MCRDFT
              ADD 1 TO MICR-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-MICR.

           WRITE MICR-OUT-REC FROM MICR-DETAIL-RECORD
           ADD 1 TO MICR-RECS-OUT

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT MCRDFT
               OUTPUT MICR-OUT

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE MCRDFT MICR-OUT

           .

       0500-EXIT.
           EXIT.

       0550-START-MICR.

           MOVE LOW-VALUES             TO MC-CONTROL-PRIMARY

           START MCRDFT KEY IS NOT < MC-CONTROL-PRIMARY

           IF MCRDFT-FILE-STATUS = '10' OR '23'
              display ' MICRDRFT FILE IS EMPTY '
              DISPLAY ' WERE THE CHECKS RELEASED PROPERLY ?'
              move ' MICRDRFT FILE IS EMPTY ' to ws-abend-message
              move mcrdft-file-status     to ws-abend-file-status
              PERFORM ABEND-PGM
              SET END-OF-MCRDFT        TO TRUE
           ELSE
              IF MCRDFT-FILE-STATUS NOT = '00'
                 display ' MICRDRFT FILE IS CORRUPT START'
                 DISPLAY ' WERE THE CHECKS RELEASED PROPERLY ?'
                 move ' MICRDRFT FILE IS CORRUPT ' to ws-abend-message
                 move mcrdft-file-status     to ws-abend-file-status
                 PERFORM ABEND-PGM
                 SET END-OF-MCRDFT     TO TRUE
              END-IF
           END-IF

           .

       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO MICR-DETAIL-RECORD

           PERFORM 0550-START-MICR     THRU 0550-EXIT
           PERFORM 0200-READ-MICR      THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.
12500  ABEND-PGM.                                                       
CIDMOD     DISPLAY '******************************'                     
CIDMOD     DISPLAY '***** ABEND PGM **************'                     
CIDMOD     DISPLAY '******************************'                     
12501                              COPY ELCABEND.                       
12502      EJECT                                                        



