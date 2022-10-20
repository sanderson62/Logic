       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDMCF1.
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
040902******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT MCRDFT           ASSIGN TO MCRDFT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS MC-CONTROL-PRIMARY
                                   FILE STATUS IS MCRDFT-FILE-STATUS.

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
121903     10  M420C-ACCT-STATE        PIC X(02).
121903     10  M420C-CLAIM-TYPE        PIC X(01).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDMCF1 WORKING STORAGE      '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-MCRDFT             VALUE 'Y'.
           88  MORE-MCRDFT               VALUE ' '.
       77  MICR-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  MICR-RECS-DEL            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.

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

           EJECT
       PROCEDURE DIVISION.

103002*************************************************************     
103002*************************************************************     
           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-MICR   THRU 0100-EXIT UNTIL
                 END-OF-MCRDFT

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' MICR RECORDS READ    '  MICR-RECS-IN
           DISPLAY ' MICR RECORDS DELETED '  MICR-RECS-DEL
           GOBACK
           .

       0100-PROCESS-MICR.

           IF M420C-CHECK-DATE NOT = '01/07/05'
              PERFORM 0350-DELETE-MICR THRU 0350-EXIT
           END-IF


           PERFORM 0200-READ-MICR      THRU 0200-EXIT

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

       0350-DELETE-MICR.

           DELETE MCRDFT
           ADD 1 TO MICR-RECS-DEL

           .

       0350-EXIT.
           EXIT.
       0400-OPEN-FILES.

           OPEN I-O MCRDFT

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE MCRDFT

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



