       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNB221.
      *AUTHOR.     SUZAN VUKOV.
      *DATE-COMPILED.
 
121703*  THIS PROGRAM IS NOT BEING USED IN THE LOGIC SYSTEM
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      *                          SMVA  NEW PROGRAM           
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ANBPAIDD-IN      ASSIGN TO ANBPAIDD
                                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ELRCON           ASSIGN TO ELRCON
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS RANDOM    
                                   RECORD KEY IS ELRCON-KEY
                                   FILE STATUS IS WS-ELRCON-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.


       DATA DIVISION.
       FILE SECTION.

       FD  ANBPAIDD-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  ANB-RECORD.
           05  ANB-ACCOUNT               PIC X(07).
           05  ANB-DRAFT-NO              PIC X(07).
           05  ANB-DRAFT-AMT             PIC S9(07)V9(02).
           05  ANB-CASHED-DATE.
               10  ANB-CASHED-MM         PIC X(02).
               10  FILLER                PIC X(01).
               10  ANB-CASHED-DD         PIC X(02).
               10  FILLER                PIC X(01).
               10  ANB-CASHED-CCYY.
                   15  ANB-CASHED-CC     PIC X(02).
                   15  ANB-CASHED-YY     PIC X(02).


       FD  ELRCON.
       01  ELRCON-RECORD.
           05  FILLER                    PIC X(02).
           05  ELRCON-KEY                PIC X(19).
           05  FILLER                    PIC X(175).



       FD  DISK-DATE
       COPY ELCDTEFD.


       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   FNB221 WORKING STORAGE       '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       01  WS-MISC.
           05  WS-EOF-SW                 PIC X(01)  VALUE SPACES.
               88  END-OF-ANBPAIDD                  VALUE 'Y'.
           05  WS-ANB-RECS-IN            PIC 9(09)  VALUE ZEROS.
           05  WS-ANB-RECS-OUT           PIC 9(09)  VALUE ZEROS.
           05  WS-ANB-GL-ACCOUNT         PIC X(10)  VALUE '2724500200'.
           05  WS-ELRCON-FILE-STATUS     PIC X(02)  VALUE ZEROS.

       01  WS-ELCDTERX-FIELDS.
           05  PGM-SUB            COMP   PIC S9(4)  VALUE +221.

       01  WS-ELCABEND-FIELDS.
00361      05  WS-ABEND-MESSAGE          PIC X(80)  VALUE SPACES.   
00362      05  WS-ABEND-FILE-STATUS      PIC X(02)  VALUE ZEROS.    
00360      05  WS-RETURN-CODE     COMP   PIC S9(4)  VALUE +0.       
00363      05  WS-ZERO            COMP-3 PIC S9(01) VALUE +0.       
00364                                                                   

       COPY ELCRCON.

      *************************************************************
      *  DATE-CONVERSION-DATA USED BY ELDATCX
      *************************************************************
       COPY ELCDATE.


      *************************************************************
      *  DATE-CARD LAYOUT USED BY ELCDTERX 
      *************************************************************
       COPY ELCDTECX.


      *************************************************************
      *  OTHER FIELDS USED BY ELCDTERX 
      *************************************************************
       COPY ELCDTEVR. 

           EJECT
       PROCEDURE DIVISION.
      *************************************************************
      **** DISK-DATE ROUTINE THAT PROVIDES COMPANY INFORMATION.
           COPY ELCDTERX.
      *************************************************************

       0000-MAIN.
 
           PERFORM 0400-OPEN-FILES       THRU 0400-EXIT
           PERFORM 0100-PROCESS-ANBPAIDD THRU 0100-EXIT UNTIL
                 END-OF-ANBPAIDD

           PERFORM 0500-CLOSE-FILES      THRU 0500-EXIT

           DISPLAY ' ANB RECORDS READ    '  WS-ANB-RECS-IN
           DISPLAY ' ANB RECORDS WRITTEN '  WS-ANB-RECS-OUT
           GOBACK

           .

       0100-PROCESS-ANBPAIDD.

           READ ANBPAIDD-IN
               AT END SET END-OF-ANBPAIDD TO TRUE
               GO TO 0100-EXIT
           END-READ

           ADD +1                         TO WS-ANB-RECS-IN

           INITIALIZE CHECK-RECONCILIATION            

           IF ANB-ACCOUNT = '5149002'
               MOVE WS-ANB-GL-ACCOUNT     TO RC-GL-ACCOUNT-NO
           ELSE
               DISPLAY " American Natl Bank acct# not recognized "    
                   ANB-ACCOUNT " " ANB-DRAFT-NO 
               GO TO 0100-EXIT
           END-IF

           MOVE 'RC'                      TO RC-RECORD-ID
           MOVE DTE-CLASIC-COMPANY-CD     TO RC-COMPANY-CD
           MOVE ANB-DRAFT-NO              TO RC-CHECK-NO
           MOVE 'C'                       TO RC-CHECK-ORIGIN
           MOVE ANB-DRAFT-AMT             TO RC-CASHED-AMOUNT
           MOVE 'R'                       TO RC-STATUS 

           MOVE ANB-CASHED-DATE           TO DC-GREG-DATE-1-EDIT
           MOVE ANB-CASHED-YY             TO DC-EDIT1-YEAR
           MOVE '2'                       TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION   THRU 8500-EXIT 
           IF NO-CONVERSION-ERROR
               MOVE ANB-CASHED-CCYY       TO RC-STATUS-YYYY
               MOVE ANB-CASHED-MM         TO RC-STATUS-MM
               MOVE ANB-CASHED-DD         TO RC-STATUS-DD
           ELSE
               DISPLAY 'INVALID ANB CASHED DATE ' ANB-CASHED-DATE
               PERFORM ABEND-PGM          THRU APS-EXIT
           END-IF

           MOVE CHECK-RECONCILIATION      TO ELRCON-RECORD
           PERFORM 0300-WRITE-ELRCON      THRU 0300-EXIT

           .

       0100-EXIT.
           EXIT.


       0300-WRITE-ELRCON.

           WRITE ELRCON-RECORD       

           EVALUATE TRUE
           WHEN WS-ELRCON-FILE-STATUS = '00'
               ADD +1                     TO WS-ANB-RECS-OUT
           WHEN WS-ELRCON-FILE-STATUS = '22'
               DISPLAY 'Duplicate key for draft# ' RC-CHECK-NO
           WHEN OTHER
               DISPLAY ' Vsam write error ' WS-ELRCON-FILE-STATUS
           END-EVALUATE

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT  ANBPAIDD-IN
                OUTPUT ELRCON  

           IF WS-ELRCON-FILE-STATUS = '00'
               CONTINUE
           ELSE
               DISPLAY 'ELRCON open error ' WS-ELRCON-FILE-STATUS
           END-IF

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ANBPAIDD-IN
                 ELRCON  

           .

       0500-EXIT.
           EXIT.


      *       IF MCRDFT-FILE-STATUS NOT = '00'
      *          display ' MICRDRFT FILE IS CORRUPT START'
      *          DISPLAY ' WERE THE CHECKS RELEASED PROPERLY ?'
      *          move ' MICRDRFT FILE IS CORRUPT ' to ws-abend-message
      *          move mcrdft-file-status     to ws-abend-file-status
      *          PERFORM ABEND-PGM
      *          SET END-OF-MCRDFT     TO TRUE
      *       END-IF

      *0550-EXIT.
      *    EXIT.


       8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8500-EXIT.
           EXIT.

       ABEND-PGM.                                                       
           DISPLAY '******************************'                     
           DISPLAY '***** ABEND PGM **************'                     
           DISPLAY '******************************'                     

           COPY ELCABEND.                       



