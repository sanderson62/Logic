       IDENTIFICATION DIVISION.                                         
                                                                        
       PROGRAM-ID.                 EL324R.
      *AUTHOR.     PABLO.                                               
      *            TEXAS.                                               
      *DATE-COMPILED.                                                   
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
      *REMARKS.                                                         
      *        THIS PROGRAM PRINTS A REGISTER OF ALL RESCISSIONS.
101110******************************************************************
101110*                   C H A N G E   L O G
101110*
101110* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101110*-----------------------------------------------------------------
101110*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101110* EFFECTIVE    NUMBER
101110*-----------------------------------------------------------------
101110* 101110                   PEMA  CORRECTIONS TO DATA AND FORMAT  
101110******************************************************************
       ENVIRONMENT DIVISION.                                            
                                                                        
       INPUT-OUTPUT SECTION.                                            
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT REPORTS-EXTRACT-FILE ASSIGN TO SYS010.
                                                                        
           SELECT DISK-DATE        ASSIGN TO SYS019.
                                                                        
           SELECT PRNTR            ASSIGN TO SYS008.

       DATA DIVISION.                                                   
                                                                        
       FILE SECTION.                                                    
                                                                        
       FD  REPORTS-EXTRACT-FILE COPY ELCEXTFD.                          
                                                                        
                                   COPY ELCEXTR.                        
                                                                        
       FD  DISK-DATE               COPY ELCDTEFD.                       
                                                                        
       FD  PRNTR                   COPY ELCPRTFD.                       
                                                                        
       WORKING-STORAGE SECTION.                                         
       01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
                                                                        
       77  FILLER  PIC X(32)   VALUE '********************************'.
       77  FILLER  PIC X(32)   VALUE '*    EL324R  WORKING STORAGE   *'.
       77  FILLER  PIC X(32)   VALUE '********* VMOD=2.001 ***********'.
       77  WS-EOF-SW                   PIC X   VALUE ' '.
           88  END-OF-INPUT               VALUE 'Y'.
       77  WS-IP-RECS                  PIC 9(9)   VALUE ZEROS.
101110 77  WS-LAST-NO-OF-DAYS          PIC 9999   VALUE ZEROS.
       01  FILLER                          COMP-3.                      
           05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   
           05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +59.   
           05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  
           05  WS-REPORT-SW                PIC S9          VALUE +1.    
           05  WS-PRINT-SW                 PIC S9          VALUE ZERO.  
           05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.  
           05  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.  
           05  WS-ZERO                     PIC S9          VALUE ZERO.  

       01  FILLER                          COMP SYNC.                   
           05  PGM-SUB                     PIC S9(4)       VALUE +324.  
           05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  
                                                                        
       01  FILLER.                                                      
           05  ABEND-CODE                  PIC X(4).                    
           05  ABEND-OPTION                PIC X.                       
           05  OLC-REPORT-NAME             PIC X(5) VALUE 'EL324'.      
           05  X                           PIC X           VALUE SPACE. 
                                                                        
           05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.
                                                                        
           05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.
                                                                        
           05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  
                                                                        
           05  WS-FILE-ERROR-MESSAGE.                                   
               10  FILLER                  PIC X(24)       VALUE        
                   'ERROR OCCURED OPENING - '.                          
               10  WS-FEM-FILE-NAME        PIC X(8).                    
                                                                        
       01  WS-HEADING1.                                                 
           05  FILLER                      PIC X(49)       VALUE '1'.   
           05  WS-H1-TITLE                 PIC X(71)       VALUE        
               '  RESCISSION REGISTER'.                               
           05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL324R'.     
                                                                        
       01  WS-HEADING2.                                                 
           05  FILLER                      PIC X(45)       VALUE SPACES.
           05  WS-H2-CLIENT-NAME           PIC X(75)       VALUE SPACES.
           05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.
           05  FILLER                      PIC X           VALUE SPACES.
                                                                        
       01  WS-HEADING3.                                                 
           05  FILLER                      PIC X(51)       VALUE SPACES.
           05  WS-H3-DATE                  PIC X(69)       VALUE SPACES.
           05  FILLER                      PIC X(5)        VALUE 'PAGE'.
           05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  
           05  FILLER                      PIC X(11)       VALUE SPACES.
                                                                        
       01  WS-HEADING4.                                                 
           05  FILLER                      PIC X(114)      VALUE        
               '-      CLAIM        CERT       INSURED                  
      -    '  ARCHIVE    SEND    INITIAL   NO OF    RECEIPT   RECORDED'.
                                                                        
       01  WS-HEADING5.                                                 
           05  FILLER                      PIC X(120)      VALUE        
               ' CAR   NUMBER      NUMBER       NAME                FORM   
      -        '   NUMBER    DATE     PRINT    DAYS    FOLLOW UP     BY 
      -        '     CSR'.
                                                                        
       01  WS-DETAIL1.
101110     05  WS-D1-CNTRL                 PIC X.
101110     05  FILLER                      PIC X.
           05  WS-D1-CARRIER               PIC X.
           05  FILLER                      PIC XXX.
           05  WS-D1-CLAIM-NO              PIC X(7).
           05  FILLER                      PIC XXX.
           05  WS-D1-CERT-NO               PIC X(11).
           05  FILLER                      PIC XXX.
           05  WS-D1-INSURED-NAME          PIC X(20).
           05  FILLER                      PIC XXXX.
           05  WS-D1-FORM                  PIC X(4).
           05  FILLER                      PIC X.
           05  WS-D1-ARCHIVE-NUMBER        PIC Z(7)9.

           05  WS-D1-ARCHIVE-NUMBER-X      REDEFINES
               WS-D1-ARCHIVE-NUMBER        PIC X(8).
           05  FILLER                      PIC XX.
           05  WS-D1-SEND-DATE             PIC X(8).
           05  FILLER                      PIC XX.
           05  WS-D1-INITIAL-PRINT         PIC X(8).                    
           05  FILLER                      PIC XX.
           05  WS-D1-NO-OF-DAYS            PIC ZZZ9.
           05  FILLER                      PIC X(5).
           05  WS-D1-REC-FOLLOW-UP-DT      PIC X(8).
           05  FILLER                      PIC X(5).
           05  WS-D1-RECORDED-BY           PIC XXXX.
           05  FILLER                      PIC XXXX.
           05  WS-D1-CSR                   PIC X(4).
           05  FILLER                      PIC X.                       
                                                                        
                                       COPY ELCDTECX.                   
                                       COPY ELCDTEVR.                   
                                       COPY ELCDATE.                    

       PROCEDURE DIVISION.                                              
                                                                        
                                       COPY ELCDTERX.
                                                                        
           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT
           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
              END-OF-INPUT

           IF WS-RECORD-COUNT = 0
              MOVE '**** NO TRANSACTION FOR THIS REPORT ****'           
                                       TO P-DATA                     
              MOVE '-'                 TO X                          
              PERFORM 0200-WRITE-REPORT
                                       THRU 0200-EXIT
           END-IF
                                                                        
           PERFORM 0090-CLOSE-FILES    THRU 0090-EXIT

           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT REPORTS-EXTRACT-FILE
                OUTPUT PRNTR

           .                                                                        
       0010-EXIT.
           EXIT.

       0020-INIT.

           PERFORM 0030-READ-INPUT     THRU 0030-EXIT

           .
       0020-EXIT.
           EXIT.

       0030-READ-INPUT.

           READ REPORTS-EXTRACT-FILE AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              IF EX-POSITIONING-CODE > '5'
                 SET END-OF-INPUT      TO TRUE
                 GO TO 0030-EXIT
              ELSE
                 IF EX-POSITIONING-CODE < '5'
                    GO TO 0030-READ-INPUT
                 END-IF
              END-IF
              ADD 1                    TO WS-IP-RECS
           END-IF

           .
       0030-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

           IF (EX-POSITIONING-CODE = '5')
              AND (EX-EXTRACT-CODE = 'E')
              AND (EX-COMPANY-CD = DTE-CLASIC-COMPANY-CD)
              AND (EX-RECORD-TYPE = 'A')
PEMTST        AND (EX-EA-LETTER-ORIGIN = 'R')
              PERFORM 0100-PROCESS-INPUT
                                       THRU 0100-EXIT
           END-IF
           
           PERFORM 0030-READ-INPUT     THRU 0030-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-INPUT.

           IF DTE-PGM-OPT = 2
              IF (BIN-RUN-DATE = EX-EA-LETTER-SENT-DT)
                 OR (BIN-RUN-DATE = EX-EA-INITIAL-PRINT-DT)
                 CONTINUE
              ELSE
                 GO TO 0100-EXIT
              END-IF
           END-IF
      
           ADD 1                       TO WS-RECORD-COUNT
101110     MOVE ' '                    TO WS-DETAIL1
101110     MOVE EX-SE4-CARRIER         TO WS-D1-CARRIER
101110     MOVE EX-SE4-CLAIM-NO        TO WS-D1-CLAIM-NO
101110     MOVE EX-SE4-CERT-NO         TO WS-D1-CERT-NO
           IF EX-SE4-NO-OF-DAYS NOT NUMERIC
              MOVE ZEROS               TO EX-SE4-NO-OF-DAYS
           END-IF
101110     COMPUTE EX-SE4-NO-OF-DAYS = 9999 - EX-SE4-NO-OF-DAYS
101110     IF EX-SE4-NO-OF-DAYS NOT = WS-LAST-NO-OF-DAYS
101110        MOVE '0'                 TO WS-D1-CNTRL
101110     END-IF
           MOVE EX-SE4-NO-OF-DAYS      TO WS-D1-NO-OF-DAYS
101110                                    WS-LAST-NO-OF-DAYS
           MOVE EX-EA-INSURED-LAST-NAME
                                       TO WS-D1-INSURED-NAME
                                                                        
           IF EX-EA-LETTER-ARCHIVE-NO > ZERO                
              MOVE EX-EA-STD-LETTER-FORM
                                       TO WS-D1-FORM
              MOVE EX-EA-LETTER-ARCHIVE-NO
                                       TO WS-D1-ARCHIVE-NUMBER
           ELSE
              MOVE 'FORM'              TO WS-D1-ARCHIVE-NUMBER-X (5:4)
              IF EX-EA-STD-LETTER-FORM = '1' OR '2'
                 MOVE 'INIT'           TO WS-D1-FORM                          
              ELSE
                 MOVE EX-EA-STD-LETTER-FORM
                                       TO WS-D1-FORM
              END-IF
           END-IF

           IF EX-EA-LETTER-SENT-DT NOT = LOW-VALUES                     
              MOVE EX-EA-LETTER-SENT-DT
                                       TO DC-BIN-DATE-1         
              MOVE SPACES              TO DC-OPTION-CODE        
              PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT                             
              MOVE DC-GREG-DATE-1-EDIT TO WS-D1-SEND-DATE
           END-IF
                                                                        
           IF EX-EA-INITIAL-PRINT-DT NOT = LOW-VALUES                   
              MOVE EX-EA-INITIAL-PRINT-DT
                                       TO DC-BIN-DATE-1         
              MOVE SPACES              TO DC-OPTION-CODE        
              PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT                             
              MOVE DC-GREG-DATE-1-EDIT TO WS-D1-INITIAL-PRINT
           END-IF
                                                                        
           IF EX-EA-RECEIPT-FOLLOW-UP NOT = LOW-VALUES                 
              MOVE EX-EA-RECEIPT-FOLLOW-UP
                                       TO DC-BIN-DATE-1       
              MOVE SPACES              TO DC-OPTION-CODE      
              PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT                             
              MOVE DC-GREG-DATE-1-EDIT TO WS-D1-REC-FOLLOW-UP-DT
           END-IF

           MOVE EX-EA-CSR              TO WS-D1-CSR
           MOVE EX-EA-RECORDED-BY      TO WS-D1-RECORDED-BY

           MOVE WS-DETAIL1             TO PRT
           PERFORM 0200-WRITE-REPORT   THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-WRITE-REPORT.

           IF WS-LINE-COUNT > WS-LINE-COUNT-MAX
              PERFORM 0250-WRITE-HEADINGS
                                       THRU 0250-EXIT
           END-IF

           EVALUATE P-CTL
              WHEN '1'
                 MOVE +1               TO WS-LINE-COUNT            
              WHEN ' '
                 ADD +1                TO WS-LINE-COUNT         
              WHEN '0'
                 ADD +2                TO WS-LINE-COUNT     
              WHEN OTHER
                 ADD +3                TO WS-LINE-COUNT
           END-EVALUATE

           PERFORM 0210-WRITE-A-LINE   THRU 0210-EXIT

           .
       0200-EXIT.
           EXIT.

       0210-WRITE-A-LINE.

           WRITE PRT

           .
       0210-EXIT.
           EXIT.

       0250-WRITE-HEADINGS.

           IF WS-H2-DATE = SPACES                      
              MOVE WS-CURRENT-DATE     TO WS-H2-DATE
              MOVE COMPANY-NAME        TO WS-H2-CLIENT-NAME
              MOVE ALPH-DATE           TO WS-H3-DATE
           END-IF

           ADD +1                      TO WS-PAGE
           MOVE WS-PAGE                TO WS-H3-PAGE
           MOVE PRT                    TO WS-SAVE-PRINT-RECORD
           MOVE ZERO                   TO WS-LINE-COUNT

           MOVE WS-HEADING1            TO PRT
           PERFORM 0200-WRITE-REPORT   THRU 0200-EXIT

           MOVE WS-HEADING2            TO PRT
           PERFORM 0200-WRITE-REPORT   THRU 0200-EXIT

           MOVE WS-HEADING3            TO PRT
           PERFORM 0200-WRITE-REPORT   THRU 0200-EXIT

           MOVE WS-HEADING4            TO PRT
           PERFORM 0200-WRITE-REPORT   THRU 0200-EXIT

           MOVE WS-HEADING5            TO PRT
           PERFORM 0200-WRITE-REPORT   THRU 0200-EXIT

           MOVE +7                     TO WS-LINE-COUNT

           MOVE WS-SAVE-PRINT-RECORD   TO PRT
           MOVE '-'                    TO P-CTL

           .
       0250-EXIT.
           EXIT.

       8500-DATE-CONVERSION. COPY ELCDCS.                       

       0090-CLOSE-FILES.

           CLOSE REPORTS-EXTRACT-FILE                                   
                 PRNTR
           .
       0090-EXIT.
           EXIT.

       ABEND-PGM SECTION. COPY ELCABEND.

