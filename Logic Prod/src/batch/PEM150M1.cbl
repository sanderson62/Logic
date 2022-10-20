       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PEM150AM1.
      *AUTHOR.     PABLO.                                               
      *REMARKS.                                                         
      *     THIS PROGRAM READS THE EXTRACTS FROM ECS150 FROM LAST YEAR
      *       AND THIS YEAR, MATCHES THEM TOGETHER, THEN OVERLAYS
      *       THE PREVIOUS YEAR TOTALS ON THIS YEARS EXTRACT WITH
      *       THE CURRENT YEAR TOTALS FROM LAST YEARS FILE
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT LAST-YEAR            ASSIGN TO SYS010.

           SELECT THIS-YEAR            ASSIGN TO SYS011.

           SELECT NEW-EXTRACT          ASSIGN TO SYS012.
                                                                        
       DATA DIVISION.                                                   
                                                                        
       FILE SECTION.                                                    
                                                                        
       FD  LAST-YEAR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  LAST-YEAR-IN-RECORD         PIC X(2701).

       FD  THIS-YEAR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  THIS-YEAR-IN-RECORD         PIC X(2701).

       FD  NEW-EXTRACT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  NEW-EXTRACT-RECORD         PIC X(2701).

       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32) VALUE '********************************'.  
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  
       77  FILLER  PIC X(32) VALUE '********************************'.  
       77  S1                          PIC S999  COMP-3 VALUE +0.
       77  S2                          PIC S999  COMP-3 VALUE +0.
       77  WS-NUMBER-SW                PIC X.
           88  HAS-NUMBERS             VALUE 'Y'.
       01  LAST-YEAR-RECORD.                                                  
           03  LY-SORT-KEY.                                             
               05  LY-CONTROL.                                          
                   07  LY-REINS-CO     PIC X(06).                       
                   07  LY-CARRIER      PIC X(01).                       
                   07  LY-COMPANY      PIC X(06).                       
                   07  LY-STATE        PIC X(02).                       
                   07  LY-ACCOUNT      PIC X(10).                       
               05  LY-ST-SEQ           PIC X(02).                       
               05  LY-REC-TYPE         PIC 99.                          
           03  LY-PERIOD-START         PIC 9(11)   COMP-3.              
           03  LY-PERIOD-END           PIC 9(11)   COMP-3.              
           03  LY-ACTIVITY-TOTALS.                                      
               05  LY-TERM-GROUP       OCCURS 3 TIMES.                  
      *                1 = 0-60 MONTHS                                  
      *                2 = 61-120 MONTHS                                
      *                3 = 121 AND OVER.                                
                   07  LY-TYPE-GROUPS  OCCURS 10 TIMES.                 
      *                    1 = LIFE(R-GP)      6 = LIFE(R-IND)          
      *                    2 = LIFE(L-GP)      7 = LIFE(L-IND)          
      *                    3 = LIFE(OB-GP)     8 = LIFE(OB-IND)         
      *                    4 = AH(GP)          9 = AH(IND)              
      *                    5 = AH(OB-GP)      10 = AH(OB-IND)           
                       09  LY-ISSUE-CNT    PIC S9(13)      COMP-3.      
                       09  LY-ISSUE-AMT    PIC S9(11)V99   COMP-3.      
                       09  LY-ISSUE-PREM   PIC S9(11)V99   COMP-3.      
                       09  LY-CANCEL-CNT   PIC S9(13)      COMP-3.      
                       09  LY-CANCEL-AMT   PIC S9(11)V99   COMP-3.      
                       09  LY-CANCEL-PREM  PIC S9(11)V99   COMP-3.      
                       09  LY-CLAIM-CNT    PIC S9(13)      COMP-3.      
                       09  LY-CLAIM-AMT    PIC S9(11)V99   COMP-3.      
                       09  LY-EARN-PREM    PIC S9(11)V99   COMP-3.      
           03  LY-EXHIBIT-TOTALS.                                       
               05  LY-TERM-GROUP-E     OCCURS 3 TIMES.                  
      *                1 = 0-60 MONTHS                                  
      *                2 = 61-120 MONTHS                                
      *                3 = 121 AND OVER                                 
                   07  LY-LINE-DETAIL  OCCURS 9 TIMES.                  
      *                1 = INFORCE PREVIOUS PER  6 = CANCEL IN PERIOD   
      *                2 = ISSUED IN PERIOD      7 = DECREASES IN PERIOD
      *                3 = TOTAL INFORCE-START   8 = TOTAL DEC IN PERIOD
      *                4 = DEATHS IN PERIOD      9 = TOTAL INFORCE (END)
      *                5 = EXPIRED IN PERIOD                            
                                                                        
                       09  LY-GROUP-CNT    PIC S9(13)      COMP-3.      
                       09  LY-GROUP-AMT    PIC S9(11)V99   COMP-3.      
                       09  LY-IND-CNT      PIC S9(13)      COMP-3.      
                       09  LY-IND-AMT      PIC S9(11)V99   COMP-3.      
                                                                        
           03  LY-AH-PD-THIS               PIC S9(11)V99   COMP-3.      
           03  LY-AH-PD-LAST               PIC S9(11)V99   COMP-3.      

       01  THIS-YEAR-RECORD.                                                  
           03  TY-SORT-KEY.                                             
               05  TY-CONTROL.                                          
                   07  TY-REINS-CO     PIC X(06).                       
                   07  TY-CARRIER      PIC X(01).                       
                   07  TY-COMPANY      PIC X(06).                       
                   07  TY-STATE        PIC X(02).                       
                   07  TY-ACCOUNT      PIC X(10).                       
               05  TY-ST-SEQ           PIC X(02).                       
               05  TY-REC-TYPE         PIC 99.                          
           03  TY-PERIOD-START         PIC 9(11)   COMP-3.              
           03  TY-PERIOD-END           PIC 9(11)   COMP-3.              
           03  TY-ACTIVITY-TOTALS.                                      
               05  TY-TERM-GROUP       OCCURS 3 TIMES.                  
      *                1 = 0-60 MONTHS                                  
      *                2 = 61-120 MONTHS                                
      *                3 = 121 AND OVER.                                
                   07  TY-TYPE-GROUPS  OCCURS 10 TIMES.                 
      *                    1 = LIFE(R-GP)      6 = LIFE(R-IND)          
      *                    2 = LIFE(L-GP)      7 = LIFE(L-IND)          
      *                    3 = LIFE(OB-GP)     8 = LIFE(OB-IND)         
      *                    4 = AH(GP)          9 = AH(IND)              
      *                    5 = AH(OB-GP)      10 = AH(OB-IND)           
                       09  TY-ISSUE-CNT    PIC S9(13)      COMP-3.      
                       09  TY-ISSUE-AMT    PIC S9(11)V99   COMP-3.      
                       09  TY-ISSUE-PREM   PIC S9(11)V99   COMP-3.      
                       09  TY-CANCEL-CNT   PIC S9(13)      COMP-3.      
                       09  TY-CANCEL-AMT   PIC S9(11)V99   COMP-3.      
                       09  TY-CANCEL-PREM  PIC S9(11)V99   COMP-3.      
                       09  TY-CLAIM-CNT    PIC S9(13)      COMP-3.      
                       09  TY-CLAIM-AMT    PIC S9(11)V99   COMP-3.      
                       09  TY-EARN-PREM    PIC S9(11)V99   COMP-3.      
           03  TY-EXHIBIT-TOTALS.
               05  TY-TERM-GROUP-E     OCCURS 3 TIMES.                  
      *                1 = 0-60 MONTHS                                  
      *                2 = 61-120 MONTHS                                
      *                3 = 121 AND OVER                                 
                   07  TY-LINE-DETAIL  OCCURS 9 TIMES.                  
      *                1 = INFORCE PREVIOUS PER  6 = CANCEL IN PERIOD   
      *                2 = ISSUED IN PERIOD      7 = DECREASES IN PERIOD
      *                3 = TOTAL INFORCE-START   8 = TOTAL DEC IN PERIOD
      *                4 = DEATHS IN PERIOD      9 = TOTAL INFORCE (END)
      *                5 = EXPIRED IN PERIOD                            
                                                                        
                       09  TY-GROUP-CNT    PIC S9(13)      COMP-3.      
                       09  TY-GROUP-AMT    PIC S9(11)V99   COMP-3.      
                       09  TY-IND-CNT      PIC S9(13)      COMP-3.      
                       09  TY-IND-AMT      PIC S9(11)V99   COMP-3.      
                                                                        
           03  TY-AH-PD-THIS               PIC S9(11)V99   COMP-3.      
           03  TY-AH-PD-LAST               PIC S9(11)V99   COMP-3.      


       01  W-MISC.                                                      
           05  WS-DATE-ALPH.
               10  FILLER             PIC XXX VALUE '000'.
               10  WS-WORK-CENT       PIC XX.
               10  WS-WORK-YR         PIC XX.
               10  WS-WORK-MO         PIC XX.
               10  WS-WORK-DA         PIC XX.
           05  WS-DATE-NUM REDEFINES WS-DATE-ALPH
                                      PIC 9(11).

           05  THIS-YEAR-CNT           PIC 9(9) VALUE ZEROS.
           05  LAST-YEAR-CNT           PIC 9(9) VALUE ZEROS.
           05  MATCH-CNT               PIC 9(9) VALUE ZEROS.
           05  NEW-EXTRACT-CNT         PIC 9(9) VALUE ZEROS.
           05  WS-EOF-SW1             PIC X VALUE SPACES.
               88  END-OF-THIS-YEAR         VALUE 'Y'.
           05  WS-EOF-SW2             PIC X VALUE ' '.
               88  END-OF-LAST-YEAR         VALUE 'Y'.
                                                                        
                                       COPY ELCDATE.
       PROCEDURE DIVISION.                                              

           OPEN INPUT THIS-YEAR LAST-YEAR

           OPEN OUTPUT NEW-EXTRACT

           PERFORM 0015-READ-LAST-YEAR THRU 0015-EXIT
           PERFORM 0010-READ-THIS-YEAR THRU 0010-EXIT

           PERFORM 0020-PROCESS THRU 0020-EXIT UNTIL
              END-OF-LAST-YEAR

           CLOSE THIS-YEAR LAST-YEAR NEW-EXTRACT
              
           DISPLAY ' LAST YEAR RECS READ  ' LAST-YEAR-CNT
           DISPLAY ' THIS YEAR RECS READ  ' THIS-YEAR-CNT
           DISPLAY ' NEW EXTRACT RECS OUT ' NEW-EXTRACT-CNT
           DISPLAY ' MATCHED RECORD CNT   ' MATCH-CNT

           GOBACK

           .
       0010-READ-THIS-YEAR.

           READ THIS-YEAR AT END                                                   
              DISPLAY ' FOUND END OF THIS YEAR '
              SET END-OF-THIS-YEAR TO TRUE
           END-READ                                                     

           IF NOT END-OF-THIS-YEAR
              MOVE THIS-YEAR-IN-RECORD TO THIS-YEAR-RECORD
              ADD  1    TO THIS-YEAR-CNT
           END-IF

           .
       0010-EXIT.
           EXIT.

       0015-READ-LAST-YEAR.

           READ LAST-YEAR AT END
              DISPLAY ' FOUND END OF LAST YEAR '
              SET END-OF-LAST-YEAR TO TRUE
           END-READ
                                                                        
           IF NOT END-OF-LAST-YEAR
              MOVE LAST-YEAR-IN-RECORD TO LAST-YEAR-RECORD
              ADD  1    TO LAST-YEAR-CNT                                  
           ELSE
              MOVE HIGH-VALUES         TO LY-SORT-KEY
           END-IF

           .
       0015-EXIT.
           EXIT.
                                                                        
       0020-PROCESS.                                                    
                                                                        
      *    DISPLAY ' EXT KEY = ' EXT-KEY
      *    DISPLAY 'CERT KEY = ' CERT-KEY
           IF LY-SORT-KEY = TY-SORT-KEY
              PERFORM 0030-MATCHED     THRU 0030-EXIT
           ELSE
              IF LY-SORT-KEY > TY-SORT-KEY
                 PERFORM 0035-LY-GT-TY THRU 0035-EXIT
              ELSE
                 PERFORM 0040-NO-MATCH THRU 0040-EXIT
              END-IF
           END-IF

           .
       0020-EXIT.                                                       
            EXIT.                                                       
                                                                        
       0030-MATCHED.

      *  I'M DOING THE IF STMT BELOW PRIMARILY FOR THE REC 20
      *  BECAUSE THERE ARE A TON OF DUPLICATES SO I DON'T
      *  RUN OUT OF LAST YEAR RECORDS

      *    IF     (TY-GROUP-CNT (1 1) = ZERO)
      *       AND (TY-GROUP-CNT (2 1) = ZERO)
      *       AND (TY-GROUP-CNT (3 1) = ZERO)
      *       AND (TY-GROUP-AMT (1 1) = ZERO)
      *       AND (TY-GROUP-AMT (2 1) = ZERO)
      *       AND (TY-GROUP-AMT (3 1) = ZERO)
      *       AND (TY-IND-CNT   (1 1) = ZERO)
      *       AND (TY-IND-CNT   (2 1) = ZERO)
      *       AND (TY-IND-CNT   (3 1) = ZERO)
      *       AND (TY-IND-AMT   (1 1) = ZERO)
      *       AND (TY-IND-AMT   (2 1) = ZERO)
      *       AND (TY-IND-AMT   (3 1) = ZERO)
      *       PERFORM 0050-WRITE-NEW-EXTRACT
      *                                THRU 0050-EXIT
      *
      *       PERFORM 0010-READ-THIS-YEAR
      *                                THRU 0010-EXIT
      *       GO TO 0030-EXIT
      *    END-IF

           ADD 1                       TO MATCH-CNT
           
      *  1 IS BEGINNING, 9 IS ENDING

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +3
              MOVE LY-GROUP-CNT (S1 9) TO TY-GROUP-CNT (S1 1)
              MOVE LY-GROUP-AMT (S1 9) TO TY-GROUP-AMT (S1 1)
              MOVE LY-IND-CNT   (S1 9) TO TY-IND-CNT   (S1 1)
              MOVE LY-IND-AMT   (S1 9) TO TY-IND-AMT   (S1 1)
           END-PERFORM


           PERFORM 0050-WRITE-NEW-EXTRACT
                                       THRU 0050-EXIT

           PERFORM 0010-READ-THIS-YEAR THRU 0010-EXIT
           PERFORM 0015-READ-LAST-YEAR THRU 0015-EXIT

           .
       0030-EXIT.                                                       
            EXIT.                                                       

       0035-LY-GT-TY.

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +3
              MOVE ZEROS               TO TY-GROUP-CNT (S1 1)
                                          TY-GROUP-AMT (S1 1)
                                          TY-IND-CNT   (S1 1)
                                          TY-IND-AMT   (S1 1)
           END-PERFORM

           PERFORM 0050-WRITE-NEW-EXTRACT
                                       THRU 0050-EXIT

           PERFORM 0010-READ-THIS-YEAR THRU 0010-EXIT
           
           .
       0035-EXIT.
           EXIT.

       0040-NO-MATCH.

           MOVE ' '                    TO WS-NUMBER-SW

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +3
              IF    (LY-GROUP-CNT (S1 9) > ZEROS)
                 OR (LY-GROUP-AMT (S1 9) > ZEROS)
                 OR (LY-IND-CNT   (S1 9) > ZEROS)
                 OR (LY-IND-AMT   (S1 9) > ZEROS)
                 SET HAS-NUMBERS       TO TRUE
              END-IF
           END-PERFORM

           IF HAS-NUMBERS
              DISPLAY '*******************************'
           END-IF
           DISPLAY ' NO MATCH LAST YEAR ' LY-SORT-KEY
              ' THIS YEAR ' TY-SORT-KEY
           IF HAS-NUMBERS
              DISPLAY '*******************************'
           END-IF

           IF NOT HAS-NUMBERS
              GO TO 0040-READ
           END-IF

           MOVE TY-PERIOD-START        TO LY-PERIOD-START
           MOVE TY-PERIOD-END          TO LY-PERIOD-END

           MOVE ZEROS                  TO LY-AH-PD-THIS 
                                          LY-AH-PD-LAST 

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +3
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                 S2 > +10
                 MOVE +0               TO LY-ISSUE-CNT   (S1 S2)
                                          LY-ISSUE-AMT   (S1 S2)
                                          LY-ISSUE-PREM  (S1 S2)
                                          LY-CANCEL-CNT  (S1 S2)
                                          LY-CANCEL-AMT  (S1 S2)
                                          LY-CANCEL-PREM (S1 S2)
                                          LY-CLAIM-CNT   (S1 S2)
                                          LY-CLAIM-AMT   (S1 S2)
                                          LY-EARN-PREM   (S1 S2)
              END-PERFORM
           END-PERFORM

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +3
              MOVE LY-GROUP-CNT (S1 9) TO LY-GROUP-CNT (S1 1)
              MOVE LY-GROUP-AMT (S1 9) TO LY-GROUP-AMT (S1 1)
              MOVE LY-IND-CNT   (S1 9) TO LY-IND-CNT   (S1 1)
              MOVE LY-IND-AMT   (S1 9) TO LY-IND-AMT   (S1 1)
           END-PERFORM

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +3
              PERFORM VARYING S2 FROM +2 BY +1 UNTIL
                 S2 > +9
                 MOVE +0               TO LY-GROUP-CNT (S1 S2)
                                          LY-GROUP-AMT (S1 S2)
                                          LY-IND-CNT   (S1 S2)
                                          LY-IND-AMT   (S1 S2)
              END-PERFORM
           END-PERFORM

           WRITE NEW-EXTRACT-RECORD    FROM LAST-YEAR-RECORD
           ADD 1                       TO NEW-EXTRACT-CNT

           .
       0040-READ.

           PERFORM 0015-READ-LAST-YEAR THRU 0015-EXIT

           .
       0040-EXIT.                                                       
            EXIT.                                                       

       0050-WRITE-NEW-EXTRACT.
       
           WRITE NEW-EXTRACT-RECORD    FROM THIS-YEAR-RECORD
           ADD 1                       TO NEW-EXTRACT-CNT

           .
       0050-EXIT.
           EXIT.
