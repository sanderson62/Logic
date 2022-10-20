       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    FNB203.                                                   
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      *  DESCRIPTION:                                                          :
      *      THIS PROGRAM GENERATES A REPORT LISTING FOR UNMATCHED             :
      *      DEBITS FOR ACCOUNT #2724500150-00-0000-000000-00.                 :
      *                                                                        :
      *      THE RECORDS ARE SORTED BY SUSPENSE CODE, ACCOUNT NO.,             :
      *      TRANSACTION AMOUNT AND TRANSACTION DATE.                          :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      *    DATE    BY  MODIFICATION                                            :
      * ========== === ========================================================:
      * 02/01/1999 VXO CREATION DATE                                           :
      * 03/19/2003 DJN CONVERT FROM MAINFRAME TO MICROFOCUS COBOL.             :
      * 07/28/2006 DJN DO NOT ADVANCE TO NEW PAGE ON FIRST PAGE OF REPORTS.    :
082506* 08/25/2006 AJR REMOVE FROM CLAIMS SYSTEM.                              :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            

           SELECT GL-SUSPENSE-TRANS-FILE                                        
               ASSIGN TO EXTERNAL SYS010
               ORGANIZATION IS LINE SEQUENTIAL
               STATUS IS SYS010-STATUS.                                         

           SELECT FNB203-GLS-UNMTCH-DEBITS                                      
               ASSIGN TO EXTERNAL SYS007
               ORGANIZATION IS LINE SEQUENTIAL.

082506**** PARM FILE
082506     SELECT PARM-FILE        ASSIGN       TO EXTERNAL IPARM
082506                             ORGANIZATION IS LINE SEQUENTIAL
082506                             FILE STATUS  IS PARM-STATUS.
082506

       DATA DIVISION.                                                           
       FILE SECTION.                                                            

       FD  GL-SUSPENSE-TRANS-FILE                                               
           LABEL RECORDS ARE STANDARD                                           
           RECORDING MODE IS F                                                  
           RECORD CONTAINS 120 CHARACTERS                                       
           BLOCK CONTAINS 0 RECORDS.                                            
       01  GL-SUSPENSE-TRANS-RECORD.                                            
           COPY FNC028.                                                         

       FD  FNB203-GLS-UNMTCH-DEBITS                                             
           LABEL RECORDS ARE OMITTED                                            
           RECORDING MODE IS F                                                  
           RECORD CONTAINS 132 CHARACTERS                                       
           BLOCK CONTAINS 0 RECORDS.                                            
       01  PRINT-RECORD     PIC X(132).                                         

082506**** PARM FILE
082506 FD  PARM-FILE
082506     LABEL RECORDS ARE STANDARD
082506     RECORDING MODE IS F
082506     BLOCK CONTAINS 0 RECORDS.
082506 01  PARM.
082506     05  PARM-CYCLE-DATE PIC X(10) VALUE SPACES.
082506

       WORKING-STORAGE SECTION.                                                 

       01 RC                       PIC S9(9)   COMP-5 VALUE 0.
       01 LIST-REC                 PIC X(132).

       01  FILLER.                                                              
           05  SYS010-STATUS      PIC XX         VALUE ZERO.                    
               88  EOF                           VALUE '10'.                    
           05  WS-PREV-ACCOUNT    PIC X(24)      VALUE SPACE.                   
082506     05  PARM-STATUS        PIC XX         VALUE SPACES.

       01  FILLER.
           05  POSITIVE-COUNT     PIC S9(3)      VALUE ZERO.                    
           05  LINE-COUNT         PIC S9(3)      VALUE ZERO.                    
           05  PAGE-COUNT         PIC S9(3)      VALUE ZERO.                    
           05  OUTPUT-COUNT       PIC S9(8)      VALUE ZERO.                    
           05  ERROR-COUNT        PIC S9(8)      VALUE ZERO.                    
           05  ZERO-AMT-COUNT     PIC S9(8)      VALUE ZERO.                    
           05  INPUT-COUNT        PIC S9(8)      VALUE ZERO.                    
               88  FIRST-RECORD                  VALUE +1.                      

       01  WS-CTRL-SUSP-CODE      PIC X(15)      VALUE SPACES.                  
       01  WS-CTRL-BREAK          PIC X          VALUE 'N'.                     
       01  WS-LAST-RECORD         PIC X          VALUE 'N'.                     

       01  WS-ACCUM-AMT           PIC S9(12)V99  VALUE ZEROES.                  
       01  WS-ACCUM-TOTAL-AMT     PIC S9(12)V99  VALUE ZEROES.                  
       01  WS-TOTAL-AMT           PIC S9(12)V99  VALUE ZEROES.                  

       01  FILLER.                                                              
           05  WS-CYCLE-DATE       PIC X(10)     VALUE SPACES.                  

       01  WS-CURRENT-DATE.                                                     
           05  WS-CURR-MM         PIC 99         VALUE ZEROES.                  
           05  WS-CURR-DD         PIC 99         VALUE ZEROES.                  
           05  WS-CURR-CC         PIC 99         VALUE ZEROES.                  
           05  WS-CURR-YY         PIC 99         VALUE ZEROES.                  

       01  WK-TRANS-DATE.                                                       
           05  WK-CC              PIC 99         VALUE ZEROES.                  
           05  WK-YY              PIC 99         VALUE ZEROES.                  
           05  WK-MM              PIC 99         VALUE ZEROES.                  
           05  WK-DD              PIC 99         VALUE ZEROES.

082506 01  WORK-DATE.
082506     05  WRK-YR    PIC 9999.
082506     05  WRK-MO    PIC 99.
082506     05  WRK-DAY   PIC 99.
082506
082506 01  SYSTEM-DATE.
082506     05  SYS-MO    PIC 99.
082506     05  SYS-DAY   PIC 99.
082506     05  SYS-YR    PIC 9999.

       01  PRINT-SUSPENSE-CODE    PIC X          VALUE ZERO.                    
           88  PRINT-SUSP-CODE                   VALUE '1'.                     
           88  DO-NOT-PRINT-CODE                 VALUE '2'.                     
      *****************************************************************         
      *  REPORT HEADER DEFINITION                                     *         
      *****************************************************************         
       01  HDR-1.                                                               
           05  FILLER           PIC X(06)   VALUE 'DATE: '.                     
           05  WS-RPT-DATE.                                                     
               10  WS-RPT-MM    PIC 99      VALUE ZEROES.                       
               10  FILLER       PIC X(01)   VALUE '/'.                          
               10  WS-RPT-DD    PIC 99      VALUE ZEROES.                       
               10  FILLER       PIC X(01)   VALUE '/'.                          
               10  WS-RPT-CC    PIC 99      VALUE ZEROES.                       
               10  WS-RPT-YY    PIC 99      VALUE ZEROES.                       
           05  FILLER           PIC X(20)   VALUE SPACES.                       
           05  FILLER           PIC X(07)   VALUE 'AS OF: '.                    
           05  WS-CYC-DATE      PIC X(10)   VALUE SPACES.                       
           05  FILLER           PIC X(31)   VALUE SPACES.                       
           05  FILLER           PIC X(06)   VALUE 'FNB203'.                     
           05  FILLER           PIC X(42)   VALUE SPACES.                       

       01  HDR-2.                                                               
           05  FILLER           PIC X(12)  VALUE 'SUSPENSE SUM'.                
           05  FILLER           PIC X(17)  VALUE 'MARY - UNMATCHED '.           
           05  FILLER           PIC X(18)  VALUE 'DEBITS            '.          
           05  FILLER           PIC X(85)  VALUE SPACES.                        

       01  HDR-3.                                                               
           05  FILLER           PIC X(13)   VALUE 'MSA ACCOUNT: '.              
           05  WS-MSA-ACCT      PIC X(07)   VALUE '2257150'.                    
           05  FILLER           PIC X(06)   VALUE SPACES.                       
           05  FILLER           PIC X(17)   VALUE 'FREEDOM ACCOUNT: '.          
           05  WS-FREEDOM-ACCT  PIC X(10)   VALUE '2724500150'.                 
           05  FILLER           PIC X(79)   VALUE SPACES.                       

       01  HDR-4.                                                               
           05  FILLER           PIC X(15)  VALUE 'CSO CLAIM DRAFT'.             
           05  FILLER           PIC X(17)  VALUE ' CLEARING ACCOUNT'.           
           05  FILLER           PIC X(100) VALUE SPACES.                        

       01  HDR-5.                                                               
           05  FILLER           PIC X(14)  VALUE 'DISTRIBUTION: '.              
           05  FILLER           PIC X(17)  VALUE 'CORPORATE FINANCE'.           
           05  FILLER           PIC X(101) VALUE SPACES.                        

       01  DETAIL-HDR1.                                                         
           05  FILLER           PIC X(10)  VALUE '  MAJOR   '.                  
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(15)  VALUE '   SUSPENSE    '.             
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(15)  VALUE '  TRANSACTION  '.             
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(08)  VALUE ' TRANS  '.                    
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(30)  VALUE SPACES.                        
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(15)  VALUE ' SUSPENSE CODE '.             
           05  FILLER           PIC X(14)  VALUE SPACES.                        

       01  DETAIL-HDR1B.                                                        
           05  FILLER           PIC X(10)  VALUE ' ACCOUNT  '.                  
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(15)  VALUE '     CODE      '.             
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(15)  VALUE '    AMOUNT     '.             
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(08)  VALUE '  DATE  '.                    
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(30)  VALUE                                
                                'TRANSACTION DESCRIPTION       '.               
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(15)  VALUE ' TOTAL AMOUNT  '.             
           05  FILLER           PIC X(14)  VALUE SPACES.                        

       01  DETAIL-HDR2.                                                         
           05  FILLER           PIC X(10)  VALUE ALL '*'.                       
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(15)  VALUE ALL '*'.                       
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(15)  VALUE ALL '*'.                       
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(08)  VALUE ALL '*'.                       
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(30)  VALUE ALL '*'.                       
           05  FILLER           PIC X(05)  VALUE SPACES.                        
           05  FILLER           PIC X(15)  VALUE ALL '*'.                       
           05  FILLER           PIC X(14)  VALUE SPACES.                        

       01  DETAIL-HDR3.                                                         
           05  FILLER           PIC X(118) VALUE ALL '-'.                       
           05  FILLER           PIC X(14)  VALUE SPACES.                        

       01  BLANK-LINE.                                                          
           05  FILLER           PIC X(132) VALUE SPACES.                        

       01  DETAIL-1.                                                            
           05  WS-MAJOR-ACCT    PIC X(10)                VALUE SPACES.          
           05  FILLER           PIC X(05)                VALUE SPACES.          
           05  WS-SUSP-CODE     PIC X(15)                VALUE SPACES.          
           05  FILLER           PIC X(05)                VALUE SPACES.          
           05  WS-TRANS-AMOUNT  PIC ++++,+++,++9.99      VALUE ZEROES.          
           05  FILLER           PIC X(05)                VALUE SPACES.          
           05  WS-TRANS-DATE.                                                   
               10  WS-TRANS-MM  PIC X(02)                VALUE SPACES.          
               10  FILLER       PIC X(01)                VALUE '/'.             
               10  WS-TRANS-DD  PIC X(02)                VALUE SPACES.          
               10  FILLER       PIC X(01)                VALUE '/'.             
               10  WS-TRANS-YY  PIC X(02)                VALUE SPACES.          
           05  FILLER           PIC X(05)                VALUE SPACES.          
           05  WS-DESCRIPTION   PIC X(30)                VALUE SPACES.          
           05  FILLER           PIC X(34)                VALUE SPACES.          

       01  TOTAL-1.                                                             
           05  FILLER           PIC X(103)             VALUE SPACES.            
           05  WS-SUSP-TOTAL    PIC ++++,+++,++9.99    VALUE ZEROES.            
           05  FILLER           PIC X(14)              VALUE SPACES.            

       01  FOOTER-1.                                                            
           05  FILLER           PIC X(62)              VALUE SPACES.            
           05  FILLER           PIC X(05)              VALUE 'PAGE '.           
           05  WS-PAGE-NO       PIC ZZ9                VALUE ZEROES.            
           05  FILLER           PIC X(62)              VALUE SPACES.            

082506*    EXEC SQL INCLUDE ISTDWORK.INC END-EXEC.

       PROCEDURE DIVISION.

           PERFORM 0000-START                                                   

           PERFORM 0500-PRINT-HEADERS                                           

           PERFORM 1000-GENERATE-REPORT UNTIL EOF

           PERFORM 7000-PRINT-SUSPENSE-TOTAL                                    

           PERFORM 8000-FORMAT-PAGE-LINE UNTIL LINE-COUNT > +55

           PERFORM 9000-END                                                     

082506*    GOBACK GIVING RC.
082506     STOP RUN GIVING RC.

      ***---------------------------------------------------------***
      ***--  PROCESS 0000-START:                                --***           
      ***--  1.  OPEN INPUT AND OUTPUT FILE.                    --***           
      ***--  2.  CHECK OPEN STATUS ON INPUT FILE.               --***           
      ***--  3.  INITIALIZE COUNTS.                             --***           
      ***---------------------------------------------------------***           
       0000-START.                                                              

082506     CALL 'FNBLIST' USING 'O' LIST-REC
           OPEN INPUT GL-SUSPENSE-TRANS-FILE.

           IF SYS010-STATUS NOT = '00'                                          
             DISPLAY 'SYS010 OPEN ERROR ' SYS010-STATUS UPON SYSERR
             MOVE SPACES TO LIST-REC
             STRING  'SYS010 OPEN ERROR ' SYS010-STATUS
               DELIMITED BY SIZE INTO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
082506       CALL 'FNBLIST' USING 'C' LIST-REC
             MOVE 16 TO RC
082506*      GOBACK GIVING RC
082506       STOP RUN GIVING RC
           END-IF

           OPEN OUTPUT FNB203-GLS-UNMTCH-DEBITS

           MOVE ZEROES TO INPUT-COUNT
           MOVE ZEROES TO OUTPUT-COUNT
           MOVE ZEROES TO LINE-COUNT
           MOVE ZEROES TO PAGE-COUNT
           MOVE ZEROES TO ERROR-COUNT.

      ***---------------------------------------------------------***           
      ***--  PROCESS 0500-PRINT-HEADERS:                        --***           
      ***--  1.  INITIALIZE LINE COUNT.                         --***           
      ***--  2.  PRINT REPORT HEADERS.                          --***           
      ***--  3.  DETERMINE CURRENT DATE USING CALL TO SYSDATE.  --***           
      ***--  4.  DETERMINE RUN DATE ACCEPTING THE SYSIN DATE.   --***           
      ***---------------------------------------------------------***           
       0500-PRINT-HEADERS.                                                      

           MOVE ZEROES TO LINE-COUNT

082506     ACCEPT WORK-DATE FROM DATE YYYYMMDD
082506     MOVE WRK-YR  TO SYS-YR
082506     MOVE WRK-MO  TO SYS-MO
082506     MOVE WRK-DAY TO SYS-DAY
082506
082506     MOVE SYSTEM-DATE TO WS-CURRENT-DATE
082506*    CALL 'SYSDATE' USING WS-CURRENT-DATE
           MOVE WS-CURR-MM TO WS-RPT-MM
           MOVE WS-CURR-DD TO WS-RPT-DD
           MOVE WS-CURR-CC TO WS-RPT-CC
           MOVE WS-CURR-YY TO WS-RPT-YY

082506*    CALL IGETPARM USING IG-P1
082506*    MOVE IG-P1 TO WS-CYCLE-DATE
082506*
082506*    MOVE WS-CYCLE-DATE TO IV-P2
082506*    MOVE 'MM/DD/YEAR'  TO IV-P3
082506*    INITIALIZE            IV-P4
082506*    CALL IVERDATE USING IV-P1 IV-P2 IV-P3 IV-P4
082506*    IF IV-P1 = 'Y'
082506     OPEN INPUT PARM-FILE.
082506     IF PARM-STATUS NOT = '00'
082506       STRING  'ERROR OPENING PARM FILE, STATUS = '
082506              PARM-STATUS
082506         DELIMITED BY SIZE INTO LIST-REC
082506       CALL 'FNBLIST' USING 'W' LIST-REC
082506       CALL 'FNBLIST' USING 'C' LIST-REC
082506       MOVE 16 TO RC
082506       STOP RUN GIVING RC
082506     END-IF.
082506     READ PARM-FILE.
082506     IF PARM-CYCLE-DATE GREATER THAN SPACES
082506       MOVE PARM-CYCLE-DATE TO WS-CYCLE-DATE
             MOVE WS-CYCLE-DATE TO WS-CYC-DATE
           ELSE
             MOVE SPACES TO LIST-REC
             STRING '*** INVALID DATE CARD: ' WS-CYCLE-DATE
               DELIMITED BY SIZE INTO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
082506       CALL 'FNBLIST' USING 'C' LIST-REC
             MOVE 16 TO RC
082506*      GOBACK GIVING RC
082506       STOP RUN GIVING RC
           END-IF

           WRITE PRINT-RECORD FROM HDR-1        AFTER ADVANCING 0 LINES

           WRITE PRINT-RECORD FROM HDR-2        AFTER ADVANCING 1 LINE

           WRITE PRINT-RECORD FROM HDR-3        AFTER ADVANCING 1 LINE

           WRITE PRINT-RECORD FROM HDR-4        AFTER ADVANCING 1 LINE

           WRITE PRINT-RECORD FROM BLANK-LINE   AFTER ADVANCING 1 LINE

           WRITE PRINT-RECORD FROM HDR-5        AFTER ADVANCING 1 LINE

           WRITE PRINT-RECORD FROM BLANK-LINE   AFTER ADVANCING 1 LINE

           WRITE PRINT-RECORD FROM BLANK-LINE   AFTER ADVANCING 1 LINE

           WRITE PRINT-RECORD FROM DETAIL-HDR1  AFTER ADVANCING 1 LINE

           WRITE PRINT-RECORD FROM DETAIL-HDR1B AFTER ADVANCING 1 LINE

           WRITE PRINT-RECORD FROM DETAIL-HDR2  AFTER ADVANCING 1 LINE

           ADD +10  TO  LINE-COUNT
           ADD +1   TO  PAGE-COUNT.

      ***---------------------------------------------------------***
      ***--  PROCESS 0800-PRINT-DETAIL-HEADERS.                 --***
      ***--  1.  INITIALIZE LINE COUNT.                         --***
      ***--  2.  PRINT DETAIL HEADERS FOR NEW PAGE.             --***
      ***--  3.  TALLY LINE COUNT BY NUMBER OF LINES WRITTEN.   --***
      ***---------------------------------------------------------***
       0800-PRINT-DETAIL-HEADERS.

           MOVE ZEROES TO LINE-COUNT.

           WRITE PRINT-RECORD FROM DETAIL-HDR1  AFTER ADVANCING PAGE

           WRITE PRINT-RECORD FROM DETAIL-HDR1B AFTER ADVANCING 1 LINE

           WRITE PRINT-RECORD FROM DETAIL-HDR2  AFTER ADVANCING 1 LINE

           ADD +3 TO LINE-COUNT
           ADD +1 TO PAGE-COUNT.

      ***---------------------------------------------------------***           
      ***--  PROCESS 1000-GENERATE-REPORT:                      --***           
      ***--  1. READ INPUT FILE AND ACCUMULATE INPUT COUNT.     --***           
      ***--  2. ONLY PROCESS RECORDS W/SPECIFIED ACCOUNT #'S.   --***           
      ***--  3. PERFORM ROUTINE TO DETERMINE CONTROL BREAK.     --***           
      ***--  4. PERFORM SUSPENSE CODE CONTROL BREAK ROUTINE.    --***           
      ***--  5. PERFORM ROUTINE TO PROCESS VALID ACCOUNTS.      --***           
      ***--  6. PERFORM ROUTINE TO WRITE VALID OUTPUT RECORDS.  --***           
      ***---------------------------------------------------------***           
       1000-GENERATE-REPORT.                                                    

           READ GL-SUSPENSE-TRANS-FILE                                          
             AT END EXIT PARAGRAPH
           END-READ

           ADD +1 TO INPUT-COUNT

           IF GLS-ACCOUNT-NO = '272450015000000000000000' AND
              GLS-TRANS-AMOUNT IS POSITIVE                                      
             ADD +1 TO POSITIVE-COUNT
           ELSE
             ADD +1 TO ERROR-COUNT
             EXIT PARAGRAPH
           END-IF

           PERFORM 5000-CONTROL-BREAK-CHECK                                     

           IF  WS-CTRL-BREAK   =  'Y'                                           
             PERFORM 7000-PRINT-SUSPENSE-TOTAL
           END-IF

           PERFORM 3000-PROCESS-DETAIL-LINE                                     

           PERFORM 4000-WRITE-DETAIL-LINE.

      ***---------------------------------------------------------***           
      ***--  PROCESS 3000-PROCESS-DETAIL-LINE:                  --***           
      ***--  1.  PERFORM ROUTINE TO FORMAT TRANSACTION DATE.    --***           
      ***--  2.  MOVE INPUT DATA TO OUTPUT FIELDS.              --***           
      ***--  3.  SUSPENSE CODE WILL ONLY BE PRINTED ON THE 1ST  --***           
      ***--      LINE OF THAT SUSPENSE CODE GROUPING.           --***           
      ***---------------------------------------------------------***           
       3000-PROCESS-DETAIL-LINE.                                                

           IF  PRINT-SUSP-CODE                                                  
             WRITE PRINT-RECORD FROM DETAIL-HDR3 AFTER ADVANCING 1 LINE
             ADD +1 TO LINE-COUNT
             MOVE GLS-SUSPENSE-CODE TO WS-SUSP-CODE
           ELSE
             MOVE SPACES            TO WS-SUSP-CODE
           END-IF

           PERFORM 6000-FORMAT-TRANS-DATE

           MOVE GLS-MAJ-ACCT     TO WS-MAJOR-ACCT
           MOVE GLS-DESCRIPTION  TO WS-DESCRIPTION
           MOVE GLS-TRANS-AMOUNT TO WS-TRANS-AMOUNT.

      ***---------------------------------------------------------***           
      ***--  PROCESS 4000-WRITE-DETAIL-LINE.                    --***           
      ***--  1.  DETERMINE IF HEADERS SHOULD BE PRINTED.        --***           
      ***--  2.  WRITE OUTPUT RECORDS.                          --***           
      ***--  3.  ACCUMULATE TRANSACTION AMTS BY SUSPENSE CODE.  --***           
      ***--  3.  ACCUMULATE LINE AND OUTPUT COUNTS.             --***           
      ***---------------------------------------------------------***           
       4000-WRITE-DETAIL-LINE.                                                  

           IF LINE-COUNT > +55
             MOVE PAGE-COUNT TO WS-PAGE-NO
             WRITE PRINT-RECORD FROM BLANK-LINE AFTER ADVANCING 1 LINE
             ADD +1 TO LINE-COUNT
             WRITE PRINT-RECORD FROM FOOTER-1   AFTER ADVANCING 1 LINE
             PERFORM 0800-PRINT-DETAIL-HEADERS
           END-IF

           WRITE PRINT-RECORD FROM DETAIL-1 AFTER ADVANCING 1 LINE

           ADD   +1                  TO  OUTPUT-COUNT
           ADD   +1                  TO  LINE-COUNT
           ADD   GLS-TRANS-AMOUNT    TO  WS-ACCUM-AMT.                          

      ***---------------------------------------------------------***           
      ***--  PROCESS 5000-CONTROL-BREAK-CHECK.                  --***           
      ***--  1. SPECIAL PROCESSING FOR FIRST RECORD.            --***           
      ***--  2. DETERMINE IF THE SUSPENSE CODE HAS CHANGED.     --***           
      ***--  3. DETERMINE IF SUSPENSE CODE SHOULD BE PRINTED,   --***           
      ***--     AND SET THE INDICATOR.                          --***           
      ***---------------------------------------------------------***           
       5000-CONTROL-BREAK-CHECK.                                                

           IF OUTPUT-COUNT = ZERO
             MOVE GLS-SUSPENSE-CODE TO WS-CTRL-SUSP-CODE
             MOVE 'N' TO WS-CTRL-BREAK
             MOVE '1' TO PRINT-SUSPENSE-CODE
             EXIT PARAGRAPH
           END-IF

           IF GLS-SUSPENSE-CODE = WS-CTRL-SUSP-CODE
             MOVE 'N' TO WS-CTRL-BREAK
             MOVE '2' TO PRINT-SUSPENSE-CODE
           ELSE
             MOVE '1' TO PRINT-SUSPENSE-CODE
             MOVE 'Y' TO WS-CTRL-BREAK
           END-IF

           MOVE GLS-SUSPENSE-CODE TO WS-CTRL-SUSP-CODE.

      ***---------------------------------------------------------***           
      ***--  PROCESS 6000-FORMAT-TRANS-DATE.                    --***           
      ***--  1. FORMAT DATE AS MM/YY/DD.                        --***           
      ***--  2. MOVE WORKING STORAGE DATE TO OUTPUT FIELD.      --***           
      ***---------------------------------------------------------***           
       6000-FORMAT-TRANS-DATE.                                                  

           MOVE GLS-TRANS-DATE TO WK-TRANS-DATE
           MOVE WK-MM          TO WS-TRANS-MM
           MOVE WK-DD          TO WS-TRANS-DD
           MOVE WK-YY          TO WS-TRANS-YY.

      ***---------------------------------------------------------***           
      ***--  PROCESS 7000-PRINT-SUSPENSE-TOTAL.                 --***           
      ***--  1.  MOVE SUSPENSE ACCUMULATED AMOUNT TO OUTPUT.    --***           
      ***--  2.  WRITE TOTAL LINE.                              --***           
      ***--  3.  ADD TO LINE COUNT.                             --***           
      ***--  4.  RESET THE WS ACCUMULATED AMOUNT FIELD TO ZERO. --***           
      ***---------------------------------------------------------***           
       7000-PRINT-SUSPENSE-TOTAL.                                               

           MOVE WS-ACCUM-AMT TO WS-SUSP-TOTAL

           WRITE PRINT-RECORD FROM TOTAL-1 AFTER ADVANCING 1 LINE

           ADD  +1           TO LINE-COUNT.
           MOVE ZEROES       TO WS-ACCUM-AMT.

      ***---------------------------------------------------------***           
      ***--  PROCESS 8000-FORMAT-PAGE-LINE.                     --***           
      ***--  1. FORCE LINE COUNT TO 55 TO PRINT LAST PAGE NO.   --***           
      ***---------------------------------------------------------***           
       8000-FORMAT-PAGE-LINE.                                                   

           WRITE PRINT-RECORD FROM BLANK-LINE AFTER ADVANCING 1 LINE
           ADD  +1  TO  LINE-COUNT.

      ***---------------------------------------------------------***           
      ***--  PROCESS 9000-END.                                  --***           
      ***--  1. PRINT LAST PAGE NUMBER.                         --***           
      ***--  2. CLOSE FILES                                     --***           
      ***--  3. DISPLAY VARIOUS RECORD COUNTS.                  --***           
      ***---------------------------------------------------------***           
       9000-END.                                                                

           MOVE PAGE-COUNT TO WS-PAGE-NO
           WRITE PRINT-RECORD FROM FOOTER-1 AFTER ADVANCING 1 LINE

           DISPLAY '*=====================================*' UPON SYSERR
           DISPLAY '*   FNB203 - GLS OUTSTANDING DRAFTS   *' UPON SYSERR
           DISPLAY '*=====================================*' UPON SYSERR
           DISPLAY '* OUTPUT FILE:  ZN.DD.FNB203.REPORTS  *' UPON SYSERR
           DISPLAY '*-------------------------------------*' UPON SYSERR
           DISPLAY '     INPUT RECORDS READ: ' INPUT-COUNT   UPON SYSERR
           DISPLAY '  RECORDS NOT PROCESSED: ' ERROR-COUNT   UPON SYSERR
           DISPLAY ' OUTPUT RECORDS WRITTEN: ' OUTPUT-COUNT  UPON SYSERR
           DISPLAY '*=====================================*' UPON SYSERR

           MOVE    '*=====================================*' TO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE    '*   FNB203 - GLS OUTSTANDING DRAFTS   *' TO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE    '*=====================================*' TO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE    '* OUTPUT FILE:  ZN.DD.FNB203.REPORTS  *' TO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE    '*-------------------------------------*' TO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE SPACES TO LIST-REC
           STRING  '     INPUT RECORDS READ: ' INPUT-COUNT
             DELIMITED BY SIZE INTO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE SPACES TO LIST-REC
           STRING  '  RECORDS NOT PROCESSED: ' ERROR-COUNT
             DELIMITED BY SIZE INTO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE SPACES TO LIST-REC
           STRING  ' OUTPUT RECORDS WRITTEN: ' OUTPUT-COUNT
             DELIMITED BY SIZE INTO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE    '*=====================================*' TO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC

           CLOSE  GL-SUSPENSE-TRANS-FILE
                  FNB203-GLS-UNMTCH-DEBITS.
082506     CALL 'FNBLIST' USING 'C' LIST-REC.
082506     CLOSE  PARM-FILE.
