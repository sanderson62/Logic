       IDENTIFICATION DIVISION.
       PROGRAM-ID. AHLCLMSP.
       AUTHOR.     PABLO
       DATE-COMPILED.
      ************************************************************************
      * THIS PROGRAM CREATES A FILE OF CLAIM PAYMENTS THAT WERE MADE BY AHL
      * PRIOR TO 4/1/2012 AND WERE VOIDED BY CSO DURING THE CURRENT AHL 
      * CYCLE MONTH.
      ************************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELTRLR           ASSIGN TO ELTRLR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AT-CONTROL-PRIMARY
                                   FILE STATUS IS ELTRLR-FILE-STATUS.

           SELECT ELMSTR           ASSIGN TO ELMSTR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CL-CONTROL-PRIMARY
                                   FILE STATUS IS ELMSTR-FILE-STATUS.


           SELECT ELCERT           ASSIGN TO ELCERT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CM-CONTROL-PRIMARY
                                   FILE STATUS IS ELCERT-FILE-STATUS.
                                                                        
           SELECT LOGIC-CONTROL-FILE                                    
                                   ASSIGN TO ELCNTL                                         
                                   ORGANIZATION IS INDEXED                                  
                                   ACCESS IS SEQUENTIAL                                     
                                   RECORD KEY IS CF-CONTROL-PRIMARY                         
                                   FILE STATUS IS ELCNTL-STATUS.                            

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT EXTRACT-FILE
               ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.
                                   
       EJECT
       DATA DIVISION.
       FILE SECTION.


       FD  ELTRLR.

                              COPY ELCTRLR.

       FD  ELMSTR.

                              COPY ELCMSTR.

       FD  ELCERT.

                              COPY ELCCERT.

       FD  DISK-DATE                                                    
           COPY ELCDTEFD.                                               

       FD  LOGIC-CONTROL-FILE.                                          
           COPY ELCCNTL.                                                                                                                        

       FD  EXTRACT-FILE                                                    
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  EXTRACT-RECORD.                                                 
           COPY FNC022.                                                 

       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMATC1   WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ELTRLR             VALUE 'Y'.
       77  TRLR-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  TRLR-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  TRLR-RECS-FOUND         PIC 9(9) VALUE ZEROS.
       
       01  WS-MISC.
           05  WS-WORK-SEQ             PIC X.
           05  WS-NUM-SEQ REDEFINES WS-WORK-SEQ PIC 9.
           05  WS-SAVE-ELTRLR          PIC X(207) VALUE LOW-VALUES.
           05  ELTRLR-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ELMSTR-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ELCERT-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ELCNTL-STATUS           PIC XX    VALUE '00'.                
           05  WS-LF-PLAN.                                              
               10  WS-LF-PLAN1     PIC X.                               
               10  WS-LF-PLAN2     PIC XXX.                             
           05  WS-AH-PLAN.                                              
               10  WS-AH-PLAN1     PIC X.                               
               10  WS-AH-PLAN2     PIC XXX.                             
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

           05  DATE-SW             PIC X         VALUE ' '.             
               88  VALID-DATE                    VALUE 'V'.             
           05  WS-ZIP              PIC ZZZZ99999.                       
           05  WS-DESCRIPTION.                                          
               10  WS-DESC-TYPE      PIC XX.                            
               10  FILLER            PIC X.                             
               10  WS-DESC-KON       PIC X(5).                          
               10  WS-DESC-DATE      PIC X(4).                          
               10  WS-DESC-DR-STATUS PIC X.                             
               10  WS-DESC-HORC      PIC X(3).                          
               10  WS-DESC-ASRC      PIC X(3).                          
               10  WS-DESC-PLAN      PIC X(6).                          
               10  FILLER            PIC X(3).
               10  WS-DESC-PAYEE-ST  PIC X(2).

           05  WS-MONTHEND-BIN-DT     PIC XX.
           
           05  WS-CURR-ME-MMDDYYYY.
               10 WS-CURR-ME-MM      PIC XX.
               10 WS-CURR-ME-DD      PIC XX.
               10 WS-CURR-ME-YYYY    PIC XXXX.
      
           05  WS-ZERO                 PIC S9(3)  COMP-3  VALUE +0.
           05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.    
           05  PGM-SUB                 PIC S9(4) COMP VALUE +310.
           05  WS-RETURN-CODE          PIC S999  COMP-3 VALUE +0.
           05  DUMP                PIC X     VALUE ' '.                 
           05  FORCE-DUMP REDEFINES DUMP PIC S9 COMP-3.                 
           05  SUB                 PIC S9    COMP-3.                    

       01  LOGIC-BEN-TABLE.                                                      
           05  BENEFIT-CODE-TABLE OCCURS 150 TIMES INDEXED BY BEN-INDEX.
               10  BEN-NUM       PIC XX    VALUE HIGH-VALUE.            
               10  BEN-ALPHA     PIC XXX   VALUE HIGH-VALUE.            
    
      * STATE EDIT TABLE
             COPY FNC018.
    
                                      COPY ELCDTECX.                                               
                                      COPY ELCDTEVR.                                               
                                      COPY ELCDATE.

       LINKAGE SECTION.
                                                                        
       01  PARM.                                                        
           05  PARM-LENGTH          PIC S9(4)   COMP.
           05  PARM-CURR-ME-DATE.
               10 PARM-CURR-ME-YYYY PIC X(4).
               10 PARM-CURR-ME-MM   PIC X(2).
               10 PARM-CURR-ME-DD   PIC X(2).  
           05  PARM-MONTHEND-DATE   PIC X(8).                       
                                                                        
       

       PROCEDURE DIVISION USING PARM.

       0000-MAIN.
                         COPY ELCDTERX.    
                                       
           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0600-INITIALIZE THRU 0600-EXIT

           PERFORM 0100-PROCESS-TRLR THRU 0100-EXIT UNTIL
                 END-OF-ELTRLR

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' '
           DISPLAY ' TRLR RECORDS READ    ' TRLR-RECS-IN
           DISPLAY ' TRLR RECORDS FOUND   ' TRLR-RECS-FOUND
           GOBACK

           .
       0100-PROCESS-TRLR.
       
           IF AT-COMPANY-CD = X'06' AND
             PAYMENT-TR AND
             AT-CHECK-WRITTEN-DT < X'A861'  AND
            (AT-CHECK-WRITTEN-DT NOT EQUAL LOW-VALUES AND SPACES) AND
             AT-VOID-SELECT-DT = WS-MONTHEND-BIN-DT
             
              DISPLAY 'CLAIM # ' AT-CLAIM-NO '  CERT # ' AT-CERT-NO
                ' CLAIM AMOUNT ' AT-AMOUNT-PAID
       	      
       	      PERFORM 0300-WRITE-EXTRACT THRU 0300-EXIT 
       	      ADD 1 TO TRLR-RECS-FOUND
              
           END-IF

           PERFORM 0200-READ-TRLR THRU 0200-EXIT

           .

       0100-EXIT.
           EXIT.

       0200-READ-TRLR.

           READ ELTRLR NEXT RECORD

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              SET END-OF-ELTRLR        TO TRUE
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELTRLR READ NEXT ' ELTRLR-FILE-STATUS
                 SET END-OF-ELTRLR     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ELTRLR
              ADD 1 TO TRLR-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.
           
           
       0250-READ-ELMSTR-AND-ELCERT.

           READ ELMSTR

           IF ELMSTR-FILE-STATUS NOT = '00'
              DISPLAY 'ELMSTR READ ' ELMSTR-FILE-STATUS
              ' ' CL-CONTROL-PRIMARY
              GO TO ABEND-PGM
           END-IF

           MOVE CL-COMPANY-CD          TO CM-COMPANY-CD
           MOVE CL-CERT-CARRIER        TO CM-CARRIER
           MOVE CL-CERT-GROUPING       TO CM-GROUPING
           MOVE CL-CERT-STATE          TO CM-STATE
           MOVE CL-CERT-ACCOUNT        TO CM-ACCOUNT
           MOVE CL-CERT-EFF-DT         TO CM-CERT-EFF-DT
           MOVE CL-CERT-NO             TO CM-CERT-NO

           READ ELCERT

           IF ELCERT-FILE-STATUS NOT = '00'
              DISPLAY 'ELCERT READ ' ELCERT-FILE-STATUS
              ' ' CL-CONTROL-PRIMARY
              GO TO ABEND-PGM
           END-IF

           .
       0250-EXIT.
           EXIT.


       0300-WRITE-EXTRACT.
           MOVE SPACES                 TO EXTRACT-RECORD
           
           MOVE AT-COMPANY-CD          TO CL-COMPANY-CD
           MOVE AT-CARRIER             TO CL-CARRIER
           MOVE AT-CLAIM-NO            TO CL-CLAIM-NO
           MOVE AT-CERT-NO             TO CL-CERT-NO
           
           PERFORM 0250-READ-ELMSTR-AND-ELCERT THRU 0250-EXIT
                                                                        
           MOVE 'CLAIMS'               TO FX-SOURCE-CODE                          
           MOVE '60'                   TO FX-TRAN-TYPE                            
      
           MOVE 'AHL CLAIMS'           TO FX-SYSTEM
           MOVE '02'                   TO FX-DIVISION


      ***** SUB-TYPE 2 = REFUND AND SUB-TYPE 1 = PAYMENT
           MOVE '02' TO FX-SUB-TYPE                                  
           MULTIPLY AT-AMOUNT-PAID BY -1 GIVING FX-AMOUNT            
                                                                        
           IF AT-CARRIER = '8'
              MOVE '04'                TO FX-SUB-TYPE
           END-IF

           PERFORM 1100-GET-PLAN THRU 1100-EXIT
           
           IF AT-CLAIM-TYPE = 'L'
              MOVE WS-LF-PLAN       TO FX-PLAN-CODE
           ELSE
              MOVE WS-AH-PLAN       TO FX-PLAN-CODE
           END-IF
           MOVE WS-CURR-ME-MMDDYYYY TO FX-POSTING-DATE                  
           MOVE AT-CERT-NO          TO FX-POLICY-NO                     
           MOVE SPACES              TO FX-CITY                          
           MOVE 'Y'                 TO FX-LOC-CODE                      
           MOVE CL-CERT-STATE       TO FX-STATE                         
                                                                        
           MOVE SPACES              TO FX-ZIP-CODE                      
                                                                        
           MOVE 'S'                 TO FX-FY-REN                        

           MOVE SPACES              TO FX-AGENT-01                      
           MOVE ' '                 TO FX-DISTR                         
           MOVE ' '                 TO FX-SOURCE-ACCT                   
           MOVE ' '                 TO FX-REFERENCE                     
           MOVE AT-CLAIM-NO         TO FX-CLAIM-NO
           MOVE '000'               TO FX-DRAFT-NO (1:3)
           MOVE AT-CHECK-NO         TO FX-DRAFT-NO (4:7)
           IF AT-PAYMENT-ORIGIN = '1'
              MOVE 'M'              TO FX-DRAFT-STATUS
           ELSE
              MOVE 'O'              TO FX-DRAFT-STATUS
           END-IF
                                                                        
           MOVE SPACES              TO FX-TAX-ID                        
                                                                        
           MOVE SPACES                   TO WS-DESCRIPTION              
           MOVE WS-DESCRIPTION           TO FX-DESCRIPTION


      **** SEARCH PULLED FROM FNB160
           IF FX-STATE NOT= SPACES
               SEARCH ALL STATE-TABLE
                   AT END DISPLAY 'INVALID STATE CODE: ' FX-STATE
                   WHEN ST-STATE (ST-INDEX) = FX-STATE
                   MOVE ST-ALT-STATE (ST-INDEX) TO FX-STATE
               END-SEARCH
           END-IF

           MOVE WS-CURR-ME-MMDDYYYY      TO FX-JOURNAL-DATE

      **** THE '*' IN POSITION 250 ENSURES A 250 BYTE RECORD IS PASSED
      **** TO FREEDOM - PREVENTS TRUNCATION OF BLANK FIELDS
           MOVE '*'                      TO EXTRACT-RECORD(250:1)


           WRITE EXTRACT-RECORD

           .
       
       0300-EXIT.
           EXIT.


       0400-OPEN-FILES.

           OPEN INPUT ELTRLR ELMSTR ELCERT
                OUTPUT EXTRACT-FILE

           IF ELTRLR-FILE-STATUS = '00' 
              CONTINUE
           ELSE
              DISPLAY 'ELTRLR OPEN ERR  ' ELTRLR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELMSTR-FILE-STATUS = '00' 
              CONTINUE
           ELSE
              DISPLAY 'ELMSTR OPEN ERR  ' ELMSTR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCERT-FILE-STATUS = '00' 
              CONTINUE
           ELSE
              DISPLAY 'ELCERT OPEN ERR  ' ELCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELTRLR ELMSTR ELCERT EXTRACT-FILE

           .

       0500-EXIT.
           EXIT.
       0550-START-ELTRLR.

           MOVE LOW-VALUES             TO AT-CONTROL-PRIMARY
           MOVE X'06'                  TO AT-COMPANY-CD

           START ELTRLR KEY IS NOT < AT-CONTROL-PRIMARY

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              SET END-OF-ELTRLR        TO TRUE
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELTRLR START     ' ELTRLR-FILE-STATUS
                 SET END-OF-ELTRLR     TO TRUE
              END-IF
           END-IF

           .

       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           PERFORM 1200-LOAD-BENEFIT-TABLE THRU 1200-EXIT
           
           MOVE PARM-MONTHEND-DATE TO DC-GREG-DATE-CYMD
           MOVE 'L'               TO DC-OPTION-CODE
           MOVE ZEROS             TO DC-ELAPSED-MONTHS
                                     DC-ELAPSED-DAYS
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1 TO WS-MONTHEND-BIN-DT
           ELSE
               DISPLAY 'ERROR CONVERTING CURR ME DATE'
               GO TO ABEND-PGM
           END-IF
           
           MOVE PARM-CURR-ME-YYYY TO WS-CURR-ME-YYYY
           MOVE PARM-CURR-ME-MM   TO WS-CURR-ME-MM
           MOVE PARM-CURR-ME-DD   TO WS-CURR-ME-DD

           PERFORM 0550-START-ELTRLR   THRU 0550-EXIT
           PERFORM 0200-READ-TRLR THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.


       1100-GET-PLAN.
                                                                       
           MOVE SPACES TO WS-LF-PLAN                                    
           MOVE SPACES TO WS-AH-PLAN                                    
                                                                        
           IF CM-LF-BENEFIT-CD NOT = '00'                                       
              MOVE CM-IND-GRP-TYPE TO WS-LF-PLAN1                                    
              SET BEN-INDEX TO +1                                       
              SEARCH BENEFIT-CODE-TABLE                                 
                 WHEN BEN-NUM (BEN-INDEX) = CM-LF-BENEFIT-CD
                   MOVE BEN-ALPHA (BEN-INDEX) TO WS-LF-PLAN2            
                 WHEN BEN-NUM (BEN-INDEX) = HIGH-VALUE                  
                   MOVE ZEROS TO WS-LF-PLAN2                            
                   DISPLAY 'INVALID LIFE BEN TYPE: '                    
                            CM-LF-BENEFIT-CD '  ' CL-CONTROL-PRIMARY                    
              END-SEARCH
           END-IF                                                       
                                                                        
           IF CM-AH-BENEFIT-CD NOT = '00'                                       
              MOVE CM-IND-GRP-TYPE TO WS-AH-PLAN1
              MOVE CM-AH-BENEFIT-CD TO WS-AH-PLAN2                              
           END-IF

           .                                                            
       1100-EXIT.                                                       
           EXIT.                                                        
           
       1200-LOAD-BENEFIT-TABLE.                                         
                                                                       
           OPEN INPUT LOGIC-CONTROL-FILE                                
           IF ELCNTL-STATUS = '00' OR '97'                              
               MOVE '00' TO ELCNTL-STATUS                               
           ELSE                                                         
               DISPLAY 'OPEN ERROR ' ELCNTL-STATUS ' ON ELCNTL'         
               GO TO ABEND-PGM
           END-IF
                                                                        
           SET BEN-INDEX TO +1
           READ LOGIC-CONTROL-FILE        
           
           PERFORM UNTIL ELCNTL-STATUS NOT = '00'                       
             IF (CF-COMPANY-ID = DTE-CLIENT)
                AND (CF-RECORD-TYPE = '4')
                PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8           
                  MOVE CF-BENEFIT-NUMERIC (SUB) TO BEN-NUM (BEN-INDEX)  
                  MOVE CF-BENEFIT-ALPHA (SUB) TO BEN-ALPHA (BEN-INDEX)  
                  SET BEN-INDEX UP BY +1                                
                  IF BEN-INDEX > 150 DISPLAY                            
                    'TABLE OVERFLOW IN ROUTINE 1200-LOAD-BENEFIT-TABLE' 
                    ADD +1 TO FORCE-DUMP                                
                  END-IF                                                
                END-PERFORM                                             
             END-IF                                                     
             READ LOGIC-CONTROL-FILE                                    
           END-PERFORM                                                  
                                                                        
           CLOSE LOGIC-CONTROL-FILE                                     
           DISPLAY 'BENEFIT TABLE LOADED'      

           .
       1200-EXIT.                                                       
           EXIT.                                                        

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.


       ABEND-PGM.
          CALL 'ABORTME'.
          
       