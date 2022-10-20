       IDENTIFICATION DIVISION.                                        
                                                                       
       PROGRAM-ID.                 CID023EX.                           
                                                                       
      *DATE-COMPILED.                                                  
                                                                       
                                                                       
      *REMARKS.                                                        
      *          ****  RISK DISTRIBUTION REPORT EXTRACT ****           
      *                                                                
      * PGM-OPT S-CODE COMING FROM ECS022:                             
      *    1......1 = RISK SUMMARY BY ACCOUNT..........(ECS-023A)      
       EJECT                                                           
       ENVIRONMENT DIVISION.                                           
       CONFIGURATION SECTION.                                          
       INPUT-OUTPUT SECTION.                                           
       FILE-CONTROL.                                                   
                                                                       
           SELECT  SORT-FILE    ASSIGN TO SYS001-UT-3380-S-SORTWK1.     
           SELECT  SUMMARY      ASSIGN TO SYS011-UT-2400-S-SYS011.      
           SELECT  ACCOUNT      ASSIGN TO ERACCTT                       
                                ACCESS         SEQUENTIAL               
                                ORGANIZATION   INDEXED                  
                                FILE STATUS    AM-FILE-STATUS           
                                RECORD KEY     AM-KEY.                  
           SELECT  DISK-DATE    ASSIGN TO SYS019-UT-3380-S-SYS019.      
           SELECT  EXTRACT-FILE ASSIGN TO SYS020-UT-2400-S-SYS020
                                ORGANIZATION IS LINE SEQUENTIAL.      
       EJECT                                                           
       DATA DIVISION.                                                  
       FILE SECTION.                                                   
                                                                       
       SD  SORT-FILE.                                                  
                                                                       
       01  SORT-REC.                                                   
           05  FILLER      PIC X(3).                                   
           05  SORT-KEY3   PIC X(7).                                   
           05  SORT-KEY2   PIC XX.                                     
           05  SORT-KEY1   PIC X(10).                                  
           05  SORT-KEY4   PIC X(6).
           05  FILLER      PIC X(1972).                                
                                                                       
       FD  EXTRACT-FILE.
      
       01  EXTRACT-RECORD  PIC X(2557). 
       EJECT                                                            
       FD  SUMMARY                                                      
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.                                            
                                                                        
       01  SUM-REC1.                                                    
           12  SREC-CODE           PIC X.                               
           12  FILLER              PIC X(1445).                         
                                                                        
       FD  ACCOUNT.                                                     
                                                                        
       01  AM-MSTR.                                                     
           05  FILLER              PIC XX.                              
           05  AM-KEY              PIC X(26).                           
           05  FILLER              PIC X(1972).                         
       EJECT                                                            
       FD  DISK-DATE                                                    
           COPY ELCDTEFD.                                               
       EJECT                                                            
       WORKING-STORAGE SECTION.                                         
       01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
       77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.   
       77  LCP-ASA                       PIC X.                         
       77  FILLER  PIC X(32) VALUE '********************************'.  
       77  FILLER  PIC X(32) VALUE '     ECS023 WORKING STORAGE     '.  
       77  FILLER  PIC X(32) VALUE '*****VMOD=2.011*****************'.  
                                                                        
       77  SUMMARY-COUNT           PIC  9(9)  COMP     VALUE ZERO.      
       77  FIRST-SW                PIC  X              VALUE 'X'.       
                                                                        
       EJECT                                                            
           COPY ERCACCT.                                                
       EJECT                                                            
                                                                        
       01  MISC-WK.                                                     
           12  AM-FILE-STATUS  PIC XX    VALUE '00'.                    
           12  HEAD-SPACE      PIC XXX   VALUE SPACES.                  
           12  AM-CO           PIC X(7)  VALUE SPACES.                  
           12  WS-SAVE-ACCOUNT PIC X(10) VALUE SPACES.
           12  L-NAME          PIC X(30) VALUE SPACES.                  
           12  X               PIC X     VALUE SPACE.                   
           12  PAGE-CT         PIC S9(5) VALUE +0  COMP-3.              
           12  X1              PIC S9(5) COMP.                          
           12  X2              PIC S9(5) COMP.                          
           12  X3              PIC S9(5) COMP.                          
           12  X4              PIC S9(5) COMP.                          
           12  X5              PIC S9(5) COMP.                          
           12  S-CNT           PIC S9(5)  VALUE +0  COMP-3.             
           12  C-CNT           PIC S9(5)  VALUE +0  COMP-3.             
           12  DUMMY-COMP-BK-SW PIC X     VALUE '0'.                    
           12  LAST-CTL.                                                
               16  L-CODE      PIC X.                                   
               16  L-CNTL      PIC X(10).                               
               16  L-CO.                                                
                   20  L-CARR  PIC X.                                   
                   20  L-GRP   PIC X(6).                                
               16  L-RPT2      PIC X(10).                               
               16  L-ST        PIC XX.                                  
               16  L-ACCT      PIC X(10).                               
           12  H4-CTL.                                                  
               16  FILLER      PIC XX.                                  
               16  H4-CMP.                                              
                   18  H4-CARR PIC X.                                   
                   18  H4-GRP  PIC X(6).                                
               16  FILLER      PIC X(4).                                
               16  H4-STE      PIC XX.                                  
           12  HOLD-PCT        PIC S9(3) OCCURS 13  COMP-3.             
           12  TOT-PREM        PIC S9(9)V99  COMP-3.                    
           12  PRM1            PIC S9(9)V99  COMP-3.                    
           12  TYP-HLD         PIC XX.                                  
           12  TYP-DES         PIC X(24).                               
           12  ST-SUB1.                                                 
               16  ST-SB       PIC 99.                                  
           12  WS-RETURN-CODE         PIC X(4)   VALUE SPACES.          
           12  WS-ABEND-OPTION        PIC X      VALUE 'Y'.             
           12  WS-ABEND-FILE-STATUS   PIC XX     VALUE SPACES.          
           12  WS-ABEND-OPTION        PIC X      VALUE 'Y'.             
           12  WS-ZERO                PIC S9     VALUE ZERO.            
           12  WS-ABEND-MESSAGE       PIC X(80)  VALUE SPACES.          
           12  PGM-SUB                PIC S999 COMP VALUE +023.         
      
       01  WORK-DATE           PIC 9(11).                               
       01  WORK-DATE-R  REDEFINES  WORK-DATE.                           
           12  FILLER          PIC 999.                                 
           12  WK-CCYY         PIC 9(04).                               
           12  WK-CCYR REDEFINES WK-CCYY.                               
               16  WK-CC       PIC 99.                                  
               16  WK-YR       PIC 99.                                  
           12  WK-MO           PIC 99.                                  
           12  WK-DA           PIC 99.                                  
       EJECT                                                            
       01  ACCUMS.                                                      
           12  ACC1    OCCURS  280 TIMES   COMP-3.                      
               16  L-PREM      PIC S9(9)V99.                            
               16  A-PREM      PIC S9(9)V99.                            
               16  L-BEN       PIC S9(12)V99.                           
               16  A-BEN       PIC S9(9)V99.                            
               16  L-CT        PIC S9(7).                               
               16  A-CT        PIC S9(7).                               
           12  ACC2    OCCURS   7  TIMES   COMP-3.                      
               16  KOUNT       PIC S9(7).                               
               16  AGE         PIC S9(11).                              
               16  TERM        PIC S9(11).                              
       01  WEIGHT-CNTRSX.                                               
           12  WEIGHT-CNTRS    OCCURS 7 TIMES    COMP-3.                
               16  WEIGHT-CNT    PIC S9(7).                             
               16  WEIGHT-FACE   PIC S9(12)V99.                         
               16  WEIGHT-WORK   PIC S9(12)V99.                         
       EJECT                                                            
       01  SUM-REC.                                                     
           12  SUM-CTL.                                                 
               16  S-CODE      PIC X.                                   
               16  S-CNTL      PIC X(10).                               
               16  S-CO        PIC X(7).                                
               16  S-RPT-CD2   PIC X(10).                               
               16  S-ST        PIC XX.                                  
               16  S-ACCT      PIC X(10).                               
           12  S-RPT-CD1       PIC X(10).                               
           12  SUM-DATA    OCCURS  40 TIMES  COMP-3.                    
               16  SL-P        PIC S9(9)V99.                            
               16  SA-P        PIC S9(9)V99.                            
               16  SL-B        PIC S9(12)V99.                           
               16  SA-B        PIC S9(9)V99.                            
               16  SL-C        PIC S9(7).                               
               16  SA-C        PIC S9(7).                               
           12  S-CT            PIC S9(7)     COMP-3.                    
           12  S-AGE           PIC S9(11)    COMP-3.                    
           12  S-TERM          PIC S9(11)    COMP-3.                    
           12  S-COUNT         PIC S9(7)     COMP-3.                    
           12  S-FACE          PIC S9(12)V99 COMP-3.                    
           12  S-WORK          PIC S9(12)V99 COMP-3.                    
      
       01  EXTRACT-REC.
           12  EXT-MOE-DATE.
               16  EXT-MOE-MM     PIC 9(2).
               16  FILLER         PIC X(1) VALUE '/'.
               16  EXT-MOE-DD     PIC 9(2).
               16  FILLER         PIC X(1) VALUE '/'.
               16  EXT-MOE-YR     PIC 9(4).
           12  FILLER             PIC X(1)  VALUE ';'.    
           12  EXT-CARRIER        PIC X(1).
           12  FILLER             PIC X(1)  VALUE ';'.    
           12  EXT-STATE          PIC X(2).
           12  FILLER             PIC X(1)  VALUE ';'.    
           12  EXT-ACCOUNT        PIC X(10).
           12  FILLER             PIC X(1)  VALUE ';'.    
           12  EXT-ACCT-NAME      PIC X(30).
           12  FILLER             PIC X(1)  VALUE ';'.    
           12  EXT-TERM-GRPS   OCCURS 4 TIMES.
               16  EXT-AGE-GRPS OCCURS 7 TIMES.
                   20  EXT-GROUP-ID PIC X(15).
                   20  FILLER     PIC X(1)  VALUE ';'.    
                   20  EXT-L-PREM PIC -(9).99.
                   20  FILLER     PIC X(1)  VALUE ';'.    
                   20  EXT-A-PREM PIC -(9).99.
                   20  FILLER     PIC X(1)  VALUE ';'.    
                   20  EXT-L-BEN  PIC -(12).99.
                   20  FILLER     PIC X(1)  VALUE ';'.    
                   20  EXT-A-BEN  PIC -(9).99.
                   20  FILLER     PIC X(1)  VALUE ';'.    
                   20  EXT-L-CNT  PIC Z(6)9.
                   20  FILLER     PIC X(1)  VALUE ';'.    
                   20  EXT-A-CNT  PIC Z(6)9.
                   20  FILLER     PIC X(1)  VALUE ';'.    
           12  EXT-NUM-POLICIES   PIC Z(6)9.
           12  FILLER             PIC X(1)  VALUE ';'.    
           12  EXT-TOT-AGE        PIC -(10)9.
           12  FILLER             PIC X(1)  VALUE ';'.    
           12  EXT-TOT-TERM       PIC -(10)9.
           12  FILLER             PIC X(1)  VALUE ';'.    
           12  EXT-WEIGHTED-AGE   PIC -(12).99.
           12  FILLER             PIC X(1)  VALUE ';'.    
           12  EXT-TOT-FACE       PIC -(12).99.
                                                                        
           COPY ELCDTECX.                                               
                                                                        
           COPY ELCDTEVR.                                               
       EJECT                                                            
       PROCEDURE DIVISION.                                              
       0000-DATE-READ.                                                  
           COPY ELCDTERX.                                               
                                                                        
       0100-START-1.                                                    
           OPEN INPUT   SUMMARY  ACCOUNT                                
                OUTPUT  EXTRACT-FILE.
                                                                        
       0120-FIRST-SET.                                                  
           MOVE RUN-DATE               TO WORK-DATE.
           MOVE WK-MO                  TO EXT-MOE-MM.
           MOVE WK-DA                  TO EXT-MOE-DD.
           MOVE WK-CCYY                TO EXT-MOE-YR.
                                                                        
       0130-CLEAR-40.                                                   
           MOVE +0                     TO X1.                           
                                                                        
       0140-CLEAR-LOOP.                                                 
           ADD +1 TO X1.                                                
           IF X1 GREATER THAN +40                                       
               GO TO 0150-CLEAR-X.                                      
                                                                        
           MOVE +0                     TO L-PREM (X1)  A-PREM (X1)      
                                          L-BEN (X1)   A-BEN (X1)       
                                          L-CT (X1)    A-CT (X1).       
           GO TO 0140-CLEAR-LOOP.                                       
                                                                        
       0150-CLEAR-X.                                                    
           EXIT.                                                        
                                                                        
       0160-CLEAR-FLDS.                                                     
           MOVE +0 TO KOUNT (1)  AGE (1)  TERM (1).                     
           MOVE ZERO                   TO WEIGHT-CNT (1)                
                                          WEIGHT-FACE (1)               
                                          WEIGHT-WORK (1).              
                                          
           MOVE +0 TO X1.
           PERFORM 4 TIMES 
               ADD +1 TO X1
               MOVE +0 TO X2
               PERFORM 7 TIMES
                   ADD +1 TO X2
                   MOVE SPACES TO EXT-GROUP-ID (X1 X2)
                   MOVE +0 TO EXT-L-PREM (X1 X2)
                   MOVE +0 TO EXT-A-PREM (X1 X2)
                   MOVE +0 TO EXT-L-BEN (X1 X2)
                   MOVE +0 TO EXT-A-BEN (X1 X2)
                   MOVE +0 TO EXT-L-CNT (X1 X2)
                   MOVE +0 TO EXT-A-CNT (X1 X2)
               END-PERFORM
           END-PERFORM.
                   
           MOVE +0 TO EXT-NUM-POLICIES.        
           MOVE +0 TO EXT-TOT-AGE.
           MOVE +0 TO EXT-TOT-TERM.
           MOVE +0 TO EXT-TOT-FACE.
           MOVE +0 TO EXT-WEIGHTED-AGE.        
       EJECT                  
       0160-EXIT.
           EXIT.                                          
       0180-SORT-PROCEDURE.                                             
           SORT SORT-FILE ON ASCENDING SORT-KEY1                        
                                       SORT-KEY3                        
                                       SORT-KEY2                        
                           DESCENDING  SORT-KEY4
               INPUT PROCEDURE  0190-GET-ACCT  THRU 0200-ACCT-END       
               OUTPUT PROCEDURE 0220-READ-ACCT THRU 0480-CLOSE-EXIT.    
                                                                        
           IF SORT-RETURN NOT = ZEROS                                   
               MOVE '0101'              TO WS-RETURN-CODE               
               GO TO ABEND-PGM.                                         
                                                                        
           GOBACK.                                                      
                                                                        
       0190-GET-ACCT SECTION.                                           
           READ ACCOUNT AT END                                          
               CLOSE ACCOUNT                                            
               GO TO 0200-ACCT-END.                                     
                                                                        
           MOVE AM-KEY (11:1)          TO AM-MSTR (2:1)
           MOVE AM-KEY (2:1)           TO AM-KEY (11:1)
           RELEASE SORT-REC FROM AM-MSTR.                               
           GO TO 0190-GET-ACCT.                                         
                                                                        
       0200-ACCT-END.                                                   
           EXIT.                                                        
                                                                        
       EJECT                                                            
                                                                        
       0220-READ-ACCT SECTION.                                          
           RETURN SORT-FILE INTO ACCOUNT-MASTER AT END                  
               MOVE HIGH-VALUES TO ACCOUNT-MASTER.                      
                                                                        
       0230-EXIT.                                                       
           EXIT.                                                        
                                                                        
       0240-READ-SUMMARY.                                               
           READ SUMMARY INTO SUM-REC AT END                             
               GO TO 0450-OVER-BK.                                      
                                                                        
           ADD +1 TO SUMMARY-COUNT.                                     
                                                                        
                                                                        
      ***** SUMMARY BY ACCOUNT                                          
           IF S-CODE  NOT EQUAL '1'  
               GO TO 0240-READ-SUMMARY.                                 
                                                                        
                                                                        
           IF LCP-ONCTR-01 =  0                                         
               ADD 1 TO LCP-ONCTR-01                                    
               MOVE SUM-CTL            TO LAST-CTL.                     
                                                                        
                                                                        
       0250-R-S-X.                                                      
           GO TO 0300-ACCUMULATE.                                       
       EJECT                                                            
       0260-FIND-ACCT.                                                  
           IF L-ACCT  = AM-ACCOUNT   AND                                
              L-CARR  = AM-CARRIER   AND                                
              L-GRP   = AM-GROUPING  AND                                
              L-ST    = AM-STATE                                        
               MOVE AM-NAME            TO L-NAME                        
               MOVE AM-ACCOUNT         TO WS-SAVE-ACCOUNT
               MOVE AM-RECORD-ID (2:1) TO WS-SAVE-ACCOUNT (1:1)
               GO TO 0290-EXIT.                          
                                                                        
           PERFORM 0220-READ-ACCT THRU 0230-EXIT.                       
           GO TO 0260-FIND-ACCT.                                        
                                                                        
                                                                        
       0290-EXIT.                                                       
           EXIT.                                                        
       EJECT                                                            
       0300-ACCUMULATE.                                                 
           MOVE +0                     TO X1.                           
                                                                        
       0310-A-LOOP.                                                     
           ADD +1 TO X1.                                                
           IF X1 GREATER THAN +40                                       
               GO TO 0320-A-CAT.                                        
                                                                        
           ADD SL-P (X1) TO L-PREM (X1).                                
           ADD SA-P (X1) TO A-PREM (X1).                                
           ADD SL-B (X1) TO L-BEN (X1).                                 
           ADD SA-B (X1) TO A-BEN (X1).                                 
           ADD SL-C (X1) TO L-CT (X1).                                  
           ADD SA-C (X1) TO A-CT (X1).                                  
                                                                        
           GO TO 0310-A-LOOP.                                           
                                                                        
       0320-A-CAT.                                                      
           ADD S-CT    TO KOUNT (1).                                    
           ADD S-AGE   TO AGE (1).                                      
           ADD S-TERM  TO TERM (1).                                     
           ADD S-COUNT TO WEIGHT-CNT (1).                               
           ADD S-FACE  TO WEIGHT-FACE (1).                              
           ADD S-WORK  TO WEIGHT-WORK (1).                              
                                                                        
       0330-A-X.                                                        
           PERFORM 0240-READ-SUMMARY.                                   
                                                                        
           IF S-CNTL NOT = L-CNTL                                       
               GO TO 0400-ACCT-BK.                                      
                                                                        
           GO TO 0300-ACCUMULATE.                                       
                                                                        
       EJECT                                                            
       0400-ACCT-BK.                                                    
                                                                        
           MOVE +0                     TO S-CNT  C-CNT.                 
                                                                        
           PERFORM 0260-FIND-ACCT THRU 0290-EXIT.
           
           MOVE L-CARR                 TO EXT-CARRIER.
           MOVE L-ST                   TO EXT-STATE.
           MOVE WS-SAVE-ACCOUNT        TO EXT-ACCOUNT.
           MOVE L-NAME                 TO EXT-ACCT-NAME.
      
           MOVE 'T-1-18-A-UN-36'       TO EXT-GROUP-ID (1 1).
           MOVE L-PREM (1)             TO EXT-L-PREM (1 1).
           MOVE A-PREM (1)             TO EXT-A-PREM (1 1).
           MOVE L-BEN (1)              TO EXT-L-BEN (1 1).
           MOVE A-BEN (1)              TO EXT-A-BEN (1 1).
           MOVE L-CT (1)               TO EXT-L-CNT (1 1).
           MOVE A-CT (1)               TO EXT-A-CNT (1 1).
      
           MOVE 'T-1-18-A-36-40'       TO EXT-GROUP-ID (1 2).
           MOVE L-PREM (2)             TO EXT-L-PREM (1 2).
           MOVE A-PREM (2)             TO EXT-A-PREM (1 2).
           MOVE L-BEN (2)              TO EXT-L-BEN (1 2).
           MOVE A-BEN (2)              TO EXT-A-BEN (1 2).
           MOVE L-CT (2)               TO EXT-L-CNT (1 2).
           MOVE A-CT (2)               TO EXT-A-CNT (1 2).
      
           MOVE 'T-1-18-A-41-45'       TO EXT-GROUP-ID (1 3).
           MOVE L-PREM (3)             TO EXT-L-PREM (1 3).
           MOVE A-PREM (3)             TO EXT-A-PREM (1 3).
           MOVE L-BEN (3)              TO EXT-L-BEN (1 3).
           MOVE A-BEN (3)              TO EXT-A-BEN (1 3).
           MOVE L-CT (3)               TO EXT-L-CNT (1 3).
           MOVE A-CT (3)               TO EXT-A-CNT (1 3).
      
           MOVE 'T-1-18-A-46-50'       TO EXT-GROUP-ID (1 4).
           MOVE L-PREM (4)             TO EXT-L-PREM (1 4).
           MOVE A-PREM (4)             TO EXT-A-PREM (1 4).
           MOVE L-BEN (4)              TO EXT-L-BEN (1 4).
           MOVE A-BEN (4)              TO EXT-A-BEN (1 4).
           MOVE L-CT (4)               TO EXT-L-CNT (1 4).
           MOVE A-CT (4)               TO EXT-A-CNT (1 4).
      
           MOVE 'T-1-18-A-51-55'       TO EXT-GROUP-ID (1 5).
           MOVE L-PREM (5)             TO EXT-L-PREM (1 5).
           MOVE A-PREM (5)             TO EXT-A-PREM (1 5).
           MOVE L-BEN (5)              TO EXT-L-BEN (1 5).
           MOVE A-BEN (5)              TO EXT-A-BEN (1 5).
           MOVE L-CT (5)               TO EXT-L-CNT (1 5).
           MOVE A-CT (5)               TO EXT-A-CNT (1 5).
      
           MOVE 'T-1-18-A-56-64'       TO EXT-GROUP-ID (1 6).
           MOVE L-PREM (6)             TO EXT-L-PREM (1 6).
           MOVE A-PREM (6)             TO EXT-A-PREM (1 6).
           MOVE L-BEN (6)              TO EXT-L-BEN (1 6).
           MOVE A-BEN (6)              TO EXT-A-BEN (1 6).
           MOVE L-CT (6)               TO EXT-L-CNT (1 6).
           MOVE A-CT (6)               TO EXT-A-CNT (1 6).
      
           MOVE 'T-1-18-A-OV-64'       TO EXT-GROUP-ID (1 7).
           MOVE L-PREM (7)             TO EXT-L-PREM (1 7).
           MOVE A-PREM (7)             TO EXT-A-PREM (1 7).
           MOVE L-BEN (7)              TO EXT-L-BEN (1 7).
           MOVE A-BEN (7)              TO EXT-A-BEN (1 7).
           MOVE L-CT (7)               TO EXT-L-CNT (1 7).
           MOVE A-CT (7)               TO EXT-A-CNT (1 7).
      
           MOVE 'T-19-36-A-UN-36'       TO EXT-GROUP-ID (2 1).
           MOVE L-PREM (9)             TO EXT-L-PREM (2 1).
           MOVE A-PREM (9)             TO EXT-A-PREM (2 1).
           MOVE L-BEN (9)              TO EXT-L-BEN (2 1).
           MOVE A-BEN (9)              TO EXT-A-BEN (2 1).
           MOVE L-CT (9)               TO EXT-L-CNT (2 1).
           MOVE A-CT (9)               TO EXT-A-CNT (2 1).
      
           MOVE 'T-19-36-A-36-40'       TO EXT-GROUP-ID (2 2).
           MOVE L-PREM (10)             TO EXT-L-PREM (2 2).
           MOVE A-PREM (10)             TO EXT-A-PREM (2 2).
           MOVE L-BEN (10)              TO EXT-L-BEN (2 2).
           MOVE A-BEN (10)              TO EXT-A-BEN (2 2).
           MOVE L-CT (10)               TO EXT-L-CNT (2 2).
           MOVE A-CT (10)               TO EXT-A-CNT (2 2).
      
           MOVE 'T-19-36-A-41-45'       TO EXT-GROUP-ID (2 3).
           MOVE L-PREM (11)             TO EXT-L-PREM (2 3).
           MOVE A-PREM (11)             TO EXT-A-PREM (2 3).
           MOVE L-BEN (11)              TO EXT-L-BEN (2 3).
           MOVE A-BEN (11)              TO EXT-A-BEN (2 3).
           MOVE L-CT (11)               TO EXT-L-CNT (2 3).
           MOVE A-CT (11)               TO EXT-A-CNT (2 3).
      
           MOVE 'T-19-36-A-46-50'       TO EXT-GROUP-ID (2 4).
           MOVE L-PREM (12)             TO EXT-L-PREM (2 4).
           MOVE A-PREM (12)             TO EXT-A-PREM (2 4).
           MOVE L-BEN (12)              TO EXT-L-BEN (2 4).
           MOVE A-BEN (12)              TO EXT-A-BEN (2 4).
           MOVE L-CT (12)               TO EXT-L-CNT (2 4).
           MOVE A-CT (12)               TO EXT-A-CNT (2 4).
      
           MOVE 'T-19-36-A-51-55'       TO EXT-GROUP-ID (2 5).
           MOVE L-PREM (13)             TO EXT-L-PREM (2 5).
           MOVE A-PREM (13)             TO EXT-A-PREM (2 5).
           MOVE L-BEN (13)              TO EXT-L-BEN (2 5).
           MOVE A-BEN (13)              TO EXT-A-BEN (2 5).
           MOVE L-CT (13)               TO EXT-L-CNT (2 5).
           MOVE A-CT (13)               TO EXT-A-CNT (2 5).
      
           MOVE 'T-19-36-A-56-64'       TO EXT-GROUP-ID (2 6).
           MOVE L-PREM (14)             TO EXT-L-PREM (2 6).
           MOVE A-PREM (14)             TO EXT-A-PREM (2 6).
           MOVE L-BEN (14)              TO EXT-L-BEN (2 6).
           MOVE A-BEN (14)              TO EXT-A-BEN (2 6).
           MOVE L-CT (14)               TO EXT-L-CNT (2 6).
           MOVE A-CT (14)               TO EXT-A-CNT (2 6).
      
           MOVE 'T-19-36-A-OV-64'       TO EXT-GROUP-ID (2 7).
           MOVE L-PREM (15)             TO EXT-L-PREM (2 7).
           MOVE A-PREM (15)             TO EXT-A-PREM (2 7).
           MOVE L-BEN (15)              TO EXT-L-BEN (2 7).
           MOVE A-BEN (15)              TO EXT-A-BEN (2 7).
           MOVE L-CT (15)               TO EXT-L-CNT (2 7).
           MOVE A-CT (15)               TO EXT-A-CNT (2 7).
      
           MOVE 'T-37-60-A-UN-36'       TO EXT-GROUP-ID (3 1).
           MOVE L-PREM (17)             TO EXT-L-PREM (3 1).
           MOVE A-PREM (17)             TO EXT-A-PREM (3 1).
           MOVE L-BEN (17)              TO EXT-L-BEN (3 1).
           MOVE A-BEN (17)              TO EXT-A-BEN (3 1).
           MOVE L-CT (17)               TO EXT-L-CNT (3 1).
           MOVE A-CT (17)               TO EXT-A-CNT (3 1).
      
           MOVE 'T-37-60-A-36-40'       TO EXT-GROUP-ID (3 2).
           MOVE L-PREM (18)             TO EXT-L-PREM (3 2).
           MOVE A-PREM (18)             TO EXT-A-PREM (3 2).
           MOVE L-BEN (18)              TO EXT-L-BEN (3 2).
           MOVE A-BEN (18)              TO EXT-A-BEN (3 2).
           MOVE L-CT (18)               TO EXT-L-CNT (3 2).
           MOVE A-CT (18)               TO EXT-A-CNT (3 2).
      
           MOVE 'T-37-60-A-41-45'       TO EXT-GROUP-ID (3 3).
           MOVE L-PREM (19)             TO EXT-L-PREM (3 3).
           MOVE A-PREM (19)             TO EXT-A-PREM (3 3).
           MOVE L-BEN (19)              TO EXT-L-BEN (3 3).
           MOVE A-BEN (19)              TO EXT-A-BEN (3 3).
           MOVE L-CT (19)               TO EXT-L-CNT (3 3).
           MOVE A-CT (19)               TO EXT-A-CNT (3 3).
      
           MOVE 'T-37-60-A-46-50'       TO EXT-GROUP-ID (3 4).
           MOVE L-PREM (20)             TO EXT-L-PREM (3 4).
           MOVE A-PREM (20)             TO EXT-A-PREM (3 4).
           MOVE L-BEN (20)              TO EXT-L-BEN (3 4).
           MOVE A-BEN (20)              TO EXT-A-BEN (3 4).
           MOVE L-CT (20)               TO EXT-L-CNT (3 4).
           MOVE A-CT (20)               TO EXT-A-CNT (3 4).
      
           MOVE 'T-37-60-A-51-55'       TO EXT-GROUP-ID (3 5).
           MOVE L-PREM (21)             TO EXT-L-PREM (3 5).
           MOVE A-PREM (21)             TO EXT-A-PREM (3 5).
           MOVE L-BEN (21)              TO EXT-L-BEN (3 5).
           MOVE A-BEN (21)              TO EXT-A-BEN (3 5).
           MOVE L-CT (21)               TO EXT-L-CNT (3 5).
           MOVE A-CT (21)               TO EXT-A-CNT (3 5).
      
           MOVE 'T-37-60-A-56-64'       TO EXT-GROUP-ID (3 6).
           MOVE L-PREM (22)             TO EXT-L-PREM (3 6).
           MOVE A-PREM (22)             TO EXT-A-PREM (3 6).
           MOVE L-BEN (22)              TO EXT-L-BEN (3 6).
           MOVE A-BEN (22)              TO EXT-A-BEN (3 6).
           MOVE L-CT (22)               TO EXT-L-CNT (3 6).
           MOVE A-CT (22)               TO EXT-A-CNT (3 6).
      
           MOVE 'T-37-60-A-OV-64'       TO EXT-GROUP-ID (3 7).
           MOVE L-PREM (23)             TO EXT-L-PREM (3 7).
           MOVE A-PREM (23)             TO EXT-A-PREM (3 7).
           MOVE L-BEN (23)              TO EXT-L-BEN (3 7).
           MOVE A-BEN (23)              TO EXT-A-BEN (3 7).
           MOVE L-CT (23)               TO EXT-L-CNT (3 7).
           MOVE A-CT (23)               TO EXT-A-CNT (3 7).
      
           MOVE 'T-61-OV-A-UN-36'       TO EXT-GROUP-ID (4 1).
           MOVE L-PREM (25)             TO EXT-L-PREM (4 1).
           MOVE A-PREM (25)             TO EXT-A-PREM (4 1).
           MOVE L-BEN (25)              TO EXT-L-BEN (4 1).
           MOVE A-BEN (25)              TO EXT-A-BEN (4 1).
           MOVE L-CT (25)               TO EXT-L-CNT (4 1).
           MOVE A-CT (25)               TO EXT-A-CNT (4 1).
      
           MOVE 'T-61-OV-A-36-40'       TO EXT-GROUP-ID (4 2).
           MOVE L-PREM (26)             TO EXT-L-PREM (4 2).
           MOVE A-PREM (26)             TO EXT-A-PREM (4 2).
           MOVE L-BEN (26)              TO EXT-L-BEN (4 2).
           MOVE A-BEN (26)              TO EXT-A-BEN (4 2).
           MOVE L-CT (26)               TO EXT-L-CNT (4 2).
           MOVE A-CT (26)               TO EXT-A-CNT (4 2).
      
           MOVE 'T-61-OV-A-41-45'       TO EXT-GROUP-ID (4 3).
           MOVE L-PREM (27)             TO EXT-L-PREM (4 3).
           MOVE A-PREM (27)             TO EXT-A-PREM (4 3).
           MOVE L-BEN (27)              TO EXT-L-BEN (4 3).
           MOVE A-BEN (27)              TO EXT-A-BEN (4 3).
           MOVE L-CT (27)               TO EXT-L-CNT (4 3).
           MOVE A-CT (27)               TO EXT-A-CNT (4 3).
      
           MOVE 'T-61-OV-A-46-50'       TO EXT-GROUP-ID (4 4).
           MOVE L-PREM (28)             TO EXT-L-PREM (4 4).
           MOVE A-PREM (28)             TO EXT-A-PREM (4 4).
           MOVE L-BEN (28)              TO EXT-L-BEN (4 4).
           MOVE A-BEN (28)              TO EXT-A-BEN (4 4).
           MOVE L-CT (28)               TO EXT-L-CNT (4 4).
           MOVE A-CT (28)               TO EXT-A-CNT (4 4).
      
           MOVE 'T-61-OV-A-51-55'       TO EXT-GROUP-ID (4 5).
           MOVE L-PREM (29)             TO EXT-L-PREM (4 5).
           MOVE A-PREM (29)             TO EXT-A-PREM (4 5).
           MOVE L-BEN (29)              TO EXT-L-BEN (4 5).
           MOVE A-BEN (29)              TO EXT-A-BEN (4 5).
           MOVE L-CT (29)               TO EXT-L-CNT (4 5).
           MOVE A-CT (29)               TO EXT-A-CNT (4 5).
      
           MOVE 'T-61-OV-A-56-64'       TO EXT-GROUP-ID (4 6).
           MOVE L-PREM (30)             TO EXT-L-PREM (4 6).
           MOVE A-PREM (30)             TO EXT-A-PREM (4 6).
           MOVE L-BEN (30)              TO EXT-L-BEN (4 6).
           MOVE A-BEN (30)              TO EXT-A-BEN (4 6).
           MOVE L-CT (30)               TO EXT-L-CNT (4 6).
           MOVE A-CT (30)               TO EXT-A-CNT (4 6).
      
           MOVE 'T-61-OV-A-OV-64'       TO EXT-GROUP-ID (4 7).
           MOVE L-PREM (31)             TO EXT-L-PREM (4 7).
           MOVE A-PREM (31)             TO EXT-A-PREM (4 7).
           MOVE L-BEN (31)              TO EXT-L-BEN (4 7).
           MOVE A-BEN (31)              TO EXT-A-BEN (4 7).
           MOVE L-CT (31)               TO EXT-L-CNT (4 7).
           MOVE A-CT (31)               TO EXT-A-CNT (4 7).
           
           MOVE KOUNT (1)               TO EXT-NUM-POLICIES.
           MOVE AGE (1)                 TO EXT-TOT-AGE.
           MOVE TERM (1)                TO EXT-TOT-TERM.
           MOVE WEIGHT-FACE (1)         TO EXT-TOT-FACE.
           MOVE WEIGHT-WORK (1)         TO EXT-WEIGHTED-AGE.
           
           WRITE EXTRACT-RECORD FROM EXTRACT-REC.
      
           PERFORM 0130-CLEAR-40 THRU 0150-CLEAR-X.                     
           PERFORM 0160-CLEAR-FLDS THRU 0160-EXIT.
      
                                                                        
       0420-ACCT-BUMP.                                                  
           MOVE SUM-CTL                TO LAST-CTL.                     
                                                                        
                                                                        
       0430-ACCT-EXIT.                                                  
           GO TO 0300-ACCUMULATE.                                       
       EJECT                                                            
                                                                        
       0450-OVER-BK.                                                    
           IF SUMMARY-COUNT = ZERO                                      
               GO TO 0460-CLOSE-FILES.                                  
                                                                        
           PERFORM 0400-ACCT-BK THRU 0420-ACCT-BUMP.                    
       EJECT                                                            
       0460-CLOSE-FILES.                                                
           CLOSE SUMMARY  EXTRACT-FILE.
                                                                        
       0470-ACCT-LOOP.                                                  
           IF AM-MSTR-CNTRL = HIGH-VALUES                               
               GO TO 0480-CLOSE-EXIT.                                   
                                                                        
           PERFORM 0220-READ-ACCT THRU 0230-EXIT.                       
                                                                        
           GO TO 0470-ACCT-LOOP.                                        
                                                                        
       0480-CLOSE-EXIT.                                                 
           EXIT.                                                        
       EJECT                                                            
                                                                        
                                                                        
       ABEND-PGM SECTION.                                               
           COPY ELCABEND.                                               
      /                                                                 
