       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.                ECS032E.
      *DATE-COMPILED.                                                   
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF   CSO       IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
      *REMARKS.                                                         
      *        SPECIAL CLAIMS RESERVE REPORT FOR EMERALD ONLY.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 040610    2010010400006  PEMA  SPECIAL FOR EMERALD ONLY
      ******************************************************************
                                                                        
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
                                                                        
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT SORT-FILE    ASSIGN TO SYS001-UT-3380-S-SORTWK1.      
           SELECT PRNTR        ASSIGN TO SYS008-UR-1403-S-SYS008.       
           SELECT EXTRACT      ASSIGN TO SYS018-UT-2400-S-SYS018.       
           SELECT DISK-DATE    ASSIGN TO SYS019-UT-3380-S-SYS019.       
           SELECT ELTRLR           ASSIGN TO ELTRLR
                                   ORGANIZATION INDEXED             
                                   ACCESS       DYNAMIC             
                                   RECORD KEY   AT-CONTROL-PRIMARY  
                                   FILE STATUS  ELTRLR-FILE-STATUS. 

           SELECT FICH         ASSIGN TO SYS020-UT-2400-S-SYS020.       
      
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       SD  SORT-FILE.                                                   
                                                                        
       01  SORT-RECORD.                                                 
           12  FILLER              PIC  X(4).                           
           12  SORT-KEY3           PIC  X(36).                          
           12  SORT-KEY1           PIC  X(1).                           
           12  SORT-KEY2           PIC  X(10).                          
           12  FILLER              PIC  X(186).                         
           12  SORT-REIN-COMP      PIC  X(6).                           
           12  FILLER              PIC  X(267).                         

       FD  PRNTR
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  PRT.
           12  P-CTL              PIC X.
           12  P-DATA             PIC X(132).

       FD  EXTRACT                                                      
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.                                         
                                                                        
       01  EXTRACT-RECORD          PIC  X(510).                         
      
       FD  DISK-DATE                                                    
                                   COPY ELCDTEFD.                       

       FD  ELTRLR.
                                       COPY ELCTRLR.

       FD  FICH
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  FICH-REC                   PIC X(133).

       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC  X(32) VALUE '********************************'. 
       77  FILLER  PIC  X(32) VALUE '    ECS032E WORKING STORAGE     '. 
       77  FILLER  PIC  X(32) VALUE '********* VMOD=2.001 ***********'. 
                                                                        
       77  PGM-SUB                 PIC S9(3)   COMP    VALUE +032.      
       77  SUB1                    PIC  9(4)   COMP    VALUE ZEROS.     
       77  SUB2                    PIC  9(4)   COMP    VALUE ZEROS.     
       77  SUB3                    PIC S9(5)   COMP-3  VALUE ZEROS.     
       77  SUB4                    PIC  9(4)   COMP    VALUE ZEROS.     
       77  X                       PIC  X              VALUE SPACE.     
       77  DIRECT-SW               PIC  X              VALUE SPACE.     
       77  FIRST-READ-SW           PIC  X              VALUE SPACE.     
           88  FIRST-READ             VALUE SPACE.                      
       77  SORT-SW                 PIC  X              VALUE SPACE.     
           88  NO-RECORDS-SORTED      VALUE SPACE.                      
       77  FST                     PIC  9              VALUE ZERO.      
       77  LINE-CNT                PIC  9(3)           VALUE 99.        
       77  PAGE-SIZE               PIC  9(3)           VALUE 56.        
       77  PGE-CNT                 PIC  9(4)           VALUE ZEROS.     
       77  RECD-CNT                PIC  9(6)           VALUE ZEROS.     
       77  W-LINES-USED            PIC  9(3)           VALUE ZEROS.     
       77  PRINT-HOLD              PIC  X(132).
       77  ELTRLR-FILE-STATUS      PIC XX              VALUE LOW-VALUES.
                                                                        
       01  TOTAL-COUNTERS.                                              
           12  COUNTERS        COMP-3      OCCURS  5  TIMES.            
               16  LIFE-PAYCUR     PIC S9(9)V99.                        
               16  LIFE-FUTURE     PIC S9(9)V99.                        
               16  DISB-PAYCUR     PIC S9(9)V99.                        
               16  DISB-FUTURE     PIC S9(9)V99.                        
               16  INCURRED-YEAR-COUNTERS  COMP-3   OCCURS  11  TIMES.  
                   20 PAYCUR-RSRV  PIC S9(9)V99.                        
                   20 FUTURE-RSRV  PIC S9(9)V99.                        

       01  TOTAL-LEVEL-NAMES.                                           
           12  FILLER              PIC  X(25)          VALUE            
                   'ACCOUNT TOTAL            '.                         
           12  FILLER              PIC  X(25)          VALUE            
                   'STATE TOTAL              '.                         
           12  FILLER              PIC  X(25)          VALUE            
                   'CARRIER X TOTAL          '.                         
           12  FILLER              PIC  X(25)          VALUE            
                   'OVERALL TOTAL            '.                         
           12  FILLER              PIC  X(25)          VALUE            
                   'OVERALL TOTAL            '.                         
                                                                        
       01  TOTAL-DESCRIPTION REDEFINES TOTAL-LEVEL-NAMES.               
           12  TOTAL-ID            PIC  X(25)      OCCURS  5  TIMES.    
       01  FILLER REDEFINES TOTAL-LEVEL-NAMES.                          
           12  FILLER                              OCCURS  5  TIMES.    
               16  FILLER          PIC  X(08).                          
               16  TOTAL-ID-CARRIER                                     
                                   PIC  XX.                             
               16  TOTAL-ID-TITLE  PIC  X(15).                          
       EJECT                                                            
       01  HEAD-1.                                                      
           12  FILLER              PIC  X(51)          VALUE SPACES.    
           12  FILLER              PIC  X(21)          VALUE            
                   'CLAIM RESERVES REPORT'.                             
           12  FILLER              PIC  X(47)          VALUE SPACES.    
           12  FILLER              PIC  X(6)           VALUE 'ECS032'.  
           12  HD1-SUFFIX          PIC  X              VALUE 'A'.       
                                                                        
       01  HEAD-2.                                                      
           12  FILLER              PIC  X(47)          VALUE SPACES.    
           12  CO-NAME             PIC  X(30).                          
           12  FILLER              PIC  X(42)          VALUE SPACES.    
           12  HD-DATE             PIC  X(8).                           
                                                                        
       01  HEAD-3.                                                      
           12  FILLER              PIC  X(53)          VALUE SPACES.    
           12  HD-ALF-DTE          PIC  X(18).                          
           12  FILLER              PIC  X(48)          VALUE SPACES.    
           12  FILLER              PIC  X(5)           VALUE 'PAGE '.   
           12  HD-PAGE             PIC ZZ,ZZZ.                          
                                                                        
       01  HEAD-4.                                                      
           12  HD4-RPT-REIN-HDG    PIC  X(20)          VALUE            
                   'REPORT CODE 1: '.                                   
           12  HD4-RPT-REIN-CD     PIC  X(10)          VALUE SPACES.    
                                                                        
      
       01  DETL-HEAD1.
           12  FILLER              PIC X(130)          VALUE
              'CAR ST  ACCOUNT   CERTIFICATE                CLAIM     IS
      -       'S    EFF DATE  ORG TRM  INC DATE  ORIG BENEFIT TYPE      
      -       '  PTC    FUTURE'.
           

      
       01  DETL-HEAD2.
           12  FILLER              PIC X(130)           VALUE
              'CAUSE                                         NUM      AG
      -       'E     PD THRU  REM TRM  RPT DATE   REM BENEFIT           
      -       'RESERVE  RESERVE'.

      
       01  DETL-LINE.                                              
           12  FILLER              PIC  X              VALUE SPACES.
           12  DETL-CAR            PIC  X.                         
           12  FILLER              PIC  XX             VALUE SPACES.
           12  DETL-ST             PIC  XX.                        
           12  FILLER              PIC  X              VALUE SPACES.
           12  DETL-ACCT           PIC  X(10).                     
           12  FILLER              PIC  X              VALUE SPACES.
           12  DETL-CERT           PIC  X(11).                     
           12  FILLER              PIC  X(16)          VALUE SPACES.
           12  DETL-CLM            PIC  X(7).
           12  FILLER              PIC  XXX            VALUE SPACES.
           12  DETL-ISS-AGE        PIC  99.                         
           12  FILLER              PIC  X(5)           VALUE SPACES.
           12  DETL-EFF            PIC  X(10).                      
           12  FILLER              PIC  XX             VALUE SPACES.
           12  DETL-ORIG-TERM      PIC  999.
           12  FILLER              PIC  XXXX           VALUE SPACES.
           12  DETL-INC-DT         PIC  X(10).
           12  FILLER              PIC  X              VALUE SPACES.
           12  DETL-ORIG-BENE      PIC  ZZZ,ZZZ.ZZ.
           12  FILLER              PIC  XX             VALUE SPACES.
           12  DETL-TYPE           PIC  X(6).
           12  FILLER              PIC  X              VALUE SPACES.
           12  DETL-PTC            PIC  ZZZ,ZZZ.ZZ.
           12  FILLER              PIC  X              VALUE SPACES.
           12  DETL-FUTURE         PIC  ZZZ,ZZZ.ZZ.
                                                                        
       01  TOTAL-LINE-P  REDEFINES  DETL-LINE.                          
           12  FILLER              PIC  X(10).                          
           12  TOTAL-NAME          PIC  X(30).                          
           12  TOTAL-DESC          PIC  X(38).                          
           12  FILLER              PIC  X(49).                          
      
       01  DETL-LINE2.
           12  DETL-CAUSE          PIC  X(60)          VALUE SPACES.
           12  FILLER              PIC  XX             VALUE SPACES.
           12  DETL-PD-THRU-DT     PIC  X(10).                     
           12  FILLER              PIC  XX             VALUE SPACES.
           12  DETL-REM-TERM       PIC  ZZZ            VALUE ZEROS.
           12  FILLER              PIC  XXXX           VALUE SPACES.
           12  DETL-RPT-DT         PIC  X(10).                     
           12  FILLER              PIC  X              VALUE SPACES.
           12  DETL-REM-BENE       PIC  ZZZ,ZZZ.ZZ.
      
       01  RUN-YEAR.                                                    
           12  FILLER              PIC X                 VALUE ' '.     
           12  PROCESS-CCYR        PIC X(4).                            
                                                                        
       01  CONTROL-HOLD.                                                
           12  RPT-1-REIN-CTL      PIC  X(10)            VALUE SPACES.  
           12  CAR-CTL             PIC  X                VALUE SPACES.  
           12  CO-CTL              PIC  X(6)             VALUE SPACES.  
           12  ST-CTL              PIC  XX               VALUE SPACES.  
           12  ACCT-CTL            PIC  X(10)            VALUE SPACES.  
                                                                        
       01  WS-WORK-DATE.
           12  WS-WORK-DATE-N          PIC 9(8).
       01  WS-ABEND.                                                    
           12  WS-RETURN-CODE          PIC  X(4)       VALUE SPACES.    
           12  WS-ABEND-MESSAGE        PIC  X(80)      VALUE SPACES.    
           12  WS-ABEND-FILE-STATUS    PIC  XX         VALUE SPACES.    
           12  WS-ZERO                 PIC S9          VALUE ZERO.      

                                       COPY ECSEXT01.                                               

                                       COPY ELCEXTVR.                                               

                                       COPY ELCDTECX.                                               

                                       COPY ELCDTEVR.                                               

       PROCEDURE DIVISION.                                              
                                                                        
                                                                        
       0000-SETUP-DATES.                                                
                                   COPY ELCDTERX.                       

           MOVE RUN-CCYR               TO  PROCESS-CCYR.                
                                                                        
           MOVE WS-CURRENT-DATE        TO  HD-DATE.                     
           MOVE COMPANY-NAME           TO  CO-NAME.                     
           MOVE ALPH-DATE              TO  HD-ALF-DTE.                  
                                                                        
           PERFORM 0400-INITIALIZATION-ROUTINE THRU 0499-EXIT.          
                                                                        
       0100-OPEN-FILES.                                                 
                                                                        
           OPEN INPUT   EXTRACT ELTRLR
                OUTPUT  PRNTR
           IF ELTRLR-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ELTRLR - ERROR - OPEN ' ELTRLR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0150-SORT-PROCEDURE.                                             
                                                                        
           SORT SORT-FILE  ASCENDING KEY  SORT-KEY1                     
                                          SORT-KEY2                     
                                          SORT-KEY3                     
               INPUT PROCEDURE 0200-READ-EXTRACT  THRU  0299-EXIT       
               OUTPUT PROCEDURE 0300-READ-REINS  THRU  0399-EXIT.       
                                                                        
           IF (SORT-RETURN  NOT = ZEROS)
              AND (NOT NO-RECORDS-SORTED)
               MOVE 'SORT RETURN CODE NON ZERO'                         
                                       TO  WS-ABEND-MESSAGE             
               MOVE '0101'             TO  WS-RETURN-CODE               
               PERFORM ABEND-PGM.                                       
                                                                        
           CLOSE PRNTR.                                                 
                                                                        
       0190-CLOSE-FICH.                                                 
                                   COPY ELCPRTC.                        

           CLOSE ELTRLR

           GOBACK.
                                                                        
       0200-READ-EXTRACT  SECTION.                                      
                                                                        
           READ EXTRACT  INTO  DETAIL-EXTRACT                           
                       AT END CLOSE EXTRACT                             
                              GO TO 0299-EXIT.                          
                                                                        
           IF DE-RECORD-ID  NOT = 'DE'                                  
               GO TO 0200-READ-EXTRACT.                                 
                                                                        
           IF NOT  DE-RESERVE                                           
               GO TO 0200-READ-EXTRACT.                                 
                                                                        
           IF DE-REINCO NOT = '145'
              GO TO 0200-READ-EXTRACT
           END-IF
      
           IF DE-CLMNO = 'IBNRRSV'
              GO TO 0200-READ-EXTRACT
           END-IF

           COPY ELCEXTM1.                                               
                                                                        
           IF DE-RSV-PROC-DT NOT = RUN-DATE
              GO TO 0200-READ-EXTRACT
           END-IF

           IF DTE-PRC-OPT NOT = 1
              GO TO 0200-READ-EXTRACT                                
           ELSE                                                       
              MOVE 'R'                 TO DE-TRANS                     
              MOVE DE-REI-COMP         TO DE-REPORT-CODE-1             
              GO TO 0215-RELEASE-TO-SORT
           END-IF

           IF FIRST-READ                                                
               MOVE '*'                TO  FIRST-READ-SW                
               GO TO 0210-PRINT-DETAIL.                                 
                                                                        
           IF  DE-CARRIER  NOT = CAR-CTL                                
               PERFORM 0600-TOTAL-LEVEL-1  THRU  0630-TOTAL-LEVEL-4     
               MOVE 99                 TO LINE-CNT                      
               GO TO 0210-PRINT-DETAIL.                                 
                                                                        
           IF  DE-STATE  NOT =  ST-CTL                                  
               PERFORM 0600-TOTAL-LEVEL-1  THRU  0610-TOTAL-LEVEL-2     
               GO TO 0210-PRINT-DETAIL.                                 
                                                                        
           IF  DE-ACCOUNT  NOT = ACCT-CTL                               
               PERFORM 0600-TOTAL-LEVEL-1
           END-IF

           .
       0210-PRINT-DETAIL.                                               
                                                                        
           MOVE DE-CARRIER             TO  CAR-CTL.                     
           MOVE DE-GROUPING            TO  CO-CTL.                      
           MOVE DE-STATE               TO  ST-CTL.                      
           MOVE DE-ACCOUNT             TO  ACCT-CTL.                    
                                                                        
           PERFORM 0500-PRINT-DETAIL THRU 0500-EXIT.                    
                                                                        
                                                                        
           IF DTE-TOT-OPT = 2                                           
             IF DE-REPORT-CODE-1 NOT = SPACES                           
                 MOVE 'B'              TO  DE-TRANS                     
                 GO TO 0215-RELEASE-TO-SORT.                            
                                                                        
           GO TO 0200-READ-EXTRACT.                                     
                                                                        
       0215-RELEASE-TO-SORT.                                            
                                                                        
      *****COPY ELCEXTM2.                                               
                                                                        
           RELEASE SORT-RECORD FROM DETAIL-EXTRACT.                     
                                                                        
           MOVE '*'                    TO  SORT-SW.                     
                                                                        
           GO TO 0200-READ-EXTRACT

           .
       0299-EXIT.                                                       
           EXIT.                                                        

       0300-READ-REINS  SECTION.                                        
                                                                        
           IF NO-RECORDS-SORTED                                         
               GO TO 0399-EXIT.                                         
                                                                        
           PERFORM 0400-INITIALIZATION-ROUTINE THRU 0499-EXIT.          
                                                                        
           PERFORM 0310-RETURN-REINS.                                   
                                                                        
           MOVE DE-TRANS               TO  HD1-SUFFIX.                  
           IF DE-TRANS = 'B'                                            
               MOVE CLAS-REPORT-CD1-CAPTION                             
                                       TO  HD4-RPT-REIN-HDG             
                                           TOTAL-ID (4)                 
               MOVE ' TOTAL'           TO  TOTAL-ID-TITLE (4)           
           ELSE                                                         
               MOVE 'REINSURANCE COMPANY '                              
                                       TO  HD4-RPT-REIN-HDG             
               MOVE 'REINSURANCE COMPANY TOTAL'                         
                                       TO  TOTAL-ID (4).                
                                                                        
           GO TO 0390-PRINT-DETAIL-LINE.                                
                                                                        
       0310-RETURN-REINS.                                               
                                                                        
           RETURN SORT-FILE  INTO  DETAIL-EXTRACT                       
                           AT END PERFORM 0600-TOTAL-LEVEL-1  THRU      
                                          0699-TOTAL-EXIT
                                  GO TO 0399-EXIT.                      
                                                                        
           MOVE DE-EFF                     TO  WS-DE-EFF-N.             
           MOVE DE-RSV-INCUR               TO  WS-DE-RSV-INCUR-N.       
                                                                        
                                                                        
       0330-CHECK-CONTROL.                                              
                                                                        
           IF DE-TRANS  NOT = HD1-SUFFIX                                
               PERFORM 0600-TOTAL-LEVEL-1  THRU  0699-TOTAL-EXIT
               PERFORM 0400-INITIALIZATION-ROUTINE THRU 0499-EXIT       
               MOVE DE-TRANS           TO  HD1-SUFFIX                   
               MOVE 'REINSURANCE COMPANY '                              
                                       TO  HD4-RPT-REIN-HDG             
               MOVE 'REINSURANCE COMPANY TOTAL'                         
                                       TO  TOTAL-ID (4)                 
               GO TO 0390-PRINT-DETAIL-LINE.                            
                                                                        
           IF DE-REPORT-CODE-1  NOT = RPT-1-REIN-CTL                    
               PERFORM 0600-TOTAL-LEVEL-1  THRU  0630-TOTAL-LEVEL-4
               MOVE 99                 TO LINE-CNT                      
               GO TO 0390-PRINT-DETAIL-LINE.                            
                                                                        
           IF  DE-CARRIER  NOT = CAR-CTL                                
               PERFORM 0600-TOTAL-LEVEL-1  THRU  0620-TOTAL-LEVEL-3     
               MOVE 99                 TO LINE-CNT                      
               GO TO 0390-PRINT-DETAIL-LINE.                            
                                                                        
           IF  DE-STATE  NOT = ST-CTL                                   
               PERFORM 0600-TOTAL-LEVEL-1  THRU  0610-TOTAL-LEVEL-2     
               GO TO 0390-PRINT-DETAIL-LINE.                            
                                                                        
           IF  DE-ACCOUNT  NOT = ACCT-CTL                               
               PERFORM 0600-TOTAL-LEVEL-1
           END-IF

           .
       0390-PRINT-DETAIL-LINE.                                          
                                                                        
           MOVE DE-REPORT-CODE-1       TO  RPT-1-REIN-CTL.              
           MOVE DE-CARRIER             TO  CAR-CTL.                     
           MOVE DE-GROUPING            TO  CO-CTL.                      
           MOVE DE-STATE               TO  ST-CTL.                      
           MOVE DE-ACCOUNT             TO  ACCT-CTL.                    
                                                                        
           PERFORM 0500-PRINT-DETAIL THRU 0500-EXIT.                    
                                                                        
           GO TO 0310-RETURN-REINS.                                     
                                                                        
       0399-EXIT.                                                       
           EXIT.                                                        
       EJECT                                                            
       PERFORMED-PROCEDURES  SECTION.                                   
                                                                        
       0400-INITIALIZATION-ROUTINE.                                     
                                                                        
           MOVE ZEROS                  TO  PGE-CNT.                     
           MOVE 99                     TO  LINE-CNT.                    
                                                                        
           PERFORM 0450-ZERO-TOTALS THRU 0459-EXIT                      
               VARYING  SUB1  FROM  1  BY  1                            
                 UNTIL  SUB1  > 5.
                                                                        
                                                                       
           GO TO 0499-EXIT.                                             
                                                                        
       0450-ZERO-TOTALS.                                                
                                                                        
           MOVE ZEROS                  TO  LIFE-PAYCUR (SUB1)           
                                           LIFE-FUTURE (SUB1)           
                                           DISB-PAYCUR (SUB1)           
                                           DISB-FUTURE (SUB1).          
                                                                        
           PERFORM 0470-ZERO-YEARS THRU 0479-EXIT                       
               VARYING  SUB3  FROM  1  BY  1                            
                 UNTIL  SUB3  IS GREATER THAN  11.                      
                                                                        
       0459-EXIT.                                                       
           EXIT.                                                        
                                                                        
       0470-ZERO-YEARS.                                                 
                                                                        
           MOVE ZEROS                  TO  PAYCUR-RSRV (SUB1 SUB3)      
                                           FUTURE-RSRV (SUB1 SUB3).     
                                                                        
       0479-EXIT.                                                       
           EXIT.                                                        
                                                                        
       0499-EXIT.                                                       
           EXIT.                                                        
                                                                        
       EJECT                                                            
       0500-PRINT-DETAIL.                                               
                                                                        
      *     12  DETL-CAUSE          PIC  X(25).                     
      *     12  FILLER              PIC  XXX            VALUE SPACES.
      *     12  FILLER              PIC  X              VALUE SPACES.
      *
      * 01  DETL-LINE2.
      *     12  FILLER              PIC  X(56)          VALUE SPACES.
      *     12  DETL-REM-TERM       PIC  999.
      *     12  FILLER              PIC  X              VALUE SPACES.
      *     12  DETL-REM-BENE       PIC  999,999.99.                


           MOVE 1                      TO  SUB1.                        
                                                                        
           MOVE DE-CARRIER             TO  DETL-CAR.                    
           MOVE DE-STATE               TO  DETL-ST.                     
           MOVE DE-ACCOUNT             TO  DETL-ACCT.                   
           MOVE DE-CERT                TO  DETL-CERT.                   
                                                                        
           MOVE DE-EFF                 TO WS-WORK-DATE-N

           STRING WS-WORK-DATE (5:2) '/' WS-WORK-DATE (7:2) '/'
              WS-WORK-DATE (1:4) DELIMITED BY SIZE INTO DETL-EFF
           END-STRING

           MOVE SPACES                 TO DETL-CAUSE

           IF DE-RSV-REM-BENEFIT NOT NUMERIC
              MOVE ZEROS               TO DE-RSV-REM-BENEFIT
           END-IF
           IF DE-RSV-REM-TERM NOT NUMERIC
              MOVE ZEROS               TO DE-RSV-REM-TERM
           END-IF

           MOVE DE-RSV-REM-BENEFIT     TO DETL-REM-BENE
           MOVE DE-RSV-REM-TERM        TO DETL-REM-TERM

           MOVE DE-RSV-INCUR           TO WS-WORK-DATE-N

           STRING WS-WORK-DATE (5:2) '/' WS-WORK-DATE (7:2) '/'
              WS-WORK-DATE (1:4) DELIMITED BY SIZE INTO DETL-INC-DT
           END-STRING

           IF DE-RSV-REPORTED NOT NUMERIC
              MOVE ZEROS               TO DE-RSV-REPORTED
           END-IF

           IF DE-RSV-RPT-YR < 70
              MOVE '20'                TO WS-WORK-DATE (1:2)
           ELSE
              MOVE '19'                TO WS-WORK-DATE (1:2)
           END-IF

           MOVE DE-RSV-REPORTED        TO WS-WORK-DATE (3:8)
           STRING WS-WORK-DATE (5:2) '/' WS-WORK-DATE (7:2) '/'
              WS-WORK-DATE (1:4) DELIMITED BY SIZE INTO DETL-RPT-DT
           END-STRING

           MOVE DE-RSV-PAYTO           TO WS-WORK-DATE-N

           STRING WS-WORK-DATE (5:2) '/' WS-WORK-DATE (7:2) '/'
              WS-WORK-DATE (1:4) DELIMITED BY SIZE INTO DETL-PD-THRU-DT
           END-STRING

           IF DETL-PD-THRU-DT = '00/00/0000'
              MOVE SPACES              TO  DETL-PD-THRU-DT
           END-IF

           MOVE DE-CLMNO               TO  DETL-CLM
                                                                        
           IF DE-LIFE-RSV                                               
              MOVE LIFE-OVERRIDE-L6    TO DETL-TYPE
              MOVE DE-LF-TERM          TO DETL-ORIG-TERM
              MOVE DE-LF-BEN           TO DETL-ORIG-BENE
           ELSE                                                         
              MOVE AH-OVERRIDE-L6      TO DETL-TYPE
              MOVE DE-AH-TERM          TO DETL-ORIG-TERM
              MOVE DE-AH-BEN           TO DETL-ORIG-BENE
           END-IF

           MOVE DE-AGE                 TO DETL-ISS-AGE
      
           MOVE DE-REI-PAYCUR      TO  DETL-PTC
           MOVE DE-REI-FUTRSV      TO  DETL-FUTURE  

           IF DE-LIFE-RSV                        
              ADD DE-REI-PAYCUR     TO LIFE-PAYCUR (SUB1)
              ADD DE-REI-FUTRSV     TO LIFE-FUTURE (SUB1)
           ELSE                                          
              ADD DE-REI-PAYCUR     TO DISB-PAYCUR (SUB1)
              ADD DE-REI-FUTRSV     TO DISB-FUTURE (SUB1)
           END-IF
      
           COMPUTE SUB3 = (RUN-CCYY - DE-RSV-INCUR-CCYY) + 1.           
                                                                        
           IF SUB3 < 01                                         
               MOVE 01                 TO  SUB3.                        
                                                                        
           IF SUB3 > 11                                      
               MOVE 11                 TO  SUB3.                        

           ADD DE-REI-PAYCUR           TO PAYCUR-RSRV (SUB1 SUB3)
           ADD DE-REI-FUTRSV           TO FUTURE-RSRV (SUB1 SUB3)
      
           MOVE DTE-CLASIC-COMPANY-CD  TO AT-CONTROL-PRIMARY
           MOVE DE-CARRIER             TO AT-CARRIER
           MOVE DE-CLMNO               TO AT-CLAIM-NO
           MOVE DE-CERT                TO AT-CERT-NO
           MOVE +90                    TO AT-SEQUENCE-NO
           READ ELTRLR
           IF ELTRLR-FILE-STATUS = '00'
              MOVE AT-INFO-LINE-1      TO DETL-CAUSE
           ELSE
              IF ELTRLR-FILE-STATUS = '10' OR '23'
                 MOVE ' NOT FOUND'     TO DETL-CAUSE
              ELSE
                 DISPLAY ' ELTRLR - ERROR - READ ' ELTRLR-FILE-STATUS
                   ' '  DE-CARRIER ' ' DE-CLMNO ' ' DE-CERT
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE DETL-LINE              TO  P-DATA.                      
           ADD 1                       TO  W-LINES-USED.                
           PERFORM 0900-WRITE-LINE  THRU  0999-EXIT.                    
                                                                        
           MOVE DETL-LINE2             TO  P-DATA.                      
           ADD 1                       TO  W-LINES-USED.                
           PERFORM 0900-WRITE-LINE  THRU  0999-EXIT.                    
                                                                        
           .
       0500-EXIT.                                                       
           EXIT.                                                        
       EJECT                                                            
       0600-TOTAL-LEVEL-1.                                              

           MOVE 1                      TO  SUB1                         
                                           W-LINES-USED.                
           MOVE 2                      TO  SUB2.                        
                                                                        
           PERFORM 0700-PRINT-TOTAL-LINE
                                       THRU 0799-EXIT
      
           PERFORM 0800-ROLL-TOTALS    THRU 0899-EXIT
      
           .
       0610-TOTAL-LEVEL-2.                                              
                                                                        
           ADD 1                       TO  SUB1 SUB2.                   
                                                                        
           PERFORM 0700-PRINT-TOTAL-LINE THRU 0799-EXIT.                
                                                                        
           PERFORM 0800-ROLL-TOTALS THRU 0899-EXIT.                     
                                                                        
       0620-TOTAL-LEVEL-3.                                              
                                                                        
           ADD 1                       TO  SUB1 SUB2.                   
      *    MOVE 99                     TO  LINE-CNT.                    
           MOVE CAR-CTL                TO  TOTAL-ID-CARRIER (SUB1).     
                                                                        
           PERFORM 0700-PRINT-TOTAL-LINE THRU 0799-EXIT.                
                                                                        
           PERFORM 0800-ROLL-TOTALS THRU 0899-EXIT.                     
                                                                        
       0630-TOTAL-LEVEL-4.                                              
                                                                        
           ADD 1                       TO  SUB1 SUB2.                   
      *    MOVE 99                     TO  LINE-CNT.                    
                                                                        
           PERFORM 0700-PRINT-TOTAL-LINE THRU 0799-EXIT.                
                                                                        
           PERFORM 0800-ROLL-TOTALS THRU 0899-EXIT.                     
                                                                        
       0640-TOTAL-LEVEL-5.                                              
                                                                        
           ADD 1                       TO  SUB1 SUB2.                   
           MOVE 99                     TO  LINE-CNT.                    
           MOVE ' OVERALL'             TO  RPT-1-REIN-CTL.              
                                                                        
           PERFORM 0700-PRINT-TOTAL-LINE THRU 0799-EXIT.                
                                                                        
       0699-TOTAL-EXIT.                                                 
           EXIT.                                                        
                                                                        
       EJECT                                                            
       0700-PRINT-TOTAL-LINE.                                           
                                                                        
      *    IF SUB1 = 1  OR  2
               GO TO 0790-PRINT-TOTALS.                                 
                                                                        
           MOVE SPACES                 TO  TOTAL-LINE-P.                
           MOVE '   RESERVE SUMMARY BY INCURRED YEAR'                   
                                       TO  TOTAL-DESC.                  
           MOVE RUN-YEAR               TO  DETL-TYPE.                   
           MOVE 1                      TO  SUB3.                        
           PERFORM 0780-PRINT-YEARS.                                    
                                                                        
           MOVE SPACES                 TO  TOTAL-DESC.                  
           MOVE ' (-1)  '              TO  DETL-TYPE.                   
           MOVE 2                      TO  SUB3.                        
           PERFORM 0780-PRINT-YEARS.                                    
                                                                        
           MOVE ' (-2)  '              TO  DETL-TYPE.                   
           MOVE 3                      TO  SUB3.                        
           PERFORM 0780-PRINT-YEARS.                                    
                                                                        
           MOVE ' (-3)  '              TO  DETL-TYPE.                   
           MOVE 4                      TO  SUB3.                        
           PERFORM 0780-PRINT-YEARS.                                    
                                                                        
           MOVE ' (-4)  '              TO  DETL-TYPE.                   
           MOVE 5                      TO  SUB3.                        
           PERFORM 0780-PRINT-YEARS.                                    
                                                                        
           MOVE ' (-5)  '              TO  DETL-TYPE.                   
           MOVE 6                      TO  SUB3.                        
           PERFORM 0780-PRINT-YEARS.                                    
                                                                        
           MOVE ' (-6)  '              TO  DETL-TYPE.                   
           MOVE 7                      TO  SUB3.                        
           PERFORM 0780-PRINT-YEARS.                                    
                                                                        
           MOVE ' (-7)  '              TO  DETL-TYPE.                   
           MOVE 8                      TO  SUB3.                        
           PERFORM 0780-PRINT-YEARS.                                    
                                                                        
           MOVE ' (-8)  '              TO  DETL-TYPE.                   
           MOVE 9                      TO  SUB3.                        
           PERFORM 0780-PRINT-YEARS.                                    
                                                                        
           MOVE ' (-9)  '              TO  DETL-TYPE.                   
           MOVE 10                     TO  SUB3.                        
           PERFORM 0780-PRINT-YEARS.                                    
                                                                        
           MOVE ' PRIOR '              TO  DETL-TYPE.                   
           MOVE 11                     TO  SUB3.                        
           PERFORM 0780-PRINT-YEARS.                                    
           MOVE 1                      TO  W-LINES-USED.                
                                                                        
           GO TO 0790-PRINT-TOTALS.                                     
                                                                        
                                                                        
       0780-PRINT-YEARS.                                                
      
           MOVE PAYCUR-RSRV (SUB1 SUB3) TO  DETL-PTC.                 
           MOVE FUTURE-RSRV (SUB1 SUB3) TO  DETL-FUTURE
                                                                        
           MOVE DETL-LINE              TO  P-DATA.                      
           ADD 1                       TO  W-LINES-USED.                
           PERFORM 0900-WRITE-LINE  THRU  0999-EXIT.                    
      
           .
       0790-PRINT-TOTALS.                                               
                                                                        
           MOVE SPACES                 TO  TOTAL-LINE-P.                
           MOVE TOTAL-ID (SUB1)        TO  TOTAL-NAME.                  
           MOVE LIFE-OVERRIDE-L6       TO  DETL-TYPE.                   
                                                                        
           MOVE LIFE-PAYCUR (SUB1)     TO  DETL-PTC.                  
           MOVE LIFE-FUTURE (SUB1)     TO  DETL-FUTURE
      
           MOVE DETL-LINE              TO  P-DATA.                      
           ADD 1                       TO  W-LINES-USED.                
           PERFORM 0900-WRITE-LINE  THRU  0999-EXIT.                    
                                                                        
           MOVE SPACES                 TO  TOTAL-LINE-P.                
           MOVE AH-OVERRIDE-L6         TO  DETL-TYPE.                   
      
           MOVE DISB-PAYCUR (SUB1)     TO  DETL-PTC.                  
           MOVE DISB-FUTURE (SUB1)     TO  DETL-FUTURE
                                                                        
           MOVE DETL-LINE              TO  P-DATA.                      
           ADD 1                       TO  W-LINES-USED.                
           PERFORM 0900-WRITE-LINE  THRU  0999-EXIT.                    
                                                                        
           ADD 1                       TO  W-LINES-USED.                
           MOVE SPACES                 TO  TOTAL-LINE-P.                
                                                                        
           IF SUB1  = 5                                                 
               MOVE 1                  TO  FST.                         
                                                                        
       0799-EXIT.                                                       
           EXIT.                                                        
                                                                        
       EJECT                                                            
       0800-ROLL-TOTALS.                                                
                                                                        
           ADD LIFE-PAYCUR (SUB1)      TO  LIFE-PAYCUR (SUB2).          
           ADD LIFE-FUTURE (SUB1)      TO  LIFE-FUTURE (SUB2).          
      
           ADD DISB-PAYCUR (SUB1)      TO  DISB-PAYCUR (SUB2).          
           ADD DISB-FUTURE (SUB1)      TO  DISB-FUTURE (SUB2).          
                                                                        
           PERFORM 0880-ROLL-YEARS THRU 0889-EXIT                       
               VARYING  SUB3  FROM  1  BY  1                            
                 UNTIL  SUB3  IS GREATER THAN  11.                      
                                                                        
           PERFORM 0450-ZERO-TOTALS THRU 0459-EXIT.                     
                                                                        
           GO TO 0899-EXIT.                                             
                                                                        
       0880-ROLL-YEARS.                                                 
                                                                        
           ADD PAYCUR-RSRV (SUB1 SUB3) TO  PAYCUR-RSRV (SUB2 SUB3).     
           ADD FUTURE-RSRV (SUB1 SUB3) TO  FUTURE-RSRV (SUB2 SUB3).     
                                                                        
       0889-EXIT.                                                       
           EXIT.                                                        
                                                                        
       0899-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
       0900-WRITE-LINE.                                                 
                                                                        
           ADD W-LINES-USED            TO  LINE-CNT.                    
                                                                        
           IF  LINE-CNT GREATER THAN PAGE-SIZE                          
               MOVE P-DATA             TO  PRINT-HOLD                   
               PERFORM 1000-PRINT-HEADINGS THRU 1099-EXIT               
               MOVE PRINT-HOLD         TO  P-DATA.                      
                                                                        
           IF  W-LINES-USED = 1                                         
               MOVE SPACE              TO  X                            
           ELSE                                                         
               IF  W-LINES-USED = 2                                     
                   MOVE '0'            TO  X                            
               ELSE                                                     
                   MOVE '-'            TO  X.                           
                                                                        
           PERFORM 1100-PRINT-LINE THRU 1100-EXIT.                      
                                                                        
           MOVE 0                      TO  W-LINES-USED.                
                                                                        
       0999-EXIT.                                                       
           EXIT.                                                        
       EJECT                                                            
       1000-PRINT-HEADINGS.                                             
                                                                        
           MOVE '1'                    TO  X.                           
           ADD 1                       TO  PGE-CNT.                     
           MOVE PGE-CNT                TO  HD-PAGE.                     
           MOVE HEAD-1                 TO  P-DATA.                      
           PERFORM 1100-PRINT-LINE  THRU  1100-EXIT.                    
                                                                        
           MOVE HEAD-2                 TO  P-DATA.                      
           MOVE SPACE                  TO  X.                           
           PERFORM 1100-PRINT-LINE  THRU  1100-EXIT.                    
                                                                        
           MOVE HEAD-3                 TO  P-DATA.                      
           PERFORM 1100-PRINT-LINE  THRU  1100-EXIT.                    
                                                                        
           MOVE ZEROS                  TO  X.                           
                                                                        
           IF  HD1-SUFFIX = 'B'  OR  'R'                                
               MOVE RPT-1-REIN-CTL     TO  HD4-RPT-REIN-CD              
               MOVE HEAD-4             TO  P-DATA                       
               PERFORM 1100-PRINT-LINE  THRU  1100-EXIT.                
                                                                        
           MOVE DETL-HEAD1             TO  P-DATA.                      
           PERFORM 1100-PRINT-LINE  THRU  1100-EXIT.                    
                                                                        
           MOVE SPACE                  TO  X.                           
           MOVE DETL-HEAD2             TO  P-DATA.                      
           PERFORM 1100-PRINT-LINE  THRU  1100-EXIT.                    
                                                                        
           IF  HD1-SUFFIX = 'B'  OR  'R'                                
               MOVE 8                  TO  LINE-CNT                     
           ELSE                                                         
               MOVE 6                  TO  LINE-CNT.                    
                                                                        
           MOVE 2                      TO  W-LINES-USED.                
                                                                        
       1099-EXIT.                                                       
           EXIT.                                                        
                                                                        
       EJECT                                                            
       1100-PRINT-LINE.                                                 
           COPY ELCPRT2.                                                
                                                                        
       1100-EXIT.                                                       
           EXIT.                                                        
                                                                        
       ABEND-PGM SECTION.                                               
                                 COPY ELCABEND.                         
                                                                        
