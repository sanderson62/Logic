000100 IDENTIFICATION DIVISION.                                         
000200 PROGRAM-ID.    CIPA020.                                          
000300 DATE-WRITTEN.  NOV, 1999.                                        
000350                                                                  
000400***************************************************************** 
000500*  N O T E:                                                     * 
000700*  ========                                                     * 
000500*                                                               * 
000500*  THIS PROGRAM PRODUCES REPORTS OF SELECTED ACCOUNTS FROM      * 
000500*  THE ECS021-02 FICH FILE THAT ARE ROUTED TO CID.              * 
000550*                                                               * 
000550*  SOME REPORTS ARE PRODUCED MONTHLY, AND SOME ARE PRODUCED     * 
000550*  SEMI-ANNUALLY.                                               * 
000550*                                                               * 
000500*   REPORT CODE 1:              (PRINT AT MONTH END)            * 
000500*      1) MIDWEST                 "    "    "    "              * 
020204*      2) RMRO-N                  "    "    "    "              * 
020204*      3) RMRO-S                  "    "    "    "              * 
000500*                                 "    "    "    "              * 
000500*   REPORT CODE 2:                "    "    "    "              * 
000500*      1) INTRUST                 "    "    "    "              * 
000500*      2) PRAIRIESTB              "    "    "    "              * 
000500*      3) SUNFLOWER               "    "    "    "              * 
000500*      4) ROTH                    "    "    "    "              * 
000500*      5) TRUCK & RV              "    "    "    "              * 
000550*                                                               * 
000500*   REPORT CODE 1:              (PRINT IN JUNE AND DECEMBER)    * 
000500*      1) IOWA                    "    "    "   "     "         * 
000500*      2) MIDWEST                 "    "    "   "     "         * 
000500*      3) MWAUTO                  "    "    "   "     "         * 
020204*      4) RMRO-N                  "    "    "   "     "         * 
020204*      5) RMRO-S                  "    "    "   "     "         * 
000500*      6) SWRO                    "    "    "   "     "         * 
000500*      7) IARO                    "    "    "   "     "         * 
000500*      8) NCRO                    "    "    "   "     "         * 
000550*                                                               * 
000600***************************************************************** 
000550*                                                               * 
000400***************************************************************** 
000500*  SELECT PRINT LINES FROM THE ECS021 FICH REPORT.              * 
111403******************************************************************
111403*                   C H A N G E   L O G
111403*
111403* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
111403*-----------------------------------------------------------------
111403*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
111403* EFFECTIVE    NUMBER
111403*-----------------------------------------------------------------
111403* 111403    2002081300007  SMVA  DO NOT SHOW SUNFLOWER ACCTS FOR
111403*                                RPTCD1 RMRO ON CODE-2-MO-RPT
020204* 020204    2004013100001  SMVA  SEPARATE RMRO-N AND RMRO-S RPTS
111403******************************************************************
000650*                                                               * 
000700 ENVIRONMENT DIVISION.                                            
000800                                                                  
000900 INPUT-OUTPUT SECTION.                                            
001000                                                                  
001100 FILE-CONTROL.                                                    
001200                                                                  
001300     SELECT ECS021-02-FICHE                                       
001400         ASSIGN TO SYS010.                                        
001500                                                                  
001600     SELECT MIDWEST-MO-RPT                                        
001700         ASSIGN TO SYS020.                                        
001800                                                                  
020204     SELECT RMRO-N-MO-RPT                                           
002000         ASSIGN TO SYS021.                                        
002100                                                                  
020204     SELECT RMRO-S-MO-RPT                                           
020204         ASSIGN TO SYS022.                                        
002100                                                                  
002200     SELECT CODE-2-MO-RPT                                         
002300         ASSIGN TO SYS024.                                        
002400                                                                  
002500     SELECT CODE-1-SA-RPT                                         
002600         ASSIGN TO SYS026.                                        
002700                                                                  
003700     SELECT DISK-DATE                                             
003800         ASSIGN TO SYS019.                                        
003900                                                                  
004000 DATA DIVISION.                                                   
004100                                                                  
004200 FILE SECTION.                                                    
004300                                                                  
004400 FD  DISK-DATE         COPY ELCDTEFD.                             
004500                                                                  
004600 FD  ECS021-02-FICHE                                              
004700     LABEL RECORDS ARE STANDARD                                   
004800     RECORDING MODE IS F                                          
004900     RECORD CONTAINS 133 CHARACTERS                               
005000     BLOCK CONTAINS 0 RECORDS.                                    
005100 01  FICHE-RECORD.                                                
005200     05  CC                       PIC X(001).                     
005300     05  FILLER                   PIC X(132).                     
005400                                                                  
003600 FD  MIDWEST-MO-RPT                                               
003700     LABEL RECORDS ARE STANDARD                                   
003800     RECORDING MODE IS F                                          
003900     RECORD CONTAINS 179 CHARACTERS                               
004000     BLOCK CONTAINS 0 RECORDS.                                    
004100 01  MIDWEST-MO-RECORD            PIC X(179).                     
003500                                                                  
020204 FD  RMRO-N-MO-RPT                                                  
003700     LABEL RECORDS ARE STANDARD                                   
003800     RECORDING MODE IS F                                          
003900     RECORD CONTAINS 179 CHARACTERS                               
004000     BLOCK CONTAINS 0 RECORDS.                                    
020204 01  RMRO-N-MO-RECORD             PIC X(179).                     
003500                                                                  
020204 FD  RMRO-S-MO-RPT                                                  
020204     LABEL RECORDS ARE STANDARD                                   
020204     RECORDING MODE IS F                                          
020204     RECORD CONTAINS 179 CHARACTERS                               
020204     BLOCK CONTAINS 0 RECORDS.                                    
020204 01  RMRO-S-MO-RECORD             PIC X(179).                     
003500                                                                  
003600 FD  CODE-2-MO-RPT                                                
003700     LABEL RECORDS ARE STANDARD                                   
003800     RECORDING MODE IS F                                          
003900     RECORD CONTAINS 179 CHARACTERS                               
004000     BLOCK CONTAINS 0 RECORDS.                                    
004100 01  CODE-2-MO-RECORD             PIC X(179).                     
003500                                                                  
003600 FD  CODE-1-SA-RPT                                                
003700     LABEL RECORDS ARE STANDARD                                   
003800     RECORDING MODE IS F                                          
003900     RECORD CONTAINS 179 CHARACTERS                               
004000     BLOCK CONTAINS 0 RECORDS.                                    
004100 01  CODE-1-SA-RECORD             PIC X(179).                     
004400                                                                  
004800     EJECT                                                        
004900                                                                  
005000 WORKING-STORAGE SECTION.                                         
005100                                                                  
005200 01  WORK.                                                        
005300     05  EOF-SW              PIC X       VALUE 'N'.               
005400         88  EOF                         VALUE 'Y'.               
005500     05  WS-RETURN-CODE      PIC 99      VALUE ZEROS.             
005500     05  WS-ABEND-MESSAGE    PIC X(80)   VALUE SPACES.            
005500     05  PGM-SUB             PIC 999     VALUE ZEROS.             
005500     05  SEL-SW              PIC X       VALUE SPACE.             
005500     05  MIDWEST-MO-SW       PIC X       VALUE SPACE.             
020204     05  RMRO-N-MO-SW        PIC X       VALUE SPACE.             
020204     05  RMRO-S-MO-SW        PIC X       VALUE SPACE.             
005500     05  CODE-2-MO-SW        PIC X       VALUE SPACE.             
005500     05  CODE-1-SA-SW        PIC X       VALUE SPACE.             
005600     05  SYS014-STATUS       PIC X(2)    VALUE SPACE.             
005700     05  S0C7                PIC S9      COMP-3.                  
005800     05  SAVE-ACCOUNT-NAME   PIC X(30)   VALUE SPACE.             
005900                                                                  
006000***************************************************************** 
006100*   DATE CARD INFORMATION                                       * 
006200***************************************************************** 
005900                                                                  
005900     COPY ELCDTECX.                                               
Y2KMOD     COPY ELCDTEVR.                                               
005900                                                                  
006000***************************************************************** 
006100*   COMPANIES TO BE SELECTED FOR REPORTING                      * 
006200***************************************************************** 
005900                                                                  
010200 01  DATE-CARD-LINE.                                              
010300     05  FILLER                 PIC X(21)                         
010300                      VALUE 'DATE CARD MONTH IS = '.              
010300     05  PR-DATE                PIC X(02)    VALUE SPACES.        
010300     05  FILLER                 PIC X(109)   VALUE SPACES.        
010100                                                                  
010200 01  ROUTE-RECORD.                                                
010300     05  R-CC                   PIC X(01)    VALUE SPACES.        
010300     05  R-REC.                                                   
010300         10  FILLER             PIC X(10)    VALUE SPACES.        
010400         10  R-ROUTE-TO         PIC X(10)    VALUE 'ROUTE TO: '.  
010300         10  FILLER             PIC X(05)    VALUE SPACES.        
010500         10  R-INFO             PIC X(60)    VALUE SPACES.        
010600         10  FILLER             PIC X(47)    VALUE SPACES.        
010100                                                                  
010200 01  STARS.                                                       
010300     05  FILLER             PIC X(50)   VALUE                     
010400         '* * * * * * * * * * * * * * * * * * * * * * * * * '.    
010300     05  FILLER             PIC X(50)   VALUE                     
010400         '* * * * * * * * * * * * * * * * * * * * * * * * * '.    
010300     05  FILLER             PIC X(30)   VALUE                     
010400         '* * * * * * * * * * * * * * * '.                        
010100                                                                  
010200 01  WORK-RECORD.                                                 
010300     05  WORK-KEY.                                                
010400         10  WORK-ACCOUNT       PIC X(030)    VALUE SPACE.        
010500         10  WORK-COPY          PIC 9(001)    VALUE ZERO.         
010600         10  WORK-REPORT        PIC X(007)    VALUE SPACE.        
010700         10  WORK-LINE-NO       PIC 9(007)    VALUE ZERO.         
010800     05  WORK-LINE              PIC X(133)    VALUE SPACE.        
010900                                                                  
011000 01  HDG1.                                                        
011100     05  FILLER                 PIC X(14).                        
011100     05  HDG1-RPT-CD            PIC X(01).                        
011100     05  FILLER                 PIC X(01).                        
011200     05  HDG1-ACCT              PIC X(10).                        
011300     05  FILLER                 PIC X(95).                        
011400     05  HDG1-REPORT            PIC X(06).                        
011500     05  FILLER                 PIC X(06).                        
011600                                                                  
012200 01  HDG2.                                                        
011500     05  FILLER                 PIC X(15).                        
011500     05  HDG2-RPT-CD            PIC X(01).                        
012400     05  HDG2-ACCT              PIC X(10).                        
012500     05  FILLER                 PIC X(107).                       
012600                                                                  
011700 01  HDG3.                                                        
011500     05  FILLER                 PIC X(01).                        
011800     05  HDG3-ST-HDG            PIC X(15).                        
011900     05  HDG3-ST                PIC X(02).                        
012000     05  FILLER                 PIC X(115).                       
012100                                                                  
012700 01  WK-DATE.                                                     
012800     10  WK-MO                  PIC X(02)  VALUE SPACES.          
012900     10  FILLER                 PIC X(01)  VALUE SPACES.          
013000     10  WK-DA                  PIC X(02)  VALUE SPACES.          
013100     10  FILLER                 PIC X(01)  VALUE SPACES.          
013200     10  WK-YR                  PIC X(02)  VALUE SPACES.          
013300                                                                  
013500 PROCEDURE DIVISION.                                              
013500                                                                  
013500 0000-DATE-ROUTINE SECTION.                                       
013500                        COPY ELCDTERX.                            
013500                                                                  
013500     MOVE  RUN-MO         TO   WK-MO.                             
013500                                                                  
013500     DISPLAY '  '.                                                
013500     DISPLAY 'RUN-MO = '  RUN-MO.                                 
013500                                                                  
013600     PERFORM INITIALIZATION.                                      
013600                                                                  
013600     MOVE ' ' TO R-CC.                                            
013600     MOVE STARS TO R-REC.                                        
013600     MOVE  ROUTE-RECORD TO WORK-LINE.                          
013600     WRITE MIDWEST-MO-RECORD  FROM WORK-RECORD.               
020204     WRITE RMRO-N-MO-RECORD   FROM WORK-RECORD.               
020204     WRITE RMRO-S-MO-RECORD   FROM WORK-RECORD.               
013600     WRITE CODE-2-MO-RECORD   FROM WORK-RECORD.               
013600     IF RUN-MO = 06 OR 12                                     
013600         WRITE CODE-1-SA-RECORD  FROM WORK-RECORD.             
013600                                                                  
013700     MOVE  ' ' TO R-CC.                                           
0I3700     MOVE  SPACES TO R-REC.                                      
013700     MOVE  ROUTE-RECORD TO WORK-LINE.                           
013700     MOVE  ROUTE-RECORD TO WORK-LINE.                          
013700     WRITE MIDWEST-MO-RECORD  FROM WORK-RECORD.               
020204     WRITE RMRO-N-MO-RECORD   FROM WORK-RECORD.               
020204     WRITE RMRO-S-MO-RECORD   FROM WORK-RECORD.               
013700     WRITE CODE-2-MO-RECORD   FROM WORK-RECORD.               
013700     IF RUN-MO = 06 OR 12                                     
013700         WRITE CODE-1-SA-RECORD  FROM WORK-RECORD.             
013700                                                                  
013800     MOVE  ' ' TO R-CC.                                           
013800     MOVE  'ROUTE TO: ' TO R-ROUTE-TO.                           
013800     MOVE  'C R E D I T   I N S U R A N C E   D I V I S I O N'  
013800                  TO R-INFO.                                      
013800     MOVE  ROUTE-RECORD TO WORK-LINE.                          
013800     WRITE MIDWEST-MO-RECORD  FROM WORK-RECORD.               
020204     WRITE RMRO-N-MO-RECORD   FROM WORK-RECORD.               
020204     WRITE RMRO-S-MO-RECORD   FROM WORK-RECORD.               
013800     WRITE CODE-2-MO-RECORD   FROM WORK-RECORD.               
013800     IF RUN-MO = 06 OR 12                                     
013800         WRITE CODE-1-SA-RECORD  FROM WORK-RECORD.             
013800                                                                  
013900     MOVE  ' ' TO R-CC.                                           
013900     MOVE  SPACES TO R-REC.                                      
013900     MOVE  ROUTE-RECORD TO WORK-LINE.                           
013900     MOVE  ROUTE-RECORD TO WORK-LINE.                          
013900     WRITE MIDWEST-MO-RECORD  FROM WORK-RECORD.               
020204     WRITE RMRO-N-MO-RECORD   FROM WORK-RECORD.               
020204     WRITE RMRO-S-MO-RECORD   FROM WORK-RECORD.               
013900     WRITE CODE-2-MO-RECORD   FROM WORK-RECORD.               
013900     IF RUN-MO = 06 OR 12                                     
013900         WRITE CODE-1-SA-RECORD  FROM WORK-RECORD.             
013900                                                                  
014000     MOVE  ' ' TO R-CC.                                           
014000     MOVE  '    ATTN: ' TO R-ROUTE-TO.                           
014000     MOVE                                                       
014000        'T I M   W O O D'  TO R-INFO.                             
014000     MOVE  ROUTE-RECORD TO WORK-LINE.                          
014000     WRITE MIDWEST-MO-RECORD  FROM WORK-RECORD.               
020204     WRITE RMRO-N-MO-RECORD   FROM WORK-RECORD.               
020204     WRITE RMRO-S-MO-RECORD   FROM WORK-RECORD.               
014000     WRITE CODE-2-MO-RECORD   FROM WORK-RECORD.               
014000     IF RUN-MO = 06 OR 12                                     
014000         WRITE CODE-1-SA-RECORD  FROM WORK-RECORD.             
014000                                                                  
014100     MOVE  ' ' TO R-CC.                                           
014100     MOVE  SPACES TO R-REC.                                      
014100     MOVE  ROUTE-RECORD TO WORK-LINE.                           
014100     WRITE MIDWEST-MO-RECORD  FROM WORK-RECORD.               
020204     WRITE RMRO-N-MO-RECORD   FROM WORK-RECORD.               
020204     WRITE RMRO-S-MO-RECORD   FROM WORK-RECORD.               
014100     WRITE CODE-2-MO-RECORD   FROM WORK-RECORD.               
014100     IF RUN-MO = 06 OR 12                                     
014100         WRITE CODE-1-SA-RECORD  FROM WORK-RECORD.             
014100                                                                  
014200     MOVE  ' ' TO R-CC.                                           
014200      MOVE  SPACES TO R-ROUTE-TO.                                 
014200       MOVE                                                       
014200        'ECS021-02  REPORT CODE 1 - MIDWEST'                      
014200           TO  R-INFO.                                            
014200        MOVE  ROUTE-RECORD TO WORK-LINE.                          
014200         WRITE MIDWEST-MO-RECORD FROM WORK-RECORD.                
014200                                                                  
014210     MOVE  SPACES TO R-ROUTE-TO.                                  
014210       MOVE                                                       
020204        'ECS021-02  REPORT CODE 1 - RMRO-N'                         
014210           TO  R-INFO.                                            
014210        MOVE  ROUTE-RECORD TO WORK-LINE.                          
020204         WRITE RMRO-N-MO-RECORD FROM WORK-RECORD.                   

020204     MOVE  SPACES TO R-ROUTE-TO.                                  
020204       MOVE                                                       
020204        'ECS021-02  REPORT CODE 1 - RMRO-S'                         
020204           TO  R-INFO.                                            
020204        MOVE  ROUTE-RECORD TO WORK-LINE.                          
020204         WRITE RMRO-S-MO-RECORD FROM WORK-RECORD.                   
014210                                                                  
014300     MOVE                                                         
014300       'ECS021-02   REPORT CODE 2 REPORTS'                        
014300           TO  R-INFO.                                            
014300     MOVE  ROUTE-RECORD TO WORK-LINE.                             
014300     WRITE CODE-2-MO-RECORD FROM WORK-RECORD.                     
014300                                                                  
014300                                                                  
014400     IF RUN-MO = 06 OR 12                                         
014400        MOVE                                                      
014400          'ECS021-02   CODE 1 SEMI-ANNUAL REPORTS'                
014400              TO  R-INFO                                          
014400        MOVE  ROUTE-RECORD TO WORK-LINE                           
014400        WRITE CODE-1-SA-RECORD FROM WORK-RECORD                   
014400     END-IF                                                       
014400                                                                  
014500     MOVE  ' ' TO R-CC.                                           
014500     MOVE  SPACES TO R-REC.                                      
014500     MOVE  ROUTE-RECORD TO WORK-LINE.                           
014500     WRITE MIDWEST-MO-RECORD  FROM WORK-RECORD.               
020204     WRITE RMRO-N-MO-RECORD   FROM WORK-RECORD.               
020204     WRITE RMRO-S-MO-RECORD   FROM WORK-RECORD.               
014500     WRITE CODE-2-MO-RECORD   FROM WORK-RECORD.               
014500     IF RUN-MO = 06 OR 12                                     
014500         WRITE CODE-1-SA-RECORD  FROM WORK-RECORD.             
014500                                                                  
014600     MOVE  ' ' TO R-CC.                                           
014600     MOVE  SPACES       TO R-ROUTE-TO.                           
014600     MOVE  '"ECS021-02" OUTPUT FROM PROGRAM "CIPA020"'          
014600                                             TO R-INFO.           
014600     MOVE  ROUTE-RECORD TO WORK-LINE.                          
014600     WRITE MIDWEST-MO-RECORD  FROM WORK-RECORD.               
020204     WRITE RMRO-N-MO-RECORD   FROM WORK-RECORD.               
020204     WRITE RMRO-S-MO-RECORD   FROM WORK-RECORD.               
014600     WRITE CODE-2-MO-RECORD   FROM WORK-RECORD.               
014600     IF RUN-MO = 06 OR 12                                     
014600         WRITE CODE-1-SA-RECORD  FROM WORK-RECORD.             
014600                                                                  
014700     MOVE  ' ' TO R-CC.                                           
014700     MOVE  SPACES TO R-REC.                                      
014700     MOVE  ROUTE-RECORD TO WORK-LINE.                           
014700     WRITE MIDWEST-MO-RECORD  FROM WORK-RECORD.               
020204     WRITE RMRO-N-MO-RECORD   FROM WORK-RECORD.               
020204     WRITE RMRO-S-MO-RECORD   FROM WORK-RECORD.               
014700     WRITE CODE-2-MO-RECORD   FROM WORK-RECORD.               
014700     IF RUN-MO = 06 OR 12                                     
014700         WRITE CODE-1-SA-RECORD  FROM WORK-RECORD.             
014700                                                                  
014800     MOVE  ' ' TO R-CC.                                           
014800      MOVE  STARS  TO R-REC.                                      
014800       MOVE  ROUTE-RECORD TO WORK-LINE.                           
014800         WRITE MIDWEST-MO-RECORD  FROM WORK-RECORD.               
020204         WRITE RMRO-N-MO-RECORD   FROM WORK-RECORD.               
020204         WRITE RMRO-S-MO-RECORD   FROM WORK-RECORD.               
014800         WRITE CODE-2-MO-RECORD   FROM WORK-RECORD.               
014800         IF RUN-MO = 06 OR 12                                     
014800            WRITE CODE-1-SA-RECORD  FROM WORK-RECORD.             
014800                                                                  
014900     MOVE  '1' TO R-CC.                                           
014900      MOVE  SPACES TO R-REC.                                      
014900       MOVE  ROUTE-RECORD TO WORK-LINE.                           
014900         WRITE MIDWEST-MO-RECORD  FROM WORK-RECORD.               
020204         WRITE RMRO-N-MO-RECORD   FROM WORK-RECORD.               
020204         WRITE RMRO-S-MO-RECORD   FROM WORK-RECORD.               
014900         WRITE CODE-2-MO-RECORD   FROM WORK-RECORD.               
014900         IF RUN-MO = 06 OR 12                                     
014900            WRITE CODE-1-SA-RECORD  FROM WORK-RECORD.             
014900                                                                  
015000 ROUTE-TO-DONE.                                                   
015000                                                                  
015000     PERFORM PROCESS-FILE UNTIL EOF.                              
015000                                                                  
015000     PERFORM END-OF-JOB.                                          
015000                                                                  
015000     STOP RUN.                                                    
015000                                                                  
015100 PROCESS-FILE SECTION.                                            
015100                                                                  
015100     READ ECS021-02-FICHE                                         
015100         AT END MOVE 'Y' TO EOF-SW.                               
015100     IF EOF                                                       
015100         GO TO PROCESS-FILE-X.                                    
015100     IF CC = '1'                                                  
015100         PERFORM CHECK-REPORT.                                    
015200     IF SEL-SW = 'Y'                                              
015300         MOVE FICHE-RECORD TO WORK-LINE                           
015400         PERFORM WRITE-REPORT.                                    
015400                                                                  
015500 PROCESS-FILE-X.                                                  
015600     EXIT.                                                        
015700                                                                  
015900 CHECK-REPORT SECTION.                                            
015800                                                                  
016000****************************************************************  
016100*  THIS ROUTINE CHECKS THE HEADING LINES ON EACH PAGE. IT SETS *  
016200*  THE SWITCHES TO 'Y' IF HEADINGS ARE TO BE PRINTED.          *  
016300*  THEY ARE SET TO "N" IF THE PAGE IS NOT TO BE PRINTED.       *  
016400****************************************************************  
016400                                                                  
016400*  NOTE - INITIALIZE THE FOLLOWING 5 SWITCHES TO "NO".            
016400                                                                  
016400     MOVE 'N' TO MIDWEST-MO-SW.                                   
020204     MOVE 'N' TO RMRO-N-MO-SW.                                      
020204     MOVE 'N' TO RMRO-S-MO-SW.                                      
016400     MOVE 'N' TO CODE-2-MO-SW.                                    
016400     MOVE 'N' TO CODE-1-SA-SW.                                    
016500     MOVE 'N' TO SEL-SW.                                          
016600     MOVE FICHE-RECORD  TO  HDG1.                                 
016900     READ ECS021-02-FICHE INTO HDG2.                              
017000     READ ECS021-02-FICHE INTO HDG3.                              
017600     MOVE HDG1-ACCT TO SAVE-ACCOUNT-NAME.                         
017800     PERFORM CHECK-ACCOUNT.                                       
018100     IF SEL-SW = 'Y'                                              
018200         PERFORM WRITE-HEADINGS.                                  
015800                                                                  
018300 CHECK-REPORT-X.                                                  
018400     EXIT.                                                        
018500                                                                  
018700 CHECK-ACCOUNT SECTION.                                           
018800                                                                  
018800*  CODE 1 MONTHLY                                                 
018800                                                                  
018900     IF HDG1-ACCT = 'MIDWEST   '                                  
019200             MOVE 'Y' TO SEL-SW                                   
019200             MOVE 'Y' TO MIDWEST-MO-SW.                           
019400                                                                  
020204     IF HDG1-ACCT = 'RMRO-N    '                                  
019200             MOVE 'Y' TO SEL-SW                                   
020204             MOVE 'Y' TO RMRO-N-MO-SW.                              
019400                                                                  
020204     IF HDG1-ACCT = 'RMRO-S    '                                  
019200             MOVE 'Y' TO SEL-SW                                   
020204             MOVE 'Y' TO RMRO-S-MO-SW.                              
019400                                                                  
018800*  CODE 2 MONTHLY                                                 
018800                                                                  
pemmod     IF HDG2-ACCT = 'INTRUST   ' OR 'PRAIRIESTB'
111403                 OR 'TRUCK & RV' OR 'ROY T & RV'  
019200             MOVE 'Y' TO SEL-SW                                   
019200             MOVE 'Y' TO CODE-2-MO-SW.                            
111403
111403     IF HDG2-ACCT = 'SUNFLOWER '
111403         AND HDG1-ACCT NOT = 'RMRO      '
111403             MOVE 'Y' TO SEL-SW                                   
111403             MOVE 'Y' TO CODE-2-MO-SW.                            
019400                                                                  
019500     IF RUN-MO NOT = (6 AND 12)                                   
019600         GO TO CHECK-ACCT-X.                                      
019700                                                                  
018800*  CODE 1 SEMI-ANNUAL                                             
018800                                                                  
018900     IF HDG1-ACCT = 'IOWA      ' OR 'MIDWEST   ' OR 'MWAUTO    '  
018900                 OR 'RMRO      ' OR 'SWRO      '                  
019000                 OR 'IARO      ' OR 'NCRO      '                  
019200             MOVE 'Y' TO SEL-SW                                   
019300             MOVE 'Y' TO CODE-1-SA-SW.                            
019400                                                                  
020700 CHECK-ACCT-X.                                                    
020800     EXIT.                                                        
020900                                                                  
022400 WRITE-REPORT SECTION.                                            
022300                                                                  
022500     ADD 1 TO WORK-LINE-NO.                                       
022300                                                                  
DAN        IF HDG2-ACCT = 'TRUCK & RV'                                  
DAN           MOVE 3 TO WORK-COPY                                       
DAN        ELSE                                                         
DAN           IF HDG2-ACCT = 'ROY T & RV'                               
DAN              MOVE 2 TO WORK-COPY                                    
DAN           ELSE                                                      
022700           MOVE 1 TO WORK-COPY                                    
DAN           END-IF                                                    
DAN        END-IF                                                       
022900                                                                  
022900*  CODE 1 MONTHLY                                                 
022900                                                                  
023000     IF MIDWEST-MO-SW = 'Y'                                       
023000         WRITE MIDWEST-MO-RECORD FROM WORK-RECORD                 
023400     END-IF.                                                      
022900                                                                  
020204     IF RMRO-N-MO-SW = 'Y'                                          
020204         WRITE RMRO-N-MO-RECORD FROM WORK-RECORD                    
023400     END-IF.                                                      
022900                                                                  
020204     IF RMRO-S-MO-SW = 'Y'                                          
020204         WRITE RMRO-S-MO-RECORD FROM WORK-RECORD                    
020204     END-IF.                                                      
022900                                                                  
022900*  CODE 2 MONTHLY                                                 
022900                                                                  
023000     IF CODE-2-MO-SW = 'Y'                                        
023000         WRITE CODE-2-MO-RECORD FROM WORK-RECORD                  
023400     END-IF.                                                      
022900                                                                  
022900*  CODE 1 SEMI-ANNUAL                                             
022900                                                                  
023000     IF CODE-1-SA-SW = 'Y'                                        
023000         WRITE CODE-1-SA-RECORD FROM WORK-RECORD                  
023400     END-IF.                                                      
023400                                                                  
023400 WRITE-REPORT-X.                                                  
023500     EXIT.                                                        
023600                                                                  
023800 WRITE-HEADINGS SECTION.                                          
023600                                                                  
023900     MOVE SAVE-ACCOUNT-NAME TO WORK-ACCOUNT.                      
024000     MOVE HDG1-REPORT       TO WORK-REPORT.                       
024100     MOVE HDG1        TO WORK-LINE.                               
024200     PERFORM WRITE-REPORT.                                        
024300     MOVE HDG2        TO WORK-LINE.                               
024400     PERFORM WRITE-REPORT.                                        
024500**   MOVE HDG3        TO WORK-LINE.  (WRITTEN IN PROCESS-FILE)    
024600**   PERFORM WRITE-REPORT.                                        
025300                                                                  
025400 WRITE-HEADINGS-X.                                                
025500     EXIT.                                                        
025600                                                                  
025800 INITIALIZATION SECTION.                                          
025700                                                                  
025700                                                                  
026000     OPEN INPUT  ECS021-02-FICHE.                                 
025700                                                                  
026100     OPEN OUTPUT MIDWEST-MO-RPT.                                  
020204     OPEN OUTPUT RMRO-N-MO-RPT.                                     
020204     OPEN OUTPUT RMRO-S-MO-RPT.                                     
026100     OPEN OUTPUT CODE-2-MO-RPT.                                   
026100     IF RUN-MO = 06 OR 12                                         
026100        OPEN OUTPUT CODE-1-SA-RPT                                 
026100     END-IF.                                                      
026100                                                                  
026200 INITIALIZATION-X.                                                
026300     EXIT.                                                        
026500                                                                  
028900                                                                  
029100 END-OF-JOB SECTION.                                              
029000                                                                  
029200     CLOSE ECS021-02-FICHE                                        
029300           MIDWEST-MO-RPT                                         
020204           RMRO-N-MO-RPT                                            
020204           RMRO-S-MO-RPT                                            
029300           CODE-2-MO-RPT                                          
029000                                                                  
029500     IF RUN-MO = 06 OR 12                                         
029500        CLOSE CODE-1-SA-RPT                                       
029500     END-IF.                                                      
029500                                                                  
029500 END-OF-JOB-X.                                                    
029500     EXIT.                                                        
029600                                                                  
029800 ABEND-PGM SECTION.                                               
029700                                                                  
029900     DISPLAY                                                      
029900     'PROGRAM CIPA020 INTENTIONALLY ABENDED WITH A S0C7.'.        
030000     DISPLAY 'SEE ERROR MESSAGE ABOVE.'.                          
030100     MOVE +16 TO RETURN-CODE.                                     
030200     ADD +1 TO S0C7.                                              
030300                                                                  
030400 ABEND-X.                                                         
030500     EXIT.                                                        
030600                                                                  
