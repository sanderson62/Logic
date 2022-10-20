000100 IDENTIFICATION DIVISION.                                         
000200 PROGRAM-ID.    CIPA010.                                          
000300 DATE-WRITTEN.  NOV, 1999.                                        
000350                                                                  
000400***************************************************************** 
000500*  N O T E:                                     
000700*  ========                                      
000500*  THIS PROGRAM PRODUCES REPORTS FOR SELECTED ACCOUNTS FROM 
000500*  THE ECS021 FICH FILE.                          
000550*                                                          
000550*  MOST REPORTS ARE PRODUCED MONTHLY, SOME ARE PRODUCED   
000550*  SEMI-ANNUALLY.                                        
020504******************************************************************
020504*                   C H A N G E   L O G
020504*
020504* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
020504*-----------------------------------------------------------------
020504*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
020504* EFFECTIVE    NUMBER
020504*-----------------------------------------------------------------
020504* 020504    2004020200016  SMVA  REMOVE OLD CODE, CHG RMRO RPT CODE TO
020504*                                -N AND -S, REMOVE SERO & ADD SAND 
033004* 033004    2003081500005  PEMA  ADD CO AND IA TO
033004*                                   MWAUTO - NE FILE EXTRACT
080504* 080504    2004080400006  SMVA  ADD NCRO REPORT        
020504******************************************************************
000650*                                                            
000700 ENVIRONMENT DIVISION.                                            
000800                                                                  
000900 INPUT-OUTPUT SECTION.                                            
001000                                                                  
001100 FILE-CONTROL.                                                    
001200                                                                  
001300     SELECT ECS021-FICHE                                          
001400         ASSIGN TO SYS010.                                        
001500                                                                  
001600     SELECT TOTAL-REPORT                                          
001700         ASSIGN TO SYS020.                                        
001800                                                                  
002500     SELECT MWAUTO-SD-REPORT                                      
002600         ASSIGN TO SYS023.                                        
002700                                                                  
002800     SELECT MWAUTO-NE-REPORT                                      
002900         ASSIGN TO SYS024.                                        
DAN                                                                     
080504     SELECT NCRO-REPORT                                      
080504         ASSIGN TO SYS025.                                        
080504                                                                  
003700     SELECT DISK-DATE                                             
003800         ASSIGN TO SYS019.                                        
003900                                                                  
004000 DATA DIVISION.                                                   
004100                                                                  
004200 FILE SECTION.                                                    
004300                                                                  
004400 FD  DISK-DATE         COPY ELCDTEFD.                             
004500                                                                  
004600 FD  ECS021-FICHE                                                 
004700     LABEL RECORDS ARE STANDARD                                   
004800     RECORDING MODE IS F                                          
004900     RECORD CONTAINS 133 CHARACTERS                               
005000     BLOCK CONTAINS 0 RECORDS.                                    
005100 01  FICHE-RECORD.                                                
005200     05  CC                  PIC X(001).                          
005300     05  FILLER              PIC X(132).                          
005400                                                                  
003600 FD  TOTAL-REPORT                                                 
003700     LABEL RECORDS ARE STANDARD                                   
003800     RECORDING MODE IS F                                          
003900     RECORD CONTAINS 179 CHARACTERS                               
004000     BLOCK CONTAINS 0 RECORDS.                                    
004100 01  TOTAL-RPT-RECORD             PIC X(179).                     
003500                                                                  
003600 FD  MWAUTO-SD-REPORT                                             
003700     LABEL RECORDS ARE STANDARD                                   
003800     RECORDING MODE IS F                                          
003900     RECORD CONTAINS 179 CHARACTERS                               
004000     BLOCK CONTAINS 0 RECORDS.                                    
004100 01  MWAUTO-SD-RECORD            PIC X(179).                      
003500                                                                  
003600 FD  MWAUTO-NE-REPORT                                             
003700     LABEL RECORDS ARE STANDARD                                   
003800     RECORDING MODE IS F                                          
003900     RECORD CONTAINS 179 CHARACTERS                               
004000     BLOCK CONTAINS 0 RECORDS.                                    
004100 01  MWAUTO-NE-RECORD             PIC X(179).                     
DAN                                                                     
080504 FD  NCRO-REPORT                                             
080504     LABEL RECORDS ARE STANDARD                                   
080504     RECORDING MODE IS F                                          
080504     RECORD CONTAINS 179 CHARACTERS                               
080504     BLOCK CONTAINS 0 RECORDS.                                    
080504 01  NCRO-RECORD                  PIC X(179).                     
DAN                                                                     
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
005500     05  TOTAL-RPT-SW        PIC X       VALUE SPACE.             
005500     05  MWAUTO-SD-SW        PIC X       VALUE SPACE.             
005500     05  MWAUTO-NE-SW        PIC X       VALUE SPACE.             
080504     05  NCRO-SW             PIC X       VALUE SPACE.             
005600     05  SYS014-STATUS       PIC X(2)    VALUE SPACE.             
005700     05  S0C7                PIC S9      COMP-3.                  
005800     05  SAVE-ACCOUNT-NAME   PIC X(30)   VALUE SPACE.             
005900                                                                  
005900 01  WS-ACCT                 PIC X(10)   VALUE SPACES.            
005900     88  SD-DO-NOT-PRT   VALUE '0000013300' '0000013330'          
005900                               '0000013430' '0000013450'          
005900                               '0000013570' '0000013580'          
005900                               '0000013600' '0000013610'          
005900                               '0000013620' '0000013690'          
005900                               '0000013700' '0000013720'          
005900                               '0000013780' '0000013840'          
005900                               '0000013850' '0000013920'          
005900                               '0000013960' '0000015510'          
005900                               '0000015530' '0000015720'          
005900                               '0000015740' '0000018010'          
005900                               '0000018050' '0000018080'          
005900                               '0000018110' '0000018170'          
005900                               '0000018200' '0000018210'.         
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
011100     05  FILLER                 PIC X(16).                        
011200     05  HDG1-ACCT              PIC X(10).                        
011300     05  FILLER                 PIC X(95).                        
011400     05  HDG1-REPORT            PIC X(06).                        
011500     05  FILLER                 PIC X(06).                        
011600                                                                  
011700 01  HDG2.                                                        
011750     05  FILLER                 PIC X(01).                        
011800     05  HDG2-ST-HDG            PIC X(15).                        
011900     05  HDG2-ST                PIC X(02).                        
012000     05  FILLER                 PIC X(115).                       
012100                                                                  
012200 01  HDG3.                                                        
012250     05  FILLER                 PIC X(01).                        
012300     05  HDG3-ACCT-HDG          PIC X(15).                        
012400     05  HDG3-ACCT              PIC X(10).                        
012500     05  FILLER                 PIC X(107).                       
012600                                                                  
012700 01  WK-DATE.                                                     
012800     05  WK-MO                  PIC X(02)  VALUE SPACES.          
012900     05  FILLER                 PIC X(01)  VALUE SPACES.          
013000     05  WK-DA                  PIC X(02)  VALUE SPACES.          
013100     05  FILLER                 PIC X(01)  VALUE SPACES.          
013200     05  WK-YR                  PIC X(02)  VALUE SPACES.          
013500                                                                  
013800 PROCEDURE DIVISION.                                              
013500                                                                  
013500 0000-DATE-ROUTINE SECTION.                                       
013500                        COPY ELCDTERX.                            
013500                                                                  
TSTMOD     MOVE  RUN-MO         TO   WK-MO.                             
013500                                                                  
TSTMOD     DISPLAY '  '.                                                
TSTMOD     DISPLAY 'RUN-MO = '  RUN-MO.                                 
013500                                                                  
013900     PERFORM INITIALIZATION.                                      
013500                                                                  
TSTMOD     MOVE ' ' TO R-CC.                                            
TSTMOD     MOVE STARS TO R-REC.                                        
TSTMOD     MOVE  ROUTE-RECORD TO WORK-LINE.                          
TSTMOD     WRITE TOTAL-RPT-RECORD  FROM WORK-RECORD.                
TSTMOD     WRITE MWAUTO-SD-RECORD  FROM WORK-RECORD.                
080504     WRITE NCRO-RECORD       FROM WORK-RECORD.                
TSTMOD     IF RUN-MO = 06 OR 12                                     
TSTMOD         WRITE MWAUTO-NE-RECORD  FROM WORK-RECORD.             
TSTMOD                                                                  
CIDMOD     MOVE  ' ' TO R-CC.                                           
CIDMOD     MOVE  SPACES TO R-REC.                                      
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                           
TSTMOD     WRITE TOTAL-RPT-RECORD  FROM WORK-RECORD.                
TSTMOD     WRITE MWAUTO-SD-RECORD  FROM WORK-RECORD.                
080504     WRITE NCRO-RECORD       FROM WORK-RECORD.                
TSTMOD     IF RUN-MO = 06 OR 12                                     
TSTMOD         WRITE MWAUTO-NE-RECORD  FROM WORK-RECORD.             
TSTMOD                                                                  
CIDMOD     MOVE  ' ' TO R-CC.                                           
CIDMOD     MOVE  'ROUTE TO: ' TO R-ROUTE-TO.                           
CIDMOD     MOVE  'C R E D I T   I N S U R A N C E   D I V I S I O N'  
CIDMOD                  TO R-INFO.                                      
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                          
TSTMOD     WRITE TOTAL-RPT-RECORD  FROM WORK-RECORD.                
TSTMOD     WRITE MWAUTO-SD-RECORD  FROM WORK-RECORD.                
080504     WRITE NCRO-RECORD       FROM WORK-RECORD.                
TSTMOD     IF RUN-MO = 06 OR 12                                     
TSTMOD         WRITE MWAUTO-NE-RECORD  FROM WORK-RECORD.             
TSTMOD                                                                  
CIDMOD     MOVE  ' ' TO R-CC.                                           
CIDMOD     MOVE  SPACES TO R-REC.                                      
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                           
TSTMOD     WRITE TOTAL-RPT-RECORD  FROM WORK-RECORD.                
TSTMOD     WRITE MWAUTO-SD-RECORD  FROM WORK-RECORD.                
080504     WRITE NCRO-RECORD       FROM WORK-RECORD.                
TSTMOD     IF RUN-MO = 06 OR 12                                     
TSTMOD         WRITE MWAUTO-NE-RECORD  FROM WORK-RECORD.             
TSTMOD                                                                  
CIDMOD     MOVE  ' ' TO R-CC.                                           
CIDMOD     MOVE  '    ATTN: ' TO R-ROUTE-TO.                           
CIDMOD     MOVE                                                       
CIDMOD        'T I M   W O O D'  TO R-INFO.                             
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                          
TSTMOD     WRITE TOTAL-RPT-RECORD  FROM WORK-RECORD.                
TSTMOD     WRITE MWAUTO-SD-RECORD  FROM WORK-RECORD.                
TSTMOD     IF RUN-MO = 06 OR 12                                     
TSTMOD         WRITE MWAUTO-NE-RECORD  FROM WORK-RECORD.             
TSTMOD                                                                  
CIDMOD     MOVE  ' ' TO R-CC.                                           
CIDMOD     MOVE  SPACES TO R-REC.                                      
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                           
TSTMOD     WRITE TOTAL-RPT-RECORD  FROM WORK-RECORD.                
TSTMOD     WRITE MWAUTO-SD-RECORD  FROM WORK-RECORD.                
TSTMOD     IF RUN-MO = 06 OR 12                                     
TSTMOD         WRITE MWAUTO-NE-RECORD  FROM WORK-RECORD.             
TSTMOD                                                                  
CIDMOD     MOVE  ' ' TO R-CC.                                           
CIDMOD     MOVE  SPACES TO R-ROUTE-TO.                                 
CIDMOD     MOVE                                                       
CIDMOD        'R E P O R T   C O D E   1   T O T A L   P A G E S'       
CIDMOD           TO  R-INFO.                                            
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                          
022800     WRITE TOTAL-RPT-RECORD FROM WORK-RECORD.                 
TSTMOD                                                                  
CIDMOD     MOVE                                                         
CIDMOD       'R E P O R T   C O D E   1  -  M W A U T O   SD'           
CIDMOD           TO  R-INFO.                                            
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                             
022800     WRITE MWAUTO-SD-RECORD FROM WORK-RECORD.                     
TSTMOD                                                                  
080504     MOVE                                                         
080504       'R E P O R T   C O D E   1  -  N C R O         '           
080504           TO  R-INFO.                                            
080504     MOVE  ROUTE-RECORD TO WORK-LINE.                             
080504     WRITE NCRO-RECORD      FROM WORK-RECORD.                     
TSTMOD                                                                  
TSTMOD     IF RUN-MO = 06 OR 12                                         
CIDMOD        MOVE                                                      
CIDMOD          'R E P O R T   C O D E   1  -  M W A U T O   NE'        
CIDMOD              TO  R-INFO                                          
015300        MOVE  ROUTE-RECORD TO WORK-LINE                           
022800        WRITE MWAUTO-NE-RECORD FROM WORK-RECORD                   
TSTMOD     END-IF                                                       
TSTMOD                                                                  
CIDMOD     MOVE  ' ' TO R-CC.                                           
CIDMOD     MOVE  SPACES TO R-REC.                                      
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                           
TSTMOD     WRITE TOTAL-RPT-RECORD  FROM WORK-RECORD.                
TSTMOD     WRITE MWAUTO-SD-RECORD  FROM WORK-RECORD.                
080504     WRITE NCRO-RECORD       FROM WORK-RECORD.                
TSTMOD     IF RUN-MO = 06 OR 12                                     
TSTMOD         WRITE MWAUTO-NE-RECORD  FROM WORK-RECORD.             
CIDMOD                                                                  
CIDMOD     MOVE  ' ' TO R-CC.                                           
CIDMOD     MOVE  SPACES       TO R-ROUTE-TO.                           
CIDMOD     MOVE  '"ECS021" OUTPUT FROM PROGRAM "CIPA010"' TO R-INFO.  
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                          
TSTMOD     WRITE TOTAL-RPT-RECORD  FROM WORK-RECORD.                
TSTMOD     WRITE MWAUTO-SD-RECORD  FROM WORK-RECORD.                
080504     WRITE NCRO-RECORD       FROM WORK-RECORD.                
TSTMOD     IF RUN-MO = 06 OR 12                                     
TSTMOD         WRITE MWAUTO-NE-RECORD  FROM WORK-RECORD.             
CIDMOD                                                                  
CIDMOD     MOVE  ' ' TO R-CC.                                           
CIDMOD     MOVE  SPACES TO R-REC.                                      
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                           
TSTMOD     WRITE TOTAL-RPT-RECORD  FROM WORK-RECORD.                
TSTMOD     WRITE MWAUTO-SD-RECORD  FROM WORK-RECORD.                
080504     WRITE NCRO-RECORD       FROM WORK-RECORD.                
TSTMOD     IF RUN-MO = 06 OR 12                                     
TSTMOD         WRITE MWAUTO-NE-RECORD  FROM WORK-RECORD.             
CIDMOD                                                                  
CIDMOD     MOVE  ' ' TO R-CC.                                           
CIDMOD     MOVE  STARS  TO R-REC.                                      
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                           
TSTMOD     WRITE TOTAL-RPT-RECORD  FROM WORK-RECORD.                
TSTMOD     WRITE MWAUTO-SD-RECORD  FROM WORK-RECORD.                
080504     WRITE NCRO-RECORD       FROM WORK-RECORD.                
TSTMOD     IF RUN-MO = 06 OR 12                                     
TSTMOD         WRITE MWAUTO-NE-RECORD  FROM WORK-RECORD.             
CIDMOD                                                                  
CIDMOD     MOVE  '1' TO R-CC.                                           
CIDMOD     MOVE  SPACES TO R-REC.                                      
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                           
TSTMOD     WRITE TOTAL-RPT-RECORD  FROM WORK-RECORD.                
TSTMOD     WRITE MWAUTO-SD-RECORD  FROM WORK-RECORD.                
080504     WRITE NCRO-RECORD       FROM WORK-RECORD.                
TSTMOD     IF RUN-MO = 06 OR 12                                     
TSTMOD         WRITE MWAUTO-NE-RECORD  FROM WORK-RECORD.             
TSTMOD                                                                  
014000 ROUTE-TO-DONE.                                                   
TSTMOD                                                                  
014000     PERFORM PROCESS-FILE UNTIL EOF.                              
013500                                                                  
014100     PERFORM END-OF-JOB.                                          
013500                                                                  
014200     STOP RUN.                                                    
014300                                                                  
014500 PROCESS-FILE SECTION.                                            
014400                                                                  
014600     READ ECS021-FICHE                                            
014700         AT END MOVE 'Y' TO EOF-SW.                               
014800     IF EOF                                                       
014900         GO TO PROCESS-FILE-X.                                    
015000     IF CC = '1'                                                  
015100         PERFORM CHECK-REPORT.                                    
015200     IF SEL-SW = 'Y'                                              
015300         MOVE FICHE-RECORD TO WORK-LINE                           
015400         PERFORM WRITE-REPORT.                                    
014400                                                                  
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
015800                                                                  
TSTMOD*  NOTE - INITIALIZE THE FOLLOWING SWITCHES TO "NO".              
015800                                                                  
TSTMOD     MOVE 'N' TO TOTAL-RPT-SW.                                    
TSTMOD     MOVE 'N' TO MWAUTO-SD-SW.                                    
TSTMOD     MOVE 'N' TO MWAUTO-NE-SW.                                    
080504     MOVE 'N' TO NCRO-SW.                                    
016500     MOVE 'N' TO SEL-SW.                                          
016600     MOVE FICHE-RECORD  TO  HDG1.                                 
016900     READ ECS021-FICHE INTO HDG2.                                 
017000     READ ECS021-FICHE INTO HDG3.                                 
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
018800*  TOTAL PAGES                                                    
018800                                                                  
020504     IF (HDG1-ACCT = 'RMRO-N    ' OR 'RMRO-S    ' OR 'MWAUTO    ' 
018900                  OR 'ROTH      ' OR 'SWRO      ' OR 'VANDE KAMP' 
DAN                     OR 'IARO      ' OR 'NCRO      ' OR 'AM EQ     '
070980                  OR 'MIDWEST   ' OR 'SAND      ' OR 'SMRO      '
070908                  OR 'WRO       ')
019100         AND HDG2-ST-HDG = SPACES                                 
019100         AND HDG3-ACCT-HDG = SPACES
019200             MOVE 'Y' TO SEL-SW                                   
019200             MOVE 'Y' TO TOTAL-RPT-SW.                            
018800                                                                  
018800     MOVE HDG3-ACCT TO WS-ACCT.                                   

018800*  MWAUTO SD                                                      
018800                                                                  
018900     IF HDG1-ACCT = 'MWAUTO    '                                  
019100         AND HDG2-ST = 'SD'                                       
019100         AND NOT SD-DO-NOT-PRT                                    
019200             MOVE 'Y' TO SEL-SW                                   
019200             MOVE 'Y' TO MWAUTO-SD-SW                             
019300             GO TO CHECK-ACCT-X.                                  
019400                                                                  
018900     IF HDG1-ACCT = 'MWAUTO    '                                  
019100         AND HDG2-ST = 'SD'                                       
019100         AND HDG3-ACCT-HDG = SPACES
019200             MOVE 'Y' TO SEL-SW                                   
019200             MOVE 'Y' TO MWAUTO-SD-SW                             
019300             GO TO CHECK-ACCT-X.                                  
019400                                                                  
080504*  NCRO
080504                                                                  
080504     IF HDG1-ACCT = 'NCRO      '                                  
080504         MOVE 'Y' TO SEL-SW                                   
080504         MOVE 'Y' TO NCRO-SW                             
080504         GO TO CHECK-ACCT-X
080504     END-IF.
080504                                                                  
019500     IF RUN-MO NOT = (6 AND 12)                                   
019600         GO TO CHECK-ACCT-X.                                      
019700                                                                  
018800*  MWAUTO NE                                                      
018800                                                                  
018900     IF (HDG1-ACCT = 'MWAUTO    ')
033004        AND (HDG2-ST = 'NE' OR 'CO' OR 'IA')
019200             MOVE 'Y' TO SEL-SW                                   
019200             MOVE 'Y' TO MWAUTO-NE-SW                             
019300             GO TO CHECK-ACCT-X.                                  
018800                                                                  
020700 CHECK-ACCT-X.                                                    
020800     EXIT.                                                        
020900                                                                  
022400 WRITE-REPORT SECTION.                                            
022300                                                                  
022500     ADD 1 TO WORK-LINE-NO.                                       
022300                                                                  
022700     MOVE 1 TO WORK-COPY.                                         
022900                                                                  
022900*  TOTAL REPORT                                                   
022900                                                                  
TSTMOD     IF TOTAL-RPT-SW = 'Y'                                        
TSTMOD         WRITE TOTAL-RPT-RECORD FROM WORK-RECORD.                 
022900                                                                  
022900*  MWAUTO SD                                                      
022900                                                                  
TSTMOD     IF MWAUTO-SD-SW = 'Y'                                        
TSTMOD         WRITE MWAUTO-SD-RECORD FROM WORK-RECORD                  
023400         GO TO WRITE-REPORT-X.                                    
022900                                                                  
022900*  MWAUTO NE                                                      
022900                                                                  
TSTMOD     IF MWAUTO-NE-SW = 'Y'                                        
TSTMOD         WRITE MWAUTO-NE-RECORD FROM WORK-RECORD                  
023400         GO TO WRITE-REPORT-X.                                    
023400                                                                  
080504*  NCRO
080504                                                                  
080504     IF NCRO-SW = 'Y'                                        
080504         WRITE NCRO-RECORD      FROM WORK-RECORD                  
080504         GO TO WRITE-REPORT-X
080504     END-IF.
080504                                                                  
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
025300                                                                  
025400 WRITE-HEADINGS-X.                                                
025500     EXIT.                                                        
025600                                                                  
025800 INITIALIZATION SECTION.                                          
025700                                                                  
025700                                                                  
026000     OPEN INPUT  ECS021-FICHE.                                    
026100     OPEN OUTPUT TOTAL-REPORT.                                    
026100     OPEN OUTPUT MWAUTO-SD-REPORT.                                
080504     OPEN OUTPUT NCRO-REPORT.                                
TSTMOD     IF RUN-MO = 06 OR 12                                         
026100         OPEN OUTPUT MWAUTO-NE-REPORT                              
CIDMOD     END-IF.                                                      
CIDMOD                                                                  
026200 INITIALIZATION-X.                                                
026300     EXIT.                                                        
026500                                                                  
028900                                                                  
029100 END-OF-JOB SECTION.                                              
029000                                                                  
029200     CLOSE ECS021-FICHE                                           
029300           TOTAL-REPORT                                           
029300           MWAUTO-SD-REPORT
080504           NCRO-REPORT.                                      
TSTMOD     IF RUN-MO = 06 OR 12                                         
026100         CLOSE MWAUTO-NE-REPORT                                    
CIDMOD     END-IF.                                                      
029000                                                                  
029400 END-OF-JOB-X.                                                    
029500     EXIT.                                                        
029600                                                                  
029800 ABEND-PGM SECTION.                                               
029700                                                                  
029900     DISPLAY                                                      
029900     'PROGRAM CIPA010 INTENTIONALLY ABENDED WITH A S0C7.'.        
030000     DISPLAY 'SEE ERROR MESSAGE ABOVE.'.                          
030100     MOVE +16 TO RETURN-CODE.                                     
030200     ADD +1 TO S0C7.                                              
030300                                                                  
030400 ABEND-X.                                                         
030500     EXIT.                                                        
030600                                                                  
