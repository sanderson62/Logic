000100 IDENTIFICATION DIVISION.                                         
000200 PROGRAM-ID.                 CIZBFMT.                             
000300                                                                  
000400*AUTHOR.     CENTRAL STATES OF OMAHA.                             
000500*            OMAHA, NEBR.                                         
000600                                                                  
000700*DATE-COMPILED.                                                   
000800                                                                  
000900*REMARKS.                                                         
001000*        THIS PROGRAM FORMATS THE ZIONS BANK RECORDS INTO THE     
001100*          400 BYTE STANDARD RECORD FORMAT FOR INPUT INTO THE     
001200*            CSO LOGIC PROCESS.                                   
001300                                                                  
001301***************************************************************** 
DAN01 * 09/08/00  CR#2000072400002 - ADD NEVADA TO ZION PROCESSING      
DAN02 * 12/27/00  CR#2000102500002 - ADD COLORADO TO ZION PROCESSING    
092702* 09/27/02                 PEMA  CORRECT PREVIOUS MYSTERY CHANGE
092702*                           ALSO CONVERTED ACCT NUMBERS PER
092702*                           AMY WAHL
001399***************************************************************** 
001400                                                                  
001500 ENVIRONMENT DIVISION.                                            
001600 INPUT-OUTPUT SECTION.                                            
001700 FILE-CONTROL.                                                    
001800     SELECT  ZION-FILE-IN    ASSIGN TO SYS010-UT-2400-S-SYS010.
092702*                            ORGANIZATION IS LINE SEQUENTIAL.   
001900     SELECT  VENDREC-OUT     ASSIGN TO SYS012-UT-2400-S-SYS012.   
002000*    SELECT  RPT-FILE        ASSIGN TO SYS013-UR-1403-S-SYS013.   
002100                                                                  
002200 SKIP3                                                            
002300 DATA DIVISION.                                                   
002400                                                                  
002500 FILE SECTION.                                                    
002600                                                                  
002700******************************************************************
002800**        INPUT TAPE FILE FROM ZIONS BANK                       **
002900******************************************************************
003000                                                                  
003100 FD  ZION-FILE-IN                                                 
003200     RECORDING MODE IS F                                          
003300     LABEL RECORDS ARE STANDARD                                   
003400     RECORD CONTAINS 100 CHARACTERS                               
003500     BLOCK CONTAINS 0 RECORDS                                     
003600     DATA RECORD IS ZION-RECORD.                                  
003700                                                                  
003800 01  ZION-RECORD             PIC X(100).                          
003900                                                                  
004000******************************************************************
004100**       OUTPUT FILE FOR INPUT TO PROGRAM 'CIZB511'             **
004200******************************************************************
004300                                                                  
004400 FD  VENDREC-OUT                                                  
004500     RECORDING MODE IS F                                          
004600     LABEL RECORDS ARE STANDARD                                   
004700     RECORD CONTAINS 400 CHARACTERS                               
004800     BLOCK CONTAINS 0 RECORDS                                     
004900     DATA RECORD IS VENDREC-REC.                                  
005000                                                                  
005100 01  VENDREC-REC             PIC X(400).                          
005200                                                                  
005300******************************************************************
005400**                 OUTPUT ERROR REPORT                          **
005500******************************************************************
005600*                                                                 
005700*FD  RPT-FILE                                                     
005800*    RECORDING MODE IS F                                          
005900*    LABEL RECORDS ARE STANDARD                                   
006000*    RECORD CONTAINS 133 CHARACTERS                               
006100*    BLOCK CONTAINS 0 RECORDS                                     
006200*    DATA RECORD IS RPT-REC-OUT.                                  
006300*                                                                 
006400*01  RPT-REC-OUT.                                                 
006500*    05  RPT-REC                  PIC X(132).                     
006600*                                                                 
006700******************************************************************
006800 SKIP3                                                            
006900                                                                  
007000 WORKING-STORAGE SECTION.                                         
007100 77  FILLER  PIC X(32) VALUE '********************************'.  
007200 77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  
007300 77  FILLER  PIC X(32) VALUE '********************************'.  
007400                                                                  
007500 77  LAST-REC-SW            PIC X         VALUE 'N'.              
007600 77  CANC-SW                PIC X         VALUE 'N'.              
007700 77  ERROR-CODE             PIC X(25)     VALUE SPACES.           
007800 77  RPT-SUB                PIC 99        VALUE ZEROS.            
007900 77  PRINT-SUB              PIC 99        VALUE ZEROS.            
008000 77  RPT-PAGE-CNT           PIC 9999      VALUE ZEROS.            
008100 77  RPT-CAN-CNT            PIC 99999     VALUE ZEROS.            
008200 77  RPT-ISS-CNT            PIC 99999     VALUE ZEROS.            
008300 77  RPT-LINE-CNT           PIC 99        VALUE ZEROS.            
008400 77  ERROR-CNT              PIC 999999    VALUE ZEROS.            
008500 77  IN-CNT                 PIC 999999    VALUE ZEROS.            
008600 77  VENDREC-CNT            PIC 999999    VALUE ZEROS.            
008700 77  NAME-SUB               PIC 99        VALUE ZEROS.            
008800 77  SAVE-LNAME             PIC X(15)     VALUE SPACES.           
008900 77  SAVE-1ST-INIT          PIC X         VALUE SPACES.           
009000 77  SAVE-MIDDLE-INIT       PIC X         VALUE SPACES.           
009100 77  TOT-LIFE-WRITTEN       PIC S9(8)V99  VALUE ZEROS.            
009200 77  TOT-LIFE-CANC          PIC S9(8)V99  VALUE ZEROS.            
009300 77  TOT-AH-WRITTEN         PIC S9(8)V99  VALUE ZEROS.            
009400 77  TOT-AH-CANC            PIC S9(8)V99  VALUE ZEROS.            
009500 77  TOT-CERT-ISS           PIC S9(4)     VALUE ZEROS.            
009600 77  TOT-CERT-CANC          PIC S9(4)     VALUE ZEROS.            
009700 77  SUB1                   PIC 99        VALUE ZEROS.            
009800 77  CANC-CNT               PIC 9999      VALUE ZEROS.            
009900 77  CERT-CNT               PIC 9999      VALUE ZEROS.            
010000 77  FIRST-SW               PIC X         VALUE 'Y'.              
010100                                                                  
010200 01  FILLER   PIC X(44) VALUE                                     
010300      '****  ZION RECORD IN  ****'.                               
010400                                                                  
010500 01  ZION-IN-REC.                                                 
010600     05  FILLER                  PIC X.                           
010700     05  ZION-TRAN-TYPE          PIC X.                           
010800     05  ZION-AGENT-CODE         PIC X(06).                       
010900     05  ZION-BRANCH-NUM         PIC X(04).                       
011000     05  FILLER                  PIC X.                           
011100     05  ZION-CUSTOMER-NUM       PIC X(07).                       
011200     05  ZION-CUSTOMER-NAME.                                      
011300         10  ZION-1ST-INIT       PIC X(01).                       
011400         10  ZION-LAST-NAME      PIC X(08).                       
011500     05  ZION-AGE                PIC XX.                          
011600     05  ZION-NOTE-DATE.                                          
011700         10  ZION-NOTE-MO        PIC XX.                          
011800         10  ZION-NOTE-DA        PIC XX.                          
011900         10  ZION-NOTE-YR        PIC XX.                          
012000     05  ZION-TERM               PIC XXX.                         
012100     05  ZION-LIFE-TYPE          PIC X.                           
012200     05  ZION-JOINT-IND          PIC X.                           
012300     05  ZION-AH-TYPE            PIC XXXX.                        
012400     05  ZION-LIFE-BEN-AMT       PIC 9(06)V99.                    
012500     05  ZION-AH-BEN-AMT         PIC 9(03)V99.                    
012600     05  ZION-LIFE-PREM          PIC 9(04)V99.                    
012700     05  ZION-AH-PREM            PIC 9(04)V99.                    
012800     05  ZION-PAYOFF-DATE.                                        
012900         10  ZION-PO-MO          PIC XX.                          
013000         10  ZION-PO-DA          PIC XX.                          
013100         10  ZION-PO-YR          PIC XX.                          
013200     05  ZION-LIFE-REFUND        PIC 9(04)V99.                    
013300     05  ZION-AH-REFUND          PIC 9(04)V99.                    
013400     05  FLEX-FILLER             PIC X(11).                       
013500                                                                  
013600 01  RPT-TOT-LINE.                                                
013700     05  FILLER              PIC X(10)           VALUE SPACES.    
013800     05  FILLER              PIC X(35)  VALUE                     
013900         ' NUMBER OF ISSUES WRITTEN IS - '.                       
014000     05  P-ISS-CNT           PIC ZZ,ZZ9.                          
014100     05  FILLER              PIC X(10)           VALUE SPACES.    
014200     05  FILLER              PIC X(35)  VALUE                     
014300         'NUMBER OF CANCELS WRITTEN IS - '.                       
014400     05  P-CAN-CNT           PIC ZZ,ZZ9.                          
014500     05  FILLER              PIC X(10)           VALUE SPACES.    
014600     05  FILLER              PIC X(10)           VALUE SPACES.    
014700                                                                  
014800 01  RPT-HD-LINE1.                                                
014900     05  FILLER              PIC X(26)           VALUE SPACES.    
015000     05  ZION-RPT-ID          PIC X(34)  VALUE                    
015100         '           ZIONS BANK REPORT FOR '.                     
015200     05  RPT-REPORT-DATE     PIC X(14)           VALUE SPACES.    
015300     05  FILLER              PIC X(16)           VALUE SPACES.    
015400     05  FILLER              PIC X(15)           VALUE            
015500         'PGM = CIZB511  '.                                       
015600     05  FILLER              PIC X(14)           VALUE SPACES.    
015700     05  FILLER              PIC X(5)   VALUE  'PAGE '.           
015800     05  RPT-PAGE            PIC Z,ZZ9.                           
015900     05  FILLER              PIC X(5)            VALUE SPACES.    
016000                                                                  
016100 01  RPT-HD-LINE2.                                                
016200     05  FILLER              PIC X(10)  VALUE 'CERT #  '.         
016300     05  FILLER              PIC X(12)  VALUE ' DETAIL INFO'.     
016400     05  FILLER              PIC X(09)  VALUE ' '.                
016500     05  FILLER              PIC X(5)   VALUE SPACES.             
016600     05  FILLER              PIC X(20)  VALUE SPACES.             
016700     05  FILLER              PIC X(20)  VALUE SPACES.             
016800     05  FILLER              PIC X(40)  VALUE                     
016900                 '              RECORD TYPE    '.                 
017000     05  FILLER              PIC X(16)  VALUE SPACES.             
017100                                                                  
017200 01  RPT-PT-LINE.                                                 
017300     05  FILLER              PIC X(80)  VALUE SPACES.             
017400     05  FILLER              PIC X(10)  VALUE SPACES.             
017500     05  PT-ERROR            PIC X(32)  VALUE SPACES.             
017600     05  FILLER              PIC X(08)  VALUE SPACES.             
017700                                                                  
017800 01  WK-CERT-FULL.                                                
017900     05 WK-CERT-FILLER      PIC X(03)     VALUE '000'.            
018000     05 WK-CERT             PIC X(07)     VALUE SPACES.           
018100                                                                  
018200 01  WK-ACCT-FULL.                                                
018300     05 WK-ACCT-FILLER      PIC X(04)     VALUE '0000'.           
018400     05 WK-ACCT             PIC X(06)     VALUE SPACES.           
018500                                                                  
018600 01  WK-AH-BEN-FULL.                                              
018700     05 WK-AH-BEN-FILLER    PIC X(01)     VALUE SPACE.            
018800     05 WK-AH-BEN           PIC X(02)     VALUE SPACES.           
018900                                                                  
019000 01  WK-FULL-NAME.                                                
019100     05  FULL-NAME           PIC X      OCCURS 29 TIMES.          
019200                                                                  
019300 01  WK-L-NAME.                                                   
019400     05  L-NAME              PIC X      OCCURS 15 TIMES.          
019500                                                                  
019600 01  WK-DATE.                                                     
019700     05  WK-MO               PIC XX     VALUE SPACES.             
019800     05  FILLER              PIC X      VALUE '/'.                
019900     05  WK-DA               PIC XX     VALUE SPACES.             
020000     05  FILLER              PIC X      VALUE '/'.                
020100     05  WK-YR               PIC XX     VALUE SPACES.             
020200                                                                  
020300 01  WORK-DATE.                                                   
020400     12  WORK-DATE-X.                                             
020500         15  WORK-MO-X       PIC XX.                              
020600         15  WORK-DA-X       PIC XX.                              
020700         15  WORK-YR-X       PIC XX.                              
020800     12  WORK-DATE-N    REDEFINES   WORK-DATE-X.                  
020900         15  WORK-MO-N       PIC 99.                              
021000         15  WORK-DA-N       PIC 99.                              
021100         15  WORK-YR-N       PIC 99.                              
021200                                                                  
021300     12  WORK-DATE-IN.                                            
021400         15  WORK-YR-IN      PIC 99.                              
021500         15  WORK-MO-IN      PIC 99.                              
021600         15  WORK-DA-IN      PIC 99.                              
021700                                                                  
021800 01  PRNT-RPT-DATES.                                              
021900     05  RPT-DT-ID.                                               
022000         10  FILLER          PIC X(14)   VALUE 'JANUARY       '.  
022100         10  FILLER          PIC X(14)   VALUE 'FEBRUARY      '.  
022200         10  FILLER          PIC X(14)   VALUE 'MARCH         '.  
022300         10  FILLER          PIC X(14)   VALUE 'APRIL         '.  
022400         10  FILLER          PIC X(14)   VALUE 'MAY           '.  
022500         10  FILLER          PIC X(14)   VALUE 'JUNE          '.  
022600         10  FILLER          PIC X(14)   VALUE 'JULY          '.  
022700         10  FILLER          PIC X(14)   VALUE 'AUGUST        '.  
022800         10  FILLER          PIC X(14)   VALUE 'SEPTEMBER     '.  
022900         10  FILLER          PIC X(14)   VALUE 'OCTOBER       '.  
023000         10  FILLER          PIC X(14)   VALUE 'NOVEMBER      '.  
023100         10  FILLER          PIC X(14)   VALUE 'DECEMBER      '.  
023200                                                                  
023300     05  RPT-DT-05     REDEFINES   RPT-DT-ID.                     
023400         10  RPT-DT        OCCURS 12    PIC X(14).                
023500                                                                  
023600 01  FILLER   PIC X(24) VALUE '*** VENDREC AREA ***'.             
023700                                                                  
023800*01  OUTPUT-COPYBOOK          COPY VENDREC.                       
023900     COPY VENDREC.                                                
024000*SKIP3                                                            
024100*    COPY ELCDATE.                                                
024200*SKIP3                                                            
024300******************************************************************
024400 SKIP3                                                            
024500 PROCEDURE DIVISION.                                              
024600 SKIP3                                                            
024700                                                                  
024800* *  START INPUT-ROUTINE PROCESSING.                              
024900                                                                  
025000     ACCEPT  WORK-DATE-IN  FROM DATE.                             
025100                                                                  
025200     MOVE  WORK-YR-IN    TO  WK-YR.                               
025300     MOVE  WORK-MO-IN    TO  WK-MO.                               
025400     MOVE  WORK-DA-IN    TO  WK-DA.                               
025500                                                                  
025600     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     
025700     DISPLAY '*   CURRENT DATE IS -- ' WK-DATE                    
025800     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     
025900                                                                  
026000     OPEN INPUT    ZION-FILE-IN.                                  
026100                                                                  
026200     OPEN  OUTPUT  VENDREC-OUT.                                   
026300*                     RPT-FILE.                                   
026400                                                                  
026500 010-READ-INPUT-FILE.                                             
026600                                                                  
026700      READ ZION-FILE-IN                                           
026800         INTO  ZION-IN-REC                                        
026900             AT END                                               
027000                MOVE 'Y'  TO  LAST-REC-SW                         
027100                GO TO END-OF-JOB.                                 
027200                                                                  
027300      ADD  1  TO IN-CNT.                                          
027400      MOVE 'N'  TO  LAST-REC-SW.                                  
027500
027600      IF ZION-TRAN-TYPE = '1' OR '2'
027700          GO  TO  BUILD-VENDREC                                   
027800      END-IF.                                                     
027900                                                                  
028000 REC-TYPE-ERROR.                                                  
028100                                                                  
028200     DISPLAY ' '                                                  
028300     DISPLAY '*** INVALID TRANS TYPE: ' ZION-TRAN-TYPE            
028800     DISPLAY '*** RECORD: ' ZION-IN-REC                           
029200     ADD 1 TO ERROR-CNT.                                          
029300     GO TO 010-READ-INPUT-FILE.                                   
029400                                                                  
029500 BUILD-VENDREC.                                                   
029600                                                                  
029700*    DISPLAY 'BUILD-VENDREC   -   ENTERED           '.            
029800                                                                  
029900     INITIALIZE  VENDOR-REC.                                      
092702     IF ZION-AGENT-CODE (5:2) = '00'
092702        MOVE '01'       TO ZION-AGENT-CODE (5:2)
092702     END-IF
030000                                                                  
030100     MOVE  'Y'  TO  V-WRITTEN-PREM-INDIC.                         

DAN02      IF ZION-AGENT-CODE = '795001'
DAN02         MOVE 'CO' TO V-STATE-CODE
DAN02      ELSE
DAN01
DAN01      IF ZION-AGENT-CODE(4:3) = '001'
DAN01         MOVE 'NV' TO V-STATE-CODE
DAN01      ELSE
DAN01
DAN01      IF ZION-AGENT-CODE(4:3) = '401'
DAN01         MOVE 'ID' TO V-STATE-CODE
DAN01      ELSE
DAN01
DAN01      IF ZION-AGENT-CODE(4:3) = '501'
DAN01         MOVE 'UT' TO V-STATE-CODE
DAN01      ELSE
DAN01
DAN01      IF ZION-AGENT-CODE(4:3) = '801'
DAN01         MOVE 'AZ' TO V-STATE-CODE
DAN01      ELSE
DAN01
DAN01         DISPLAY ' '                                               
DAN01         DISPLAY '*** INVALID ZION ACCOUNT NO: ' ZION-AGENT-CODE
DAN01         DISPLAY '*** RECORD: ' ZION-IN-REC                        
DAN01         ADD 1 TO ERROR-CNT                                        
DAN01         GO TO 010-READ-INPUT-FILE.                                

034300
034400     MOVE  ZION-AGENT-CODE     TO  WK-ACCT.                       
034500     MOVE  WK-ACCT-FULL        TO  V-ACCOUNT-ID.                  
034600
034700     IF ZION-TRAN-TYPE =  '1'                                     
034800         MOVE  '2'             TO  V-TRANS-TYPE                   
034900     ELSE                                                         
035000         MOVE  '3'             TO  V-TRANS-TYPE                   
035100     END-IF                                                       
035200     MOVE  ZION-NOTE-DATE      TO  V-ISS-CERT-EFF-DATE.           
035300                                                                  
035400*    DISPLAY '   '.                                               
035500*    DISPLAY 'EFFECTIVE-DATE = '   V-ISS-CERT-EFF-DATE.           
035600                                                                  
035700     IF  ZION-AGE NOT NUMERIC                                     
035800         MOVE 40 TO  ZION-AGE.                                    
035900                                                                  
036000     IF  ZION-AGE GREATER THAN ZEROS                              
036100         NEXT  SENTENCE                                           
036200      ELSE                                                        
036300         MOVE 40 TO  ZION-AGE.                                    
036400                                                                  
036500     MOVE  ZION-AGE            TO  V-AGE.                         
036600                                                                  
036700     IF ZION-JOINT-IND = '2'                                      
036800        MOVE  ZION-1ST-INIT       TO  V-JT-FIRST-NAME             
036900        MOVE  ZION-LAST-NAME      TO  V-JT-LAST-NAME              
037000        MOVE  '40'                TO  V-JT-AGE                    
037100     END-IF                                                       
037200                                                                  
037300     IF ZION-LIFE-PREM  >  ZERO                                   
DAN02         IF V-STATE-CODE  =  'UT' OR 'CO'                          
037500           IF ZION-JOINT-IND  =  '2'                              
037600              MOVE '83' TO  V-LF-BEN-CODE                         
037700           ELSE                                                   
037800              MOVE '82' TO  V-LF-BEN-CODE                         
037900           END-IF                                                 
038000        END-IF                                                    
DAN01         IF V-STATE-CODE  =  'ID' OR 'NV'                          
038200           IF ZION-JOINT-IND  =  '2'                              
038300              MOVE '03' TO  V-LF-BEN-CODE                         
038400           ELSE                                                   
038500              MOVE '01' TO  V-LF-BEN-CODE                         
038600           END-IF                                                 
038700        END-IF                                                    
038800                                                                  
038900        IF ZION-JOINT-IND = '2'                                   
039000           MOVE 'J'                TO  V-LF-COVG-TYPE
039100        ELSE
039200           MOVE 'S'                TO  V-LF-COVG-TYPE
039300        END-IF
039400
039500        MOVE  ZION-TERM            TO  V-LF-TERM                  
039600        MOVE  ZION-LIFE-BEN-AMT    TO  V-LF-BEN-AMT               
039700        MOVE  ZEROS                TO  V-LF-ALT-BEN-AMT           
039800     END-IF                                                       
039900                                                                  
040000     IF ZION-AH-PREM  >  ZERO                                     
040200        MOVE ' 01'              TO  V-AH-BEN-CODE                 
040500        MOVE  ZION-TERM         TO  V-AH-TERM                     
040600        MOVE  ZION-AH-BEN-AMT   TO  V-AH-BEN-AMT                  
040700     END-IF                                                       
040800                                                                  
040900     IF ZION-TRAN-TYPE = '2'                                      
041000        IF ZION-LIFE-PREM  >  ZERO                                
041100           MOVE ZION-PAYOFF-DATE  TO  V-LF-COVG-CANC-DATE         
041200        END-IF                                                    
041300        IF ZION-AH-PREM    >  ZERO                                
041400           MOVE ZION-PAYOFF-DATE  TO  V-AH-COVG-CANC-DATE         
041500        END-IF                                                    
041600     END-IF                                                       
041700                                                                  
041800     IF ZION-NOTE-YR  LESS THAN  '50'                             
041900        MOVE '00'  TO  V-CENTURY-X                                
042000     ELSE                                                         
042100        MOVE '19'  TO  V-CENTURY-X                                
042200     END-IF                                                       
042300                                                                  
042400     MOVE  ZION-CUSTOMER-NUM        TO  WK-CERT.                  
042500     MOVE  WK-CERT-FULL             TO  V-ISS-CERT-NUMBER.        
042600                                                                  
042700*       ** PP&A DATA **                                           
042800                                                                  
042900     MOVE  ZION-BRANCH-NUM          TO  V-BANK-ID.                
043000     MOVE  ZION-NOTE-DATE           TO  V-NOTE-DATE.              
043100     MOVE  ZION-1ST-INIT            TO  V-FIRST-NAME.             
043200     MOVE  ZION-LAST-NAME           TO  V-LAST-NAME.              
043300     MOVE  ZION-LIFE-REFUND         TO  V-LF-CANC-REFUND.         
043400     MOVE  ZION-AH-REFUND           TO  V-AH-CANC-REFUND.         
043500     MOVE  ZION-LIFE-PREM           TO  V-LIFE-PREM-WRITTEN.      
043600     MOVE  ZION-AH-PREM             TO  V-AH-PREM-WRITTEN.        
043700                                                                  
043800     WRITE VENDREC-REC FROM VENDOR-REC.                           
043900     ADD 1 TO VENDREC-CNT.                                        
044000                                                                  
044100     GO  TO  010-READ-INPUT-FILE.                                 
044200                                                                  
044300*DATE-CONVERT-ROUTINE.                                            
044400*                                                                 
044500*    CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   
044600*                                                                 
044700*DATE-CONVERT-X.                                                  
044800*    EXIT.                                                        
044900                                                                  
045000 SKIP3                                                            
045100 END-OF-JOB.                                                      
045200                                                                  
045300*    DISPLAY 'END-OF-JOB    -   ENTERED '.                        
045400                                                                  
045410     DISPLAY ' '                                                  
045420     DISPLAY ' '                                                  
045430     DISPLAY ' '                                                  
045500     DISPLAY '************************************************'   
045600     DISPLAY 'INPUT RECORDS      -- ' IN-CNT.                     
045700     DISPLAY ' '                                                  
045800     DISPLAY 'VENDRECS WRITTEN   -- ' VENDREC-CNT.                
045900     DISPLAY ' '                                                  
046000     DISPLAY 'ERROR COUNT        -- ' ERROR-CNT.                  
046100     DISPLAY ' '                                                  
046200     DISPLAY '************************************************'.  
046300                                                                  
046400     CLOSE ZION-FILE-IN                                           
046500           VENDREC-OUT.                                           
046600*          RPT-FILE.                                              
046700                                                                  
046800                                                                  
046900     GOBACK.                                                      
047000                                                                  
