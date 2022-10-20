000100 IDENTIFICATION DIVISION.                                         
LGC004 PROGRAM-ID. CILGEX01.                                            
000300 AUTHOR. CSO.                                                     
LGC004 DATE-WRITTEN.  06-13-88.                                         
000500 DATE-COMPILED.                                                   
000600 REMARKS.    PENDING PAYMENT AND ADJUSTMENTS EXTRACT.             
000700                                                                  
LGC004***************************************************************** 
LGC004*****    THIS PROGRAM WAS COPIED COMPLETELY FROM AILGEX01   ***** 
LGC004*****    FOR THE LOGIC CLASSIC II REINSTALL.  STATEMENTS    ***** 
LGC004*****    THAT ARE PREFIXED WITH LGC004 HAVE BEEN CHANGED    ***** 
LGC004*****    FROM THE ORIGINAL STATEMENTS IN AILGEX01.          ***** 
000800***************************************************************** 
000900**                                                             ** 
001000**            P R O G R A M   C H A N G E   B L O C K          ** 
001100**                                                             ** 
001200**       THE FOLLOWING PROGRAM PROBLEM REPORT NUMBERS HAVE     ** 
001300**            BEEN APPLIED TO THIS PROGRAM                     ** 
001400**                                                             ** 
001500**                                                             ** 
001600***************************************************************** 
001700                                                                  
001800 ENVIRONMENT DIVISION.                                            
001900 CONFIGURATION SECTION.                                           
002000 SOURCE-COMPUTER. IBM-370-H40.                                    
002100 OBJECT-COMPUTER. IBM-370-H40.                                    
002200 SPECIAL-NAMES.                                                   
002300     C01 IS HOF.                                                  
002400 INPUT-OUTPUT SECTION.                                            
002500 FILE-CONTROL.                                                    
002600     SELECT DATE-CARD    ASSIGN  TO  UT-S-SYS005.                 
002700     SELECT PRTOUT       ASSIGN  TO  UT-S-SYS006.                 
002800     SELECT WORKFIL-CID  ASSIGN  TO  UT-S-SYS032.                 
003000     SELECT ERCPYAJ-IN     ASSIGN TO  SYS031                      
003100                           ORGANIZATION IS INDEXED                
003200                           ACCESS MODE IS SEQUENTIAL              
003300                           RECORD KEY IS PY-CONTROL-PRIMARY       
003400                           FILE STATUS IS PY-RETURN.              
003500     EJECT                                                        
003600 DATA DIVISION.                                                   
003700 FILE SECTION.                                                    
003800                                                                  
003900 FD  WORKFIL-CID                                                  
004000     BLOCK CONTAINS 0 RECORDS                                     
004100     LABEL RECORDS ARE STANDARD                                   
004200     DATA RECORDS ARE OUTREC-CID.                                 
LGC004 01  OUTREC-CID         PIC X(200).                               
005100                                                                  
005200                                                                  
005300 FD  DATE-CARD                                                    
005400     BLOCK CONTAINS 0 RECORDS                                     
005500     LABEL RECORDS ARE STANDARD                                   
005600     DATA RECORDS ARE OUTREC-DM.                                  
005700 01  DATE-REC           PIC X(80).                                
005800                                                                  
005900                                                                  
006000 FD  PRTOUT                                                       
006100     BLOCK CONTAINS 0 RECORDS                                     
006200     LABEL RECORDS ARE STANDARD                                   
006300     DATA RECORDS ARE PRTREC.                                     
006400 01  PRTREC.                                                      
006500     03 PRT-DATA        PIC X(132).                               
006600                                                                  
006700 FD  ERCPYAJ-IN                                                   
006800         LABEL     RECORDS  ARE  STANDARD                         
006900         DATA      RECORDS  ARE  RECIN-CRCPYAJ.                   
007000                                                                  
pemuni*01  PENDING-PAY-ADJ      COPY ERCPYAJ.
pemuni                          COPY ERCPYAJ.
007200     EJECT                                                        
007300 WORKING-STORAGE SECTION.                                         
007400 77  FILLER       PIC X(30) VALUE '**AILGEX01 WORKING STORAGE**'. 
007500 77  COUNT-IN             PIC S9(6)  VALUE ZERO.                  
007600 77  COUNT-IN-PY          PIC S9(6)  VALUE ZERO.                  
007700 77  COUNT-IN-PY-SEL      PIC S9(6)  VALUE ZERO.                  
007800 77  COUNT-OUT-CID        PIC S9(6)  VALUE ZERO.                  
008000 77  PG-CNT               PIC S9(6)  VALUE ZERO.                  
008100 77  AMT-IN               PIC S9(8)V99  VALUE ZERO.               
008200 77  AMT-IN-PY            PIC S9(8)V99  VALUE ZERO.               
008300 77  AMT-IN-PY-SEL        PIC S9(8)V99  VALUE ZERO.               
008400 77  AMT-OUT-CID          PIC S9(8)V99  VALUE ZERO.               
008600 77  PY-RETURN            PIC 99        VALUE ZERO.               
008700 77  LNCNT                PIC 99        VALUE ZERO.               
008800 77  SUB                  PIC 99        VALUE ZERO.               
010300 77  FILLER       PIC X(23) VALUE '**END WORKING-STORAGE**'.      
008900     EJECT                                                        
009300 01  HLD-RECORD-ID        PIC XX.                                 
009400     88 VALID-PY          VALUE IS 'PY'.                          
009500                                                                  
009600 01  EOF-FILE             PIC XXX.                                
009700     88 END-OF-FILE       VALUE IS 'YES'.                         
009800                                                                  
009900     EJECT                                                        
010000 01  FORCE-DUMP-X       PIC X    VALUE SPACES.                    
010100 01  FORCE-DUMP         REDEFINES FORCE-DUMP-X  PIC S9.           
010200                                                                  
010300 01  FILLER             PIC X(12) VALUE '**WORK-DATE*'.           
010400 01  WORK-DATE.                                                   
010500     05 WORK-YR         PIC S999  VALUE ZERO.                     
010600     05 WORK-MO         PIC S999  VALUE ZERO.                     
010700     05 WORK-DAY        PIC S999  VALUE ZERO.                     
010800     05 WORK-DAYS-RE    PIC S999  VALUE ZERO.                     
010900                                                                  
011000 01  TRANS-DATE.                                                  
011100     05 TRAN-YR         PIC 99  VALUE ZERO.                       
011200     05 TRAN-MO         PIC 99  VALUE ZERO.                       
011300     05 TRAN-DAY        PIC 99  VALUE ZERO.                       
011400                                                                  
011500 01  SAVE-PREV-DATE.                                              
011600     05 SAVE-P-YR         PIC 99  VALUE ZERO.                     
011700     05 SAVE-P-MO         PIC 99  VALUE ZERO.                     
011800     05 SAVE-P-DAY        PIC 99  VALUE ZERO.                     
011900                                                                  
012000 01  SAVE-CURR-DATE.                                              
012100     05 SAVE-C-YR         PIC 99  VALUE ZERO.                     
012200     05 SAVE-C-MO         PIC 99  VALUE ZERO.                     
012300     05 SAVE-C-DAY        PIC 99  VALUE ZERO.                     
012400                                                                  
012500 01  FILLER             PIC X(12)  VALUE '**BIN-DATE**'.          
012600 01  WS-BINARY-DATE     PIC S9(9)  COMP-3  VALUE ZERO.            
012700 01  BIN-DATE           PIC S9(9)  COMP    VALUE ZERO.            
012800 01  FILLER        REDEFINES BIN-DATE.                            
012900     05 FILLER          PIC XX.                                   
013000     05 BIN-DATE-WORK   PIC XX.                                   
013100                                                                  
013200 01  HEAD1.                                                       
013300     05 FILLER          PIC X(23) VALUE '  CENTRAL STATES HEALTH'.
013400     05 FILLER          PIC X(23) VALUE ' & LIFE CO. OF OMAHA   '.
013500     05 FILLER          PIC X(23) VALUE '   EXTRACT PROGRAM - '.  
LGC004     05 FILLER          PIC X(08) VALUE 'CILGEX01'.               
013700                                                                  
013800 01  HEAD2.                                                       
013900     05 FILLER          PIC X(23) VALUE '  LOGIC PAYMENT AND ADJ'.
014000     05 FILLER          PIC X(23) VALUE 'USTMENT FILE EXTRACT - '.
014100     05 FILLER          PIC X(16) VALUE 'PREVIOUS CYCLE  '.       
014200     05 HD2-P-MO        PIC XX    VALUE SPACES.                   
014300     05 HD2-DASH1       PIC X     VALUE '-'.                      
014400     05 HD2-P-DAY       PIC XX    VALUE SPACES.                   
014500     05 HD2-DASH2       PIC X     VALUE '-'.                      
014600     05 HD2-P-YR        PIC XX    VALUE SPACES.                   
014700     05 FILLER          PIC X(17) VALUE '  CURRENT CYCLE  '.      
014800     05 HD2-C-MO        PIC XX    VALUE SPACES.                   
014900     05 HD2-DASH3       PIC X     VALUE '-'.                      
015000     05 HD2-C-DAY       PIC XX    VALUE SPACES.                   
015100     05 HD2-DASH4       PIC X     VALUE '-'.                      
015200     05 HD2-C-YR        PIC XX    VALUE SPACES.                   
015300                                                                  
017000 01  FILLER           PIC X(28)  VALUE '**PRTLNE      **   '.     
015400 01  PRTLNE.                                                      
015500     05 FILLER             PIC XXX.                               
015600     05 PRTMES             PIC X(30).                             
015700     05 PRT-AMOUNT         PIC ZZZ,ZZZ,ZZZ.99.                    
015800     05 FILLER             PIC XXX.                               
015900     05 PRTCNT             PIC ZZZ,ZZZ,ZZZ.                       
016000                                                                  
017000 01  FILLER           PIC X(28)  VALUE '**WS-DATE-CARD**   '.     
017100 01  WS-DATE-CARD.                                                
017200     05 DATE-ID                      PIC X.                       
017300     05 DATE-PREVIOUS.                                            
017400        10 DATE-P-MO                 PIC XX.                      
017500        10 DATE-P-DAY                PIC XX.                      
017600        10 DATE-P-YR                 PIC XX.                      
017700     05 DATE-CURRENT.                                             
017800        10 DATE-C-MO                 PIC XX.                      
017900        10 DATE-C-DAY                PIC XX.                      
018000        10 DATE-C-YR                 PIC XX.                      
018100     05 FILLER                       PIC X(73).                   
018200                                                                  
018200**    **WS-ERPYAJ-RECORD**                                        
018200**    *LAYOUT IS LRECL=200*                                       
00017                                                                   
00018  01  WS-ERPYAJ-RECORD.                                            
00019 ******************************************************************
00020      12  WS-PY-RECORD-ID                  PIC XX.                 
00021          88  WS-VALID-PY-ID                     VALUE 'PY'.       
00022                                                                   
00023      12  WS-PY-CONTROL-PRIMARY.                                   
00024          16  WS-PY-COMPANY-CD                PIC X.               
00025          16  WS-PY-CARRIER                   PIC X.               
00026          16  WS-PY-GROUPING                  PIC X(6).            
00027          16  WS-PY-FIN-RESP                  PIC X(10).           
00028          16  WS-PY-ACCOUNT                   PIC X(10).           
00029          16  WS-PY-PRODUCER REDEFINES WS-PY-ACCOUNT               
00030                                              PIC X(10).           
00031          16  WS-PY-FILE-SEQ-NO               PIC S9(8)     COMP.  
00032          16  WS-PY-RECORD-TYPE               PIC X.               
00033              88  WS-PY-REMIT-RECEIVED            VALUE 'R'.       
00034              88  WS-PY-DEPOSIT                   VALUE 'D'.       
00035              88  WS-PY-CHARGE-TO-AGENT           VALUE 'C'.       
00036              88  WS-PY-ADJ-REM-RECEIVED          VALUE 'S'.       
00037              88  WS-PY-ADJ-DEPOSIT               VALUE 'T'.       
00038              88  WS-PY-ADJ-CHG-TO-AGT            VALUE 'U'.       
00039              88  WS-PY-ADD-TO-YTD-COMP           VALUE 'X'.       
00040              88  WS-PY-SUBTRACT-YTD-COMP         VALUE 'Y'.       
00041              88  WS-PY-ADD-TO-BALANCE            VALUE 'Z'.       
00042              88  WS-PY-FICA-ENTRY                VALUE 'F'.       
00043              88  WS-PY-REMIT-IND-GROUPING        VALUE 'G'.       
00044              88  WS-PY-POLICY-FEE                VALUE 'W'.       
00045                                                                   
00046      12  WS-PY-PYMT-TYPE                     PIC X.               
00047              88  WS-PY-NEW-BUS-PYMT              VALUE 'B'.       
00048              88  WS-PY-REINS-PYMT                VALUE 'R'.       
00049              88  WS-PY-EXP-PYMT                  VALUE 'E'.       
00050                                                                   
00051      12  WS-PY-BIL-INV                       PIC X(6).            
00052      12  WS-PY-REF-NO                        PIC X(12).           
00053                                                                   
00054      12  WS-PY-LAST-MAINT-DT                 PIC XX.              
00055      12  WS-PY-LAST-MAINT-BY                 PIC X(4).            
00056      12  WS-PY-LAST-MAINT-HHMMSS             PIC S9(6) COMP-3.    
00057                                                                   
00058      12  WS-PY-PYADJ-RECORD.                                      
00059          16  WS-PY-ENTRY-AMT                 PIC S9(7)V99 COMP-3. 
00060          16  WS-PY-ENTRY-COMMENT             PIC X(30).           
00061                                                                   
00062          16  WS-PY-MSA-DATA     REDEFINES WS-PY-ENTRY-COMMENT.    
00063              20  WS-PY-MSA-ACCOUNT          PIC X(07).            
00064              20  WS-PY-MSA-STATE            PIC X(02).            
00065              20  WS-PY-MSA-CANC-SW          PIC X(01).            
00066                  88  WS-PY-MSA-CANC-CW-ON       VALUE 'Y'.        
00067                  88  WS-PY-MSA-CANC-CW-OFF      VALUE 'N'.        
00068              20  WS-PY-MSA-COMMENT          PIC X(10).            
00069              20  FILLER      REDEFINES WS-PY-MSA-COMMENT.         
00070                  24  WS-PY-MSA-CHECK-NO     PIC 9(06).            
00071                  24  FILLER              PIC X(04).               
00072              20  FILLER                  PIC X(10).               
00073                                                                   
00074          16  WS-PY-SAVE-ACCOUNT             PIC X(10).            
00075          16  WS-PY-SAVE-TYPE                PIC X.                
00076                                                                   
00077          16  FILLER                       PIC X(18).              
00078                                                                   
00079      12  WS-PY-RECORD-STATUS.                                     
00080          16  WS-PY-CREDIT-SELECT-DT          PIC XX.              
00081          16  WS-PY-CREDIT-ACCEPT-DT          PIC XX.              
00082          16  WS-PY-BILLED-DATE               PIC XX.              
00083          16  WS-PY-REPORTED-DT               PIC XX.              
00084          16  WS-PY-PMT-APPLIED               PIC X.               
00085              88  WS-PY-ACCOUNT-PMT               VALUE 'A'.       
00086              88  WS-PY-GA-PMT                    VALUE 'G'.       
00087              88  WS-PY-OVWRITE-PMT               VALUE 'O'.       
00088              88  WS-PY-NON-AR-PMT                VALUE 'N'.       
00089          16  FILLER                       PIC X(5).               
00090          16  WS-PY-INPUT-DT                  PIC XX.              
00091          16  WS-PY-CHECK-NUMBER              PIC X(6).            
00092          16  WS-PY-VOID-SW                   PIC X.               
00093              88  WS-PY-CHECK-VOIDED              VALUE 'V'.       
00094          16  WS-PY-CHECK-ORIGIN-SW           PIC X.               
00095              88  WS-PY-BILLING-CHECK             VALUE 'B'.       
00096              88  WS-PY-REFUND-CHECK              VALUE 'R'.       
00097              88  WS-PY-GA-CHECK                  VALUE 'G'.       
00098              88  WS-PY-CHECK-WRITTEN             VALUE 'W'.       
00099              88  WS-PY-CHECK-REVERSAL            VALUE 'V'.       
00100          16  WS-PY-CHECK-WRITTEN-DT          PIC XX.              
00101          16  WS-PY-CHECK-QUE-CONTROL         PIC S9(8) COMP.      
00102          16  WS-PY-CHECK-QUE-SEQUENCE        PIC S9(4) COMP.      
00103          16  WS-PY-BILL-FLAG                 PIC X.               
00104              88  WS-PY-BILLED                    VALUE 'B'.       
00105          16  WS-PY-AR-FLAG                   PIC X.               
00106              88  WS-PY-AR-CYCLE                  VALUE 'C'.       
00107              88  WS-PY-AR-MONTH-END              VALUE 'M'.       
00108          16  WS-PY-AR-DATE                   PIC XX.              
00109                                                                   
00110      12  WS-PY-GL-CODES.                                          
00111          16  WS-PY-GL-DB                     PIC X(14).           
00112          16  WS-PY-GL-CR                     PIC X(14).           
00113          16  WS-PY-GL-FLAG                   PIC X.               
00114          16  WS-PY-GL-DATE                   PIC XX.              
00115                                                                   
00116      12  FILLER                           PIC X(5).               
00117 ******************************************************************
LGC004*                                                                 
LGC004* ALL LGC004 LINES WERE PRIOR TO LOGIC UPGRADE IN 1992.           
LGC004*                                                                 
LGC004* NEW ERCPYAJ LAYOUT FROM UPGRADE NOW HARDCODED ABOVE.            
LGC004*                                                                 
LGC004*                                                                 
LGC004*    12  WS-PY-RECORD-ID                  PIC XX.                 
LGC004*        88  WS-VALID-PY-ID                     VALUE 'PY'.       
LGC004*                                                                 
LGC004*    12  WS-PY-CONTROL-PRIMARY.                                   
LGC004*        16  WS-PY-COMPANY-CD             PIC X.                  
LGC004*        16  WS-PY-CARRIER                PIC X.                  
LGC004*        16  WS-PY-GROUPING               PIC X(6).               
LGC004*        16  WS-PY-FIN-RESP               PIC X(10).              
LGC004*        16  WS-PY-ACCOUNT                PIC X(10).              
LGC004*        16  WS-PY-FILE-SEQ-NO            PIC S9(8)     COMP.     
LGC004*        16  WS-PY-RECORD-TYPE            PIC X.                  
LGC004*            88  WS-PY-REMIT-RECEIVED         VALUE 'R'.          
LGC004*            88  WS-PY-ADJ-REM-RECEIVED       VALUE 'S'.          
LGC004*            88  WS-PY-CHARGE-TO-AGENT        VALUE 'C'.          
LGC004*            88  WS-PY-ADJ-CHG-TO-AGENT       VALUE 'D'.          
LGC004*            88  WS-PY-ADD-TO-YTD-COMP        VALUE 'X'.          
LGC004*            88  WS-PY-SUBTRACT-YTD-COMP      VALUE 'Y'.          
LGC004*            88  WS-PY-ADD-TO-BALANCE         VALUE 'Z'.          
LGC004*            88  WS-PY-FICA-ENTRY             VALUE 'F'.          
LGC004*            88  WS-PY-CHG-AGT-PAID-COMM      VALUE 'P'.          
LGC004*                                                                 
LGC004*    12  WS-PY-PYMT-TYPE                  PIC X.                  
LGC004*            88  WS-PY-NEW-BUS-PYMT           VALUE 'B'.          
LGC004*            88  WS-PY-REINS-PYMT             VALUE 'R'.          
LGC004*            88  WS-PY-EXP-PYMT               VALUE 'E'.          
LGC004*                                                                 
LGC004*    12  WS-PY-BIL-INV                    PIC X(6).               
LGC004*    12  WS-PY-REF-NO                     PIC X(12).              
LGC004*                                                                 
LGC004*    12  WS-PY-LAST-MAINT-DT              PIC XX.                 
LGC004*    12  WS-PY-LAST-MAINT-BY              PIC X(4).               
LGC004*    12  WS-PY-LAST-MAINT-HHMMSS          PIC S9(6)     COMP-3.   
LGC004*                                                                 
LGC004*    12  WS-PY-PYADJ-RECORD.                                      
LGC004*        16  WS-PY-ENTRY-AMT              PIC S9(7)V99  COMP-3.   
LGC004*        16  WS-PY-ENTRY-COMMENT          PIC X(30).              
LGC004*                                                                 
LGC004*        16  WS-PY-MSA-DATA     REDEFINES WS-PY-ENTRY-COMMENT.    
LGC004*            20  WS-PY-MSA-ACCOUNT          PIC X(07).            
LGC004*            20  WS-PY-MSA-STATE            PIC X(02).            
LGC004*            20  WS-PY-MSA-CANC-SW          PIC X(01).            
LGC004*                88  WS-PY-MSA-CANC-CW-ON       VALUE 'Y'.        
LGC004*                88  WS-PY-MSA-CANC-CW-OFF      VALUE 'N'.        
LGC004*            20  WS-PY-MSA-COMMENT          PIC X(10).            
LGC004*            20  FILLER      REDEFINES WS-PY-MSA-COMMENT.         
LGC004*                24  WS-PY-MSA-CHECK-NO     PIC 9(06).            
LGC004*                24  FILLER              PIC X(04).               
LGC004*            20  FILLER                  PIC X(09).               
LGC004*                                                                 
LGC004*        16  WS-PY-SAVE-ACCOUNT          PIC X(10).               
LGC004*        16  WS-PY-SAVE-TYPE             PIC X.                   
LGC004*                                                                 
LGC004*        16  FILLER                       PIC X(18).              
LGC004*                                                                 
LGC004*    12  WS-PY-RECORD-STATUS.                                     
LGC004*        16  WS-PY-CREDIT-SELECT-DT       PIC XX.                 
LGC004*        16  WS-PY-CREDIT-ACCEPT-DT       PIC XX.                 
LGC004*        16  WS-PY-BILLED-DATE            PIC XX.                 
LGC004*        16  WS-PY-REPORTED-DT            PIC XX.                 
LGC004*        16  WS-PY-PMT-APPLIED            PIC X.                  
LGC004*            88  WS-PY-ACCOUNT-PMT            VALUE 'A'.          
LGC004*            88  WS-PY-GA-PMT                 VALUE 'G'.          
LGC004*            88  WS-PY-OVWRITE-PMT            VALUE 'O'.          
LGC004*        16  FILLER                       PIC X(5).               
LGC004*        16  WS-PY-INPUT-DT               PIC XX.                 
LGC004*        16  WS-PY-CHECK-NUMBER           PIC X(6).               
LGC004*        16  WS-PY-VOID-SW                PIC X.                  
LGC004*            88  WS-PY-CHECK-VOIDED           VALUE 'V'.          
LGC004*        16  WS-PY-CHECK-ORIGIN-SW        PIC X.                  
LGC004*            88  WS-PY-BILLING-CHECK          VALUE 'B'.          
LGC004*            88  WS-PY-REFUND-CHECK           VALUE 'R'.          
LGC004*            88  WS-PY-GA-CHECK               VALUE 'G'.          
LGC004*            88  WS-PY-CHECK-WRITTEN          VALUE 'W'.          
LGC004*            88  WS-PY-CHECK-REVERSAL         VALUE 'V'.          
LGC004*        16  WS-PY-CHECK-WRITTEN-DT       PIC XX.                 
LGC004*        16  WS-PY-CHECK-QUE-CONTROL      PIC S9(8) COMP.         
LGC004*        16  WS-PY-CHECK-QUE-SEQUENCE     PIC S9(4) COMP.         
LGC004*        16  WS-PY-BILL-FLAG              PIC X.                  
LGC004*            88  WS-PY-BILLED                 VALUE 'B'.          
LGC004*        16  WS-PY-AR-FLAG                PIC X.                  
LGC004*        16  WS-PY-AR-DATE                PIC XX.                 
LGC004*                                                                 
LGC004*    12  WS-PY-GL-CODES.                                          
LGC004*        16  WS-PY-GL-DB                  PIC X(14).              
LGC004*        16  WS-PY-GL-CR                  PIC X(14).              
LGC004*                                                                 
LGC004*    12  FILLER                           PIC X(8).               
LGC004*                                                                 
LGC004******************************************************************
023100        EJECT                                                     
023500                                                                  
023600 PROCEDURE DIVISION.                                              
023700 START-PROGRAM.                                                   
023800     OPEN  INPUT  ERCPYAJ-IN.                                     
023900     IF PY-RETURN IS EQUAL TO  '00' OR '97'                       
024000         NEXT SENTENCE                                            
024100         ELSE                                                     
024200         DISPLAY '***** OPEN ERROR ON ERCPYAJ FILE *****'         
024300         DISPLAY 'PY FILE STATUS = ', PY-RETURN                   
024400         DISPLAY 'PROGRAM CANCELLED, GOING TO EOJ'                
024500         GO TO END-OF-JOB-TOTALS-EXIT.                            
024600     OPEN INPUT  DATE-CARD.                                       
024700     OPEN OUTPUT WORKFIL-CID.                                     
024900     OPEN OUTPUT PRTOUT.                                          
025000                                                                  
025000     DISPLAY '*******************************************'.       
025000     DISPLAY ' PERFORM READ-DATE-CARD                    '.       
025000     DISPLAY '*******************************************'.       
025000                                                                  
025000                                                                  
025000                                                                  
025000                                                                  
025100     PERFORM READ-DATE-CARD.                                      
025000                                                                  
025000     DISPLAY '*******************************************'.       
025000     DISPLAY ' PERFORM HEAD-ROUTINE                      '.       
025000     DISPLAY '*******************************************'.       
025000                                                                  
025200     PERFORM HEAD-ROUTINE.                                        
025000                                                                  
025000     DISPLAY '*******************************************'.       
025000     DISPLAY ' PERFORM READ-PY-FILE                      '.       
025000     DISPLAY '*******************************************'.       
025000                                                                  
025300     PERFORM READ-PY-FILE   UNTIL END-OF-FILE.                    
025400                                                                  
025000                                                                  
025000     DISPLAY '*******************************************'.       
025000     DISPLAY ' END-OF-JOB-TOTALS                         '.       
025000     DISPLAY '*******************************************'.       
025000                                                                  
025500     PERFORM END-OF-JOB-TOTALS.                                   
025600     CLOSE ERCPYAJ-IN.                                            
025700     IF PY-RETURN IS GREATER THAN '00'                            
025800         DISPLAY '***** CLOSE ERROR ON ERCPYAJ FILE *****'        
025900         DISPLAY 'PY FILE STATUS = ', PY-RETURN.                  
026000     CLOSE PRTOUT.                                                
026100     CLOSE WORKFIL-CID.                                           
026300     CLOSE DATE-CARD.                                             
026400     STOP RUN.                                                    
026500     EJECT                                                        
026600                                                                  
026700                                                                  
026800                                                                  
026900 READ-DATE-CARD        SECTION.                                   
027000     READ  DATE-CARD INTO WS-DATE-CARD.                           
027100     MOVE DATE-P-YR TO SAVE-P-YR, HD2-P-YR.                       
027200     MOVE DATE-P-MO TO SAVE-P-MO, HD2-P-MO.                       
027300     MOVE DATE-P-DAY TO SAVE-P-DAY, HD2-P-DAY.                    
027400     MOVE DATE-C-YR TO SAVE-C-YR, HD2-C-YR.                       
027500     MOVE DATE-C-MO TO SAVE-C-MO, HD2-C-MO.                       
027600     MOVE DATE-C-DAY TO SAVE-C-DAY, HD2-C-DAY.                    
027700 READ-DATE-CARD-EXIT.                                             
027800     EXIT.                                                        
027900     EJECT                                                        
028000                                                                  
028100                                                                  
028200                                                                  
028300 READ-PY-FILE     SECTION.                                        
028400     READ  ERCPYAJ-IN.                                            
028500     IF PY-RETURN IS GREATER THAN  '00'                           
028600         MOVE 'YES' TO EOF-FILE                                   
028700         GO TO READ-PY-EXIT.                                      
028800                                                                  
028900     MOVE PENDING-PAY-ADJ TO WS-ERPYAJ-RECORD.                    
029000     MOVE WS-PY-RECORD-ID TO HLD-RECORD-ID.                       
029100     ADD +1 TO COUNT-IN.                                          
029300                                                                  
029400     IF WS-VALID-PY-ID                                            
029500         NEXT SENTENCE                                            
029600         ELSE   GO TO READ-PY-EXIT.                               
029700                                                                  
029200     ADD WS-PY-ENTRY-AMT TO AMT-IN.                               
029800     ADD +1 TO COUNT-IN-PY.                                       
029900     ADD WS-PY-ENTRY-AMT TO AMT-IN-PY.                            
030000     IF WS-PY-REMIT-RECEIVED  OR  WS-PY-CHARGE-TO-AGENT           
030100         NEXT SENTENCE                                            
030200         ELSE    GO TO READ-PY-EXIT.                              
030300                                                                  
030400     PERFORM CONVERT-DATE.                                        
030500                                                                  
031600     IF WS-PY-CARRIER IS EQUAL TO 6 OR 9                          
030600       IF TRANS-DATE IS GREATER THAN SAVE-PREV-DATE               
030700           NEXT SENTENCE                                          
030800           ELSE    GO TO READ-PY-FILE.                            
030900                                                                  
031000     IF TRANS-DATE IS GREATER THAN SAVE-CURR-DATE                 
031100         GO TO READ-PY-FILE.                                      
031200                                                                  
031300     ADD +1 TO COUNT-IN-PY-SEL.                                   
031400     ADD WS-PY-ENTRY-AMT TO AMT-IN-PY-SEL.                        
031500                                                                  
031600     IF WS-PY-CARRIER IS EQUAL TO 6 OR 9                          
031700         PERFORM WRITE-CID-FILE.                                  
032000                                                                  
032100     GO TO READ-PY-EXIT.                                          
032200                                                                  
032300 READ-PY-EXIT.                                                    
032400     EXIT.                                                        
032500                                                                  
032600                                                                  
032700     EJECT                                                        
032800                                                                  
032900                                                                  
033000 HEAD-ROUTINE    SECTION.                                         
033100     MOVE SPACE TO PRTREC   WRITE PRTREC BEFORE ADVANCING HOF.    
033300     MOVE HEAD1 TO PRTREC   WRITE PRTREC BEFORE ADVANCING 2 LINES.
033400     MOVE HEAD2 TO PRTREC   WRITE PRTREC BEFORE ADVANCING 2 LINES.
033500     MOVE +4 TO LNCNT.                                            
033600 HEAD-ROUTINE-EXIT.                                               
033700     EXIT.                                                        
033800                                                                  
033900                                                                  
034000                                                                  
034100                                                                  
034200                                                                  
034300                                                                  
034400 CONVERT-DATE            SECTION.                                 
034500     MOVE ZERO TO BIN-DATE.                                       
034600     MOVE WS-PY-LAST-MAINT-DT TO BIN-DATE-WORK.                   
034700     MOVE BIN-DATE TO WS-BINARY-DATE.                             
034800                                                                  
034900     DIVIDE WS-BINARY-DATE BY +384 GIVING WORK-YR                 
035000         REMAINDER WORK-DAYS-RE.                                  
035100     DIVIDE WORK-DAYS-RE BY   +32  GIVING WORK-MO                 
035200         REMAINDER WORK-DAY.                                      
035300     ADD +1 TO WORK-MO.                                           
035400                                                                  
035500     MOVE WORK-YR TO TRAN-YR.                                     
035600     MOVE WORK-MO TO TRAN-MO.                                     
035700     MOVE WORK-DAY TO TRAN-DAY.                                   
035800*    DISPLAY 'WS-BINARY-DATE  - ', WS-BINARY-DATE.                
035900*    DISPLAY 'TRAN-YR         - ', TRAN-YR.                       
036000*    DISPLAY 'TRAN-MO         - ', TRAN-MO.                       
036100*    DISPLAY 'TRAN-DAY        - ', TRAN-DAY.                      
036200 CONVERT-DATE-EXIT.                                               
036300     EXIT.                                                        
036400     EJECT                                                        
036500                                                                  
036600                                                                  
036700                                                                  
036800                                                                  
036900 WRITE-CID-FILE      SECTION.                                     
037000     MOVE WS-ERPYAJ-RECORD TO OUTREC-CID.                         
037100     WRITE OUTREC-CID.                                            
037200     ADD +1 TO COUNT-OUT-CID.                                     
037300     ADD WS-PY-ENTRY-AMT TO AMT-OUT-CID.                          
037400     EJECT                                                        
037500                                                                  
037600                                                                  
038400 END-OF-JOB-TOTALS    SECTION.                                    
038500     MOVE SPACES TO PRTREC  WRITE PRTREC BEFORE ADVANCING 5 LINES.
038600                                                                  
038700     MOVE SPACES TO PRTLNE.                                       
038800     MOVE 'ALL RECORDS IN' TO PRTMES.                             
038900     MOVE COUNT-IN TO PRTCNT.                                     
039000     MOVE AMT-IN   TO PRT-AMOUNT.                                 
039100     MOVE PRTLNE TO PRTREC  WRITE PRTREC BEFORE ADVANCING 2 LINES.
039200                                                                  
039300     MOVE SPACES TO PRTLNE.                                       
039400     MOVE 'PY RECORDS IN' TO PRTMES.                              
039500     MOVE COUNT-IN-PY TO PRTCNT.                                  
039600     MOVE AMT-IN-PY   TO PRT-AMOUNT.                              
039700     MOVE PRTLNE TO PRTREC  WRITE PRTREC BEFORE ADVANCING 2 LINES.
039800                                                                  
039900     MOVE 'PY RECORDS SELECTED' TO PRTMES.                        
040000     MOVE COUNT-IN-PY-SEL TO PRTCNT.                              
040100     MOVE AMT-IN-PY-SEL   TO PRT-AMOUNT.                          
040200     MOVE PRTLNE TO PRTREC  WRITE PRTREC BEFORE ADVANCING 2 LINES.
040300                                                                  
040400     MOVE SPACES TO PRTLNE.                                       
040500     MOVE 'CID RECORDS OUT' TO PRTMES.                            
040600     MOVE COUNT-OUT-CID TO PRTCNT.                                
040700     MOVE AMT-OUT-CID   TO PRT-AMOUNT.                            
040800     MOVE PRTLNE TO PRTREC  WRITE PRTREC BEFORE ADVANCING 2 LINES.
040900                                                                  
041700                                                                  
041800 END-OF-JOB-TOTALS-EXIT.                                          
041900     EXIT.                                                        
