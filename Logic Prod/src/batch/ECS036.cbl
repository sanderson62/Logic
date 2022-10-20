00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 ECS036.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 11/28/95 11:10:55.                 
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          
00008 *                            VMOD=2.014.                          
00009                                                                   
00010 *AUTHOR.        LOGIC, INC.                                       
00011 *               DALLAS, TEXAS.                                    
00012                                                                   
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANSE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
120602* 120602    2002120500014  SMVA  MODIFY COPYBOOK MODULE PRTN036
012703* 012703                   SMVA  DATA SHOWING IN ECS036D RPT FROM
012703*                                PREVIOUS PASSES - CLEAR HD-6
112503* 112503                   SMVA  FIX PASS 4 INITIAL MISSING HEADER
012704* 012704    2004011500004  SMVA  REMOVE RMRO,UMB, & MFG FROM SYS050
012704*                                AND REMOVE ALL SYS051 PRINT             
050504* 050504    2004042100005  SMVA  DROP CENTS FROM A&H BEN AMT FLD 
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
122804* 122804    2004101500005  PEMA  ADD ITD AND PYTD AND PL12
010306* 010306    2006010300002  PEMA  CORRECT PROBLEM WITH PYTD TOTALS
072709* 072709  CR2009042000001  AJRA  ADD PASS 6 FOR USER SELECT 2
082609* 082609  CR2009030400001  AJRA  ADD L12,YTD AND PYTD GRAND TOTALS
031511* 031511  CR2009082100002  AJRA  ADD PASS 7 FOR USER SELECT 5
091614* 091614  IR2014091000003  PEMA  FIX PYTD STATE TOTALS AHPRM
012517* 012517  CR2017012300004  PEMA  Add extract 8, produce 36H RPT
      ******************************************************************
00024 *REMARKS.                                                         
00025 *        PRINTS MONTHLY PRODUCTION REPORTS FOR LAST 12 MONTHS -   
00026 *                                                                 
00027 *  PASS 1 - STATE OVERALL................................ECS036A  
00028 *  PASS 2 - CARRIER/COMPANY WITHIN BUSINESS TYPE.........ECS036B  
00029 *  PASS 3 - CARRIER/COMPANY WITHIN AGENCY................ECS036C  
00030 *  PASS 4 - REPORT CODE 1 OVERALL........................ECS036D  
00031 *  PASS 5 - REPORT CODE 2 WITHIN CARRIER & GROUPING......ECS036E  
00032 *                 (STATE BREAKS)     
072709*  PASS 6 - USER SELECT 2 ...............................ECS036F                             
031511*  PASS 7 - USER SELECT 5/REPORT CODE 1 .................ECS036G
012517*  PASS 8 - report code 3 overall                        ECS036H
00033 *                                                                 
00034 *     -- FORMAT OPTIONS --                                        
00035 *        1 = PRINT ALL ECS035 REPORTS                             
00036 *        2 = PRINT ECS036A ONLY - STATE OVERALL                   
00037 *        3 = PRINT ECS036B ONLY - CARR/CO WITHIN BUS TYPE         
00038 *        4 = PRINT ECS036C ONLY - CARR/CO WITHIN AGENCY           
00039 *        5 = PRINT ECS036D ONLY - REPORT CODE 1 OVERALL           
00040 *        6 = PRINT ECS036E ONLY - REPORT CODE 2 WITHIN CARR       
00041 *                                                                 
00042 *     -- REPORT CODE TOTAL OPTIONS --                             
00043 *        1 = NO REPORT CODING                                     
00044 *        2 = REPORT CODE 1 ONLY                                   
00045 *        3 = REPORT CODE 2 ONLY                                   
00046 *        4 = BOTH REPORT CODES                                    
00047 *        5 = REPORT CODE 1 ONLY (DETAIL)                          
00048 *        6 = REPORT CODE 2 ONLY (DETAIL)                          
00049 *        7 = BOTH REPORT CODES  (DETAIL)                          
00050  EJECT                                                            
00051  ENVIRONMENT DIVISION.                                            
00052  CONFIGURATION SECTION.                                           
00053                                                                   
00054  INPUT-OUTPUT SECTION.                                            
00055  FILE-CONTROL.                                                    
00056                                                                   
00057      SELECT SORT-FILE    ASSIGN TO SYS001.      
00058      SELECT EARNED-PREM  ASSIGN TO SYS012-UT-FBA1-S-SYS012.       
00059      SELECT PRNT         ASSIGN TO SYS008-UR-1403-S-SYS008.       
00060      SELECT DISK-DATE    ASSIGN TO SYS019-UT-FBA1-S-SYS019.       
00061      SELECT FICH         ASSIGN TO SYS020-UT-2400-S-SYS020.       
CIDMOD     SELECT SELECT-FILE  ASSIGN TO SYS050-UT-1403-S-SYS050.       
00062                                                                   
00063  DATA DIVISION.                                                   
00064  FILE SECTION.                                                    
00065                                                                   
CIDMOD FD  SELECT-FILE                                                  
CIDMOD     RECORDING MODE F.                                            
CIDMOD                                                                  
CIDMOD 01  SELECT-RECORD.                                               
CIDMOD     05  SELECT-CC               PIC X.                           
CIDMOD     05  SLCT-FLD-1              PIC X(11).                       
CIDMOD     05  SLCT-FLD-2              PIC X(10).                       
CIDMOD     05  SLCT-REC                PIC X(111).                      
CIDMOD                                                                  
CIDMOD*                                                                 
00066  SD  SORT-FILE.                                                   
00067                                                                   
00068  01  SORT-FILE-REC.                                               
00069      05 SORT-CONTROL-74          PIC X(40).                       
CIDMOD     05  SORT-DATE               PIC X(04).                       
CIDMOD     05  FILLER                  PIC X(98).                       
CIDMOD     05  SORT-AM-EXPIRE          PIC X(06).
CIDMOD     05  SORT-AM-HI-CERT         PIC X(06).
CIDMOD     05  SORT-HI-CERT            PIC X(06).                       
00068      05  FILLER                  PIC X(36).                       
00071                                                                   
00072  FD  EARNED-PREM                                                  
00073      BLOCK CONTAINS 0 RECORDS
00074      RECORDING MODE F.                                            
00075                                                                   
CIDMOD 01  EP-EXTR                     PIC X(196).                      
CIDMOD*01  EP-EXTR                     PIC X(166).                      
00077                                                                   
00078  FD  PRNT                                                         
00079      COPY ELCPRTFD.                                               
00080  EJECT                                                            
00081  FD  DISK-DATE                                                    
00082      COPY ELCDTEFD.                                               
00083  EJECT                                                            
00084  FD  FICH                                                         
00085      COPY ELCFCHFD.                                               
00086  EJECT                                                            
00087  WORKING-STORAGE SECTION.                                         
00088  77  FILLER  PIC X(32) VALUE '********************************'.  
00089  77  FILLER  PIC X(32) VALUE '     ECS036 WORKING STORAGE     '.  
00090  77  FILLER  PIC X(32) VALUE '******* VMOD=2.014 *************'.  
00091                                                                   
CIDMOD 77  SAVE-NAME                   PIC X(07)           VALUE SPACE. 
CIDMOD 77  PRINT-SELECT-SW             PIC X(03)           VALUE 'NO '. 
CIDMOD 77  CC-SW                       PIC X               VALUE SPACE. 
00092  77  STATE-TOTAL-LINE            PIC X(30)           VALUE SPACE. 
00093  77  SAVE-STATE                  PIC XX              VALUE SPACE. 
00094  77  SKIP-HD-SW                  PIC X               VALUE SPACE. 
00095  77  PASS-NUMBER                 PIC X               VALUE '1'.   
00096  77  FIRST-TIME-SWITCH           PIC X               VALUE 'Y'.   
00097      88  FIRST-TIME                                  VALUE 'Y'.   
00098  77  LINE-CNT                    PIC S999  COMP-3    VALUE ZEROS. 
00099  77  PAGE-CNT                    PIC S9(5) COMP-3    VALUE ZEROS. 
00100  77  PGM-SUB                     PIC S999  COMP      VALUE +036.  
00101  77  X                           PIC X.                           
00102  77  X1                          PIC S999  COMP      VALUE ZERO.  
122804 77  DISP-X1                     PIC 999             VALUE ZEROS.
00103  77  Y1                          PIC S999  COMP      VALUE ZERO.  
00104  77  EOF-EPX                     PIC X               VALUE SPACE. 
00105  77  WRITE-AC-SW                 PIC X               VALUE SPACE. 
00106  77  ST-SUB                      PIC 99              VALUE ZEROS. 
00107  77  DT-SUB                      PIC S99   COMP      VALUE ZEROS. 
00108  77  WORK-BUSC                   PIC XX.                          
00109  77  SET-CTR                     PIC S999  COMP-3    VALUE ZERO.  
012517 77  in-cnt                      pic s9(7) comp-3    value +0.
00110                                                                   
00111  01  QTR-SW                      PIC X               VALUE SPACE. 
00112      88  QTR-END                                     VALUE '1'.   
00113                                                                   
00114  01  QTR-COMP                    PIC X               VALUE SPACE. 
00115      88  QTR-CO                                      VALUE '1'.   
00116  EJECT                                                            
00117  01  WS-EXTR-REC.                                                 
00118      05  EP-PASS-NO              PIC X.                           
00119      05  EP-CNTL.                                                 
00120          10  EP-CNTL-1.                                           
00121              15  EP-CNTL-GA      PIC X(10).                       
00122              15  EP-CNTL-ACCT    PIC X(10).                       
00123          10  EP-CNTL-2.                                           
00124              15  EP-CARR         PIC X.                           
00125              15  EP-GROUP        PIC X(6).                        
00126          10  FILLER              PIC X(12).                       
00127      05  EP-ALT-RPT-CNTL  REDEFINES  EP-CNTL.                     
00128          10  EP-A-RPT-CD-1       PIC X(10).                       
00129          10  EP-A-CARR           PIC X.                           
00130          10  EP-A-GROUP          PIC X(6).                        
00131          10  EP-A-RPT-CD-2       PIC X(10).                       
00132          10  EP-A-STATE          PIC XX.                          
00133          10  EP-A-ACCT           PIC X(10).                       
00134      05  EP-DTE                  PIC 9(07)  COMP-3.               
00135      05  EP-CNTRS    COMP-3.                                      
00136          10  EP-CERT             PIC S9(11)V99.                   
00137          10  EP-LBEN             PIC S9(11)V99.                   
00138          10  EP-LPRM             PIC S9(11)V99.                   
00139          10  EP-LCLM             PIC S9(11)V99.                   
00140          10  EP-ABEN             PIC S9(11)V99.                   
00141          10  EP-APRM             PIC S9(11)V99.                   
00142          10  EP-ACLM             PIC S9(11)V99.                   
00143          10  EP-TPRM             PIC S9(11)V99.                   
00144          10  EP-TCOM             PIC S9(11)V99.                   
102004     05  FILLER                  PIC X(4).                        
102004     05  EP-ACCT-STATUS          PIC X.
00146      05  EP-ACCT-NAME            PIC X(30).                       
00147      05  EP-ACC-EXPIRES          PIC 9(11)  COMP-3.               
00148      05  EP-ACC-HI-CERT          PIC 9(11)  COMP-3.               
00149      05  EP-MTH-HI-CERT          PIC 9(11)  COMP-3.               
00150      05  EP-ISS-CNT              PIC S9(9) COMP-3.                
00151      05  FILLER                  PIC X.                           
CIDMOD     05  EP-ACCT-CITY            PIC X(30).                       
00152                                                                   
00153  01  PRINT-SWITCHES.                                              
00154      05  P-ACC-SW                PIC X VALUE ' '.                 
00155      05  P-ST-SW                 PIC X VALUE ' '.                 
00156      05  P-GP-SW                 PIC X VALUE ' '.                 
00157      05  P-CA-SW                 PIC X VALUE ' '.                 
00158      05  P-RP-SW                 PIC X VALUE ' '.                 
031511     05  P-RP2-SW                PIC X VALUE ' '.                 
00159                                                                   
00160  01  RUN-DT.                                                      
00161      05  RUN-DT-CCYY             PIC 9(04) VALUE ZEROS.           
00162      05  RUN-DT-CCYR REDEFINES RUN-DT-CCYY.                       
00163          10  RUN-DT-CC           PIC 99.                          
00164          10  RUN-DT-YR           PIC 99.                          
00165      05  RUN-DT-MO               PIC 99  VALUE ZEROS.             
00166                                                                   
00167  01  RUN9DT REDEFINES RUN-DT     PIC 9(6).                        
00168                                                                   
00169  01  WORK-ACCUM.                                                  
00170      05  CERT                    PIC S9(11)    COMP-3 VALUE ZEROS.
00171      05  ISS                     PIC S9(9)     COMP-3 VALUE ZEROS.
00172      05  LBEN                    PIC S9(12)V99 COMP-3 VALUE ZEROS.
00173      05  LPRM                    PIC S9(11)V99 COMP-3 VALUE ZEROS.
00174      05  LCLM                    PIC S9(12)V99 COMP-3 VALUE ZEROS.
00175      05  ABEN                    PIC S9(12)V99 COMP-3 VALUE ZEROS.
00176      05  APRM                    PIC S9(11)V99 COMP-3 VALUE ZEROS.
00177      05  ACLM                    PIC S9(12)V99 COMP-3 VALUE ZEROS.
00178      05  TPRM                    PIC S9(11)V99 COMP-3 VALUE ZEROS.
00179      05  TCOM                    PIC S9(11)V99 COMP-3 VALUE ZEROS.
00180                                                                   
00181  01  REPT-ACCUM.                                                  
00182      05  RPCERT                  PIC S9(11)V99 COMP-3 VALUE ZEROS.
00183      05  RPLBEN                  PIC S9(12)V99 COMP-3 VALUE ZEROS.
00184      05  RPLPRM                  PIC S9(11)V99 COMP-3 VALUE ZEROS.
00185      05  RPLCLM                  PIC S9(12)V99 COMP-3 VALUE ZEROS.
00186      05  RPABEN                  PIC S9(12)V99 COMP-3 VALUE ZEROS.
00187      05  RPAPRM                  PIC S9(11)V99 COMP-3 VALUE ZEROS.
00188      05  RPACLM                  PIC S9(12)V99 COMP-3 VALUE ZEROS.
00189      05  RPTPRM                  PIC S9(11)V99 COMP-3 VALUE ZEROS.
00190      05  RPTCOM                  PIC S9(11)V99 COMP-3 VALUE ZEROS.
031511
031511 01  REPT2-ACCUM.                                                  
031511     05  RP2CERT                 PIC S9(11)V99 COMP-3 VALUE ZEROS.
031511     05  RP2LBEN                 PIC S9(12)V99 COMP-3 VALUE ZEROS.
031511     05  RP2LPRM                 PIC S9(11)V99 COMP-3 VALUE ZEROS.
031511     05  RP2LCLM                 PIC S9(12)V99 COMP-3 VALUE ZEROS.
031511     05  RP2ABEN                 PIC S9(12)V99 COMP-3 VALUE ZEROS.
031511     05  RP2APRM                 PIC S9(11)V99 COMP-3 VALUE ZEROS.
031511     05  RP2ACLM                 PIC S9(12)V99 COMP-3 VALUE ZEROS.
031511     05  RP2TPRM                 PIC S9(11)V99 COMP-3 VALUE ZEROS.
031511     05  RP2TCOM                 PIC S9(11)V99 COMP-3 VALUE ZEROS.
00191  EJECT                                                            
00192  01  HD-1-STATE.                                                  
00193      05  FILLER                  PIC X(46)   VALUE SPACES.        
00194      05  HD-1-ST                 PIC X(10)   VALUE ' MONTHLY'.    
00195      05  FILLER                  PIC X(6)    VALUE 'STATE '.      
00196      05  FILLER                  PIC X(17)                        
00197                                  VALUE 'PRODUCTION REPORT'.       
00198      05  FILLER                  PIC X(05)   VALUE SPACES.
122304     05  HD-1-ST-SINGLE-FEE      PIC X(21)   VALUE SPACES.
122304     05  FILLER                  PIC X(14)   VALUE SPACES.
00199      05  FILLER                  PIC X(7)    VALUE 'ECS036A'.     
00200                                                                   
00201  01  HD-1-BUSINESS.                                               
00202      05  FILLER                  PIC X(44)   VALUE SPACES.        
00203      05  HD-1-BUS                PIC X(10)   VALUE ' MONTHLY'.    
00204      05  FILLER                  PIC X(9)    VALUE 'BUSINESS'.    
00205      05  FILLER                  PIC X(17)                        
00206                                  VALUE 'PRODUCTION REPORT'.       
00207      05  FILLER                  PIC X(05)   VALUE SPACES.        
122304     05  HD-1-BUS-SINGLE-FEE     PIC X(21)   VALUE SPACES.
122304     05  FILLER                  PIC X(13)   VALUE SPACES.        
00208      05  FILLER                  PIC X(7)    VALUE 'ECS036B'.     
00209                                                                   
00210  01  HD-1-AGENCY.                                                 
00211      05  FILLER                  PIC X(45)   VALUE SPACES.        
00212      05  HD-1-AGCY               PIC X(10)   VALUE ' MONTHLY'.    
00213      05  FILLER                  PIC X(7)    VALUE 'AGENCY'.      
00214      05  FILLER                  PIC X(17)                        
00215                                  VALUE 'PRODUCTION REPORT'.       
122304     05  FILLER                  PIC X(05)   VALUE SPACES.
122304     05  HD-1-AGCY-SINGLE-FEE    PIC X(21)   VALUE SPACES.
122304     05  FILLER                  PIC X(14)   VALUE SPACES.
00217      05  FILLER                  PIC X(7)    VALUE 'ECS036C'.     
00218                                                                   
00219  01  HD-1-RPT-CODE-1.                                             
112503     05  HD-1-SLCT-FLD-1         PIC X(11)   VALUE 'RPT CODE 1 '.
112503     05  HD-1-SLCT-FLD-2         PIC X(10)   VALUE SPACES.
112503     05  HD-1-SLCT-REC-POS1      PIC X(01)   VALUE SPACE.
112503     05  FILLER                  PIC X(22)   VALUE SPACES.
00221      05  HD-1-RC-1               PIC X(9)    VALUE ' MONTHLY'.    
00222      05  HD-1-RPT-CD-1           PIC X(10)   VALUE SPACES.        
00223      05  FILLER                  PIC X(18)                        
00224                                  VALUE ' PRODUCTION REPORT'.      
122304     05  FILLER                  PIC X(05)   VALUE SPACES.
122304     05  HD-1-RC1-SINGLE-FEE     PIC X(21)   VALUE SPACES.
122304     05  FILLER                  PIC X(12)   VALUE SPACES.
00226      05  FILLER                  PIC X(6)    VALUE 'ECS036'.      
00227      05  HD-1-DF                 PIC X       VALUE 'D'.           
00228                                                                   
00229  01  HD-1-RPT-CODE-2.                                             
00230      05  FILLER                  PIC X(44)   VALUE SPACES.        
00231      05  HD-1-RC-2               PIC X(9)    VALUE ' MONTHLY'.    
00232      05  HD-1-RPT-CD-2           PIC X(10)   VALUE SPACES.        
00233      05  FILLER                  PIC X(18)                        
00234                                  VALUE ' PRODUCTION REPORT'.      
122304     05  FILLER                  PIC X(05)   VALUE SPACES.
122304     05  HD-1-RC2-SINGLE-FEE     PIC X(21)   VALUE SPACES.
122304     05  FILLER                  PIC X(12)   VALUE SPACES.
00236      05  FILLER                  PIC X(7)    VALUE 'ECS036E'.     
00237                                                                   
00238  01  HD-1-CARRIER.                                                
00239      05  FILLER                  PIC X(45)   VALUE SPACES.        
00240      05  HD-1-CAR                PIC X(10)   VALUE ' MONTHLY'.    
00241      05  FILLER                  PIC X(8)    VALUE 'CARRIER'.     
00242      05  FILLER                  PIC X(17)                        
00243                                  VALUE 'PRODUCTION REPORT'.       
122304     05  FILLER                  PIC X(05)   VALUE SPACES.
122304     05  HD-1-CAR-SINGLE-FEE     PIC X(21)   VALUE SPACES.
122304     05  FILLER                  PIC X(13)   VALUE SPACES.
00245      05  FILLER                  PIC X(6)    VALUE 'ECS036'.      
00246      05  HD-1-CAR-SUF            PIC X       VALUE ' '.           
00247                                                                   
00248  01  HD-1-GROUPING.                                               
00249      05  FILLER                  PIC X(45)   VALUE SPACES.        
00250      05  HD-1-GRP                PIC X(8)    VALUE 'MONTHLY'.     
00251      05  FILLER                  PIC X(9)    VALUE 'GROUPING'.    
00252      05  FILLER                  PIC X(17)                        
00253                                  VALUE 'PRODUCTION REPORT'.       
122304     05  FILLER                  PIC X(05)   VALUE SPACES.
122304     05  HD-1-GRP-SINGLE-FEE     PIC X(21)   VALUE SPACES.
122304     05  FILLER                  PIC X(14)   VALUE SPACES.
00255      05  FILLER                  PIC X(6)    VALUE 'ECS036'.      
00256      05  HD-1-GRP-SUF            PIC X       VALUE ' '.           
00257                                                                   
072709 01  HD-1-USER-SEL-2.                                             
072709     05  HD-1-US2-FLD-1          PIC X(11)   VALUE 'USER SEL 2 '.
072709     05  HD-1-US2-FLD-2          PIC X(10)   VALUE SPACES.
072709     05  HD-1-US2-REC-POS1       PIC X(01)   VALUE SPACE.
072709     05  FILLER                  PIC X(22)   VALUE SPACES.
072709     05  HD-1-US2                PIC X(9)    VALUE ' MONTHLY'.    
072709     05  HD-1-USERSEL2           PIC X(10)   VALUE 'USER SEL 2'.        
072709     05  FILLER                  PIC X(18)                        
072709                                 VALUE ' PRODUCTION REPORT'.      
072709     05  FILLER                  PIC X(05)   VALUE SPACES.
072709     05  HD-1-US2-SINGLE-FEE     PIC X(21)   VALUE SPACES.
072709     05  FILLER                  PIC X(12)   VALUE SPACES.
072709     05  FILLER                  PIC X(7)    VALUE 'ECS036F'.      
072709                                                                   
031511 01  HD-1-USER-SEL-5.                                             
031511     05  HD-1-US5-FLD-1          PIC X(11)   VALUE 'USER SEL 5 '.
031511     05  HD-1-US5-FLD-2          PIC X(10)   VALUE SPACES.
031511     05  HD-1-US5-REC-POS1       PIC X(01)   VALUE SPACE.
031511     05  FILLER                  PIC X(22)   VALUE SPACES.
031511     05  HD-1-US5                PIC X(9)    VALUE ' MONTHLY'.    
031511     05  HD-1-USERSEL5           PIC X(10)   VALUE 'USER SEL 5'.        
031511     05  FILLER                  PIC X(18)                        
031511                                 VALUE ' PRODUCTION REPORT'.      
031511     05  FILLER                  PIC X(05)   VALUE SPACES.
031511     05  HD-1-US5-SINGLE-FEE     PIC X(21)   VALUE SPACES.
031511     05  FILLER                  PIC X(12)   VALUE SPACES.
031511     05  FILLER                  PIC X(7)    VALUE 'ECS036G'.      
031511                                                                   
00258  01  HD-1-GRAND-TOTALS.                                           
00259      05  FILLER                  PIC X(47)   VALUE SPACES.        
00260      05  FILLER                  PIC X(17)                        
00261                                  VALUE 'PRODUCTION REPORT'.       
00262      05  FILLER                  PIC X(13)                        
00263                                  VALUE ' GRAND TOTALS'.           
122304     05  FILLER                  PIC X(05)   VALUE SPACES.
122304     05  HD-1-GT-SINGLE-FEE      PIC X(21)   VALUE SPACES.
122304     05  FILLER                  PIC X(16)   VALUE SPACES.
00265      05  FILLER                  PIC X(6)    VALUE 'ECS036'.      
00266      05  HD-1-GT                 PIC X.                           
00267                                                                   
00268  01  HD-2.                                                        
112503     05  HD-2-SLCT-FLD-1         PIC X(11)   VALUE SPACES.
112503     05  HD-2-SLCT-FLD-2         PIC X(10)   VALUE SPACES.
112503     05  HD-2-SLCT-REC-POS1      PIC X(01)   VALUE SPACE.
112503     05  FILLER                  PIC X(25)   VALUE SPACES.
00270      05  HD-COMPANY-NAME         PIC X(30)   VALUE SPACES.        
00271      05  FILLER                  PIC X(42)   VALUE SPACES.        
00272      05  HD-IPL-DATE             PIC X(8)    VALUE SPACES.        
00273                                                                   
00274  01  HD-3.                                                        
112503     05  HD-3-SLCT-FLD-1         PIC X(11)   VALUE SPACES.
112503     05  HD-3-SLCT-FLD-2         PIC X(10)   VALUE SPACES.
112503     05  HD-3-SLCT-REC-POS1      PIC X(01)   VALUE SPACE.
112503     05  FILLER                  PIC X(31)   VALUE SPACES.
00276      05  HD-ALPHA-DATE           PIC X(18)   VALUE SPACES.        
00277      05  FILLER                  PIC X(48)   VALUE SPACES.        
00278      05  FILLER                  PIC X(5)    VALUE 'PAGE '.       
00279      05  HD-PAGE                 PIC ZZ,ZZ9.                      
00280                                                                   
00281  01  HD-4.                                                        
00282      05  FILLER                  PIC X(8)    VALUE 'CARRIER'.     
00283      05  HD-4-CARRIER            PIC X       VALUE SPACES.        
00284      05  FILLER-HD-4             PIC X(10)   VALUE ' GROUPING'.   
00285      05  HD-4-GROUPING           PIC X(6)    VALUE SPACES.        
031511
031511 01  HD-4G.
031511     05  FILLER                  PIC X(10)   VALUE 'REPT CD 1'.
031511     05  HD-4G-REPT-CD-2         PIC X(10)   VALUE SPACES.
00286                                                                   
00287  01  HD-5.                                                        
00288      05  FILLER                  PIC X(8)    VALUE 'ACCOUNT'.     
00289      05  HD-ACCOUNT              PIC X(10)   VALUE SPACES.        
00290      05  FILLER                  PIC XX      VALUE SPACES.        
CIDMOD*    05  HD-ACCT-NAME            PIC X(20)   VALUE SPACES.        
CIDMOD     05  HD-ACCT-NAME            PIC X(30)   VALUE SPACES.        
00292      05  FILLER                  PIC X(6)    VALUE SPACES.        
CIDMOD*    05  HD-ACCT-ADDRESS         PIC X(20)   VALUE SPACES.        
CIDMOD     05  HD-ACCT-ADDRESS         PIC X(30)   VALUE SPACES.        
CIDMOD                                                                  
CIDMOD 01  HD-5A.                                                       
CIDMOD     05  FILLER                  PIC X(8)    VALUE 'CITY   '.     
CIDMOD     05  FILLER                  PIC X(12)   VALUE SPACES.        
CIDMOD     05  HD-5A-CITY              PIC X(30)   VALUE SPACES.        
CIDMOD     05  FILLER                  PIC X(26)   VALUE SPACES.        
00294                                                                   
00295  01  HD-6.                                                        
00296      05  FILLER-HD-6.                                             
00297        07  FILLER-HD-6-1         PIC X(6)    VALUE 'STATE'.       
00298        07  FILLER-HD-6-2         PIC X(8)    VALUE SPACES.        
00299      05  HD-6-NSTATE             PIC XX      VALUE SPACES.        
00300      05  FILLER                  PIC X       VALUE SPACES.        
00301      05  HD-STATE                PIC X(30)   VALUE SPACES.        
00302      05  FILLER                  PIC X(92)   VALUE SPACES.        
00303                                                                   
00304  01  HD-7.                                                        
00305      05  FILLER-HD-7             PIC X(20)   VALUE SPACES.        
00306      05  HD-7-DESC               PIC X(10)   VALUE SPACES.        
00307      05  FILLER-HD-7A            PIC X(13)   VALUE SPACES.        
00308      05  HD-CARRIER              PIC X       VALUE SPACES.        
00309                                                                   
00310  01  HD-8.                                                        
00311      05  FILLER                  PIC X(132)                       
00312                                  VALUE 'GRAND TOTALS'.            
00313                                                                   
00314  01  HD-8A.                                                       
00315      05  HD-8A-RPT-CD-1-CAPTION  PIC X(10)   VALUE SPACES.        
00316      05  FILLER                  PIC X       VALUE SPACES.        
00317      05  HD-8A-RPT-CODE-1        PIC X(10)   VALUE SPACES.        
00318      05  FILLER                  PIC X(111)  VALUE SPACES.        
00319                                                                   
00320  01  HD-8B.                                                       
00321      05  HD-8B-RPT-CD-2-CAPTION  PIC X(10)   VALUE SPACES.        
00322      05  FILLER                  PIC X       VALUE SPACES.        
00323      05  HD-8B-RPT-CODE-2        PIC X(10)   VALUE SPACES.        
00324      05  FILLER                  PIC X(111)  VALUE SPACES.        
00325                                                                   
072709 01  HD-8C.                                                       
072709     05  HD-8C-US2-CAPTION       PIC X(10)   VALUE 'USER SEL 2'.        
072709     05  FILLER                  PIC X       VALUE SPACES.        
072709     05  HD-8C-USER-SEL-2        PIC X(10)   VALUE SPACES.        
072709     05  FILLER                  PIC X(111)  VALUE SPACES.        
072709                                                                   
031511 01  HD-8D.                                                       
031511     05  HD-8D-US5-CAPTION       PIC X(10)   VALUE 'USER SEL 5'.
031511     05  FILLER                  PIC X       VALUE SPACES.
031511     05  HD-8D-USER-SEL-5        PIC X(10)   VALUE SPACES.
031511     05  FILLER                  PIC X(111)  VALUE SPACES.
031511
00326  01  HD-9.                                                        
00327      05  HD-9-1          PIC X(20)   VALUE '               NET  '.
00328      05  FILLER          PIC X(5)    VALUE SPACES.                
00329      05  HD-9-LF-OVRD-1  PIC X(6).                                
00330      05  FILLER          PIC X(8)    VALUE SPACES.                
00331      05  HD-9-LF-OVRD-2  PIC X(6).                                
00332      05  FILLER          PIC X(9)    VALUE SPACES.                
00333      05  HD-9-LF-OVRD-3  PIC X(6).                                
00334      05  FILLER          PIC X(8)    VALUE SPACES.                
00335      05  HD-9-AH-OVRD-1  PIC X(6).                                
00336      05  FILLER          PIC X(8)    VALUE SPACES.                
00337      05  HD-9-AH-OVRD-2  PIC X(6).                                
00338      05  FILLER          PIC X(9)    VALUE SPACES.                
00339      05  HD-9-AH-OVRD-3  PIC X(6).                                
00340      05  FILLER          PIC X(9)    VALUE SPACES.                
00341      05  FILLER          PIC X(20)   VALUE 'TOTAL        TOTAL  '.
00342                                                                   
00343  01  HD-10.                                                       
00344      05  HD-10-1         PIC X(21)  VALUE '             ACTIVITY'.
00345      05  FILLER          PIC X(19)   VALUE '   BENEFITS       P'. 
00346      05  FILLER          PIC X(20)   VALUE 'REMIUM        CLAIMS'.
00347      05  FILLER          PIC X(20)   VALUE '       BENEFITS     '.
00348      05  FILLER          PIC X(20)   VALUE '  PREMIUM        CLA'.
00349      05  FILLER          PIC X(20)   VALUE 'IMS        PREMIUM  '.
00350      05  FILLER          PIC X(12)   VALUE '  COMMISSION'.        
00351                                                                   
00352  01  HD-9-A.                                                      
00353      12  HD-9-A-1        PIC X(34)   VALUE                        
00354              '       HI-CERT    CERT        NET '.                
00355      12  FILLER          PIC X(6)    VALUE SPACES.                
00356      12  HD-9A-LF-OVRD-1 PIC X(6).                                
00357      12  FILLER          PIC X(8)    VALUE SPACES.                
00358      12  HD-9A-LF-OVRD-2 PIC X(6).                                
00359      12  FILLER          PIC X(9)    VALUE SPACES.                
00360      12  HD-9A-LF-OVRD-3 PIC X(6).                                
00361      12  FILLER          PIC X(7)    VALUE SPACES.                
00362      12  HD-9A-AH-OVRD-1 PIC X(6).                                
00363      12  FILLER          PIC X(9)    VALUE SPACES.                
00364      12  HD-9A-AH-OVRD-2 PIC X(6).                                
00365      12  FILLER          PIC X(29)   VALUE                        
00366              '        TOTAL         TOTAL  '.                     
00367                                                                   
00368  01  HD-10-A.                                                     
00369      12  HD-10-A-1       PIC X(36)   VALUE                        
00370              '         DATE    COUNT      ACTIVITY'.              
00371      12  FILLER          PIC X(8)   VALUE                         
00372              '   BENEF'.                                          
00373      12  FILLER          PIC X(44)   VALUE                        
00374              'ITS       PREMIUM        CLAIMS       PREMIU'.      
00375      12  FILLER          PIC X(44)   VALUE                        
00376              'M        CLAIMS        PREMIUM    COMMISSION'.      
00377                                                                   
00378  01  DETAIL-LINE.                                                 
00379      05  DET-TITLE.                                               
00380          10  DET-DATE.                                            
00381              15  DET-MO          PIC XXX     VALUE SPACES.        
00382              15  FILLER          PIC X       VALUE SPACES.        
00383              15  DET-YR          PIC XX      VALUE SPACES.        
00384              15  FILLER          PIC X       VALUE SPACES.        
00385          10  DET-CERTS           PIC ZZZ,ZZZ,Z99-.                
00386      05  DET-LBEN                PIC ZZZZZ,ZZZ,Z99-.              
00387      05  DET-LPRM                PIC ZZ,ZZZ,ZZ9.99-.              
00388      05  DET-LCLM                PIC ZZ,ZZZ,ZZ9.99-.              
050504     05  DET-ABEN                PIC ZZZZZZ,ZZZ,Z99-.             
00390      05  DET-APRM                PIC ZZ,ZZZ,ZZ9.99-.              
00391      05  DET-ACLM                PIC ZZ,ZZZ,ZZ9.99-.              
00392      05  DET-TPRM                PIC ZZZ,ZZZ,ZZ9.99-.             
00393      05  DET-TCOM                PIC ZZZZZ,ZZ9.99-.               
00394                                                                   
00395  01  DETAIL-LINE-2.                                               
00396      12  DET-TITLE-2.                                             
00397          16  DET-DATE-2.                                          
00398              20  DET-MO-2        PIC XXX     VALUE SPACES.        
00399              20  FILLER          PIC X       VALUE SPACES.        
00400              20  DET-YR-2        PIC XX      VALUE SPACES.        
00401              20  FILLER          PIC X(2)    VALUE SPACES.        
00402          16  DET-HIGH-CERT.                                       
00403              20  DET-HI-MO       PIC XX.                          
00404              20  DET-HI-SLASH    PIC X.                           
00405              20  DET-HI-YR       PIC XX.                          
00406              20  FILLER          PIC X(1)    VALUE SPACES.        
00407          16  DET-FILL REDEFINES                                   
00408              DET-HIGH-CERT       PIC X(6).                        
00409          16  DET-ISS-COUNT-2     PIC ZZZZZ99-.                    
00410          16  DET-CERTS-2         PIC ZZZ,ZZZ,Z99-.                
00411      12  DET-LBEN-2              PIC ZZZZZ,ZZZ,Z99-.              
00412      12  DET-LPRM-2              PIC ZZ,ZZZ,ZZ9.99-.              
00413      12  DET-LCLM-2              PIC ZZ,ZZZ,ZZ9.99-.              
00414      12  DET-APRM-2              PIC ZZ,ZZZ,ZZ9.99-.              
00415      12  DET-ACLM-2              PIC ZZ,ZZZ,ZZ9.99-.              
00416      12  DET-TPRM-2              PIC ZZZ,ZZZ,ZZ9.99-.             
00417      12  DET-TCOM-2              PIC ZZZZZ,ZZ9.99-.               
00418                                                                   
00419  01  DET-DATE-LINE.                                               
00420      12  DET-DATE-DESC           PIC X(17)   VALUE SPACES.        
00421      12  DET-DATE-MO             PIC XX.                          
00422      12  DET-DATE-SLASH-1        PIC X       VALUE '/'.           
00423      12  DET-DATE-DA             PIC XX.                          
00424      12  DET-DATE-SLASH-2        PIC X       VALUE '/'.           
00425      12  DET-DATE-YR             PIC XX.                          
00426  EJECT                                                            
00427  01  ACCUMULATORS.                                                
00428 *    12  ACCOUNT-ACCUM    OCCURS 13 TIMES.                        
122804     12  ACCOUNT-ACCUM    OCCURS 15 TIMES.                        
00429          16  AC-CERT             PIC S9(9)         COMP-3.        
00430          16  AC-ISS              PIC S9(9)         COMP-3.        
00431          16  AC-LBEN             PIC S9(11)V99     COMP-3.        
00432          16  AC-LPRM             PIC S9(9)V99      COMP-3.        
00433          16  AC-LCLM             PIC S9(9)V99      COMP-3.        
00434          16  AC-ABEN             PIC S9(9)V99      COMP-3.        
00435          16  AC-APRM             PIC S9(9)V99      COMP-3.        
00436          16  AC-ACLM             PIC S9(9)V99      COMP-3.        
00437          16  AC-TPRM             PIC S9(9)V99      COMP-3.        
00438          16  AC-TCOM             PIC S9(9)V99      COMP-3.        
00439          16  AC-DATE             PIC 9(11) COMP-3 VALUE 0.        
00440          16  AC-DATE-R.                                           
00441              20  FILLER          PIC 999.                         
00442              20  AC-DATE-CC      PIC 99.                          
00443              20  AC-DATE-YR      PIC 99.                          
00444              20  AC-DATE-MO      PIC 99.                          
00445              20  AC-DATE-DA      PIC 99.                          
00446 *    12  GROUPING-ACCUM   OCCURS 13 TIMES.                        
122804     12  GROUPING-ACCUM   OCCURS 15 TIMES.                        
00447          16  GP-CERT             PIC S9(9)         COMP-3.        
00448          16  GP-ISS              PIC S9(9)         COMP-3.        
00449          16  GP-LBEN             PIC S9(11)V99     COMP-3.        
00450          16  GP-LPRM             PIC S9(9)V99      COMP-3.        
00451          16  GP-LCLM             PIC S9(9)V99      COMP-3.        
00452          16  GP-ABEN             PIC S9(9)V99      COMP-3.        
00453          16  GP-APRM             PIC S9(9)V99      COMP-3.        
00454          16  GP-ACLM             PIC S9(9)V99      COMP-3.        
00455          16  GP-TPRM             PIC S9(9)V99      COMP-3.        
00456          16  GP-TCOM             PIC S9(9)V99      COMP-3.        
00457          16  GP-DATE             PIC 9(11) COMP-3 VALUE 0.        
00458          16  GP-DATE-R.                                           
00459              20  FILLER          PIC 999.                         
00460              20  GP-DATE-CC      PIC 99.                          
00461              20  GP-DATE-YR      PIC 99.                          
00462              20  GP-DATE-MO      PIC 99.                          
00463              20  GP-DATE-DA      PIC 99.                          
00464 *    12  CARRIER-ACCUM    OCCURS 13 TIMES.                        
122804     12  CARRIER-ACCUM    OCCURS 15 TIMES.                        
00465          16  CA-CERT             PIC S9(9)         COMP-3.        
00466          16  CA-ISS              PIC S9(9)         COMP-3.        
00467          16  CA-LBEN             PIC S9(11)V99     COMP-3.        
00468          16  CA-LPRM             PIC S9(9)V99      COMP-3.        
00469          16  CA-LCLM             PIC S9(9)V99      COMP-3.        
00470          16  CA-ABEN             PIC S9(9)V99      COMP-3.        
00471          16  CA-APRM             PIC S9(9)V99      COMP-3.        
00472          16  CA-ACLM             PIC S9(9)V99      COMP-3.        
00473          16  CA-TPRM             PIC S9(9)V99      COMP-3.        
00474          16  CA-TCOM             PIC S9(9)V99      COMP-3.        
00475          16  CA-DATE             PIC 9(11) COMP-3 VALUE 0.        
00476          16  CA-DATE-R.                                           
00477              20  FILLER          PIC 999.                         
00478              20  CA-DATE-CC      PIC 99.                          
00479              20  CA-DATE-YR      PIC 99.                          
00480              20  CA-DATE-MO      PIC 99.                          
00481              20  CA-DATE-DA      PIC 99.                          
00482 *    12  STATE-ACCUM      OCCURS 13 TIMES.                        
122804     12  STATE-ACCUM      OCCURS 15 TIMES.                        
00483          16  ST-CERT             PIC S9(9)         COMP-3.        
00484          16  ST-ISS              PIC S9(9)         COMP-3.        
00485          16  ST-LBEN             PIC S9(11)V99     COMP-3.        
00486          16  ST-LPRM             PIC S9(9)V99      COMP-3.        
00487          16  ST-LCLM             PIC S9(9)V99      COMP-3.        
00488          16  ST-ABEN             PIC S9(9)V99      COMP-3.        
00489          16  ST-APRM             PIC S9(9)V99      COMP-3.        
00490          16  ST-ACLM             PIC S9(9)V99      COMP-3.        
00491          16  ST-TPRM             PIC S9(9)V99      COMP-3.        
00492          16  ST-TCOM             PIC S9(9)V99      COMP-3.        
00493          16  ST-DATE             PIC X(6).                        
00494 *    12  REPTCD-ACCUM     OCCURS 13 TIMES.                        
00494      12  REPTCD-ACCUM     OCCURS 15 TIMES.                        
00495          16  RP-CERT             PIC S9(9)         COMP-3.        
00496          16  RP-ISS              PIC S9(9)         COMP-3.        
00497          16  RP-LBEN             PIC S9(11)V99     COMP-3.        
00498          16  RP-LPRM             PIC S9(9)V99      COMP-3.        
00499          16  RP-LCLM             PIC S9(9)V99      COMP-3.        
00500          16  RP-ABEN             PIC S9(9)V99      COMP-3.        
00501          16  RP-APRM             PIC S9(9)V99      COMP-3.        
00502          16  RP-ACLM             PIC S9(9)V99      COMP-3.        
00503          16  RP-TPRM             PIC S9(9)V99      COMP-3.        
00504          16  RP-TCOM             PIC S9(9)V99      COMP-3.        
00505          16  RP-DATE             PIC 9(11) COMP-3 VALUE 0.        
00506          16  RP-DATE-R.                                           
00507              20  FILLER          PIC 999.                         
00508              20  RP-DATE-CC      PIC 99.                          
00509              20  RP-DATE-YR      PIC 99.                          
00510              20  RP-DATE-MO      PIC 99.                          
00511              20  RP-DATE-DA      PIC 99.                          
031511     12  REPTCD2-ACCUM     OCCURS 15 TIMES.                        
031511         16  RP2-CERT             PIC S9(9)         COMP-3.        
031511         16  RP2-ISS              PIC S9(9)         COMP-3.        
031511         16  RP2-LBEN             PIC S9(11)V99     COMP-3.        
031511         16  RP2-LPRM             PIC S9(9)V99      COMP-3.        
031511         16  RP2-LCLM             PIC S9(9)V99      COMP-3.        
031511         16  RP2-ABEN             PIC S9(9)V99      COMP-3.        
031511         16  RP2-APRM             PIC S9(9)V99      COMP-3.        
031511         16  RP2-ACLM             PIC S9(9)V99      COMP-3.        
031511         16  RP2-TPRM             PIC S9(9)V99      COMP-3.        
031511         16  RP2-TCOM             PIC S9(9)V99      COMP-3.        
031511         16  RP2-DATE             PIC 9(11) COMP-3 VALUE 0.        
031511         16  RP2-DATE-R.                                           
031511             20  FILLER          PIC 999.                         
031511             20  RP2-DATE-CC      PIC 99.                          
031511             20  RP2-DATE-YR      PIC 99.                          
031511             20  RP2-DATE-MO      PIC 99.                          
031511             20  RP2-DATE-DA      PIC 99.                          
00512      12  GRAND-TOTALS.                                            
00513          16  GR-CERT             PIC S9(9)       COMP-3 VALUE +0. 
00514          16  GR-ISS              PIC S9(9)       COMP-3 VALUE +0. 
00515          16  GR-LBEN             PIC S9(11)V99   COMP-3 VALUE +0. 
00516          16  GR-LPRM             PIC S9(9)V99    COMP-3 VALUE +0. 
00517          16  GR-LCLM             PIC S9(9)V99    COMP-3 VALUE +0. 
00518          16  GR-ABEN             PIC S9(9)V99    COMP-3 VALUE +0. 
00519          16  GR-APRM             PIC S9(9)V99    COMP-3 VALUE +0. 
00520          16  GR-ACLM             PIC S9(9)V99    COMP-3 VALUE +0. 
00521          16  GR-TPRM             PIC S9(9)V99    COMP-3 VALUE +0. 
00522          16  GR-TCOM             PIC S9(9)V99    COMP-3 VALUE +0. 
00523 *    12  GRAND-ACCUM      OCCURS 13 TIMES.                        
122804     12  GRAND-ACCUM      OCCURS 15 TIMES.                        
00524          16  GT-CERT             PIC S9(9)         COMP-3.        
00525          16  GT-ISS              PIC S9(9)         COMP-3.        
00526          16  GT-LBEN             PIC S9(11)V99     COMP-3.        
00527          16  GT-LPRM             PIC S9(9)V99      COMP-3.        
00528          16  GT-LCLM             PIC S9(9)V99      COMP-3.        
00529          16  GT-ABEN             PIC S9(9)V99      COMP-3.        
00530          16  GT-APRM             PIC S9(9)V99      COMP-3.        
00531          16  GT-ACLM             PIC S9(9)V99      COMP-3.        
00532          16  GT-TPRM             PIC S9(9)V99      COMP-3.        
00533          16  GT-TCOM             PIC S9(9)V99      COMP-3.        
00534          16  GT-DATE             PIC 9(11) COMP-3 VALUE 0.        
00535          16  GT-DATE-R.                                           
00536              20  FILLER          PIC 999.                         
00537              20  GT-DATE-CC      PIC 99.                          
00538              20  GT-DATE-YR      PIC 99.                          
00539              20  GT-DATE-MO      PIC 99.                          
00540              20  GT-DATE-DA      PIC 99.                          
00541      12  LAST-12-ACCOUNT-ACCUM.                                   
00542          16  L12AC-LBEN          PIC S9(11)V99   COMP-3 VALUE +0. 
00543          16  L12AC-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00544          16  L12AC-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00545          16  L12AC-ABEN          PIC S9(9)V99    COMP-3 VALUE +0. 
00546          16  L12AC-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00547          16  L12AC-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00548          16  L12AC-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00549          16  L12AC-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00550      12  YTD-ACCOUNT-ACCUM.                                       
00551          16  YTDAC-LBEN          PIC S9(11)V99   COMP-3 VALUE +0. 
00552          16  YTDAC-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00553          16  YTDAC-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00554          16  YTDAC-ABEN          PIC S9(9)V99    COMP-3 VALUE +0. 
00555          16  YTDAC-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00556          16  YTDAC-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00557          16  YTDAC-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00558          16  YTDAC-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00559      12  LAST-12-GROUPING-ACCUM.                                  
00560          16  L12GP-LBEN          PIC S9(11)V99   COMP-3 VALUE +0. 
00561          16  L12GP-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00562          16  L12GP-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00563          16  L12GP-ABEN          PIC S9(9)V99    COMP-3 VALUE +0. 
00564          16  L12GP-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00565          16  L12GP-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00566          16  L12GP-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00567          16  L12GP-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00568      12  LAST-12-CARRIER-ACCUM.                                   
00569          16  L12CA-LBEN          PIC S9(11)V99   COMP-3 VALUE +0. 
00570          16  L12CA-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00571          16  L12CA-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00572          16  L12CA-ABEN          PIC S9(9)V99    COMP-3 VALUE +0. 
00573          16  L12CA-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00574          16  L12CA-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00575          16  L12CA-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00576          16  L12CA-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00577      12  LAST-12-STATE-ACCUM.                                     
00578          16  L12ST-LBEN          PIC S9(11)V99   COMP-3 VALUE +0. 
00579          16  L12ST-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00580          16  L12ST-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00581          16  L12ST-ABEN          PIC S9(9)V99    COMP-3 VALUE +0. 
00582          16  L12ST-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00583          16  L12ST-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00584          16  L12ST-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00585          16  L12ST-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00586      12  LAST-12-REPTCD-ACCUM.                                    
00587          16  L12RP-LBEN          PIC S9(11)V99   COMP-3 VALUE +0. 
00588          16  L12RP-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00589          16  L12RP-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00590          16  L12RP-ABEN          PIC S9(9)V99    COMP-3 VALUE +0. 
00591          16  L12RP-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00592          16  L12RP-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00593          16  L12RP-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00594          16  L12RP-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
031511     12  LAST-12-REPTCD2-ACCUM.                                    
031511         16  L12RP2-LBEN         PIC S9(11)V99   COMP-3 VALUE +0. 
031511         16  L12RP2-LPRM         PIC S9(9)V99    COMP-3 VALUE +0. 
031511         16  L12RP2-LCLM         PIC S9(9)V99    COMP-3 VALUE +0. 
031511         16  L12RP2-ABEN         PIC S9(9)V99    COMP-3 VALUE +0. 
031511         16  L12RP2-APRM         PIC S9(9)V99    COMP-3 VALUE +0. 
031511         16  L12RP2-ACLM         PIC S9(9)V99    COMP-3 VALUE +0. 
031511         16  L12RP2-TPRM         PIC S9(9)V99    COMP-3 VALUE +0. 
031511         16  L12RP2-TCOM         PIC S9(9)V99    COMP-3 VALUE +0. 
082609     12  LAST-12-GRAND-ACCUM.                                  
082609         16  L12GT-LBEN          PIC S9(11)V99   COMP-3 VALUE +0. 
082609         16  L12GT-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
082609         16  L12GT-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
082609         16  L12GT-ABEN          PIC S9(9)V99    COMP-3 VALUE +0. 
082609         16  L12GT-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
082609         16  L12GT-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
082609         16  L12GT-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
082609         16  L12GT-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00595      12  YTD-GROUPING-ACCUM.                                      
00596          16  YTDGP-LBEN          PIC S9(11)V99   COMP-3 VALUE +0. 
00597          16  YTDGP-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00598          16  YTDGP-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00599          16  YTDGP-ABEN          PIC S9(9)V99    COMP-3 VALUE +0. 
00600          16  YTDGP-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00601          16  YTDGP-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00602          16  YTDGP-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00603          16  YTDGP-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00604      12  YTD-CARRIER-ACCUM.                                       
00605          16  YTDCA-LBEN          PIC S9(11)V99   COMP-3 VALUE +0. 
00606          16  YTDCA-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00607          16  YTDCA-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00608          16  YTDCA-ABEN          PIC S9(9)V99    COMP-3 VALUE +0. 
00609          16  YTDCA-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00610          16  YTDCA-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00611          16  YTDCA-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00612          16  YTDCA-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00613      12  YTD-STATE-ACCUM.                                         
00614          16  YTDST-LBEN          PIC S9(11)V99   COMP-3 VALUE +0. 
00615          16  YTDST-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00616          16  YTDST-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00617          16  YTDST-ABEN          PIC S9(9)V99    COMP-3 VALUE +0. 
00618          16  YTDST-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00619          16  YTDST-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00620          16  YTDST-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00621          16  YTDST-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00622      12  YTD-REPTCD-ACCUM.                                        
00623          16  YTDRP-LBEN          PIC S9(11)V99   COMP-3 VALUE +0. 
00624          16  YTDRP-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00625          16  YTDRP-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00626          16  YTDRP-ABEN          PIC S9(9)V99    COMP-3 VALUE +0. 
00627          16  YTDRP-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00628          16  YTDRP-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00629          16  YTDRP-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00630          16  YTDRP-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
031511     12  YTD-REPTCD2-ACCUM.                                        
031511         16  YTDRP2-LBEN         PIC S9(11)V99   COMP-3 VALUE +0. 
031511         16  YTDRP2-LPRM         PIC S9(9)V99    COMP-3 VALUE +0. 
031511         16  YTDRP2-LCLM         PIC S9(9)V99    COMP-3 VALUE +0. 
031511         16  YTDRP2-ABEN         PIC S9(9)V99    COMP-3 VALUE +0. 
031511         16  YTDRP2-APRM         PIC S9(9)V99    COMP-3 VALUE +0. 
031511         16  YTDRP2-ACLM         PIC S9(9)V99    COMP-3 VALUE +0. 
031511         16  YTDRP2-TPRM         PIC S9(9)V99    COMP-3 VALUE +0. 
031511         16  YTDRP2-TCOM         PIC S9(9)V99    COMP-3 VALUE +0. 
082609     12  YTD-GRAND-ACCUM.                                      
082609         16  YTDGT-LBEN          PIC S9(11)V99   COMP-3 VALUE +0. 
082609         16  YTDGT-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
082609         16  YTDGT-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
082609         16  YTDGT-ABEN          PIC S9(9)V99    COMP-3 VALUE +0. 
082609         16  YTDGT-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
082609         16  YTDGT-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
082609         16  YTDGT-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
082609         16  YTDGT-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00631      12  ZERO-OCCURS-ACCUM.                                       
00632          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00633          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00634          16  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00635          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00636          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00637          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00638          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00639          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00640          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00641          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00642          16  FILLER              PIC X(6)       VALUE LOW-VALUES. 
00643      12  ZERO-ACCUM.                                              
00644          16  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00645          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00646          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00647          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00648          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00649          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00650          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00651          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00652      12  ZERO-GRAND-TOTALS.                                       
00653          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00654          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00655          16  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00656          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00657          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00658          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00659          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00660          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00661          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00662          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 


122804 01  PYTD-ACCOUNT-ACCUM.
122804     12  PYTDAC-LBEN             PIC S9(11)V99   COMP-3 VALUE +0.
122804     12  PYTDAC-LPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDAC-LCLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDAC-ABEN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDAC-APRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDAC-ACLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDAC-TPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDAC-TCOM             PIC S9(9)V99    COMP-3 VALUE +0.

122804 01  PYTD-GROUPING-ACCUM.
122804     12  PYTDGP-LBEN             PIC S9(11)V99   COMP-3 VALUE +0.
122804     12  PYTDGP-LPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGP-LCLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGP-ABEN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGP-APRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGP-ACLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGP-TPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGP-TCOM             PIC S9(9)V99    COMP-3 VALUE +0.

122804 01  PYTD-CARRIER-ACCUM.
122804     12  PYTDCA-LBEN             PIC S9(11)V99   COMP-3 VALUE +0.
122804     12  PYTDCA-LPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDCA-LCLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDCA-ABEN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDCA-APRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDCA-ACLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDCA-TPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDCA-TCOM             PIC S9(9)V99    COMP-3 VALUE +0.

122804 01  PYTD-STATE-ACCUM.
122804     12  PYTDST-LBEN             PIC S9(11)V99   COMP-3 VALUE +0.
122804     12  PYTDST-LPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDST-LCLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDST-ABEN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDST-APRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDST-ACLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDST-TPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDST-TCOM             PIC S9(9)V99    COMP-3 VALUE +0.

122804 01  PYTD-REPTCD-ACCUM.
122804     12  PYTDRP-LBEN             PIC S9(11)V99   COMP-3 VALUE +0.
122804     12  PYTDRP-LPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDRP-LCLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDRP-ABEN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDRP-APRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDRP-ACLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDRP-TPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDRP-TCOM             PIC S9(9)V99    COMP-3 VALUE +0.

031511 01  PYTD-REPTCD2-ACCUM.
031511     12  PYTDRP2-LBEN            PIC S9(11)V99   COMP-3 VALUE +0.
031511     12  PYTDRP2-LPRM            PIC S9(9)V99    COMP-3 VALUE +0.
031511     12  PYTDRP2-LCLM            PIC S9(9)V99    COMP-3 VALUE +0.
031511     12  PYTDRP2-ABEN            PIC S9(9)V99    COMP-3 VALUE +0.
031511     12  PYTDRP2-APRM            PIC S9(9)V99    COMP-3 VALUE +0.
031511     12  PYTDRP2-ACLM            PIC S9(9)V99    COMP-3 VALUE +0.
031511     12  PYTDRP2-TPRM            PIC S9(9)V99    COMP-3 VALUE +0.
031511     12  PYTDRP2-TCOM            PIC S9(9)V99    COMP-3 VALUE +0.
031511
082609 01  PYTD-GRAND-ACCUM.
082609     12  PYTDGT-LBEN             PIC S9(11)V99   COMP-3 VALUE +0.
082609     12  PYTDGT-LPRM             PIC S9(9)V99    COMP-3 VALUE +0.
082609     12  PYTDGT-LCLM             PIC S9(9)V99    COMP-3 VALUE +0.
082609     12  PYTDGT-ABEN             PIC S9(9)V99    COMP-3 VALUE +0.
082609     12  PYTDGT-APRM             PIC S9(9)V99    COMP-3 VALUE +0.
082609     12  PYTDGT-ACLM             PIC S9(9)V99    COMP-3 VALUE +0.
082609     12  PYTDGT-TPRM             PIC S9(9)V99    COMP-3 VALUE +0.
082609     12  PYTDGT-TCOM             PIC S9(9)V99    COMP-3 VALUE +0.

00663  EJECT                                                            
00664  01  PRINT-DECISION-TABLE COMP-3.                                 
00665      05  PRINT-TABLE      OCCURS 12 TIMES.                        
00666          10  PLBEN               PIC S9(9)V99.                    
00667          10  PLPRM               PIC S9(9)V99.                    
00668          10  PLCLM               PIC S9(9)V99.                    
00669          10  PABEN               PIC S9(9)V99.                    
00670          10  PAPRM               PIC S9(9)V99.                    
00671          10  PACLM               PIC S9(9)V99.                    
00672          10  PTCOM               PIC S9(9)V99.                    
00673                                                                   
00674  01  PRINT-ZERO-TABLE  COMP-3.                                    
00675      05  FILLER                  PIC S9(9)V99    VALUE ZEROS.     
00676      05  FILLER                  PIC S9(9)V99    VALUE ZEROS.     
00677      05  FILLER                  PIC S9(9)V99    VALUE ZEROS.     
00678      05  FILLER                  PIC S9(9)V99    VALUE ZEROS.     
00679      05  FILLER                  PIC S9(9)V99    VALUE ZEROS.     
00680      05  FILLER                  PIC S9(9)V99    VALUE ZEROS.     
00681      05  FILLER                  PIC S9(9)V99    VALUE ZEROS.     
00682                                                                   
00683  01  WS.                                                          
00684      12  WS-RETURN-CODE          PIC S9(4)    COMP   VALUE ZERO.  
00685      12  WS-ZERO                 PIC S9              VALUE ZERO.  
00686      12  WS-ABEND-MESSAGE        PIC X(80)           VALUE SPACES.
00687      12  WS-ABEND-FILE-STATUS    PIC XX              VALUE ZERO.  
00688      12  WS-EP-ACC-EXPIRES       PIC 9(11).                       
00689      12  WS-EP-ACC-EXPIRES-R REDEFINES WS-EP-ACC-EXPIRES.         
00690          16  FILLER              PIC 999.                         
00691          16  EP-ACC-EXP-CC       PIC 99.                          
00692          16  EP-ACC-EXP-YR       PIC 99.                          
00693          16  EP-ACC-EXP-MO       PIC 99.                          
00694          16  EP-ACC-EXP-DA       PIC 99.                          
00695      12  WS-EP-ACC-HI-CERT       PIC 9(11).                       
00696      12  WS-EP-ACC-HI-CERT-R REDEFINES WS-EP-ACC-HI-CERT.         
00697          16  FILLER              PIC 999.                         
00698          16  EP-ACC-HI-CC        PIC 99.                          
00699          16  EP-ACC-HI-YR        PIC 99.                          
00700          16  EP-ACC-HI-MO        PIC 99.                          
00701          16  EP-ACC-HI-DA        PIC 99.                          
00702      12  WS-EP-MTH-HI-CERT       PIC 9(11).                       
00703      12  WS-EP-MTH-HI-CERT-R REDEFINES WS-EP-MTH-HI-CERT.         
00704          16  FILLER              PIC 999.                         
00705          16  EP-MTH-HI-CC        PIC 99.                          
00706          16  EP-MTH-HI-YR        PIC 99.                          
00707          16  EP-MTH-HI-MO        PIC 99.                          
00708          16  EP-MTH-HI-DA        PIC 99.                          
00709      12  WS-EP-DTE               PIC 9(07).                       
00710      12  WS-EP-DTE-R REDEFINES WS-EP-DTE.                         
00711          16  FILLER              PIC 9.                           
00712          16  EP-RUN-CC           PIC 99.                          
00713          16  EP-RUN-YR           PIC 99.                          
00714          16  EP-RUN-MO           PIC 99.                          
00715  EJECT                                                            
00716  01  CONVERSION-DATE             PIC 9(08) VALUE ZEROS.           
00717  01  CONVERSION-DATE-R REDEFINES CONVERSION-DATE.                 
00718      05  CONV-CC                 PIC XX.                          
00719      05  CONV-YR                 PIC XX.                          
00720      05  CONV-MO                 PIC XX.                          
00721      05  CONV-DA                 PIC XX.                          
00722                                                                   
00723  01  COMPARE-DATE-TABLE.                                          
00724 *    05  COMPARE-DTS    OCCURS 13 TIMES.                          
122804     05  COMPARE-DTS    OCCURS 15 TIMES.                          
00725          10  COMPARE-DT.                                          
00726              15  COMP-CCYY       PIC 9(4).                        
00727              15  COMP-CCYR REDEFINES COMP-CCYY.                   
00728                  20  COMP-CC     PIC 99.                          
00729                  20  COMP-YR     PIC 99.                          
00730              15  COMP-MO         PIC 99.                          
00731                                                                   
00732  01  COMPARE-DATE9TABLE REDEFINES COMPARE-DATE-TABLE.             
00733 *    05  COMPARE9DTS    OCCURS 13 TIMES.                          
00733      05  COMPARE9DTS    OCCURS 15 TIMES.                          
00734          10  COMPARE9DT          PIC 9(06).                       
00735                                                                   
122804 01  YEAR-END-DT                 PIC 9(06)  VALUE ZEROS.
122804 01  TWO-YEAR-ENDS-AGO           PIC 9(06)  VALUE ZEROS.
122804 01  TWO-YEARS-AGO               PIC 9(06)  VALUE ZEROS.
00736  01  YEAR-OLD-DATE               PIC 9(06).                       
00737                                                                   
00738  01  FISCAL-DATE         REDEFINES YEAR-OLD-DATE.                 
00739      05  FISCAL-CCYY             PIC 9(04).                       
00740      05  FISCAL-CCYR REDEFINES FISCAL-CCYY.                       
00741          10  FISCAL-CC           PIC 99.                          
00742          10  FISCAL-YR           PIC 99.                          
00743      05  FISCAL-MO               PIC 99.                          
00744                                                                   
00745  01  SAVE-EPX.                                                    
00746      12  SAVE-CNTL.                                               
00747          16  S-EPX-CNTL-1.                                        
00748              20  S-EPX-CNTL-GA       PIC X(10).                   
00749              20  S-EPX-CNTL-ACCT     PIC X(10).                   
00750          16  S-EPX-CNTL-2.                                        
00751              20  S-EPX-CARR          PIC X.                       
00752              20  S-EPX-GRP           PIC X(6).                    
00753          16  FILLER                  PIC X(12).                   
00754      12  SAVE-ALT-CNTL  REDEFINES  SAVE-CNTL.                     
00755          16  S-EPX-A-RPT-CD-1        PIC X(10).                   
00756          16  S-EPX-A-CARR            PIC X.                       
00757          16  S-EPX-A-GROUP           PIC X(6).                    
00758          16  S-EPX-A-RPT-CD-2        PIC X(10).                   
00759          16  S-EPX-A-STATE           PIC XX.                      
00760          16  S-EPX-A-ACCT            PIC X(10).                   
00761                                                                   
102004 01  SAVE-ACCT-STATUS            PIC X(12).
00762  01  SAVE-ACCT-NAME              PIC X(30).                       
CIDMOD 01  SAVE-ACCT-CITY              PIC X(30).                       
00763                                                                   
00764  01  SAVE-DATES.                                                  
00765      05  SAVE-AM-EXPIRES         PIC 9(11).                       
00766      05  SAVE-AM-EXPIRES-R REDEFINES SAVE-AM-EXPIRES.             
00767          12  FILLER              PIC 999.                         
00768          12  SAVE-AM-EXP-CC      PIC 99.                          
00769          12  SAVE-AM-EXP-YR      PIC 99.                          
00770          12  SAVE-AM-EXP-MO      PIC 99.                          
00771          12  SAVE-AM-EXP-DA      PIC 99.                          
00772      05  SAVE-AM-HI-CERT         PIC 9(11).                       
00773      05  SAVE-AM-HI-CERT-R REDEFINES SAVE-AM-HI-CERT.             
00774          12  FILLER              PIC 999.                         
00775          12  SAVE-AM-HI-CC       PIC 99.                          
00776          12  SAVE-AM-HI-YR       PIC 99.                          
00777          12  SAVE-AM-HI-MO       PIC 99.                          
00778          12  SAVE-AM-HI-DA       PIC 99.                          
00779      05  SAVE-HER-HI-CERT        PIC 9(11).                       
00780      05  SAVE-HER-HI-CERT-R REDEFINES SAVE-HER-HI-CERT.           
00781          12  FILLER              PIC 999.                         
00782          12  SAVE-HER-HI-CC      PIC 99.                          
00783          12  SAVE-HER-HI-YR      PIC 99.                          
00784          12  SAVE-HER-HI-MO      PIC 99.                          
00785          12  SAVE-HER-HI-DA      PIC 99.                          
00786                                                                   
00787  01  EP-DATE.                                                     
00788      05  E-CC                    PIC 99      VALUE ZEROS.         
00789      05  E-YR                    PIC 99      VALUE ZEROS.         
00790      05  E-MO                    PIC 99      VALUE ZEROS.         
00791                                                                   
00792  01  EP9DATE REDEFINES EP-DATE   PIC 9(6).                        
00793  EJECT                                                            
00794  01  WS-BIN-DATES.                                                
00795      12  BIN-AC-DATE             PIC XX      VALUE SPACES.        
00796      12  BIN-EP-HI-CERT          PIC XX      VALUE SPACES.        
00797      12  BIN-MTH-HI-CERT         PIC XX      VALUE SPACES.        
00798      12  BIN-GP-DATE             PIC XX      VALUE SPACES.        
00799      12  BIN-RP-DATE             PIC XX      VALUE SPACES.        
00800      12  BIN-CA-DATE             PIC XX      VALUE SPACES.        
00801      12  BIN-GT-DATE             PIC XX      VALUE SPACES.        
00802                                                                   
00803  COPY ELCDATE.                                                    
00804                                                                   
00805                                  COPY ELCDTECX.                   
00806                                                                   
00807                                  COPY ELCDTEVR.                   
122304 LINKAGE SECTION.                                                 
122304                                                                  
122304 01  PARM.                                                        
122304     05  PARM-LENGTH BINARY  PICTURE IS S9(4).                    
122304     05  PARM-VALUE  DISPLAY PICTURE IS X(100).                   
122304                                                                  
00808  EJECT                                                            
00809  PROCEDURE DIVISION USING PARM.
00810                                                                   
00811  0000-START-HERE.                                                 
00812                     COPY ELCDTERX.                                
00813                                                                   
00814      MOVE 0                      TO SAVE-HER-HI-CERT.             
00815      MOVE LIFE-OVERRIDE-L6       TO  HD-9A-LF-OVRD-1              
00816                                      HD-9A-LF-OVRD-2              
00817                                      HD-9A-LF-OVRD-3              
00818                                      HD-9-LF-OVRD-1               
00819                                      HD-9-LF-OVRD-2               
00820                                      HD-9-LF-OVRD-3.              
00821      MOVE AH-OVERRIDE-L6         TO  HD-9A-AH-OVRD-1              
00822                                      HD-9A-AH-OVRD-2              
00823                                      HD-9-AH-OVRD-1               
00824                                      HD-9-AH-OVRD-2               
00825                                      HD-9-AH-OVRD-3.              
00826      MOVE CLAS-REPORT-CD1-CAPTION                                 
00827                                  TO HD-1-RPT-CD-1                 
00828                                     HD-8A-RPT-CD-1-CAPTION.       
00829      MOVE CLAS-REPORT-CD2-CAPTION                                 
00830                                  TO HD-1-RPT-CD-2                 
00831                                     HD-8B-RPT-CD-2-CAPTION.       
00832                                                                   
00833      MOVE DTE-CONV-DT            TO CONVERSION-DATE.              
00834      MOVE WS-CURRENT-DATE        TO HD-IPL-DATE.                  
00835      MOVE COMPANY-NAME           TO HD-COMPANY-NAME.              
00836      MOVE ALPH-DATE              TO HD-ALPHA-DATE.                
00837                                                                   
00838      IF DTE-QTR-CO = '1'                                          
00839          MOVE '1'                TO QTR-COMP.                     
122304     IF PARM-LENGTH > +0
              IF PARM-VALUE = 'SPECIAL'
                 MOVE 'SINGLE FEE EQUIVALENT'  TO
                      HD-1-ST-SINGLE-FEE
                      HD-1-RC1-SINGLE-FEE
                      HD-1-RC2-SINGLE-FEE
                      HD-1-GRP-SINGLE-FEE
                      HD-1-CAR-SINGLE-FEE
                      HD-1-BUS-SINGLE-FEE
                      HD-1-AGCY-SINGLE-FEE
                      HD-1-GT-SINGLE-FEE
072709                HD-1-US2-SINGLE-FEE
031511                HD-1-US5-SINGLE-FEE
              END-IF
           END-IF
              
00840      .                                                            
00841  0100-SORT-ROUTINE SECTION.                                       
00842                                                                   
00843      SORT SORT-FILE ASCENDING KEY SORT-CONTROL-74
CIDMOD                       SORT-HI-CERT
00844          USING EARNED-PREM                                        
00845          OUTPUT PROCEDURE 0200-PRINT-ROUTINE THRU 9099-EXIT.      
00846                                                                   
00847      IF SORT-RETURN NOT = ZEROS
00848          MOVE '0101'             TO WS-RETURN-CODE                
00849          GO TO ABEND-PGM.                                         
00850                                                                   
00851      GOBACK.                                                      
00852  EJECT                                                            
00853  0200-PRINT-ROUTINE SECTION.                                      

00854                                                                   
00855      OPEN OUTPUT PRNT.                                            
CIDMOD     OPEN OUTPUT SELECT-FILE.                                     
00856                                                                   
00857  0210-CLEAR-CNTRS.                                                
00858      MOVE ZERO                   TO X1.                           
00859      PERFORM 0220-ZERO-PRINT-TABLE 12 TIMES.                      
00860      MOVE ZERO                   TO X1.                           
00861 *    PERFORM 0230-ZERO-ACCUM-ACC 13 TIMES.                        
00861      PERFORM 0230-ZERO-ACCUM-ACC 15 TIMES.                        
00862      MOVE ZERO                   TO X1.                           
00863 *    PERFORM 0240-ZERO-ACCUM-GRP 13 TIMES.                        
00863      PERFORM 0240-ZERO-ACCUM-GRP 15 TIMES.                        
00864      MOVE ZERO                   TO X1.                           
00865      PERFORM 0250-ZERO-ACCUM-CARR 15 TIMES.                       
00866      MOVE ZERO                   TO X1.                           
00867      PERFORM 0255-ZERO-ACCUM-STATE 15 TIMES.                      
00868      MOVE ZERO                   TO X1.                           
00869      PERFORM 0260-ZERO-ACCUM-REPT 15 TIMES.                       
00870      MOVE ZERO                   TO X1.                           
00871      PERFORM 0270-ZERO-ACCUM-GRAND 15 TIMES.                      
00872      MOVE ZERO-GRAND-TOTALS      TO GRAND-TOTALS.                 
00873      MOVE ZEROS                  TO EP-CNTL.                      
00874      GO TO 0280-FIND-DATE-RANGE.                                  
00875                                                                   
00876  0220-ZERO-PRINT-TABLE.                                           
00877      ADD +1 TO X1.                                                
00878      MOVE PRINT-ZERO-TABLE       TO PRINT-TABLE (X1).             
00879                                                                   
00880  0230-ZERO-ACCUM-ACC.                                             
00881      ADD +1 TO X1.                                                
00882      MOVE ZERO-OCCURS-ACCUM      TO ACCOUNT-ACCUM (X1).           
00883                                                                   
00884  0240-ZERO-ACCUM-GRP.                                             
00885      ADD +1 TO X1.                                                
00886      MOVE ZERO-OCCURS-ACCUM      TO GROUPING-ACCUM (X1).          
00887                                                                   
00888  0250-ZERO-ACCUM-CARR.                                            
00889      ADD +1 TO X1.                                                
00890      MOVE ZERO-OCCURS-ACCUM      TO CARRIER-ACCUM (X1).           
00891                                                                   
00892  0255-ZERO-ACCUM-STATE.                                           
00893      ADD +1 TO X1.                                                
00894      MOVE ZERO-OCCURS-ACCUM      TO STATE-ACCUM (X1).             
00895                                                                   
031511 0256-ZERO-ACCUM-REPT2.                                            
031511     ADD +1 TO X1.                                                
031511     MOVE ZERO-OCCURS-ACCUM      TO REPTCD2-ACCUM (X1).            
031511                                                                   
00896  0260-ZERO-ACCUM-REPT.                                            
00897      ADD +1 TO X1.                                                
00898      MOVE ZERO-OCCURS-ACCUM      TO REPTCD-ACCUM (X1).            
00899                                                                   
00900  0270-ZERO-ACCUM-GRAND.                                           
00901      ADD +1 TO X1.                                                
00902      MOVE ZERO-OCCURS-ACCUM      TO GRAND-ACCUM (X1).             
00903                                                                   
00904  0280-FIND-DATE-RANGE.                                            
00905      IF QTR-CO                                                    
00906          IF RUN-MO = 03 OR 06 OR 09 OR 12                         
00907              MOVE '1'            TO QTR-SW.                       
00908                                                                   
00909      MOVE RUN-MO                 TO RUN-DT-MO.                    
00910      MOVE RUN-CCYY               TO RUN-DT-CCYY.                  
00911      MOVE +1                     TO X1.                           
00912      MOVE +0                     TO Y1.                           
00913                                                                   
00914      IF QTR-CO AND QTR-END                                        
00915          MOVE 'QUARTERLY'        TO HD-1-AGCY HD-1-ST HD-1-GRP    
00916                                     HD-1-CAR HD-1-BUS HD-1-RC-1   
00917                                     HD-1-RC-2                     
00918          COMPUTE COMPARE9DT (X1) = RUN9DT - 300                   
00919      ELSE                                                         
00920          COMPUTE COMPARE9DT (X1) = RUN9DT - 100.                  
00921                                                                   
00922      COMPUTE YEAR-OLD-DATE = RUN9DT - 100.                        
122804     COMPUTE TWO-YEARS-AGO = RUN9DT - 200
00923                                                                   
00924      IF DTE-CLIENT = 'AFL'                                        
00925          MOVE 06                  TO FISCAL-MO                    
00926          IF RUN-MO GREATER 06                                     
00927             MOVE RUN-CCYY       TO FISCAL-CCYY                    
00928          ELSE                                                     
00929             COMPUTE FISCAL-CCYY = RUN-CCYY - 1.                   
00930                                                                   
00931  0290-BUILD-DATE-TABLE.                                           

122804     IF (COMP-MO (X1) = 12)
122804        AND (TWO-YEAR-ENDS-AGO = ZEROS)
122804        COMPUTE TWO-YEAR-ENDS-AGO = COMPARE9DT (X1) - 100
122804        MOVE COMPARE9DT (X1)     TO YEAR-END-DT
122804     END-IF

00932      ADD +1 TO X1 Y1.                                             
00933                                                                   
00934      IF X1 GREATER +13
122804        MOVE TWO-YEARS-AGO       TO COMPARE9DT (14)
122804        MOVE TWO-YEAR-ENDS-AGO   TO COMPARE9DT (15)
              PERFORM VARYING X1 FROM +1 BY +1 UNTIL
                 X1 > 15
                 MOVE X1               TO DISP-X1
                 DISPLAY ' COMPARE DATE ' DISP-X1 ' ' COMPARE9DT (X1)
              END-PERFORM
              DISPLAY ' YEAR END DATE ' YEAR-END-DT
00935         GO TO 0310-READ-EARNED-PREM-EXTRACT
           END-IF
00936                                                                   
00937      IF QTR-CO AND QTR-END                                        
00938          COMPUTE COMPARE9DT (X1) = COMPARE9DT (Y1) + 0003         
00939      ELSE                                                         
00940          COMPUTE COMPARE9DT (X1) = COMPARE9DT (Y1) + 0001.        
00941                                                                   
00942      IF COMP-MO (X1) GREATER 12                                   
00943          COMPUTE COMPARE9DT (X1) = COMPARE9DT (X1) + 0088.        
00944                                                                   
00945      GO TO 0290-BUILD-DATE-TABLE.                                 
00946  EJECT                                                            
00947  0310-READ-EARNED-PREM-EXTRACT.                                   
00948      RETURN SORT-FILE INTO WS-EXTR-REC                            
00949                           AT END MOVE '1' TO EOF-EPX              
00950                                  GO TO 1500-PASS-NUMBER-CHANGE.   
00952      IF DTE-FMT-OPT = 2                                           
00953          IF EP-PASS-NO NOT = '1'                                  
00954              GO TO 0310-READ-EARNED-PREM-EXTRACT.                 
00955                                                                   
00956      IF DTE-FMT-OPT = 3                                           
00957          IF EP-PASS-NO NOT = '2'                                  
00958              GO TO 0310-READ-EARNED-PREM-EXTRACT.                 
00959                                                                   
00960      IF DTE-FMT-OPT = 4                                           
00961          IF EP-PASS-NO NOT = '3'                                  
00962              GO TO 0310-READ-EARNED-PREM-EXTRACT.                 
00963                                                                   
00964      IF DTE-FMT-OPT = 5                                           
012517         IF EP-PASS-NO NOT = '4' and '8'
00966              GO TO 0310-READ-EARNED-PREM-EXTRACT.                 
00967                                                                   
00968      IF DTE-FMT-OPT = 6                                           
00969          IF EP-PASS-NO NOT = '5'                                  
00970              GO TO 0310-READ-EARNED-PREM-EXTRACT.                 
00971                                                                   
00972      IF FIRST-TIME                                                
00973          MOVE EP-PASS-NO         TO PASS-NUMBER                   
00974          MOVE 'N'                TO FIRST-TIME-SWITCH             
00975          GO TO 0340-ACCUMULATE.                                   
00976                                                                   
00977  0320-CHECK-EXTRACT-SEQ.                                          
00978      IF EP-PASS-NO NOT = PASS-NUMBER                              
00979          GO TO 1500-PASS-NUMBER-CHANGE.                           

           add +1 to in-cnt
00980                                                                   
00981      IF EP-CNTL LESS THAN SAVE-EPX                                
00982          DISPLAY 'EARNED PREMIUM EXTRACT OUT OF SEQUENCE'         
00983          DISPLAY 'SAVE = ' SAVE-EPX                               
00984          DISPLAY 'READ = ' EP-CNTL                                
00985          MOVE '0610'             TO WS-RETURN-CODE                
00986          GO TO ABEND-PGM.                                         
00987                                                                   
00988  0330-COMPARE-RECORDS.                                            
00989      IF PASS-NUMBER = '1'                                         
00990          IF S-EPX-CNTL-1 NOT = EP-CNTL-1                          
00991              PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT    
00992              GO TO 0340-ACCUMULATE.                               
00993                                                                   
00994      IF PASS-NUMBER = '3'                                         
00995          IF S-EPX-CNTL-ACCT NOT = HIGH-VALUES                     
00996              IF S-EPX-CNTL-ACCT = EP-CNTL-ACCT  AND               
00997                 S-EPX-CNTL-2    = EP-CNTL-2                       
00998                  NEXT SENTENCE                                    
00999              ELSE                                                 
01000                  PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT
01001                  GO TO 0340-ACCUMULATE.                           
01002                                                                   
012517     IF PASS-NUMBER = '4' or '8'
01004          IF DTE-TOT-OPT = 1 OR 3 OR 6                             
01005              GO TO 0310-READ-EARNED-PREM-EXTRACT                  
01006          ELSE                                                     
01007              IF DTE-TOT-OPT = 5 OR 7                              
01008                  IF EP-CNTL NOT = SAVE-CNTL                       
01009                      PERFORM 0700-REPTCD-BREAK                    
01010                          THRU 0790-ZERO-REPTCD.                   
01011                                                                   
01012      IF PASS-NUMBER = '5'                                         
01013          IF DTE-TOT-OPT = 1 OR 2 OR 5                             
01014              GO TO 0310-READ-EARNED-PREM-EXTRACT                  
01015          ELSE                                                     
01016              IF DTE-TOT-OPT = 6 OR 7                              
01017                  IF EP-CNTL NOT = SAVE-CNTL                       
01018                      PERFORM 0700-REPTCD-BREAK                    
01019                          THRU 0790-ZERO-REPTCD.                   
072709                                                                  
072709     IF PASS-NUMBER = '6' 
072709         IF DTE-TOT-OPT = 1 OR 3 OR 6                             
072709             GO TO 0310-READ-EARNED-PREM-EXTRACT                  
072709         ELSE                                                     
072709             IF DTE-TOT-OPT = 5 OR 7                              
072709                 IF EP-CNTL NOT = SAVE-CNTL                       
072709                     PERFORM 0700-REPTCD-BREAK                    
072709                         THRU 0790-ZERO-REPTCD.                   
01020                                                                   
012517     IF PASS-NUMBER = '4' or '8'
01022        IF S-EPX-A-RPT-CD-1 = EP-A-RPT-CD-1                        
01023          GO TO 0340-ACCUMULATE                                    
01024         ELSE                                                      
01025          MOVE '1'            TO  SKIP-HD-SW                       
01026          PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT        
01027          GO TO 0340-ACCUMULATE.                                   
072709                                                                  
072709     IF PASS-NUMBER = '6' 
072709       IF S-EPX-A-RPT-CD-1 = EP-A-RPT-CD-1                        
072709         GO TO 0340-ACCUMULATE                                    
072709        ELSE                                                      
072709         MOVE '1'            TO  SKIP-HD-SW                       
072709         PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT        
072709         GO TO 0340-ACCUMULATE.                                   
031511                                                                  
031511     IF PASS-NUMBER = '7'
031511       IF S-EPX-A-RPT-CD-1 = EP-A-RPT-CD-1  
031511         IF S-EPX-A-RPT-CD-2 = EP-A-RPT-CD-2
031511            IF EP-CNTL = SAVE-CNTL
031511               GO TO 0340-ACCUMULATE
031511            ELSE
031511               MOVE ' '     TO  SKIP-HD-SW                       
031511               PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT
031511               GO TO 0340-ACCUMULATE
031511            END-IF
031511         ELSE
031511             MOVE ' '        TO  SKIP-HD-SW
031511             PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT
031511             MOVE '1'        TO  SKIP-HD-SW
031511             PERFORM 1700-REPTCD2-BREAK THRU 1790-ZERO-REPTCD2
031511             GO TO 0340-ACCUMULATE
031511         END-IF
031511       ELSE                                                      
031511         MOVE ' '            TO  SKIP-HD-SW                       
031511         PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT
031511         MOVE '1'        TO  SKIP-HD-SW
031511         PERFORM 1700-REPTCD2-BREAK THRU 1790-ZERO-REPTCD2
031511         MOVE '2'            TO  SKIP-HD-SW        
031511         PERFORM 0700-REPTCD-BREAK  THRU 0790-ZERO-REPTCD
031511         GO TO 0340-ACCUMULATE
031511       END-IF
031511     END-IF.                                   
031511
01028                                                                   
01029      IF PASS-NUMBER = '5'                                         
01030      IF S-EPX-A-CARR NOT = EP-A-CARR                              
01031          MOVE '1'            TO  SKIP-HD-SW                       
01032          PERFORM 0600-CARRIER-BREAK THRU 0680-ZERO-CARRIER        
01033          GO TO 0340-ACCUMULATE.                                   
01034                                                                   
01035      IF PASS-NUMBER = '5'                                         
01036      IF S-EPX-A-GROUP NOT = EP-A-GROUP                            
01037          MOVE '1'            TO  SKIP-HD-SW                       
01038          PERFORM 0500-GROUPING-BREAK THRU 0580-ZERO-GROUPING      
01039          GO TO 0340-ACCUMULATE.                                   
01040                                                                   
01041      IF PASS-NUMBER = '5'                                         
01042          IF S-EPX-A-CARR     = EP-A-CARR  AND                     
01043             S-EPX-A-GROUP    = EP-A-GROUP AND                     
01044             S-EPX-A-RPT-CD-2 = EP-A-RPT-CD-2                      
01045             IF S-EPX-A-STATE = EP-A-STATE                         
01046              GO TO 0340-ACCUMULATE                                
01047            ELSE                                                   
01048              MOVE '1'            TO  SKIP-HD-SW                   
01049              PERFORM 0375-STATE-BREAK THRU 0380-ZERO-STATE        
01050              MOVE ' '            TO  SKIP-HD-SW                   
01051              GO TO 0340-ACCUMULATE                                
01052          ELSE                                                     
01053              MOVE '1'            TO  SKIP-HD-SW                   
01054              PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT    
01055              GO TO 0340-ACCUMULATE.                               
01056                                                                   
01057  0335-NOT-5.                                                      
01058      IF S-EPX-CNTL-1 NOT = EP-CNTL-1                              
01059          PERFORM 0600-CARRIER-BREAK THRU 0680-ZERO-CARRIER        
01060          GO TO 0340-ACCUMULATE.                                   
01061                                                                   
01062      IF S-EPX-CARR NOT = EP-CARR                                  
01063          PERFORM 0500-GROUPING-BREAK THRU 0580-ZERO-GROUPING      
01064          GO TO 0340-ACCUMULATE.                                   
01065                                                                   
01066      IF S-EPX-GRP NOT = EP-GROUP                                  
01067          PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT.       
01068                                                                   
01069  0340-ACCUMULATE.                                                 
01070      MOVE +0                     TO X1.                           
01071                                                                   
01072  0350-ACCUMULATE-B.                                               
01073      ADD +1 TO X1.                                                
01074                                                                   
122804     IF X1 GREATER 15
01075 *    IF X1 GREATER 13                                             
01076 *        DISPLAY 'DATE TABLE ERROR OR EP-RUN-DATE ERROR'          
01077 *        DISPLAY 'EP-DATE ' EP-DTE                                
01078 *        DISPLAY 'RUN-DT  ' RUN-DT                                
01079 *        DISPLAY 'COMPARE-DATE-TABLE ' COMPARE-DATE-TABLE         
01080 *        MOVE '0301'             TO WS-RETURN-CODE                
01081 *        GO TO ABEND-PGM.                                         
010306         GO TO 0310-READ-EARNED-PREM-EXTRACT
           END-IF
01082                                                                   
01083      IF COMPARE9DT (X1) NOT = EP-DTE                              
01084          GO TO 0350-ACCUMULATE-B.                                 
01085                                                                   
01086      IF EP-MTH-HI-CERT  GREATER  AC-DATE (X1)                     
01087          MOVE EP-MTH-HI-CERT     TO  AC-DATE (X1)                 
01088                                      AC-DATE-R(X1).               
01089                                                                   
01090      ADD EP-CERT                 TO  AC-CERT (X1)  RP-CERT (X1)   
031511                                     RP2-CERT (X1)
01091                                      ST-CERT (X1).                
01092      ADD EP-ISS-CNT              TO  AC-ISS  (X1)  RP-ISS  (X1)   
031511                                     RP2-ISS (X1)
01093                                      ST-ISS  (X1).                
01094      ADD EP-LBEN                 TO  AC-LBEN (X1)  RP-LBEN (X1)   
031511                                     RP2-LBEN (X1)
01095                                      ST-LBEN (X1).                
01096      ADD EP-LPRM                 TO  AC-LPRM (X1)  RP-LPRM (X1)   
031511                                     RP2-LPRM (X1)
01097                                      ST-LPRM (X1).                
01098      ADD EP-LCLM                 TO  AC-LCLM (X1)  RP-LCLM (X1)   
031511                                     RP2-LCLM (X1)
01099                                      ST-LCLM (X1).                
01100      ADD EP-ABEN                 TO  AC-ABEN (X1)  RP-ABEN (X1)   
031511                                     RP2-ABEN (X1)
01101                                      ST-ABEN (X1).                
01102      ADD EP-APRM                 TO  AC-APRM (X1)  RP-APRM (X1)   
031511                                     RP2-APRM (X1)
01103                                      ST-APRM (X1).                
01104      ADD EP-ACLM                 TO  AC-ACLM (X1)  RP-ACLM (X1)   
031511                                     RP2-ACLM (X1)
01105                                      ST-ACLM (X1).                
01106      ADD EP-TPRM                 TO  AC-TPRM (X1)  RP-TPRM (X1)   
031511                                     RP2-TPRM (X1)
01107                                      ST-TPRM (X1).                
01108      ADD EP-TCOM                 TO  AC-TCOM (X1)  RP-TCOM (X1)   
031511                                     RP2-TCOM (X1)
01109                                      ST-TCOM (X1).                
01110      MOVE EP-CNTL                TO SAVE-EPX.                     
102004     EVALUATE EP-ACCT-STATUS
102004        WHEN 'A'
                 MOVE 'ACTIVE'         TO SAVE-ACCT-STATUS
              WHEN 'I'
                 MOVE 'INACTIVE'       TO SAVE-ACCT-STATUS
              WHEN 'T'
                 MOVE 'TRANSFERRED'    TO SAVE-ACCT-STATUS
              WHEN 'C'
                 MOVE 'CANCELLED'      TO SAVE-ACCT-STATUS
              WHEN OTHER
                 MOVE 'UNKNOWN'        TO SAVE-ACCT-STATUS
           END-EVALUATE
           
01111      MOVE EP-ACCT-NAME           TO SAVE-ACCT-NAME.               
CIDMOD     MOVE EP-ACCT-CITY           TO SAVE-ACCT-CITY.               
01112      MOVE EP-ACC-EXPIRES         TO SAVE-AM-EXPIRES.              
01113      MOVE EP-ACC-HI-CERT         TO SAVE-AM-HI-CERT.              
01114                                                                   
010306     IF X1 < +15
010306        GO TO 0350-ACCUMULATE-B
010306     END-IF

01115      GO TO 0310-READ-EARNED-PREM-EXTRACT

           .
01117  0375-STATE-BREAK.                                                
01118      MOVE +0                     TO X1.                           
01119      PERFORM 0220-ZERO-PRINT-TABLE 12 TIMES.                      
01120      PERFORM 0800-PRINT-DECISION THRU 0899-EXIT.                  
01121                                                                   
01122      MOVE 'TOTALS'               TO STATE-TOTAL-LINE.             
01123                                                                   
01124      IF P-ST-SW = '1'                                             
01125          MOVE ' '                TO P-ST-SW                       
01126      ELSE                                                         
01127          IF DTE-CLIENT  =  'NCL'                                  
01128              MOVE SPACES         TO  STATE-TOTAL-LINE             
01129              GO TO 0380-ZERO-STATE                                
01130          ELSE                                                     
01131              GO TO 0380-ZERO-STATE.                               
01132                                                                   
01133      PERFORM 0900-HEAD-RTN  THRU 0999-EXIT.                       
01134      MOVE +1                     TO X1.                           
01135      MOVE +0                     TO Y1.                           
01136                                                                   
01137  0375-STATE-BREAK-PRINT.                                          
01138      ADD +1 TO X1 Y1.                                             
01139                                                                   
01140      IF X1 GREATER THAN 13                                        
01141          GO TO 0375-PRINT-LAST-ST-12.                             
01142                                                                   
01143      COMPUTE CERT = ST-CERT (X1) - ST-CERT (Y1).                  
01144      COMPUTE ISS  = ST-ISS  (X1) - ST-ISS  (Y1).                  
01145      COMPUTE LBEN = ST-LBEN (X1) - ST-LBEN (Y1).                  
01146      COMPUTE LPRM = ST-LPRM (X1) - ST-LPRM (Y1).                  
01147      COMPUTE LCLM = ST-LCLM (X1) - ST-LCLM (Y1).                  
01148      COMPUTE ABEN = ST-ABEN (X1) - ST-ABEN (Y1).                  
01149      COMPUTE APRM = ST-APRM (X1) - ST-APRM (Y1).                  
01150      COMPUTE ACLM = ST-ACLM (X1) - ST-ACLM (Y1).                  
01151      COMPUTE TCOM = ST-TCOM (X1) - ST-TCOM (Y1).                  
01152      COMPUTE TPRM = LPRM + APRM.                                  
01153                                                                   
01154      MOVE COMP-YR (X1)           TO DET-YR.                       
01155      MOVE COMP-MO (X1)           TO DET-MO.                       
01156                                                                   
01157      PERFORM 1200-LOAD-ALPHA-MONTH THRU 1299-EXIT.                
01158                                                                   
01159      MOVE CERT                   TO DET-CERTS.                    
01160      MOVE LBEN                   TO DET-LBEN.                     
01161      MOVE LPRM                   TO DET-LPRM.                     
01162      MOVE LCLM                   TO DET-LCLM.                     
01163      MOVE ABEN                   TO DET-ABEN.                     
01164      MOVE APRM                   TO DET-APRM.                     
01165      MOVE ACLM                   TO DET-ACLM.                     
01166      MOVE TPRM                   TO DET-TPRM.                     
01167      MOVE TCOM                   TO DET-TCOM.                     
01168                                                                   
01169      IF COMP-YR (X1) = CONV-YR AND                                
01170         COMP-MO (X1) = CONV-MO                                    
01171          MOVE ZEROS TO DET-CERTS DET-LBEN DET-LPRM                
01172                        DET-ABEN  DET-APRM DET-LCLM                
01173                        DET-ACLM  DET-TPRM DET-TCOM                
01174      ELSE                                                         
01175          PERFORM 0385-ADD-TO-OTHERS THRU 0385-EXIT.               
01176                                                                   
01177      MOVE DETAIL-LINE            TO P-DATA.                       
01178                                                                   
01179      IF X1 = 2                                                    
01180          MOVE '0'                TO X                             
01181      ELSE                                                         
01182          MOVE ' '                TO X.                            
01183                                                                   
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01185      GO TO 0375-STATE-BREAK-PRINT.                                
01186                                                                   
01187  0375-PRINT-LAST-ST-12.                                           
01188      MOVE L12ST-LBEN             TO DET-LBEN.                     
01189      MOVE L12ST-LPRM             TO DET-LPRM.                     
01190      MOVE L12ST-LCLM             TO DET-LCLM.                     
01191      MOVE L12ST-ABEN             TO DET-ABEN.                     
01192      MOVE L12ST-APRM             TO DET-APRM.                     
01193      MOVE L12ST-ACLM             TO DET-ACLM.                     
01194      MOVE L12ST-TPRM             TO DET-TPRM.                     
01195      MOVE L12ST-TCOM             TO DET-TCOM.                     
01196      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    
01197                                                                   
01198      MOVE '0'                    TO X.                            
01199      MOVE DETAIL-LINE            TO P-DATA.                       
01200      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01201                                                                   
      *  PRINT PRIOR L12  

122804*    MOVE PL12ST-LBEN             TO DET-LBEN
122804*    MOVE PL12ST-LPRM             TO DET-LPRM
122804*    MOVE PL12ST-LCLM             TO DET-LCLM
122804*    MOVE PL12ST-ABEN             TO DET-ABEN
122804*    MOVE PL12ST-APRM             TO DET-APRM
122804*    MOVE PL12ST-ACLM             TO DET-ACLM
122804*    MOVE PL12ST-TPRM             TO DET-TPRM
122804*    MOVE PL12ST-TCOM             TO DET-TCOM
122804*    MOVE 'PRIOR L12         '   TO DET-TITLE
122804*    MOVE ' '                    TO X
122804*    MOVE DETAIL-LINE            TO P-DATA
122804*    PERFORM 1100-WRITE-PRINT THRU 1199-EXIT
           .
01202  0375-PRINT-ST-YTD.                                               
01203      MOVE YTDST-LBEN             TO DET-LBEN.                     
01204      MOVE YTDST-LPRM             TO DET-LPRM.                     
01205      MOVE YTDST-LCLM             TO DET-LCLM.                     
01206      MOVE YTDST-ABEN             TO DET-ABEN.                     
01207      MOVE YTDST-APRM             TO DET-APRM.                     
01208      MOVE YTDST-ACLM             TO DET-ACLM.                     
01209      MOVE YTDST-TPRM             TO DET-TPRM.                     
01210      MOVE YTDST-TCOM             TO DET-TCOM.                     
01211      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    
01212      MOVE ' '                    TO X.                            
01213      MOVE DETAIL-LINE            TO P-DATA.                       
01214      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01215      MOVE SPACES                 TO DET-TITLE.                    
01216                                                                   
      *  PRINT PRIOR YTD  

122804     MOVE PYTDST-LBEN             TO DET-LBEN
122804     MOVE PYTDST-LPRM             TO DET-LPRM
122804     MOVE PYTDST-LCLM             TO DET-LCLM
122804     MOVE PYTDST-ABEN             TO DET-ABEN
122804     MOVE PYTDST-APRM             TO DET-APRM
122804     MOVE PYTDST-ACLM             TO DET-ACLM
122804     MOVE PYTDST-TPRM             TO DET-TPRM
122804     MOVE PYTDST-TCOM             TO DET-TCOM
122804     MOVE 'PRIOR YTD         '   TO DET-TITLE
122804     MOVE ' '                    TO X
122804     MOVE DETAIL-LINE            TO P-DATA
122804     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT
122804     MOVE SPACES                 TO DET-TITLE
           .
01202  0376-PRINT-ST-ITD.                                               

122804*    MOVE ITDST-LBEN             TO DET-LBEN
122804*    MOVE ITDST-LPRM             TO DET-LPRM
122804*    MOVE ITDST-LCLM             TO DET-LCLM
122804*    MOVE ITDST-ABEN             TO DET-ABEN
122804*    MOVE ITDST-APRM             TO DET-APRM
122804*    MOVE ITDST-ACLM             TO DET-ACLM
122804*    MOVE ITDST-TPRM             TO DET-TPRM
122804*    MOVE ITDST-TCOM             TO DET-TCOM
122804*    MOVE 'INCEPTION TO DATE '   TO DET-TITLE
122804*    MOVE ' '                    TO X
122804*    MOVE DETAIL-LINE            TO P-DATA
122804*    PERFORM 1100-WRITE-PRINT THRU 1199-EXIT

122804     MOVE SPACES                 TO DET-TITLE
01216                                                                   
01217      MOVE ZERO-ACCUM             TO LAST-12-STATE-ACCUM
01218                                     YTD-STATE-ACCUM
122804*                                   ITD-STATE-ACCUM
122804                                    PYTD-STATE-ACCUM
122804*                                   PL12-STATE-ACCUM
01219      .                                                            
01220  0380-ZERO-STATE.                                                 
01221      MOVE +0                     TO X1.                           
01222      PERFORM 0255-ZERO-ACCUM-STATE 15 TIMES.                      
01223                                                                   
01224  0380-RETURN.                                                     
01225      GO TO 0310-READ-EARNED-PREM-EXTRACT.                         
01226                                                                   
01227  0385-ADD-TO-OTHERS.                                              
01228                                                                   
122804*  THIS IS COMPARING THE HIGH CERT DATE
01229      IF AC-DATE (X1)  GREATER  GP-DATE (Y1)                       
01230          MOVE AC-DATE (X1)       TO  GP-DATE (Y1)                 
01231                                      GP-DATE-R(Y1).               
01232                                                                   
01233      IF AC-DATE (X1)  GREATER  CA-DATE (Y1)                       
01234          MOVE AC-DATE (X1)       TO  CA-DATE (Y1)                 
01235                                      CA-DATE-R(Y1).               
01236                                                                   
01237      IF AC-DATE (X1)  GREATER  RP-DATE (Y1)                       
01238          MOVE AC-DATE (X1)       TO  RP-DATE (Y1)                 
01239                                      RP-DATE-R (Y1).              
01240                                                                   
01241                                                                   
01242      IF AC-DATE (X1)  GREATER  GT-DATE (Y1)                       
01243          MOVE AC-DATE (X1)       TO  GT-DATE (Y1)                 
01244                                      GT-DATE-R (Y1).              
01245                                                                   
01246      IF COMPARE9DT (X1) GREATER YEAR-OLD-DATE                     
01247          ADD LBEN TO L12ST-LBEN                                   
01248          ADD LPRM TO L12ST-LPRM                                   
01249          ADD LCLM TO L12ST-LCLM                                   
01250          ADD ABEN TO L12ST-ABEN                                   
01251          ADD APRM TO L12ST-APRM                                   
01252          ADD ACLM TO L12ST-ACLM                                   
01253          ADD TPRM TO L12ST-TPRM                                   
01254          ADD TCOM TO L12ST-TCOM.                                  
01255                                                                   
01256      IF RUN-YR = COMP-YR (X1)                                     
01257          ADD LBEN TO YTDST-LBEN                                   
01258          ADD LPRM TO YTDST-LPRM                                   
01259                      YTDST-TPRM                                   
01260          ADD LCLM TO YTDST-LCLM                                   
01261          ADD TCOM TO YTDST-TCOM                                   
01262          ADD ABEN TO YTDST-ABEN                                   
01263          ADD APRM TO YTDST-APRM                                   
01264                      YTDST-TPRM                                   
01265          ADD ACLM TO YTDST-ACLM.                                  
01266                                                                   
122804*    IF X1 = 13
      *       COMPUTE ITDST-LBEN = ITDST-LBEN + ST-LBEN (X1)
      *       COMPUTE ITDST-LPRM = ITDST-LPRM + ST-LPRM (X1)
      *       COMPUTE ITDST-LCLM = ITDST-LCLM + ST-LCLM (X1)
      *       COMPUTE ITDST-ABEN = ITDST-ABEN + ST-ABEN (X1)
      *       COMPUTE ITDST-APRM = ITDST-APRM + ST-APRM (X1)
      *       COMPUTE ITDST-ACLM = ITDST-ACLM + ST-ACLM (X1)
      *       COMPUTE ITDST-TPRM = ITDST-TPRM + ST-TPRM (X1)
      *       COMPUTE ITDST-TCOM = ITDST-TCOM + ST-TCOM (X1)
      *    END-IF

122804     IF X1 = 13
              COMPUTE PYTDST-LBEN = PYTDST-LBEN +
                 (ST-LBEN (1) - ST-LBEN (15))
              COMPUTE PYTDST-LPRM = PYTDST-LPRM +
                 (ST-LPRM (1) - ST-LPRM (15))
              COMPUTE PYTDST-LCLM = PYTDST-LCLM +
                 (ST-LCLM (1) - ST-LCLM (15))
              COMPUTE PYTDST-ABEN = PYTDST-ABEN +
                 (ST-ABEN (1) - ST-ABEN (15))
091614        COMPUTE PYTDST-APRM = PYTDST-APRM +
                 (ST-APRM (1) - ST-APRM (15))
              COMPUTE PYTDST-ACLM = PYTDST-ACLM +
                 (ST-ACLM (1) - ST-ACLM (15))
              COMPUTE PYTDST-TPRM = PYTDST-TPRM +
                 (ST-TPRM (1) - ST-TPRM (15))
              COMPUTE PYTDST-TCOM = PYTDST-TCOM +
                 (ST-TCOM (1) - ST-TCOM (15))
           END-IF

122804*    IF X1 = 13
      *       COMPUTE PL12ST-LBEN = PL12ST-LBEN +
      *          (ST-LBEN (1) - ST-LBEN (14))
      *       COMPUTE PL12ST-LPRM = PL12ST-LPRM +
      *          (ST-LPRM (1) - ST-LPRM (14))
      *       COMPUTE PL12ST-LCLM = PL12ST-LCLM +
      *          (ST-LCLM (1) - ST-LCLM (14))
      *       COMPUTE PL12ST-ABEN = PL12ST-ABEN +
      *          (ST-ABEN (1) - ST-ABEN (14))
      *       COMPUTE PL12ST-APRM = PL12ST-APRM +
      *          (ST-APRM (1) - ST-ABEN (14))
      *       COMPUTE PL12ST-ACLM = PL12ST-ACLM +
      *          (ST-ACLM (1) - ST-ACLM (14))
      *       COMPUTE PL12ST-TPRM = PL12ST-TPRM +
      *          (ST-TPRM (1) - ST-TPRM (14))
      *       COMPUTE PL12ST-TCOM = PL12ST-TCOM +
      *          (ST-TCOM (1) - ST-TCOM (14))
      *    END-IF

           .
01267  0385-EXIT.                                                       
01268       EXIT.                                                       
01269  EJECT                                                            
01270  0400-ACCOUNT-BREAK.                                              
01271      IF PASS-NUMBER = '5'                                         
01272          PERFORM 0375-STATE-BREAK THRU 0380-ZERO-STATE.           
01273                                                                   
01274      MOVE +0                     TO X1.                           
01275      PERFORM 0220-ZERO-PRINT-TABLE 12 TIMES.                      
01276      PERFORM 0800-PRINT-DECISION THRU 0899-EXIT.                  
01277                                                                   
01278      IF P-ACC-SW = '1'                                            
01279          MOVE ' '                TO  P-ACC-SW                     
01280      ELSE                                                         
01281          GO TO 0495-ZERO-ACCOUNT.                                 
01282                                                                   
01283      IF PASS-NUMBER = '5'                                         
01284         IF DTE-CLIENT  =  'NCL'                                   
01285             MOVE S-EPX-A-STATE   TO SAVE-STATE                    
01286         ELSE                                                      
01287             MOVE S-EPX-A-STATE   TO SAVE-STATE                    
01288             MOVE ' '             TO S-EPX-A-STATE.                
01289                                                                   
01290      PERFORM 0900-HEAD-RTN  THRU 0999-EXIT.                       
01291                                                                   
01292      IF PASS-NUMBER = '5'                                         
01293          MOVE SAVE-STATE         TO S-EPX-A-STATE.                
01294                                                                   
01295      MOVE +1                     TO X1.                           
01296      MOVE +0                     TO Y1.                           
01297                                                                   
01298  0410-ACCOUNT-BREAK-PRINT.                                        
01299      ADD +1                      TO X1 Y1.                        
01300                                                                   
01301      IF X1 GREATER THAN 13                                        
01302          GO TO 0460-PRINT-LAST-AC-12.                             
01303                                                                   
01304      MOVE AC-DATE (X1)           TO  SAVE-HER-HI-CERT.            
01305                                                                   
01306      COMPUTE CERT = AC-CERT (X1) - AC-CERT (Y1).                  
01307      COMPUTE ISS  = AC-ISS  (X1) - AC-ISS  (Y1).                  
01308      COMPUTE LBEN = AC-LBEN (X1) - AC-LBEN (Y1).                  
01309      COMPUTE LPRM = AC-LPRM (X1) - AC-LPRM (Y1).                  
01310      COMPUTE LCLM = AC-LCLM (X1) - AC-LCLM (Y1).                  
01311      COMPUTE ABEN = AC-ABEN (X1) - AC-ABEN (Y1).                  
01312      COMPUTE APRM = AC-APRM (X1) - AC-APRM (Y1).                  
01313      COMPUTE ACLM = AC-ACLM (X1) - AC-ACLM (Y1).                  
01314      COMPUTE TCOM = AC-TCOM (X1) - AC-TCOM (Y1).                  
01315      COMPUTE TPRM = LPRM + APRM.                                  
01316                                                                   
01317      IF DTE-CLIENT = 'HER'                                        
01318          MOVE COMP-YR (X1)       TO  DET-YR-2                     
01319          MOVE COMP-MO (X1)       TO  DET-MO-2                     
01320      ELSE                                                         
01321          MOVE COMP-YR (X1)       TO  DET-YR                       
01322          MOVE COMP-MO (X1)       TO  DET-MO.                      
01323                                                                   
01324      PERFORM 1200-LOAD-ALPHA-MONTH THRU 1299-EXIT.                
01325                                                                   
01326      IF DTE-CLIENT = 'HER'                                        
01327          NEXT SENTENCE                                            
01328      ELSE                                                         
01329          GO TO 0420-CONTINUE.                                     
01330                                                                   
01331      IF (CERT = ZEROS                                             
01332         OR ISS EQUAL ZEROS)                                       
01333          MOVE SPACES             TO  DET-FILL                     
01334      ELSE                                                         
01335          MOVE SAVE-HER-HI-MO     TO  DET-HI-MO                    
01336          MOVE '/'                TO  DET-HI-SLASH                 
01337          MOVE SAVE-HER-HI-YR     TO  DET-HI-YR                    
01338          MOVE 0                  TO  SAVE-HER-HI-CERT.            
01339                                                                   
01340      MOVE CERT                   TO  DET-CERTS-2.                 
01341      MOVE ISS                    TO  DET-ISS-COUNT-2.             
01342      MOVE LBEN                   TO  DET-LBEN-2.                  
01343      MOVE LPRM                   TO  DET-LPRM-2.                  
01344      MOVE LCLM                   TO  DET-LCLM-2.                  
01345      MOVE APRM                   TO  DET-APRM-2.                  
01346      MOVE ACLM                   TO  DET-ACLM-2.                  
01347      MOVE TPRM                   TO  DET-TPRM-2.                  
01348      MOVE TCOM                   TO  DET-TCOM-2.                  
01349                                                                   
01350      IF COMP-YR (X1) = CONV-YR AND                                
01351         COMP-MO (X1) = CONV-MO                                    
01352          MOVE ZEROS              TO  DET-CERTS-2  DET-LBEN-2      
01353                                      DET-LPRM-2   DET-LCLM-2      
01354                                      DET-APRM-2   DET-ACLM-2      
01355                                      DET-TPRM-2   DET-TCOM-2      
01356                                      DET-ISS-COUNT-2              
01357      ELSE                                                         
01358          PERFORM 0430-ADD-TO-OTHERS  THRU  0450-EXIT.             
01359                                                                   
01360      MOVE DETAIL-LINE-2          TO  P-DATA.                      
01361                                                                   
01362      IF X1  =  2                                                  
01363          MOVE '0'                TO  X                            
01364      ELSE                                                         
01365          MOVE ' '                TO  X.                           
01366                                                                   
01367      PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.                   
01368                                                                   
01369      GO TO 0410-ACCOUNT-BREAK-PRINT.                              
01370                                                                   
01371  0420-CONTINUE.                                                   
01372      MOVE CERT                   TO DET-CERTS.                    
01373      MOVE LBEN                   TO DET-LBEN.                     
01374      MOVE LPRM                   TO DET-LPRM.                     
01375      MOVE LCLM                   TO DET-LCLM.                     
01376      MOVE ABEN                   TO DET-ABEN.                     
01377      MOVE APRM                   TO DET-APRM.                     
01378      MOVE ACLM                   TO DET-ACLM.                     
01379      MOVE TPRM                   TO DET-TPRM.                     
01380      MOVE TCOM                   TO DET-TCOM.                     
01381                                                                   
01382      IF COMP-YR (X1) = CONV-YR AND                                
01383         COMP-MO (X1) = CONV-MO                                    
01384          MOVE ZEROS TO DET-CERTS DET-LBEN DET-LPRM                
01385                        DET-ABEN DET-APRM DET-LCLM                 
01386                        DET-ACLM DET-TPRM DET-TCOM                 
01387        ELSE                                                       
01388          PERFORM 0430-ADD-TO-OTHERS THRU 0450-EXIT.               
01389                                                                   
01390      MOVE DETAIL-LINE            TO P-DATA.                       
01391                                                                   
01392      IF X1 = 2                                                    
01393          MOVE '0'                TO X                             
01394      ELSE                                                         
01395          MOVE ' '                TO X.                            
01396                                                                   
01397      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01398      GO TO 0410-ACCOUNT-BREAK-PRINT.                              
01399                                                                   
01400  0430-ADD-TO-OTHERS.                                              
01401      IF PASS-NUMBER = '3'                                         
01402          IF S-EPX-CNTL-ACCT NOT = HIGH-VALUES                     
01403              GO TO 0440-ADD-CONTINUE.                             
01404                                                                   
01405      IF BIN-AC-DATE   GREATER  BIN-GP-DATE                        
01406          MOVE AC-DATE (X1)       TO  GP-DATE (Y1)                 
01407                                      GP-DATE-R (Y1).              
01408                                                                   
01409      IF BIN-AC-DATE   GREATER  BIN-CA-DATE                        
01410          MOVE AC-DATE (X1)       TO  CA-DATE (Y1)                 
01411                                      CA-DATE-R (Y1).              
01412                                                                   
01413      IF BIN-AC-DATE   GREATER  BIN-RP-DATE                        
01414          MOVE AC-DATE (X1)       TO  RP-DATE (Y1)                 
01415                                      RP-DATE-R (Y1).              
01416                                                                   
01417      IF BIN-AC-DATE   GREATER  BIN-GT-DATE                        
01418          MOVE AC-DATE (X1)       TO  GT-DATE (Y1)                 
01419                                      GT-DATE-R (Y1).              
01420                                                                   
01421      ADD CERT TO GP-CERT (Y1) CA-CERT (Y1) GR-CERT GT-CERT (Y1).  
01422      ADD ISS  TO GP-ISS  (Y1) CA-ISS  (Y1) GR-ISS  GT-ISS  (Y1).  
01423      ADD LBEN TO GP-LBEN (Y1) CA-LBEN (Y1) GR-LBEN GT-LBEN (Y1).  
01424      ADD LPRM TO GP-LPRM (Y1) CA-LPRM (Y1) GR-LPRM GT-LPRM (Y1).  
01425      ADD LCLM TO GP-LCLM (Y1) CA-LCLM (Y1) GR-LCLM GT-LCLM (Y1).  
01426      ADD ABEN TO GP-ABEN (Y1) CA-ABEN (Y1) GR-ABEN GT-ABEN (Y1).  
01427      ADD APRM TO GP-APRM (Y1) CA-APRM (Y1) GR-APRM GT-APRM (Y1).  
01428      ADD ACLM TO GP-ACLM (Y1) CA-ACLM (Y1) GR-ACLM GT-ACLM (Y1).  
01429      ADD TPRM TO GP-TPRM (Y1) CA-TPRM (Y1) GR-TPRM GT-TPRM (Y1).  
01430      ADD TCOM TO GP-TCOM (Y1) CA-TCOM (Y1) GR-TCOM GT-TCOM (Y1).  
01431                                                                   
01432  0440-ADD-CONTINUE.                                               
01433      IF COMPARE9DT (X1) GREATER YEAR-OLD-DATE                     
01434          ADD LBEN TO L12AC-LBEN                                   
01435          ADD LPRM TO L12AC-LPRM                                   
01436          ADD LCLM TO L12AC-LCLM                                   
01437          ADD ABEN TO L12AC-ABEN                                   
01438          ADD APRM TO L12AC-APRM                                   
01439          ADD ACLM TO L12AC-ACLM                                   
01440          ADD TPRM TO L12AC-TPRM                                   
01441          ADD TCOM TO L12AC-TCOM                                   
01442          IF PASS-NUMBER = '3'  AND                                
01443             S-EPX-CNTL-ACCT NOT = HIGH-VALUES                     
01444              NEXT SENTENCE                                        
01445          ELSE                                                     
012517             IF (PASS-NUMBER = '4' OR '5' or '8')  AND                     
082609                (DTE-TOT-OPT NOT = 5 AND 6 AND 7)
01448                  NEXT SENTENCE                                    
01449              ELSE                                                 
01450                  ADD LBEN TO L12GP-LBEN L12CA-LBEN                
082609                             L12GT-LBEN
01451                  ADD LPRM TO L12GP-LPRM L12CA-LPRM                
082609                             L12GT-LPRM
01452                  ADD LCLM TO L12GP-LCLM L12CA-LCLM                
082609                             L12GT-LCLM
01453                  ADD ABEN TO L12GP-ABEN L12CA-ABEN                
082609                             L12GT-ABEN
01454                  ADD APRM TO L12GP-APRM L12CA-APRM                
082609                             L12GT-APRM
01455                  ADD ACLM TO L12GP-ACLM L12CA-ACLM                
082609                             L12GT-ACLM
01456                  ADD TPRM TO L12GP-TPRM L12CA-TPRM                
082609                             L12GT-TPRM
01457                  ADD TCOM TO L12GP-TCOM L12CA-TCOM .              
082609                 ADD TCOM TO L12GT-TCOM.
01458                                                                   
01459      IF RUN-YR = COMP-YR (X1)                                     
01460          ADD LBEN TO YTDAC-LBEN                                   
01461          ADD LPRM TO YTDAC-LPRM                                   
01462                      YTDAC-TPRM                                   
01463          ADD LCLM TO YTDAC-LCLM                                   
01464          ADD TCOM TO YTDAC-TCOM                                   
01465          ADD ABEN TO YTDAC-ABEN                                   
01466          ADD APRM TO YTDAC-APRM                                   
01467                      YTDAC-TPRM                                   
01468          ADD ACLM TO YTDAC-ACLM                                   
01469          IF PASS-NUMBER = '3'  AND                                
01470             S-EPX-CNTL-ACCT NOT = HIGH-VALUES                     
01471              NEXT SENTENCE                                        
01472          ELSE                                                     
012517             IF (PASS-NUMBER = '4' OR '5' or '8') AND                      
082609                (DTE-TOT-OPT NOT = 5 AND 6 AND 7)                   
01475                  NEXT SENTENCE                                    
01476              ELSE                                                 
01477                  ADD LBEN TO YTDGP-LBEN YTDCA-LBEN                
082609                             YTDGT-LBEN
01478                  ADD LPRM TO YTDGP-LPRM YTDCA-LPRM                
01479                              YTDGP-TPRM YTDCA-TPRM                
082609                             YTDGT-LPRM YTDGT-TPRM
01480                  ADD LCLM TO YTDGP-LCLM YTDCA-LCLM                
082609                             YTDGT-LCLM
01481                  ADD TCOM TO YTDGP-TCOM YTDCA-TCOM                
082609                             YTDGT-TCOM
01482                  ADD ABEN TO YTDGP-ABEN YTDCA-ABEN                
082609                             YTDGT-ABEN
01483                  ADD APRM TO YTDGP-APRM YTDCA-APRM                
01484                              YTDGP-TPRM YTDCA-TPRM                
082609                             YTDGT-APRM YTDGT-TPRM
01485                  ADD ACLM TO YTDGP-ACLM YTDCA-ACLM.               
082609                 ADD ACLM TO YTDGT-ACLM.
01486                                                                   
      *    IF X1 = 13
      *       COMPUTE ITDAC-LBEN = ITDAC-LBEN + AC-LBEN (X1)
      *       COMPUTE ITDAC-LPRM = ITDAC-LPRM + AC-LPRM (X1)
      *       COMPUTE ITDAC-LCLM = ITDAC-LCLM + AC-LCLM (X1)
      *       COMPUTE ITDAC-ABEN = ITDAC-ABEN + AC-ABEN (X1)
      *       COMPUTE ITDAC-APRM = ITDAC-APRM + AC-APRM (X1)
      *       COMPUTE ITDAC-ACLM = ITDAC-ACLM + AC-ACLM (X1)
      *       COMPUTE ITDAC-TPRM = ITDAC-TPRM + AC-TPRM (X1)
      *       COMPUTE ITDAC-TCOM = ITDAC-TCOM + AC-TCOM (X1)
      *       IF (PASS-NUMBER = '3')
      *          AND (S-EPX-CNTL-ACCT NOT = HIGH-VALUES)
      *          CONTINUE
      *       ELSE
      *          IF (PASS-NUMBER = '4' OR '5')
      *             AND (DTE-TOT-OPT NOT = 5 AND 6 AND 7)
      *             CONTINUE
      *          ELSE
      *             ADD ITDAC-LBEN TO ITDGP-LBEN ITDCA-LBEN
      *             ADD ITDAC-LPRM TO ITDGP-LPRM ITDCA-LPRM
      *             ADD ITDAC-LCLM TO ITDGP-LCLM ITDCA-LCLM
      *             ADD ITDAC-ABEN TO ITDGP-ABEN ITDCA-ABEN
      *             ADD ITDAC-APRM TO ITDGP-APRM ITDCA-APRM
      *             ADD ITDAC-ACLM TO ITDGP-ACLM ITDCA-ACLM
      *             ADD ITDAC-TPRM TO ITDGP-TPRM ITDCA-TPRM
      *             ADD ITDAC-TCOM TO ITDGP-TCOM ITDCA-TCOM
      *          END-IF
      *       END-IF
      *    END-IF

           IF X1 = 13
              COMPUTE PYTDAC-LBEN = PYTDAC-LBEN +
                 (AC-LBEN (1) - AC-LBEN (15))
              COMPUTE PYTDAC-LPRM = PYTDAC-LPRM +
                 (AC-LPRM (1) - AC-LPRM (15))
              COMPUTE PYTDAC-LCLM = PYTDAC-LCLM +
                 (AC-LCLM (1) - AC-LCLM (15))
              COMPUTE PYTDAC-ABEN = PYTDAC-ABEN +
                 (AC-ABEN (1) - AC-ABEN (15))
              COMPUTE PYTDAC-APRM = PYTDAC-APRM +
                 (AC-APRM (1) - AC-APRM (15))
              COMPUTE PYTDAC-ACLM = PYTDAC-ACLM +
                 (AC-ACLM (1) - AC-ACLM (15))
              COMPUTE PYTDAC-TPRM = PYTDAC-TPRM +
                 (AC-TPRM (1) - AC-TPRM (15))
              COMPUTE PYTDAC-TCOM = PYTDAC-TCOM +
                 (AC-TCOM (1) - AC-TCOM (15))
              IF (PASS-NUMBER = '3')
                 AND (S-EPX-CNTL-ACCT NOT = HIGH-VALUES)
                 CONTINUE
              ELSE
012517           IF (PASS-NUMBER = '4' OR '5' or '8')
                    AND (DTE-TOT-OPT NOT = 5 AND 6 AND 7)
                    CONTINUE
                 ELSE
                    ADD PYTDAC-LBEN TO PYTDGP-LBEN PYTDCA-LBEN
082609                                 PYTDGT-LBEN
                    ADD PYTDAC-LPRM TO PYTDGP-LPRM PYTDCA-LPRM
082609                                 PYTDGT-LPRM
                    ADD PYTDAC-LCLM TO PYTDGP-LCLM PYTDCA-LCLM
082609                                 PYTDGT-LCLM
                    ADD PYTDAC-ABEN TO PYTDGP-ABEN PYTDCA-ABEN
082609                                 PYTDGT-ABEN
                    ADD PYTDAC-APRM TO PYTDGP-APRM PYTDCA-APRM
082609                                 PYTDGT-APRM
                    ADD PYTDAC-ACLM TO PYTDGP-ACLM PYTDCA-ACLM
082609                                 PYTDGT-ACLM
                    ADD PYTDAC-TPRM TO PYTDGP-TPRM PYTDCA-TPRM
082609                                 PYTDGT-TPRM
                    ADD PYTDAC-TCOM TO PYTDGP-TCOM PYTDCA-TCOM
082609                                 PYTDGT-TCOM
                 END-IF
              END-IF
           END-IF

      *    IF X1 = 13
      *       COMPUTE PL12AC-LBEN = PL12AC-LBEN +
      *          (AC-LBEN (1) - AC-LBEN (14))
      *       COMPUTE PL12AC-LPRM = PL12AC-LPRM +
      *          (AC-LPRM (1) - AC-LPRM (14))
      *       COMPUTE PL12AC-LCLM = PL12AC-LCLM +
      *          (AC-LCLM (1) - AC-LCLM (14))
      *       COMPUTE PL12AC-ABEN = PL12AC-ABEN +
      *          (AC-ABEN (1) - AC-ABEN (14))
      *       COMPUTE PL12AC-APRM = PL12AC-APRM +
      *          (AC-APRM (1) - AC-APRM (14))
      *       COMPUTE PL12AC-ACLM = PL12AC-ACLM +
      *          (AC-ACLM (1) - AC-ACLM (14))
      *       COMPUTE PL12AC-TPRM = PL12AC-TPRM +
      *          (AC-TPRM (1) - AC-TPRM (14))
      *       COMPUTE PL12AC-TCOM = PL12AC-TCOM +
      *          (AC-TCOM (1) - AC-TCOM (14))
      *       IF (PASS-NUMBER = '3')
      *          AND (S-EPX-CNTL-ACCT NOT = HIGH-VALUES)
      *          CONTINUE
      *       ELSE
      *          IF (PASS-NUMBER = '4' OR '5')
      *             AND (DTE-TOT-OPT NOT = 5 AND 6 AND 7)
      *             CONTINUE
      *          ELSE
      *             ADD PL12AC-LBEN TO PL12GP-LBEN PL12CA-LBEN
      *             ADD PL12AC-LPRM TO PL12GP-LPRM PL12CA-LPRM
      *             ADD PL12AC-LCLM TO PL12GP-LCLM PL12CA-LCLM
      *             ADD PL12AC-ABEN TO PL12GP-ABEN PL12CA-ABEN
      *             ADD PL12AC-APRM TO PL12GP-APRM PL12CA-APRM
      *             ADD PL12AC-ACLM TO PL12GP-ACLM PL12CA-ACLM
      *             ADD PL12AC-TPRM TO PL12GP-TPRM PL12CA-TPRM
      *             ADD PL12AC-TCOM TO PL12GP-TCOM PL12CA-TCOM
      *          END-IF
      *       END-IF
      *    END-IF



           .
01487  0450-EXIT.                                                       
01488       EXIT.                                                       
01489                                                                   
01490  0460-PRINT-LAST-AC-12.                                           
01491      IF DTE-CLIENT = 'HER'                                        
01492          NEXT SENTENCE                                            
01493      ELSE                                                         
01494          GO TO 0480-PRINT-LAST-AC-12.                             
01495                                                                   
01496      MOVE L12AC-LBEN             TO  DET-LBEN-2.                  
01497      MOVE L12AC-LPRM             TO  DET-LPRM-2.                  
01498      MOVE L12AC-LCLM             TO  DET-LCLM-2.                  
01499      MOVE L12AC-APRM             TO  DET-APRM-2.                  
01500      MOVE L12AC-ACLM             TO  DET-ACLM-2.                  
01501      MOVE L12AC-TPRM             TO  DET-TPRM-2.                  
01502      MOVE L12AC-TCOM             TO  DET-TCOM-2.                  
01503      MOVE 'LAST 12 MONTHS'       TO  DET-TITLE-2.                 
01504      MOVE '0'                    TO  X.                           
01505      MOVE DETAIL-LINE-2          TO  P-DATA.                      
01506                                                                   
01507      PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.                   
01508                                                                   
01509      MOVE ZERO-ACCUM             TO  LAST-12-ACCOUNT-ACCUM.       
01510                                                                   
01511  0470-PRINT-AC-YTD.                                               
01512      MOVE YTDAC-LBEN             TO  DET-LBEN-2.                  
01513      MOVE YTDAC-LPRM             TO  DET-LPRM-2.                  
01514      MOVE YTDAC-LCLM             TO  DET-LCLM-2.                  
01515      MOVE YTDAC-APRM             TO  DET-APRM-2.                  
01516      MOVE YTDAC-ACLM             TO  DET-ACLM-2.                  
01517      MOVE YTDAC-TPRM             TO  DET-TPRM-2.                  
01518      MOVE YTDAC-TCOM             TO  DET-TCOM-2.                  
01519      MOVE 'YEAR TO DATE'         TO  DET-TITLE-2.                 
01520      MOVE ' '                    TO  X.                           
01521      MOVE DETAIL-LINE-2          TO  P-DATA.                      
01522                                                                   
01523      PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.                   
01524                                                                   
01525      MOVE SPACES                 TO  DET-TITLE-2.                 
01526      MOVE ZERO-ACCUM             TO  YTD-ACCOUNT-ACCUM.           
01527                                                                   
01528      GO TO 0495-ZERO-ACCOUNT.                                     
01529                                                                   
01530  0480-PRINT-LAST-AC-12.                                           

01531      MOVE L12AC-LBEN             TO DET-LBEN.                     
01532      MOVE L12AC-LPRM             TO DET-LPRM.                     
01533      MOVE L12AC-LCLM             TO DET-LCLM.                     
01534      MOVE L12AC-ABEN             TO DET-ABEN.                     
01535      MOVE L12AC-APRM             TO DET-APRM.                     
01536      MOVE L12AC-ACLM             TO DET-ACLM.                     
01537      MOVE L12AC-TPRM             TO DET-TPRM.                     
01538      MOVE L12AC-TCOM             TO DET-TCOM.                     
01539      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    
01540                                                                   
01541      IF DTE-CLIENT = 'AFL'                                        
01542          MOVE 'FISCAL TO DATE'   TO DET-TITLE.                    
01543                                                                   
01544      MOVE '0'                    TO X.                            
01545      MOVE DETAIL-LINE            TO P-DATA.                       
01546      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01547      MOVE ZERO-ACCUM             TO LAST-12-ACCOUNT-ACCUM.        
01548                                                                   
      ********   PRINT PRIOR L12 MO

      *    MOVE PL12AC-LBEN            TO DET-LBEN
      *    MOVE PL12AC-LPRM            TO DET-LPRM
      *    MOVE PL12AC-LCLM            TO DET-LCLM
      *    MOVE PL12AC-ABEN            TO DET-ABEN
      *    MOVE PL12AC-APRM            TO DET-APRM
      *    MOVE PL12AC-ACLM            TO DET-ACLM
      *    MOVE PL12AC-TPRM            TO DET-TPRM
      *    MOVE PL12AC-TCOM            TO DET-TCOM
      *    MOVE 'PRIOR L12         '   TO DET-TITLE

      *    MOVE ' '                    TO X
      *    MOVE DETAIL-LINE            TO P-DATA
      *    PERFORM 1100-WRITE-PRINT THRU 1199-EXIT

      *    MOVE ZERO-ACCUM             TO PL12-ACCOUNT-ACCUM
           .
01549  0490-PRINT-AC-YTD.                                               

01550      MOVE YTDAC-LBEN             TO DET-LBEN.                     
01551      MOVE YTDAC-LPRM             TO DET-LPRM.                     
01552      MOVE YTDAC-LCLM             TO DET-LCLM.                     
01553      MOVE YTDAC-ABEN             TO DET-ABEN.                     
01554      MOVE YTDAC-APRM             TO DET-APRM.                     
01555      MOVE YTDAC-ACLM             TO DET-ACLM.                     
01556      MOVE YTDAC-TPRM             TO DET-TPRM.                     
01557      MOVE YTDAC-TCOM             TO DET-TCOM.                     
01558      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    
01559      MOVE ' '                    TO X.                            
01560      MOVE DETAIL-LINE            TO P-DATA.                       
01561      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01562      MOVE SPACES                 TO DET-TITLE.                    
01563                                                                   
      ********   PRINT PRIOR YTD   

122804     MOVE PYTDAC-LBEN            TO DET-LBEN
122804     MOVE PYTDAC-LPRM            TO DET-LPRM
122804     MOVE PYTDAC-LCLM            TO DET-LCLM
122804     MOVE PYTDAC-ABEN            TO DET-ABEN
122804     MOVE PYTDAC-APRM            TO DET-APRM
122804     MOVE PYTDAC-ACLM            TO DET-ACLM
122804     MOVE PYTDAC-TPRM            TO DET-TPRM
122804     MOVE PYTDAC-TCOM            TO DET-TCOM
122804     MOVE 'PRIOR YTD         '   TO DET-TITLE

122804     MOVE ' '                    TO X
122804     MOVE DETAIL-LINE            TO P-DATA
122804     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT
122804     MOVE SPACES                 TO DET-TITLE
           .
122804 0492-PRINT-ITD-AC.

122804*    MOVE ITDAC-LBEN             TO DET-LBEN
122804*    MOVE ITDAC-LPRM             TO DET-LPRM
122804*    MOVE ITDAC-LCLM             TO DET-LCLM
122804*    MOVE ITDAC-ABEN             TO DET-ABEN
122804*    MOVE ITDAC-APRM             TO DET-APRM
122804*    MOVE ITDAC-ACLM             TO DET-ACLM
122804*    MOVE ITDAC-TPRM             TO DET-TPRM
122804*    MOVE ITDAC-TCOM             TO DET-TCOM
122804*    MOVE 'INCEPTION TO DATE '   TO DET-TITLE
122804*    MOVE ' '                    TO X
122804*    MOVE DETAIL-LINE            TO P-DATA
122804*    PERFORM 1100-WRITE-PRINT THRU 1199-EXIT

122804     MOVE SPACES                 TO DET-TITLE
122804                                                                  
01564      IF DTE-CLIENT = 'MON'                                        
01565          IF PASS-NUMBER = '3'                                     
01566              IF SAVE-AM-EXPIRES = ZEROS AND                       
01567                 SAVE-AM-HI-CERT = ZEROS                           
01568                  NEXT SENTENCE                                    
01569              ELSE                                                 
01570                  MOVE 'HIGH CERT DATE'                            
01571                                      TO  DET-DATE-DESC            
01572                  MOVE SAVE-AM-HI-MO  TO  DET-DATE-MO              
01573                  MOVE SAVE-AM-HI-DA  TO  DET-DATE-DA              
01574                  MOVE SAVE-AM-HI-YR  TO  DET-DATE-YR              
01575                  MOVE '/'            TO  DET-DATE-SLASH-1         
01576                                          DET-DATE-SLASH-2         
01577                  MOVE ' '            TO  X                        
01578                  MOVE DET-DATE-LINE  TO  P-DATA                   
01579                  PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT        
01580                  MOVE SPACES         TO  DET-DATE-LINE            
01581                  MOVE 'EXPIRATION DATE'                           
01582                                      TO  DET-DATE-DESC            
01583                  MOVE SAVE-AM-EXP-MO TO  DET-DATE-MO              
01584                  MOVE SAVE-AM-EXP-DA TO  DET-DATE-DA              
01585                  MOVE SAVE-AM-EXP-YR TO  DET-DATE-YR              
01586                  MOVE '/'            TO  DET-DATE-SLASH-1         
01587                                          DET-DATE-SLASH-2         
01588                  MOVE ' '            TO  X                        
01589                  MOVE DET-DATE-LINE  TO  P-DATA                   
01590                  PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT        
01591                  MOVE SPACES         TO  DET-DATE-LINE.           
01592                                                                   
01593      MOVE ZERO-ACCUM             TO YTD-ACCOUNT-ACCUM             
01594                                     YTD-STATE-ACCUM               
01595                                     LAST-12-STATE-ACCUM
122804*                                   ITD-ACCOUNT-ACCUM
122804                                    PYTD-ACCOUNT-ACCUM
122804*                                   PL12-ACCOUNT-ACCUM
01596      MOVE +0                     TO X1.                           
01597      PERFORM 0255-ZERO-ACCUM-STATE 15 TIMES.                      
01598                                                                   
01599  0495-ZERO-ACCOUNT.                                               
01600      MOVE ' '                    TO SKIP-HD-SW.                   
01601      MOVE +0                     TO X1.                           
01602      PERFORM 0230-ZERO-ACCUM-ACC 15 TIMES.                        
01603                                                                   
01604  0499-ACCOUNT-RETURN.                                             
01605      GO TO 0310-READ-EARNED-PREM-EXTRACT.                         
01606  EJECT                                                            
01607  0500-GROUPING-BREAK.                                             
01608      PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT.           
01609                                                                   
01610  0510-GROUPING-BREAK-1.                                           
01611      IF P-GP-SW = '1'                                             
01612          MOVE ' '                TO P-GP-SW                       
01613      ELSE                                                         
01614          GO TO 0580-ZERO-GROUPING.                                
01615                                                                   
01616      PERFORM 1010-GP-HD.                                          
01617      MOVE +1                     TO X1.                           
01618                                                                   
01619  0520-GROUPING-BREAK-2.                                           
01620      IF DTE-CLIENT = 'HER'                                        
01621          NEXT SENTENCE                                            
01622      ELSE                                                         
01623          GO TO 0550-GROUPING-BREAK-2.                             
01624                                                                   
01625      IF X1 GREATER 12                                             
01626          GO TO 0530-PRINT-LAST-GP-12.                             
01627                                                                   
01628      MOVE GP-DATE (X1)           TO  SAVE-HER-HI-CERT.            
01629                                                                   
01630      IF (GP-CERT (X1) = ZEROS                                     
01631         OR GP-ISS (X1) EQUAL ZEROS)                               
01632          MOVE SPACES             TO  DET-FILL                     
01633      ELSE                                                         
01634          MOVE SAVE-HER-HI-MO     TO  DET-HI-MO                    
01635          MOVE '/'                TO  DET-HI-SLASH                 
01636          MOVE SAVE-HER-HI-YR     TO  DET-HI-YR                    
01637          MOVE 0                  TO  SAVE-HER-HI-CERT.            
01638                                                                   
01639      MOVE GP-CERT (X1)           TO  DET-CERTS-2.                 
01640      MOVE GP-ISS  (X1)           TO  DET-ISS-COUNT-2.             
01641      MOVE GP-LBEN (X1)           TO  DET-LBEN-2.                  
01642      MOVE GP-LPRM (X1)           TO  DET-LPRM-2.                  
01643      MOVE GP-LCLM (X1)           TO  DET-LCLM-2.                  
01644      MOVE GP-APRM (X1)           TO  DET-APRM-2.                  
01645      MOVE GP-ACLM (X1)           TO  DET-ACLM-2.                  
01646      MOVE GP-TPRM (X1)           TO  DET-TPRM-2.                  
01647      MOVE GP-TCOM (X1)           TO  DET-TCOM-2.                  
01648      ADD +1                      TO  X1.                          
01649      MOVE COMP-YR (X1)           TO  DET-YR-2.                    
01650      MOVE COMP-MO (X1)           TO  DET-MO-2.                    
01651                                                                   
01652      PERFORM 1200-LOAD-ALPHA-MONTH  THRU  1299-EXIT.              
01653                                                                   
01654      MOVE DETAIL-LINE-2          TO  P-DATA.                      
01655                                                                   
01656      IF X1 = 2                                                    
01657          MOVE '0'                TO  X                            
01658      ELSE                                                         
01659          MOVE ' '                TO  X.                           
01660                                                                   
01661      PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.                   
01662                                                                   
01663      GO TO 0520-GROUPING-BREAK-2.                                 
01664                                                                   
01665  0530-PRINT-LAST-GP-12.                                           
01666      MOVE L12GP-LBEN             TO  DET-LBEN-2.                  
01667      MOVE L12GP-LPRM             TO  DET-LPRM-2.                  
01668      MOVE L12GP-LCLM             TO  DET-LCLM-2.                  
01669      MOVE L12GP-APRM             TO  DET-APRM-2.                  
01670      MOVE L12GP-ACLM             TO  DET-ACLM-2.                  
01671      MOVE L12GP-TPRM             TO  DET-TPRM-2.                  
01672      MOVE L12GP-TCOM             TO  DET-TCOM-2.                  
01673      MOVE 'LAST 12 MONTHS'       TO  DET-TITLE-2.                 
01674      MOVE '0'                    TO  X.                           
01675      MOVE DETAIL-LINE-2          TO  P-DATA.                      
01676                                                                   
01677      PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.                   
01678                                                                   
01679      MOVE ZERO-ACCUM             TO  LAST-12-GROUPING-ACCUM.      
01680                                                                   
01681  0540-PRINT-GP-YTD.                                               
01682      MOVE YTDGP-LBEN             TO  DET-LBEN-2.                  
01683      MOVE YTDGP-LPRM             TO  DET-LPRM-2.                  
01684      MOVE YTDGP-LCLM             TO  DET-LCLM-2.                  
01685      MOVE YTDGP-APRM             TO  DET-APRM-2.                  
01686      MOVE YTDGP-ACLM             TO  DET-ACLM-2.                  
01687      MOVE YTDGP-TPRM             TO  DET-TPRM-2.                  
01688      MOVE YTDGP-TCOM             TO  DET-TCOM-2.                  
01689      MOVE 'YEAR TO DATE'         TO  DET-TITLE-2.                 
01690      MOVE ' '                    TO  X.                           
01691      MOVE DETAIL-LINE-2          TO  P-DATA.                      
01692                                                                   
01693      PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.                   
01694                                                                   
01695      MOVE SPACES                 TO  DET-TITLE-2.                 
01696      MOVE ZERO-ACCUM             TO  YTD-GROUPING-ACCUM.          
01697                                                                   
01698      GO TO 0580-ZERO-GROUPING.                                    
01699                                                                   
01700  0550-GROUPING-BREAK-2.                                           
01701      IF X1 GREATER 12                                             
01702          GO TO 0560-PRINT-LAST-GP-12.                             
01703                                                                   
01704      MOVE GP-CERT (X1)           TO DET-CERTS.                    
01705      MOVE GP-LBEN (X1)           TO DET-LBEN.                     
01706      MOVE GP-LPRM (X1)           TO DET-LPRM.                     
01707      MOVE GP-LCLM (X1)           TO DET-LCLM.                     
01708      MOVE GP-ABEN (X1)           TO DET-ABEN.                     
01709      MOVE GP-APRM (X1)           TO DET-APRM.                     
01710      MOVE GP-ACLM (X1)           TO DET-ACLM.                     
01711      MOVE GP-TPRM (X1)           TO DET-TPRM.                     
01712      MOVE GP-TCOM (X1)           TO DET-TCOM.                     
01713      ADD +1 TO X1.                                                
01714      MOVE COMP-YR (X1)           TO DET-YR.                       
01715      MOVE COMP-MO (X1)           TO DET-MO.                       
01716                                                                   
01717      PERFORM 1200-LOAD-ALPHA-MONTH THRU 1299-EXIT.                
01718                                                                   
01719      MOVE DETAIL-LINE            TO P-DATA.                       
01720                                                                   
01721      IF X1 = 2                                                    
01722          MOVE '0'                TO X                             
01723      ELSE                                                         
01724          MOVE ' '                TO X.                            
01725                                                                   
01726      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01727      GO TO 0550-GROUPING-BREAK-2.                                 
01728                                                                   
01729  0560-PRINT-LAST-GP-12.                                           
01730      MOVE L12GP-LBEN             TO DET-LBEN.                     
01731      MOVE L12GP-LPRM             TO DET-LPRM.                     
01732      MOVE L12GP-LCLM             TO DET-LCLM.                     
01733      MOVE L12GP-ABEN             TO DET-ABEN.                     
01734      MOVE L12GP-APRM             TO DET-APRM.                     
01735      MOVE L12GP-ACLM             TO DET-ACLM.                     
01736      MOVE L12GP-TPRM             TO DET-TPRM.                     
01737      MOVE L12GP-TCOM             TO DET-TCOM.                     
01738      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    
01739                                                                   
01740      IF DTE-CLIENT = 'AFL'                                        
01741          MOVE 'FISCAL TO DATE'   TO DET-TITLE.                    
01742                                                                   
01743      MOVE '0'                    TO X.                            
01744      MOVE DETAIL-LINE            TO P-DATA.                       
01745      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01746      MOVE ZERO-ACCUM             TO LAST-12-GROUPING-ACCUM.       
01747                                                                   
122804*    MOVE PL12GP-LBEN             TO DET-LBEN.                     
122804*    MOVE PL12GP-LPRM             TO DET-LPRM.                     
122804*    MOVE PL12GP-LCLM             TO DET-LCLM.                     
122804*    MOVE PL12GP-ABEN             TO DET-ABEN.                     
122804*    MOVE PL12GP-APRM             TO DET-APRM.                     
122804*    MOVE PL12GP-ACLM             TO DET-ACLM.                     
122804*    MOVE PL12GP-TPRM             TO DET-TPRM.                     
122804*    MOVE PL12GP-TCOM             TO DET-TCOM.                     
122804*    MOVE 'PRIOR L12         '   TO DET-TITLE.                    
122804*    MOVE ' '                    TO X.                            
122804*    MOVE DETAIL-LINE            TO P-DATA.                       
122804*    PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01746 *    MOVE ZERO-ACCUM             TO PL12-GROUPING-ACCUM
           .
01748  0570-PRINT-GP-YTD.                                               
01749      MOVE YTDGP-LBEN             TO DET-LBEN.                     
01750      MOVE YTDGP-LPRM             TO DET-LPRM.                     
01751      MOVE YTDGP-LCLM             TO DET-LCLM.                     
01752      MOVE YTDGP-ABEN             TO DET-ABEN.                     
01753      MOVE YTDGP-APRM             TO DET-APRM.                     
01754      MOVE YTDGP-ACLM             TO DET-ACLM.                     
01755      MOVE YTDGP-TPRM             TO DET-TPRM.                     
01756      MOVE YTDGP-TCOM             TO DET-TCOM.                     
01757      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    
01758      MOVE ' '                    TO X.                            
01759      MOVE DETAIL-LINE            TO P-DATA.                       
01760      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01761      MOVE SPACES                 TO DET-TITLE.                    
01762      MOVE ZERO-ACCUM             TO YTD-GROUPING-ACCUM.           
01763                                                                   
122804     MOVE PYTDGP-LBEN             TO DET-LBEN.                     
122804     MOVE PYTDGP-LPRM             TO DET-LPRM.                     
122804     MOVE PYTDGP-LCLM             TO DET-LCLM.                     
122804     MOVE PYTDGP-ABEN             TO DET-ABEN.                     
122804     MOVE PYTDGP-APRM             TO DET-APRM.                     
122804     MOVE PYTDGP-ACLM             TO DET-ACLM.                     
122804     MOVE PYTDGP-TPRM             TO DET-TPRM.                     
122804     MOVE PYTDGP-TCOM             TO DET-TCOM.                     
122804     MOVE 'PRIOR YTD         '   TO DET-TITLE.                    
122804     MOVE ' '                    TO X.                            
122804     MOVE DETAIL-LINE            TO P-DATA.                       
122804     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     

01762      MOVE ZERO-ACCUM             TO PYTD-GROUPING-ACCUM

122804*    MOVE ITDGP-LBEN             TO DET-LBEN.                     
122804*    MOVE ITDGP-LPRM             TO DET-LPRM.                     
122804*    MOVE ITDGP-LCLM             TO DET-LCLM.                     
122804*    MOVE ITDGP-ABEN             TO DET-ABEN.                     
122804*    MOVE ITDGP-APRM             TO DET-APRM.                     
122804*    MOVE ITDGP-ACLM             TO DET-ACLM.                     
122804*    MOVE ITDGP-TPRM             TO DET-TPRM.                     
122804*    MOVE ITDGP-TCOM             TO DET-TCOM.                     
122804*    MOVE 'INCEPTION TO DATE '   TO DET-TITLE.                    
122804*    MOVE ' '                    TO X.                            
122804*    MOVE DETAIL-LINE            TO P-DATA.                       
122804*    PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     

122804     MOVE SPACES                 TO DET-TITLE.                    
122804*    MOVE ZERO-ACCUM             TO ITD-GROUPING-ACCUM.           
122804                                                                  
01764  0580-ZERO-GROUPING.                                              
01765      MOVE +0                     TO X1.                           
01766      PERFORM 0240-ZERO-ACCUM-GRP 15 TIMES.                        
01767                                                                   
01768  0590-GROUPING-RETURN.                                            
01769      GO TO 0310-READ-EARNED-PREM-EXTRACT.                         
01770  EJECT                                                            
01771  0600-CARRIER-BREAK.                                              
01772      PERFORM 0400-ACCOUNT-BREAK    THRU 0495-ZERO-ACCOUNT.        
01773  0605-CARRIER-BREAK-5.                                            
01774      PERFORM 0510-GROUPING-BREAK-1 THRU 0580-ZERO-GROUPING.       
01775                                                                   
01776  0610-CARRIER-BREAK-1.                                            
01777      IF P-CA-SW = '1'                                             
01778          MOVE ' '                TO P-CA-SW                       
01779      ELSE                                                         
01780          GO TO 0680-ZERO-CARRIER.                                 
01781                                                                   
01782      PERFORM 1020-CA-HD.                                          
01783      MOVE +1                     TO X1.                           
01784                                                                   
01785  0620-CARRIER-BREAK-2.                                            
01786      IF DTE-CLIENT = 'HER'                                        
01787          NEXT SENTENCE                                            
01788      ELSE                                                         
01789          GO TO 0650-CARRIER-BREAK-2.                              
01790                                                                   
01791      IF X1 GREATER 12                                             
01792          GO TO 0630-PRINT-LAST-CA-12.                             
01793                                                                   
01794      MOVE CA-DATE (X1)           TO  SAVE-HER-HI-CERT.            
01795                                                                   
01796      IF (CA-CERT (X1) = ZEROS                                     
01797         OR CA-ISS (X1) EQUAL ZEROS)                               
01798          MOVE SPACES             TO  DET-FILL                     
01799      ELSE                                                         
01800          MOVE SAVE-HER-HI-MO     TO  DET-HI-MO                    
01801          MOVE '/'                TO  DET-HI-SLASH                 
01802          MOVE SAVE-HER-HI-YR     TO  DET-HI-YR                    
01803          MOVE 0                  TO  SAVE-HER-HI-CERT.            
01804                                                                   
01805      MOVE CA-CERT (X1)           TO  DET-CERTS-2.                 
01806      MOVE CA-ISS  (X1)           TO  DET-ISS-COUNT-2.             
01807      MOVE CA-LBEN (X1)           TO  DET-LBEN-2.                  
01808      MOVE CA-LPRM (X1)           TO  DET-LPRM-2.                  
01809      MOVE CA-LCLM (X1)           TO  DET-LCLM-2.                  
01810      MOVE CA-APRM (X1)           TO  DET-APRM-2.                  
01811      MOVE CA-ACLM (X1)           TO  DET-ACLM-2.                  
01812      MOVE CA-TPRM (X1)           TO  DET-TPRM-2.                  
01813      MOVE CA-TCOM (X1)           TO  DET-TCOM-2.                  
01814      ADD +1                      TO  X1.                          
01815      MOVE COMP-YR (X1)           TO  DET-YR-2.                    
01816      MOVE COMP-MO (X1)           TO  DET-MO-2.                    
01817                                                                   
01818      PERFORM 1200-LOAD-ALPHA-MONTH  THRU  1299-EXIT.              
01819                                                                   
01820      MOVE DETAIL-LINE-2          TO  P-DATA.                      
01821                                                                   
01822      IF X1 = 2                                                    
01823          MOVE '0'                TO  X                            
01824      ELSE                                                         
01825          MOVE ' '                TO  X.                           
01826                                                                   
01827      PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.                   
01828                                                                   
01829      GO TO 0620-CARRIER-BREAK-2.                                  
01830                                                                   
01831  0630-PRINT-LAST-CA-12.                                           
01832      MOVE L12CA-LBEN             TO  DET-LBEN-2.                  
01833      MOVE L12CA-LPRM             TO  DET-LPRM-2.                  
01834      MOVE L12CA-LCLM             TO  DET-LCLM-2.                  
01835      MOVE L12CA-APRM             TO  DET-APRM-2.                  
01836      MOVE L12CA-ACLM             TO  DET-ACLM-2.                  
01837      MOVE L12CA-TPRM             TO  DET-TPRM-2.                  
01838      MOVE L12CA-TCOM             TO  DET-TCOM-2.                  
01839      MOVE 'LAST 12 MONTHS'       TO  DET-TITLE-2.                 
01840      MOVE '0'                    TO  X.                           
01841      MOVE DETAIL-LINE-2          TO  P-DATA.                      
01842                                                                   
01843      PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.                   
01844                                                                   
01845      MOVE ZERO-ACCUM             TO  LAST-12-CARRIER-ACCUM.       
01846                                                                   
01847  0640-PRINT-CA-YTD.                                               
01848      MOVE YTDCA-LBEN             TO  DET-LBEN-2.                  
01849      MOVE YTDCA-LPRM             TO  DET-LPRM-2.                  
01850      MOVE YTDCA-LCLM             TO  DET-LCLM-2.                  
01851      MOVE YTDCA-APRM             TO  DET-APRM-2.                  
01852      MOVE YTDCA-ACLM             TO  DET-ACLM-2.                  
01853      MOVE YTDCA-TPRM             TO  DET-TPRM-2.                  
01854      MOVE YTDCA-TCOM             TO  DET-TCOM-2.                  
01855      MOVE 'YEAR TO DATE'         TO  DET-TITLE-2.                 
01856      MOVE ' '                    TO  X.                           
01857      MOVE DETAIL-LINE-2          TO  P-DATA.                      
01858                                                                   
01842                                                                   
01859      PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.                   
01860                                                                   
01861      MOVE SPACES                 TO  DET-TITLE-2.                 
01862      MOVE ZERO-ACCUM             TO  YTD-CARRIER-ACCUM.           
01863                                                                   
01864      GO TO 0680-ZERO-CARRIER.                                     
01865                                                                   
01866  0650-CARRIER-BREAK-2.                                            
01867      IF X1 GREATER 12                                             
01868          GO TO 0660-PRINT-LAST-CA-12.                             
01869                                                                   
01870      MOVE CA-CERT (X1)           TO DET-CERTS.                    
01871      MOVE CA-LBEN (X1)           TO DET-LBEN.                     
01872      MOVE CA-LPRM (X1)           TO DET-LPRM.                     
01873      MOVE CA-LCLM (X1)           TO DET-LCLM.                     
01874      MOVE CA-ABEN (X1)           TO DET-ABEN.                     
01875      MOVE CA-APRM (X1)           TO DET-APRM.                     
01876      MOVE CA-ACLM (X1)           TO DET-ACLM.                     
01877      MOVE CA-TPRM (X1)           TO DET-TPRM.                     
01878      MOVE CA-TCOM (X1)           TO DET-TCOM.                     
01879      ADD +1 TO X1.                                                
01880      MOVE COMP-YR (X1)           TO DET-YR.                       
01881      MOVE COMP-MO (X1)           TO DET-MO.                       
01882                                                                   
01883      PERFORM 1200-LOAD-ALPHA-MONTH THRU 1299-EXIT.                
01884                                                                   
01885      MOVE DETAIL-LINE            TO P-DATA.                       
01886                                                                   
01887      IF X1 = 2                                                    
01888          MOVE '0'                TO X                             
01889      ELSE                                                         
01890          MOVE ' '                TO X.                            
01891                                                                   
01892      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01893      GO TO 0650-CARRIER-BREAK-2.                                  
01894                                                                   
01895  0660-PRINT-LAST-CA-12.                                           
01896      MOVE L12CA-LBEN             TO DET-LBEN.                     
01897      MOVE L12CA-LPRM             TO DET-LPRM.                     
01898      MOVE L12CA-LCLM             TO DET-LCLM.                     
01899      MOVE L12CA-ABEN             TO DET-ABEN.                     
01900      MOVE L12CA-APRM             TO DET-APRM.                     
01901      MOVE L12CA-ACLM             TO DET-ACLM.                     
01902      MOVE L12CA-TPRM             TO DET-TPRM.                     
01903      MOVE L12CA-TCOM             TO DET-TCOM.                     
01904      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    
01905                                                                   
01906      IF DTE-CLIENT = 'AFL'                                        
01907          MOVE 'FISCAL TO DATE'   TO DET-TITLE.                    
01908                                                                   
01909      MOVE '0'                    TO X.                            
01910      MOVE DETAIL-LINE            TO P-DATA.                       
01911      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01912      MOVE ZERO-ACCUM             TO LAST-12-CARRIER-ACCUM.        
01913                                                                   
122804*    MOVE PL12CA-LBEN             TO DET-LBEN.                     
122804*    MOVE PL12CA-LPRM             TO DET-LPRM.                     
122804*    MOVE PL12CA-LCLM             TO DET-LCLM.                     
122804*    MOVE PL12CA-ABEN             TO DET-ABEN.                     
122804*    MOVE PL12CA-APRM             TO DET-APRM.                     
122804*    MOVE PL12CA-ACLM             TO DET-ACLM.                     
122804*    MOVE PL12CA-TPRM             TO DET-TPRM.                     
122804*    MOVE PL12CA-TCOM             TO DET-TCOM.                     
122804*    MOVE 'PRIOR L12         '   TO DET-TITLE.                    
122804*    MOVE ' '                    TO X.                            
122804*    MOVE DETAIL-LINE            TO P-DATA.                       
122804*    PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01912 *    MOVE ZERO-ACCUM             TO PL12-CARRIER-ACCUM.

01914  0670-PRINT-CA-YTD.                                               
01915      MOVE YTDCA-LBEN             TO DET-LBEN.                     
01916      MOVE YTDCA-LPRM             TO DET-LPRM.                     
01917      MOVE YTDCA-LCLM             TO DET-LCLM.                     
01918      MOVE YTDCA-ABEN             TO DET-ABEN.                     
01919      MOVE YTDCA-APRM             TO DET-APRM.                     
01920      MOVE YTDCA-ACLM             TO DET-ACLM.                     
01921      MOVE YTDCA-TPRM             TO DET-TPRM.                     
01922      MOVE YTDCA-TCOM             TO DET-TCOM.                     
01923      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    
01924      MOVE ' '                    TO X.                            
01925      MOVE DETAIL-LINE            TO P-DATA.                       
01926      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01927      MOVE SPACES                 TO DET-TITLE.                    
01928      MOVE ZERO-ACCUM             TO YTD-CARRIER-ACCUM.            
01929                                                                   
122804     MOVE PYTDCA-LBEN             TO DET-LBEN.                     
122804     MOVE PYTDCA-LPRM             TO DET-LPRM.                     
122804     MOVE PYTDCA-LCLM             TO DET-LCLM.                     
122804     MOVE PYTDCA-ABEN             TO DET-ABEN.                     
122804     MOVE PYTDCA-APRM             TO DET-APRM.                     
122804     MOVE PYTDCA-ACLM             TO DET-ACLM.                     
122804     MOVE PYTDCA-TPRM             TO DET-TPRM.                     
122804     MOVE PYTDCA-TCOM             TO DET-TCOM.                     
122804     MOVE 'PRIOR YTD         '   TO DET-TITLE.                    
122804     MOVE ' '                    TO X.                            
122804     MOVE DETAIL-LINE            TO P-DATA.                       
122804     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     

01928      MOVE ZERO-ACCUM             TO PYTD-CARRIER-ACCUM.            

122804*    MOVE ITDCA-LBEN             TO DET-LBEN.                     
122804*    MOVE ITDCA-LPRM             TO DET-LPRM.                     
122804*    MOVE ITDCA-LCLM             TO DET-LCLM.                     
122804*    MOVE ITDCA-ABEN             TO DET-ABEN.                     
122804*    MOVE ITDCA-APRM             TO DET-APRM.                     
122804*    MOVE ITDCA-ACLM             TO DET-ACLM.                     
122804*    MOVE ITDCA-TPRM             TO DET-TPRM.                     
122804*    MOVE ITDCA-TCOM             TO DET-TCOM.                     
122804*    MOVE 'INCEPTION TO DATE '   TO DET-TITLE.                    
122804*    MOVE ' '                    TO X.                            
122804*    MOVE DETAIL-LINE            TO P-DATA.                       
122804*    PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     


122804     MOVE SPACES                 TO DET-TITLE.                    
122804*    MOVE ZERO-ACCUM             TO ITD-CARRIER-ACCUM.            
122804                                                                  
01930  0680-ZERO-CARRIER.                                               
01931      MOVE +0                     TO X1.                           
01932      PERFORM 0250-ZERO-ACCUM-CARR 15 TIMES.                       
01933                                                                   
01934  0690-CARRIER-RETURN.                                             
01935      GO TO 0310-READ-EARNED-PREM-EXTRACT.                         
01936  EJECT                                                            
01937  0700-REPTCD-BREAK.                                               
01938      MOVE +0                     TO X1.                           
01939      PERFORM 0220-ZERO-PRINT-TABLE 12 TIMES.                      
01940      PERFORM 0800-PRINT-DECISION THRU 0899-EXIT.                  
01941                                                                   
01942      IF P-RP-SW = '1'                                             
01943          MOVE ' '                TO P-RP-SW                       
01944      ELSE                                                         
01945          GO TO 0790-ZERO-REPTCD.                                  
01946                                                                   
01947      PERFORM 0900-HEAD-RTN THRU 0999-EXIT.                        
01948                                                                   
01949      MOVE +1                     TO X1.                           
01950      MOVE +0                     TO Y1.                           
01951                                                                   
01952  0710-REPTCD-BREAK-PRINT.                                         
01953      IF DTE-CLIENT = 'HER'                                        
01954          NEXT SENTENCE                                            
01955      ELSE                                                         
01956          GO TO 0720-REPTCD-BREAK-POINT.                           
01957                                                                   
01958      ADD +1 TO X1 Y1.                                             
01959                                                                   
01960      IF X1 GREATER THAN 13                                        
01961          GO TO 0750-PRINT-LAST-RP-12.                             
01962                                                                   
01963      COMPUTE RPCERT = RP-CERT (X1) - RP-CERT (Y1).                
01964      COMPUTE RPLBEN = RP-LBEN (X1) - RP-LBEN (Y1).                
01965      COMPUTE RPLPRM = RP-LPRM (X1) - RP-LPRM (Y1).                
01966      COMPUTE RPLCLM = RP-LCLM (X1) - RP-LCLM (Y1).                
01967      COMPUTE RPABEN = RP-ABEN (X1) - RP-ABEN (Y1).                
01968      COMPUTE RPAPRM = RP-APRM (X1) - RP-APRM (Y1).                
01969      COMPUTE RPACLM = RP-ACLM (X1) - RP-ACLM (Y1).                
01970      COMPUTE RPTCOM = RP-TCOM (X1) - RP-TCOM (Y1).                
01971      COMPUTE RPTPRM = RPLPRM  + RPAPRM.                           
01972                                                                   
01973      MOVE COMP-YR (X1)           TO  DET-YR-2.                    
01974      MOVE COMP-MO (X1)           TO  DET-MO-2.                    
01975                                                                   
01976      PERFORM 1200-LOAD-ALPHA-MONTH  THRU  1299-EXIT.              
01977                                                                   
01978      MOVE RP-DATE (X1)           TO  SAVE-HER-HI-CERT.            
01979                                                                   
01980      IF RP-CERT (X1) = ZEROS                                      
01981          MOVE SPACES             TO  DET-FILL                     
01982      ELSE                                                         
01983          MOVE SAVE-HER-HI-MO     TO  DET-HI-MO                    
01984          MOVE '/'                TO  DET-HI-SLASH                 
01985          MOVE SAVE-HER-HI-YR     TO  DET-HI-YR                    
01986          MOVE 0                  TO  SAVE-HER-HI-CERT.            
01987                                                                   
01988      MOVE RPCERT                 TO  DET-CERTS-2.                 
01989      MOVE RPLBEN                 TO  DET-LBEN-2.                  
01990      MOVE RPLPRM                 TO  DET-LPRM-2.                  
01991      MOVE RPLCLM                 TO  DET-LCLM-2.                  
01992      MOVE RPAPRM                 TO  DET-APRM-2.                  
01993      MOVE RPACLM                 TO  DET-ACLM-2.                  
01994      MOVE RPTPRM                 TO  DET-TPRM-2.                  
01995      MOVE RPTCOM                 TO  DET-TCOM-2.                  
01996                                                                   
01997      IF COMP-YR (X1) = CONV-YR                                    
01998        AND COMP-MO (X1) = CONV-MO                                 
01999          MOVE ZEROS              TO  DET-CERTS-2  DET-LBEN-2      
02000                                      DET-LPRM-2   DET-APRM-2      
02001                                      DET-LCLM-2   DET-ACLM-2      
02002                                      DET-TPRM-2   DET-TCOM-2      
02003                                      DET-ISS-COUNT-2              
02004      ELSE                                                         
02005          PERFORM 0730-ADD-TO-L12-YTD  THRU  0740-EXIT.            
02006                                                                   
02007      MOVE DETAIL-LINE-2          TO  P-DATA.                      
02008                                                                   
02009      IF X1 = 2                                                    
02010          MOVE '0'                TO  X                            
02011      ELSE                                                         
02012          MOVE ' '                TO  X.                           
02013                                                                   
02014      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02015                                                                   
02016      GO TO 0710-REPTCD-BREAK-PRINT.                               
02017                                                                   
02018  0720-REPTCD-BREAK-POINT.                                         
02019      ADD +1 TO X1 Y1.                                             
02020                                                                   
02021      IF X1 GREATER THAN 13                                        
02022          GO TO 0750-PRINT-LAST-RP-12.                             
02023                                                                   
02024      COMPUTE RPCERT = RP-CERT (X1) - RP-CERT (Y1).                
02025      COMPUTE RPLBEN = RP-LBEN (X1) - RP-LBEN (Y1).                
02026      COMPUTE RPLPRM = RP-LPRM (X1) - RP-LPRM (Y1).                
02027      COMPUTE RPLCLM = RP-LCLM (X1) - RP-LCLM (Y1).                
02028      COMPUTE RPABEN = RP-ABEN (X1) - RP-ABEN (Y1).                
02029      COMPUTE RPAPRM = RP-APRM (X1) - RP-APRM (Y1).                
02030      COMPUTE RPACLM = RP-ACLM (X1) - RP-ACLM (Y1).                
02031      COMPUTE RPTCOM = RP-TCOM (X1) - RP-TCOM (Y1).                
02032      COMPUTE RPTPRM = RPLPRM  + RPAPRM.                           
02033      MOVE COMP-YR (X1)           TO DET-YR.                       
02034      MOVE COMP-MO (X1)           TO DET-MO.                       
02035      PERFORM 1200-LOAD-ALPHA-MONTH THRU 1299-EXIT.                
02036                                                                   
02037      MOVE RPCERT                 TO DET-CERTS.                    
02038      MOVE RPLBEN                 TO DET-LBEN.                     
02039      MOVE RPLPRM                 TO DET-LPRM.                     
02040      MOVE RPLCLM                 TO DET-LCLM.                     
02041      MOVE RPABEN                 TO DET-ABEN.                     
02042      MOVE RPAPRM                 TO DET-APRM.                     
02043      MOVE RPACLM                 TO DET-ACLM.                     
02044      MOVE RPTPRM                 TO DET-TPRM.                     
02045      MOVE RPTCOM                 TO DET-TCOM.                     
02046                                                                   
02047      IF COMP-YR (X1) = CONV-YR AND                                
02048         COMP-MO (X1) = CONV-MO                                    
02049          MOVE ZEROS TO DET-CERTS DET-LBEN DET-LPRM                
02050                        DET-ABEN DET-APRM DET-LCLM                 
02051                        DET-ACLM DET-TPRM DET-TCOM                 
02052         ELSE                                                      
02053           PERFORM 0730-ADD-TO-L12-YTD THRU 0740-EXIT.             
02054                                                                   
02055      MOVE DETAIL-LINE            TO P-DATA.                       
02056                                                                   
02057      IF X1 = 2                                                    
02058          MOVE '0'                TO X                             
02059      ELSE                                                         
02060          MOVE ' '                TO X.                            
02061                                                                   
02062      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02063      GO TO 0720-REPTCD-BREAK-POINT.                               
02064                                                                   
02065  0730-ADD-TO-L12-YTD.                                             
02066      IF COMPARE9DT (X1) GREATER YEAR-OLD-DATE                     
02067          ADD RPLBEN TO L12RP-LBEN                                 
02068          ADD RPLPRM TO L12RP-LPRM                                 
02069          ADD RPLCLM TO L12RP-LCLM                                 
02070          ADD RPABEN TO L12RP-ABEN                                 
02071          ADD RPAPRM TO L12RP-APRM                                 
02072          ADD RPACLM TO L12RP-ACLM                                 
02073          ADD RPTPRM TO L12RP-TPRM                                 
02074          ADD RPTCOM TO L12RP-TCOM.                                
02075                                                                   
02076      IF RUN-YR = COMP-YR (X1)                                     
02077          ADD RPLBEN TO YTDRP-LBEN                                 
02078          ADD RPLPRM TO YTDRP-LPRM                                 
02079                        YTDRP-TPRM                                 
02080          ADD RPLCLM TO YTDRP-LCLM                                 
02081          ADD RPTCOM TO YTDRP-TCOM                                 
02082          ADD RPABEN TO YTDRP-ABEN                                 
02083          ADD RPAPRM TO YTDRP-APRM                                 
02084                        YTDRP-TPRM                                 
02085          ADD RPACLM TO YTDRP-ACLM.                                
02086                                                                   
122804*    IF X1 = 13
      *       COMPUTE ITDRP-LBEN = ITDRP-LBEN + RP-LBEN (X1)
      *       COMPUTE ITDRP-LPRM = ITDRP-LPRM + RP-LPRM (X1)
      *       COMPUTE ITDRP-LCLM = ITDRP-LCLM + RP-LCLM (X1)
      *       COMPUTE ITDRP-ABEN = ITDRP-ABEN + RP-ABEN (X1)
      *       COMPUTE ITDRP-APRM = ITDRP-APRM + RP-APRM (X1)
      *       COMPUTE ITDRP-ACLM = ITDRP-ACLM + RP-ACLM (X1)
      *       COMPUTE ITDRP-TPRM = ITDRP-TPRM + RP-TPRM (X1)
      *       COMPUTE ITDRP-TCOM = ITDRP-TCOM + RP-TCOM (X1)
      *    END-IF

122804     IF X1 = 13
              COMPUTE PYTDRP-LBEN = PYTDRP-LBEN +
                 (RP-LBEN (1) - RP-LBEN (15))
              COMPUTE PYTDRP-LPRM = PYTDRP-LPRM +
                 (RP-LPRM (1) - RP-LPRM (15))
              COMPUTE PYTDRP-LCLM = PYTDRP-LCLM +
                 (RP-LCLM (1) - RP-LCLM (15))
              COMPUTE PYTDRP-ABEN = PYTDRP-ABEN +
                 (RP-ABEN (1) - RP-ABEN (15))
              COMPUTE PYTDRP-APRM = PYTDRP-APRM +
                 (RP-APRM (1) - RP-APRM (15))
              COMPUTE PYTDRP-ACLM = PYTDRP-ACLM +
                 (RP-ACLM (1) - RP-ACLM (15))
              COMPUTE PYTDRP-TPRM = PYTDRP-TPRM +
                 (RP-TPRM (1) - RP-TPRM (15))
              COMPUTE PYTDRP-TCOM = PYTDRP-TCOM +
                 (RP-TCOM (1) - RP-TCOM (15))
           END-IF

122804*    IF X1 = 13
      *       COMPUTE PL12RP-LBEN = PL12RP-LBEN +
      *          (RP-LBEN (1) - RP-LBEN (14))
      *       COMPUTE PL12RP-LPRM = PL12RP-LPRM +
      *          (RP-LPRM (1) - RP-LPRM (14))
      *       COMPUTE PL12RP-LCLM = PL12RP-LCLM +
      *          (RP-LCLM (1) - RP-LCLM (14))
      *       COMPUTE PL12RP-ABEN = PL12RP-ABEN +
      *          (RP-ABEN (1) - RP-ABEN (14))
      *       COMPUTE PL12RP-APRM = PL12RP-APRM +
      *          (RP-APRM (1) - RP-APRM (14))
      *       COMPUTE PL12RP-ACLM = PL12RP-ACLM +
      *          (RP-ACLM (1) - RP-ACLM (14))
      *       COMPUTE PL12RP-TPRM = PL12RP-TPRM +
      *          (RP-TPRM (1) - RP-TPRM (14))
      *       COMPUTE PL12RP-TCOM = PL12RP-TCOM +
      *          (RP-TCOM (1) - RP-TCOM (14))
      *    END-IF

           .
02087  0740-EXIT.                                                       
02088       EXIT.                                                       
02089                                                                   
02090  0750-PRINT-LAST-RP-12.                                           
02091      IF DTE-CLIENT = 'HER'                                        
02092          NEXT SENTENCE                                            
02093      ELSE                                                         
02094          GO TO 0770-PRINT-LAST-RP-12.                             
02095                                                                   
02096      MOVE L12RP-LBEN             TO  DET-LBEN-2.                  
02097      MOVE L12RP-LPRM             TO  DET-LPRM-2.                  
02098      MOVE L12RP-LCLM             TO  DET-LCLM-2.                  
02099      MOVE L12RP-APRM             TO  DET-APRM-2.                  
02100      MOVE L12RP-ACLM             TO  DET-ACLM-2.                  
02101      MOVE L12RP-TPRM             TO  DET-TPRM-2.                  
02102      MOVE L12RP-TCOM             TO  DET-TCOM-2.                  
02103      MOVE 'LAST 12 MONTHS'       TO  DET-TITLE-2.                 
02104      MOVE '0'                    TO  X.                           
02105      MOVE DETAIL-LINE-2          TO  P-DATA.                      
02106                                                                   
02107      PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.                   
02108                                                                   
02109      MOVE ZERO-ACCUM             TO  LAST-12-REPTCD-ACCUM.        
02110                                                                   
02111  0760-PRINT-RP-YTD.                                               
02112      MOVE YTDRP-LBEN             TO  DET-LBEN-2.                  
02113      MOVE YTDRP-LPRM             TO  DET-LPRM-2.                  
02114      MOVE YTDRP-LCLM             TO  DET-LCLM-2.                  
02115      MOVE YTDRP-APRM             TO  DET-APRM-2.                  
02116      MOVE YTDRP-ACLM             TO  DET-ACLM-2.                  
02117      MOVE YTDRP-TPRM             TO  DET-TPRM-2.                  
02118      MOVE YTDRP-TCOM             TO  DET-TCOM-2.                  
02119      MOVE 'YEAR TO DATE'         TO  DET-TITLE-2.                 
02120      MOVE ' '                    TO  X.                           
02121      MOVE DETAIL-LINE-2          TO  P-DATA.                      
02122                                                                   
02123      PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.                   
02124                                                                   
02125      MOVE SPACES                 TO  DET-TITLE-2.                 
02126      MOVE ZERO-ACCUM             TO  YTD-REPTCD-ACCUM.            
02127                                                                   
02128      GO TO 0790-ZERO-REPTCD.                                      
02129                                                                   
02130  0770-PRINT-LAST-RP-12.                                           
02131      MOVE L12RP-LBEN             TO DET-LBEN.                     
02132      MOVE L12RP-LPRM             TO DET-LPRM.                     
02133      MOVE L12RP-LCLM             TO DET-LCLM.                     
02134      MOVE L12RP-ABEN             TO DET-ABEN.                     
02135      MOVE L12RP-APRM             TO DET-APRM.                     
02136      MOVE L12RP-ACLM             TO DET-ACLM.                     
02137      MOVE L12RP-TPRM             TO DET-TPRM.                     
02138      MOVE L12RP-TCOM             TO DET-TCOM.                     
02139      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    
02140                                                                   
02141      IF DTE-CLIENT = 'AFL'                                        
02142          MOVE 'FISCAL TO DATE'   TO DET-TITLE.                    
02143                                                                   
02144      MOVE '0'                    TO X.                            
02145      MOVE DETAIL-LINE            TO P-DATA.                       
02146      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02147      MOVE ZERO-ACCUM             TO LAST-12-REPTCD-ACCUM.         

122804*    MOVE PL12RP-LBEN             TO DET-LBEN
122804*    MOVE PL12RP-LPRM             TO DET-LPRM
122804*    MOVE PL12RP-LCLM             TO DET-LCLM
122804*    MOVE PL12RP-ABEN             TO DET-ABEN
122804*    MOVE PL12RP-APRM             TO DET-APRM
122804*    MOVE PL12RP-ACLM             TO DET-ACLM
122804*    MOVE PL12RP-TPRM             TO DET-TPRM
122804*    MOVE PL12RP-TCOM             TO DET-TCOM
122804*    MOVE 'PRIOR L12         '   TO DET-TITLE
122804*    MOVE ' '                    TO X
122804*    MOVE DETAIL-LINE            TO P-DATA
122804*    PERFORM 1100-WRITE-PRINT THRU 1199-EXIT

122804     MOVE SPACES                 TO DET-TITLE
122804*    MOVE ZERO-ACCUM             TO PL12-REPTCD-ACCUM
           .
02149  0780-PRINT-RP-YTD.                                               
02150      MOVE YTDRP-LBEN             TO DET-LBEN.                     
02151      MOVE YTDRP-LPRM             TO DET-LPRM.                     
02152      MOVE YTDRP-LCLM             TO DET-LCLM.                     
02153      MOVE YTDRP-ABEN             TO DET-ABEN.                     
02154      MOVE YTDRP-APRM             TO DET-APRM.                     
02155      MOVE YTDRP-ACLM             TO DET-ACLM.                     
02156      MOVE YTDRP-TPRM             TO DET-TPRM.                     
02157      MOVE YTDRP-TCOM             TO DET-TCOM.                     
02158      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    
02159      MOVE ' '                    TO X.                            
02160      MOVE DETAIL-LINE            TO P-DATA.                       
02161      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02162      MOVE SPACES                 TO DET-TITLE.                    
02163      MOVE ZERO-ACCUM             TO YTD-REPTCD-ACCUM.             
02164                                                                   
122804     MOVE PYTDRP-LBEN             TO DET-LBEN
122804     MOVE PYTDRP-LPRM             TO DET-LPRM
122804     MOVE PYTDRP-LCLM             TO DET-LCLM
122804     MOVE PYTDRP-ABEN             TO DET-ABEN
122804     MOVE PYTDRP-APRM             TO DET-APRM
122804     MOVE PYTDRP-ACLM             TO DET-ACLM
122804     MOVE PYTDRP-TPRM             TO DET-TPRM
122804     MOVE PYTDRP-TCOM             TO DET-TCOM
122804     MOVE 'PRIOR YTD         '   TO DET-TITLE
122804     MOVE ' '                    TO X
122804     MOVE DETAIL-LINE            TO P-DATA
122804     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT

122804     MOVE SPACES                 TO DET-TITLE
02163      MOVE ZERO-ACCUM             TO PYTD-REPTCD-ACCUM.
122804 0782-PRINT-RP-ITD.

122804*    MOVE ITDRP-LBEN             TO DET-LBEN
122804*    MOVE ITDRP-LPRM             TO DET-LPRM
122804*    MOVE ITDRP-LCLM             TO DET-LCLM
122804*    MOVE ITDRP-ABEN             TO DET-ABEN
122804*    MOVE ITDRP-APRM             TO DET-APRM
122804*    MOVE ITDRP-ACLM             TO DET-ACLM
122804*    MOVE ITDRP-TPRM             TO DET-TPRM
122804*    MOVE ITDRP-TCOM             TO DET-TCOM
122804*    MOVE 'INCEPTION TO DATE '   TO DET-TITLE
122804*    MOVE ' '                    TO X
122804*    MOVE DETAIL-LINE            TO P-DATA
122804*    PERFORM 1100-WRITE-PRINT THRU 1199-EXIT 

      *    MOVE ZERO-ACCUM             TO ITD-REPTCD-ACCUM
122804     .                                                            
02165  0790-ZERO-REPTCD.                                                
02166      MOVE +0                     TO X1.                           
02167      PERFORM 0260-ZERO-ACCUM-REPT 15 TIMES.                       
02168                                                                   
02169  0799-REPTCD-RETURN.                                              
02170      GO TO 0310-READ-EARNED-PREM-EXTRACT.                         
02171  EJECT                                                            
02172  0800-PRINT-DECISION.                                             
02173      MOVE +1                     TO X1.                           
02174      MOVE +0                     TO Y1.                           
02175                                                                   
02176  0810-PRINT-DECISION-1.                                           
02177      ADD +1 TO X1 Y1.                                             
02178                                                                   
02179      IF X1 GREATER 13                                             
02180          MOVE +0                 TO X1                            
02181          GO TO 0820-PRINT-DECISION-2.                             
02182                                                                   
02183      COMPUTE LBEN = AC-LBEN (X1) - AC-LBEN (Y1).                  
02184      COMPUTE LPRM = AC-LPRM (X1) - AC-LPRM (Y1).                  
02185      COMPUTE LCLM = AC-LCLM (X1) - AC-LCLM (Y1).                  
02186      COMPUTE ABEN = AC-ABEN (X1) - AC-ABEN (Y1).                  
02187      COMPUTE APRM = AC-APRM (X1) - AC-APRM (Y1).                  
02188      COMPUTE ACLM = AC-ACLM (X1) - AC-ACLM (Y1).                  
02189      COMPUTE TCOM = AC-TCOM (X1) - AC-TCOM (Y1).                  
02190                                                                   
02191      IF COMP-YR (X1) = CONV-YR AND                                
02192         COMP-MO (X1) = CONV-MO                                    
02193          MOVE ZEROS TO LBEN LPRM LCLM ABEN APRM ACLM TCOM.        
02194                                                                   
02195      IF DTE-CLIENT  =  'NCL'                                      
02196          MOVE LBEN               TO PLBEN (Y1)                    
02197          MOVE LPRM               TO PLPRM (Y1)                    
02198          MOVE LCLM               TO PLCLM (Y1)                    
02199          MOVE ABEN               TO PABEN (Y1)                    
02200          MOVE APRM               TO PAPRM (Y1)                    
02201          MOVE ACLM               TO PACLM (Y1)                    
02202          MOVE TCOM               TO PTCOM (Y1)                    
02203      ELSE                                                         
02204          MOVE LBEN               TO PLBEN (X1)                    
02205          MOVE LPRM               TO PLPRM (X1)                    
02206          MOVE LCLM               TO PLCLM (X1)                    
02207          MOVE ABEN               TO PABEN (X1)                    
02208          MOVE APRM               TO PAPRM (X1)                    
02209          MOVE ACLM               TO PACLM (X1)                    
02210          MOVE TCOM               TO PTCOM (X1).                   
02211                                                                   
02212      GO TO 0810-PRINT-DECISION-1.                                 
02213                                                                   
02214  0820-PRINT-DECISION-2.                                           
02215      ADD +1 TO X1.                                                
02216                                                                   
02217      IF X1 GREATER 12                                             
02218          GO TO 0899-EXIT.                                         
02219                                                                   
02220      IF (PRINT-ZERO-TABLE NOT = PRINT-TABLE (X1))
010306        OR ((AC-LPRM (1) - AC-LPRM (15)) NOT = ZEROS)
010306        OR ((AC-APRM (1) - AC-APRM (15)) NOT = ZEROS)
010306        OR ((AC-LCLM (1) - AC-LCLM (15)) NOT = ZEROS)
010306        OR ((AC-ACLM (1) - AC-ACLM (15)) NOT = ZEROS)
02221          MOVE '1'             TO P-ACC-SW P-ST-SW P-GP-SW P-CA-SW 
02222                                  P-RP-SW                          
031511                                 P-RP2-SW
02223          GO TO 0899-EXIT.                                         
02224                                                                   
02225      GO TO 0820-PRINT-DECISION-2.                                 
02226                                                                   
02227  0899-EXIT.                                                       
02228      EXIT.                                                        
02229  EJECT                                                            
02230  0900-HEAD-RTN.                                                   
02231      IF DTE-PGM-OPT NOT = 2                                       
02232          GO TO 0910-HEAD-RTN-A.                                   
02233                                                                   
02234      IF SET-CTR = +1                                              
02235          MOVE '-'                TO X                             
02236          MOVE SPACES             TO P-DATA                        
02237          PERFORM 1100-WRITE-PRINT THRU 1199-EXIT                  
02238          MOVE SPACES             TO P-DATA                        
02239          PERFORM 1100-WRITE-PRINT THRU 1199-EXIT                  
02240          MOVE ZERO               TO SET-CTR                       
02241          MOVE ' '                TO X                             
02242          GO TO 0920-HEAD-RTN-B.                                   
02243                                                                   
02244      MOVE +1                     TO SET-CTR.                      
02245                                                                   
02246  0910-HEAD-RTN-A.                                                 
02247      ADD +1 TO PAGE-CNT.                                          
02248      MOVE PAGE-CNT               TO HD-PAGE.                      
02249      MOVE '1'                    TO X.                            
02250                                                                   
02251      IF PASS-NUMBER = '1'                                         
02252          MOVE 'A'                TO HD-1-GRP-SUF                  
02253                                     HD-1-CAR-SUF                  
02254                                     HD-1-GT                       
02255          MOVE HD-1-STATE         TO P-DATA.                       
02256                                                                   
02257      IF PASS-NUMBER = '2'                                         
02258          MOVE 'B'                TO HD-1-GRP-SUF                  
02259                                     HD-1-CAR-SUF                  
02260                                     HD-1-GT                       
02261          MOVE HD-1-BUSINESS      TO P-DATA.                       
02262                                                                   
02263      IF PASS-NUMBER = '3'                                         
02264          MOVE 'C'                TO HD-1-GRP-SUF                  
02265                                     HD-1-CAR-SUF                  
02266                                     HD-1-GT                       
02267          MOVE HD-1-AGENCY        TO P-DATA.                       
02268                                                                   
02269      IF PASS-NUMBER = '4'                                         
112503         MOVE 'RPT CODE 1 '      TO HD-1-SLCT-FLD-1               
112503         MOVE S-EPX-A-RPT-CD-1   TO HD-1-SLCT-FLD-2               
112503         MOVE 'Y'                TO HD-1-SLCT-REC-POS1            
02270          MOVE 'D'                TO HD-1-DF                       
02271                                     HD-1-GRP-SUF                  
02272                                     HD-1-CAR-SUF                  
02273                                     HD-1-GT                       
02274          MOVE HD-1-RPT-CODE-1    TO P-DATA
112503         MOVE 'RPT CODE 1 '      TO HD-2-SLCT-FLD-1               
112503         MOVE S-EPX-A-RPT-CD-1   TO HD-2-SLCT-FLD-2               
112503         MOVE 'Y'                TO HD-2-SLCT-REC-POS1
112503         MOVE 'RPT CODE 1 '      TO HD-3-SLCT-FLD-1               
112503         MOVE S-EPX-A-RPT-CD-1   TO HD-3-SLCT-FLD-2               
112503         MOVE 'Y'                TO HD-3-SLCT-REC-POS1.            
02275                                                                   
02276      IF PASS-NUMBER = '5'                                         
02277          MOVE 'E'                TO HD-1-GRP-SUF                  
02278                                     HD-1-CAR-SUF                  
02279                                     HD-1-GT                       
02280          MOVE HD-1-RPT-CODE-2    TO P-DATA.                       
072709                                                                  
072709     IF PASS-NUMBER = '6'                                         
072709         MOVE 'USER SEL 2 '      TO HD-1-US2-FLD-1               
072709         MOVE S-EPX-A-RPT-CD-1   TO HD-1-US2-FLD-2               
072709         MOVE 'Y'                TO HD-1-US2-REC-POS1            
072709         MOVE 'F'                TO HD-1-GRP-SUF                  
072709                                    HD-1-CAR-SUF                  
072709                                    HD-1-GT                       
072709         MOVE HD-1-USER-SEL-2    TO P-DATA
072709         MOVE 'USER SEL 2 '      TO HD-2-SLCT-FLD-1               
072709         MOVE S-EPX-A-RPT-CD-1   TO HD-2-SLCT-FLD-2               
072709         MOVE 'Y'                TO HD-2-SLCT-REC-POS1
072709         MOVE 'USER SEL 2 '      TO HD-3-SLCT-FLD-1               
072709         MOVE S-EPX-A-RPT-CD-1   TO HD-3-SLCT-FLD-2               
072709         MOVE 'Y'                TO HD-3-SLCT-REC-POS1.            
031511                                                                  
031511     IF PASS-NUMBER = '7'                                         
031511         MOVE 'USER SEL 5 '      TO HD-1-US5-FLD-1               
031511         MOVE S-EPX-A-RPT-CD-1   TO HD-1-US5-FLD-2               
031511         MOVE 'Y'                TO HD-1-US5-REC-POS1            
031511         MOVE 'G'                TO HD-1-GRP-SUF                  
031511                                    HD-1-CAR-SUF                  
031511                                    HD-1-GT                       
031511         MOVE HD-1-USER-SEL-5    TO P-DATA
031511         MOVE 'USER SEL 5 '      TO HD-2-SLCT-FLD-1               
031511         MOVE S-EPX-A-RPT-CD-1   TO HD-2-SLCT-FLD-2               
031511         MOVE 'Y'                TO HD-2-SLCT-REC-POS1
031511         MOVE 'USER SEL 5 '      TO HD-3-SLCT-FLD-1               
031511         MOVE S-EPX-A-RPT-CD-1   TO HD-3-SLCT-FLD-2               
031511         MOVE 'Y'                TO HD-3-SLCT-REC-POS1.            

012517     IF PASS-NUMBER = '8'                                         
012517        MOVE 'RPT CODE 3 '       TO HD-1-SLCT-FLD-1               
012517                                    HD-1-RPT-CD-1
012517        MOVE S-EPX-A-RPT-CD-1    TO HD-1-SLCT-FLD-2               
012517        MOVE 'Y'                 TO HD-1-SLCT-REC-POS1            
012517        MOVE 'H'                 TO HD-1-DF                       
012517                                    HD-1-GRP-SUF                  
012517                                    HD-1-CAR-SUF                  
012517                                    HD-1-GT                       
012517        MOVE HD-1-RPT-CODE-1     TO P-DATA
012517        MOVE 'RPT CODE 3 '       TO HD-2-SLCT-FLD-1               
012517        MOVE S-EPX-A-RPT-CD-1    TO HD-2-SLCT-FLD-2               
012517        MOVE 'Y'                 TO HD-2-SLCT-REC-POS1
012517        MOVE 'RPT CODE 3 '       TO HD-3-SLCT-FLD-1               
012517        MOVE S-EPX-A-RPT-CD-1    TO HD-3-SLCT-FLD-2               
012517        MOVE 'Y'                 TO HD-3-SLCT-REC-POS1
012517     end-if

02282      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02283      MOVE ' '                    TO X.                            
02284      MOVE HD-2                   TO P-DATA.                       
02285      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02286      MOVE ' '                    TO X.                            
02287      MOVE HD-3                   TO P-DATA.                       
02288      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02289      MOVE '-'                    TO X.                            
02290                                                                   
02291  0920-HEAD-RTN-B.                                                 
02292      IF PASS-NUMBER = '1'                                         
02293          MOVE ' '                TO X                             
02294          MOVE S-EPX-CNTL-1       TO STATE-L                       
02295                                     HD-6-NSTATE                   
02296          PERFORM 1400-STATE-LOOKUP THRU 1499-EXIT                 
02297          IF STATE-L = SPACES                                      
02298              MOVE 'INVALID STATE'         TO HD-STATE             
02299              MOVE HD-6                    TO P-DATA               
02300              PERFORM 1100-WRITE-PRINT THRU 1199-EXIT              
02301           ELSE                                                    
02302              MOVE STATE-PIC (CLAS-INDEXS) TO HD-STATE             
02303              MOVE HD-6                    TO P-DATA               
02304              PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.             
02305                                                                   
02306      IF PASS-NUMBER = '2'                                         
02307          MOVE S-EPX-CNTL-1       TO HD-6-NSTATE                   
02308          MOVE 'BUSINESS TYPE'    TO FILLER-HD-6                   
02309          PERFORM 1510-BUSS-LOOKUP THRU 1599-EXIT                  
02310          MOVE ' '                TO X                             
02311          MOVE HD-6               TO P-DATA                        
02312          PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                 
02313                                                                   
02314      IF PASS-NUMBER = '3'                                         
02315          MOVE SPACE              TO HD-6-NSTATE                   
02316          MOVE 'AGENCY'           TO FILLER-HD-6                   
02317          MOVE S-EPX-CNTL-GA      TO HD-STATE                      
02318          MOVE ' '                TO X                             
02319          MOVE HD-6               TO P-DATA                        
02320          PERFORM 1100-WRITE-PRINT THRU 1199-EXIT                  
02321          IF S-EPX-CNTL-ACCT NOT = HIGH-VALUES                     
02322              MOVE S-EPX-CNTL-ACCT   TO HD-ACCOUNT                 
02323              MOVE SAVE-ACCT-NAME    TO HD-ACCT-NAME               
102004             MOVE SAVE-ACCT-STATUS  TO HD-ACCT-ADDRESS
02324              MOVE ' '               TO X                          
02325              MOVE HD-5              TO P-DATA                     
CIDMOD*            PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.             
CIDMOD             PERFORM 1100-WRITE-PRINT THRU 1199-EXIT              
CIDMOD                 MOVE SAVE-ACCT-CITY    TO HD-5A-CITY             
CIDMOD                 MOVE ' '               TO X                      
CIDMOD                 MOVE HD-5A             TO P-DATA                 
CIDMOD                 PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.         
02327                                                                   
02328      IF PASS-NUMBER = '4'                                         
02329          MOVE S-EPX-A-RPT-CD-1   TO HD-8A-RPT-CODE-1              
02330          MOVE ' '                TO X                             
02331          MOVE HD-8A              TO P-DATA                        
02332          PERFORM 1100-WRITE-PRINT THRU 1199-EXIT                  
02333 ********************************                                  
02334          IF SKIP-HD-SW = '1'                                      
02335              NEXT SENTENCE                                        
02336          ELSE                                                     
02337          IF DTE-TOT-OPT = 5 OR 6 OR 7                             
02338              MOVE S-EPX-A-CARR       TO HD-4-CARRIER              
02339              MOVE S-EPX-A-GROUP      TO HD-4-GROUPING             
02340              MOVE HD-4               TO P-DATA                    
02341              MOVE ' '                TO X                         
02342              PERFORM 1100-WRITE-PRINT THRU 1199-EXIT              
012703             MOVE SPACES             TO HD-6 
012703             MOVE 'STATE '           TO FILLER-HD-6-1 
02343              MOVE S-EPX-A-STATE      TO HD-6-NSTATE               
02344              MOVE HD-6               TO P-DATA                    
02345              MOVE ' '                TO X                         
02346              PERFORM 1100-WRITE-PRINT THRU 1199-EXIT              
02347              MOVE S-EPX-A-ACCT       TO HD-ACCOUNT                
02348              MOVE SAVE-ACCT-NAME     TO HD-ACCT-NAME              
                   MOVE SAVE-ACCT-STATUS   TO HD-ACCT-ADDRESS
02349              MOVE HD-5               TO P-DATA                    
02350              MOVE ' '                TO X                         
CIDMOD*            PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.             
CIDMOD             PERFORM 1100-WRITE-PRINT THRU 1199-EXIT              
CIDMOD                 MOVE SAVE-ACCT-CITY    TO HD-5A-CITY             
CIDMOD                 MOVE ' '               TO X                      
CIDMOD                 MOVE HD-5A             TO P-DATA                 
CIDMOD                 PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.         
02352 ********************************                                  
02353                                                                   
02354      IF PASS-NUMBER = '5'                                         
02355          MOVE S-EPX-A-CARR       TO HD-4-CARRIER                  
02356          MOVE S-EPX-A-GROUP      TO HD-4-GROUPING                 
02357          MOVE HD-4               TO P-DATA                        
02358          PERFORM 1100-WRITE-PRINT THRU 1199-EXIT                  
02359          MOVE 'STATE'            TO FILLER-HD-6                   
02360          MOVE S-EPX-A-STATE      TO HD-6-NSTATE                   
02361          MOVE STATE-TOTAL-LINE   TO FILLER-HD-6-2                 
02362          MOVE SPACES             TO STATE-TOTAL-LINE              
02363                                     HD-STATE                      
02364          MOVE HD-6               TO P-DATA                        
02365          MOVE ' '                TO X                             
02366          PERFORM 1100-WRITE-PRINT THRU 1199-EXIT                  
02367          MOVE S-EPX-A-RPT-CD-2   TO HD-8B-RPT-CODE-2              
02368          MOVE ' '                TO X                             
02369          MOVE HD-8B              TO P-DATA                        
02370          PERFORM 1100-WRITE-PRINT THRU 1199-EXIT                  
02371 ********************************                                  
02372          IF SKIP-HD-SW = '1'                                      
02373              NEXT SENTENCE                                        
02374          ELSE                                                     
02375          IF DTE-TOT-OPT = 6 OR 7                                  
02376              MOVE S-EPX-A-ACCT       TO HD-ACCOUNT                
02377              MOVE SAVE-ACCT-NAME     TO HD-ACCT-NAME              
102004             MOVE SAVE-ACCT-STATUS   TO HD-ACCT-ADDRESS
02378              MOVE HD-5               TO P-DATA                    
CIDMOD*            PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.             
CIDMOD             PERFORM 1100-WRITE-PRINT THRU 1199-EXIT              
CIDMOD                 MOVE SAVE-ACCT-CITY    TO HD-5A-CITY             
CIDMOD                 MOVE ' '               TO X                      
CIDMOD                 MOVE HD-5A             TO P-DATA                 
CIDMOD                 PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.         
02380 ********************************                                  
072709                                                                  
072709     IF PASS-NUMBER = '6'                                         
072709         MOVE S-EPX-A-RPT-CD-1   TO HD-8C-USER-SEL-2              
072709         MOVE ' '                TO X                             
072709         MOVE HD-8C              TO P-DATA                        
072709         PERFORM 1100-WRITE-PRINT THRU 1199-EXIT                  
072709********************************                                  
072709         IF SKIP-HD-SW = '1'                                      
072709             NEXT SENTENCE                                        
072709         ELSE                                                     
072709         IF DTE-TOT-OPT = 5 OR 6 OR 7                             
072709             MOVE S-EPX-A-CARR       TO HD-4-CARRIER              
072709             MOVE S-EPX-A-GROUP      TO HD-4-GROUPING             
072709             MOVE HD-4               TO P-DATA                    
072709             MOVE ' '                TO X                         
072709             PERFORM 1100-WRITE-PRINT THRU 1199-EXIT              
072709             MOVE SPACES             TO HD-6 
072709             MOVE 'STATE '           TO FILLER-HD-6-1 
072709             MOVE S-EPX-A-STATE      TO HD-6-NSTATE               
072709             MOVE HD-6               TO P-DATA                    
072709             MOVE ' '                TO X                         
072709             PERFORM 1100-WRITE-PRINT THRU 1199-EXIT              
072709             MOVE S-EPX-A-ACCT       TO HD-ACCOUNT                
072709             MOVE SAVE-ACCT-NAME     TO HD-ACCT-NAME              
072709             MOVE SAVE-ACCT-STATUS   TO HD-ACCT-ADDRESS
072709             MOVE HD-5               TO P-DATA                    
072709             MOVE ' '                TO X                         
072709*            PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.             
072709             PERFORM 1100-WRITE-PRINT THRU 1199-EXIT              
072709                 MOVE SAVE-ACCT-CITY    TO HD-5A-CITY             
072709                 MOVE ' '               TO X                      
072709                 MOVE HD-5A             TO P-DATA                 
072709                 PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.         
072709                                                                  
031511
031511     IF PASS-NUMBER = '7'
031511         MOVE S-EPX-A-RPT-CD-1   TO HD-8D-USER-SEL-5
031511         MOVE ' '                TO X
031511         MOVE HD-8D              TO P-DATA
031511         PERFORM 1100-WRITE-PRINT THRU 1199-EXIT
031511         IF SKIP-HD-SW = '2'
031511             NEXT SENTENCE
031511         ELSE
031511             MOVE S-EPX-A-RPT-CD-2   TO HD-4G-REPT-CD-2
031511             MOVE HD-4G              TO P-DATA
031511             MOVE ' '                TO X
031511             PERFORM 1100-WRITE-PRINT THRU 1199-EXIT
031511         END-IF
031511         IF SKIP-HD-SW = '1'
031511             NEXT SENTENCE
031511         ELSE
031511           IF DTE-TOT-OPT = 5 OR 6 OR 7
031511             MOVE SPACES             TO HD-6
031511             MOVE 'STATE '           TO FILLER-HD-6-1
031511             MOVE S-EPX-A-STATE      TO HD-6-NSTATE
031511             MOVE HD-6               TO P-DATA
031511             MOVE ' '                TO X
031511             PERFORM 1100-WRITE-PRINT THRU 1199-EXIT
031511             MOVE S-EPX-A-ACCT       TO HD-ACCOUNT
031511             MOVE SAVE-ACCT-NAME     TO HD-ACCT-NAME
031511             MOVE SAVE-ACCT-STATUS   TO HD-ACCT-ADDRESS
031511             MOVE HD-5               TO P-DATA
031511             MOVE ' '                TO X
031511             PERFORM 1100-WRITE-PRINT THRU 1199-EXIT
031511             MOVE SAVE-ACCT-CITY    TO HD-5A-CITY
031511             MOVE ' '               TO X
031511             MOVE HD-5A             TO P-DATA
031511             PERFORM 1100-WRITE-PRINT THRU 1199-EXIT
031511           END-IF
031511         END-IF
031511     END-IF.

012517     IF PASS-NUMBER = '8'                                         
012517        move 'RPT CODE 3'        TO HD-8A-RPT-CD-1-CAPTION
012517        MOVE S-EPX-A-RPT-CD-1    TO HD-8A-RPT-CODE-1              
012517        MOVE ' '                 TO X                             
012517        MOVE HD-8A               TO P-DATA                        
012517        PERFORM 1100-WRITE-PRINT THRU 1199-EXIT                  
012517********************************                                  
012517        IF SKIP-HD-SW = '1'                                      
012517           continue
012517        ELSE                                                     
012517           IF DTE-TOT-OPT = 5 OR 6 OR 7                             
012517              MOVE S-EPX-A-CARR  TO HD-4-CARRIER              
012517              MOVE S-EPX-A-GROUP TO HD-4-GROUPING             
012517              MOVE HD-4          TO P-DATA                    
012517              MOVE ' '           TO X                         
012517              PERFORM 1100-WRITE-PRINT THRU 1199-EXIT              
012517              MOVE SPACES        TO HD-6 
012517              MOVE 'STATE '      TO FILLER-HD-6-1 
012517              MOVE S-EPX-A-STATE TO HD-6-NSTATE               
012517              MOVE HD-6          TO P-DATA                    
012517              MOVE ' '           TO X                         
012517              PERFORM 1100-WRITE-PRINT THRU 1199-EXIT              
012517              MOVE S-EPX-A-ACCT  TO HD-ACCOUNT                
012517              MOVE SAVE-ACCT-NAME TO HD-ACCT-NAME              
012517              MOVE SAVE-ACCT-STATUS TO HD-ACCT-ADDRESS
012517              MOVE HD-5          TO P-DATA                    
012517              MOVE ' '           TO X                         
012517              PERFORM 1100-WRITE-PRINT THRU 1199-EXIT              
012517              MOVE SAVE-ACCT-CITY TO HD-5A-CITY             
012517              MOVE ' '           TO X                      
012517              MOVE HD-5A         TO P-DATA                 
012517              PERFORM 1100-WRITE-PRINT THRU 1199-EXIT
012517           end-if
012517        end-if
012517     end-if

02382      IF PASS-NUMBER NOT = '1' AND '4' AND '5'  
012517        AND '6' AND '7' and '8'
02383          MOVE S-EPX-CARR         TO HD-4-CARRIER                  
02384          MOVE S-EPX-GRP          TO HD-4-GROUPING                 
02385          MOVE HD-4               TO P-DATA                        
02386          PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                 
02387                                                                   
02388      MOVE '0'                    TO X.                            
02389                                                                   
02390      IF DTE-CLIENT = 'HER'                                        
02391          MOVE HD-9-A             TO  P-DATA                       
02392          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT                
02393          MOVE ' '                TO  X                            
02394          MOVE HD-10-A            TO  P-DATA                       
02395          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT                
02396      ELSE                                                         
02397          MOVE HD-9               TO  P-DATA                       
02398          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT                
02399          MOVE ' '                TO  X                            
02400          MOVE HD-10              TO  P-DATA                       
02401          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.               
02402                                                                   
02403  0999-EXIT.                                                       
02404      EXIT.                                                        
02405  EJECT                                                            
02406  1000-GR-HD.                                                      
02407      MOVE ZERO                   TO SET-CTR.                      
02408      ADD +1 TO PAGE-CNT.                                          
02409      MOVE PAGE-CNT               TO HD-PAGE.                      
02410      MOVE '1'                    TO X.                            
02411      MOVE HD-1-GRAND-TOTALS      TO P-DATA.                       
02412      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02413      MOVE ' '                    TO X.                            
02414      MOVE HD-2                   TO P-DATA.                       
02415      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02416      MOVE ' '                    TO X.                            
02417      MOVE HD-3                   TO P-DATA.                       
02418      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02419      MOVE '-'                    TO X.                            
02420                                                                   
02421      IF DTE-CLIENT = 'HER'                                        
02422          MOVE HD-9-A             TO  P-DATA                       
02423          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT                
02424          MOVE ' '                TO  X                            
02425          MOVE HD-10-A            TO  P-DATA                       
02426          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT                
02427      ELSE                                                         
02428          MOVE SPACES             TO HD-9-1 HD-10-1                
02429          MOVE HD-9               TO  P-DATA                       
02430          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT                
02431          MOVE ' '                TO  X                            
02432          MOVE HD-10              TO  P-DATA                       
02433          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.               
02434                                                                   
02435      MOVE '                NET '  TO  HD-9-1.                     
02436      MOVE '             ACTIVITY'  TO  HD-10-1.                   
02437      MOVE '       HI-CERT    CERT        NET '                    
02438                                  TO  HD-9-A-1.                    
02439      MOVE '         DATE    COUNT      ACTIVITY'                  
02440                                  TO  HD-10-A-1.                   
02441                                                                   
02442  1010-GP-HD.                                                      
02443      MOVE ZERO                   TO SET-CTR.                      
02444      ADD +1 TO PAGE-CNT.                                          
02445      MOVE PAGE-CNT               TO HD-PAGE.                      
02446      MOVE '1'                    TO X.                            
02447      MOVE HD-1-GROUPING          TO P-DATA.                       
02448                                                                   
02449      IF PASS-NUMBER = '5'                                         
02450          MOVE HD-1-RPT-CODE-2    TO P-DATA.                       
02451                                                                   
02452      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02453                                                                   
02454      MOVE ' '                    TO X.                            
02455      MOVE HD-2                   TO P-DATA.                       
02456      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02457      MOVE ' '                    TO X.                            
02458      MOVE HD-3                   TO P-DATA.                       
02459      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02460      MOVE '-'                    TO X.                            
02461                                                                   
02462      IF PASS-NUMBER = '2'                                         
02463          MOVE 'TOTAL BUSINESS TYPE'  TO FILLER-HD-7               
02464      ELSE                                                         
02465      IF PASS-NUMBER = '5'                                         
02466          MOVE 'TOTAL GROUPING'       TO FILLER-HD-7               
02467      ELSE                                                         
02468          MOVE 'TOTAL AGENCY'         TO FILLER-HD-7.              
02469                                                                   
02470      MOVE ' FOR CARRIER'         TO FILLER-HD-7A.                 
02471      MOVE S-EPX-CNTL-1           TO HD-7-DESC.                    
02472      MOVE S-EPX-CARR             TO HD-CARRIER.                   
02473      IF PASS-NUMBER = '5'                                         
02474          MOVE S-EPX-A-GROUP      TO HD-7-DESC                     
02475          MOVE S-EPX-A-CARR       TO HD-CARRIER.                   
02476      MOVE HD-7                   TO P-DATA.                       
02477      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02478      MOVE '0'                    TO X.                            
02479                                                                   
02480      MOVE '                NET '  TO  HD-9-1.                     
02481      MOVE '             ACTIVITY'  TO  HD-10-1.                   
02482      MOVE '       HI-CERT    CERT        NET '                    
02483                                  TO  HD-9-A-1.                    
02484      MOVE '         DATE    COUNT      ACTIVITY'                  
02485                                  TO  HD-10-A-1.                   
02486                                                                   
02487      IF DTE-CLIENT = 'HER'                                        
02488          MOVE HD-9-A             TO  P-DATA                       
02489          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT                
02490          MOVE ' '                TO  X                            
02491          MOVE HD-10-A            TO  P-DATA                       
02492          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT                
02493      ELSE                                                         
02494          MOVE HD-9               TO  P-DATA                       
02495          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT                
02496          MOVE ' '                TO  X                            
02497          MOVE HD-10              TO  P-DATA                       
02498          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.               
02499                                                                   
02500  1020-CA-HD.                                                      
02501      MOVE ZERO                   TO SET-CTR.                      
02502      ADD +1 TO PAGE-CNT.                                          
02503      MOVE PAGE-CNT               TO HD-PAGE.                      
02504      MOVE '1'                    TO X.                            
02505      MOVE HD-1-CARRIER           TO P-DATA.                       
02506      IF PASS-NUMBER = '5'                                         
02507          MOVE HD-1-RPT-CODE-2    TO P-DATA.                       
02508      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02509      MOVE ' '                    TO X.                            
02510      MOVE HD-2                   TO P-DATA.                       
02511      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02512      MOVE ' '                    TO X.                            
02513      MOVE HD-3                   TO P-DATA.                       
02514      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02515      MOVE '-'                    TO X.                            
02516                                                                   
02517      IF PASS-NUMBER = '2'                                         
02518          MOVE 'TOTAL BUSINESS TYPE'  TO FILLER-HD-7               
02519      ELSE                                                         
02520      IF PASS-NUMBER = '5'                                         
02521          MOVE 'TOTAL CARRIER'        TO FILLER-HD-7               
02522      ELSE                                                         
02523          MOVE 'TOTAL AGENCY'         TO FILLER-HD-7.              
02524                                                                   
02525      MOVE SPACE                  TO FILLER-HD-7A.                 
02526      MOVE S-EPX-CNTL-1           TO HD-7-DESC.                    
02527      IF PASS-NUMBER = '5'                                         
02528          MOVE S-EPX-A-CARR       TO HD-7-DESC.                    
02529      MOVE SPACE                  TO HD-CARRIER.                   
02530      MOVE HD-7                   TO P-DATA.                       
02531      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02532      MOVE '0'                    TO X.                            
02533                                                                   
02534      MOVE '                NET '  TO  HD-9-1.                     
02535      MOVE '             ACTIVITY'  TO  HD-10-1.                   
02536      MOVE '       HI-CERT    CERT        NET '                    
02537                                  TO  HD-9-A-1.                    
02538      MOVE '         DATE    COUNT      ACTIVITY'                  
02539                                  TO  HD-10-A-1.                   
02540                                                                   
02541      IF DTE-CLIENT = 'HER'                                        
02542          MOVE HD-9-A             TO  P-DATA                       
02543          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT                
02544          MOVE ' '                TO  X                            
02545          MOVE HD-10-A            TO  P-DATA                       
02546          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT                
02547      ELSE                                                         
02548          MOVE HD-9               TO  P-DATA                       
02549          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT                
02550          MOVE ' '                TO  X                            
02551          MOVE HD-10              TO  P-DATA                       
02552          PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.               
02553                                                                   
02554  EJECT                                                            
02555  1100-WRITE-PRINT.                                                
CIDMOD*              COPY ELCPRT2.                                      
CIDMOD               COPY PRTN036.
02557  1199-EXIT.                                                       
02558      EXIT.                                                        
02559  EJECT                                                            
02560  1200-LOAD-ALPHA-MONTH.                                           
02561      IF DTE-CLIENT = 'HER'                                        
02562          NEXT SENTENCE                                            
02563      ELSE                                                         
02564          GO TO 1210-LOAD-ALPHA-MONTH.                             
02565                                                                   
02566      IF DET-MO-2 = '01 '                                          
02567          MOVE 'JAN'              TO  DET-MO-2.                    
02568                                                                   
02569      IF DET-MO-2 = '02 '                                          
02570          MOVE 'FEB'              TO  DET-MO-2.                    
02571                                                                   
02572      IF DET-MO-2 = '03 '                                          
02573          MOVE 'MAR'              TO  DET-MO-2.                    
02574                                                                   
02575      IF DET-MO-2 = '04 '                                          
02576          MOVE 'APR'              TO  DET-MO-2.                    
02577                                                                   
02578      IF DET-MO-2 = '05 '                                          
02579          MOVE 'MAY'              TO  DET-MO-2.                    
02580                                                                   
02581      IF DET-MO-2 = '06 '                                          
02582          MOVE 'JUN'              TO  DET-MO-2.                    
02583                                                                   
02584      IF DET-MO-2 = '07 '                                          
02585          MOVE 'JUL'              TO  DET-MO-2.                    
02586                                                                   
02587      IF DET-MO-2 = '08 '                                          
02588          MOVE 'AUG'              TO  DET-MO-2.                    
02589                                                                   
02590      IF DET-MO-2 = '09 '                                          
02591          MOVE 'SEP'              TO  DET-MO-2.                    
02592                                                                   
02593      IF DET-MO-2 = '10 '                                          
02594          MOVE 'OCT'              TO  DET-MO-2.                    
02595                                                                   
02596      IF DET-MO-2 = '11 '                                          
02597          MOVE 'NOV'              TO  DET-MO-2.                    
02598                                                                   
02599      IF DET-MO-2 = '12 '                                          
02600          MOVE 'DEC'              TO  DET-MO-2.                    
02601                                                                   
02602      GO TO 1299-EXIT.                                             
02603                                                                   
02604  1210-LOAD-ALPHA-MONTH.                                           
02605      IF DET-MO = '01 ' MOVE 'JAN' TO DET-MO.                      
02606      IF DET-MO = '02 ' MOVE 'FEB' TO DET-MO.                      
02607      IF DET-MO = '03 ' MOVE 'MAR' TO DET-MO.                      
02608      IF DET-MO = '04 ' MOVE 'APR' TO DET-MO.                      
02609      IF DET-MO = '05 ' MOVE 'MAY' TO DET-MO.                      
02610      IF DET-MO = '06 ' MOVE 'JUN' TO DET-MO.                      
02611      IF DET-MO = '07 ' MOVE 'JUL' TO DET-MO.                      
02612      IF DET-MO = '08 ' MOVE 'AUG' TO DET-MO.                      
02613      IF DET-MO = '09 ' MOVE 'SEP' TO DET-MO.                      
02614      IF DET-MO = '10 ' MOVE 'OCT' TO DET-MO.                      
02615      IF DET-MO = '11 ' MOVE 'NOV' TO DET-MO.                      
02616      IF DET-MO = '12 ' MOVE 'DEC' TO DET-MO.                      
02617                                                                   
02618  1299-EXIT.                                                       
02619       EXIT.                                                       
02620  EJECT                                                            
02621  1300-PRINT-GRAND-TOTALS.                                         
02622      IF ZERO-GRAND-TOTALS = GRAND-TOTALS                          
02623          GO TO 1399-EXIT.                                         
02624                                                                   
02625      PERFORM 1000-GR-HD.                                          
02626      MOVE +1                     TO X1.                           
02627                                                                   
02628  1310-GRAND-TOTAL-1.                                              
02629      IF DTE-CLIENT = 'HER'                                        
02630          NEXT SENTENCE                                            
02631      ELSE                                                         
02632          GO TO 1330-GRAND-TOTAL-1.                                
02633                                                                   
02634      IF X1 GREATER 12                                             
02635          GO TO 1320-GRAND-TOTAL-2.                                
02636                                                                   
02637      MOVE GT-DATE (X1)           TO  SAVE-HER-HI-CERT.            
02638                                                                   
02639      IF (GT-CERT (X1) = ZEROS                                     
02640         OR GT-ISS (X1) EQUAL ZEROS)                               
02641          MOVE SPACES             TO  DET-FILL                     
02642      ELSE                                                         
02643          MOVE SAVE-HER-HI-MO     TO  DET-HI-MO                    
02644          MOVE '/'                TO  DET-HI-SLASH                 
02645          MOVE SAVE-HER-HI-YR     TO  DET-HI-YR                    
02646          MOVE 0                  TO  SAVE-HER-HI-CERT.            
02647                                                                   
02648      MOVE GT-CERT (X1)           TO  DET-CERTS-2.                 
02649      MOVE GT-ISS  (X1)           TO  DET-ISS-COUNT-2.             
02650      MOVE GT-LBEN (X1)           TO  DET-LBEN-2.                  
02651      MOVE GT-LPRM (X1)           TO  DET-LPRM-2.                  
02652      MOVE GT-LCLM (X1)           TO  DET-LCLM-2.                  
02653      MOVE GT-APRM (X1)           TO  DET-APRM-2.                  
02654      MOVE GT-ACLM (X1)           TO  DET-ACLM-2.                  
02655      MOVE GT-TPRM (X1)           TO  DET-TPRM-2.                  
02656      MOVE GT-TCOM (X1)           TO  DET-TCOM-2.                  
02657                                                                   
02658      ADD +1                      TO  X1.                          
02659                                                                   
02660      MOVE COMP-YR (X1)           TO  DET-YR-2.                    
02661      MOVE COMP-MO (X1)           TO  DET-MO-2.                    
02662                                                                   
02663      PERFORM 1200-LOAD-ALPHA-MONTH  THRU  1299-EXIT.              
02664                                                                   
02665      MOVE DETAIL-LINE-2          TO  P-DATA.                      
02666                                                                   
02667      IF X1 = 2                                                    
02668          MOVE '0'                TO  X                            
02669      ELSE                                                         
02670          MOVE ' '                TO  X.                           
02671                                                                   
02672      PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.                   
02673                                                                   
02674      GO TO 1310-GRAND-TOTAL-1.                                    
02675                                                                   
02676  1320-GRAND-TOTAL-2.                                              
02677      MOVE 'GRAND TOTALS'         TO  DET-TITLE-2.                 
02678      MOVE '-'                    TO  X.                           
02679      MOVE GR-LBEN                TO  DET-LBEN-2.                  
02680      MOVE GR-LPRM                TO  DET-LPRM-2.                  
02681      MOVE GR-LCLM                TO  DET-LCLM-2.                  
02682      MOVE GR-APRM                TO  DET-APRM-2.                  
02683      MOVE GR-ACLM                TO  DET-ACLM-2.                  
02684      MOVE GR-TPRM                TO  DET-TPRM-2.                  
02685      MOVE GR-TCOM                TO  DET-TCOM-2.                  
02686      MOVE DETAIL-LINE-2          TO  P-DATA.                      
02687                                                                   
02688      PERFORM 1100-WRITE-PRINT  THRU  1199-EXIT.                   
02689                                                                   
02690      MOVE SPACES                 TO  DET-TITLE-2.                 
02691      MOVE ZERO-GRAND-TOTALS      TO  GRAND-TOTALS.                
02692                                                                   
02693      GO TO 1399-EXIT.                                             
02694                                                                   
02695  1330-GRAND-TOTAL-1.                                              
02696      IF X1 GREATER 12                                             
02697          GO TO 1340-GRAND-TOTAL-2.                                
02698                                                                   
02699      MOVE GT-LBEN (X1)           TO DET-LBEN.                     
02700      MOVE GT-LPRM (X1)           TO DET-LPRM.                     
02701      MOVE GT-LCLM (X1)           TO DET-LCLM.                     
02702      MOVE GT-ABEN (X1)           TO DET-ABEN.                     
02703      MOVE GT-APRM (X1)           TO DET-APRM.                     
02704      MOVE GT-ACLM (X1)           TO DET-ACLM.                     
02705      MOVE GT-TPRM (X1)           TO DET-TPRM.                     
02706      MOVE GT-TCOM (X1)           TO DET-TCOM.                     
02707      ADD +1 TO X1.                                                
02708      MOVE COMP-YR (X1)           TO DET-YR.                       
02709      MOVE COMP-MO (X1)           TO DET-MO.                       
02710      PERFORM 1200-LOAD-ALPHA-MONTH THRU 1299-EXIT.                
02711      MOVE DETAIL-LINE            TO P-DATA.                       
02712                                                                   
02713      IF X1 = 2                                                    
02714          MOVE '0' TO X                                            
02715      ELSE                                                         
02716          MOVE ' ' TO X.                                           
02717                                                                   
02718      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02719      GO TO 1330-GRAND-TOTAL-1.                                    
02720                                                                   
02721  1340-GRAND-TOTAL-2.                                              
082609     MOVE L12GT-LBEN             TO DET-LBEN.                     
082609     MOVE L12GT-LPRM             TO DET-LPRM.                     
082609     MOVE L12GT-LCLM             TO DET-LCLM.                     
082609     MOVE L12GT-ABEN             TO DET-ABEN.                     
082609     MOVE L12GT-APRM             TO DET-APRM.                     
082609     MOVE L12GT-ACLM             TO DET-ACLM.                     
082609     MOVE L12GT-TPRM             TO DET-TPRM.                     
082609     MOVE L12GT-TCOM             TO DET-TCOM.                     
082609     MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    
082609                                                                  
082609     MOVE '0'                    TO X.                            
082609     MOVE DETAIL-LINE            TO P-DATA.                       
082609     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
082609     MOVE ZERO-ACCUM             TO LAST-12-GRAND-ACCUM.
082609                                                                  
082609 1350-PRINT-GRAND-YTD.                                               
082609     MOVE YTDGT-LBEN             TO DET-LBEN.                     
082609     MOVE YTDGT-LPRM             TO DET-LPRM.                     
082609     MOVE YTDGT-LCLM             TO DET-LCLM.                     
082609     MOVE YTDGT-ABEN             TO DET-ABEN.                     
082609     MOVE YTDGT-APRM             TO DET-APRM.                     
082609     MOVE YTDGT-ACLM             TO DET-ACLM.                     
082609     MOVE YTDGT-TPRM             TO DET-TPRM.                     
082609     MOVE YTDGT-TCOM             TO DET-TCOM.                     
082609     MOVE 'YEAR TO DATE'         TO DET-TITLE.                    
082609     MOVE ' '                    TO X.                            
082609     MOVE DETAIL-LINE            TO P-DATA.                       
082609     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
082609     MOVE SPACES                 TO DET-TITLE.                    
082609     MOVE ZERO-ACCUM             TO YTD-GRAND-ACCUM.           
082609                                                                  
082609     MOVE PYTDGT-LBEN            TO DET-LBEN.                     
082609     MOVE PYTDGT-LPRM            TO DET-LPRM.                     
082609     MOVE PYTDGT-LCLM            TO DET-LCLM.                     
082609     MOVE PYTDGT-ABEN            TO DET-ABEN.                     
082609     MOVE PYTDGT-APRM            TO DET-APRM.                     
082609     MOVE PYTDGT-ACLM            TO DET-ACLM.                     
082609     MOVE PYTDGT-TPRM            TO DET-TPRM.                     
082609     MOVE PYTDGT-TCOM            TO DET-TCOM.                     
082609     MOVE 'PRIOR YTD         '   TO DET-TITLE.                    
082609     MOVE ' '                    TO X.                            
082609     MOVE DETAIL-LINE            TO P-DATA.                       
082609     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
082609     MOVE SPACES                 TO DET-TITLE.                    
082609     MOVE ZERO-ACCUM             TO PYTD-GRAND-ACCUM
082609
082609*02722      MOVE 'GRAND TOTALS'         TO DET-TITLE.                    
082609*02723      MOVE '-'                    TO X.                            
082609*02724      MOVE GR-LBEN                TO DET-LBEN.                     
082609*02725      MOVE GR-LPRM                TO DET-LPRM.                     
082609*02726      MOVE GR-LCLM                TO DET-LCLM.                     
082609*02727      MOVE GR-ABEN                TO DET-ABEN.                     
082609*02728      MOVE GR-APRM                TO DET-APRM.                     
082609*02729      MOVE GR-ACLM                TO DET-ACLM.                     
082609*02730      MOVE GR-TPRM                TO DET-TPRM.                     
082609*02731      MOVE GR-TCOM                TO DET-TCOM.                     
082609*02732      MOVE DETAIL-LINE            TO P-DATA.                       
082609*02733      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02734      MOVE ZERO-GRAND-TOTALS      TO GRAND-TOTALS.                 
02735                                                                   
02736  1399-EXIT.                                                       
02737      EXIT.                                                        
02738  EJECT                                                            
02739  1400-STATE-LOOKUP.                                               
02740                              COPY ECSSTLOK.                       
02741  1499-EXIT.                                                       
02742      EXIT.                                                        
02743  EJECT                                                            
02744  1500-PASS-NUMBER-CHANGE.                                         
02745      MOVE ZERO                   TO SET-CTR.                      
02746                                                                   
012517     IF PASS-NUMBER = '4' or '8'                                         
02748          IF DTE-TOT-OPT = 1 OR 3 OR 6                             
02749              NEXT SENTENCE                                        
02750          ELSE                                                     
02751          IF DTE-TOT-OPT = 5 OR 7                                  
02752              PERFORM 0700-REPTCD-BREAK  THRU 0790-ZERO-REPTCD     
02753              MOVE '1'            TO SKIP-HD-SW                    
02754              PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT    
02755          ELSE                                                     
02756              PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT.   
02757                                                                   
02758      IF PASS-NUMBER = '5'                                         
02759          IF DTE-TOT-OPT = 1 OR 2 OR 5                             
02760              NEXT SENTENCE                                        
02761          ELSE                                                     
02762          IF DTE-TOT-OPT = 6 OR 7                                  
02763              PERFORM 0700-REPTCD-BREAK  THRU 0790-ZERO-REPTCD     
02764              MOVE '1'            TO SKIP-HD-SW                    
02765              PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT    
02766            ELSE                                                   
02767              PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT.   
072709                                                                  
072709     IF PASS-NUMBER = '6'                                         
072709         IF DTE-TOT-OPT = 1 OR 3 OR 6                             
072709             NEXT SENTENCE                                        
072709         ELSE                                                     
072709         IF DTE-TOT-OPT = 5 OR 7                                  
072709             PERFORM 0700-REPTCD-BREAK  THRU 0790-ZERO-REPTCD     
072709             MOVE '1'            TO SKIP-HD-SW                    
072709             PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT    
072709         ELSE                                                     
072709             PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT.  
031511
031511     IF PASS-NUMBER = '7'
031511         MOVE ' '                TO SKIP-HD-SW
031511         PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT
031511         MOVE '1'                TO SKIP-HD-SW
031511         PERFORM 1700-REPTCD2-BREAK THRU 1790-ZERO-REPTCD2
031511         MOVE '2'            TO  SKIP-HD-SW        
031511         PERFORM 0700-REPTCD-BREAK  THRU 0790-ZERO-REPTCD
031511     END-IF. 
02768                                                                   
02769      IF PASS-NUMBER = '1'                                         
02770          PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT        
02771      ELSE                                                         
012517         IF PASS-NUMBER = '4' OR '6' OR '7' or '8'
02773              NEXT SENTENCE                                        
02774          ELSE                                                     
02775          IF PASS-NUMBER = '5'                                     
02776              PERFORM 0605-CARRIER-BREAK-5 THRU 0680-ZERO-CARRIER  
02777          ELSE                                                     
02778              PERFORM 0600-CARRIER-BREAK THRU 0680-ZERO-CARRIER.   
02779                                                                   
02780      PERFORM 1300-PRINT-GRAND-TOTALS THRU 1399-EXIT.              
02781                                                                   
02782      IF EOF-EPX = '1'                                             
02783          GO TO 9000-END-OF-JOB.                                   
02784                                                                   
012517     IF PASS-NUMBER = '8'
02786          GO TO 9000-END-OF-JOB.                                   
02787                                                                   
02788      MOVE ZERO                   TO PAGE-CNT                      
02789                                     X1.                           
02790      PERFORM 0220-ZERO-PRINT-TABLE 12 TIMES.                      
02791      MOVE +0                     TO X1.                           
02792      PERFORM 0230-ZERO-ACCUM-ACC 15 TIMES.                        
02793      MOVE +0                     TO X1.                           
02794      PERFORM 0240-ZERO-ACCUM-GRP 15 TIMES.                        
02795      MOVE +0                     TO X1.                           
02796      PERFORM 0250-ZERO-ACCUM-CARR 15 TIMES.                       
02797      MOVE +0                     TO X1.                           
02798      PERFORM 0255-ZERO-ACCUM-STATE 15 TIMES.                      
02799      MOVE +0                     TO X1.                           
031511     PERFORM 0256-ZERO-ACCUM-REPT2 15 TIMES.                       
031511     MOVE +0                     TO X1.                           
02800      PERFORM 0260-ZERO-ACCUM-REPT 15 TIMES.                       
02801      MOVE +0                     TO X1.                           
02802      PERFORM 0270-ZERO-ACCUM-GRAND 15 TIMES.                      
02803      MOVE ZERO-GRAND-TOTALS      TO GRAND-TOTALS.                 
02804      MOVE ZERO-ACCUM TO LAST-12-ACCOUNT-ACCUM YTD-ACCOUNT-ACCUM   
02805                     LAST-12-GROUPING-ACCUM  LAST-12-CARRIER-ACCUM 
02806                     LAST-12-REPTCD-ACCUM    LAST-12-STATE-ACCUM   
02807                     YTD-GROUPING-ACCUM      YTD-CARRIER-ACCUM     
02808                     YTD-REPTCD-ACCUM        YTD-STATE-ACCUM
02807                     PYTD-GROUPING-ACCUM     PYTD-CARRIER-ACCUM     
02808                     PYTD-REPTCD-ACCUM       PYTD-STATE-ACCUM
082609                    LAST-12-GRAND-ACCUM     YTD-GRAND-ACCUM 
083609                    PYTD-GRAND-ACCUM
031511                    LAST-12-REPTCD2-ACCUM   YTD-REPTCD2-ACCUM
031511                    PYTD-REPTCD2-ACCUM
02807 *                   PL12-GROUPING-ACCUM     PL12-CARRIER-ACCUM     
02808 *                   PL12-REPTCD-ACCUM       PL12-STATE-ACCUM
02807 *                   ITD-GROUPING-ACCUM      ITD-CARRIER-ACCUM     
02808 *                   ITD-REPTCD-ACCUM        ITD-STATE-ACCUM.      
02809      MOVE ZERO                   TO CERT                          
02810                                     LBEN                          
02811                                     LPRM                          
02812                                     LCLM                          
02813                                     ABEN                          
02814                                     APRM                          
02815                                     ACLM                          
02816                                     TPRM                          
02817                                     TCOM                          
02818                                     ISS.                          
02819                                                                   
02820      MOVE SPACE                  TO DET-TITLE.                    
02821                                                                   
02822      MOVE EP-PASS-NO             TO PASS-NUMBER.                  
02823                                                                   
02824      GO TO 0340-ACCUMULATE.                                       
02825                                                                   
02826  1510-BUSS-LOOKUP.                                                
02827      MOVE CLAS-STARTB            TO CLAS-INDEXB.                  
02828      MOVE S-EPX-CNTL-1           TO WORK-BUSC.                    
02829                                                                   
02830  1520-BUSS-LOOP.                                                  
02831      IF CLAS-INDEXB GREATER CLAS-MAXB                             
02832          MOVE 'INVALID BUS TYPE' TO HD-STATE                      
02833          GO TO 1599-EXIT.                                         
02834                                                                   
02835      IF WORK-BUSC = CLAS-BUSC-CODE (CLAS-INDEXB)                  
02836          MOVE CLAS-BUSC-DESC (CLAS-INDEXB) TO HD-STATE            
02837          GO TO 1599-EXIT.                                         
02838                                                                   
02839      ADD +1 TO CLAS-INDEXB.                                       
02840      GO TO 1520-BUSS-LOOP.                                        
02841                                                                   
02842  1599-EXIT.                                                       
02843      EXIT.                                                        
02844  EJECT                                                            
031511 1700-REPTCD2-BREAK.                                               
031511     MOVE +0                     TO X1.                           
031511     PERFORM 0220-ZERO-PRINT-TABLE 12 TIMES.                      
031511     PERFORM 0800-PRINT-DECISION THRU 0899-EXIT.                  
031511                                                                  
031511     IF P-RP2-SW = '1'                                             
031511         MOVE ' '                TO P-RP2-SW                       
031511     ELSE                                                         
031511         GO TO 1790-ZERO-REPTCD2.                                  
031511                                                                  
031511     PERFORM 0900-HEAD-RTN THRU 0999-EXIT.                        
031511                                                                  
031511     MOVE +1                     TO X1.                           
031511     MOVE +0                     TO Y1.                           
031511                                                                  
031511 1720-REPTCD2-BREAK-POINT.                                         
031511     ADD +1 TO X1 Y1.                                             
031511                                                                  
031511     IF X1 GREATER THAN 13                                        
031511         GO TO 1770-PRINT-LAST-RP2-12.                             
031511                                                                  
031511     COMPUTE RP2CERT = RP2-CERT (X1) - RP2-CERT (Y1).                
031511     COMPUTE RP2LBEN = RP2-LBEN (X1) - RP2-LBEN (Y1).                
031511     COMPUTE RP2LPRM = RP2-LPRM (X1) - RP2-LPRM (Y1).                
031511     COMPUTE RP2LCLM = RP2-LCLM (X1) - RP2-LCLM (Y1).                
031511     COMPUTE RP2ABEN = RP2-ABEN (X1) - RP2-ABEN (Y1).                
031511     COMPUTE RP2APRM = RP2-APRM (X1) - RP2-APRM (Y1).                
031511     COMPUTE RP2ACLM = RP2-ACLM (X1) - RP2-ACLM (Y1).                
031511     COMPUTE RP2TCOM = RP2-TCOM (X1) - RP2-TCOM (Y1).                
031511     COMPUTE RP2TPRM = RP2LPRM  + RP2APRM.                           
031511     MOVE COMP-YR (X1)           TO DET-YR.                       
031511     MOVE COMP-MO (X1)           TO DET-MO.                       
031511     PERFORM 1200-LOAD-ALPHA-MONTH THRU 1299-EXIT.                
031511                                                                  
031511     MOVE RP2CERT                TO DET-CERTS.                    
031511     MOVE RP2LBEN                TO DET-LBEN.                     
031511     MOVE RP2LPRM                TO DET-LPRM.                     
031511     MOVE RP2LCLM                TO DET-LCLM.                     
031511     MOVE RP2ABEN                TO DET-ABEN.                     
031511     MOVE RP2APRM                TO DET-APRM.                     
031511     MOVE RP2ACLM                TO DET-ACLM.                     
031511     MOVE RP2TPRM                TO DET-TPRM.                     
031511     MOVE RP2TCOM                TO DET-TCOM.                     
031511                                                                  
031511     IF COMP-YR (X1) = CONV-YR AND                                
031511        COMP-MO (X1) = CONV-MO                                    
031511         MOVE ZEROS TO DET-CERTS DET-LBEN DET-LPRM                
031511                       DET-ABEN DET-APRM DET-LCLM                 
031511                       DET-ACLM DET-TPRM DET-TCOM                 
031511        ELSE                                                      
031511          PERFORM 1730-ADD-TO-L12-YTD THRU 1740-EXIT.             
031511                                                                  
031511     MOVE DETAIL-LINE            TO P-DATA.                       
031511                                                                  
031511     IF X1 = 2                                                    
031511         MOVE '0'                TO X                             
031511     ELSE                                                         
031511         MOVE ' '                TO X.                            
031511                                                                  
031511     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
031511     GO TO 1720-REPTCD2-BREAK-POINT.                               
031511                                                                  
031511 1730-ADD-TO-L12-YTD.                                             
031511     IF COMPARE9DT (X1) GREATER YEAR-OLD-DATE                     
031511         ADD RP2LBEN TO L12RP2-LBEN                                 
031511         ADD RP2LPRM TO L12RP2-LPRM                                 
031511         ADD RP2LCLM TO L12RP2-LCLM                                 
031511         ADD RP2ABEN TO L12RP2-ABEN                                 
031511         ADD RP2APRM TO L12RP2-APRM                                 
031511         ADD RP2ACLM TO L12RP2-ACLM                                 
031511         ADD RP2TPRM TO L12RP2-TPRM                                 
031511         ADD RP2TCOM TO L12RP2-TCOM.                                
031511                                                                  
031511     IF RUN-YR = COMP-YR (X1)                                     
031511         ADD RP2LBEN TO YTDRP2-LBEN                                 
031511         ADD RP2LPRM TO YTDRP2-LPRM                                 
031511                        YTDRP2-TPRM                                 
031511         ADD RP2LCLM TO YTDRP2-LCLM                                 
031511         ADD RP2TCOM TO YTDRP2-TCOM                                 
031511         ADD RP2ABEN TO YTDRP2-ABEN                                 
031511         ADD RP2APRM TO YTDRP2-APRM                                 
031511                        YTDRP2-TPRM                                 
031511         ADD RP2ACLM TO YTDRP2-ACLM.                                
031511                                                                  
031511     IF X1 = 13
031511        COMPUTE PYTDRP2-LBEN = PYTDRP2-LBEN +
031511           (RP2-LBEN (1) - RP2-LBEN (15))
031511        COMPUTE PYTDRP2-LPRM = PYTDRP2-LPRM +
031511           (RP2-LPRM (1) - RP2-LPRM (15))
031511        COMPUTE PYTDRP2-LCLM = PYTDRP2-LCLM +
031511           (RP2-LCLM (1) - RP2-LCLM (15))
031511        COMPUTE PYTDRP2-ABEN = PYTDRP2-ABEN +
031511           (RP2-ABEN (1) - RP2-ABEN (15))
031511        COMPUTE PYTDRP2-APRM = PYTDRP2-APRM +
031511           (RP2-APRM (1) - RP2-APRM (15))
031511        COMPUTE PYTDRP2-ACLM = PYTDRP2-ACLM +
031511           (RP2-ACLM (1) - RP2-ACLM (15))
031511        COMPUTE PYTDRP2-TPRM = PYTDRP2-TPRM +
031511           (RP2-TPRM (1) - RP2-TPRM (15))
031511        COMPUTE PYTDRP2-TCOM = PYTDRP2-TCOM +
031511           (RP2-TCOM (1) - RP2-TCOM (15))
031511     END-IF
031511
031511     .
031511 1740-EXIT.                                                       
031511      EXIT.                                                       
031511                                                                  
031511 1770-PRINT-LAST-RP2-12.                                           
031511     MOVE L12RP2-LBEN            TO DET-LBEN.                     
031511     MOVE L12RP2-LPRM            TO DET-LPRM.                     
031511     MOVE L12RP2-LCLM            TO DET-LCLM.                     
031511     MOVE L12RP2-ABEN            TO DET-ABEN.                     
031511     MOVE L12RP2-APRM            TO DET-APRM.                     
031511     MOVE L12RP2-ACLM            TO DET-ACLM.                     
031511     MOVE L12RP2-TPRM            TO DET-TPRM.                     
031511     MOVE L12RP2-TCOM            TO DET-TCOM.                     
031511     MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    
031511                                                                  
031511     MOVE '0'                    TO X.                            
031511     MOVE DETAIL-LINE            TO P-DATA.                       
031511     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
031511     MOVE ZERO-ACCUM             TO LAST-12-REPTCD2-ACCUM.         
031511
031511     MOVE SPACES                 TO DET-TITLE
031511     .
031511 1780-PRINT-RP2-YTD.                                               
031511     MOVE YTDRP2-LBEN            TO DET-LBEN.                     
031511     MOVE YTDRP2-LPRM            TO DET-LPRM.                     
031511     MOVE YTDRP2-LCLM            TO DET-LCLM.                     
031511     MOVE YTDRP2-ABEN            TO DET-ABEN.                     
031511     MOVE YTDRP2-APRM            TO DET-APRM.                     
031511     MOVE YTDRP2-ACLM            TO DET-ACLM.                     
031511     MOVE YTDRP2-TPRM            TO DET-TPRM.                     
031511     MOVE YTDRP2-TCOM            TO DET-TCOM.                     
031511     MOVE 'YEAR TO DATE'         TO DET-TITLE.                    
031511     MOVE ' '                    TO X.                            
031511     MOVE DETAIL-LINE            TO P-DATA.                       
031511     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
031511     MOVE SPACES                 TO DET-TITLE.                    
031511     MOVE ZERO-ACCUM             TO YTD-REPTCD2-ACCUM.             
031511                                                                  
031511     MOVE PYTDRP2-LBEN           TO DET-LBEN
031511     MOVE PYTDRP2-LPRM           TO DET-LPRM
031511     MOVE PYTDRP2-LCLM           TO DET-LCLM
031511     MOVE PYTDRP2-ABEN           TO DET-ABEN
031511     MOVE PYTDRP2-APRM           TO DET-APRM
031511     MOVE PYTDRP2-ACLM           TO DET-ACLM
031511     MOVE PYTDRP2-TPRM           TO DET-TPRM
031511     MOVE PYTDRP2-TCOM           TO DET-TCOM
031511     MOVE 'PRIOR YTD         '   TO DET-TITLE
031511     MOVE ' '                    TO X
031511     MOVE DETAIL-LINE            TO P-DATA
031511     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT
031511
031511     MOVE SPACES                 TO DET-TITLE
031511     MOVE ZERO-ACCUM             TO PYTD-REPTCD2-ACCUM.
031511     .                                                            
031511 1790-ZERO-REPTCD2.                                                
031511     MOVE +0                     TO X1.                           
031511     PERFORM 0256-ZERO-ACCUM-REPT2 15 TIMES.                       
031511                                                                  
031511 1799-REPTCD2-RETURN.                                              
031511     GO TO 0310-READ-EARNED-PREM-EXTRACT.                         
031511 EJECT                                                            
02845  COPY ELCDCS.                                                     
02846  9000-END-OF-JOB.                                                 
02847                              COPY ELCPRTC.                        
02848      CLOSE PRNT.                                                  
CIDMOD     CLOSE SELECT-FILE.                                           
02849                                                                   
02850  9099-EXIT.                                                       
02851      EXIT.                                                        
02852                                                                   
02853  ABEND-PGM SECTION.              COPY ELCABEND.                   
02854                                                                   
