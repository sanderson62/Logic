00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL331 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 04/20/94 15:30:06.                 
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            
00008 *                            VMOD=2.037.                          
00009                                                                   
00010 *AUTHOR.     LOGIC INC.                                           
00011 *            DALLAS, TEXAS.                                       
00012                                                                   
00013 *DATE-COMPILED.                                                   
00014                                                                   
00015 *SECURITY.   *****************************************************
00016 *            *                                                   *
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00018 *            *                                                   *
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024                                                                   
00025 *REMARKS.                                                         
00026 *         THIS PROGRAM UPDATES THE ONLINE INFORCE FILE USING      
00027 *       AN EXTRACT FILE FROM ECS CREDIT SYSTEM- AN AUDIT          
00028 *       REPORT IS PRODUCED SHOWING THE CHANGE IN STATUS OF        
00029 *       THE CERTIFICATE HAVING ASSOCIATED CLAIMS.                 
00030 *                                                                 
00031 *         THE PROGRAM ALSO LOADS AND MAINTAINS THE COMMISSION     
00032 *       EXCEPTION FILE (ERCOMM).                                  
00033 *                                                                 
00034 **** NOTE FOLLOWING FILES MUST BE CLOSED TO RUN THIS PROG         
00035 *            ERCOMM, ELCERT, ERMAIL                               
00036 *                                                                 
00037 *       INPUT FILES-             CLAIM MASTER                     
00038 *                                ACCOUNT MASTER (IF SYSTEM D)     
00039 *                                DATE CARD FILE                   
00040 *                                CERTIFICATE MASTER               
00041 *                                ECS CERTIFICATE MASTER           
00042 *                                DISK DATE                        
00043 *                                MAIL DATA                        
00044 *                                                                 
00045 *       OUTPUT-                  CERT LOAD CONTROL PAGE           
00046 *                                ERCOMM FILE (IF SYSTEM D)        
00047 *                                CERTIFICATE MASTER               
00048 *                                (FICH FILE)                      
00049 *                                MAIL DATA                        
00050 *                                                                 
062104******************************************************************
062104*                   C H A N G E   L O G
062104*
062104* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
062104*-----------------------------------------------------------------
062104*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
062104* EFFECTIVE    NUMBER
062104*-----------------------------------------------------------------
062104* 062104    2004050700001  SMVA  ADD NEW FILE TO AUTOMATE ME BALANCING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
020906* 020906                   PEMA  INCREASE SIZE OF LOAN OFFICER
042607* 042607    2007032700002  PEMA  KEEP SPEC VA STUFF ON LINE
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
042314* 042314  IR2013012200001  PEMA  Correct status on old certs.
042814* 042814  IR2014042400001  PEMA  Correct status on old certs.
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
090314* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
011215* 011215  IR2015011200001  PEMA  Add I and G to claim type chks
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
012918* 012918  IR2018012400001  PEMA  FIX CUR STATUS ISSUE
062017* 062017  CR2015091000001  PEMA  ADD PROCESSING FOR TN REF INTEREST
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
062104******************************************************************
00051      EJECT                                                        
00052  ENVIRONMENT DIVISION.                                            
00053                                                                   
00054  INPUT-OUTPUT SECTION.                                            
00055                                                                   
00056  FILE-CONTROL.                                                    
00057                                                                   
00058      SELECT PRNTR            ASSIGN TO SYS008.
00059                                                                   
00060      SELECT DISK-DATE        ASSIGN TO SYS019.
00061                                                                   
00062      SELECT FICH             ASSIGN TO SYS020.

062104     SELECT ME50-EL331-BALANCE
062104                             ASSIGN TO SYS015
062104                             ORGANIZATION IS LINE SEQUENTIAL.
00063                                                                   
00064      SELECT ECSCRT01         ASSIGN TO SYS010.
00065                                                                   
00066      SELECT ELMSTR5          ASSIGN TO SYS011-3380-ELMSTR5
00067                              ORGANIZATION IS INDEXED              
00068                              ACCESS IS DYNAMIC
00069                              RECORD KEY IS CL-CONTROL-BY-CERT-NO  
00070                              FILE STATUS IS CL-STATUS.
00071                                                                   
00072      SELECT ELRETR5          ASSIGN TO SYS011-3380-ELRETR5        
00073                              ORGANIZATION IS INDEXED              
00074                              ACCESS IS DYNAMIC
00075                              RECORD KEY IS RL-CONTROL-BY-CERT-NO  
00076                              FILE STATUS IS ELRETR5-FILE-STATUS.
00077                                                                   
00078      SELECT ELCERTF          ASSIGN TO SYS012-3380-ELCERT         
00079                              ORGANIZATION IS INDEXED              
00080                              ACCESS IS DYNAMIC                    
00081                              RECORD KEY IS CM-CONTROL-PRIMARY     
00082                              FILE STATUS IS CM-STATUS.            
00083                                                                   
00084      SELECT ERCOMM           ASSIGN TO SYS013-3380-ERCOMM         
00085                              ORGANIZATION IS INDEXED              
00086                              ACCESS IS DYNAMIC                    
00087                              RECORD KEY IS CE-CONTROL-PRIMARY     
00088                              FILE STATUS IS CE-STATUS.            
00089                                                                   
00090      SELECT ERACCT           ASSIGN TO SYS014-3380-ERACCT         
00091                              ORGANIZATION IS INDEXED              
00092                              ACCESS IS DYNAMIC                    
00093                              RECORD KEY IS AM-CONTROL-PRIMARY     
00094                              FILE STATUS IS AM-STATUSF.
00095                                                                   
00096      SELECT ELREPT           ASSIGN TO SYS018-3380-ELREPT         
00097                              ORGANIZATION IS INDEXED              
00098                              ACCESS IS DYNAMIC                    
00099                              RECORD KEY IS RF-CONTROL-PRIMARY     
00100                              FILE STATUS IS DTE-VSAM-FLAGS.       
00101                                                                   
00102      SELECT ERMEBL           ASSIGN SYS024-3380-ERMEBL            
00103                              ORGANIZATION INDEXED                 
00104                              ACCESS DYNAMIC                       
00105                              RECORD KEY ME-CONTROL-PRIMARY        
00106                              FILE STATUS ERMEBL-FILE-STATUS.      
00107                                                                   
00108      SELECT ERMAIL           ASSIGN TO SYS021-3380-ERMAIL         
00109                              ORGANIZATION IS INDEXED              
00110                              ACCESS IS RANDOM                     
00111                              RECORD KEY IS MA-CONTROL-PRIMARY     
00112                              FILE STATUS IS ERMAIL-FILE-STATUS.   

pemuni     SELECT ERCTBL           ASSIGN TO ERCTBLT
00114                              ORGANIZATION IS INDEXED              
00115                              ACCESS IS RANDOM                     
00116                              RECORD KEY IS CT-CONTROL-PRIMARY     
00117                              FILE STATUS IS ERCTBL-FILE-STATUS.   

CIDMOD     SELECT  DISPLAY-PRT     ASSIGN TO SYS022-UR-1403-S-SYS022.   
CIDMOD                                                                  
00118      EJECT                                                        
00119                                                                   
00120  DATA DIVISION.                                                   
00121                                                                   
00122  FILE SECTION.                                                    
00123                                                                   
00124  FD  PRNTR                       COPY ELCPRTFD.                   
00125                                                                   
062104 FD  ME50-EL331-BALANCE
062104     RECORDING MODE IS F
062104     BLOCK CONTAINS 0 RECORDS.
062104 01  ME50-EL331-BALANCE-REC    PIC X(95).

00126      EJECT                                                        
00127  FD  ECSCRT01                    COPY ECSCRIFD.                   
00128                                                                   
00129              COPY ECSCRT01 REPLACING CERTIFICATE-MASTER           
00130                                   BY ECS-CERT-RECORD.             
00131                                                                   
00132      EJECT                                                        
00133  FD  ELCERTF.                                                     
00134                                  COPY ELCCERT.                    
00135                                                                   
00136      EJECT                                                        
00137  FD  ERCOMM.                                                      
00138                                                                   
00139                                  COPY ERCCOMM.                    
00140                                                                   
00141      EJECT                                                        
00142  FD  ERACCT.                                                      
00143                                                                   
00144                                  COPY ERCACCT.                    
00145                                                                   
00146      EJECT                                                        
00147  FD  ELMSTR5.                                                     
00148                                                                   
00149                                  COPY ELCMSTR.                    
00150      EJECT                                                        
00151  FD  ELRETR5.                                                     
00152                                                                   
00153                                  COPY ELCRETR.                    
00154                                                                   
00155      EJECT                                                        
00156  FD  DISK-DATE                   COPY ELCDTEFD.                   
00157                                                                   
00158      EJECT                                                        
00159  FD  FICH                        COPY ELCFCHFD.                   
00160                                                                   
00161      EJECT                                                        
00162  FD  ELREPT                      COPY ELCRPTFD.                   
00163                                  COPY ELCREPT.                    
00164                                                                   
00165      EJECT                                                        
00166  FD  ERMEBL.                                                      
00167                                                                   
00168                                  COPY ERCMEBL.                    
00169                                                                   
00170       EJECT                                                       
00171  FD  ERMAIL.                                                      
00172                                                                   
00173                                  COPY ERCMAIL.                    
00174                                                                   
00175       EJECT                                                       
00176  FD  ERCTBL.                                                      
00177                                                                   
00178                                  COPY ERCCTBL.                    
00179                                                                   
00180       EJECT                                                       
CIDMOD FD  DISPLAY-PRT                                                  
CIDMOD     RECORDING MODE F                                             
CIDMOD     LABEL RECORDS ARE STANDARD                                   
CIDMOD     RECORD CONTAINS 133 CHARACTERS                               
CIDMOD     BLOCK CONTAINS 0 RECORDS
CIDMOD     DATA RECORD IS DISPLAY-REC.                                  
CIDMOD                                                                  
CIDMOD 01  DISPLAY-REC.                                                 
CIDMOD     12  DISPLAY-CC              PIC X.                           
CIDMOD     12  DISPLAY-INFO            PIC X(132).                      
CIDMOD                                                                  
CIDMOD      EJECT                                                       
00181  WORKING-STORAGE SECTION.                                         
00182  77  FILLER  PIC X(32) VALUE '********************************'.  
00183  77  FILLER  PIC X(32) VALUE '*    EL331  WORKING-STORAGE    *'.  
00184  77  FILLER  PIC X(32) VALUE '********** VMOD=2.037 **********'.  
00185                                                                   
CIDMOD 77  DIS-HEAD-SW             PIC X       VALUE 'Y'.               
CIDMOD 77  ERROR-COUNT             PIC 9(7)    VALUE ZEROS.             
CIDMOD 77  DIS-LINE-CNT            PIC 99      VALUE ZEROS.             
00191                                                                   
00186  77  NDX                         PIC S9(4) COMP     VALUE ZERO.   
00187  77  CINDX                       PIC S9(4) COMP     VALUE ZERO.   
00188                                                                   
00189  77  WS-ACT-END-SW               PIC X              VALUE SPACE.  
00190      88  NO-MORE-ACT-RECORDS         VALUE 'X'.                   
00191      88  MORE-ACT-RECORDS            VALUE ' '.                   
00192                                                                   
00193  77  WS-FIRST-TIME-SW            PIC X              VALUE '1'.    
00194      88  FIRST-TIME                  VALUE '1'.                   
00195                                                                   
00196  77  WS-PRT-FULL-LINE-SW         PIC X              VALUE SPACE.  
00197      88  LINE-FULL                   VALUE 'X'.                   
00198                                                                   
00199  77  WS-ONL-GE-SW                PIC X              VALUE SPACE.  
00200      88  ONL-KEY-EQUAL-OR-GREATER    VALUE 'X'.                   
00201                                                                   
00202  77  WS-B-END-SW                 PIC X              VALUE SPACE.  
00203      88  END-O-FILE                  VALUE 'E'.                   
00204                                                                   
00205  77  WS-CLM-END-SW               PIC X              VALUE SPACE.  
00206      88  NO-MORE-CLAIM-MATCH         VALUE 'X'.                   
00207                                                                   
00208  77  WS-ONL-END-SW               PIC X              VALUE SPACE.  
00209      88  NO-MORE-ONL-RECORDS         VALUE 'X'.                   
00210      88  MORE-ONL-RECORDS            VALUE ' '.                   
00211                                                                   
00212  77  WS-ALL-END-SW               PIC X              VALUE SPACE.  
00213      88  NO-MORE-RECORDS             VALUE 'X'.                   
00214                                                                   
00215  77  WS-ECS-END-SW               PIC X              VALUE SPACE.  
00216      88  NO-MORE-ECS-RECORDS         VALUE 'X'.                   
00217      88  MORE-ECS-RECORDS            VALUE ' '.                   
00218                                                                   
00219  77  ERROR-SW                    PIC X              VALUE SPACE.  
00220      88  ERROR-OCCURRED              VALUE 'E'.                   
00221      88  NO-ERRORS                   VALUE ' '.                   
00222                                                                   
00223  77  WS-O-MATCH-SW               PIC X              VALUE 'M'.    
00224      88  MATCHED-REC                 VALUE 'M'.                   
00225      88  NO-MATCH-FOUND              VALUE ' '.                   
00226                                                                   
00227  77  WS-IO-ERROR-SW              PIC X              VALUE SPACE.  
00228      88  IO-ERROR                    VALUE 'E'.                   
00229      88  IO-ERROR-OCCURRED           VALUE 'E'.                   
00230                                                                   
00231  77  NO-PREM-CT                  PIC 9(07)          VALUE ZEROS.  
00232  77  NO-PLAN-CODE-CT             PIC 9(07)          VALUE ZEROS.  
00233  77  WS-LINE-CNT                 PIC S999  COMP-3   VALUE ZEROS.  
00234  77  WS-PAGE-CNT                 PIC S999  COMP-3   VALUE ZEROS.  
00235  77  WS-MAX-LINES                PIC S999  COMP-3   VALUE +60.    
00236  77  X                           PIC X              VALUE SPACE.  
00237  77  WS-ZERO                     PIC S9    COMP-3   VALUE ZERO.   
00238  77  WS-RETURN-CODE              PIC S9(4) COMP     VALUE ZERO.   
00239  77  WS-LEVEL-INDEX              PIC S9(4) COMP     VALUE ZERO.   
00240                                                                   
00241  77  WS-ABEND-MESSAGE            PIC X(80)          VALUE SPACES. 
00242  77  WS-ABEND-FILE-STATUS        PIC XX             VALUE ZERO.   
00243                                                                   
00244  77  WS-CLAIM-SW                 PIC X              VALUE SPACE.  
00245      88  NO-ASSOCIATED-CLAIM         VALUE 'N'.                   
00246      88  ASSOCIATED-CLAIM            VALUE 'A'.                   
00247                                                                   
00248  77  WS-ERCOMM-SW                PIC X              VALUE SPACE.  
00249      88  ASSOCIATED-ERCOMM           VALUE '1'.                   
042314 77  ws-cert-load-sw             pic x value spaces.
042314     88  do-not-load                value 'Y'.
012918 77  ws-current-bin-dt           pic xx value low-values.
00250                                                                   
062104 01  WS-BAL50-DESCRIPTION          PIC X(50)  VALUE
062104     'ECS010 Out Recs should match EL331 Offline Certs  '.

062104 01  WS-ME50-BALANCE-REC.
062104     12  WS-ME50-BAL-JOB           PIC X(11)  VALUE SPACES.
062104     12  WS-ME50-BAL-DELIM1        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-STEP          PIC X(08)  VALUE 'EL331   '.
062104     12  WS-ME50-BAL-DELIM2        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-AMT-LOW       PIC ZZZ,ZZZ,ZZ9.
062104     12  WS-ME50-BAL-DELIM3        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-AMT-HIGH      PIC ZZZ,ZZZ,ZZ9.
062104     12  WS-ME50-BAL-DELIM4        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-DESCRIP       PIC X(50)  VALUE SPACES.

CIDMOD 01  DISPLAY-HD-1.                                                
CIDMOD     12  FILLER      PIC X     VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(51) VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(23) VALUE 'PROCESSING ERROR REPORT'.   
CIDMOD     12  FILLER      PIC X(51) VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(07) VALUE ' EL-331'.                   
CIDMOD                                                                  
CIDMOD 01  DISPLAY-HD-2.                                                
CIDMOD     12  FILLER      PIC X     VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X     VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(56) VALUE SPACES.                      
CIDMOD     12  DIS-DATE    PIC X(8)  VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(67) VALUE SPACES.                      
CIDMOD                                                                  
CIDMOD 01  DISPLAY-LINE.                                                
CIDMOD     05  DISPLAY-LINE-05.                                         
CIDMOD        10  DIS-CC              PIC X.                            
CIDMOD        10  DIS-LINE-REASON     PIC X(32).                        
CIDMOD        10  DIS-LINE-REC        PIC X(100).                       
CIDMOD                                                                  
CIDMOD     05  DISPLAY-LINE-ALT  REDEFINES  DISPLAY-LINE-05.            
CIDMOD        10  DIS-CC-ALT          PIC X.                            
CIDMOD        10  DIS-FLD-1           PIC X(52).                        
CIDMOD        10  DIS-FLD-2           PIC X(80).                        
CIDMOD                                                                  
00251  01  WK-MEMB-ACCT.                                                
00252      12  WK-MEMB-ACCT-CHAR       PIC X   OCCURS 6.                
00253  01  WK-LAST-NAME.                                                
00254      12  WK-LAST-NAME-CHAR       PIC X   OCCURS 12.               
00255                                                                   
090314 01  ws-epiq-cert-added          pic 9(7) value zeros.
00256  01  WORK-AREA.                                                   
00257      12  LST-NM-SUB              PIC S9(4) COMP  VALUE +0.        
00258      12  ACCT-SUB                PIC S9(4) COMP  VALUE +0.        
00259      12  BIN-CR-DT               PIC XX    VALUE SPACES.          
00260      12  ERROR-FLAG              PIC X           VALUE ' '.       
00261         88 NO-ERRORS                    VALUE ' '.                
00262         88 NON-ALPHA                    VALUE 'A'.                
00263         88 BLANK-NAME                   VALUE 'B'.                
00264                                                                   
00265  01  MONTH-END-DATA.                                              
00266      12  ME-START-DATE.                                           
00267          16  ME-START-MO         PIC 99.                          
00268          16  FILLER              PIC X.                           
00269          16  ME-START-DA         PIC 99.                          
00270          16  FILLER              PIC X.                           
00271          16  ME-START-YR         PIC 99.                          
00272      12  ME-CNDS-DATE            PIC 9(6).                        
00273      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   
00274          16  ME-CNDS-MO          PIC 99.                          
00275          16  ME-CNDS-DA          PIC 99.                          
00276          16  ME-CNDS-YR          PIC 99.                          
00277      12  ME-START-TIME           PIC 9(6).                        
00278      12  ME-UPDATE-FLAG          PIC X VALUE 'Y'.                 
00279          88  ME-DO-UPDATE        VALUE 'Y'.                       
00280          88  ME-NO-UPDATE        VALUE 'N'.                       
00281      12  ERMEBL-FILE-STATUS      PIC XX.                          
00282      12  ERCTBL-FILE-STATUS      PIC XX.                          
00283      12  MONTH-END-MOYR          PIC S9(5)    COMP-3.             
00284                                                                   
00285  01  DTE-INTERFACE-AREAS.                                         
00286      12  PGM-SUB                 PIC S9(4)    COMP VALUE +331.    
00287      12  ABEND-CODE              PIC XXXX          VALUE SPACE.   
00288      12  ABEND-OPTION            PIC X.                           
00289      12  OLC-REPORT-NAME         PIC X(5)          VALUE 'EL331'. 
00290                                                                   
00291  01  WS-SAVE-AREAS.                                               
00292      12  FILLER   PIC X(27)  VALUE '---------------------------'. 
00293      12  FILLER   PIC X(27)  VALUE '---- EL331 SAVED AREAS ----'. 
00294      12  FILLER   PIC X(27)  VALUE '---------------------------'. 
00295                                                                   
00296      12  WS-ECS-READ       COMP-3     PIC S9(9)   VALUE ZEROS.    
00297      12  WS-ACT-READ       COMP-3     PIC S9(9)   VALUE ZEROS.    
00298      12  WS-ACCT-READS     COMP-3     PIC S9(9)   VALUE ZEROS.    
00299      12  WS-ACCT-MATCH     COMP-3     PIC S9(9)   VALUE ZEROS.    
00300      12  WS-EXCP-ADD       COMP-3     PIC S9(9)   VALUE ZEROS.    
00301      12  WS-EXCP-TBL       COMP-3     PIC S9(9)   VALUE ZEROS.    
00302      12  WS-EXCP-DEL       COMP-3     PIC S9(9)   VALUE ZEROS.    
00303      12  WS-MAIL-DEL       COMP-3     PIC S9(9)   VALUE ZEROS.    
00304      12  WS-MATCHED-ONL    COMP-3     PIC S9(9)   VALUE ZEROS.    
00305      12  WS-ONL-READ       COMP-3     PIC S9(9)   VALUE ZEROS.    
00306      12  WS-LOADED-ECS     COMP-3     PIC S9(9)   VALUE ZEROS.    
00307      12  WS-ONL-DELETED    COMP-3     PIC S9(9)   VALUE ZEROS.    
00308      12  WS-ONL-EXIST      COMP-3     PIC S9(9)   VALUE ZEROS.    
00309      12  WS-CLAIM-MATCH    COMP-3     PIC S9(9)   VALUE ZEROS.    
00310      12  WS-INACTIVE-CERTS COMP-3     PIC S9(9)   VALUE ZEROS.    
00311      12  WS-EXPIRE-MOS     COMP-3     PIC S9(9)   VALUE ZEROS.    
00312      12  WS-WORK-TERM      COMP-3     PIC S9(3)   VALUE ZEROS.    
00313      12  WS-CURRENT-MOS           PIC S9(9) COMP-3  VALUE ZEROS.  
00314      12  WS-UPDATE-SW             PIC S9  COMP-3    VALUE ZERO.   
00315      12  WS-ONL-RECORD            PIC  X(450)       VALUE SPACES. 
00316      12  WS-BEFORE-CERT           PIC  X(450)       VALUE SPACES. 
00317      12  WS-LAST-CLAIM-MATCHED    PIC  X(450)       VALUE SPACES. 
00318      12  WS-CLM-CNT               PIC  S9(4)  COMP  VALUE ZEROS.  
00319      12  WS-CM-ORIG               PIC  X            VALUE SPACE.  
00320      12  WS-CL-ORIG               PIC  X            VALUE SPACE.  
00321      12  WS-CM-INSURED-ADDRESS-SW PIC  X            VALUE SPACE.  
00322      12  WS-CM-ENTRY-BATCH        PIC  X(6)         VALUE SPACES. 
00323      12  WS-CM-LF-EXIT-BATCH      PIC  X(6)         VALUE SPACES. 
00324      12  WS-CM-AH-EXIT-BATCH      PIC  X(6)         VALUE SPACES. 
00325      12  WS-CM-BENEFICIARY        PIC  X(25)        VALUE SPACES. 
00326      12  WS-LST-ADD-ON-DT         PIC  XX           VALUE SPACES. 
00327      12  WS-LN-NUMBER             PIC  X(8)         VALUE SPACES. 
00328      12  WS-LN-BALANCE            PIC S9(7)V99 COMP-3 VALUE ZERO. 
00329      12  WS-CLAIM-DEDUCT-WITHHELD PIC S9(5)V99 COMP-3 VALUE ZERO. 
00330      12  WS-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99 COMP-3 VALUE ZERO.
00331      12  WS-LF-DEATH-EXIT-DT      PIC X(02)      VALUE LOW-VALUES.
00332      12  WS-LF-CURRENT-STATUS     PIC X(01)      VALUE SPACE.     
00333      12  WS-LF-STATUS-AT-DEATH    PIC X(01)      VALUE SPACE.     
00334      12  WS-EOM-DT                PIC X(02)      VALUE LOW-VALUES.
00335                                                                   
00336      12  WS-NAME-WORK.                                            
00337          16  WS-NAME-14           PIC X(14)          VALUE SPACE. 
00338          16  WS-NAME-15           PIC X              VALUE '|'.   
00339                                                                   
00340      12  WS-EFF-ACT-NAME.                                         
00341          16  WS-NAME-EFF-DT      PIC X(6)  VALUE SPACES.          
00342          16  WS-NAME-FILL        PIC XX    VALUE SPACES.          
00343          16  WS-NAME-ACCT        PIC X(7)  VALUE SPACES.          
00344                                                                   
00345      12  WS-HAN-ACCOUNT.                                          
00346          16  FILLER              PIC XXX   VALUE SPACES.          
00347          16  WS-HAN-ACCT-4-10    PIC X(7)  VALUE SPACES.          
00348                                                                   
00349      12  WS-HI-LAST-NAME.                                         
00350          16  FILLER              PIC X(8)  VALUE SPACES.          
00351          16  WS-HI-CERT-4-10     PIC X(7)  VALUE SPACES.          
00352                                                                   
00353      12  WS-CLAS-I-CALC-TYPE     PIC X              VALUE SPACE.  
00354      12  WS-CLAS-I-BEN           PIC XX             VALUE SPACES. 
00355                                                                   
00356      12  CL-STATUS.                                               
00357          16  CL-STAT-1           PIC X              VALUE ZERO.   
00358          16  CL-STAT-2           PIC X              VALUE ZERO.   
00359                                                                   
00360      12  ELRETR5-FILE-STATUS.                                     
00361          16  ELRETR5-STAT-1      PIC X              VALUE ZERO.   
00362          16  ELRETR5-STAT-2      PIC X              VALUE ZERO.   
00363                                                                   
00364      12  ERMAIL-FILE-STATUS.                                      
00365          16  CFS-STAT-1          PIC X              VALUE ZERO.   
00366          16  CFS-STAT-2          PIC X              VALUE ZERO.   
00367                                                                   
00368      12  CM-STATUS.                                               
00369          16  CM-STAT-1           PIC X              VALUE ZERO.   
00370          16  CM-STAT-2           PIC X              VALUE ZERO.   
00371                                                                   
00372      12  CE-STATUS.                                               
00373          16  CE-STAT-1           PIC X              VALUE ZERO.   
00374          16  CE-STAT-2           PIC X              VALUE ZERO.   
00375                                                                   
00376      12  AM-STATUSF.                                              
00377          16  AM-STAT-1           PIC X              VALUE ZERO.   
00378          16  AM-STAT-2           PIC X              VALUE ZERO.   
00379                                                                   
00380      12  WS-CM-CONTROL-PRIMARY   PIC X(33)  VALUE LOW-VALUES.     
00381                                                                   
00382      12  WS-SAVED-ECS-KEY.                                        
00383          16  WS-SAVED-B-COMP     PIC X     VALUE LOW-VALUE.       
00384          16  WS-SAVED-B-CARR     PIC X     VALUE SPACE.           
00385          16  WS-SAVED-B-GRPG     PIC X(6)  VALUE SPACES.          
00386          16  WS-SAVED-B-STAT     PIC XX    VALUE SPACES.          
00387          16  WS-SAVED-B-ACCT     PIC X(10) VALUE SPACES.          
00388          16  WS-SAVED-B-DATE     PIC 9(11) COMP-3 VALUE ZEROS.    
00389          16  WS-SAVED-B-CERT     PIC X(11) VALUE SPACES.          
00390                                                                   
00391      12  WS-SAVED-ONL-KEY.                                        
00392          16  WS-SAVED-O-COMP     PIC X     VALUE LOW-VALUE.       
00393          16  WS-SAVED-O-CARR     PIC X     VALUE SPACE.           
00394          16  WS-SAVED-O-GRPG     PIC X(6)  VALUE SPACES.          
00395          16  WS-SAVED-O-STAT     PIC XX    VALUE SPACES.          
00396          16  WS-SAVED-O-ACCT     PIC X(10) VALUE SPACES.          
00397          16  WS-SAVED-O-DATE     PIC 9(11) COMP-3 VALUE ZEROS.    
00398          16  WS-SAVED-O-CERT     PIC X(11) VALUE SPACES.          
00399                                                                   
00400      12  WS-CLAIM-MATCH-KEY.                                      
00401          16  WS-CL-CARR          PIC X     VALUE SPACES.          
00402          16  WS-CL-GRPG          PIC X(6)  VALUE SPACES.          
00403          16  WS-CL-STAT          PIC XX    VALUE SPACES.          
00404          16  WS-CL-ACCT          PIC X(10) VALUE SPACES.          
00405          16  WS-CL-EFDT          PIC XX    VALUE LOW-VALUES.      
00406                                                                   
00407      12  WS-SEC-CLAIM-INDEX.                                      
00408          16  WS-S-CL-COMP        PIC X     VALUE LOW-VALUES.      
00409          16  WS-S-CL-PRIM        PIC X(10) VALUE SPACES.          
00410          16  WS-S-CL-SFX         PIC X     VALUE SPACE.           
00411                                                                   
00412      12  WS-CM-CANCEL-DATE.                                       
00413          16  WS-CM-CANC-MO       PIC XX   VALUE SPACES.           
00414          16  WS-CM-CANC-DA       PIC XX   VALUE SPACES.           
00415          16  WS-CM-CANC-YR       PIC XX   VALUE SPACES.           
00416                                                                   
00417      12  WS-CURRENT-DATE-MDY.                                     
00418          16  WS-CURR-MO          PIC XX   VALUE SPACES.           
00419          16  WS-CURR-DA          PIC XX   VALUE SPACES.           
00420          16  WS-CURR-YR          PIC XX   VALUE SPACES.           
00421                                                                   
00422      12  WS-CTBL-KEY.                                             
00423          16  WS-CTBL-COMP-CD     PIC X    VALUE LOW-VALUES.       
00424          16  WS-CTBL-TABLE       PIC XXX  VALUE SPACES.           
00425          16  WS-CTBL-BEN-TYPE    PIC X    VALUE SPACE.            
00426          16  WS-CTBL-BEN-CODE    PIC XX   VALUE '  '.             
00427      12  WS-LAST-CTBL-KEY        PIC X(7) VALUE SPACES.           
00428                                                                   
00429  01  COMM-MATCH-AREA.                                             
00430      12  WS-L-COM                PIC SV9(5) COMP-3 VALUE +0.      
00431      12  WS-J-COM                PIC SV9(5) COMP-3 VALUE +0.      
00432      12  WS-A-COM                PIC SV9(5) COMP-3 VALUE +0.      
00433      12  WS-COMM-CK-AMT          PIC S9(9)V99 COMP-3 VALUE +0.    
00434      12  WS-CK-AGE               PIC S999    VALUE ZEROS.         
00435      12  WS-CK-TERM              PIC S99     VALUE ZEROS.         
00436                                                                   
00437      EJECT                                                        
00438                                  COPY ELCDATE.                    
00439                                                                   
00440      EJECT                                                        
00441  01  PRINTER-WORK-AREAS.                                          
00442      12  HEADING-1.                                               
00443          16  FILLER      PIC X(50) VALUE SPACE.                   
00444          16  FILLER      PIC X(21) VALUE '  CERTIFICATE AUDIT  '. 
00445          16  FILLER      PIC X(60) VALUE SPACES.                  
00446                                                                   
00447      12  HEADING-2.                                               
00448          16  FILLER      PIC X(47) VALUE SPACE.                   
00449          16  HD-COMP-NM  PIC X(30) VALUE SPACE.                   
00450          16  FILLER      PIC X(42) VALUE SPACES.                  
00451          16  FILLER      PIC X(08) VALUE 'EL331  '.               
00452                                                                   
00453      12  HEADING-3.                                               
00454          16  FILLER      PIC X(50) VALUE SPACE.                   
00455          16  FILLER      PIC X(3)  VALUE '** '.                   
00456          16  HD-DATE-FULL PIC X(18).                              
00457          16  FILLER      PIC X(3)  VALUE ' **'.                   
00458          16  FILLER      PIC X(45)  VALUE SPACES.                 
00459          16  HD-RUN-DATE PIC X(8).                                
00460                                                                   
00461      12  HEADING-4.                                               
00462          16  FILLER      PIC X(119) VALUE SPACE.                  
00463          16  FILLER      PIC X(4)   VALUE 'PAGE'.                 
00464          16  HD-PAGE-NO  PIC ZZZ9.                                
00465                                                                   
00466      12  HEADING-5.                                               
00467          16  FILLER      PIC X(44)  VALUE                         
00468          ' *---------- CERTIFICATE CONTROLS ---------*'.          
00469          16  FILLER      PIC X(44)  VALUE                         
00470          '  *------------ ASSOCIATED CLAIM -----------'.          
00471          16  FILLER      PIC X(45)  VALUE                         
00472          '---* *------- EXCEPTION CONDITION ----------*'.         
00473                                                                   
00474      12  HEADING-6.                                               
00475          16  FILLER      PIC X(46)  VALUE                         
00476              '   NUMBER   EFFECTIVE C GROUP  ST  ACCOUNT    '.    
00477          16  FILLER      PIC X(46)  VALUE                         
00478              'NUMBER  TYPE STATUS LN KIND INCURRED PROCESSOR'.    
00479          16  FILLER      PIC X(41)  VALUE  SPACES.                
00480                                                                   
00481      12  DTL-1.                                                   
00482          16  FILLER              PIC X              VALUE SPACE.  
00483          16  DTL-CERT-NO         PIC X(11).                       
00484          16  FILLER              PIC X              VALUE SPACE.  
00485          16  DTL-EFFECTIVE-DT.                                    
00486              20  DTL-EFF-MO      PIC XX.                          
00487              20  FILLER          PIC X              VALUE '/'.    
00488              20  DTL-EFF-DA      PIC XX.                          
00489              20  FILLER          PIC X              VALUE '/'.    
00490              20  DTL-EFF-YR      PIC XX.                          
00491          16  FILLER              PIC X              VALUE SPACE.  
00492          16  DTL-CARR            PIC X.                           
00493          16  FILLER              PIC X             VALUE SPACE.   
00494          16  DTL-GRPG            PIC X(6).                        
00495          16  FILLER              PIC X             VALUE SPACE.   
00496          16  DTL-STAT            PIC XX.                          
00497          16  FILLER              PIC X             VALUE SPACE.   
00498          16  DTL-ACCT            PIC X(10).                       
00499          16  FILLER              PIC X             VALUE SPACE.   
00500          16  DTL-CL-NO           PIC X(7).                        
00501          16  FILLER              PIC X             VALUE SPACE.   
00502          16  DTL-CL-TYPE         PIC XXXX.                        
00503          16  FILLER              PIC X             VALUE SPACE.   
00504          16  DTL-CL-STATUS       PIC X(6).                        
00505          16  FILLER              PIC X              VALUE SPACE.  
00506          16  DTL-CL-LN-KIND      PIC X(7).                        
00507          16  FILLER              PIC X              VALUE SPACE.  
00508          16  DTL-CL-INCURRED     PIC X(8).                        
00509          16  FILLER              PIC XXX            VALUE SPACE.  
00510          16  DTL-CL-PROCESSOR    PIC XXXX.                        
00511          16  FILLER              PIC X(4)           VALUE SPACE.  
00512          16  DTL-EXC-MSG         PIC X(40).                       
00513          16  FILLER              PIC XX             VALUE SPACE.  
00514                                                                   
00515      12  WS-EXC-MSG-CANCEL.                                       
00516          16  WS-EXC-MSG-DESC    PIC X(26)                         
00517                VALUE ' XXXX COV CANCELLED AS OF '.                
00518          16  WS-EXC-MSG-CANCEL-DATE.                              
00519              25  WS-EXC-C-MO    PIC XX.                           
00520              25  FILLER         PIC X                VALUE '/'.   
00521              25  WS-EXC-C-DA    PIC XX.                           
00522              25  FILLER         PIC X                VALUE '/'.   
00523              25  WS-EXC-C-YR    PIC XX.                           
00524                                                                   
00525      12  WS-EXC-MSG-NO-NOTES.                                     
00526          16  WS-EXC-MSG-NO-DE    PIC X(21)                        
00527                VALUE ' CERTIFICATE PENDING-'.                     
00528          16  WS-EXC-MSG-NO-COPY  PIC X(15)                        
00529                VALUE 'NOTES ATTACHED.'.                           
00530                                                                   
00531      12  WS-EXC-MSG-CO-DELETE   PIC X(40)                         
00532                VALUE ' COVERAGE DELETED - SEE CREDIT DEPT.    '.  
00533                                                                   
00534      12  WS-EXC-MSG-CE-DELETE.                                    
00535          16  WS-EXC-MSG-CE-DE    PIC X(21)                        
00536                VALUE ' CERTIFICATE DELETED '.                     
00537          16  WS-EXC-MSG-CE-COPY  PIC X(15)                        
00538                VALUE SPACES.                                      
00539                                                                   
00540      12  WS-EXC-MSG-EE-EXPIRED.                                   
00541          16  WS-EXC-MSG-EE-DE    PIC X(21)                        
00542                VALUE ' CERTIFICATE EXPIRED '.                     
00543          16  WS-EXC-MSG-EE-COPY  PIC X(15)                        
00544                VALUE SPACES.                                      
00545                                                                   
00546      12  WS-EXC-MSG-ADDED.                                        
00547          16  FILLER             PIC X(22) VALUE                   
00548                      ' COVERAGE ADDED - AMT '.                    
00549          16  WS-EXC-MSG-ADDED-AMT     PIC ZZZZZZ9.99.             
00550          16  FILLER             PIC X(6) VALUE ' TYPE-'.          
00551          16  WS-EXC-MSG-ADDED-TYPE    PIC XX.                     
00552                                                                   
00553      12  WS-EXC-MSG-CHANGED-NEW.                                  
00554          16  FILLER            PIC X(22) VALUE                    
00555                      ' COVERAGE CHANGED-NEW '.                    
00556          16  WS-EXC-MSG-CHGD-AMT-NEW  PIC ZZZZZZ9.99.             
00557          16  FILLER            PIC X(6) VALUE ' TYPE-'.           
00558          16  WS-EXC-MSG-CHGD-TYPE-NEW PIC XX.                     
00559                                                                   
00560      12  WS-EXC-MSG-CHANGED-OLD.                                  
00561          16  FILLER              PIC X(18) VALUE   SPACES.        
00562          16  FILLER              PIC X(04) VALUE                  
00563                                        'OLD '.                    
00564          16  WS-EXC-MSG-CHGD-AMT-OLD  PIC ZZZZZZ9.99.             
00565          16  FILLER                   PIC X(6) VALUE ' TYPE-'.    
00566          16  WS-EXC-MSG-CHGD-TYPE-OLD PIC XX.                     
00567                                                                   
00568      12  WS-EXC-MSG-COPY.                                         
00569          16  FILLER              PIC X(20) VALUE                  
00570                                        'CERT COPIED FROM ECS'.    
00571                                                                   
00572      12  FTG-1.                                                   
00573          16  P-MATCHED-ONL       PIC ZZZ,ZZZ,ZZ9-.                
00574          16  FILLER              PIC X(35)                        
00575                    VALUE  ' MATCHED / UPDATED (+)   '.            
00576                                                                   
00577      12  FTG-2.                                                   
00578          16  P-LOADED-ECS        PIC ZZZ,ZZZ,ZZ9-.                
00579          16  FILLER              PIC X(35)                        
00580                    VALUE  ' LOADED FROM OFFLINE (+) '.            
00581                                                                   
00582      12  FTG-3.                                                   
00583          16  P-ONL-DELETED       PIC ZZZ,ZZZ,ZZ9-.                
00584          16  FILLER              PIC X(35)                        
00585                    VALUE  ' ONLINE DELETED    (-)   '.            
00586          16  FILLER              PIC X(30)                        
00587                    VALUE  ' CURRENT ELCERT RECORD COUNT--'.       
00588          16  P-ONL-EXIST         PIC ZZZ,ZZZ,ZZ9-.                
00589                                                                   
00590      12  FTG-4.                                                   
00591          16  P-CLAIM-MATCH       PIC ZZZ,ZZZ,ZZ9-.                
00592          16  FILLER              PIC X(35)                        
00593                    VALUE  ' HAVE ATTACHED CLAIMS    '.            
00594                                                                   
00595      12  FTG-5.                                                   
00596          16  P-INACTIVE-CERTS    PIC ZZZ,ZZZ,ZZ9-.                
00597          16  FILLER              PIC X(35)                        
00598                    VALUE  ' INACTIVE AND BYPASSED   '.            
00599                                                                   
00600      12  FTG-6.                                                   
00601          16  P-ONL-READ          PIC ZZZ,ZZZ,ZZ9-.                
00602          16  FILLER              PIC X(35)                        
00603                    VALUE  ' ONLINE CERTS READ       '.            
00604                                                                   
00605      12  FTG-7.                                                   
00606          16  P-ECS-READ          PIC ZZZ,ZZZ,ZZ9-.                
00607          16  FILLER              PIC X(35)                        
00608                    VALUE  ' OFFLINE CERTS READ      '.            
00609                                                                   
00610      12  FTG-8.                                                   
00611          16  P-EXCP-ADD          PIC ZZZ,ZZZ,ZZ9-.                
00612          16  FILLER              PIC X(35)                        
00613                    VALUE  ' COMP EXCEPTIONS ADDED'.               
00614                                                                   
00615      12  FTG-9.                                                   
00616          16  P-EXCP-DEL          PIC ZZZ,ZZZ,ZZ9-.                
00617          16  FILLER              PIC X(35)                        
00618                    VALUE  ' COMP EXCEPTIONS DELETED'.             
00619                                                                   
00620      12  FTG-10.                                                  
00621          16  P-EXCP-TBL          PIC ZZZ,ZZZ,ZZ9-.                
00622          16  FILLER              PIC X(35)                        
00623                    VALUE  ' ACCTS WITH COMM TABLES '.             
00624                                                                   
00625      12  FTG-11.                                                  
00626          16  P-MAIL-DEL          PIC ZZZ,ZZZ,ZZ9-.                
00627          16  FILLER              PIC X(35)                        
00628                    VALUE  ' INSURED ADDRESS MAIL RECS DELETED '.  
00629                                                                   
00630      12  FTG-12.                                                  
00631          16  P-ACCT-READS        PIC ZZZ,ZZZ,ZZ9-.                
00632          16  FILLER              PIC X(35)                        
00633                    VALUE  ' ACCOUNT MASTER MATCH ATTEMPTS '.      
00634                                                                   
00635      12  FTG-13.                                                  
00636          16  P-ACT-READ          PIC ZZZ,ZZZ,ZZ9-.                
00637          16  FILLER              PIC X(35)                        
00638                    VALUE  ' ERACCT RECORDS READ '.                
00639                                                                   
00640      12  FTG-14.                                                  
00641          16  P-ACCT-MATCH        PIC ZZZ,ZZZ,ZZ9-.                
00642          16  FILLER              PIC X(35)                        
00643                    VALUE  ' CERTS MATCHED TO ACCOUNT MASTER '.    
00644                                                                   
00645                                                                   
00646                                  COPY ELCDTECX.                   
00647                                                                   
00648                                  COPY ELCDTEVR.                   
00649                                                                   
00650                                  COPY ELCCRTVR.                   
00651                                                                   
00652      EJECT                                                        
00653  PROCEDURE DIVISION.                                              
00654                                                                   
00655  00000-LOAD-DATE-CARD.           COPY ELCDTERX.                   
00656                                                                   
00657      MOVE BIN-RUN-DATE           TO  WS-EOM-DT.                   
00658                                                                   
012918     MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT
012918     MOVE '2'                    TO DC-OPTION-CODE     
012918     PERFORM 7100-DATE-RTN THRU  7100-EXIT             
012918     MOVE DC-BIN-DATE-1          TO ws-current-bin-dt

00659      MOVE WS-TIME                TO ME-START-TIME.                
00660      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                
00661      MOVE ME-START-MO            TO ME-CNDS-MO.                   
00662      MOVE ME-START-DA            TO ME-CNDS-DA.                   
00663      MOVE ME-START-YR            TO ME-CNDS-YR.                   
00664                                                                   
00665      COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO.           

062104     IF DTE-CLIENT = 'CID'
062104         MOVE 'CILG331'          TO  WS-ME50-BAL-JOB
062104     ELSE
030612       IF DTE-CLIENT = 'AHL'
030612         MOVE 'AHLG331'          TO  WS-ME50-BAL-JOB
030612       ELSE
062104         MOVE 'CIDCLG331'        TO  WS-ME50-BAL-JOB
030612       END-IF
062104     END-IF.
00666                                                                   
00667      EJECT                                                        
00668                                                                   
00669  0000-MAINLINE.                                                   
00670                                                                   
00671      PERFORM 7000-INITIALIZE    THRU 7000-EXIT.                   
00672                                                                   
00673      PERFORM 2000-PROCESS-CERT  THRU 2000-EXIT                    
00674          UNTIL NO-MORE-RECORDS OR                                 
00675                IO-ERROR-OCCURRED.                                 
00676                                                                   
00677      PERFORM 8000-FINALIZE THRU 8000-EXIT.                        
00678                                                                   
00679      IF DTE-CLIENT EQUAL 'JAL' OR 'JAI'                           
00680         DISPLAY ' CERTS WITHOUT PREM ...' NO-PREM-CT              
00681         DISPLAY ' CERTS WITHOUT PLANS ..' NO-PLAN-CODE-CT.        

090314     display ' epiq certs ' ws-epiq-cert-added

           .
00683  0000-EXIT.                                                       
00684                                                                   
00685 ****************************************************************  
00686 *     OPEN I/O (ERMEBL)   MONTH END BALANCE FILE ACCESSED      *  
00687 *     THRU ME-CONTROL-PRIMARY.  STATISTICS FOR EL331 UPDATED   *  
00688 *     IN MONTH-END RECORD EACH TIME RUN.                       *  
00689 ****************************************************************  
00690                                                                   
00691      OPEN I-O ERMEBL.                                             
00692                                                                   
00693      IF ERMEBL-FILE-STATUS NOT = ZERO AND '97'                    
00694          MOVE 'N'                TO ME-UPDATE-FLAG.               
00695                                                                   
00696      MOVE DTE-CLIENT             TO ME-COMPANY.                   
00697      MOVE MONTH-END-MOYR         TO ME-MOYR.                      
00698                                                                   
00699      IF ME-DO-UPDATE                                              
00700          READ ERMEBL INVALID KEY                                  
00701          MOVE 'N'                TO ME-UPDATE-FLAG                
00702          CLOSE ERMEBL.                                            
00703                                                                   
00704      IF ME-DO-UPDATE                                              
070714*        MOVE ME-START-TIME      TO ME-331-START                  
00706          MOVE ME-CNDS-DATE       TO ME-331-RUN-DT                 
00707          ACCEPT WS-TIME-OF-DAY   FROM  TIME                       
070714*        MOVE WS-TIME            TO ME-331-END                    
00709          ADD 1                   TO ME-331-RUN-CT                 
00710          MOVE 1                  TO ME-331-FLAG                   
00711          REWRITE MONTH-END-BALANCES                               
00712          CLOSE ERMEBL.                                            
00713                                                                   
00714      GOBACK.                                                      
00715                                                                   
00716        EJECT                                                      
00717 ******************************************************************
00718 *  PROCESSING FLOW BASED UPON CERT CONTROL COMPARISON:           *
00719 *                                                                *
00720 * 1) ECS (BATCH CERT CONTROL) GREATER  ONL (ONLINE CERT CONTROL) *
00721 *    2300-PROCESS-N-READ-ONL                                     *
00722 *        2330-FIND-ASSOCIATED-CLAIM                              *
00723 *                                                                *
00724 *            -NO CLAIMS  7320-BLD-ONL-PRT-LINE                   *
00725 *                        7350-BLD-CLM-INFO                       *
00726 *                        7370-PRINT-DETAIL                       *
00727 *                        2280-DELETE-ERMAIL                      *
00728 *                        2270-DELETE-ERCOMM                      *
00729 *                        DELETE ONLINE CERT MASTER               *
00730 *                                                                *
00731 *            -CLAIMS     REWRITE ONLINE CERT MASTER              *
00732 *                        7320-BLD-ONL-PRT-LINE                   *
00733 *                        7350-BLD-CLM-INFO                       *
00734 *                        7370-PRINT-DETAIL                       *
00735 *                                                                *
00736 * 2) ECS (BATCH CERT CONTROL) EQUAL  ONL (ONLINE CERT CONTROL)   *
00737 *    2500-PROCESS-MATCHING                                       *
00738 *        2330-FIND-ASSOCIATED-CLAIM                              *
00739 *                                                                *
00740 *            -NO CLAIMS  2600-REWRITE-CERT                       *
00741 *                        REBUILDS ONLINE CERT FROM BATCH CERT    *
00742 *                                                                *
00743 *            -CLAIMS     2510-BUILD-PRINT (REFLECTS ACTIVITY)    *
00744 *                        2600-REWRITE-CERT                       *
00745 *                        REBUILDS ONLINE CERT FROM BATCH CERT    *
00746 *                                                                *
00747 * 3) ONL (ONLINE CERT CONTROL) GREATER  ECS (BATCH CERT CONTROL) *
00748 *    2800-ADD-NEW-CERT                                           *
00749 *        2830-MOVE-DATA                                          *
00750 *        2820-BUILD-NEW-KEY                                      *
00751 *        2200-MATCH-ACCT                                         *
00752 *        WRITE ONLINE CERT MASTER                                *
00753 ******************************************************************
00754                                                                   
00755  2000-PROCESS-CERT.                                               
00756                                                                   
00757      PERFORM 2100-READ-ECS-RECORDS   THRU 2100-EXIT.              
00758                                                                   
00759      IF WS-SAVED-ECS-KEY GREATER WS-SAVED-ONL-KEY                 
00760          MOVE SPACES             TO WS-ONL-GE-SW                  
00761          PERFORM 2300-PROCESS-N-READ-ONL THRU 2300-EXIT           
00762          UNTIL ONL-KEY-EQUAL-OR-GREATER.                          
00763                                                                   
00764      IF NO-MORE-ECS-RECORDS  AND  NO-MORE-ONL-RECORDS             
00765           MOVE  'X'              TO WS-ALL-END-SW                 
00766           GO TO 2000-EXIT.                                        
00767                                                                   
00768      IF WS-SAVED-ONL-KEY EQUAL WS-SAVED-ECS-KEY                   
00769         PERFORM 2500-PROCESS-MATCHING  THRU 2500-EXIT.            

042314     if (ws-saved-onl-key > ws-saved-ecs-key)
                        AND 
042314        ((not do-not-load)
090314         or (CR-TEMP-EPIQ = 'EPIQ'))
042314        ADD +1                   TO WS-LOADED-ECS                 
042314        perform 2800-add-new-cert thru 2800-exit
042314     end-if

           .
00775  2000-EXIT.                                                       
00776       EXIT.                                                       
00777                                                                   
00778      EJECT                                                        
00779 ***************************************************************** 
00780 *  2100-READ-ECS-RECORDS                                        * 
00781 ***************************************************************** 
00782 *  READ BATCH CERTIFICATE MASTER RECORD AND CALCULATE AN EXPIRY * 
00783 *  DATE BASED UPON ISSUE DATE, COVERAGE TERM, CERTIFICATE TYPE, * 
00784 *  AND ANY CLAIM ACTIVITY. THIS COMPUTED DATE IS COMPARED WITH  * 
00785 *  THE RUN DATE PROVIDED THRU THE DATE CARD LOAD TO DETERMINE   * 
00786 *  IF THE CERTIFICATE WILL BE LOADED BACK TO ONLINE. BENEFIT    * 
00787 *  TYPES OF UNEXPIRED CERTS ARE VALIDATED AGAINST THE BENEFIT   * 
00788 *  TABLE.                                                       * 
00789 *  EXPIRY DATE EQUAL :                                          * 
00790 *      1) CLAIMS PAID (EFFECTIVE DATE + LONGEST TERM + 36)      * 
00791 *      2) NO CLAIMS   (EFFECTIVE DATE + LONGEST TERM + 12)      * 
00792 *      3) O.B. CERTS  (EFFECTIVE DATE + LONGEST TERM + 36)      * 
00793 ***************************************************************** 
00794                                                                   
00795  2100-READ-ECS-RECORDS.                                           
00796                                                                   
00797      IF NO-MORE-ECS-RECORDS                                       
00798          GO TO 2100-EXIT.                                         
00799                                                                   
00800      READ ECSCRT01  AT END                                        
00801          MOVE 'X'                TO WS-ECS-END-SW                 
00802          MOVE HIGH-VALUES        TO WS-SAVED-ECS-KEY              
00803          GO TO 2100-EXIT.                                         
00804                                                                   
00805      ADD  1                      TO WS-ECS-READ.
042314     move spaces                 to ws-cert-load-sw
00806                                                                   
00807      IF  CR-LF-CANCEL-EXIT-DATE  NOT NUMERIC                      
00808           MOVE ZEROS             TO CR-LF-CANCEL-EXIT-DATE.       
00809                                                                   
00810      IF  CR-AH-CANCEL-EXIT-DATE  NOT NUMERIC                      
00811           MOVE ZEROS             TO CR-AH-CANCEL-EXIT-DATE.       
00812                                                                   
00813      IF  CR-LF-CLAIM-EXIT-DATE  NOT NUMERIC                       
00814           MOVE ZEROS             TO CR-LF-CLAIM-EXIT-DATE.        
00815                                                                   
00816      IF  CR-AH-SETTLEMENT-EXIT-DATE  NOT NUMERIC                  
00817           MOVE ZEROS             TO CR-AH-SETTLEMENT-EXIT-DATE.   
00818                                                                   
00819      COPY ELCCRTM1.                                               
00820                                                                   
00821      IF DTE-CLIENT EQUAL 'HAN'                                    
00822         IF CR-GROUPING NOT EQUAL '010184'                         
00823            MOVE CR-LNAME            TO WS-EFF-ACT-NAME            
00824            MOVE CR-ACCOUNT          TO WS-HAN-ACCOUNT             
00825            IF WS-NAME-EFF-DT NUMERIC                              
00826               MOVE WS-HAN-ACCT-4-10 TO WS-NAME-ACCT               
00827 *             MOVE CR-ACCOUNT       TO WS-NAME-ACCT               
00828               MOVE SPACES           TO WS-NAME-FILL               
00829               MOVE WS-EFF-ACT-NAME  TO CR-LNAME.                  
00830                                                                   
00831      IF DTE-CLIENT EQUAL 'HAN'                                    
00832         IF CR-GROUPING EQUAL '010184'                             
00833            MOVE CR-LNAME            TO WS-HI-LAST-NAME            
00834            MOVE CR-CERT-PRIME       TO WS-HI-CERT-4-10            
00835            MOVE WS-HI-LAST-NAME     TO CR-LNAME.                  
00836                                                                   
00837      IF CR-LF-TERM NOT EQUAL ZERO                                 
00838          MOVE CR-LF-TERM         TO WS-WORK-TERM.                 
00839                                                                   
00840      IF (CR-AH-TERM NOT EQUAL ZERO) AND                           
00841         ((CR-AH-TERM GREATER CR-LF-TERM) OR                       
00842          (WS-WORK-TERM EQUAL ZERO))                               
00843            MOVE CR-AH-TERM       TO WS-WORK-TERM.                 
00844                                                                   
00845      IF (CR-DTH-DT NOT EQUAL ZERO)
00846         or (CR-DIS-DT NOT EQUAL ZERO)
00847             COMPUTE WS-EXPIRE-MOS EQUAL                           
00848                    (CR-CCYY * 12) + CR-MO + WS-WORK-TERM + 36     
00849        ELSE                                                       
00850             COMPUTE WS-EXPIRE-MOS EQUAL                           
00851                    (CR-CCYY * 12) + CR-MO + WS-WORK-TERM + 12.    
00852                                                                   
00853      IF WS-WORK-TERM EQUAL +1                                     
00854             COMPUTE WS-EXPIRE-MOS EQUAL                           
00855                    (CR-CCYY * 12) + CR-MO + WS-WORK-TERM + 36.    
00856                                                                   
00857      IF DTE-CLIENT = 'LAP'  OR  'RMC'                             
00858             COMPUTE WS-EXPIRE-MOS EQUAL                           
00859                    (CR-CCYY * 12) + CR-MO + WS-WORK-TERM + 36.    
00860                                                                   
           IF DTE-CLIENT = 'CID'
              IF (CR-ENTRY-BATCH (1:2) = 'BL' OR 'VA')
                 OR (CR-LF-EXIT-BATCH (1:2) = 'BL' OR 'VA')
                 OR (CR-AH-EXIT-BATCH (1:2) = 'BL' OR 'VA')
                 COMPUTE WS-EXPIRE-MOS =
                    (CR-CCYY * 12) + CR-MO + WS-WORK-TERM + 300
              END-IF
           END-IF

062121     IF DTE-CLIENT = 'AHL' or 'FNL'
032612        COMPUTE WS-EXPIRE-MOS =
032612           (CR-CCYY * 12) + CR-MO + WS-WORK-TERM + 300
032612     END-IF
           
042314     IF WS-EXPIRE-MOS < WS-CURRENT-MOS
042314        IF (CR-ENTRY-CCYY = RUN-CCYY)
042314           and (CR-ENTRY-MO = RUN-MO)
042314           continue
042314        ELSE
042314           set do-not-load        to true
042314        end-if
042314     end-if

00861      IF WS-EXPIRE-MOS LESS WS-CURRENT-MOS                         
010716       IF DTE-CLIENT NOT = 'LGX' and 'CID' and 'DCC' and 'VPP'
00863            IF CR-ENTRY-CCYY = RUN-CCYY AND                        
00864               CR-ENTRY-MO = RUN-MO                                
00865                NEXT SENTENCE                                      
00866            ELSE                                                   
042814               ADD 1 TO WS-INACTIVE-CERTS
00868                GO TO 2100-READ-ECS-RECORDS.                       
00869                                                                   
00870      PERFORM 2120-SAVE-ECS-KEY  THRU 2120-EXIT.                   
00871                                                                   
00872      MOVE CLAS-STARTA            TO CLAS-INDEXA.                  
00873      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  
00874                                                                   
00875  2100-LF-TYPE.                                                    
00876                                                                   
00877      IF CR-LFTYP EQUAL ZERO                                       
00878          GO TO 2100-AH-TYPE.                                      
00879                                                                   
00880      IF CLAS-INDEXL GREATER CLAS-MAXL                             
00881          DISPLAY LIFE-OVERRIDE-L6                                 
00882                  ' BENEFIT TYPE NOT FOUND IN TABLE EQUAL ',       
00883                  CR-LFTYP                                         
00884          MOVE 'BENEFIT TYPE NOT FOUND (SEE DISPLAY)'              
00885                                  TO WS-ABEND-MESSAGE              
00886          GO TO ABEND-PGM.                                         
00887                                                                   
00888      IF CR-LFTYP EQUAL CLAS-I-BEN (CLAS-INDEXL)                   
00889          GO TO 2100-AH-TYPE.                                      
00890                                                                   
00891      ADD +1                      TO CLAS-INDEXL.                  
00892      GO TO 2100-LF-TYPE.                                          
00893                                                                   
00894  2100-AH-TYPE.                                                    
00895                                                                   
00896      IF CR-AHTYP EQUAL ZERO                                       
00897         GO TO 2100-EXIT.                                          
00898                                                                   
00899      IF CLAS-INDEXA GREATER CLAS-MAXA                             
00900          DISPLAY AH-OVERRIDE-L6                                   
00901                  ' BENEFIT TYPE NOT FOUND IN TABLE EQUAL ',       
00902                  CR-AHTYP                                         
00903          MOVE 'BENEFIT TYPE NOT FOUND (SEE DISPLAY)'              
00904                                  TO WS-ABEND-MESSAGE              
00905          GO TO ABEND-PGM.                                         
00906                                                                   
00907      IF CR-AHTYP EQUAL CLAS-I-BEN (CLAS-INDEXA)                   
00908             GO TO 2100-EXIT.                                      
00909                                                                   
00910      ADD +1                      TO CLAS-INDEXA.                  
00911      GO TO 2100-AH-TYPE.                                          
00912                                                                   
00913  2100-EXIT.                                                       
00914       EXIT.                                                       
00915                                                                   
00916      EJECT                                                        
00917 ****************************************************              
00918 *  2120-SAVE-ECS-KEY                               *              
00919 ****************************************************              
00920 *  SAVE BATCH CERT MASTER KEY IN WS-SAVED-ECS-KEY  *              
00921 ****************************************************              
00922                                                                   
00923  2120-SAVE-ECS-KEY.                                               
00924                                                                   
00925      MOVE DTE-CLASIC-COMPANY-CD  TO WS-SAVED-B-COMP.              
00926      MOVE CR-CARRIER             TO WS-SAVED-B-CARR.              
00927      MOVE CR-GROUPING            TO WS-SAVED-B-GRPG.              
00928      MOVE CR-STATE               TO WS-SAVED-B-STAT.              
00929      MOVE CR-ACCOUNT             TO WS-SAVED-B-ACCT.              
00930      MOVE CR-CERT-NO             TO WS-SAVED-B-CERT.              
00931      MOVE CR-DT                  TO WS-SAVED-B-DATE               
00932                                     DC-GREG-DATE-CYMD.            
00933                                                                   
00934      MOVE 'L'                    TO DC-OPTION-CODE.               
00935      PERFORM 7100-DATE-RTN  THRU   7100-EXIT.                     
00936      MOVE DC-BIN-DATE-1          TO BIN-CR-DT.                    
00937                                                                   
00938  2120-EXIT.                                                       
00939       EXIT.                                                       
00940                                                                   
00941      EJECT                                                        
00942 ****************************************************              
00943 *  2200-MATCH-ACCT                                 *              
00944 ****************************************************              
00945 *  IF ONLINE BILLING OPTION IS USED RETRIEVE THE   *              
00946 *  ACCOUNT MASTER FILE RECORD ASSOCIATED WITH THE  *              
00947 *  CERTIFICATE BEING PROCESSED. IF A MATCH OCCURS, *              
00948 *  THE COMPENSATION STRUCTURES BETWEEN THE TWO     *              
00949 *  RECORDS ARE COMPARED, IF ANY DIFFERENCES ARE    *              
00950 *  DETECTED A COMPENSATION EXCEPTION RECORD IS     *              
00951 *  CREATED. THIS RECORD WILL CONTAIN THE ORIGINAL  *              
00952 *  COMPENSATION STRUCTURE THE CERTIFICATE WAS      *              
00953 *  ISSUED UNDER.                                   *              
00954 ****************************************************              
00955                                                                   
00956  2200-MATCH-ACCT.                                                 
00957      IF DTE-SYS-D-DEMAND-BILL EQUAL '1' OR                        
00958         DTE-SYS-G-AR-USED     EQUAL '1'                           
00959          NEXT SENTENCE                                            
00960      ELSE                                                         
00961          GO TO 2200-EXIT.                                         
00962                                                                   
00963      IF NO-MORE-ACT-RECORDS                                       
00964          GO TO 2200-EXIT.                                         
00965                                                                   
00966      IF CR-ACCT-CONTROL GREATER AM-CONTROL-A                      
00967         PERFORM 2260-READ-ACCT THRU 2260-EXIT                     
00968         GO TO 2200-MATCH-ACCT.                                    
00969                                                                   
00970      IF CR-ACCT-CONTROL LESS AM-CONTROL-A                         
00971         GO TO 2200-EXIT.                                          
00972                                                                   
00973      IF BIN-CR-DT LESS AM-EFFECTIVE-DT  OR                        
00974         BIN-CR-DT NOT LESS AM-EXPIRATION-DT                       
00975            PERFORM 2260-READ-ACCT THRU 2260-EXIT                  
00976            GO TO 2200-MATCH-ACCT.                                 
00977                                                                   
00978 ****************************************************              
00979 *  MATCHED  *  CHECK COMPENSATION STRUCTURE        *              
00980 ****************************************************              
00981                                                                   
00982      IF NOT CR-POLICY-IS-DECLINED  AND                            
00983         NOT CR-POLICY-IS-VOID      AND                            
00984         NOT CR-POLICY-IS-REIN-ONLY                                
00985          ADD 1 TO WS-ACCT-MATCH                                   
00986          PERFORM 2210-MATCH-LEVELS THRU 2250-EXIT.                
00987                                                                   
00988  2200-EXIT.                                                       
00989       EXIT.                                                       
00990                                                                   
00991      EJECT                                                        
00992 ****************************************************              
00993 *  2210-MATCH-LEVELS.                              *              
00994 ****************************************************              
00995 *  THE COMPENSATION STRUCTURES BETWEEN THE TWO     *              
00996 *  RECORDS ARE COMPARED, IF ANY DIFFERENCES ARE    *              
00997 *  DETECTED A COMPENSATION EXCEPTION RECORD        *              
00998 *  IS CREATED. THIS RECORD WILL CONTAIN THE ORIG-  *              
00999 *  INAL COMPENSATION STRUCTURE THE CERTIFICATE WAS *              
01000 *  ISSUED UNDER.                                   *              
01001 *                                                  *              
01002 *  AM-COM-TYP =   O (OVERWRITE COMMISSION ON GROSS *              
01003 *                  BUSINESS ONLY)                  *              
01004 *                                                  *              
01005 *  AM-COM-TYP =   P (OVERWRITE COMMISSION ON GROSS *              
01006 *                  AND REINSURANCE BUSINESS)       *              
01007 ****************************************************              
01008                                                                   
01009  2210-MATCH-LEVELS.                                               
01010                                                                   
01011      MOVE 1                      TO NDX.                          
01012      MOVE CLAS-STARTA            TO CLAS-INDEXA.                  
01013      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  
01014      PERFORM 2100-LF-TYPE  THRU  2100-EXIT.                       
01015                                                                   
01016  2220-GET-NEXT-LEVEL.                                             
01017                                                                   
01018      IF CR-AGT-TYPE (NDX)  =  AM-COM-TYP (NDX)                    
01019          IF (AM-COM-TYP (NDX) NOT EQUAL 'O' AND 'P'               
052814              AND 'G' AND 'B' AND 'S')                                    
01021              GO TO 2230-GET-NEXT.                                 
01022                                                                   
01023      IF (AM-J-COM (NDX) NOT NUMERIC)   OR                         
01024         (AM-L-COM (NDX) NOT NUMERIC)   OR                         
01025         (AM-A-COM (NDX) NOT NUMERIC)                              
01026           NEXT SENTENCE                                           
01027      ELSE                                                         
01028           MOVE AM-L-COM (NDX)    TO  WS-L-COM                     
01029           MOVE AM-J-COM (NDX)    TO  WS-J-COM                     
01030           MOVE AM-A-COM (NDX)    TO  WS-A-COM                     
01031           GO TO 2225-CHECK-NUMERIC.                               
01032                                                                   
01033      IF CR-LFTYP = ZERO                                           
01034          GO TO 2224-CHECK-AH-TABLE.                               
01035                                                                   
01036      IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                          
01037          GO TO 2221-JOINT-LIFE-COMM.                              
01038                                                                   
01039      IF AM-L-COM (NDX) NUMERIC                                    
01040          MOVE AM-L-COM (NDX)    TO  WS-L-COM                      
01041          GO TO 2224-CHECK-AH-TABLE.                               
01042                                                                   
01043      MOVE AM-L-COMA (NDX)        TO  WS-CTBL-TABLE.               
01044      MOVE LIFE-OVERRIDE-L1       TO  WS-CTBL-BEN-TYPE.            
01045      MOVE CR-LFTYP               TO  WS-CTBL-BEN-CODE.            
01046      MOVE CR-LFAMT               TO  WS-COMM-CK-AMT.              
01047      MOVE CR-LF-TERM             TO  WS-CK-TERM.                  
01048      MOVE CR-AGE                 TO  WS-CK-AGE.                   
01049      PERFORM  2249-CTBL-PROCESS  THRU  2249-READ-CTBL-EXIT.       
01050      MOVE CT-RT (CINDX)          TO  WS-L-COM.                    
01051      GO TO 2224-CHECK-AH-TABLE.                                   
01052                                                                   
01053  2221-JOINT-LIFE-COMM.                                            
01054                                                                   
01055      IF AM-J-COM (NDX) NUMERIC                                    
01056          MOVE AM-J-COM (NDX)    TO  WS-J-COM                      
01057          GO TO 2224-CHECK-AH-TABLE.                               
01058                                                                   
01059      MOVE AM-J-COMA (NDX)        TO  WS-CTBL-TABLE.               
01060      MOVE LIFE-OVERRIDE-L1       TO  WS-CTBL-BEN-TYPE.            
01061      MOVE CR-LFTYP               TO  WS-CTBL-BEN-CODE.            
01062      MOVE CR-LFAMT               TO  WS-COMM-CK-AMT.              
01063      MOVE CR-LF-TERM             TO  WS-CK-TERM.                  
01064      MOVE CR-AGE                 TO  WS-CK-AGE.                   
01065      PERFORM  2249-CTBL-PROCESS  THRU  2249-READ-CTBL-EXIT.       
01066      MOVE CT-RT (CINDX)          TO  WS-J-COM.                    
01067                                                                   
01068  2224-CHECK-AH-TABLE.                                             
01069                                                                   
01070      IF CR-AHTYP = ZERO                                           
01071          GO TO 2225-CHECK-NUMERIC.                                
01072                                                                   
01073      IF AM-A-COM (NDX) NUMERIC                                    
01074          MOVE AM-A-COM (NDX)     TO  WS-A-COM                     
01075          GO TO 2225-CHECK-NUMERIC.                                
01076                                                                   
01077      MOVE AM-A-COMA (NDX)        TO  WS-CTBL-TABLE.               
01078      MOVE AH-OVERRIDE-L1         TO  WS-CTBL-BEN-TYPE.            
01079      MOVE CR-AHTYP               TO  WS-CTBL-BEN-CODE.            
01080      MOVE CR-AHAMT               TO  WS-COMM-CK-AMT.              
01081      MOVE CR-AH-TERM             TO  WS-CK-TERM.                  
01082      MOVE CR-AGE                 TO  WS-CK-AGE.                   
01083      PERFORM  2249-CTBL-PROCESS  THRU  2249-READ-CTBL-EXIT.       
01084      MOVE CT-RT (CINDX)          TO  WS-A-COM.                    
01085                                                                   
01086  2225-CHECK-NUMERIC.                                              
01087                                                                   
01088      IF DTE-CLIENT EQUAL 'WDS'                                    
01089         IF CR-COM-AGT (NDX) EQUAL '0000000001'                    
01090            GO TO 2230-GET-NEXT.                                   
01091                                                                   
01092      IF (CR-COM-AGT  (NDX) NOT EQUAL AM-AGT (NDX)) OR             
01093         (CR-AGT-TYPE (NDX) NOT EQUAL AM-COM-TYP (NDX))            
01094            GO TO 2235-NO-MATCH.                                   
01095                                                                   
01096      IF CR-LFTYP EQUAL ZERO                                       
01097          NEXT SENTENCE                                            
01098      ELSE                                                         
01099          IF ((CLAS-I-JOINT (CLAS-INDEXL) EQUAL 'J') AND           
01100              (CR-LCOM-L  (NDX) NOT EQUAL WS-J-COM ))              
01101          OR                                                       
01102             ((CLAS-I-JOINT (CLAS-INDEXL) NOT EQUAL 'J') AND       
01103              (CR-LCOM-L  (NDX) NOT EQUAL WS-L-COM ))              
01104                  GO TO 2235-NO-MATCH.                             
01105                                                                   
01106      IF CR-AHTYP EQUAL ZERO                                       
01107          NEXT SENTENCE                                            
01108      ELSE                                                         
01109          IF CR-LCOM-AH (NDX) NOT EQUAL WS-A-COM                   
01110                GO TO 2235-NO-MATCH.                               
01111                                                                   
01112  2230-GET-NEXT.                                                   
01113                                                                   
01114      ADD 1                       TO NDX.                          
01115                                                                   
01116      IF NDX GREATER 10                                            
01117         GO TO 2250-EXIT.                                          
01118                                                                   
01119      GO TO 2220-GET-NEXT-LEVEL.                                   
01120                                                                   
01121  2235-NO-MATCH.                                                   
01122                                                                   
01123      MOVE SPACES                 TO COMMISSION-EXCEPTIONS.        
01124      MOVE 'CE'                   TO CE-RECORD-ID.                 
01125      MOVE DTE-CLASIC-COMPANY-CD  TO CE-COMPANY-CD.                
01126      MOVE CR-CARRIER             TO CE-CARRIER.                   
01127      MOVE CR-GROUPING            TO CE-GROUPING.                  
01128      MOVE CR-STATE               TO CE-STATE.                     
01129      MOVE CR-ACCOUNT             TO CE-ACCOUNT.                   
01130                                                                   
01131      MOVE BIN-CR-DT              TO CE-CERT-EFF-DT.               
01132                                                                   
01133      MOVE CR-CERT-NO             TO CE-CERT-NO.                   
01134      MOVE CR-COMPENSATION-LEVELS TO CE-COMP-STRUCTURE.            
01135                                                                   
01136      MOVE CR-LF-EXPIRE-DATE      TO DC-GREG-DATE-CYMD.            
01137                                                                   
01138      IF CR-LF-EXPIRE-DATE EQUAL ZERO                              
01139           MOVE CR-AH-EXPIRE-DATE TO DC-GREG-DATE-CYMD.            
01140                                                                   
01141      MOVE 'L'                    TO DC-OPTION-CODE.               
01142      PERFORM 7100-DATE-RTN  THRU   7100-EXIT.                     
01143      MOVE DC-BIN-DATE-1          TO CE-CERT-EXPIRATION-DT.        
01144                                                                   
01145      WRITE COMMISSION-EXCEPTIONS.                                 
01146                                                                   
01147      IF CE-STATUS EQUAL '22'                                      
01148          GO TO 2245-SKIP-DUP.                                     
01149                                                                   
01150      IF CE-STAT-1  NOT EQUAL ZERO                                 
01151          MOVE CE-STATUS          TO  WS-ABEND-FILE-STATUS         
01152          MOVE 'ERROR OCCURED WRITE - ERCOMM'                      
01153                                  TO  WS-ABEND-MESSAGE             
01154          GO TO ABEND-PGM.                                         
01155                                                                   
01156  2245-SKIP-DUP.                                                   
01157                                                                   
01158      MOVE '1'                    TO CM-COMP-EXCP-SW.              
01159      ADD 1                       TO WS-EXCP-ADD.                  
01160      GO TO 2250-EXIT.                                             
01161                                                                   
01162  2249-CTBL-PROCESS.                                               
01163                                                                   
01164      IF WS-CTBL-KEY = WS-LAST-CTBL-KEY                            
01165          GO TO 2249-SEARCH-TABLE.                                 
01166                                                                   
01167      MOVE DTE-CLASIC-COMPANY-CD  TO  WS-CTBL-COMP-CD.             
01168                                                                   
01169  2249-READ-CTBL.                                                  
01170                                                                   
01171      MOVE WS-CTBL-KEY            TO  CT-CONTROL-PRIMARY           
01172                                      WS-LAST-CTBL-KEY.            
01173                                                                   
01174      READ ERCTBL.                                                 
01175                                                                   
CIDMOD     IF ERCTBL-FILE-STATUS     = '97'                             
CIDMOD        MOVE '00' TO ERCTBL-FILE-STATUS.                          
CIDMOD                                                                  
01176      IF ERCTBL-FILE-STATUS NOT = '00'                             
CIDMOD        ADD  +1  TO  ERROR-COUNT                                  
CIDMOD***1**                                                            
CIDMOD        MOVE 'ERCTBL-FILE-STATUS IS = ' TO DIS-LINE-REASON        
CIDMOD        MOVE ERCTBL-FILE-STATUS         TO DIS-LINE-REC           
CIDMOD        PERFORM 8600-DISPLAY-PRT THRU                             
CIDMOD              8600-DISPLAY-EXIT                                   
CIDMOD***2**                                                            
CIDMOD        MOVE 'WS-CTBL-KEY        IS = ' TO DIS-LINE-REASON        
CIDMOD        MOVE  WS-CTBL-KEY               TO DIS-LINE-REC           
CIDMOD        PERFORM 8600-DISPLAY-PRT THRU                             
CIDMOD              8600-DISPLAY-EXIT                                   
CIDMOD***3**                                                            
CIDMOD        MOVE 'CM-CONTROL-PRIMARY IS = ' TO DIS-LINE-REASON        
CIDMOD        MOVE  CM-CONTROL-PRIMARY        TO DIS-LINE-REC           
CIDMOD        PERFORM 8600-DISPLAY-PRT THRU                             
CIDMOD              8600-DISPLAY-EXIT                                   
CIDMOD***4**                                                            
CIDMOD        MOVE 'CE-CONTROL-PRIMARY IS = ' TO DIS-LINE-REASON        
CIDMOD        MOVE  CE-CONTROL-PRIMARY        TO DIS-LINE-REC           
CIDMOD        PERFORM 8600-DISPLAY-PRT THRU                             
CIDMOD              8600-DISPLAY-EXIT                                   
CIDMOD***5**                                                            
CIDMOD        MOVE 'AM-CONTROL-PRIMARY IS = ' TO DIS-LINE-REASON        
CIDMOD        MOVE  AM-CONTROL-PRIMARY        TO DIS-LINE-REC           
CIDMOD        PERFORM 8600-DISPLAY-PRT THRU                             
CIDMOD              8600-DISPLAY-EXIT                                   
CIDMOD***6**                                                            
CIDMOD        MOVE 'CL-CONTROL-PRIMARY IS = ' TO DIS-LINE-REASON        
CIDMOD        MOVE  CL-CONTROL-PRIMARY        TO DIS-LINE-REC           
CIDMOD        PERFORM 8600-DISPLAY-PRT THRU                             
CIDMOD              8600-DISPLAY-EXIT                                   
CIDMOD***7**                                                            
CIDMOD        MOVE  SPACES                    TO DIS-LINE-REASON        
CIDMOD        MOVE  SPACES                    TO DIS-LINE-REC           
CIDMOD        PERFORM 8600-DISPLAY-PRT THRU                             
CIDMOD              8600-DISPLAY-EXIT                                   
CIDMOD                                                                  
01177          MOVE 'ERROR OCCURRED READ - ERCTBL'                      
01178                                  TO  WS-ABEND-MESSAGE             
01179          MOVE ERCTBL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01180          DISPLAY 'WS-CTBL-KEY = ' WS-CTBL-KEY                     
CIDMOD         GO TO 2100-READ-ECS-RECORDS.                             
CIDMOD*        GO TO ABEND-PGM.                                         
01182                                                                   
01183  2249-SEARCH-TABLE.                                               
01184                                                                   
01185       MOVE +1                    TO  CINDX.                       
01186                                                                   
01187       IF  WS-COMM-CK-AMT GREATER THAN CT-TBF (1)                  
01188           ADD +9 TO CINDX                                         
01189           IF WS-COMM-CK-AMT GREATER THAN CT-TBF (2)               
01190               ADD +9 TO CINDX.                                    
01191                                                                   
01192       IF WS-CK-AGE GREATER THAN CT-AGE (1)                        
01193           ADD +3 TO CINDX                                         
01194           IF WS-CK-AGE GREATER THAN CT-AGE (2)                    
01195               ADD +3 TO CINDX.                                    
01196                                                                   
01197       IF WS-CK-TERM GREATER THAN CT-TRM (1)                       
01198           ADD +1 TO CINDX                                         
01199           IF WS-CK-TERM GREATER THAN CT-TRM (2)                   
01200               ADD +1 TO CINDX.                                    
01201                                                                   
01202       IF CT-RT (CINDX) NOT NUMERIC                                
01203           MOVE WS-LAST-CTBL-KEY  TO  CT-CONTROL-PRIMARY           
01204           MOVE CT-RT-R (CINDX)   TO  WS-CTBL-TABLE                
01205           GO TO 2249-READ-CTBL.                                   
01206                                                                   
01207  2249-READ-CTBL-EXIT.                                             
01208       EXIT.                                                       
01209                                                                   
01210  2250-EXIT.                                                       
01211       EXIT.                                                       
01212                                                                   
01213      EJECT                                                        
01214 ****************************************************              
01215 *  2260-READ-ACCT                                  *              
01216 ****************************************************              
01217 *  READ ACCOUNT MASTER FILE RECORD MATCHING THE    *              
01218 *  ECS CERTIFICATE RECORD BEING PROCESSED.         *              
01219 ****************************************************              
01220                                                                   
01221  2260-READ-ACCT.                                                  
01222                                                                   
01223      IF NO-MORE-ACT-RECORDS                                       
01224          GO TO 2260-EXIT.                                         
01225                                                                   
01226      READ ERACCT NEXT RECORD.                                     
01227                                                                   
01228      ADD 1 TO WS-ACT-READ.                                        
01229                                                                   
01230 ***  DISPLAY '*** ERACCT READ: ' AM-CARRIER '-' AM-GROUPING '-'   
01231 ***      AM-STATE '-' AM-ACCOUNT ' EXP: ' AM-EXPIRE-DT ' EFF: '   
01232 ***      AM-EFFECTIVE-DT '-' AM-EFFECT-DT '  CERT: ' CR-CARRIER   
01233 ***      '-' CR-GROUPING '-' CR-STATE '-' CR-ACCOUNT ' EFF: '     
01234 ***      CR-DT.                                                   
01235                                                                   
01236      IF AM-STAT-1 EQUAL '1'                                       
01237          MOVE 'X'   TO WS-ACT-END-SW                              
01238          GO TO 2260-EXIT.                                         
01239                                                                   
01240      IF AM-STAT-1 NOT EQUAL ZERO                                  
01241          MOVE 'ERROR OCCURED READ  - ERACCT'                      
01242                                 TO  WS-ABEND-MESSAGE              
01243          MOVE AM-STATUSF        TO  WS-ABEND-FILE-STATUS          
01244          GO TO ABEND-PGM.                                         
01245                                                                   
01246      IF AM-COMPANY-CD NOT EQUAL DTE-CLASIC-COMPANY-CD             
01247          MOVE 'X'               TO WS-ACT-END-SW                  
01248          GO TO 2260-EXIT.                                         
01249                                                                   
01250  2260-EXIT.                                                       
01251       EXIT.                                                       
01252                                                                   
01253      EJECT                                                        
01254 ****************************************************              
01255 *  2270-DELETE-ERCOMM                              *              
01256 ****************************************************              
01257 *  DELETE COMPENSATION EXCEPTION RECORDS FOR CERT- *              
01258 *  IFICATE MASTER RECORDS THAT HAVE EXPIRED.       *              
01259 ****************************************************              
01260                                                                   
01261  2270-DELETE-ERCOMM.                                              
01262                                                                   
01263      IF DTE-SYS-D-DEMAND-BILL EQUAL '1' OR                        
01264         DTE-SYS-G-AR-USED     EQUAL '1'                           
01265           NEXT SENTENCE                                           
01266      ELSE                                                         
01267         GO TO 2270-EXIT.                                          
01268                                                                   
01269      IF THIS-CERT-HAS-ERCOMM-ENTRY                                
01270          MOVE CM-CONTROL-PRIMARY TO CE-CONTROL-PRIMARY            
01271      ELSE                                                         
01272          GO TO 2270-EXIT.                                         
01273                                                                   
01274      READ ERCOMM.                                                 
01275                                                                   
01276      IF CE-STATUS  EQUAL '23'                                     
01277          GO TO 2270-EXIT.                                         
01278                                                                   
01279      IF CE-STAT-1  NOT EQUAL ZERO                                 
01280         MOVE CE-STATUS           TO  WS-ABEND-FILE-STATUS         
01281         MOVE 'ERROR OCCURED READ - ERCOMM'                        
01282                                  TO  WS-ABEND-MESSAGE             
01283         GO TO ABEND-PGM.                                          
01284                                                                   
01285      DELETE ERCOMM.                                               
01286                                                                   
01287      IF CE-STAT-1  NOT EQUAL ZERO                                 
01288         MOVE CE-STATUS           TO  WS-ABEND-FILE-STATUS         
01289         MOVE 'ERROR OCCURED DELETE - ERCOMM'                      
01290                                  TO  WS-ABEND-MESSAGE             
01291         GO TO ABEND-PGM.                                          
01292                                                                   
01293      ADD 1                       TO WS-EXCP-DEL.                  
01294                                                                   
01295  2270-EXIT.                                                       
01296      EXIT.                                                        
01297                                                                   
01298      EJECT                                                        
01299 *****************************************************             
01300 *  2280-DELETE-ERMAIL                               *             
01301 *****************************************************             
01302 *  DELETE NAME AND ADDRESS RECORDS FOR CERTIFICATE  *             
01303 *  MASTER RECORDS THAT HAVE EXPIRED.                *             
01304 *****************************************************             
01305                                                                   
01306  2280-DELETE-ERMAIL.                                              
01307                                                                   
01308      IF INSURED-ADDR-PRESENT                                      
01309          MOVE CM-CONTROL-PRIMARY TO MA-CONTROL-PRIMARY            
01310      ELSE                                                         
01311          GO TO 2280-EXIT.                                         
01312                                                                   
01313      READ ERMAIL.                                                 
01314                                                                   
01315      IF ERMAIL-FILE-STATUS EQUAL '23'                             
01316          GO TO 2280-EXIT.                                         
01317                                                                   
01318      IF CFS-STAT-1 NOT EQUAL ZERO                                 
01319         MOVE ERMAIL-FILE-STATUS  TO  WS-ABEND-FILE-STATUS         
01320         MOVE 'ERROR OCCURED READ - ERMAIL'                        
01321                                  TO  WS-ABEND-MESSAGE             
01322         GO TO ABEND-PGM.                                          
01323                                                                   
01324      DELETE ERMAIL.                                               
01325                                                                   
01326      IF CFS-STAT-1 NOT EQUAL ZERO                                 
01327         MOVE ERMAIL-FILE-STATUS  TO  WS-ABEND-FILE-STATUS         
01328         MOVE 'ERROR OCCURED DELETE - ERMAIL'                      
01329                                  TO  WS-ABEND-MESSAGE             
01330         GO TO ABEND-PGM.                                          
01331                                                                   
01332      ADD 1                       TO WS-MAIL-DEL.                  
01333                                                                   
01334  2280-EXIT.                                                       
01335      EXIT.                                                        
01336                                                                   
01337      EJECT                                                        
01338 *****************************************************             
01339 *  2300-PROCESS-N-READ-ONL.                         *             
01340 *****************************************************             
01341 *  THIS ROUTINE IS EXECUTED WHEN THE BATCH CERT     *             
01342 *  MASTER CONTROL IS GREATER THAN THE ONLINE CERT   *             
01343 *  MASTER CONTROL. THE ONLINE CERT MASTER WILL BE   *             
01344 *  DELETED UNLESS IT IS AN OPEN-END CERT OR CLAIM   *             
01345 *  PAYMENT ACTIVITY IS ASSOCIATED WITH THE CERT.    *             
01346 *  DELETE ANY ASSOCIATED COMPENSATION EXCEPTION     *             
01347 *  RECORDS(ERCOMM) AND NAME AND ADDRESS RECORDS     *             
01348 *  (ERMAIL) ASSOCIATED WITH THE EXPIRED CERT MASTER *             
01349 *  RECORD.                                          *             
01350 *****************************************************             
01351                                                                   
01352  2300-PROCESS-N-READ-ONL.                                         
01353                                                                   
01354      MOVE ZEROS                  TO CM-STATUS.                    
01355                                                                   
01356      IF DTE-CLIENT = 'MON'                                        
01357          GO TO 2300-PROCESS-N-READ-MON.                           
01358                                                                   
01359 *****COPY ELCCRTM2.                                               
01360                                                                   
01361      IF (NO-MATCH-FOUND)                                          
01362         IF (CERT-PEND-ISSUE-ERROR)                                
01363             IF CM-NOTE-SW NOT EQUAL ' '                           
01364                  MOVE WS-EXC-MSG-NO-NOTES   TO DTL-EXC-MSG        
01365                  MOVE WS-CURRENT-DATE       TO DC-GREG-DATE-1-EDIT
01366                  MOVE '2'                   TO DC-OPTION-CODE     
01367                  PERFORM 7100-DATE-RTN THRU 7100-EXIT             
01368                  MOVE DC-BIN-DATE-1         TO CM-LAST-MONTH-END  
01369                  PERFORM 4000-EXPIRE-DATE   THRU  4099-EXIT       
01370                                                                   
01371                  REWRITE CERTIFICATE-MASTER                       
01372                                                                   
01373                  PERFORM 7320-BLD-ONL-PRT-LINE  THRU 7320-EXIT    
01374                  PERFORM 7370-PRINT-DETAIL      THRU 7370-EXIT    
01375                  IF CM-STAT-1 NOT EQUAL ZERO                      
01376                     MOVE 'ERROR OCCURED REWRITE  - ELCERT'        
01377                                     TO  WS-ABEND-MESSAGE          
01378                     MOVE CM-STATUS  TO  WS-ABEND-FILE-STATUS      
01379                     GO TO ABEND-PGM                               
01380                  ELSE                                             
01381                     GO TO 2301-READ-ONL-CERT.                     
01382                                                                   
01383      IF (NO-MATCH-FOUND) AND                                      
01384         (NOT CM-OPEN-END)                                         
01385          PERFORM 2330-FIND-ASSOCIATED-CLAIM  THRU 2330-EXIT       
01386          IF (NO-ASSOCIATED-CLAIM) AND                             
01387             (CM-CLAIM-ATTACHED-COUNT = 0)                         
01388               MOVE SPACES                TO WS-EXC-MSG-CE-COPY    
01389               MOVE WS-EXC-MSG-CE-DELETE  TO DTL-EXC-MSG           
01390               PERFORM 7320-BLD-ONL-PRT-LINE  THRU 7320-EXIT       
01391               PERFORM 7350-BLD-CLM-INFO      THRU 7350-EXIT       
01392               PERFORM 7370-PRINT-DETAIL      THRU 7370-EXIT       
01393               ADD +1                     TO WS-ONL-DELETED        
01394 *             PERFORM 2280-DELETE-ERMAIL THRU 2280-EXIT           
01395               PERFORM 2270-DELETE-ERCOMM THRU 2270-EXIT           
01396                                                                   
01397               DELETE ELCERTF                                      
01398                                                                   
01399               IF CM-STAT-1 NOT EQUAL ZERO                         
01400                  MOVE 'ERROR OCCURED DELETE  - ELCERT'            
01401                                  TO  WS-ABEND-MESSAGE             
01402                  MOVE CM-STATUS  TO  WS-ABEND-FILE-STATUS         
01403                  GO TO ABEND-PGM                                  
01404               ELSE                                                
01405                  NEXT SENTENCE                                    
01406          ELSE                                                     
01407              MOVE '- COPY RETAINED'     TO WS-EXC-MSG-EE-COPY     
01408              MOVE WS-EXC-MSG-EE-EXPIRED TO DTL-EXC-MSG            
01409              IF (CM-CREDIT-INTERFACE-SW-1 EQUAL '3') AND          
01410                 (CM-CLAIM-INTERFACE-SW    EQUAL '2') AND          
01411                 (CM-CLAIM-ATTACHED-COUNT  EQUAL WS-CLM-CNT)       
01412                   NEXT SENTENCE                                   
01413                ELSE                                               
01414                   MOVE WS-CLM-CNT TO CM-CLAIM-ATTACHED-COUNT      
01415                   MOVE WS-CURRENT-DATE   TO DC-GREG-DATE-1-EDIT   
01416                   MOVE '2'               TO DC-OPTION-CODE        
01417                   PERFORM 7100-DATE-RTN THRU 7100-EXIT            
01418                   MOVE DC-BIN-DATE-1     TO CM-LAST-MONTH-END     
01419                   PERFORM 4000-EXPIRE-DATE  THRU  4099-EXIT       
01420                   REWRITE CERTIFICATE-MASTER                      
01421                   PERFORM 7320-BLD-ONL-PRT-LINE  THRU 7320-EXIT   
01422                   PERFORM 7350-BLD-CLM-INFO      THRU 7350-EXIT   
01423                   PERFORM 7370-PRINT-DETAIL      THRU 7370-EXIT   
01424                   IF CM-STAT-1 NOT EQUAL ZERO                     
01425                      MOVE 'ERROR OCCURED REWRITE  - ELCERT'       
01426                                      TO  WS-ABEND-MESSAGE         
01427                      MOVE CM-STATUS  TO  WS-ABEND-FILE-STATUS     
01428                      GO TO ABEND-PGM.                             
01429                                                                   
01430      GO TO 2301-READ-ONL-CERT.                                    
01431                                                                   
01432  2300-PROCESS-N-READ-MON.                                         
01433                                                                   
01434 *****COPY ELCCRTM2.                                               
01435                                                                   
01436      IF (NO-MATCH-FOUND)                                          
01437         IF (CERT-PEND-ISSUE-ERROR)                                
01438             IF CM-NOTE-SW NOT EQUAL ' '                           
01439                  MOVE WS-EXC-MSG-NO-NOTES   TO DTL-EXC-MSG        
01440                  MOVE WS-CURRENT-DATE       TO DC-GREG-DATE-1-EDIT
01441                  MOVE '2'                   TO DC-OPTION-CODE     
01442                  PERFORM 7100-DATE-RTN THRU 7100-EXIT             
01443                  MOVE DC-BIN-DATE-1         TO CM-LAST-MONTH-END  
01444                  PERFORM 4000-EXPIRE-DATE   THRU  4099-EXIT       
01445                                                                   
01446                  REWRITE CERTIFICATE-MASTER                       
01447                                                                   
01448                  PERFORM 7320-BLD-ONL-PRT-LINE  THRU 7320-EXIT    
01449                  PERFORM 7370-PRINT-DETAIL      THRU 7370-EXIT    
01450                  IF CM-STAT-1 NOT EQUAL ZERO                      
01451                     MOVE 'ERROR OCCURED REWRITE  - ELCERT'        
01452                                     TO  WS-ABEND-MESSAGE          
01453                     MOVE CM-STATUS  TO  WS-ABEND-FILE-STATUS      
01454                     GO TO ABEND-PGM                               
01455                  ELSE                                             
01456                     GO TO 2301-READ-ONL-CERT.                     
01457                                                                   
01458      IF (NO-MATCH-FOUND) AND                                      
01459         (NOT CM-OPEN-END)                                         
01460          PERFORM 2330-FIND-ASSOCIATED-CLAIM  THRU 2330-EXIT       
01461          IF (NO-ASSOCIATED-CLAIM) AND                             
01462             ((CM-CLAIM-ATTACHED-COUNT = 0) OR                     
01463             (CM-GROUPING NOT NUMERIC))                            
01464               MOVE SPACES                TO WS-EXC-MSG-CE-COPY    
01465               MOVE WS-EXC-MSG-CE-DELETE  TO DTL-EXC-MSG           
01466               PERFORM 7320-BLD-ONL-PRT-LINE  THRU 7320-EXIT       
01467               PERFORM 7350-BLD-CLM-INFO      THRU 7350-EXIT       
01468               PERFORM 7370-PRINT-DETAIL      THRU 7370-EXIT       
01469               ADD +1                     TO WS-ONL-DELETED        
01470 *             PERFORM 2280-DELETE-ERMAIL THRU 2280-EXIT           
01471               PERFORM 2270-DELETE-ERCOMM THRU 2270-EXIT           
01472                                                                   
01473               DELETE ELCERTF                                      
01474                                                                   
01475               IF CM-STAT-1 NOT EQUAL ZERO                         
01476                  MOVE 'ERROR OCCURED DELETE  - ELCERT'            
01477                                  TO  WS-ABEND-MESSAGE             
01478                  MOVE CM-STATUS  TO  WS-ABEND-FILE-STATUS         
01479                  GO TO ABEND-PGM                                  
01480               ELSE                                                
01481                  NEXT SENTENCE                                    
01482          ELSE                                                     
01483              MOVE '- COPY RETAINED'     TO WS-EXC-MSG-EE-COPY     
01484              MOVE WS-EXC-MSG-EE-EXPIRED TO DTL-EXC-MSG            
01485              IF (CM-CREDIT-INTERFACE-SW-1 EQUAL '3') AND          
01486                 (CM-CLAIM-INTERFACE-SW    EQUAL '2') AND          
01487                 (CM-CLAIM-ATTACHED-COUNT  EQUAL WS-CLM-CNT)       
01488                   NEXT SENTENCE                                   
01489                ELSE                                               
01490                   MOVE WS-CLM-CNT TO CM-CLAIM-ATTACHED-COUNT      
01491                   MOVE WS-CURRENT-DATE   TO DC-GREG-DATE-1-EDIT   
01492                   MOVE '2'               TO DC-OPTION-CODE        
01493                   PERFORM 7100-DATE-RTN THRU 7100-EXIT            
01494                   MOVE DC-BIN-DATE-1     TO CM-LAST-MONTH-END     
01495                   PERFORM 4000-EXPIRE-DATE  THRU  4099-EXIT       
01496                   REWRITE CERTIFICATE-MASTER                      
01497                   PERFORM 7320-BLD-ONL-PRT-LINE  THRU 7320-EXIT   
01498                   PERFORM 7350-BLD-CLM-INFO      THRU 7350-EXIT   
01499                   PERFORM 7370-PRINT-DETAIL      THRU 7370-EXIT   
01500                   IF CM-STAT-1 NOT EQUAL ZERO                     
01501                      MOVE 'ERROR OCCURED REWRITE  - ELCERT'       
01502                                      TO  WS-ABEND-MESSAGE         
01503                      MOVE CM-STATUS  TO  WS-ABEND-FILE-STATUS     
01504                      GO TO ABEND-PGM.                             
01505                                                                   
01506  2301-READ-ONL-CERT.                                              
01507                                                                   
01508      READ ELCERTF  NEXT RECORD.                                   
01509                                                                   
01510      IF CM-STAT-1 EQUAL '1'                                       
01511          MOVE 'X'                TO WS-ONL-END-SW                 
01512                                     WS-ONL-GE-SW                  
01513          MOVE HIGH-VALUES        TO WS-SAVED-ONL-KEY              
01514          MOVE SPACE              TO WS-O-MATCH-SW                 
01515          GO TO 2300-EXIT.                                         
01516                                                                   
01517      IF CM-STAT-1 NOT EQUAL ZERO                                  
01518          MOVE 'ERROR OCCURED READ NEXT - ELCERT'                  
01519                                  TO  WS-ABEND-MESSAGE             
01520          MOVE CM-STATUS          TO  WS-ABEND-FILE-STATUS         
01521          GO TO ABEND-PGM.                                         
01522                                                                   
01523      IF CM-COMPANY-CD NOT EQUAL DTE-CLASIC-COMPANY-CD             
01524          MOVE 'X'                TO WS-ONL-END-SW                 
01525                                     WS-ONL-GE-SW                  
01526          MOVE HIGH-VALUES        TO WS-SAVED-ONL-KEY              
01527          MOVE SPACE              TO WS-O-MATCH-SW                 
01528          GO TO 2300-EXIT.                                         
01529                                                                   
01530      IF CM-OPEN-END                                               
01531          GO TO 2301-READ-ONL-CERT.                                
01532                                                                   
01533      MOVE SPACE                  TO WS-O-MATCH-SW.                
01534                                                                   
01535  2302-PROC-ONL-CERT.                                              
01536      ADD 1                        TO WS-ONL-READ.                 
01537                                                                   
01542                                                                   
011410     IF CM-ADDL-CLP NOT NUMERIC
011410        MOVE ZEROS                TO CM-ADDL-CLP
011410     END-IF
011410     IF CM-LF-CLP NOT NUMERIC
011410        MOVE ZEROS               TO CM-LF-CLP
011410     END-IF
011410     IF CM-AH-CLP NOT NUMERIC
011410        MOVE ZEROS               TO CM-AH-CLP
011410     END-IF
           IF CM-DDF-IU-RATE-UP NOT NUMERIC
              MOVE ZEROS               TO CM-DDF-IU-RATE-UP
           END-IF

062017     IF CM-INT-ON-REFS NOT NUMERIC                           
062017         MOVE ZEROS               TO CM-INT-ON-REFS.         
01547                                                                   
01548 *****COPY ELCCRTM2.                                               
01549                                                                   
01550      MOVE CERTIFICATE-MASTER      TO WS-ONL-RECORD.               
01551                                                                   
01552      MOVE CM-CLAIM-ATTACHED-COUNT TO WS-CLM-CNT.                  
01553      MOVE CM-CLAIM-INTERFACE-SW   TO WS-CM-ORIG.                  
01554      MOVE CM-LOAN-NUMBER          TO WS-LN-NUMBER.                
01555      MOVE CM-LOAN-BALANCE         TO WS-LN-BALANCE.               
01556      MOVE CM-BENEFICIARY          TO WS-CM-BENEFICIARY.           
01557      MOVE CM-LAST-ADD-ON-DT       TO WS-LST-ADD-ON-DT.            
01558      MOVE CM-ENTRY-BATCH          TO WS-CM-ENTRY-BATCH.           
01559      MOVE CM-LF-EXIT-BATCH        TO WS-CM-LF-EXIT-BATCH.         
01560      MOVE CM-AH-EXIT-BATCH        TO WS-CM-AH-EXIT-BATCH.         
01561      MOVE CM-COMP-EXCP-SW         TO WS-ERCOMM-SW.                
01562      MOVE CM-INSURED-ADDRESS-SW   TO WS-CM-INSURED-ADDRESS-SW.    
020906*    MOVE CM-CLAIM-DEDUCT-WITHHELD                                
020906*                                 TO WS-CLAIM-DEDUCT-WITHHELD.    
020906*    MOVE CM-CANCEL-DEDUCT-WITHHELD                               
020906*                                 TO WS-CANCEL-DEDUCT-WITHHELD.   
01567      MOVE CM-LF-DEATH-EXIT-DT     TO WS-LF-DEATH-EXIT-DT.         
01568      MOVE CM-LF-CURRENT-STATUS    TO WS-LF-CURRENT-STATUS.        
01569      MOVE CM-LF-STATUS-AT-DEATH   TO WS-LF-STATUS-AT-DEATH.       
01570      PERFORM 2370-SAVE-ONL-KEY    THRU 2370-EXIT.                 
01571                                                                   
01572      IF WS-SAVED-ONL-KEY NOT LESS WS-SAVED-ECS-KEY                
01573          MOVE 'X'                 TO WS-ONL-GE-SW.                
01574                                                                   
01575  2300-EXIT.                                                       
01576       EXIT.                                                       
01577                                                                   
01578      EJECT                                                        
01579 *****************************************************             
01580 *  2330-FIND-ASSOCIATED-CLAIM                       *             
01581 *****************************************************             
01582 *  THIS ROUTINE WILL BROWSE THE CLAIM MASTER FILE   *             
01583 *  THRU THE CERTIFICATE ALTERNATE INDEX AND ACCUM-  *             
01584 *  ULATE A COUNT OF CLAIMS ASSOCIATED WITH THE CURR-*             
01585 *  ENT CLAIM MASTER. THE LAST CLAIM MASTER FOUND IS *             
01586 *  SAVED IN CLAIM-MASTER.                           *             
01587 *****************************************************             
01588                                                                   
01589  2330-FIND-ASSOCIATED-CLAIM.                                      
01590                                                                   
01591      MOVE 'N'                   TO WS-CLAIM-SW.                   
01592                                                                   
01593      IF DTE-SYS-E-CLASIC-CLAIMS EQUAL ZERO OR 'N'                 
01594          GO TO 2330-EXIT.                                         
01595                                                                   
01596 ********************************************************          
01597 *  BUILD CL-CONTROL-BY-CERT-NO AND WS-SEC-CLAIM-INDEX  *          
01598 *  FROM THE CURRENT ONLINE CERT MASTER RECORD.         *          
01599 ********************************************************          
01600                                                                   
01601      MOVE CM-COMPANY-CD         TO CL-COMPANY-CD-A4               
01602                                    WS-S-CL-COMP.                  
01603      MOVE CM-CERT-PRIME         TO CL-CERT-A4-PRIME               
01604                                    WS-S-CL-PRIM.                  
01605      MOVE CM-CERT-SFX           TO CL-CERT-A4-SFX                 
01606                                    WS-S-CL-SFX.                   
01607      MOVE CM-GROUPING           TO WS-CL-GRPG.                    
01608      MOVE CM-STATE              TO WS-CL-STAT.                    
01609      MOVE CM-ACCOUNT            TO WS-CL-ACCT.                    
01610      MOVE CM-CARRIER            TO WS-CL-CARR.                    
01611      MOVE CM-CERT-EFF-DT        TO WS-CL-EFDT.                    
01612                                                                   
01613      MOVE ZERO                  TO  WS-CLM-CNT.                   
01614                                                                   
01615      START ELMSTR5                                                
01616                 KEY EQUAL CL-CONTROL-BY-CERT-NO.                  
01617 ***                                                               
01618      IF CL-STATUS EQUAL '23'                                      
01619          GO TO 2350-CHECK-ELRETR.                                 
01620                                                                   
01621      IF CL-STAT-1 EQUAL ZERO                                      
01622          MOVE SPACE              TO WS-CLM-END-SW                 
01623          PERFORM 2335-READNEXT-CLAIM  THRU 2335-EXIT              
01624              UNTIL NO-MORE-CLAIM-MATCH                            
01625      ELSE                                                         
01626          MOVE 'ERROR OCCURED START - ELMSTR'                      
01627                                  TO  WS-ABEND-MESSAGE             
01628          MOVE CL-STATUS          TO  WS-ABEND-FILE-STATUS         
01629          GO TO ABEND-PGM.                                         
01630                                                                   
01631      IF ASSOCIATED-CLAIM                                          
01632          MOVE WS-LAST-CLAIM-MATCHED TO CLAIM-MASTER.              
01633                                                                   
01634  2330-EXIT.                                                       
01635       EXIT.                                                       
01636                                                                   
01637      EJECT                                                        
01638 *****************************************************             
01639 *  2335-READNEXT-CLAIM                              *             
01640 *****************************************************             
01641 *  READ CLAIM MASTER FILE AND CHECK KEYS FOR MATCH  *             
01642 *****************************************************             
01643                                                                   
01644  2335-READNEXT-CLAIM.                                             
01645                                                                   
01646      READ ELMSTR5  NEXT  RECORD.                                  
01647                                                                   
01648      IF CL-STAT-1  EQUAL '1'                                      
01649          MOVE 'X'                TO WS-CLM-END-SW                 
01650          GO TO 2335-EXIT.                                         
01651                                                                   
01652      IF CL-STAT-1  NOT  EQUAL '0'                                 
01653          MOVE 'ERROR OCCURED READNEXT - ELMSTR'                   
01654                                  TO  WS-ABEND-MESSAGE             
01655          MOVE CL-STATUS          TO  WS-ABEND-FILE-STATUS         
01656          GO TO ABEND-PGM.                                         
01657                                                                   
01658      IF CL-CONTROL-BY-CERT-NO  NOT EQUAL WS-SEC-CLAIM-INDEX       
01659          MOVE  'X'               TO WS-CLM-END-SW                 
01660      ELSE                                                         
01661          IF CL-CERT-KEY-DATA EQUAL WS-CLAIM-MATCH-KEY             
01662              ADD +1              TO  WS-CLM-CNT                   
01663              MOVE CLAIM-MASTER   TO WS-LAST-CLAIM-MATCHED         
01664              MOVE 'A'            TO WS-CLAIM-SW.                  
01665                                                                   
01666  2335-EXIT.                                                       
01667       EXIT.                                                       
01668                                                                   
01669      EJECT                                                        
01670                                                                   
01671 ********************************************************          
01672 *  BUILD RL-CONTROL-BY-CERT-NO AND WS-SEC-CLAIM-INDEX  *          
01673 *  FROM THE CURRENT ONLINE CERT MASTER RECORD.         *          
01674 ********************************************************          
01675                                                                   
01676  2350-CHECK-ELRETR.                                               
01677      MOVE CM-COMPANY-CD         TO RL-COMPANY-CD-A4               
01678                                    WS-S-CL-COMP.                  
01679      MOVE CM-CERT-PRIME         TO RL-CERT-A4-PRIME               
01680                                    WS-S-CL-PRIM.                  
01681      MOVE CM-CERT-SFX           TO RL-CERT-A4-SFX                 
01682                                    WS-S-CL-SFX.                   
01683      MOVE CM-GROUPING           TO WS-CL-GRPG.                    
01684      MOVE CM-STATE              TO WS-CL-STAT.                    
01685      MOVE CM-ACCOUNT            TO WS-CL-ACCT.                    
01686      MOVE CM-CARRIER            TO WS-CL-CARR.                    
01687      MOVE CM-CERT-EFF-DT        TO WS-CL-EFDT.                    
01688                                                                   
01689      MOVE ZERO                  TO WS-CLM-CNT.                    
01690                                                                   
01691      START ELRETR5                                                
01692                 KEY EQUAL RL-CONTROL-BY-CERT-NO.                  
01693                                                                   
01694      IF ELRETR5-FILE-STATUS = '23'                                
01695          GO TO 2330-EXIT.                                         
01696                                                                   
01697      IF ELRETR5-FILE-STATUS = ZERO                                
01698          MOVE SPACE              TO WS-CLM-END-SW                 
01699          PERFORM 2355-READNEXT-CLAIM  THRU 2355-EXIT              
01700              UNTIL NO-MORE-CLAIM-MATCH                            
01701      ELSE                                                         
01702          MOVE 'ERROR OCCURRED START - ELRETR'                     
01703                                   TO WS-ABEND-MESSAGE             
01704          MOVE ELRETR5-FILE-STATUS TO WS-ABEND-FILE-STATUS         
01705          GO TO ABEND-PGM.                                         
01706                                                                   
01707      IF ASSOCIATED-CLAIM                                          
01708          MOVE WS-LAST-CLAIM-MATCHED TO CLAIM-MASTER.              
01709                                                                   
01710      GO TO 2330-EXIT.                                             
01711                                                                   
01712      EJECT                                                        
01713 *****************************************************             
01714 *  2355-READNEXT-CLAIM (ELRETR)                     *             
01715 *****************************************************             
01716 *  READ RETRIEVE FILE AND CHECK KEYS FOR MATCH      *             
01717 *****************************************************             
01718                                                                   
01719  2355-READNEXT-CLAIM.                                             
01720                                                                   
01721      READ ELRETR5  NEXT  RECORD.                                  
01722                                                                   
01723      IF ELRETR5-STAT-1 = '1'                                      
01724          MOVE 'X'                TO WS-CLM-END-SW                 
01725          GO TO 2355-EXIT.                                         
01726                                                                   
01727      IF ELRETR5-STAT-1 NOT = ZERO                                 
01728          MOVE 'ERROR OCCURRED READNEXT - ELMSTR'                  
01729                                   TO WS-ABEND-MESSAGE             
01730          MOVE ELRETR5-FILE-STATUS TO WS-ABEND-FILE-STATUS         
01731          GO TO ABEND-PGM.                                         
01732                                                                   
01733      IF RL-CONTROL-BY-CERT-NO NOT = WS-SEC-CLAIM-INDEX            
01734          MOVE  'X'               TO WS-CLM-END-SW                 
01735      ELSE                                                         
01736          IF RL-CERT-KEY-DATA = WS-CLAIM-MATCH-KEY                 
01737              ADD +1 TO WS-CLM-CNT                                 
01738              MOVE RETRIEVE-MASTER  TO WS-LAST-CLAIM-MATCHED       
01739              MOVE 'A'              TO WS-CLAIM-SW.                
01740                                                                   
01741  2355-EXIT.                                                       
01742       EXIT.                                                       
01743                                                                   
01744      EJECT                                                        
01745 ***********************************************************       
01746 *  2370-SAVE-ONL-KEY                                      *       
01747 ***********************************************************       
01748 *  STORE ONLINE CERT MASTER KEY IN WS-CM-CONTROL-PRIMARY  *       
01749 *  AND BUILD WS-SAVED-ONL-KEY FOR COMPARISON.             *       
01750 ***********************************************************       
01751                                                                   
01752  2370-SAVE-ONL-KEY.                                               
01753                                                                   
01754      MOVE CM-CONTROL-PRIMARY     TO WS-CM-CONTROL-PRIMARY.        
01755      MOVE DTE-CLASIC-COMPANY-CD  TO WS-SAVED-O-COMP.              
01756      MOVE CM-CARRIER             TO WS-SAVED-O-CARR.              
01757      MOVE CM-GROUPING            TO WS-SAVED-O-GRPG.              
01758      MOVE CM-STATE               TO WS-SAVED-O-STAT.              
01759      MOVE CM-ACCOUNT             TO WS-SAVED-O-ACCT.              
01760      MOVE CM-CERT-NO             TO WS-SAVED-O-CERT.              
01761                                                                   
01762      MOVE SPACES                 TO DC-OPTION-CODE.               
01763      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.                
01764      PERFORM 7100-DATE-RTN  THRU   7100-EXIT.                     
01765      MOVE DC-GREG-DATE-CYMD      TO WS-SAVED-O-DATE.              
01766                                                                   
01767  2370-EXIT.                                                       
01768       EXIT.                                                       
01769                                                                   
01770      EJECT                                                        
01771 ***********************************************************       
01772 *  2500-PROCESS-MATCHING                                  *       
01773 ***********************************************************       
01774 *  A MATCH HAS OCCURRED BETWEEN THE ONLINE AND BATCH      *       
01775 *  CERT MASTER FILES. DETERMINE THE TYPE OF ACTIVITY IF   *       
01776 *  ANY THAT HAS OCCURRED SINCE LAST MONTH-END AND REPORT  *       
01777 *  IT.                                                    *       
01778 ***********************************************************       
01779                                                                   
01780  2500-PROCESS-MATCHING.                                           
01781      ADD 1                       TO WS-MATCHED-ONL.               
01782      MOVE 'M'                    TO WS-O-MATCH-SW.                
01783      MOVE SPACES                 TO WS-FIRST-TIME-SW.             
01784      MOVE SPACES                 TO WS-PRT-FULL-LINE-SW.          
01785                                                                   
01786 ***************************************************************** 
01787 ***************************************************************** 
01788 *****    SPECIAL CODE FOR JAL AND JAI TO INFORM THEM OF     ***** 
01789 *****    ZERO CERTIFICATES COMING FROM THE BLAZER LOAN      ***** 
01790 *****    SYSTEM.                                            ***** 
01791 ***************************************************************** 
01792 ***************************************************************** 
01793                                                                   
01794      IF DTE-CLIENT EQUAL 'JAL' OR 'JAI'                           
01795         IF (CR-AHPRM + CR-LFPRM) LESS THAN +.05                   
01796            ADD 1 TO NO-PREM-CT                                    
01797            DISPLAY 'NO PREMIUMS FOR CERT ' CR-FULL-CONTROL        
01798            IF ((CR-AHTYP EQUAL ZERO) AND (CR-LFTYP EQUAL ZERO))   
01799               DISPLAY 'NO PLAN CODES FOR CERT ' CR-FULL-CONTROL   
01800               ADD 1 TO NO-PLAN-CODE-CT                            
01801               MOVE +1 TO CR-AHPRM CR-LFPRM                        
01802            ELSE                                                   
01803               IF CR-LFTYP EQUAL ZEROS                             
01804                  MOVE +1 TO CR-AHPRM                              
01805               ELSE                                                
01806                  IF CR-AHTYP EQUAL ZEROS                          
01807                     MOVE +1 TO CR-LFPRM                           
01808                  ELSE                                             
01809                     MOVE +1 TO CR-AHPRM CR-LFPRM.                 
01810                                                                   
01811 ***************************************************************** 
01812 ***************************************************************** 
01813 *****    SPECIAL CODE FOR JAL AND JAI TO INFORM THEM OF     ***** 
01814 *****    ZERO CERTIFICATES COMING FROM THE BLAZER LOAN      ***** 
01815 *****    SYSTEM.                                            ***** 
01816 ***************************************************************** 
01817 ***************************************************************** 
01818                                                                   
01819 ***************************************************************** 
01820 *  PREMIUM TOTAL LESS THAN (.05) EXIT ROUTINE (RE-ISSUED CERT)  * 
01821 ***************************************************************** 
01822                                                                   
01823 *    IF (CR-AHPRM + CR-LFPRM) LESS +.05                           
01824 *        GO TO 2500-EXIT.                                         
01825                                                                   
01826 ***************************************************************** 
01827 *  PROCESS OPTION (2) RE-ATTACHES CERT MASTER TO CLAIMS MASTER  * 
01828 ***************************************************************** 
01829                                                                   
01830      IF DTE-CLIENT = 'MON'                                        
01831          NEXT SENTENCE                                            
01832      ELSE                                                         
01833          IF DTE-PRC-OPT NOT EQUAL '2'                             
01834              IF NO-CLAIM-ATTACHED                                 
01835                  PERFORM 2600-REWRITE-CERT THRU 2600-EXIT         
01836                  GO TO 2500-EXIT.                                 
01837                                                                   
01838      PERFORM 2330-FIND-ASSOCIATED-CLAIM  THRU 2330-EXIT.          
01839                                                                   
01840      IF NO-ASSOCIATED-CLAIM                                       
01841          PERFORM 2600-REWRITE-CERT THRU 2600-EXIT                 
01842          GO TO 2500-EXIT.                                         
01843                                                                   
01844      ADD 1                       TO WS-CLAIM-MATCH.               
01845                                                                   
01846 *********************************************************         
01847 *****  LIFE COVERAGE CANCELLED BUT NOW REINSTATED   *****         
01848 *********************************************************         
01849                                                                   
01850      IF CR-LF-STATUS-AT-CANCEL EQUAL SPACE AND                    
01851         CM-LF-CANCEL-DT NOT EQUAL LOW-VALUE
100518       AND CL-CLAIM-TYPE NOT EQUAL 'O'
01852            MOVE 'POLICY REINSTATED AFTER CANCEL' TO DTL-EXC-MSG   
01853            PERFORM 2510-BUILD-PRINT  THRU 2510-EXIT               
01854            PERFORM 2600-REWRITE-CERT THRU 2600-EXIT               
01855            GO TO 2500-EXIT.                                       
01856                                                                   
01857 *********************************************************         
01858 *****  AH COVERAGE CANCELLED BUT NOW REINSTATED     *****         
01859 *********************************************************         
01860                                                                   
01861      IF CR-AH-STATUS-AT-CANCEL EQUAL SPACE AND                    
01862         CM-AH-CANCEL-DT NOT EQUAL LOW-VALUE                       
01863            MOVE 'POLICY REINSTATED AFTER CANCEL' TO DTL-EXC-MSG   
01864            PERFORM 2510-BUILD-PRINT  THRU 2510-EXIT               
01865            PERFORM 2600-REWRITE-CERT THRU 2600-EXIT               
01866            GO TO 2500-EXIT.                                       
01867                                                                   
01868 *************************************                             
01869 *****  LIFE COVERAGE CANCELLED  *****                             
01870 *************************************                             
01871                                                                   
01872      IF CR-LF-STATUS-AT-CANCEL NOT EQUAL SPACE AND                
01873         CM-LF-CANCEL-DT EQUAL LOW-VALUE                           
01874            MOVE ' LIFE COV CANCELLED AS OF '                      
01875                                    TO WS-EXC-MSG-DESC             
01876            MOVE CR-LF-CNC-MO       TO WS-EXC-C-MO                 
01877            MOVE CR-LF-CNC-DA       TO WS-EXC-C-DA                 
01878            MOVE CR-LF-CNC-YR       TO WS-EXC-C-YR                 
01879            MOVE WS-EXC-MSG-CANCEL  TO DTL-EXC-MSG                 
01880            PERFORM 2510-BUILD-PRINT  THRU 2510-EXIT               
01881            PERFORM 2600-REWRITE-CERT THRU 2600-EXIT               
01882            GO TO 2500-EXIT.                                       
01883                                                                   
01884 *************************************                             
01885 *****  AH COVERAGE CANCELLED    *****                             
01886 *************************************                             
01887                                                                   
01888      IF CR-AH-STATUS-AT-CANCEL NOT EQUAL SPACE AND                
01889         CM-AH-CANCEL-DT EQUAL LOW-VALUE                           
01890            MOVE ' A&H  COV CANCELLED AS OF '                      
01891                                    TO WS-EXC-MSG-DESC             
01892            MOVE CR-AH-CNC-MO       TO WS-EXC-C-MO                 
01893            MOVE CR-AH-CNC-DA       TO WS-EXC-C-DA                 
01894            MOVE CR-AH-CNC-YR       TO WS-EXC-C-YR                 
01895            MOVE WS-EXC-MSG-CANCEL  TO DTL-EXC-MSG                 
01896            PERFORM 2510-BUILD-PRINT  THRU 2510-EXIT               
01897            PERFORM 2600-REWRITE-CERT THRU 2600-EXIT               
01898            GO TO 2500-EXIT.                                       
01899                                                                   
01900 *************************************                             
01901 *****  LIFE COVERAGE DELETED    *****                             
01902 *************************************                             
01903                                                                   
01904      IF CR-LFTYP EQUAL ZEROS AND                                  
01905         CM-LF-BENEFIT-CD NOT EQUAL ZERO                           
01906          MOVE WS-EXC-MSG-CO-DELETE TO DTL-EXC-MSG                 
01907          PERFORM 2510-BUILD-PRINT  THRU 2510-EXIT                 
01908          PERFORM 2600-REWRITE-CERT THRU 2600-EXIT                 
01909          GO TO 2500-EXIT.                                         
01910                                                                   
01911 *************************************                             
01912 *****  AH COVERAGE DELETED      *****                             
01913 *************************************                             
01914                                                                   
01915      IF CR-AHTYP EQUAL ZEROS AND                                  
01916         CM-AH-BENEFIT-CD NOT EQUAL ZERO                           
01917          MOVE WS-EXC-MSG-CO-DELETE TO DTL-EXC-MSG                 
01918          PERFORM 2510-BUILD-PRINT  THRU 2510-EXIT                 
01919          PERFORM 2600-REWRITE-CERT THRU 2600-EXIT                 
01920          GO TO 2500-EXIT.                                         
01921                                                                   
01922 *************************************                             
01923 *****  COVERAGE ADDED FOR LIFE  *****                             
01924 *************************************                             
01925                                                                   
01926      IF CERT-WAS-CREATED-FOR-CLAIM                                
100518         IF CL-CLAIM-TYPE EQUAL LIFE-OVERRIDE-L1 OR 'O'
01928             MOVE CR-LFTYP         TO WS-EXC-MSG-ADDED-TYPE        
01929             MOVE CR-LFAMT         TO WS-EXC-MSG-ADDED-AMT         
01930             MOVE WS-EXC-MSG-ADDED TO DTL-EXC-MSG                  
01931             PERFORM 2600-REWRITE-CERT THRU 2600-EXIT              
01932             PERFORM 2510-BUILD-PRINT  THRU 2510-EXIT              
01933             GO TO 2500-EXIT.                                      
01934                                                                   
01935 *************************************                             
01936 *****  COVERAGE ADDED FOR AH   *****                              
01937 *************************************                             
01938                                                                   
01939      IF CERT-WAS-CREATED-FOR-CLAIM                                
011215         IF CL-CLAIM-TYPE = AH-OVERRIDE-L1 or 'I' or 'G' or 'F'
022122            OR 'B' OR 'H'
01941             MOVE CR-AHTYP         TO WS-EXC-MSG-ADDED-TYPE        
01942             MOVE CR-AHAMT         TO WS-EXC-MSG-ADDED-AMT         
01943             MOVE WS-EXC-MSG-ADDED TO DTL-EXC-MSG                  
01944             PERFORM 2600-REWRITE-CERT THRU 2600-EXIT              
01945             PERFORM 2510-BUILD-PRINT  THRU 2510-EXIT              
01946             GO TO 2500-EXIT.                                      
01947                                                                   
100518     IF (CL-CLAIM-TYPE EQUAL LIFE-OVERRIDE-L1 OR 'O') AND
01949         (CR-LFTYP EQUAL CM-LF-BENEFIT-CD) AND                     
01950         (CR-LFAMT EQUAL CM-LF-BENEFIT-AMT)                        
01951           PERFORM 2600-REWRITE-CERT THRU 2600-EXIT                
01952           GO TO 2500-EXIT.                                        
01953                                                                   
011215     IF (CL-CLAIM-TYPE = AH-OVERRIDE-L1 or 'I' or 'G' or 'F'
022122         OR 'B' OR 'H') AND
01955         (CR-AHTYP  EQUAL CM-AH-BENEFIT-CD) AND                    
01956         (CR-AHAMT  EQUAL CM-AH-BENEFIT-AMT)                       
01957           PERFORM 2600-REWRITE-CERT THRU 2600-EXIT                
01958           GO TO 2500-EXIT.                                        
01959                                                                   
100518     IF CL-CLAIM-TYPE EQUAL LIFE-OVERRIDE-L1 OR 'O'
01961          MOVE CM-LF-BENEFIT-AMT      TO WS-EXC-MSG-CHGD-AMT-NEW   
01962          MOVE CM-LF-BENEFIT-CD       TO WS-EXC-MSG-CHGD-TYPE-NEW  
01963          MOVE WS-EXC-MSG-CHANGED-NEW TO DTL-EXC-MSG               
01964          PERFORM 2510-BUILD-PRINT  THRU 2510-EXIT                 
01965          MOVE CR-LFTYP               TO WS-EXC-MSG-CHGD-TYPE-OLD  
01966          MOVE CR-LFAMT               TO WS-EXC-MSG-CHGD-AMT-OLD   
01967          PERFORM 2600-REWRITE-CERT THRU 2600-EXIT.                
01968                                                                   
011215     IF CL-CLAIM-TYPE = AH-OVERRIDE-L1 or 'I' or 'G' or 'F'
022122        OR 'B' OR 'H'
01970          MOVE CM-AH-BENEFIT-AMT      TO WS-EXC-MSG-CHGD-AMT-NEW   
01971          MOVE CM-AH-BENEFIT-CD       TO WS-EXC-MSG-CHGD-TYPE-NEW  
01972          MOVE WS-EXC-MSG-CHANGED-NEW TO DTL-EXC-MSG               
01973          PERFORM 2510-BUILD-PRINT THRU 2510-EXIT                  
01974          MOVE CR-AHTYP               TO WS-EXC-MSG-CHGD-TYPE-OLD  
01975          MOVE CR-AHAMT               TO WS-EXC-MSG-CHGD-AMT-OLD   
01976          PERFORM 2600-REWRITE-CERT THRU 2600-EXIT.                
01977                                                                   
042314***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
042314***                                                            ***
042314***  We found a condition where the claim type was blank       ***
042314***  and the on-line cert had a pending status and it should   ***
042314***  not have so we decided to re-build the on-line cert       ***
042314***  based on the off-line cert. BTW, do-not-load means that   ***
042314***  the cert is old and it thinks there isnt an on-line equal ***
042314***                                                            ***
042314***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
042314
042314     if (cl-claim-type = ' ')
042314        and (do-not-load)
042314        and ((cm-lf-current-status = '2')
042314           or (cm-ah-current-status = '2'))
042314        PERFORM 2600-REWRITE-CERT THRU 2600-EXIT
042314        GO TO 2500-EXIT
042314     end-if

090314     if (cl-claim-type = ' ')
090314        and (CR-TEMP-EPIQ = 'EPIQ')
090314        and (CM-TEMP-EPIQ not = 'EQ')
090314        move 'EQ'                to cm-temp-epiq
090314        PERFORM 2600-REWRITE-CERT THRU 2600-EXIT
090314        go to 2500-exit
090314     end-if

01978      PERFORM 2530-PRINT-OLD THRU 2530-EXIT.                       
01979                                                                   
01980  2500-EXIT.                                                       
01981       EXIT.                                                       
01982                                                                   
01983      EJECT                                                        
01984  2510-BUILD-PRINT.                                                
01985      IF NOT LINE-FULL                                             
01986          PERFORM 7300-BLD-ECS-PRT-LINE  THRU 7300-EXIT            
01987          PERFORM 7350-BLD-CLM-INFO      THRU 7350-EXIT.           
01988                                                                   
01989      PERFORM 7370-PRINT-DETAIL THRU 7370-EXIT.                    
01990                                                                   
01991  2510-EXIT.                                                       
01992       EXIT.                                                       
01993                                                                   
01994  2530-PRINT-OLD.                                                  
01995                                                                   
01996      MOVE SPACES                 TO DTL-1.                        
01997      MOVE WS-EXC-MSG-CHANGED-OLD TO DTL-EXC-MSG.                  
01998      MOVE DTL-1                  TO P-DATA.                       
01999      MOVE ' '                    TO X.                            
02000      ADD  1                      TO WS-LINE-CNT.                  
02001      PERFORM 7600-PRT-RTN   THRU 7600-EXIT.                       
02002                                                                   
02003  2530-EXIT.                                                       
02004       EXIT.                                                       
02005                                                                   
02006      EJECT                                                        
02007 ***********************************************************       
02008 *  2600-REWRITE-CERT                                      *       
02009 ***********************************************************       
02010 *  REWRITE THE UPDATED ONLINE CERTIFICATE MASTER RECORD   *       
02011 ***********************************************************       
02012                                                                   
02013  2600-REWRITE-CERT.                                               
02014                                                                   
02015 *****COPY ELCCRTM1.                                               
02016                                                                   
02017      MOVE CERTIFICATE-MASTER TO WS-BEFORE-CERT.                   
02018                                                                   
02019      PERFORM 2830-MOVE-DATA  THRU 2830-EXIT.                      
02020                                                                   
02021      IF MATCHED-REC                                               
02022          MOVE WS-LN-NUMBER        TO CM-LOAN-NUMBER               
02023          MOVE WS-LN-BALANCE       TO CM-LOAN-BALANCE              
02024          MOVE WS-CM-BENEFICIARY   TO CM-BENEFICIARY               
02025          MOVE WS-LST-ADD-ON-DT    TO CM-LAST-ADD-ON-DT            
02026          MOVE WS-CM-ENTRY-BATCH   TO CM-ENTRY-BATCH               
02027          MOVE WS-CM-LF-EXIT-BATCH TO CM-LF-EXIT-BATCH             
02028          MOVE WS-CM-AH-EXIT-BATCH TO CM-AH-EXIT-BATCH             
02029          MOVE WS-CLM-CNT          TO CM-CLAIM-ATTACHED-COUNT      
02030          MOVE WS-CM-INSURED-ADDRESS-SW                            
02031                                   TO CM-INSURED-ADDRESS-SW        
020906*        MOVE WS-CLAIM-DEDUCT-WITHHELD                            
020906*                                 TO CM-CLAIM-DEDUCT-WITHHELD     
020906*        MOVE WS-CANCEL-DEDUCT-WITHHELD                           
020906*                                 TO CM-CANCEL-DEDUCT-WITHHELD    
02036          IF CM-CLAIM-ATTACHED-COUNT EQUAL ZERO                    
02037                MOVE SPACE         TO CM-CLAIM-INTERFACE-SW        
02038            ELSE                                                   
02039                MOVE '1'           TO CM-CLAIM-INTERFACE-SW.       
02040                                                                   
02041      IF MATCHED-REC                                               
02042          IF WS-LF-DEATH-EXIT-DT NOT LESS THAN WS-EOM-DT           
02043              MOVE WS-LF-DEATH-EXIT-DT                             
02044                                   TO  CM-LF-DEATH-EXIT-DT         
02045              MOVE WS-LF-CURRENT-STATUS                            
02046                                   TO  CM-LF-CURRENT-STATUS        
02047              MOVE WS-LF-STATUS-AT-DEATH                           
02048                                   TO  CM-LF-STATUS-AT-DEATH.      
02049                                                                   
02050      IF ASSOCIATED-ERCOMM                                         
02051         MOVE ' '                  TO CM-COMP-EXCP-SW              
02052         PERFORM 2270-DELETE-ERCOMM THRU 2270-EXIT.                
02053                                                                   
02054      ADD 1 TO WS-ACCT-READS.                                      
02055      PERFORM 2200-MATCH-ACCT THRU 2200-EXIT.                      
02056                                                                   
02057      PERFORM 4000-EXPIRE-DATE  THRU  4099-EXIT.                   
02058                                                                   
02059      MOVE WS-CURRENT-DATE         TO DC-GREG-DATE-1-EDIT.         
02060      MOVE '2'                     TO DC-OPTION-CODE.              
02061      PERFORM 7100-DATE-RTN THRU 7100-EXIT.                        
02062                                                                   
02063 *****COPY ELCCRTM2.                                               
02064                                                                   
02065      IF WS-BEFORE-CERT NOT EQUAL CERTIFICATE-MASTER               
02066         MOVE DC-BIN-DATE-1        TO CM-LAST-MONTH-END            
02067         REWRITE CERTIFICATE-MASTER.                               
02068                                                                   
02069      IF CM-STAT-1 NOT EQUAL ZERO                                  
02070           MOVE CM-STATUS          TO  WS-ABEND-FILE-STATUS        
02071           MOVE 'ERROR OCCURED REWRITE - ELCERT'                   
02072                                   TO  WS-ABEND-MESSAGE            
02073          GO TO ABEND-PGM.                                         
02074                                                                   
02075      MOVE ZERO                    TO  WS-UPDATE-SW.               
02076                                                                   
02077  2600-EXIT.                                                       
02078       EXIT.                                                       
02079                                                                   
02080      EJECT                                                        
02081 ***********************************************************       
02082 *  2800-ADD-NEW-CERT                                      *       
02083 ***********************************************************       
02084 *  REWRITE THE UPDATED ONLINE CERTIFICATE MASTER RECORD   *       
02085 ***********************************************************       
02086                                                                   
02087  2800-ADD-NEW-CERT.                                               
02088                                                                   
02089      MOVE SPACES                 TO CERTIFICATE-MASTER.           
02090                                                                   
02091 *****COPY ELCCRTM1.                                               
02092                                                                   
02093      PERFORM 2830-MOVE-DATA      THRU 2830-EXIT.                  
02094      PERFORM 2820-BUILD-NEW-KEY  THRU 2820-EXIT.                  
02095                                                                   
02096      ADD 1 TO WS-ACCT-READS.                                      
02097      PERFORM 2200-MATCH-ACCT THRU 2200-EXIT.                      
02098                                                                   
02099      MOVE WS-CURRENT-DATE         TO DC-GREG-DATE-1-EDIT.         
02100      MOVE '2'                     TO DC-OPTION-CODE.              
02101      PERFORM 7100-DATE-RTN THRU 7100-EXIT.                        
02102      MOVE DC-BIN-DATE-1           TO CM-LAST-MONTH-END            
02103      PERFORM 4000-EXPIRE-DATE  THRU  4099-EXIT.                   
02104                                                                   
02105      WRITE CERTIFICATE-MASTER.                                    
02106                                                                   
02107      IF CM-STATUS EQUAL '22'                                      
02108          NEXT SENTENCE                                            
02109       ELSE                                                        
02110        IF CM-STAT-1  NOT EQUAL ZERO                               
02111          MOVE CM-STATUS          TO  WS-ABEND-FILE-STATUS         
02112          MOVE 'ERROR OCCURED WRITE - ELCERT'                      
02113                                  TO  WS-ABEND-MESSAGE             
02114          GO TO ABEND-PGM.                                         
02115                                                                   
02116      MOVE  WS-ONL-RECORD         TO CERTIFICATE-MASTER.           
02117                                                                   
02118  2800-EXIT.                                                       
02119       EXIT.                                                       
02120                                                                   
02121      EJECT                                                        
02122 ***********************************************************       
02123 *  2820-BUILD-NEW-KEY                                     *       
02124 ***********************************************************       
02125 *  BUILD PRIMARY AND ALL ALTERNATE KEY FIELDS IN ONLINE   *       
02126 *  CERT MASTER FROM BATCH CERT MASTER.                    *       
02127 ***********************************************************       
02128                                                                   
02129  2820-BUILD-NEW-KEY.                                              
02130                                                                   
02131      MOVE DTE-CLASIC-COMPANY-CD  TO CM-COMPANY-CD                 
02132                                     CM-COMPANY-CD-A1              
02133                                     CM-COMPANY-CD-A2              
02134                                     CM-COMPANY-CD-A4              
02135                                     CM-COMPANY-CD-A5.             
02136                                                                   
02137      MOVE CR-CARRIER             TO CM-CARRIER.                   
02138      MOVE CR-GROUPING            TO CM-GROUPING.                  
02139      MOVE CR-STATE               TO CM-STATE.                     
02140      MOVE CR-ACCOUNT             TO CM-ACCOUNT.                   
02141      MOVE CR-CERT-NO             TO CM-CERT-NO                    
02142                                     CM-CERT-NO-A4.                
02143                                                                   
02144      MOVE BIN-CR-DT              TO CM-CERT-EFF-DT.               
02145                                                                   
02146      IF CR-LNAME EQUAL SPACES                                     
02147          MOVE CR-CERT-NO         TO CR-LNAME.                     
02148                                                                   
02149      MOVE CR-INIT                TO CM-INSURED-INITIAL2.          
02150      MOVE CR-1ST-INITIAL         TO CM-INSURED-INITIAL1.          
02151      MOVE CR-LNAME               TO CM-INSURED-LAST-NAME.         
02152                                                                   
02153      IF CR-MEMBER-NO NOT = SPACES                                 
02154          MOVE CR-MEMBER-NO       TO CM-MEMBER-NO.                 
02155                                                                   
02156      INSPECT CR-MEMBER-NO                                         
02157          REPLACING ALL ' ' BY '0'.                                
02158                                                                   
02159      IF CR-MEMBER-NO = ZEROS                                      
02160        IF DTE-CLIENT = 'MON'                                      
02161            MOVE CR-LNAME            TO WK-LAST-NAME               
02162            MOVE SPACES              TO WK-MEMB-ACCT               
02163                                        ERROR-FLAG                 
02164            MOVE 1                   TO ACCT-SUB                   
02165            PERFORM 2825-FILL-ACCT THRU 2825-EXIT                  
02166                VARYING LST-NM-SUB FROM 1 BY 1                     
02167                  UNTIL LST-NM-SUB GREATER 12                      
02168            MOVE WK-MEMB-ACCT        TO CM-MEMB-ACCOUNT            
02169            MOVE CR-YR               TO CM-INSURED-INITIALS-A5     
02170            MOVE CR-MO               TO CM-PART-LAST-NAME-A5       
02171         ELSE                                                      
02172            MOVE CR-STATE            TO CM-MEMB-STATE              
02173            MOVE CR-ACCT-PRIME       TO CM-MEMB-ACCOUNT            
02174            MOVE CR-LNAME            TO CM-PART-LAST-NAME-A5       
02175            MOVE CM-INSURED-INITIALS TO CM-INSURED-INITIALS-A5.    
02176                                                                   
02177      IF CR-SOC-SEC NOT = SPACES                                   
02178          MOVE CR-SOC-SEC         TO CM-SOC-SEC-NO.                
02179                                                                   
02180      INSPECT CR-SOC-SEC                                           
02181          REPLACING ALL ' ' BY '0'.                                
02182                                                                   
02183      IF CR-SOC-SEC = ZEROS                                        
02184          MOVE CR-STATE           TO CM-SSN-STATE                  
02185          MOVE CR-ACCT-PRIME      TO CM-SSN-ACCOUNT                
02186          MOVE CR-LNAME           TO CM-PART-LAST-NAME-A2          
02187          MOVE CM-INSURED-INITIALS TO CM-INSURED-INITIALS-A2.      
02188                                                                   
02189  2820-EXIT.                                                       
02190       EXIT.                                                       
02191                                                                   
02192      EJECT                                                        
02193  2825-FILL-ACCT.                                                  
02194      IF ACCT-SUB GREATER 6                                        
02195          MOVE 12            TO LST-NM-SUB                         
02196          GO TO 2825-EXIT.                                         
02197      IF WK-LAST-NAME-CHAR (LST-NM-SUB) NUMERIC                    
02198          MOVE 'A'           TO ERROR-FLAG                         
02199          GO TO 2825-EXIT.                                         
02200      IF WK-LAST-NAME-CHAR (LST-NM-SUB) ALPHABETIC AND             
02201         WK-LAST-NAME-CHAR (LST-NM-SUB) NOT = SPACE                
02202          MOVE WK-LAST-NAME-CHAR (LST-NM-SUB)                      
02203                             TO WK-MEMB-ACCT-CHAR (ACCT-SUB)       
02204          ADD 1 TO ACCT-SUB.                                       
02205                                                                   
02206  2825-EXIT.                                                       
02207       EXIT.                                                       
02208      EJECT                                                        
02209 ***********************************************************       
02210 *  2830-MOVE-DATA                                         *       
02211 ***********************************************************       
02212 *                                                         *       
02213 *  BUILD ONLINE CERT MASTER RECORD FROM THE BATCH CERT    *       
02214 *  MASTER RECORD.                                         *       
02215 *                                                         *       
02216 ***********************************************************       
02217 ****  STRAIGHT MOVES FROM BATCH CERT TO ONLINE CERT    ****       
02218 ***********************************************************       
02219                                                                   
02220  2830-MOVE-DATA.                                                  
02221                                                                   
02222      MOVE 'CM'                   TO CM-RECORD-ID.                 
02223      MOVE LOW-VALUES             TO CM-STATUS-DATA.               
02224      MOVE SPACE                  TO CM-CLAIM-INTERFACE-SW.        
02225      MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-1      
02226                                     CM-CREDIT-INTERFACE-SW-2.     
02227      MOVE ZERO                   TO CM-CLAIM-ATTACHED-COUNT       
02228                                     CM-LOAN-BALANCE               
02229                                     CM-LF-REMAINING-AMT.          
02230                                                                   
02231      MOVE CR-LFRFND              TO CM-LF-ITD-CANCEL-AMT.         
02232      MOVE CR-DTHAMT              TO CM-LF-ITD-DEATH-AMT.          
02233      MOVE CR-AHRFND              TO CM-AH-ITD-CANCEL-AMT.         
02234      MOVE CR-RATING-CLASS        TO CM-RATE-CLASS.                
032612     if dte-client = 'AHL'
032612        move cr-lf-class-cd      to cm-lf-class-cd
032612        move cr-ah-class-cd      to cm-ah-class-cd
032612     end-if

090314     if cr-temp-epiq = 'EPIQ'
090314        add 1 to ws-epiq-cert-added
090314        MOVE 'EQ'                to cm-temp-epiq
090314     else
090314        move spaces              to cm-temp-epiq
090314     end-if

02235      MOVE CR-AH-DEV-CODE         TO CM-AH-DEV-CODE.               
02236      MOVE CR-LF-DEV-CODE         TO CM-LF-DEV-CODE.               
02237      MOVE CR-AH-DEV-PCT          TO CM-AH-DEV-PCT.                
02238      MOVE CR-LF-DEV-PCT          TO CM-LF-DEV-PCT.                
02239      MOVE CR-APR                 TO CM-LOAN-APR.                  
02240      MOVE CR-PMT-EXTENSION-DAYS  TO CM-PMT-EXTENSION-DAYS         
02241      MOVE CR-PMT-FREQ            TO CM-PAY-FREQUENCY.             
02242      MOVE CR-LOAN-TERM           TO CM-LOAN-TERM                  
02243      MOVE CR-LF-TERM-IN-DAYS     TO CM-LF-TERM-IN-DAYS            
02244      MOVE CR-LOAN-OFFICER        TO CM-LOAN-OFFICER.              
02245      MOVE CR-JOINT-NAME          TO CM-JOINT-INSURED-NAME.        
02246      MOVE CR-FNAME               TO CM-INSURED-FIRST-NAME.        
061405     MOVE CR-CLP-STATE           TO CM-CLP-STATE
032612     if dte-client not = 'AHL'
02247         MOVE CR-USER-FUTURE         TO CM-USER-RESERVED
032612     end-if
02248                                                                   
020906*    IF CR-CLAIM-DEDUCT-WITHHELD NUMERIC                          
020906*        MOVE CR-CLAIM-DEDUCT-WITHHELD                            
020906*                                 TO CM-CLAIM-DEDUCT-WITHHELD     
020906*    ELSE                                                         
020906*        MOVE ZEROS               TO CM-CLAIM-DEDUCT-WITHHELD.    
020906*    IF CR-CANCEL-DEDUCT-WITHHELD NUMERIC                         
020906*        MOVE CR-CANCEL-DEDUCT-WITHHELD                           
020906*                                 TO CM-CANCEL-DEDUCT-WITHHELD    
020906*    ELSE                                                         
020906*        MOVE ZEROS               TO CM-CANCEL-DEDUCT-WITHHELD.   
02259                                                                   
02260      MOVE CR-CSR-CODE             TO CM-CSR-CODE.                 
02261      MOVE CR-USER-CODE            TO CM-USER-FIELD.               
02262      MOVE CR-UNDERWRITING-CODE    TO CM-UNDERWRITING-CODE.        
02263                                                                   
011410     IF CR-ADDL-CLP NOT NUMERIC
011410        MOVE ZEROS                TO CR-ADDL-CLP
011410     END-IF
011410     IF CR-LF-CLP NOT NUMERIC
011410        MOVE ZEROS                TO CR-LF-CLP
011410     END-IF
011410     IF CR-AH-CLP NOT NUMERIC
011410        MOVE ZEROS                TO CR-AH-CLP
011410     END-IF
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
011410        MOVE CR-ADDL-CLP         TO CM-ADDL-CLP
011410        MOVE CR-LF-CLP           TO CM-LF-CLP
011410        MOVE CR-AH-CLP           TO CM-AH-CLP
              MOVE CR-DDF-IU-RATE-UP   TO CM-DDF-IU-RATE-UP
011410     END-IF
02268                                                                   
062017     IF CR-INT-ON-REF     NUMERIC                              
062017         MOVE CR-INT-ON-REF      TO CM-INT-ON-REFS          
02271      ELSE                                                         
062017         MOVE ZEROS               TO CM-INT-ON-REFS.         
02273                                                                   
02274      IF DTE-CLIENT EQUAL 'FIA'                                    
02275          INSPECT CR-LNAME REPLACING ALL '0' BY ' '                
02276          INSPECT CR-LNAME REPLACING ALL '{' BY ' '.               
02277                                                                   
02278      IF CR-LNAME IS EQUAL TO SPACES                               
02279          NEXT SENTENCE                                            
02280      ELSE                                                         
02281          MOVE CR-LNAME          TO CM-INSURED-LAST-NAME.          
02282                                                                   
02283      IF CR-MEMBER-NO IS EQUAL TO SPACES                           
02284          MOVE CR-STATE          TO CM-MEMB-STATE                  
02285          MOVE CR-ACCT-PRIME     TO CM-MEMB-ACCOUNT                
02286          MOVE CR-LNAME          TO CM-PART-LAST-NAME-A5           
02287          MOVE CM-INSURED-INITIALS TO CM-INSURED-INITIALS-A5       
02288      ELSE                                                         
02289          MOVE CR-MEMBER-NO      TO CM-MEMBER-NO.                  
02290                                                                   
02291      IF CM-POLICY-FORM-NO EQUAL SPACES                            
02292          MOVE CR-POLICY-FORM-NO  TO CM-POLICY-FORM-NO.            
02293                                                                   
02294      IF CR-IND-GRP EQUAL '1' OR 'I'                               
02295          MOVE 'I'                TO CM-IND-GRP-TYPE               
02296       ELSE                                                        
02297          MOVE 'G'                TO CM-IND-GRP-TYPE.              
02298                                                                   
02299      MOVE SPACE                  TO CM-PAYMENT-MODE.              
02300      MOVE WS-CM-ENTRY-BATCH      TO CM-ENTRY-BATCH.               
010716     if cr-entry-batch not = spaces
010716        move cr-entry-batch      to cm-entry-batch
010716     end-if
02301      MOVE CR-REIN-TABLE          TO CM-REIN-TABLE.                
02302      MOVE CR-REIN-SPEC           TO CM-SPECIAL-REIN-CODE.         
02303                                                                   
02304      MOVE SPACE                  TO CM-SKIP-CODE.                 
02305      IF CR-SKIP = 01                                              
02306         MOVE '1'                 TO CM-SKIP-CODE.                 
02307      IF CR-SKIP = 02                                              
02308         MOVE '2'                 TO CM-SKIP-CODE.                 
02309      IF CR-SKIP = 03                                              
02310         MOVE '3'                 TO CM-SKIP-CODE.                 
02311      IF CR-SKIP = 04                                              
02312         MOVE '4'                 TO CM-SKIP-CODE.                 
02313      IF CR-SKIP = 05                                              
02314         MOVE '5'                 TO CM-SKIP-CODE.                 
02315      IF CR-SKIP = 06                                              
02316         MOVE '6'                 TO CM-SKIP-CODE.                 
02317      IF CR-SKIP = 07                                              
02318         MOVE '7'                 TO CM-SKIP-CODE.                 
02319      IF CR-SKIP = 08                                              
02320         MOVE '8'                 TO CM-SKIP-CODE.                 
02321      IF CR-SKIP = 09                                              
02322         MOVE '9'                 TO CM-SKIP-CODE.                 
02323      IF CR-SKIP = 10                                              
02324         MOVE 'A'                 TO CM-SKIP-CODE.                 
02325      IF CR-SKIP = 11                                              
02326         MOVE 'X'                 TO CM-SKIP-CODE.                 
02327                                                                   
02328      MOVE ZERO                   TO CM-LIFE-COMM-PCT              
02329                                     CM-AH-COMM-PCT.               
02330                                                                   
02331      MOVE +1                     TO WS-LEVEL-INDEX.               
02332                                                                   
02333  2830-LOAD-COMM-PCTS.                                             
02334                                                                   
02335      IF (CR-AGT-TYPE (WS-LEVEL-INDEX) EQUAL 'C' OR 'D'            
052814                  OR 'F')                                  
02337          MOVE CR-LCOM-L (WS-LEVEL-INDEX)  TO CM-LIFE-COMM-PCT     
02338          MOVE CR-LCOM-AH (WS-LEVEL-INDEX) TO CM-AH-COMM-PCT       
02339          GO TO 2830-COMM-LOAD-END                                 
02340      ELSE                                                         
02341          ADD +1 TO WS-LEVEL-INDEX                                 
02342          IF WS-LEVEL-INDEX GREATER +10                            
02343              NEXT SENTENCE                                        
02344          ELSE                                                     
02345               GO TO 2830-LOAD-COMM-PCTS.                          
02346                                                                   
02347  2830-COMM-LOAD-END.                                              
02348                                                                   
02349 **************************************                            
02350 *  PREMIUM TYPE (2) EQUAL O.B. COVERAGE *                         
02351 *  PREMIUM TYPE (3) EQUAL OPEN END   *                            
02352 **************************************                            
02353 ***********************************************************       
02354 ****  MOVES FROM BATCH CERT TO ONLINE CERT ONLY FOR    ****       
02355 ****  SINGLE PREMIUM CERTS.                            ****       
02356 ***********************************************************       
02357                                                                   
02358      IF MATCHED-REC                                               
02359        AND CM-PREMIUM-TYPE EQUAL ('2' OR '3')                     
02360           GO TO 2830-OB-ADJUST-END.                               
02361                                                                   
02362      IF MATCHED-REC   AND                                         
02363        (CR-LFPRM + CR-AHPRM) LESS +.05                            
02364           GO TO 2830-OB-ADJUST-END.                               
02365                                                                   
02366      MOVE CR-AGE                 TO CM-INSURED-ISSUE-AGE.         
02367      MOVE CR-JOINT-AGE           TO CM-INSURED-JOINT-AGE          
02368      MOVE CR-SEX                 TO CM-INSURED-SEX.               
02369      MOVE CR-LFTYP               TO CM-LF-BENEFIT-CD.             
02370      MOVE CR-LF-TERM             TO CM-LF-ORIG-TERM.              
02371      MOVE CR-LFAMT               TO CM-LF-BENEFIT-AMT.            
02372      MOVE CR-LFAMT-ALT           TO CM-LF-ALT-BENEFIT-AMT.        
02373      MOVE CR-LFPRM               TO CM-LF-PREMIUM-AMT.            
02374      MOVE CR-LFPRM-ALT           TO CM-LF-ALT-PREMIUM-AMT.        
02375      MOVE CR-LF-NSP-PRM          TO CM-LF-NSP-PREMIUM-AMT.        
02376      MOVE CR-AH-NSP-PRM          TO CM-AH-NSP-PREMIUM-AMT.        
02377      MOVE CR-LFPRM-RATE          TO CM-LF-PREMIUM-RATE.           
02378      MOVE CR-LFPRM-RATE-ALT      TO CM-LF-ALT-PREMIUM-RATE.       
02379      MOVE CR-LF-CRIT-PERIOD      TO CM-LF-CRITICAL-PERIOD.        
02380      MOVE CR-AH-CRIT-PERIOD      TO CM-AH-CRITICAL-PERIOD.        
02381      MOVE CR-AHTYP               TO CM-AH-BENEFIT-CD.             
02382      MOVE CR-AH-TERM             TO CM-AH-ORIG-TERM.              
02383      MOVE CR-AHAMT               TO CM-AH-BENEFIT-AMT.            
02384      MOVE CR-AHPRM               TO CM-AH-PREMIUM-AMT.            
02385      MOVE CR-AHPRM-RATE          TO CM-AH-PREMIUM-RATE.           
02386      MOVE CR-DISAMT              TO CM-AH-ITD-AH-PMT.             
02387      MOVE '1'                    TO CM-PREMIUM-TYPE.              
02388                                                                   
010716     IF DTE-CLIENT NOT = 'DCC' and 'VPP'
02389         MOVE ZERO                TO CM-LIVES
           END-IF
02390      MOVE CLAS-STARTA            TO CLAS-INDEXA.                  
02391      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  
02392                                                                   
02393  2830-LF-OB-ADJUST.                                               
02394                                                                   
02395 ******************************************************************
02396 ****  SEARCH BENEFIT TABLE TO DETERMINE BENEFITS PREMIUM TYPE  ***
02397 ******************************************************************
02398                                                                   
02399      IF CM-LF-BENEFIT-CD EQUAL ZERO                               
02400         GO TO 2830-AH-OB-ADJUST.                                  
02401                                                                   
02402      IF CLAS-INDEXL GREATER CLAS-MAXL                             
02403          GO TO 2830-AH-OB-ADJUST.                                 
02404                                                                   
02405      IF CM-LF-BENEFIT-CD EQUAL CLAS-I-BEN (CLAS-INDEXL)           
02406          PERFORM  4100-REVIEW-LIFE     THRU 4100-LF-EXIT          
02407             GO TO 2830-AH-OB-ADJUST.                              
02408                                                                   
02409      ADD +1                      TO CLAS-INDEXL.                  
02410      GO TO 2830-LF-OB-ADJUST.                                     
02411                                                                   
02412  2830-AH-OB-ADJUST.                                               
02413                                                                   
02414 ******************************************************************
02415 ****  SEARCH BENEFIT TABLE TO DETERMINE BENEFITS PREMIUM TYPE  ***
02416 ******************************************************************
02417                                                                   
02418      IF CM-AH-BENEFIT-CD EQUAL ZERO                               
02419         GO TO 2830-OB-ADJUST-END.                                 
02420                                                                   
02421      IF CLAS-INDEXA GREATER CLAS-MAXA                             
02422          GO TO 2830-OB-ADJUST-END.                                
02423                                                                   
02424      IF CM-AH-BENEFIT-CD EQUAL CLAS-I-BEN (CLAS-INDEXA)           
02425          PERFORM 4100-REVIEW-AH  THRU 4100-AH-EXIT                
02426             GO TO 2830-OB-ADJUST-END.                             
02427                                                                   
02428      ADD +1                      TO CLAS-INDEXA.                  
02429      GO TO 2830-AH-OB-ADJUST.                                     
02430                                                                   
02431  2830-OB-ADJUST-END.                                              
02432                                                                   
02433                                                                   
02434  2830-SUM-ADJUST-END.                                             
02435                                                                   
02436      IF CR-AH-LUMP-SUM-DISAB                                      
02437          MOVE CR-DTHAMT         TO CM-AH-ITD-LUMP-PMT             
02438      ELSE                                                         
02439          MOVE ZEROS             TO CM-AH-ITD-LUMP-PMT.            
02440                                                                   
02441      MOVE 'L'                   TO DC-OPTION-CODE.                
02442      MOVE LOW-VALUE             TO CM-AH-PAID-THRU-DT.            
02443      MOVE CR-DIS-PTO-DT         TO DC-GREG-DATE-CYMD.             
02444                                                                   
02445      IF CR-DIS-PTO-DT NOT EQUAL ZERO                              
02446          PERFORM 7100-DATE-RTN THRU 7100-EXIT                     
02447          IF NO-CONVERSION-ERROR                                   
02448             MOVE DC-BIN-DATE-1  TO CM-AH-PAID-THRU-DT.            
02449                                                                   
02450      MOVE LOW-VALUE             TO CM-AH-CANCEL-DT                
02451                                    CM-LF-CANCEL-DT.               
02452                                                                   
02453      MOVE 'L'                   TO DC-OPTION-CODE.                
02454      MOVE CR-LF-CANC-DT         TO DC-GREG-DATE-CYMD.             
02455                                                                   
02456      IF CR-LF-CANC-DT NOT EQUAL ZERO                              
02457          PERFORM 7100-DATE-RTN  THRU   7100-EXIT                  
02458          IF NO-CONVERSION-ERROR                                   
02459             MOVE DC-BIN-DATE-1   TO CM-LF-CANCEL-DT.              
02460                                                                   
02461      MOVE 'L'                    TO DC-OPTION-CODE.               
02462      MOVE CR-AH-CANC-DT          TO DC-GREG-DATE-CYMD.            
02463                                                                   
02464      IF CR-AH-CANC-DT NOT EQUAL ZERO                              
02465          PERFORM 7100-DATE-RTN  THRU   7100-EXIT                  
02466          IF NO-CONVERSION-ERROR                                   
02467             MOVE DC-BIN-DATE-1   TO CM-AH-CANCEL-DT.              
02468                                                                   
02469      MOVE LOW-VALUE              TO CM-AH-SETTLEMENT-DT.          
02470                                                                   
02471      IF CR-AH-LUMP-SUM-DISAB   OR                                 
02472         CR-AH-STATUS-AT-CANCEL EQUAL 6                            
02473           IF CR-DIS-DT NOT EQUAL ZERO                             
02474              MOVE CR-DIS-DT      TO DC-GREG-DATE-CYMD             
02475              MOVE 'L'            TO DC-OPTION-CODE                
02476              PERFORM 7100-DATE-RTN  THRU   7100-EXIT              
02477               IF NO-CONVERSION-ERROR                              
02478                   MOVE DC-BIN-DATE-1 TO CM-AH-SETTLEMENT-DT.      
02479                                                                   
02480      MOVE CR-ENTRY-STATUS        TO CM-ENTRY-STATUS.              
02481                                                                   
012918     if (cr-lf-claim-exit-date = zeros)
012918        and (ws-lf-death-exit-dt <> low-values)
012918        and (ws-lf-death-exit-dt <= ws-current-bin-dt)
012918        move low-values to ws-lf-death-exit-dt
012918     end-if
02482      MOVE CR-LF-CURRENT-STATUS   TO  CM-LF-CURRENT-STATUS.        
02483      MOVE CR-LF-STATUS-AT-DEATH  TO  CM-LF-STATUS-AT-DEATH.       
02484      MOVE CR-LF-STATUS-AT-CANCEL TO  CM-LF-STATUS-AT-CANCEL.      
02485                                                                   
02486      MOVE CR-AH-CURRENT-STATUS   TO CM-AH-CURRENT-STATUS.         
02487      MOVE CR-AH-STATUS-AT-SETTLEMENT                              
02488                                  TO CM-AH-STATUS-AT-SETTLEMENT.   
02489      MOVE CR-AH-STATUS-AT-CANCEL TO CM-AH-STATUS-AT-CANCEL.       
02490                                                                   
02491      MOVE 'L'                    TO DC-OPTION-CODE.               
02492      MOVE LOW-VALUES             TO CM-LF-DEATH-DT.               
02493      MOVE CR-DTH-DT              TO DC-GREG-DATE-CYMD.            
02494      PERFORM 7100-DATE-RTN  THRU   7100-EXIT.                     
02495      IF NO-CONVERSION-ERROR                                       
02496          MOVE DC-BIN-DATE-1      TO CM-LF-DEATH-DT                
02497      ELSE                                                         
02498          MOVE LOW-VALUES         TO CM-LF-DEATH-DT.               
02499                                                                   
02500      IF CR-LF-EXPIRE-DATE NOT NUMERIC                             
02501          MOVE ZERO               TO CR-LF-EXPIRE-DATE.            
02502                                                                   
02503      MOVE 'L'                    TO DC-OPTION-CODE.               
02504      MOVE CR-LF-EXPIRE-DATE      TO DC-GREG-DATE-CYMD.            
02505                                                                   
02506      PERFORM 7100-DATE-RTN  THRU   7100-EXIT.                     
02507      IF NO-CONVERSION-ERROR                                       
02508          MOVE DC-BIN-DATE-1      TO CM-LF-LOAN-EXPIRE-DT          
02509      ELSE                                                         
02510          MOVE LOW-VALUES         TO CM-LF-LOAN-EXPIRE-DT.         
02511                                                                   
02512      IF CR-AH-EXPIRE-DATE NOT NUMERIC                             
02513          MOVE ZERO               TO CR-AH-EXPIRE-DATE.            
02514                                                                   
02515      MOVE 'L'                    TO DC-OPTION-CODE.               
02516      MOVE CR-AH-EXPIRE-DATE      TO DC-GREG-DATE-CYMD             
02517      PERFORM 7100-DATE-RTN  THRU   7100-EXIT.                     
02518      IF NO-CONVERSION-ERROR                                       
02519          MOVE DC-BIN-DATE-1      TO CM-AH-LOAN-EXPIRE-DT          
02520      ELSE                                                         
02521          MOVE LOW-VALUES         TO CM-AH-LOAN-EXPIRE-DT.         
02522                                                                   
02523      IF CR-LOAN-1ST-PMT-DT NOT NUMERIC                            
02524          MOVE ZERO               TO CR-LOAN-1ST-PMT-DT.           
02525                                                                   
02526      MOVE '3'                    TO DC-OPTION-CODE.               
02527      MOVE CR-LOAN-1ST-PMT-DT     TO DC-GREG-DATE-1-YMD.           
02528      PERFORM 7100-DATE-RTN  THRU   7100-EXIT.                     
02529      IF NO-CONVERSION-ERROR                                       
02530          MOVE DC-BIN-DATE-1      TO CM-LOAN-1ST-PMT-DT            
02531      ELSE                                                         
02532          MOVE LOW-VALUES         TO CM-LOAN-1ST-PMT-DT.           
02533                                                                   
02534      MOVE CR-LAST-ADD-ON-DT      TO CM-LAST-ADD-ON-DT.            
02535                                                                   
02536      MOVE 'L'                    TO DC-OPTION-CODE.               
02537      MOVE CR-ENTRY-DATE          TO DC-GREG-DATE-CYMD.            
02538      PERFORM 7100-DATE-RTN  THRU   7100-EXIT.                     
02539      IF NO-CONVERSION-ERROR                                       
02540          MOVE DC-BIN-DATE-1      TO CM-ENTRY-DT                   
02541      ELSE                                                         
02542          MOVE LOW-VALUES         TO CM-ENTRY-DT.                  
02543                                                                   
02544      IF CR-LF-CANCEL-EXIT-DATE NOT EQUAL ZEROS                    
02545          MOVE CR-LF-CANCEL-EXIT-DATE TO DC-GREG-DATE-CYMD         
02546          MOVE  'L'                   TO DC-OPTION-CODE            
02547          PERFORM 7100-DATE-RTN  THRU 7100-EXIT                    
02548          IF NO-CONVERSION-ERROR                                   
02549              MOVE DC-BIN-DATE-1      TO CM-LF-CANCEL-EXIT-DT.     
02550                                                                   
02551      IF CR-AH-CANCEL-EXIT-DATE NOT EQUAL ZEROS                    
02552          MOVE CR-AH-CANCEL-EXIT-DATE TO DC-GREG-DATE-CYMD         
02553          MOVE  'L'                   TO DC-OPTION-CODE            
02554          PERFORM 7100-DATE-RTN  THRU 7100-EXIT                    
02555          IF NO-CONVERSION-ERROR                                   
02556              MOVE DC-BIN-DATE-1      TO CM-AH-CANCEL-EXIT-DT.     
02557                                                                   
02558      IF CR-LF-CLAIM-EXIT-DATE NOT EQUAL ZEROS                     
02559          MOVE CR-LF-CLAIM-EXIT-DATE  TO DC-GREG-DATE-CYMD         
02560          MOVE  'L'                  TO DC-OPTION-CODE             
02561          PERFORM 7100-DATE-RTN  THRU 7100-EXIT                    
02562          IF NO-CONVERSION-ERROR                                   
02563              MOVE DC-BIN-DATE-1     TO CM-LF-DEATH-EXIT-DT.       
02564                                                                   
02565      IF CR-AH-SETTLEMENT-EXIT-DATE NOT EQUAL ZEROS                
02566          MOVE CR-AH-SETTLEMENT-EXIT-DATE TO DC-GREG-DATE-CYMD     
02567          MOVE  'L'                  TO DC-OPTION-CODE             
02568          PERFORM 7100-DATE-RTN  THRU 7100-EXIT                    
02569          IF NO-CONVERSION-ERROR                                   
02570              MOVE DC-BIN-DATE-1     TO CM-AH-SETTLEMENT-EXIT-DT.  
02571                                                                   
02572  2830-EXIT.                                                       
02573       EXIT.                                                       
02574                                                                   
02575      EJECT                                                        
02576  4000-EXPIRE-DATE.                                                
02577                                                                   
02578      IF CM-PREMIUM-TYPE  EQUAL '1'                                
02579          GO TO 4099-EXIT.                                         
02580                                                                   
02581      IF CM-LF-ORIG-TERM  NOT NUMERIC                              
02582          GO TO 4020-CHECK-AH.                                     
02583                                                                   
02584      IF CM-LF-ORIG-TERM  GREATER  +1                              
02585          NEXT SENTENCE                                            
02586      ELSE                                                         
02587          GO TO 4020-CHECK-AH.                                     
02588                                                                   
02589      MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.               
02590      MOVE CM-LF-ORIG-TERM        TO  DC-ELAPSED-MONTHS.           
02591      MOVE ZEROS                  TO  DC-ODD-DAYS-OVER.            
02592      MOVE ZEROS                  TO  DC-ELAPSED-DAYS.             
02593      MOVE '6'                    TO  DC-OPTION-CODE.              
02594                                                                   
02595      PERFORM 7100-DATE-RTN  THRU  7100-EXIT.                      
02596                                                                   
02597      IF NO-CONVERSION-ERROR                                       
02598          MOVE DC-BIN-DATE-2      TO  CM-LF-LOAN-EXPIRE-DT         
02599      ELSE                                                         
02600          MOVE LOW-VALUES         TO  CM-LF-LOAN-EXPIRE-DT.        
02601                                                                   
02602  4020-CHECK-AH.                                                   
02603                                                                   
02604      IF CM-AH-ORIG-TERM  NOT NUMERIC                              
02605          GO TO 4099-EXIT.                                         
02606                                                                   
02607      IF CM-AH-ORIG-TERM  GREATER  +1                              
02608          NEXT SENTENCE                                            
02609      ELSE                                                         
02610          GO TO 4099-EXIT.                                         
02611                                                                   
02612      MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.               
02613      MOVE CM-AH-ORIG-TERM        TO  DC-ELAPSED-MONTHS.           
02614      MOVE ZEROS                  TO  DC-ODD-DAYS-OVER.            
02615      MOVE ZEROS                  TO  DC-ELAPSED-DAYS.             
02616      MOVE '6'                    TO  DC-OPTION-CODE.              
02617                                                                   
02618      PERFORM 7100-DATE-RTN  THRU  7100-EXIT.                      
02619                                                                   
02620      IF NO-CONVERSION-ERROR                                       
02621          MOVE DC-BIN-DATE-2      TO  CM-AH-LOAN-EXPIRE-DT         
02622      ELSE                                                         
02623          MOVE LOW-VALUES         TO  CM-AH-LOAN-EXPIRE-DT.        
02624                                                                   
02625  4099-EXIT.                                                       
02626      EXIT.                                                        
02627 ******************************************************************
02628 ****  SEARCH BENEFIT TABLE TO DETERMINE IF SUMMARY COVERAGE    ***
02629 ******************************************************************
02630                                                                   
02631  4100-REVIEW-LIFE.                                                
02632                                                                   
02633      IF CLAS-I-BAL (CLAS-INDEXL) EQUAL 'B'                        
02634         MOVE '2'                 TO CM-PREMIUM-TYPE               
02635         GO TO 4100-LF-EXIT.                                       
02636                                                                   
02637      IF ((CR-LFTYP NOT EQUAL ZERO) AND                            
02638          (CLAS-I-BAL (CLAS-INDEXL) EQUAL 'Z'))                    
02639          NEXT SENTENCE                                            
02640       ELSE                                                        
02641          GO TO 4100-LF-EXIT.                                      
02642                                                                   
02643      IF CR-LIVES NOT NUMERIC                                      
02644          MOVE ZERO               TO CR-LIVES.                     
02645      IF CR-SUM-CAN-CNT-ITD NOT NUMERIC                            
02646          MOVE ZERO               TO CR-SUM-CAN-CNT-ITD.           
02647                                                                   
02648      COMPUTE CM-LIVES EQUAL                                       
02649            CR-LIVES - CR-SUM-CAN-CNT-ITD.                         
02650                                                                   
02651  4100-LF-EXIT.                                                    
02652  EJECT                                                            
02653  4100-REVIEW-AH.                                                  
02654      IF CLAS-I-BAL (CLAS-INDEXA) EQUAL 'B'                        
02655         MOVE '2'             TO CM-PREMIUM-TYPE                   
02656         GO TO 4100-AH-EXIT.                                       
02657                                                                   
02658      IF ((CR-AHTYP NOT EQUAL ZERO) AND                            
02659          (CLAS-I-BAL (CLAS-INDEXA) EQUAL 'Z'))                    
02660          NEXT SENTENCE                                            
02661       ELSE                                                        
02662          GO TO 4100-AH-EXIT.                                      
02663                                                                   
02664      IF CR-LIVES NOT NUMERIC                                      
02665          MOVE ZERO               TO CR-LIVES.                     
02666      IF CR-SUM-CAN-CNT-ITD NOT NUMERIC                            
02667          MOVE ZERO               TO CR-SUM-CAN-CNT-ITD.           
02668                                                                   
02669      COMPUTE CM-LIVES EQUAL                                       
02670            CR-LIVES - CR-SUM-CAN-CNT-ITD.                         
02671                                                                   
02672  4100-AH-EXIT.                                                    
02673  EJECT                                                            
02674      EJECT                                                        
02675 ****************************************************************  
02676 *  7000-INITIALIZE                                             *  
02677 ****************************************************************  
02678 *                                                              *  
02679 *  1) IF CLASIC CLAIMS SYSTEM PRESENT   OPEN INPUT (ELMSTR5)   *  
02680 *     ALTERNATE PATH (5) OF CLAIM MASTER FILE ACCESSED THRU    *  
02681 *     CL-CONTROL-BY-CERT-NO.                                   *  
02682 *                                                              *  
02683 *  1A) IF CLASIC CLAIMS SYSTEM PRESENT  OPEN INPUT (ELRETR5)   *  
02684 *     ALTERNATE PATH (5) OF RETRIEVE MSTR FILE ACCESSED THRU   *  
02685 *     RL-CONTROL-BY-CERT-NO.                                   *  
02686 *                                                              *  
02687 *  2) OPEN I/O (ELCERTF)   ONLINE CERTIFICATE MASTER FILE      *  
02688 *     ACCESSED THRU CM-CONTROL-PRIMARY AND ISSUE START TO SET  *  
02689 *     VSAM POINTER.                                            *  
02690 *                                                              *  
02691 *  3) OPEN I/O (ERMAIL)   CERTIFICATE MASTER NAME AND ADDRESS  *  
02692 *     FILE ACCESSED THRU MA-CONTROL-PRIMARY.                   *  
02693 *                                                              *  
02694 *  4) OPEN INPUT (ECSCRT01)   BATCH CERTIFICATE MASTER FILE.   *  
02695 *                                                              *  
02696 *  5) OPEN OUTPUT (PRNTR)   PRINT FILE.                        *  
02697 *                                                              *  
02698 *  6) OPEN INPUT (ERACCT)   ACCOUNT MASTER FILE ACCESSED THRU  *  
02699 *     AM-CONTROL-PRIMARY AND ISSUE START TO SET VSAM POINTER.  *  
02700 *                                                              *  
02701 *  7) OPEN I/O (ERCOMM)   CERTIFICATE MASTER COMMISSION STRUC- *  
02702 *     TURE ACCESSED THRU CE-CONTROL-PRIMARY. THE COMMISSION    *  
02703 *     STRUCTURE OF THE CERTIFICATE VARIES FROM THE COMMISSION  *  
02704 *     STRUCTURE FOR THE ASSOCIATED ACCOUNT MASTER.             *  
02705 *                                                              *  
02706 ****************************************************************  
02707                                                                   
02708  7000-INITIALIZE.                                                 
02709                                                                   
02710      IF DTE-SYS-E-CLASIC-CLAIMS EQUAL 'Y'                         
02711          OPEN INPUT  ELMSTR5                                      
02712          IF (CL-STAT-1  NOT EQUAL '0') AND                        
02713             (CL-STATUS NOT EQUAL '97')                            
02714              MOVE 'ERROR OCCURED OPEN - ELMSTR'                   
02715                                  TO  WS-ABEND-MESSAGE             
02716              MOVE CL-STATUS      TO  WS-ABEND-FILE-STATUS         
02717              GO TO ABEND-PGM                                      
02718          END-IF                                                   
02719          OPEN INPUT  ELRETR5                                      
02720          IF ELRETR5-FILE-STATUS NOT = '00' AND '97'               
02721              MOVE 'ERROR OCCURRED OPEN - ELRETR'                  
02722                                       TO WS-ABEND-MESSAGE         
02723              MOVE ELRETR5-FILE-STATUS TO WS-ABEND-FILE-STATUS     
02724              GO TO ABEND-PGM.                                     
02725                                                                   
02726      OPEN I-O    ELCERTF.                                         
02727                                                                   
02728      IF (CM-STAT-1  NOT EQUAL '0') AND                            
02729         (CM-STATUS NOT EQUAL '97')                                
02730          MOVE 'ERROR OCCURED OPEN - ELCERT'                       
02731                                 TO  WS-ABEND-MESSAGE              
02732          MOVE CM-STATUS         TO  WS-ABEND-FILE-STATUS          
02733          GO TO ABEND-PGM.                                         
02734                                                                   
02735      MOVE LOW-VALUE             TO CM-CONTROL-PRIMARY.            
02736      MOVE DTE-CLASIC-COMPANY-CD TO CM-COMPANY-CD.                 
02737                                                                   
02738      START ELCERTF                                                
02739             KEY NOT LESS CM-CONTROL-PRIMARY.                      
02740                                                                   
02741      IF CM-STAT-1  NOT EQUAL '0'                                  
02742          MOVE 'ERROR OCCURED START - ELCERT'                      
02743                                  TO  WS-ABEND-MESSAGE             
02744          MOVE CM-STATUS      TO  WS-ABEND-FILE-STATUS             
02745          GO TO ABEND-PGM.                                         
02746                                                                   
02747      OPEN I-O ERMAIL.                                             
02748                                                                   
02749      IF (CFS-STAT-1  NOT EQUAL '0') AND                           
02750         (ERMAIL-FILE-STATUS NOT EQUAL '97')                       
02751          MOVE 'ERROR OCCURED OPEN - ERMAIL'                       
02752                                   TO  WS-ABEND-MESSAGE            
02753          MOVE ERMAIL-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        
02754          GO TO ABEND-PGM.                                         
02755                                                                   
02756      OPEN INPUT ECSCRT01                                          
02757                 ERCTBL.                                           
02758                                                                   
02759      IF ERCTBL-FILE-STATUS NOT = '00' AND '97'                    
02760          MOVE 'ERROR OCCURED OPEN - ERCTBL'                       
02761                                   TO  WS-ABEND-MESSAGE            
02762          MOVE ERCTBL-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        
02763          GO TO ABEND-PGM.                                         
02764                                                                   
CIDMOD     OPEN OUTPUT PRNTR                                            
062104                 ME50-EL331-BALANCE
CIDMOD                 DISPLAY-PRT.
02766                                                                   
02767      MOVE 1  TO WS-PAGE-CNT.                                      
02768                                                                   
CIDMOD     MOVE WS-CURRENT-DATE TO DIS-DATE.                            
CIDMOD                                                                  
CIDMOD     PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT.           
02672                                                                   
02769      MOVE LOW-VALUES TO WS-SAVED-ECS-KEY                          
02770                         WS-SAVED-ONL-KEY                          
02771                         WS-CLAIM-MATCH-KEY                        
02772                         WS-SEC-CLAIM-INDEX.                       
02773                                                                   
02774      MOVE WS-CURRENT-DATE TO HD-RUN-DATE.                         
02775                                                                   
02776      MOVE WS-CD-MM        TO WS-CURR-MO.                          
02777      MOVE WS-CD-DD        TO WS-CURR-DA.                          
02778      MOVE WS-CD-YY        TO WS-CURR-YR.                          
02779                                                                   
02780      COMPUTE WS-CURRENT-MOS EQUAL (RUN-CCYY * 12) + RUN-MO.       
02781                                                                   
02782      MOVE ALPH-DATE     TO HD-DATE-FULL.                          
02783      MOVE COMPANY-NAME  TO HD-COMP-NM.                            
02784                                                                   
02785      PERFORM 7500-HEADING-RTN  THRU 7500-EXIT.                    
02786                                                                   
02787      IF DTE-SYS-D-DEMAND-BILL EQUAL '1' OR                        
02788         DTE-SYS-G-AR-USED     EQUAL '1'                           
02789           NEXT SENTENCE                                           
02790      ELSE                                                         
02791          GO TO 7000-EXIT.                                         
02792                                                                   
02793      OPEN INPUT ERACCT.                                           
02794                                                                   
02795      IF (AM-STAT-1  NOT EQUAL '0') AND                            
02796         (AM-STATUSF NOT EQUAL '97')                               
02797          MOVE 'ERROR OCCURED OPEN - ERACCT'                       
02798                              TO  WS-ABEND-MESSAGE                 
02799          MOVE AM-STATUSF     TO  WS-ABEND-FILE-STATUS             
02800          GO TO ABEND-PGM.                                         
02801                                                                   
02802      MOVE LOW-VALUES            TO AM-CONTROL-PRIMARY.            
02803      MOVE DTE-CLASIC-COMPANY-CD TO AM-COMPANY-CD.                 
02804      START ERACCT                                                 
02805             KEY NOT LESS AM-CONTROL-PRIMARY.                      
02806                                                                   
02807      IF AM-STAT-1  NOT EQUAL '0'                                  
02808          MOVE 'ERROR OCCURED START - ERACCT'                      
02809                                  TO  WS-ABEND-MESSAGE             
02810          MOVE AM-STATUSF     TO  WS-ABEND-FILE-STATUS             
02811          GO TO ABEND-PGM.                                         
02812                                                                   
02813      OPEN I-O ERCOMM.                                             
02814                                                                   
02815      IF (CE-STAT-1  NOT EQUAL '0') AND                            
02816         (CE-STATUS NOT EQUAL '97')                                
02817          MOVE 'ERROR OCCURED OPEN - ERCOMM'                       
02818                                  TO  WS-ABEND-MESSAGE             
02819          MOVE CE-STATUS          TO  WS-ABEND-FILE-STATUS         
02820          GO TO ABEND-PGM.                                         
02821                                                                   
02822  7000-EXIT.                                                       
02823       EXIT.                                                       
02824                                                                   
02825  7100-DATE-RTN.                                                   
02826      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  
02827                                                                   
02828      IF DC-ERROR-CODE NOT EQUAL SPACE                             
02829          MOVE  ZEROS             TO DC-CONVERSION-DATES.          
02830                                                                   
02831  7100-EXIT.                                                       
02832       EXIT.                                                       
02833      EJECT                                                        
02834  7300-BLD-ECS-PRT-LINE.                                           
02835      MOVE CR-CERT-NO             TO DTL-CERT-NO.                  
02836      MOVE CR-YR                  TO DTL-EFF-YR.                   
02837      MOVE CR-MO                  TO DTL-EFF-MO.                   
02838      MOVE CR-DA                  TO DTL-EFF-DA.                   
02839      MOVE CR-CARRIER             TO DTL-CARR.                     
02840      MOVE CR-GROUPING            TO DTL-GRPG.                     
02841      MOVE CR-STATE               TO DTL-STAT.                     
02842      MOVE CR-ACCOUNT             TO DTL-ACCT.                     
02843      MOVE 'X'                    TO WS-PRT-FULL-LINE-SW.          
02844                                                                   
02845  7300-EXIT.                                                       
02846       EXIT.                                                       
02847                                                                   
02848  7320-BLD-ONL-PRT-LINE.                                           
02849      MOVE CM-CERT-NO             TO DTL-CERT-NO.                  
02850                                                                   
02851      MOVE ' '                    TO DC-OPTION-CODE.               
02852      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.                
02853      PERFORM 7100-DATE-RTN  THRU   7100-EXIT.                     
02854      MOVE DC-GREG-DATE-1-EDIT    TO DTL-EFFECTIVE-DT.             
02855      MOVE CM-CARRIER             TO DTL-CARR.                     
02856      MOVE CM-GROUPING            TO DTL-GRPG.                     
02857      MOVE CM-STATE               TO DTL-STAT.                     
02858      MOVE CM-ACCOUNT             TO DTL-ACCT.                     
02859                                                                   
02860  7320-EXIT.                                                       
02861       EXIT.                                                       
02862                                                                   
02863  7350-BLD-CLM-INFO.                                               
02864      IF NO-ASSOCIATED-CLAIM                                       
02865         MOVE SPACES              TO  DTL-CL-NO                    
02866                                      DTL-CL-TYPE                  
02867                                      DTL-CL-STATUS                
02868                                      DTL-CL-LN-KIND               
02869                                      DTL-CL-INCURRED              
02870                                      DTL-CL-PROCESSOR             
02871         GO TO 7350-EXIT.                                          
02872                                                                   
02873      MOVE SPACES                 TO  DTL-CL-TYPE                  
02874                                      DTL-CL-STATUS                
02875                                      DTL-CL-LN-KIND.              
02876                                                                   
02877      MOVE CL-CLAIM-NO            TO DTL-CL-NO.                    
02878                                                                   
011215     evaluate true
011215        when cl-claim-type = life-override-l1
011215           move life-override-l2 to dtl-cl-type
011215        when cl-claim-type = 'F'
011215           move 'FL'             to dtl-cl-type
011215        when cl-claim-type = 'I'
011215           move 'IU'             to dtl-cl-type
011215        when cl-claim-type = 'G'
011215           MOVE 'GP'             to dtl-cl-type
022122        when cl-claim-type = 'B'
022122           move 'BR'             to dtl-cl-type
022122        when cl-claim-type = 'H'
022122           move 'HS'             to dtl-cl-type
100518        when cl-claim-type = 'O'
100518           MOVE 'OT'             to dtl-cl-type
011215        when other
011215           move ah-override-l2   to dtl-cl-type
011215     end-evaluate
011215
02885      IF CLAIM-IS-OPEN                                             
02886         MOVE ' OPEN '            TO DTL-CL-STATUS                 
02887       ELSE                                                        
02888         IF CLAIM-IS-CLOSED                                        
02889             MOVE 'CLOSED'        TO DTL-CL-STATUS.                
02890                                                                   
02891      IF SINGLE-PREMIUM                                            
02892         MOVE 'SNG PRM'           TO DTL-CL-LN-KIND                
02893        ELSE                                                       
02894         IF O-B-COVERAGE                                           
02895             MOVE '  O.B. '       TO DTL-CL-LN-KIND                
02896          ELSE                                                     
02897           IF OPEN-END-COVERAGE                                    
02898               MOVE 'OPN END'     TO DTL-CL-LN-KIND.               
02899                                                                   
02900      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.                
02901      MOVE ' '                    TO DC-OPTION-CODE.               
02902      PERFORM 7100-DATE-RTN   THRU 7100-EXIT.                      
02903      MOVE DC-GREG-DATE-1-EDIT    TO DTL-CL-INCURRED.              
02904      MOVE CL-PROCESSOR-ID        TO DTL-CL-PROCESSOR.             
02905                                                                   
02906  7350-EXIT.                                                       
02907       EXIT.                                                       
02908                                                                   
02909  7370-PRINT-DETAIL.                                               
02910      MOVE DTL-1                  TO P-DATA.                       
02911      MOVE '0'                    TO X.                            
02912      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
02913      ADD  2                      TO WS-LINE-CNT.                  
02914                                                                   
02915      IF WS-LINE-CNT GREATER WS-MAX-LINES                          
02916           PERFORM 7500-HEADING-RTN  THRU 7500-EXIT.               
02917                                                                   
02918      MOVE SPACES                 TO DTL-1.                        
02919                                                                   
02920  7370-EXIT.                                                       
02921       EXIT.                                                       
02922                                                                   
02923  7500-HEADING-RTN.                                                
02924      MOVE  WS-PAGE-CNT           TO HD-PAGE-NO.                   
02925      MOVE '1'                    TO X.                            
02926      MOVE HEADING-1              TO P-DATA.                       
02927      PERFORM 7600-PRT-RTN   THRU 7600-EXIT.                       
02928                                                                   
02929      MOVE SPACES                 TO X.                            
02930      MOVE HEADING-2              TO P-DATA.                       
02931      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
02932                                                                   
02933      MOVE SPACES                 TO X.                            
02934      MOVE HEADING-3              TO P-DATA.                       
02935      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
02936                                                                   
02937      MOVE SPACES                 TO X.                            
02938      MOVE HEADING-4              TO P-DATA.                       
02939      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
02940                                                                   
02941      MOVE SPACES                 TO  X.                           
02942      MOVE HEADING-5              TO P-DATA.                       
02943      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
02944                                                                   
02945      MOVE SPACES                 TO  X.                           
02946      MOVE HEADING-6              TO P-DATA.                       
02947      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
02948                                                                   
02949      MOVE 16                     TO WS-LINE-CNT.                  
02950      ADD 1 TO WS-PAGE-CNT.                                        
02951                                                                   
02952  7500-EXIT.                                                       
02953       EXIT.                                                       
02954                                                                   
02955  7600-PRT-RTN.                   COPY ELCPRT2X.                   
02956  7600-PRT-CLEAR.                                                  
02957      MOVE SPACES                 TO P-DATA.                       
02958      MOVE SPACES                 TO DTL-1.                        
02959                                                                   
02960  7600-EXIT.                                                       
02961       EXIT.                                                       
02962                                                                   
02963  8000-FINALIZE.                                                   
CIDMOD
CIDMOD     DISPLAY ' '.                                                 
CIDMOD     DISPLAY '* * * * * * * * * * * * * * * * * *'.               
CIDMOD     DISPLAY ' DISPLAY ERROR COUNT = '  ERROR-COUNT.              
CIDMOD     DISPLAY '* * * * * * * * * * * * * * * * * *'.               
CIDMOD     DISPLAY ' '.                                                 
CIDMOD                                                                  
CIDMOD     PERFORM 8600-DISPLAY-HD  THRU                                
CIDMOD               8600-HD-EXIT.                                      
CIDMOD                                                                  
CIDMOD     MOVE ' DISPLAY ERROR COUNT = '    TO DIS-LINE-REASON         
CIDMOD     MOVE ERROR-COUNT                TO  DIS-LINE-REC             
CIDMOD     PERFORM 8600-DISPLAY-PRT THRU                                
CIDMOD               8600-DISPLAY-EXIT.                                 
CIDMOD                                                                  
CIDMOD     CLOSE  DISPLAY-PRT.                                          
02886                                                                   
CIDMOD
02964      MOVE ALL '*'                TO  P-DATA.                      
02965      PERFORM  7600-PRT-RTN   THRU 7600-EXIT.                      
02966                                                                   
02967      MOVE WS-MATCHED-ONL         TO  P-MATCHED-ONL.               
02968      MOVE WS-LOADED-ECS          TO  P-LOADED-ECS.                
02969      MOVE WS-ONL-DELETED         TO  P-ONL-DELETED.               
02970      MOVE WS-CLAIM-MATCH         TO  P-CLAIM-MATCH.               
02971      MOVE WS-INACTIVE-CERTS      TO  P-INACTIVE-CERTS.            
02972      MOVE WS-ECS-READ            TO  P-ECS-READ.                  

062104     MOVE WS-ECS-READ            TO  WS-ME50-BAL-AMT-LOW.
062104     MOVE WS-ECS-READ            TO  WS-ME50-BAL-AMT-HIGH.
062104     MOVE WS-BAL50-DESCRIPTION   TO WS-ME50-BAL-DESCRIP.
062104     WRITE ME50-EL331-BALANCE-REC FROM WS-ME50-BALANCE-REC.

02973      MOVE WS-ONL-READ            TO  P-ONL-READ.                  
02974      MOVE WS-EXCP-ADD            TO  P-EXCP-ADD.                  
02975      MOVE WS-EXCP-DEL            TO  P-EXCP-DEL.                  
02976      MOVE WS-EXCP-TBL            TO  P-EXCP-TBL.                  
02977      MOVE WS-MAIL-DEL            TO  P-MAIL-DEL.                  
02978      MOVE WS-ACCT-READS          TO  P-ACCT-READS.                
02979      MOVE WS-ACT-READ            TO  P-ACT-READ.                  
02980      MOVE WS-ACCT-MATCH          TO  P-ACCT-MATCH.                
02981                                                                   
02982      COMPUTE WS-ONL-EXIST EQUAL                                   
02983              WS-ONL-READ - WS-ONL-DELETED + WS-LOADED-ECS.        
02984                                                                   
02985      MOVE  WS-ONL-EXIST          TO  P-ONL-EXIST.                 
02986                                                                   
02987      MOVE '0'                    TO X.                            
02988      MOVE FTG-1                  TO P-DATA.                       
02989      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
02990                                                                   
02991      MOVE '0'                    TO X.                            
02992      MOVE FTG-2                  TO P-DATA.                       
02993      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
02994                                                                   
02995      MOVE '0'                    TO X.                            
02996      MOVE FTG-3                  TO P-DATA.                       
02997      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
02998                                                                   
02999      MOVE '0'                    TO X.                            
03000      MOVE FTG-4                  TO P-DATA.                       
03001      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
03002                                                                   
03003      MOVE '0'                    TO X.                            
03004      MOVE FTG-5                  TO P-DATA.                       
03005      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
03006                                                                   
03007      MOVE '0'                    TO X.                            
03008      MOVE FTG-6                  TO P-DATA.                       
03009      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
03010                                                                   
03011      MOVE '0'                     TO X.                           
03012      MOVE FTG-7                   TO P-DATA.                      
03013      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
03014                                                                   
03015      MOVE '0'                    TO X.                            
03016      MOVE FTG-8                  TO P-DATA.                       
03017      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
03018                                                                   
03019      MOVE '0'                    TO X.                            
03020      MOVE FTG-9                  TO P-DATA.                       
03021      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
03022                                                                   
03023      MOVE '0'                    TO X.                            
03024      MOVE FTG-10                 TO P-DATA.                       
03025      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
03026                                                                   
03027      MOVE '0'                    TO X.                            
03028      MOVE FTG-11                 TO P-DATA.                       
03029      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
03030                                                                   
03031      MOVE '0'                    TO X.                            
03032      MOVE FTG-12                 TO P-DATA.                       
03033      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
03034                                                                   
03035      MOVE '0'                    TO X.                            
03036      MOVE FTG-13                 TO P-DATA.                       
03037      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
03038                                                                   
03039      MOVE '0'                    TO X.                            
03040      MOVE FTG-14                 TO P-DATA.                       
03041      PERFORM 7600-PRT-RTN  THRU 7600-EXIT.                        
03042                                                                   
03043      IF DTE-SYS-E-CLASIC-CLAIMS EQUAL 'Y'                         
03044          CLOSE ELMSTR5                                            
03045          IF CL-STAT-1 NOT EQUAL ZERO                              
03046              MOVE 'ERROR OCCURED OPEN - ELMSTR'                   
03047                                  TO  WS-ABEND-MESSAGE             
03048              MOVE CL-STATUS      TO  WS-ABEND-FILE-STATUS         
03049              GO TO ABEND-PGM                                      
03050          END-IF                                                   
03051          CLOSE ELRETR5                                            
03052          IF ELRETR5-FILE-STATUS NOT = ZERO                        
03053              MOVE 'ERROR OCCURRED CLOSE - ELRETR'                 
03054                                  TO  WS-ABEND-MESSAGE             
03055              MOVE ELRETR5-FILE-STATUS                             
03056                                  TO  WS-ABEND-FILE-STATUS         
03057              GO TO ABEND-PGM.                                     
03058                                                                   
03059      CLOSE PRNTR                                                  
062104           ME50-EL331-BALANCE
03060            ECSCRT01                                               
03061            ELCERTF                                                
03062            ERCTBL.                                                
03063                                                                   
03064      IF CM-STAT-1  NOT EQUAL ZERO                                 
03065          MOVE 'ERROR OCCURED CLOSE - ELCERT'                      
03066                                  TO  WS-ABEND-MESSAGE             
03067          MOVE CM-STATUS          TO  WS-ABEND-FILE-STATUS         
03068          GO TO ABEND-PGM.                                         
03069                                                                   
03070      CLOSE ERMAIL.                                                
03071                                                                   
03072      IF CFS-STAT-1 NOT EQUAL '0'                                  
03073          MOVE 'ERROR OCCURED CLOSE - ERMAIL'                      
03074                                   TO  WS-ABEND-MESSAGE            
03075          MOVE ERMAIL-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        
03076          GO TO ABEND-PGM.                                         
03077                                                                   
03078      IF DTE-SYS-D-DEMAND-BILL EQUAL '1' OR                        
03079         DTE-SYS-G-AR-USED     EQUAL '1'                           
03080           NEXT SENTENCE                                           
03081      ELSE                                                         
03082         GO TO 8000-CLOSE-OTHER.                                   
03083                                                                   
03084      CLOSE ERACCT.                                                
03085                                                                   
03086      IF AM-STAT-1  NOT EQUAL ZERO                                 
03087          MOVE 'ERROR OCCURED CLOSE - ERACCT'                      
03088                              TO  WS-ABEND-MESSAGE                 
03089          MOVE AM-STATUSF     TO  WS-ABEND-FILE-STATUS             
03090          GO TO ABEND-PGM.                                         
03091                                                                   
03092      CLOSE ERCOMM.                                                
03093                                                                   
03094      IF CE-STAT-1  NOT EQUAL ZERO                                 
03095          MOVE 'ERROR OCCURED CLOSE - ERCOMM'                      
03096                              TO  WS-ABEND-MESSAGE                 
03097          MOVE CE-STATUS      TO  WS-ABEND-FILE-STATUS             
03098          GO TO ABEND-PGM.                                         
03099                                                                   
03100  8000-CLOSE-OTHER.        COPY ELCPRTCX.                          
03101                                                                   
03102  8000-EXIT.                                                       
03103       EXIT.                                                       
03104                                                                   
CIDMOD 8600-DIS-PRINT SECTION.                                          
CIDMOD                                                                  
CIDMOD 8600-DISPLAY-PRT.                                                
CIDMOD                                                                  
CIDMOD     IF  DIS-HEAD-SW =  'Y'                                       
CIDMOD       MOVE 'N' TO  DIS-HEAD-SW                                   
CIDMOD         PERFORM 8600-DISPLAY-HD THRU                             
CIDMOD             8600-HD-EXIT                                         
CIDMOD           GO TO 8600-DISPLAY-EXIT.                               
CIDMOD                                                                  
CIDMOD     IF  DIS-LINE-CNT GREATER THAN 59                             
CIDMOD         PERFORM 8600-DISPLAY-HD THRU                             
CIDMOD             8600-HD-EXIT.                                        
CIDMOD                                                                  
CIDMOD     MOVE   SPACES TO DIS-CC.                                     
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     WRITE  DISPLAY-REC FROM DISPLAY-LINE.                        
CIDMOD     MOVE   SPACES TO DISPLAY-REC.                                
CIDMOD     MOVE   SPACES TO DISPLAY-LINE.                               
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD 8600-DISPLAY-EXIT.                                               
CIDMOD     EXIT.                                                        
CIDMOD                                                                  
CIDMOD 8600-DIS-HEAD SECTION.                                           
CIDMOD                                                                  
CIDMOD 8600-DISPLAY-HD.                                                 
CIDMOD                                                                  
CIDMOD     MOVE '1' TO  DISPLAY-CC.                                     
CIDMOD     MOVE ZEROS TO DIS-LINE-CNT.                                  
CIDMOD     WRITE DISPLAY-REC FROM DISPLAY-HD-1.                         
CIDMOD     ADD  +1  TO DIS-LINE-CNT.                                    
CIDMOD     MOVE ' ' TO  DISPLAY-CC.                                     
CIDMOD     MOVE   SPACES TO DISPLAY-REC.                                
CIDMOD*    WRITE  DISPLAY-REC.                                          
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     WRITE  DISPLAY-REC FROM DISPLAY-HD-2.                        
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     MOVE   SPACES TO DISPLAY-REC.                                
CIDMOD     WRITE  DISPLAY-REC.                                          
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD 8600-HD-EXIT.                                                    
CIDMOD     EXIT.                                                        
03105  9999-PROGRAM-END.                                                
03106                                                                   
03107      GOBACK.                                                      
03108                                                                   
03109      EJECT                                                        
03110  ABEND-PGM SECTION. COPY ELCABEND.                                
