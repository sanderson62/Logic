00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL6301.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 04/21/94 14:29:04.                 
00007 *                            VMOD=2.075                           
00008 *                                                                 
00009 *AUTHOR.     LOGIC,INC.                                           
00010 *            DALLAS, TEXAS.                                       
00011                                                                   
00012 *DATE-COMPILED.                                                   
00013                                                                   
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023                                                                   
00024 *REMARKS.                                                         
00025 *         TRANSACTION - EXA6 - NEW BUSINESS - DATA ENTRY (ISSUES).
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
100803* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
081606* 081606  CR2006080800002  PEMA  ADD VIN NUMBER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
072308* 072308  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
030309* 030309  CR2009021700001  PEMA  ADD EDIT FOR BENE AND INS ADDR
020210* 020210  IR2010011100002  PEMA  CORRECT ATTRB ON BCZIPCD
030310* 030310  IR2010022400001  PEMA  CORRECT PF5 LOGIC, ADD APR PROC
030310* 030310  CR2009031200002  PEMA  CHECK LOAN OFF EDIT SWITCH
060211* 060211  CR2011051600002  PEMA  OPEN CP FIELD
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
050713* 050713  CR2008042200001  PEMA  ADD ZERO PCT APR PROCESSING
111913* 111913  CR2008042200001  PEMA  ADDITIONAL 0 % APR CHANGES
020514* 020514  IR2014012400001  PEMA  DARK OUT AH ALT BEN FOR CID&AHL
042114* 042114  CR2014032000001  PEMA  rearrange dob,jntdob&ssn
071714* 071714    2013100100002  PEMA  FIX CRIT PERIOD EDITS
101615* 101615  CR2015080300002  PEMA  ALLOW VIN FOR CID
062017* 062017  CR2015091000001  PEMA  ADD PROCESSING FOR TN REF INTEREST
041320* 041320  CR2020040200001  PEMA  PENDING BUSINESS JOURNALING
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
070622* 070622  CR2020061200002  TANA  Add cancel reason logic
122002******************************************************************

00027  ENVIRONMENT DIVISION.                                            
00028  DATA DIVISION.                                                   
00029  WORKING-STORAGE SECTION.                                         
00030  77  FILLER  PIC X(32)  VALUE '********************************'. 
00031  77  FILLER  PIC X(32)  VALUE '*    EL6301 WORKING STORAGE    *'. 
00032  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.075 *********'. 
030310 77  WS-BEG                      PIC S999 COMP-3 VALUE +0.
101110 77  A1                          PIC S999 COMP-3 VALUE +0.
030310 77  WS-END                      PIC S999 COMP-3 VALUE +0.
00033                                                                   
00034      COPY ELCSCTM.                                                
00035                                                                   
00036      COPY ELCSCRTY.                                               
00038                                                                   
       01  WS-NUM-TABLE                PIC X(26)  VALUE
           '12345678012345070923456789'.
       01  FILLER REDEFINES WS-NUM-TABLE.
           05  WS-NUM OCCURS 26        PIC 9.
       01  V1                          PIC S999 COMP-3.
       01  V2                          PIC S999 COMP-3.
       01  V3                          PIC S999 COMP-3.
       01  WS-WORK-VIN                 PIC X(17)  VALUE SPACES.
       01  FILLER REDEFINES WS-WORK-VIN.
           05  WS-WORK-VIN-N OCCURS 17        PIC 9.
       01  WS-VIN-TOTAL                PIC S9(9)  VALUE +0.
       01  WS-VIN-FINAL                PIC S9(7)  VALUE +0.
       01  WS-VIN-REMAINDER            PIC S999   VALUE +0.
       01  WS-HEX-WORK.
           05  FILLER                  PIC X  VALUE LOW-VALUES.
           05  WS-HEX-BYTE             PIC X.
       01  WS-CHARCD REDEFINES WS-HEX-WORK PIC S9(4)  COMP.
NTTDel*EXEC SQL
NTTDel*   INCLUDE SQLDA
NTTDel*END-EXEC
070622
070622 EXEC SQL
070622    INCLUDE SQLCA
070622 END-EXEC
070622
070622 EXEC SQL
070622    BEGIN DECLARE SECTION
070622 END-EXEC
070622
070622 01  SQLCMD                      PIC X(1024).
070622 01  SVR                         PIC X(32).
070622 01  USR                         PIC X(32).
070622 01  PASS                        PIC X(32).
070622 01  USR-PASS                    PIC X(64).
       01  WS-CHARCD-A                 PIC S9(4)   COMP VALUE +65.
070622
070622 01  WS-SQL-DATA.
070622     05  WS-CYCLE-DATE           PIC X(10).
070622     05  WS-NEXT-BUS-DT          PIC X(10).
070622     05  WS-LOOKUPID             PIC X(4).
070622     05  WS-LOOKUPNAME           PIC X(4).
070622     05  WS-LOOKUP-VALUE         PIC X(100).
070622     05  WS-CARRIER              PIC X.
070622     05  WS-GROUP                PIC X(6).
070622     05  WS-STATE                PIC XX.
070622     05  WS-ACCOUNT              PIC X(10).
070622     05  WS-EFF-DT               PIC XX.
070622     05  WS-CERT-NO              PIC X(10).
070622     05  WS-CERT-NO-SUF          PIC X(01).
070622
070622 EXEC SQL
070622    END DECLARE SECTION
070622 END-EXEC

070622 01  P pointer.
070622 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
070622 01  KIXHOST                     pic x(9) value Z"HOSTNAME".
070622 01  var-ptr pointer.
070622 01  env-var-len                 pic 9(4)  binary.
070622 01  rc                          pic 9(9)  binary.
070622
070622 01  WS-KIXSYS.
070622     05  WS-KIX-FIL1             PIC X(10).
070622     05  WS-KIX-APPS             PIC X(10).
070622     05  WS-KIX-ENV              PIC X(10).
070622     05  WS-KIX-MYENV            PIC X(10).
070622     05  WS-KIX-SYS              PIC X(10).
070622 01  WS-KIXHOST                  PIC X(10).

00039  01  WS-COMM-LENGTH          PIC S9(4) COMP VALUE +1900.          
00040                                                                   
00041  01  STANDARD-AREAS.                                              
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
00042      12  GETMAIN-SPACE       PIC X       VALUE SPACE.             
00043      12  EL630B              PIC X(8)    VALUE 'EL630B'.          
00044      12  EL630C              PIC X(8)    VALUE 'EL630C'.          
00045      12  MAPSET-EL6301S      PIC X(8)    VALUE 'EL6301S'.         
00046      12  TRANS-EXA6          PIC X(4)    VALUE 'EXA6'.            
00047      12  THIS-PGM            PIC X(8)    VALUE 'EL6301'.          
00048      12  PGM-NAME            PIC X(8).                            
00049      12  TIME-IN             PIC S9(7).                           
00050      12  TIME-OUT-R  REDEFINES TIME-IN.                           
00051          16  FILLER          PIC X.                               
00052          16  TIME-OUT        PIC 99V99.                           
00053          16  FILLER          PIC XX.                              
00054      12  LINK-EL001          PIC X(8)    VALUE 'EL001'.           
00055      12  LINK-EL004          PIC X(8)    VALUE 'EL004'.           
00056      12  XCTL-EL005          PIC X(8)    VALUE 'EL005'.           
00057      12  XCTL-EL010          PIC X(8)    VALUE 'EL010'.           
00058      12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.           
00059      12  XCTL-EL630          PIC X(8)    VALUE 'EL630'.           
00060      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         
00061      12  FILE-ID-ERPNDB      PIC X(8)    VALUE 'ERPNDB'.          
00062      12  FILE-ID-ERPNDM      PIC X(8)    VALUE 'ERPNDM'.          
00063      12  FILE-ID-ELCERT      PIC X(8)    VALUE 'ELCERT'.          
00064      12  FILE-ID-ELCNTL      PIC X(8)    VALUE 'ELCNTL'.          
00065      12  WS-CURRENT-DT       PIC X(8)    VALUE SPACES.            
00066      12  WS-CURRENT-BIN-DT   PIC XX      VALUE SPACES.            
00067      12  WS-TERM-IN-DAYS-SW  PIC X.                               
00068          88  WS-TERM-IN-DAYS-FOUND       VALUE 'Y'.               
00069                                                                   
00070      EJECT                                                        
00071  01  ERROR-MESSAGES.                                              
00072      12  ER-0000                 PIC X(4)  VALUE '0000'.          
00073      12  ER-0004                 PIC X(4)  VALUE '0004'.          
00074      12  ER-0008                 PIC X(4)  VALUE '0008'.          
00075      12  ER-0029                 PIC X(4)  VALUE '0029'.          
00076      12  ER-0070                 PIC X(4)  VALUE '0070'.          
00077      12  ER-0582                 PIC X(4)  VALUE '0582'.          
00078      12  ER-1923                 PIC X(4)  VALUE '1923'.          
           12  ER-2049                 PIC X(4)  VALUE '2049'.
00079      12  ER-2119                 PIC X(4)  VALUE '2119'.          
00080      12  ER-2212                 PIC X(4)  VALUE '2212'.          
00081      12  ER-2217                 PIC X(4)  VALUE '2217'.          
00082      12  ER-2218                 PIC X(4)  VALUE '2218'.          
00083      12  ER-2200                 PIC X(4)  VALUE '2200'.          
           12  ER-2209                 PIC X(4)  VALUE '2209'.
00084      12  ER-2220                 PIC X(4)  VALUE '2220'.          
00085      12  ER-2222                 PIC X(4)  VALUE '2222'.          
00086      12  ER-2223                 PIC X(4)  VALUE '2223'.          
00087      12  ER-2224                 PIC X(4)  VALUE '2224'.          
00088      12  ER-2226                 PIC X(4)  VALUE '2226'.          
00089      12  ER-2227                 PIC X(4)  VALUE '2227'.          
00090      12  ER-2228                 PIC X(4)  VALUE '2228'.          
00091      12  ER-2240                 PIC X(4)  VALUE '2240'.          
00092      12  ER-2241                 PIC X(4)  VALUE '2241'.          
00093      12  ER-2247                 PIC X(4)  VALUE '2247'.          
00094      12  ER-2423                 PIC X(4)  VALUE '2423'.          
00095      12  ER-2424                 PIC X(4)  VALUE '2424'.          
00096      12  ER-2425                 PIC X(4)  VALUE '2425'.          
00097      12  ER-2426                 PIC X(4)  VALUE '2426'.          
00098      12  ER-2427                 PIC X(4)  VALUE '2427'.          
00099      12  ER-2428                 PIC X(4)  VALUE '2428'.          
00100      12  ER-2431                 PIC X(4)  VALUE '2431'.          
00101      12  ER-2433                 PIC X(4)  VALUE '2433'.          
00102      12  ER-2437                 PIC X(4)  VALUE '2437'.          
00103      12  ER-2429                 PIC X(4)  VALUE '2429'.          
00104      12  ER-2442                 PIC X(4)  VALUE '2442'.          
00105      12  ER-2471                 PIC X(4)  VALUE '2471'.          
00106      12  ER-2526                 PIC X(4)  VALUE '2526'.          
00107      12  ER-2529                 PIC X(4)  VALUE '2529'.          
00108      12  ER-2531                 PIC X(4)  VALUE '2531'.          
00109      12  ER-2532                 PIC X(4)  VALUE '2532'.          
00110      12  ER-2541                 PIC X(4)  VALUE '2541'.          
00111      12  ER-2542                 PIC X(4)  VALUE '2542'.          
00112      12  ER-2589                 PIC X(4)  VALUE '2589'.          
00113      12  ER-2591                 PIC X(4)  VALUE '2591'.          
00114      12  ER-2592                 PIC X(4)  VALUE '2592'.          
00115      12  ER-2593                 PIC X(4)  VALUE '2593'.          
00116      12  ER-2594                 PIC X(4)  VALUE '2594'.          
00117      12  ER-2629                 PIC X(4)  VALUE '2629'.          
00118      12  ER-2630                 PIC X(4)  VALUE '2630'.          
00119      12  ER-2635                 PIC X(4)  VALUE '2635'.          
00120      12  ER-2636                 PIC X(4)  VALUE '2636'.          
00121      12  ER-2651                 PIC X(4)  VALUE '2651'.          
00122      12  ER-2670                 PIC X(4)  VALUE '2670'.          
00123      12  ER-2683                 PIC X(4)  VALUE '2683'.          
00124      12  ER-2700                 PIC X(4)  VALUE '2700'.          
00125      12  ER-2701                 PIC X(4)  VALUE '2701'.          
00126      12  ER-2702                 PIC X(4)  VALUE '2702'.          
00127      12  ER-2901                 PIC X(4)  VALUE '2901'.          
           12  ER-2963                 PIC X(4)  VALUE '2963'.
           12  ER-2964                 PIC X(4)  VALUE '2964'.
00128      12  ER-3166                 PIC X(4)  VALUE '3166'.
           12  ER-3825                 PIC X(4)  VALUE '3825'.
           12  ER-3826                 PIC X(4)  VALUE '3826'.
00129      12  ER-7400                 PIC X(4)  VALUE '7400'.          
00130      12  ER-7403                 PIC X(4)  VALUE '7403'.          
00131      12  ER-7404                 PIC X(4)  VALUE '7404'.          
00132      12  ER-7405                 PIC X(4)  VALUE '7405'.          
00133      12  ER-7423                 PIC X(4)  VALUE '7423'.          
00134      12  ER-7424                 PIC X(4)  VALUE '7424'.          
00135      12  ER-7530                 PIC X(4)  VALUE '7530'.          
00136      12  ER-7632                 PIC X(4)  VALUE '7632'.          
00137      12  ER-7630                 PIC X(4)  VALUE '7630'.          
00138      12  ER-7631                 PIC X(4)  VALUE '7631'.          
00139      12  ER-7633                 PIC X(4)  VALUE '7633'.          
00140      12  ER-7997                 PIC X(4)  VALUE '7997'.          
00141      12  ER-7998                 PIC X(4)  VALUE '7998'.
           12  ER-9841                 PIC X(4)  VALUE '9841'.
00142      12  ER-9999                 PIC X(4)  VALUE '9999'.          
00143                                                                   
00144      EJECT                                                        
00145                                                                   
00146  01  ACCESS-KEYS.                                                 
00147      12  ERPNDB-KEY.                                              
00148          16  ERPNDB-COMP-CD          PIC X     VALUE SPACE.       
00149          16  ERPNDB-ENTRY-BATCH      PIC X(6)  VALUE SPACES.      
00150          16  ERPNDB-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.     
00151          16  ERPNDB-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.     
00152                                                                   
00153      12  ERPNDB-RECORD-LENGTH        PIC S9(4) COMP VALUE +585.   
00154      12  ERPNDB-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +608.   
00155                                                                   
00156      12  ELCNTL-KEY.                                              
00157          16  ELCNTL-COMPANY-ID       PIC X(3)  VALUE SPACES.      
00158          16  ELCNTL-REC-TYPE         PIC X     VALUE SPACES.      
00159          16  ELCNTL-ACCESS.                                       
00160              20  FILLER              PIC XX.                      
00161              20  ELCNTL-HI-BEN       PIC XX.                      
00162          16  ELCNTL-SEQ              PIC S9(4) VALUE +0 COMP.     
00163                                                                   
00164      12  ELCNTL-RECORD-LENGTH        PIC S9(4) COMP VALUE +504.   
00165      12  ELCNTL-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +527.   
070622     12  ERACCT-KEY.
070622         16  ERACCT-COMPANY-CD          PIC  X.
070622         16  ERACCT-CARRIER             PIC  X.
070622         16  ERACCT-GROUPING            PIC  X(6).
070622         16  ERACCT-STATE               PIC  XX.
070622         16  ERACCT-ACCOUNT             PIC  X(10).
070622         16  ERACCT-EXP-DT.
070622             20  ERACCT-DT              PIC  XX.
070622             20  ERACCT-FILL            PIC  X(4).
00166                                                                   
00167      12  ELCERT-KEY.                                              
00168          16  ELCERT-COMPANY-CD       PIC X.                       
00169          16  ELCERT-CARRIER          PIC X.                       
00170          16  ELCERT-GROUPING         PIC X(6).                    
00171          16  ELCERT-STATE            PIC XX.                      
00172          16  ELCERT-ACCOUNT          PIC X(10).                   
00173          16  ELCERT-CERT-EFF-DT      PIC XX.                      
00174          16  ELCERT-CERT-NO.                                      
00175              20  ELCERT-CERT-PRIME   PIC X(10).                   
00176              20  ELCERT-CERT-SFX     PIC X.                       
00177                                                                   
00178      12  ELCERT-RECORD-LENGTH        PIC S9(4) COMP VALUE +450.   
00179      12  ELCERT-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +473.   
00180                                                                   
00181      12  ERPNDM-KEY.                                              
00182          16  ERPNDM-COMP-CD          PIC X     VALUE SPACE.       
00183          16  ERPNDM-ENTRY-BATCH      PIC X(6)  VALUE SPACES.      
00184          16  ERPNDM-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.     
00185          16  ERPNDM-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.     
00186                                                                   
CIDMOD*    12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +250.   
CIDMOD     12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +374.   
CIDMOD*    12  ERPNDM-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +273.   
CIDMOD     12  ERPNDM-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +397.   
00189                                                                   
00190      EJECT                                                        
00191  01  WORK-AREA.                                                   
00192      12  DEEDIT-FIELD            PIC X(15).                       
00193      12  FILLER REDEFINES DEEDIT-FIELD.                           
00194          16  FILLER              PIC X(4).                        
00195          16  DEEDIT-FIELD-X11    PIC X(11).                       
00196      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(15).       
CIDMOD     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD PIC S9(13)V99.    
CIDMOD     12  DEEDIT-FIELD-V3 REDEFINES DEEDIT-FIELD PIC S9(12)V9(3).  
CIDMOD     12  DEEDIT-FIELD-V4 REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).  
CIDMOD     12  DEEDIT-FIELD-V5 REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).  
00197                                                                   
00198      12  WS-SUB                  PIC S9(4) VALUE +0  COMP.        
00199      12  WS-SUB1                 PIC S9(4) VALUE +0  COMP.        
00200      12  WS-SUB2                 PIC S9(4) VALUE +0  COMP.        
00201      12  WS-SUB3                 PIC S9(4) VALUE +0  COMP.        
00202      12  WS-ACCT-SUB             PIC S9(4) VALUE +0  COMP.        
00203      12  WS-COV-SUB              PIC S9(4) VALUE +0  COMP.        
00204      12  WS-EDIT-SUB             PIC S9(4) VALUE +0  COMP.        
00205      12  CENTURY-ADJ             PIC S9(08) VALUE +38400 COMP.    
00206      12  WS-WORK-BIN-RED         PIC S9(08) VALUE +0 COMP.        
00207      12  FILLER REDEFINES WS-WORK-BIN-RED.                        
00208          16  FILLER              PIC X(02).                       
00209          16  WS-WORK-BIN-DT      PIC X(02).                       
00210                                                                   
00211      12  WS-CALC-TERM            PIC S999V9(5) VALUE ZEROS.       
00212      12  WS-CALC-TERM-R REDEFINES WS-CALC-TERM.                   
00213          16  WS-CALC-TERM-WHOLE  PIC S999.                        
00214          16  WS-CALC-TERM-REMAIN PIC SV9(5).                      
00215                                                                   
00216      12  ERROR-SW                PIC X     VALUE SPACE.           
00217          88  NO-ERROR                VALUE SPACE.                 
00218          88  ERRORS                  VALUE 'Y'.                   
CIDMOD         88  WS-COVERAGE-PRESENT     VALUE 'Y'.                   
00219                                                                   
00220      12  WS-DATA-KEYED-SW        PIC X     VALUE SPACE.           
00221          88  WS-DATA-NOT-KEYED       VALUE SPACE.                 
00222          88  WS-DATA-KEYED           VALUE 'Y'.                   
00223                                                                   
00224      12  WS-EDITED-LF-CODE       PIC XX   VALUE SPACES.           
00225      12  WS-LF-ABBR-DESC         PIC XXX  VALUE SPACES.
050713     12  ws-lf-earnings-calc     pic x    value spaces.
00226                                                                   
00227      12  WS-EDITED-AH-CODE       PIC XX   VALUE ZEROS.            
00228      12  WS-AH-ABBR-DESC         PIC XXX  VALUE SPACES.           
00229                                                                   
00230      12  WS-BEN-CD               PIC XX   VALUE SPACES.           
00231                                                                   
00232      12  WS-ENTRY-CODE           PIC X     VALUE SPACE.           
00233          88  WS-ENTRY-CODE-VALID   VALUE ' ' 'E' 'R' 'P'          
00234                                          'M' 'D' 'V' 'U'.         
00235                                                                   
00236      12  WS-FORCE-CODE           PIC X     VALUE SPACE.           
00237          88  WS-FORCE-CODE-VALID   VALUE ' ' 'A' 'D'.             
00238                                                                   
00239      12  WS-ALL-NINES            PIC S9(7)V99 VALUE +9999999.99.  
00240                                                                   
00241      12  WS-MODE-CODE            PIC X     VALUE SPACE.           
00242          88 WS-MODE-CODE-VALID     VALUE ' ' 'M' 'W' 'S' 'B' 'T'. 
00243                                                                   
00244      12  WS-SKIP-CODE            PIC X     VALUE SPACE.           
00245          88 WS-SKIP-CODE-VALID     VALUE ' ' 'A' 'X' '0' THRU '9'.
00246                                                                   
00247      12  WS-KIND                 PIC XX    VALUE SPACE.           
00248          88 WS-KIND-LF             VALUE 'LF'.                    
00249          88 WS-KIND-AH             VALUE 'AH'.                    
00250          88 WS-KIND-PR             VALUE 'PR'.                    
00251          88 WS-KIND-UE             VALUE 'UE'.                    
00252          88 WS-KIND-DI             VALUE 'DI'.                    
00253          88 WS-KIND-MONTHLY        VALUE 'AH' 'UE'.               
00254                                                                   
00255      12  WS-JOURNAL-RECORD-LENGTH   PIC S9(4) COMP VALUE +0000.   
00256                                                                   
00257      12  WS-EDIT-CODE               PIC X(4)  VALUE SPACES.       
00258                                                                   
00259      12  WS-SAVE-INPUT-FIELDS.                                    
00260          16  WS-BAGE                 PIC 99       VALUE ZERO.     
00261          16  WS-BJNT-AGE             PIC 99       VALUE ZERO.     
00262          16  WS-BDAYS                PIC 999      VALUE ZERO.     
00263          16  WS-BLN-TERM             PIC 999      VALUE ZERO.     
00264          16  WS-BFREQ                PIC 99       VALUE ZERO.     
00265          16  WS-BPHONE               PIC 9(12)    VALUE  0 COMP-3.
030310         16  WS-BAPR                 PIC 99V9(4) VALUE ZEROS.
030310         16  FILLER REDEFINES WS-BAPR.
030310             20  WS-APR-WHOLE-NUM    PIC 99.
030310             20  WS-APR-DEC          PIC 9999.
00267          16  WS-BPMT                 PIC S9(6)V99 VALUE +0 COMP-3.
00268          16  WS-BPMTS                PIC S999     VALUE +0 COMP-3.
00269          16  WS-BLIVES               PIC 9(7)      COMP-3.        
00270                                                                   
00271          16  WS-B-COVERAGE.
00272              20  WS-BTERM1            PIC 999       COMP-3.        
00274              20  WS-BBEN1             PIC S9(10)V99 COMP-3.        
00275              20  WS-BALT-BEN1     PIC S9(10)V99 COMP-3.            
00276              20  WS-BPREM1        PIC S9(10)V99 COMP-3.            
00277              20  WS-BALT-PREM1    PIC S9(7)V99 COMP-3.             
00272              20  WS-BTERM2            PIC 999       COMP-3.        
00273              20  WS-BCRIT-PERD2       PIC 99        COMP-3.        
00274              20  WS-BBEN2             PIC S9(10)V99 COMP-3.        
00276              20  WS-BPREM2        PIC S9(10)V99 COMP-3.            
00275              20  WS-BALT-BEN2     PIC S9(10)V99 COMP-3.            
00277              20  WS-BALT-PREM2    PIC S9(7)V99 COMP-3.             
00278                                                                   
00279          16  WS-C-FIELDS   OCCURS 4 TIMES.                        
00280              20  WS-CLIVES      PIC 9(3)          COMP-3.         
00281              20  WS-CREFUND1    PIC S9(7)V99      COMP-3.         
00282              20  WS-CREFUND2    PIC S9(7)V99      COMP-3.         
00283              20  WS-CAN-REA     PIC X.
00284                                                                   
00285      12  WS-CONVERTED-BIRTH      OCCURS 2 PIC XX.
00286      12  WS-CONVERTED-EFFDT      PIC XX    VALUE SPACES.          
00287      12  WS-CONVERTED-1ST-PMT-DT PIC XX    VALUE SPACES.          
00288      12  WS-CONVERTED-EXPIRDT      OCCURS 2 TIMES PIC XX.         
00289      12  WS-CONVERTED-CANCEL-DATES OCCURS 4 TIMES.                
00290          16  WS-CONVERTED-CANDT1 PIC XX.                          
00291          16  WS-CONVERTED-CANDT2 PIC XX.                          
00292      12  WS-CONVERTED-CAN-EFF-DATES OCCURS 4 TIMES.               
00293          16  WS-CONVERTED-CAN-EFF-DT PIC XX.                      
00294                                                                   
00295      12  WS-FIRST-NAME.                                           
00296          16  WS-1ST-INIT         PIC X.                           
00297          16  FILLER              PIC X(9).                        
00298                                                                   
00299      12  WS-INITIALS.                                             
00300          16  WS-INITIAL-1        PIC X.                           
00301          16  WS-INITIAL-2        PIC X.                           
00302                                                                   
00303      12  WS-ZIP-CODE.                                             
00304          16  WS-ZIP-1            PIC X.                           
00305              88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.           
00306          16  WS-ZIP-2-3          PIC XX.                          
00307          16  WS-ZIP-4            PIC X.                           
00308          16  WS-ZIP-5            PIC X.                           
00309          16  WS-ZIP-6            PIC X.                           
00310          16  FILLER              PIC X(4).                        
00311      12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.                     
00312          16  WS-ZIP-AM-1-CODE    PIC X(5).                        
00313          16  WS-ZIP-AM-1-PLUS4   PIC X(4).                        
00314          16  FILLER              PIC X.                           
00315      12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.                     
00316          16  WS-ZIP-AM-2-CODE    PIC X(5).                        
00317          16  WS-ZIP-AM-2-DASH    PIC X.                           
00318          16  WS-ZIP-AM-2-PLUS4   PIC X(4).                        
00319      12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.                    
00320          16  WS-ZIP-CAN-1-POST1  PIC XXX.                         
00321          16  WS-ZIP-CAN-1-POST2  PIC XXX.                         
00322          16  FILLER              PIC X(4).                        
00323      12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.                    
00324          16  WS-ZIP-CAN-2-POST1  PIC XXX.                         
00325          16  FILLER              PIC X.                           
00326          16  WS-ZIP-CAN-2-POST2  PIC XXX.                         
00327          16  FILLER              PIC XXX.                         
00328      12  WS-MEMBER-NO            PIC X(12).                       
00329      12  FILLER  REDEFINES  WS-MEMBER-NO.                         
00330          16  WS-MEMBER-NO-1-8    PIC 9(8).                        
00331          16  FILLER              PIC X(4).                        
00332      12  WS-I-MICRO-NO           PIC S9(9)        COMP-3.         
00333                                                                   
00334      EJECT                                                        
00335                                                                   
00336  01  WS-DATE-AREA.                                                
00337      12  WS-COMPARE-CURRENT-DT.                                   
00338          16  FILLER              PIC X(4)    VALUE SPACES.        
00339          16  WS-COMPARE-CURR-YR  PIC X(2)    VALUE SPACES.        
00340      12  WS-SAVE-BIRTH-DATE.                                      
00341          16  FILLER              PIC X(4)   VALUE SPACES.         
00342          16  WS-SAVE-BIRTH-YR    PIC X(2)   VALUE SPACES.         
00343                                                                   
00344  01  CLASIC-WARNING.                                              
00345      12  WARNING-LENGTH              PIC S9(4)  VALUE +124 COMP.  
00346      12  WARNING-TEXT.                                            
00347          16  FILLER                  PIC X(80)  VALUE             
00348              'THIS DATA MAY HAVE PREVIOUSLY BEEN PROCESSED'.      
00349          16  FILLER                  PIC X(44)  VALUE             
00350              'CONTACT PAUL @ CSO FOR FURTHER INFORMATION'.        
00351                                                                   
00352      EJECT                                                        
00353                                                                   
00354      COPY ELCDATE.                                                
00355                                                                   
00356      EJECT                                                        
00357      COPY ELCLOGOF.                                               
00358                                                                   
00359      EJECT                                                        
00360      COPY ELCATTR.                                                
00361                                                                   
00362      EJECT                                                        
00363      COPY ELCEMIB.                                                
00364                                                                   
00365      EJECT                                                        
00366      COPY ELCINTF.                                                
00367      COPY ELC630PI.                                               
00368      EJECT                                                        
00369      COPY ELCJPFX.                                                
00370                              PIC X(608).                          
00371                                                                   
00372      EJECT                                                        
00373      COPY ELCAID.                                                 
00374                                                                   
00375  01  FILLER    REDEFINES DFHAID.                                  
00376      12  FILLER              PIC X(8).                            
00377      12  PF-VALUES           PIC X       OCCURS 2.                
00378                                                                   
00379      EJECT                                                        
00380      COPY EL6301S.                                                
00381                                                                   
00382      EJECT                                                        
00383                                                                   
00384  01  MAP-B REDEFINES EL630BI.                                     
00385      12  FILLER                      PIC X(42).                   
00386      12  DATA-AREA-B.                                             
00387          16  BSEQ-LEN                PIC S9(4)  COMP.             
00388          16  BSEQ-ATTRB              PIC X.                       
00389          16  BSEQ                    PIC 9(4).                    
00390          16  BMO-END-LEN             PIC S9(4)  COMP.             
00391          16  BMO-END-ATTRB           PIC X.                       
00392          16  BMO-END                 PIC X(8).                    
00393          16  BACCT-NM-LEN            PIC S9(4)  COMP.             
00394          16  BACCT-NM-ATTRB          PIC X.                       
00395          16  BACCT-NM                PIC X(30).                   
00396          16  BCERT-LEN               PIC S9(4)  COMP.             
00397          16  BCERT-ATTRB             PIC X.                       
00398          16  BCERT                   PIC X(10).                   
00399          16  BSFX-LEN                PIC S9(4)  COMP.             
00400          16  BSFX-ATTRB              PIC X.                       
00401          16  BSFX                    PIC X.                       

               16  BVINHD-LEN              PIC S9(4)  COMP.
               16  BVINHD-ATTRB            PIC X.
               16  BVINNDI                 PIC X(5).
               16  BVIN-LEN                PIC S9(4)  COMP.
               16  BVIN-ATTRB              PIC X.
               16  BVIN-NOI                PIC X(17).



               16  BLN-OFFICER-LEN         PIC S9(4)  COMP.             
               16  BLN-OFFICER-ATTRB       PIC X.                       
               16  BLN-OFFICER             PIC X(5).                    
00408          16  B1ST-NAME-LEN           PIC S9(4)  COMP.             
00409          16  B1ST-NAME-ATTRB         PIC X.                       
00410          16  B1ST-NAME               PIC X(10).                   
00411          16  BINIT-LEN               PIC S9(4)  COMP.             
00412          16  BINIT-ATTRB             PIC X.                       
00413          16  BINIT                   PIC X.                       
00405          16  BLAST-NAME-LEN          PIC S9(4)  COMP.             
00406          16  BLAST-NAME-ATTRB        PIC X.                       
00407          16  BLAST-NAME              PIC X(15).                   
00417          16  BAGE-LEN                PIC S9(4)  COMP.             
00418          16  BAGE-ATTRB              PIC X.                       
00419          16  BAGE                    PIC 99.                      

042114         16  BBIRTH-LEN              PIC S9(4)  COMP.             
042114         16  BBIRTH-ATTRB            PIC X.                       
042114         16  BBIRTH-DT               PIC 9(6).                    



00482          16  BJNT-1ST-NAME-LEN       PIC S9(4)   COMP.            
00483          16  BJNT-1ST-NAME-ATTRB     PIC X.                       
00484          16  BJNT-1ST-NAME           PIC X(10).                   
00485          16  BJNT-INIT-LEN           PIC S9(4)   COMP.            
00486          16  BJNT-INIT-ATTRB         PIC X.                       
00487          16  BJNT-INIT               PIC X.                       
00488          16  BJNT-LST-NAME-LEN       PIC S9(4)   COMP.            
00489          16  BJNT-LST-NAME-ATTRB     PIC X.                       
00490          16  BJNT-LST-NAME           PIC X(15).                   
00491          16  BJNT-AGE-LEN            PIC S9(4)   COMP.            
00492          16  BJNT-AGE-ATTRB          PIC X.                       
00493          16  BJNT-AGE                PIC 99.                      

042114         16  BJNTDOB-LEN             PIC S9(4)  COMP.          
042114         16  BJNTDOB-ATTRB           PIC X.                       
042114         16  BJNTDOB-DT              PIC 9(6).                    



00575          16  BADDRS1-LEN             PIC S9(4)  COMP.             
00576          16  BADDRS1-ATTRB           PIC X.                       
00577          16  BADDRS1                 PIC X(30).                   
00578          16  BADDRS2-LEN             PIC S9(4)  COMP.             
00579          16  BADDRS2-ATTRB           PIC X.                       
00580          16  BADDRS2                 PIC X(30).                   
00581          16  BCITY-LEN               PIC S9(4)  COMP.             
00582          16  BCITY-ATTRB             PIC X.                       
00583          16  BCITY                   PIC X(28).                   
00581          16  BSTATE-LEN              PIC S9(4)  COMP.             
00582          16  BSTATE-ATTRB            PIC X.                       
00583          16  BSTATE                  PIC XX.
00584          16  BZIPCDE-LEN             PIC S9(4)  COMP.             
00585          16  BZIPCDE-ATTRB           PIC X.                       
00586          16  BZIPCDE                 PIC X(10).                   
00499          16  BBENEFICIARY-LEN        PIC S9(4)   COMP.            
00500          16  BBENEFICIARY-ATTRB      PIC X.                       
00501          16  BBENEFICIARY            PIC X(25).                   
00575          16  BCADDR1-LEN             PIC S9(4)  COMP.             
00576          16  BCADDR1-ATTRB           PIC X.                       
00577          16  BCADDR1                 PIC X(30).                   
00578          16  BCADDR2-LEN             PIC S9(4)  COMP.             
00579          16  BCADDR2-ATTRB           PIC X.                       
00580          16  BCADDR2                 PIC X(30).                   
00581          16  BCCITY-LEN              PIC S9(4)  COMP.             
00582          16  BCCITY-ATTRB            PIC X.                       
00583          16  BCCITY                  PIC X(28).                   
00581          16  BCSTATE-LEN             PIC S9(4)  COMP.             
00582          16  BCSTATE-ATTRB           PIC X.                       
00583          16  BCSTATE                 PIC XX.
00584          16  BCZIPCD-LEN             PIC S9(4)  COMP.             
00585          16  BCZIPCD-ATTRB           PIC X.                       
00586          16  BCZIPCD                 PIC X(10).                   
00402          16  BEFFDT-LEN              PIC S9(4)  COMP.             
00403          16  BEFFDT-ATTRB            PIC X.                       
00404          16  BEFFDT                  PIC X(6).                    
00502          16  B1ST-PMT-LEN            PIC S9(4)  COMP.             
00503          16  B1ST-PMT-ATTRB          PIC X.                       
00504          16  B1ST-PMT                PIC 9(6).                    
00564          16  BAPR-LEN                PIC S9(4)  COMP.             
00565          16  BAPR-ATTRB              PIC X.                       
030310         16  BAPR-IN                 PIC X(7).
00567          16  BAPR-OUT REDEFINES BAPR-IN                           
00568                                      PIC 99.9999.                 
00510          16  BLN-TERM-LEN            PIC S9(4)  COMP.             
00511          16  BLN-TERM-ATTRB          PIC X.                       
00512          16  BLN-TERMI               PIC 9(3).                    
00513          16  BLN-TERMO REDEFINES                                  
00514                           BLN-TERMI  PIC ZZZ.                     

00437          16  BKIND1-LEN           PIC S9(4)  COMP.             
00438          16  BKIND1-ATTRB         PIC X.                       
00439          16  BKIND1               PIC XX.                      
00440          16  BTYPE1-LEN           PIC S9(4)  COMP.             
00441          16  BTYPE1-ATTRB         PIC X.                       
00442          16  BTYPE1               PIC X(3).                    
00443          16  BTERM1-LEN           PIC S9(4)  COMP.             
00444          16  BTERM1-ATTRB         PIC X.                       
00445          16  BTERM1I              PIC 999.                     
00446          16  BTERM1O REDEFINES                                 
00447                         BTERM1I   PIC ZZZ.                     
00448          16  BBENE1-LEN            PIC S9(4)  COMP.             
00449          16  BBENE1-ATTRB          PIC X.                       
CIDMOD         16  BBENE1I               PIC 9(10)V99.                
CIDMOD*        16  BBENE1I               PIC 9(12).                   
00451          16  BBENE1O REDEFINES                                  
00452                            BBENE1I PIC Z(9).99.                 
00453          16  BPREM1-LEN           PIC S9(4)  COMP.             
00454          16  BPREM1-ATTRB         PIC X.                       
CIDMOD         16  BPREM1I              PIC 9(9)V99.                 
CIDMOD*        16  BPREM1I              PIC 9(11).                   
00456          16  BPREM1O REDEFINES                                 
00457                           BPREM1I PIC Z(7).99-.                
00466          16  BALT-BEN1-LEN        PIC S9(4)  COMP.             
00467          16  BALT-BEN1-ATTRB      PIC X.                       
CIDMOD         16  BALT-BEN1I           PIC 9(10)V99.                
CIDMOD*        16  BALT-BEN1I           PIC 9(12).                   
00469          16  BALT-BEN1O REDEFINES                              
00470                            BALT-BEN1I PIC Z(9).ZZ.             
00471          16  BALT-PREM1-LEN       PIC S9(4)  COMP.             
00472          16  BALT-PREM1-ATTRB     PIC X.                       
CIDMOD         16  BALT-PREM1I          PIC 9(7)V99.                 
CIDMOD*        16  BALT-PREM1I          PIC 9(9).                    
00474          16  BALT-PREM1O REDEFINES                             
00475                            BALT-PREM1I PIC Z(6).ZZ.            

00437          16  BKIND2-LEN           PIC S9(4)  COMP.             
00438          16  BKIND2-ATTRB         PIC X.                       
00439          16  BKIND2               PIC XX.                      
00440          16  BTYPE2-LEN           PIC S9(4)  COMP.             
00441          16  BTYPE2-ATTRB         PIC X.                       
00442          16  BTYPE2               PIC X(3).                    
00443          16  BTERM2-LEN           PIC S9(4)  COMP.             
00444          16  BTERM2-ATTRB         PIC X.                       
00445          16  BTERM2I              PIC 999.                     
00446          16  BTERM2O REDEFINES                                 
00447                         BTERM2I   PIC ZZZ.                     
00448          16  BBENE2-LEN            PIC S9(4)  COMP.             
00449          16  BBENE2-ATTRB          PIC X.                       
CIDMOD         16  BBENE2I               PIC 9(10)V99.                
CIDMOD*        16  BBENE2I               PIC 9(12).                   
00451          16  BBENE2O REDEFINES                                  
00452                            BBENE2I PIC Z(9).99.                 
00453          16  BPREM2-LEN           PIC S9(4)  COMP.             
00454          16  BPREM2-ATTRB         PIC X.                       
CIDMOD         16  BPREM2I              PIC 9(9)V99.                 
CIDMOD*        16  BPREM2I              PIC 9(11).                   
00456          16  BPREM2O REDEFINES                                 
00457                           BPREM2I PIC Z(7).99-.                
00461          16  BCRIT-PERD2-LEN      PIC S9(4)  COMP.             
00462          16  BCRIT-PERD2-ATTRB    PIC X.                       
00463          16  BCRIT-PERD2I         PIC 99.                      
00464          16  BCRIT-PERD2O REDEFINES                            
00465                  BCRIT-PERD2I PIC ZZ.                      
00466          16  BALT-BEN2-LEN        PIC S9(4)  COMP.             
00467          16  BALT-BEN2-ATTRB      PIC X.                       
CIDMOD         16  BALT-BEN2I           PIC 9(10)V99.                
CIDMOD*        16  BALT-BEN2I           PIC 9(12).                   
00469          16  BALT-BEN2O REDEFINES                              
00470                            BALT-BEN2I PIC Z(9).ZZ.             
00471          16  BALT-PREM2-LEN       PIC S9(4)  COMP.             
00472          16  BALT-PREM2-ATTRB     PIC X.                       
CIDMOD         16  BALT-PREM2I          PIC 9(7)V99.                 
CIDMOD*        16  BALT-PREM2I          PIC 9(9).                    
00474          16  BALT-PREM2O REDEFINES                             
00475                            BALT-PREM2I PIC Z(6).ZZ.            

00599  01  MAP-C REDEFINES EL630BI.                                     
00600      12  FILLER                  PIC X(86).                       
00601      12  DATA-AREA-C             OCCURS 4 TIMES.                  
00602          16  CSEQ-LEN                PIC S9(4)  COMP.             
00603          16  CSEQ-ATTRB              PIC X.                       
00604          16  CSEQ                    PIC 9(4).                    
00605          16  CCERT-LEN               PIC S9(4)  COMP.             
00606          16  CCERT-ATTRB             PIC X.                       
00607          16  CCERT                   PIC X(10).                   
00608          16  CSFX-LEN                PIC S9(4)  COMP.             
00609          16  CSFX-ATTRB              PIC X.                       
00610          16  CSFX                    PIC X.                       
00611          16  CEFFDT-LEN              PIC S9(4)  COMP.             
00612          16  CEFFDT-ATTRB            PIC X.                       
00613          16  CEFFDT                  PIC 9(6).                    
00614          16  CLAST-NAME-LEN          PIC S9(4)  COMP.             
00615          16  CLAST-NAME-ATTRB        PIC X.                       
00616          16  CLAST-NAME              PIC X(15).                   
00617          16  CANCEL-INFO.                                         
00618              20  CKIND1-LEN          PIC S9(4)  COMP.             
00619              20  CKIND1-ATTRB        PIC X.                       
00620              20  CKIND1              PIC XX.                      
00621              20  CCANDT1-LEN         PIC S9(4)  COMP.             
00622              20  CCANDT1-ATTRB       PIC X.                       
00623              20  CCANDT1             PIC 9(6).                    
00624              20  CREFUND1-LEN        PIC S9(4)  COMP.             
00625              20  CREFUND1-ATTRB      PIC X.                       
CIDMOD             20  CREFUND1I           PIC S9(9)V99.                
CIDMOD*            20  CREFUND1I           PIC X(11).                   
00627              20  CREFUND1O REDEFINES                              
00628                            CREFUND1I PIC Z(7).99-.                
00629              20  CMTHD1-LEN          PIC S9(4)  COMP.             
00630              20  CMTHD1-ATTRB        PIC X.                       
00631              20  CMTHD1              PIC X.                       
00632              20  CKIND2-LEN          PIC S9(4)  COMP.             
00633              20  CKIND2-ATTRB        PIC X.                       
00634              20  CKIND2              PIC XX.                      
00635              20  CCANDT2-LEN         PIC S9(4)  COMP.             
00636              20  CCANDT2-ATTRB       PIC X.                       
00637              20  CCANDT2             PIC 9(6).                    
00638              20  CREFUND2-LEN        PIC S9(4)  COMP.             
00639              20  CREFUND2-ATTRB      PIC X.                       
CIDMOD             20  CREFUND2I           PIC S9(9)V99.                
CIDMOD*            20  CREFUND2I           PIC X(11).                   
00641              20  CREFUND2O REDEFINES                              
00642                            CREFUND2I PIC Z(7).99-.                
00643              20  CMTHD2-LEN          PIC S9(4)  COMP.             
00644              20  CMTHD2-ATTRB        PIC X.                       
00645              20  CMTHD2              PIC X.                       
00646              20  CCHK-LEN            PIC S9(4)  COMP.             
00647              20  CCHK-ATTRB          PIC X.                       
00648              20  CCHK                PIC X.                       
00649              20  CPAYEE-LEN          PIC S9(4)  COMP.             
00650              20  CPAYEE-ATTRB        PIC X.                       
00651              20  CPAYEE              PIC X(6).                    
00652              20  CLIVES-LEN          PIC S9(4)  COMP.             
00653              20  CLIVES-ATTRB        PIC X.                       
00654              20  CLIVESI             PIC 999.                     
00655              20  CLIVESO REDEFINES                                
00656                           CLIVESI    PIC ZZZ.                     
00660              20  CCANREA-LEN         PIC S9(4)  COMP.             
00661              20  CCANREA-ATTRB       PIC X.                       
00662              20  CCANREA             PIC X.
00665      EJECT                                                        
00666  LINKAGE SECTION.                                                 
00667  01  DFHCOMMAREA             PIC X(1900).                         
00668                                                                   
00669      EJECT                                                        
00670 *01 PARMLIST .                                                    
00671 *    02  FILLER              PIC S9(8)   COMP.                    
00672 *    02  ERPNDB-POINTER      PIC S9(8)   COMP.                    
00673 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                    
00674 *    02  ELCERT-POINTER      PIC S9(8)   COMP.                    
00675 *    02  ERPNDM-POINTER      PIC S9(8)   COMP.                    
00676                                                                   
00677      EJECT                                                        
00678                                                                   
00679      COPY ERCPNDB.                                                
00680      EJECT                                                        
00681                                                                   
00682      COPY ELCCNTL.                                                
00683      EJECT                                                        
070622     COPY ERCACCT.
00686      EJECT
00685      COPY ELCCERT.                                                
00686      EJECT                                                        
00687                                                                   
00688      COPY ERCPNDM.                                                
00689      EJECT                                                        
00690                                                                   
070622 01  var                         pic x(30).
00691  PROCEDURE DIVISION.                                              
00692                                                                   
00693      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      
00694      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.            
00695                                                                   
00696      MOVE +2                     TO EMI-NUMBER-OF-LINES.          
00697                                                                   
070622     move spaces to ws-kix-myenv
070622     set P to address of KIXSYS
070622     CALL "getenv" using by value P returning var-ptr
070622     if var-ptr = null then
070622        display ' kixsys not set '
070622     else
070622        set address of var to var-ptr
070622        move 0 to env-var-len
070622        inspect var tallying env-var-len
070622          for characters before X'00'
070622        unstring var (1:env-var-len) delimited by '/'
070622           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
070622              WS-KIX-SYS
070622        end-unstring
070622     end-if
070622     perform varying a1 from +1 by +1 until a1 > +10
070622        if ws-kix-myenv (a1:1) = low-values or high-values
070622           display ' found low or hi val '
070622           move spaces to ws-kix-myenv (a1:1)
070622        end-if
070622     end-perform

070622     set P to address of KIXHOST
070622     CALL "getenv" using by value P returning var-ptr
070622     if var-ptr = null then
070622        display ' kixhost not set '
070622     else
070622        set address of var to var-ptr
070622        move 0 to env-var-len
070622        inspect var tallying env-var-len
070622          for characters before X'00'
070622        MOVE var(1:env-var-len)  to ws-kixhost
070622        DISPLAY ' WS KIX HOST ' WS-KIXHOST
070622     end-if.

      *    display ' env *' ws-kix-myenv '*'
00698      IF EIBCALEN = 0                                              
00699          GO TO 8800-UNAUTHORIZED-ACCESS.                          
00700                                                                   
00701      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
00702      MOVE '5'                    TO DC-OPTION-CODE.               
00703      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
00704      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            
00705      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                
00706      MOVE DC-GREG-DATE-1-MDY     TO WS-COMPARE-CURRENT-DT.        
00707                                                                   
00708      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
00709          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   
00710              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      
00711              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      
00712              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      
00713              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      
00714              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      
00715              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      
00716              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    
00717              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      
00718          ELSE                                                     
00719              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      
00720              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    
00721              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      
00722              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      
00723              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      
00724              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      
00725              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      
00726              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     
00727                                                                   
00728      MOVE LOW-VALUES             TO EL630BI.                      
00729                                                                   
00730      IF EIBTRNID NOT = TRANS-EXA6                                 
00731          MOVE ZEROS              TO PI-LF-ISS-ENTERED             
00732                                     PI-LF-CAN-ENTERED             
00733                                     PI-AH-ISS-ENTERED             
00734                                     PI-AH-CAN-ENTERED             
00735                                     PI-ISS-CNT-ENTERED            
00736                                     PI-CAN-CNT-ENTERED            
00737          IF PI-MAINT-FUNC = 'N'                                   
00738             MOVE +0              TO PI-LAST-SEQ-NO-ADDED          
00739             MOVE +1              TO PI-NEXT-DISPLAY-SEQ-NO        
00740             IF PI-MAP-NAME = EL630B                               
00741                PERFORM 8550-SET-MAP-SEQ-NOS                       
00742                GO TO 8100-SEND-INITIAL-MAP                        
00743             ELSE                                                  
00744                PERFORM 8550-SET-MAP-SEQ-NOS                       
00745                        VARYING WS-SUB2 FROM 1 BY 1                
00746                        UNTIL WS-SUB2   GREATER 4                  
00747                  GO TO 8100-SEND-INITIAL-MAP                      
00748          ELSE                                                     
00749              GO TO 3000-CONTINUE-ENTRY.                           
00750                                                                   
00751      EXEC CICS HANDLE CONDITION                                   
00752          PGMIDERR  (9600-PGMID-ERROR)                             
00753          ERROR     (9990-ABEND)                                   
00754      END-EXEC.                                                    
00755                                                                   
00756      IF EIBAID = DFHCLEAR                                         
00757          MOVE SPACE TO PI-DISPLAY-SW                              
00758                        PI-BROWSE-SW                               
00759          GO TO 9400-CLEAR.                                        
00760                                                                   
00761      EJECT                                                        
00762                                                                   
00763  0200-RECEIVE.                                                    
00764      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       
00765          MOVE ER-0008            TO EMI-ERROR                     
00766          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00767          IF PI-MAP-NAME = EL630B                                  
00768              MOVE -1             TO BPFENTRL                      
00769              GO TO 8200-SEND-DATAONLY                             
00770          ELSE                                                     
00771              MOVE -1             TO CPFENTRL                      
00772              GO TO 8200-SEND-DATAONLY.                            
00773                                                                   
00774      EXEC CICS RECEIVE                                            
00775          MAP      (PI-MAP-NAME)                                   
00776          MAPSET   (MAPSET-EL6301S)                                
00777          INTO     (EL630BI)                                       
00778      END-EXEC.                                                    
00779                                                                   
00780      INSPECT EL630BI CONVERTING '_' TO ' '.                       
00781                                                                   
00782      IF PI-MAP-NAME = EL630B                                      
00783          IF BPFENTRL GREATER ZERO                                 
00784              IF EIBAID NOT = DFHENTER                             
00785                  MOVE ER-0004    TO EMI-ERROR                     
00786                  MOVE AL-UNBOF   TO BPFENTRA                      
00787                  MOVE -1         TO BPFENTRL                      
00788                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
00789                  GO TO 8200-SEND-DATAONLY                         
00790              ELSE                                                 
00791                  IF BPFENTRI NUMERIC AND                          
00792                     BPFENTRI GREATER 0 AND LESS 23                
00793                      MOVE PF-VALUES (BPFENTRI) TO EIBAID          
00794                  ELSE                                             
00795                      MOVE ER-0029  TO EMI-ERROR                   
00796                      MOVE AL-UNBOF TO BPFENTRA                    
00797                      MOVE -1       TO BPFENTRL                    
00798                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
00799                      GO TO 8200-SEND-DATAONLY                     
00800          ELSE                                                     
00801              NEXT SENTENCE                                        
00802      ELSE                                                         
00803      IF PI-MAP-NAME = EL630C                                      
00804          IF CPFENTRL GREATER ZERO                                 
00805              IF EIBAID NOT = DFHENTER                             
00806                  MOVE ER-0004    TO EMI-ERROR                     
00807                  MOVE AL-UNBOF   TO CPFENTRA                      
00808                  MOVE -1         TO CPFENTRL                      
00809                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
00810                  GO TO 8200-SEND-DATAONLY                         
00811              ELSE                                                 
00812                  IF CPFENTRI NUMERIC AND                          
00813                     CPFENTRI GREATER 0 AND LESS 23                
00814                      MOVE PF-VALUES (CPFENTRI) TO EIBAID          
00815                  ELSE                                             
00816                      MOVE ER-0029  TO EMI-ERROR                   
00817                      MOVE AL-UNBOF TO BPFENTRA                    
00818                      MOVE -1       TO BPFENTRL                    
00819                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
00820                      GO TO 8200-SEND-DATAONLY.                    
00821                                                                   
00822      EJECT                                                        
00823 ******************************************************************
00824 *   PF KEY FUNCTIONS:                                            *
00825 *                                                                *
00826 *   PF1 = BROWSE FOWARD                                          *
00827 *   PF2 = BROWSE BACKWARD                                        *
00828 *   PF3 = ADD ISSUE RECORD                                       *
00829 *   PF4 = ADD CANCEL RECORD                                      *
00830 *   PF5 = RESET TABS (OPEN PROTECTED FIELDS)                     *
00831 *   PF6 = DELETE ENTRY                                           *
00832 ******************************************************************
00833                                                                   
00834  0300-CHECK-PFKEYS.                                               
00835      IF EIBAID = DFHPF12                                          
00836          GO TO 9500-PF12.                                         
00837                                                                   
00838      IF EIBAID NOT = DFHPF5                                       
00839         MOVE SPACE               TO PI-BROWSE-SW.                 
00840                                                                   
00841      IF EIBAID = DFHENTER                                         
00842          GO TO 1000-EDIT-MAPB.                                    
00843                                                                   
00844      IF EIBAID = DFHPF1                                           
00845          MOVE 'Y'                TO PI-BROWSE-SW                  
00846          GO TO 2000-BROWSE-FWD.                                   
00847                                                                   
00848      IF EIBAID = DFHPF2                                           
00849          MOVE 'Y'                TO PI-BROWSE-SW                  
00850          GO TO 2100-BROWSE-BKWD.                                  
00851                                                                   
00852      IF EIBAID = DFHPF3                                           
00853          MOVE SPACE              TO PI-DISPLAY-SW                 
00854          MOVE LOW-VALUES         TO EL630BI                       
00855          ADD +1                     PI-LAST-SEQ-NO-ADDED          
00856                GIVING PI-NEXT-DISPLAY-SEQ-NO                      
00857          MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ          
00858          MOVE EL630B             TO PI-MAP-NAME                   
00859          PERFORM 8550-SET-MAP-SEQ-NOS                             
00860          GO TO 8100-SEND-INITIAL-MAP.                             
00861                                                                   
00862      IF EIBAID = DFHPF4                                           
00863          MOVE SPACE              TO PI-DISPLAY-SW                 
00864          MOVE LOW-VALUES         TO MAP-C                         
00865          ADD +1                     PI-LAST-SEQ-NO-ADDED          
00866                GIVING PI-NEXT-DISPLAY-SEQ-NO                      
00867          MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ          
00868          MOVE EL630C             TO PI-MAP-NAME                   
00869          PERFORM 8550-SET-MAP-SEQ-NOS                             
00870                 VARYING WS-SUB2 FROM 1 BY 1                       
00871                 UNTIL WS-SUB2 GREATER +4                          
00872          GO TO 8100-SEND-INITIAL-MAP.                             
00873                                                                   
00874      IF EIBAID = DFHPf5                                           
00875         IF PI-BROWSE                                              
00876            IF PI-MAP-NAME = EL630B                                
00877               MOVE -1          TO BCERTL
00878               GO TO 8200-SEND-DATAONLY                            
00879            ELSE                                                   
00880               MOVE -1          TO CCERT-LEN (1)                   
00881               GO TO 8200-SEND-DATAONLY.                           
00882                                                                   
00883      IF EIBAID = DFHPF5                                           
00884         IF PI-MAP-NAME = EL630B                                   
00885            PERFORM 0610-UNPROTECT-FIELDS THRU 0610-EXIT           
00886            MOVE -1             TO BCERTL
00887            GO TO 8200-SEND-DATAONLY                               
00888         ELSE                                                      
00889            PERFORM 0710-UNPROTECT-FIELDS THRU 0710-EXIT           
00890            ADD +1    PI-LAST-SEQ-NO-ADDED                         
00891                   GIVING PI-NEXT-DISPLAY-SEQ-NO                   
00892           MOVE -1             TO CCERT-LEN  (1)                   
00893           GO TO 8200-SEND-DATAONLY.                               
00894                                                                   
00895      IF EIBAID = DFHPF6                                           
00896          IF PI-LAST-FUNC-DISPLAY                                  
00897              GO TO 6000-DELETE-PEND-BUS-RECORD                    
00898          ELSE                                                     
00899              MOVE ER-2594        TO EMI-ERROR                     
00900              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00901              IF PI-MAP-NAME = EL630B                              
00902                  MOVE -1         TO BPFENTRL                      
00903                  GO TO 8200-SEND-DATAONLY                         
00904              ELSE                                                 
00905                  MOVE -1         TO CPFENTRL                      
00906                  GO TO 8200-SEND-DATAONLY.                        
00907                                                                   
00908      MOVE ER-0008 TO EMI-ERROR.                                   
00909      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00910                                                                   
00911      IF PI-MAP-NAME = EL630B                                      
00912          MOVE -1                 TO BPFENTRL                      
00913      ELSE                                                         
00914          MOVE -1                 TO CPFENTRL.                     
00915                                                                   
00916      GO TO 8200-SEND-DATAONLY.                                    
00917                                                                   
00918      EJECT                                                        
00919  0600-PROTECT-FIELDS.                                             
00920      IF PI-COMPANY-ID = 'MON'                                     
00921          MOVE AL-UANOF         TO BSFX-ATTRB                      
00922      ELSE                                                         
00923      IF PI-COMPANY-ID = 'PEM' OR                                  
00924                         'CGL' OR                                  
00925                         'TIH' OR                                  
00926                         'TII' OR                                  
00927                         'FGL' OR                                  
00928                         'OFL'                                     
00929         NEXT SENTENCE                                             
00930      ELSE                                                         
00931         IF NOT PI-ISS-SUFFIX-KEYED                                
00932            MOVE AL-SANOF         TO BSFX-ATTRB.                   
00933                                                                   
00934      IF PI-PROCESSOR-ID = 'LGXX'                                  
00935         IF NOT PI-ISS-SUFFIX-KEYED                                
00936            MOVE AL-SANOF         TO BSFX-ATTRB.                   
00937                                                                   
00938 *    IF NOT PI-IG-KEYED                                           
00939 *        MOVE AL-SANOF           TO BIND-GRP-ATTRB.               
00940                                                                   
00941 *    IF NOT PI-1ST-PMT-KEYED                                      
00942 *        MOVE AL-SANOF           TO B1ST-PMT-ATTRB.               
00943                                                                   
00944 *    IF NOT PI-DAYS-KEYED                                         
00945 *        MOVE AL-SANOF           TO BDAYS-ATTRB.                  
00946                                                                   
00947      IF PI-COMPANY-ID = 'PEM' OR 'CID' OR 'DCC' OR
030612                        'CGL' OR 'AHL' or 'VPP' or
062121                        'TIH' OR 'FNL' or                       
00950                         'TII' OR                                  
00951                         'FGL' OR                                  
00952                         'OFL'                                     
00953         NEXT SENTENCE                                             
00954      ELSE                                                         
00955         IF NOT PI-APR-KEYED                                       
00956            MOVE AL-SANOF         TO BAPR-ATTRB.                   
00957                                                                   
           IF NOT PI-VIN-KEYED
              MOVE AL-SANOF            TO BVIN-ATTRB
           END-IF

00958      IF PI-PROCESSOR-ID = 'LGXX'                                  
00959         IF NOT PI-APR-KEYED                                       
00960            MOVE AL-SANOF         TO BAPR-ATTRB.                   
00961                                                                   
00962 *    IF NOT PI-FREQ-KEYED                                         
00963 *        MOVE AL-SANOF           TO BFREQ-ATTRB.                  
00964                                                                   
00965 *    IF PI-COMPANY-ID = 'PEM' OR                                  
00966 *                       'NCL' OR                                  
00967 *                       'CGL' OR                                  
00968 *                       'TIH' OR                                  
00969 *                       'TII' OR                                  
00970 *                       'FGL' OR                                  
00971 *                       'OFL' OR                                  
00972 *                       'TMS' OR                                  
00973 *                       'FLA' OR                                  
00974 *                       'CRI'                                     
00975 *       NEXT SENTENCE                                             
00976 *    ELSE                                                         
00977 *       IF NOT PI-SIG-KEYED                                       
00978 *          MOVE AL-SANOF         TO BSIG-ATTRB.                   
00979                                                                   
00980 *    IF NOT PI-ISS-LIVES-KEYED                                    
00981 *        MOVE AL-SANOF           TO BLIVES-ATTRB.                 
00982                                                                   
00995 *    IF NOT PI-MEMBER-KEYED                                       
00996 *        MOVE AL-SANOF           TO BMEM-NO-ATTRB.                
00997                                                                   
00998 *    IF PI-COMPANY-ID = 'HER'                                     
00999 *        NEXT SENTENCE                                            
01000 *    ELSE                                                         
01001 *        IF NOT PI-MICRO-NO-KEYED                                 
01002 *            MOVE AL-SANOF       TO BMICROFILM-NO-ATTRB.          
01003                                                                   
01004 *    IF NOT PI-MODE-KEYED                                         
01005 *        MOVE AL-SANOF           TO BMODE-ATTRB.                  
01006                                                                   
01007 *    IF NOT PI-PMTS-KEYED                                         
01008 *        MOVE AL-SANOF           TO BPMTS-ATTRB.                  
01009                                                                   
030612     IF PI-COMPANY-ID = 'CID' OR 'DCC' or 'AHL' or 'VPP'
062121           OR 'FNL'
030310        CONTINUE
030310     ELSE
030310        IF NOT PI-LN-OFFICER-KEYED                                   
030310           MOVE AL-SANOF         TO BLN-OFFICER-ATTRB
030310        END-IF
030310     END-IF
01012                                                                   
030612     IF PI-COMPANY-ID = 'CID' OR 'DCC' or 'AHL' or 'VPP'
062121        OR 'FNL'
              CONTINUE
           ELSE
01013         IF NOT PI-LNTRM-KEYED
01014            MOVE AL-SANOF         TO BLN-TERM-ATTRB
              END-IF
           END-IF

01016 *    IF NOT PI-ENTRY-KEYED                                        
01017 *        MOVE AL-SANOF           TO BENTRY-ATTRB.                 
01018                                                                   
01019 *    IF NOT PI-FORCE-KEYED                                        
01020 *        MOVE AL-SANOF           TO BFORCE-ATTRB.                 
01021                                                                   
01022 *    IF NOT PI-BILLCD-KEYED                                       
01023 *        MOVE AL-SANOF           TO BBILLCD-ATTRB.                
01024                                                                   
01025 *    IF PI-COMPANY-ID EQUAL 'CVL' OR 'LGX'                        
01026 *       NEXT SENTENCE                                             
01027 *    ELSE                                                         
01028 *       IF NOT PI-POLICY-KEYED                                    
01029 *          MOVE AL-SANOF           TO BPOLICY-ATTRB.              
01030                                                                   
01031 *    IF NOT PI-RINCD-KEYED                                        
01032 *        MOVE AL-SANOF           TO BRINCD-ATTRB.                 
01033                                                                   
01034 *    IF NOT PI-RTCLS-KEYED                                        
01035 *        MOVE AL-SANOF           TO BRTCLS-ATTRB.                 
01036                                                                   
01037 *    IF PI-COMPANY-ID = 'PEM' OR                                  
01038 *                       'CGL' OR                                  
01039 *                       'TIH' OR                                  
01040 *                       'TII' OR                                  
01041 *                       'FGL' OR                                  
01042 *                       'OFL' OR                                  
01043 *                       'LBL' OR 'LGX'                            
01044 *       NEXT SENTENCE                                             
01045 *    ELSE                                                         
01046 *       IF NOT PI-EXPIRE-KEYED                                    
01047 *          MOVE AL-SANOF         TO BEXPIRE-ATTRB (1)             
01048 *                                   BEXPIRE-ATTRB (2).            
01049 *    IF PI-PROCESSOR-ID = 'LGXX'                                  
01050 *       IF NOT PI-EXPIRE-KEYED                                    
01051 *          MOVE AL-SANOF         TO BEXPIRE-ATTRB (1)             
01052 *                                   BEXPIRE-ATTRB (2).            

071714*    IF PI-COMPANY-ID = 'CID' OR 'DCC' or 'AHL' or 'VPP'
071714*       CONTINUE
071714*    ELSE
060211        IF NOT PI-CRIT-PERD-KEYED
060211           MOVE AL-SANOF         TO BCRIT-PERD2-ATTRB
060211        END-IF
071714*    END-IF

01058 *    IF NOT PI-PMT-KEYED                                          
01059 *        MOVE AL-SANOF           TO BPMT-ATTRB.                   
01060                                                                   
01061 *    IF NOT PI-SKPCD-KEYED                                        
01062 *        MOVE AL-SANOF           TO BSKPCD-ATTRB.                 
01063                                                                   
01064      IF PI-COMPANY-ID = 'CRI' OR 'LGX'
030612        OR 'FLA' OR 'CID' OR 'DCC' or 'AHL' or 'VPP'
062121        OR 'FNL'
01065         CONTINUE
01066      ELSE                                                         
01067         IF PI-BIRTH-DATE-IS-INPUT                                
01068            MOVE AL-SANOF         TO BAGE-ATTRB                    
01069         ELSE                                                     
01070            MOVE AL-SANOF         TO BBIRTH-ATTRB
                                          BJNTDOB-ATTRB
               END-IF
           END-IF
01071                                                                   
01072      IF PI-COMPANY-ID = 'CRI' OR 'LGX'                            
01073          IF PI-BIRTHDT-KEYED  AND  NOT PI-AGE-KEYED               
01074              MOVE AL-SANOF       TO BAGE-ATTRB.                   
01075                                                                   
01076      IF PI-COMPANY-ID = 'CRI' OR 'LGX'                            
01077          IF PI-AGE-KEYED  AND  NOT PI-BIRTHDT-KEYED               
01078              MOVE AL-SANOF       TO BBIRTH-ATTRB.                 
01079                                                                   
01080      IF PI-COMPANY-ID = 'PEM' OR 'CGL' OR 'TIH' OR 'TII' OR
01084         'FGL' OR 'OFL'
01086         CONTINUE
01087      ELSE                                                         
01088         IF NOT PI-JNT-AGE-KEYED                                   
01089            MOVE AL-SANOF         TO BJNT-AGE-ATTRB
              END-IF
           END-IF

01099      IF NOT PI-JNT-NAME-KEYED                                  
01100         MOVE AL-SANOF            TO BJNT-INIT-ATTRB             
01101                                     BJNT-LST-NAME-ATTRB         
01102                                     BJNT-1ST-NAME-ATTRB
                                          BJNTDOB-ATTRB
           end-if

030612     IF PI-COMPANY-ID = 'TMS' OR 'CID' OR 'DCC' or 'AHL' or 'VPP'
062121        OR 'FNL'
01105         NEXT SENTENCE                                             
01106      ELSE                                                         
01107         IF NOT PI-BENEFICIARY-KEYED                               
01108             MOVE AL-SANOF           TO BBENEFICIARY-ATTRB.        
01109                                                                   
01110      IF NOT PI-ALT-BEN-KEYED                                      
01111          MOVE AL-SANOF           TO BALT-BEN1-ATTRB
           END-IF
01112                                                                   
01113      IF NOT PI-ALT-PREM-KEYED                                     
01114          MOVE AL-SANOF           TO BALT-PREM1-ATTRB
           END-IF

           IF NOT PI-ALT-BEN-KEYED
               MOVE AL-SANOF           TO BALT-BEN2-ATTRB
           END-IF

           IF NOT PI-ALT-PREM-KEYED
               MOVE AL-SANOF           TO BALT-PREM2-ATTRB
           END-IF

01116      IF PI-PROCESSOR-ID = 'LGXX'                                  
CIDMOD        CONTINUE                                                  
01118      ELSE                                                         
01119         IF PI-COMPANY-ID = 'PEM' OR                               
01120                            'CGL' OR                               
01121                            'TIH' OR                               
01122                            'TII' OR                               
01123                            'FGL' OR                               
01124                            'OFL'                                  
01125            MOVE AL-SANOF            TO BADDRS2-ATTRB              
CIDMOD*                                      BPHONE-ATTRB               
CIDMOD        ELSE
062121           IF PI-COMPANY-ID ='CID' or 'AHL' OR 'FNL'
CIDMOD              MOVE AL-SANOF         TO BADDRS2-ATTRB
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD 
01128      IF PI-COMPANY-ID = 'TMS'                                     
01129 *        MOVE AL-UANOF              TO BSIG-ATTRB                 
01130          MOVE AL-UANOF              TO BJNT-AGE-ATTRB             
01131                                        BJNT-LST-NAME-ATTRB        
01132                                        BJNT-1ST-NAME-ATTRB        
01133                                        BJNT-INIT-ATTRB            
01134                                        BAPR-ATTRB.                
01135                                                                   
01136  0600-EXIT.                                                       
01137      EXIT.                                                        
01138                                                                   
01139      EJECT                                                        
01140  0610-UNPROTECT-FIELDS.                                           
01141      IF PI-COMPANY-ID = 'PEM' OR                                  
01142                         'CGL' OR                                  
01143                         'TIH' OR                                  
01144                         'TII' OR                                  
01145                         'FGL' OR                                  
01146                         'OFL'                                     
01147         NEXT SENTENCE                                             
01148      ELSE                                                         
01149         IF NOT PI-ISS-SUFFIX-KEYED                                
01150            MOVE AL-UANOF         TO BSFX-ATTRB.                   
01151                                                                   
01152      IF PI-PROCESSOR-ID = 'LGXX'                                  
01153         IF NOT PI-ISS-SUFFIX-KEYED                                
01154            MOVE AL-UANOF         TO BSFX-ATTRB.                   
01155                                                                   
01156 *    IF NOT PI-IG-KEYED                                           
01157 *        MOVE AL-UANOF           TO BIND-GRP-ATTRB.               
01158                                                                   
01159 *    IF NOT PI-1ST-PMT-KEYED                                      
01160 *        MOVE AL-UANOF           TO B1ST-PMT-ATTRB.               
01161                                                                   
01162 *    IF NOT PI-DAYS-KEYED                                         
01163 *        MOVE AL-UANOF           TO BDAYS-ATTRB.                  
01164                                                                   
030310     IF PI-COMPANY-ID = 'PEM' OR 'CGL' OR 'TIH' OR 'TII' OR 'FGL'
062121        OR 'OFL' OR 'CID' OR 'DCC' or 'AHL' or 'VPP' OR 'FNL'
030310        CONTINUE
030310     ELSE                                                         
030310        IF NOT PI-APR-KEYED                                       
030310           MOVE AL-UANOF         TO BAPR-ATTRB
030310        END-IF
030310     END-IF
01175                                                                   
01176      IF PI-PROCESSOR-ID = 'LGXX'                                  
01177         IF NOT PI-APR-KEYED                                       
01178             MOVE AL-UANOF        TO BAPR-ATTRB.                   
01179                                                                   
01180 *    IF NOT PI-FREQ-KEYED                                         
01181 *        MOVE AL-UANOF           TO BFREQ-ATTRB.                  
01182                                                                   
01183 *    IF PI-COMPANY-ID = 'PEM' OR                                  
01184 *                       'NCL' OR                                  
01185 *                       'CGL' OR                                  
01186 *                       'TIH' OR                                  
01187 *                       'TII' OR                                  
01188 *                       'FGL' OR                                  
01189 *                       'OFL' OR                                  
01190 *                       'TMS' OR                                  
01191 *                       'FLA' OR                                  
01192 *                       'CRI'                                     
01193 *       NEXT SENTENCE                                             
01194 *    ELSE                                                         
01195 *       IF NOT PI-SIG-KEYED                                       
01196 *          MOVE AL-UANOF           TO BSIG-ATTRB.                 
01197                                                                   
01198 *    IF NOT PI-ISS-LIVES-KEYED                                    
01199 *        MOVE AL-UANOF           TO BLIVES-ATTRB.                 
01200                                                                   
01212 *    IF NOT PI-MEMBER-KEYED                                       
01213 *        MOVE AL-UANOF           TO BMEM-NO-ATTRB.                
01214                                                                   
01215 *    IF PI-COMPANY-ID = 'HER'                                     
01216 *        NEXT SENTENCE                                            
01217 *    ELSE                                                         
01218 *        IF NOT PI-MICRO-NO-KEYED                                 
01219 *            MOVE AL-UNNOF       TO BMICROFILM-NO-ATTRB.          
01220                                                                   
01221 *    IF NOT PI-MODE-KEYED                                         
01222 *        MOVE AL-UANOF           TO BMODE-ATTRB.                  
01223                                                                   
01224 *    IF NOT PI-PMTS-KEYED                                         
01225 *        MOVE AL-UANOF           TO BPMTS-ATTRB.                  
01226                                                                   
030612     IF PI-COMPANY-ID = 'CID' OR 'DCC' or 'AHL' or 'VPP'
062121           OR 'FNL'
030310        CONTINUE
030310     ELSE
030310        IF NOT PI-LN-OFFICER-KEYED                                   
030310           MOVE AL-UANOF         TO BLN-OFFICER-ATTRB
030310        END-IF
030310     END-IF
01229                                                                   
01230 *    IF NOT PI-ENTRY-KEYED                                        
01231 *        MOVE AL-UANOF           TO BENTRY-ATTRB.                 
01232                                                                   
01233 *    IF NOT PI-FORCE-KEYED                                        
01234 *        MOVE AL-UANOF           TO BFORCE-ATTRB.                 
01235                                                                   
01236 *    IF NOT PI-BILLCD-KEYED                                       
01237 *        MOVE AL-UANOF           TO BBILLCD-ATTRB.                
01238                                                                   
01239 *    IF PI-COMPANY-ID EQUAL 'CVL' OR 'LGX'                        
01240 *       NEXT SENTENCE                                             
01241 *    ELSE                                                         
01242 *       IF NOT PI-POLICY-KEYED                                    
01243 *          MOVE AL-UANOF           TO BPOLICY-ATTRB.              
01244                                                                   
01245 *    IF NOT PI-RINCD-KEYED                                        
01246 *        MOVE AL-UANOF           TO BRINCD-ATTRB.                 
01247                                                                   
01248 *    IF NOT PI-RTCLS-KEYED                                        
01249 *        MOVE AL-UANOF           TO BRTCLS-ATTRB.                 
01250                                                                   
030612     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121        OR 'FNL'
030310        CONTINUE
030310     ELSE
030310        IF NOT PI-LNTRM-KEYED                                        
030310           MOVE AL-UANOF         TO BLN-TERM-ATTRB
030310        END-IF
030310     END-IF
01253                                                                   
01254 *    IF PI-COMPANY-ID = 'LBL' OR 'LGX'                            
01255 *        NEXT SENTENCE                                            
01256 *    ELSE                                                         
01257 *        IF NOT PI-EXPIRE-KEYED                                   
01258 *            MOVE AL-UANOF       TO BEXPIRE-ATTRB (1)             
01259 *                                   BEXPIRE-ATTRB (2).            

071714*    IF PI-COMPANY-ID = 'CID' OR 'DCC' or 'AHL' or 'VPP'
071714*       CONTINUE
071714*    ELSE
060211        IF NOT PI-CRIT-PERD-KEYED
060211           MOVE AL-UANOF         TO BCRIT-PERD2-ATTRB
060211        END-IF
071714*    END-IF

01265 *    IF NOT PI-PMT-KEYED                                          
01266 *        MOVE AL-UANOF           TO BPMT-ATTRB.                   
01267                                                                   
01268 *    IF NOT PI-SKPCD-KEYED                                        
01269 *        MOVE AL-UANOF           TO BSKPCD-ATTRB.                 
01270                                                                   
01271      IF PI-COMPANY-ID = 'CRI' OR 'LGX' OR 'CID' OR 'DCC' or 'VPP'
062121        or 'AHL' OR 'FNL'
01272         CONTINUE
01273      ELSE                                                         
01274         IF PI-BIRTH-DATE-IS-INPUT                                
01275            MOVE AL-UANOF         TO BAGE-ATTRB                    
01276         ELSE                                                     
01277            MOVE AL-UANOF         TO BBIRTH-ATTRB
                                          BJNTDOB-ATTRB
              END-IF
           END-IF
01278                                                                   
01279      IF PI-COMPANY-ID = 'FLA'                                     
01280         IF BAGE-LEN NOT GREATER THAN +0                            
01281            MOVE AL-UANOF       TO BAGE-ATTRB.                     
01282                                                                   
01283      IF PI-COMPANY-ID = 'FLA'                                     
01284         IF BAGE-LEN NOT GREATER THAN +0                           
01285            MOVE AL-UANOF       TO BBIRTH-ATTRB.                   
01286                                                                   
01287      IF PI-COMPANY-ID = 'CRI' OR                                  
01288                         'LGX'                                     
01289         IF NOT PI-AGE-KEYED                                       
01290            IF BAGE-LEN NOT GREATER THAN +0                        
01291               MOVE AL-UANOF     TO BAGE-ATTRB.                    
01292                                                                   
01293      IF PI-COMPANY-ID = 'CRI' OR                                  
01294                         'LGX'                                     
01295          IF NOT PI-BIRTHDT-KEYED                                  
01296              MOVE AL-UANOF       TO BBIRTH-ATTRB.                 
01297                                                                   
01298      IF PI-COMPANY-ID = 'PEM' OR                                  
01299                         'CGL' OR                                  
01300                         'TIH' OR                                  
01301                         'TII' OR                                  
01302                         'FGL' OR                                  
01303                         'OFL'                                     
01304         NEXT SENTENCE                                             
01305      ELSE                                                         
01306         IF NOT PI-JNT-AGE-KEYED                                   
01307            MOVE AL-UANOF           TO BJNT-AGE-ATTRB.             
01308                                                                   
           if not pi-vin-keyed
              move al-uanof            to bvin-attrb
           end-if

01317      IF NOT PI-JNT-NAME-KEYED                                  
01318         MOVE AL-UANOF            TO BJNT-INIT-ATTRB             
01319                                     BJNT-LST-NAME-ATTRB         
01320                                     BJNT-1ST-NAME-ATTRB
                                          BJNTDOB-ATTRB
           end-if
01321                                                                   
030612     IF PI-COMPANY-ID = 'TMS' OR 'CID' OR 'DCC' or 'AHL' or 'VPP'
062121        OR 'FNL'
01323         NEXT SENTENCE                                             
01324      ELSE                                                         
01325         IF NOT PI-BENEFICIARY-KEYED                               
01326             MOVE AL-UANOF           TO BBENEFICIARY-ATTRB.        
01327                                                                   
01328      IF NOT PI-ALT-BEN-KEYED                                      
01329          MOVE AL-UANOF           TO BALT-BEN1-ATTRB
           END-IF
01330                                                                   
01331      IF NOT PI-ALT-PREM-KEYED                                     
01332          MOVE AL-UANOF           TO BALT-PREM1-ATTRB
           END-IF

020514     if pi-company-id = 'DCC' or 'VPP'
              IF NOT PI-ALT-BEN-KEYED
                 MOVE AL-UANOF         TO BALT-BEN2-ATTRB
              END-IF
              IF NOT PI-ALT-PREM-KEYED
                 MOVE AL-UANOF         TO BALT-PREM2-ATTRB
              END-IF
020514     end-if

01334      IF PI-PROCESSOR-ID = 'LGXX'                                  
CIDMOD        CONTINUE                                                  
01336      ELSE                                                         
01337         IF PI-COMPANY-ID = 'PEM' OR                               
01338                            'CGL' OR                               
01339                            'TIH' OR                               
01340                            'TII' OR                               
01341                            'FGL' OR                               
01342                            'OFL'                                  
01343            MOVE AL-UANOF            TO BADDRS2-ATTRB              
01344 *                                      BPHONE-ATTRB               
CIDMOD        ELSE
062121           IF (PI-COMPANY-ID = 'CID' or 'AHL' OR 'FNL') AND
CIDMOD              (PI-MAIL-YES)
CIDMOD              MOVE AL-UANOF         TO BADDRS2-ATTRB
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
01345                                                                   
CIDMOD     .
01346  0610-EXIT.                                                       
01347      EXIT.                                                        
01348                                                                   
01349      EJECT                                                        
01350                                                                   
01351  0700-PROTECT-FIELDS.                                             
01352                                                                   
01353      IF PI-COMPANY-ID = 'PEM' OR                                  
01354                         'CGL' OR                                  
01355                         'TIH' OR                                  
01356                         'TII' OR                                  
01357                         'FGL' OR                                  
01358                         'OFL'                                     
01359         NEXT SENTENCE                                             
01360      ELSE                                                         
01361         IF NOT PI-CAN-SUFFIX-KEYED                                
01362            MOVE AL-SANOF         TO CSFX-ATTRB (1)                
01363                                     CSFX-ATTRB (2)                
01364                                     CSFX-ATTRB (3)                
01365                                     CSFX-ATTRB (4).               
01366                                                                   
01367      IF PI-PROCESSOR-ID = 'LGXX'                                  
01368         IF NOT PI-CAN-SUFFIX-KEYED                                
01369            MOVE AL-SANOF         TO CSFX-ATTRB (1)                
01370                                     CSFX-ATTRB (2)                
01371                                     CSFX-ATTRB (3)                
01372                                     CSFX-ATTRB (4).               
01373                                                                   
062121     IF PI-COMPANY-ID = 'CSO' OR 'CID' or 'AHL' OR 'FNL'          
01375         MOVE AL-SANOF            TO CLAST-NAME-ATTRB (1)          
01376                                     CLAST-NAME-ATTRB (2)          
01377                                     CLAST-NAME-ATTRB (3)          
01378                                     CLAST-NAME-ATTRB (4).         
01379                                                                   
01380      IF NOT PI-CAN-LIVES-KEYED                                    
01381         MOVE AL-SANOF            TO CLIVES-ATTRB (1)              
01382                                     CLIVES-ATTRB (2)              
01383                                     CLIVES-ATTRB (3)              
01384                                     CLIVES-ATTRB (4).             
01385                                                                   
           IF NOT PI-CAN-REA-KEYED
              MOVE AL-SANOF            TO CCANREA-ATTRB (1)
                                          CCANREA-ATTRB (2)
                                          CCANREA-ATTRB (3)
                                          CCANREA-ATTRB (4)
           END-IF

01386 *    IF PI-COMPANY-ID = 'HER'                                     
01387 *        NEXT SENTENCE                                            
01388 *    ELSE                                                         
01389 *        IF NOT PI-MICRO-NO-KEYED                                 
01390 *            MOVE AL-SANOF       TO CMICRO-NO-ATTRB (1)           
01391 *                                   CMICRO-NO-ATTRB (2)           
01392 *                                   CMICRO-NO-ATTRB (3)           
01393 *                                   CMICRO-NO-ATTRB (4).          
01394                                                                   
01395      IF NOT PI-PAYEE-KEYED                                        
01396          MOVE AL-SANOF           TO CPAYEE-ATTRB (1)              
01397                                     CPAYEE-ATTRB (2)              
01398                                     CPAYEE-ATTRB (3)              
01399                                     CPAYEE-ATTRB (4).             
01400      IF NOT PI-CHK-REQ-KEYED                                      
01401          MOVE AL-SANOF           TO CCHK-ATTRB   (1)              
01402                                     CCHK-ATTRB   (2)              
01403                                     CCHK-ATTRB   (3)              
01404                                     CCHK-ATTRB   (4).             
01405                                                                   
01406      IF NOT PI-REFUND-MTHD-KEYED                                  
01407          MOVE AL-SANOF           TO CMTHD1-ATTRB (1)              
01408                                     CMTHD2-ATTRB (1)              
01409                                     CMTHD1-ATTRB (2)              
01410                                     CMTHD2-ATTRB (2)              
01411                                     CMTHD1-ATTRB (3)              
01412                                     CMTHD2-ATTRB (3)              
01413                                     CMTHD1-ATTRB (4)              
01414                                     CMTHD2-ATTRB (4).             
01415                                                                   
01416  0700-EXIT.                                                       
01417      EXIT.                                                        
01418                                                                   
01419      EJECT                                                        
01420  0710-UNPROTECT-FIELDS.                                           
01421      IF PI-COMPANY-ID = 'PEM' OR                                  
01422                         'CGL' OR                                  
01423                         'TIH' OR                                  
01424                         'TII' OR                                  
01425                         'FGL' OR                                  
01426                         'OFL'                                     
01427         NEXT SENTENCE                                             
01428      ELSE                                                         
01429         IF NOT PI-CAN-SUFFIX-KEYED                                
01430          MOVE AL-UANOF           TO CSFX-ATTRB (1)                
01431                                     CSFX-ATTRB (2)                
01432                                     CSFX-ATTRB (3)                
01433                                     CSFX-ATTRB (4).               
01434                                                                   
01435      IF PI-PROCESSOR-ID = 'LGXX'                                  
01436         IF NOT PI-CAN-SUFFIX-KEYED                                
01437            MOVE AL-UANOF         TO CSFX-ATTRB (1)                
01438                                     CSFX-ATTRB (2)                
01439                                     CSFX-ATTRB (3)                
01440                                     CSFX-ATTRB (4).               
01441                                                                   
062121     IF PI-COMPANY-ID = 'CSO' OR 'CID' or 'AHL' OR 'FNL'
01443         MOVE AL-UANOF            TO CLAST-NAME-ATTRB (1)          
01444                                     CLAST-NAME-ATTRB (2)          
01445                                     CLAST-NAME-ATTRB (3)          
01446                                     CLAST-NAME-ATTRB (4).         
01447                                                                   
01448      IF NOT PI-CAN-LIVES-KEYED                                    
01449          MOVE AL-UANOF           TO CLIVES-ATTRB (1)              
01450                                     CLIVES-ATTRB (2)              
01451                                     CLIVES-ATTRB (3)              
01452                                     CLIVES-ATTRB (4).             
01453                                                                   
           IF NOT PI-CAN-REA-KEYED
              MOVE AL-UANOF            TO CCANREA-ATTRB (1)
                                          CCANREA-ATTRB (2)
                                          CCANREA-ATTRB (3)
                                          CCANREA-ATTRB (4)
           END-IF

01454 *    IF PI-COMPANY-ID = 'HER'                                     
01455 *        NEXT SENTENCE                                            
01456 *    ELSE                                                         
01457 *        IF NOT PI-MICRO-NO-KEYED                                 
01458 *            MOVE AL-UNNOF       TO CMICRO-NO-ATTRB (1)           
01459 *                                   CMICRO-NO-ATTRB (2)           
01460 *                                   CMICRO-NO-ATTRB (3)           
01461 *                                   CMICRO-NO-ATTRB (4).          
01462                                                                   
01463      IF NOT PI-PAYEE-KEYED                                        
01464          MOVE AL-UANOF           TO CPAYEE-ATTRB (1)              
01465                                     CPAYEE-ATTRB (2)              
01466                                     CPAYEE-ATTRB (3)              
01467                                     CPAYEE-ATTRB (4).             
01468      IF NOT PI-CHK-REQ-KEYED                                      
01469          MOVE AL-UANOF           TO CCHK-ATTRB   (1)              
01470                                     CCHK-ATTRB   (2)              
01471                                     CCHK-ATTRB   (3)              
01472                                     CCHK-ATTRB   (4).             
01473                                                                   
01474      IF NOT PI-REFUND-MTHD-KEYED                                  
01475          MOVE AL-UANOF           TO CMTHD1-ATTRB (1)              
01476                                     CMTHD2-ATTRB (1)              
01477                                     CMTHD1-ATTRB (2)              
01478                                     CMTHD2-ATTRB (2)              
01479                                     CMTHD1-ATTRB (3)              
01480                                     CMTHD2-ATTRB (3)              
01481                                     CMTHD1-ATTRB (4)              
01482                                     CMTHD2-ATTRB (4).             
01483                                                                   
01484  0710-EXIT.                                                       
01485      EXIT.                                                        
01486      EJECT                                                        
01487  1000-EDIT-MAPB.                                                  
01488      IF PI-MAP-NAME NOT = EL630B                                  
01489          GO TO 1100-EDIT-MAPC.                                    
01490                                                                   
01491      IF PI-LAST-FUNC-DISPLAY                                      
01492        AND BSFX-LEN           = ZEROS                             
01493        AND B1ST-NAME-LEN      = ZEROS                             
01494        AND BLAST-NAME-LEN     = ZEROS                             
01495        AND BINIT-LEN          = ZEROS                             
01496        AND BJNT-1ST-NAME-LEN  = ZEROS                             
01497        AND BJNT-INIT-LEN      = ZEROS                             
01498        AND BJNT-LST-NAME-LEN  = ZEROS                             
01500        AND BAGE-LEN           = ZEROS                             
01502 *      AND BIND-GRP-LEN       = ZEROS                             
01503        AND BAPR-LEN           = ZEROS                             
01504 *      AND BFREQ-LEN          = ZEROS                             
01505 *      AND BSIG-LEN           = ZEROS                             
01506        AND BTERM1-LEN         = ZEROS                             
01507        AND BTERM2-LEN         = ZEROS                             
01508        AND BTYPE1-LEN         = ZEROS                             
01509        AND BTYPE2-LEN         = ZEROS                             
01510        AND BBENE1-LEN         = ZEROS                             
01511        AND BBENE2-LEN         = ZEROS                             
01512        AND BALT-BEN1-LEN      = ZEROS                             
01513        AND BPREM1-LEN         = ZEROS                             
01514        AND BPREM2-LEN         = ZEROS                             
01515        AND BALT-PREM1-LEN     = ZEROS                             
081606       AND BVIN-LEN           = ZEROS
01516 *      AND BLIVES-LEN         = ZEROS                             
01517 *      AND BPOLICY-LEN        = ZEROS                             
01518 *      AND BENTRY-LEN         = ZEROS                             
01519 *      AND BFORCE-LEN         = ZEROS                             
01520 *      AND BRINCD-LEN         = ZEROS                             
01521 *      AND BBILLCD-LEN        = ZEROS                             
01523 *      AND BMEM-NO-LEN        = ZEROS                             
01524 *      AND BMICROFILM-NO-LEN  = ZEROS                             
01525        AND BJNT-AGE-LEN       = ZEROS                             
01526        AND BBENEFICIARY-LEN   = ZEROS
             AND BCADDR1-LEN        = ZEROS
             AND BCADDR2-LEN        = ZEROS
             AND BCCITY-LEN         = ZEROS
             AND BCSTATE-LEN        = ZEROS
             AND BCZIPCD-LEN        = ZEROS
01527        AND BBIRTH-LEN         = ZEROS
             AND BJNTDOB-LEN        = ZEROS
01528 *      AND BMODE-LEN          = ZEROS                             
01529 *      AND BPMTS-LEN          = ZEROS                             
01530        AND BLN-OFFICER-LEN    = ZEROS                             
01531 *      AND BDAYS-LEN          = ZEROS                             
01532        AND BLN-TERM-LEN       = ZEROS                             
01533 *      AND BEXPIRE-LEN (1)    = ZEROS                             
01534 *      AND BEXPIRE-LEN (2)    = ZEROS                             
01535 *      AND BPMT-LEN           = ZEROS                             
01536        AND B1ST-PMT-LEN       = ZEROS                             
01537 *      AND BSKPCD-LEN         = ZEROS                             
01538        AND BADDRS1-LEN        = ZEROS                             
01539        AND BADDRS2-LEN        = ZEROS                             
01540        AND BCITY-LEN          = ZEROS                             
             AND BSTATE-LEN         = ZEROS
01541        AND BZIPCDE-LEN        = ZEROS                             
01542        AND BAGE-LEN           = ZEROS                             
01543 *      AND BZIP4-LEN          = ZEROS                             
01544 *      AND BPHONE-LEN         = ZEROS                             
01545          MOVE SPACE              TO PI-DISPLAY-SW                 
01546          GO TO 1030-NOTHING-TO-EDIT.                              
01547                                                                   
01548  1010-EDIT-MAPB.                                                  
01549      IF BCERT-LEN             = ZEROS                             
01550        AND BLAST-NAME-LEN     = ZEROS                             
01551        AND BEFFDT-LEN         = ZEROS                             
01552        AND NOT PI-LAST-FUNC-DISPLAY                               
01553          GO TO 1030-NOTHING-TO-EDIT.                              
01554                                                                   
01555      MOVE AL-SABON               TO BSEQ-ATTRB.                   
01556                                                                   
01557      IF BCERT-LEN  GREATER ZEROS                                  
01558        AND PI-LAST-FUNC-DISPLAY                                   
01559          NEXT SENTENCE                                            
01560      ELSE                                                         
01561          IF BCERT-LEN  GREATER ZEROS                              
01562              MOVE AL-UANON       TO BCERT-ATTRB                   
01563          ELSE                                                     
01564              MOVE -1             TO BCERT-LEN                     
01565              MOVE ER-2218        TO EMI-ERROR                     
01566              MOVE AL-UABON       TO BCERT-ATTRB                   
01567              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
01568                                                                   
01569      IF BSFX-LEN  NOT = ZEROS                                     
01570          MOVE 'Y'                TO PI-ISS-SUFFIX-KEYED-SW        
01571          MOVE AL-UANON           TO BSFX-ATTRB.                   
01572                                                                   
01573      IF BEFFDT-LEN  = ZEROS                                       
01574        AND PI-LAST-FUNC-DISPLAY                                   
01575          NEXT SENTENCE                                            
01576      ELSE                                                         
01577          IF BEFFDT-LEN   GREATER ZEROS                            
01578              MOVE AL-UNNON           TO BEFFDT-ATTRB              
01579              IF BEFFDT   NUMERIC                                  
01580                  MOVE 4              TO DC-OPTION-CODE            
01581                  MOVE BEFFDT    TO DC-GREG-DATE-1-MDY             
01582                  PERFORM 8500-DATE-CONVERT THRU 8500-EXIT         
01583                  MOVE DC-BIN-DATE-1  TO WS-CONVERTED-EFFDT        
01584                  IF NO-CONVERSION-ERROR                           
01585                      IF WS-CONVERTED-EFFDT NOT LESS               
01586                        PI-ACCT-LOW-EFF-DT  AND LESS               
01587                        PI-ACCT-HIGH-EXP-DT                        
01588                          PERFORM 1500-EDIT-ACCT-DT-RANGES THRU    
01589                                  1590-EXIT                        
01590                      ELSE                                         
01591                          MOVE 'Y' TO PI-FIN-RESP-ERROR-SW         
01592 *                        MOVE -1       TO BEFFDT-LEN              
01593 *                        MOVE ER-2589  TO EMI-ERROR               
01594 *                        MOVE AL-UNBON TO BEFFDT-ATTRB            
01595 *                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT 
01596                  ELSE                                             
01597                      MOVE -1         TO BEFFDT-LEN                
01598                      MOVE ER-2226    TO EMI-ERROR                 
01599                      MOVE AL-UNBON   TO BEFFDT-ATTRB              
01600                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
01601              ELSE                                                 
01602                  MOVE -1             TO BEFFDT-LEN                
01603                  MOVE ER-2223        TO EMI-ERROR                 
01604                  MOVE AL-UNBON       TO BEFFDT-ATTRB              
01605                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
01606          ELSE                                                     
01607              MOVE -1                 TO BEFFDT-LEN                
01608              MOVE ER-2220            TO EMI-ERROR                 
01609              MOVE AL-UNBON           TO BEFFDT-ATTRB              
01610              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
01611                                                                   
01612 *    IF PI-COMPANY-ID = ('PEM' OR 'CRI')                          
01613 *      AND NOT PI-LAST-FUNC-DISPLAY                               
01614 *       IF BCERT      = BCERTV   AND                              
01615 *          BSFX       = BSFXV    AND                              
01616 *          BEFFDT     = BEFFDTV  AND                              
01617 *          BLAST-NAME = BLAST-NAMEV                               
01618 *          NEXT SENTENCE                                          
01619 *       ELSE                                                      
01620 *          MOVE -1               TO BCERT-LEN                     
01621 *          MOVE -1               TO BLAST-NAME-LEN                
01622 *          MOVE ER-3166          TO EMI-ERROR                     
01623 *          MOVE AL-UNBON         TO BCERT-ATTRB                   
01624 *                                   BCERTV-ATTRB                  
01625 *                                   BSFX-ATTRB                    
01626 *                                   BSFXV-ATTRB                   
01627 *                                   BEFFDT-ATTRB                  
01628 *                                   BEFFDTV-ATTRB                 
01629 *                                   BLAST-NAME-ATTRB              
01630 *                                   BLAST-NAMEV-ATTRB             
01631 *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01632                                                                   
01633      IF BLAST-NAME-LEN   GREATER ZEROS                            
01634          MOVE AL-UANON           TO BLAST-NAME-ATTRB.             
01635                                                                   
01636      IF B1ST-NAME-LEN    GREATER ZEROS                            
01637          MOVE AL-UANON           TO B1ST-NAME-ATTRB.              
01638                                                                   
01639      IF BINIT-LEN        GREATER ZEROS                            
01640          MOVE AL-UANON           TO BINIT-ATTRB.                  
01641                                                                   
01651      IF BAGE-LEN > 0
01652         MOVE 'Y'                 TO PI-AGE-KEYED-SW               
01653         IF BAGE NUMERIC                                           
01654            MOVE BAGE             TO WS-BAGE                       
01655            MOVE AL-UNNON         TO BAGE-ATTRB                    
01656         ELSE                                                      
01657            MOVE -1             TO BAGE-LEN                        
01658            MOVE ER-2223        TO EMI-ERROR                       
01659            MOVE AL-UNBON       TO BAGE-ATTRB                      
01660            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01665                                                                   
01666      MOVE +0                     TO WS-SUB1

           .
01670  1020-EDIT-COVERAGES.                                             
01671      IF NOT MODIFY-CAP                                            
01672           MOVE 'UPDATE'       TO SM-READ                          
01673           PERFORM 9995-SECURITY-VIOLATION                         
01674           MOVE ER-0070        TO EMI-ERROR                        
01675           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                
01676           GO TO 8100-SEND-INITIAL-MAP.                            
01677                                                                   
01678      ADD +1                      TO WS-SUB1.                      
01679                                                                   
01680 *    IF WS-SUB1 GREATER +2                                        
01681 *       GO TO 1025-CONT-EDIT.                                     
01682                                                                   
01683 *    IF BTYPE1-LEN       > ZEROS OR
      *       BTYPE2-LEN       > ZEROS OR
01684 *       BTERM1-LEN       > ZEROS OR
01684 *       BTERM2-LEN       > ZEROS OR
01685 *       BBENE1-LEN       > ZEROS OR
01685 *       BBENE2-LEN       > ZEROS OR
01686 *       BPREM1-LEN       > ZEROS OR
01686 *       BPREM2-LEN       > ZEROS OR
01687 *       BCRIT-PERD2-LEN  > ZEROS OR
01688 *       BEXPIRE-LEN      > ZEROS OR
01689 *       BALT-PREM1-LEN   > ZEROS OR
01690 *       BALT-BEN1-LEN    > ZEROS OR
01689 *       BALT-PREM2-LEN   > ZEROS OR
01690 *       BALT-BEN2-LEN    > ZEROS
01691 *       MOVE 'Y'                 TO WS-DATA-KEYED-SW
01692 *    ELSE
01693 *       GO TO 1020-EDIT-COVERAGES
      *    END-IF

01683      IF BTYPE1-LEN       > ZEROS OR
01684         BTERM1-LEN       > ZEROS OR
01685         BBENE1-LEN       > ZEROS OR
01686         BPREM1-LEN       > ZEROS OR
01689         BALT-PREM1-LEN   > ZEROS OR
01690         BALT-BEN1-LEN    > ZEROS
01691         MOVE 'Y'                 TO WS-DATA-KEYED-SW
01692      ELSE
01693         GO TO 1020-EDIT-BENEFIT-2
           END-IF

           MOVE +1                     TO WS-SUB1
01695      IF NOT PI-LAST-FUNC-DISPLAY                                  
01696         IF BTYPE1-LEN  > ZEROS                     
01697            MOVE AL-UANON         TO BTYPE1-ATTRB
01698            PERFORM 1040-EDIT-INPUT-CODE
                                       THRU 1059-EXIT          
01699         END-IF                                                    
01701      ELSE                                                         
01702         IF BTYPE1-LEN > ZEROS                     
01703            IF BTYPE1 = SPACES OR ZEROS                 
01704               MOVE AL-UANON      TO BTYPE1-ATTRB
01705            ELSE                                                   
01706               MOVE AL-UANON      TO BTYPE1-ATTRB
01707               PERFORM 1040-EDIT-INPUT-CODE
                                       THRU 1059-EXIT
                 END-IF
              END-IF
           END-IF

01709 *    IF BPMTS-LEN GREATER ZEROS                                   
01710 *        MOVE 'Y'                TO PI-PMTS-KEYED-SW              
01711 *        MOVE BPMTS-IN           TO DEEDIT-FIELD                  
01712 *        PERFORM 8600-DEEDIT                                      
01713 *        IF DEEDIT-FIELD-V0 NUMERIC                               
01714 *           MOVE DEEDIT-FIELD-V0 TO WS-BPMTS                      
01715 *           MOVE AL-UNNON        TO BPMTS-ATTRB.                  
01716                                                                   
01717 *    IF BPMT-LEN GREATER ZEROS                                    
01718 *        MOVE 'Y'                TO PI-PMT-KEYED-SW               
01719 *        EXEC CICS BIF DEEDIT                                     
01720 *            FIELD  (BPMTI)                                       
01721 *            LENGTH (9)                                           
01722 *        END-EXEC                                                 
01723 *        IF BPMTI NUMERIC                                         
01724 *          MOVE BPMTI           TO WS-BPMT                        
01725 *          MOVE AL-UNNON        TO BPMT-ATTRB.                    
01726                                                                   
01727 *    IF BDAYS-LEN GREATER ZEROS                                   
01728 *       MOVE 'Y'                TO PI-DAYS-KEYED-SW               
01729 *       MOVE BDAYSI             TO DEEDIT-FIELD                   
01730 *       PERFORM 8600-DEEDIT                                       
01731 *       IF DEEDIT-FIELD-V0 NUMERIC                                
01732 *          MOVE DEEDIT-FIELD-V0  TO WS-BDAYS                      
01733 *          MOVE AL-UNNON        TO BDAYS-ATTRB                    
01734 *       ELSE                                                      
01735 *          MOVE -1              TO BDAYS-LEN                      
01736 *          MOVE ER-7530         TO EMI-ERROR                      
01737 *          MOVE AL-UNBON        TO BDAYS-ATTRB                    
01738 *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01739                                                                   
01740      IF BTERM1-LEN > ZEROS                         
01741         CONTINUE
01742      ELSE
01743         IF BLN-TERM-LEN > ZERO  AND                         
01744            WS-BLN-TERM NUMERIC                                    
01745            MOVE WS-BLN-TERM      TO BTERM1I
01746            MOVE +3               TO BTERM1-LEN
              END-IF
           END-IF

01748 *    IF  WS-TERM-IN-DAYS-FOUND  AND                               
01749 *        BMODE-LEN   GREATER ZERO                                 
01750 *          PERFORM 1090-CALCULATE-MONTHLY-TERM THRU 1094-EXIT     
01751 *       ELSE                                                      
01752 *          IF BMODE-LEN   GREATER ZEROS AND                       
01753 *             BPMTS-LEN   GREATER ZEROS                           
01754 *              PERFORM 1080-TERM-CONVERSION THRU 1089-EXIT.       
01755                                                                   
01756      IF PI-LAST-FUNC-DISPLAY                                      
01757         IF BTERM1-LEN  = ZEROS                           
01758            CONTINUE
01759         ELSE
01760            MOVE BTERM1I          TO DEEDIT-FIELD
01761            PERFORM 8600-DEEDIT                                    
01762            IF DEEDIT-FIELD-V0 NUMERIC                             
01763               MOVE DEEDIT-FIELD-V0
                                       TO WS-BTERM1
01764               IF WS-BTERM1 > ZERO                 
01765                  MOVE AL-UNNON   TO BTERM1-ATTRB
01766               ELSE                                                
01767                  MOVE ER-2241    TO EMI-ERROR                
01768                  MOVE -1         TO BTERM1-LEN
01769                  MOVE AL-UNBOF   TO BTERM1-ATTRB
01770                  PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                    END-IF
01771            ELSE                                                   
01772               MOVE ER-2223       TO EMI-ERROR
01773               MOVE -1            TO BTERM1-LEN
01774               MOVE AL-UNBON      TO BTERM1-ATTRB
01775               PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
              END-IF
01776      ELSE
01777         IF BTERM1-LEN > ZEROS              
01778            MOVE BTERM1I          TO DEEDIT-FIELD
01779            PERFORM 8600-DEEDIT                                   
01780            IF DEEDIT-FIELD-V0      NUMERIC                       
01781               IF DEEDIT-FIELD-V0 > ZERO
01782                  MOVE DEEDIT-FIELD-V0 TO WS-BTERM1
01783                  MOVE AL-UNNON        TO BTERM1-ATTRB
01784               ELSE
01785                  MOVE ER-2241         TO EMI-ERROR               
01786                  MOVE -1              TO BTERM1-LEN
01787                  MOVE AL-UNBOF        TO BTERM1-ATTRB
01788                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
01789            ELSE
01790               MOVE ER-2223             TO EMI-ERROR               
01791               MOVE -1                  TO BTERM1-LEN
01792               MOVE AL-UNBON            TO BTERM1-ATTRB
01793               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
01794         ELSE                                                      
100703           IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
01795               MOVE ER-2240             TO EMI-ERROR               
01796               MOVE -1                  TO BTERM1-LEN
01797               MOVE AL-UNBOF            TO BTERM1-ATTRB
01798               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF


01800      IF BBENE1-LEN = ZEROS
01801         AND PI-LAST-FUNC-DISPLAY                                 
01802         CONTINUE
01803      ELSE                                                         
01804         IF BBENE1-LEN > ZEROS                       
01805            MOVE AL-UNNON           TO BBENE1-ATTRB
01806            EXEC CICS BIF DEEDIT                                   
01807                FIELD  (BBENE1I)
01808                LENGTH (12)                                        
01809            END-EXEC                                               
01810            IF BBENE1I NUMERIC                             
01811               IF BBENE1I > ZEROS
01812                  MOVE BBENE1I TO WS-BBEN1
CIDMOD*          MOVE BBENE1I           TO DEEDIT-FIELD
CIDMOD*          PERFORM 8600-DEEDIT                                    
CIDMOD*          IF DEEDIT-FIELD-V2  NUMERIC                            
CIDMOD*             IF DEEDIT-FIELD-V2 > ZEROS                    
CIDMOD*                MOVE DEEDIT-FIELD-V2 TO WS-BBEN1
01813               ELSE                                                
01814                  MOVE ER-7632    TO EMI-ERROR                     
01815                  MOVE -1         TO BBENE1-LEN
01816                  MOVE AL-UNBOF   TO BBENE1-ATTRB
01817                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
01818            ELSE                                                   
01819               MOVE ER-2223         TO EMI-ERROR                   
01820               MOVE AL-UNBON        TO BBENE1-ATTRB
01821               MOVE -1              TO BBENE1-LEN
                 END-IF
01822         ELSE                                                      
                 IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
01823              MOVE ER-7632    TO EMI-ERROR                           
01824              MOVE -1         TO BBENE1-LEN
01825              MOVE AL-UNBOF   TO BBENE1-ATTRB
01826              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF


01828      IF BPREM1-LEN = ZEROS
01829         AND PI-LAST-FUNC-DISPLAY                                  
01830         CONTINUE
01831      ELSE                                                         
01832         IF BPREM1-LEN > ZEROS                      
01833            MOVE AL-UNNON           TO BPREM1-ATTRB
CIDMOD*          MOVE BPREM1I            TO DEEDIT-FIELD                
CIDMOD*          PERFORM 8600-DEEDIT                                    
CIDMOD*          IF DEEDIT-FIELD-V2  NUMERIC                            
CIDMOD*             IF DEEDIT-FIELD-V2 GREATER ZEROS                    
CIDMOD*                MOVE DEEDIT-FIELD-V2 TO WS-BPREM1
01834            EXEC CICS BIF DEEDIT                                   
01835                FIELD  (BPREM1I)                          
01836                LENGTH (11)                                        
01837            END-EXEC                                               
01838            IF BPREM1I NUMERIC                            
01839               IF BPREM1I > ZEROS                   
01840                  MOVE BPREM1I TO WS-BPREM1
01841               ELSE                                                
01842                  MOVE ER-7633    TO EMI-ERROR                     
01843                  MOVE -1         TO BPREM1-LEN
01844                  MOVE AL-UNBOF   TO BPREM1-ATTRB
01845                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
01846            ELSE                                                   
01847               MOVE AL-UNBON   TO BPREM1-ATTRB
01848               MOVE ER-2223         TO EMI-ERROR                   
01849               MOVE -1              TO BPREM1-LEN
                 END-IF
01850         ELSE                                                      
100703           IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
01851               MOVE ER-7633    TO EMI-ERROR
01852               MOVE -1         TO BPREM1-LEN
01853               MOVE AL-UNBOF   TO BPREM1-ATTRB
01854               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF

01890      IF BALT-BEN1-LEN = ZEROS                            
01891         AND PI-LAST-FUNC-DISPLAY                                  
01892         CONTINUE
01893      ELSE                                                         
01894         IF BALT-BEN1-LEN > ZEROS                   
01895            MOVE 'Y'              TO PI-ALT-BEN-KEYED-SW           
01896            MOVE AL-UNNON         TO BALT-BEN1-ATTRB
CIDMOD*          MOVE BALT-BEN1I       TO DEEDIT-FIELD            
CIDMOD*          PERFORM 8600-DEEDIT                                    
CIDMOD*          IF DEEDIT-FIELD-V2  NUMERIC                            
CIDMOD*             MOVE DEEDIT-FIELD-V2 TO WS-BALT-BEN1
01897            EXEC CICS BIF DEEDIT                                   
01898                FIELD  (BALT-BEN1I)                       
01899                LENGTH (12)                                        
01900            END-EXEC                                               
01901            IF BALT-BEN1I NUMERIC                         
01902               MOVE BALT-BEN1I TO WS-BALT-BEN1
01903            ELSE                                                   
01904               MOVE ER-2223       TO EMI-ERROR                     
01905               MOVE AL-UNBON      TO BALT-BEN1-ATTRB
01906               MOVE -1            TO BALT-BEN1-LEN
                 END-IF
              END-IF
           END-IF

01908      IF BALT-PREM1-LEN = ZEROS                        
01909         AND PI-LAST-FUNC-DISPLAY                                  
01910         NEXT SENTENCE                                          
01911      ELSE                                                         
01912         IF BALT-PREM1-LEN > ZEROS                
01913            MOVE 'Y'              TO PI-ALT-PREM-KEYED-SW          
01914            MOVE AL-UNNON         TO BALT-PREM1-ATTRB
CIDMOD*          MOVE BALT-PREM1I      TO DEEDIT-FIELD             
CIDMOD*          PERFORM 8600-DEEDIT                                    
CIDMOD*          IF DEEDIT-FIELD-V2  NUMERIC                            
CIDMOD*             MOVE DEEDIT-FIELD-V2 TO WS-BALT-PREM1
01915            EXEC CICS BIF DEEDIT                                   
01916                FIELD  (BALT-PREM1I)                      
01917                LENGTH (9)                                         
01918            END-EXEC                                               
01919            IF BALT-PREM1I NUMERIC                        
01920               MOVE BALT-PREM1I TO WS-BALT-PREM1
01921            ELSE                                                   
01922               MOVE AL-UNBON      TO BALT-PREM1-ATTRB
01923               MOVE ER-2223       TO EMI-ERROR
01924               MOVE -1            TO BALT-PREM1-LEN
                 END-IF
              END-IF
           END-IF

           .
       1020-EDIT-BENEFIT-2.

           IF BTYPE2-LEN       > ZEROS OR
01684         BTERM2-LEN       > ZEROS OR
01685         BBENE2-LEN       > ZEROS OR
01686         BPREM2-LEN       > ZEROS OR
01687         BCRIT-PERD2-LEN  > ZEROS OR
01689         BALT-PREM2-LEN   > ZEROS OR
01690         BALT-BEN2-LEN    > ZEROS
01691         MOVE 'Y'                 TO WS-DATA-KEYED-SW
01692      ELSE
01693         GO TO 1025-CONT-EDIT
           END-IF

           MOVE +2                     TO WS-SUB1
01695      IF NOT PI-LAST-FUNC-DISPLAY                                  
01696         IF BTYPE2-LEN  > ZEROS                     
01697            MOVE AL-UANON         TO BTYPE2-ATTRB
01698            PERFORM 1040-EDIT-INPUT-CODE
                                       THRU 1059-EXIT          
01699         END-IF                                                    
01701      ELSE                                                         
01702         IF BTYPE2-LEN > ZEROS                     
01703            IF BTYPE2 = SPACES OR ZEROS                 
01704               MOVE AL-UANON      TO BTYPE2-ATTRB
01705            ELSE                                                   
01706               MOVE AL-UANON      TO BTYPE2-ATTRB
01707               PERFORM 1040-EDIT-INPUT-CODE
                                       THRU 1059-EXIT
                 END-IF
              END-IF
           END-IF

01740      IF BTERM2-LEN > ZEROS                         
01741         CONTINUE
01742      ELSE
01743         IF BLN-TERM-LEN > ZERO  AND                         
01744            WS-BLN-TERM NUMERIC                                    
01745            MOVE WS-BLN-TERM      TO BTERM2I
01746            MOVE +3               TO BTERM2-LEN
              END-IF
           END-IF

01756      IF PI-LAST-FUNC-DISPLAY                                      
01757         IF BTERM2-LEN  = ZEROS                           
01758            CONTINUE
01759         ELSE
01760            MOVE BTERM2I          TO DEEDIT-FIELD
01761            PERFORM 8600-DEEDIT                                    
01762            IF DEEDIT-FIELD-V0 NUMERIC                             
01763               MOVE DEEDIT-FIELD-V0
                                       TO WS-BTERM2
01764               IF WS-BTERM2 > ZERO                 
01765                  MOVE AL-UNNON   TO BTERM2-ATTRB
01766               ELSE                                                
01767                  MOVE ER-2241    TO EMI-ERROR                
01768                  MOVE -1         TO BTERM2-LEN
01769                  MOVE AL-UNBOF   TO BTERM2-ATTRB
01770                  PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                    END-IF
01771            ELSE                                                   
01772               MOVE ER-2223       TO EMI-ERROR
01773               MOVE -1            TO BTERM2-LEN
01774               MOVE AL-UNBON      TO BTERM2-ATTRB
01775               PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
              END-IF
01776      ELSE
01777         IF BTERM2-LEN > ZEROS              
01778            MOVE BTERM2I          TO DEEDIT-FIELD
01779            PERFORM 8600-DEEDIT                                   
01780            IF DEEDIT-FIELD-V0      NUMERIC                       
01781               IF DEEDIT-FIELD-V0 > ZERO
01782                  MOVE DEEDIT-FIELD-V0 TO WS-BTERM2
01783                  MOVE AL-UNNON        TO BTERM2-ATTRB
01784               ELSE
01785                  MOVE ER-2241         TO EMI-ERROR               
01786                  MOVE -1              TO BTERM2-LEN
01787                  MOVE AL-UNBOF        TO BTERM2-ATTRB
01788                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
01789            ELSE
01790               MOVE ER-2223             TO EMI-ERROR               
01791               MOVE -1                  TO BTERM2-LEN
01792               MOVE AL-UNBON            TO BTERM2-ATTRB
01793               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
01794         ELSE                                                      
100703           IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
01795               MOVE ER-2240             TO EMI-ERROR               
01796               MOVE -1                  TO BTERM2-LEN
01797               MOVE AL-UNBOF            TO BTERM2-ATTRB
01798               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF

01800      IF BBENE2-LEN = ZEROS
01801         AND PI-LAST-FUNC-DISPLAY                                 
01802         CONTINUE
01803      ELSE                                                         
01804         IF BBENE2-LEN > ZEROS                       
01805            MOVE AL-UNNON           TO BBENE2-ATTRB
01806            EXEC CICS BIF DEEDIT                                   
01807                FIELD  (BBENE2I)                           
01808                LENGTH (12)                                        
01809            END-EXEC                                               
01810            IF BBENE2I NUMERIC                             
01811               IF BBENE2I > ZEROS
01812                  MOVE BBENE2I TO WS-BBEN2
CIDMOD*          MOVE BBENE2I           TO DEEDIT-FIELD
CIDMOD*          PERFORM 8600-DEEDIT                                    
CIDMOD*          IF DEEDIT-FIELD-V2  NUMERIC                            
CIDMOD*             IF DEEDIT-FIELD-V2 > ZEROS                    
CIDMOD*                MOVE DEEDIT-FIELD-V2 TO WS-BBEN2
01813               ELSE                                                
01814                  MOVE ER-7632    TO EMI-ERROR                     
01815                  MOVE -1         TO BBENE2-LEN
01816                  MOVE AL-UNBOF   TO BBENE2-ATTRB
01817                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
01818            ELSE                                                   
01819               MOVE ER-2223         TO EMI-ERROR                   
01820               MOVE AL-UNBON        TO BBENE2-ATTRB
01821               MOVE -1              TO BBENE2-LEN
                 END-IF
01822         ELSE                                                      
                 IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
01823              MOVE ER-7632    TO EMI-ERROR                           
01824              MOVE -1         TO BBENE2-LEN
01825              MOVE AL-UNBOF   TO BBENE2-ATTRB
01826              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF

01828      IF BPREM2-LEN = ZEROS
01829         AND PI-LAST-FUNC-DISPLAY                                  
01830         CONTINUE
01831      ELSE                                                         
01832         IF BPREM2-LEN > ZEROS                      
01833            MOVE AL-UNNON           TO BPREM2-ATTRB
CIDMOD*          MOVE BPREM2I            TO DEEDIT-FIELD                
CIDMOD*          PERFORM 8600-DEEDIT                                    
CIDMOD*          IF DEEDIT-FIELD-V2  NUMERIC                            
CIDMOD*             IF DEEDIT-FIELD-V2 GREATER ZEROS                    
CIDMOD*                MOVE DEEDIT-FIELD-V2 TO WS-BPREM2
01834            EXEC CICS BIF DEEDIT                                   
01835                FIELD  (BPREM2I)                          
01836                LENGTH (11)                                        
01837            END-EXEC                                               
01838            IF BPREM2I NUMERIC                            
01839               IF BPREM2I > ZEROS                   
01840                  MOVE BPREM2I TO WS-BPREM2
01841               ELSE                                                
01842                  MOVE ER-7633    TO EMI-ERROR                     
01843                  MOVE -1         TO BPREM2-LEN
01844                  MOVE AL-UNBOF   TO BPREM2-ATTRB
01845                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
01846            ELSE                                                   
01847               MOVE AL-UNBON   TO BPREM2-ATTRB
01848               MOVE ER-2223         TO EMI-ERROR                   
01849               MOVE -1              TO BPREM2-LEN
                 END-IF
01850         ELSE                                                      
100703           IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
01851               MOVE ER-7633    TO EMI-ERROR
01852               MOVE -1         TO BPREM2-LEN
01853               MOVE AL-UNBOF   TO BPREM2-ATTRB
01854               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF

01856 *    IF BEXPIRE-LEN (WS-SUB1)   GREATER ZEROS                     
01857 *       MOVE 'Y'                 TO PI-EXPIRE-KEYED-SW            
01858 *        IF BEXPIRE (WS-SUB1)    NUMERIC                          
01859 *            MOVE AL-UNNON       TO BEXPIRE-ATTRB (WS-SUB1)       
01860 *            MOVE 4              TO DC-OPTION-CODE                
01861 *            MOVE BEXPIRE (WS-SUB1)   TO DC-GREG-DATE-1-MDY       
01862 *            PERFORM 8500-DATE-CONVERT THRU 8500-EXIT             
01863 *            MOVE DC-BIN-DATE-1  TO WS-CONVERTED-EXPIRDT (WS-SUB1)
01864 *            IF NO-CONVERSION-ERROR                               
01865 *                NEXT SENTENCE                                    
01866 *            ELSE                                                 
01867 *                MOVE -1         TO BEXPIRE-LEN   (WS-SUB1)       
01868 *                MOVE ER-2531    TO EMI-ERROR                     
01869 *                MOVE AL-UNBON   TO BEXPIRE-ATTRB (WS-SUB1)       
01870 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
01871 *        ELSE                                                     
01872 *            MOVE -1             TO BEXPIRE-LEN   (WS-SUB1)       
01873 *            MOVE ER-2532        TO EMI-ERROR                     
01874 *            MOVE AL-UNBON       TO BEXPIRE-ATTRB (WS-SUB1)       
01875 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
01876                                                                   
01877      IF BCRIT-PERD2-LEN > ZEROS                
01878         MOVE 'Y'                     TO PI-CRIT-PERD-KEYED-SW     
01879         MOVE BCRIT-PERD2I            TO DEEDIT-FIELD              
01880         PERFORM 8600-DEEDIT                                       
01881         IF DEEDIT-FIELD-V0 NUMERIC                                
01882            MOVE DEEDIT-FIELD-V0      TO WS-BCRIT-PERD2
01883            MOVE AL-UNNON             TO BCRIT-PERD2-ATTRB
01884         ELSE                                                     
01885            MOVE -1                   TO BCRIT-PERD2-LEN
01886            MOVE AL-UNBON             TO BCRIT-PERD2-ATTRB
01887            MOVE ER-2223              TO EMI-ERROR                 
01888            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF
01889                                                                   
01890      IF BALT-BEN2-LEN = ZEROS                            
01891         AND PI-LAST-FUNC-DISPLAY                                  
01892         CONTINUE
01893      ELSE                                                         
01894         IF BALT-BEN2-LEN > ZEROS                   
01895            MOVE 'Y'              TO PI-ALT-BEN-KEYED-SW           
01896            MOVE AL-UNNON         TO BALT-BEN2-ATTRB
CIDMOD*          MOVE BALT-BEN2I       TO DEEDIT-FIELD            
CIDMOD*          PERFORM 8600-DEEDIT                                    
CIDMOD*          IF DEEDIT-FIELD-V2  NUMERIC                            
CIDMOD*             MOVE DEEDIT-FIELD-V2 TO WS-BALT-BEN2
01897            EXEC CICS BIF DEEDIT                                   
01898                FIELD  (BALT-BEN2I)                       
01899                LENGTH (12)                                        
01900            END-EXEC                                               
01901            IF BALT-BEN2I NUMERIC                         
01902               MOVE BALT-BEN2I TO WS-BALT-BEN2
01903            ELSE                                                   
01904               MOVE ER-2223       TO EMI-ERROR                     
01905               MOVE AL-UNBON      TO BALT-BEN2-ATTRB
01906               MOVE -1            TO BALT-BEN2-LEN
                 END-IF
              END-IF
           END-IF

01908      IF BALT-PREM2-LEN = ZEROS                        
01909         AND PI-LAST-FUNC-DISPLAY                                  
01910         NEXT SENTENCE                                          
01911      ELSE                                                         
01912         IF BALT-PREM2-LEN > ZEROS                
01913            MOVE 'Y'              TO PI-ALT-PREM-KEYED-SW          
01914            MOVE AL-UNNON         TO BALT-PREM2-ATTRB
CIDMOD*          MOVE BALT-PREM2I      TO DEEDIT-FIELD             
CIDMOD*          PERFORM 8600-DEEDIT                                    
CIDMOD*          IF DEEDIT-FIELD-V2  NUMERIC                            
CIDMOD*             MOVE DEEDIT-FIELD-V2 TO WS-BALT-PREM2
01915            EXEC CICS BIF DEEDIT                                   
01916                FIELD  (BALT-PREM2I)                      
01917                LENGTH (9)                                         
01918            END-EXEC                                               
01919            IF BALT-PREM2I NUMERIC                        
01920               MOVE BALT-PREM2I TO WS-BALT-PREM2
01921            ELSE                                                   
01922               MOVE AL-UNBON      TO BALT-PREM2-ATTRB
01923               MOVE ER-2223       TO EMI-ERROR
01924               MOVE -1            TO BALT-PREM2-LEN
                 END-IF
              END-IF
           END-IF

01926 *    GO TO 1020-EDIT-COVERAGES.                                   
           .
01928  1025-CONT-EDIT.                                                  
01929 *    IF BLIVES-LEN               GREATER ZEROS                    
01930 *       MOVE 'Y'                   TO PI-ISS-LIVES-KEYED-SW       
01931 *       MOVE BLIVESI               TO DEEDIT-FIELD                
01932 *       PERFORM 8600-DEEDIT                                       
01933 *       IF DEEDIT-FIELD-V0 NUMERIC                                
01934 *          MOVE DEEDIT-FIELD-V0    TO WS-BLIVES                   
01935 *          MOVE AL-UNNON           TO BLIVES-ATTRB                
01936 *       ELSE                                                      
01937 *          MOVE -1                 TO BLIVES-LEN                  
01938 *          MOVE AL-UNBON           TO BLIVES-ATTRB                
01939 *          MOVE ER-2223            TO EMI-ERROR                   
01940 *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              

           IF (BSTATE-LEN > 0)
              AND (BSTATE NOT = '  ' AND '00')
              MOVE AL-UANON            TO BSTATE-ATTRB
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE BSTATE              TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              EXEC CICS READ
                 DATASET   (FILE-ID-ELCNTL)
                 SET       (ADDRESS OF CONTROL-FILE)
                 RIDFLD    (ELCNTL-KEY)
                 RESP      (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 CONTINUE
              ELSE
                 MOVE ER-2963          TO EMI-ERROR
      *          MOVE -1               TO BSTATE-LEN
                 MOVE -1               TO BPFENTRL
                 MOVE AL-UABON         TO BSTATE-ATTRB
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 SUBTRACT +1 FROM EMI-FATAL-CTR
              END-IF
      *    ELSE
      *       MOVE ER-2209          TO EMI-ERROR
      *       MOVE -1               TO BSTATE-LEN
      *       MOVE AL-UABON         TO BSTATE-ATTRB
      *       PERFORM 9900-ERROR-FORMAT
      *                             THRU 9900-EXIT
      *       SUBTRACT +1 FROM EMI-FATAL-CTR
           END-IF

           IF BCSTATE-LEN > 0
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE BCSTATE             TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              EXEC CICS READ
                 DATASET   (FILE-ID-ELCNTL)
                 SET       (ADDRESS OF CONTROL-FILE)
                 RIDFLD    (ELCNTL-KEY)
                 RESP      (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 CONTINUE
              ELSE
                 MOVE ER-2964          TO EMI-ERROR
      *          MOVE -1               TO BCSTATE-LEN
                 MOVE -1               TO BPFENTRL
                 MOVE AL-UABON         TO BCSTATE-ATTRB
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 SUBTRACT +1 FROM EMI-FATAL-CTR
              END-IF
           END-IF

           if bvin-len <> zeros
              move 'Y'                 to pi-vin-keyed-sw
              move al-uanon            to bvin-attrb
           end-if

01942      IF BJNT-1ST-NAME-LEN     GREATER ZEROS OR                    
01943         BJNT-INIT-LEN         GREATER ZEROS OR                    
01944         BJNT-LST-NAME-LEN     GREATER ZEROS or
              BJNTDOB-len           greater zeros
01945          MOVE 'Y'                TO PI-JNT-NAME-KEYED-SW          
01946          MOVE AL-UANON           TO BJNT-1ST-NAME-ATTRB           
01947                                     BJNT-INIT-ATTRB               
01948                                     BJNT-LST-NAME-ATTRB
                                          BJNTDOB-ATTRB.
01949                                                                   
01950      IF BJNT-AGE-LEN GREATER ZEROS                                
01951         MOVE 'Y'                 TO PI-JNT-AGE-KEYED-SW           
01952         IF BJNT-AGE NUMERIC                                       
01953            MOVE BJNT-AGE         TO WS-BJNT-AGE                   
01954            MOVE AL-UNNON         TO BJNT-AGE-ATTRB                
01955         ELSE                                                      
01956            MOVE -1             TO BJNT-AGE-LEN                    
01957            MOVE ER-2223        TO EMI-ERROR                       
01958            MOVE AL-UNBON       TO BJNT-AGE-ATTRB                  
01959            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01960                                                                   
01961 *    IF BMICROFILM-NO-LEN  GREATER  ZEROS                         
01962 *        MOVE 'Y'                TO  PI-MICRO-NO-KEYED-SW         
01963 *        MOVE BMICROFILM-NOI     TO  DEEDIT-FIELD                 
01964 *        PERFORM 8600-DEEDIT                                      
01965 *        IF DEEDIT-FIELD-V0  NUMERIC                              
01966 *            MOVE DEEDIT-FIELD-V0                                 
01967 *                                TO  WS-I-MICRO-NO                
01968 *            MOVE AL-UNNON       TO  BMICROFILM-NO-ATTRB          
01969 *        ELSE                                                     
01970 *            MOVE -1             TO  BMICROFILM-NO-LEN            
01971 *            MOVE AL-UNBON       TO  BMICROFILM-NO-ATTRB          
01972 *            MOVE ER-2701        TO  EMI-ERROR                    
01973 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
01974                                                                   
01975      IF BBENEFICIARY-LEN GREATER ZEROS                            
01976          MOVE 'Y'                TO PI-BENEFICIARY-KEYED-SW       
01977          MOVE AL-UANON           TO BBENEFICIARY-ATTRB.           
01978                                                                   
01979      IF B1ST-PMT-LEN GREATER ZEROS                                
01980         MOVE 'Y'                     TO PI-1ST-PMT-KEYED-SW       
01981         IF B1ST-PMT = SPACES                                      
01982            MOVE LOW-VALUES           TO WS-CONVERTED-1ST-PMT-DT   
01983         ELSE                                                      
01984            MOVE B1ST-PMT             TO DEEDIT-FIELD              
01985            PERFORM 8600-DEEDIT                                    
01986            MOVE DEEDIT-FIELD-V0      TO DC-GREG-DATE-1-MDY        
01987            MOVE AL-UNNON             TO B1ST-PMT-ATTRB            
01988            MOVE 4                    TO DC-OPTION-CODE            
01989            PERFORM 8500-DATE-CONVERT                              
01990            IF NO-CONVERSION-ERROR                                 
01991               MOVE DC-BIN-DATE-1     TO WS-CONVERTED-1ST-PMT-DT   
01992               MOVE AL-UANON          TO B1ST-PMT-ATTRB            
01993            ELSE                                                   
01994               MOVE -1                TO B1ST-PMT-LEN              
01995               MOVE ER-2200           TO EMI-ERROR                 
01996               MOVE AL-UNBON          TO B1ST-PMT-ATTRB            
01997               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           
01998                                                                   
01999      IF BLN-TERM-LEN GREATER ZEROS                                
02000         MOVE 'Y'                 TO PI-LNTRM-KEYED-SW             
02001         MOVE BLN-TERMI           TO DEEDIT-FIELD                  
02002         PERFORM 8600-DEEDIT                                       
02003         IF DEEDIT-FIELD-V0 NUMERIC                                
02004            MOVE DEEDIT-FIELD-V0  TO WS-BLN-TERM                   
02005            IF WS-BLN-TERM  GREATER ZERO                           
02006               MOVE AL-UNNON TO BLN-TERM-ATTRB                     
02007                  ELSE                                             
02008                      MOVE ER-2241  TO EMI-ERROR                   
02009                      MOVE -1       TO BLN-TERM-LEN                
02010                      MOVE AL-UNBOF TO BLN-TERM-ATTRB              
02011                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
02012              ELSE                                                 
02013                  MOVE ER-2223      TO EMI-ERROR                   
02014                  MOVE -1           TO BLN-TERM-LEN                
02015                  MOVE AL-UNBON     TO BLN-TERM-ATTRB              
02016                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        
02017                                                                   
02018      IF BLN-OFFICER-LEN GREATER ZEROS                             
02019          MOVE 'Y'                TO PI-LN-OFFICER-KEYED-SW        
02020          MOVE AL-UANON           TO BLN-OFFICER-ATTRB.            
02021                                                                   
02022 *    IF BMODE-LEN GREATER ZEROS                                   
02023 *        MOVE BMODE              TO WS-MODE-CODE                  
02024 *        MOVE 'Y'                TO PI-MODE-KEYED-SW              
02025 *        IF WS-MODE-CODE-VALID                                    
02026 *            MOVE AL-UANON       TO BMODE-ATTRB                   
02027 *        ELSE                                                     
02028 *            MOVE -1             TO BMODE-LEN                     
02029 *            MOVE ER-2591        TO EMI-ERROR                     
02030 *            MOVE AL-UABON       TO BMODE-ATTRB                   
02031 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
02032                                                                   
02033 *    IF BFREQ-LEN GREATER ZEROS                                   
02034 *       MOVE 'Y'                 TO PI-FREQ-KEYED-SW              
02035 *       MOVE BFREQI              TO DEEDIT-FIELD                  
02036 *       PERFORM 8600-DEEDIT                                       
02037 *       IF DEEDIT-FIELD-V0 NUMERIC                                
02038 *          MOVE DEEDIT-FIELD-V0  TO WS-BFREQ                      
02039 *          MOVE AL-UNNON         TO BFREQ-ATTRB                   
02040 *       ELSE                                                      
02041 *          MOVE -1             TO BFREQ-LEN                       
02042 *          MOVE ER-2223        TO EMI-ERROR                       
02043 *          MOVE AL-UNBON       TO BFREQ-ATTRB                     
02044 *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
02045                                                                   
02046 *    IF BPMTS-LEN GREATER ZEROS                                   
02047 *        MOVE 'Y'                TO PI-PMTS-KEYED-SW              
02048 *        MOVE BPMTS-IN           TO DEEDIT-FIELD                  
02049 *        PERFORM 8600-DEEDIT                                      
02050 *        IF DEEDIT-FIELD-V0 NUMERIC                               
02051 *           MOVE DEEDIT-FIELD-V0 TO WS-BPMTS                      
02052 *           MOVE AL-UNNON        TO BPMTS-ATTRB                   
02053 *        ELSE                                                     
02054 *            MOVE -1             TO BPMTS-LEN                     
02055 *            MOVE ER-2592        TO EMI-ERROR                     
02056 *            MOVE AL-UNBON       TO BPMTS-ATTRB                   
02057 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
02058 *                                                                 
02059 *    IF BPMT-LEN GREATER ZEROS                                    
02060 *        MOVE 'Y'                TO PI-PMT-KEYED-SW               
CIDMOD*        MOVE BPMTI              TO DEEDIT-FIELD                  
CIDMOD*        PERFORM 8600-DEEDIT                                      
CIDMOD*        IF DEEDIT-FIELD-V2     NUMERIC                           
CIDMOD*          MOVE DEEDIT-FIELD-V2 TO WS-BPMT                        
CIDMOD*          MOVE AL-UNNON           TO BPMT-ATTRB                  
02061 *        EXEC CICS BIF DEEDIT                                     
02062 *            FIELD  (BPMTI)                                       
02063 *            LENGTH (9)                                           
02064 *        END-EXEC                                                 
02065 *        IF BPMTI NUMERIC                                         
02066 *          MOVE BPMTI              TO WS-BPMT                     
02067 *          MOVE AL-UNNON           TO BPMT-ATTRB                  
02068 *        ELSE                                                     
02069 *          MOVE -1                 TO BPMT-LEN                    
02070 *          MOVE ER-2529            TO EMI-ERROR                   
02071 *          MOVE AL-UNBON           TO BPMT-ATTRB                  
02072 *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
02073                                                                   
02074 *    IF BRINCD-LEN              GREATER ZEROS                     
02075 *        MOVE 'Y'                TO PI-RINCD-KEYED-SW             
02076 *        MOVE AL-UANON           TO BRINCD-ATTRB.                 
02077 *                                                                 
02078 *    IF BENTRY-LEN           GREATER ZEROS                        
02079 *        MOVE 'Y'                TO PI-ENTRY-KEYED-SW             
02080 *        MOVE BENTRY             TO WS-ENTRY-CODE                 
02081 *        IF WS-ENTRY-CODE-VALID                                   
02082 *           MOVE AL-UANON       TO BENTRY-ATTRB                   
02083 *        ELSE                                                     
02084 *           MOVE ER-2224        TO EMI-ERROR                      
02085 *           MOVE -1             TO BENTRY-LEN                     
02086 *           MOVE AL-UABON       TO BENTRY-ATTRB                   
02087 *           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
02088                                                                   
02089 *    IF BFORCE-LEN           GREATER ZEROS                        
02090 *        MOVE 'Y'                TO PI-FORCE-KEYED-SW             
02091 *        MOVE BFORCE             TO WS-FORCE-CODE                 
02092 *        IF WS-FORCE-CODE-VALID                                   
02093 *           MOVE AL-UANON       TO BFORCE-ATTRB                   
02094 *        ELSE                                                     
02095 *           MOVE ER-2670        TO EMI-ERROR                      
02096 *           MOVE -1             TO BFORCE-LEN                     
02097 *           MOVE AL-UABON       TO BFORCE-ATTRB                   
02098 *           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
02099                                                                   
02100 *    IF BBILLCD-LEN             GREATER ZEROS                     
02101 *        MOVE 'Y'                TO PI-BILLCD-KEYED-SW            
02102 *        MOVE AL-UANON           TO BBILLCD-ATTRB.                
02103                                                                   
02104 *    IF  BSKPCD-LEN          GREATER ZEROS                        
02105 *        MOVE 'Y'                TO PI-SKPCD-KEYED-SW             
02106 *        MOVE BSKPCD             TO WS-SKIP-CODE                  
02107 *        IF WS-SKIP-CODE-VALID                                    
02108 *            MOVE AL-UANON       TO BSKPCD-ATTRB                  
02109 *        ELSE                                                     
02110 *            MOVE -1             TO BSKPCD-LEN                    
02111 *            MOVE ER-2683        TO EMI-ERROR                     
02112 *            MOVE AL-UABON       TO BSKPCD-ATTRB                  
02113 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
02114                                                                   
02115 *    IF BIND-GRP-LEN GREATER ZEROS                                
02116 *        MOVE 'Y'                TO PI-IG-KEYED-SW                
02117 *        MOVE AL-UANON           TO BIND-GRP-ATTRB.               
02118                                                                   
02119 *    IF BSIG-LEN GREATER ZEROS                                    
02120 *        MOVE 'Y'                TO PI-SIG-KEYED-SW               
02121 *        IF PI-COMPANY-ID = 'CRI'                                 
02122 *            IF BSIG = 'S' OR 'J' OR ' '                          
02123 *                MOVE AL-UANON   TO BSIG-ATTRB                    
02124 *            ELSE                                                 
02125 *                MOVE -1         TO BSIG-LEN                      
02126 *                MOVE ER-2702    TO EMI-ERROR                     
02127 *                MOVE AL-UABON   TO BSIG-ATTRB                    
02128 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
02129 *        ELSE                                                     
02130 *        IF PI-COMPANY-ID = 'NCL'                                 
02131 *            IF BSIG = 'S' OR 'J' OR 'N' OR 'Y' OR ' '            
02132 *                MOVE AL-UANON   TO BSIG-ATTRB                    
02133 *            ELSE                                                 
02134 *                MOVE -1         TO BSIG-LEN                      
02135 *                MOVE ER-1923    TO EMI-ERROR                     
02136 *                MOVE AL-UABON   TO BSIG-ATTRB                    
02137 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
02138 *        ELSE                                                     
02139 *        IF PI-COMPANY-ID = 'TMS'                                 
02140 *            IF BSIG = 'N' OR 'U' OR 'O' OR ' '                   
02141 *                MOVE AL-UANON   TO BSIG-ATTRB                    
02142 *            ELSE                                                 
02143 *                MOVE -1         TO BSIG-LEN                      
02144 *                MOVE ER-2700    TO EMI-ERROR                     
02145 *                MOVE AL-UABON   TO BSIG-ATTRB                    
02146 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
02147 *        ELSE                                                     
02148 *            IF BSIG   = 'Y' OR ' '                               
02149 *                MOVE AL-UANON   TO BSIG-ATTRB                    
02150 *            ELSE                                                 
02151 *                MOVE -1         TO BSIG-LEN                      
02152 *                MOVE ER-2651    TO EMI-ERROR                     
02153 *                MOVE AL-UABON   TO BSIG-ATTRB                    
02154 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        
02155                                                                   
02156 *    IF BPOLICY-LEN GREATER ZEROS                                 
02157 *        MOVE 'Y'                TO PI-POLICY-KEYED-SW            
02158 *        MOVE AL-UANON           TO BPOLICY-ATTRB.                
02159                                                                   
02160 *    IF BRTCLS-LEN GREATER ZEROS                                  
02161 *        MOVE 'Y'                TO PI-RTCLS-KEYED-SW             
02162 *        MOVE AL-UANON           TO BRTCLS-ATTRB.                 
02163                                                                   
02164      IF BAPR-LEN > 0
02165         MOVE 'Y'                 TO PI-APR-KEYED-SW               
030310        MOVE +0                  TO  WS-BAPR
030310        PERFORM VARYING WS-SUB FROM +1 BY +1 UNTIL
030310           (WS-SUB > 7)
030310           OR (BAPR-IN (WS-SUB:1) NUMERIC)
030310           OR (BAPR-IN (WS-SUB:1) = '.')
030310        END-PERFORM
030310        IF WS-SUB > 7
030310           MOVE ER-2471       TO EMI-ERROR                     
030310           MOVE -1            TO BAPR-LEN                      
030310           MOVE AL-UNBON      TO BAPR-ATTRB                    
030310           PERFORM 9900-ERROR-FORMAT
030310                                 THRU 9900-EXIT
030310        ELSE
030310           IF BAPR-IN (WS-SUB:1) NUMERIC
030310              MOVE WS-SUB           TO WS-BEG
030310              PERFORM VARYING WS-SUB FROM WS-SUB BY +1 UNTIL
030310                 (WS-SUB > 7)
030310                 OR (BAPR-IN (WS-SUB:1) NOT NUMERIC)
030310              END-PERFORM
030310              COMPUTE WS-END = WS-SUB - WS-BEG
030310              MOVE BAPR-IN (WS-BEG:WS-END)
030310                                 TO WS-APR-WHOLE-NUM
030310           END-IF
030310           IF BAPR-IN (WS-SUB:1) = '.'
030310              COMPUTE WS-BEG = WS-SUB + 1
030310              IF (WS-BEG < 8)
030310                 AND (BAPR-IN (WS-BEG:1) NUMERIC)
030310                 PERFORM VARYING WS-SUB FROM WS-BEG BY +1 UNTIL
030310                    (WS-SUB > 7)
030310                    OR (BAPR-IN (WS-SUB:1) NOT NUMERIC)
030310                 END-PERFORM
030310                 COMPUTE WS-END = WS-SUB - WS-BEG
030310                 MOVE BAPR-IN (WS-BEG:WS-END)
030310                                 TO WS-APR-DEC (1:WS-END)
030310              END-IF
030310           END-IF
030310           MOVE AL-UNNON      TO BAPR-ATTRB
030310           MOVE WS-BAPR       TO BAPR-OUT
030310        END-IF
030310     END-IF

           IF BJNTDOB-LEN > ZEROS
              MOVE 'Y'                 TO PI-1ST-PMT-KEYED-SW
              IF BJNTDOB-DT = SPACES
                 MOVE LOW-VALUES       TO WS-CONVERTED-BIRTH (2)
              ELSE
                 MOVE BJNTDOB-DT       TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT
                 MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY
                 MOVE AL-UNNON         TO BJNTDOB-ATTRB
                 MOVE 4                TO DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERT                              
                 IF NO-CONVERSION-ERROR
                    IF DC-BIN-DATE-1 > WS-CONVERTED-EFFDT
                       MOVE DC-BIN-DATE-1 TO WS-WORK-BIN-DT
                       SUBTRACT CENTURY-ADJ FROM WS-WORK-BIN-RED
                       MOVE WS-WORK-BIN-DT TO DC-BIN-DATE-1
                    END-IF
                    MOVE DC-BIN-DATE-1 TO WS-CONVERTED-BIRTH (2)
                    MOVE AL-UANON      TO BJNTDOB-ATTRB
                 ELSE
                    MOVE -1            TO BJNTDOB-LEN
                    MOVE ER-2228       TO EMI-ERROR
                    MOVE AL-UNBON      TO BJNTDOB-ATTRB
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF

02179      IF BBIRTH-LEN GREATER ZEROS                                  
02180         NEXT SENTENCE                                             
02181      ELSE                                                         
02182         GO TO 1027-CHECK-FUNCTION.                                
02183                                                                   
02184      IF BBIRTH-DT NOT NUMERIC                                     
02185         GO TO 1026-CHECK-ERROR.                                   
02186                                                                   
02187      MOVE 'Y'                    TO PI-BIRTHDT-KEYED-SW           
02188      MOVE AL-UNNON               TO BBIRTH-ATTRB                  
02189      MOVE 4                      TO DC-OPTION-CODE                
02190      MOVE BBIRTH-DT              TO DC-GREG-DATE-1-MDY            
02191      MOVE DC-GREG-DATE-1-MDY     TO WS-SAVE-BIRTH-DATE            
02192      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                     
02193      MOVE DC-BIN-DATE-1          TO WS-CONVERTED-BIRTH (1)
CIDMOD     IF (DATE-CONVERSION-ERROR)
CIDMOD*    IF (DATE-CONVERSION-ERROR) OR
CIDMOD*       ((DC-BIN-DATE-1 GREATER THAN WS-CONVERTED-EFFDT) AND      
CIDMOD*       (WS-SAVE-BIRTH-YR GREATER THAN '22'))                     
02195         MOVE -1                  TO BBIRTH-LEN                    
02196         MOVE ER-2228             TO EMI-ERROR                     
02197         MOVE AL-UNBON            TO BBIRTH-ATTRB                  
02198         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
02199         GO TO 1027-CHECK-FUNCTION
           END-IF
02200                                                                   
CIDMOD     IF (NO-CONVERSION-ERROR) AND                                 
CIDMOD        (DC-BIN-DATE-1 GREATER THAN WS-CONVERTED-EFFDT)           
02202         MOVE DC-BIN-DATE-1       TO WS-WORK-BIN-DT                
02203         SUBTRACT CENTURY-ADJ FROM WS-WORK-BIN-RED                 
02204         MOVE WS-WORK-BIN-DT      TO DC-BIN-DATE-1                 
02205                                     WS-CONVERTED-BIRTH (1)
02206         MOVE AL-UANON            TO BBIRTH-ATTRB                  
02207      ELSE                                                         
02208         MOVE DC-BIN-DATE-1       TO WS-CONVERTED-BIRTH (1)
02209         MOVE AL-UANON            TO BBIRTH-ATTRB
           END-IF
02210                                                                  
02211                                                                   
02212 *    IF BAGE-LEN   = ZERO                                         
02213 *       PERFORM 1095-CALC-AGE THRU 1099-EXIT.                     
02214                                                                   
02215      GO TO 1028-CHECK-MEMNO.                                      
02216                                                                   
02217  1026-CHECK-ERROR.                                                
02218                                                                   
02219         IF PI-BIRTH-DATE-IS-INPUT                                 
02220            MOVE ER-2442    TO EMI-ERROR                           
02221            MOVE -1         TO BBIRTH-LEN                          
02222            MOVE AL-UNBON   TO BBIRTH-ATTRB                        
02223            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
02224         ELSE                                                      
02225           MOVE -1         TO BBIRTH-LEN                           
02226           MOVE ER-2223    TO EMI-ERROR                            
02227           MOVE AL-UNBON   TO BBIRTH-ATTRB                         
02228           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               
02229                                                                   
02230  1027-CHECK-FUNCTION.                                             
02231                                                                   
02232      IF PI-LAST-FUNC-DISPLAY                                      
02233         NEXT SENTENCE                                             
02234      ELSE                                                         
02235         IF PI-COMPANY-ID NOT = 'CRI' AND 'LGX'
062121           AND 'CID' AND 'DCC' and 'AHL' and 'VPP' AND 'FNL'
02236            IF PI-BIRTH-DATE-IS-INPUT                              
02237               MOVE ER-2442       TO EMI-ERROR                     
02238               MOVE -1            TO BBIRTH-LEN                    
02239               MOVE AL-UNBON      TO BBIRTH-ATTRB                  
02240               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
02241           ELSE                                                    
02242               MOVE AL-SANOF      TO BBIRTH-ATTRB.                 
02243                                                                   
02244  1028-CHECK-MEMNO.                                                
02245                                                                   
02246 *    IF BMEM-NO-LEN GREATER ZEROS                                 
02247 *        MOVE 'Y'                TO PI-MEMBER-KEYED-SW            
02248 *        MOVE AL-UANON           TO BMEM-NO-ATTRB.                
02249                                                                   
02250 *    IF  BPHONE-LEN GREATER ZEROS                                 
02251 *        MOVE BPHONE             TO DEEDIT-FIELD                  
02252 *        PERFORM 8600-DEEDIT                                      
02253 *        MOVE DEEDIT-FIELD-V0    TO WS-BPHONE                     
02254 *        MOVE AL-UANON           TO BPHONE-ATTRB.                 
02255                                                                   
02256 *    IF BENTRYI = 'D' OR 'V'                                      
02257 *       GO TO 1029-CHECK-ERRORS.                                  
02258                                                                   
02259      IF NOT PI-LAST-FUNC-DISPLAY                                  
02260         AND WS-DATA-NOT-KEYED                                     
02261         MOVE ER-7400             TO EMI-ERROR                     
02262         MOVE -1                  TO BTYPE1-LEN
02263         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
02264         MOVE 'Y'                 TO PI-ERROR-SW.                  
02265                                                                   
02266  1029-CHECK-ERRORS.                                               
02267                                                                   
02268      IF EMI-ERROR = ZEROS                                         
02269          MOVE 'Y'                TO PI-UPDATE-SW                  
02270          MOVE SPACE              TO PI-DISPLAY-SW                 
02271          PERFORM 4000-BUILD-ISSUE-RECORD THRU 4900-EXIT           
02272      ELSE                                                         
              IF (EMI-FATAL-CTR = ZEROS)
                 AND (EMI-FORCABLE-CTR = ZEROS)
                 MOVE 'Y'              TO PI-UPDATE-SW
                 MOVE SPACE            TO PI-DISPLAY-SW
                 PERFORM 4000-BUILD-ISSUE-RECORD THRU 4900-EXIT
                 MOVE 'Y'              TO PI-ERROR-SW
              ELSE
02273            MOVE ZEROS              TO EMI-ERROR                     
02274            MOVE 'Y'                TO PI-ERROR-SW
              END-IF
           END-IF

           .
02276  1030-NOTHING-TO-EDIT.                                            
02277      IF PI-DATA-ERRORS                                            
02278          MOVE AL-SABON           TO BSEQ-ATTRB                    
02279          GO TO 8200-SEND-DATAONLY.                                
02280                                                                   
02281      IF PI-SAV-BATCH-SEQ LESS PI-LAST-SEQ-NO-ADDED                
02282          SUBTRACT 1 FROM PI-SAV-BATCH-SEQ                         
02283          MOVE ER-0000        TO EMI-ERROR                         
02284          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
02285          GO TO 2000-BROWSE-FWD.                                   
02286                                                                   
02287      MOVE LOW-VALUES     TO EL630BI.                              
02288      ADD +1                 PI-LAST-SEQ-NO-ADDED                  
02289                             GIVING PI-NEXT-DISPLAY-SEQ-NO.        
02290      MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ.             
02291                                                                   
02292      PERFORM 8550-SET-MAP-SEQ-NOS.                                
02293                                                                   
02294      MOVE ER-0000        TO EMI-ERROR.                            
02295      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02296      GO TO 8100-SEND-INITIAL-MAP.                                 
02297                                                                   
02298      EJECT                                                        
02299                                                                   
02300  1040-EDIT-INPUT-CODE.                                            
02301      IF WS-SUB1 = +2                                              
02302         GO TO 1050-EDIT-INPUT-AH-CODE.                            
02303                                                                   
02304      MOVE SPACES                 TO ELCNTL-ACCESS.                
02305      MOVE 'L'                    TO ELCNTL-REC-TYPE.              
02306      PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.                     
02307                                                                   
02308      IF EMI-ERROR = 9999                                          
02309          GO TO 1048-NO-RECORD.                                    
02310                                                                   
02311      MOVE +1 TO WS-EDIT-SUB.                                      
02312                                                                   
02313  1041-SEARCH-LOOP.                                                
02314      IF CF-LIFE-CODE-OUT (WS-EDIT-SUB) = ZEROS                    
02315          GO TO 1047-NO-MATCH-FOUND.                               
02316                                                                   
02317      IF BTYPE1  = CF-LIFE-CODE-IN (WS-EDIT-SUB)                 
02318          MOVE CF-LIFE-CODE-OUT (WS-EDIT-SUB) TO WS-EDITED-LF-CODE 
02319          PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT          
02320          GO TO 1059-EXIT.                                         
02321                                                                   
02322      ADD 1   TO WS-EDIT-SUB.                                      
02323                                                                   
02324      IF WS-EDIT-SUB GREATER 120                                   
02325          GO TO 1047-NO-MATCH-FOUND.                               
02326                                                                   
02327      GO TO 1041-SEARCH-LOOP.                                      
02328                                                                   
02329  1047-NO-MATCH-FOUND.                                             
02330 *    MOVE ER-2424                TO EMI-ERROR.                    
02331 *    MOVE AL-UABON               TO BTYPE1-ATTRB
02332 *    MOVE -1                     TO BTYPE1-LEN
02333 *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02334 *    MOVE 'Y'                    TO ERROR-SW.                     
02335                                                                   
02336      MOVE BTYPE1                 TO WS-EDITED-LF-CODE.            
02337      PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.             
02338                                                                   
02339      GO TO 1059-EXIT.                                             
02340                                                                   
02341  1048-NO-RECORD.                                                  
02342 *    MOVE ER-2423                TO EMI-ERROR.                    
02343 *    MOVE AL-UABON               TO BTYPE1-ATTRB
02344 *    MOVE -1                     TO BTYPE1-LEN
02345 *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02346 *    MOVE 'Y'                    TO ERROR-SW.                     
02347                                                                   
02348      MOVE BTYPE1                 TO WS-EDITED-LF-CODE.            
02349      PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.             
02350                                                                   
02351      GO TO 1059-EXIT.                                             
02352                                                                   
02353  1050-EDIT-INPUT-AH-CODE.                                         
02354      MOVE SPACES                 TO ELCNTL-ACCESS.                
02355      MOVE 'A'                    TO ELCNTL-REC-TYPE.              
02356                                                                   
02357      PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.                     
02358                                                                   
02359      IF EMI-ERROR = 9999                                          
02360          GO TO 1058-NO-RECORD.                                    
02361                                                                   
02362      MOVE +1 TO WS-EDIT-SUB.                                      
02363                                                                   
02364  1051-SEARCH-LOOP.                                                
02365      IF CF-AH-CODE-OUT (WS-EDIT-SUB) = ZEROS                      
02366          GO TO 1057-NO-MATCH-FOUND.                               
02367                                                                   
02368      IF BTYPE2    = CF-AH-CODE-IN (WS-EDIT-SUB)                   
02369          MOVE CF-AH-CODE-OUT (WS-EDIT-SUB) TO WS-EDITED-AH-CODE   
02370          PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT          
02371          GO TO 1059-EXIT.                                         
02372                                                                   
02373      ADD +1  TO WS-EDIT-SUB.                                      
02374                                                                   
02375      IF WS-EDIT-SUB GREATER +96                                   
02376          GO TO 1057-NO-MATCH-FOUND.                               
02377                                                                   
02378      GO TO 1051-SEARCH-LOOP.                                      
02379                                                                   
02380  1057-NO-MATCH-FOUND.                                             
02381 *    MOVE ER-2428                TO EMI-ERROR.                    
02382 *    MOVE AL-UABON               TO BTYPE2-ATTRB
02383 *    MOVE -1                     TO BTYPE2-LEN
02384 *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02385 *    MOVE 'Y'                    TO ERROR-SW.                     
02386                                                                   
02387      MOVE BTYPE2                 TO WS-EDITED-AH-CODE.            
02388      PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.             
02389                                                                   
02390      GO TO 1059-EXIT.                                             
02391                                                                   
02392  1058-NO-RECORD.                                                  
02393 *    MOVE ER-2427                TO EMI-ERROR.                    
02394 *    MOVE AL-UABON               TO BTYPE2-ATTRB
02395 *    MOVE -1                     TO BTYPE2-LEN
02396 *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02397 *    MOVE 'Y'                    TO ERROR-SW.                     
02398                                                                   
02399      MOVE BTYPE2                 TO WS-EDITED-AH-CODE.            
02400      PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.             
02401                                                                   
02402  1059-EXIT.                                                       
02403      EXIT.                                                        
02404                                                                   
02405      EJECT                                                        
02406                                                                   
02407  1060-BENEFIT-MASTER-READ.                                        
02408      MOVE SPACES                 TO ELCNTL-ACCESS.                
02409                                                                   
02410      IF ELCNTL-REC-TYPE = 'L'                                     
02411          MOVE WS-EDITED-LF-CODE  TO WS-BEN-CD                     
02412                                     ELCNTL-HI-BEN                 
02413          MOVE '4'                TO ELCNTL-REC-TYPE               
02414      ELSE                                                         
02415          MOVE WS-EDITED-AH-CODE  TO WS-BEN-CD                     
02416                                     ELCNTL-HI-BEN                 
02417          MOVE '5'                TO ELCNTL-REC-TYPE.              
02418                                                                   
02419      PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.                     
02420                                                                   
02421      IF EMI-ERROR = 9999                                          
02422          GO TO 1062-NO-RECORD.                                    
02423                                                                   
02424      IF ELCNTL-COMPANY-ID NOT = CF-COMPANY-ID  OR                 
02425         ELCNTL-REC-TYPE   NOT = CF-RECORD-TYPE                    
02426            GO TO 1062-NO-RECORD.                                  
02427                                                                   
050713     perform varying ws-sub from +1 by +1 until
050713        (ws-sub > +8)
050713        or (cf-benefit-code (ws-sub) = ws-ben-cd)
050713     end-perform
02432                                                                   
02433      IF WS-SUB NOT = +9                                           
02434          IF ELCNTL-REC-TYPE = '4'                                 
02435              MOVE CF-BENEFIT-ALPHA (WS-SUB) TO WS-LF-ABBR-DESC    
050713             move cf-co-earnings-calc (ws-sub)
050713                                 to ws-lf-earnings-calc
02436          ELSE                                                     
02437              MOVE CF-BENEFIT-ALPHA (WS-SUB) TO WS-AH-ABBR-DESC    
02438      ELSE                                                         
02439          GO TO 1063-NO-MATCH-FOUND.                               
02440                                                                   
02441                                                                   
02442      IF  CF-TERM-IN-DAYS (WS-SUB)                                 
02443          MOVE 'Y'                TO WS-TERM-IN-DAYS-SW.           
02444                                                                   
02445      GO TO 1069-EXIT.                                             
02446                                                                   
02451  1062-NO-RECORD.                                                  
02452      MOVE ER-2426                TO EMI-ERROR.                    
02453                                                                   
02454      IF ELCNTL-REC-TYPE = '4'                                     
02455          MOVE AL-UABON           TO BTYPE1-ATTRB
02456          MOVE -1                 TO BTYPE1-LEN
02457      ELSE                                                         
02458          MOVE AL-UABON           TO BTYPE2-ATTRB
02459          MOVE -1                 TO BTYPE2-LEN
           END-IF
02460                                                                   
02461      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02462      MOVE 'Y'                    TO ERROR-SW.                     
02463                                                                   
02464      GO TO 1069-EXIT.                                             
02465                                                                   
02466  1063-NO-MATCH-FOUND.                                             
02467      IF ELCNTL-REC-TYPE = '4'                                     
02468          MOVE ER-2425            TO EMI-ERROR                     
02469          MOVE AL-UABON           TO BTYPE1-ATTRB
02470          MOVE -1                 TO BTYPE1-LEN
02471      ELSE                                                         
02472          MOVE ER-2429            TO EMI-ERROR                     
02473          MOVE AL-UABON           TO BTYPE2-ATTRB
02474          MOVE -1                 TO BTYPE2-LEN
           END-IF
02475                                                                   
02476      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02477      MOVE 'Y'                    TO ERROR-SW.                     
02478      GO TO 1069-EXIT.                                             
02479                                                                   
02480  1069-EXIT.                                                       
02481      EXIT.                                                        
02482                                                                   
02483      EJECT                                                        
02484                                                                   
02485  1070-ELCNTL-READ.                                                
02486      EXEC CICS HANDLE CONDITION                                   
02487          NOTFND  (1078-NO-RECORD)                                 
02488          ENDFILE (1078-NO-RECORD)                                 
02489      END-EXEC.                                                    
02490                                                                   
02491      IF ELCNTL-REC-TYPE = '1' OR '4' OR '5'                       
02492          EXEC CICS READ                                           
02493              DATASET (FILE-ID-ELCNTL)                             
02494              SET     (ADDRESS OF CONTROL-FILE)                    
02495              RIDFLD  (ELCNTL-KEY)                                 
02496              GTEQ                                                 
02497          END-EXEC                                                 
02498      ELSE                                                         
02499          EXEC CICS READ                                           
02500              DATASET (FILE-ID-ELCNTL)                             
02501              SET     (ADDRESS OF CONTROL-FILE)                    
02502              RIDFLD  (ELCNTL-KEY)                                 
02503          END-EXEC.                                                
02504                                                                   
02505      GO TO 1079-EXIT.                                             
02506                                                                   
02507  1078-NO-RECORD.                                                  
02508      MOVE ER-9999                TO EMI-ERROR.                    
02509                                                                   
02510  1079-EXIT.                                                       
02511      EXIT.                                                        
02512                                                                   
02513      EJECT                                                        
02514                                                                   
02515 *1080-TERM-CONVERSION.                                            
02516 *    IF BMODE   = ' ' OR 'M'                                      
02517 *        MOVE WS-BPMTS      TO WS-CALC-TERM-WHOLE                 
02518 *        GO TO 1085-ROUND-TERM.                                   
02519                                                                   
02520 *    IF BMODE   = 'S'                                             
02521 *        COMPUTE WS-CALC-TERM = WS-BPMTS / 2                      
02522 *        GO TO 1085-ROUND-TERM.                                   
02523                                                                   
02524 *    IF BMODE   = 'W'                                             
02525 *        COMPUTE WS-CALC-TERM = WS-BPMTS / 4.33333                
02526 *        GO TO 1085-ROUND-TERM.                                   
02527                                                                   
02528 *    IF BMODE   = 'B'                                             
02529 *        COMPUTE WS-CALC-TERM = WS-BPMTS / 2.16667                
02530 *        GO TO 1085-ROUND-TERM.                                   
02531                                                                   
02532 *    IF BMODE   = 'T'                                             
02533 *        COMPUTE WS-CALC-TERM = WS-BPMTS / 1.08334.               
02534                                                                   
02535 *1085-ROUND-TERM.                                                 
02536 *    IF WS-CALC-TERM-REMAIN GREATER .00000                        
02537 *       ADD +1 TO WS-CALC-TERM.                                   
02538 *    MOVE ZEROS                  TO WS-CALC-TERM-REMAIN.          
02539                                                                   
02540 *    IF  BTYPE-LEN       (WS-SUB1)  GREATER ZEROS                 
02541**        IF  WS-KIND-MONTHLY                                      
02542 *            IF BTERM-LEN    (WS-SUB1)  = ZEROS                   
02543 *               IF BPREM-LEN (WS-SUB1)  GREATER ZEROS             
02544 *                  IF  BPMT-LEN   GREATER ZEROS                   
02545 *                      NEXT SENTENCE                              
02546 *                  ELSE                                           
02547 *                      GO TO 1087-EDIT-TERM.                      
02548                                                                   
02549 *    IF PI-COMPANY-ID IS EQUAL TO 'HAN' OR 'JHL'                  
02550 *        IF BBEN-LEN (WS-SUB1) IS GREATER THAN ZEROS              
02551 *            GO TO 1087-EDIT-TERM.                                
02552                                                                   
02553 *    IF  BMODE   = 'M' OR ' '                                     
02554 *        MOVE WS-BPMT            TO  WS-BBEN    (WS-SUB1).        
02555                                                                   
02556 *    IF  BMODE   = 'W'                                            
02557 *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 4.33333.   
02558                                                                   
02559 *    IF  BMODE   = 'S'                                            
02560 *        COMPUTE WS-BBEN (WS-SUB1)  ROUNDED = WS-BPMT * 2.        
02561                                                                   
02562 *    IF  BMODE   = 'B'                                            
02563 *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 2.16667.   
02564                                                                   
02565 *    IF  BMODE   = 'T'                                            
02566 *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 1.08334.   
02567                                                                   
02568 *    IF WS-SUB1 = +1                                              
02569 *        COMPUTE WS-BBEN  (WS-SUB1) = WS-BPMT * WS-BPMTS.         
02570                                                                   
02571 *    IF PI-COMPANY-ID = 'CRI'       OR  'LGX'                     
02572 *        IF WS-SUB1 = +1  AND                                     
02573 *           BBEN-LEN (WS-SUB1) GREATER ZEROS                      
02574 *             GO TO 1087-EDIT-TERM.                               
02575                                                                   
02576 *    MOVE  WS-BBEN (WS-SUB1)     TO BBENO       (WS-SUB1).        
02577 *    MOVE +12                    TO BBEN-LEN    (WS-SUB1).        
02578 *    MOVE AL-UNNON               TO BBEN-ATTRB  (WS-SUB1).        
02579                                                                   
02580 *1087-EDIT-TERM.                                                  
02581 *    IF BTERM-LEN (WS-SUB1)  = ZEROS                              
02582 *        MOVE WS-CALC-TERM-WHOLE   TO BTERMI      (WS-SUB1)       
02583 *                                     WS-BTERM    (WS-SUB1)       
02584 *        MOVE +3                   TO BTERM-LEN   (WS-SUB1)       
02585 *        GO TO 1089-EXIT.                                         
02586                                                                   
02587 *    MOVE BTERMI    (WS-SUB1)   TO DEEDIT-FIELD.                  
02588 *    PERFORM 8600-DEEDIT.                                         
02589 *    IF DEEDIT-FIELD-V0 NUMERIC                                   
02590 *       MOVE DEEDIT-FIELD-V0    TO WS-BTERM    (WS-SUB1)          
02591 *       IF WS-BTERM (WS-SUB1)  GREATER ZERO                       
02592 *          MOVE AL-UNNON        TO BTERM-ATTRB (WS-SUB1)          
02593 *       ELSE                                                      
02594 *          MOVE ER-2241         TO EMI-ERROR                      
02595 *          MOVE -1              TO BTERM-LEN   (WS-SUB1)          
02596 *          MOVE AL-UNBOF        TO BTERM-ATTRB (WS-SUB1)          
02597 *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
02598 *          GO TO 1089-EXIT.                                       
02599                                                                   
02600 *    IF WS-BTERM (WS-SUB1)   NOT = WS-CALC-TERM-WHOLE             
02601 *        MOVE -1                   TO BTERM-LEN   (WS-SUB1)       
02602 *        MOVE ER-2593              TO EMI-ERROR                   
02603 *        MOVE AL-UNBON             TO BTERM-ATTRB (WS-SUB1)       
02604 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
02605                                                                   
02606 *1089-EXIT.                                                       
02607 *    EXIT.                                                        
02608                                                                   
02610 *1090-CALCULATE-MONTHLY-TERM.                                     
02611 *    IF  BTYPE-LEN  (WS-SUB1)  GREATER ZEROS                      
02612 *        IF BTERM-LEN (WS-SUB1)  = ZEROS                          
02613 *           IF BPREM-LEN ( WS-SUB1) GREATER ZEROS                 
02614 *              IF  BPMT-LEN   GREATER ZEROS                       
02615 *                  NEXT SENTENCE                                  
02616 *              ELSE                                               
02617 *                  GO TO 1094-EXIT.                               
02618                                                                   
02619 *    COMPUTE  WS-CALC-TERM = WS-BDAYS / 31.                       
02620                                                                   
02621 *    IF WS-CALC-TERM-REMAIN GREATER .00000                        
02622 *       ADD +1 TO WS-CALC-TERM.                                   
02623                                                                   
02624 *    MOVE ZEROS                  TO  WS-CALC-TERM-REMAIN.         
02625                                                                   
02626 *    MOVE WS-CALC-TERM           TO  BTERMI    (WS-SUB1).         
02627 *    MOVE +3                     TO  BTERM-LEN (WS-SUB1).         
02628                                                                   
02629 *    IF  BMODE   = 'M'  OR  ' '                                   
02630 *        MOVE WS-BPMT            TO  WS-BBEN    (WS-SUB1).        
02631                                                                   
02632 *    IF  BMODE   = 'W'                                            
02633 *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 4.33333.   
02634                                                                   
02635 *    IF  BMODE   = 'S'                                            
02636 *        COMPUTE WS-BBEN (WS-SUB1)  ROUNDED = WS-BPMT * 2.        
02637                                                                   
02638 *    IF  BMODE   = 'B'                                            
02639 *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 2.16667.   
02640                                                                   
02641 *    IF  BMODE   = 'T'                                            
02642 *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 1.08334.   
02643                                                                   
02644 *    IF WS-SUB1 = +1                                              
02645 *       COMPUTE WS-BBEN  (WS-SUB1) =                              
02646 *               WS-BBEN (WS-SUB1) * WS-CALC-TERM.                 
02647                                                                   
02648 *    MOVE  WS-BBEN (WS-SUB1)     TO BBENO       (WS-SUB1).        
02649 *    MOVE +12                    TO BBEN-LEN    (WS-SUB1).        
02650 *    MOVE AL-UNNON               TO BBEN-ATTRB  (WS-SUB1).        
02651                                                                   
02652 *1094-EXIT.                                                       
02653 *    EXIT.                                                        

02657  1095-CALC-AGE.                                                   
02658      MOVE WS-CONVERTED-BIRTH (1) TO DC-BIN-DATE-1
02659      MOVE WS-CURRENT-BIN-DT  TO DC-BIN-DATE-2.                    
02660      MOVE 1 TO DC-OPTION-CODE.                                    
02661      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
02662      IF NO-CONVERSION-ERROR                                       
02663          COMPUTE WS-BAGE = DC-ELAPSED-MONTHS / 12                 
02664          MOVE WS-BAGE            TO BAGE                          
02665          MOVE +2                 TO BAGE-LEN                      
02666          MOVE AL-UNNON TO BAGE-ATTRB.                             
02667                                                                   
02668  1099-EXIT.                                                       
02669      EXIT.                                                        
02670                                                                   
02671      EJECT                                                        
02672                                                                   
02673  1100-EDIT-MAPC.                                                  
02674      MOVE +1                     TO WS-SUB2.                      
02675                                                                   
02676      IF PI-LAST-FUNC-DISPLAY                                      
02677         AND CLAST-NAME-LEN (1)   = ZEROS                          
02678         AND CLAST-NAME-LEN (2)   = ZEROS                          
02679         AND CLAST-NAME-LEN (3)   = ZEROS                          
02680         AND CLAST-NAME-LEN (4)   = ZEROS                          
02681         AND CCANDT1-LEN     (1) = ZEROS                           
02682         AND CCANDT2-LEN     (1) = ZEROS                           
02683         AND CCANDT1-LEN     (2) = ZEROS                           
02684         AND CCANDT2-LEN     (2) = ZEROS                           
02685         AND CCANDT1-LEN     (3) = ZEROS                           
02686         AND CCANDT2-LEN     (3) = ZEROS                           
02687         AND CCANDT1-LEN     (4) = ZEROS                           
02688         AND CCANDT2-LEN     (4) = ZEROS                           
02689         AND CMTHD1-LEN      (1) = ZEROS                           
02690         AND CMTHD2-LEN      (1) = ZEROS                           
02691         AND CMTHD1-LEN      (2) = ZEROS                           
02692         AND CMTHD2-LEN      (2) = ZEROS                           
02693         AND CMTHD1-LEN      (3) = ZEROS                           
02694         AND CMTHD2-LEN      (3) = ZEROS                           
02695         AND CMTHD1-LEN      (4) = ZEROS                           
02696         AND CMTHD2-LEN      (4) = ZEROS                           
02697         AND CREFUND1-LEN    (1) = ZEROS                           
02698         AND CREFUND2-LEN    (1) = ZEROS                           
02699         AND CREFUND1-LEN    (2) = ZEROS                           
02700         AND CREFUND2-LEN    (2) = ZEROS                           
02701         AND CREFUND1-LEN    (3) = ZEROS                           
02702         AND CREFUND2-LEN    (3) = ZEROS                           
02703         AND CREFUND1-LEN    (4) = ZEROS                           
02704         AND CREFUND2-LEN    (4) = ZEROS                           
02705         AND CLIVES-LEN     (1) = ZEROS                            
02706         AND CLIVES-LEN     (2) = ZEROS                            
02707         AND CLIVES-LEN     (3) = ZEROS                            
02708         AND CLIVES-LEN     (4) = ZEROS                            
02709         AND CCANREA-LEN    (1) = ZEROS                            
02710         AND CCANREA-LEN    (2) = ZEROS                            
02711         AND CCANREA-LEN    (3) = ZEROS                            
02712         AND CCANREA-LEN    (4) = ZEROS                            
02713         AND CPAYEE-LEN     (1) = ZEROS                            
02714         AND CPAYEE-LEN     (2) = ZEROS                            
02715         AND CPAYEE-LEN     (3) = ZEROS                            
02716         AND CPAYEE-LEN     (4) = ZEROS                            
02717         AND CCHK-LEN       (1) = ZEROS                            
02718         AND CCHK-LEN       (2) = ZEROS                            
02719         AND CCHK-LEN       (3) = ZEROS                            
02720         AND CCHK-LEN       (4) = ZEROS                            
02721          GO TO 1130-NOTHING-TO-EDIT.                              
02722                                                                   
02723  1110-EDIT-MAPC-LOOP.                                             
02724      IF CCERT-LEN       (WS-SUB2) = ZEROS                         
02725        AND CEFFDT-LEN   (WS-SUB2) = ZEROS                         
02726        AND CCANDT1-LEN  (WS-SUB2) = ZEROS                         
02727        AND CCANDT2-LEN  (WS-SUB2) = ZEROS                         
02728        AND CMTHD1-LEN   (WS-SUB2) = ZEROS                         
02729        AND CMTHD2-LEN   (WS-SUB2) = ZEROS                         
02730        AND CREFUND1-LEN (WS-SUB2) = ZEROS                         
02731        AND CREFUND2-LEN (WS-SUB2) = ZEROS                         
02732        AND NOT PI-LAST-FUNC-DISPLAY                               
02733          GO TO 1120-INCREMENT-OCCURRENCE.                         
02734                                                                   
02735      MOVE 'Y'                    TO WS-DATA-KEYED-SW.             
02736                                                                   
02737      MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).         
02738                                                                   
02739      IF CCERT-LEN (WS-SUB2) = ZEROS                               
02740        AND PI-LAST-FUNC-DISPLAY                                   
02741          NEXT SENTENCE                                            
02742      ELSE                                                         
02743          IF CCERT-LEN (WS-SUB2)  NOT = ZEROS                      
02744              MOVE AL-UANON       TO CCERT-ATTRB (WS-SUB2)         
02745          ELSE                                                     
02746              MOVE -1             TO CCERT-LEN   (WS-SUB2)         
02747              MOVE ER-2218        TO EMI-ERROR                     
02748              MOVE AL-UABON       TO CCERT-ATTRB (WS-SUB2)         
02749              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
02750                                                                   
02751      IF CSFX-LEN (WS-SUB2)      GREATER ZEROS                     
02752          MOVE 'Y'                TO PI-CAN-SUFFIX-KEYED-SW        
02753          MOVE AL-UANON           TO CSFX-ATTRB (WS-SUB2).         
02754                                                                   
02755      IF CEFFDT-LEN (WS-SUB2) = ZEROS                              
02756        AND PI-LAST-FUNC-DISPLAY                                   
02757          NEXT SENTENCE                                            
02758      ELSE                                                         
02759          IF CEFFDT-LEN (WS-SUB2) GREATER ZEROS                    
02760              MOVE AL-UNNON           TO CEFFDT-ATTRB (WS-SUB2)    
02761              IF CEFFDT (WS-SUB2) NUMERIC                          
02762                  MOVE 4              TO DC-OPTION-CODE            
02763                  MOVE CEFFDT (WS-SUB2)  TO DC-GREG-DATE-1-MDY     
02764                  PERFORM 8500-DATE-CONVERT THRU 8500-EXIT         
02765                  MOVE DC-BIN-DATE-1  TO                           
02766                              WS-CONVERTED-CAN-EFF-DT (WS-SUB2)    
02767                              WS-CONVERTED-EFFDT                   
02768                  IF NO-CONVERSION-ERROR                           
02769                      IF WS-CONVERTED-CAN-EFF-DT (WS-SUB2)         
02770                        NOT LESS PI-ACCT-LOW-EFF-DT                
02771                        AND LESS PI-ACCT-HIGH-EXP-DT               
02772                          PERFORM 1500-EDIT-ACCT-DT-RANGES THRU    
02773                                  1590-EXIT                        
02774                      ELSE                                         
02775                          NEXT SENTENCE                            
02776 *                        MOVE -1       TO CEFFDT-LEN (WS-SUB2)    
02777 *                        MOVE ER-2589  TO EMI-ERROR               
02778 *                        MOVE AL-UNBON TO CEFFDT-ATTRB (WS-SUB2)  
02779 *                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT 
02780                  ELSE                                             
02781                      MOVE -1         TO CEFFDT-LEN (WS-SUB2)      
02782                      MOVE ER-2226    TO EMI-ERROR                 
02783                      MOVE AL-UNBON   TO CEFFDT-ATTRB (WS-SUB2)    
02784                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
02785              ELSE                                                 
02786                  MOVE -1             TO CEFFDT-LEN (WS-SUB2)      
02787                  MOVE ER-2223        TO EMI-ERROR                 
02788                  MOVE AL-UNBON       TO CEFFDT-ATTRB (WS-SUB2)    
02789                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
02790          ELSE                                                     
02791              MOVE -1                 TO CEFFDT-LEN (WS-SUB2)      
02792              MOVE ER-2220            TO EMI-ERROR                 
02793              MOVE AL-UNBON           TO CEFFDT-ATTRB (WS-SUB2)    
02794              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
02795                                                                   
02796      IF CLAST-NAME-LEN (WS-SUB2) GREATER ZEROS                    
02797          MOVE AL-UANON           TO CLAST-NAME-ATTRB (WS-SUB2).   
02798                                                                   
02799      EJECT                                                        
02800                                                                   
02801  1115-EDIT-COVERAGES.                                             
02802      IF CCANDT1-LEN (WS-SUB2) = ZEROS                             
02803        AND PI-LAST-FUNC-DISPLAY                                   
02804          NEXT SENTENCE                                            
02805      ELSE                                                         
02806          IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS                   
02807              MOVE AL-UNNON       TO CCANDT1-ATTRB (WS-SUB2)       
02808              IF PI-LAST-FUNC-DISPLAY AND                          
02809                 CCANDT1 (WS-SUB2) = SPACES                        
02810                  MOVE LOW-VALUES TO WS-CONVERTED-CANDT1 (WS-SUB2) 
02811              ELSE                                                 
02812                 IF CCANDT1 (WS-SUB2) NUMERIC                      
02813                    MOVE 4              TO DC-OPTION-CODE          
02814                    MOVE CCANDT1 (WS-SUB2) TO                      
02815                                       DC-GREG-DATE-1-MDY          
02816                    PERFORM 8500-DATE-CONVERT THRU 8500-EXIT       
02817                    MOVE DC-BIN-DATE-1  TO WS-CONVERTED-CANDT1     
02818                                                          (WS-SUB2)
02819                    IF NO-CONVERSION-ERROR                         
02820                       NEXT SENTENCE                               
02821                    ELSE                                           
02822                       MOVE -1       TO CCANDT1-LEN   (WS-SUB2)    
02823                       MOVE ER-2227  TO EMI-ERROR                  
02824                       MOVE AL-UNBON TO CCANDT1-ATTRB (WS-SUB2)    
02825                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT    
02826                 ELSE                                              
02827                    MOVE -1         TO CCANDT1-LEN   (WS-SUB2)     
02828                    MOVE ER-2223    TO EMI-ERROR                   
02829                    MOVE AL-UNBON   TO CCANDT1-ATTRB (WS-SUB2)     
02830                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT       
02831          ELSE                                                     
02832              IF CREFUND1-LEN (WS-SUB2) GREATER ZEROS              
02833                 MOVE -1             TO CCANDT1-LEN   (WS-SUB2)    
02834                 MOVE ER-2222        TO EMI-ERROR                  
02835                 MOVE AL-UNBOF       TO CCANDT1-ATTRB (WS-SUB2)    
02836                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.         
02837                                                                   
02838      IF CREFUND1-LEN (WS-SUB2) = ZEROS                            
02839        AND PI-LAST-FUNC-DISPLAY                                   
02840          NEXT SENTENCE                                            
02841      ELSE                                                         
02842          IF CREFUND1-LEN (WS-SUB2) NOT = ZEROS                    
02843             MOVE AL-UNNON       TO CREFUND1-ATTRB (WS-SUB2)       
CIDMOD*           MOVE CREFUND1I (WS-SUB2) TO DEEDIT-FIELD-X11          
CIDMOD*           PERFORM 8600-DEEDIT                                   
CIDMOD*           MOVE DEEDIT-FIELD-V2  TO WS-CREFUND1 (WS-SUB2).       
02844             EXEC CICS BIF DEEDIT                                  
02845                 FIELD  (CREFUND1I (WS-SUB2))                      
02846                 LENGTH (11)                                       
02847             END-EXEC                                              
02848             MOVE CREFUND1I (WS-SUB2) TO WS-CREFUND1 (WS-SUB2).    
02849                                                                   
02850 ******************************************************************
02851 *********** REFUND METHODS CORRESPOND TO EARNING METHODS *********
02852 *********** METHOD 'R' INDICATES A REPOSSESSION FOR 'FLC'*********
02853 ******************************************************************
02854      IF CMTHD1-LEN (WS-SUB2) = ZEROS                              
02855        AND PI-LAST-FUNC-DISPLAY                                   
02856          NEXT SENTENCE                                            
02857      ELSE                                                         
02858          IF CMTHD1-LEN (WS-SUB2) NOT = ZEROS                      
02859             MOVE 'Y'            TO PI-REFUND-MTHD-KEYED-SW        
02860             MOVE AL-UNNON       TO CMTHD1-ATTRB (WS-SUB2)         
02861             IF CMTHD1 (WS-SUB2) EQUAL '1' OR '2' OR '3' OR '4'    
02862                                    OR '5' OR '6' OR '8' OR ' '    
02863 *                                  OR 'R'                         
02864                NEXT SENTENCE                                      
02865             ELSE                                                  
02866                 MOVE -1         TO CMTHD1-LEN   (WS-SUB2)         
02867                 MOVE ER-0582    TO EMI-ERROR                      
02868                 MOVE AL-UNBON   TO CMTHD1-ATTRB (WS-SUB2)         
02869                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.         
02870                                                                   
02871      MOVE SPACES                  TO WS-CONVERTED-CANDT2 (WS-SUB2)
02872      IF CCANDT2-LEN (WS-SUB2) = ZEROS                             
02873        AND PI-LAST-FUNC-DISPLAY                                   
02874          NEXT SENTENCE                                            
02875      ELSE                                                         
02876          IF CCANDT2-LEN (WS-SUB2) GREATER ZEROS                   
02877              MOVE AL-UNNON       TO CCANDT2-ATTRB (WS-SUB2)       
02878              IF PI-LAST-FUNC-DISPLAY AND                          
02879                 CCANDT2 (WS-SUB2) = SPACES                        
02880                  MOVE LOW-VALUES TO WS-CONVERTED-CANDT2 (WS-SUB2) 
02881              ELSE                                                 
02882                 IF CCANDT2 (WS-SUB2) NUMERIC                      
02883                    MOVE 4              TO DC-OPTION-CODE          
02884                    MOVE CCANDT2 (WS-SUB2) TO                      
02885                                       DC-GREG-DATE-1-MDY          
02886                    PERFORM 8500-DATE-CONVERT THRU 8500-EXIT       
02887                    MOVE DC-BIN-DATE-1  TO WS-CONVERTED-CANDT2     
02888                                                          (WS-SUB2)
02889                    IF NO-CONVERSION-ERROR                         
02890                       NEXT SENTENCE                               
02891                    ELSE                                           
02892                       MOVE -1       TO CCANDT2-LEN   (WS-SUB2)    
02893                       MOVE ER-2227  TO EMI-ERROR                  
02894                       MOVE AL-UNBON TO CCANDT2-ATTRB (WS-SUB2)    
02895                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT    
02896                 ELSE                                              
02897                    MOVE -1         TO CCANDT2-LEN   (WS-SUB2)     
02898                    MOVE ER-2223    TO EMI-ERROR                   
02899                    MOVE AL-UNBON   TO CCANDT2-ATTRB (WS-SUB2)     
02900                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.      
02901 ****     ELSE                                                     
02902 ****         IF CREFUND2-LEN (WS-SUB2) GREATER ZEROS              
02903 ****            MOVE -1             TO CCANDT2-LEN   (WS-SUB2)    
02904 ****            MOVE ER-2222        TO EMI-ERROR                  
02905 ****            MOVE AL-UNBOF       TO CCANDT2-ATTRB (WS-SUB2)    
02906 ****            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.         
02907                                                                   
02908      IF CREFUND2-LEN (WS-SUB2) = ZEROS                            
02909        AND PI-LAST-FUNC-DISPLAY                                   
02910          NEXT SENTENCE                                            
02911      ELSE                                                         
02912          IF CREFUND2-LEN (WS-SUB2) NOT = ZEROS                    
02913             MOVE AL-UNNON       TO CREFUND2-ATTRB (WS-SUB2)       
CIDMOD*           MOVE CREFUND2I (WS-SUB2) TO DEEDIT-FIELD-X11          
CIDMOD*           PERFORM 8600-DEEDIT                                   
CIDMOD*           MOVE DEEDIT-FIELD-V2  TO WS-CREFUND2 (WS-SUB2).       
02914             EXEC CICS BIF DEEDIT                                  
02915                 FIELD  (CREFUND2I (WS-SUB2))                      
02916                 LENGTH (11)                                       
02917             END-EXEC                                              
02918             MOVE CREFUND2I (WS-SUB2) TO WS-CREFUND2 (WS-SUB2).    
02919                                                                   
02920      IF CMTHD2-LEN (WS-SUB2) = ZEROS                              
02921        AND PI-LAST-FUNC-DISPLAY                                   
02922          NEXT SENTENCE                                            
02923      ELSE                                                         
02924          IF CMTHD2-LEN (WS-SUB2) NOT = ZEROS                      
02925             MOVE 'Y'            TO PI-REFUND-MTHD-KEYED-SW        
02926             MOVE AL-UNNON       TO CMTHD2-ATTRB (WS-SUB2)         
02927             IF CMTHD2 (WS-SUB2) EQUAL '1' OR '2' OR '3' OR '4'    
02928                                    OR '5' OR '6' OR '8' OR ' '    
02929 *                                  OR 'R'                         
02930                NEXT SENTENCE                                      
02931             ELSE                                                  
02932                 MOVE -1         TO CMTHD2-LEN   (WS-SUB2)         
02933                 MOVE ER-0582    TO EMI-ERROR                      
02934                 MOVE AL-UNBON   TO CMTHD2-ATTRB (WS-SUB2)         
02935                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.         
02936                                                                   
02937      IF CLIVES-LEN  (WS-SUB2)  GREATER ZEROS                      
02938          MOVE 'Y'                TO PI-CAN-LIVES-KEYED-SW         
02939          MOVE CLIVESI (WS-SUB2 ) TO DEEDIT-FIELD                  
02940          PERFORM 8600-DEEDIT                                      
02941          IF DEEDIT-FIELD-V0 NUMERIC                               
02942              MOVE DEEDIT-FIELD-V0 TO WS-CLIVES   (WS-SUB2)        
02943              MOVE AL-UNNON       TO CLIVES-ATTRB (WS-SUB2)        
02944          ELSE                                                     
02945              MOVE -1             TO CLIVES-LEN   (WS-SUB2)        
02946              MOVE AL-UNBON       TO CLIVES-ATTRB (WS-SUB2)        
02947              MOVE ER-2223        TO EMI-ERROR                     
02948              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
02949                                                                   
02950      IF CCANREA-LEN (WS-SUB2)  GREATER  ZEROS                   
02951         MOVE 'Y'                TO  PI-CAN-REA-KEYED-SW
070622        PERFORM 1150-CHECK-CANCEL-REASON THRU 1150-EXIT
070622        IF SQLCODE = ZERO
02956             MOVE CCANREA (WS-SUB2) TO  WS-CAN-REA (WS-SUB2)
02958             MOVE AL-UANON       TO  CCANREA-ATTRB (WS-SUB2)
      *        IF CCANREA (WS-SUB2) = 'R' OR ' '
02956 *           MOVE CCANREA (WS-SUB2)
02957 *                                TO  WS-CAN-REA (WS-SUB2)
02958 *            MOVE AL-UANON       TO  CCANREA-ATTRB (WS-SUB2)
              ELSE
070622        IF SQLCODE = 100
02960              MOVE -1             TO  CCANREA-LEN (WS-SUB2)      
02961              MOVE AL-UABON       TO  CCANREA-ATTRB (WS-SUB2)    
02962              MOVE ER-9841        TO  EMI-ERROR                    
02963              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
02964                                                                   
02965      IF CPAYEE-LEN  (WS-SUB2)  GREATER ZEROS                      
02966         MOVE 'Y'                 TO PI-PAYEE-KEYED-SW.            
02967                                                                   
02968      IF CCHK-LEN    (WS-SUB2)  GREATER ZEROS                      
02969         MOVE 'Y'                 TO PI-CHK-REQ-KEYED-SW           
02970         IF  CCHK    (WS-SUB2)  = 'R' OR ' '                       
02971             NEXT SENTENCE                                         
02972         ELSE                                                      
02973             MOVE ER-7405         TO EMI-ERROR                     
02974             MOVE -1              TO CCHK-LEN      (WS-SUB2)       
02975             MOVE AL-UABON        TO CCHK-ATTRB    (WS-SUB2)       
02976             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
02977                                                                   
02978      IF PI-LAST-FUNC-DISPLAY                                      
02979         NEXT SENTENCE                                             
02980      ELSE                                                         
02981         IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS OR                 
02982            CCANDT2-LEN (WS-SUB2) GREATER ZEROS                    
02983              NEXT SENTENCE                                        
02984            ELSE                                                   
02985              MOVE ER-2222          TO EMI-ERROR                   
02986              MOVE -1               TO CCANDT1-LEN   (WS-SUB2)     
02987              MOVE AL-UNBOF         TO CCANDT1-ATTRB (WS-SUB2)     
02988              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
02989                                                                   
02990      IF PI-LAST-FUNC-DISPLAY                                      
02991         NEXT SENTENCE                                             
02992      ELSE                                                         
02993         IF WS-CONVERTED-CANDT1 (WS-SUB2) NOT = LOW-VALUES AND     
02994            WS-CONVERTED-CANDT2 (WS-SUB2) = SPACES AND             
02995            CREFUND2-LEN (WS-SUB2) GREATER ZEROS                   
02996              MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO                
02997                   WS-CONVERTED-CANDT2 (WS-SUB2)                   
02998              MOVE CCANDT1     (WS-SUB2) TO CCANDT2     (WS-SUB2)  
02999              MOVE CCANDT1-LEN (WS-SUB2) TO CCANDT2-LEN (WS-SUB2). 
03000                                                                   
03001      IF  EMI-ERROR = ZEROS                                        
03002          MOVE 'Y'                TO PI-UPDATE-SW                  
03003      ELSE                                                         
03004          MOVE 'Y'                TO PI-ERROR-SW.                  
03005                                                                   
03006  1120-INCREMENT-OCCURRENCE.                                       
03007      ADD +1                      TO WS-SUB2.                      
03008                                                                   
03009      IF WS-SUB2 GREATER +4 OR PI-LAST-FUNC-DISPLAY                
03010          NEXT SENTENCE                                            
03011      ELSE                                                         
03012          GO TO 1110-EDIT-MAPC-LOOP.                               
03013                                                                   
03014      IF PI-DATA-ERRORS                                            
03015          MOVE AL-SABON           TO CSEQ-ATTRB (1) CSEQ-ATTRB (2) 
03016                                     CSEQ-ATTRB (3) CSEQ-ATTRB (4) 
03017         GO TO 8200-SEND-DATAONLY                                  
03018      ELSE                                                         
03019         PERFORM 5000-BUILD-CANCEL-RECORD THRU 5900-EXIT.          
03020                                                                   
03021  1130-NOTHING-TO-EDIT.                                            
03022      IF NOT PI-LAST-FUNC-DISPLAY                                  
03023         AND WS-DATA-NOT-KEYED                                     
03024           MOVE ER-7400             TO EMI-ERROR                   
03025           MOVE -1                  TO CCANDT1-LEN (1)             
03026           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                
03027           MOVE 'Y'                 TO PI-ERROR-SW                 
03028           GO TO 8200-SEND-DATAONLY.                               
03029                                                                   
03030      IF PI-SAV-BATCH-SEQ LESS PI-LAST-SEQ-NO-ADDED                
03031         SUBTRACT 1 FROM PI-SAV-BATCH-SEQ                          
03032         MOVE ER-0000            TO EMI-ERROR                      
03033         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
03034         GO TO 2000-BROWSE-FWD.                                    
03035                                                                   
03036      MOVE LOW-VALUES     TO EL630BI.                              
03037                                                                   
03038      MOVE SPACE          TO PI-DISPLAY-SW.                        
03039                                                                   
03040      ADD +1                 PI-LAST-SEQ-NO-ADDED                  
03041                             GIVING PI-NEXT-DISPLAY-SEQ-NO.        
03042                                                                   
03043      MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ.             
03044                                                                   
03045      PERFORM 8550-SET-MAP-SEQ-NOS                                 
03046                   VARYING WS-SUB2 FROM +1 BY +1                   
03047                   UNTIL WS-SUB2 GREATER +4.                       
03048                                                                   
03049      MOVE ER-0000            TO EMI-ERROR.                        
03050      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03051      GO TO 8100-SEND-INITIAL-MAP.                                 
03052                                                                   
070622 1150-CHECK-CANCEL-REASON.
070622
070622*      Read Napersoft CancelReasons Lookup Table
070622     MOVE 'naperadmin'           TO USR
070622     MOVE 'cCm8naper'            TO PASS
070622     IF WS-KIXHOST = 'logictest'
070622        MOVE 'HOVTSTDB01_NaperRepo'
070622                                 TO SVR
070622        MOVE '1029'              TO WS-LOOKUPID
070622     ELSE
070622        MOVE 'SDVDB01_NaperRepo' TO SVR
070622        MOVE '1029'              TO WS-LOOKUPID
070622     END-IF
070622
070622     MOVE SPACES                 TO WS-LOOKUP-VALUE
070622
070622     PERFORM 9000-CONNECT-TO-DB  THRU 9000-EXIT
070622
070622     MOVE CCANREA (WS-SUB2) TO WS-LOOKUPNAME
070622
070622     EXEC SQL
070622           SELECT LOOKUPVALUE
070622             INTO :WS-LOOKUP-VALUE
070622             FROM LOOKUPVALUES
070622               WHERE LOOKUPID = :WS-LOOKUPID
070622                 AND LOOKUPNAME = :WS-LOOKUPNAME
070622     END-EXEC
070622
070622     IF SQLCODE = 0
070622      OR SQLCODE = 100
070622        CONTINUE
070622     ELSE
070622        DISPLAY "ERROR: INVALID CancelReasons Lookup Table SELECT"
070622        DISPLAY ' SQL RETURN CODE ' SQLCODE
070622        DISPLAY ' SQL ERR MESS    ' SQLERRMC
070622     END-IF.
070622
070622     PERFORM 9050-DISCONNECT THRU 9050-EXIT.
070622
070622 1150-EXIT.
070622     EXIT.

03053      EJECT
03054                                                                   
03055  1500-EDIT-ACCT-DT-RANGES.                                        
03056                                                                   
03057 ******************************************************************
03058 *                                                                *
03059 *         E D I T   A C C O U N T   D A T E   R A N G E S        *
03060 *                                                                *
03061 *                                                                *
03062 *    NOTE:  IT IS ONLY NECESSARY TO EDIT THE DATE RANGES         *
03063 *           FOR COMPANYS THAT USE THE ACCOUNTS RECEIVABLE        *
03064 *           SYSTEM.                                              *
03065 *                                                                *
03066 *    1.  DETERMINE THE DATE RANGE FOR THE EFFECTIVE DATE.        *
03067 *                                                                *
03068 *    2.  VERIFY THE ACCOUNT AGENT.  THE ACCOUNT AGENT SHOULD BE  *
03069 *        THE SAME FOR THE ENTIRE BATCH.  IF IT CHANGES, IT IS    *
03070 *        AN ERROR.                                               *
03071 *                                                                *
03072 *    3.  VERIFY THAT THE FINANCIAL RESPONSIBLITY.  THE FINANCIAL *
03073 *        RESPONSIBILITY SHOULD BE THE SAME FOR THE ENTIRE BATCH. *
03074 *        IF IT CHANGES, IT IS AN ERROR.                          *
03075 *                                                                *
03076 ******************************************************************
03077                                                                   
03078      IF PI-AR-PROCESSING                                          
03079         NEXT SENTENCE                                             
03080      ELSE                                                         
03081         GO TO 1590-EXIT.                                          
03082                                                                   
03083      MOVE +0                     TO WS-ACCT-SUB.                  
03084                                                                   
03085  1525-FIND-ACCT-DT-RANGE.                                         
03086                                                                   
03087      ADD  +1                     TO WS-ACCT-SUB.                  
03088                                                                   
03089      IF WS-ACCT-SUB GREATER +32                                   
03090         MOVE  ER-2119            TO EMI-ERROR                     
03091         IF PI-MAP-NAME = EL630B                                   
03092            MOVE -1               TO BPFENTRL                      
03093            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
03094            GO TO 8200-SEND-DATAONLY                               
03095         ELSE                                                      
03096            MOVE -1               TO CEFFDT-LEN (WS-SUB2)          
03097            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
03098            GO TO 1590-EXIT.                                       
03099                                                                   
03100      IF WS-CONVERTED-EFFDT NOT LESS PI-ACCT-EFF-DT                
03101                                                  (WS-ACCT-SUB)    
03102         IF WS-CONVERTED-EFFDT  LESS PI-ACCT-EXP-DT                
03103                                                  (WS-ACCT-SUB)    
03104            NEXT SENTENCE                                          
03105         ELSE                                                      
03106            GO TO 1525-FIND-ACCT-DT-RANGE.                         
03107                                                                   
03108      IF PI-ACCOUNT-AGENT = SPACES                                 
03109         MOVE PI-ACCT-AGENT  (WS-ACCT-SUB) TO PI-ACCOUNT-AGENT     
03110         MOVE PI-REMIT-AGENT (WS-ACCT-SUB) TO PI-FIN-RESP.         
03111                                                                   
03112      IF PI-ACCT-AGENT  (WS-ACCT-SUB) = PI-ACCOUNT-AGENT           
03113         NEXT SENTENCE                                             
03114      ELSE                                                         
03115         MOVE 'Y'                 TO PI-ACCT-AGENT-ERROR-SW.       
03116                                                                   
03117      IF PI-REMIT-AGENT (WS-ACCT-SUB) = PI-FIN-RESP                
03118         NEXT SENTENCE                                             
03119      ELSE                                                         
03120         MOVE 'Y'                 TO PI-FIN-RESP-ERROR-SW.         
03121                                                                   
03122  1590-EXIT.                                                       
03123       EXIT.                                                       
03124                                                                   
03125      EJECT                                                        
03126                                                                   
03127  2000-BROWSE-FWD.                                                 
03128      MOVE LOW-VALUES             TO EL630BI.                      
03129                                                                   
03130      ADD +1                      TO PI-SAV-BATCH-SEQ.             
03131                                                                   
03132      EXEC CICS HANDLE CONDITION                                   
03133          NOTFND (2020-END-FILE)                                   
03134      END-EXEC.                                                    
03135                                                                   
03136      EXEC CICS READ                                               
03137          SET     (ADDRESS OF PENDING-BUSINESS)                    
03138          DATASET (FILE-ID-ERPNDB)                                 
03139          RIDFLD  (PI-SAV-ENDING-ERPNDB-KEY)                       
03140          GTEQ                                                     
03141      END-EXEC.                                                    
03142                                                                   
03143      IF PB-COMPANY-CD  = PI-SAV-COMP-CD  AND                      
03144         PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH                       
03145          NEXT SENTENCE                                            
03146      ELSE                                                         
03147          GO TO 2020-END-FILE.                                     
03148                                                                   
03149      IF PB-BATCH-TRAILER                                          
03150          GO TO 2020-END-FILE.                                     
03151                                                                   
03152      MOVE PB-BATCH-SEQ-NO        TO PI-SAV-BATCH-SEQ.             
03153                                                                   
03154      IF PB-ISSUE                                                  
03155          MOVE EL630B             TO PI-MAP-NAME                   
03156          MOVE AL-SANOF           TO BDELHDGA                      
03157          PERFORM 7000-FORMAT-ISSUE-SCREEN THRU 7090-EXIT          
03158      ELSE                                                         
03159          MOVE EL630C             TO PI-MAP-NAME                   
03160          MOVE AL-SANOF           TO CDELHDGA                      
03161          PERFORM 7100-FORMAT-CANCEL-SCREEN THRU 7190-EXIT.        
03162                                                                   
03163  2010-SEND-MAP.                                                   
03164      GO TO 8100-SEND-INITIAL-MAP.                                 
03165                                                                   
03166  2020-END-FILE.                                                   
03167      MOVE SPACE                  TO PI-DISPLAY-SW.                
03168                                                                   
03169      IF PI-MAP-NAME = EL630B                                      
03170          MOVE LOW-VALUES         TO EL630BI                       
03171          MOVE -1                 TO BPFENTRL                      
03172          ADD +1                     PI-LAST-SEQ-NO-ADDED          
03173                GIVING PI-NEXT-DISPLAY-SEQ-NO                      
03174          PERFORM 8550-SET-MAP-SEQ-NOS                             
03175      ELSE                                                         
03176          MOVE LOW-VALUES         TO EL630BI                       
03177          MOVE -1                 TO CPFENTRL                      
03178          ADD +1                     PI-LAST-SEQ-NO-ADDED          
03179                GIVING PI-NEXT-DISPLAY-SEQ-NO                      
03180          PERFORM 8550-SET-MAP-SEQ-NOS                             
03181                  VARYING WS-SUB2 FROM +1 BY +1                    
03182                  UNTIL WS-SUB2 GREATER +4.                        
03183                                                                   
03184      MOVE ER-2217                TO EMI-ERROR.                    
03185      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03186      GO TO 2010-SEND-MAP.                                         
03187                                                                   
03188      EJECT                                                        
03189                                                                   
03190  2100-BROWSE-BKWD.                                                
03191      MOVE LOW-VALUES             TO EL630BI.                      
03192                                                                   
03193      SUBTRACT +1             FROM PI-SAV-BATCH-SEQ.               
03194                                                                   
03195      IF PI-SAV-BATCH-SEQ NOT GREATER +0                           
03196          GO TO 2120-END-FILE.                                     
03197                                                                   
03198      EXEC CICS HANDLE CONDITION                                   
03199          NOTFND (2100-BROWSE-BKWD)                                
03200      END-EXEC.                                                    
03201                                                                   
03202      EXEC CICS READ                                               
03203          SET     (ADDRESS OF PENDING-BUSINESS)                    
03204          DATASET (FILE-ID-ERPNDB)                                 
03205          RIDFLD  (PI-SAV-ENDING-ERPNDB-KEY)                       
03206      END-EXEC.                                                    
03207                                                                   
03208      IF PB-COMPANY-CD  = PI-SAV-COMP-CD  AND                      
03209         PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH                       
03210          NEXT SENTENCE                                            
03211      ELSE                                                         
03212          GO TO 2120-END-FILE.                                     
03213                                                                   
03214      IF PB-BATCH-TRAILER                                          
03215          GO TO 2120-END-FILE.                                     
03216                                                                   
03217      IF PB-ISSUE                                                  
03218          MOVE EL630B             TO PI-MAP-NAME                   
03219          MOVE AL-SANOF           TO BDELHDGA                      
03220          PERFORM 7000-FORMAT-ISSUE-SCREEN THRU 7090-EXIT          
03221      ELSE                                                         
03222          MOVE EL630C             TO PI-MAP-NAME                   
03223          MOVE AL-SANOF           TO CDELHDGA                      
03224          PERFORM 7100-FORMAT-CANCEL-SCREEN THRU 7190-EXIT.        
03225                                                                   
03226  2110-SEND-MAP.                                                   
03227      GO TO 8100-SEND-INITIAL-MAP.                                 
03228                                                                   
03229  2120-END-FILE.                                                   
03230      MOVE ER-2431                TO EMI-ERROR.                    
03231      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03232      MOVE ER-0000                TO EMI-ERROR.                    
03233      MOVE ZEROS                  TO PI-SAV-BATCH-SEQ.             
03234      GO TO 2000-BROWSE-FWD.                                       
03235                                                                   
03236      EJECT                                                        
03237                                                                   
03238  3000-CONTINUE-ENTRY.                                             
03239      MOVE PI-SAV-ENDING-ERPNDB-KEY TO ERPNDB-KEY.                 
03240                                                                   
03241      EXEC CICS HANDLE CONDITION                                   
03242          NOTFND (3300-REC-NOT-FND)                                
03243      END-EXEC.                                                    
03244                                                                   
03245      EXEC CICS STARTBR                                            
03246          DATASET (FILE-ID-ERPNDB)                                 
03247          RIDFLD  (ERPNDB-KEY)                                     
03248          GTEQ                                                     
03249      END-EXEC.                                                    
03250                                                                   
03251  3100-READ-LOOP.                                                  
03252      EXEC CICS HANDLE CONDITION                                   
03253          ENDFILE (3200-END-BROWSE)                                
03254      END-EXEC.                                                    
03255                                                                   
03256      EXEC CICS READNEXT                                           
03257          SET     (ADDRESS OF PENDING-BUSINESS)                    
03258          DATASET (FILE-ID-ERPNDB)                                 
03259          RIDFLD  (ERPNDB-KEY)                                     
03260      END-EXEC.                                                    
03261                                                                   
03262      IF PB-COMPANY-CD  = PI-SAV-COMP-CD   AND                     
03263         PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH                       
03264          NEXT SENTENCE                                            
03265      ELSE                                                         
03266          GO TO 3200-END-BROWSE.                                   
03267                                                                   
03268      IF NOT PB-BATCH-TRAILER                                      
03269          GO TO 3110-NOT-BATCH-TRAILER.                            
03270                                                                   
03271  3105-PRIME-PI-COUNTS.                                            
03272      IF PI-LF-ISS-REMITTED = ZEROS                                
03273          MOVE PB-B-LF-ISS-PRM-REMITTED  TO PI-LF-ISS-REMITTED.    
03274                                                                   
03275      IF PI-AH-ISS-REMITTED = ZEROS                                
03276          MOVE PB-B-AH-ISS-PRM-REMITTED  TO PI-AH-ISS-REMITTED.    
03277                                                                   
03278      IF PI-ISS-CNT-REMITTED = ZEROS                               
03279          MOVE PB-B-ISSUE-CNT-REMITTED   TO PI-ISS-CNT-REMITTED.   
03280                                                                   
03281      IF PI-CAN-CNT-REMITTED = ZEROS                               
03282          MOVE PB-B-CANCEL-CNT-REMITTED  TO PI-CAN-CNT-REMITTED.   
03283                                                                   
03284      IF PI-LF-CAN-REMITTED = ZEROS                                
03285          MOVE PB-B-LF-CAN-PRM-REMITTED  TO PI-LF-CAN-REMITTED.    
03286                                                                   
03287      IF PI-AH-CAN-REMITTED = ZEROS                                
03288          MOVE PB-B-AH-CAN-PRM-REMITTED  TO PI-AH-CAN-REMITTED.    
03289                                                                   
03290      GO TO 3200-END-BROWSE.                                       
03291                                                                   
03292  3110-NOT-BATCH-TRAILER.                                          
03293      IF PB-ISSUE                                                  
03294          ADD PB-I-LF-PREMIUM-AMT     TO PI-LF-ISS-ENTERED         
03295          ADD PB-I-LF-ALT-PREMIUM-AMT TO PI-LF-ISS-ENTERED         
03296          ADD PB-I-AH-PREMIUM-AMT     TO PI-AH-ISS-ENTERED         
03297          ADD +1                      TO PI-ISS-CNT-ENTERED        
03298      ELSE                                                         
03299          ADD PB-C-LF-CANCEL-AMT      TO PI-LF-CAN-ENTERED         
03300          ADD PB-C-AH-CANCEL-AMT      TO PI-AH-CAN-ENTERED         
03301          ADD +1                      TO PI-CAN-CNT-ENTERED.       
03302                                                                   
03303      MOVE PB-BATCH-SEQ-NO            TO PI-LAST-SEQ-NO-ADDED      
03304                                         PI-SAV-BATCH-SEQ.         
03305                                                                   
03306      GO TO 3100-READ-LOOP.                                        
03307                                                                   
03308  3200-END-BROWSE.                                                 
03309      EXEC CICS ENDBR                                              
03310          DATASET (FILE-ID-ERPNDB)                                 
03311      END-EXEC.                                                    
03312                                                                   
03313      ADD +1                         PI-LAST-SEQ-NO-ADDED          
03314                             GIVING PI-NEXT-DISPLAY-SEQ-NO.        
03315                                                                   
03316      IF PI-MAINT-FUNC = 'B'                                       
03317          MOVE ZEROS              TO PI-SAV-BATCH-SEQ              
03318          GO TO 2000-BROWSE-FWD                                    
03319      ELSE                                                         
03320          ADD +1                  TO PI-SAV-BATCH-SEQ.             
03321                                                                   
03322      IF PI-LF-ISS-REMITTED  = ZEROS  AND                          
03323         PI-AH-ISS-REMITTED  = ZEROS  AND                          
03324         PI-LF-CAN-REMITTED  = ZEROS  AND                          
03325         PI-AH-CAN-REMITTED  = ZEROS  AND                          
03326         PI-ISS-CNT-REMITTED = ZEROS  AND                          
03327         PI-CAN-CNT-REMITTED = ZEROS                               
03328          MOVE  EL630B            TO PI-MAP-NAME                   
03329      ELSE                                                         
03330          IF PI-LF-ISS-REMITTED  = ZEROS AND                       
03331             PI-AH-ISS-REMITTED  = ZEROS AND                       
03332             PI-ISS-CNT-REMITTED = ZEROS                           
03333              MOVE  EL630C        TO PI-MAP-NAME                   
03334          ELSE                                                     
03335              MOVE  EL630B        TO PI-MAP-NAME.                  
03336                                                                   
03337      IF PI-MAP-NAME = EL630B                                      
03338          PERFORM 8550-SET-MAP-SEQ-NOS                             
03339      ELSE                                                         
03340          PERFORM 8550-SET-MAP-SEQ-NOS                             
03341                  VARYING WS-SUB2 FROM +1 BY +1                    
03342                  UNTIL WS-SUB2 GREATER +4.                        
03343                                                                   
03344      GO TO 8100-SEND-INITIAL-MAP.                                 
03345                                                                   
03346  3300-REC-NOT-FND.                                                
03347      MOVE ER-2212                TO EMI-ERROR.                    
03348      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03349      GO TO 8100-SEND-INITIAL-MAP.                                 
03350                                                                   
03351      EJECT                                                        
03352                                                                   
03353  4000-BUILD-ISSUE-RECORD.                                         
03354      IF BSEQ  GREATER PI-LAST-SEQ-NO-ADDED                        
03355          GO TO 4100-ADD-ISSUE-RECORD.                             
03356                                                                   
03357 ******************************************************************
03358 *   THE DATA ENTRY SYSTEM ALLOWS BROWSING OF THE CURRENT BUS.    *
03359 *   FILE. THE DATA ENTRY SYS. DOES NOT HAVE A MAINT. FUNCTION.   *
03360 *   THE PROGRAM ASSUMES THAT IF A MATCH ON THE READ FOR UPDATE   *
03361 *   IS SUCCESSFUL, THE RECORD HAS PREVIOUSLY BEEN DISPLAYED      *
03362 *   THRUOUGH A BROWSE.  CHANGES ARE APPLIED AND THE PB-RECORD IS *
03363 *   REWRITTEN, ELSE A NEW PENDING BUS. RECORD IS ADDED.          *
03364 ******************************************************************
03365                                                                   
03366      MOVE PI-COMPANY-CD          TO ERPNDB-COMP-CD.               
03367      MOVE PI-SAV-ENTRY-BATCH     TO ERPNDB-ENTRY-BATCH.           
03368      MOVE BSEQ                   TO ERPNDB-BATCH-SEQ.             
03369                                                                   
03370      EXEC CICS HANDLE CONDITION                                   
03371          NOTFND (4100-ADD-ISSUE-RECORD)                           
03372      END-EXEC.                                                    
03373                                                                   
03374      EXEC CICS READ                                               
03375          SET     (ADDRESS OF PENDING-BUSINESS)                    
03376          DATASET (FILE-ID-ERPNDB)                                 
03377          RIDFLD  (ERPNDB-KEY)                                     
03378          UPDATE                                                   
03379      END-EXEC.                                                    
03380                                                                   
03381      MOVE PB-CONTROL-PRIMARY     TO ERPNDM-KEY.                   
03382                                                                   
03383      MOVE 'B'                    TO JP-RECORD-TYPE                
03384      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
03385      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
03386      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
041320     PERFORM 8400-LOG-JOURNAL-RECORD  *> Before Issue Image
03388                                                                   
03389      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             
03390      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
03391      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.             
03392                                                                   
03393      IF BLAST-NAME-LEN GREATER ZEROS                              
03394          MOVE BLAST-NAME         TO PB-I-INSURED-LAST-NAME.       
03395                                                                   
03396      IF B1ST-NAME-LEN GREATER ZEROS                               
03397          MOVE B1ST-NAME          TO PB-I-INSURED-FIRST-NAME.      
03398                                                                   
03399      IF BINIT-LEN     GREATER ZEROS                               
03400          MOVE BINIT              TO PB-I-INSURED-MIDDLE-INIT.     
03401                                                                   
03402      IF BAGE-LEN      GREATER ZEROS                               
03403          MOVE WS-BAGE            TO PB-I-AGE.                     
03404                                                                   
03405      IF BJNT-AGE-LEN  GREATER ZEROS                               
03406          MOVE WS-BJNT-AGE        TO PB-I-JOINT-AGE.               
03407                                                                   
03408      IF BBIRTH-LEN > ZEROS
03409         MOVE WS-CONVERTED-BIRTH (1)
                                       TO PB-I-BIRTHDAY
           END-IF

081108     IF BJNTDOB-LEN > ZEROS
081108        MOVE WS-CONVERTED-BIRTH (2)
081108                                 TO PB-I-JOINT-BIRTHDAY
042114     end-if

03414      IF BTERM1-LEN > ZEROS                             
03415         MOVE WS-BTERM1           TO PB-I-LF-TERM.                 
03416                                                                   
03417      IF BTERM2-LEN > ZEROS                             
03418         MOVE WS-BTERM2             TO PB-I-AH-TERM.                
03419                                                                   
03420      IF BLN-TERM-LEN    GREATER ZEROS                             
03421          MOVE WS-BLN-TERM        TO PB-I-LOAN-TERM.               
03422                                                                   
03423 *    IF BFREQ-LEN       GREATER ZEROS                             
03424 *        MOVE WS-BFREQ           TO PB-I-PAY-FREQUENCY.           
03425                                                                   
03426 *    IF BSKPCD-LEN      GREATER ZEROS                             
03427 *        MOVE BSKPCD             TO PB-I-SKIP-CODE.               
03428                                                                   
03429 *    IF BMODE-LEN       GREATER ZEROS                             
03430 *        MOVE BMODE              TO PB-I-TERM-TYPE.               
03431                                                                   
03432 *    IF BPMTS-LEN       GREATER ZEROS                             
03433 *        MOVE WS-BPMTS           TO PB-I-NO-OF-PAYMENTS.          
03434                                                                   
03435 *    IF BPMT-LEN        GREATER ZEROS                             
03436 *        MOVE WS-BPMT            TO PB-I-PAYMENT-AMOUNT.          
03437                                                                   
03438 *    IF BPOLICY-LEN     GREATER ZEROS                             
03439 *        MOVE BPOLICY            TO PB-I-POLICY-FORM-NO.          
03440                                                                   
03441 ******************************************************************
03442 *          IF BTYPE = ZEROS DELETE LIFE COVERAGE.                *
03443 *                                                                *
03444 *          IF BTYPE = SPACES ZERO OUT BENEFIT CODE.              *
03445 ******************************************************************
03446                                                                   
03447      IF BTYPE1-LEN > ZEROS                         
03448         IF BTYPE1           NOT = SPACES AND ZEROS                
03449            MOVE BTYPE1            TO PB-I-LF-INPUT-CD             
03450            MOVE WS-EDITED-LF-CODE TO PB-I-LIFE-BENEFIT-CD         
03451            MOVE WS-LF-ABBR-DESC   TO PB-I-LF-ABBR                 
03452         ELSE                                                      
03453            IF BTYPE1      = SPACES                                
03454               MOVE SPACES         TO PB-I-LF-INPUT-CD             
03455                                      PB-I-LF-ABBR                 
03456               MOVE ZEROS          TO PB-I-LIFE-BENEFIT-CD         
03457            ELSE                                                   
03458               SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED 
03459               SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM               
03460                        PI-LF-ISS-ENTERED                          
03461               MOVE SPACES         TO PB-I-LF-INPUT-CD             
03462                                      PB-I-LF-ABBR                 
03463               MOVE ZEROS          TO PB-I-LF-TERM                 
03464                                      PB-I-LF-BENEFIT-AMT          
03465                                      PB-I-LF-PREMIUM-AMT          
03466                                      PB-I-LF-BENEFIT-CD           
03467                                      PB-I-LF-PREM-CALC            
03468                                      PB-I-LF-ALT-BENEFIT-AMT      
03469                                      PB-I-LF-ALT-PREMIUM-AMT      
03470                                      PB-I-LF-CRIT-PER             
03471               MOVE LOW-VALUES     TO PB-I-LF-EXPIRE-DT.           
03472                                                                   
03473      IF  BBENE1-LEN > ZEROS                         
03474          MOVE WS-BBEN1           TO PB-I-LF-BENEFIT-AMT.          
03475                                                                   
03476      IF  BALT-BEN1-LEN > ZEROS                      
03477          MOVE WS-BALT-BEN1          TO PB-I-LF-ALT-BENEFIT-AMT.   
03478                                                                   
03479      IF  BPREM1-LEN > ZEROS                         
03480          IF WS-BPREM1       = WS-ALL-NINES OR                     
03481             WS-BPREM1       GREATER WS-ALL-NINES                  
03482             SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED   
03483             MOVE ZEROS           TO PB-I-LF-PREMIUM-AMT           
03484             MOVE '?'             TO PB-I-LF-CALC-FLAG             
03485          ELSE                                                     
03486             SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED   
03487             MOVE WS-BPREM1       TO PB-I-LF-PREMIUM-AMT           
03488             ADD  WS-BPREM1       TO PI-LF-ISS-ENTERED             
03489             MOVE SPACE           TO PB-I-LF-CALC-FLAG.            
03490                                                                   
03491      IF  BALT-PREM1-LEN > ZEROS                      
03492          SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM PI-LF-ISS-ENTERED  
03493          MOVE WS-BALT-PREM1      TO PB-I-LF-ALT-PREMIUM-AMT       
03494          ADD  WS-BALT-PREM1      TO PI-LF-ISS-ENTERED.            
03495                                                                   
011904     IF  BALT-PREM2-LEN > ZEROS
011904         MOVE WS-BALT-PREM2      TO PB-I-TOT-FEES
011904     END-IF
011904                                                                  
03496 ******************************************************************
03497 *          IF BTYPE = ZEROS DELETE A&H COVERAGE.                 *
03498 *                                                                *
03499 *          IF BTYPE = SPACES ZERO OUT BENEFIT CODE.              *
03500 ******************************************************************
03501                                                                   
03502      IF BTYPE2-LEN > ZEROS                         
03503         IF BTYPE2           NOT = SPACES AND ZEROS                
03504            MOVE BTYPE2            TO PB-I-AH-INPUT-CD             
03505            MOVE WS-EDITED-LF-CODE TO PB-I-AH-BENEFIT-CD           
03506            MOVE WS-LF-ABBR-DESC   TO PB-I-AH-ABBR                 
03507         ELSE                                                      
03508            IF BTYPE2      = SPACES                                
03509               MOVE SPACES         TO PB-I-AH-INPUT-CD             
03510                                      PB-I-AH-ABBR                 
03511               MOVE ZEROS          TO PB-I-AH-BENEFIT-CD           
03512            ELSE                                                   
03513               SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED 
03514               MOVE SPACES         TO PB-I-AH-INPUT-CD             
03515                                      PB-I-AH-ABBR                 
03516               MOVE ZEROS          TO PB-I-AH-TERM                 
03517                                      PB-I-AH-BENEFIT-AMT          
03518                                      PB-I-AH-PREMIUM-AMT          
03519                                      PB-I-AH-BENEFIT-CD           
03520                                      PB-I-AH-PREM-CALC            
03521                                      PB-I-AH-CRIT-PER
011904                                     PB-I-TOT-FEES
011904                                     PB-I-TOT-FEES-CALC
03522               MOVE LOW-VALUES     TO PB-I-AH-EXPIRE-DT.           
03523                                                                   
03524      IF  BBENE2-LEN > ZEROS                         
03525          MOVE WS-BBEN2           TO PB-I-AH-BENEFIT-AMT.          
03526                                                                   
03527      IF  BPREM2-LEN > ZEROS                         
03528          IF WS-BPREM2       = WS-ALL-NINES OR                     
03529             WS-BPREM2       GREATER WS-ALL-NINES                  
03530             SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED   
03531             MOVE ZEROS              TO PB-I-AH-PREMIUM-AMT        
03532             MOVE '?'                TO PB-I-AH-CALC-FLAG          
03533          ELSE                                                     
03534             SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED   
03535             MOVE WS-BPREM2       TO PB-I-AH-PREMIUM-AMT           
03536             ADD  WS-BPREM2       TO PI-AH-ISS-ENTERED             
03537             MOVE SPACE           TO PB-I-AH-CALC-FLAG.            
03538                                                                   
03539 *    IF BCRIT-PERD2-LEN > ZEROS                   
03540 *       MOVE WS-BCRIT-PERD2 (1)   TO PB-I-LF-CRIT-PER.
           MOVE ZEROS                  TO PB-I-LF-CRIT-PER
03541                                                                   
03542      IF BCRIT-PERD2-LEN > ZEROS                   
03543         MOVE WS-BCRIT-PERD2       TO PB-I-AH-CRIT-PER.            
03544                                                                   
03545 *    IF BIND-GRP-LEN     GREATER ZEROS                            
03546 *        MOVE BIND-GRP           TO PB-I-INDV-GRP-OVRD.           
03547                                                                   
03548 *    IF BRTCLS-LEN       GREATER ZEROS                            
03549 *        MOVE BRTCLS             TO PB-I-RATE-CLASS-OVRD.         
03550                                                                   
03551 *    IF BSIG-LEN         GREATER ZEROS                            
03552 *        MOVE BSIG               TO PB-I-SIG-SW.                  

050713     IF BAPR-LEN > zeros
050713        MOVE WS-BAPR             TO PB-I-LOAN-APR
050713     else
050713        if (pi-company-id = 'CID')
050713           and (ws-lf-earnings-calc = '5')
050713           move +99.9999         to pb-i-loan-apr
050713        else
050713           move zeros            to pb-i-loan-apr
050713        end-if
050713     end-if

03560 *    IF BMEM-NO-LEN      GREATER ZEROS                            
03561 *        MOVE BMEM-NO        TO PB-I-MEMBER-NO.                   
03562                                                                   
03563 *    IF BMICROFILM-NO-LEN  GREATER  ZEROS                         
03564 *        MOVE WS-I-MICRO-NO      TO  PB-I-MICROFILM-NO.           
03565                                                                   
03566      IF BLN-OFFICER-LEN  GREATER ZEROS                            
03567          MOVE BLN-OFFICER        TO PB-I-LOAN-OFFICER.            
03568                                                                   
03569 *    IF BEXPIRE-LEN    (1)  GREATER ZEROS                         
03570 *       MOVE WS-CONVERTED-EXPIRDT (1) TO PB-I-LF-EXPIRE-DT.       
03571                                                                   
03572 *    IF BEXPIRE-LEN    (2)  GREATER ZEROS                         
03573 *       MOVE WS-CONVERTED-EXPIRDT (2) TO PB-I-AH-EXPIRE-DT.       
03574                                                                   
03575      IF B1ST-PMT-LEN    GREATER ZEROS                             
03576         MOVE WS-CONVERTED-1ST-PMT-DT TO  PB-I-1ST-PMT-DT.         
03577                                                                   
03578 *    IF BDAYS-LEN       GREATER ZEROS                             
03579 *       MOVE WS-BDAYS            TO PB-I-TERM-IN-DAYS             
03580 *                                   PB-I-EXTENTION-DAYS.          
03581                                                                   
03582 *    IF BDAYS-LEN GREATER ZEROS                                   
03583 *       MOVE '2'                 TO PB-I-DATA-ENTRY-SW.           
03584                                                                   
03585 *    IF BEXPIRE-LEN    (1)  GREATER ZEROS                         
03586 *       IF WS-CONVERTED-EXPIRDT   (1) GREATER LOW-VALUES          
03587 *          MOVE '3'              TO PB-I-DATA-ENTRY-SW.           
03588                                                                   
03589 *    IF BEXPIRE-LEN    (2)  GREATER ZEROS                         
03590 *       IF WS-CONVERTED-EXPIRDT   (2) GREATER LOW-VALUES          
03591 *          MOVE '3'              TO PB-I-DATA-ENTRY-SW.           
03592                                                                   
03593      IF B1ST-PMT-LEN GREATER ZEROS                                
03594         MOVE '4'                 TO PB-I-DATA-ENTRY-SW.           
03595                                                                   
03596 *    IF PB-EXT-DAYS-PROCESSING                                    
03597 *       IF PB-I-EXTENTION-DAYS = ZEROS                            
03598 *          MOVE '1'              TO PB-I-DATA-ENTRY-SW.           
03599                                                                   
03600 *    IF PB-EXPIRE-DT-PROCESSING                                   
03601 *       IF PB-I-LF-EXPIRE-DT = LOW-VALUES AND                     
03602 *          PB-I-AH-EXPIRE-DT = LOW-VALUES                         
03603 *          MOVE '1'              TO PB-I-DATA-ENTRY-SW.           
03604                                                                   
03605      IF PB-1ST-PMT-DT-PROCESSING                                  
03606         IF PB-I-1ST-PMT-DT = LOW-VALUES                           
03607            MOVE '1'              TO PB-I-DATA-ENTRY-SW.           
03608                                                                   
03609 *    IF BRINCD-LEN               GREATER ZEROS                    
03610 *       MOVE BRINCD              TO PB-I-SPECIAL-REIN-CODE.       
03611                                                                   
03612 *    IF BBILLCD-LEN              GREATER ZEROS                    
03613 *       MOVE BBILLCD             TO PB-RECORD-BILL                
03614 *    ELSE                                                         
03615 *        IF PB-COMPANY-ID = 'LAP'  OR  'RMC'                      
03616 *            MOVE 'H'            TO PB-RECORD-BILL.               
03617                                                                   
03618 *    IF BENTRY-LEN   NOT = ZEROS                                  
03619 *       IF BENTRY = 'U' OR 'D'                                    
03620 *          MOVE BENTRY           TO PB-I-UNDERWRITING-STATUS      
03621 *                                   PB-BATCH-ENTRY                
03622 *       ELSE                                                      
03623 *          MOVE BENTRY           TO PB-BATCH-ENTRY.               
03624                                                                   
03625 *    IF BFORCE-LEN   NOT = ZEROS                                  
03626 *       MOVE BFORCE              TO PB-FORCE-CODE.                
03627                                                                   
081606     IF BVIN-LEN > ZEROS
081606        MOVE BVIN-NOI            TO PB-I-VIN
081606     END-IF
 
03628 *    IF BLIVES-LEN          GREATER ZEROS                         
03629 *       MOVE WS-BLIVES           TO PB-I-LIVES.                   
03630                                                                   
03631      IF BJNT-1ST-NAME-LEN   GREATER ZEROS                         
03632          MOVE BJNT-1ST-NAME      TO PB-I-JOINT-FIRST-NAME.        
03633                                                                   
03634      IF BJNT-INIT-LEN       GREATER ZEROS                         
03635          MOVE BJNT-INIT          TO PB-I-JOINT-MIDDLE-INIT.       
03636                                                                   
03637      IF BJNT-LST-NAME-LEN   GREATER ZEROS                         
03638          MOVE BJNT-LST-NAME      TO PB-I-JOINT-LAST-NAME.         
03639                                                                   
03640      IF BBENEFICIARY-LEN    GREATER ZEROS                         
03641          MOVE BBENEFICIARY       TO PB-I-BENEFICIARY-NAME.        
03642                                                                   
03643      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.          
03644      MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.            
03645                                                                   
03646      IF PI-MAIL-YES                                               
03647         MOVE '1'                 TO PB-I-MAIL-ADDRS-SW.           
03648                                                                   
03649      MOVE 'C'                    TO JP-RECORD-TYPE.               
03650      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
03651      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
03652                                                                   
03653      EXEC CICS REWRITE                                            
03654          DATASET (FILE-ID-ERPNDB)                                 
03655          FROM    (PENDING-BUSINESS)                               
03656      END-EXEC.                                                    
03657                                                                   
03658      MOVE ERPNDB-KEY             TO PI-SAV-ENDING-ERPNDB-KEY.     
03659                                                                   
041220     PERFORM 8400-LOG-JOURNAL-RECORD  *> After Issue Image
03661                                                                   
03662      IF EIBAID = DFHENTER                                         
03663          MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ                      
03664          ADD +1                      TO PI-NEXT-DISPLAY-SEQ-NO    
03665          MOVE AL-SABON               TO BSEQ-ATTRB.               
03666                                                                   
03667      IF PI-MAIL-YES                                               
03668         NEXT SENTENCE                                             
03669      ELSE                                                         
03670         GO TO 4900-EXIT.                                          
03671                                                                   
03672      IF BLAST-NAME-LEN = ZEROS AND                                
03673         B1ST-NAME-LEN  = ZEROS AND                                
03674         BINIT-LEN      = ZEROS AND                                
03675         BADDRS1-LEN    = ZEROS AND                                
03676         BADDRS2-LEN    = ZEROS AND                                
03677         BCITY-LEN      = ZEROS AND
              BSTATE-LEN     = ZEROS AND
03678         BZIPCDE-LEN    = ZEROS AND                                
03679**       BZIP4-LEN      = ZEROS AND                                
03680 *       BPHONE-LEN     = ZEROS AND                                
              BBENEFICIARY-LEN = ZEROS AND
              BCADDR1-LEN    = ZEROS AND
              BCADDR2-LEN    = ZEROS AND
              BCCITY-LEN     = ZEROS AND
              BCSTATE-LEN    = ZEROS AND
              BCZIPCD-LEN    = ZEROS
03682         GO TO 4900-EXIT.                                          
03683                                                                   
03684      EXEC CICS HANDLE CONDITION                                   
03685          NOTFND (4185-ADD-MAILING-RECORD)                         
03686      END-EXEC.                                                    
03687                                                                   
03688      EXEC CICS READ                                               
03689          SET     (ADDRESS OF PENDING-MAILING-DATA)                
03690          DATASET (FILE-ID-ERPNDM)                                 
03691          RIDFLD  (ERPNDM-KEY)                                     
03692          UPDATE                                                   
03693      END-EXEC.                                                    
03694                                                                   
03695      MOVE 'B'                    TO JP-RECORD-TYPE.               
03696      MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.               
03697      MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
03698      MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.                   
03699      PERFORM 8400-LOG-JOURNAL-RECORD.                             
03700                                                                   
03701      MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY.             
03702      MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.         
03703      MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT.             
03704                                                                   
03705      IF BLAST-NAME-LEN      GREATER ZEROS                         
03706          MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.         
03707                                                                   
03708      IF B1ST-NAME-LEN       GREATER ZEROS                         
03709          MOVE B1ST-NAME          TO PM-INSURED-FIRST-NAME.        
03710                                                                   
03711      IF BINIT-LEN           GREATER ZEROS                         
03712          MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.       
03713                                                                   
03714      IF BAGE-LEN            GREATER ZEROS                         
03715          MOVE WS-BAGE            TO PM-INSURED-ISSUE-AGE.         
03716                                                                   
03717      IF BBIRTH-LEN > ZEROS
03718         MOVE  WS-CONVERTED-BIRTH (1) TO PM-INSURED-BIRTH-DT
           END-IF

081108     IF BJNTDOB-LEN > ZEROS
081108        MOVE  WS-CONVERTED-BIRTH (2) TO PM-JOINT-BIRTH-DT
081108     END-IF

03720      IF BLAST-NAME-LEN      GREATER ZEROS                         
03721          MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.         
03722                                                                   
03723      IF BINIT-LEN           GREATER ZEROS                         
03724          MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.       
03725                                                                   
03726      IF BADDRS1-LEN         GREATER ZERO                          
03727          MOVE BADDRS1            TO PM-ADDRESS-LINE-1.            
03728                                                                   
03729      IF BADDRS2-LEN         GREATER ZERO                          
03730          MOVE BADDRS2            TO PM-ADDRESS-LINE-2.            
03731                                                                   
03732      IF BCITY-LEN > 0
03733         MOVE BCITY               TO PM-CITY
           END-IF

03732      IF BSTATE-LEN > 0
03733         MOVE BSTATE              TO PM-STATE
           END-IF
03734                                                                   
03735      IF BZIPCDE-LEN GREATER ZEROS                                 
03736         MOVE BZIPCDE             TO WS-ZIP-CODE                  
03737      ELSE                                                         
03738         GO TO 4010-CRED-BENE
           END-IF

03740      IF WS-CANADIAN-ZIP                                           
03741         IF WS-ZIP-4 = SPACE  OR  '-'                             
03742            MOVE WS-ZIP-CAN-2-POST1   TO PM-CAN-POST1            
03743            MOVE WS-ZIP-CAN-2-POST2   TO PM-CAN-POST2            
03744         ELSE                                                     
03745            MOVE WS-ZIP-CAN-1-POST1   TO PM-CAN-POST1            
03746            MOVE WS-ZIP-CAN-1-POST2   TO PM-CAN-POST2
              END-IF
03747      ELSE                                                         
03748         IF WS-ZIP-6 = SPACE  OR  '-'                             
03749            MOVE WS-ZIP-AM-2-CODE     TO PM-ZIP-CODE             
03750            MOVE WS-ZIP-AM-2-PLUS4    TO PM-ZIP-PLUS4            
03751         ELSE                                                     
03752            MOVE WS-ZIP-AM-1-CODE     TO PM-ZIP-CODE             
03753            MOVE WS-ZIP-AM-1-PLUS4    TO PM-ZIP-PLUS4
              END-IF
           END-IF

           .
       4010-CRED-BENE.

           IF BBENEFICIARY-LEN > ZEROS
              MOVE BBENEFICIARY        TO PM-CRED-BENE-NAME
           END-IF                      
           IF BCADDR1-LEN > ZEROS      
              MOVE BCADDR1             TO PM-CRED-BENE-ADDR
           END-IF                      
           IF BCADDR2-LEN > ZEROS      
              MOVE BCADDR2             TO PM-CRED-BENE-ADDR2
           END-IF                      
           IF BCCITY-LEN > ZEROS      
              MOVE BCCITY              TO PM-CRED-BENE-CITY
           END-IF
           IF BCSTATE-LEN > ZEROS      
              MOVE BCSTATE             TO PM-CRED-BENE-STATE
           END-IF

           IF BCZIPCD-LEN > ZEROS                                 
              MOVE BCZIPCD             TO WS-ZIP-CODE
           ELSE                                                         
              GO TO 4010-CONTINUE
           END-IF

03740      IF WS-CANADIAN-ZIP                                           
03741         IF WS-ZIP-4 = SPACE  OR  '-'                             
03742            MOVE WS-ZIP-CAN-2-POST1   TO PM-CB-CAN-POST1
03743            MOVE WS-ZIP-CAN-2-POST2   TO PM-CB-CAN-POST2
03744         ELSE                                                     
03745            MOVE WS-ZIP-CAN-1-POST1   TO PM-CB-CAN-POST1
03746            MOVE WS-ZIP-CAN-1-POST2   TO PM-CB-CAN-POST2
              END-IF
03747      ELSE                                                         
03748         IF WS-ZIP-6 = SPACE  OR  '-'                             
03749            MOVE WS-ZIP-AM-2-CODE     TO PM-CB-ZIP-CODE
03750            MOVE WS-ZIP-AM-2-PLUS4    TO PM-CB-ZIP-PLUS4
03751         ELSE                                                     
03752            MOVE WS-ZIP-AM-1-CODE     TO PM-CB-ZIP-CODE
03753            MOVE WS-ZIP-AM-1-PLUS4    TO PM-CB-ZIP-PLUS4
              END-IF
           END-IF

           .
03755  4010-CONTINUE.                                                   
03756                                                                   
03757 *    IF BPHONE-LEN          GREATER ZERO                          
03758 *        MOVE WS-BPHONE          TO PM-PHONE-NO.                  
03762                                                                   
03763      MOVE 'C'                    TO JP-RECORD-TYPE.               
03764      MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.               
03765      MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
03766      MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.                   
03767                                                                   
03768      EXEC CICS REWRITE                                            
03769          DATASET (FILE-ID-ERPNDM)                                 
03770          FROM    (PENDING-MAILING-DATA)                           
03771      END-EXEC.                                                    
03772                                                                   
03773      PERFORM 8400-LOG-JOURNAL-RECORD.                             
03774                                                                   
03775      GO TO 4900-EXIT.                                             
03776                                                                   
03777      EJECT                                                        
03778                                                                   
03779  4100-ADD-ISSUE-RECORD.                                           
03780      EXEC CICS GETMAIN                                            
03781          SET     (ADDRESS OF PENDING-BUSINESS)                    
03782          LENGTH  (ERPNDB-RECORD-LENGTH)                           
03783          INITIMG (GETMAIN-SPACE)                                  
03784      END-EXEC.                                                    
03785                                                                   
03786      MOVE 'PB'                   TO PB-RECORD-ID.                 
03787      MOVE PI-COMPANY-CD          TO PB-COMPANY-CD                 
03788                                     PB-COMPANY-CD-A1.             
03789      MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.                
03790      MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH.               
03791      MOVE BSEQ                   TO PB-BATCH-SEQ-NO.              
03792                                                                   
03793      IF BSEQ   GREATER PI-LAST-SEQ-NO-ADDED                       
03794          MOVE BSEQ          TO PI-LAST-SEQ-NO-ADDED.              
03795                                                                   
03796      MOVE PI-SAV-CARRIER         TO PB-CARRIER.                   
03797      MOVE PI-SAV-GROUPING        TO PB-GROUPING.                  
03798      MOVE PI-SAV-STATE           TO PB-STATE.                     
03799      MOVE PI-SAV-ACCOUNT         TO PB-ACCOUNT.                   
03800      MOVE '1'                    TO PB-RECORD-TYPE.               
03801      MOVE BCERT                  TO PB-CERT-PRIME.                
03802      MOVE WS-CONVERTED-EFFDT     TO PB-CERT-EFF-DT.               
03803      MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO           
03804                                     PB-ALT-CHG-SEQ-NO.            
03805                                                                   
03806      MOVE +0                     TO PB-NO-OF-ERRORS.              
03807                                                                   
03808      MOVE LOW-VALUES             TO PB-COMMON-ERRORS.             
03809                                                                   
03810      MOVE ZEROS                  TO PB-I-LOAN-TERM                
03811                                     PB-I-LF-POLICY-FEE            
03812                                     PB-I-LF-PREM-CALC             
03813                                     PB-I-LF-ALT-PREM-CALC         
03814                                     PB-I-LF-RATE                  
03815                                     PB-I-LF-ALT-RATE              
03816                                     PB-I-LF-REI-RATE              
03817                                     PB-I-LF-ALT-REI-RATE          
03818                                     PB-I-RATE-DEV-PCT-LF          
03819                                     PB-I-CANCEL-FEE            
03820                                     PB-I-AH-PREM-CALC             
011904                                    PB-I-TOT-FEES
011904                                    PB-I-TOT-FEES-CALC
03821                                     PB-I-AH-RATE                  
03822                                     PB-I-AH-REI-RATE              
03823                                     PB-I-AH-RATE-TRM              
03824                                     PB-I-RATE-DEV-PCT-AH          
03825                                     PB-I-BUSINESS-TYPE            
03826                                     PB-I-LIFE-COMMISSION          
03827                                     PB-I-JOINT-COMMISSION         
03828                                     PB-I-AH-COMMISSION            
03829                                     PB-I-CURR-SEQ                 
03830                                     PB-CHG-COUNT                  
03831                                     PB-LF-BILLED-AMTS             
03832                                     PB-AH-BILLED-AMTS             
03833                                     PB-CALC-TOLERANCE             
03834                                     PB-I-EXTENTION-DAYS           
020210*                                   PB-I-MICROFILM-NO             
03836                                     PB-I-TERM-IN-DAYS             
03837                                     PB-I-STATE-TAX                
03838                                     PB-I-MUNI-TAX                 
03839                                     PB-I-NUM-BILLED.              
03840                                                                   
03841      MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT           
03842                                     PB-I-LF-EXPIRE-DT             
03843                                     PB-I-AH-EXPIRE-DT             
03844                                     PB-I-1ST-PMT-DT               
03845                                     PB-BILLED-DT                  
03846                                     PB-ACCT-EFF-DT                
03847                                     PB-ACCT-EXP-DT.               
03848                                                                   
03849                                                                   
03850      MOVE 'X'                    TO PB-FATAL-FLAG.                
03851                                                                   
03852      IF PI-MAIL-YES                                               
03853         MOVE '1'                 TO PB-I-MAIL-ADDRS-SW.           
03854                                                                   
03855      IF PI-NB-MONTH-END-DT NOT = SPACES                           
03856         MOVE PI-NB-MONTH-END-DT  TO PB-CREDIT-SELECT-DT           
03857        ELSE                                                       
03858         MOVE PI-CR-MONTH-END-DT     TO PB-CREDIT-SELECT-DT.       
03859                                                                   
03860      IF BSFX-LEN            GREATER ZEROS                         
03861          MOVE BSFX               TO PB-CERT-SFX.                  
03862                                                                   
03863      IF BLAST-NAME-LEN      GREATER ZEROS                         
03864          MOVE BLAST-NAME         TO PB-I-INSURED-LAST-NAME.       
03865                                                                   
03866      IF B1ST-NAME-LEN       GREATER ZEROS                         
03867          MOVE B1ST-NAME          TO PB-I-INSURED-FIRST-NAME.      
03868                                                                   
03869      IF BINIT-LEN           GREATER ZEROS                         
03870          MOVE BINIT              TO PB-I-INSURED-MIDDLE-INIT.     
03871                                                                   
03872      IF BAGE-LEN            GREATER ZEROS                         
03873          MOVE WS-BAGE            TO PB-I-AGE                      
03874      ELSE                                                         
03875          MOVE ZEROS              TO PB-I-AGE.                     
03876                                                                   
03877      IF BJNT-AGE-LEN        GREATER ZEROS                         
03878          MOVE WS-BJNT-AGE        TO PB-I-JOINT-AGE                
03879      ELSE                                                         
03880          MOVE ZEROS              TO PB-I-JOINT-AGE.               
03881                                                                   
03882      IF BBIRTH-LEN > ZEROS
03883         MOVE WS-CONVERTED-BIRTH (1) TO PB-I-BIRTHDAY
03884      ELSE                                                         
03885          MOVE LOW-VALUES          TO PB-I-BIRTHDAY.               

           IF BJNTDOB-LEN > ZEROS
              MOVE WS-CONVERTED-BIRTH (2)
                                       TO PB-I-JOINT-BIRTHDAY
           ELSE
              MOVE LOW-VALUES          TO PB-I-JOINT-BIRTHDAY
           END-IF

03890      IF BTERM1-LEN > ZEROS                         
03891         MOVE WS-BTERM1              TO PB-I-LF-TERM               
03892      ELSE                                                         
03893         MOVE ZEROS               TO PB-I-LF-TERM.                 
03894                                                                   
03895      IF BTERM2-LEN > ZEROS                         
03896         MOVE WS-BTERM2              TO PB-I-AH-TERM               
03897      ELSE                                                         
03898         MOVE ZEROS              TO PB-I-AH-TERM.                  
03899                                                                   
03900      IF BLN-TERM-LEN        GREATER ZEROS                         
03901          MOVE WS-BLN-TERM        TO PB-I-LOAN-TERM                
03902      ELSE                                                         
03903          MOVE ZEROS              TO PB-I-LOAN-TERM.               
03904                                                                   
03905 *    IF BFREQ-LEN           GREATER ZEROS                         
03906 *        MOVE WS-BFREQ           TO PB-I-PAY-FREQUENCY            
03907 *    ELSE                                                         
03908          MOVE ZEROS              TO PB-I-PAY-FREQUENCY.           
03909                                                                   
03910 *    IF BSKPCD-LEN          GREATER ZEROS                         
03911 *        MOVE BSKPCD             TO PB-I-SKIP-CODE.               
03912                                                                   
03913 *    IF BMODE-LEN           GREATER ZEROS                         
03914 *        MOVE BMODE              TO PB-I-TERM-TYPE.               
03915                                                                   
03916 *    IF BPMTS-LEN           GREATER ZEROS                         
03917 *        MOVE WS-BPMTS           TO PB-I-NO-OF-PAYMENTS           
03918 *    ELSE                                                         
03919          MOVE ZEROS              TO PB-I-NO-OF-PAYMENTS.          
03920                                                                   
03921 *    IF BPMT-LEN            GREATER ZEROS                         
03922 *        MOVE WS-BPMT            TO PB-I-PAYMENT-AMOUNT           
03923 *    ELSE                                                         
03924          MOVE ZEROS              TO PB-I-PAYMENT-AMOUNT.          
03925                                                                   
03926 *    IF BPOLICY-LEN         GREATER ZEROS                         
03927 *        MOVE BPOLICY            TO PB-I-POLICY-FORM-NO.          
03928                                                                   
03929      IF BTYPE1-LEN > ZEROS                         
03930         IF BTYPE1           NOT = ZEROS OR SPACES                 
03931            MOVE BTYPE1            TO PB-I-LF-INPUT-CD             
03932            MOVE WS-EDITED-LF-CODE TO PB-I-LIFE-BENEFIT-CD         
03933            MOVE WS-LF-ABBR-DESC   TO PB-I-LF-ABBR                 
03934         ELSE                                                      
03935            MOVE ZEROS             TO PB-I-LIFE-BENEFIT-CD         
03936      ELSE                                                         
03937            MOVE ZEROS             TO PB-I-LIFE-BENEFIT-CD.        
03938                                                                   
03939      IF  BBENE1-LEN > ZEROS                         
03940          MOVE WS-BBEN1           TO PB-I-LF-BENEFIT-AMT           
03941      ELSE                                                         
03942          MOVE ZEROS              TO PB-I-LF-BENEFIT-AMT.          
03943                                                                   
03944      IF  BALT-BEN1-LEN > ZEROS                      
03945          MOVE WS-BALT-BEN1       TO PB-I-LF-ALT-BENEFIT-AMT       
03946      ELSE                                                         
03947          MOVE ZEROS              TO PB-I-LF-ALT-BENEFIT-AMT.      
03948                                                                   
03949      IF  BPREM1-LEN > ZEROS                         
03950          IF WS-BPREM1      = WS-ALL-NINES OR                      
03951             WS-BPREM1      GREATER WS-ALL-NINES                   
03952             MOVE ZEROS           TO PB-I-LF-PREMIUM-AMT           
03953             MOVE '?'             TO PB-I-LF-CALC-FLAG             
03954          ELSE                                                     
03955             ADD  WS-BPREM1       TO PI-LF-ISS-ENTERED             
03956             MOVE WS-BPREM1       TO PB-I-LF-PREMIUM-AMT           
03957      ELSE                                                         
03958          MOVE ZEROS              TO PB-I-LF-PREMIUM-AMT.          
03959                                                                   
03960      IF  BALT-PREM1-LEN > ZEROS                      
03961          MOVE WS-BALT-PREM1      TO PB-I-LF-ALT-PREMIUM-AMT       
03962          ADD  WS-BALT-PREM1      TO PI-LF-ISS-ENTERED             
03963      ELSE                                                         
03964          MOVE ZEROS              TO PB-I-LF-ALT-PREMIUM-AMT.      
03965                                                                   
011904     IF  BALT-PREM2-LEN > ZEROS                      
011904         MOVE WS-BALT-PREM2      TO PB-I-TOT-FEES
011904     ELSE                                                         
011904         MOVE ZEROS              TO PB-I-TOT-FEES
           END-IF
011904                                                                  
03966      IF BTYPE2-LEN > ZEROS                         
03967         IF BTYPE2           NOT = ZEROS                           
03968            MOVE BTYPE2            TO PB-I-AH-INPUT-CD             
03969            MOVE WS-EDITED-AH-CODE TO PB-I-AH-BENEFIT-CD           
03970            MOVE WS-AH-ABBR-DESC   TO PB-I-AH-ABBR                 
03971         ELSE                                                      
03972            MOVE ZEROS             TO PB-I-AH-BENEFIT-CD           
03973      ELSE                                                         
03974            MOVE ZEROS             TO PB-I-AH-BENEFIT-CD.          
03975                                                                   
03976      IF  BBENE2-LEN > ZEROS                      
03977          MOVE WS-BBEN2           TO PB-I-AH-BENEFIT-AMT           
03978      ELSE                                                         
03979          MOVE ZEROS              TO PB-I-AH-BENEFIT-AMT.          
03980                                                                   
03981      IF  BPREM2-LEN > ZEROS                         
03982          IF WS-BPREM2      = WS-ALL-NINES OR                      
03983             WS-BPREM2      GREATER WS-ALL-NINES                   
03984             MOVE ZEROS            TO PB-I-AH-PREMIUM-AMT          
03985             MOVE '?'              TO PB-I-AH-CALC-FLAG            
03986          ELSE                                                     
03987             ADD  WS-BPREM2        TO PI-AH-ISS-ENTERED            
03988             MOVE WS-BPREM2        TO PB-I-AH-PREMIUM-AMT          
03989      ELSE                                                         
03990          MOVE ZEROS               TO PB-I-AH-PREMIUM-AMT.         
03991                                                                   
03992      IF PB-COMPANY-ID = 'NSL'                                     
03993      IF PB-I-AGE       GREATER 49   OR                            
03994         PB-I-JOINT-AGE GREATER 49                                 
03995          MOVE 'H'                TO PB-RECORD-BILL                
03996        ELSE                                                       
03997      IF PB-I-LF-BENEFIT-AMT GREATER +14999.99 OR                  
03998         PB-I-AH-BENEFIT-AMT GREATER +14999.99                     
03999           MOVE 'H'                TO PB-RECORD-BILL.              
04000                                                                   
04001 *    IF BCRIT-PERD-LEN      (1)   GREATER ZEROS                   
04002 *       MOVE WS-BCRIT-PERD  (1)   TO PB-I-LF-CRIT-PER             
04003 *    ELSE                                                         
04004         MOVE ZEROS                TO PB-I-LF-CRIT-PER.            
04005                                                                   
04006      IF BCRIT-PERD2-LEN > ZEROS                   
04007         MOVE WS-BCRIT-PERD2       TO PB-I-AH-CRIT-PER             
04008      ELSE                                                         
04009         MOVE ZEROS                TO PB-I-AH-CRIT-PER.            
04010                                                                   
04011 *    IF BIND-GRP-LEN        GREATER ZEROS                         
04012 *        MOVE BIND-GRP           TO PB-I-INDV-GRP-OVRD.           
04013                                                                   
04014 *    IF BRTCLS-LEN          GREATER ZEROS                         
04015 *        MOVE BRTCLS             TO PB-I-RATE-CLASS-OVRD.         
04016                                                                   
04017 *    IF BSIG-LEN            GREATER ZEROS                         
04018 *        MOVE BSIG               TO PB-I-SIG-SW.                  
04019                                                                   
04020      IF BAPR-LEN            GREATER ZEROS                         
04021          MOVE WS-BAPR            TO PB-I-LOAN-APR                 
04022      ELSE                                                         
111913        if (pi-company-id = 'CID')
111913           and (ws-lf-earnings-calc = '5')
111913           move +99.9999         to pb-i-loan-apr
111913        else
111913           move zeros            to pb-i-loan-apr
111913        end-if
111913     end-if
04024                                                                   
04028 *    IF BMEM-NO-LEN         GREATER ZEROS                         
04029 *        MOVE BMEM-NO        TO PB-I-MEMBER-NO.                   
04030                                                                   
04031 *    IF BMICROFILM-NO-LEN  GREATER  ZEROS                         
04032 *        MOVE WS-I-MICRO-NO      TO  PB-I-MICROFILM-NO.           
04033                                                                   
04034      IF BLN-OFFICER-LEN     GREATER ZEROS                         
04035          MOVE BLN-OFFICER        TO PB-I-LOAN-OFFICER.            
04036                                                                   
04037 *    IF BEXPIRE-LEN    (1)  GREATER ZEROS                         
04038 *       MOVE WS-CONVERTED-EXPIRDT (1) TO PB-I-LF-EXPIRE-DT        
04039 *    ELSE                                                         
04040         MOVE LOW-VALUES               TO PB-I-LF-EXPIRE-DT.       
04041                                                                   
04042 *    IF BEXPIRE-LEN    (2)  GREATER ZEROS                         
04043 *       MOVE WS-CONVERTED-EXPIRDT (2) TO PB-I-AH-EXPIRE-DT        
04044 *    ELSE                                                         
04045         MOVE LOW-VALUES               TO PB-I-AH-EXPIRE-DT.       
04046                                                                   
04047      IF B1ST-PMT-LEN        GREATER ZEROS                         
04048         MOVE WS-CONVERTED-1ST-PMT-DT TO  PB-I-1ST-PMT-DT.         
04049                                                                   
04050 *    IF BDAYS-LEN           GREATER ZEROS                         
04051 *       MOVE WS-BDAYS            TO PB-I-TERM-IN-DAYS             
04052 *                                   PB-I-EXTENTION-DAYS           
04053 *    ELSE                                                         
04054         MOVE ZEROS               TO PB-I-TERM-IN-DAYS             
04055                                     PB-I-EXTENTION-DAYS.          
04056                                                                   
04057 *    IF BDAYS-LEN GREATER ZEROS                                   
04058 *       MOVE '2'                 TO PB-I-DATA-ENTRY-SW.           
04059                                                                   
04060 *    IF BEXPIRE-LEN    (1)  GREATER ZEROS                         
04061 *       IF WS-CONVERTED-EXPIRDT   (1) GREATER LOW-VALUES          
04062 *          MOVE '3'              TO PB-I-DATA-ENTRY-SW.           
04063                                                                   
04064 *    IF BEXPIRE-LEN    (2)  GREATER ZEROS                         
04065 *       IF WS-CONVERTED-EXPIRDT   (2) GREATER LOW-VALUES          
04066 *          MOVE '3'              TO PB-I-DATA-ENTRY-SW.           
04067                                                                   
04068      IF B1ST-PMT-LEN GREATER ZEROS                                
04069         MOVE '4'                 TO PB-I-DATA-ENTRY-SW.           
04070                                                                   
04071 *    IF PB-EXT-DAYS-PROCESSING                                    
04072 *       IF PB-I-EXTENTION-DAYS = ZEROS                            
04073 *          MOVE '1'              TO PB-I-DATA-ENTRY-SW.           
04074                                                                   
04075 *    IF PB-EXPIRE-DT-PROCESSING                                   
04076 *       IF PB-I-LF-EXPIRE-DT = LOW-VALUES AND                     
04077 *          PB-I-AH-EXPIRE-DT = LOW-VALUES                         
04078 *            MOVE '1'            TO PB-I-DATA-ENTRY-SW.           
04079                                                                   
04080      IF PB-1ST-PMT-DT-PROCESSING                                  
04081         IF PB-I-1ST-PMT-DT = LOW-VALUES                           
04082            MOVE '1'              TO PB-I-DATA-ENTRY-SW.           
04083                                                                   
04084 *    IF BRINCD-LEN               GREATER ZEROS                    
04085 *       MOVE BRINCD              TO PB-I-SPECIAL-REIN-CODE.       
04086                                                                   
04087 *    IF BBILLCD-LEN              GREATER ZEROS                    
04088 *       MOVE BBILLCD             TO PB-RECORD-BILL                
04089 *    ELSE                                                         
04090 *        IF PB-COMPANY-ID = 'LAP'  OR  'RMC'                      
04091 *            MOVE 'H'            TO PB-RECORD-BILL.               
04092                                                                   
04093 *    IF BENTRY-LEN   NOT = ZEROS                                  
04094 *       IF BENTRY = 'U' OR 'D'                                    
04095 *          MOVE BENTRY        TO PB-I-UNDERWRITING-STATUS         
04096 *                                PB-BATCH-ENTRY                   
04097 *       ELSE                                                      
04098 *          MOVE BENTRY        TO PB-BATCH-ENTRY.                  
04099                                                                   
04100 *    IF BFORCE-LEN   NOT = ZEROS                                  
04101 *       MOVE BFORCE              TO PB-FORCE-CODE.                
04102                                                                   
081606     IF BVIN-LEN > ZEROS
081606        MOVE BVIN-NOI            TO PB-I-VIN
081606     ELSE
081606        MOVE SPACES              TO PB-I-VIN
081606     END-IF

04103 *    IF BLIVES-LEN          GREATER ZEROS                         
04104 *       MOVE WS-BLIVES           TO PB-I-LIVES                    
04105 *    ELSE                                                         
04106         MOVE ZEROS               TO PB-I-LIVES.                   
04107                                                                   
04108      IF BJNT-1ST-NAME-LEN   GREATER ZEROS                         
04109          MOVE BJNT-1ST-NAME      TO PB-I-JOINT-FIRST-NAME.        
04110                                                                   
04111      IF BJNT-INIT-LEN       GREATER ZEROS                         
04112          MOVE BJNT-INIT          TO PB-I-JOINT-MIDDLE-INIT.       
04113                                                                   
04114      IF BJNT-LST-NAME-LEN   GREATER ZEROS                         
04115          MOVE BJNT-LST-NAME      TO PB-I-JOINT-LAST-NAME.         
04116                                                                   
04117      IF BBENEFICIARY-LEN    GREATER ZEROS                         
04118          MOVE BBENEFICIARY       TO PB-I-BENEFICIARY-NAME.        
04119                                                                   
04120      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.          
04121      MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.            
04122                                                                   
04123  4175-WRITE-PB-RECORD.                                            
04124      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY              
04125                                     PB-INPUT-BY.                  
04126      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
04127      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT              
04128                                     PB-INPUT-DT.                  
04129                                                                   
04130      MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH.     
04131                                                                   
04132      MOVE PB-COMPANY-CD          TO PB-CSR-COMPANY-CD.            
04133      MOVE PI-CSR-ID              TO PB-CSR-ID.                    
04134      MOVE PB-ENTRY-BATCH         TO PB-CSR-ENTRY-BATCH.           
04135      MOVE PB-BATCH-SEQ-NO        TO PB-CSR-BATCH-SEQ-NO.          
04136      MOVE PB-BATCH-CHG-SEQ-NO    TO PB-CSR-BATCH-CHG-SEQ-NO.      
04137                                                                   
04138      MOVE 'A'                    TO JP-RECORD-TYPE.               
04139      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
04140      MOVE PB-CONTROL-PRIMARY     TO PI-SAV-ENDING-ERPNDB-KEY      
04141                                     ERPNDM-KEY.                   
04142                                                                   
011904*    MOVE PI-SAV-REFERENCE       TO PB-I-REFERENCE.               
04144      ADD +1                      TO PI-SAV-BATCH-SEQ.             
04145                                                                   
04146      EXEC CICS HANDLE CONDITION                                   
04147          DUPREC (4200-DUPLICATE-ALT-INDEX)                        
04148      END-EXEC.                                                    
04149                                                                   
04150      EXEC CICS WRITE                                              
04151          DATASET (FILE-ID-ERPNDB)                                 
04152          FROM    (PENDING-BUSINESS)                               
04153          RIDFLD  (PB-CONTROL-PRIMARY)                             
04154      END-EXEC.                                                    
04155                                                                   
04156      ADD +1                      TO PI-ISS-CNT-ENTERED.           
04157                                                                   
04158      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
04159      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
041320     PERFORM 8400-LOG-JOURNAL-RECORD  *>  Add Issue Image
04161                                                                   
04162      MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ.                         
04163      MOVE AL-SABON               TO BSEQ-ATTRB.                   
04164                                                                   
04165      ADD +1                      TO PI-NEXT-DISPLAY-SEQ-NO.       
04166                                                                   
04167      EJECT                                                        
04168                                                                   
04169 ******************************************************************
04170 *    CHECK THE FIRST ISSUE RECORD IN EVERY NEW BATCH.  VERIFY    *
04171 *    THAT THE CERTIFICATE DOES NOT EXIST ON THE CERT. MASTER     *
04172 *    FILE.  IF IT DOES DISPLAY WARNING MESSAGE ON BLANK SCREEN.  *
04173 ******************************************************************
04174                                                                   
04175      IF  PI-MAINT-FUNC = 'N' NEXT SENTENCE                        
04176         ELSE                                                      
04177          GO TO 4185-ADD-MAILING-RECORD.                           
04178                                                                   
04179      IF  PI-ISSUE-ADDED                                           
04180          GO TO 4185-ADD-MAILING-RECORD.                           
04181                                                                   
04182      MOVE 'Y'                    TO  PI-ISSUE-ADDED-SW.           
04183                                                                   
04184      EXEC CICS HANDLE CONDITION                                   
04185          NOTFND (4185-ADD-MAILING-RECORD)                         
04186      END-EXEC.                                                    
04187                                                                   
04188      MOVE PB-CONTROL-BY-ACCOUNT  TO  ELCERT-KEY.                  
04189      MOVE PI-SAV-FC-CARRIER      TO  ELCERT-CARRIER.              
04190      MOVE PI-SAV-FC-GROUPING     TO  ELCERT-GROUPING.             
04191      MOVE PI-SAV-FC-STATE        TO  ELCERT-STATE.                
04192                                                                   
04193      EXEC CICS READ                                               
04194          SET     (ADDRESS OF CERTIFICATE-MASTER)                  
04195          DATASET (FILE-ID-ELCERT)                                 
04196          RIDFLD  (ELCERT-KEY)                                     
04197          LENGTH  (ELCERT-RECORD-LENGTH)                           
04198          UPDATE                                                   
04199      END-EXEC.                                                    
04200                                                                   
04201      IF  CERT-WAS-CREATED-FOR-CLAIM                               
04202          GO TO 4185-ADD-MAILING-RECORD.

050713     go to 4900-exit
           .
04208  4185-ADD-MAILING-RECORD.                                         
04209                                                                   
04210      IF  PI-MAIL-YES                                              
04211                                                                   
04212          IF  BADDRS1-LEN > ZERO                             
04213                  OR                                               

                   BBENEFICIARY-LEN > ZERO
                        OR
04214              BADDRS2-LEN > ZERO                             
04215                  OR                                               
04216              BCITY-LEN > ZERO
                       OR
                   BSTATE-LEN > ZERO
                       OR
                   BCADDR1-LEN > ZERO
                       OR
                   BCADDR2-LEN > ZERO
                       OR
                   BCCITY-LEN > ZERO
                       OR
                   BCSTATE-LEN > ZERO
04217              NEXT SENTENCE                                        
04218                                                                   
04219          ELSE                                                     
04220              GO TO 4900-EXIT                                      
04221                                                                   
04222      ELSE                                                         
04223          GO TO 4900-EXIT.                                         
04224                                                                   
04225      EXEC CICS GETMAIN                                            
04226          SET     (ADDRESS OF PENDING-MAILING-DATA)                
04227          LENGTH  (ERPNDM-RECORD-LENGTH)                           
04228          INITIMG (GETMAIN-SPACE)                                  
04229      END-EXEC.                                                    
04230                                                                   
04231      MOVE 'PM'                   TO PM-RECORD-ID.                 
04232      MOVE 'ER'                   TO PM-SOURCE-SYSTEM.             
04233                                                                   
04234      MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY              
04235                                     PM-RECORD-ADDED-BY.           
04236      MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.         
04237      MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT              
04238                                     PM-RECORD-ADD-DT.             
04239                                                                   
04240      MOVE ERPNDM-KEY             TO PM-CONTROL-PRIMARY.           
04241                                                                   
04242      IF BLAST-NAME-LEN      GREATER ZEROS                         
04243          MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.         
04244                                                                   
04245      IF B1ST-NAME-LEN       GREATER ZEROS                         
04246          MOVE B1ST-NAME          TO PM-INSURED-FIRST-NAME.        
04247                                                                   
04248      IF BINIT-LEN           GREATER ZEROS                         
04249          MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.       
04250                                                                   
04251      IF BAGE-LEN            GREATER ZEROS                         
04252          MOVE WS-BAGE            TO PM-INSURED-ISSUE-AGE          
04253      ELSE                                                         
04254          MOVE ZEROS              TO PM-INSURED-ISSUE-AGE.         
04255                                                                   
04256      IF BBIRTH-LEN          GREATER ZEROS                         
04257         MOVE  WS-CONVERTED-BIRTH (1) TO PM-INSURED-BIRTH-DT           
04258      ELSE                                                         
04259         MOVE LOW-VALUES          TO PM-INSURED-BIRTH-DT.          
04260                                                                   
081108     IF BJNTDOB-LEN > ZEROS
081108        MOVE WS-CONVERTED-BIRTH (2)
                                       TO PM-JOINT-BIRTH-DT
           ELSE
              MOVE LOW-VALUES          TO PM-JOINT-BIRTH-DT
081108     END-IF

04261      IF BLAST-NAME-LEN      GREATER ZEROS                         
04262          MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.         
04263                                                                   
04264      IF BINIT-LEN           GREATER ZEROS                         
04265          MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.       
04266                                                                   
04267      IF BADDRS1-LEN         GREATER ZERO                          
04268          MOVE BADDRS1            TO PM-ADDRESS-LINE-1.            
04269                                                                   
04270      IF BADDRS2-LEN         GREATER ZERO                          
04271          MOVE BADDRS2            TO PM-ADDRESS-LINE-2.            
04272                                                                   
04273      IF BCITY-LEN > 0
04274         MOVE BCITY               TO PM-CITY
           END-IF

04273      IF BSTATE-LEN > 0
04274         MOVE BSTATE              TO PM-STATE
           END-IF
04275                                                                   
04276      IF BZIPCDE-LEN GREATER ZEROS                                 
04277          MOVE BZIPCDE            TO  WS-ZIP-CODE                  
04278      ELSE                                                         
04279          GO TO 4188-CRED-BENE.                                    
04280                                                                   
04281      IF WS-CANADIAN-ZIP                                           
04282          IF WS-ZIP-4 = SPACE  OR  '-'                             
04283              MOVE WS-ZIP-CAN-2-POST1   TO PM-CAN-POST1            
04284              MOVE WS-ZIP-CAN-2-POST2   TO PM-CAN-POST2            
04285          ELSE                                                     
04286              MOVE WS-ZIP-CAN-1-POST1   TO PM-CAN-POST1            
04287              MOVE WS-ZIP-CAN-1-POST2   TO PM-CAN-POST2            
04288      ELSE                                                         
04289          IF WS-ZIP-6 = SPACE  OR  '-'                             
04290              MOVE WS-ZIP-AM-2-CODE     TO PM-ZIP-CODE             
04291              MOVE WS-ZIP-AM-2-PLUS4    TO PM-ZIP-PLUS4            
04292          ELSE                                                     
04293              MOVE WS-ZIP-AM-1-CODE     TO PM-ZIP-CODE             
04294              MOVE WS-ZIP-AM-1-PLUS4    TO PM-ZIP-PLUS4.           
04295                                                                   
           .
       4188-CRED-BENE.


           IF BBENEFICIARY-LEN > ZEROS
              MOVE BBENEFICIARY        TO PM-CRED-BENE-NAME
           END-IF                      
           IF BCADDR1-LEN > ZEROS      
              MOVE BCADDR1             TO PM-CRED-BENE-ADDR
           END-IF                      
           IF BCADDR2-LEN > ZEROS      
              MOVE BCADDR2             TO PM-CRED-BENE-ADDR2
           END-IF                      
           IF BCCITY-LEN > ZEROS      
              MOVE BCCITY              TO PM-CRED-BENE-CITY
           END-IF
           IF BCSTATE-LEN > ZEROS      
              MOVE BCSTATE             TO PM-CRED-BENE-STATE
           END-IF

           IF BCZIPCD-LEN > ZEROS                                 
              MOVE BCZIPCD             TO WS-ZIP-CODE
           ELSE                                                         
              GO TO 4188-CONTINUE
           END-IF

03740      IF WS-CANADIAN-ZIP                                           
03741         IF WS-ZIP-4 = SPACE  OR  '-'                             
03742            MOVE WS-ZIP-CAN-2-POST1   TO PM-CB-CAN-POST1
03743            MOVE WS-ZIP-CAN-2-POST2   TO PM-CB-CAN-POST2
03744         ELSE                                                     
03745            MOVE WS-ZIP-CAN-1-POST1   TO PM-CB-CAN-POST1
03746            MOVE WS-ZIP-CAN-1-POST2   TO PM-CB-CAN-POST2
              END-IF
03747      ELSE                                                         
03748         IF WS-ZIP-6 = SPACE  OR  '-'                             
03749            MOVE WS-ZIP-AM-2-CODE     TO PM-CB-ZIP-CODE
03750            MOVE WS-ZIP-AM-2-PLUS4    TO PM-CB-ZIP-PLUS4
03751         ELSE                                                     
03752            MOVE WS-ZIP-AM-1-CODE     TO PM-CB-ZIP-CODE
03753            MOVE WS-ZIP-AM-1-PLUS4    TO PM-CB-ZIP-PLUS4
              END-IF
           END-IF

           .
04296  4188-CONTINUE.                                                   
04297                                                                   
04298 *    IF BPHONE-LEN          GREATER ZERO                          
04299 *        MOVE WS-BPHONE          TO PM-PHONE-NO                   
04300 *    ELSE                                                         
04301          MOVE ZEROS              TO PM-PHONE-NO.                  
04302                                                                   
04306      MOVE 'A'                    TO JP-RECORD-TYPE.               
04307      MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.               
04308      MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
04309      MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.                   
04310                                                                   
04311      EXEC CICS WRITE                                              
04312          DATASET (FILE-ID-ERPNDM)                                 
04313          FROM    (PENDING-MAILING-DATA)                           
04314          RIDFLD  (PM-CONTROL-PRIMARY)                             
04315      END-EXEC.                                                    
04316                                                                   
04317      PERFORM 8400-LOG-JOURNAL-RECORD.                             
04318                                                                   
04319      MOVE LOW-VALUES             TO MAP-B.                        
04320                                                                   
04321      GO TO 4900-EXIT.                                             
04322                                                                   
04323  4200-DUPLICATE-ALT-INDEX.                                        
04324      MOVE ER-2247                TO EMI-ERROR.                    
04325      MOVE -1                     TO BCERT-LEN.                    
04326      MOVE AL-UABON               TO BCERT-ATTRB.                  
04327      MOVE AL-UNBON               TO BEFFDT-ATTRB.                 
04328      MOVE 'Y'                    TO PI-ERROR-SW.                  
04329                                                                   
04330      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     
04331                                                                   
04332      IF BPREM1-LEN > ZEROS                               
04333          SUBTRACT WS-BPREM1                                       
04334                                  FROM PI-LF-ISS-ENTERED.          
04335                                                                   
04336      IF BALT-PREM1-LEN > ZEROS                           
04337          SUBTRACT WS-BALT-PREM1                                   
04338                                  FROM PI-LF-ISS-ENTERED.          
04339                                                                   
04340      IF BPREM2-LEN > ZEROS                               
04341          SUBTRACT WS-BPREM2                                       
04342                                  FROM PI-AH-ISS-ENTERED.          
04343                                                                   
04344      SUBTRACT +1                 FROM PI-LAST-SEQ-NO-ADDED        
04345                                       PI-SAV-BATCH-SEQ.           
04346                                                                   
04347  4900-EXIT.                                                       
04348      EXIT.                                                        
04349                                                                   
04350      EJECT                                                        
04351                                                                   
04352  5000-BUILD-CANCEL-RECORD.                                        
04353      MOVE +0                     TO WS-SUB2.                      
04354                                                                   
04355  5025-PROCESS-CANCEL.                                             
04356      ADD +1                      TO WS-SUB2.                      
04357                                                                   
04358      IF PI-LAST-FUNC-DISPLAY                                      
04359         IF WS-SUB2 GREATER +1                                     
04360            GO TO 5900-EXIT.                                       
04361                                                                   
04362      IF WS-SUB2 GREATER +4                                        
04363         GO TO 5900-EXIT.                                          
04364                                                                   
04365      IF CCERT-LEN    (WS-SUB2) = ZEROS AND                        
04366         CEFFDT-LEN   (WS-SUB2) = ZEROS AND                        
04367         CCANDT1-LEN  (WS-SUB2) = ZEROS AND                        
04368         CCANDT2-LEN  (WS-SUB2) = ZEROS AND                        
04369         CMTHD1-LEN   (WS-SUB2) = ZEROS AND                        
04370         CMTHD2-LEN   (WS-SUB2) = ZEROS AND                        
04371         CREFUND1-LEN (WS-SUB2) = ZEROS AND                        
04372         CREFUND2-LEN (WS-SUB2) = ZEROS AND                        
04373         CLIVES-LEN   (WS-SUB2) = ZEROS AND                        
04374         CCHK-LEN     (WS-SUB2) = ZEROS                            
04375            GO TO 5025-PROCESS-CANCEL.                             
04376                                                                   
04377      IF CSEQ (WS-SUB2) GREATER PI-LAST-SEQ-NO-ADDED               
04378          GO TO 5100-ADD-CANCEL-RECORD.                            
04379                                                                   
04380 ******************************************************************
04381 *   THE DATA ENTRY SYSTEM ALLOWS BROWSING OF THE CURRENT BUS.    *
04382 *   FILE. THE DATA ENTRY SYS. DOES NOT HAVE A MAINT. FUNCTION.   *
04383 *   THE PROGRAM ASSUMES THAT IF A MATCH ON THE READ FOR UPDATE   *
04384 *   IS SUCCESSFUL, THE RECORD HAS PREVIOUSLY BEEN DISPLAYED      *
04385 *   THROUGH A BROWSE.  CHANGES ARE APPLIED AND THE PB-RECORD IS  *
04386 *   REWRITTEN, ELSE A NEW PB-RECORD IS ADDED.                    *
04387 ******************************************************************
04388                                                                   
04389      MOVE PI-COMPANY-CD          TO ERPNDB-COMP-CD.               
04390      MOVE PI-SAV-ENTRY-BATCH     TO ERPNDB-ENTRY-BATCH.           
04391      MOVE CSEQ (WS-SUB2)         TO ERPNDB-BATCH-SEQ.             
04392                                                                   
04393      EXEC CICS HANDLE CONDITION                                   
04394          NOTFND (5100-ADD-CANCEL-RECORD)                          
04395      END-EXEC.                                                    
04396                                                                   
04397      EXEC CICS READ                                               
04398          SET     (ADDRESS OF PENDING-BUSINESS)                    
04399          DATASET (FILE-ID-ERPNDB)                                 
04400          RIDFLD  (ERPNDB-KEY)                                     
04401          UPDATE                                                   
04402      END-EXEC.                                                    
04403                                                                   
04404      MOVE 'B'                    TO JP-RECORD-TYPE                
04405      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
04406      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
04407      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
04408                                                                   
041320     PERFORM 8400-LOG-JOURNAL-RECORD  *> Before Cancel Image
04410                                                                   
04411      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             
04412      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
04413      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.             
04414                                                                   
04415      IF CSFX-LEN  (WS-SUB2) GREATER ZEROS                         
04416         MOVE CSFX (WS-SUB2)      TO PB-CERT-SFX.                  
04417                                                                   
04418      IF CLAST-NAME-LEN (WS-SUB2) GREATER ZEROS                    
04419          MOVE CLAST-NAME (WS-SUB2)  TO PB-C-LAST-NAME.            
04420                                                                   
04421      IF CREFUND1-LEN   (WS-SUB2) GREATER ZEROS                    
04422          IF WS-CREFUND1 (WS-SUB2) = WS-ALL-NINES OR               
04423             WS-CREFUND1 (WS-SUB2) GREATER WS-ALL-NINES            
04424             SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED    
04425             MOVE ZEROS            TO PB-C-LF-CANCEL-AMT           
04426             MOVE '?'              TO PB-C-LF-CALC-REQ             
04427          ELSE                                                     
04428             SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED    
04429             ADD WS-CREFUND1  (WS-SUB2) TO PI-LF-CAN-ENTERED       
04430             MOVE WS-CREFUND1 (WS-SUB2) TO PB-C-LF-CANCEL-AMT      
04431             MOVE SPACE                 TO PB-C-LF-CALC-REQ.       
04432                                                                   
04433      IF CREFUND2-LEN   (WS-SUB2) GREATER ZEROS                    
04434          IF WS-CREFUND2 (WS-SUB2) = WS-ALL-NINES OR               
04435             WS-CREFUND2 (WS-SUB2) GREATER WS-ALL-NINES            
04436             SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED    
04437             MOVE ZEROS            TO PB-C-AH-CANCEL-AMT           
04438             MOVE '?'              TO PB-C-AH-CALC-REQ             
04439          ELSE                                                     
04440             SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED    
04441             ADD WS-CREFUND2  (WS-SUB2) TO PI-AH-CAN-ENTERED       
04442             MOVE WS-CREFUND2 (WS-SUB2) TO PB-C-AH-CANCEL-AMT      
04443             MOVE SPACE                 TO PB-C-AH-CALC-REQ.       
04444                                                                   
04445 ******************************************************************
04446 *      IF CANCEL DATE = SPACES (LOW-VALUES) DELETE COVERAGE.     *
04447 ******************************************************************
04448                                                                   
04449      IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS                       
04450         MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO PB-C-LF-CANCEL-DT   
04451         IF   WS-CONVERTED-CANDT1 (WS-SUB2) = LOW-VALUES           
04452              SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED   
04453              MOVE ZEROS          TO PB-C-LF-REF-CALC              
04454                                     PB-C-LF-CANCEL-AMT.           
04455                                                                   
04456      IF CCANDT2-LEN (WS-SUB2) GREATER ZEROS                       
04457         MOVE WS-CONVERTED-CANDT2 (WS-SUB2) TO PB-C-AH-CANCEL-DT   
04458         IF   WS-CONVERTED-CANDT2 (WS-SUB2) = LOW-VALUES           
04459              SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED   
04460              MOVE ZEROS          TO PB-C-AH-REF-CALC              
04461                                     PB-C-AH-CANCEL-AMT.           
04462                                                                   
04463      IF CMTHD1-LEN (WS-SUB2) GREATER THAN +0                      
04464         MOVE CMTHD1 (WS-SUB2)    TO PB-C-LF-REFUND-OVERRIDE.      
04465                                                                   
04466      IF CMTHD2-LEN (WS-SUB2) GREATER THAN +0                      
04467         MOVE CMTHD2 (WS-SUB2)    TO PB-C-AH-REFUND-OVERRIDE.      
04468                                                                   
04469      IF CLIVES-LEN  (WS-SUB2) GREATER ZEROS                       
04470         MOVE WS-CLIVES (WS-SUB2) TO PB-C-LIVES.                   
04471                                                                   
           IF CCANREA-LEN (WS-SUB2) > ZEROS
              MOVE WS-CAN-REA (WS-SUB2) TO PB-C-CANCEL-REASON
           END-IF

04472 *    IF CMICRO-NO-LEN (WS-SUB2)  GREATER  ZEROS                   
04473 *        MOVE WS-MICRO-NO (WS-SUB2)                               
04474 *                                TO  PB-C-MICROFILM-NO.           
04475                                                                   
04476      IF CPAYEE-LEN  (WS-SUB2) GREATER ZEROS                       
04477         MOVE CPAYEE (WS-SUB2)    TO PB-C-PAYEE-CODE.              
04478                                                                   
04479      IF CCHK-LEN    (WS-SUB2) GREATER ZEROS                       
04480         MOVE CCHK   (WS-SUB2)    TO PB-C-REFUND-SW.               
04481                                                                   
04482      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.          
04483      MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.            
04484                                                                   
04485      MOVE 'C'                    TO JP-RECORD-TYPE.               
04486      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
04487      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
04488      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
04489                                                                   
04490      EXEC CICS REWRITE                                            
04491          DATASET (FILE-ID-ERPNDB)                                 
04492          FROM    (PENDING-BUSINESS)                               
04493      END-EXEC.                                                    
04494                                                                   
04495      MOVE ERPNDB-KEY             TO PI-SAV-ENDING-ERPNDB-KEY.     
04496                                                                   
041320     PERFORM 8400-LOG-JOURNAL-RECORD  *> After Cancel Image
04498                                                                   
04499      MOVE LOW-VALUES             TO DATA-AREA-C (WS-SUB2).        
04500                                                                   
04501      IF EIBAID = DFHENTER                                         
04502          MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ (WS-SUB2)            
04503          ADD +1 TO PI-NEXT-DISPLAY-SEQ-NO                         
04504          MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).     
04505                                                                   
04506      GO TO 5900-EXIT.                                             
04507                                                                   
04508      EJECT                                                        
04509                                                                   
04510  5100-ADD-CANCEL-RECORD.                                          
04511      EXEC CICS GETMAIN                                            
04512          SET     (ADDRESS OF PENDING-BUSINESS)                    
04513          LENGTH  (ERPNDB-RECORD-LENGTH)                           
04514          INITIMG (GETMAIN-SPACE)                                  
04515      END-EXEC.                                                    
04516                                                                   
04517      MOVE 'PB'                   TO PB-RECORD-ID.                 
04518      MOVE PI-COMPANY-CD          TO PB-COMPANY-CD                 
04519                                     PB-COMPANY-CD-A1.             
04520      MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.                
04521      MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH.               
04522      MOVE CSEQ (WS-SUB2)         TO PB-BATCH-SEQ-NO.              
04523                                                                   
04524      IF CSEQ (WS-SUB2) GREATER PI-LAST-SEQ-NO-ADDED               
04525         MOVE CSEQ (WS-SUB2)      TO PI-LAST-SEQ-NO-ADDED.         
04526                                                                   
04527      MOVE PI-SAV-CARRIER         TO PB-CARRIER.                   
04528      MOVE PI-SAV-GROUPING        TO PB-GROUPING.                  
04529      MOVE PI-SAV-STATE           TO PB-STATE.                     
04530      MOVE PI-SAV-ACCOUNT         TO PB-ACCOUNT.                   
04531      MOVE '2'                    TO PB-RECORD-TYPE.               
04532      MOVE CCERT (WS-SUB2)        TO PB-CERT-PRIME.                
04533      MOVE WS-CONVERTED-CAN-EFF-DT (WS-SUB2)                       
04534                                  TO PB-CERT-EFF-DT.               
04535                                                                   
011904*    MOVE PI-SAV-REFERENCE       TO PB-C-REFERENCE.               
04537                                                                   
04538      MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO           
04539                                     PB-ALT-CHG-SEQ-NO.            
04540                                                                   
04541      MOVE +0                     TO PB-NO-OF-ERRORS.              
04542                                                                   
04543      MOVE LOW-VALUES             TO PB-COMMON-ERRORS.             
04544                                                                   
04545      MOVE ZEROS                  TO PB-C-LF-REF-CALC              
04546                                     PB-C-AH-REF-CALC              
                                          PB-C-LF-RFND-CLP
                                          PB-C-AH-RFND-CLP
04547                                     PB-CI-INSURED-AGE             
04548                                     PB-CI-LF-TERM                 
04549                                     PB-CI-AH-TERM                 
04550                                     PB-CI-LF-BENEFIT-CD           
04551                                     PB-CI-LF-BENEFIT-AMT          
04552                                     PB-CI-LF-ALT-BENEFIT-AMT      
04553                                     PB-CI-LF-PREMIUM-AMT          
04554                                     PB-CI-LF-ALT-PREMIUM-AMT      
04555                                     PB-CI-AH-BENEFIT-CD           
04556                                     PB-CI-AH-BENEFIT-AMT          
04557                                     PB-CI-AH-PREMIUM-AMT          
04558                                     PB-CI-PAY-FREQUENCY           
04559                                     PB-CI-LOAN-APR                
04560                                     PB-CI-LOAN-TERM               
04561                                     PB-CI-LIFE-COMMISSION         
04562                                     PB-CI-AH-COMMISSION           
04563                                     PB-CI-CURR-SEQ                
04564                                     PB-CI-AH-CANCEL-AMT           
04565                                     PB-CI-LF-CANCEL-AMT           
04566                                     PB-CI-RATE-DEV-PCT-LF         
04567                                     PB-CI-RATE-DEV-PCT-AH         
04568                                     PB-CI-EXTENTION-DAYS          
04569                                     PB-CI-TERM-IN-DAYS            
04570                                     PB-CI-LIVES                   
04571                                     PB-CI-LF-CRIT-PER             
04572                                     PB-CI-AH-CRIT-PER             
04573                                     PB-C-LF-REM-TERM              
04574                                     PB-C-AH-REM-TERM              
04575                                     PB-CHG-COUNT                  
04576                                     PB-LF-BILLED-AMTS             
04577                                     PB-AH-BILLED-AMTS             
04578 *                                   PB-C-MICROFILM-NO             
062017                                    PB-C-INT-ON-REFS
04579                                     PB-CALC-TOLERANCE.            
04580                                                                   
04581      MOVE LOW-VALUES             TO PB-CI-AH-PAID-THRU-DT         
04582                                     PB-CI-AH-SETTLEMENT-DT        
04583                                     PB-CI-DEATH-DT                
04584                                     PB-CI-LF-PRIOR-CANCEL-DT      
04585                                     PB-CI-AH-PRIOR-CANCEL-DT      
04586                                     PB-CI-ENTRY-DT                
04587                                     PB-CI-LF-EXPIRE-DT            
04588                                     PB-CI-AH-EXPIRE-DT            
04589                                     PB-CI-LOAN-1ST-PMT-DT         
04590                                     PB-C-LF-CANCEL-DT             
04591                                     PB-C-AH-CANCEL-DT             
04592                                     PB-CREDIT-ACCEPT-DT           
04593                                     PB-BILLED-DT                  
04594                                     PB-ACCT-EFF-DT                
04595                                     PB-ACCT-EXP-DT.               
04596                                                                   
04597      IF PI-NB-MONTH-END-DT NOT = SPACES                           
04598         MOVE PI-NB-MONTH-END-DT  TO PB-CREDIT-SELECT-DT           
04599        ELSE                                                       
04600         MOVE PI-CR-MONTH-END-DT  TO PB-CREDIT-SELECT-DT.          
04601                                                                   
04602      MOVE 'X'                    TO PB-FATAL-FLAG.                
04603                                                                   
04604      IF CSFX-LEN  (WS-SUB2) GREATER ZEROS                         
04605         MOVE CSFX (WS-SUB2)      TO PB-CERT-SFX.                  
04606                                                                   
04607      IF CLAST-NAME-LEN (WS-SUB2) GREATER ZEROS                    
04608          MOVE CLAST-NAME (WS-SUB2)  TO PB-C-LAST-NAME.            
04609                                                                   
04610      IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS                       
04611         MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO PB-C-LF-CANCEL-DT.  
04612                                                                   
04613      IF CCANDT2-LEN (WS-SUB2) GREATER ZEROS                       
04614         MOVE WS-CONVERTED-CANDT2 (WS-SUB2) TO PB-C-AH-CANCEL-DT.  
04615                                                                   
04616      IF CMTHD1-LEN (WS-SUB2) GREATER ZEROS                        
04617         MOVE CMTHD1 (WS-SUB2) TO PB-C-LF-REFUND-OVERRIDE.         
04618                                                                   
04619      IF CMTHD2-LEN (WS-SUB2) GREATER ZEROS                        
04620         MOVE CMTHD2 (WS-SUB2) TO PB-C-AH-REFUND-OVERRIDE.         
04621                                                                   
04622      IF CREFUND1-LEN   (WS-SUB2) GREATER ZEROS                    
04623          IF WS-CREFUND1 (WS-SUB2) = WS-ALL-NINES OR               
04624             WS-CREFUND1 (WS-SUB2) GREATER WS-ALL-NINES            
04625             MOVE ZEROS            TO PB-C-LF-CANCEL-AMT           
04626             MOVE '?'              TO PB-C-LF-CALC-REQ             
04627          ELSE                                                     
04628             ADD  WS-CREFUND1  (WS-SUB2) TO PI-LF-CAN-ENTERED      
04629             MOVE WS-CREFUND1  (WS-SUB2) TO PB-C-LF-CANCEL-AMT     
04630      ELSE                                                         
04631          MOVE ZEROS            TO PB-C-LF-CANCEL-AMT.             
04632                                                                   
04633      IF CREFUND2-LEN   (WS-SUB2) GREATER ZEROS                    
04634          IF WS-CREFUND2 (WS-SUB2) = WS-ALL-NINES OR               
04635             WS-CREFUND2 (WS-SUB2) GREATER WS-ALL-NINES            
04636             MOVE ZEROS            TO PB-C-AH-CANCEL-AMT           
04637             MOVE '?'              TO PB-C-AH-CALC-REQ             
04638          ELSE                                                     
04639             ADD  WS-CREFUND2  (WS-SUB2) TO PI-AH-CAN-ENTERED      
04640             MOVE WS-CREFUND2  (WS-SUB2) TO PB-C-AH-CANCEL-AMT     
04641      ELSE                                                         
04642          MOVE ZEROS              TO PB-C-AH-CANCEL-AMT.           
04643                                                                   
04644      IF CLIVES-LEN  (WS-SUB2) GREATER ZEROS                       
04645         MOVE WS-CLIVES (WS-SUB2) TO PB-C-LIVES                    
04646      ELSE                                                         
04647         MOVE ZEROS               TO PB-C-LIVES.                   
04648                                                                   
           IF CCANREA-LEN (WS-SUB2) > ZEROS
              MOVE WS-CAN-REA (WS-SUB2) TO PB-C-CANCEL-REASON
           END-IF

04649 *    IF CMICRO-NO-LEN (WS-SUB2)  GREATER  ZEROS                   
04650 *        MOVE WS-MICRO-NO (WS-SUB2)                               
04651 *                                TO  PB-C-MICROFILM-NO            
04652 *    ELSE                                                         
04653 *        MOVE ZEROS              TO  PB-C-MICROFILM-NO.           
04654                                                                   
04655      IF CPAYEE-LEN  (WS-SUB2) GREATER ZEROS                       
04656         MOVE CPAYEE (WS-SUB2)    TO PB-C-PAYEE-CODE.              
04657                                                                   
04658      IF CCHK-LEN    (WS-SUB2) GREATER ZEROS                       
04659         MOVE CCHK   (WS-SUB2)    TO PB-C-REFUND-SW.               
04660                                                                   
04661      IF PB-COMPANY-ID = 'LAP'  OR  'RMC'                          
04662          MOVE 'H'                TO PB-RECORD-BILL.               
04663                                                                   
04664      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.          
04665      MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.            
04666                                                                   
04667      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY              
04668                                     PB-INPUT-BY.                  
04669      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
04670      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT              
04671                                     PB-INPUT-DT.                  
04672                                                                   
04673      MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH.     
04674                                                                   
04675      MOVE PB-COMPANY-CD          TO PB-CSR-COMPANY-CD.            
04676      MOVE PI-CSR-ID              TO PB-CSR-ID.                    
04677      MOVE PB-ENTRY-BATCH         TO PB-CSR-ENTRY-BATCH.           
04678      MOVE PB-BATCH-SEQ-NO        TO PB-CSR-BATCH-SEQ-NO.          
04679      MOVE PB-BATCH-CHG-SEQ-NO    TO PB-CSR-BATCH-CHG-SEQ-NO.      
04680                                                                   
04681      MOVE 'A'                    TO JP-RECORD-TYPE.               
04682      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
04683      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
04684      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
04685      MOVE PB-CONTROL-PRIMARY     TO PI-SAV-ENDING-ERPNDB-KEY.     
04686      ADD +1                      TO PI-SAV-BATCH-SEQ.             
04687                                                                   
04688      EXEC CICS HANDLE CONDITION                                   
04689          DUPREC (5200-DUPLICATE-ALT-INDEX)                        
04690      END-EXEC.                                                    
04691                                                                   
04692      EXEC CICS WRITE                                              
04693          DATASET (FILE-ID-ERPNDB)                                 
04694          FROM    (PENDING-BUSINESS)                               
04695          RIDFLD  (PB-CONTROL-PRIMARY)                             
04696      END-EXEC.                                                    
04697                                                                   
04698      ADD +1                      TO PI-CAN-CNT-ENTERED.           
04699                                                                   
041320     PERFORM 8400-LOG-JOURNAL-RECORD  *> Add BHDR Image
04701                                                                   
04702      MOVE LOW-VALUES             TO DATA-AREA-C (WS-SUB2).        
04703      MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ        (WS-SUB2).        
04704      MOVE AL-SABON               TO CSEQ-ATTRB  (WS-SUB2).        
04705                                                                   
04706      ADD +1 TO                   PI-NEXT-DISPLAY-SEQ-NO.          
04707                                                                   
04708      GO TO 5025-PROCESS-CANCEL.                                   
04709                                                                   
04710  5200-DUPLICATE-ALT-INDEX.                                        
04711      MOVE ER-2247                TO EMI-ERROR.                    
04712      MOVE -1                     TO CCERT-LEN    (WS-SUB2).       
04713      MOVE AL-UABON               TO CCERT-ATTRB  (WS-SUB2).       
04714      MOVE AL-UNBON               TO CEFFDT-ATTRB (WS-SUB2).       
04715      MOVE 'Y'                    TO PI-ERROR-SW.                  
04716                                                                   
04717      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
04718                                                                   
04719      IF CREFUND1-LEN (WS-SUB2) GREATER ZEROS                      
04720          SUBTRACT WS-CREFUND1 (WS-SUB2) FROM PI-LF-CAN-ENTERED.   
04721                                                                   
04722      IF CREFUND2-LEN (WS-SUB2) GREATER ZEROS                      
04723          SUBTRACT WS-CREFUND2 (WS-SUB2) FROM PI-AH-CAN-ENTERED.   
04724                                                                   
04725      GO TO 8200-SEND-DATAONLY.                                    
04726                                                                   
04727  5900-EXIT.                                                       
04728      EXIT.                                                        
04729                                                                   
04730      EJECT                                                        
04731                                                                   
04732  6000-DELETE-PEND-BUS-RECORD.                                     
04733      MOVE PI-COMPANY-CD          TO ERPNDB-COMP-CD.               
04734      MOVE PI-SAV-ENTRY-BATCH     TO ERPNDB-ENTRY-BATCH.           
04735                                                                   
04736      IF PI-MAP-NAME = EL630B                                      
04737          MOVE BSEQ               TO ERPNDB-BATCH-SEQ              
04738      ELSE                                                         
04739          MOVE CSEQ (1)           TO ERPNDB-BATCH-SEQ.             
04740                                                                   
04741      EXEC CICS HANDLE CONDITION                                   
04742          NOTFND (6990-REC-NOTFND)                                 
04743      END-EXEC.                                                    
04744                                                                   
04745      EXEC CICS READ                                               
04746          SET     (ADDRESS OF PENDING-BUSINESS)                    
04747          DATASET (FILE-ID-ERPNDB)                                 
04748          RIDFLD  (ERPNDB-KEY)                                     
04749          UPDATE                                                   
04750      END-EXEC.                                                    
04751                                                                   
04752 ******************************************************************
04753 *    PENDING BUSINESS RECORD CAN NOT BE DELETED THROUGH DATA     *
04754 *    ENTRY IF THE RECORD HAS BEEN EDITED.  IF THE RECORD HAS     *
04755 *    BEEN EDITED, THE CURRENT BUSINESS RECORD CAN ONLY BE DELETED*
04756 *    THROUGH REVIEW AND CORRECTION.                              *
04757 ******************************************************************
04758                                                                   
04759      IF  PB-ACCT-EFF-DT = LOW-VALUES NEXT SENTENCE                
04760         ELSE                                                      
04761          GO TO 6880-DELETE-ERROR.                                 
04762                                                                   
04763      IF PB-ISSUE                                                  
04764          SUBTRACT PB-I-LF-PREMIUM-AMT     FROM PI-LF-ISS-ENTERED  
04765          SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM PI-LF-ISS-ENTERED  
04766          SUBTRACT PB-I-AH-PREMIUM-AMT     FROM PI-AH-ISS-ENTERED  
04767          SUBTRACT +1 FROM PI-ISS-CNT-ENTERED                      
04768      ELSE                                                         
04769          SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED       
04770          SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED       
04771          SUBTRACT +1 FROM PI-CAN-CNT-ENTERED.                     
04772                                                                   
04773  6300-DELETE-PB-RECORD.                                           
04774      MOVE 'D'                    TO JP-RECORD-TYPE.               
04775      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
04776      MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
04777      MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
04778                                                                   
04779      EXEC CICS DELETE                                             
04780          DATASET (FILE-ID-ERPNDB)                                 
04781      END-EXEC.                                                    
04782                                                                   
041320     PERFORM 8400-LOG-JOURNAL-RECORD  *> Journal Delete
04784                                                                   
04785      MOVE 'Y'                    TO PI-UPDATE-SW.                 
04786      MOVE ER-0000                TO EMI-ERROR.                    
04787                                                                   
04788      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
04789                                                                   
04790      ADD +1             PI-LAST-SEQ-NO-ADDED                      
04791                      GIVING PI-NEXT-DISPLAY-SEQ-NO.               
04792                                                                   
04793      IF PI-MAP-NAME = EL630B                                      
04794          MOVE LOW-VALUES         TO MAP-B                         
04795          PERFORM 8550-SET-MAP-SEQ-NOS                             
04796      ELSE                                                         
04797          MOVE SPACE              TO PI-DISPLAY-SW                 
04798          MOVE LOW-VALUES         TO MAP-C                         
04799          PERFORM 8550-SET-MAP-SEQ-NOS                             
04800                  VARYING WS-SUB2 FROM +1 BY +1                    
04801                  UNTIL WS-SUB2 GREATER +5.                        
04802                                                                   
04803      GO TO 8100-SEND-INITIAL-MAP.                                 
04804                                                                   
070622 6410-READ-ERACCT.
070622
070622     MOVE PB-CONTROL-BY-ACCOUNT  TO ERACCT-KEY
070622     MOVE LOW-VALUES             TO ERACCT-FILL
070622
070622     EXEC CICS READ
070622          DATASET   ('ERACCT')
070622          INTO      (ACCOUNT-MASTER)
070622          RIDFLD    (ERACCT-KEY)
070622          GTEQ
070622          RESP      (WS-RESPONSE)
070622     END-EXEC
070622
070622     .
070622 6410-EXIT.
070622     EXIT.

04805  6880-DELETE-ERROR.
04806      EXEC CICS UNLOCK                                             
04807           DATASET (FILE-ID-ERPNDB)                                
04808      END-EXEC.                                                    
04809                                                                   
04810      MOVE ER-2901        TO EMI-ERROR.                            
04811      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
04812      IF PI-MAP-NAME = EL630B                                      
04813          MOVE -1                 TO BPFENTRL                      
04814      ELSE                                                         
04815          MOVE -1                 TO CPFENTRL.                     
04816                                                                   
04817      GO TO 8200-SEND-DATAONLY.                                    
04818                                                                   
04819  6990-REC-NOTFND.                                                 
04820      MOVE ER-2433                TO EMI-ERROR                     
04821                                                                   
04822      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
04823                                                                   
04824      IF PI-MAP-NAME = EL630B                                      
04825          MOVE -1                 TO BPFENTRL                      
04826      ELSE                                                         
04827          MOVE -1                 TO CPFENTRL.                     
04828                                                                   
04829      GO TO 8200-SEND-DATAONLY.                                    
04830                                                                   
04831      EJECT                                                        
04832                                                                   
04833  7000-FORMAT-ISSUE-SCREEN.                                        
04834      MOVE 'Y'                        TO PI-DISPLAY-SW.            
04835      MOVE LOW-VALUES                 TO DATA-AREA-B.              
04836      MOVE -1                         TO BPFENTRL.                 
04837      MOVE PB-BATCH-SEQ-NO            TO BSEQ.                     
04838      MOVE AL-SABON                   TO BSEQ-ATTRB.               
04839      MOVE PB-CERT-PRIME              TO BCERT.                    
04840      MOVE AL-SANON                   TO BCERT-ATTRB.              
04841      MOVE PB-CERT-SFX                TO BSFX.                     
04842      MOVE AL-SANOF                   TO BSFX-ATTRB.               
04843      MOVE PB-CERT-EFF-DT             TO DC-BIN-DATE-1.            
04844      MOVE SPACE                      TO DC-OPTION-CODE.           
04845      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
04846      MOVE DC-GREG-DATE-1-MDY         TO BEFFDT.                   
04847      MOVE AL-SANON                   TO BEFFDT-ATTRB.             
04848                                                                   
04849      MOVE PI-LIFE-OVERRIDE-L2        TO BKIND1
04850      MOVE PI-AH-OVERRIDE-L2          TO BKIND2
04851                                                                   
04852      IF PB-I-INSURED-LAST-NAME GREATER SPACES                     
04853         MOVE PB-I-INSURED-LAST-NAME  TO BLAST-NAME.               
04854                                                                   
04855      IF PB-I-INSURED-FIRST-NAME GREATER SPACES                    
04856         MOVE PB-I-INSURED-FIRST-NAME TO B1ST-NAME.                
04857                                                                   
04858      IF PB-I-INSURED-MIDDLE-INIT GREATER SPACES                   
04859         MOVE PB-I-INSURED-MIDDLE-INIT TO BINIT.                   
04860                                                                   
04861      IF PB-I-AGE GREATER ZEROS                                    
04862         MOVE PB-I-AGE                TO BAGE.                     
04863                                                                   
04864      IF PB-I-JOINT-AGE GREATER ZEROS                              
04865         MOVE PB-I-JOINT-AGE          TO BJNT-AGE.                 
04866                                                                   
04867      IF PB-I-BIRTHDAY NOT = LOW-VALUES                            
04868         MOVE PB-I-BIRTHDAY           TO DC-BIN-DATE-1             
04869         MOVE SPACE                   TO DC-OPTION-CODE            
04870         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  
04871         MOVE DC-GREG-DATE-1-MDY      TO BBIRTH-DT.                

           IF PB-I-JOINT-BIRTHDAY NOT = LOW-VALUES                            
              MOVE PB-I-JOINT-BIRTHDAY TO DC-BIN-DATE-1             
              MOVE SPACE               TO DC-OPTION-CODE            
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-MDY  TO BJNTDOB-DT
           END-IF

04876      IF PB-I-LF-TERM GREATER ZEROS                                
04877         MOVE PB-I-LF-TERM            TO BTERM1O.               
04878                                                                   
04879      IF PB-I-AH-TERM GREATER ZEROS                                
04880         MOVE PB-I-AH-TERM            TO BTERM2O.               
04881                                                                   
04882      IF PB-I-LOAN-TERM GREATER ZEROS                              
04883         MOVE PB-I-LOAN-TERM          TO BLN-TERMO.                
04884                                                                   
04885 *    IF PB-I-PAY-FREQUENCY GREATER ZEROS                          
04886 *       MOVE PB-I-PAY-FREQUENCY      TO BFREQO.                   
04887                                                                   
04888 *    IF PB-I-SKIP-CODE GREATER SPACES                             
04889 *       MOVE PB-I-SKIP-CODE          TO BSKPCD.                   
04890                                                                   
04891 *    IF PB-I-TERM-TYPE GREATER SPACES                             
04892 *       MOVE PB-I-TERM-TYPE          TO BMODE.                    
04893                                                                   
04894 *    IF PB-I-NO-OF-PAYMENTS GREATER ZEROS                         
04895 *       MOVE PB-I-NO-OF-PAYMENTS     TO BPMTS-OUT.                
04896                                                                   
04897 *    IF PB-I-PAYMENT-AMOUNT GREATER ZEROS                         
04898 *       MOVE PB-I-PAYMENT-AMOUNT     TO BPMTO.                    
04899                                                                   
04900 *    IF PB-I-POLICY-FORM-NO GREATER SPACES                        
04901 *       MOVE PB-I-POLICY-FORM-NO     TO BPOLICY.                  
04902                                                                   
04903      IF PB-I-LF-INPUT-CD GREATER SPACES                           
04904         MOVE PB-I-LF-INPUT-CD        TO BTYPE1.                
04905                                                                   
04906      IF PB-I-LF-BENEFIT-AMT GREATER ZEROS                         
04907         MOVE PB-I-LF-BENEFIT-AMT     TO BBENE1O.                
04908                                                                   
04909      IF PB-I-LF-ALT-BENEFIT-AMT GREATER ZEROS                     
04910         MOVE PB-I-LF-ALT-BENEFIT-AMT TO BALT-BEN1O.            
04911                                                                   
04912      IF PB-I-LF-PREMIUM-AMT GREATER ZEROS                         
04913         MOVE PB-I-LF-PREMIUM-AMT     TO BPREM1O.               
04914                                                                   
04915      IF PB-I-LF-ALT-PREMIUM-AMT GREATER ZEROS                     
04916         MOVE PB-I-LF-ALT-PREMIUM-AMT TO BALT-PREM1O.           
04917                                                                   
04918      IF PB-I-AH-INPUT-CD GREATER SPACES                           
04919         MOVE PB-I-AH-INPUT-CD        TO BTYPE2.                
04920                                                                   
04921      IF PB-I-AH-BENEFIT-AMT GREATER ZEROS                         
04922         MOVE PB-I-AH-BENEFIT-AMT     TO BBENE2O.                
04923                                                                   
04924      IF PB-I-AH-PREMIUM-AMT GREATER ZEROS                         
04925         MOVE PB-I-AH-PREMIUM-AMT     TO BPREM2O.               
04926                                                                   
04927 *    IF PB-I-LF-CRIT-PER GREATER ZEROS                            
04928 *       MOVE PB-I-LF-CRIT-PER        TO BCRIT-PERDO (1).          
04929                                                                   
04930      IF PB-I-AH-CRIT-PER GREATER ZEROS                            
04931         MOVE PB-I-AH-CRIT-PER           TO BCRIT-PERD2O.       
04932                                                                   
04933 *    IF PB-BATCH-ENTRY GREATER SPACES                             
04934 *       MOVE PB-BATCH-ENTRY          TO BENTRY.                   
04935                                                                   
04936 *    IF PB-FORCE-CODE  GREATER SPACES                             
04937 *       MOVE PB-FORCE-CODE           TO BFORCE.                   
04938                                                                   
04939 *    IF PB-I-SPECIAL-REIN-CODE GREATER SPACE                      
04940 *      MOVE PB-I-SPECIAL-REIN-CODE   TO BRINCD.                   
04941                                                                   
04942 *    IF PB-RECORD-BILL         GREATER SPACE                      
04943 *      MOVE PB-RECORD-BILL           TO BBILLCD.                  
04944                                                                   
04945 *    IF PB-I-INDV-GRP-OVRD GREATER SPACES                         
04946 *       MOVE PB-I-INDV-GRP-OVRD      TO BIND-GRP.                 
04947                                                                   
04948 *    IF PB-I-RATE-CLASS-OVRD GREATER SPACES                       
04949 *       MOVE PB-I-RATE-CLASS-OVRD    TO BRTCLS.                   
04950                                                                   
04951 *    IF PB-I-SIG-SW GREATER SPACES                                
04952 *        MOVE PB-I-SIG-SW            TO BSIG.                     
04953                                                                   
04954      IF PB-I-LOAN-APR GREATER ZEROS                               
04955         MOVE PB-I-LOAN-APR           TO BAPR-OUT.                 
04956                                                                   
04960 *    IF PB-I-MEMBER-NO GREATER SPACES                             
04961 *       MOVE PB-I-MEMBER-NO          TO BMEM-NO.                  
04962                                                                   
04963 *    IF PI-COMPANY-ID EQUAL 'HER'                                 
04964 *        MOVE PB-I-MEMBER-NO     TO  WS-MEMBER-NO                 
04965 *        IF WS-MEMBER-NO-1-8  IS NUMERIC                          
04966 *            IF WS-MEMBER-NO-1-8  GREATER  ZEROS                  
04967 *                MOVE WS-MEMBER-NO-1-8                            
04968 *                                TO  BMICROFILM-NOO.              
04969                                                                   
04970 *    IF PB-I-MICROFILM-NO  IS NUMERIC                             
04971 *        IF PB-I-MICROFILM-NO  GREATER  ZEROS                     
04972 *            MOVE PB-I-MICROFILM-NO                               
04973 *                                TO  BMICROFILM-NOO.              
04974                                                                   
04975      IF PB-I-LOAN-OFFICER GREATER SPACES                          
04976         MOVE PB-I-LOAN-OFFICER       TO BLN-OFFICER.              
04977                                                                   
04978 *    IF PB-I-LF-EXPIRE-DT NOT = LOW-VALUES                        
04979 *       MOVE PB-I-LF-EXPIRE-DT   TO DC-BIN-DATE-1                 
04980 *       MOVE SPACE               TO DC-OPTION-CODE                
04981 *       PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  
04982 *       MOVE DC-GREG-DATE-1-MDY  TO BEXPIRE (1).                  
04983                                                                   
04984 *    IF PB-I-AH-EXPIRE-DT NOT = LOW-VALUES                        
04985 *       MOVE PB-I-AH-EXPIRE-DT   TO DC-BIN-DATE-1                 
04986 *       MOVE SPACE               TO DC-OPTION-CODE                
04987 *       PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  
04988 *       MOVE DC-GREG-DATE-1-MDY  TO BEXPIRE (2).                  
04989                                                                   
04990      IF PB-I-1ST-PMT-DT GREATER LOW-VALUES                        
04991         MOVE PB-I-1ST-PMT-DT     TO DC-BIN-DATE-1                 
04992         MOVE SPACE               TO DC-OPTION-CODE                
04993         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  
04994         MOVE DC-GREG-DATE-1-MDY  TO B1ST-PMT.                     
04995                                                                   
04996 *    IF PB-I-EXTENTION-DAYS NUMERIC                               
04997 *       IF PB-I-EXTENTION-DAYS NOT = ZEROS                        
04998 *          MOVE PB-I-EXTENTION-DAYS  TO BDAYSO                    
04999 *       ELSE                                                      
05000 *          IF PB-I-TERM-IN-DAYS NUMERIC                           
05001 *             IF PB-I-TERM-IN-DAYS NOT = ZEROS                    
05002 *                MOVE PB-I-TERM-IN-DAYS TO BDAYSO.                
05003                                                                   
081606     IF PB-I-VIN > ZEROS
081606        MOVE PB-I-VIN            TO BVIN-NOI
081606     END-IF

05004 *    IF PB-I-LIVES GREATER ZEROS                                  
05005 *       MOVE PB-I-LIVES              TO BLIVESO.                  
05006                                                                   
05007      IF PB-I-JOINT-FIRST-NAME GREATER SPACES                      
05008         MOVE PB-I-JOINT-FIRST-NAME   TO BJNT-1ST-NAME.            
05009                                                                   
05010      IF PB-I-JOINT-MIDDLE-INIT GREATER SPACES                     
05011         MOVE PB-I-JOINT-MIDDLE-INIT  TO BJNT-INIT.                
05012                                                                   
05013      IF PB-I-JOINT-LAST-NAME GREATER SPACES                       
05014         MOVE PB-I-JOINT-LAST-NAME    TO BJNT-LST-NAME.            
05015                                                                   
05016      IF PB-I-BENEFICIARY-NAME GREATER SPACES                      
05017         MOVE PB-I-BENEFICIARY-NAME   TO BBENEFICIARY.             
05018                                                                   
05019      MOVE PB-CONTROL-PRIMARY         TO ERPNDM-KEY.               
05020                                                                   
05021      EXEC CICS HANDLE CONDITION                                   
05022          NOTFND (7090-EXIT)                                       
05023      END-EXEC.                                                    
05024                                                                   
05025      EXEC CICS READ                                               
05026          SET     (ADDRESS OF PENDING-MAILING-DATA)                
05027          DATASET (FILE-ID-ERPNDM)                                 
05028          RIDFLD  (ERPNDM-KEY)                                     
05029          UPDATE                                                   
05030      END-EXEC.                                                    
05031                                                                   
05032      IF PM-ADDRESS-LINE-1 GREATER SPACES                          
05033         MOVE PM-ADDRESS-LINE-1       TO BADDRS1.                  
05034                                                                   
05035      IF PM-ADDRESS-LINE-2 GREATER SPACES                          
05036         MOVE PM-ADDRESS-LINE-2       TO BADDRS2.                  
05037                                                                   
05038      IF PM-CITY > SPACES                              
05039         MOVE PM-CITY                 TO BCITY
           END-IF

05038      IF PM-STATE > SPACES                              
05039         MOVE PM-STATE                TO BSTATE
           END-IF
05040                                                                   
05041      IF PM-ZIP            GREATER SPACES                          
05042          MOVE SPACES               TO WS-ZIP-CODE                 
05043          IF PM-CANADIAN-POST-CODE                                 
05044              MOVE PM-CAN-POST1     TO WS-ZIP-CAN-2-POST1          
05045              MOVE PM-CAN-POST2     TO WS-ZIP-CAN-2-POST2          
05046              MOVE WS-ZIP-CODE      TO BZIPCDE                     
05047          ELSE                                                     
05048              MOVE PM-ZIP-CODE      TO WS-ZIP-AM-2-CODE            
05049              MOVE WS-ZIP-CODE      TO BZIPCDE                     
05050              IF PM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS             
05051                  MOVE '-'          TO WS-ZIP-AM-2-DASH            
05052                  MOVE PM-ZIP-PLUS4 TO WS-ZIP-AM-2-PLUS4           
05053                  MOVE WS-ZIP-CODE  TO BZIPCDE.                    
05054                                                                   
05055 *    IF PM-PHONE-NO NUMERIC                                       
05056 *       IF PM-PHONE-NO GREATER ZEROS                              
05057 *          MOVE PM-PHONE-NO          TO  BPHONE-NO                
05058 *          INSPECT BPHONE-NO CONVERTING ' ' TO '-'.               


05032      IF PM-CRED-BENE-ADDR > SPACES                          
05033         MOVE PM-CRED-BENE-ADDR       TO BCADDR1
           END-IF
05034                                                                   
05035      IF PM-CRED-BENE-ADDR2 > SPACES                          
05036         MOVE PM-CRED-BENE-ADDR2     TO BCADDR2
           END-IF
05037                                                                   
05038      IF PM-CRED-BENE-CITY > SPACES
05039         MOVE PM-CRED-BENE-CITY       TO BCCITY
           END-IF
05038      IF PM-CRED-BENE-STATE > SPACES
05039         MOVE PM-CRED-BENE-STATE      TO BCSTATE
           END-IF
05040                                                                   
05041      IF PM-CRED-BENE-ZIP > SPACES                          
05042         MOVE SPACES               TO WS-ZIP-CODE                 
05043         IF PM-CB-CANADIAN-POST-CODE                                 
05044            MOVE PM-CB-CAN-POST1     TO WS-ZIP-CAN-2-POST1          
05045            MOVE PM-CB-CAN-POST2     TO WS-ZIP-CAN-2-POST2          
05046            MOVE WS-ZIP-CODE      TO BZIPCDE                     
05047         ELSE                                                     
05048            MOVE PM-CB-ZIP-CODE      TO WS-ZIP-AM-2-CODE            
05049            MOVE WS-ZIP-CODE      TO BCZIPCD                     
05050            IF PM-CB-ZIP-PLUS4 NOT = SPACES  AND  ZEROS             
05051               MOVE '-'          TO WS-ZIP-AM-2-DASH            
05052               MOVE PM-CB-ZIP-PLUS4 TO WS-ZIP-AM-2-PLUS4           
05053               MOVE WS-ZIP-CODE  TO BCZIPCD
                 END-IF
              END-IF
           END-IF

           .                                                            
05060  7090-EXIT.                                                       
05061      EXIT.                                                        
05062                                                                   
05063      EJECT                                                        
05064                                                                   
05065  7100-FORMAT-CANCEL-SCREEN.                                       
05066      MOVE 'Y'                    TO PI-DISPLAY-SW.                
05067                                                                   
05068      MOVE LOW-VALUES             TO DATA-AREA-C (2)               
05069                                     DATA-AREA-C (3).              
05070                                                                   
05071      MOVE -1                     TO CPFENTRL.                     
05072                                                                   
05073      MOVE PB-BATCH-SEQ-NO        TO CSEQ        (1).              
05074      MOVE AL-SABON               TO CSEQ-ATTRB  (1).              
05075      MOVE PB-CERT-PRIME          TO CCERT       (1).              
05076      MOVE AL-SANON               TO CCERT-ATTRB (1).              
05077      MOVE PB-CERT-SFX            TO CSFX        (1).              
05078      MOVE AL-SANON               TO CSFX-ATTRB  (1).              
05079                                                                   
05080      MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1.                
05081      MOVE SPACE                  TO DC-OPTION-CODE.               
05082      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
05083      MOVE DC-GREG-DATE-1-MDY     TO CEFFDT       (1).             
05084      MOVE AL-SANON               TO CEFFDT-ATTRB (1).             
05085      MOVE PB-C-LAST-NAME         TO CLAST-NAME   (1).             
05086                                                                   
05087      IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES                        
05088         MOVE PB-C-LF-CANCEL-DT   TO DC-BIN-DATE-1                 
05089         MOVE SPACE               TO DC-OPTION-CODE                
05090         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  
05091         MOVE DC-GREG-DATE-1-MDY  TO CCANDT1 (1)                   
05092         MOVE AL-UANON            TO CCANDT1-ATTRB (1).            
05093                                                                   
05094      IF PB-C-AH-CANCEL-DT NOT = LOW-VALUES                        
05095         MOVE PB-C-AH-CANCEL-DT   TO DC-BIN-DATE-1                 
05096         MOVE SPACE               TO DC-OPTION-CODE                
05097         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  
05098         MOVE DC-GREG-DATE-1-MDY  TO CCANDT2 (1)                   
05099         MOVE AL-UANON            TO CCANDT2-ATTRB (1).            
05100                                                                   
05101      IF PB-C-LF-REFUND-OVERRIDE EQUAL SPACES                      
05102         NEXT SENTENCE                                             
05103      ELSE                                                         
05104         MOVE PB-C-LF-REFUND-OVERRIDE                              
05105                                  TO CMTHD1 (1)                    
05106         MOVE AL-UANON            TO CMTHD1-ATTRB (1).             
05107                                                                   
05108      IF PB-C-AH-REFUND-OVERRIDE EQUAL SPACES                      
05109         NEXT SENTENCE                                             
05110      ELSE                                                         
05111         MOVE PB-C-AH-REFUND-OVERRIDE                              
05112                                  TO CMTHD2 (1)                    
05113         MOVE AL-UANON            TO CMTHD2-ATTRB (1).             
05114                                                                   
05115      IF PB-C-LF-CANCEL-AMT NOT =  ZEROS                           
05116         MOVE PB-C-LF-CANCEL-AMT  TO CREFUND1O (1)                 
05117         MOVE AL-UNNON            TO CREFUND1-ATTRB (1).           
05118                                                                   
05119      IF PB-C-AH-CANCEL-AMT NOT =  ZEROS                           
05120         MOVE PB-C-AH-CANCEL-AMT  TO CREFUND2O      (1)            
05121         MOVE AL-UNNON            TO CREFUND2-ATTRB (1).           
05122                                                                   
05123      IF PB-C-LIVES GREATER ZEROS                                  
05124         MOVE PB-C-LIVES          TO CLIVESO      (1)              
05125         MOVE AL-UNNON            TO CLIVES-ATTRB (1).             
05126                                                                   
           IF PB-C-CANCEL-REASON GREATER SPACES
              MOVE PB-C-CANCEL-REASON  TO CCANREA (1)
              MOVE AL-UANON            TO CCANREA-ATTRB (1)
070622     ELSE
070622        PERFORM 6410-READ-ERACCT THRU 6410-EXIT
070622        IF AM-GPCD > 1
070622          AND AM-GPCD < 6
070622           MOVE 'Y'                 TO PB-C-CANCEL-REASON
070622           MOVE PB-C-CANCEL-REASON  TO CCANREA (1)
070622           MOVE AL-UANON            TO CCANREA-ATTRB (1)
070622        END-IF
           END-IF.

05127 *    IF PB-C-MICROFILM-NO  IS NUMERIC                             
05128 *        IF PB-C-MICROFILM-NO  NOT =  ZEROS                       
05129 *            MOVE PB-C-MICROFILM-NO                               
05130 *                                TO  CMICRO-NOO (1)               
05131 *            MOVE AL-UNNON       TO  CMICRO-NO-ATTRB (1).         
05132                                                                   
05133      IF PB-C-PAYEE-CODE GREATER SPACES                            
05134         MOVE PB-C-PAYEE-CODE     TO CPAYEE       (1)              
05135         MOVE AL-UANON            TO CPAYEE-ATTRB (1).             
05136                                                                   
05137      IF PB-C-REFUND-SW  GREATER SPACES                            
05138         MOVE PB-C-REFUND-SW      TO CCHK         (1)              
05139         MOVE AL-UANON            TO CCHK-ATTRB   (1).             
05140                                                                   
05141      IF PB-C-LAST-NAME GREATER SPACES                             
05142         MOVE PB-C-LAST-NAME      TO CLAST-NAME       (1)          
05143         MOVE AL-UANON            TO CLAST-NAME-ATTRB (1).         
05144                                                                   
05145      PERFORM 7180-PROTECT-FIELDS VARYING WS-SUB2 FROM +2 BY +1    
05146                                  UNTIL WS-SUB2 GREATER +4.        
05147                                                                   
05148      GO TO 7190-EXIT.                                             
05149                                                                   
05150  7180-PROTECT-FIELDS.                                             
05151      MOVE AL-SANOF               TO CCERT-ATTRB      (WS-SUB2)    
05152                                     CSFX-ATTRB       (WS-SUB2)    
05153                                     CEFFDT-ATTRB     (WS-SUB2)    
05154                                     CLAST-NAME-ATTRB (WS-SUB2)    
05155                                     CCANDT1-ATTRB    (WS-SUB2)    
05156                                     CCANDT2-ATTRB    (WS-SUB2)    
05157                                     CMTHD1-ATTRB     (WS-SUB2)    
05158                                     CMTHD2-ATTRB     (WS-SUB2)    
05159                                     CREFUND1-ATTRB   (WS-SUB2)    
05160                                     CREFUND2-ATTRB   (WS-SUB2)    
05161                                     CCHK-ATTRB       (WS-SUB2)    
05162                                     CPAYEE-ATTRB     (WS-SUB2)    
05163                                     CLIVES-ATTRB     (WS-SUB2)    
05164                                     CCANREA-ATTRB    (WS-SUB2).
05165                                                                   
05166  7190-EXIT.                                                       
05167                                                                   
05168      EJECT                                                        
05169                                                                   
05170  8100-SEND-INITIAL-MAP.                                           
05171      IF PI-MAP-NAME = EL630B                                      
05172          NEXT SENTENCE                                            
05173      ELSE                                                         
05174          GO TO 8110-SEND-INITIAL-CANCEL-MAP.                      
05175                                                                   
05176 *    MOVE PI-MEMBER-CAPTION        TO BCAPTNO.                    
05177                                                                   
05178      IF EIBAID NOT = DFHPF1   AND                                 
05179         EIBAID NOT = DFHPF2   AND                                 
05180         EIBAID NOT = DFHPF5   AND                                 
05181         PI-MAINT-FUNC NOT = 'B'                                   
05182           PERFORM 0600-PROTECT-FIELDS THRU 0600-EXIT.             
05183                                                                   
05184      MOVE PI-SAV-ENTRY-BATCH     TO BBATCHO.                      
05185      MOVE PI-AM-NAME             TO BACCTNMO
           IF ((PI-AM-ADDR1 NOT = SPACES)
              OR (PI-AM-ADDR2 NOT = SPACES))
              IF BNFICRYO (1:5) = LOW-VALUES OR '_____'
                 MOVE PI-AM-NAME       TO BNFICRYO
                 MOVE AL-UANON         TO BBENEFICIARY-ATTRB
              END-IF
              IF BCADDR1 (1:5) = LOW-VALUES OR '_____'
                 MOVE PI-AM-ADDR2      TO BCADDR1
                 MOVE AL-UANON         TO BCADDR1-ATTRB
              END-IF
              IF BCADDR2 = SPACES OR LOW-VALUES
      *          MOVE PI-AM-ADDR2      TO BCADDR2
                 MOVE AL-UANON         TO BCADDR2-ATTRB
              END-IF
              IF BCCITY = SPACES OR LOW-VALUES
                 MOVE PI-AM-CITY       TO BCCITY
                 MOVE AL-UANON         TO BCCITY-ATTRB
              END-IF
              IF BCSTATE = SPACES OR LOW-VALUES
                 MOVE PI-AM-STATE      TO BCSTATE
                 MOVE AL-UANON         TO BCSTATE-ATTRB
              END-IF
              IF BCZIPCD = SPACES OR LOW-VALUES
                 MOVE PI-AM-ZIP        TO BCZIPCD
020210           MOVE AL-UANON         TO BCZIPCD-ATTRB
              END-IF
           END-IF

030310     IF PI-AM-EDIT-LOAN-OFC = 'N'
030310        MOVE AL-SANOF            TO BLN-OFFICER-ATTRB
030310     END-IF

05187      IF PI-MAIL-YES                                               
020514        continue
05189      ELSE                                                         
05190         MOVE AL-SANOF            TO BADDRS1-ATTRB                 
05191                                     BADDRS2-ATTRB                 
05192                                     BCITY-ATTRB
                                          BSTATE-ATTRB
05193                                     BZIPCDE-ATTRB                 
05194 *                                   BZIP4-ATTRB                   
05195 *                                   BPHONE-ATTRB
           END-IF

101615     IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
101615        MOVE AL-SADOF            TO balt-ben2-attrb
101615                                    balt-prem2-attrb
101615     END-IF

101615     IF PI-COMPANY-ID NOT = 'DCC' and 'CID' and 'VPP'
101615        MOVE AL-SADOF            TO BVINHD-ATTRB
101615                                    BVIN-ATTRB
101615     END-IF

05197 *    IF PI-LAST-FUNC-DISPLAY                                      
05198 *          MOVE AL-SADOF            TO BCERTV-ATTRB               
05199 *                                      BSFXV-ATTRB                
05200 *                                      BEFFDTV-ATTRB              
05201 *                                      BLAST-NAMEV-ATTRB          
05202 *    ELSE                                                         
05203 *       IF PI-COMPANY-ID = ('PEM' OR 'CRI')                       
05204 *          MOVE AL-UANOF            TO BCERTV-ATTRB               
05205 *                                      BSFXV-ATTRB                
05206 *                                      BEFFDTV-ATTRB              
05207 *                                      BLAST-NAMEV-ATTRB          
05208 *       ELSE                                                      
05209 *          MOVE AL-SADOF            TO BCERTV-ATTRB               
05210 *                                      BSFXV-ATTRB                
05211 *                                      BEFFDTV-ATTRB              
05212 *                                      BLAST-NAMEV-ATTRB.         
05213                                                                   
05214      IF PI-LAST-FUNC-DISPLAY                                      
05215         NEXT SENTENCE                                             
05216      ELSE                                                         
05217         IF PI-COMPANY-ID = 'PEM' OR 'CRI'                         
05218            MOVE AL-UADOF            TO BCERT-ATTRB                
05219                                        BSFX-ATTRB                 
05220                                        BEFFDT-ATTRB               
05221                                        BLAST-NAME-ATTRB           
05222         ELSE                                                      
05223            MOVE AL-UANOF            TO BCERT-ATTRB                
05224                                        BEFFDT-ATTRB.              
05225                                                                   
05226      IF PI-NB-MONTH-END-DT NOT = SPACES                           
05227         MOVE PI-NB-MONTH-END-DT  TO DC-BIN-DATE-1                 
05228        ELSE                                                       
05229         MOVE PI-CR-MONTH-END-DT  TO DC-BIN-DATE-1.                
05230                                                                   
05231      MOVE SPACE                  TO DC-OPTION-CODE.               
05232      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
05233      MOVE DC-GREG-DATE-1-EDIT    TO BMOENDO.                      
05234                                                                   
05235      MOVE WS-CURRENT-DT          TO BDATEO.                       
05236      MOVE EIBTIME                TO TIME-IN.                      
05237      MOVE TIME-OUT               TO BTIMEO.                       
05238                                                                   
05239      MOVE PI-LIFE-OVERRIDE-L2    TO BKIND1
05240      MOVE AL-SABOF               TO BKIND1-ATTRB
05241      MOVE PI-AH-OVERRIDE-L2      TO BKIND2
05242      MOVE AL-SABOF               TO BKIND2-ATTRB
05243                                                                   
05244      IF PI-DATA-ERRORS
05245         MOVE SPACE               TO PI-ERROR-SW
05246      ELSE
05247         IF EIBAID = DFHPF1 OR DFHPF2
05248            CONTINUE
05249         ELSE
05250            MOVE -1               TO BCERTL
               END-IF
           END-IF

05252      MOVE EMI-MESSAGE-AREA (1)   TO BERMSG1O.                     
05253      MOVE EMI-MESSAGE-AREA (2)   TO BERMSG2O.                     
05254                                                                   
05255      EXEC CICS SEND                                               
05256          MAP      (PI-MAP-NAME)                                   
05257          MAPSET   (MAPSET-EL6301S)                                
05258          FROM     (EL630BI)                                       
05259          ERASE                                                    
05260          CURSOR                                                   
05261      END-EXEC.                                                    
05262                                                                   
05263      GO TO 9100-RETURN-TRAN.                                      
05264                                                                   
05265      EJECT                                                        
05266  8110-SEND-INITIAL-CANCEL-MAP.                                    
05267      IF EIBAID NOT = DFHPF5  AND                                  
05268         EIBAID NOT = DFHPF1  AND                                  
05269         EIBAID NOT = DFHPF2  AND                                  
05270         PI-MAINT-FUNC NOT = 'B'                                   
05271           PERFORM 0700-PROTECT-FIELDS THRU 0700-EXIT.             
05272                                                                   
05273      MOVE PI-SAV-ENTRY-BATCH     TO CBATCHO.                      
05274      MOVE PI-AM-NAME             TO CACCTNMO.                     
05275                                                                   
05276      IF PI-NB-MONTH-END-DT NOT = SPACES                           
05277         MOVE PI-NB-MONTH-END-DT  TO DC-BIN-DATE-1                 
05278        ELSE                                                       
05279         MOVE PI-CR-MONTH-END-DT  TO DC-BIN-DATE-1.                
05280                                                                   
05281      MOVE SPACE                  TO DC-OPTION-CODE.               
05282      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
05283      MOVE DC-GREG-DATE-1-EDIT    TO CMOENDO.                      
05284                                                                   
05285      MOVE WS-CURRENT-DT          TO CDATEO.                       
05286      MOVE EIBTIME                TO TIME-IN.                      
05287      MOVE TIME-OUT               TO CTIMEO.                       
05288                                                                   
05289      MOVE PI-LIFE-OVERRIDE-L2    TO CKIND1 (1)                    
05290                                     CKIND1 (2)                    
05291                                     CKIND1 (3)                    
05292                                     CKIND1 (4).                   
05293      MOVE PI-AH-OVERRIDE-L2      TO CKIND2 (1)                    
05294                                     CKIND2 (2)                    
05295                                     CKIND2 (3)                    
05296                                     CKIND2 (4).                   
05297      MOVE AL-SABOF               TO CKIND1-ATTRB (1)              
05298                                     CKIND1-ATTRB (2)              
05299                                     CKIND1-ATTRB (3)              
05300                                     CKIND1-ATTRB (4)              
05301                                     CKIND2-ATTRB (1)              
05302                                     CKIND2-ATTRB (2)              
05303                                     CKIND2-ATTRB (3)              
05304                                     CKIND2-ATTRB (4).             
05305                                                                   
05306      IF PI-DATA-ERRORS                                            
05307          MOVE SPACE              TO PI-ERROR-SW                   
05308      ELSE                                                         
05309          IF EIBAID = DFHPF1 OR DFHPF2                             
05310              NEXT SENTENCE                                        
05311          ELSE                                                     
05312              MOVE -1             TO CCERT1L.                      
05313                                                                   
05314      MOVE EMI-MESSAGE-AREA (1)   TO CERMSG1O.                     
05315      MOVE EMI-MESSAGE-AREA (2)   TO CERMSG2O.                     
05316                                                                   
05317      EXEC CICS SEND                                               
05318          MAP      (PI-MAP-NAME)                                   
05319          MAPSET   (MAPSET-EL6301S)                                
05320          FROM     (EL630BI)                                       
05321          ERASE                                                    
05322          CURSOR                                                   
05323      END-EXEC.                                                    
05324                                                                   
05325      GO TO 9100-RETURN-TRAN.                                      
05326                                                                   
05327      EJECT                                                        
05328                                                                   
05329  8200-SEND-DATAONLY.                                              
05330      MOVE SPACE              TO PI-ERROR-SW.                      
05331                                                                   
05332      IF PI-MAP-NAME = EL630B                                      
05333 *        MOVE PI-MEMBER-CAPTION      TO BCAPTNO                   
05334          MOVE WS-CURRENT-DT          TO BDATEO                    
05335          MOVE EIBTIME                TO TIME-IN                   
05336          MOVE TIME-OUT               TO BTIMEO                    
05337          MOVE EMI-MESSAGE-AREA (1)   TO BERMSG1O                  
05338          MOVE EMI-MESSAGE-AREA (2)   TO BERMSG2O                  
05339          EXEC CICS SEND                                           
05340              MAP      (PI-MAP-NAME)                               
05341              MAPSET   (MAPSET-EL6301S)                            
05342              FROM     (EL630BI)                                   
05343              DATAONLY                                             
05344              CURSOR                                               
05345          END-EXEC                                                 
05346      ELSE                                                         
05347          MOVE WS-CURRENT-DT          TO CDATEO                    
05348          MOVE EIBTIME                TO TIME-IN                   
05349          MOVE TIME-OUT               TO CTIMEO                    
05350          MOVE EMI-MESSAGE-AREA (1)   TO CERMSG1O                  
05351          MOVE EMI-MESSAGE-AREA (2)   TO CERMSG2O                  
05352          EXEC CICS SEND                                           
05353              MAP      (PI-MAP-NAME)                               
05354              MAPSET   (MAPSET-EL6301S)                            
05355              FROM     (EL630BI)                                   
05356              DATAONLY                                             
05357              CURSOR                                               
05358          END-EXEC.                                                
05359                                                                   
05360      GO TO 9100-RETURN-TRAN.                                      
05361                                                                   
05362      EJECT                                                        
05363                                                                   
05364  8300-SEND-TEXT.                                                  
05365      EXEC CICS SEND TEXT                                          
05366          FROM     (LOGOFF-TEXT)                                   
05367          LENGTH   (LOGOFF-LENGTH)                                 
05368          ERASE                                                    
05369          FREEKB                                                   
05370      END-EXEC.                                                    
05371                                                                   
05372      EXEC CICS RETURN                                             
05373      END-EXEC.                                                    
05374                                                                   
05375  8350-SEND-WARNING.                                               
05376      EXEC CICS SEND TEXT                                          
05377          FROM     (WARNING-TEXT)                                  
05378          LENGTH   (WARNING-LENGTH)                                
05379          ERASE                                                    
05380          FREEKB                                                   
05381      END-EXEC.                                                    
05382                                                                   
05383      GO TO 9100-RETURN-TRAN.                                      
05384                                                                   
05385  8400-LOG-JOURNAL-RECORD.                                         

041320     if (pi-journal-file-id > 0)
041320        and (jp-file-id = file-id-erpndb)
041320
041320        move eibdate             to jp-date
041320        move eibtime             to jp-time
041320        MOVE PI-PROCESSOR-ID     TO JP-USER-ID
041320        MOVE 03                  TO PI-JOURNAL-FILE-ID
041320        MOVE THIS-PGM            TO JP-PROGRAM-ID
041320        
041320**      length is 585 plus 30 extra for jrnl stuff
041320**      system already accounts for the 34.
041320        
041320        EXEC CICS JOURNAL
041320           JFILEID   (PI-JOURNAL-FILE-ID)
041320           JTYPEID   ('EL')
041320           FROM      (JOURNAL-RECORD)
041320           LENGTH    (615)
041320           resp      (ws-response)
041320        END-EXEC
041320        
041320        if resp-normal
041320           continue
041320        else
041320           display ' error-el6301-journal ' ws-response
041320        end-if
041320     end-if
041320
041320*    EXEC CICS JOURNAL                                            
041320*        JFILEID     (PI-JOURNAL-FILE-ID)                         
041320*        JTYPEID     ('EL')                                       
041320*        FROM        (JOURNAL-RECORD)                             
041320*        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   
041320*        END-EXEC.                                                

           .
       8400-exit.
           exit.

05396  8500-DATE-CONVERT.                                               
05397      EXEC CICS LINK                                               
05398          PROGRAM  (LINK-ELDATCV)                                  
05399          COMMAREA (DATE-CONVERSION-DATA)                          
05400          LENGTH   (DC-COMM-LENGTH)                                
05401      END-EXEC.                                                    
05402                                                                   
05403  8500-EXIT.                                                       
05404      EXIT.                                                        
05405                                                                   
05406      EJECT                                                        
05407                                                                   
05408  8550-SET-MAP-SEQ-NOS.                                            
05409      IF PI-MAP-NAME = EL630B                                      
05410          MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ                      
05411          MOVE AL-SABON               TO BSEQ-ATTRB                
05412      ELSE                                                         
05413          MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ (WS-SUB2)            
05414          MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).     
05415                                                                   
05416      ADD +1  TO PI-NEXT-DISPLAY-SEQ-NO.                           
05417                                                                   
05418  8555-EXIT.                                                       
05419      EXIT.                                                        
05420                                                                   
05421  8600-DEEDIT.                                                     
05422      EXEC CICS BIF DEEDIT                                         
05423          FIELD   (DEEDIT-FIELD)                                   
05424          LENGTH  (15)                                             
05425      END-EXEC.                                                    
05426                                                                   
05427  8600-EXIT.                                                       
05428      EXIT.                                                        
05429                                                                   
05430  8800-UNAUTHORIZED-ACCESS.                                        
05431      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   
05432      GO TO 8300-SEND-TEXT.                                        
070622
070622 9000-CONNECT-TO-DB.
070622
070622     IF SVR > SPACES
070622        CONTINUE
070622     ELSE
063022*  The below is unnecessary but I'll play along
070622        MOVE 'PROD_LOGIC'           TO SVR
070622        MOVE 'appuser'              TO USR
070622        MOVE 'appuser@cso'          TO PASS
070622     END-IF
070622
070622     STRING
070622         USR DELIMITED SPACE
070622         "." DELIMITED SIZE
070622         PASS DELIMITED SPACE INTO USR-PASS
070622     END-STRING
070622
070622     EXEC SQL
070622        CONNECT TO :SVR USER :USR-PASS
070622     END-EXEC
070622
070622     IF SQLCODE NOT = 0
070622        DISPLAY "ERROR: CANNOT CONNECT "
070622        DISPLAY SQLCODE
070622        DISPLAY SQLERRMC
070622     END-IF
070622
070622     .
070622 9000-EXIT.
070622     EXIT.
070622 9050-DISCONNECT.
070622
070622     EXEC SQL
070622        DISCONNECT
070622     END-EXEC
070622     .
070622 9050-EXIT.
070622     EXIT.
05433                                                                   
05434  9000-RETURN-CICS.                                                
05435      EXEC CICS RETURN                                             
05436      END-EXEC.                                                    
05437                                                                   
05438  9100-RETURN-TRAN.                                                
05439      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             
05440                                                                   
05441      IF  PI-MAP-NAME = EL630B                                     
05442          MOVE '630B'             TO PI-CURRENT-SCREEN-NO.         
05443                                                                   
05444      IF  PI-MAP-NAME = EL630C                                     
05445          MOVE '630C'             TO PI-CURRENT-SCREEN-NO.         
05446                                                                   
05447      EXEC CICS RETURN                                             
05448          TRANSID    (TRANS-EXA6)                                  
05449          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
05450          LENGTH     (WS-COMM-LENGTH)                              
05451      END-EXEC.                                                    
05452                                                                   
05453  9300-XCTL.                                                       
05454      EXEC CICS XCTL                                               
05455          PROGRAM    (PGM-NAME)                                    
05456          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
05457          LENGTH     (WS-COMM-LENGTH)                              
05458      END-EXEC.                                                    
05459                                                                   
05460  9400-CLEAR.                                                      
05461      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                      
05462      GO TO 9300-XCTL.                                             
05463                                                                   
05464  9500-PF12.                                                       
05465      MOVE XCTL-EL010             TO PGM-NAME.                     
05466      GO TO 9300-XCTL.                                             
05467                                                                   
05468  9600-PGMID-ERROR.                                                
05469      EXEC CICS HANDLE CONDITION                                   
05470          PGMIDERR    (8300-SEND-TEXT)                             
05471      END-EXEC.                                                    
05472                                                                   
05473      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           
05474      MOVE ' '                    TO PI-ENTRY-CD-1.                
05475      MOVE XCTL-EL005            TO PGM-NAME.                      
05476      MOVE PGM-NAME               TO LOGOFF-PGM.                   
05477      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  
05478      GO TO 9300-XCTL.                                             
05479                                                                   
05480  9900-ERROR-FORMAT.                                               
05481      IF PI-MAP-NAME = EL630B                                      
05482         MOVE 2                   TO EMI-NUMBER-OF-LINES           
05483        ELSE                                                       
05484         MOVE 1                   TO EMI-NUMBER-OF-LINES.          
05485                                                                   
05486      IF NOT EMI-ERRORS-COMPLETE                                   
05487          MOVE LINK-EL001         TO PGM-NAME                      
05488          EXEC CICS LINK                                           
05489              PROGRAM    (PGM-NAME)                                
05490              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           
05491              LENGTH     (EMI-COMM-LENGTH)                         
05492          END-EXEC.                                                
05493                                                                   
05494  9900-EXIT.                                                       
05495      EXIT.                                                        
05496                                                                   
05497  9990-ABEND.                                                      
05498      MOVE LINK-EL004             TO PGM-NAME.                     
05499      MOVE DFHEIBLK               TO EMI-LINE1.                    
05500      EXEC CICS LINK                                               
05501          PROGRAM   (PGM-NAME)                                     
05502          COMMAREA  (EMI-LINE1)                                    
05503          LENGTH    (72)                                           
05504      END-EXEC.                                                    
05505                                                                   
05506      IF PI-MAP-NAME = EL630B                                      
05507          MOVE -1 TO BPFENTRL                                      
05508      ELSE                                                         
05509          MOVE -1 TO CPFENTRL.                                     
05510                                                                   
05511      GO TO 8200-SEND-DATAONLY.                                    
05512                                                                   
05513      GOBACK.                                                      
05514                                                                   
05515  9995-SECURITY-VIOLATION.                                         
05516      COPY ELCSCTP.                                                
05517                                                                   
05518  9995-EXIT.                                                       
05519      EXIT.                                                        
05520                                                                   
