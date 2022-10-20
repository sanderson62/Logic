00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 ECS010.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 04/27/94 07:51:58.                 
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          
00008 *                            VMOD=2.082.                          
00009                                                                   
00010 *AUTHOR.        LOGIC, INC.                                       
00011 *               DALLAS, TEXAS.                                    
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
00026 *        THIS PROGRAM ACCEPTS VALIDATED TRANSACTIONS FROM EL522   
00027 *        AND APPLIES THEM TO THE CERTIFICATE MASTER FILE.         
00028 *        EXTRACTS ARE PRODUCED FOR PRODUCTION  EARNED PREMIUM &   
00029 *        LOSS, REINSURANCE, COMMISSIONS, CLAIMS REGISTER, ETC.    
031102******************************************************************
031102*                   C H A N G E   L O G
031102*
031102* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031102*-----------------------------------------------------------------
031102*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031102* EFFECTIVE    NUMBER
031102*-----------------------------------------------------------------
061402* 061802                   PEMA  ADD PROCESS FOR PEOPLES
062602* 062602    2002061900011  SMVA  CHANGE SYS009 SYSOUT TO FILE WITH
062602*                              ORGANIZATION AS LINE SEQUENTIAL
070102* 070102                   SMVA  CLEAN UP COMPILE ERRORS
080702* 080702    2002061800008  PEMA  ADD PROCESS FOR SUNFLOWER
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 450 TO 900
111402* 111402    2001061800003  PEMA  ADD DCC PROCESSING
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
052203* 052703                   PEMA  MODIFY CEDE BALLOON PREM TO NSP
070203* 070203                   PEMA  BUILD REIN COMP DATA FOR NSP
100703* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
123003* 123003                   PEMA  MODIFY COMPUTE STMTS FOR REIN
040504* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
062104* 062104    2004050700001  SMVA  ADD NEW FILES TO AUTOMATE ME BALANCING
102204* 102204    2004101500007  PEMA  IBNR FOR DCC
111204* 111204    2004110300005  PEMA  SPLIT SPP BANK FEE
123004* 123004                   PEMA  ADD REFUND PROCESSING FOR "MONTHLY"
020305* 020305    2005020000000  PEMA  ADD CLP STATE PROCESSING
092705* 092705  CR2005050300006  PEMA  ADD SPP LEASES
052306* 052306  CR2006050800002  PEMA  ADD COMM TYPE 'J'
061206* 061206  CR2006050500001  PEMA  ADD FIRST PREMIER BANK
011207* 011207                   PEMA  PASS SIG SW TO BATCH CERT FILE
042607*  special one time only ecs010 to process VA thru 04/30/07 eom
031102******************************************************************
00030  EJECT                                                            
00031  ENVIRONMENT DIVISION.                                            
00032  CONFIGURATION SECTION.                                           
00033  SPECIAL-NAMES.                                                   
00034      C02 IS LCP-CH2                                               
00035      C03 IS LCP-CH3                                               
00036      C04 IS LCP-CH4                                               
00037      C05 IS LCP-CH5                                               
00038      C06 IS LCP-CH6                                               
00039      C07 IS LCP-CH7                                               
00040      C08 IS LCP-CH8                                               
00041      C09 IS LCP-CH9                                               
00042      C10 IS LCP-CH10                                              
00043      C11 IS LCP-CH11                                              
00044      C12 IS LCP-CH12                                              
00045      S01 IS LCP-P01                                               
00046      S02 IS LCP-P02.                                              
00047                                                                   
00048  INPUT-OUTPUT SECTION.                                            
00049  FILE-CONTROL.                                                    
00050                                                                   
00051      SELECT PENDING-EXTRACT  ASSIGN TO SYS005.
00052                                                                   
00053      SELECT CERT-MASTER-IN   ASSIGN TO SYS010.
00054                                                                   
070102     SELECT ERACCTT-IN       ASSIGN TO ERACCTT
00056                              ACCESS IS SEQUENTIAL                 
00057                              ORGANIZATION IS INDEXED              
00058                              FILE STATUS IS ERACCTT-FILE-STATUS   
00059                              RECORD KEY IS AM-CONTROL-PRIMARY.    
00060                                                                   
070102     SELECT ERRTBL-IN        ASSIGN TO ERRTBLT
00062                              ACCESS IS DYNAMIC                    
00063                              ORGANIZATION IS INDEXED              
00064                              FILE STATUS IS ERRTBL-FILE-STATUS    
00065                              RECORD KEY IS RE-CONTROL-PRIMARY.    
00066                                                                   
pemuni     SELECT ERCTBL           ASSIGN TO ERCTBLT
00068                              ACCESS IS DYNAMIC                    
00069                              ORGANIZATION IS INDEXED              
00070                              FILE STATUS IS ERCTBL-FILE-STATUS    
00071                              RECORD KEY IS CT-CONTROL-PRIMARY.    
00072                                                                   
070102     SELECT ERMEBL-INOUT     ASSIGN TO ERMEBL
00074                              ORGANIZATION INDEXED                 
00075                              ACCESS DYNAMIC                       
00076                              RECORD KEY ME-CONTROL-PRIMARY        
00077                              FILE STATUS ERMEBL-FILE-STATUS.      
00078                                                                   
070102     SELECT ERRESC-IN        ASSIGN TO ERRESC
00080                              ORGANIZATION IS INDEXED              
00081                              ACCESS IS DYNAMIC                    
00082                              RECORD KEY IS ERRESC-RECORD-KEY      
00083                              FILE STATUS IS ERRESC-FILE-STATUS.   
00084                                                                   
111204     SELECT ELCRTT           ASSIGN TO ELCRTT
111204                             ACCESS IS DYNAMIC                    
111204                             ORGANIZATION IS INDEXED              
111204                             FILE STATUS IS ELCRTT-FILE-STATUS    
111204                             RECORD KEY IS CS-CONTROL-PRIMARY.    
111204                                                                  
111204     SELECT ERAGTC           ASSIGN TO ERAGTC
111204                             ACCESS IS DYNAMIC                    
111204                             ORGANIZATION IS INDEXED              
111204                             FILE STATUS IS ERAGTC-FILE-STATUS    
111204                             RECORD KEY IS AG-CONTROL-PRIMARY.    
111204                                                                  
00085      SELECT CERT-MASTER-OUT  ASSIGN TO SYS011.
00086                                                                   
00087      SELECT DETAIL-EXTR      ASSIGN TO SYS017.
00088                                                                   
00089      SELECT SUMMARY-EXTR     ASSIGN TO SYS018.
00090                                                                   

062104     SELECT ME-ECS010-BALANCE
062104                             ASSIGN TO SYS012
062104                             ORGANIZATION IS LINE SEQUENTIAL.

062104     SELECT ME50-ECS010-BALANCE
062104                             ASSIGN TO SYS013
062104                             ORGANIZATION IS LINE SEQUENTIAL.

CIDMOD     SELECT DISPLAY-PRT      ASSIGN TO SYS022.
CIDMOD
00091      SELECT DISK-DATE        ASSIGN TO SYS019.
00092                                                                   
070102     SELECT PRINT-COPY       ASSIGN TO SYS008.
00094                                                                   
00095      SELECT MISMATCH-REPORT  ASSIGN TO SYS009. 
00096                                                                   
00097      SELECT FICH             ASSIGN TO SYS020.
00098  EJECT                                                            
00099  DATA DIVISION.                                                   
00100  FILE SECTION.                                                    
00101                                                                   
00102  FD  PENDING-EXTRACT                                              
00103      BLOCK CONTAINS 0 RECORDS
00104      RECORDING MODE F.                                            
00105                                                                   
00106                              COPY ERCEXTD.                        
00107  EJECT                                                            
00108  FD  CERT-MASTER-IN                                               
00109                              COPY ECSCRIFD.                       
00110                                                                   
00111  01  CI-RECORD.                                                   
00112      12  FILLER          PIC  X(03).                              
00113      12  CI-ACCOUNT-CONTROL.                                      
00114          16  CI-CARRIER      PIC  X(01).                          
00115          16  CI-GROUPING     PIC  X(06).                          
00116          16  CI-STATE        PIC  X(02).                          
00117          16  CI-ACCOUNT      PIC  X(10).                          
00118      12  FILLER          PIC  X(06).                              
00119      12  CI-CERT-NO.                                              
00120          16  CI-CERT     PIC X(10).                               
00121          16  CI-CERT-SFX PIC X.                                   
00122      12  FILLER          PIC  X(1017).                            
00123                                                                   
CIDMOD******************************************************
CIDMOD*   THIS FILE DEFINITION WAS ADDED TO ACCOMMODATE THE EXECUTION
CIDMOD*   OF A "MID MONTH" BILLING REPORT (EL562) FOR
CIDMOD*   COMMERCIAL FEDERAL BANK
CIDMOD******************************************************
CIDMOD 01  CF-RECORD.
CIDMOD     12  FILLER          PIC X(759).
CIDMOD     12  CF-THIRD-AGENT  PIC X(10).
CIDMOD     12  FILLER          PIC X(7).
CIDMOD     12  CF-FOURTH-AGENT PIC X(10).
CIDMOD     12  FILLER          PIC X(270).
CIDMOD
CIDMOD******************************************************
00124  EJECT                                                            
070102 FD  ERACCTT-IN.
00126                                                                   
00127                              COPY ERCACCT.                        
00128  EJECT                                                            
070102 FD  ERRTBL-IN.
00130                                                                   
00131                              COPY ERCREIN.                        
00132  EJECT                                                            
00133  FD  ERCTBL.                                                      
00134                                                                   
00135                              COPY ERCCTBL.                        
00136  EJECT                                                            
070102 FD  ERMEBL-INOUT.
00138                                                                   
00139                              COPY ERCMEBL.                        
00140  EJECT                                                            
00141  FD  CERT-MASTER-OUT                                              
00142                              COPY ECSCROFD.                       
00143  EJECT                                                            
00144  FD  DETAIL-EXTR                                                  
00145      BLOCK CONTAINS 0 RECORDS
00146      RECORDING MODE F.                                            
00147                                                                   
00148                              COPY ECSEXT01.                       
00149  EJECT                                                            
00150  FD  SUMMARY-EXTR                                                 
00151      BLOCK CONTAINS 0 RECORDS
00152      RECORDING MODE F.                                            
00153                                                                   
00154                              COPY ECSEPC01.                       
00155                                                                   
070102 FD  ERRESC-IN.
00157                              COPY ERCRESC.                        
111204 FD  ELCRTT.                                                      
111204                                                                  
111204                             COPY ELCCRTT.
111204
111204 FD  ERAGTC.                                                      
111204                                                                  
111204                             COPY ERCAGTC.
111204
062104 FD  ME-ECS010-BALANCE
062104     RECORDING MODE IS F
062104     BLOCK CONTAINS 0 RECORDS.
062104 01  ME-ECS010-BALANCE-REC    PIC X(83).

062104 FD  ME50-ECS010-BALANCE
062104     RECORDING MODE IS F
062104     BLOCK CONTAINS 0 RECORDS.
062104 01  ME50-ECS010-BALANCE-REC  PIC X(95).

00158  EJECT                                                            
00159  FD  DISK-DATE                                                    
00160                              COPY ELCDTEFD.                       
00161  EJECT                                                            
070102 FD  PRINT-COPY
00163                              COPY ELCPRTFD.                       
00164  EJECT                                                            
00165  FD  MISMATCH-REPORT                                              
00166      RECORDING MODE F.                                            
00167                                                                   
00168  01  MISMATCH-RPT.                                                
00169      12  MR-CTL              PIC X.                               
00170      12  MR-DATA             PIC X(132).                          
00171  EJECT                                                            
00172  FD  FICH                                                         
00173                              COPY ELCFCHFD.                       
00174  EJECT                                                            
00160  FD  DISPLAY-PRT
00161      RECORDING MODE IS F
00162      LABEL RECORDS ARE STANDARD
00163      BLOCK CONTAINS 0 RECORDS
00164      RECORD CONTAINS 133 CHARACTERS.
00165
00166  01  DISPLAY-REC.
00167      05  DISPLAY-CC           PIC X.
00168      05  DISPLAY-INFO         PIC X(132).
00169  EJECT
00175  WORKING-STORAGE SECTION.                                         
00176  77  LCP-ASA                       PIC X.                         
00177  77  FILLER  PIC X(32)  VALUE '********************************'. 
00178  77  FILLER  PIC X(32)  VALUE '*     ECS010 WORKING-STORAGE    '. 
00179  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.082 ************'. 
00180                                                                   
       77  WS-DISP-AMT             PIC Z,ZZZ,ZZZ.99    VALUE ZEROS.
CIDMOD 77  ERROR-COUNT             PIC 9(7)            VALUE ZEROS.
CIDMOD 77  DIS-LINE-CNT            PIC 99              VALUE ZEROS.
00181  77  PGM-SUB                 PIC S999    COMP    VALUE +010.      
00182  77  FIRST-ACCOUNT           PIC S9      COMP-3  VALUE +1.        
00183  77  FIRST-CERT              PIC S9      COMP-3  VALUE +1.        
00184  77  TR-CNT                  PIC S9(5)   COMP-3  VALUE +0.        
00185  77  CMIN-CNT                PIC S9(7)   COMP-3  VALUE +0.        
00186  77  CMOT-CNT                PIC S9(7)   COMP-3  VALUE +0.        
00187  77  DE-CNT                  PIC S9(7)   COMP-3  VALUE +0.        
00188  77  EP-CNT                  PIC S9(7)   COMP-3  VALUE +0.        
00189  77  LN-CNT                  PIC S999    COMP-3  VALUE +61.       
00190  77  PG-CNT                  PIC S9(5)   COMP-3  VALUE +0.        
00191  77  DUP-CNT                 PIC S9(5)   COMP-3  VALUE +0.        
00192  77  DISABILITY-TBL-MAX      PIC S9              VALUE +5.        
00193  77  DUP-MAX                 PIC S9(5)   COMP-3  VALUE +500.      
00194  77  RANGE-CNT               PIC S999    COMP-3  VALUE +0.        
00195  77  ERACCTT-READS           PIC S9(7)   COMP-3  VALUE +0.        
111204 77  ERAGTC-READS            PIC S9(7)   COMP-3  VALUE +0.
111204 77  ELCRTT-READS            PIC S9(7)   COMP-3  VALUE +0.
00196  77  ERRTBL-READS            PIC S9(7)   COMP-3  VALUE +0.        
00197  77  ERCTBL-READS            PIC S9(7)   COMP-3  VALUE +0.        
00198  77  FLA-CLM-REIN-BASE       PIC S9(9)V99 COMP-3.                 
00199  77  WK-RATE                 PIC S99V9(5) COMP-3 VALUE +0.        
00200  77  COMM-CK-AMT             PIC S9(9)V99 COMP-3 VALUE +0.        
00201  77  ADJ-FLA-REIN-BASE       PIC S9(9)V9(4) COMP-3.               
CIDMOD 77  ISS-WK                  PIC S9(9)V99 COMP-3 VALUE +0.        
CIDMOD 77  CNC-WK                  PIC S9(9)V99 COMP-3 VALUE +0.        
100703 77  CNC-FACT                PIC S9(3)V9(7) COMP-3 VALUE +0.
00202  77  COMM-WK                 PIC S9(9)V99 COMP-3 VALUE +0.        
00203  77  X                       PIC X.                               
00204  77  DD-FLG                  PIC X               VALUE SPACE.     
00205  77  AGT-SW                  PIC X               VALUE SPACE.     
00206  77  RC-COMM-FLG             PIC X               VALUE SPACE.     
00207  77  RECALC-TYPE             PIC X               VALUE SPACE.     
00208  77  LIFE-REFUND             PIC S9(7)V99        COMP-3.          
00209  77  LIFE-REFUND-CALC        PIC S9(7)V99        COMP-3.          
00210  77  A-H-REFUND              PIC S9(7)V99        COMP-3.          
00211  77  A-H-REFUND-CALC         PIC S9(7)V99        COMP-3.          
00212  77  YTD-CANCEL-CNT          PIC S9(3)           COMP-3.          
00213  77  MISMATCH-COUNT          PIC S9(5)           VALUE +0.        
00214  77  MISMATCH-LINE-COUNT     PIC S9(5)           VALUE +99.       
00215  77  MISMATCH-PAGE-NUMBER    PIC S9(5)           VALUE +0.        
00216  77  SV-HI-CERT              PIC 9(11)           VALUE ZEROS.     
00217  77  SV-LO-CERT              PIC 9(11)           VALUE ZEROS.     
00218  77  LST-TRD-CERT            PIC X(11)           VALUE SPACES.    
00219  77  SV-TRD-CERT             PIC X(11).                           
00220  77  LST-TRD-ISDT            PIC X(10)           VALUE SPACES.    
00221  77  SV-TRD-ISDT             PIC X(10).                           
00222  77  SAVE-EXTRACT            PIC X(510)          VALUE SPACES.    
00223 *77  W-TLR                   PIC S9(01)V9(04) COMP-3 VALUE +.40.  
00224 *77  W-AH-FACTOR             PIC S9(01)V9(04) COMP-3 VALUE +.50.  
00225  77  W-EPEC-EP               PIC S9(11)V9(02) COMP-3 VALUE +0.    
00226  77  W-EPEC-EP-IN            PIC S9(11)V9(02) COMP-3 VALUE +0.    
00227  77  W-EPEC-EP-1             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00228  77  W-EPEC-EP-2             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00229  77  W-P-EP                  PIC S9(11)V9(02) COMP-3 VALUE +0.    
00230  77  W-M-EP                  PIC S9(11)V9(02) COMP-3 VALUE +0.    
00231  77  W-R-EP                  PIC S9(11)V9(02) COMP-3 VALUE +0.    
00232  77  W-WY-EP                 PIC S9(11)V9(02) COMP-3 VALUE +0.    
00233  77  W-VA-EP                 PIC S9(11)V9(02) COMP-3 VALUE +0.    
00234  77  W-1-EP                  PIC S9(11)V9(02) COMP-3 VALUE +0.    
00235  77  W-OTHERS-COUNT          PIC S9(11)       COMP-3 VALUE +0.    
00236  77  W-AH-CLAIM-EP           PIC S9(11)V9(02) COMP-3 VALUE +0.    
00237  77  W-AH-CLAIM-CTR          PIC S9(11)       COMP-3 VALUE +0.    
00238  77  W-EP-LESS-THAN-0        PIC S9(11)       COMP-3 VALUE +0.    
00239  77  W-CLAIMS-MADE-PAID-IN   PIC S9(11)V9(02) COMP-3 VALUE +0.    
00240  77  W-CLAIMS-MADE-PAID      PIC S9(11)V9(02) COMP-3 VALUE +0.    
00241  77  W-AH-IBNR               PIC S9(11)V9(02) COMP-3 VALUE +0.    
00242  77  W-AH-IBNR-1             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00243  77  W-AH-IBNR-1-CNT         PIC S9(11)       COMP-3 VALUE +0.    
00244  77  W-AH-IBNR-2             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00245  77  W-AH-IBNR-2-CNT         PIC S9(11)       COMP-3 VALUE +0.    
00246  77  W-LIFE-IBNR             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00247  77  W-LIFE-IBNR-IN          PIC S9(11)V9(02) COMP-3 VALUE +0.    
00248  77  W-LIFE-REMAINING-AMT    PIC S9(11)V9(02) COMP-3 VALUE +0.    
00249  77  W-RTRM-0-CLMAMT         PIC S9(11)V9(02) COMP-3 VALUE +0.    
00250  77  W-CLM-EXCEPT-CNT        PIC S9(11)       COMP-3 VALUE +0.    
00251  77  W-CLM-EXCEPT-AMT        PIC S9(11)V9(02) COMP-3 VALUE +0.    
00252  77  W-RTRM-0-CNT            PIC S9(11)       COMP-3 VALUE +0.    
00253  77  W-RTRM-EP-1             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00254  77  W-RTRM-EP-2             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00255  77  W-RTRM-EP               PIC S9(11)V9(02) COMP-3 VALUE +0.    
00256  77  W-01-REMAMT             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00257  77  W-01-DATA-USED          PIC S9(11)       COMP-3 VALUE +0.    
00258  77  W-02-REMAMT             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00259  77  W-02-DATA-USED          PIC S9(11)       COMP-3 VALUE +0.    
00260  77  W-03-REMAMT             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00261  77  W-03-DATA-USED          PIC S9(11)       COMP-3 VALUE +0.    
00262  77  W-04-REMAMT             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00263  77  W-04-DATA-USED          PIC S9(11)       COMP-3 VALUE +0.    
00264  77  W-05-REMAMT             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00265  77  W-05-DATA-USED          PIC S9(11)       COMP-3 VALUE +0.    
00266  77  W-06-REMAMT             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00267  77  W-06-DATA-USED          PIC S9(11)       COMP-3 VALUE +0.    
00268  77  W-30-REMAMT             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00269  77  W-30-DATA-USED          PIC S9(11)       COMP-3 VALUE +0.    
00270  77  W-31-REMAMT             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00271  77  W-31-DATA-USED          PIC S9(11)       COMP-3 VALUE +0.    
00272  77  W-XX-REMAMT             PIC S9(11)V9(02) COMP-3 VALUE +0.    
00273  77  WS-TOT-PRM-AMT          PIC S9(11)V9(02) COMP-3 VALUE +0.    
00274  77  W-XX-DATA-USED          PIC S9(11)       COMP-3 VALUE +0.    
00275  77  W-MM-LF-PRM             PIC S9(7)V99     COMP-3 VALUE +0.    
00276  77  W-MM-AH-PRM             PIC S9(7)V99     COMP-3 VALUE +0.    
00277  77  W-MM-LF-RFD             PIC S9(7)V99     COMP-3 VALUE +0.    
00278  77  W-MM-AH-RFD             PIC S9(7)V99     COMP-3 VALUE +0.    
00279  77  W-MM-LF-CLM             PIC S9(7)V99     COMP-3 VALUE +0.    
00280  77  W-MM-AH-CLM             PIC S9(7)V99     COMP-3 VALUE +0.    
00281  77  WS-CEDE-FACT            PIC S9V9(4)      COMP-3 VALUE +0.    
00282  77  WS-SAVE-CARR            PIC X                   VALUE ' '.   
00283  77  WS-LOOP-MAX             PIC S99          COMP-3 VALUE +10.   
CIDMOD 77  WS-CR-PMT-FREQ          PIC 99           VALUE ZEROS.
052203 77  WS-WE-LFPRM             PIC S9(7)V99    COMP-3 VALUE +0.              
040504 77  WS-FIRST-K              PIC X                  VALUE ' '.
040504 77  WS-DLR-INC-CHG-BACK     PIC X                  VALUE ' '.
102204 77  TOT-AH-IBNR             PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  TOT-AH-PRM              PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  TOT-AH-PD-CLMS          PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  TOT-AH-CLMRSV           PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  TOT-LF-IBNR             PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  TOT-LF-PRM              PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  TOT-LF-PD-CLMS          PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  TOT-LF-CLMRSV           PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  RTOT-AH-IBNR            PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  RTOT-AH-PRM             PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  RTOT-AH-PD-CLMS         PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  RTOT-AH-CLMRSV          PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  RTOT-LF-IBNR            PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  RTOT-LF-PRM             PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  RTOT-LF-PD-CLMS         PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  RTOT-LF-CLMRSV          PIC S9(11)V99 COMP-3 VALUE +0.
102204 77  IBNR-FACT               PIC S9(3)V9(7) COMP-3 VALUE +0.
00284                                                                   
00285  01  MONTH-END-DATA.                                              
00286      12  ME-START-DATE.                                           
00287          16  ME-START-MO         PIC 99.                          
00288          16  FILLER              PIC X.                           
00289          16  ME-START-DA         PIC 99.                          
00290          16  FILLER              PIC X.                           
00291          16  ME-START-YR         PIC 99.                          
00292      12  ME-CNDS-DATE            PIC 9(6).                        
00293      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   
00294          16  ME-CNDS-MO          PIC 99.                          
00295          16  ME-CNDS-DA          PIC 99.                          
00296          16  ME-CNDS-YR          PIC 99.                          
00297      12  ME-START-TIME           PIC 9(6).                        
00298      12  ME-UPDATE-FLAG          PIC X VALUE 'Y'.                 
00299          88  ME-DO-UPDATE        VALUE 'Y'.                       
00300          88  ME-NO-UPDATE        VALUE 'N'.                       
00301      12  MONTH-END-MOYR          PIC S9(5) COMP-3.                
00302                                                                   
062104 01  WS-BALANCE-DESCRIPTION1       PIC X(50)  VALUE
062104     'Life Claims Paid'.
00302                                                                   
062104 01  WS-BALANCE-DESCRIPTION2       PIC X(50)  VALUE
062104     'A&H Claims Paid'.
062104                                                                  
062104 01  WS-BALANCE-DESCRIPTION3       PIC X(50)  VALUE
062104     'Output Certificate Master Records Prior Month'.
062104                                                                  
062104 01  WS-BAL50-DESCRIPTION          PIC X(50)  VALUE
062104     'ECS010 Out Recs should match EL331 Offline Certs  '.
062104                                                                  
062104 01  WS-ME-BALANCE-REC.
062104     12  WS-ME-BAL-JOB           PIC X(11)  VALUE SPACES.
062104     12  WS-ME-BAL-DELIM1        PIC X(01)  VALUE ';'.
062104     12  WS-ME-BAL-STEP          PIC X(08)  VALUE 'ECS010  '.
062104     12  WS-ME-BAL-DELIM2        PIC X(01)  VALUE ';'.
062104     12  WS-ME-BAL-AMT           PIC ZZZ,ZZZ,ZZ9.
062104     12  WS-ME-BAL-DELIM3        PIC X(01)  VALUE ';'.
062104     12  WS-ME-BAL-DESCRIP       PIC X(50)  VALUE SPACES.

062104 01  WS-ME50-BALANCE-REC.
062104     12  WS-ME50-BAL-JOB           PIC X(11)  VALUE SPACES.
062104     12  WS-ME50-BAL-DELIM1        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-STEP          PIC X(08)  VALUE 'ECS010  '.
062104     12  WS-ME50-BAL-DELIM2        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-AMT-LOW       PIC ZZZ,ZZZ,ZZ9.
062104     12  WS-ME50-BAL-DELIM3        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-AMT-HIGH      PIC ZZZ,ZZZ,ZZ9.
062104     12  WS-ME50-BAL-DELIM4        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-DESCRIP       PIC X(50)  VALUE SPACES.

       01  WS-PREV-BANK-COMM-KEY       PIC X(21)  VALUE LOW-VALUES.
       01  WS-BANK-INIT                PIC X(260) VALUE SPACES.
       01  WS-BANK-COMMISSIONS.
           12  WS-BANK-COMMS           OCCURS 10.
               16  WS-BNK-AGT          PIC X(10).
               16  WS-BNK-TYPE         PIC X.
               16  WS-BNK-SPP-FEES     PIC S9(5)V99  COMP-3.
               16  WS-BNK-RECALC       PIC X.
               16  FILLER              PIC X(10).           

00303  01  PENDING-MATCH-CONTROL.                                       
00304      12  PENDING-ACCOUNT-CONTROL.                                 
00305          16  PEND-MATCH-CARRIER    PIC X.                         
00306          16  PEND-MATCH-GROUPING   PIC X(6).                      
00307          16  PEND-MATCH-STATE      PIC XX.                        
00308          16  PEND-MATCH-ACCOUNT    PIC X(10).                     
00309      12  PEND-MATCH-EFF-DTE        PIC 9(11) COMP-3.              
00310      12  PEND-MATCH-CERT-NO.                                      
00311          16  PEND-MATCH-CERT       PIC X(10).                     
00312          16  PEND-MATCH-CERT-SFX   PIC X.                         
00313  01  PENDING-RECORD-TYPE           PIC X.                         
00314      88  ISSUE-TRANSACTION            VALUE '1'.                  
00315      88  CANCEL-TRANSACTION           VALUE '2'.                  
00316      88  CLAIM-TRANSACTION            VALUE '3'.                  
00317      88  RESERVE-TRANSACTION          VALUE '4'.                  
00318      88  UPDATE-TRANSACTION           VALUE '5'.                  
00319                                                                   
00320  01  CLM-INCUR-REIN-DATE.                                         
00321      12  CIRD-CCYY.                                               
00322          16  CIRD-CC         PIC 99.                              
00323          16  CIRD-YR         PIC 99.                              
00324      12  CIRD-MO             PIC 99.                              
00325                                                                   
00326  01  HIGH-CERT               PIC 9(11)           VALUE ZEROS.     
00327                                                                   
00328  01  LOW-CERT                PIC 9(11)           VALUE ZEROS.     
00329                                                                   
00330  01  REIN-BILLING-TABLE.                                          
00331      12  REIN-CO-ENTRIES OCCURS 50 TIMES.                         
00332          16  RB-COMPANY      PIC X(6).                            
00333          16  RB-COUNT        PIC S9(5)   COMP-3.                  
00334                                                                   
00335  01  B-SUB                   PIC S9(4)   COMP.                    
00336                                                                   
00337  01  BINARY-COUNTERS     COMP      SYNC.                          
00338      12  IA                  PIC S999            VALUE +0.        
00339      12  IB                  PIC S999            VALUE +0.        
00340      12  SUB                 PIC S999            VALUE +0.        
00341      12  SUBM                PIC S999            VALUE +0.        
00342      12  SUB3                PIC S999            VALUE +0.        
00343      12  SUB4                PIC S999            VALUE +0.        
00344      12  SUB5                PIC S999            VALUE +0.        
00345      12  SUB6                PIC S999            VALUE +0.        
00346      12  SUB7                PIC S999            VALUE +0.        
00347      12  SUB8                PIC S999            VALUE +0.        
00348      12  SUB9                PIC S999            VALUE +0.        
00349      12  SUB10               PIC S999            VALUE +0.        
00350      12  SUB11               PIC S999            VALUE +0.        
00351      12  SUB12               PIC S999            VALUE +0.        
00352      12  SUB13               PIC S999            VALUE +0.        
00353      12  SUB14               PIC S999            VALUE +0.        
00354      12  SUB15               PIC S999            VALUE +0.        
00355      12  SUB16               PIC S999            VALUE +0.        
00356      12  LEVELS-BUILT        PIC S999            VALUE +0.        
00357  01  WS-SEQ-NBR              PIC 9               VALUE ZERO.      
00358                                                                   
00359  01  WS.                                                          
00360      12  WS-RETURN-CODE        PIC S9(4)   COMP   VALUE +0.       
00361      12  WS-ABEND-MESSAGE      PIC X(80)          VALUE SPACES.   
00362      12  WS-ABEND-FILE-STATUS  PIC XX             VALUE ZEROS.    
00363      12  WS-ZERO               PIC S9      COMP-3 VALUE +0.       
00364                                                                   
00365      12  WS-ABEND-CODE         PIC 9(4).                          
00366      12  ABEND-CODE  REDEFINES  WS-ABEND-CODE.                    
00367          16  ABEND-CODE-1    PIC XX.                              
00368          16  ABEND-CODE-2.                                        
00369              20  AC2-ONE     PIC X.                               
00370              20  AC2-TWO     PIC X.                               
00371                                                                   
00372      12  CTBL-SEARCH.                                             
00373          16  CTBL-COMP-CD    PIC X.                               
00374          16  CTBL-TABLE-CD   PIC XXX.                             
00375          16  CTBL-BEN-TYPE   PIC X.                               
00376          16  CTBL-BEN-CODE   PIC XX.                              
00377      12  LAST-CTBL           PIC X(7)            VALUE SPACES.    
00378      12  COMM-OPEN-SW        PIC X               VALUE SPACES.    
00379      12  WS-COMM-TERM        PIC S999            COMP-3.          
00380      12  CTBL-ERROR-SWITCH   PIC X               VALUE SPACE.     
00381          88  CTBL-ERROR         VALUE '*'.                        
00382                                                                   
00383      12  REIN-SPEC.                                               
00384          16  REIN-SPEC-A     PIC X.                               
00385          16  REIN-SPEC-B     PIC X.                               
00386          16  REIN-SPEC-C     PIC X.                               
00387                                                                   
00388      12  WS-CHECK-NUMBER.                                         
00389          16  WS-CHECK-NO     PIC X(4).                            
00390          16  FILLER          PIC XXX.                             
00391                                                                   
00392      12  AH-EARN-METHOD      PIC X               VALUE SPACES.    
00393          88  AH-EARN-ANTICIPATION                VALUE 'A' 'C'.   
00394          88  AH-EARN-NET                         VALUE 'N'.       
00395                                                                   
00396  01  ERACCTT-FILE-STATUS     PIC XX VALUE ZEROS.                  
00397  01  ERCTBL-FILE-STATUS      PIC XX VALUE ZEROS.                  
00398  01  ERMEBL-FILE-STATUS      PIC XX VALUE ZEROS.                  
00399  01  ERRTBL-FILE-STATUS      PIC XX VALUE ZEROS.                  
00400  01  ERRESC-FILE-STATUS      PIC XX VALUE ZEROS.                  
111204 01  ERAGTC-FILE-STATUS      PIC XX VALUE ZEROS.
111204 01  ELCRTT-FILE-STATUS      PIC XX VALUE ZEROS.
00401                                                                   
00402  01  MONTHS-DIFF-LF          PIC S9(05) VALUE +0.                 
00403  01  MONTHS-DIFF-AH          PIC S9(05) VALUE +0.                 
00404  01  WS-CR-BIN-DATE          PIC XX VALUE LOW-VALUES.             
00405  01  WS-BIN-CANCEL-DATE      PIC XX VALUE LOW-VALUES.             
00406                                                                   
00407  01  WS-LEVELS-CHARGEBACK-SWITCHES.                               
00408      12  WS-LF-CHARGEBACK-LEVELS.                                 
00409          16  WS-LF-CHARGEBACK-SW   PIC X(01) OCCURS 10 TIMES.     
00410      12  WS-AH-CHARGEBACK-LEVELS.                                 
00411          16  WS-AH-CHARGEBACK-SW   PIC X(01) OCCURS 10 TIMES.     
00412 ***************************************************************** 
00413 *        WORKING STORAGE AREA FOR CLIENT-DMD               START* 
00414 ***************************************************************** 
00415 *RESIDENT STATE TAXES MASTER(ERRESC)                              
00416 *                                                                 
00417  01  WORK-AREAS.                                                  
00418      12  WS-ERRESC-KEY.                                           
00419          16 WS-ERRESC-SEARCH-KEY.                                 
00420              20  WS-ERRESC-COMPANY-CD PIC  X.                     
00421              20  WS-ERRESC-CARRIER    PIC  X.                     
00422              20  WS-ERRESC-GROUP      PIC X(6).                   
00423              20  WS-ERRESC-STATE      PIC XX.                     
00424              20  WS-ERRESC-ACCOUNT    PIC X(10).                  
00425          16 WS-ERRESC-RESIDUAL-KEY.                               
00426              20  WS-ERRESC-AGENT      PIC X(10).                  
00427              20  WS-ERRESC-RES-STATE  PIC XX.                     
00428              20  WS-ERRESC-EXPIRE-DT  PIC 9(8) COMP-3.            
00429 *                                          YYYYMMDD               
00430      12  WS-YYYYMMDD-DT               PIC 9(8).                   
00431      12  WS-COMMISSION                PIC SV9(5) COMP-3.          
00432      12  WS-SUB                       PIC    99  COMP-3.          
00433      12  WS-SUB1                      PIC    99  COMP-3.          
00434 ***************************************************************** 
00435 *        WORKING STORAGE AREA FOR CLIENT-DMD                 END* 
00436 ***************************************************************** 
00437                                                                   
00438  01  COMM-WORK-AREAS.                                             
00439      12  FILLER              PIC  X(17)                           
00440          VALUE 'COMMON WORK AREAS'.                               
00441      12  ACCT-COMM-LF        PIC SV9(5)  COMP-3.                  
00442      12  ACCT-COMM-AH        PIC SV9(5)  COMP-3.                  
00443      12  W-AH-ACTIVE         PIC S9(13)  COMP-3 VALUE ZEROS.      
00444      12  W-AH-CALC-B         PIC S9(13)  COMP-3 VALUE ZEROS.      
00445      12  W-AH-CLAIMS         PIC S9(13)  COMP-3 VALUE ZEROS.      
00446      12  W-AH-DATA-USED      PIC S9(13)  COMP-3 VALUE ZEROS.      
00447      12  W-AH-INACTIVE       PIC S9(13)  COMP-3 VALUE ZEROS.      
00448      12  W-AH-ISSUES         PIC S9(13)  COMP-3 VALUE ZEROS.      
00449      12  W-AH-REIN           PIC S9(13)  COMP-3 VALUE ZEROS.      
00450      12  W-AH-REISSUE        PIC S9(13)  COMP-3 VALUE ZEROS.      
00451      12  W-AH-REMTERM-0      PIC S9(13)  COMP-3 VALUE ZEROS.      
00452      12  W-AH-RESERVES       PIC S9(13)  COMP-3 VALUE ZEROS.      
00453      12  W-AH-RESV           PIC S9(13)  COMP-3 VALUE ZEROS.      
00454      12  W-AHTYP-99          PIC S9(13)  COMP-3 VALUE ZEROS.      
00455      12  W-BAL-REMTRM        PIC S9(05)V99  COMP-3.               
00456      12  W-CERT-COUNT        PIC S9(08)  COMP.                    
00457      12  W-COUNT             PIC S9(08)  COMP   VALUE +0.         
00458      12  W-CLMAMT            PIC S9(07)V9(02)   VALUE ZEROS.      
00459      12  W-90-DAY-MAX        PIC S9(07)V9(02)   VALUE ZEROS.      
00460                                                                   
00461      12  W-EARNED-PREMIUM    PIC S9(07)V9(02)                     
00462                                          COMP-3 VALUE ZEROS.      
00463      12  W-EARNED-PREMIUM-1  PIC S9(07)V9(02)                     
00464                                          COMP-3 VALUE ZEROS.      
00465      12  W-EARNED-PREMIUM-2  PIC S9(07)V9(02)                     
00466                                          COMP-3 VALUE ZEROS.      
00467      12  W-EDIT-FIELD        PIC ----------9.                     
00468      12  W-EDIT-FIELD2       PIC ----------9.99.                  
00469      12  W-EDIT-7            PIC Z(06)9.                          
00470      12  W-FACTOR            PIC S9(09)V9(08)                     
00471                                          COMP-3 VALUE ZEROS.      
00472      12  W-IBNR-1            PIC S9(07)V9(02)                     
00473                                          COMP-3 VALUE ZEROS.      
00474      12  W-IBNR-2            PIC S9(07)V9(02)                     
00475                                          COMP-3 VALUE ZEROS.      
00476      12  W-ISS-BEN           PIC S9(11)V9(02)                     
00477                                          COMP-3 VALUE ZEROS.      
00478      12  W-ISS-BEN-GROSS     PIC S9(11)V9(02)                     
00479                                          COMP-3 VALUE ZEROS.      
00480      12  W-LF-ACTIVE         PIC S9(13)  COMP-3 VALUE ZEROS.      
00481      12  W-LF-BALLOON        PIC S9(13)  COMP-3 VALUE ZEROS.      
00482      12  W-LF-CLAIMS         PIC S9(13)  COMP-3 VALUE ZEROS.      
00483      12  W-LF-CALC-B         PIC S9(13)  COMP-3 VALUE ZEROS.      
00484      12  W-LF-DATA-USED      PIC S9(13)  COMP-3 VALUE ZEROS.      
00485      12  W-LF-INACTIVE       PIC S9(13)  COMP-3 VALUE ZEROS.      
00486      12  W-LF-ISSUES         PIC S9(13)  COMP-3 VALUE ZEROS.      
00487      12  W-LF-REMAMT-0       PIC S9(13)  COMP-3 VALUE ZEROS.      
00488      12  W-LF-REMTERM-0      PIC S9(13)  COMP-3 VALUE ZEROS.      
00489      12  W-LF-REIN           PIC S9(13)  COMP-3 VALUE ZEROS.      
00490      12  W-LF-REISSUE        PIC S9(13)  COMP-3 VALUE ZEROS.      
00491      12  W-LF-RESERVES       PIC S9(13)  COMP-3 VALUE ZEROS.      
00492      12  W-LF-RESV           PIC S9(13)  COMP-3 VALUE ZEROS.      
00493      12  W-LFTYP-99          PIC S9(13)  COMP-3 VALUE ZEROS.      
00494      12  W-LIFE-YEARS        PIC S9(09)  COMP-3 VALUE ZEROS.      
00495      12  W-LIFE-YEARS-INT    PIC S9(09)V9(06)                     
00496                                          COMP-3 VALUE ZEROS.      
00497      12  W-NDX               PIC S9(03)  COMP-3 VALUE ZEROS.      
00498      12  W-ORGTRM-0-AH       PIC S9(13)  COMP-3 VALUE ZEROS.      
00499      12  W-ORGTRM-0-LF       PIC S9(13)  COMP-3 VALUE ZEROS.      
00500      12  W-RET-CODE-AH       PIC S9(13)  COMP-3 VALUE ZEROS.      
00501      12  W-RET-CODE-LF       PIC S9(13)  COMP-3 VALUE ZEROS.      
00502      12  W-PENDING-CLMS      PIC S9(13)  COMP-3 VALUE ZEROS.      
00503      12  W-POL-ISSUES        PIC S9(13)  COMP-3 VALUE ZEROS.      
00504      12  W-POLICY-ACTIVE-CTR PIC S9(13)  COMP-3 VALUE ZEROS.      
00505      12  W-POLICY-INACTIVE-CTR                                    
00506                              PIC S9(13)  COMP-3 VALUE ZEROS.      
00507      12  W-POLICY-REISSUE-CTR                                     
00508                              PIC S9(13)  COMP-3 VALUE ZEROS.      
00509      12  W-POLICY-REIN-CTR   PIC S9(13)  COMP-3 VALUE ZEROS.      
00510      12  W-POLICY-INACTIVE   PIC S9(13)  COMP-3 VALUE ZEROS.      
00511      12  W-TARGET-LOSS-RATIO PIC S9(01)V9(04)                     
00512                                          COMP-3 VALUE ZEROS.      
00513      12  W-VAL-SOURCE        PIC S9(03)  COMP-3 VALUE ZEROS.      
00514      12  W-WK-IBNR           PIC S9(07)V9(02)                     
00515                                          COMP-3 VALUE ZEROS.      
00516                                                                   
00517      12  W-CLAIM-TYPE        PIC  X(01).                          
00518          88  W-A-H                              VALUE 'A'.        
00519          88  W-LIFE                             VALUE 'L'.        
00520      12  W-CUTOFF-DATE       PIC  9(08).                          
00521      12  W-DIS-DT            PIC  XX.                             
00522      12  W-DIS-PTO-DT        PIC  XX.                             
00523      12  WK-LEVEL.                                                
00524          16  WK-AGT          PIC X(10).                           
00525          16  WK-A-TYPE       PIC X.                               
00526          16  WK-L-RT         PIC SV9(5)  COMP-3.                  
00527          16  WK-J-RT         PIC SV9(5)  COMP-3.                  
00528          16  WK-A-RT         PIC SV9(5)  COMP-3.                  
00529          16  FILLER          PIC X(6).                            
00530      12  WK-LEVEL-R REDEFINES WK-LEVEL.                           
00531          16  FILLER          PIC X(11).                           
00532          16  WK-L-RTX        PIC X(3).                            
00533          16  WK-J-RTX        PIC X(3).                            
00534          16  WK-A-RTX        PIC X(3).                            
00535          16  FILLER          PIC X(6).                            
00536      12  WK-NEW-RATES.                                            
00537          16  WK-N-R      OCCURS 10.                               
00538              20  WK-N-AGT    PIC X(10).                           
00539              20  WK-N-TYPE   PIC X.                               
00540              20  WK-L-RT1    PIC SV9(5)  COMP-3.                  
00541              20  WK-A-RT1    PIC SV9(5)  COMP-3.                  
00542      12  W-LIFE-YEARS-ZERO.                                       
00543          16  FILLER          PIC  X(06)         VALUE ZEROS.      
00544          16  FILLER          PIC  X(01)         VALUE SPACES.     
00545          16  FILLER          PIC  X(02)         VALUE ZEROS.      
00546          16  FILLER          PIC  X(01)         VALUE SPACES.     
00547          16  FILLER          PIC  X(01)         VALUE SPACES.     
00548          16  FILLER          PIC  X(01)         VALUE SPACES.     
00549          16  FILLER          PIC S9(09)  COMP-3 VALUE ZEROS.      
00550          16  FILLER          PIC S9(09)  COMP-3 VALUE ZEROS.      
00551          16  FILLER          PIC S9(09)  COMP-3 VALUE ZEROS.      
00552          16  FILLER          PIC S9(09)  COMP-3 VALUE ZEROS.      
00553          16  FILLER          PIC S9(09)  COMP-3 VALUE ZEROS.      
00554          16  FILLER          PIC S9(09)  COMP-3 VALUE ZEROS.      
00555          16  FILLER          PIC S9(09)  COMP-3 VALUE ZEROS.      
00556          16  FILLER          PIC S9(09)  COMP-3 VALUE ZEROS.      
00557          16  FILLER          PIC S9(09)  COMP-3 VALUE ZEROS.      
00558          16  FILLER          PIC S9(09)  COMP-3 VALUE ZEROS.      
00559          16  FILLER          PIC S9(09)  COMP-3 VALUE ZEROS.      
00560          16  FILLER          PIC S9(09)  COMP-3 VALUE ZEROS.      
00561          16  FILLER          PIC S9(09)  COMP-3 VALUE ZEROS.      
00562          16  FILLER          PIC S9(09)V9(02) COMP-3              
00563                                                 VALUE ZEROS.      
00564          16  FILLER          PIC S9(09)V9(02) COMP-3              
00565                                                 VALUE ZEROS.      
00566          16  FILLER          PIC S9(09)V9(02) COMP-3              
00567                                                 VALUE ZEROS.      
00568          16  FILLER          PIC S9(09)V9(02) COMP-3              
00569                                                 VALUE ZEROS.      
00570          16  FILLER          PIC S9(11)V9(02) COMP-3              
00571                                                 VALUE ZEROS.      
00572                                                                   
00573      12  W-REIN-PROCESS-IND  PIC  X(01)         VALUE SPACES.     
00574          88 W-REIN-IN-PROCESS                   VALUE 'Y'.        
00575      12  W-REIN-PROCESS-SW   PIC  X(01)         VALUE SPACES.     
00576          88  W-REIN-PROCESS                     VALUE 'Y'.        
00577          88  W-NOT-REIN-PROCESS                 VALUE SPACES.     
00578      12  W-SELECTION-SW      PIC  X(01)         VALUE SPACES.     
00579          88  W-SELECTION-ON                     VALUE 'Y'.        
00580          88  W-SELECTION-OFF                    VALUE SPACES.     
00581      12  W-RTRM-0-SW         PIC  X(01)         VALUE SPACES.     
00582          88  W-RTRM-0                           VALUE 'Y'.        
00583          88  W-RTRM-NOT-0                       VALUE SPACES.     
00584      12  W-SUB-KEY           PIC  X(03)         VALUE SPACES.     
00585      12  W-TABLE-USED-SW     PIC  X(01)         VALUE SPACES.     
00586          88  W-TABLE-USED                       VALUE 'Y'.        
00587          88  W-TABLE-NOT-USED                   VALUE SPACES.     
00588      12  W-UNDERWRITING-SW   PIC  X             VALUE SPACES.     
00589          88  W-UNDERWRITTEN-DATA                VALUE '*'.        
00590                                                                   
00591      12  W-VAL-ERRORS.                                            
00592          16  FILLER          PIC  X(40)                           
00593              VALUE 'PENDING CANC PROCESS - CR-LF-CANC-DT - '.     
00594          16  FILLER          PIC  X(40)                           
00595              VALUE 'EPEC PROCESS - LIFE - RUN DATE - '.           
00596          16  FILLER          PIC  X(40)                           
00597              VALUE 'EPEC CANCEL - LIFE - CR-LF-CANC-DT - '.       
00598          16  FILLER          PIC  X(40)                           
00599              VALUE 'EPEC CANCEL LIFE SUM - CR-LF-CANC-DT - '.     
00600          16  FILLER          PIC  X(40)                           
00601              VALUE 'EOM RUN DATE - EPEC - COMM - '.               
00602          16  FILLER          PIC  X(40)                           
00603              VALUE 'RUN DATE EPEC - A&H - '.                      
00604          16  FILLER          PIC  X(40)                           
00605              VALUE 'CUTOFF DTE (OPT-MTHD) - '.                    
00606          16  FILLER          PIC  X(40)                           
00607              VALUE 'EOM RUN DATE - A&H EARNED PREMIUM - '.        
00608          16  FILLER          PIC  X(40)                           
00609              VALUE 'EARNINGS START DATE - REIN CO RCD - '.        
00610      12  FILLER REDEFINES W-VAL-ERRORS.                           
00611          16  W-VAL-ERROR OCCURS 9 TIMES                           
00612                              PIC  X(40).                          
00613                                                                   
00614      12  W-WORK-DATE         PIC  X(02).                          
00615      12  SAVE-WK-NEW-RATES.                                       
00616          16  FILLER          PIC X(170).                          
00617                                                                   
092602***   The following data area is for NCL only
CIDMOD*    12  W-WORK-OB-DATA OCCURS 300.                               
092602*    12  W-WORK-OB-DATA OCCURS 220.                               
092602     12  W-WORK-OB-DATA OCCURS 900.                               
00619          16  WS-OB-REIN-CO       PIC X(06).                       
00620          16  WS-OB-LF-AH         PIC X.                           
00621          16  WS-OB-BENEFIT-CODE  PIC XX.                          
00622          16  WS-OB-EARNED OCCURS 3.                               
00623              20  WS-EPR-R78   PIC S9(09)V99 COMP-3.               
00624                                                                   
00625  EJECT                                                            
00626  01  EPEC-WORK-AREAS.                                             
00627      12  WE-REIN-CO          PIC X(6).                            
00628      12  WE-LF-AH            PIC X.                               
00629      12  WE-BEN-TYPE         PIC XX.                              
00630      12  WE-LFAMT            PIC S9(9)V99    COMP-3.              
00631      12  WE-LFPRM            PIC S9(7)V99    COMP-3.              
00632      12  WE-LFAMT-ALT        PIC S9(9)V99    COMP-3.              
00633      12  WE-LFPRM-ALT        PIC S9(7)V99    COMP-3.              
00634      12  WE-LFRFND           PIC S9(7)V99    COMP-3.              
00635      12  WE-DTHAMT           PIC S9(9)V99    COMP-3.              
00636      12  WE-DTHEXP           PIC S9(9)V99    COMP-3.              
00637      12  WE-NUM-DTH-CLM      PIC S999        COMP-3.              
00638      12  WE-DTH-IBNR         PIC S9(7)V99    COMP-3.              
00639      12  WE-DTH-PAYCUR       PIC S9(7)V99    COMP-3.              
00640      12  WE-DTH-FUTRSV       PIC S9(7)V99    COMP-3.              
00641      12  WE-AHAMT            PIC S9(9)V99    COMP-3.              
00642      12  WE-AHPRM            PIC S9(7)V99    COMP-3.              
00643      12  WE-AHRFND           PIC S9(7)V99    COMP-3.              
00644      12  WE-DSPY-AMT         PIC S9(9)V99    COMP-3.              
00645      12  WE-DSPY-EXP         PIC S9(9)V99    COMP-3.              
00646      12  WE-NUM-DIS-CLM      PIC S999        COMP-3.              
00647      12  WE-DIS-IBNR         PIC S9(7)V99    COMP-3.              
00648      12  WE-DIS-PAYCUR       PIC S9(7)V99    COMP-3.              
00649      12  WE-DIS-FUTRSV       PIC S9(7)V99    COMP-3.              
00650      12  ISS-BEN             PIC S9(9)V99    COMP-3.              
00651      12  CNC-BEN             PIC S9(9)V99    COMP-3.              
00652      12  ISS-PRM             PIC S9(7)V99    COMP-3.              
PEMMOD     12  ISS-PRMTAX          PIC S9(7)V99    COMP-3.              
00653      12  CNC-PRM             PIC S9(7)V99    COMP-3.              
00654      12  GROSS-ISS-BEN-LF    PIC S9(9)V99    COMP-3.              
00655      12  GROSS-CNC-BEN-LF    PIC S9(9)V99    COMP-3.              
00656      12  GROSS-ISS-PRM-LF    PIC S9(7)V99    COMP-3.              
00657      12  GROSS-CNC-PRM-LF    PIC S9(7)V99    COMP-3.              
00658      12  GROSS-ISS-BEN-AH    PIC S9(7)V99    COMP-3.              
00659      12  GROSS-CNC-BEN-AH    PIC S9(7)V99    COMP-3.              
00660      12  GROSS-ISS-PRM-AH    PIC S9(7)V99    COMP-3.              
00661      12  GROSS-CNC-PRM-AH    PIC S9(7)V99    COMP-3.              
00662      12  EARNED-BY-CANCEL    PIC S9(7)V99    COMP-3.              
00663      12  EPR-R78             PIC S9(7)V99    COMP-3.              
00664      12  EPR-PRO             PIC S9(7)V99    COMP-3.              
00665      12  EPR-ST              PIC S9(7)V99    COMP-3.              
00666      12  W-OPT-R78           PIC S9(7)V99    COMP-3.              
00667      12  W-OPT-PRO           PIC S9(7)V99    COMP-3.              
00668      12  W-OPT-ST            PIC S9(7)V99    COMP-3.              
00669      12  WX-ACTIVE-COUNT     PIC S9          COMP-3.              
00670      12  WX-CANC-DT          PIC 9(11)       COMP-3.              
00671      12  WX-CLM-AMT          PIC S9(9)V99    COMP-3.              
00672      12  WX-CLM-EXP          PIC S9(9)V99    COMP-3.              
00673      12  WX-CLM-CNT          PIC S9(7)       COMP-3.              
00674      12  WX-CLM-CRT          PIC S9(7)       COMP-3.              
00675      12  WX-CNC-CNT          PIC S9(7)       COMP-3.              
00676      12  WX-CLAIM-EXIT-DATE  PIC  9(11)      COMP-3.              
00677      12  WX-SETTLEMENT-EXIT-DATE                                  
00678                              PIC  9(11)      COMP-3.              
00679      12  WX-STATUS-AT-CANCEL PIC  X(1).                           
00680      12  WX-STATUS-AT-DEATH  PIC  X(1).                           
00681      12  WX-STATUS-AT-SETTLEMENT                                  
00682                              PIC  X(1).                           
00683      12  WX-EOM-RUN-DATE     PIC  9(11)      COMP-3.              
00684                                                                   
00685  01  EPEC-WORK.                                                   
00686      12  EW-REIN-CO          PIC X(6).                            
00687      12  EW-SUB-KEY.                                              
00688          16  EW-LF-AH        PIC X.                               
00689          16  EW-BEN-TYPE     PIC XX.                              
00690      12  EW-ISS-BEN          PIC S9(11)V99   COMP-3.              
00691      12  EW-ISS-BEN-GROSS    PIC S9(11)V99   COMP-3.              
00692      12  EW-CNC-BEN          PIC S9(11)V99   COMP-3.              
00693      12  EW-CNC-BEN-GROSS    PIC S9(11)V99   COMP-3.              
00694      12  EW-ISS-PRM          PIC S9(9)V99    COMP-3.              
00695      12  EW-ISS-PRM-GROSS    PIC S9(9)V99    COMP-3.              
00696      12  EW-CNC-PRM          PIC S9(9)V99    COMP-3.              
00697      12  EW-CNC-PRM-GROSS    PIC S9(9)V99    COMP-3.              
00698      12  EW-EPR-R78          PIC S9(9)V99    COMP-3.              
00699      12  EW-EPR-R78-ADJ      PIC S9(9)V99    COMP-3.              
00700      12  EW-EPR-PRO          PIC S9(9)V99    COMP-3.              
00701      12  EW-EPR-PRO-ADJ      PIC S9(9)V99    COMP-3.              
00702      12  EW-EPR-ST           PIC S9(9)V99    COMP-3.              
00703      12  EW-EPR-ST-ADJ       PIC S9(9)V99    COMP-3.              
00704      12  EW-CLM-AMT          PIC S9(9)V99    COMP-3.              
00705      12  EW-CLM-IBNR         PIC S9(7)V99    COMP-3.              
00706      12  EW-CLM-PAYCUR       PIC S9(7)V99    COMP-3.              
00707      12  EW-CLM-FUTRSV       PIC S9(7)V99    COMP-3.              
00708      12  EW-CLM-CNT          PIC S9(7)       COMP-3.              
00709      12  EW-CLM-CRT          PIC S9(7)       COMP-3.              
00710      12  EW-ISS-CNT          PIC S9(7)       COMP-3.              
00711      12  EW-CNC-CNT          PIC S9(7)       COMP-3.              
PEMMOD     12  EW-ISS-PRM-TAX      PIC S9(7)V99    COMP-3.
00712      12  EW-AGENTS       OCCURS 10.                               
00713          16  EW-AGT-NO       PIC X(10).                           
00714          16  EW-AGT-TYPE     PIC X.                               
00715          16  EW-ISS-COM      PIC S9(9)V99    COMP-3.              
00716          16  EW-CNC-COM      PIC S9(9)V99    COMP-3.              
00717          16  EW-ECR-R78      PIC S9(9)V99    COMP-3.              
00718          16  EW-ECR-R78-ADJ  PIC S9(9)V99    COMP-3.              
00719          16  EW-ECR-PRO      PIC S9(9)V99    COMP-3.              
00720          16  EW-ECR-PRO-ADJ  PIC S9(9)V99    COMP-3.              
00721          16  EW-ECR-ST       PIC S9(9)V99    COMP-3.              
00722      12  EW-COV-DTE          PIC 9(11)       COMP-3.              
00723      12  EW-INFORCE-CNT      PIC S9(9)       COMP-3.              
00724      12  EW-ORIG-TERM        PIC S9(9)       COMP-3.              
00725      12  EW-REM-TERM         PIC S9(9)       COMP-3.              
00726      12  EW-AGE              PIC S9(9)       COMP-3.              
00727      12  EW-AGE-BEN          PIC S9(11)      COMP-3.              
00728      12  EW-TERM-BEN         PIC S9(11)      COMP-3.              
00729      12  EW-LEVEL-USED       PIC X.                               
00730                                                                   
00731  01  EPEC-TOTALS.                                                 
092602     12  EPEC-LEVELS     OCCURS 900.                              
092602*    12  EPEC-LEVELS     OCCURS 220.                              
CIDMOD*    12  EPEC-LEVELS     OCCURS 300.                              
00733          16  EPEC-REIN-CO          PIC X(6).                      
00734          16  EPEC-SUB-KEY.                                        
00735              24  EPEC-LF-AH        PIC X.                         
00736              24  EPEC-BEN-TYPE     PIC XX.                        
PEMMOD*        16  FILLER                PIC X(694).                    
PEMMOD         16  FILLER                PIC X(699).                    
00738      12  EPEC-LEVELS-END           PIC X(6).                      
00739                                                                   
00740  01  EPEC-UNDERWRITTEN-TOTALS.                                    
092602     12  EPEC-LEVELS-II  OCCURS 900.                              
092602*    12  EPEC-LEVELS-II  OCCURS 220.                              
CIDMOD*    12  EPEC-LEVELS-II  OCCURS 300.                              
00742          16  EPEC-REIN-CO-II       PIC X(6).                      
00743          16  EPEC-SUB-KEY-II.                                     
00744              24  EPEC-LF-AH-II     PIC X.                         
00745              24  EPEC-BEN-TYPE-II  PIC XX.                        
PEMMOD*        16  FILLER                PIC X(694).                    
PEMMOD         16  FILLER                PIC X(699).                    
00747      12  EPEC-LEVELS-END-II        PIC X(6).                      
00748                                                                   
00749                                                                   
00750  01  FILLER                      PIC  X(11)                       
00751                                  VALUE 'OPT RSV TBL'.             
CIDMOD*                                                                 
CIDMOD* THE 'PIC' VALUE OF THE NEXT 2 DATA NAMES IS 300 OCCURANCES      
CIDMOD*  TIMES THE SIZE OF THE DATA NAME THAT OCCURS.                   
CIDMOD*  EX: EPEC-OPT-GRP  (300) TIMES THE TOTAL SIZE OF FIELDS IN      
CIDMOD*      EPEC-OPT-GRP  (108)                                        
CIDMOD*                                                                 
CIDMOD*      300 X 108 = 32400.                                         
CIDMOD*                                                                 
00752  01  EPEC-OPT-DATA-TABLE.                                         
CIDMOD*    12  FILLER                  PIC X(32400).                    
092602*    12  FILLER                  PIC X(23760).                    
092602     12  FILLER                  PIC X(97200).                    
00754                                                                   
00755  01  FILLER                      PIC  X(16)                       
00756                                  VALUE 'OPT RSV TBL UNDW'.        
00757  01  EPEC-UNDW-OPT-DATA-TABLE.                                    
CIDMOD*    12  FILLER                  PIC X(32400).                    
092602*    12  FILLER                  PIC X(23760).                    
092602     12  FILLER                  PIC X(97200).                    
00759                                                                   
00760  01  FILLER                      PIC  X(16)                       
00761                                  VALUE 'OPT RSV TBL WORK'.        
00762  01  EPEC-OPT-WORK-TABLE.                                         
CIDMOD*    12  EPEC-OPT-GRP        OCCURS 300.                          
092602*    12  EPEC-OPT-GRP        OCCURS 220.                          
092602     12  EPEC-OPT-GRP        OCCURS 900.                          
00764          16  EPEC-REIN-CO-OPT                                     
00765                              PIC  X(06).                          
00766          16  EPEC-SUB-KEY-OPT.                                    
00767              24  EPEC-LF-AH-OPT                                   
00768                              PIC  X(01).                          
00769              24  EPEC-BEN-TYPE-OPT                                
00770                              PIC  X(02).                          
00771          16  EPEC-OPT-DATA.                                       
00772              20  EPEC-REIN-ONLY-IND                               
00773                              PIC  X(01).                          
00774                  88  EPEC-REIN-ONLY-DATA   VALUE 'R'.             
00775              20  EPEC-LINE-NOT-USED-IND                           
00776                              PIC  X(01).                          
00777                  88  EPEC-LINE-NOT-USED    VALUE 'N'.             
00778              20  EPEC-LIFE-CTR-USED-IND                           
00779                              PIC  X(01).                          
00780                  88  EPEC-LIFE-CTR-USED    VALUE 'Y'.             
00781                  88  EPEC-LIFE-CTR-NOT-USED                       
00782                                            VALUE ' '.             
00783              20  EPEC-LIFE-CTR   OCCURS 13 TIMES                  
00784                              PIC S9(09)       COMP-3.             
00785              20  EPEC-EARNED-PREMIUM-AH                           
00786                              PIC S9(09)V9(02) COMP-3.             
00787              20  EPEC-CLAIMS-MADE-PAID                            
00788                              PIC S9(09)V9(02) COMP-3.             
00789              20  EPEC-CLM-EXP                                     
00790                              PIC S9(09)V9(02) COMP-3.             
00791              20  EPEC-LIFE-IBNR                                   
00792                              PIC S9(09)V9(02) COMP-3.             
00793              20  EPEC-REIN-ISS-BEN                                
00794                              PIC S9(11)V9(02) COMP-3.             
00795                                                                   
00796  01  FILLER                  PIC X(09)                            
00797                                  VALUE 'EOM TABLE'.               
00798                                                                   
00799  01  EOM-DATE-TABLE.                                              
00800      12  EOM-DATE            OCCURS 13                            
00801                              PIC 9(08).                           
00802                                                                   
00803  01  EPEC-SUBS.                                                   
00804      12  EPEC-SUB-LEVELS OCCURS 31.                               
00805          16  EPEC-SUB-REIN   PIC X(6).                            
092602         16  EPEC-SUB-LF     PIC S999  COMP  OCCURS 900.          
092602         16  EPEC-SUB-AH     PIC S999  COMP  OCCURS 900.          
00808      12  EPEC-SUBS-END       PIC X(6).                            
00809                                                                   
00810  01  SAVE-REIN-WORK.                                              
00811      12  REIN-SUB-1          PIC S9(4)   COMP    VALUE +0.        
00812      12  REIN-SUB-2          PIC S9(4)   COMP    VALUE +0.        
00813      12  REIN-MAX            PIC S9(4)   COMP    VALUE +32.       
00814      12  REIN-LOW-TIME       PIC 9(6)            VALUE 999999.    
00815      12  REIN-LOW-SUB        PIC S9(4)   COMP    VALUE +1.        
00816                                                                   
00817  01  SAVE-REIN-TABLE.                                             
00818      12  SAVE-REIN-ENTRY     OCCURS 32 TIMES.                     
00819          16  SR-TIME         PIC 9(6).                            
00820          16  SR-ENTRY.                                            
00821              20  FILLER      PIC XX.                              
00822              20  SR-CONTROL  PIC X(8).                            
00823              20  FILLER      PIC X(3990).                         
00824                                                                   
00825                                                                   
00826                              COPY ECSRITAB.                       
00827                                                                   
00828                              COPY ELCPSEVR.                       
00829                                                                   
00830                              COPY ELCEPCVR.                       
00831                                                                   
00832                              COPY ELCREINV.                       
00833                                                                   
00834  01  RC-REIN-HOLD-AREAS.                                          
00835      12  RC-REIN-LEVELS  OCCURS 30.                               
00836          16  RC-REIN-COMP    PIC X(6).                            
00837          16  RC-OLD-ISS      PIC X.                               
00838          16  RC-NEW-ISS      PIC X.                               
00839          16  RC-OLD-CNC      PIC X.                               
00840          16  RC-NEW-CNC      PIC X.                               
00841          16  RC-OLD-LF-CLM   PIC X.                               
00842          16  RC-OLD-AH-CLM   PIC X.                               
00843          16  RC-NEW-LF-CLM   PIC X.                               
00844          16  RC-NEW-AH-CLM   PIC X.                               
00845          16  RC-REIN-LFAMT   PIC S9(9)V99    COMP-3.              
00846          16  RC-REIN-LFPRM   PIC S9(7)V99    COMP-3.              
00847          16  RC-REIN-AHAMT   PIC S9(7)V99    COMP-3.              
00848          16  RC-REIN-AHPRM   PIC S9(7)V99    COMP-3.              
00849          16  RC-REIN-LFRFND  PIC S9(7)V99    COMP-3.              
00850          16  RC-REIN-AHRFND  PIC S9(7)V99    COMP-3.              
00851          16  RC-REIN-LFCLM   PIC S9(9)V99    COMP-3.              
00852          16  RC-REIN-AHCLM   PIC S9(9)V99    COMP-3.              
00853          16  RC-REIN-REM-SW  PIC X.                               
00854      12  RC-REIN-LEVELS-END  PIC X(6).                            
00855                                                                   
00856  01  TOTAL-PAGES-COUNTERS    COMP-3.                              
00857      12  TPC-COUNTERS    OCCURS 6.                                
00858          16  TPL-ISS-PRM     PIC S9(9)V99.                        
00859          16  TPA-ISS-PRM     PIC S9(9)V99.                        
00860          16  TPL-CNC-PRM     PIC S9(9)V99.                        
00861          16  TPA-CNC-PRM     PIC S9(9)V99.                        
00862          16  TPL-ACCT-COM    PIC S9(9)V99.                        
00863          16  TPA-ACCT-COM    PIC S9(9)V99.                        
00864          16  TPL-CLAIMS      PIC S9(9)V99.                        
00865          16  TPA-CLAIMS      PIC S9(9)V99.                        
00866                                                                   
00867  01  HLD-DTS SYNC.                                                
00868      12  EOM-RUN-DATE        PIC 9(08).                           
00869      12  EOM-RUN-DATE-R REDEFINES EOM-RUN-DATE.                   
00870          16  EOM-CCYY.                                            
00871              20  EOM-CC      PIC 99.                              
00872              20  EOM-YR      PIC 99.                              
00873          16  EOM-MO          PIC 99.                              
00874          16  EOM-DA          PIC 99.                              
00875                                                                   
00876 ****   1ST OCCURRENCE IS MONTHEND DATE MINUS ONE MONTH            
00877 ****   2ND OCCURRENCE IS MONTHEND DATE MINUS TWO MONTHS           
00878      12  OB-RUN-DATES OCCURS 2.                                   
00879          16  OB-CCYY         PIC 9(4).                            
00880          16  OB-MO           PIC 99.                              
00881          16  OB-DA           PIC 99.                              
00882                                                                   
00883      12  OB-DATE-1-SWITCH    PIC X           VALUE SPACE.         
00884          88  OB-DATE-1-USED     VALUE '*'.                        
00885      12  OB-DATE-2-SWITCH    PIC X           VALUE SPACE.         
00886          88  OB-DATE-2-USED     VALUE '*'.                        
00887      12  OB-DATE-3-SWITCH    PIC X           VALUE SPACE.         
00888          88  OB-DATE-3-USED     VALUE '*'.                        
00889      12  EARNING-START-DATE  PIC 9(08).                           
00890      12  EARNING-START-DATE-R REDEFINES  EARNING-START-DATE.      
00891          16  E-ERN-START-CC  PIC 99.                              
00892          16  E-ERN-START-YR  PIC 99.                              
00893          16  E-ERN-START-MO  PIC 99.                              
00894          16  E-ERN-START-DA  PIC 99.                              
00895      12  EARNING-STOP-DATE   PIC 9(08).                           
00896      12  EARNING-STOP-DATE-R REDEFINES  EARNING-STOP-DATE.        
00897          16  E-ERN-STOP-CC   PIC 99.                              
00898          16  E-ERN-STOP-YR   PIC 99.                              
00899          16  E-ERN-STOP-MO   PIC 99.                              
00900          16  E-ERN-STOP-DA   PIC 99.                              
00901      12  STOP-LF-DATE        PIC 9(08).                           
00902      12  STOP-LF-DATE-R REDEFINES STOP-LF-DATE.                   
00903          16  STOP-LF-CC      PIC 99.                              
00904          16  STOP-LF-YR      PIC 99.                              
00905          16  STOP-LF-MO      PIC 99.                              
00906          16  STOP-LF-DA      PIC 99.                              
00907      12  STOP-AH-DATE        PIC 9(08).                           
00908      12  STOP-AH-DATE-R REDEFINES STOP-AH-DATE.                   
00909          16  STOP-AH-CC      PIC 99.                              
00910          16  STOP-AH-YR      PIC 99.                              
00911          16  STOP-AH-MO      PIC 99.                              
00912          16  STOP-AH-DA      PIC 99.                              
00913      12  BIN-VALUATION-DT    PIC XX.                              
00914      12  BIN-EOM-RUN-DATE    PIC XX.                              
00915      12  LAST-YRS-DATE       PIC 9(08).                           
00916      12  LAST-YRS-DATE-R  REDEFINES  LAST-YRS-DATE.               
00917          16  LYD-CCYY        PIC 9(04).                           
00918          16  LYD-CCYR REDEFINES LYD-CCYY.                         
00919              20  LYD-CC      PIC 99.                              
00920              20  LYD-YR      PIC 99.                              
00921          16  LYD-MO          PIC 99.                              
00922          16  LYD-DA          PIC 99.                              
00923      12  VALUATION-DT        PIC 9(08)  VALUE 0.                  
00924      12  VALUATION-DT-R REDEFINES VALUATION-DT.                   
00925          16  VAL-CC          PIC 99.                              
00926          16  VAL-YR          PIC 99.                              
00927          16  VAL-MO          PIC 99.                              
00928          16  VAL-DA          PIC 99.                              
00929      12  INCURRED-DATE       PIC 9(08).                           
00930      12  INCURRED-DATE-R REDEFINES INCURRED-DATE.                 
00931          16  DTO-CC          PIC 99.                              
00932          16  DTO-YR          PIC 99.                              
00933          16  DTO-MO          PIC 99.                              
00934          16  DTO-DA          PIC 99.                              
00935      12  REPORTED-DATE.                                           
00936          16  RPT-YR          PIC 99.                              
00937          16  RPT-MO          PIC 99.                              
00938          16  RPT-DA          PIC 99.                              
00939      12  PAYMENT-DATE        PIC 9(08).                           
00940      12  PAYMENT-DATE-R REDEFINES PAYMENT-DATE.                   
00941          16  PAY-CC          PIC 99.                              
00942          16  PAY-YR          PIC 99.                              
00943          16  PAY-MO          PIC 99.                              
00944          16  PAY-DA          PIC 99.                              
00945      12  PAY-TO-DATE         PIC 9(8).                            
00946      12  PAY-TO-DATE-R  REDEFINES PAY-TO-DATE.                    
00947          16  PAYTO-CC        PIC 99.                              
00948          16  PAYTO-YR        PIC 99.                              
00949          16  PAYTO-MO        PIC 99.                              
00950          16  PAYTO-DA        PIC 99.                              
00951                                                                   
00952  01  AH-RES-STATE-TABLE.                                          
00953      12  FILLER              PIC X(42)       VALUE                
00954              'AKALAZCADEGAIDILKSKYLAMDMIMTNVOHORSCSDWIWY'.        
00955      12  FILLER              PIC XX          VALUE HIGH-VALUES.   
00956                                                                   
00957  01  AH-RES-STATE-TAB REDEFINES AH-RES-STATE-TABLE.               
00958      12  AH-RES-STATE-ENT    PIC XX      OCCURS 22 TIMES.         
00959                                                                   
00960  01  BILLING-COUNTERS    COMP-3.                                  
00961      12  BC-NET-WRIT         PIC S9(7)V99        VALUE +0.        
00962      12  BC-NET-WRIT-SP      PIC S9(7)V99        VALUE +0.        
00963      12  BC-NET-WRIT-OB      PIC S9(7)V99        VALUE +0.        
00964      12  BC-G-ISS            PIC S9(5)           VALUE +0.        
00965      12  BC-G-CNC            PIC S9(5)           VALUE +0.        
00966      12  BC-G-CLM            PIC S9(5)           VALUE +0.        
00967      12  BC-G-CRS            PIC S9(5)           VALUE +0.        
00968      12  BC-G-CTN            PIC S9(5)           VALUE +0.        
00969      12  BC-R-ISS            PIC S9(5)           VALUE +0.        
00970      12  BC-R-CNC            PIC S9(5)           VALUE +0.        
00971      12  BC-R-CLM            PIC S9(5)           VALUE +0.        
00972      12  BC-R-CRS            PIC S9(5)           VALUE +0.        
00973      12  BC-R-CTN            PIC S9(5)           VALUE +0.        
00974      12  BC-COMM             PIC S9(5)           VALUE +0.        
00975      12  BC-NEW-ACCTS        PIC S9(5)           VALUE +0.        
00976      12  BC-ACCTS            PIC S9(5)           VALUE +0.        
00977      12  BC-ACT-ACCTS        PIC S9(5)           VALUE +0.        
00978      12  BC-G-CERTS          PIC S9(7)           VALUE +0.        
00979      12  BC-G-A-CERTS        PIC S9(7)           VALUE +0.        
00980      12  BC-R-CERTS          PIC S9(7)           VALUE +0.        
00981      12  BC-R-A-CERTS        PIC S9(7)           VALUE +0.        
00982      12  BC-ER-ACCTS         PIC S9(5)           VALUE +0.        
00983      12  BC-CS-ACCTS         PIC S9(5)           VALUE +0.        
00984      12  ACTIVE-COUNT        PIC S9              VALUE +0.        
00985                                                                   
00986  01  FLAGS SYNC.                                                  
00987      12  BILLED-SWITCH           PIC X           VALUE 'N'.       
00988          88  CHECK-BILLED                        VALUE 'Y'.       
00989      12  TRANS-EOF-FLAG          PIC X           VALUE SPACE.     
00990          88  EOF-ON-TRANS                        VALUE '*'.       
00991      12  CERT-EOF-FLAG           PIC X           VALUE SPACE.     
00992          88  EOF-ON-CERT                         VALUE '*'.       
00993      12  PROCESSING-FLAG         PIC X           VALUE SPACE.     
00994          88  PROCESSING-COMPLETED                VALUE '*'.       
00995      12  DEATH-VOID-FLAG         PIC X           VALUE SPACE.     
00996          88  DEATH-CLAIM-VOIDED                  VALUE '*'.       
00997      12  CERT-CHANGE-FLAG        PIC X           VALUE SPACE.     
00998          88  PRINTING-CERT-CHANGES               VALUE '*'.       
00999      12  STATUTORY-SWITCH        PIC X           VALUE SPACE.     
01000          88  STATUTORY-REQUIREMENT               VALUE '*'.       
01001      12  STATUTORY-SWITCH-AH     PIC X           VALUE SPACE.     
01002          88  STATUTORY-REQUIREMENT-AH            VALUE '*'.       
01003      12  LIFE-EARNED-SWITCH      PIC X           VALUE SPACE.     
01004          88  LIFE-FULLY-EARNED                   VALUE '*'.       
01005      12  A-H-EARNED-SWITCH       PIC X           VALUE SPACE.     
01006          88  A-H-FULLY-EARNED                    VALUE '*'.       
01007      12  CNCL-FLG            PIC X               VALUE SPACE.     
01008      12  TRANSACTION-FLAG    PIC X               VALUE SPACE.     
01009          88  NO-TRANSACTION-WAS-PROCESSED    VALUE SPACE.         
01010          88  TRANSACTION-WAS-PROCESSED       VALUE '*'.           
01011      12  RISK-REIN-FLAG      PIC X               VALUE SPACE.     
01012          88  REIN-IS-RISK-PREMIUM            VALUE '*'.           
01013      12  RISK-PREM-FLAG      PIC X               VALUE SPACE.     
01014          88  UPDATING-RISK-PREMIUMS          VALUE '*'.           
01015      12  EP-FLG              PIC X               VALUE SPACE.     
01016      12  END-FLG             PIC X               VALUE SPACE.     
01017      12  PRIOR-MATCH-CNTRL   PIC X(36)           VALUE LOW-VALUE. 
01018      12  PRIOR-CERT-IN-CNTRL PIC X(36)           VALUE LOW-VALUE. 
01019      12  ACCT-FLG            PIC X               VALUE SPACE.     
01020      12  STATE-FLG           PIC X               VALUE SPACE.     
01021      12  COMP-FLG            PIC X               VALUE SPACE.     
01022      12  CARR-FLG            PIC X               VALUE SPACE.     
01023      12  FINAL-FLG           PIC X               VALUE SPACE.     
01024      12  LOW-CTL             PIC X(19)           VALUE SPACES.    
01025      12  REIN-FLG            PIC X               VALUE SPACE.     
01026      12  REIN-EP-FLG         PIC X               VALUE SPACE.     
01027      12  REIN-RT-SW          PIC X               VALUE SPACE.     
01028                                                                   
01029  01  TEXAS-REG-WORK-AREAS   SYNC.                                 
01030      12  TEX-FACT-1          PIC S9(9)V99    COMP-3.              
01031      12  TEX-FACT-2          PIC S999        COMP-3.              
01032      12  TEX-FACT-3          PIC S999        COMP-3.              
01033      12  TEX-FACT-4          PIC S9(7)       COMP-3.              
01034      12  TEX-FACT-5          PIC S999        COMP-3.              
01035      12  TEX-FACT-6          PIC S999        COMP-3.              
01036      12  TEX-FACT-7          PIC S9(7)       COMP-3.              
01037      12  TEX-FACT-8          PIC S9V9(6)     COMP-3.              
01038      12  TEX-FACT-9          PIC S9(4)V9(11) COMP-3.              
01039                                                                   
01040  01  NET-PAY-INTERFACE.                                           
01041      12  NP-APR              PIC S999V9(4)   COMP-3.              
01042      12  NP-ORIG             PIC S999        COMP-3.              
01043      12  NP-REM              PIC S999        COMP-3.              
01044      12  NP-OPT              PIC X.                               
01045      12  NP-CAP              PIC S999        COMP-3.              
01046      12  NP-FACTOR           PIC S9(4)V9(9)  COMP-3.              
01047      12  NP-WORK1            PIC S9(9)V9(9)  COMP-3.              
01048      12  NP-WORK2            PIC S9(9)V9(9)  COMP-3.              
01049      12  NP-BENEFIT          PIC S9(9)V99    COMP-3.              
01050      12  NP-REMAINING        PIC S9(9)V99    COMP-3.              
01051      12  NP-AHPRM            PIC S9(7)V99    COMP-3.              
01052      12  NP-ACCOUNT          PIC X(10).                           
01053                                                                   
01054  01  HOLDERS SYNC.                                                
01055      12  DIS-IBNR            PIC S9(7)V99 COMP-3 VALUE +0.        
01056      12  DIS-PAYCUR          PIC S9(7)V99 COMP-3 VALUE +0.        
01057      12  DIS-FUTRSV          PIC S9(7)V99 COMP-3 VALUE +0.        
01058      12  DTH-IBNR            PIC S9(7)V99 COMP-3 VALUE +0.        
01059      12  DTH-PAYCUR          PIC S9(7)V99 COMP-3 VALUE +0.        
01060      12  DTH-FUTRSV          PIC S9(7)V99 COMP-3 VALUE +0.        
01061      12  EXCESS-RESERVE      PIC S9(7)V99 COMP-3 VALUE +0.        
01062      12  H-LFCM              PIC S9(9)V99 COMP-3 VALUE +0.        
01063      12  H-AHCM              PIC S9(9)V99 COMP-3 VALUE +0.        
01064      12  TEMP-1              PIC S9(5)V99 COMP-3 VALUE +0.        
01065      12  TEMP-2              PIC S9(5)V99 COMP-3 VALUE +0.        
01066      12  TEMP-3              PIC S9(5)V99 COMP-3 VALUE +0.        
01067      12  TEMP-4              PIC S9V9(6)  COMP-3 VALUE +0.        
01068      12  TEMP-5              PIC S9V9(6)  COMP-3 VALUE +0.        
01069      12  TEMP-6              PIC S99V9(6) COMP-3 VALUE +0.        
01070      12  TEMP-7              PIC S9(9)V99 COMP-3 VALUE +0.        
01071      12  TEMP-8              PIC S9(9)V99 COMP-3 VALUE +0.        
01072      12  REM-TRM1            PIC S999V99  COMP-3.                 
01073      12  REM-TRM2            PIC S999V99  COMP-3.                 
01074      12  REM-TRM3            PIC S999V99  COMP-3.                 
01075      12  TEM-TRM1            PIC S999V99  COMP-3.                 
01076      12  MAX-REM-TERM        PIC S999V99  COMP-3.                 
01077      12  ORIG-TERM           PIC S999V99  COMP-3.                 
01078      12  HLD-CONTROL.                                             
01079          16  HLD-CARRIER     PIC X               VALUE SPACES.    
01080          16  HLD-GROUPING    PIC X(6)            VALUE SPACES.    
01081          16  HLD-STATE       PIC XX              VALUE SPACES.    
01082          16  HLD-ACCT        PIC X(10)           VALUE SPACES.    
01083      12  LN-MX               PIC S999     COMP-3 VALUE +56.       
01084                                                                   
PEMTST 01  DISPLAY-IBNR-LINE.
           05  FILLER                  PIC X       VALUE SPACES.
           05  DIL-COMMENT             PIC X(20)   VALUE SPACES.
           05  FILLER                  PIC XX      VALUE SPACES.
           05  DIL-TYPE                PIC X       VALUE SPACES.
           05  FILLER                  PIC X       VALUE SPACES.
           05  DIL-BEN-CODE            PIC ZZ      VALUE SPACES.
           05  FILLER                  PIC XXX     VALUE SPACES.
           05  DIL-AMOUNT              PIC ZZZ9.99999-.
PEMTST
CIDMOD 01  DISPLAY-HD-1.
CIDMOD     05  FILLER      PIC X       VALUE SPACES.
CIDMOD     05  FILLER      PIC X(50)   VALUE SPACES.
102204     05  FILLER      PIC X(23)   VALUE 'PROCESSING ERROR REPORT'.
CIDMOD     05  FILLER      PIC X(51)   VALUE SPACES.
CIDMOD     05  FILLER      PIC X(8)    VALUE 'ECS010 '.
CIDMOD
CIDMOD 01  DISPLAY-HD-2.
CIDMOD     05  FILLER                 PIC X       VALUE SPACE.
CIDMOD     05  FILLER                 PIC X(57)   VALUE SPACES.
CIDMOD     05  DIS-DATE               PIC X(8).
CIDMOD     05  FILLER                 PIC X(66)   VALUE SPACES.
CIDMOD
CIDMOD 01  DISPLAY-LINE.
CIDMOD     05  DL-FULL.
CIDMOD         10  FILLER           PIC X       VALUE SPACE.
CIDMOD         10  DIS-LINE-REASON  PIC X(32)   VALUE SPACES.
CIDMOD         10  DIS-LINE-REC     PIC X(100)  VALUE SPACES.
CIDMOD
01085  01  PRT-HDNGS SYNC.                                              
01086      12  TR-HD-1.                                                 
01087          16  FILLER      PIC X       VALUE '1'.                   
01088          16  FILLER      PIC X(53)   VALUE SPACES.                
01089          16  FILLER      PIC X(18)   VALUE 'TRANSACTION REPORT'.  
01090          16  FILLER      PIC X(48)   VALUE SPACES.                
01091          16  FILLER      PIC X(8)    VALUE 'ECS010 '.             
01092      12  TR-HD-2.                                                 
01093          16  FILLER      PIC X       VALUE SPACE.                 
01094          16  FILLER      PIC X(47)   VALUE SPACES.                
01095          16  TRHDNM      PIC X(30).                               
01096          16  FILLER      PIC X(42)   VALUE SPACES.                
01097          16  TR-RN-DT    PIC X(8).                                
01098      12  TR-HD-3.                                                 
01099          16  FILLER      PIC X       VALUE SPACE.                 
01100          16  FILLER      PIC X(53)   VALUE SPACES.                
01101          16  TR-MODAYR   PIC X(18).                               
01102          16  FILLER      PIC X       VALUE SPACE.                 
01103          16  TR-YRASK    PIC X       VALUE SPACE.                 
01104          16  FILLER      PIC X(46)   VALUE SPACES.                
01105          16  FILLER      PIC X(5)    VALUE 'PAGE '.               
01106          16  TR-PG       PIC ZZ,ZZ9.                              
01107      12  TR-HD-4.                                                 
01108          16  FILLER      PIC X       VALUE SPACE.                 
01109          16  TR-AGCY     PIC X(30).                               
01110      12  TR-HD-5.                                                 
01111          16  FILLER      PIC X       VALUE SPACE.                 
01112          16  TR-CARR     PIC X.                                   
01113          16  FILLER      PIC X       VALUE '-'.                   
01114          16  TR-GROUP    PIC X(6).                                
01115          16  FILLER      PIC X       VALUE '-'.                   
01116          16  TR-ST-NO    PIC X(2).                                
01117          16  FILLER      PIC X       VALUE '-'.                   
01118          16  TR-AG-NO    PIC X(10).                               
01119          16  FILLER      PIC X(23)   VALUE SPACES.                
01120          16  TR5-RP-TYP.                                          
01121              20  FILLER  PIC X(20)   VALUE                        
01122                      '* * * * TRANSACTION '.                      
01123              20  FILLER  PIC X(15)   VALUE 'LISTING * * * *'.     
01124      12  TR-HD-6.                                                 
01125          16  FILLER      PIC X       VALUE SPACE.                 
01126          16  TR-CE-MO    PIC XX.                                  
01127          16  FILLER      PIC X       VALUE '-'.                   
01128          16  TR-CE-DA    PIC XX.                                  
01129          16  FILLER      PIC X       VALUE '-'.                   
01130          16  TR-CE-YR    PIC XX.                                  
01131          16  FILLER      PIC X(6)    VALUE '  TO  '.              
01132          16  TR-CX.                                               
01133              20  TR-CX-MO PIC XX.                                 
01134              20  HSHR1    PIC X.                                  
01135              20  TR-CX-DA PIC XX.                                 
01136              20  HSHR2    PIC X.                                  
01137              20  TR-CX-YR PIC XX.                                 
01138      12  TR-HD-7.                                                 
01139          16  FILLER  PIC X      VALUE '0'.                        
01140          16  FILLER  PIC X(24)  VALUE SPACES.                     
01141          16  FILLER  PIC X(19)  VALUE '*----------------- '.      
01142          16  TR-7-LF PIC X(6).                                    
01143          16  FILLER  PIC X(9)   VALUE ' COVERAGE'.                
01144          16  FILLER  PIC X(19)  VALUE ' -----------------*'.      
01145          16  FILLER  PIC X(20)  VALUE '  *---------------- '.     
01146          16  TR-7-AH PIC X(6).                                    
01147          16  FILLER  PIC X(10)  VALUE ' COVERAGE '.               
01148          16  FILLER  PIC X(19)  VALUE ' -----------------*'.      
01149      12  TR-HD-8.                                                 
01150          16  FILLER  PIC X      VALUE SPACE.                      
01151          16  FILLER  PIC X(24)  VALUE '   CERT      ISSUE      '. 
01152          16  FILLER  PIC X(24)  VALUE '        PREMIUM    COMMI'. 
01153          16  FILLER  PIC X(24)  VALUE 'SSION   ACTION          '. 
01154          16  FILLER  PIC X(24)  VALUE '                PREMIUM '. 
01155          16  FILLER  PIC X(22)  VALUE '   COMMISSION   ACTION'.   
01156      12  TR-HD-9.                                                 
01157          16  FILLER  PIC X      VALUE SPACE.                      
01158          16  FILLER  PIC X(24)  VALUE '  NUMBER      DATE  AGE '. 
01159          16  FILLER  PIC X(24)  VALUE 'TYP TRM  / REFUND    /  '. 
01160          16  FILLER  PIC X(24)  VALUE 'CLAIM    DATE     ACTION'. 
01161          16  FILLER  PIC X(24)  VALUE '  ST    TYP TRM  / REFUN'. 
01162          16  FILLER  PIC X(24)  VALUE 'D    /  CLAIM    DATE   '. 
01163          16  FILLER  PIC X(12)  VALUE '  ACTION  ST'.             
01164      12  TR-TTL-HD.                                               
01165          16  FILLER      PIC X       VALUE '0'.                   
01166          16  TTL-PT-CNT  PIC ZZZZ,ZZZ,ZZZ,ZZ9.                    
01167          16  FILLER      PIC XX      VALUE SPACES.                
01168          16  TTL-HD-DS   PIC X(60)   VALUE                        
01169              'ECS010 CONTROL TOTALS'.                             
01170                                                                   
01171  01  TR-DTL.                                                      
01172      12  FILLER                  PIC X           VALUE SPACE.     
01173      12  TR-DTL1.                                                 
01174          16  TRD-CERT            PIC X(11).                       
01175          16  TRD-ISDT.                                            
01176              20  FILLER          PIC X           VALUE SPACE.     
01177              20  TRD-ISMO        PIC XX.                          
01178              20  TRD-IS-S1       PIC X           VALUE '/'.       
01179              20  TRD-ISDA        PIC XX.                          
01180              20  TRD-IS-S2       PIC X           VALUE '/'.       
01181              20  TRD-ISYR        PIC XX.                          
01182              20  FILLER          PIC X           VALUE SPACE.     
01183          16  TRD-AGE             PIC XX.                          
01184      12  TR-DTL2.                                                 
01185          16  FILLER              PIC X           VALUE SPACE.     
01186          16  TRD-LFTYP           PIC XXX.                         
01187          16  FILLER              PIC X           VALUE SPACE.     
01188          16  TRD-LFTRM           PIC ZZ9.                         
01189          16  TRD-LFPRM-RFND      PIC ZZZZ,ZZZ.99  BLANK WHEN ZERO.
01190          16  TRD-LFCOM-CLM       PIC ZZZZ,ZZZ.99- BLANK WHEN ZERO.
01191          16  TRD-LFACTDT.                                         
01192              20  FILLER          PIC X           VALUE SPACE.     
01193              20  TRD-LFACTMO     PIC XX.                          
01194              20  TRD-LF-S1       PIC X           VALUE '/'.       
01195              20  TRD-LFACTDA     PIC XX.                          
01196              20  TRD-LF-S2       PIC X           VALUE '/'.       
01197              20  TRD-LFACTYR     PIC XX.                          
01198              20  FILLER          PIC X           VALUE SPACE.     
01199          16  TRD-LF-ACTION       PIC X(10)       VALUE SPACE.     
01200          16  FILLER              PIC X           VALUE SPACE.     
01201          16  TRD-LF-STATUS       PIC X.                           
01202          16  FILLER              PIC X(4)        VALUE SPACE.     
01203          16  TRD-AHTYP           PIC XXX.                         
01204          16  FILLER              PIC X           VALUE SPACE.     
01205          16  TRD-AHTRM           PIC ZZ9.                         
01206          16  TRD-AHPRM-RFND      PIC ZZZZ,ZZZ.99  BLANK WHEN ZERO.
01207          16  TRD-AHCOM-CLM       PIC ZZZZ,ZZZ.99- BLANK WHEN ZERO.
01208          16  TRD-AHACTDT.                                         
01209              20  FILLER          PIC X           VALUE SPACE.     
01210              20  TRD-AHACTMO     PIC XX.                          
01211              20  TRD-AH-S1       PIC X           VALUE '/'.       
01212              20  TRD-AHACTDA     PIC XX.                          
01213              20  TRD-AH-S2       PIC X           VALUE '/'.       
01214              20  TRD-AHACTYR     PIC XX.                          
01215              20  FILLER          PIC X           VALUE SPACE.     
01216          16  TRD-AH-ACTION       PIC X(10)       VALUE SPACE.     
01217          16  FILLER              PIC X           VALUE SPACE.     
01218          16  TRD-AH-STATUS       PIC X.                           
01219      12  TR-DTL3 REDEFINES TR-DTL2.                               
01220          16  FILLER              PIC X(20).                       
01221          16  TRD-IBNR            PIC ZZZ,ZZZ.99-.                 
01222          16  FILLER              PIC X.                           
01223          16  TRD-IBNRX           PIC X(4).                        
01224          16  FILLER              PIC X(2).                        
01225          16  TRD-PAYCUR          PIC ZZZ,ZZZ.99-.                 
01226          16  TRD-PAYCUR-SPACE REDEFINES TRD-PAYCUR                
01227                                  PIC X(11).                       
01228          16  FILLER              PIC X.                           
01229          16  TRD-PAYCURX         PIC X(8).                        
01230          16  FILLER              PIC X(2).                        
01231          16  TRD-FUTRSV-DATA.                                     
01232              20  TRD-FUTRSV      PIC ZZZ,ZZZ.99-.                 
01233              20  TRD-FUTRSV-SPACE REDEFINES TRD-FUTRSV            
01234                                  PIC X(11).                       
01235              20  FILLER          PIC X.                           
01236              20  TRD-FUTRSVX     PIC X(6).                        
01237          16  TRD-BENEFIT-DATA REDEFINES TRD-FUTRSV-DATA.          
01238              20  TRD-BENEFIT-X   PIC X(10).                       
01239              20  TRD-BENEFIT-CODE                                 
01240                                  PIC X(02).                       
01241              20  FILLER          PIC X(06).                       
01242          16  FILLER              PIC X(4).                        
01243          16  TRD-RSV-DESC        PIC X(6).                        
01244          16  TRD-RSV-ACTION      PIC X(21).                       
01245                                                                   
01246  01  DET-PR-UREC.                                                 
01247      12  FILLER              PIC X(7)     VALUE ' AGE - '.        
01248      12  DET-AGE             PIC 99         BLANK WHEN ZERO.      
01249      12  FILLER              PIC X(11)    VALUE ' JT. AGE - '.    
01250      12  DET-JT-AGE          PIC 99         BLANK WHEN ZERO.      
01251      12  FILLER              PIC X(7)     VALUE ' SEX - '.        
01252      12  DET-SEX             PIC X.                               
01253      12  FILLER              PIC X(9)     VALUE '   APR - '.      
01254      12  DET-PR-APR          PIC Z9.999     BLANK WHEN ZERO.      
01255      12  FILLER              PIC X(9)     VALUE '  MEM# - '.      
01256      12  DET-PR-MEM          PIC X(12).                           
01257      12  FILLER              PIC X(9)     VALUE '   SS# - '.      
01258      12  DET-PR-SS           PIC X(11).                           
01259      12  FILLER              PIC XXX      VALUE SPACES.           
01260      12  DET-1-ACTION        PIC X(20)    VALUE ' MAINTENANCE'.   
01261                                                                   
01262  01  DET-PR-UREC-2.                                               
01263      12  FILLER              PIC X        VALUE SPACE.            
01264      12  DET-LF-DESC         PIC XX.                              
01265      12  FILLER              PIC X(10)    VALUE ' TERM CHG '.     
01266      12  DET-O-LF-TERM       PIC 999        BLANK WHEN ZERO.      
01267      12  FILLER              PIC X(4)     VALUE ' TO '.           
01268      12  DET-N-LF-TERM       PIC 999        BLANK WHEN ZERO.      
01269      12  FILLER              PIC X        VALUE SPACE.            
01270      12  DET-AH-DESC         PIC XX.                              
01271      12  FILLER              PIC X(10)    VALUE ' TERM CHG '.     
01272      12  DET-O-AH-TERM       PIC 999        BLANK WHEN ZERO.      
01273      12  FILLER              PIC X(4)     VALUE ' TO '.           
01274      12  DET-N-AH-TERM       PIC 999        BLANK WHEN ZERO.      
01275      12  FILLER              PIC X(12)    VALUE ' OLD LIVES -'.   
01276      12  DET-O-LIVES         PIC ZZZZ999    BLANK WHEN ZERO.      
01277      12  FILLER              PIC X(12)    VALUE ' NEW LIVES -'.   
01278      12  DET-N-LIVES         PIC ZZZZ999    BLANK WHEN ZERO.      
01279      12  FILLER              PIC X(5)     VALUE SPACES.           
01280      12  DET-2-ACTION        PIC X(20)    VALUE ' MAINTENANCE'.   
01281                                                                   
01282  01  DET-PR-UREC-3.                                               
01283      12  FILLER              PIC X(7)     VALUE ' I/G - '.        
01284      12  DET-IND-GRP         PIC X.                               
01285      12  FILLER              PIC X(13)    VALUE '  PMT FREQ - '.  
01286      12  DET-PMT-FREQ        PIC 99         BLANK WHEN ZERO.      
01287      12  FILLER              PIC X(14)    VALUE '  POL FORM# - '. 
01288      12  DET-POL-FORM        PIC X(12).                           
01289      12  FILLER              PIC X(14)    VALUE '  LOAN OFF# - '. 
01290      12  DET-LOAN-OFFICER    PIC XXX.                             
01291      12  FILLER              PIC X(14)    VALUE '  USER CODE - '. 
01292      12  DET-USER-CODE       PIC X.                               
01293      12  FILLER              PIC X(8)     VALUE SPACES.           
01294      12  DET-3-ACTION        PIC X(20)    VALUE ' MAINTENANCE'.   
01295                                                                   
01296  01  DET-PR-UREC-4.                                               
01297      12  FILLER              PIC X(18) VALUE ' CLM DEDUCT AMT - '.
01298      12  DET-CLM-DEDUCT      PIC 99999.99   BLANK WHEN ZERO.      
01299      12  FILLER              PIC X(18) VALUE ' CAN DEDUCT AMT - '.
01300      12  DET-CAN-DEDUCT      PIC 99999.99   BLANK WHEN ZERO.      
01301      12  FILLER              PIC X(37)    VALUE SPACES.           
01302      12  DET-3-ACTION        PIC X(20)    VALUE ' MAINTENANCE'.   
01303                                                                   
01304  01  DET-PR-UREC-5.                                               
01305      12  FILLER              PIC X(13) VALUE ' LF BEN CD - '.     
01306      12  DET-LF-BEN-CD       PIC XX.                              
01307      12  FILLER              PIC X(11) VALUE ' LF PREM - '.       
01308      12  DET-LF-PREM         PIC ZZZZZZZ.99   BLANK WHEN ZERO.    
01309      12  FILLER              PIC X(11) VALUE ' LF BENE - '.       
01310      12  DET-LF-BEN          PIC ZZZZZZZZZ.99   BLANK WHEN ZERO.  
01311      12  FILLER              PIC X(30)    VALUE SPACES.           
01312      12  DET-3-ACTION        PIC X(20)    VALUE ' MAINTENANCE'.   
01313                                                                   
01314  01  DET-PR-UREC-6.                                               
01315      12  FILLER              PIC X(13) VALUE ' AH BEN CD - '.     
01316      12  DET-AH-BEN-CD       PIC XX.                              
01317      12  FILLER              PIC X(11) VALUE ' AH PREM - '.       
01318      12  DET-AH-PREM         PIC ZZZZZZZ.99   BLANK WHEN ZERO.    
01319      12  FILLER              PIC X(11) VALUE ' AH BENE - '.       
01320      12  DET-AH-BEN          PIC ZZZZZZZZZ.99   BLANK WHEN ZERO.  
01321      12  FILLER              PIC X(30)    VALUE SPACES.           
01322      12  DET-3-ACTION        PIC X(20)    VALUE ' MAINTENANCE'.   
01323                                                                   
01324  01  WS-CHANGE-REC-SW        PIC X  VALUE 'N'.                    
01325      88  WS-CHANGE-REC              VALUE 'Y'.                    
01326                                                                   
01327  01  TOTAL-PAGE-HD1.                                              
01328      12  FILLER              PIC X               VALUE SPACE.     
01329      12  TPH1-TYPE           PIC X(8).                            
01330      12  TPH1-DESC.                                               
01331          16  TPH1-ST-CODE    PIC XX.                              
01332          16  FILLER          PIC XXX.                             
01333          16  TPH1-ST-DESC    PIC X(20).                           
01334                                                                   
01335  01  TOTAL-PAGE-HD2.                                              
01336      12  FILLER              PIC X           VALUE SPACE.         
01337      12  TPH2-DESC           PIC X(20)       VALUE SPACES.        
01338      12  FILLER              PIC X(7)        VALUE 'WRITTEN'.     
01339      12  FILLER              PIC X(9)        VALUE SPACES.        
01340      12  FILLER              PIC X(9)        VALUE 'CANCELLED'.   
01341      12  FILLER              PIC X(11)       VALUE SPACES.        
01342      12  FILLER              PIC X(3)        VALUE 'NET'.         
01343      12  FILLER              PIC X(11)       VALUE SPACES.        
01344      12  FILLER              PIC X(7)        VALUE 'ACCOUNT'.     
01345      12  FILLER              PIC X(10)       VALUE SPACES.        
01346      12  FILLER              PIC X(10)       VALUE 'NET PREM -'.  
01347                                                                   
01348  01  TOTAL-PAGE-HD3.                                              
01349      12  FILLER              PIC X           VALUE SPACE.         
01350      12  TPH3-DESC           PIC X(20)       VALUE SPACES.        
01351      12  FILLER              PIC X(7)        VALUE 'PREMIUM'.     
01352      12  FILLER              PIC X(10)       VALUE SPACES.        
01353      12  FILLER              PIC X(7)        VALUE 'PREMIUM'.     
01354      12  FILLER              PIC X(10)       VALUE SPACES.        
01355      12  FILLER              PIC X(7)        VALUE 'PREMIUM'.     
01356      12  FILLER              PIC X(7)        VALUE SPACES.        
01357      12  FILLER              PIC X(10)       VALUE 'COMMISSION'.  
01358      12  FILLER              PIC X(08)       VALUE SPACES.        
01359      12  FILLER              PIC X(09)       VALUE 'ACCT COMP'.   
01360      12  FILLER              PIC X(11)       VALUE SPACES.        
01361      12  FILLER              PIC X(6)        VALUE 'CLAIMS'.      
01362                                                                   
01363  01  TOTAL-PAGE-DETAIL.                                           
01364      12  FILLER              PIC X(5).                            
01365      12  TPD-DESC            PIC X(6).                            
01366      12  FILLER              PIC X(3).                            
01367      12  TPD-ISS-PRM         PIC ZZZ,ZZZ,ZZZ.99-.                 
01368      12  FILLER              PIC X(2).                            
01369      12  TPD-CNC-PRM         PIC ZZZ,ZZZ,ZZZ.99-.                 
01370      12  FILLER              PIC X(2).                            
01371      12  TPD-NET-PRM         PIC ZZZ,ZZZ,ZZZ.99-.                 
01372      12  FILLER              PIC X(2).                            
01373      12  TPD-ACCT-COM        PIC ZZZ,ZZZ,ZZZ.99-.                 
01374      12  FILLER              PIC X(2).                            
01375      12  TPD-PREM-COMP       PIC ZZZ,ZZZ,ZZZ.99-.                 
01376      12  FILLER              PIC X(2).                            
01377      12  TPD-CLAIMS          PIC ZZZ,ZZZ,ZZZ.99-.                 
01378      12  FILLER              PIC X(4).                            
01379      12  TPD-LO-CERT         PIC X(6).                            
01380      12  FILLER              PIC X(2).                            
01381      12  TPD-HI-CERT         PIC X(6).                            
01382 *    12  FILLER              PIC X.                               
01383 *    12  TPD-LO-CERT         PIC X(8).                            
01384 *    12  FILLER              PIC X.                               
01385 *    12  TPD-HI-CERT         PIC X(8).                            
01386                                                                   
01387  01  DUPE-CERT-TABLE.                                             
01388      12  DUPE-ENTRIES  OCCURS 1000 TIMES.                         
01389          16  DUPE-ENTRY      PIC X(36).                           
01390          16  DUPE-STATUS     PIC X(01).                           
01391          16  DUPE-LF-PRM     PIC S9(7)V99    COMP-3.              
01392          16  DUPE-AH-PRM     PIC S9(7)V99    COMP-3.              
01393                                                                   
01394  01  MISMATCH-HEADING-1.                                          
01395      12  FILLER              PIC X           VALUE '1'.           
01396      12  FILLER              PIC X(45)       VALUE SPACES.        
01397      12  FILLER              PIC X(42)       VALUE                
01398              'MISMATCHED TRANS AND DUPLICATE CERT REPORT'.        
01399      12  FILLER              PIC X(35)       VALUE SPACES.        
CIDMOD     12  FILLER              PIC X(10)       VALUE 'ECS010    '.  
01400                                                                   
01401  01  MISMATCH-HEADING-2.                                          
01402      12  FILLER              PIC X           VALUE '0'.           
01403      12  FILLER              PIC X(44)       VALUE                
01404              ' C  GRP ST  ACCT    EFF DTE  CERT      REC T'.      
01405      12  FILLER              PIC X(44)       VALUE                
01406              'YPE      CERT IN CONTROL                    '.      
01407      12  FILLER              PIC X(44)       VALUE                
01408              '        CERT OUT CONTROL           STATUS   '.      
01409                                                                   
01410  01  MISMATCH-DETAIL.                                             
01411      12  FILLER              PIC X           VALUE '0'.           
01412      12  FILLER              PIC X           VALUE SPACES.        
01413      12  DTL-MATCH-CONTROL   PIC X(36).                           
01414      12  FILLER              PIC X(3)        VALUE SPACES.        
01415      12  DTL-REC-TYPE        PIC X.                               
01416      12  FILLER              PIC X(2)        VALUE SPACES.        
01417      12  DTL-REC-TYPE-DESC   PIC X(3).                            
01418      12  FILLER              PIC X(3)        VALUE SPACES.        
01419      12  DTL-CERT-IN-CONTROL                                      
01420                              PIC X(36).                           
01421      12  FILLER              PIC X           VALUE SPACES.        
01422      12  DTL-CERT-OUT-CONTROL                                     
01423                              PIC X(36).                           
01424      12  FILLER              PIC X           VALUE SPACES.        
01425      12  DTL-STATUS          PIC X(7)        VALUE 'DROPPED'.     
01426      12  FILLER              PIC X(2)        VALUE SPACES.        
01427                                                                   
01428  01  MISMATCH-DETAIL-2.                                           
01429      12  FILLER              PIC X           VALUE ' '.           
01430      12  FILLER              PIC X           VALUE SPACES.        
01431      12  MD2-LF-PREM-DESC    PIC X(16)       VALUE                
01432              ' LIFE PREMIUM = '.                                  
01433      12  MD2-LF-PREM         PIC Z,ZZZ,ZZZ.99-.                   
01434      12  FILLER              PIC X           VALUE SPACES.        
01435      12  MD2-LF-ALT-PREM-DESC PIC X(16)       VALUE               
01436              'LIFE ALT PREM = '.                                  
01437      12  MD2-LF-ALT-PREM     PIC Z,ZZZ,ZZZ.99-.                   
01438      12  FILLER              PIC XX          VALUE SPACES.        
01439      12  MD2-AH-PREM-DESC    PIC X(18)       VALUE                
01440              'A/H PREMIUM = '.                                    
01441      12  MD2-AH-PREM         PIC Z,ZZZ,ZZZ.99-.                   
01442      12  FILLER              PIC X(39)       VALUE SPACES.        
01443                                                                   
01444  01  MISMATCH-DETAIL-3.                                           
01445      12  FILLER              PIC X           VALUE ' '.           
01446      12  FILLER              PIC X           VALUE SPACES.        
01447      12  FILLER              PIC X(16)       VALUE                
01448              ' LIFE  REFUND = '.                                  
01449      12  MD3-LF-REFUND       PIC Z,ZZZ,ZZZ.99-.                   
01450      12  FILLER              PIC X           VALUE SPACES.        
01451      12  FILLER              PIC X(16)       VALUE                
01452              'A & H  REFUND = '.                                  
01453      12  MD3-AH-REFUND       PIC Z,ZZZ,ZZZ.99-.                   
01454      12  FILLER              PIC X(72)       VALUE SPACES.        
01455                                                                   
01456  01  MISMATCH-DETAIL-4.                                           
01457      12  FILLER              PIC X           VALUE ' '.           
01458      12  FILLER              PIC X           VALUE SPACES.        
01459      12  FILLER              PIC X(16)       VALUE                
01460              ' LIFE  CLAIM  = '.                                  
01461      12  MD4-LF-CLAIM        PIC Z,ZZZ,ZZZ.99-.                   
01462      12  FILLER              PIC X           VALUE SPACES.        
01463      12  FILLER              PIC X(16)       VALUE                
01464              'A & H  CLAIM  = '.                                  
01465      12  MD4-AH-CLAIM        PIC Z,ZZZ,ZZZ.99-.                   
01466      12  FILLER              PIC X(72)       VALUE SPACES.        
01467  EJECT                                                            
01468 *                                                                 
01469 *                                                                 
01470 * LEAVE THE ABOVE 2 LINES IN TO PREVENT LOSING THE LINE ABOVE     
01475  EJECT                                                            
01476  01  PB-STARTS-HERE              PIC X(48)                        
01477        VALUE '*****  PENDING BUSINESS RECORD STARTS HERE  ****'.  
01478                                                                   
01479                              COPY ERCPNDB.                        
01480  EJECT                                                            
01481  01  PC-STARTS-HERE              PIC X(48)                        
01482        VALUE '*****   PENDING CLAIMS RECORD STARTS HERE   ****'.  
01483                                                                   
01484                              COPY ERCPNDC.                        
01485  EJECT                                                            
01486  01  CC-STARTS-HERE              PIC X(48)                        
01487        VALUE '***** CERTIFICATE CHANGE RECORD STARTS HERE ****'.  
01488                                                                   
01489                              COPY ERCCRTC.                        
01490  EJECT                                                            
01491  01  CR-STARTS-HERE              PIC X(48)                        
01492        VALUE '***** CERTIFICATE MASTER RECORD STARTS HERE ****'.  
01493                                                                   
01494                              COPY ECSCRT01.                       
01495                              COPY ELCCRTVR.                       
01496                                                                   
01497  01  INITIALIZED-CERTIFICATE     PIC X(1056).                     
01498  EJECT                                                            
01499                              COPY ELCDATE.                        
                                   COPY ELCFUNDT.
01500  EJECT                                                            
01501  01  CPA-STARTS-HERE             PIC X(48)                        
01502        VALUE '***** CALCULATION PASS AREA STARTS HERE ********'.  
01503                              COPY ELCCALC.                        
01504  EJECT                                                            
01505                              COPY ELCDTECX.                       
01506  EJECT                                                            
01507                              COPY ELCDTEVR.                       
01508  EJECT                                                            
01509                              COPY ELCACCTV.                       
01510  EJECT                                                            
01511                              COPY ELCEXTVR.                       
01512  EJECT                                                            
01513  PROCEDURE DIVISION.                                              
01514                                                                   
01515  0000-START.                                                      
01516                                                                   
01517 ******************************************************************
01518 ***        D A T E   C A R D   L O A D   R O U T I N E         ***
01519 ******************************************************************
01520                                                                   
01521 *0003-LOAD-DATE-CARD.                                             
01522                              COPY ELCDTERX.                       
01523  EJECT                                                            
01524 ******************************************************************
01525 ***           O P E N   F I L E S   R O U T I N E              ***
01526 ******************************************************************
01527                                                                   
01528      DISPLAY ' '.                                                 
01529      DISPLAY '****** THE FOLLOWING MESSAGES WERE CREATED BY '     
01530          'ECS010 ******'                                          
01531      DISPLAY ' '.                                                 
01532                                                                   
           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           MOVE 'Y'                    TO DC-FORCE-EL310-DATE-SW
           MOVE FUNCTION-DATE          TO DC-EL310-DATE
           .
01533  0080-OPEN-FILES.                                                 
pemuni*    IF DTE-PGM-OPT = '1'
pemuni*       open output detail-extr
pemuni*    end-if
01535      OPEN INPUT  PENDING-EXTRACT
01536                  CERT-MASTER-IN                                   
070102                 ERACCTT-IN
111204                 ERAGTC
01538                                                                   
070102          I-O    ERMEBL-INOUT
111204                 ELCRTT
01540                                                                   
01541           OUTPUT SUMMARY-EXTR                                     
062104                 ME-ECS010-BALANCE
062104                 ME50-ECS010-BALANCE
CIDMOD                 DISPLAY-PRT
01542                  MISMATCH-REPORT                                  
070102                 PRINT-COPY.
01544                                                                   
01545      EJECT                                                        
01546 *START*************CUSTOM CODE FOR CLIENT "DMD"*************      
01547      IF DTE-CLIENT NOT = 'DMD'                                    
01548          NEXT SENTENCE                                            
01549      ELSE                                                         
070102         OPEN INPUT ERRESC-IN
01551             IF ERRESC-FILE-STATUS   = '00' OR '97'                
01552                 NEXT SENTENCE                                     
01553             ELSE                                                  
01554                 MOVE ERRESC-FILE-STATUS TO WS-ABEND-FILE-STATUS   
01555                 MOVE 'ERROR OPENING ERRESC FILE'                  
01556                                         TO WS-ABEND-MESSAGE       
01557                 GO TO ABEND-PGM.                                  
01558 *END***************CUSTOM CODE FOR CLIENT "DMD"*************      
01559                                                                   
01560      IF ERACCTT-FILE-STATUS  = '00'  OR  '97'                     
01561          NEXT SENTENCE                                            
01562        ELSE                                                       
01563          MOVE '11'                    TO ABEND-CODE-1             
01564          MOVE ERACCTT-FILE-STATUS     TO ABEND-CODE-2             
01565          MOVE WS-ABEND-CODE           TO WS-RETURN-CODE           
01566          GO TO ABEND-PGM.                                         
01567                                                                   
111204     IF ELCRTT-FILE-STATUS  = '00'  OR  '97'                     
111204        CONTINUE
111204     ELSE                                                       
              DISPLAY ' OPEN ERROR ELCRTT '
111204        MOVE '31'                TO ABEND-CODE-1             
111204        MOVE ELCRTT-FILE-STATUS  TO ABEND-CODE-2             
111204        MOVE WS-ABEND-CODE       TO WS-RETURN-CODE           
111204        GO TO ABEND-PGM
           END-IF
111204                                                                  
111204     IF ERAGTC-FILE-STATUS  = '00'  OR  '97'                     
111204        CONTINUE
111204     ELSE                                                       
              DISPLAY ' OPEN ERROR ERAGTC '
111204        MOVE '41'                TO ABEND-CODE-1             
111204        MOVE ERAGTC-FILE-STATUS  TO ABEND-CODE-2             
111204        MOVE WS-ABEND-CODE       TO WS-RETURN-CODE           
111204        GO TO ABEND-PGM
           END-IF
111204                                                                  
01568      IF ERMEBL-FILE-STATUS  = '00'  OR  '97'                      
01569          NEXT SENTENCE                                            
01570        ELSE                                                       
01571          MOVE 'N'                     TO ME-UPDATE-FLAG.          
01572                                                                   
01573      IF DTE-PGM-OPT = '1'                                         
01574          OPEN OUTPUT  CERT-MASTER-OUT                             
pemuni                      DETAIL-EXTR
01576      ELSE                                                         
01577          MOVE HIGH-VALUES             TO PENDING-MATCH-CONTROL    
01578          MOVE '*'                     TO TRANS-EOF-FLAG.          
01579                                                                   
01580      MOVE 'R'                         TO CP-RATE-FILE.            
01581      MOVE 'O'                         TO CP-IO-FUNCTION.          
01582      PERFORM 8100-GET-RATE THRU 8199-GET-RATE-X.                  
01583      IF IO-ERROR                                                  
01584          MOVE 'ERROR OCCURED OPENING - ELRATE'                    
01585                                       TO WS-ABEND-MESSAGE         
01586          MOVE 0302                    TO WS-RETURN-CODE           
01587          GO TO ABEND-PGM.                                         
01588                                                                   
01589 ******************************************************************
01590 ***       I N I T I A L I Z A T I O N   R O U T I N E          ***
01591 ******************************************************************
01592                                                                   
01593  0090-INITIALIZATION-ROUTINE.                                     
01594                                                                   
01595      ACCEPT WS-TIME-OF-DAY   FROM TIME.                           
01596      MOVE WS-TIME                     TO ME-START-TIME.           
01597      MOVE WS-CURRENT-DATE             TO ME-START-DATE            
01598                                          TR-RN-DT.                
01599      MOVE ME-START-MO                 TO ME-CNDS-MO.              
01600      MOVE ME-START-DA                 TO ME-CNDS-DA.              
01601      MOVE ME-START-YR                 TO ME-CNDS-YR.              
01602      COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO.           
01603      MOVE COMPANY-NAME                TO TRHDNM.                  
01604      MOVE ALPH-DATE                   TO TR-MODAYR.               
01605      MOVE RUN-DATE                    TO EOM-RUN-DATE.            
01606      MOVE BIN-RUN-DATE                TO BIN-EOM-RUN-DATE.        
01607                                                                   
01608      MOVE EOM-RUN-DATE                TO  LAST-YRS-DATE.          
01609      COMPUTE LYD-CCYY = LYD-CCYY - 1.                             
01610      COMPUTE LYD-MO = LYD-MO - 1.                                 
01611      IF LYD-MO = ZERO                                             
01612          MOVE 12                      TO LYD-MO                   
01613          COMPUTE LYD-CCYY = LYD-CCYY - 1.                         
01614                                                                   
01615      MOVE BIN-EOM-RUN-DATE       TO BIN-VALUATION-DT.             
01616                                                                   

062104     IF DTE-CLIENT = 'CID'
062104         MOVE 'CILGM15'          TO  WS-ME-BAL-JOB
062104         MOVE 'CILGM15'          TO  WS-ME50-BAL-JOB
062104     ELSE
062104         MOVE 'CIDCLGM15'        TO  WS-ME-BAL-JOB
062104         MOVE 'CIDCLGM15'        TO  WS-ME50-BAL-JOB
062104     END-IF.

01617      IF  NOT DTE-OPT-RESERVE-METHOD-AUTH                          
01618          GO TO 0090-CONTINUE.                                     
01619                                                                   
01620      MOVE BIN-VALUATION-DT       TO DC-BIN-DATE-1.                
01621      MOVE -12                    TO DC-ELAPSED-MONTHS.            
01622      MOVE ZEROS                  TO DC-ELAPSED-DAYS.              
01623      MOVE '6'                    TO DC-OPTION-CODE.               
01624      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
01625                                                                   
01626      IF  DC-ERROR-CODE NOT EQUAL SPACES                           
01627          MOVE '**** WORK DATE CALC ERROR ****'                    
01628                                  TO WS-ABEND-MESSAGE              
01629          MOVE '90'               TO ABEND-CODE-1                  
01630          MOVE '0'                TO AC2-ONE                       
01631          MOVE DC-ERROR-CODE      TO AC2-TWO                       
01632          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                
01633          GO TO ABEND-PGM.                                         
01634                                                                   
01635      MOVE DC-BIN-DATE-2          TO  W-WORK-DATE.                 
01636                                                                   
01637      MOVE DC-GREG-DATE-CYMD      TO  EOM-DATE (01).               
01638      MOVE EOM-RUN-DATE           TO  EOM-DATE (13).               
01639                                                                   
01640      PERFORM 0095-GET-EOM-DATES THRU 0095-EXIT                    
01641              VARYING                                              
01642          SUBM FROM +2 BY +1                                       
01643              UNTIL                                                
01644          SUBM GREATER THAN +12.                                   
01645                                                                   
01646      DISPLAY 'END OF MONTH TABLE VALUES USED BY ECS010 - '.       
01647                                                                   
01648      PERFORM 0096-DISPLAY-EOM-DATES THRU 0096-EXIT                
01649              VARYING                                              
01650          SUBM FROM +1 BY +1                                       
01651              UNTIL                                                
01652          SUBM GREATER THAN +13.                                   
01653                                                                   
01654      MOVE BIN-VALUATION-DT       TO DC-BIN-DATE-1.                
01655 *    MOVE -90                    TO DC-ELAPSED-DAYS.              
01656      MOVE ZEROS                  TO DC-ELAPSED-DAYS               
01657                                     DC-ELAPSED-MONTHS.            
01658      SUBTRACT COMPANY-IBNR-LAG-MONTH FROM DC-ELAPSED-MONTHS.      
01659      MOVE 1                      TO DC-END-OF-MONTH.              
01660      MOVE '6'                    TO DC-OPTION-CODE.               
01661      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
01662                                                                   
01663      IF  DC-ERROR-CODE NOT EQUAL SPACES                           
01664          MOVE '**** CUT OFF DATE ERROR ****'                      
01665                                  TO WS-ABEND-MESSAGE              
01666          MOVE '90'               TO ABEND-CODE-1                  
01667          MOVE '0'                TO AC2-ONE                       
01668          MOVE DC-ERROR-CODE      TO AC2-TWO                       
01669          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                
01670          GO TO ABEND-PGM.                                         
01671                                                                   
01672      MOVE DC-GREG-DATE-CYMD      TO W-CUTOFF-DATE.                
01673      DISPLAY ' CUT OFF DATE - '  W-CUTOFF-DATE.                   
01674                                                                   
01675  0090-CONTINUE.                                                   
01676                                                                   
01677      MOVE BIN-VALUATION-DT       TO DC-BIN-DATE-1.                
01678      MOVE -1                     TO DC-ELAPSED-MONTHS.            
01679      MOVE +0                     TO DC-ELAPSED-DAYS.              
01680      MOVE '6'                    TO DC-OPTION-CODE.               
01681      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
01682                                                                   
01683      IF NO-CONVERSION-ERROR                                       
01684         MOVE DC-GREG-DATE-CYMD   TO OB-RUN-DATES (1).             
01685                                                                   
01686      MOVE DC-BIN-DATE-2          TO DC-BIN-DATE-1.                
01687      MOVE -1                     TO DC-ELAPSED-MONTHS.            
01688      MOVE +0                     TO DC-ELAPSED-DAYS.              
01689      MOVE '6'                    TO DC-OPTION-CODE.               
01690      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
01691                                                                   
01692      IF NO-CONVERSION-ERROR                                       
01693         MOVE DC-GREG-DATE-CYMD   TO OB-RUN-DATES (2).             
01694                                                                   
01695      MOVE +0                     TO DC-ELAPSED-MONTHS             
01696                                     DC-ELAPSED-DAYS.              
01697                                                                   
01698      MOVE SPACES                 TO CERTIFICATE-RECORD.           
01699      MOVE 'CR'                   TO CR-RECORD-ID.                 
01700      MOVE ZEROS                  TO                               
01701                CR-AGE                  CR-JOINT-AGE               
01702                CR-LFTYP                CR-AHTYP                   
01703                CR-LF-TERM              CR-AH-TERM                 
01704                CR-LF-CRIT-PERIOD       CR-AH-CRIT-PERIOD          
01705                CR-LF-TERM-IN-DAYS                                 
01706                CR-LF-DEV-PCT           CR-AH-DEV-PCT              
01707                CR-LFAMT                CR-AHAMT                   
01708                CR-LFPRM                CR-AHPRM                   
01709                CR-LF-POLICY-FEE        CR-AH-POLICY-FEE           
01710                CR-LFPRM-CALC           CR-AHPRM-CALC              
01711                CR-LFPRM-RATE           CR-AHPRM-RATE              
01712                CR-LFAMT-ALT                                       
01713                CR-LFPRM-ALT            CR-ISS-MICROFILM-NO        
01714                CR-LFPRM-CALC-ALT       CR-CAN-MICROFILM-NO        
01715                CR-LFPRM-RATE-ALT                                  
01716                CR-LFRFND               CR-AHRFND                  
01717                CR-LFRFND-CALC          CR-AHRFND-CALC             
01718                CR-LF-NSP-PRM           CR-AH-NSP-PRM              
01719                CR-LF-NSP-PRM-RATE      CR-AH-NSP-PRM-RATE         
01720                CR-LF-REI-RISK-PRM      CR-AH-REI-RISK-PRM         
01721                CR-LF-EXPIRE-DATE       CR-AH-EXPIRE-DATE          
01722                CR-LIVES                CR-APR                     
01723                CR-BILLED                                          
PEMMOD               CR-LF-ISS-PREM-TAX      CR-AH-ISS-PREM-TAX
PEMMOD               CR-LF-CNC-PREM-TAX      CR-AH-CNC-PREM-TAX
01724                CR-PMT-FREQ             CR-LOAN-TERM               
01725                CR-SKIP                 CR-LOAN-1ST-PMT-DT         
01726                CR-SUM-CAN-CNT-ITD      CR-SUM-CAN-CNT-YTD         
01727                CR-PMT-EXTENSION-DAYS   CR-ENTRY-DATE              
01728                CR-LF-CANC-DT           CR-AH-CANC-DT              
01729                CR-LF-CANCEL-EXIT-DATE  CR-AH-CANCEL-EXIT-DATE     
01730                CR-LF-CLAIM-EXIT-DATE   CR-AH-SETTLEMENT-EXIT-DATE 
01731                CR-NUM-DTH-CLM          CR-NUM-DIS-CLM             
01732                CR-DTH-DT               CR-DIS-DT                  
01733                CR-DTH-RPT-DT           CR-DIS-RPT-DT              
01734                CR-DTH-PAY-DT           CR-DIS-PAY-DT              
01735                                        CR-DIS-PTO-DT              
01736                CR-DTHAMT               CR-DISAMT                  
01737                CR-DTHAMT-YTD           CR-DISAMT-YTD              
01738                CR-DTHAMT-LAST          CR-DISAMT-LAST             
01739                CR-DTHEXP               CR-DISEXP                  
01740                CR-DTHEXP-YTD           CR-DISEXP-YTD              
01741                CR-DTH-AGE              CR-DAYS-DISAB              
01742                CR-DIS-INCUR-DT (1)     CR-CLAIM-DEDUCT-WITHHELD   
01743                CR-INCUR-DISAMT (1)     CR-CANCEL-DEDUCT-WITHHELD  
01744                CR-INCUR-DISEXP (1)     CR-REMIT-TO                
01745                CR-CANCEL-STATE-TAX     CR-CANCEL-MUNI-TAX
011904               CR-MOB-NET-TOT-FEES
040504               CR-ADDL-CLP.
01746                                                                   
01747      MOVE CR-DISAB-DETAIL-DATA (1)  TO CR-DISAB-DETAIL-DATA (2)   
01748                                        CR-DISAB-DETAIL-DATA (3)   
01749                                        CR-DISAB-DETAIL-DATA (4)   
01750                                        CR-DISAB-DETAIL-DATA (5).  
01751                                                                   
01752      MOVE CERTIFICATE-RECORD     TO INITIALIZED-CERTIFICATE.      
01753                                                                   
           MOVE SPACES                 TO WS-BANK-COMMISSIONS
           PERFORM VARYING IA FROM +1 BY +1 UNTIL
              IA > +10
              MOVE ZEROS               TO WS-BNK-SPP-FEES (IA)
           END-PERFORM
           MOVE WS-BANK-COMMISSIONS    TO WS-BANK-INIT

01754      MOVE LOW-VALUES   TO ACCOUNT-MASTER   CERTIFICATE-RECORD.    
01755                                                                   
01756      MOVE HIGH-VALUES  TO REIN-HOLD-AREAS  RC-REIN-HOLD-AREAS     
01757                           EPEC-TOTALS      EPEC-SUBS              
01758                           EPEC-UNDERWRITTEN-TOTALS.               
01759                                                                   
01760      MOVE ZEROS        TO EPEC-LEVELS-END  EPEC-SUBS-END          
01761                           EPEC-LEVELS-END-II.                     
01762      MOVE SPACES       TO REIN-LEVELS-END  RC-REIN-LEVELS-END     
01763                           DUPE-CERT-TABLE.                        
01764                                                                   
01765      PERFORM CLEAR-REIN-HOLD        THRU CLEAR-REIN-HOLD-X.       
01766      PERFORM 4550-CLEAR-EPEC-TOTALS THRU 4589-CLEAR-EPEC-TOTALS-X.
01767      PERFORM 5900-CLEAR-TOTAL-PAGES THRU 5999-CLEAR-TOTAL-PAGES-X 
01768                 VARYING SUB6 FROM 1 BY 1 UNTIL SUB6 GREATER 6.    
01769      PERFORM 7900-CLEAR-BILLING-TABLE THRU 7999-CLEAR-BILLING-X   
01770                 VARYING B-SUB FROM 1 BY 1 UNTIL B-SUB GREATER 50. 
01771                                                                   
01772      IF RUN-DATE NOT = EP-DT                                      
01773          MOVE '*'                     TO TR-YRASK.                
01774      MOVE LIFE-OVERRIDE-L6            TO TR-7-LF.                 
01775      MOVE LIFE-OVERRIDE-L2            TO DET-LF-DESC.             
01776      MOVE AH-OVERRIDE-L6              TO TR-7-AH.                 
01777      MOVE AH-OVERRIDE-L2              TO DET-AH-DESC.             
01778                                                                   
01779      IF DTE-CLIENT = 'FIA' OR 'TAO' OR 'PEK'                      
01780          MOVE +1000                   TO DUP-MAX.                 
01781                                                                   
01782      MOVE DTE-CLIENT                  TO ME-COMPANY.              
01783      MOVE MONTH-END-MOYR              TO ME-MOYR.                 
01784      IF ME-DO-UPDATE                                              
070102         READ ERMEBL-INOUT INVALID KEY
070102                       CLOSE ERMEBL-INOUT
01787                        MOVE 'N'       TO ME-UPDATE-FLAG.          
01788                                                                   
01789      GO TO 0150-READ-ACCOUNT-MASTER.                              
01790                                                                   
01791  0090-EXIT.                                                       
01792      EXIT.                                                        
01793  EJECT                                                            
01794  0095-GET-EOM-DATES.                                              
01795                                                                   
01796      MOVE W-WORK-DATE            TO DC-BIN-DATE-1.                
01797      MOVE '6'                    TO DC-OPTION-CODE.               
01798      MOVE 1                      TO DC-ELAPSED-MONTHS             
01799                                     DC-END-OF-MONTH.              
01800      MOVE ZEROS                  TO DC-ELAPSED-DAYS.              
01801      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
01802                                                                   
01803      IF  DC-ERROR-CODE NOT EQUAL SPACES                           
01804          MOVE '**** EOM PLUS 1 DATE CALC ERROR ****'              
01805                                  TO WS-ABEND-MESSAGE              
01806          MOVE '90'               TO ABEND-CODE-1                  
01807          MOVE '0'                TO AC2-ONE                       
01808          MOVE DC-ERROR-CODE      TO AC2-TWO                       
01809          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                
01810          GO TO ABEND-PGM.                                         
01811                                                                   
01812      MOVE DC-BIN-DATE-2          TO  W-WORK-DATE                  
01813                                      DC-BIN-DATE-1.               
01814      MOVE SPACES                 TO  DC-BIN-DATE-2.               
01815      MOVE ' '                    TO  DC-OPTION-CODE.              
01816      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
01817                                                                   
01818      IF  DC-ERROR-CODE NOT EQUAL SPACES                           
01819          MOVE '**** YMD EOM DATE CALC ERROR ****'                 
01820                                  TO WS-ABEND-MESSAGE              
01821          MOVE '90'               TO ABEND-CODE-1                  
01822          MOVE '0'                TO AC2-ONE                       
01823          MOVE DC-ERROR-CODE      TO AC2-TWO                       
01824          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                
01825          GO TO ABEND-PGM.                                         
01826                                                                   
01827      MOVE DC-GREG-DATE-CYMD      TO  EOM-DATE (SUBM).             
01828                                                                   
01829  0095-EXIT.                                                       
01830      EXIT.                                                        
01831                                                                   
01832  0096-DISPLAY-EOM-DATES.                                          
01833                                                                   
01834      DISPLAY SUBM '. ' EOM-DATE (SUBM).                           
01835                                                                   
01836  0096-EXIT.                                                       
01837      EXIT.                                                        
01838  EJECT                                                            
01839 ******************************************************************
01840 ***    R E A D   A C C O U N T   M A S T E R   R O U T I N E   ***
01841 ******************************************************************
01842                                                                   
01843  0100-ACCT-MASTER-READ-ROUTINE.                                   
01844                                                                   
01845      IF EP-FLG = '1'                                              
01846         ADD +1                   TO BC-ACT-ACCTS
01847         IF DTE-OPT-RESERVE-METHOD-AUTH                          
01848            PERFORM 4700-CALC-OPT-IBNR
102204                                 THRU 4700-EXIT            
102204        END-IF
102204        PERFORM 4500-BUILD-EPEC-EXTRACT                      
102204                                 THRU 4599-BUILD-EPEC-EXTRACT-X
102204     END-IF
01854                                                                   
01855      IF TRANSACTION-WAS-PROCESSED                                 
01856          MOVE '1'                     TO ACCT-FLG                 
01857                                          STATE-FLG                
01858                                          COMP-FLG                 
01859                                          CARR-FLG                 
01860                                          FINAL-FLG.               
01861                                                                   
01862  0150-READ-ACCOUNT-MASTER.                                        
01863                                                                   
070102     READ ERACCTT-IN AT END
01865          GO TO 9930-EOJ-3.                                        
01866                                                                   
01867      ADD +1                            TO ERACCTT-READS.          
01868                                                                   
01869      COPY ELCACCTI.                                               
01870                                                                   
01871      IF FIRST-ACCOUNT EQUAL +1                                    
01872         MOVE +0                  TO FIRST-ACCOUNT                 
01873         GO TO 0190-PROCESS-NEW-ACCOUNT.                           
01874                                                                   
01875      IF AM-CARRIER NOT = HLD-CARRIER                              
01876          PERFORM 5400-CARR-BREAK THRU 5499-CARR-BREAK-X           
01877      ELSE                                                         
01878      IF AM-GROUPING NOT = HLD-GROUPING                            
01879          PERFORM 5300-COMP-BREAK THRU 5399-COMP-BREAK-X           
01880      ELSE                                                         
01881      IF AM-STATE NOT = HLD-STATE                                  
01882          PERFORM 5200-STATE-BREAK THRU 5299-STATE-BREAK-X         
01883      ELSE                                                         
01884      IF AM-ACCOUNT NOT = HLD-ACCT                                 
01885          PERFORM 5100-ACCOUNT-BREAK THRU 5199-ACCOUNT-BREAK-X     
01886      ELSE                                                         
01887      PERFORM 5000-DATE-RANGE-BREAK THRU 5099-DATE-RANGE-BREAK-X.  
01888                                                                   
01889      IF (AM-CARRIER NOT = HLD-CARRIER) OR                         
01890         (AM-GROUPING NOT = HLD-GROUPING)                          
01891          PERFORM 7000-CLIENT-BILLING-ROUTINE THRU 7099-BILL-X.    
01892                                                                   
01893  0190-PROCESS-NEW-ACCOUNT.                                        
01894                                                                   
01895      IF AM-CARRIER NOT = HLD-CARRIER                              
01896          PERFORM 0199A-CARRIER-TABLE-LOOKUP THRU 0199A-LOOKUP-XIT.
01897                                                                   
01898      MOVE AM-CONTROL-A          TO HLD-CONTROL.                   
01899      MOVE AM-CARRIER            TO TR-CARR.                       
01900      MOVE AM-GROUPING           TO TR-GROUP.                      
01901      MOVE AM-ACCOUNT            TO TR-AG-NO.                      
01902      MOVE AM-NAME               TO TR-AGCY.                       
01903      MOVE AM-EFF-MO             TO TR-CE-MO.                      
01904      MOVE AM-EFF-DA             TO TR-CE-DA.                      
01905      MOVE AM-EFF-YR             TO TR-CE-YR.                      
01906      MOVE AM-STATE              TO TR-ST-NO.                      
01907      IF AM-EXPIRE-DT =  99999999999                               
01908          MOVE 'CURRENT '        TO TR-CX                          
01909      ELSE                                                         
01910          MOVE AM-EXP-MO         TO TR-CX-MO                       
01911          MOVE AM-EXP-DA         TO TR-CX-DA                       
01912          MOVE AM-EXP-YR         TO TR-CX-YR                       
01913          MOVE '-'               TO HSHR1  HSHR2.                  
01914                                                                   
01915      ADD +1 TO BC-ACCTS.                                          
01916                                                                   
01917      IF AM-RECORD-ID = '**'                                       
01918          ADD +1 TO BC-NEW-ACCTS.                                  
01919      IF AM-RET-Y-N = 'Y' OR 'I' OR 'G' OR 'L' OR 'D'              
01920          ADD +1 TO BC-ER-ACCTS.                                   
01921                                                                   
01922      MOVE ZEROS                 TO SV-LO-CERT                     
01923                                    SV-HI-CERT.                    
01924      MOVE SPACE                 TO TRANSACTION-FLAG               
01925                                    LST-TRD-CERT                   
01926                                    LST-TRD-ISDT                   
01927                                    OB-DATE-1-SWITCH               
01928                                    OB-DATE-2-SWITCH               
01929                                    OB-DATE-3-SWITCH.              
01930      MOVE +81                   TO LN-CNT.                        
01931                                                                   
01932  0199-READ-ACCOUNT-X.                                             
01933      EXIT.                                                        
01934                                                                   
01935  0199A-CARRIER-TABLE-LOOKUP.                                      
01936                                                                   
01937      PERFORM 0199B-SEARCH-CARRIER-TABLE                           
01938             VARYING CLAS-INDEXCN FROM 1 BY 1                      
01939          UNTIL AM-CARRIER = CARRIER-SUB (CLAS-INDEXCN)  OR        
01940                CLAS-INDEXCN GREATER CLAS-MAXCN.                   
01941                                                                   
01942      IF CLAS-INDEXCN GREATER CLAS-MAXCN                           
01943          DISPLAY 'CARRIER NOT IN CARRIER NAME TABLE: ' AM-CARRIER 
01944          MOVE '** CARRIER NOT IN CARRIER NAME TABLE **'           
01945                                  TO WS-ABEND-MESSAGE              
01946          MOVE 4004               TO WS-ABEND-CODE                 
01947          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                
01948          GO TO ABEND-PGM.                                         
01949                                                                   
01950  0199A-LOOKUP-XIT.                                                
01951      EXIT.                                                        
01952  0199B-SEARCH-CARRIER-TABLE.                                      
01953      EXIT.                                                        
01954  EJECT                                                            
01955 ******************************************************************
01956 ***       R E A D   C E R T   M A S T E R   R O U T I N E      ***
01957 ******************************************************************
01958                                                                   
01959  0200-CERT-MASTER-READ-ROUTINE.                                   
01960                                                                   
01961      IF EOF-ON-CERT                                               
01962          GO TO 0299-READ-CERT-X.                                  
01963                                                                   
01964      IF FIRST-CERT EQUAL +1                                       
01965         MOVE +0                  TO FIRST-CERT                    
01966         GO TO 0250-READ-CERT-MASTER.                              
01967                                                                   
01968      MOVE CERT-IN-RECORD             TO CERTIFICATE-RECORD.       
01969                                                                   
01970      PERFORM CERT-DATE-LOAD.                                      
01971                                                                   
01972      IF CR-IND-GRP = 'I'  OR  '1'                                 
01973          MOVE '1'                    TO CR-IND-GRP                
01974      ELSE                                                         
01975          MOVE '2'                    TO CR-IND-GRP.               
01976                                                                   
01977      IF CR-LF-REI-RISK-PRM NOT NUMERIC                            
01978          MOVE ZEROS                TO CR-LF-REI-RISK-PRM.         
01979      IF CR-AH-REI-RISK-PRM NOT NUMERIC                            
01980          MOVE ZEROS                TO CR-AH-REI-RISK-PRM.         
01981                                                                   
01982 ************ TEMP CODE TO INITIALIZE RISK PREM FIELDS ************
01983      IF DTE-CLIENT NOT = 'NSL'                                    
01984          MOVE ZEROS                TO CR-LF-REI-RISK-PRM          
01985                                       CR-AH-REI-RISK-PRM.         
01986 ******************************************************************
01987                                                                   
01988      IF CR-CLAIM-DEDUCT-WITHHELD NOT NUMERIC                      
01989          MOVE ZEROS                TO CR-CLAIM-DEDUCT-WITHHELD.   
01990      IF CR-CANCEL-DEDUCT-WITHHELD NOT NUMERIC                     
01991          MOVE ZEROS                TO CR-CANCEL-DEDUCT-WITHHELD.  
01992                                                                   
01993      MOVE CR-STATE                   TO STATE-L.                  
01994      PERFORM 8200-STATE-CODE-LOOKUP THRU 8299-STATE-LOOKUP-X.     
01995                                                                   
01996      IF CR-LFTYP NOT = ZERO                                       
01997          MOVE CR-LFTYP               TO CLAS-LOOK                 
01998          PERFORM 8300-LIFE-TYPE-LOOKUP THRU 8399-LIFE-LOOKUP-X.   
01999                                                                   
02000      IF CR-AHTYP NOT = ZERO                                       
02001          MOVE CR-AHTYP               TO CLAS-LOOK                 
02002          PERFORM 8400-A-H-TYPE-LOOKUP THRU 8499-A-H-LOOKUP-X.     
02003                                                                   
           MOVE CR-DT                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8000-DATE-CONVERT-ROUTINE
                                       THRU 8099-DATE-CONVERT-X
           MOVE DC-BIN-DATE-1          TO WS-CR-BIN-DATE

           IF (DTE-CLIENT = 'DCC')
092705        AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
              PERFORM 0395-GET-CERT-TRLRS
                                       THRU 0395-EXIT
           END-IF

02004      IF RUN-MO NOT = 01                                           
02005          GO TO 0250-READ-CERT-MASTER.                             
02006                                                                   
02007      MOVE ZERO                 TO CR-DTHAMT-YTD  CR-DISAMT-YTD    
02008                                   CR-DTHEXP-YTD  CR-DISEXP-YTD.   
02009                                                                   
02010      IF ((CR-LFTYP NOT = ZERO)  AND                               
02011          (CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'))  OR              
02012         ((CR-AHTYP NOT = ZERO)  AND                               
02013          (CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'Z'))                  
02014            MOVE ZERO           TO CR-SUM-CAN-CNT-YTD.             
02015                                                                   
           .           
02016  0250-READ-CERT-MASTER.                                           
02017                                                                   
02018      READ CERT-MASTER-IN                                          
02019                AT END GO TO 9920-EOJ-2.                           
02020                                                                   
02021      ADD +1 TO CMIN-CNT.                                          
02022                                                                   
02023      IF CIR-CONTROL LESS THAN PRIOR-CERT-IN-CNTRL                 
02024          DISPLAY 'SEQUENCE ERROR ON CERT MASTER'                  
02025          DISPLAY 'FIRST  - '  PRIOR-CERT-IN-CNTRL                 
02026          DISPLAY 'SECOND - '  CIR-CONTROL                         
02027          MOVE 0612                    TO WS-ABEND-CODE            
02028          MOVE WS-ABEND-CODE           TO WS-RETURN-CODE           
02029          GO TO ABEND-PGM.                                         
02030                                                                   
02031      IF CIR-CONTROL = PRIOR-CERT-IN-CNTRL                         
02032          DISPLAY 'DUPLICATE CERT ON FILE'  PRIOR-CERT-IN-CNTRL    
02033          IF DTE-CLIENT EQUAL 'SEN'                                
02034             GO TO 0250-READ-CERT-MASTER                           
02035          ELSE                                                     
02036             MOVE 'DUPLICATE CERT ON FILE'                         
02037                                       TO WS-ABEND-MESSAGE         
02038             MOVE 0302                 TO WS-ABEND-CODE            
02039             MOVE WS-ABEND-CODE        TO WS-RETURN-CODE           
02040             GO TO ABEND-PGM.                                      
02041                                                                   
CIDMOD******************************************************
CIDMOD*   THIS LOGIC WAS ADDED TO ACCOMMODATE THE EXECUTION
CIDMOD*   OF A "MID MONTH" BILLING REPORT (EL562) FOR
CIDMOD*   COMMERCIAL FEDERAL BANK AND PEOPLES TRUST
CIDMOD******************************************************
CIDMOD     IF DTE-CLIENT = 'CID'
CIDMOD        IF COMPANY-NAME = '      COMMERCIAL FEDERAL'
CIDMOD           IF (CF-THIRD-AGENT  = '0000713100') OR
CIDMOD              (CF-FOURTH-AGENT = '0000713100')
CIDMOD              CONTINUE
CIDMOD           ELSE
CIDMOD              GO TO 0250-READ-CERT-MASTER                         
CIDMOD           END-IF
061802        ELSE
061802           IF COMPANY-NAME = '        PEOPLES TRUST'
061802              IF CI-ACCOUNT = '0000845800'
061802                 CONTINUE
061802              ELSE
061802                 GO TO 0250-READ-CERT-MASTER
061802              END-IF
061802           ELSE
080702              IF COMPANY-NAME = '        SUNFLOWER BANK'
080702                 IF (CF-THIRD-AGENT  = '0000681700') OR
080702                    (CF-FOURTH-AGENT = '0000681700')
080702                    CONTINUE
080702                 ELSE
080702                    GO TO 0250-READ-CERT-MASTER
080702                 END-IF
061206              ELSE
061206                 IF COMPANY-NAME = '      FIRST PREMIER BANK'
061206                    IF CI-ACCOUNT = '0001107200'
061206                       CONTINUE
061206                    ELSE
061206                       GO TO 0250-READ-CERT-MASTER
061206                    END-IF
061206                 END-IF
080702              END-IF
080702           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
CIDMOD******************************************************
02042      MOVE CIR-CONTROL                 TO PRIOR-CERT-IN-CNTRL.     
02043                                                                   
02044  0299-READ-CERT-X.                                                
02045      EXIT.                                                        
02046  EJECT                                                            
02047 ******************************************************************
02048 ***     R E A D   P E N D I N G   F I L E   R O U T I N E      ***
02049 ******************************************************************
02050                                                                   
02051  0300-READ-PENDING-TRANSACTIONS.                                  
02052                                                                   
02053      READ PENDING-EXTRACT                                         
02054                    AT END GO TO 9910-EOJ-1.                       
02055                                                                   
02056      MOVE SPACE                    TO PENDING-RECORD-TYPE.        
02057                                                                   
02058      IF NOT ED-PENDING-BUSINESS  AND                              
02059         NOT ED-PENDING-CLAIMS  AND                                
02060         NOT ED-CERT-CHANGES                                       
02061            DISPLAY 'INVALID EXTRACT TYPE INTO ECS010 - TYPE = '   
02062                                                ED-RECORD-TYPE     
02063            MOVE 0301                        TO WS-ABEND-CODE      
02064            MOVE WS-ABEND-CODE        TO WS-RETURN-CODE            
02065            GO TO ABEND-PGM.                                       
02066                                                                   
02067      PERFORM 0310-SET-UP-MATCHING-KEY THRU 0310-EXIT.             
02068                                                                   
02069      IF DTE-CLIENT = 'NCL'                                        
02070          IF PEND-MATCH-EFF-DTE LESS THAN 19400101                 
02071              DISPLAY 'TRANS DROPPED: ' PENDING-MATCH-CONTROL      
02072                      '  TRANS TYPE: ' PENDING-RECORD-TYPE         
02073              GO TO 0300-READ-PENDING-TRANSACTIONS.                
02074                                                                   
02075      IF PENDING-MATCH-CONTROL  LESS THAN  PRIOR-MATCH-CNTRL       
02076          DISPLAY 'SEQUENCE ERROR ON TRANSACTION FILE'             
02077          DISPLAY ' PRIOR SEQ.   = '  PRIOR-MATCH-CNTRL            
02078          DISPLAY ' CURRENT SEQ. = '  PENDING-MATCH-CONTROL        
02079          MOVE 0603                    TO WS-ABEND-CODE            
02080          MOVE WS-ABEND-CODE        TO WS-RETURN-CODE              
02081          GO TO ABEND-PGM.                                         
02082                                                                   
02083      ADD +1 TO TR-CNT.                                            
02084                                                                   
02085      IF ISSUE-TRANSACTION   OR                                    
02086         CANCEL-TRANSACTION  OR                                    
02087         CLAIM-TRANSACTION   OR                                    
02088         RESERVE-TRANSACTION OR                                    
02089         UPDATE-TRANSACTION                                        
02090          MOVE PENDING-MATCH-CONTROL   TO PRIOR-MATCH-CNTRL        
02091          GO TO 0330-MATCH-PENDING-TO-CERT.                        
02092                                                                   
02093      DISPLAY 'INVALID TRANSACTION INTO ECS010 '.                  
02094                                                                   
02095      DISPLAY 'INVALID TRANSACTION INTO ECS010 '.                  
02096      DISPLAY '  MATCH CONTROL: ' PENDING-MATCH-CONTROL ' TYPE: (' 
02097              PENDING-RECORD-TYPE ')'.                             
02098                                                                   
02099      MOVE 0301                   TO WS-ABEND-CODE.                
02100      MOVE WS-ABEND-CODE        TO WS-RETURN-CODE                  
02101      GO TO ABEND-PGM.                                             
02102                                  EJECT                            
02103  0310-SET-UP-MATCHING-KEY.                                        
02104                                                                   
02105      IF ED-PENDING-BUSINESS                                       
02106          MOVE ED-DATA-AREAS      TO PENDING-BUSINESS              
02107          MOVE PB-CARRIER         TO PEND-MATCH-CARRIER            
02108          MOVE PB-GROUPING        TO PEND-MATCH-GROUPING           
02109          MOVE PB-STATE           TO PEND-MATCH-STATE              
02110          MOVE PB-ACCOUNT         TO PEND-MATCH-ACCOUNT            
02111          MOVE PB-CERT-EFF-DT     TO DC-BIN-DATE-1                 
02112          MOVE PB-CERT-NO         TO PEND-MATCH-CERT-NO            
02113          MOVE PB-RECORD-TYPE     TO PENDING-RECORD-TYPE.          
02114                                                                   
02115      IF ED-PENDING-CLAIMS                                         
02116          ADD +1                  TO W-PENDING-CLMS                
02117          MOVE ED-DATA-AREAS      TO PENDING-CLAIMS                
02118          MOVE PC-CARRIER         TO PEND-MATCH-CARRIER            
02119          MOVE PC-GROUPING        TO PEND-MATCH-GROUPING           
02120          MOVE PC-STATE           TO PEND-MATCH-STATE              
02121          MOVE PC-ACCOUNT         TO PEND-MATCH-ACCOUNT            
02122          MOVE PC-CERT-EFF-DT     TO DC-BIN-DATE-1                 
02123          MOVE PC-CERT-NO         TO PEND-MATCH-CERT-NO            
02124          IF PC-CLAIMS                                             
02125              MOVE '3'            TO PENDING-RECORD-TYPE           
02126          ELSE                                                     
02127              IF PC-RESERVES                                       
02128                  MOVE '4'        TO PENDING-RECORD-TYPE.          
02129                                                                   
02130      IF ED-CERT-CHANGES                                           
02131          MOVE ED-DATA-AREAS      TO PENDING-MAINT-TO-CERT-FILE    
02132          MOVE CC-CARRIER         TO PEND-MATCH-CARRIER            
02133          MOVE CC-GROUPING        TO PEND-MATCH-GROUPING           
02134          MOVE CC-STATE           TO PEND-MATCH-STATE              
02135          MOVE CC-ACCOUNT         TO PEND-MATCH-ACCOUNT            
02136          MOVE CC-CERT-EFF-DT     TO DC-BIN-DATE-1                 
02137          MOVE CC-CERT-NO         TO PEND-MATCH-CERT-NO            
02138          MOVE '5'                TO PENDING-RECORD-TYPE.          
02139                                                                   
02140      MOVE ' '                    TO DC-OPTION-CODE.               
02141      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
02142      MOVE DC-GREG-DATE-CYMD      TO PEND-MATCH-EFF-DTE.           
02143                                                                   
02144  0310-EXIT.                                                       
02145      EXIT.                                                        
02146                                                                   
02147 ******************************************************************
02148 ***    M A T C H   P E N D I N G   F I L E   T O   C E R T     ***
02149 ******************************************************************
02150                                                                   
02151  0330-MATCH-PENDING-TO-CERT.                                      
02152                                                                   
02153      IF PENDING-MATCH-CONTROL  LESS THAN  CR-FULL-CONTROL         
02154          GO TO 0390-CERT-MISMATCH-ERROR.                          
02155                                                                   
02156      IF PENDING-MATCH-CONTROL  EQUAL   CR-FULL-CONTROL            
02157        IF CLAIM-TRANSACTION                                       
02158          IF NOT PC-CHARGEBLE-EXPENSE  AND                         
02159             NOT PC-NON-CHARGEBLE-EXPENSE                          
02160              IF ((PC-CLAIM-TYPE = '1' OR '3') AND CR-LFTYP = '00')
02161                                       OR                          
02162                 ((PC-CLAIM-TYPE = '2' OR '4') AND CR-AHTYP = '00')
02163                   GO TO 0390-CERT-MISMATCH-ERROR.                 
02164                                                                   
02165      IF PENDING-MATCH-CONTROL  EQUAL   CR-FULL-CONTROL            
02166        IF RESERVE-TRANSACTION                                     
02167          IF ((PC-CLAIM-TYPE = '1' OR '3') AND CR-LFTYP = '00')    
02168                                   OR                              
02169             ((PC-CLAIM-TYPE = '2' OR '4') AND CR-AHTYP = '00')    
02170               GO TO 0390-CERT-MISMATCH-ERROR.                     
02171                                                                   
02172      IF PENDING-MATCH-CONTROL  EQUAL   CR-FULL-CONTROL            
02173          IF ISSUE-TRANSACTION                                     
02174              GO TO 0380-DUPLICATE-CERT-ERROR                      
02175          ELSE                                                     
02176              GO TO 0350-MATCH-PENDING-TO-ACCOUNT.                 
02177                                                                   
02178      IF PENDING-MATCH-CONTROL  NOT LESS THAN  CIR-CONTROL         
02179          PERFORM 0400-WRITE-CERT-RECORD THRU 0499-WRITE-CERT-X    
02180          PERFORM 0200-CERT-MASTER-READ-ROUTINE THRU               
02181                                                0299-READ-CERT-X   
02182          GO TO 0330-MATCH-PENDING-TO-CERT.                        
02183                                                                   
02184      IF ISSUE-TRANSACTION                                         
02185          PERFORM 0400-WRITE-CERT-RECORD THRU 0499-WRITE-CERT-X    
02186      ELSE                                                         
02187          GO TO 0390-CERT-MISMATCH-ERROR.                          
02188                                                                   
02189  0350-MATCH-PENDING-TO-ACCOUNT.                                   
02190                                                                   
02191      IF PENDING-ACCOUNT-CONTROL LESS THAN AM-CONTROL-A            
CIDMOD       ADD +1  TO ERROR-COUNT
CIDMOD         MOVE    '*** MISMATCH BETWEEN PENDING TRAN AND ACCOUNT'
CIDMOD           TO  DL-FULL
CIDMOD             PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT
CIDMOD         MOVE    ' PENDING CONTROL = '
CIDMOD           TO  DIS-LINE-REASON
CIDMOD         MOVE  PENDING-MATCH-CONTROL
CIDMOD           TO  DIS-LINE-REC
CIDMOD             PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT
CIDMOD         MOVE    ' KEY OF PENDING REC BYPASSED - '
CIDMOD           TO  DIS-LINE-REASON
CIDMOD         MOVE  PENDING-MATCH-CONTROL
CIDMOD           TO  DIS-LINE-REC
CIDMOD             PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT
CIDMOD         MOVE  SPACES
CIDMOD           TO  DISPLAY-LINE
CIDMOD             PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT
*****************
02192          DISPLAY '*** MISMATCH BETWEEN PENDING TRAN AND ACCOUNT'  
02193          DISPLAY '    PENDING CONTROL = ' PENDING-MATCH-CONTROL   
02106          DISPLAY 'PENDING REC BYPASSED - ' PENDING-MATCH-CONTROL
02194          DISPLAY 'TRAN TYPE =   '  PENDING-RECORD-TYPE            
02195          DISPLAY '  TYPE  1=ISS   2=CAN   3=CLM   4=RSV   5=CHG'  
02196          DISPLAY '    ACCOUNT CONTROL = '                         
02197          AM-MSTR-CNTRL AM-EFFECT-DT                               
02198          IF DTE-CLIENT = 'UAL'  OR  'NCL' OR 'CID'                
02199             DISPLAY 'TRANS BYPASSED.... CLIENT = ' DTE-CLIENT     
02200             GO TO 0300-READ-PENDING-TRANSACTIONS                  
02201          ELSE                                                     
02202             MOVE 'MISMATCH BETWEEN PENDING TRAN AND ACCOUNT'      
02203                                       TO WS-ABEND-MESSAGE         
02204             MOVE 0302 TO WS-ABEND-CODE                            
02205             MOVE WS-ABEND-CODE        TO WS-RETURN-CODE           
02206             GO TO ABEND-PGM.                                      
02207                                                                   
02208      IF PENDING-ACCOUNT-CONTROL GREATER AM-CONTROL-A              
02209          PERFORM 0100-ACCT-MASTER-READ-ROUTINE THRU               
02210                                               0199-READ-ACCOUNT-X 
02211          GO TO 0350-MATCH-PENDING-TO-ACCOUNT.                     
02212                                                                   
02213      IF PEND-MATCH-EFF-DTE NOT LESS THAN AM-EFFECT-DT  AND        
02214         PEND-MATCH-EFF-DTE LESS THAN AM-EXPIRE-DT                 
02215          GO TO 0500-APPLY-PENDING-BUSINESS.                       
02216                                                                   
02217      PERFORM 0100-ACCT-MASTER-READ-ROUTINE THRU                   
02218                                        0199-READ-ACCOUNT-X.       
02219      GO TO 0350-MATCH-PENDING-TO-ACCOUNT.                         
02220                                                                   
02221                                                                   
02222  0370-ACCOUNT-MISMATCH-ERROR.                                     
02223                                                                   
CIDMOD     DISPLAY 'MISMATCH ACCOUNT ERROR'                             
CIDMOD     DISPLAY '    ACCOUNT CONTROL = ' AM-MSTR-CNTRL AM-EFFECT-DT.
02224      MOVE 'MISMATCH ACCOUNT ERROR'                                
02225                                TO WS-ABEND-MESSAGE.               
02226      MOVE 0302 TO WS-ABEND-CODE.                                  
02137          DISPLAY '******************************'
02138          DISPLAY '***** ERROR LOCATION 009 *****'
02139          DISPLAY '******************************'
02227      MOVE WS-ABEND-CODE        TO WS-RETURN-CODE                  
02228      GO TO ABEND-PGM.                                             
02229                                                                   
02230                                                                   
02231  0380-DUPLICATE-CERT-ERROR.                                       
02232                                                                   
02233      IF PB-I-OB  OR  PB-I-SUMMARY                                 
02234          GO TO 0300-READ-PENDING-TRANSACTIONS.                    
02235                                                                   
02236      ADD +1 TO DUP-CNT.                                           
02237                                                                   
02238      IF DUP-CNT NOT GREATER THAN DUP-MAX                          
02239          MOVE CR-FULL-CONTROL           TO DUPE-ENTRY (DUP-CNT)   
02240          COMPUTE DUPE-LF-PRM (DUP-CNT) = PB-I-LF-PREMIUM-AMT +    
02241                                          PB-I-LF-ALT-PREMIUM-AMT  
02242          MOVE  PB-I-AH-PREMIUM-AMT      TO  DUPE-AH-PRM (DUP-CNT) 
02243          MOVE  PB-I-ENTRY-STATUS        TO  DUPE-STATUS (DUP-CNT) 
02244          GO TO 0300-READ-PENDING-TRANSACTIONS.                    
CIDMOD                                                                  
02246      DISPLAY 'MORE THAN ' DUP-MAX ' DUPLICATE ISSUES PROCESSED'.  
02245      GO TO 0300-READ-PENDING-TRANSACTIONS.                        
CIDMOD
02247 *    MOVE 0301 TO WS-ABEND-CODE.                                  
02248 *    MOVE WS-ABEND-CODE        TO WS-RETURN-CODE                  
02249 *    GO TO ABEND-PGM.                                             
02250                                                                   
02251                                                                   
02252  0390-CERT-MISMATCH-ERROR.                                        
02253                                                                   
02254      IF ISSUE-TRANSACTION  OR                                     
02255         CANCEL-TRANSACTION  OR                                    
02256         CLAIM-TRANSACTION                                         
02257          ADD +1                  TO  MISMATCH-COUNT.              
CIDMOD
CIDMOD     IF DTE-CLIENT  =  'CID'  OR  'CSO'
CIDMOD       ADD  +1  TO  ERROR-COUNT
CIDMOD         MOVE    '*** MISMATCH BETWEEN CERT AND TRANS'
CIDMOD           TO  DL-FULL
CIDMOD             PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT
CIDMOD         MOVE    ' TRANS CONTROL = '
CIDMOD           TO  DIS-LINE-REASON
CIDMOD         MOVE  PENDING-MATCH-CONTROL
CIDMOD           TO  DIS-LINE-REC
CIDMOD             PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT
CIDMOD         MOVE    ' TRANS TYPE    = '
CIDMOD           TO  DIS-LINE-REASON
CIDMOD         MOVE  PENDING-RECORD-TYPE
CIDMOD           TO  DIS-LINE-REC
CIDMOD             PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT
CIDMOD         MOVE '  TYPE: 1=ISS   2=CAN   3=CLM   4=RSV   5=CHG'
CIDMOD           TO  DL-FULL
CIDMOD             PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT
CIDMOD         MOVE    ' CERT IN CTL   = '
CIDMOD           TO  DIS-LINE-REASON
CIDMOD         MOVE  CIR-CONTROL
CIDMOD           TO  DIS-LINE-REC
CIDMOD             PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT
CIDMOD         MOVE    ' BYPASSED TRANS  = '
CIDMOD           TO  DIS-LINE-REASON
CIDMOD         MOVE  PENDING-MATCH-CONTROL
CIDMOD           TO  DIS-LINE-REC
CIDMOD             PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT
CIDMOD         MOVE  SPACES
CIDMOD           TO  DISPLAY-LINE
CIDMOD             PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT
CIDMOD         DISPLAY 'BYPASSED TRANS = ' PENDING-MATCH-CONTROL.
02258                                                                   
02259      MOVE PENDING-MATCH-CONTROL  TO  DTL-MATCH-CONTROL.           
02260      MOVE PENDING-RECORD-TYPE    TO  DTL-REC-TYPE.                
02261                                                                   
02262      IF ISSUE-TRANSACTION                                         
02263          MOVE 'ISS'              TO  DTL-REC-TYPE-DESC            
02264      ELSE                                                         
02265          IF CANCEL-TRANSACTION                                    
02266              MOVE 'CAN'          TO  DTL-REC-TYPE-DESC            
02267          ELSE                                                     
02268              IF CLAIM-TRANSACTION                                 
02269                  MOVE 'CLM'      TO  DTL-REC-TYPE-DESC            
02270              ELSE                                                 
02271                  IF RESERVE-TRANSACTION                           
02272                      MOVE 'RES'  TO  DTL-REC-TYPE-DESC            
02273                  ELSE                                             
02274                      IF UPDATE-TRANSACTION                        
02275                          MOVE 'CHG'                               
02276                                  TO  DTL-REC-TYPE-DESC.           
02277                                                                   
02278      MOVE CIR-CONTROL            TO  DTL-CERT-IN-CONTROL.         
02279      MOVE CR-FULL-CONTROL        TO  DTL-CERT-OUT-CONTROL.        
02280      MOVE 'DROPPED'              TO  DTL-STATUS.                  
02281                                                                   
02282      IF ISSUE-TRANSACTION                                         
02283          MOVE PB-I-LF-PREMIUM-AMT                                 
02284                                  TO  MD2-LF-PREM                  
02285          MOVE PB-I-LF-ALT-PREMIUM-AMT                             
02286                                  TO  MD2-LF-ALT-PREM              
02287          MOVE PB-I-AH-PREMIUM-AMT                                 
02288                                  TO  MD2-AH-PREM                  
02289          ADD PB-I-LF-PREMIUM-AMT TO W-MM-LF-PRM                   
02290          ADD PB-I-LF-ALT-PREMIUM-AMT TO W-MM-LF-PRM               
02291          ADD PB-I-AH-PREMIUM-AMT TO W-MM-AH-PRM                   
02292      ELSE                                                         
02293          IF CANCEL-TRANSACTION                                    
02294              MOVE PB-C-LF-CANCEL-AMT                              
02295                                  TO  MD3-LF-REFUND                
02296              MOVE PB-C-AH-CANCEL-AMT                              
02297                                  TO  MD3-AH-REFUND                
02298              ADD PB-C-LF-CANCEL-AMT TO W-MM-LF-RFD                
02299              ADD PB-C-AH-CANCEL-AMT TO W-MM-AH-RFD                
02300          ELSE                                                     
02301              IF CLAIM-TRANSACTION                                 
02302                  IF PC-LF-CLAIM  OR  PC-OB-LF-CLAIM               
02303                      MOVE PC-CLAIM-PAYMENT  TO MD4-LF-CLAIM       
02304                      MOVE ZEROS             TO MD4-AH-CLAIM       
02305                      ADD PC-CLAIM-PAYMENT TO W-MM-LF-CLM          
02306                  ELSE                                             
02307                      MOVE ZEROS             TO MD4-LF-CLAIM       
02308                      MOVE PC-CLAIM-PAYMENT  TO MD4-AH-CLAIM       
02309                      ADD PC-CLAIM-PAYMENT TO W-MM-AH-CLM.         
02310                                                                   
02311      IF  MISMATCH-LINE-COUNT GREATER THAN +56                     
02312          ADD +1                  TO  MISMATCH-PAGE-NUMBER         
02313          MOVE MISMATCH-PAGE-NUMBER                                
02314                                  TO  TR-PG                        
02315          MOVE MISMATCH-HEADING-1 TO  MISMATCH-RPT                 
02316          MOVE MR-CTL             TO  X                            
02317          MOVE X TO LCP-ASA                                        
02318          PERFORM LCP-WRITE-POS-MISMATCH-RPT                       
02319              THRU LCP-WRITE-END-MISMATCH-RPT                      
02320          MOVE TR-HD-2            TO  MISMATCH-RPT                 
02321          MOVE MR-CTL             TO  X                            
02322          MOVE X TO LCP-ASA                                        
02323          PERFORM LCP-WRITE-POS-MISMATCH-RPT                       
02324              THRU LCP-WRITE-END-MISMATCH-RPT                      
02325          MOVE TR-HD-3            TO  MISMATCH-RPT                 
02326          MOVE MR-CTL             TO  X                            
02327          MOVE X TO LCP-ASA                                        
02328          PERFORM LCP-WRITE-POS-MISMATCH-RPT                       
02329              THRU LCP-WRITE-END-MISMATCH-RPT                      
02330          MOVE MISMATCH-HEADING-2 TO  MISMATCH-RPT                 
02331          MOVE MR-CTL             TO  X                            
02332          MOVE X TO LCP-ASA                                        
02333          PERFORM LCP-WRITE-POS-MISMATCH-RPT                       
02334              THRU LCP-WRITE-END-MISMATCH-RPT                      
02335          MOVE +6                 TO  MISMATCH-LINE-COUNT.         
02336                                                                   
02337      MOVE MISMATCH-DETAIL        TO  MISMATCH-RPT.                
02338      MOVE MR-CTL                 TO  X.                           
02339      ADD +2                      TO  MISMATCH-LINE-COUNT.         
02340                                                                   
02341      MOVE X TO LCP-ASA                                            
02342      PERFORM LCP-WRITE-POS-MISMATCH-RPT                           
02343          THRU LCP-WRITE-END-MISMATCH-RPT.                         
02344                                                                   
02345      IF ISSUE-TRANSACTION                                         
02346          MOVE MISMATCH-DETAIL-2  TO  MISMATCH-RPT                 
02347          MOVE MR-CTL             TO  X                            
02348          ADD +1                  TO  MISMATCH-LINE-COUNT          
02349          MOVE X TO LCP-ASA                                        
02350          PERFORM LCP-WRITE-POS-MISMATCH-RPT                       
02351              THRU LCP-WRITE-END-MISMATCH-RPT.                     
02352                                                                   
02353      IF CANCEL-TRANSACTION                                        
02354          MOVE MISMATCH-DETAIL-3  TO  MISMATCH-RPT                 
02355          MOVE MR-CTL             TO  X                            
02356          ADD +1                  TO  MISMATCH-LINE-COUNT          
02357          MOVE X TO LCP-ASA                                        
02358          PERFORM LCP-WRITE-POS-MISMATCH-RPT                       
02359              THRU LCP-WRITE-END-MISMATCH-RPT.                     
02360                                                                   
02361      IF CLAIM-TRANSACTION                                         
02362          MOVE MISMATCH-DETAIL-4  TO  MISMATCH-RPT                 
02363          MOVE MR-CTL             TO  X                            
02364          ADD +1                  TO  MISMATCH-LINE-COUNT          
02365          MOVE X TO LCP-ASA                                        
02366          PERFORM LCP-WRITE-POS-MISMATCH-RPT                       
02367              THRU LCP-WRITE-END-MISMATCH-RPT.                     
02368                                                                   
02369      MOVE SPACES                 TO  MISMATCH-RPT.                
02370                                                                   
02371      GO TO 0300-READ-PENDING-TRANSACTIONS.                        
02372                                                                   
       0395-GET-CERT-TRLRS.

           MOVE WS-BANK-INIT           TO WS-BANK-COMMISSIONS
           MOVE DTE-CLASIC-COMPANY-CD  TO CS-COMPANY-CD
           MOVE CR-CARRIER             TO CS-CARRIER
           MOVE CR-GROUPING            TO CS-GROUPING
           MOVE CR-STATE               TO CS-STATE
           MOVE CR-ACCOUNT             TO CS-ACCOUNT
           MOVE WS-CR-BIN-DATE         TO CS-CERT-EFF-DT
           MOVE CR-CERT-NO             TO CS-CERT-NO
           MOVE 'A'                    TO CS-TRAILER-TYPE

           READ ELCRTT

           IF ELCRTT-FILE-STATUS = '00'
              MOVE CS-BANK-COMMISSION-AREA
                                       TO WS-BANK-COMMISSIONS
           ELSE
              IF ELCRTT-FILE-STATUS NOT = '10' AND '23'
                 DISPLAY ' BAD READ    ELCRTT ' ELCRTT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0395-EXIT.
           EXIT.

02374 ******************************************************************
02375 ***       W R I T E   C E R T I F I C A T E   R E C O R D      ***
02376 ******************************************************************
02377                                                                   
02378  0400-WRITE-CERT-RECORD.                                          
02379                                                                   
02380      IF CERTIFICATE-RECORD = LOW-VALUES                           
02381          GO TO 0499-WRITE-CERT-X.                                 
02382                                                                   
02383      IF CR-ACCT-CONTROL LESS THAN AM-CONTROL-A                    
02384          DISPLAY '*** MISMATCH BETWEEN CERT MASTER AND ACCOUNT'   
02385          DISPLAY '    CERT. CONTROL = ' CR-FULL-CONTROL           
CIDMOD         ADD  +1  TO  ERROR-COUNT
CIDMOD         MOVE    '*** CERT ACCT LESS THAN TRANS ACCT'
CIDMOD           TO  DL-FULL
CIDMOD             PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT
CIDMOD         MOVE    ' CERT. CONTROL = '
CIDMOD           TO  DIS-LINE-REASON
CIDMOD         MOVE  CR-FULL-CONTROL
CIDMOD           TO  DIS-LINE-REC
CIDMOD             PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT
CIDMOD         MOVE  SPACES
CIDMOD           TO  DISPLAY-LINE
CIDMOD             PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT
CIDMOD            WRITE CERT-OUT-RECORD FROM CERTIFICATE-RECORD
CIDMOD            ADD +1 TO CMOT-CNT
CIDMOD            PERFORM 0200-CERT-MASTER-READ-ROUTINE
CIDMOD               GO TO 0400-WRITE-CERT-RECORD.
CIDMOD
CIDMOD*          GO TO 0370-ACCOUNT-MISMATCH-ERROR.                     
02387                                                                   
02388      IF CR-ACCT-CONTROL GREATER THAN AM-CONTROL-A                 
02389          PERFORM 0100-ACCT-MASTER-READ-ROUTINE THRU               
02390                                               0199-READ-ACCOUNT-X 
02391          GO TO 0400-WRITE-CERT-RECORD.                            
02392                                                                   
02393      IF CR-DT LESS THAN AM-EFFECT-DT  OR                          
02394         CR-DT NOT LESS THAN AM-EXPIRE-DT                          
02395          PERFORM 0100-ACCT-MASTER-READ-ROUTINE THRU               
02396                                               0199-READ-ACCOUNT-X 
02397          GO TO 0400-WRITE-CERT-RECORD.                            
02398                                                                   
02399      PERFORM 3000-REINSURE-ROUTINE THRU 3099-REINSURE-ROUTINE-X.  
02400                                                                   
02401      IF CR-LFTYP = ZEROS  AND                                     
02402         CR-DTHEXP NOT = ZEROS                                     
02403          ADD +1                     TO W-LFTYP-99                 
02404          MOVE '99'                  TO CR-LFTYP  CLAS-LOOK        
02405          PERFORM 8300-LIFE-TYPE-LOOKUP THRU 8399-LIFE-LOOKUP-X.   
02406                                                                   
02407      IF CR-AHTYP = ZEROS  AND                                     
02408         CR-DISEXP NOT = ZEROS                                     
02409          ADD +1                     TO W-AHTYP-99                 
02410          MOVE '99'                  TO CR-AHTYP  CLAS-LOOK        
02411          PERFORM 8400-A-H-TYPE-LOOKUP THRU 8499-A-H-LOOKUP-X.     
02412                                                                   
02413      PERFORM 4000-EP-BUMP-RTN THRU 4099-EP-BUMP-RTN-X.            
02414                                                                   
02415      IF CR-LFTYP = '99'                                           
02416          MOVE '00'                  TO CR-LFTYP.                  
02417      IF CR-AHTYP = '99'                                           
02418          MOVE '00'                  TO CR-AHTYP.                  
02419                                                                   
02420      IF CR-ISS-MICROFILM-NO NOT NUMERIC                           
02421          MOVE ZEROS                 TO CR-ISS-MICROFILM-NO.       
02422                                                                   
02423      IF CR-CAN-MICROFILM-NO NOT NUMERIC                           
02424          MOVE ZEROS                 TO CR-CAN-MICROFILM-NO.       
02425                                                                   
02426      IF  CR-POLICY-IS-ACTIVE                                      
02427          ADD +1                  TO W-POLICY-ACTIVE-CTR           
02428          IF  CR-POLICY-IS-REISSUE         
122002             OR CR-POLICY-IS-MONTHLY
02429              ADD +1              TO W-POLICY-REISSUE-CTR          
02430          ELSE                                                     
02431              IF  CR-POLICY-IS-REIN-ONLY                           
02432                  ADD +1          TO W-POLICY-REIN-CTR             
02433              ELSE                                                 
02434                  NEXT SENTENCE                                    
02435      ELSE                                                         
02436          ADD +1                  TO W-POLICY-INACTIVE-CTR.        
02437                                                                   
02438      IF DTE-PGM-OPT = '1'                                         
02439        IF REIN-IS-RISK-PREMIUM  AND                               
02440           CR-ENTRY-DATE NOT = EOM-RUN-DATE                        
02441          IF NOT CR-POLICY-IS-REISSUE  AND                         
02442             NOT CR-POLICY-IS-DECLINED  AND                        
02443             NOT CR-POLICY-IS-VOID                                 
122002            AND NOT CR-POLICY-IS-MONTHLY
02444              MOVE '*'            TO RISK-PREM-FLAG                
02445              PERFORM 3100-REINSURE-ISSUE THRU                     
02446                      3199-REINSURE-ISSUE-X.                       
02447                                                                   
02448      COPY ELCCRTM2.                                               
02449                                                                   
02450      IF DTE-PGM-OPT = '1'                                         
02451          WRITE CERT-OUT-RECORD FROM CERTIFICATE-RECORD            
02452          ADD +1 TO CMOT-CNT.                                      
02453                                                                   
02454      MOVE SPACE                  TO REIN-FLG                      
02455                                     RISK-REIN-FLAG                
02456                                     RISK-PREM-FLAG                
02457                                     RC-COMM-FLG.                  
02458                                                                   
02459  0499-WRITE-CERT-X.                                               
02460      EXIT.                                                        
02461  EJECT                                                            
02462 ******************************************************************
02463 ***     A P P L Y   P E N D I N G   T R A N S A C T I O N S    ***
02464 ******************************************************************
02465                                                                   
02466  0500-APPLY-PENDING-BUSINESS.                                     
02467                                                                   
02468      IF CANCEL-TRANSACTION                                        
02469          GO TO 0600-PROCESS-CANCELLATIONS.                        
02470                                                                   
02471      IF CLAIM-TRANSACTION                                         
02472          GO TO 0700-PROCESS-CLAIMS.                               
02473                                                                   
02474      IF RESERVE-TRANSACTION                                       
02475          GO TO 0800-PROCESS-RESERVES.                             
02476                                                                   
02477      IF UPDATE-TRANSACTION                                        
02478          GO TO 0900-PROCESS-CERT-CHANGES.                         
02479                                                                   
02480 ******************************************************************
02481 ***     P R O C E S S   I S S U E   T R A N S A C T I O N S    ***
02482 ******************************************************************
02483                                                                   
02484  0505-PROCESS-ISSUES.                                             
02485                                                                   
02486      MOVE INITIALIZED-CERTIFICATE TO CERTIFICATE-RECORD.          
02487      PERFORM CERT-DATE-LOAD.                                      
02488                                                                   
02489      ADD +1                      TO  W-POL-ISSUES.                
02490                                                                   
02491      MOVE PB-COMPANY-CD          TO  CR-COMPANY-CD.               
02492      MOVE PB-CARRIER             TO  CR-CARRIER.                  
02493      MOVE PB-GROUPING            TO  CR-GROUPING.                 
02494      MOVE PB-STATE               TO  CR-STATE.                    
02495      MOVE PB-ACCOUNT             TO  CR-ACCOUNT.                  
02496      MOVE PB-CERT-EFF-DT         TO  DC-BIN-DATE-1
111204                                     WS-CR-BIN-DATE
02497      MOVE ' '                    TO  DC-OPTION-CODE.              
02498      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
02499      MOVE DC-GREG-DATE-CYMD      TO  CR-DT                        
02500                                      WS-CR-DT-N.                  
02501      MOVE PB-CERT-NO             TO  CR-CERT-NO.                  
02502                                                                   
02503      MOVE PB-I-NAME              TO  CR-NAME.                     
02504      MOVE PB-I-AGE               TO  CR-AGE.                      
02505      MOVE PB-I-INSURED-SEX       TO  CR-SEX.                      
02506      MOVE PB-I-SOC-SEC-NO        TO  CR-SOC-SEC.                  
02507                                                                   
02508      IF PB-I-JOINT-COVERAGE                                       
02509          MOVE PB-I-JOINT-INSURED TO  CR-JOINT-NAME                
02510          MOVE PB-I-JOINT-AGE     TO  CR-JOINT-AGE                 
02511      ELSE                                                         
02512         IF (DTE-CLIENT EQUAL 'BOA')  AND                          
02513            (PB-I-JOINT-INSURED NOT EQUAL SPACES)                  
02514              MOVE PB-I-JOINT-INSURED TO  CR-JOINT-NAME.           
02515                                                                   
02516      IF PB-I-SUMMARY  OR                                          
02517         DTE-CLIENT = 'DMD'                                        
02518          MOVE PB-I-LIVES         TO  CR-LIVES.                    
02519                                                                   
02520      IF PB-I-NUM-BILLED NOT NUMERIC                               
02521         MOVE ZEROS               TO PB-I-NUM-BILLED.              
02522                                                                   
02523      IF PB-I-STATE-TAX NOT NUMERIC                                
02524         MOVE ZEROS               TO PB-I-STATE-TAX.               
02525                                                                   
02526      IF PB-I-MUNI-TAX NOT NUMERIC                                 
02527         MOVE ZEROS               TO PB-I-MUNI-TAX.                
011904     IF PB-I-TOT-FEES NOT NUMERIC
011904        MOVE ZEROS               TO PB-I-TOT-FEES
011904     END-IF
040504     IF PB-I-ADDL-CLP NOT NUMERIC
040504        MOVE ZEROS               TO PB-I-ADDL-CLP
040504     END-IF
02528                                                                   
011904     MOVE PB-I-TOT-FEES          TO  CR-MOB-NET-TOT-FEES
040504     MOVE PB-I-ADDL-CLP          TO  CR-ADDL-CLP
020305     MOVE PB-I-CLP-STATE         TO  CR-CLP-STATE
02529      MOVE PB-I-NUM-BILLED        TO  CR-BILLED.                   
02530      MOVE PB-I-STATE-TAX         TO  CR-STATE-TAX.                
02531      MOVE PB-I-RESIDENT-STATE    TO  CR-RESIDENT-STATE.           
02532      MOVE PB-I-RATE-CODE         TO  CR-RATE-CODE.                
02533      MOVE PB-I-MUNI-TAX          TO  CR-MUNI-TAX.                 
02534      MOVE PB-I-LOAN-APR          TO  CR-APR.                      
02535      MOVE PB-I-PAY-FREQUENCY     TO  CR-PMT-FREQ.                 
02536      MOVE PB-I-LOAN-TERM         TO  CR-LOAN-TERM.                
02537                                                                   
02538      IF PB-I-RATE-CLASS-OVRD = SPACES                             
02539          MOVE PB-I-RATE-CLASS    TO  CR-RATING-CLASS              
02540      ELSE                                                         
02541          MOVE PB-I-RATE-CLASS-OVRD TO CR-RATING-CLASS.            
02542                                                                   
02543      MOVE PB-I-POLICY-FORM-NO    TO  CR-POLICY-FORM-NO.           
02544      MOVE PB-I-BUSINESS-TYPE     TO  CR-GRPTYP.                   
02545                                                                   
02546      IF PB-I-INDV-GRP-OVRD = SPACES                               
02547          MOVE PB-I-INDV-GRP-CD   TO  CR-IND-GRP                   
02548      ELSE                                                         
02549          MOVE PB-I-INDV-GRP-OVRD TO  CR-IND-GRP.                  
02550                                                                   
02551      IF CR-IND-GRP = 'I'  OR  '1'                                 
02552          MOVE '1'                TO  CR-IND-GRP                   
02553      ELSE                                                         
02554          MOVE '2'                TO  CR-IND-GRP.                  
02555                                                                   
02556      IF PB-I-SKIP-CODE = '1'                                      
02557          MOVE 01                 TO  CR-SKIP.                     
02558      IF PB-I-SKIP-CODE = '2'                                      
02559          MOVE 02                 TO  CR-SKIP.                     
02560      IF PB-I-SKIP-CODE = '3'                                      
02561          MOVE 03                 TO  CR-SKIP.                     
02562      IF PB-I-SKIP-CODE = '4'                                      
02563          MOVE 04                 TO  CR-SKIP.                     
02564      IF PB-I-SKIP-CODE = '5'                                      
02565          MOVE 05                 TO  CR-SKIP.                     
02566      IF PB-I-SKIP-CODE = '6'                                      
02567          MOVE 06                 TO  CR-SKIP.                     
02568      IF PB-I-SKIP-CODE = '7'                                      
02569          MOVE 07                 TO  CR-SKIP.                     
02570      IF PB-I-SKIP-CODE = '8'                                      
02571          MOVE 08                 TO  CR-SKIP.                     
02572      IF PB-I-SKIP-CODE = '9'                                      
02573          MOVE 09                 TO  CR-SKIP.                     
02574      IF PB-I-SKIP-CODE = 'A'                                      
02575          MOVE 10                 TO  CR-SKIP.                     
02576      IF PB-I-SKIP-CODE = 'X'                                      
02577          MOVE 11                 TO  CR-SKIP.                     
02578                                                                   
02579      MOVE PB-I-MORT-CODE         TO  CR-MORT.                     
02580      MOVE PB-I-MEMBER-NO         TO  CR-MEMBER-NO.                
02581      MOVE PB-I-LOAN-OFFICER      TO  CR-LOAN-OFFICER.             
02582                                                                   
02583      IF PB-I-SPECIAL-REIN-CODE NOT = SPACE                        
02584          MOVE PB-I-SPECIAL-REIN-CODE TO CR-REIN-SPEC              
02585                                         REIN-SPEC-A               
02586                                         REIN-SPEC-B               
02587                                         REIN-SPEC-C               
02588          MOVE REIN-SPEC          TO CR-REIN-TABLE                 
02589      ELSE                                                         
02590          MOVE AM-REI-TABLE       TO CR-REIN-TABLE.                
02591                                                                   
02592      MOVE PB-I-1ST-PMT-DT        TO  DC-BIN-DATE-1.               
02593      MOVE ' '                    TO  DC-OPTION-CODE.              
02594      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
02595      MOVE DC-GREG-DATE-1-YMD     TO  CR-LOAN-1ST-PMT-DT.          
02596                                                                   
02597      MOVE PB-I-EXTENTION-DAYS    TO  CR-PMT-EXTENSION-DAYS.       
02598                                                                   
02599      IF PB-I-LAST-ADD-ON-DT  =  ZEROS  OR  SPACES                 
02600          MOVE LOW-VALUES          TO CR-LAST-ADD-ON-DT            
02601      ELSE                                                         
02602          MOVE PB-I-LAST-ADD-ON-DT TO CR-LAST-ADD-ON-DT.           
02603                                                                   
02604      IF PB-I-UNDERWRITING-STATUS = 'A'  OR  'D'                   
02605          MOVE 'Y'                TO  CR-UNDERWRITING-CODE.        
02606                                                                   
02607      IF PB-BATCH-ENTRY = SPACE  OR                                
02608         (PB-I-ENTRY-STATUS = '2'  OR  'U')                        
02609          MOVE '1'                TO  CR-ENTRY-STATUS              
02610      ELSE                                                         
02611          MOVE PB-I-ENTRY-STATUS  TO  CR-ENTRY-STATUS.             
02612                                                                   
02613      IF PB-POLICY-IS-DECLINED                                     
02614          MOVE 'D'                TO  CR-ENTRY-STATUS.             
02615                                                                   
02616      IF PB-POLICY-IS-VOIDED                                       
02617          MOVE 'V'                TO  CR-ENTRY-STATUS.             
02618                                                                   
02619 ***  IF DTE-CLIENT NOT = 'CRI'                                    
02620 ***      IF PB-I-PREM-ACCTNG-ONLY                                 
02621 ***          MOVE '1'            TO  CR-ENTRY-STATUS.             
02622                                                                   
02623      MOVE EOM-RUN-DATE           TO  CR-ENTRY-DATE                
02624                                      WS-CR-ENTRY-DATE-N.          
02625                                                                   
02626      IF PB-I-MICROFILM-NO NUMERIC                                 
02627          MOVE PB-I-MICROFILM-NO  TO CR-ISS-MICROFILM-NO.          
02628                                                                   
02629      IF DTE-CLIENT = 'TIH' OR 'FLA' OR 'TII' OR                   
011207        'TMS' OR 'NCL' OR 'HER' OR 'CID' OR 'DCC'
02631          MOVE PB-I-SIG-SW        TO CR-USER-CODE
011207     END-IF
02632                                                                   
02633 *************************************************                 
02634 ***  ADDED IN 06/93 WHEN CERTIFICATE-RECORD   ***                 
02635 ***  WAS CONVERTED TO 1056 BYTES.             ***                 
02636 *************************************************                 
02637      MOVE PB-ENTRY-BATCH         TO  CR-ENTRY-BATCH.              
02638      MOVE PB-I-BENEFICIARY-NAME  TO  CR-BENEFICIARY.              
02639                                                                   
02640      MOVE PB-LAST-MAINT-BY       TO  CR-CSR-CODE.                 
02641                                                                   
02642      MOVE CR-STATE               TO STATE-L.                      
02643                                                                   
02644      PERFORM 8200-STATE-CODE-LOOKUP THRU 8299-STATE-LOOKUP-X.     
02645                                                                   
02646      PERFORM 8600-SETUP-DETAIL-LINE THRU 8689-SETUP-DETAIL-X.     
02647                                                                   
02648 ***************************************************               
02649 ***  WHEN AN ISSUE WAS PROCESSED THAT HAD       ***               
02650 ***  INVALID BENEFIT CODES LOCATED IN BOTH      ***               
02651 ***  BENEFITS,  THE 'ACTION' ON THE DETAIL LIST ***               
02652 ***  LINE WAS BYPASSED - ONLY THE CERTIFICATE,  ***               
02653 ***  THE EFFECTIVE DATE AND THE AGE PRINTED.    ***               
02654 ***************************************************               
02655      IF (PB-INVALID-LIFE AND PB-INVALID-AH)                       
02656          IF CR-LF-POLICY-IS-RESTORE                               
02657              MOVE 'RESTORED  '          TO TRD-LF-ACTION          
02658              GO TO 0530-PROCESS-ISSUE-CONTINUED                   
02659          ELSE                                                     
02660          IF CR-LF-POLICY-IS-REISSUE                               
02661              MOVE 'RE-ISSUED '          TO TRD-LF-ACTION          
02662              GO TO 0530-PROCESS-ISSUE-CONTINUED                   
02663          ELSE                                                     
02664          IF CR-LF-IS-REIN-ONLY                                    
02665              MOVE 'REIN. ONLY'          TO TRD-LF-ACTION          
02666              GO TO 0530-PROCESS-ISSUE-CONTINUED                   
02667          ELSE                                                     
02668          IF CR-LF-IS-DECLINED                                     
02669              MOVE 'DECLINED  '          TO TRD-LF-ACTION          
02670              GO TO 0530-PROCESS-ISSUE-CONTINUED                   
02671          ELSE                                                     
02672          IF CR-LF-IS-VOID                                         
02673              MOVE 'VOIDED    '          TO TRD-LF-ACTION          
122002             GO TO 0530-PROCESS-ISSUE-CONTINUED                   
122002         ELSE
122002         IF CR-LF-POLICY-IS-MONTHLY
122002            MOVE 'MONTHLY'       TO TRD-LF-ACTION
122002            GO TO 0530-PROCESS-ISSUE-CONTINUED.
02675                                                                   
02676                                                                   
02677  0510-PROCESS-LIFE-ISSUE.                                         
02678                                                                   
02679      IF PB-INVALID-LIFE                                           
02680          GO TO 0520-PROCESS-A-H-ISSUE.                            
02681                                                                   
02682      MOVE PB-I-LF-BENEFIT-CD     TO  CLAS-LOOK.                   
02683      PERFORM 8300-LIFE-TYPE-LOOKUP THRU 8399-LIFE-LOOKUP-X.       
02684      MOVE CLAS-LOOK              TO  PB-I-LF-BENEFIT-CD.          
02685                                                                   
02686      MOVE PB-I-LF-BENEFIT-CD     TO  CR-LFTYP.                    
02687      MOVE PB-I-LF-TERM           TO  CR-LF-TERM.                  
02688      MOVE PB-I-LF-CRIT-PER       TO  CR-LF-CRIT-PERIOD.           
02689      MOVE PB-I-TERM-IN-DAYS      TO  CR-LF-TERM-IN-DAYS.          
02690      MOVE PB-I-RATE-DEVIATION-LF TO  CR-LF-DEV-CODE.              
02691      MOVE PB-I-RATE-DEV-PCT-LF   TO  CR-LF-DEV-PCT.               
02692                                                                   
02693      MOVE PB-I-LF-BENEFIT-AMT    TO  CR-LFAMT.                    
02694      MOVE PB-I-LF-PREMIUM-AMT    TO  CR-LFPRM.                    
02695      IF PB-I-LF-POLICY-FEE  NUMERIC                               
02696          MOVE PB-I-LF-POLICY-FEE TO  CR-LF-POLICY-FEE.            
02697      MOVE PB-I-LF-PREM-CALC      TO  CR-LFPRM-CALC.               
02698      MOVE PB-I-LF-RATE           TO  CR-LFPRM-RATE.               
02699                                                                   
PEMMOD     IF PB-I-LF-PREM-TAX NUMERIC
PEMMOD        MOVE PB-I-LF-PREM-TAX    TO CR-LF-ISS-PREM-TAX
PEMMOD     END-IF


02700      IF PB-I-LF-REI-RATE NOT = ZEROS                              
103102        IF DTE-CLIENT = 'DCC'
111402           CONTINUE                                 
103102*          PERFORM 0795-DCC-NSP-LF  THRU 0795-EXIT
103102        ELSE   
02701            MOVE PB-I-LF-REI-RATE TO CR-LF-NSP-PRM-RATE           
02702            COMPUTE CR-LF-NSP-PRM ROUNDED =                          
02703                           (CR-LFAMT / 100) * PB-I-LF-REI-RATE    
103102        END-IF
103102     END-IF

02705      IF CLAS-I-EP (CLAS-INDEXL) = 'B'                             
02706          MOVE PB-I-LF-ALT-BENEFIT-AMT TO CR-LFAMT-ALT             
02707          MOVE PB-I-LF-ALT-PREMIUM-AMT TO CR-LFPRM-ALT             
02708          MOVE PB-I-LF-ALT-PREM-CALC   TO CR-LFPRM-CALC-ALT        
02709          MOVE PB-I-LF-ALT-RATE        TO CR-LFPRM-RATE-ALT        
02710          IF PB-I-LF-ALT-REI-RATE NOT = ZEROS                      
02711              COMPUTE CR-LF-NSP-PRM ROUNDED =  CR-LF-NSP-PRM +     
02712                  ((CR-LFAMT-ALT / 100) * PB-I-LF-ALT-REI-RATE).   
02713                                                                   
02714      MOVE PB-I-LF-EXPIRE-DT      TO  DC-BIN-DATE-1.               
02715      MOVE ' '                    TO  DC-OPTION-CODE.              
02716      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
02717      MOVE DC-GREG-DATE-CYMD      TO  CR-LF-EXPIRE-DATE            
02718                                      WS-CR-LF-EXPIRE-DATE-N.      
02719                                                                   
02720      IF CR-ENTRY-STATUS = 'P'                                     
02721          MOVE '1'                TO  CR-LF-CURRENT-STATUS         
02722      ELSE                                                         
02723          MOVE CR-ENTRY-STATUS    TO  CR-LF-CURRENT-STATUS.        
02724                                                                   
02725      MOVE CLAS-I-AB3 (CLAS-INDEXL)  TO TRD-LFTYP.                 
02726      MOVE CR-LF-TERM                TO TRD-LFTRM.                 
02727      COMPUTE TRD-LFPRM-RFND = CR-LFPRM + CR-LFPRM-ALT.            
02728      MOVE 'ISSUE     '              TO TRD-LF-ACTION.             
02729      MOVE CR-LF-CURRENT-STATUS      TO TRD-LF-STATUS.             
02730                                                                   
02731      IF CR-LF-POLICY-IS-RESTORE                                   
02732          MOVE 'RESTORED  '          TO TRD-LF-ACTION.             
02733      IF CR-LF-POLICY-IS-REISSUE                                   
02734          MOVE 'RE-ISSUED '          TO TRD-LF-ACTION.             
02735      IF CR-LF-IS-REIN-ONLY                                        
02736          MOVE 'REIN. ONLY'          TO TRD-LF-ACTION.             
02737                                                                   
02738      IF CR-LF-IS-DECLINED                                         
02739          MOVE 'DECLINED  '          TO TRD-LF-ACTION.             
02740                                                                   
02741      IF CR-LF-IS-VOID                                             
02742          MOVE 'VOIDED    '          TO TRD-LF-ACTION.             
02733      IF CR-LF-POLICY-IS-MONTHLY                                   
02734          MOVE 'MONTHLY   '          TO TRD-LF-ACTION.             
02743                                                                   
02744      ADD +1                      TO  W-LF-ISSUES.                 
02745                                                                   
02746  0520-PROCESS-A-H-ISSUE.                                          
02747                                                                   
02748      IF PB-INVALID-AH                                             
02749          GO TO 0530-PROCESS-ISSUE-CONTINUED.                      
02750                                                                   
02751      MOVE PB-I-AH-BENEFIT-CD     TO CLAS-LOOK.                    
02752      PERFORM 8400-A-H-TYPE-LOOKUP THRU 8499-A-H-LOOKUP-X.         
02753      MOVE CLAS-LOOK              TO  PB-I-AH-BENEFIT-CD.          
02754                                                                   
02755      MOVE PB-I-AH-BENEFIT-CD     TO  CR-AHTYP.                    
02756      MOVE PB-I-AH-TERM           TO  CR-AH-TERM.                  
02757      MOVE PB-I-AH-CRIT-PER       TO  CR-AH-CRIT-PERIOD.           
02758      MOVE PB-I-RATE-DEVIATION-AH TO  CR-AH-DEV-CODE.              
02759      MOVE PB-I-RATE-DEV-PCT-AH   TO  CR-AH-DEV-PCT.               
02760                                                                   
02761      MOVE PB-I-AH-BENEFIT-AMT    TO  CR-AHAMT.                    
02762      MOVE PB-I-AH-PREMIUM-AMT    TO  CR-AHPRM.                    
02763      IF PB-I-AH-POLICY-FEE  NUMERIC                               
02764          MOVE PB-I-AH-POLICY-FEE TO  CR-AH-POLICY-FEE.            
02765      MOVE PB-I-AH-PREM-CALC      TO  CR-AHPRM-CALC.               
02766      MOVE PB-I-AH-RATE           TO  CR-AHPRM-RATE.               
02767                                                                   
PEMMOD     IF PB-I-AH-PREM-TAX NUMERIC
PEMMOD        MOVE PB-I-AH-PREM-TAX    TO CR-AH-ISS-PREM-TAX
PEMMOD     END-IF

02768      IF PB-I-AH-REI-RATE NOT = ZEROS                              
111402        IF DTE-CLIENT = 'DCC'
111402           CONTINUE
111402        ELSE
02769            MOVE PB-I-AH-REI-RATE   TO  CR-AH-NSP-PRM-RATE           
02770            COMPUTE CR-AH-NSP-PRM ROUNDED =                          
02771              ((CR-AHAMT * CR-AH-TERM) / 100) * PB-I-AH-REI-RATE   
111402        END-IF
111402     END-IF

092705     IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L'
100703        MOVE PB-I-AMOUNT-FINANCED TO CR-LFAMT
100703        MOVE PB-I-UNPAID-CASH-PRICE TO CR-LFAMT-ALT
100703        MOVE PB-I-CLP-AMOUNT     TO CR-LFPRM-ALT
100703     END-IF
02773      MOVE PB-I-AH-EXPIRE-DT      TO  DC-BIN-DATE-1.               
02774      MOVE ' '                    TO  DC-OPTION-CODE.              
02775      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
02776      MOVE DC-GREG-DATE-CYMD      TO  CR-AH-EXPIRE-DATE            
02777                                      WS-CR-AH-EXPIRE-DATE-N.      
02778                                                                   
02779      IF CR-ENTRY-STATUS = 'P'                                     
02780          MOVE '1'                TO  CR-AH-CURRENT-STATUS         
02781      ELSE                                                         
02782          MOVE CR-ENTRY-STATUS    TO  CR-AH-CURRENT-STATUS.        
02783                                                                   
02784      MOVE CLAS-I-AB3 (CLAS-INDEXA)  TO TRD-AHTYP.                 
02785      MOVE CR-AH-TERM                TO TRD-AHTRM.                 
02786      MOVE CR-AHPRM                  TO TRD-AHPRM-RFND.            
02787      MOVE 'ISSUE     '              TO TRD-AH-ACTION.             
02788      MOVE CR-AH-CURRENT-STATUS      TO TRD-AH-STATUS.             
02789                                                                   
02790      IF CR-AH-POLICY-IS-RESTORE                                   
02791          MOVE 'RESTORED  '          TO TRD-AH-ACTION.             
02792      IF CR-AH-POLICY-IS-REISSUE                                   
02793          MOVE 'RE-ISSUED '          TO TRD-AH-ACTION.             
02794      IF CR-AH-IS-REIN-ONLY                                        
02795          MOVE 'REIN. ONLY'          TO TRD-AH-ACTION.             
02796                                                                   
02797      IF CR-AH-IS-DECLINED                                         
02798          MOVE 'DECLINED  '          TO TRD-AH-ACTION.             
02799                                                                   
02800      IF CR-AH-IS-VOID                                             
02801          MOVE 'VOIDED    '          TO TRD-AH-ACTION.             
122002     IF CR-AH-POLICY-IS-MONTHLY                                   
122002         MOVE 'MONTHLY   '          TO TRD-AH-ACTION.             
02802                                                                   
02803      ADD +1                      TO  W-AH-ISSUES.                 
02804                                                                   
02805  0530-PROCESS-ISSUE-CONTINUED.                                    
02806                                                                   
02807      PERFORM 1000-SET-COMMISSION-STRUCTURE THRU 1499-SET-COMM-X.  
02808      MOVE WK-NEW-RATES           TO CR-COMPENSATION-LEVELS.       
02809      MOVE  SPACES                TO WS-LEVELS-CHARGEBACK-SWITCHES.
02810      PERFORM 1500-GET-ACCT-LEVEL-LF-COMM THRU 1519-ACCT-LF-COMM-X.

111204     IF (DTE-CLIENT = 'DCC')
111204        AND (CR-AHTYP NOT = '00' AND '  ')
092705        IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L'
                 PERFORM 0920-GET-BANK-FEES
                                       THRU 0920-EXIT
                 PERFORM 0930-BUILD-CERT-TRLR
                                       THRU 0930-EXIT
                 PERFORM 0395-GET-CERT-TRLRS
                                       THRU 0395-EXIT
              END-IF
           END-IF

111402     IF DTE-CLIENT = 'DCC'
111402        IF CR-LF-DEV-PCT NOT = +0 AND +1
111402           COMPUTE CR-LF-NSP-PRM ROUNDED =
111402              (CR-LFPRM + CR-LFPRM-ALT) -
111402              ((CR-LFPRM + CR-LFPRM-ALT) * CR-LCOM-L (1))
111402*             ((CR-LFPRM + CR-LFPRM-ALT) * WK-L-RT1 (1))
111402        END-IF
111402     END-IF
02704                                                                   
092705     IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L'
100703        MOVE CR-LFPRM-ALT        TO CR-AH-NSP-PRM
100703     ELSE
111402        IF DTE-CLIENT = 'DCC'
111402           IF CR-AH-DEV-PCT NOT = +0 AND +1
111402              COMPUTE CR-AH-NSP-PRM ROUNDED =
111402              CR-AHPRM -
111402              (CR-AHPRM * CR-LCOM-AH (1))
111402*             (CR-AHPRM * WK-A-RT1 (1))
100703           END-IF
111402        END-IF
111402     END-IF
02772                                                                   
02812      IF CR-ENTRY-STATUS = 'P'                                     
02813          MOVE '1'                   TO CR-ENTRY-STATUS.           
02814                                                                   
02815      IF CR-POLICY-IS-REISSUE  OR                                  
02816         CR-POLICY-IS-REIN-ONLY OR                                 
02817         CR-POLICY-IS-DECLINED OR                                  
02818         CR-POLICY-IS-VOID                                         
122002        OR CR-POLICY-IS-MONTHLY
02819          GO TO 0590-END-ISSUE-PROCESSING.                         
02820                                                                   
02821      ADD CR-LFPRM TO TPL-ISS-PRM (1).
092705     IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) NOT = 'G' AND 'L'
02822         ADD CR-LFPRM-ALT TO TPL-ISS-PRM (1)
100703     END-IF
02823      ADD CR-AHPRM TO TPA-ISS-PRM (1).                             
02824                                                                   
02825      IF NOT PB-INVALID-LIFE                                       
02826          COMPUTE H-LFCM ROUNDED =                                 
02827                     (CR-LFPRM + CR-LFPRM-ALT) * ACCT-COMM-LF      
02828          MOVE H-LFCM                   TO TRD-LFCOM-CLM           
02829          ADD H-LFCM                    TO TPL-ACCT-COM (1).       
02830                                                                   
02831      IF NOT PB-INVALID-AH
092705        IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L'
100703           COMPUTE H-AHCM ROUNDED = CR-AHPRM - (CR-LFPRM-ALT
040504           + CR-ADDL-CLP)
100703        ELSE                                         
02832            COMPUTE H-AHCM ROUNDED = CR-AHPRM * ACCT-COMM-AH
100703        END-IF
02833         MOVE H-AHCM                   TO TRD-AHCOM-CLM           
02834         ADD H-AHCM                    TO TPA-ACCT-COM (1)
100703     END-IF
02835                                                                   
02836      ADD +1 TO BC-G-ISS.                                          
02837      ADD +1 TO BC-COMM.                                           
02838      ADD CR-LFPRM TO BC-NET-WRIT.                                 
092705     IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) NOT = 'G' AND 'L'
02839         ADD CR-LFPRM-ALT TO BC-NET-WRIT
100703     END-IF
02840      ADD CR-AHPRM TO BC-NET-WRIT.                                 
02841                                                                   
02842      IF CLAS-I-BAL (CLAS-INDEXL) = 'B'                           
02843         ADD CR-LFPRM             TO BC-NET-WRIT-OB                
02844         ADD CR-LFPRM-ALT         TO BC-NET-WRIT-OB                
02845      ELSE                                                         
02846         ADD CR-LFPRM             TO BC-NET-WRIT-SP
092705        IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) NOT = 'G' AND 'L'
02847            ADD CR-LFPRM-ALT      TO BC-NET-WRIT-SP
100703        END-IF
100703     END-IF
02848                                                                   
02849      IF  CLAS-I-BAL (CLAS-INDEXA) = 'B'                           
02850          ADD CR-AHPRM            TO BC-NET-WRIT-OB                
02851      ELSE                                                         
02852          ADD CR-AHPRM            TO BC-NET-WRIT-SP.               
02853                                                                   
02854  0590-END-ISSUE-PROCESSING.                                       
02855                                                                   
02856      PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.       
02857                                                                   
02858      PERFORM 3000-REINSURE-ROUTINE THRU 3099-REINSURE-ROUTINE-X.  
02859                                                                   
02860      IF NOT CR-POLICY-IS-REIN-ONLY  AND                           
02861         NOT CR-POLICY-IS-DECLINED  AND                            
02862         NOT CR-POLICY-IS-VOID                                     
02863          MOVE 'Y' TO BILLED-SWITCH                                
02864          PERFORM 2100-BUILD-ISSUE-EXTRACT THRU 2199-BLD-ISSUE-X.  
02865                                                                   
02866      IF NOT CR-POLICY-IS-REISSUE  AND                             
02867         NOT CR-POLICY-IS-DECLINED  AND                            
02868         NOT CR-POLICY-IS-VOID                                     
122002        AND NOT CR-POLICY-IS-MONTHLY
02869          PERFORM 3100-REINSURE-ISSUE THRU 3199-REINSURE-ISSUE-X.  
02870                                                                   
02871      MOVE '*'                   TO TRANSACTION-FLAG.              
02872      GO TO 0300-READ-PENDING-TRANSACTIONS.                        
02873  EJECT                                                            
02874 ******************************************************************
02875 ***    P R O C E S S   C A N C E L   T R A N S A C T I O N S   ***
02876 ******************************************************************
02877                                                                   
02878  0600-PROCESS-CANCELLATIONS.                                      
02879                                                                   
02880      PERFORM 3000-REINSURE-ROUTINE THRU 3099-REINSURE-ROUTINE-X.  
02881                                                                   
02882      IF AM-RECALC-COMM = ('Y' OR '1')
02883         PERFORM 1600-RECALC-COMM-EXTR
02884                                  THRU 1899-RECALC-COMM-EXTR-X
060105        MOVE ' '                 TO RECALC-TYPE
060105     END-IF
02885                                                                   
02886      MOVE CR-LFRFND               TO LIFE-REFUND.                 
02887      MOVE CR-LFRFND-CALC          TO LIFE-REFUND-CALC.            
02888      MOVE CR-AHRFND               TO A-H-REFUND.                  
02889      MOVE CR-AHRFND-CALC          TO A-H-REFUND-CALC.             
02890      MOVE CR-SUM-CAN-CNT-YTD      TO YTD-CANCEL-CNT.              
02891                                                                   
02892      MOVE PB-C-LF-CANCEL-AMT      TO CR-LFRFND.                   
02893      IF CR-LFRFND-CALC = ZEROS                                    
02894          MOVE PB-C-LF-REF-CALC    TO CR-LFRFND-CALC               
02895                                      LIFE-REFUND-CALC             
02896      ELSE                                                         
02897          MOVE PB-C-LF-CANCEL-AMT  TO CR-LFRFND-CALC.              
02898                                                                   
02899      MOVE PB-C-AH-CANCEL-AMT      TO CR-AHRFND.                   
02900      IF CR-AHRFND-CALC = ZEROS                                    
02901          MOVE PB-C-AH-REF-CALC    TO CR-AHRFND-CALC               
02902                                      A-H-REFUND-CALC              
02903      ELSE                                                         
02904          MOVE PB-C-AH-CANCEL-AMT  TO CR-AHRFND-CALC.              
02905                                                                   
PEMTMP*    MOVE CR-DT                 TO  DC-GREG-DATE-CYMD.            
PEMTMP*    MOVE 'L'                   TO  DC-OPTION-CODE.               
PEMTMP*    PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
PEMTMP*    MOVE DC-BIN-DATE-1         TO  WS-CR-BIN-DATE.               
02910                                                                   
02911      IF PB-C-MICROFILM-NO NUMERIC  AND                            
02912         PB-C-MICROFILM-NO NOT = ZEROS                             
02913          MOVE PB-C-MICROFILM-NO  TO CR-CAN-MICROFILM-NO.          
02914                                                                   
02915      IF PB-CI-STATE-TAX NOT NUMERIC                               
02916         MOVE ZEROS               TO PB-CI-STATE-TAX.              
02917                                                                   
02918      IF PB-CI-MUNI-TAX NOT NUMERIC                                
02919         MOVE ZEROS               TO PB-CI-MUNI-TAX.               
02920                                                                   
02921      IF CR-CANCEL-STATE-TAX NUMERIC                               
02922          ADD PB-CI-STATE-TAX  TO CR-CANCEL-STATE-TAX              
02923        ELSE                                                       
02924          MOVE PB-CI-STATE-TAX TO CR-CANCEL-STATE-TAX.             
02925                                                                   
02926      IF CR-CANCEL-MUNI-TAX NUMERIC                                
02927          ADD PB-CI-MUNI-TAX   TO CR-CANCEL-MUNI-TAX               
02928        ELSE                                                       
02929          MOVE PB-CI-MUNI-TAX  TO CR-CANCEL-MUNI-TAX.              
02930                                                                   
02931      MOVE ZEROS                  TO MONTHS-DIFF-LF                
02932                                     MONTHS-DIFF-AH.               
02933                                                                   
02934      PERFORM 8600-SETUP-DETAIL-LINE THRU 8689-SETUP-DETAIL-X.     
02935                                                                   
02936  0630-PROCESS-LIFE-CANCEL.                                        
02937                                                                   
02938      IF CR-LFTYP = ZERO  OR                                       
02939         PB-C-LF-CANCEL-DT = LOW-VALUES  OR  SPACES                
02940          MOVE ZEROS               TO H-LFCM                       
02941          GO TO 0650-PROCESS-A-H-CANCEL.                           
02942                                                                   
PEMMOD     IF PB-C-LF-PREM-TAX NUMERIC
PEMMOD        MOVE PB-C-LF-PREM-TAX    TO CR-LF-CNC-PREM-TAX
PEMMOD     END-IF
02943      MOVE PB-C-LF-CANCEL-DT       TO  DC-BIN-DATE-1.              
02944      MOVE ' '                     TO  DC-OPTION-CODE.             
02945      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
02946      MOVE DC-GREG-DATE-CYMD       TO  CR-LF-CANC-DT               
02947                                       WS-CR-LF-CANC-DT-N.         
02948                                                                   
02949      MOVE WS-CR-BIN-DATE          TO  DC-BIN-DATE-1.              
02950      MOVE PB-C-LF-CANCEL-DT       TO  DC-BIN-DATE-2.              
02951      MOVE '1'                     TO  DC-OPTION-CODE.             
02952      MOVE ' '                     TO  DC-CENTURY-ADJUSTMENT.      
02953      MOVE ZEROS                   TO  DC-ELAPSED-MONTHS           
02954                                       DC-ODD-DAYS-OVER            
02955                                       DC-ELAPSED-DAYS.            
02956      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
02957      MOVE DC-ELAPSED-MONTHS       TO  MONTHS-DIFF-LF.             
02958                                                                   
02959      IF DC-ODD-DAYS-OVER   GREATER THAN  ZERO                     
02960          ADD  +1                  TO  MONTHS-DIFF-LF.             
02961                                                                   
02962      MOVE  'YYYYYYYYYY'           TO  WS-LF-CHARGEBACK-LEVELS.    
02963                                                                   
02964      MOVE  ZEROS                  TO  SUB.                        
02965                                                                   
02966  0635-LIFE-CHARGEBACK-LOOP.                                       
02967                                                                   
02968      ADD    +1                    TO  SUB.                        
02969                                                                   
02970      IF  SUB  GREATER THAN  +10                                   
02971          GO TO  0640-CONTINUE-LIFE-CANCEL.                        
02972                                                                   
02973      IF  AM-COMM-CHARGEBACK (SUB)  NOT  NUMERIC  OR               
02974          AM-COMM-CHARGEBACK (SUB)  =  ZEROS                       
02975          GO TO  0635-LIFE-CHARGEBACK-LOOP.                        
02976                                                                   
02977      IF  AM-COMM-CHARGEBACK (SUB)  EQUAL    '99'                  
02978          MOVE  'N'    TO  WS-LF-CHARGEBACK-SW (SUB).              
02979                                                                   
02980      IF  MONTHS-DIFF-LF GREATER THAN AM-COMM-CHARGEBACK (SUB)     
02981          MOVE  'N'        TO  WS-LF-CHARGEBACK-SW (SUB).          
02982                                                                   
02983      GO TO  0635-LIFE-CHARGEBACK-LOOP.                            
02984                                                                   
02985                                                                   
02986  0640-CONTINUE-LIFE-CANCEL.                                       
02987                                                                   
02988      IF DC-ODD-DAYS-OVER GREATER THAN ZEROS                       
02989         SUBTRACT +1 FROM MONTHS-DIFF-LF.                          
02990                                                                   
02991      PERFORM 1500-GET-ACCT-LEVEL-LF-COMM THRU 1519-ACCT-LF-COMM-X.
02992                                                                   
02993      IF DTE-CLIENT = 'GIC'                                        
02994          IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'                  
02995              MOVE CR-LFPRM-CALC   TO CR-LFRFND-CALC               
02996                                      LIFE-REFUND-CALC.            
02997                                                                   
040504*    IF DTE-CLIENT = 'TMS'                                        
040504     MOVE PB-LF-REFUND-TYPE      TO CR-LF-REFUND-TYPE
03000                                                                   
03001      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                      
03002          MOVE PB-C-LIVES               TO CR-SUM-CAN-CNT-YTD      
03003      ELSE                                                         
03004          IF CR-LF-CANCEL-EXIT-DATE = ZEROS                        
03005              MOVE CR-LF-CURRENT-STATUS TO CR-LF-STATUS-AT-CANCEL  
03006              MOVE '8'                  TO CR-LF-CURRENT-STATUS    
03007              MOVE PB-C-LF-COMM-CHARGEBACK TO CR-LF-COMM-CHARGEBACK
03008              MOVE EOM-RUN-DATE      TO CR-LF-CANCEL-EXIT-DATE     
03009                                        WS-CR-LF-CANCEL-EXIT-DATE-N
03010          ELSE                                                     
03011              IF CR-LF-CURRENT-STATUS = '6'                        
03012                  MOVE '8'              TO CR-LF-CURRENT-STATUS    
03013                  MOVE PB-C-LF-COMM-CHARGEBACK                     
03014                                        TO CR-LF-COMM-CHARGEBACK   
03015                  MOVE EOM-RUN-DATE TO CR-LF-CANCEL-EXIT-DATE      
03016                                       WS-CR-LF-CANCEL-EXIT-DATE-N.
03017                                                                   
03018 *************************************************                 
03019 ***  ADDED IN 06/93 WHEN CERTIFICATE-RECORD   ***                 
03020 ***  WAS CONVERTED TO 1056 BYTES.             ***                 
03021 *************************************************                 
03022      MOVE PB-ENTRY-BATCH               TO CR-LF-EXIT-BATCH.       
03023                                                                   
03024      MOVE CLAS-I-AB3 (CLAS-INDEXL)     TO TRD-LFTYP.              
03025      MOVE CR-LF-TERM                   TO TRD-LFTRM.              
03026      MOVE CR-LFRFND                    TO TRD-LFPRM-RFND.         
03027                                                                   
03028      COMPUTE H-LFCM ROUNDED =                                     
03029                               (CR-LFRFND * ACCT-COMM-LF) * (-1).  
03030      MOVE H-LFCM                       TO TRD-LFCOM-CLM.          
03031                                                                   
03032      MOVE CR-LF-CNC-MO                 TO TRD-LFACTMO.            
03033      MOVE CR-LF-CNC-DA                 TO TRD-LFACTDA.            
03034      MOVE CR-LF-CNC-YR                 TO TRD-LFACTYR.            
03035      MOVE '/'                          TO TRD-LF-S1  TRD-LF-S2.   
03036                                                                   
03037      MOVE 'CANCEL    '                 TO TRD-LF-ACTION.          
03038      MOVE CR-LF-CURRENT-STATUS         TO TRD-LF-STATUS.          
03039                                                                   
03040  0650-PROCESS-A-H-CANCEL.                                         
03041                                                                   
03042      IF CR-AHTYP = ZERO  OR                                       
03043         PB-C-AH-CANCEL-DT = LOW-VALUES  OR  SPACES                
03044          MOVE ZEROS               TO H-AHCM                       
03045          GO TO 0660-PROCESS-CANCEL-CONTINUE.                      
03046                                                                   
PEMTMP*    MOVE CR-DT                 TO  DC-GREG-DATE-CYMD.            
PEMTMP*    MOVE 'L'                   TO  DC-OPTION-CODE.               
PEMTMP*    PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
PEMTMP*    MOVE DC-BIN-DATE-1         TO  WS-CR-BIN-DATE.               
03051                                                                   
03052      MOVE PB-C-AH-CANCEL-DT       TO  DC-BIN-DATE-1.              
03053      MOVE ' '                     TO  DC-OPTION-CODE.             
03054      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
03055      MOVE DC-GREG-DATE-CYMD       TO  CR-AH-CANC-DT               
03056                                       WS-CR-AH-CANC-DT-N.         
03057                                                                   
PEMMOD     IF PB-C-AH-PREM-TAX NUMERIC
PEMMOD        MOVE PB-C-AH-PREM-TAX    TO CR-AH-CNC-PREM-TAX
PEMMOD     END-IF
03058      MOVE WS-CR-BIN-DATE          TO  DC-BIN-DATE-1.              
03059      MOVE PB-C-AH-CANCEL-DT       TO  DC-BIN-DATE-2.              
03060      MOVE '1'                     TO  DC-OPTION-CODE.             
03061      MOVE ' '                     TO  DC-CENTURY-ADJUSTMENT.      
03062      MOVE ZEROS                   TO  DC-ELAPSED-MONTHS           
03063                                       DC-ODD-DAYS-OVER            
03064                                       DC-ELAPSED-DAYS.            
03065      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
03066      MOVE DC-ELAPSED-MONTHS       TO  MONTHS-DIFF-AH.             
03067                                                                   
03068      IF  DC-ODD-DAYS-OVER   GREATER THAN   ZERO                   
03069           ADD  +1                 TO  MONTHS-DIFF-AH.             
03070                                                                   
03071      MOVE  'YYYYYYYYYY'           TO  WS-AH-CHARGEBACK-LEVELS.    
100703     IF CR-BANK-NOCHRGB-MONTHS NOT NUMERIC
100703        MOVE ZEROS               TO CR-BANK-NOCHRGB-MONTHS
           END-IF
      *    IF DTE-CLIENT = 'DCC'
092705*       IF (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
      *          AND (CR-BANK-NOCHRGB-MONTHS > ZEROS)
      *          PERFORM VARYING SUB FROM +1 BY +1 UNTIL
      *             (CR-BANK-NO = CR-COM-AGT (SUB))
      *             OR (SUB > +10)
      *          END-PERFORM
      *          IF CR-BANK-NO = CR-COM-AGT (SUB)
      *             MOVE CR-BANK-NOCHRGB-MONTHS
      *                                TO AM-COMM-CHARGEBACK (SUB)
      *          END-IF
      *       END-IF
      *    END-IF

03072                                                                   
040504     MOVE 'Y'                     TO WS-FIRST-K
040504     MOVE ' '                     TO WS-DLR-INC-CHG-BACK
03073      MOVE  ZEROS                  TO  SUB.                        
03074                                                                   
03075  0653-A-H-CHARGEBACK-LOOP.                                        
03076                                                                   
03077      ADD    +1                    TO  SUB.                        
03078                                                                   
03079      IF  SUB  GREATER THAN  +10                                   
03080          GO TO  0655-CONTINUE-A-H-CANCEL.                         
03081                                                                   
03082      IF  AM-COMM-CHARGEBACK (SUB)  NOT  NUMERIC  OR               
03083          AM-COMM-CHARGEBACK (SUB)  =  ZEROS                       
03084          GO TO  0653-A-H-CHARGEBACK-LOOP.                         
03085                                                                   
03086      IF  AM-COMM-CHARGEBACK (SUB)  EQUAL    '99'                  
03087          MOVE  'N'    TO  WS-AH-CHARGEBACK-SW (SUB).              
03088                                                                   
03089      IF  (MONTHS-DIFF-AH GREATER THAN AM-COMM-CHARGEBACK (SUB))
               OR ((DTE-CLIENT = 'DCC')
                   AND (CR-AHRFND = ZEROS))
03090          MOVE  'N'        TO  WS-AH-CHARGEBACK-SW (SUB).          
03091                                                                   
03092      GO TO  0653-A-H-CHARGEBACK-LOOP.                             
03093                                                                   
03094                                                                   
03095  0655-CONTINUE-A-H-CANCEL.                                        
03096                                                                   
03097      IF DC-ODD-DAYS-OVER GREATER THAN ZEROS                       
03098         SUBTRACT +1 FROM MONTHS-DIFF-AH.                          
03099                                                                   
03100      PERFORM 1550-GET-ACCT-LEVEL-AH-COMM THRU 1599-ACCT-AH-COMM-X.
03101                                                                   
03102      IF DTE-CLIENT = 'GIC'                                        
03103          IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'B'                  
03104              MOVE CR-AHPRM-CALC   TO CR-AHRFND-CALC               
03105                                      A-H-REFUND-CALC.             
03106                                                                   
040504*    IF DTE-CLIENT = 'TMS'                                        
040504     MOVE PB-AH-REFUND-TYPE      TO CR-AH-REFUND-TYPE
      *    DISPLAY ' PEND AH REF TYPE ' PB-AH-REFUND-TYPE

03109                                                                   
03110      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'Z'                      
03111          MOVE PB-C-LIVES               TO CR-SUM-CAN-CNT-YTD      
03112      ELSE                                                         
03113          IF CR-AH-CANCEL-EXIT-DATE = ZEROS                        
03114              MOVE CR-AH-CURRENT-STATUS TO CR-AH-STATUS-AT-CANCEL  
03115              MOVE '8'                  TO CR-AH-CURRENT-STATUS    
03116              MOVE PB-C-AH-COMM-CHARGEBACK TO CR-AH-COMM-CHARGEBACK
03117              MOVE EOM-RUN-DATE    TO CR-AH-CANCEL-EXIT-DATE       
03118                                      WS-CR-AH-CANCEL-EXIT-DATE-N  
03119          ELSE                                                     
03120              IF CR-AH-CURRENT-STATUS = '7'                        
03121                  MOVE '8'              TO CR-AH-CURRENT-STATUS    
03122                  MOVE PB-C-AH-COMM-CHARGEBACK                     
03123                                        TO CR-AH-COMM-CHARGEBACK   
03124                  MOVE EOM-RUN-DATE TO CR-AH-CANCEL-EXIT-DATE      
03125                                       WS-CR-AH-CANCEL-EXIT-DATE-N.
03126                                                                   
03127 *************************************************                 
03128 ***  ADDED IN 06/93 WHEN CERTIFICATE-RECORD   ***                 
03129 ***  WAS CONVERTED TO 1056 BYTES.             ***                 
03130 *************************************************                 
03131      MOVE PB-ENTRY-BATCH               TO CR-AH-EXIT-BATCH.       
03132                                                                   
03133      MOVE CLAS-I-AB3 (CLAS-INDEXA)     TO TRD-AHTYP.              
03134      MOVE CR-AH-TERM                   TO TRD-AHTRM.              
03135      MOVE CR-AHRFND                    TO TRD-AHPRM-RFND.         
03136                                                                   
092705     IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L'
100703        COMPUTE CNC-FACT = CR-AHRFND / CR-AHPRM
100703        COMPUTE H-AHCM ROUNDED = CNC-FACT *
100703           (CR-AHPRM - (CR-LFPRM-ALT
040504           + CR-ADDL-CLP)) * -1
100703     ELSE
03137         COMPUTE H-AHCM ROUNDED =
100703          (CR-AHRFND * ACCT-COMM-AH) * (-1)
100703     END-IF
03138                                                                   
03139      MOVE H-AHCM                       TO TRD-AHCOM-CLM.          
03140                                                                   
03141      MOVE CR-AH-CNC-MO                 TO TRD-AHACTMO.            
03142      MOVE CR-AH-CNC-DA                 TO TRD-AHACTDA.            
03143      MOVE CR-AH-CNC-YR                 TO TRD-AHACTYR             
03144      MOVE '/'                          TO TRD-AH-S1  TRD-AH-S2.   
03145                                                                   
03146      MOVE 'CANCEL    '                 TO TRD-AH-ACTION.          
03147      MOVE CR-AH-CURRENT-STATUS         TO TRD-AH-STATUS.          
03148                                                                   
03149  0660-PROCESS-CANCEL-CONTINUE.                                    
03150                                                                   
03151      PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.       
03152                                                                   
03153      IF CR-POLICY-IS-REIN-ONLY
03154         GO TO 0660-CONT.                                          
03155                                                                   
03156      MOVE 'Y'                    TO BILLED-SWITCH.                
03157      PERFORM 2200-BUILD-CANCEL-EXTRACT THRU 2299-BLD-CANCEL-X.    
122002                                                                  
123004     IF CR-POLICY-IS-MONTHLY
123004        GO TO 0660-CONT
123004     END-IF
122002                                                                  
03158      ADD +1                      TO BC-G-CNC.                     
03159      ADD +1                      TO BC-COMM.                      
03160      SUBTRACT CR-LFRFND FROM BC-NET-WRIT.                         
03161      SUBTRACT CR-AHRFND FROM BC-NET-WRIT.                         
03162      ADD CR-LFRFND               TO TPL-CNC-PRM (1).              
03163      ADD H-LFCM                  TO TPL-ACCT-COM (1).             
03164      ADD CR-AHRFND               TO TPA-CNC-PRM (1).              
03165      ADD H-AHCM                  TO TPA-ACCT-COM (1).             
03166                                                                   
03167      IF  CLAS-I-BAL (CLAS-INDEXL) = 'B'                           
03168          SUBTRACT CR-LFRFND FROM BC-NET-WRIT-OB                   
03169      ELSE                                                         
03170          SUBTRACT CR-LFRFND FROM BC-NET-WRIT-SP.                  
03171                                                                   
03172      IF  CLAS-I-BAL (CLAS-INDEXA) = 'B'                           
03173          SUBTRACT CR-AHRFND FROM BC-NET-WRIT-OB                   
03174      ELSE                                                         
03175          SUBTRACT CR-AHRFND FROM BC-NET-WRIT-SP.                  
03176                                                                   
03177                                                                   
03178  0660-CONT.                                                       
03179                                                                   
03180      MOVE MONTHS-DIFF-LF            TO REIN-LCNC-EARN-TERM.       
03181      MOVE MONTHS-DIFF-AH            TO REIN-ACNC-EARN-TERM.       
03182                                                                   
03183      IF CR-LFRFND NEGATIVE                                        
03184        IF CR-LFRFND + LIFE-REFUND = ZEROS                         
03185            MOVE '*'                 TO REIN-VOID-LF-SW.           
03186      IF CR-AHRFND NEGATIVE                                        
03187        IF CR-AHRFND + A-H-REFUND = ZEROS                          
03188            MOVE '*'                 TO REIN-VOID-AH-SW.           
03189                                                                   
03190      IF CR-LFRFND NOT = ZEROS                                     
03191        IF CR-LF-CANCEL-EXIT-DATE NOT = EOM-RUN-DATE               
03192            MOVE '*'                 TO REIN-ADDL-LF-SW.           
03193      IF CR-AHRFND NOT = ZEROS                                     
03194        IF CR-AH-CANCEL-EXIT-DATE NOT = EOM-RUN-DATE               
03195            MOVE '*'                 TO REIN-ADDL-AH-SW.           
03196                                                                   
03197      IF CR-DT = CR-LF-CANC-DT                                     
03198        MOVE '*'                     TO REIN-CANCEL-LF-SW.         
03199      IF CR-DT = CR-AH-CANC-DT                                     
03200        MOVE '*'                     TO REIN-CANCEL-AH-SW.         
03201                                                                   
03202      PERFORM 3200-REINSURE-CANCEL THRU 3299-REINSURE-CANCEL-X.    
03203                                                                   
03204      MOVE SPACES                    TO REIN-VOID-LF-SW            
03205                                        REIN-VOID-AH-SW            
03206                                        REIN-ADDL-LF-SW            
03207                                        REIN-ADDL-AH-SW.           
03208                                                                   
03209      COMPUTE CR-LFRFND = LIFE-REFUND + PB-C-LF-CANCEL-AMT.        
03210      MOVE LIFE-REFUND-CALC          TO CR-LFRFND-CALC.            
03211                                                                   
03212      COMPUTE CR-AHRFND = A-H-REFUND  + PB-C-AH-CANCEL-AMT.        
03213      MOVE A-H-REFUND-CALC           TO CR-AHRFND-CALC.            
03214                                                                   
03215      IF (CR-LFTYP NOT = ZERO  AND                                 
03216         CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z')  OR                 
03217         (CR-AHTYP NOT = ZERO  AND                                 
03218         CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'Z')                     
03219          ADD PB-C-LIVES                TO CR-SUM-CAN-CNT-ITD      
03220          COMPUTE CR-SUM-CAN-CNT-YTD = YTD-CANCEL-CNT + PB-C-LIVES.
03221                                                                   
03222  0670-LIFE-CANCEL-VOID.                                           
03223                                                                   
03224      IF CR-LFTYP NOT = ZERO  AND                                  
03225         CR-LFRFND = ZERO  AND                                     
03226         PB-C-LF-CANCEL-AMT NEGATIVE                               
03227          NEXT SENTENCE                                            
03228      ELSE                                                         
03229          GO TO 0680-A-H-CANCEL-VOID.                              
03230                                                                   
03231      MOVE ZEROS                        TO CR-LF-CANCEL-EXIT-DATE  
03232                                        WS-CR-LF-CANCEL-EXIT-DATE-N
03233                                           CR-LF-CANC-DT           
03234                                           WS-CR-LF-CANC-DT-N      
03235                                           CR-LFRFND-CALC.         
03236                                                                   
03237      MOVE SPACES                       TO CR-LF-COMM-CHARGEBACK.  
03238                                                                   
03239      IF CR-LF-CANCEL-APPLIED                                      
03240          MOVE CR-LF-STATUS-AT-CANCEL   TO CR-LF-CURRENT-STATUS    
03241          MOVE ' '                      TO CR-LF-STATUS-AT-CANCEL  
03242      ELSE                                                         
03243          MOVE CR-LF-STATUS-AT-CANCEL   TO CR-LF-STATUS-AT-DEATH   
03244          MOVE ' '                      TO CR-LF-STATUS-AT-CANCEL. 
03245                                                                   
03246      PERFORM 8600-SETUP-DETAIL-LINE THRU 8689-SETUP-DETAIL-X.     
03247                                                                   
03248      MOVE 'VOID CANCL'                 TO TRD-LF-ACTION.          
03249      MOVE CR-LF-CURRENT-STATUS         TO TRD-LF-STATUS.          
03250                                                                   
03251      PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.       
03252                                                                   
03253  0680-A-H-CANCEL-VOID.                                            
03254                                                                   
03255      IF CR-AHTYP NOT = ZERO  AND                                  
03256         CR-AHRFND = ZERO  AND                                     
03257         PB-C-AH-CANCEL-AMT NEGATIVE                               
03258          NEXT SENTENCE                                            
03259      ELSE                                                         
03260          GO TO 0690-END-CANCEL-PROCESSING.                        
03261                                                                   
03262      MOVE ZEROS                      TO CR-AH-CANCEL-EXIT-DATE    
03263                                      WS-CR-AH-CANCEL-EXIT-DATE-N  
03264                                         CR-AH-CANC-DT             
03265                                         WS-CR-AH-CANC-DT-N        
03266                                         CR-AHRFND-CALC.           
03267                                                                   
03268      MOVE SPACES                        TO CR-AH-COMM-CHARGEBACK. 
03269                                                                   
03270      IF CR-AH-CANCEL-APPLIED                                      
03271          MOVE CR-AH-STATUS-AT-CANCEL    TO CR-AH-CURRENT-STATUS   
03272          MOVE ' '                       TO CR-AH-STATUS-AT-CANCEL 
03273      ELSE                                                         
03274          MOVE CR-AH-STATUS-AT-CANCEL                              
03275                                     TO CR-AH-STATUS-AT-SETTLEMENT 
03276          MOVE ' '                       TO CR-AH-STATUS-AT-CANCEL.
03277                                                                   
03278      PERFORM 8600-SETUP-DETAIL-LINE THRU 8689-SETUP-DETAIL-X.     
03279                                                                   
03280      MOVE 'VOID CANCL'                  TO TRD-AH-ACTION.         
03281      MOVE CR-AH-CURRENT-STATUS          TO TRD-AH-STATUS.         
03282                                                                   
03283      PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.       
03284                                                                   
03285  0690-END-CANCEL-PROCESSING.                                      
03286                                                                   
03287      IF CR-LF-CANC-DT = ZEROS  AND                                
03288         CR-AH-CANC-DT = ZEROS                                     
03289          MOVE ZEROS             TO CR-CAN-MICROFILM-NO.           
03290                                                                   
03291      IF LIFE-REFUND = ZERO  AND  A-H-REFUND = ZERO                
03292          NEXT SENTENCE                                            
03293      ELSE                                                         
03294          MOVE 'X'               TO REIN-RT-SW                     
03295          PERFORM 3200-REINSURE-CANCEL THRU 3299-REINSURE-CANCEL-X 
03296          MOVE ' '               TO REIN-RT-SW.                    
03297                                                                   
03298      MOVE '*'                   TO TRANSACTION-FLAG.              
03299      GO TO 0300-READ-PENDING-TRANSACTIONS.                        
03300                                                                   
03301  EJECT                                                            
03302 ******************************************************************
03303 ***    P R O C E S S   C L A I M S   T R A N S A C T I O N S   ***
03304 ******************************************************************
03305                                                                   
03306  0700-PROCESS-CLAIMS.                                             
03307                                                                   
03308      IF NOT PC-LF-CLAIM  AND  NOT  PC-OB-LF-CLAIM  AND            
03309         NOT PC-AH-CLAIM  AND  NOT  PC-OB-AH-CLAIM                 
03310          DISPLAY 'INVALID CLAIM TYPE TO ECS010 '                  
03311              PENDING-MATCH-CONTROL                                
03312          MOVE 0301 TO WS-ABEND-CODE                               
CIDMOD         DISPLAY '******************************'
CIDMOD         DISPLAY '***** ERROR LOCATION 011 *****'
CIDMOD         DISPLAY '******************************'
03313          MOVE WS-ABEND-CODE        TO WS-RETURN-CODE              
03314          GO TO ABEND-PGM.                                         
03315                                                                   
03316      PERFORM 3000-REINSURE-ROUTINE THRU 3099-REINSURE-ROUTINE-X.  
03317                                                                   
03318      ADD +1                       TO  W-LF-CLAIMS.                
03319                                                                   
03320      MOVE PC-INCURRED-DT          TO  DC-BIN-DATE-1.              
03321      MOVE ' '                     TO  DC-OPTION-CODE.             
03322      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
03323      MOVE DC-GREG-DATE-CYMD       TO  INCURRED-DATE.              
03324                                                                   
03325      MOVE PC-REPORTED-DT          TO  DC-BIN-DATE-1.              
03326      MOVE ' '                     TO  DC-OPTION-CODE.             
03327      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
03328      MOVE DC-GREG-DATE-1-YMD      TO  REPORTED-DATE.              
03329                                                                   
03330      MOVE PC-PAYMENT-DT           TO  DC-BIN-DATE-1.              
03331      MOVE ' '                     TO  DC-OPTION-CODE.             
03332      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
03333      MOVE DC-GREG-DATE-CYMD       TO  PAYMENT-DATE.               
03334                                                                   
03335      MOVE PC-PAID-THRU-DT         TO  DC-BIN-DATE-1.              
03336      MOVE ' '                     TO  DC-OPTION-CODE.             
03337      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
03338      MOVE DC-GREG-DATE-CYMD       TO  PAY-TO-DATE.                
03339                                                                   
03340      PERFORM 8600-SETUP-DETAIL-LINE THRU 8689-SETUP-DETAIL-X.     
03341                                                                   
03342      IF DTE-CLIENT = 'LAP'  OR  'RMC'                             
03343        MOVE PC-CHECK-NO            TO WS-CHECK-NUMBER             
03344        IF WS-CHECK-NO = 'DEDU'  OR  ' DED'  OR  '0DED'            
03345          IF CR-CLAIM-DEDUCT-WITHHELD = +99999.99                  
03346              MOVE PC-CLAIM-PAYMENT TO CR-CLAIM-DEDUCT-WITHHELD    
03347          ELSE                                                     
03348              ADD PC-CLAIM-PAYMENT  TO CR-CLAIM-DEDUCT-WITHHELD.   
03349                                                                   
03350      IF PC-AH-CLAIM  OR  PC-OB-AH-CLAIM                           
03351          GO TO 0750-PROCESS-DISABILITY-CLAIM.                     
03352                                                                   
03353 ******************************************************************
03354 ***           P R O C E S S   D E A T H   C L A I M            ***
03355 ******************************************************************
03356                                                                   
03357      IF REIN-IS-RISK-PREMIUM                                      
03358        IF CR-DTH-DT = ZEROS                                       
03359            MOVE INCURRED-DATE      TO DC-GREG-DATE-CYMD           
03360            PERFORM 3025-CALC-ELAPSED-MONTHS THRU 3026-CALC-EXIT   
03361            MOVE DC-ELAPSED-MONTHS  TO REIN-LF-CLM-MONTHS.         
03362                                                                   
03363      MOVE INCURRED-DATE            TO CR-DTH-DT                   
03364                                       WS-CR-DTH-DT-N.             
03365                                                                   
03366      IF PC-CHARGEBLE-EXPENSE  OR                                  
03367         PC-NON-CHARGEBLE-EXPENSE                                  
03368          IF CR-LFTYP = '00'                                       
03369              MOVE '99'             TO CR-LFTYP  CLAS-LOOK         
03370              PERFORM 8300-LIFE-TYPE-LOOKUP THRU                   
03371                      8399-LIFE-LOOKUP-X.                          
03372                                                                   
03373      IF PC-CHARGEBLE-EXPENSE  OR                                  
03374         PC-NON-CHARGEBLE-EXPENSE                                  
03375          ADD PC-CLAIM-PAYMENT      TO CR-DTHEXP                   
03376          ADD PC-CLAIM-PAYMENT      TO CR-DTHEXP-YTD               
03377          ADD PC-CLAIM-PAYMENT      TO TPL-CLAIMS (1)              
03378          MOVE PC-CLAIM-PAYMENT     TO TRD-LFCOM-CLM               
03379          MOVE 'CLAIM EXP '         TO TRD-LF-ACTION               
03380          GO TO 0780-END-CLAIMS-PROCESSING.                        
03381                                                                   
03382      ADD +1                       TO  CR-NUM-DTH-CLM.             
03383      MOVE REPORTED-DATE           TO  CR-DTH-RPT-DT.              
03384      MOVE PAYMENT-DATE            TO  CR-DTH-PAY-DT               
03385                                       WS-CR-DTH-PAY-DT-N.         
03386                                                                   
03387      ADD PC-CLAIM-PAYMENT         TO  CR-DTHAMT.                  
03388      ADD PC-CLAIM-PAYMENT         TO  CR-DTHAMT-YTD.              
03389      MOVE PC-CLAIM-PAYMENT        TO  CR-DTHAMT-LAST.             
03390      MOVE PC-AGE-AT-CLAIM         TO  CR-DTH-AGE.                 
03391      IF CR-DTH-PAY-CD NOT = 'F'                                   
03392          MOVE PC-PAYMENT-TYPE     TO  CR-DTH-PAY-CD.              
03393      INSPECT CR-DTH-PAY-CD                                        
03394          CONVERTING '12349' TO 'PFSXV'.                           
03395      MOVE PC-CAUSE-CODE           TO  CR-DEATH-CAUSE.             
03396                                                                   
03397      IF (LIFE-OVERRIDE-L1 EQUAL 'P'  OR                           
03398          CLAS-I-RL-AH (CLAS-INDEXL) EQUAL 'P')                    
03399        AND                                                        
03400        (CR-DTHAMT LESS THAN (CR-LFAMT + CR-LFAMT-ALT)             
03401        OR                                                         
03402         DTE-CLIENT = 'ACC' OR 'FDL')                              
03403         NEXT SENTENCE                                             
03404      ELSE                                                         
03405      IF (CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'Z')                
03406          IF CR-LF-CLAIM-EXIT-DATE = ZEROS                         
03407              MOVE CR-LF-CURRENT-STATUS TO CR-LF-STATUS-AT-DEATH   
03408              MOVE '7'            TO CR-LF-CURRENT-STATUS          
03409              MOVE EOM-RUN-DATE   TO CR-LF-CLAIM-EXIT-DATE         
03410                                     WS-CR-LF-CLAIM-EXIT-DATE-N.   
03411                                                                   
03412      MOVE CLAS-I-AB3 (CLAS-INDEXL)     TO TRD-LFTYP.              
03413      MOVE CR-LF-TERM                   TO TRD-LFTRM.              
03414      MOVE PC-CLAIM-PAYMENT        TO  TRD-LFCOM-CLM.              
03415      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'P'                          
03416          MOVE 'PROPERTY  '       TO TRD-LF-ACTION                 
03417      ELSE                                                         
03418          MOVE 'CLAIM PYMT'       TO TRD-LF-ACTION.                
03419      MOVE CR-LF-CURRENT-STATUS   TO TRD-LF-STATUS.                
03420                                                                   
03421      IF NOT CR-POLICY-IS-REIN-ONLY                                
03422          ADD PC-CLAIM-PAYMENT    TO TPL-CLAIMS (1).               
03423                                                                   
03424  0720-CANCEL-AH-COVERAGE.                                         
03425                                                                   
03426      IF CR-AHTYP = ZEROS  OR                                      
03427         NOT CR-AH-POLICY-IS-ACTIVE                                
03428          GO TO 0740-DEATH-CLAIM-VOID.                             
03429                                                                   
03430      MOVE CR-AH-CURRENT-STATUS          TO CR-AH-STATUS-AT-CANCEL.
03431      MOVE '7'                           TO CR-AH-CURRENT-STATUS.  
03432      MOVE CR-DTH-DT                     TO CR-AH-CANC-DT          
03433                                            WS-CR-AH-CANC-DT-N.    
03434      MOVE EOM-RUN-DATE               TO CR-AH-CANCEL-EXIT-DATE    
03435                                      WS-CR-AH-CANCEL-EXIT-DATE-N. 
03436                                                                   
03437      MOVE CLAS-I-AB3 (CLAS-INDEXA)      TO TRD-AHTYP.             
03438      MOVE CR-AH-TERM                    TO TRD-AHTRM.             
03439      MOVE CR-DTH-MO                     TO TRD-AHACTMO.           
03440      MOVE CR-DTH-DA                     TO TRD-AHACTDA.           
03441      MOVE CR-DTH-YR                     TO TRD-AHACTYR.           
03442      MOVE '/'                           TO TRD-AH-S1  TRD-AH-S2.  
03443      MOVE 'CLAIM CANC'                  TO TRD-AH-ACTION.         
03444      MOVE CR-AH-CURRENT-STATUS          TO TRD-AH-STATUS.         
03445                                                                   
03446  0740-DEATH-CLAIM-VOID.                                           
03447                                                                   
03448      IF CR-DTHAMT = ZERO  AND                                     
03449         PC-CLAIM-PAYMENT NEGATIVE                                 
03450          NEXT SENTENCE                                            
03451      ELSE                                                         
03452          GO TO 0780-END-CLAIMS-PROCESSING.                        
03453                                                                   
03454      MOVE '*'                           TO DEATH-VOID-FLAG.       
03455                                                                   
03456      MOVE ZEROS                         TO CR-LF-CLAIM-EXIT-DATE  
03457                                         WS-CR-LF-CLAIM-EXIT-DATE-N
03458                                            CR-NUM-DTH-CLM         
03459                                            CR-DTH-RPT-DT          
03460                                            CR-DTH-PAY-DT          
03461                                            WS-CR-DTH-PAY-DT-N     
03462 *                                          CR-DTHAMT-YTD          
03463 *                                          CR-DTHAMT-LAST         
03464                                            CR-DTH-AGE.            
03465      MOVE ' '                           TO CR-DTH-PAY-CD.         
03466                                                                   
03467      IF CR-LF-DEATH-CLAIM-APPLIED                                 
03468          MOVE CR-LF-STATUS-AT-DEATH     TO CR-LF-CURRENT-STATUS   
03469          MOVE ' '                       TO CR-LF-STATUS-AT-DEATH  
03470      ELSE                                                         
03471          MOVE CR-LF-STATUS-AT-DEATH     TO CR-LF-STATUS-AT-CANCEL 
03472          MOVE ' '                       TO CR-LF-STATUS-AT-DEATH. 
03473                                                                   
03474      MOVE 'VOID CLAIM'                  TO TRD-LF-ACTION.         
03475      MOVE CR-LF-CURRENT-STATUS          TO TRD-LF-STATUS.         
03476                                                                   
03477      IF CR-AH-DEATH-CLAIM-APPLIED                                 
03478          MOVE CR-AH-STATUS-AT-CANCEL    TO CR-AH-CURRENT-STATUS   
03479          MOVE ' '                       TO CR-AH-STATUS-AT-CANCEL 
03480          MOVE ZEROS                     TO CR-AH-CANC-DT          
03481                                            WS-CR-AH-CANC-DT-N     
03482                                            CR-AH-CANCEL-EXIT-DATE 
03483                                        WS-CR-AH-CANCEL-EXIT-DATE-N
03484          MOVE CLAS-I-AB3 (CLAS-INDEXA)  TO TRD-AHTYP              
03485          MOVE CR-AH-TERM                TO TRD-AHTRM              
03486          MOVE 'VOID CANCL'              TO TRD-AH-ACTION          
03487          MOVE CR-AH-CURRENT-STATUS      TO TRD-AH-STATUS.         
03488                                                                   
03489      GO TO 0780-END-CLAIMS-PROCESSING.                            
03490                                                                   
03491 ******************************************************************
03492 ***       P R O C E S S   D I S A B I L I T Y   C L A I M      ***
03493 ******************************************************************
03494                                                                   
03495  0750-PROCESS-DISABILITY-CLAIM.                                   
03496                                                                   
03497      ADD +1                       TO W-AH-CLAIMS.                 
03498                                                                   
03499      MOVE INCURRED-DATE           TO CR-DIS-DT                    
03500                                      WS-CR-DIS-DT-N.              
03501                                                                   
03502      PERFORM 0790-TABLE-DISAB-PERIODS THRU 0791-TABLE-AH-EXIT.    
03503                                                                   
03504      IF REIN-AH-CLM-MONTHS = +1                                   
03505          MOVE REIN-AH-CLM-MTH-1        TO REIN-AH-CLM-MONTHS      
03506      ELSE                                                         
03507        IF REIN-AH-CLM-MONTHS = +2                                 
03508            MOVE REIN-AH-CLM-MTH-2      TO REIN-AH-CLM-MONTHS      
03509        ELSE                                                       
03510          IF REIN-AH-CLM-MONTHS = +3                               
03511              MOVE REIN-AH-CLM-MTH-3    TO REIN-AH-CLM-MONTHS      
03512          ELSE                                                     
03513            IF REIN-AH-CLM-MONTHS = +4                             
03514                MOVE REIN-AH-CLM-MTH-4  TO REIN-AH-CLM-MONTHS      
03515            ELSE                                                   
03516                MOVE REIN-AH-CLM-MTH-5  TO REIN-AH-CLM-MONTHS.     
03517                                                                   
03518      IF PC-CHARGEBLE-EXPENSE  OR                                  
03519         PC-NON-CHARGEBLE-EXPENSE                                  
03520          IF CR-AHTYP = '00'                                       
03521              MOVE '99'             TO CR-AHTYP  CLAS-LOOK         
03522              PERFORM 8400-A-H-TYPE-LOOKUP THRU                    
03523                      8499-A-H-LOOKUP-X.                           
03524                                                                   
03525      IF PC-CHARGEBLE-EXPENSE  OR                                  
03526         PC-NON-CHARGEBLE-EXPENSE                                  
03527          ADD PC-CLAIM-PAYMENT      TO CR-DISEXP                   
03528          ADD PC-CLAIM-PAYMENT      TO CR-DISEXP-YTD               
03529          ADD PC-CLAIM-PAYMENT      TO TPA-CLAIMS (1)              
03530          MOVE PC-CLAIM-PAYMENT     TO TRD-AHCOM-CLM               
03531          MOVE 'CLAIM EXP '         TO TRD-AH-ACTION               
03532          GO TO 0780-END-CLAIMS-PROCESSING.                        
03533                                                                   
03534      ADD +1                       TO CR-NUM-DIS-CLM.              
03535                                                                   
03536      MOVE REPORTED-DATE           TO  CR-DIS-RPT-DT.              
03537      MOVE PAYMENT-DATE            TO  CR-DIS-PAY-DT               
03538                                       WS-CR-DIS-PAY-DT-N.         
03539      MOVE PAY-TO-DATE             TO  CR-DIS-PTO-DT               
03540                                       WS-CR-DIS-PTO-DT-N.         
03541                                                                   
03542      ADD  PC-CLAIM-PAYMENT         TO CR-DISAMT.                  
03543      ADD  PC-CLAIM-PAYMENT         TO CR-DISAMT-YTD.              
03544      MOVE PC-CLAIM-PAYMENT         TO CR-DISAMT-LAST.             
03545                                                                   
03546      MOVE PC-NO-OF-DAYS-PAID       TO CR-DAYS-DISAB.              
03547      IF CR-DIS-PAY-CD NOT = 'S'                                   
03548          MOVE PC-PAYMENT-TYPE      TO CR-DIS-PAY-CD.              
03549      INSPECT CR-DIS-PAY-CD                                        
03550          CONVERTING '12349' TO 'PFSXV'.                           
03551      MOVE PC-CAUSE-CODE            TO CR-DISAB-CAUSE.             
03552                                                                   
03553      IF PC-LUMP-SUM-PAYMENT                                       
03554          IF CR-AH-SETTLEMENT-EXIT-DATE = ZEROS                    
03555            MOVE CR-AH-CURRENT-STATUS TO CR-AH-STATUS-AT-SETTLEMENT
03556            MOVE '6'                  TO CR-AH-CURRENT-STATUS      
03557            MOVE EOM-RUN-DATE    TO CR-AH-SETTLEMENT-EXIT-DATE     
03558                                    WS-CR-AH-SETTLEMENT-EXIT-DATEN.
03559                                                                   
03560      MOVE CLAS-I-AB3 (CLAS-INDEXA) TO TRD-AHTYP.                  
03561      MOVE CR-AH-TERM               TO TRD-AHTRM.                  
03562      MOVE PC-CLAIM-PAYMENT         TO TRD-AHCOM-CLM.              
03563      MOVE 'CLAIM PYMT'             TO TRD-AH-ACTION.              
03564      MOVE CR-AH-CURRENT-STATUS     TO TRD-AH-STATUS.              
03565                                                                   
03566      IF NOT CR-POLICY-IS-REIN-ONLY                                
03567          ADD PC-CLAIM-PAYMENT      TO TPA-CLAIMS (1).             
03568                                                                   
03569      IF NOT PC-LUMP-SUM-PAYMENT                                   
03570          GO TO 0780-END-CLAIMS-PROCESSING.                        
03571                                                                   
03572  0770-CANCEL-LF-COVERAGE.                                         
03573                                                                   
03574      IF CR-LFTYP = ZEROS  OR                                      
03575         NOT CR-LF-POLICY-IS-ACTIVE                                
03576          GO TO 0780-END-CLAIMS-PROCESSING.                        
03577                                                                   
03578      MOVE CR-LF-CURRENT-STATUS          TO CR-LF-STATUS-AT-CANCEL.
03579      MOVE '6'                           TO CR-LF-CURRENT-STATUS.  
03580      MOVE CR-DIS-PAY-DT                 TO CR-LF-CANC-DT          
03581                                            WS-CR-LF-CANC-DT-N.    
03582      MOVE EOM-RUN-DATE               TO CR-LF-CANCEL-EXIT-DATE    
03583                                      WS-CR-LF-CANCEL-EXIT-DATE-N. 
03584                                                                   
03585      MOVE CLAS-I-AB3 (CLAS-INDEXL)      TO TRD-LFTYP.             
03586      MOVE CR-LF-TERM                    TO TRD-LFTRM.             
03587      MOVE CR-DIS-PAY-MO                 TO TRD-LFACTMO.           
03588      MOVE CR-DIS-PAY-DA                 TO TRD-LFACTDA.           
03589      MOVE CR-DIS-PAY-YR                 TO TRD-LFACTYR.           
03590      MOVE '/'                           TO TRD-LF-S1  TRD-LF-S2.  
03591      MOVE 'CLAIM CANC'                  TO TRD-LF-ACTION.         
03592      MOVE CR-LF-CURRENT-STATUS          TO TRD-LF-STATUS.         
03593                                                                   
03594  0780-END-CLAIMS-PROCESSING.                                      
03595                                                                   
03596      IF PC-LF-CLAIM  OR  PC-OB-LF-CLAIM                           
03597          MOVE DTO-MO               TO TRD-LFACTMO                 
03598          MOVE DTO-DA               TO TRD-LFACTDA                 
03599          MOVE DTO-YR               TO TRD-LFACTYR                 
03600          MOVE '/'                  TO TRD-LF-S1  TRD-LF-S2        
03601      ELSE                                                         
03602          MOVE PAY-MO               TO TRD-AHACTMO                 
03603          MOVE PAY-DA               TO TRD-AHACTDA                 
03604          MOVE PAY-YR               TO TRD-AHACTYR                 
03605          MOVE '/'                  TO TRD-AH-S1  TRD-AH-S2.       
03606                                                                   
03607      PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.       
03608                                                                   
03609      IF NOT CR-POLICY-IS-REIN-ONLY                                
03610          PERFORM 2300-BUILD-CLAIM-EXTRACT THRU 2399-BLD-CLAIM-X   
03611          ADD +1                    TO BC-G-CLM.                   
03612                                                                   
03613      PERFORM 3300-REINSURE-CLAIM THRU 3399-REINSURE-CLAIM-X.      
03614                                                                   
03615      IF DEATH-CLAIM-VOIDED                                        
03616          MOVE ZEROS                TO CR-DTH-DT                   
03617                                       WS-CR-DTH-DT-N.             
03618      MOVE SPACE                    TO DEATH-VOID-FLAG.            
03619                                                                   
03620      MOVE '*'                      TO TRANSACTION-FLAG.           
03621      GO TO 0300-READ-PENDING-TRANSACTIONS.                        
03622                                                                   
03623  EJECT                                                            
03624  0790-TABLE-DISAB-PERIODS.                                        
03625                                                                   
03626      MOVE +1                       TO SUB14.                      
03627      PERFORM 0794-REVIEW-TABLE     THRU 0794-REVIEW-TABLE-EXIT.   
03628                                                                   
03629      MOVE +1                       TO SUB14.                      
03630                                                                   
03631 **************************************************************    
03632 **                                                          **    
03633 **   THIS ROUTINE SEARCHES THROUGH THE DISAB-DETAIL-DATA    **    
03634 **  AND MATCHES INCURRED DATE.  IT THEN ADDS THE AMOUNT OF  **    
03635 **  THE CLAIM TO THAT OCCURRENCE.  IF THE INCURRED DATE     **    
03636 **  DOES NOT MATCH ANY PREVIOUS INCURRED DATE,  IT WILL     **    
03637 **  INITIALIZE THE NEXT OCCURRENCE.  (OLDEST TO MOST RECENT)**    
03638 **                                                          **    
03639 **    IF ALL 5 OCCURRENCES ARE FILLED,  THE TWO OLDEST      **    
03640 **  AMOUNTS ARE COMBINED LEAVING THE LAST OCCURRENCE OPEN   **    
03641 **  FOR THE MOST RECENT CLAIM INFORMATION.                  **    
03642 **                                                          **    
03643 **************************************************************    
03644                                                                   
03645  0791-TABLE-AH-AMOUNT.                                            
03646                                                                   
03647      IF SUB14 GREATER THAN DISABILITY-TBL-MAX                     
03648          PERFORM 0792-MOVE-TABLE  THRU 0792-MOVE-TABLE-EXIT.      
03649                                                                   
03650      IF CR-DIS-INCUR-DT (SUB14)  EQUAL ZEROS                      
03651          MOVE INCURRED-DATE        TO CR-DIS-INCUR-DT (SUB14)     
03652                                       WS-CR-DIS-INCUR-DT-N (SUB14)
03653          IF REIN-IS-RISK-PREMIUM                                  
03654              MOVE INCURRED-DATE     TO DC-GREG-DATE-CYMD          
03655              PERFORM 3025-CALC-ELAPSED-MONTHS THRU 3026-CALC-EXIT 
03656              IF SUB14 = +1                                        
03657                MOVE DC-ELAPSED-MONTHS TO REIN-AH-CLM-MTH-1        
03658              ELSE                                                 
03659              IF SUB14 = +2                                        
03660                MOVE DC-ELAPSED-MONTHS TO REIN-AH-CLM-MTH-2        
03661              ELSE                                                 
03662              IF SUB14 = +3                                        
03663                MOVE DC-ELAPSED-MONTHS TO REIN-AH-CLM-MTH-3        
03664              ELSE                                                 
03665              IF SUB14 = +4                                        
03666                MOVE DC-ELAPSED-MONTHS TO REIN-AH-CLM-MTH-4        
03667              ELSE                                                 
03668                MOVE DC-ELAPSED-MONTHS TO REIN-AH-CLM-MTH-5.       
03669                                                                   
03670                                                                   
03671      IF INCURRED-DATE EQUAL CR-DIS-INCUR-DT (SUB14)               
03672          MOVE SUB14                TO REIN-AH-CLM-MONTHS          
03673          COMPUTE REIN-AH-PRIOR-CLMS-PAID = CR-INCUR-DISAMT (SUB14)
03674                                         +  CR-INCUR-DISEXP (SUB14)
03675          IF PC-CHARGEBLE-EXPENSE  OR                              
03676             PC-NON-CHARGEBLE-EXPENSE                              
03677              ADD PC-CLAIM-PAYMENT  TO CR-INCUR-DISEXP (SUB14)     
03678              GO TO 0791-TABLE-AH-EXIT                             
03679          ELSE                                                     
03680              ADD  PC-CLAIM-PAYMENT TO CR-INCUR-DISAMT (SUB14)     
03681              GO TO 0791-TABLE-AH-EXIT.                            
03682                                                                   
03683      ADD +1                       TO SUB14.                       
03684                                                                   
03685      GO TO 0791-TABLE-AH-AMOUNT.                                  
03686                                                                   
03687  0791-TABLE-AH-EXIT.                                              
03688      EXIT.                                                        
03689                                                                   
03690  EJECT                                                            
03691 **************************************************************    
03692 **                                                          **    
03693 ** THIS ROUTINE IS PERFORMED WHEN THERE HAVE BEEN MORE THAN **    
03694 ** 5 DISABILITY PERIODS ATTACHED TO THE SAME CERTIFICATE.   **    
03695 **                                                          **    
03696 ** THE SECOND OCCURRENCE IS ADDED TO THE 1ST OCCURRENCE     **    
03697 ** COMBINING THE TWO OLDEST OCCURRENCES INTO ONE BUCKET     **    
03698 ** THE NEWEST CLAIM INFORMATION IS MOVED INTO THE LAST      **    
03699 ** OCCURRENCE.                                              **    
03700 **************************************************************    
03701  0792-MOVE-TABLE.                                                 
03702                                                                   
03703      MOVE CR-DIS-INCUR-DT (2)  TO CR-DIS-INCUR-DT (1)             
03704                                   WS-CR-DIS-INCUR-DT-N (1).       
03705      ADD  CR-INCUR-DISAMT (2)  TO CR-INCUR-DISAMT (1).            
03706      ADD  CR-INCUR-DISEXP (2)  TO CR-INCUR-DISEXP (1).            
03707                                                                   
03708      MOVE +2                   TO SUB14.                          
03709      MOVE +3                   TO SUB15.                          
03710      PERFORM 0794A-BUMP-TBL    THRU 0794A-BUMP-TBL-EXIT.          
03711                                                                   
03712      MOVE DISABILITY-TBL-MAX   TO SUB14.                          
03713                                                                   
03714      MOVE INCURRED-DATE        TO CR-DIS-INCUR-DT (SUB14)         
03715                                   WS-CR-DIS-INCUR-DT-N (SUB14).   
03716                                                                   
03717  0792-MOVE-TABLE-EXIT.                                            
03718      EXIT.                                                        
03719                                                                   
03720  EJECT                                                            
03721 **************************************************************    
03722 **                                                          **    
03723 **  A REVIEW OF ALL OCCURRENCES IS PERFORMED TO ELIMINATE   **    
03724 ** ANY WITH NO PAYMENTS OR EXPENSES ATTACHED. THESE CAN     **    
03725 ** OCCUR WHEN DISABILITY PAYMENTS HAVE BEEN VOIDED, BECAUSE **    
03726 ** CLAIM ATTACHED TO WRONG CERTIFICATE, OR WRONG CLAIM TYPE **    
03727 ** PAID, OR A CHANGE IN THE INCURRED DATE OF A CLAIM.       **    
03728 **************************************************************    
03729                                                                   
03730  0794-REVIEW-TABLE.                                               
03731                                                                   
03732      IF SUB14 GREATER THAN DISABILITY-TBL-MAX                     
03733          GO TO 0794-REVIEW-TABLE-EXIT.                            
03734                                                                   
03735      IF CR-DIS-INCUR-DT (SUB14) EQUAL ZEROS                       
03736          GO TO 0794-REVIEW-TABLE-EXIT.                            
03737                                                                   
03738      IF CR-INCUR-DISAMT (SUB14) EQUAL ZEROS  AND                  
03739         CR-INCUR-DISEXP (SUB14) EQUAL ZEROS                       
03740          DISPLAY CR-ACCOUNT ' ' CR-CERT-NO ' ' SUB14              
03741          MOVE ZEROS            TO CR-DIS-INCUR-DT (SUB14)         
03742                                   WS-CR-DIS-INCUR-DT-N (SUB14)    
03743          MOVE SUB14            TO SUB15                           
03744          ADD +1                TO SUB15                           
03745          PERFORM 0794A-BUMP-TBL   THRU 0794A-BUMP-TBL-EXIT        
03746          MOVE +1               TO SUB14                           
03747          GO TO 0794-REVIEW-TABLE.                                 
03748                                                                   
03749      ADD +1                    TO SUB14.                          
03750                                                                   
03751      GO TO 0794-REVIEW-TABLE.                                     
03752                                                                   
03753  0794-REVIEW-TABLE-EXIT.                                          
03754      EXIT.                                                        
03755                                                                   
03756  0794A-BUMP-TBL.                                                  
03757                                                                   
03758      IF SUB15 GREATER THAN DISABILITY-TBL-MAX                     
03759          GO TO 0794A-BUMP-TBL-EXIT.                               
03760                                                                   
03761      MOVE CR-DISAB-DETAIL-DATA (SUB15)  TO                        
03762                              CR-DISAB-DETAIL-DATA (SUB14).        
03763                                                                   
03764      MOVE ZEROS                TO CR-DIS-INCUR-DT (SUB15)         
03765                                   WS-CR-DIS-INCUR-DT-N (SUB15)    
03766                                   CR-INCUR-DISAMT (SUB15)         
03767                                   CR-INCUR-DISEXP (SUB15).        
03768                                                                   
03769      ADD +1                    TO SUB14                           
03770                                   SUB15.                          
03771      GO TO 0794A-BUMP-TBL.                                        
03772                                                                   
03773  0794A-BUMP-TBL-EXIT.                                             
03774      EXIT.                                                        
03775                                                                   
103102 0795-DCC-NSP-LF.

103102     MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
103102     MOVE 'L'                        TO DC-OPTION-CODE.           
103102     PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
103102     MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
103102     MOVE SPACES                     TO CP-ACCT-FLD-5.            
103102     MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
103102     MOVE CR-AGE                     TO CP-ISSUE-AGE.             
103102     MOVE CR-APR                     TO CP-LOAN-APR.              
103102     MOVE CR-PMT-FREQ                TO CP-PAY-FREQUENCY.         
103102     MOVE CR-LOAN-TERM               TO CP-LOAN-TERM.             
103102     MOVE ZEROS                      TO CP-DEVIATION-CODE.        
103102     MOVE CR-RATING-CLASS            TO CP-CLASS-CODE.            
103102                                                                  
103102     MOVE '3'                        TO CP-PROCESS-TYPE.          
103102     MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
103102     MOVE CR-PMT-EXTENSION-DAYS      TO CP-TERM-OR-EXT-DAYS.      
103102                                                                  
103102     IF CR-LFTYP = ZERO                                           
103102         GO TO 0795-EXIT
103102     END-IF
103102     
103102     IF (CLAS-I-BAL (CLAS-INDEXL) = 'B')                      
103102         GO TO 0795-EXIT
103102     END-IF
103102     MOVE CR-LFPRM                   TO CP-ORIGINAL-PREMIUM       
103102     MOVE CLAS-I-RL-AH (CLAS-INDEXL) TO CP-BENEFIT-TYPE.          
103102     MOVE CR-LF-TERM                 TO CP-ORIGINAL-TERM.         
103102     MOVE CR-LFAMT                   TO CP-ORIGINAL-BENEFIT       
103102                                        CP-RATING-BENEFIT-AMT.    
103102     MOVE CLAS-I-EP (CLAS-INDEXL)    TO CP-EARNING-METHOD.        
103102     MOVE CLAS-I-BAL (CLAS-INDEXL)   TO CP-SPECIAL-CALC-CD.       
103102     MOVE CR-LFTYP                   TO CP-BENEFIT-CD.            
103102     MOVE LIFE-OVERRIDE-L1           TO CP-LIFE-OVERRIDE-CODE.    
103102     MOVE ZEROS                      TO CP-RATE-DEV-PCT.          
103102     MOVE CR-STATE                   TO CP-STATE.                 
103102     MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV.       
103102                                                                  
103102     IF CP-STATE-STD-ABBRV = 'OR'                                 
103102         COMPUTE CP-RATING-BENEFIT-AMT = CR-LFAMT + CR-LFAMT-ALT. 
103102                                                                  
103102     PERFORM 8100-GET-RATE THRU 8199-GET-RATE-X.                  
103102                                                                  
103102     IF CP-ERROR-RATE-NOT-FOUND  OR                               
103102        CP-ERROR-RATE-IS-ZERO                                     
103102         MOVE 'LIFE NET SINGLE PREMIUM RATE NOT FOUND'            
103102                                     TO WS-ABEND-MESSAGE          
103102         MOVE 0302                   TO WS-ABEND-CODE             
103102         DISPLAY '******************************'
103102         DISPLAY '***** ERROR LOCATION 018 *****'
103102         DISPLAY '******************************'
103102         MOVE WS-ABEND-CODE        TO WS-RETURN-CODE              
103102         GO TO ABEND-PGM.                                         
103102                                                                  
103102     MOVE CP-CALC-PREMIUM          TO CR-LF-NSP-PRM.           

103102*    COMPUTE CR-LF-NSP-PRM ROUNDED =                              
103102*                      (CR-LFAMT / 100) * CP-PREMIUM-RATE.        
103102                                                                  
103102     COMPUTE CR-LF-NSP-PRM ROUNDED =  CR-LF-NSP-PRM +             
103102                   ((CR-LFAMT-ALT / 100) * CP-PREMIUM-RATE).      
103102                                                                  
103102     MOVE CP-PREMIUM-RATE            TO CR-LF-NSP-PRM-RATE.       
103102                                                                   
103102 0795-exit.
103102     exit.

03776  EJECT                                                            
03777 ******************************************************************
03778 ***   P R O C E S S   R E S E R V E   T R A N S A C T I O N S  ***
03779 ******************************************************************
03780                                                                   
03781  0800-PROCESS-RESERVES.                                           
03782                                                                   
03783      IF PC-AH-CLAIM  OR  PC-OB-AH-CLAIM                           
03784        IF CARRIER-UEP-PCT (CLAS-INDEXCN) NOT = ZEROS              
03785            MOVE ZEROS             TO  PC-IBNR-RESERVE-AMT.        
03786                                                                   
03787      PERFORM 3000-REINSURE-ROUTINE THRU 3099-REINSURE-ROUTINE-X.  
03788                                                                   
03789      MOVE PC-INCURRED-DT          TO  DC-BIN-DATE-1.              
03790      MOVE ' '                     TO  DC-OPTION-CODE.             
03791      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
03792      MOVE DC-GREG-DATE-CYMD       TO  INCURRED-DATE.              
03793                                                                   
03794      MOVE PC-REPORTED-DT          TO  DC-BIN-DATE-1.              
03795      MOVE ' '                     TO  DC-OPTION-CODE.             
03796      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
03797      MOVE DC-GREG-DATE-1-YMD      TO  REPORTED-DATE.              
03798                                                                   
03799      MOVE PC-PAID-THRU-DT         TO  DC-BIN-DATE-1.              
03800      MOVE ' '                     TO  DC-OPTION-CODE.             
03801      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
03802      MOVE DC-GREG-DATE-CYMD       TO  PAY-TO-DATE.                
03803                                                                   
03804      PERFORM 8600-SETUP-DETAIL-LINE THRU 8689-SETUP-DETAIL-X.     
03805                                                                   
03806      IF PC-LF-CLAIM  OR  PC-OB-LF-CLAIM                           
03807          ADD +1                     TO W-LF-RESERVES              
03808          ADD PC-IBNR-RESERVE-AMT    TO DTH-IBNR                   
03809          ADD PC-PTC-RESERVE-AMT     TO DTH-PAYCUR                 
03810          ADD PC-FUTURE-RESERVE-AMT  TO DTH-FUTRSV                 
03811          ADD PC-MANUAL-RESERVE-AMT  TO DTH-FUTRSV                 
03812          MOVE INCURRED-DATE         TO CR-DTH-DT                  
03813                                        WS-CR-DTH-DT-N             
03814          MOVE LIFE-OVERRIDE-L6      TO TRD-RSV-DESC               
03815                                                                   
03816      ELSE                                                         
03817          ADD +1                     TO W-AH-RESERVES              
03818          ADD PC-IBNR-RESERVE-AMT    TO DIS-IBNR                   
03819          ADD PC-PTC-RESERVE-AMT     TO DIS-PAYCUR                 
03820          ADD PC-FUTURE-RESERVE-AMT  TO DIS-FUTRSV                 
03821          ADD PC-MANUAL-RESERVE-AMT  TO DIS-FUTRSV                 
03822          MOVE INCURRED-DATE         TO CR-DIS-DT                  
03823                                        WS-CR-DIS-DT-N             
03824          MOVE AH-OVERRIDE-L6        TO TRD-RSV-DESC.              
03825                                                                   
03826      MOVE PC-IBNR-RESERVE-AMT       TO TRD-IBNR.                  
03827      MOVE 'IBNR'                    TO TRD-IBNRX.                 
03828      MOVE PC-PTC-RESERVE-AMT        TO TRD-PAYCUR.                
03829      MOVE 'PAY CURR'                TO TRD-PAYCURX.               
03830      COMPUTE TRD-FUTRSV EQUAL                                     
03831             (PC-FUTURE-RESERVE-AMT + PC-MANUAL-RESERVE-AMT).      
03832      MOVE 'FUTURE'                  TO TRD-FUTRSVX.               
03833      MOVE ' CLAIM RESERVES      '   TO TRD-RSV-ACTION.            
03834                                                                   
03835  0820-END-RESERVE-PROCESSING.                                     
03836                                                                   
03837      PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.       
03838                                                                   
03839      IF NOT CR-POLICY-IS-REIN-ONLY                                
03840          PERFORM 2400-BUILD-RESERVE-EXTRACT THRU                  
03841                                             2499-BLD-RESERVE-X    
03842          ADD +1 TO BC-G-CRS.                                      
03843                                                                   
03844      PERFORM 3400-REINSURE-RESERVE THRU 3499-REINSURE-RESERVE-X.  
03845                                                                   
03846      MOVE '*'                    TO TRANSACTION-FLAG.             
03847      GO TO 0300-READ-PENDING-TRANSACTIONS.                        
03848                                                                   
03849  EJECT                                                            
03850 ******************************************************************
03851 ***    P R O C E S S   C H A N G E   T R A N S A C T I O N S   ***
03852 ******************************************************************
03853                                                                   
03854  0900-PROCESS-CERT-CHANGES.                                       
03855                                                                   
03856      PERFORM 8600-SETUP-DETAIL-LINE THRU 8689-SETUP-DETAIL-X.     
03857                                                                   
03858      IF CC-AGE NOT NUMERIC  OR                                    
03859         CC-AGE = CR-AGE                                           
03860          MOVE ZEROS TO CC-AGE.                                    
03861                                                                   
03862      IF CC-INSURED-JOINT-AGE NOT NUMERIC  OR                      
03863         CC-INSURED-JOINT-AGE = CR-JOINT-AGE                       
03864          MOVE ZEROS TO CC-INSURED-JOINT-AGE.                      
03865                                                                   
03866      IF CC-INSURED-SEX NOT = 'M'  AND                             
03867         CC-INSURED-SEX NOT = 'F'                                  
03868          MOVE SPACES TO CC-INSURED-SEX.                           
03869                                                                   
03870      IF CC-LOAN-APR NOT NUMERIC  OR                               
03871         CC-LOAN-APR = CR-APR                                      
03872          MOVE ZEROS TO CC-LOAN-APR.                               
03873                                                                   
03874      IF CC-PAY-FREQUENCY NOT NUMERIC  OR                          
03875         CC-PAY-FREQUENCY = CR-PMT-FREQ                            
03876          MOVE ZEROS TO CC-PAY-FREQUENCY.                          
03877                                                                   
03878      IF CC-IND-GRP-TYPE NOT = '1'  AND                            
03879         CC-IND-GRP-TYPE NOT = '2'                                 
03880          MOVE SPACES TO CC-IND-GRP-TYPE.                          
03881                                                                   
03882      IF CC-CLAIM-DEDUCT-WITHHELD NOT NUMERIC  OR                  
03883         CC-CLAIM-DEDUCT-WITHHELD = CR-CLAIM-DEDUCT-WITHHELD       
03884          MOVE ZEROS TO CC-CLAIM-DEDUCT-WITHHELD.                  
03885                                                                   
03886      IF CC-CANCEL-DEDUCT-WITHHELD NOT NUMERIC  OR                 
03887         CC-CANCEL-DEDUCT-WITHHELD = CR-CANCEL-DEDUCT-WITHHELD     
03888          MOVE ZEROS TO CC-CANCEL-DEDUCT-WITHHELD.                 
03889                                                                   
03890      IF CC-LF-ORIG-TERM NOT NUMERIC  OR                           
03891         CC-LF-ORIG-TERM = CR-LF-TERM  OR                          
03892         CR-LFTYP = ZEROS                                          
03893          MOVE ZEROS TO CC-LF-ORIG-TERM.                           
03894                                                                   
03895      IF CC-AH-ORIG-TERM NOT NUMERIC  OR                           
03896         CC-AH-ORIG-TERM = CR-AH-TERM  OR                          
03897         CR-AHTYP = ZEROS                                          
03898          MOVE ZEROS TO CC-AH-ORIG-TERM.                           
03899                                                                   
03900      IF CR-BILLED NOT NUMERIC                                     
03901          MOVE ZEROS TO CR-BILLED.                                 
03902                                                                   
03903      IF CR-LIVES NOT NUMERIC                                      
03904          MOVE ZEROS TO CR-LIVES.                                  
03905                                                                   
03906      IF CC-LIVES NOT NUMERIC  OR                                  
03907         CC-LIVES = CR-LIVES                                       
03908          MOVE ZEROS TO CC-LIVES.                                  
03909                                                                   
03910      IF CC-BILLED NOT NUMERIC  OR                                 
03911         CC-BILLED = CR-BILLED                                     
03912          MOVE ZEROS TO CC-BILLED.                                 
03913                                                                   
03914      IF CC-INSURED-LAST-NAME NOT = SPACES                         
03915          MOVE CC-INSURED-LAST-NAME   TO CR-LNAME.                 
03916      IF CC-INSURED-FIRST-NAME NOT = SPACES                        
03917          MOVE CC-INSURED-FIRST-NAME  TO CR-FNAME.                 
03918      IF CC-INSURED-INITIAL2 NOT = SPACES                          
03919          MOVE CC-INSURED-INITIAL2    TO CR-INIT.                  
03920      IF CC-JT-LAST-NAME NOT = SPACES                              
03921          MOVE CC-JT-LAST-NAME        TO CR-JT-LNAME.              
03922      IF CC-JT-FIRST-NAME NOT = SPACES                             
03923          MOVE CC-JT-FIRST-NAME       TO CR-JT-FNAME.              
03924      IF CC-JT-INITIAL NOT = SPACES                                
03925          MOVE CC-JT-INITIAL          TO CR-JT-INIT.               
03926                                                                   
03927      IF CC-AGE NOT = ZEROS                                        
03928          MOVE CC-AGE                 TO CR-AGE                    
03929                                         DET-AGE                   
03930      ELSE                                                         
03931          MOVE ZEROS                  TO DET-AGE.                  
03932                                                                   
03933      IF CC-INSURED-JOINT-AGE NOT = ZEROS                          
03934          MOVE CC-INSURED-JOINT-AGE   TO CR-JOINT-AGE              
03935                                         DET-JT-AGE                
03936      ELSE                                                         
03937          MOVE ZEROS                  TO DET-JT-AGE.               
03938                                                                   
03939      IF CC-INSURED-SEX NOT = SPACES                               
03940          MOVE CC-INSURED-SEX         TO CR-SEX                    
03941                                         DET-SEX                   
03942      ELSE                                                         
03943          MOVE SPACE                  TO DET-SEX.                  
03944                                                                   
03945      IF CC-MEMBER-NO NOT = SPACES                                 
03946          MOVE CC-MEMBER-NO           TO CR-MEMBER-NO              
03947                                         DET-PR-MEM                
03948      ELSE                                                         
03949          MOVE SPACES                 TO DET-PR-MEM.               
03950                                                                   
03951      IF CC-SOC-SEC-NO NOT = SPACES                                
03952          MOVE CC-SOC-SEC-NO          TO CR-SOC-SEC                
03953                                         DET-PR-SS                 
03954      ELSE                                                         
03955          MOVE SPACES                 TO DET-PR-SS.                
03956                                                                   
03957      IF CC-LOAN-APR NOT = ZEROS                                   
03958          MOVE CC-LOAN-APR            TO CR-APR                    
03959                                         DET-PR-APR                
03960      ELSE                                                         
03961          MOVE ZEROS                  TO DET-PR-APR.               
03962                                                                   
03963      IF CC-AGE NOT = ZERO  OR                                     
03964         CC-INSURED-JOINT-AGE NOT = ZERO  OR                       
03965         CC-INSURED-SEX NOT = SPACE  OR                            
03966         CC-MEMBER-NO NOT = SPACES  OR                             
03967         CC-SOC-SEC-NO NOT = SPACES  OR                            
03968         CC-LOAN-APR NOT = ZERO                                    
03969          MOVE DET-PR-UREC            TO TR-DTL2                   
03970          MOVE 'Y'                    TO WS-CHANGE-REC-SW          
03971          PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.   
03972                                                                   
03973      IF CC-LF-ORIG-TERM NOT = ZEROS                               
03974          MOVE CR-LF-TERM             TO DET-O-LF-TERM             
03975          MOVE CC-LF-ORIG-TERM        TO CR-LF-TERM                
03976                                         DET-N-LF-TERM             
03977      ELSE                                                         
03978          MOVE ZEROS                  TO DET-O-LF-TERM             
03979                                         DET-N-LF-TERM.            
03980                                                                   
03981      IF CC-AH-ORIG-TERM NOT = ZEROS                               
03982          MOVE CR-AH-TERM             TO DET-O-AH-TERM             
03983          MOVE CC-AH-ORIG-TERM        TO CR-AH-TERM                
03984                                         DET-N-AH-TERM             
03985      ELSE                                                         
03986          MOVE ZEROS                  TO DET-O-AH-TERM             
03987                                         DET-N-AH-TERM.            
03988                                                                   
03989      IF CC-LIVES NOT = ZEROS                                      
03990          MOVE CR-LIVES               TO DET-O-LIVES               
03991          MOVE CC-LIVES               TO CR-LIVES                  
03992                                         DET-N-LIVES               
03993      ELSE                                                         
03994          MOVE ZEROS                  TO DET-O-LIVES               
03995                                         DET-N-LIVES.              
03996                                                                   
03997      IF CC-BILLED NOT  = ZEROES                                   
03998          MOVE CC-BILLED              TO CR-BILLED.                
03999                                                                   
04000      IF CC-LF-ORIG-TERM NOT = ZERO  OR                            
04001         CC-AH-ORIG-TERM NOT = ZERO  OR                            
04002         CC-LIVES NOT = ZERO                                       
04003          MOVE DET-PR-UREC-2          TO TR-DTL2                   
04004          MOVE 'Y'                    TO WS-CHANGE-REC-SW          
04005          PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.   
04006                                                                   
04007      IF CC-IND-GRP-TYPE NOT = SPACES                              
04008          MOVE CC-IND-GRP-TYPE        TO CR-IND-GRP                
04009                                         DET-IND-GRP               
04010      ELSE                                                         
04011          MOVE SPACE                  TO DET-IND-GRP.              
04012                                                                   
04013      IF CC-PAY-FREQUENCY NOT = ZEROS                              
04014          MOVE CC-PAY-FREQUENCY       TO CR-PMT-FREQ               
04015                                         DET-PMT-FREQ              
04016      ELSE                                                         
04017          MOVE ZEROS                  TO DET-PMT-FREQ.             
04018                                                                   
04019      IF CC-POLICY-FORM-NO NOT = SPACES                            
04020          MOVE CC-POLICY-FORM-NO      TO CR-POLICY-FORM-NO         
04021                                         DET-POL-FORM              
04022      ELSE                                                         
04023          MOVE SPACES                 TO DET-POL-FORM.             
04024                                                                   
04025      IF CC-LOAN-OFFICER NOT = SPACES                              
04026          MOVE CC-LOAN-OFFICER        TO CR-LOAN-OFFICER           
04027                                         DET-LOAN-OFFICER          
04028      ELSE                                                         
04029          MOVE SPACES                 TO DET-LOAN-OFFICER.         
04030                                                                   
04031      IF CC-USER-FIELD NOT = SPACES                                
04032          MOVE CC-USER-FIELD          TO CR-USER-CODE              
04033                                         DET-USER-CODE             
04034      ELSE                                                         
04035          MOVE SPACE                  TO DET-USER-CODE.            
04036                                                                   
04037      IF CC-IND-GRP-TYPE NOT = SPACE  OR                           
04038         CC-PAY-FREQUENCY NOT = ZEROS  OR                          
04039         CC-POLICY-FORM-NO NOT = SPACES  OR                        
04040         CC-LOAN-OFFICER NOT = SPACES  OR                          
04041         CC-USER-FIELD NOT = SPACES                                
04042          MOVE DET-PR-UREC-3          TO TR-DTL2                   
04043          MOVE 'Y'                    TO WS-CHANGE-REC-SW          
04044          PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.   
04045                                                                   
04046      IF CC-CLAIM-DEDUCT-WITHHELD NOT = ZEROS                      
04047          MOVE CC-CLAIM-DEDUCT-WITHHELD                            
04048                                      TO CR-CLAIM-DEDUCT-WITHHELD  
04049                                         DET-CLM-DEDUCT            
04050      ELSE                                                         
04051          MOVE ZEROS                  TO DET-CLM-DEDUCT.           
04052                                                                   
04053      IF CC-CANCEL-DEDUCT-WITHHELD NOT = ZEROS                     
04054          MOVE CC-CANCEL-DEDUCT-WITHHELD                           
04055                                      TO CR-CANCEL-DEDUCT-WITHHELD 
04056                                         DET-CAN-DEDUCT            
04057      ELSE                                                         
04058          MOVE ZEROS                  TO DET-CAN-DEDUCT.           
04059                                                                   
04060      IF CC-CLAIM-DEDUCT-WITHHELD NOT = ZEROS  OR                  
04061         CC-CANCEL-DEDUCT-WITHHELD NOT = ZEROS                     
04062          MOVE DET-PR-UREC-4          TO TR-DTL2                   
04063          MOVE 'Y'                    TO WS-CHANGE-REC-SW          
04064          PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.   
04065                                                                   
04066      IF CC-LF-PREMIUM-AMT NOT NUMERIC                             
04067          MOVE ZEROS              TO  CC-LF-PREMIUM-AMT.           
04068                                                                   
04069      IF CC-LF-BENEFIT-AMT NOT NUMERIC                             
04070          MOVE ZEROS              TO  CC-LF-BENEFIT-AMT.           
04071                                                                   
04072      IF CC-AH-PREMIUM-AMT NOT NUMERIC                             
04073          MOVE ZEROS              TO  CC-AH-PREMIUM-AMT.           
04074                                                                   
04075      IF CC-AH-BENEFIT-AMT NOT NUMERIC                             
04076          MOVE ZEROS              TO  CC-AH-BENEFIT-AMT.           
04077                                                                   
04078      IF CC-LF-BENEFIT-CD NOT = SPACES                             
04079          MOVE CC-LF-BENEFIT-CD   TO  CR-LFTYP                     
04080                                      DET-LF-BEN-CD                
04081      ELSE                                                         
04082          MOVE SPACES             TO DET-LF-BEN-CD.                
04083                                                                   
04084      IF CC-LF-PREMIUM-AMT NOT = ZEROS                             
04085          MOVE CC-LF-PREMIUM-AMT  TO  CR-LFPRM                     
04086                                      DET-LF-PREM                  
04087      ELSE                                                         
04088          MOVE ZEROS              TO DET-LF-PREM.                  
04089                                                                   
04090      IF CC-LF-BENEFIT-AMT NOT = ZEROS                             
04091          MOVE CC-LF-BENEFIT-AMT  TO  CR-LFAMT                     
04092                                      DET-LF-BEN                   
04093      ELSE                                                         
04094          MOVE ZEROS              TO DET-LF-BEN.                   
04095                                                                   
04096      IF (CC-LF-BENEFIT-CD NOT = SPACES)  OR                       
04097         (CC-LF-PREMIUM-AMT NOT = ZEROS)  OR                       
04098         (CC-LF-BENEFIT-AMT NOT = ZEROS)                           
04099          MOVE DET-PR-UREC-5          TO TR-DTL2                   
04100          MOVE 'Y'                    TO WS-CHANGE-REC-SW          
04101          PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.   
04102                                                                   
04103      IF CC-AH-BENEFIT-CD NOT = SPACES                             
04104          MOVE CC-AH-BENEFIT-CD   TO  CR-AHTYP                     
04105                                      DET-AH-BEN-CD                
04106      ELSE                                                         
04107          MOVE SPACES             TO DET-AH-BEN-CD.                
04108                                                                   
04109      IF CC-AH-PREMIUM-AMT NOT = ZEROS                             
04110          MOVE CC-AH-PREMIUM-AMT  TO  CR-AHPRM                     
04111                                      DET-AH-PREM                  
04112      ELSE                                                         
04113          MOVE ZEROS              TO DET-AH-PREM.                  
04114                                                                   
04115      IF CC-AH-BENEFIT-AMT NOT = ZEROS                             
04116          MOVE CC-AH-BENEFIT-AMT  TO  CR-AHAMT                     
04117                                      DET-AH-BEN                   
04118      ELSE                                                         
04119          MOVE ZEROS              TO DET-AH-BEN.                   
04120                                                                   
04121      IF CC-AH-BENEFIT-CD NOT = SPACES OR                          
04122         CC-AH-PREMIUM-AMT NOT = ZEROS OR                          
04123         CC-AH-BENEFIT-AMT NOT = ZEROS                             
04124          MOVE DET-PR-UREC-6          TO TR-DTL2                   
04125          MOVE 'Y'                    TO WS-CHANGE-REC-SW          
04126          PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.   
04127                                                                   
04128  0910-END-CHANGE-PROCESSING.                                      
04129                                                                   
04130      IF CR-LNAME = ALL '*'                                        
04131          MOVE SPACES                 TO CR-LNAME.                 
04132      IF CR-FNAME = ALL '*'                                        
04133          MOVE SPACES                 TO CR-FNAME.                 
04134      IF CR-INIT = '*'                                             
04135          MOVE SPACES                 TO CR-INIT.                  
04136      IF CR-SOC-SEC = ALL '*'                                      
04137          MOVE SPACES                 TO CR-SOC-SEC.               
04138      IF CR-JT-LNAME = ALL '*'                                     
04139          MOVE SPACES                 TO CR-JT-LNAME.              
04140      IF CR-JT-FNAME = ALL '*'                                     
04141          MOVE SPACES                 TO CR-JT-FNAME.              
04142      IF CR-JT-INIT = '*'                                          
04143          MOVE SPACES                 TO CR-JT-INIT.               
04144      IF CR-POLICY-FORM-NO = ALL '*'                               
04145          MOVE SPACES                 TO CR-POLICY-FORM-NO.        
04146      IF CR-MEMBER-NO = ALL '*'                                    
04147          MOVE SPACES                 TO CR-MEMBER-NO.             
04148      IF CR-LOAN-OFFICER = ALL '*'                                 
04149          MOVE SPACES                 TO CR-LOAN-OFFICER.          
04150      IF CR-USER-CODE = ALL '*'                                    
04151          MOVE SPACES                 TO CR-USER-CODE.             
04152                                                                   
04153      ADD +1 TO BC-G-CTN.                                          
04154      GO TO 0300-READ-PENDING-TRANSACTIONS.                        
04155  EJECT                                                            
111204 0920-GET-BANK-FEES.

           IF CR-CERT-NO = '0008771354 '
              DISPLAY ' MADE IT TO 0920 '
           END-IF

           MOVE DTE-CLASIC-COMPANY-CD  TO AG-COMPANY-CD
           MOVE CR-CARRIER             TO AG-CARRIER
           MOVE CR-GROUPING            TO AG-GROUPING
           MOVE CR-BANK-NO             TO AG-BANK
           MOVE WS-CR-BIN-DATE         TO AG-EXP-DT
           MOVE 'B'                    TO AG-TYPE

      *    IF WS-PREV-BANK-COMM-KEY = AG-CONTROL-PRIMARY
      *       PERFORM 0925-BUILD-WORK-BNK-COMM
      *                                THRU 0925-EXIT
      *       GO TO 0920-EXIT
      *    END-IF

           START ERAGTC KEY NOT < AG-CONTROL-PRIMARY

           IF CR-CERT-NO = '0008771354 '
              DISPLAY ' MADE IT AFTER START '
           END-IF

           .
       0920-READ-ERAGTC.
       
           IF ERAGTC-FILE-STATUS = '00'
              READ ERAGTC NEXT RECORD
           IF CR-CERT-NO = '0008771354 '
              DISPLAY ' MADE ONE READ '
           END-IF

              IF ERAGTC-FILE-STATUS = '00'
                 IF (CR-CARRIER         = AG-CARRIER)
                    AND (CR-GROUPING    = AG-GROUPING)
                    AND (CR-BANK-NO     = AG-BANK)
                    IF WS-CR-BIN-DATE  = AG-EXP-DT
                       GO TO 0920-READ-ERAGTC
                    ELSE
                       IF (WS-CR-BIN-DATE NOT < AG-EFF-DT)
                          AND (WS-CR-BIN-DATE < AG-EXP-DT)
                          PERFORM 0925-BUILD-WORK-BNK-COMM
                                       THRU 0925-EXIT
                          MOVE AG-CONTROL-PRIMARY
                                       TO WS-PREV-BANK-COMM-KEY
                          GO TO 0920-EXIT
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF

           MOVE WS-BANK-INIT           TO WS-BANK-COMMISSIONS
           PERFORM 0925-BUILD-WORK-BNK-COMM

           .
       0920-EXIT.
           EXIT.

       0925-BUILD-WORK-BNK-COMM.

           IF CR-CERT-NO = '0008771354 '
              DISPLAY ' MADE IT TO 0925 '
           END-IF

           MOVE +1                     TO IA IB
           
100703     IF (CR-BANK-NO NOT = SPACES AND ZEROS)
100703        AND (PB-I-BANK-FEE NOT = ZEROS)
100703        MOVE CR-BANK-NO          TO WS-BNK-AGT (IB)
100703        MOVE 'B'                 TO WS-BNK-TYPE (IB)
              MOVE PB-I-BANK-FEE       TO WS-BNK-SPP-FEES (IB)
              MOVE ' '                 TO WS-BNK-RECALC (IB)
              ADD +1                   TO IB
100703     END-IF
           .
       0925-BUILD-CONTINUE.
       
           IF CR-CERT-NO = '00087735841'
              DISPLAY ' WS-BNK-AGT ' WS-BNK-AGT (1)
              MOVE WS-BNK-SPP-FEES (1) TO WS-DISP-AMT
              DISPLAY ' WS-BNK-FEE ' WS-DISP-AMT
           END-IF
           
           PERFORM VARYING IA FROM +2 BY +1 UNTIL
              (IA > +10)
              OR (IB > +10)
              IF AG-AGT (IA) NOT = SPACES AND ZEROS
                 MOVE AG-AGT (IA)      TO WS-BNK-AGT (IB)
                 MOVE 'B'              TO WS-BNK-TYPE (IB)
      *          MOVE AG-COM-TYP (IA)  TO WS-BNK-TYPE (IB)
                 IF AG-SPP-FEES (IA) NOT NUMERIC
                    MOVE ZEROS         TO AG-SPP-FEES (IA)
                 END-IF
                 MOVE AG-SPP-FEES (IA) TO WS-BNK-SPP-FEES (IB)
                 MOVE AG-RECALC-LV-INDIC (IA)
                                       TO WS-BNK-RECALC (IB)
              ELSE
                 MOVE SPACES           TO WS-BNK-AGT (IB)
                                          WS-BNK-TYPE (IB)
                                          WS-BNK-RECALC (IB)
                 MOVE ZEROS            TO WS-BNK-SPP-FEES (IB)
              END-IF
              ADD +1                   TO IB
           END-PERFORM

           .
       0925-EXIT.
           EXIT.
                                            
111204 0930-BUILD-CERT-TRLR.

           MOVE SPACES                 TO CERTIFICATE-TRAILERS
           MOVE 'CS'                   TO CS-RECORD-ID
           MOVE DTE-CLASIC-COMPANY-CD  TO CS-COMPANY-CD
           MOVE CR-CARRIER             TO CS-CARRIER
           MOVE CR-GROUPING            TO CS-GROUPING
           MOVE CR-STATE               TO CS-STATE
           MOVE CR-ACCOUNT             TO CS-ACCOUNT
           MOVE WS-CR-BIN-DATE         TO CS-CERT-EFF-DT
           MOVE CR-CERT-NO             TO CS-CERT-NO
           MOVE 'A'                    TO CS-TRAILER-TYPE

           MOVE WS-BANK-COMMISSIONS    TO CS-BANK-COMMISSION-AREA
           
           IF CS-AGT (1) NOT = SPACES AND ZEROS
              WRITE CERTIFICATE-TRAILERS
              IF ELCRTT-FILE-STATUS = '00'
                 CONTINUE
              ELSE
                 IF ELCRTT-FILE-STATUS = '22'
                    READ ELCRTT
                    IF ELCRTT-FILE-STATUS = '00'
                       MOVE WS-BANK-COMMISSIONS
                                       TO CS-BANK-COMMISSION-AREA
                       REWRITE CERTIFICATE-TRAILERS
                       IF ELCRTT-FILE-STATUS NOT = '00'
                          DISPLAY ' BAD REWRITE ELCRTT '
                                       ELCRTT-FILE-STATUS
                          PERFORM ABEND-PGM
                       END-IF
                    ELSE
                       DISPLAY ' BAD READ    ELCRTT '
                                        ELCRTT-FILE-STATUS
                       PERFORM ABEND-PGM
                    END-IF
                 ELSE
                    DISPLAY ' BAD WRITE ELCRTT '
                                       ELCRTT-FILE-STATUS
                    PERFORM ABEND-PGM
                 END-IF
              END-IF
           END-IF

           .
       0930-EXIT.
           EXIT.

04156 ******************************************************************
04157 ***      S E T   C O M M I S S I O N   S T R U C T U R E       ***
04158 ******************************************************************
04159                                                                   
04160  1000-SET-COMMISSION-STRUCTURE.                                   
04161                                                                   
04162      MOVE +1                     TO IA                            
04163                                     IB.                           
04164                                                                   
04165  1100-GET-COMMISSION-LEVEL.                                       
04166                                                                   
04167      MOVE AM-AGT-COMMS (IA)      TO WK-LEVEL.                     
04168                                                                   
04169      IF WK-AGT = '0000000000' OR SPACES                           
04170          MOVE +0                 TO WK-L-RT                       
04171                                     WK-J-RT                       
04172                                     WK-A-RT                       
04173          GO TO 1200-SET-COMMISSION-LEVEL.                         
04174                                                                   
04175  1110-LIFE-COMMISSION.                                            
04176                                                                   
04177      IF CR-LFTYP = ZERO  OR                                       
04178        (CR-ENTRY-STATUS = 'P' OR 'D' OR 'V')                      
04179          MOVE +0 TO WK-L-RT                                       
04180                     WK-J-RT                                       
04181          GO TO 1130-A-H-COMMISSION.                               
04182                                                                   
04183      IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                          
04184          GO TO 1120-JOINT-LIFE-COMMISSION.                        
04185                                                                   
04186      IF WK-L-RT NUMERIC                                           
PEMUNI        AND (WK-L-RTX (3:1) NOT = 'L' AND 'M')
04187          MOVE WK-L-RT                TO WS-COMMISSION
04188          PERFORM 6650-GET-RES-STATE-COMM  THRU 6700-COMM-EXIT     
04189          MOVE WS-COMMISSION          TO WK-L-RT                   
04190          GO TO 1130-A-H-COMMISSION.                               
04191                                                                   
04192      MOVE LIFE-OVERRIDE-L1           TO CTBL-BEN-TYPE.            
04193      MOVE CR-LFTYP                   TO CTBL-BEN-CODE.            
04194      MOVE ZEROS                      TO WS-SUB1.                  
04195      PERFORM 6650-GET-RES-STATE-COMM  THRU 6700-COMM-EXIT         
04196                                                                   
04197      IF WS-SUB1 = ZEROS                                           
04198          MOVE WK-L-RTX               TO CTBL-TABLE-CD             
04199      ELSE                                                         
04200          MOVE WK-RATE                TO WK-L-RT                   
04201          GO TO 1130-A-H-COMMISSION.                               
04202                                                                   
04203      PERFORM 1300-READ-ERCTBL-FILE THRU 1399-READ-ERCTBL-X.       
04204                                                                   
04205      MOVE CR-LFAMT                   TO COMM-CK-AMT.              
04206      MOVE CR-LF-TERM                 TO WS-COMM-TERM.             
04207      PERFORM 1400-GET-COMM-PERCENT THRU 1489-GET-PERCENT-X.       
04208      MOVE WK-RATE                    TO WK-L-RT.                  
04209                                                                   
04210      GO TO 1130-A-H-COMMISSION.                                   
04211                                                                   
04212  1120-JOINT-LIFE-COMMISSION.                                      
04213                                                                   
04214      IF WK-J-RT NUMERIC                                           
PEMUNI        AND (WK-J-RTX (3:1) NOT = 'L' AND 'M')
04215          MOVE WK-J-RT                TO WS-COMMISSION             
04216          PERFORM 6650-GET-RES-STATE-COMM  THRU 6700-COMM-EXIT     
04217          MOVE WS-COMMISSION          TO WK-J-RT                   
04218          GO TO 1130-A-H-COMMISSION.                               
04219                                                                   
04220      MOVE LIFE-OVERRIDE-L1           TO CTBL-BEN-TYPE.            
04221      MOVE CR-LFTYP                   TO CTBL-BEN-CODE.            
04222      MOVE ZEROS                      TO WS-SUB1.                  
04223      PERFORM 6650-GET-RES-STATE-COMM  THRU 6700-COMM-EXIT         
04224                                                                   
04225      IF WS-SUB1 = ZEROS                                           
04226          MOVE WK-J-RTX               TO CTBL-TABLE-CD             
04227      ELSE                                                         
04228          MOVE WK-RATE                TO WK-J-RT                   
04229          GO TO 1130-A-H-COMMISSION.                               
04230                                                                   
04231      PERFORM 1300-READ-ERCTBL-FILE THRU 1399-READ-ERCTBL-X.       
04232                                                                   
04233      MOVE CR-LFAMT                   TO COMM-CK-AMT.              
04234      MOVE CR-LF-TERM                 TO WS-COMM-TERM.             
04235      PERFORM 1400-GET-COMM-PERCENT THRU 1489-GET-PERCENT-X.       
04236      MOVE WK-RATE                    TO WK-J-RT.                  
04237                                                                   
04238  1130-A-H-COMMISSION.                                             
04239                                                                   
04240      IF CR-AHTYP = ZERO  OR                                       
04241        (CR-ENTRY-STATUS = 'P' OR 'D' OR 'V')                      
04242          MOVE +0 TO WK-A-RT                                       
04243          GO TO 1200-SET-COMMISSION-LEVEL.                         
04244                                                                   
04245      IF WK-A-RT NUMERIC                                           
PEMUNI        AND (WK-A-RTX (3:1) NOT = 'L' AND 'M')
04246          MOVE WK-A-RT                TO WS-COMMISSION             
04247          PERFORM 6650-GET-RES-STATE-COMM  THRU 6700-COMM-EXIT     
04248          MOVE WS-COMMISSION          TO WK-A-RT                   
04249          GO TO 1200-SET-COMMISSION-LEVEL.                         
04250                                                                   
04251      MOVE AH-OVERRIDE-L1             TO CTBL-BEN-TYPE.            
04252      MOVE CR-AHTYP                   TO CTBL-BEN-CODE.            
04253      MOVE ZEROS                      TO WS-SUB1.                  
04254      PERFORM 6650-GET-RES-STATE-COMM  THRU 6700-COMM-EXIT         
04255                                                                   
04256      IF WS-SUB1 = ZEROS                                           
04257          MOVE WK-A-RTX               TO CTBL-TABLE-CD             
04258      ELSE                                                         
04259          MOVE WK-RATE                TO WK-A-RT                   
04260          GO TO 1200-SET-COMMISSION-LEVEL.                         
04261                                                                   
04262      PERFORM 1300-READ-ERCTBL-FILE THRU 1399-READ-ERCTBL-X.       
04263                                                                   
04264      COMPUTE COMM-CK-AMT = CR-AHAMT * CR-AH-TERM.                 
04265      MOVE CR-AH-TERM                 TO WS-COMM-TERM.             
04266      PERFORM 1400-GET-COMM-PERCENT THRU 1489-GET-PERCENT-X.       
04267      MOVE WK-RATE                    TO WK-A-RT.                  
04268                                                                   
04269  1200-SET-COMMISSION-LEVEL.                                       
04270                                                                   
04271      MOVE WK-AGT                      TO WK-N-AGT  (IA).          
04272      IF WK-AGT = '0000000000' OR SPACES                           
04273          MOVE SPACE                   TO WK-N-TYPE (IA)           
04274      ELSE                                                         
04275          MOVE WK-A-TYPE               TO WK-N-TYPE (IA).          
04276                                                                   
04277      IF CR-LFTYP = ZERO                                           
04278          MOVE +0                      TO WK-L-RT1  (IA)           
04279      ELSE                                                         
04280          IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                      
04281              MOVE WK-J-RT             TO WK-L-RT1 (IA)            
04282          ELSE                                                     
04283              MOVE WK-L-RT             TO WK-L-RT1 (IA).           
04284                                                                   
04285      MOVE WK-A-RT                     TO WK-A-RT1 (IA).           
04286                                                                   
04287      IF RC-COMM-FLG = 'X'                                         
04288         IF (AM-RECALC-LV-INDIC (IA) = 'N')
092705*          OR ((CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
100703*             AND (IA = +5))
04289             MOVE CR-COM-AGT  (IA)         TO WK-N-AGT  (IA)       
04290             MOVE CR-AGT-TYPE (IA)         TO WK-N-TYPE (IA)       
04291             MOVE CR-LCOM-L   (IA)         TO WK-L-RT1  (IA)       
04292             MOVE CR-LCOM-AH  (IA)         TO WK-A-RT1  (IA).      
04293                                                                   
04294      IF IA LESS THAN +10                                          
04295          ADD +1 TO IA                                             
04296          GO TO 1100-GET-COMMISSION-LEVEL.                         
04297                                                                   
04298      GO TO 1499-SET-COMM-X.                                       
04299                                                                   
04300  1300-READ-ERCTBL-FILE.                                           
04301                                                                   
04302      IF CTBL-SEARCH = LAST-CTBL                                   
04303          GO TO 1399-READ-ERCTBL-X.                                
04304                                                                   
04305      MOVE SPACE                          TO CTBL-ERROR-SWITCH.    
04306                                                                   
04307      IF COMM-OPEN-SW = ' '                                        
04308          MOVE 'X'                        TO COMM-OPEN-SW          
04309          MOVE DTE-CLASIC-COMPANY-CD      TO CTBL-COMP-CD          
04310          OPEN INPUT ERCTBL                                        
04311          IF ERCTBL-FILE-STATUS = '00'  OR  '97'                   
04312              NEXT SENTENCE                                        
04313             ELSE                                                  
04314              MOVE '41'                   TO ABEND-CODE-1          
04315              MOVE ERCTBL-FILE-STATUS     TO ABEND-CODE-2          
CIDMOD             DISPLAY '******************************'
CIDMOD             DISPLAY '***** ERROR LOCATION 012 *****'
CIDMOD             DISPLAY '******************************'
CIDMOD             DISPLAY '******************************'
04316              MOVE WS-ABEND-CODE          TO WS-RETURN-CODE        
04317              GO TO ABEND-PGM.                                     
04318                                                                   
04319      MOVE CTBL-SEARCH                    TO CT-CONTROL-PRIMARY    
04320                                             LAST-CTBL.            
04321      READ ERCTBL.                                                 
04322                                                                   
04323      IF ERCTBL-FILE-STATUS = '23'                                 
04324          DISPLAY 'INVALID COMM TABLE CODE FOR: ' CR-FULL-CONTROL  
04325                  ' ENTERED: ' CR-ENTRY-STATUS '-' CR-ENTRY-DATE   
04326                  ' CTBL: ' CTBL-SEARCH                            
04327          MOVE '*'                TO CTBL-ERROR-SWITCH             
04328          MOVE '44'               TO ABEND-CODE-1                  
04329          MOVE ERCTBL-FILE-STATUS TO ABEND-CODE-2                  
04324          DISPLAY 'CT-CONTROL-PRIMARY = ' CT-CONTROL-PRIMARY       
CIDMOD         DISPLAY '******************************'
CIDMOD         DISPLAY '***** ERROR LOCATION 013 *****'
CIDMOD         DISPLAY '******************************'
04330          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                
CIDMOD*        IF DTE-CLIENT NOT = 'NCL'                                
CIDMOD         IF DTE-CLIENT NOT = 'NCL' AND 'CID' AND 'CSO'            
04332              GO TO ABEND-PGM                                      
04333          ELSE                                                     
04334              NEXT SENTENCE                                        
04335      ELSE                                                         
04336          IF ERCTBL-FILE-STATUS NOT = '00'                         
04337              MOVE 'READ ERROR ON ERCTBL' TO WS-ABEND-MESSAGE      
04338              MOVE '44'               TO ABEND-CODE-1              
04339              MOVE ERCTBL-FILE-STATUS TO ABEND-CODE-2              
CIDMOD             DISPLAY '******************************'
CIDMOD             DISPLAY '***** ERROR LOCATION 014 *****'
CIDMOD             DISPLAY '******************************'
04340              MOVE WS-ABEND-CODE      TO WS-RETURN-CODE            
04341              GO TO ABEND-PGM.                                     
04342                                                                   
04343      ADD +1                            TO ERCTBL-READS.           
04344                                                                   
04345  1399-READ-ERCTBL-X.                                              
04346      EXIT.                                                        
04347                                                                   
04348  1400-GET-COMM-PERCENT.                                           
04349                                                                   
04350      IF CTBL-ERROR                                                
04351          MOVE ZEROS                      TO WK-RATE               
04352          GO TO 1489-GET-PERCENT-X.                                
04353                                                                   
04354      MOVE +1 TO IB.                                               
04355      IF (COMM-CK-AMT GREATER CT-TBF (1)) AND                      
04356         (CT-TBF (1) NOT EQUAL +9999999.99)                        
04357          ADD +9 TO IB                                             
04358          IF (COMM-CK-AMT GREATER CT-TBF (2)) AND                  
04359             (CT-TBF (2) NOT EQUAL +9999999.99)                    
04360              ADD +9 TO IB.                                        
04361                                                                   
04362      IF CR-AGE GREATER CT-AGE (1)                                 
04363          ADD +3 TO IB                                             
04364          IF CR-AGE GREATER CT-AGE (2)                             
04365              ADD +3 TO IB.                                        
04366                                                                   
04367      IF WS-COMM-TERM GREATER CT-TRM (1)                           
04368          ADD +1 TO IB                                             
04369          IF WS-COMM-TERM GREATER CT-TRM (2)                       
04370              ADD +1 TO IB.                                        
04371                                                                   
04372      IF CT-RT (IB) NOT NUMERIC                                    
04373          MOVE LAST-CTBL     TO CT-CONTROL-PRIMARY                 
04374          MOVE CT-RT-R (IB)  TO CTBL-TABLE-CD                      
04375          GO TO 1300-READ-ERCTBL-FILE.                             
04376                                                                   
04377      MOVE CT-RT (IB) TO WK-RATE.                                  
04378                                                                   
04379  1489-GET-PERCENT-X.                                              
04380      EXIT.                                                        
04381                                                                   
04382  1499-SET-COMM-X.                                                 
04383      EXIT.                                                        
04384  EJECT                                                            
04385 ******************************************************************
04386 ***  C O M P U T E   A C C T   L E V E L   C O M M I S S I O N ***
04387 ******************************************************************
04388                                                                   
04389  1500-GET-ACCT-LEVEL-LF-COMM.                                     
04390                                                                   
04391      MOVE ZEROS                      TO ACCT-COMM-LF              
04392                                         ACCT-COMM-AH.             
04393                                                                   
04394      MOVE +1                         TO IA.                       
04395                                                                   
04396  1510-ACCT-LEVEL-LOOP.                                            
04397                                                                   
04398      IF CR-AGT-TYPE (IA) = 'C'  OR  'D' OR 'F' OR 'S'             
04399          IF WS-LEVELS-CHARGEBACK-SWITCHES = SPACES                
04400              ADD CR-LCOM-L (IA)  TO ACCT-COMM-LF                  
04401              ADD CR-LCOM-AH (IA) TO ACCT-COMM-AH                  
04402          ELSE                                                     
04403              IF WS-LF-CHARGEBACK-SW (IA)  =  'Y'                  
04404                  ADD CR-LCOM-L (IA)  TO ACCT-COMM-LF.             
04405                                                                   
04406      IF IA LESS THAN +10                                          
04407          ADD +1 TO IA                                             
04408          GO TO 1510-ACCT-LEVEL-LOOP.                              
04409                                                                   
04410  1519-ACCT-LF-COMM-X.                                             
04411      EXIT.                                                        
04412                                                                   
04413                                                                   
04414  1550-GET-ACCT-LEVEL-AH-COMM.                                     
04415                                                                   
04416      MOVE ZEROS                      TO ACCT-COMM-AH.             
04417                                                                   
04418      MOVE +1                         TO IA.                       
04419                                                                   
04420  1555-ACCT-LEVEL-LOOP.                                            
04421                                                                   
04422      IF CR-AGT-TYPE (IA) = 'C'  OR  'D' OR 'F' OR 'S'             
04423          IF WS-AH-CHARGEBACK-SW (IA)  =  'Y'                      
04424              ADD CR-LCOM-AH (IA)  TO ACCT-COMM-AH.                
04425                                                                   
04426      IF IA LESS THAN +10                                          
04427          ADD +1 TO IA                                             
04428          GO TO 1555-ACCT-LEVEL-LOOP.                              
04429                                                                   
04430  1599-ACCT-AH-COMM-X.                                             
04431      EXIT.                                                        
04432  EJECT                                                            
04433 ******************************************************************
04434 ***      C O M M I S S I O N   R E C A L C   R O U T I N E     ***
04435 ******************************************************************
04436                                                                   
04437  1600-RECALC-COMM-EXTR.                                           
04438                                                                   
04439      IF RC-COMM-FLG = 'X'  OR                                     
04440         (DTE-PGM-OPT = '2'  OR  '3')  OR                          
04441         CR-POLICY-IS-DECLINED  OR                                 
04442         CR-POLICY-IS-VOID                                         
04443          GO TO 1899-RECALC-COMM-EXTR-X.                           
04444                                                                   
04445      MOVE 'X'                           TO RC-COMM-FLG.           
04446                                                                   
04447      IF CR-ENTRY-DATE = EOM-RUN-DATE                              
04448          GO TO 1899-RECALC-COMM-EXTR-X.                           
04449                                                                   
04450      PERFORM 1000-SET-COMMISSION-STRUCTURE THRU 1499-SET-COMM-X.  
04451                                                                   
04452      IF CR-COMPENSATION-LEVELS = WK-NEW-RATES                     
04453          GO TO 1899-RECALC-COMM-EXTR-X.                           
04454                                                                   
04455      MOVE WK-NEW-RATES                  TO SAVE-WK-NEW-RATES.     
04456                                                                   
04457      PERFORM 1610-CK-DUPLICATE-ENTRIES                            
04458          THRU 1619-CK-DUPLICATE-ENTRIES-XIT                       
04459              VARYING SUB11 FROM 1 BY 1 UNTIL SUB11 GREATER 10.    
04460                                                                   
04461      IF CR-COMPENSATION-LEVELS = WK-NEW-RATES                     
04462          MOVE SAVE-WK-NEW-RATES         TO CR-COMPENSATION-LEVELS 
04463                                            WK-NEW-RATES           
04464          GO TO 1899-RECALC-COMM-EXTR-X.                           
04465                                                                   
04466      MOVE +1                            TO SUB8.                  
04467      GO TO 1700-REVERSE-OLD-COMM-LOOP.                            
04468                                                                   
04469  1610-CK-DUPLICATE-ENTRIES.                                       
04470      PERFORM 1620-CK-DUPL-ENTR-NEW                                
04471          THRU 1629-CK-DUPL-ENTR-NEW-XIT                           
04472              VARYING SUB12 FROM 1 BY 1 UNTIL SUB12 GREATER 10.    
04473                                                                   
04474  1619-CK-DUPLICATE-ENTRIES-XIT.                                   
04475      EXIT.                                                        
04476                                                                   
04477  1620-CK-DUPL-ENTR-NEW.                                           
04478      IF CR-AGT-LEVELS (SUB11) = WK-N-R (SUB12)                    
04479          MOVE SPACES                    TO CR-COM-AGT (SUB11)     
04480          MOVE SPACE                     TO CR-AGT-TYPE (SUB11)    
04481          MOVE +0                        TO CR-LCOM-L (SUB11)      
04482                                            CR-LCOM-AH (SUB11)     
04483          MOVE CR-AGT-LEVELS (SUB11)     TO WK-N-R (SUB12)         
04484          MOVE 11                        TO SUB12.                 
04485                                                                   
04486  1629-CK-DUPL-ENTR-NEW-XIT.                                       
04487      EXIT.                                                        
04488                                                                   
04489  1700-REVERSE-OLD-COMM-LOOP.                                      
04490      COMPUTE CR-LCOM-L (SUB8) = CR-LCOM-L (SUB8) * -1.            
04491      COMPUTE CR-LCOM-AH (SUB8) = CR-LCOM-AH (SUB8) * -1.          
04492                                                                   
04493      IF SUB8 LESS THAN +10                                        
04494          ADD +1 TO SUB8                                           
04495          GO TO 1700-REVERSE-OLD-COMM-LOOP.                        
04496                                                                   
04497      MOVE 'O' TO RECALC-TYPE.                                     
04498                                                                   
04499      PERFORM 1710-DO-R-C-EXTRACTS THRU 1719-DO-R-C-EXTRACTS-X.    
04500                                                                   
04501      GO TO 1800-MOVE-NEW-COMM-RATES.                              
04502                                                                   
04503  1710-DO-R-C-EXTRACTS.                                            
04504                                                                   
04505      PERFORM 2000-INITIALIZE-DETAIL-EXTRACT THRU 2099-INIT-DTL-X. 
04506                                                                   
04507      PERFORM 1720-CHECK-COMM-LEVELS                               
04508          THRU 1729-CHECK-COMM-LEVELS-EXIT                         
04509              VARYING SUB13 FROM +1 BY +1                          
04510                  UNTIL SUB13 GREATER THAN 10.                     
04511                                                                   
04512      MOVE '8'                           TO DE-TRANS.              
04513      MOVE RECALC-TYPE                   TO DE-RECALC-TYPE.        
04514 *****PERFORM EXTRACT-DATE-RELOAD.                                 
04515      MOVE DETAIL-EXTRACT                TO SAVE-EXTRACT.          
04516      PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X.   
04517      MOVE SAVE-EXTRACT                  TO DETAIL-EXTRACT.        
04518      PERFORM EXTRACT-DATE-LOAD.                                   
04519      MOVE 'R'                           TO DE-REIN.               
04520      MOVE DETAIL-EXTRACT                TO SAVE-EXTRACT.          
04521      MOVE +1                            TO SUB1.                  
04522      PERFORM 3110-REINSURE-ISSUE-LOOP                             
04523          THRU 3119-REINSURE-ISSUE-LOOP-END.                       
04524                                                                   
04525      IF CR-LF-CANCEL-APPLIED  OR  CR-AH-CANCEL-APPLIED            
04526          PERFORM 2200-BUILD-CANCEL-EXTRACT THRU                   
04527                                        2259-END-CANCEL-BUILD      
04528          PERFORM 1720-CHECK-COMM-LEVELS                           
04529              THRU 1729-CHECK-COMM-LEVELS-EXIT                     
04530                  VARYING SUB13 FROM +1 BY +1                      
04531                      UNTIL SUB13 GREATER THAN 10                  
04532          MOVE '7'                       TO DE-TRANS               
04533          MOVE RECALC-TYPE               TO DE-RECALC-TYPE         
04534 *********PERFORM EXTRACT-DATE-RELOAD                              
04535          MOVE DETAIL-EXTRACT            TO SAVE-EXTRACT           
04536          PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X
04537          MOVE SAVE-EXTRACT              TO DETAIL-EXTRACT         
04538          PERFORM EXTRACT-DATE-LOAD                                
04539          MOVE 'R'                       TO DE-REIN                
04540          MOVE DETAIL-EXTRACT            TO SAVE-EXTRACT           
04541          MOVE +1                        TO SUB1                   
04542          PERFORM 3210-REINSURE-CANCEL-LOOP                        
04543              THRU 3219-REINSURE-CANCEL-LOOP-END.                  
04544                                                                   
04545  1719-DO-R-C-EXTRACTS-X.                                          
04546      EXIT.                                                        
04547                                                                   
04548  1720-CHECK-COMM-LEVELS.                                          
04549                                                                   
04550      IF  DE-AGT (SUB13)       =  SPACES  AND                      
04551          DE-AGT-TYPE (SUB13)  =  SPACES  AND                      
04552          DE-L-PC (SUB13)      =  ZEROS   AND                      
04553          DE-A-PC (SUB13)      =  ZEROS                            
04554          MOVE '0000000000'        TO  DE-AGT (SUB13).             
04555                                                                   
04556  1729-CHECK-COMM-LEVELS-EXIT.                                     
04557      EXIT.                                                        
04558                                                                   
04559  1800-MOVE-NEW-COMM-RATES.                                        
04560      MOVE WK-NEW-RATES                  TO CR-COMPENSATION-LEVELS.
04561      MOVE 'N'                           TO RECALC-TYPE.           
04562      PERFORM 1710-DO-R-C-EXTRACTS THRU 1719-DO-R-C-EXTRACTS-X.    
04563      MOVE SAVE-WK-NEW-RATES             TO WK-NEW-RATES           
04564                                            CR-COMPENSATION-LEVELS.
111402     IF DTE-CLIENT = 'DCC'
111402        IF CR-LF-NSP-PRM NOT = ZEROS
111402           COMPUTE CR-LF-NSP-PRM ROUNDED =
111402              (CR-LFPRM + CR-LFPRM-ALT) -
111402              ((CR-LFPRM + CR-LFPRM-ALT) * CR-LCOM-L (1))
111402        END-IF
111402        IF (CR-AH-NSP-PRM NOT = ZEROS)
111402           AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA)
092705                                        NOT = 'G' AND 'L')
111402           COMPUTE CR-AH-NSP-PRM ROUNDED =
111402           CR-AHPRM -
111402           (CR-AHPRM * CR-LCOM-AH (1))
111402        END-IF
111402     END-IF
111402     .
04565                                                                   
04566  1899-RECALC-COMM-EXTR-X.                                         
04567      EXIT.                                                        
04568  EJECT                                                            
04569 ******************************************************************
04570 ***      I N I T I A L I Z E   D E T A I L   E X T R A C T     ***
04571 ******************************************************************
04572                                                                   
04573  2000-INITIALIZE-DETAIL-EXTRACT.                                  
04574                                                                   
04575      MOVE SPACES                   TO DETAIL-EXTRACT.             
04576                                                                   
04577      MOVE 'DE'                     TO DE-RECORD-ID.               
04578      MOVE CR-COMPANY-CD            TO DE-COMPANY-CD.              
04579      MOVE CR-FULL-CONTROL          TO DE-CONTROL.                 
04580                                                                   
04581      MOVE AM-REPORT-CODE-1         TO DE-REPORT-CODE-1.           
04582      MOVE AM-REPORT-CODE-2         TO DE-REPORT-CODE-2.           
04583                                                                   
04584      MOVE CR-NAME                  TO DE-NAME.                    
04585      MOVE CR-AGE                   TO DE-AGE.                     
04586      MOVE CR-SEX                   TO DE-SEX.                     
04587      MOVE CR-SOC-SEC               TO DE-SOC-SEC-NO.              
04588                                                                   
04589      MOVE CR-LFTYP                 TO DE-LF-TYPE.                 
04590      MOVE CR-LF-TERM               TO DE-LF-TERM.                 
04591      MOVE CR-LFAMT                 TO DE-LF-BEN.                  
04592      MOVE ZEROS                    TO DE-LF-CNBEN.                
04593      MOVE CR-LFPRM                 TO DE-LF-PRM.                  
04594      MOVE CR-LFAMT-ALT             TO DE-LF-BEN-ALT.              
04595      MOVE ZEROS                    TO DE-LF-CNBEN-ALT.            
04596      MOVE CR-LFPRM-ALT             TO DE-LF-PRM-ALT.              
04597      MOVE ZEROS                    TO DE-LF-RFND.                 
04598                                                                   
04599      MOVE CR-LF-CURRENT-STATUS     TO DE-LF-STAT-CDE.             
04600      MOVE ZEROS                    TO DE-LF-CANC-DTE              
04601                                       DE-LF-CANC-EXIT-DT.         
04602                                                                   
04603      MOVE CR-AHTYP                 TO DE-AH-TYPE.                 
04604      MOVE CR-AH-TERM               TO DE-AH-TERM.                 
04605      MOVE CR-AHAMT                 TO DE-AH-BEN.                  
04606      MOVE CR-AHPRM                 TO DE-AH-PRM.                  
04607      MOVE ZEROS                    TO DE-AH-RFND.                 
04608                                                                   
04609      MOVE CR-AH-CURRENT-STATUS     TO DE-AH-STAT-CDE.             
04610      MOVE ZEROS                    TO DE-AH-CANC-DTE              
04611                                       DE-AH-CANC-EXIT-DT.         
011904     MOVE CR-MOB-NET-TOT-FEES      TO DE-MOB-NET-TOT-FEES.
04612                                                                   
040504     IF DTE-CLIENT = 'DCC'
040504        MOVE CR-ADDL-CLP         TO DE-ADDL-CLP
020305        MOVE CR-CLP-STATE        TO DE-CLP-STATE
040504     ELSE
04613         IF (CR-LFTYP NOT = ZERO   AND                                
04614            CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z')  OR                
04615            (CR-AHTYP NOT = ZERO  AND                                 
04616            CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'Z')                    
04617            MOVE CR-LIVES           TO DE-LIVES                    
04618            MOVE ZERO               TO DE-CANCEL-CNT-ITD           
04619                                       DE-CANCEL-CNT-YTD
040504        END-IF
040504     END-IF
04620                                                                   
04621      MOVE CR-MEMBER-NO             TO DE-MEMBER-NO.               
Y2KMOD     IF AM-ZIP-PRIME NUMERIC                                      
Y2KMOD        MOVE AM-ZIP-PRIME          TO DE-ZIP                      
Y2KMOD      ELSE                                                        
Y2KMOD        MOVE ZEROS                 TO DE-ZIP.                     
04622      MOVE CR-LOAN-OFFICER          TO DE-LN-OFFICER.              
04623                                                                   
04624      MOVE CR-APR                   TO DE-APR.                     
04625      MOVE CR-PMT-FREQ              TO DE-PMT-FREQ.                
04626      MOVE CR-GRPTYP                TO DE-ACC-GPCD.                
04627      MOVE CR-IND-GRP               TO DE-IG.                      
04628      MOVE AM-REMIT-TO              TO DE-REMIT-TO.                
04629      MOVE CR-MEMBER-NO             TO DE-MEMBER-NO.               
04630                                                                   
04631      MOVE CR-ENTRY-STATUS          TO DE-ENTRY-STATUS.            
04632      MOVE CR-ENTRY-DATE            TO DE-ENTRY-DTE.               
04634      MOVE CR-UNDERWRITING-CODE     TO DE-UNDERWRITING-CODE.       

042607     IF (CR-ENTRY-BATCH (1:2) = 'VA')
              OR (CR-LF-EXIT-BATCH (1:2) = 'VA')
              OR (CR-AH-EXIT-BATCH (1:2) = 'VA')
              MOVE 'VA '                 TO DE-NCL-POOL-CODE
           END-IF

04635 *    MOVE AM-POOL-PRIME            TO DE-NCL-POOL-CODE.           
04636                                                                   
04637      MOVE ZEROS                    TO DE-REI-LFAMT  DE-REI-LFPRM  
04638                                       DE-REI-AHAMT  DE-REI-AHPRM  
04639                                       DE-REI-LFRFND DE-REI-AHRFND 
04640                                       DE-REI-CNAMT.               
04641                                                                   
04642      MOVE +1                       TO SUB1.                       
04643                                                                   
04644      PERFORM EXTRACT-DATE-LOAD.                                   
04645                                                                   
04646      MOVE ZEROS                    TO WS-DE-PROC-DT-N             
04647                                       WS-DE-INCUR-N               
04648                                       WS-DE-PAY-N                 
04649                                       WS-DE-PAID-TO-N             
04650                                       WS-DE-ACC-EXP-DTE-N         
04651                                       WS-DE-ACC-EFF-DTE-N         
04652                                       WS-DE-RSV-INCUR-N           
04653                                       WS-DE-RSV-PAYTO-N           
04654                                       WS-DE-ACC-EXP-DTE-RSV-N     
04655                                       WS-DE-ACC-EFF-DTE-RSV-N.    
04656                                                                   
04657  2090-GL-CODE-LOOP.                                               
04658                                                                   
04659      MOVE AM-GL-CODES (SUB1)       TO DE-GL-CODES (SUB1).         
04660                                                                   
04661      IF SUB1 LESS THAN +10                                        
04662          ADD +1 TO SUB1                                           
04663          GO TO 2090-GL-CODE-LOOP.                                 
04664                                                                   
04665      MOVE AM-RECALC-COMM           TO DE-RECALC-CODE.             
04666                                                                   
04667      MOVE CR-COMPENSATION-LEVELS   TO DE-COMM-LEVELS.             

111204     IF (DTE-CLIENT = 'DCC')
092705        AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
060105        AND (RECALC-TYPE = SPACES)
              MOVE +1                  TO IA
              IF CR-CERT-NO = '0008773536 '
                 DISPLAY ' MADE IT TO 2000 '
                 DISPLAY ' CERT AREA  REIN ' DE-REIN
                 PERFORM VARYING IB FROM +1 BY +1 UNTIL
                    (IB > +10)
                    IF CR-COM-AGT (IB) NOT = SPACES AND ZEROS
                       MOVE IB TO DIL-BEN-CODE
                       MOVE CR-LCOM-AH (IB) TO DIL-AMOUNT
                       MOVE CR-COM-AGT (IB) TO DIL-COMMENT
                       DISPLAY DISPLAY-IBNR-LINE
                    END-IF
                 END-PERFORM
                 DISPLAY ' TRLR AREA  '
                 DISPLAY ' CS CERT NO ' CS-CERT-NO
                 PERFORM VARYING IB FROM +1 BY +1 UNTIL
                    (IB > +10)
                    IF WS-BNK-AGT (IB) NOT = SPACES AND ZEROS
                       MOVE IB TO DIL-BEN-CODE
                       MOVE WS-BNK-SPP-FEES (IB) TO DIL-AMOUNT
                       MOVE WS-BNK-AGT (IB) TO DIL-COMMENT
                       DISPLAY DISPLAY-IBNR-LINE
                    END-IF
                 END-PERFORM
              END-IF
              PERFORM VARYING IB FROM +1 BY +1 UNTIL
                 IB > +10
                 IF DE-AGT (IB) NOT = SPACES AND ZEROS
                    CONTINUE
                 ELSE
                    IF WS-BNK-AGT (IA) NOT = SPACES AND ZEROS
                       MOVE WS-BNK-AGT (IA)  TO DE-AGT (IB)
                       MOVE WS-BNK-TYPE (IA) TO DE-AGT-TYPE (IB)
                       COMPUTE DE-A-PC (IB) =
                              WS-BNK-SPP-FEES (IA) / +1000
                       MOVE +0              TO DE-L-PC (IB)
                       ADD +1               TO IA
                    END-IF
      *             IF CS-AGT (IA) NOT = SPACES AND ZEROS
      *                MOVE CS-AGT (IA)     TO DE-AGT (IB)
      *                MOVE CS-COM-TYP (IA) TO DE-AGT-TYPE (IB)
      *                COMPUTE DE-A-PC (IB) = CS-SPP-FEES (IA) / +1000
      *                MOVE +0              TO DE-L-PC (IB)
      *                ADD +1               TO IA
      *             END-IF
                 END-IF
              END-PERFORM
111204     END-IF



04668      .                                                            
04669  2099-INIT-DTL-X.                                                 
04670      EXIT.                                                        
04671                                                                   
04672  EJECT                                                            
04673 ******************************************************************
04674 ***            B U I L D   I S S U E   E X T R A C T           ***
04675 ******************************************************************
04676                                                                   
04677  2100-BUILD-ISSUE-EXTRACT.                                        
04678                                                                   
04679      PERFORM 2000-INITIALIZE-DETAIL-EXTRACT THRU 2099-INIT-DTL-X. 
04680                                                                   
04681      MOVE 'I'                      TO DE-TRANS.                   
04682                                                                   
04683      IF CHECK-BILLED                                              
04684          MOVE 'N' TO BILLED-SWITCH                                
04685      ELSE                                                         
04686          GO TO 2190-WRITE-ISSUE-EXTRACT.                          
04687                                                                   
04688      MOVE +1                        TO SUB1.                      
04689  2150-GA-BILL-LOOP.                                               
04690                                                                   
04691      IF PB-GA-BILL-DT (SUB1) NOT = LOW-VALUES  AND  SPACES        
04692          MOVE 'B'                   TO DE-GA-BILL-STATUS (SUB1).  
04693                                                                   
04694      IF SUB1 LESS THAN +5                                         
04695          ADD +1 TO SUB1                                           
04696          GO TO 2150-GA-BILL-LOOP.                                 
04697                                                                   
04698      MOVE PB-RECORD-BILL            TO DE-BILL-SW.                
04699      MOVE ZEROS                     TO DE-BILLED-LFPRM            
04700                                        DE-BILLED-LFRFND           
04701                                        DE-BILLED-AHPRM            
04702                                        DE-BILLED-AHRFND.          
04703                                                                   
04704      IF PB-BILLED-DT = LOW-VALUES  OR  SPACES                     
04705          IF PB-LF-BILLED-AMTS = ZERO  AND                         
04706             PB-AH-BILLED-AMTS = ZERO                              
04707              MOVE ' '                    TO DE-BILL-STATUS        
04708          ELSE                                                     
04709              IF PB-LF-BILLED-AMTS = PB-I-LF-PREMIUM-AMT  AND      
04710                 PB-AH-BILLED-AMTS = PB-I-AH-PREMIUM-AMT           
04711                  MOVE 'B'                TO DE-BILL-STATUS        
04712              ELSE                                                 
04713                  MOVE 'R'                TO DE-BILL-STATUS        
04714                  MOVE PB-LF-BILLED-AMTS  TO DE-BILLED-LFPRM       
04715                  MOVE PB-AH-BILLED-AMTS  TO DE-BILLED-AHPRM       
04716      ELSE                                                         
04717          MOVE 'B'                        TO DE-BILL-STATUS.       
04718                                                                   
04719  2190-WRITE-ISSUE-EXTRACT.                                        
04720                                                                   
04721      PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X.   
04722                                                                   
04723  2199-BLD-ISSUE-X.                                                
04724      EXIT.                                                        
04725  EJECT                                                            
04726 ******************************************************************
04727 ***           B U I L D   C A N C E L   E X T R A C T          ***
04728 ******************************************************************
04729                                                                   
04730  2200-BUILD-CANCEL-EXTRACT.                                       
04731                                                                   
04732      PERFORM 2000-INITIALIZE-DETAIL-EXTRACT THRU 2099-INIT-DTL-X. 
04733                                                                   
04734      MOVE 'C'                           TO DE-TRANS.              
04735                                                                   
04736      IF CR-LFTYP = ZERO                                           
04737          GO TO 2250-CONTINUE-CANCEL-BUILD.                        
04738                                                                   
04739      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'Z'                  
04740          IF (CR-LF-STATUS-AT-CANCEL = ' ' OR '7')  AND            
04741              CR-LFRFND = +0                                       
04742               MOVE +0                   TO TEMP-5                 
04743               GO TO 2250-CONTINUE-CANCEL-BUILD.                   
04744                                                                   
04745      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                      
04746           IF CR-LFRFND = +0                                       
04747               GO TO 2250-CONTINUE-CANCEL-BUILD.                   
04748                                                                   
04749      MOVE +1 TO TEMP-5.                                           
04750      MOVE CR-LF-TERM             TO ORIG-TERM.                    
04751      MOVE CR-LF-CANC-DT          TO VALUATION-DT.                 
04752      MOVE LIFE-OVERRIDE-L1       TO WE-LF-AH.                     
04753      MOVE '1'                    TO W-VAL-SOURCE.                 
04754      PERFORM 8500-REMAINING-TERM-ROUTINE THRU 8599-REM-TERM-X.    
04755                                                                   
04756      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'                   
04757          MOVE CR-LFAMT TO DE-LF-CNBEN                             
04758          GO TO 2250-CONTINUE-CANCEL-BUILD.                        
04759                                                                   
04760      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                      
04761          GO TO 2220-BLD-DE-CANCEL-SUM.                            
04762                                                                   
04763      IF REM-TRM3 = CR-LF-TERM                                     
04764          MOVE CR-LFAMT TO DE-LF-CNBEN                             
04765          GO TO 2250-CONTINUE-CANCEL-BUILD.                        
04766                                                                   
04767      IF CLAS-I-EP (CLAS-INDEXL) = 'T'                             
04768          GO TO 2210-BLD-DE-CANCEL-TEX.                            
04769                                                                   
04770      IF DTE-CLIENT = 'NCL'                                        
04771          IF CR-DT LESS THAN 19910101                              
04772              GO TO 2205-BLD-DE-CANCEL-BEN.                        
04773                                                                   
04774      IF CLAS-I-EP (CLAS-INDEXL) = 'N'                             
04775          GO TO 2230-BLD-DE-CANCEL-NP.                             
04776                                                                   
04777      IF DTE-CLIENT = 'FUL'                                        
04778          GO TO 2240-BLD-DE-CANCEL-FUL.                            
04779                                                                   
CIDMOD*    IF STATE-ABBR (CLAS-INDEXS) = 'OH'                           
CIDMOD     IF (STATE-ABBR (CLAS-INDEXS) = 'OH') AND                     
CIDMOD        (CR-RATING-CLASS NOT = 'L ')
04781          IF (CR-APR GREATER THAN ZERO)  AND                       
04782             (CR-LF-TERM GREATER THAN 060)  AND                    
04783             (CR-DT GREATER THAN 19831031)                         
04784               GO TO 2230-BLD-DE-CANCEL-NP.                        
04785                                                                   
04786      IF STATE-ABBR (CLAS-INDEXS) = 'MT'                           
04787          IF (CR-APR GREATER THAN ZERO)  AND                       
04788             (CR-LF-TERM GREATER THAN 061)  AND                    
04789             (CR-DT GREATER THAN 19830318)                         
04790               GO TO 2230-BLD-DE-CANCEL-NP.                        
04791                                                                   
04792      IF STATE-ABBR (CLAS-INDEXS) = 'UT'                           
04793          IF (CR-APR GREATER THAN ZERO)  AND                       
04794             (CR-LF-TERM GREATER THAN 062)  AND                    
04795             (CR-DT GREATER THAN 19810831)  AND                    
04796             (CR-DT LESS THAN 19830901)                            
04797               GO TO 2230-BLD-DE-CANCEL-NP.                        
04798                                                                   
04799      IF STATE-ABBR (CLAS-INDEXS) = 'RI'                           
04800          IF (CR-APR GREATER THAN ZERO)  AND                       
04801             (CR-LF-TERM GREATER THAN 060)  AND                    
04802             (CR-DT GREATER THAN 19831231)                         
04803               GO TO 2230-BLD-DE-CANCEL-NP.                        
04804                                                                   
04805  2205-BLD-DE-CANCEL-BEN.                                          
04806                                                                   
04807      IF CR-LF-TERM EQUAL +0                                       
04808          MOVE +0          TO TEMP-5                               
04809      ELSE                                                         
04810          COMPUTE TEMP-5 = REM-TRM3 / CR-LF-TERM.                  
04811                                                                   
04812      COMPUTE DE-LF-CNBEN = CR-LFAMT * TEMP-5.                     
04813                                                                   
04814      GO TO 2250-CONTINUE-CANCEL-BUILD.                            
04815                                                                   
04816  2210-BLD-DE-CANCEL-TEX.                                          
04817                                                                   
04818      DIVIDE CR-LFAMT BY CR-LF-TERM                                
04819          GIVING TEX-FACT-1.                                       
04820      DIVIDE REM-TRM3 BY CR-PMT-FREQ                               
04821          GIVING TEX-FACT-2                                        
04822              REMAINDER TEX-FACT-3.                                
04823      IF TEX-FACT-3 NOT = ZERO                                     
04824          ADD +1 TO TEX-FACT-2.                                    
04825      COMPUTE DE-LF-CNBEN =                                        
04826          (TEX-FACT-1 * (TEX-FACT-2 * CR-PMT-FREQ)).               
04827                                                                   
04828      IF CR-LFAMT EQUAL +0                                         
04829          MOVE +0                  TO TEMP-5                       
04830      ELSE                                                         
04831          COMPUTE TEMP-5 = DE-LF-CNBEN / CR-LFAMT.                 
04832                                                                   
04833      GO TO 2250-CONTINUE-CANCEL-BUILD.                            
04834                                                                   
04835  2220-BLD-DE-CANCEL-SUM.                                          
04836                                                                   
04837      MOVE CR-SUM-CAN-CNT-YTD      TO DE-CANCEL-CNT-YTD.           
04838      MOVE CR-SUM-CAN-CNT-ITD      TO DE-CANCEL-CNT-ITD.           
04839                                                                   
04840      IF CR-LFRFND NOT LESS CR-LFPRM                               
04841          MOVE CR-LFAMT           TO DE-LF-CNBEN                   
04842           GO TO 2250-CONTINUE-CANCEL-BUILD.                       
04843                                                                   
04844      COMPUTE TEMP-6 ROUNDED = CR-LFRFND / CR-LFPRM.               
04845      COMPUTE TEMP-7 ROUNDED = TEMP-6 * CR-LFAMT.                  
04846      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'                   
04847          MOVE TEMP-7             TO DE-LF-CNBEN                   
04848        ELSE                                                       
04849          COMPUTE TEMP-5 = REM-TRM3 / CR-LF-TERM                   
04850          COMPUTE DE-LF-CNBEN = TEMP-7 * TEMP-5.                   
04851                                                                   
04852      IF CR-LFAMT EQUAL +0                                         
04853          MOVE +0                  TO TEMP-5                       
04854      ELSE                                                         
04855          COMPUTE TEMP-5 ROUNDED =  DE-LF-CNBEN / CR-LFAMT.        
04856                                                                   
04857      IF CLAS-I-EP (CLAS-INDEXL) NOT = 'N'                         
04858          GO TO 2250-CONTINUE-CANCEL-BUILD.                        
04859                                                                   
04860  2230-BLD-DE-CANCEL-NP.                                           
04861                                                                   
04862      MOVE CR-DT                      TO  DC-GREG-DATE-CYMD.       
04863      MOVE 'L'                        TO  DC-OPTION-CODE.          
04864      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
04865      MOVE DC-BIN-DATE-1              TO  CP-CERT-EFF-DT.          
04866                                                                   
04867      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                            
04868        MOVE CR-LOAN-1ST-PMT-DT       TO  DC-GREG-DATE-1-YMD       
04869        MOVE '3'                      TO  DC-OPTION-CODE           
04870        PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X 
04871        MOVE DC-BIN-DATE-1            TO  CP-FIRST-PAY-DATE        
04872      ELSE                                                         
04873        MOVE CP-CERT-EFF-DT           TO  DC-BIN-DATE-1            
04874        MOVE '6'                      TO  DC-OPTION-CODE           
04875        MOVE +1                       TO  DC-ELAPSED-MONTHS        
04876        MOVE +0                       TO  DC-ELAPSED-DAYS          
04877        PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X 
04878        MOVE DC-BIN-DATE-2            TO  CP-FIRST-PAY-DATE.       
04879                                                                   
CIDMOD     MOVE CR-RATING-CLASS            TO  CP-CLASS-CODE
04880      MOVE CR-LFAMT                   TO  CP-ORIGINAL-BENEFIT.     
04881      MOVE CR-APR                     TO  CP-LOAN-APR.             
04882      MOVE CR-LF-TERM                 TO  CP-ORIGINAL-TERM.        
04883      MOVE CR-LOAN-TERM               TO  CP-LOAN-TERM.            
04884      MOVE REM-TRM3                   TO  CP-REMAINING-TERM.       
04885      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL) TO  CP-SPECIAL-CALC-CD.  
04886      MOVE CLAS-I-RL-AH     (CLAS-INDEXL) TO  CP-BENEFIT-TYPE.     
04887      MOVE CLAS-I-EP        (CLAS-INDEXL) TO  CP-EARNING-METHOD.   
04888                                                                   
04889      CALL 'ELRAMTX' USING CALCULATION-PASS-AREA.                  
04890                                                                   
04891      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                      
04892          COMPUTE DE-LF-CNBEN =                                    
04893                  CP-REMAMT-FACTOR * (TEMP-7 / +1000)              
04894       ELSE                                                        
04895           COMPUTE DE-LF-CNBEN =                                   
04896                   CP-REMAMT-FACTOR * (CR-LFAMT / +1000).          
04897                                                                   
04898      IF CR-LFAMT EQUAL +0                                         
04899          MOVE +0                  TO TEMP-5                       
04900      ELSE                                                         
04901          COMPUTE TEMP-5 = DE-LF-CNBEN / CR-LFAMT.                 
04902                                                                   
04903      GO TO 2250-CONTINUE-CANCEL-BUILD.                            
04904                                                                   
04905  2240-BLD-DE-CANCEL-FUL.                                          
04906                                                                   
04907      MOVE CR-APR                  TO NP-APR.                      
04908      MOVE CR-LF-TERM              TO NP-ORIG.                     
04909      MOVE REM-TRM3                TO NP-REM.                      
04910      MOVE '2'                     TO NP-OPT.                      
04911      MOVE CR-AHPRM                TO NP-AHPRM.                    
04912      MOVE CR-ACCOUNT              TO NP-ACCOUNT.                  
04913      MOVE CR-LFAMT                TO NP-BENEFIT.                  
04914                                                                   
04915 *    CALL 'FULNETRM'                                              
04916 *        USING NP-APR NP-ORIG NP-REM NP-OPT NP-BENEFIT NP-AHPRM   
04917 *              NP-ACCOUNT NP-REMAINING.                           
04918                                                                   
04919      MOVE NP-REMAINING            TO DE-LF-CNBEN.                 
04920                                                                   
04921      IF CR-LFAMT EQUAL +0                                         
04922          MOVE +0                  TO TEMP-5                       
04923      ELSE                                                         
04924          COMPUTE TEMP-5 = DE-LF-CNBEN / CR-LFAMT.                 
04925                                                                   
04926  2250-CONTINUE-CANCEL-BUILD.                                      
04927                                                                   
04928      MOVE CR-LFRFND                   TO DE-LF-RFND.              
04929      MOVE CR-LF-STATUS-AT-CANCEL      TO DE-LF-PREV-STAT.         
04930      MOVE CR-LF-CANC-DT               TO DE-LF-CANC-DTE           
04931                                          WS-DE-LF-CANC-DTE-N.     
04932      MOVE CR-LF-CANCEL-EXIT-DATE      TO DE-LF-CANC-EXIT-DT       
04933                                          WS-DE-LF-CANC-EXIT-DT-N. 
04934      MOVE CR-LF-COMM-CHARGEBACK       TO DE-LF-COMM-CHARGEBACK.   
04935                                                                   
04936      MOVE CR-AHRFND                   TO DE-AH-RFND.              

040504     IF DTE-CLIENT = 'DCC'
040504        IF DE-AH-RFND = ZEROS
040504           MOVE ZEROS                 TO DE-ADDL-CLP
040504        END-IF
040504     END-IF

04937      MOVE CR-AH-STATUS-AT-CANCEL      TO DE-AH-PREV-STAT.         
04938      MOVE CR-AH-CANC-DT               TO DE-AH-CANC-DTE           
04939                                          WS-DE-AH-CANC-DTE-N.     
04940      MOVE CR-AH-CANCEL-EXIT-DATE      TO DE-AH-CANC-EXIT-DT       
04941                                          WS-DE-AH-CANC-EXIT-DT-N. 
04942      MOVE CR-AH-COMM-CHARGEBACK       TO DE-AH-COMM-CHARGEBACK.   
04943                                                                   
04944      IF PB-LAST-MAINT-BY = 'E570'                                 
04945          MOVE 'R'                     TO DE-AUTO-GEND-IND.        
04946                                                                   
04947      IF CHECK-BILLED                                              
04948          MOVE 'N' TO BILLED-SWITCH                                
04949      ELSE                                                         
04950          GO TO 2259-END-CANCEL-BUILD.                             
04951                                                                   
04952      MOVE +1                        TO SUB1.                      
04953  2255-GA-BILL-LOOP.                                               
04954                                                                   
04955      IF PB-GA-BILL-DT (SUB1) NOT = LOW-VALUES  AND  SPACES        
04956          MOVE 'B'                   TO DE-GA-BILL-STATUS (SUB1).  
04957                                                                   
04958      IF SUB1 LESS THAN +5                                         
04959          ADD +1 TO SUB1                                           
04960          GO TO 2255-GA-BILL-LOOP.                                 
04961                                                                   
04962      MOVE PB-RECORD-BILL            TO DE-BILL-SW.                
04963      MOVE PB-C-REFUND-SW            TO DE-REFUND-SW.              
04964      MOVE ZEROS                     TO DE-BILLED-LFPRM            
04965                                        DE-BILLED-LFRFND           
04966                                        DE-BILLED-AHPRM            
04967                                        DE-BILLED-AHRFND.          
04968                                                                   
04969      IF PB-BILLED-DT = LOW-VALUES  OR  SPACES                     
04970          IF PB-LF-BILLED-AMTS = ZERO  AND                         
04971             PB-AH-BILLED-AMTS = ZERO                              
04972              MOVE ' '                    TO DE-BILL-STATUS        
04973          ELSE                                                     
04974              IF PB-LF-BILLED-AMTS = PB-C-LF-CANCEL-AMT  AND       
04975                 PB-AH-BILLED-AMTS = PB-C-AH-CANCEL-AMT            
04976                  MOVE 'B'                TO DE-BILL-STATUS        
04977              ELSE                                                 
04978                  MOVE 'R'                TO DE-BILL-STATUS        
04979                  MOVE PB-LF-BILLED-AMTS  TO DE-BILLED-LFRFND      
04980                  MOVE PB-AH-BILLED-AMTS  TO DE-BILLED-AHRFND      
04981      ELSE                                                         
04982          MOVE 'B'                        TO DE-BILL-STATUS.       
04983                                                                   
04984  2259-END-CANCEL-BUILD.                                           
04985      EXIT.                                                        
04986                                                                   
04987  2290-WRITE-CANCEL-EXTRACT.                                       
04988                                                                   
04989      PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X.   
04990                                                                   
04991  2299-BLD-CANCEL-X.                                               
04992      EXIT.                                                        
04993  EJECT                                                            
04994 ******************************************************************
04995 ***            B U I L D   C L A I M   E X T R A C T           ***
04996 ******************************************************************
04997                                                                   
04998  2300-BUILD-CLAIM-EXTRACT.                                        
04999                                                                   
05000      PERFORM 2000-INITIALIZE-DETAIL-EXTRACT THRU 2099-INIT-DTL-X. 
05001                                                                   
05002      MOVE 'X'                       TO DE-TRANS.                  
05003                                                                   
05004      MOVE PC-CLAIM-TYPE             TO DE-TYPE.                   
05005                                                                   
05006      MOVE PC-CLAIM-PAYMENT          TO DE-CLAIM-AMT.              
05007      MOVE ZEROS                     TO DE-REI-CLAIM-AMT.          
05008                                                                   
05009      MOVE INCURRED-DATE             TO DE-INCUR                   
05010                                        WS-DE-INCUR-N.             
05011      MOVE REPORTED-DATE             TO DE-REPORTED.               
05012      MOVE PAYMENT-DATE              TO DE-PAY                     
05013                                        WS-DE-PAY-N.               
05014      MOVE PAY-TO-DATE               TO DE-PAID-TO                 
05015                                        WS-DE-PAID-TO-N.           
05016                                                                   
05017      MOVE PC-CLAIM-NO               TO DE-CNUM.                   
05018      MOVE PC-CHECK-NO               TO DE-CHECK.                  
05019      MOVE PC-TRLR-SEQ-NO            TO DE-PMT-TRAILER-SEQ.        
05020      MOVE PC-NO-OF-DAYS-PAID        TO DE-DAYS-DISAB.             
05021      MOVE PC-AGE-AT-CLAIM           TO DE-CLM-AGE.                
05022      MOVE PC-PAYMENT-TYPE           TO DE-PAY-CODE.               
05023      INSPECT DE-PAY-CODE                                          
05024          CONVERTING '1234569' TO 'PFSXEEV'.                       
05025                                                                   
05026      MOVE AM-NAME                   TO DE-ACC-NAME.               
05027      MOVE AM-EXPIRE-DT              TO DE-ACC-EXP-DTE             
05028                                        WS-DE-ACC-EXP-DTE-N.       
05029      MOVE AM-EFFECT-DT              TO DE-ACC-EFF-DTE             
05030                                        WS-DE-ACC-EFF-DTE-N.       
05031      MOVE PC-CAUSE-CODE             TO DE-CLM-CAUSE.              
05032      MOVE CR-LOAN-OFFICER           TO DE-LOAN-OFFICER.           
05033                                                                   
05034      IF DE-DTH  OR  DE-OB-DTH                                     
05035          MOVE CR-LF-STATUS-AT-DEATH TO DE-LF-PREV-STAT            
05036      ELSE                                                         
05037          IF CR-AH-LUMP-SUM-DISAB                                  
05038              MOVE CR-AH-STATUS-AT-SETTLEMENT                      
05039                                     TO DE-AH-PREV-STAT.           
05040                                                                   
05041      IF PC-LAST-MAINT-BY EQUAL 'E570'                             
05042          MOVE 'R'                     TO DE-AUTO-GEND-IND.        
05043                                                                   
05044  2390-WRITE-CLAIM-EXTRACT.                                        
05045                                                                   
05046      PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X.   
05047                                                                   
05048  2399-BLD-CLAIM-X.                                                
05049      EXIT.                                                        
05050  EJECT                                                            
05051 ******************************************************************
05052 ***          B U I L D   R E S E R V E   E X T R A C T         ***
05053 ******************************************************************
05054                                                                   
05055  2400-BUILD-RESERVE-EXTRACT.                                      
05056                                                                   
05057      PERFORM 2000-INITIALIZE-DETAIL-EXTRACT THRU 2099-INIT-DTL-X. 
05058                                                                   
05059      MOVE 'Y'                         TO DE-TRANS.                
05060      MOVE PC-CLAIM-TYPE               TO DE-RESERVE-TYPE.         
05061                                                                   
05062      MOVE PC-IBNR-RESERVE-AMT         TO DE-IBNR.                 
05063      MOVE PC-PTC-RESERVE-AMT          TO DE-PAYCUR.               
05064      MOVE PC-FUTURE-RESERVE-AMT       TO DE-FUTRSV.               
05065      ADD  PC-MANUAL-RESERVE-AMT       TO DE-FUTRSV.               
05066                                                                   
05067      IF DTE-CLIENT  =  'NCL'                                      
05068         IF PC-MANUAL-RESERVE-AMT  GREATER THAN   ZEROS            
05069             MOVE 'Y'                  TO DE-MANUAL-RSV-SWITCH.    
05070                                                                   
05071      MOVE PC-CLAIM-NO                 TO DE-CLMNO.                
05072                                                                   
05073      MOVE AM-EXPIRE-DT                TO DE-ACC-EXP-DTE-RSV       
05074                                          WS-DE-ACC-EXP-DTE-RSV-N. 
05075                                                                   
05076      MOVE AM-EFFECT-DT                TO DE-ACC-EFF-DTE-RSV       
05077                                          WS-DE-ACC-EFF-DTE-RSV-N. 
05078                                                                   
05079      MOVE INCURRED-DATE               TO DE-RSV-INCUR             
05080                                          WS-DE-RSV-INCUR-N.       
05081      MOVE REPORTED-DATE               TO DE-RSV-REPORTED.         
05082      MOVE PAY-TO-DATE                 TO DE-RSV-PAYTO             
05083                                          WS-DE-RSV-PAYTO-N.       
05084                                                                   
05085      MOVE ZEROS                       TO DE-REI-IBNR              
05086                                          DE-REI-PAYCUR            
05087                                          DE-REI-FUTRSV.           
05088                                                                   
05089      MOVE AM-NAME                     TO DE-RSV-ACC-NAME.         
05090                                                                   
05091      IF PC-LAST-MAINT-BY EQUAL 'E570'                             
05092          MOVE 'R'                     TO DE-AUTO-GEND-IND.        
05093                                                                   
05094  2490-WRITE-RESERVE-EXTRACT.                                      
05095                                                                   
05096      PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X.   
05097                                                                   
05098  2499-BLD-RESERVE-X.                                              
05099      EXIT.                                                        
05100  EJECT                                                            
05101 ******************************************************************
05102 ***          W R I T E   D E T A I L   E X T R A C T S         ***
05103 ******************************************************************
05104                                                                   
05105  2500-WRITE-DETAIL-EXTRACTS.                                      
05106                                                                   
05107                                                                   
05108      MOVE  ZEROS            TO  SUB.                              
05109                                                                   
05110      IF  NOT  DE-CANCEL  AND   NOT  DE-RC-CANCEL                  
05111          GO TO 2520-WRITE-CONTINUE.                               
05112                                                                   
05113  2510-ZERO-CHARGEBACK-LOOP.                                       
05114                                                                   
05115      ADD  +1                TO  SUB.                              
05116                                                                   
05117      IF  SUB    GREATER  THAN     +10                             
05118          GO TO  2520-WRITE-CONTINUE.                              
05119                                                                   
      *    DISPLAY ' CERT NO ' CR-CERT-NO
      *    DISPLAY ' REFUND TYPE ' CR-AH-REFUND-TYPE
      *    DISPLAY '  SUB  ' SUB '   ' WS-AH-CHARGEBACK-SW (SUB)
           

05120      IF  WS-LF-CHARGEBACK-SW (SUB)  = 'N'                         
05121          MOVE  ZEROS            TO  DE-L-PC (SUB).                
05122                                                                   
05123      IF  (WS-AH-CHARGEBACK-SW (SUB)  = 'N')
                         OR
040504         ((CR-AH-REFUND-TYPE = 'G')
               AND (CR-AHRFND = ZEROS))
040504         MOVE  ZEROS             TO DE-A-PC (SUB)
040504     END-IF
05125                                                                   
05126      GO TO 2510-ZERO-CHARGEBACK-LOOP.                             
05127                                                                   
05128  2520-WRITE-CONTINUE.                                             
05129                                                                   
05130      IF DTE-CLIENT NOT = 'HER'                                    
05131          GO TO 2590-WRITE-EXTRACTS.                               
05132                                                                   
05133      IF DE-REINCO NOT = 'LNL'                                     
05134          GO TO 2590-WRITE-EXTRACTS.                               
05135                                                                   
05136      IF DE-CLAIM  OR  DE-RESERVE  OR  DE-RR-RC-CLM                
05137          GO TO 2590-WRITE-EXTRACTS.                               
05138                                                                   
05139      IF DE-AGT-TYPE (1)  = 'D'   MOVE 'C'    TO DE-AGT-TYPE (1).  
05140      IF DE-AGT-TYPE (2)  = 'P'   MOVE 'O'    TO DE-AGT-TYPE (2).  
05141      IF DE-AGT-TYPE (3)  = 'P'   MOVE 'O'    TO DE-AGT-TYPE (3).  
05142      IF DE-AGT-TYPE (4)  = 'P'   MOVE 'O'    TO DE-AGT-TYPE (4).  
05143      IF DE-AGT-TYPE (5)  = 'P'   MOVE 'O'    TO DE-AGT-TYPE (5).  
05144      IF DE-AGT-TYPE (6)  = 'P'   MOVE 'O'    TO DE-AGT-TYPE (6).  
05145      IF DE-AGT-TYPE (7)  = 'P'   MOVE 'O'    TO DE-AGT-TYPE (7).  
05146      IF DE-AGT-TYPE (8)  = 'P'   MOVE 'O'    TO DE-AGT-TYPE (8).  
05147      IF DE-AGT-TYPE (9)  = 'P'   MOVE 'O'    TO DE-AGT-TYPE (9).  
05148      IF DE-AGT-TYPE (10) = 'P'   MOVE 'O'    TO DE-AGT-TYPE (10). 
05149                                                                   
05150      IF DE-AGT-TYPE (1)  = 'S'   MOVE 'F'    TO DE-AGT-TYPE (1).  
05151      IF DE-AGT-TYPE (2)  = 'B'   MOVE 'G'    TO DE-AGT-TYPE (2).  
05152      IF DE-AGT-TYPE (3)  = 'B'   MOVE 'G'    TO DE-AGT-TYPE (3).  
05153      IF DE-AGT-TYPE (4)  = 'B'   MOVE 'G'    TO DE-AGT-TYPE (4).  
05154      IF DE-AGT-TYPE (5)  = 'B'   MOVE 'G'    TO DE-AGT-TYPE (5).  
05155      IF DE-AGT-TYPE (6)  = 'B'   MOVE 'G'    TO DE-AGT-TYPE (6).  
05156      IF DE-AGT-TYPE (7)  = 'B'   MOVE 'G'    TO DE-AGT-TYPE (7).  
05157      IF DE-AGT-TYPE (8)  = 'B'   MOVE 'G'    TO DE-AGT-TYPE (8).  
05158      IF DE-AGT-TYPE (9)  = 'B'   MOVE 'G'    TO DE-AGT-TYPE (9).  
05159      IF DE-AGT-TYPE (10) = 'B'   MOVE 'G'    TO DE-AGT-TYPE (10). 
05160                                                                   
05161                                                                   
05162  2590-WRITE-EXTRACTS.                                             
05163                                                                   
05164      MOVE EOM-RUN-DATE            TO DE-PROC-DT                   
05165                                      WS-DE-PROC-DT-N.             
05166                                                                   
05167      IF DTE-PGM-OPT = '1'                                         
05168          ADD +1                   TO DE-CNT                       
05169 *********PERFORM EXTRACT-DATE-RELOAD                              
05170          WRITE DETAIL-EXTRACT.                                    
05171                                                                   
05172                                                                   
05173  2599-WRT-DETAIL-X.                                               
05174      EXIT.                                                        
05175  EJECT                                                            
05176 ******************************************************************
05177 ***         R E I N S U R A N C E   P R O C E S S I N G        ***
05178 ******************************************************************
05179                                                                   
05180  3000-REINSURE-ROUTINE.                                           
05181                                                                   
05182      IF (CR-POLICY-IS-DECLINED) OR                                
05183         (CR-POLICY-IS-VOID)                                       
05184         GO TO 3099-REINSURE-ROUTINE-X.                            
05185                                                                   
05186      IF REIN-FLG = 'X'                                            
05187          GO TO 3099-REINSURE-ROUTINE-X.                           
05188                                                                   
05189      MOVE 'X' TO REIN-FLG.                                        
05190                                                                   
05191      PERFORM CLEAR-REIN-HOLD THRU CLEAR-REIN-HOLD-X.              
05192                                                                   
05193      IF CR-REIN-TABLE = SPACES OR ZEROS                           
05194          GO TO 3030-RE-CALCULATE-REINSURANCE.                     
05195                                                                   
05196 ******************************************************************
05197 ***    R E A D   R E I N S U R A N C E   T A B L E   F I L E   ***
05198 ******************************************************************
05199                                                                   
05200  3010-RR-READ-REIN.                                               
05201                                                                   
05202      IF REIN-OPEN-SW = ' '                                        
05203          MOVE 'X'                       TO REIN-OPEN-SW           
05204          MOVE DTE-CLASIC-COMPANY-CD     TO REIN-SRCH-COMP-CD      
05205          MOVE 'A'                       TO REIN-SRCH-CODE         
070102         OPEN INPUT ERRTBL-IN
05207          IF ERRTBL-FILE-STATUS = '00'  OR  '97'                   
05208              PERFORM REIN-CO-TABLE-BUILD THRU REIN-BUILD-EXIT     
05209           ELSE                                                    
05210              MOVE '21'                  TO ABEND-CODE-1           
05211              MOVE ERRTBL-FILE-STATUS    TO ABEND-CODE-2           
CIDMOD             DISPLAY '******************************'
CIDMOD             DISPLAY '***** ERROR LOCATION 015 *****'
CIDMOD             DISPLAY '******************************'
05212              MOVE WS-ABEND-CODE        TO WS-RETURN-CODE          
05213              GO TO ABEND-PGM.                                     
05214                                                                   
05215      IF CR-REIN-TABLE = REIN-SRCH                                 
05216          GO TO 3019-RR-READ-REIN-X.                               
05217                                                                   
05218      MOVE +0                            TO REIN-SUB-1.            
05219      MOVE 999999                        TO REIN-LOW-TIME.         
05220      MOVE CR-REIN-TABLE                 TO REIN-SRCH.             
05221                                                                   
05222  3011-RR-CHECK-REIN-SAVE-TABLE.                                   
05223                                                                   
05224      ADD +1                             TO REIN-SUB-1.            
05225      IF REIN-SUB-1 GREATER THAN REIN-MAX  OR                      
05226         REIN-SUB-1 GREATER THAN REIN-SUB-2                        
05227          GO TO 3015-RR-READ-ERRTBL.                               
05228                                                                   
05229      IF REIN-SEARCH = SR-CONTROL (REIN-SUB-1)                     
05230          MOVE SR-ENTRY (REIN-SUB-1)     TO REINSURANCE-RECORD     
05231          ACCEPT WS-TIME-OF-DAY   FROM TIME                        
05232          MOVE WS-TIME                   TO SR-TIME (REIN-SUB-1)   
05233          GO TO 3019-RR-READ-REIN-X.                               
05234                                                                   
05235      IF SR-TIME (REIN-SUB-1) LESS THAN REIN-LOW-TIME              
05236          MOVE SR-TIME (REIN-SUB-1)      TO REIN-LOW-TIME          
05237          MOVE REIN-SUB-1                TO REIN-LOW-SUB.          
05238                                                                   
05239      GO TO 3011-RR-CHECK-REIN-SAVE-TABLE.                         
05240                                                                   
05241  3015-RR-READ-ERRTBL.                                             
05242                                                                   
05243      MOVE REIN-SEARCH                   TO RE-CONTROL-PRIMARY.    
05244                                                                   
070102     READ ERRTBL-IN.
05246                                                                   
05247      IF ERRTBL-FILE-STATUS  = '23'                                
05248          DISPLAY 'INVALID REINSURANCE TABLE CODE - ' REIN-SRCH    
05249                                            ' ' CR-FULL-CONTROL    
05250          MOVE '24'                      TO ABEND-CODE-1           
05251          MOVE ERRTBL-FILE-STATUS        TO ABEND-CODE-2           
CIDMOD         DISPLAY '******************************'
CIDMOD         DISPLAY '***** ERROR LOCATION 016 *****'
CIDMOD         DISPLAY '******************************'
05252          MOVE WS-ABEND-CODE        TO WS-RETURN-CODE              
05253          GO TO ABEND-PGM.                                         
05254                                                                   
05255      IF ERRTBL-FILE-STATUS  = '00'  OR  '97'                      
05256          NEXT SENTENCE                                            
05257        ELSE                                                       
05258          MOVE '24'                      TO ABEND-CODE-1           
05259          MOVE ERRTBL-FILE-STATUS        TO ABEND-CODE-2           
CIDMOD         DISPLAY '******************************'
CIDMOD         DISPLAY '***** ERROR LOCATION 017 *****'
CIDMOD         DISPLAY '******************************'
05260          MOVE WS-ABEND-CODE        TO WS-RETURN-CODE              
05261          GO TO ABEND-PGM.                                         
05262                                                                   
05263      ADD +1                            TO ERRTBL-READS.           
05264                                                                   
05265      IF REIN-SUB-1 GREATER THAN REIN-MAX                          
05266          MOVE REINSURANCE-RECORD       TO SR-ENTRY (REIN-LOW-SUB) 
05267          ACCEPT WS-TIME-OF-DAY   FROM TIME                        
05268          MOVE WS-TIME                  TO SR-TIME (REIN-LOW-SUB)  
05269      ELSE                                                         
05270          MOVE REINSURANCE-RECORD       TO SR-ENTRY (REIN-SUB-1)   
05271          ACCEPT WS-TIME-OF-DAY   FROM TIME                        
05272          MOVE WS-TIME                  TO SR-TIME (REIN-SUB-1)    
05273          MOVE REIN-SUB-1               TO REIN-SUB-2.             
05274                                                                   
05275  3019-RR-READ-REIN-X.                                             
05276      EXIT.                                                        
05277                                                                   
05278  3020-CALCULATE-REINSURANCE.                                      
05279                                                                   
05280 *    IF DTE-CLIENT = '???'  AND                                   
05281 *       (CR-REIN-TABLE = 'SE1')                                   
05282 *        PERFORM 3640-NSP-REIN-RATING-ROUTINE THRU 3669-NSP-REIN-X
05283 *        IF CR-STATE NOT = STATE-L                                
05284 *            MOVE CR-STATE             TO STATE-L                 
05285 *            PERFORM 8200-STATE-CODE-LOOKUP                       
05286 *                         THRU 8299-STATE-LOOKUP-X.               
05287                                                                   

111402*    IF DTE-CLIENT = 'DCC'
111402*       IF CR-LF-NSP-PRM NOT = ZEROS
111402*          COMPUTE CR-LF-NSP-PRM ROUNDED =
111402*             (CR-LFPRM + CR-LFPRM-ALT) -
111402*             ((CR-LFPRM + CR-LFPRM-ALT) * CR-LCOM-L (1))
111402*       END-IF
111402*       IF CR-AH-NSP-PRM NOT = ZEROS
111402*          COMPUTE CR-AH-NSP-PRM ROUNDED =
111402*          CR-AHPRM -
111402*          (CR-AHPRM * CR-LCOM-AH (1))
111402*       END-IF
111402*    END-IF


05288      IF RE-REMAINING (1) = 'I'                                    
05289          MOVE '*'                      TO RISK-REIN-FLAG          
05290      ELSE                                                         
05291          GO TO 3029-CONTINUE-REIN-CALC.                           
05292                                                                   
05293      MOVE SPACE                        TO REIN-CANCEL-LF-SW       
05294                                           REIN-CANCEL-AH-SW.      
05295      MOVE ZEROS                        TO REIN-EARN-LF-TERM       
05296                                           REIN-LCNC-EARN-TERM     
05297                                           REIN-LF-CLM-MONTHS      
05298                                           REIN-EARN-AH-TERM       
05299                                           REIN-ACNC-EARN-TERM     
05300                                           REIN-AH-PRIOR-CLMS-PAID 
05301                                           REIN-AH-CLM-MONTHS      
05302                                           REIN-AH-CLM-MTH-1       
05303                                           REIN-AH-CLM-MTH-2       
05304                                           REIN-AH-CLM-MTH-3       
05305                                           REIN-AH-CLM-MTH-4       
05306                                           REIN-AH-CLM-MTH-5.      
05307                                                                   
PEMTMP*    MOVE CR-DT                        TO DC-GREG-DATE-CYMD.      
PEMTMP*    MOVE 'L'                          TO DC-OPTION-CODE.         
PEMTMP*    PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
PEMTMP*    MOVE DC-BIN-DATE-1                TO WS-CR-BIN-DATE.         
05312                                                                   
05313      IF CR-LFTYP = ZEROS  OR  SPACES                              
05314          GO TO 3023-CONTINUE.                                     
05315                                                                   
05316      MOVE EOM-RUN-DATE                 TO VALUATION-DT.           
05317                                                                   
05318      IF CR-LF-CANCEL-EXIT-DATE NOT = ZEROS                        
05319        IF CR-LF-CANCEL-EXIT-DATE LESS THAN VALUATION-DT           
05320            MOVE CR-LF-CANCEL-EXIT-DATE TO VALUATION-DT.           
05321                                                                   
05322      IF CR-LF-CLAIM-EXIT-DATE NOT = ZEROS                         
05323        IF CR-LF-CLAIM-EXIT-DATE LESS THAN VALUATION-DT            
05324            MOVE CR-LF-CLAIM-EXIT-DATE  TO VALUATION-DT.           
05325                                                                   
05326      MOVE VALUATION-DT                 TO DC-GREG-DATE-CYMD.      
05327      PERFORM 3025-CALC-ELAPSED-MONTHS THRU 3026-CALC-EXIT.        
05328                                                                   
05329      IF DC-ELAPSED-MONTHS GREATER THAN CR-LF-TERM                 
05330          MOVE CR-LF-TERM               TO MONTHS-DIFF-LF          
05331      ELSE                                                         
05332          MOVE DC-ELAPSED-MONTHS        TO MONTHS-DIFF-LF.         
05333                                                                   
05334      IF CR-LF-CANC-DT NOT = ZEROS                                 
05335          MOVE CR-LF-CANC-DT            TO DC-GREG-DATE-CYMD       
05336          PERFORM 3025-CALC-ELAPSED-MONTHS THRU 3026-CALC-EXIT     
05337          MOVE DC-ELAPSED-MONTHS        TO REIN-LCNC-EARN-TERM     
05338          IF CR-DT = CR-LF-CANC-DT                                 
05339              MOVE '*'                  TO REIN-CANCEL-LF-SW.      
05340                                                                   
05341      IF CR-DTH-DT NOT = ZEROS                                     
05342          MOVE CR-DTH-DT                TO DC-GREG-DATE-CYMD       
05343          PERFORM 3025-CALC-ELAPSED-MONTHS THRU 3026-CALC-EXIT     
05344          MOVE DC-ELAPSED-MONTHS        TO REIN-LF-CLM-MONTHS.     
05345                                                                   
05346      MOVE MONTHS-DIFF-LF               TO REIN-EARN-LF-TERM.      
05347                                                                   
05348                                                                   
05349  3023-CONTINUE.                                                   
05350                                                                   
05351      IF CR-AHTYP = ZEROS  OR  SPACES                              
05352         GO TO 3029-CONTINUE-REIN-CALC.                            
05353                                                                   
05354      MOVE EOM-RUN-DATE                 TO VALUATION-DT.           
05355                                                                   
05356      IF CR-AH-CANCEL-EXIT-DATE NOT = ZEROS                        
05357        IF CR-AH-CANCEL-EXIT-DATE LESS THAN VALUATION-DT           
05358            MOVE CR-AH-CANCEL-EXIT-DATE TO VALUATION-DT.           
05359                                                                   
05360      IF CR-AH-SETTLEMENT-EXIT-DATE NOT = ZEROS                    
05361        IF CR-AH-SETTLEMENT-EXIT-DATE LESS THAN VALUATION-DT       
05362            MOVE CR-AH-SETTLEMENT-EXIT-DATE TO VALUATION-DT.       
05363                                                                   
05364      MOVE VALUATION-DT                 TO DC-GREG-DATE-CYMD.      
05365      PERFORM 3025-CALC-ELAPSED-MONTHS THRU 3026-CALC-EXIT.        
05366                                                                   
05367      IF DC-ELAPSED-MONTHS GREATER THAN CR-AH-TERM                 
05368          MOVE CR-AH-TERM               TO MONTHS-DIFF-AH          
05369      ELSE                                                         
05370          MOVE DC-ELAPSED-MONTHS        TO MONTHS-DIFF-AH.         
05371                                                                   
05372      IF CR-AH-CANC-DT NOT = ZEROS                                 
05373          MOVE CR-AH-CANC-DT           TO DC-GREG-DATE-CYMD        
05374          PERFORM 3025-CALC-ELAPSED-MONTHS THRU 3026-CALC-EXIT     
05375          MOVE DC-ELAPSED-MONTHS        TO REIN-ACNC-EARN-TERM     
05376          IF CR-DT = CR-AH-CANC-DT                                 
05377              MOVE '*'                  TO REIN-CANCEL-AH-SW.      
05378                                                                   
05379      IF CR-DIS-INCUR-DT (1) NOT = ZEROS                           
05380          MOVE CR-DIS-INCUR-DT (1)    TO DC-GREG-DATE-CYMD         
05381          PERFORM 3025-CALC-ELAPSED-MONTHS THRU 3026-CALC-EXIT     
05382          MOVE DC-ELAPSED-MONTHS        TO REIN-AH-CLM-MTH-1.      
05383                                                                   
05384      IF CR-DIS-INCUR-DT (2) NOT = ZEROS                           
05385          MOVE CR-DIS-INCUR-DT (2)    TO DC-GREG-DATE-CYMD         
05386          PERFORM 3025-CALC-ELAPSED-MONTHS THRU 3026-CALC-EXIT     
05387          MOVE DC-ELAPSED-MONTHS        TO REIN-AH-CLM-MTH-2.      
05388                                                                   
05389      IF CR-DIS-INCUR-DT (3) NOT = ZEROS                           
05390          MOVE CR-DIS-INCUR-DT (3)      TO DC-GREG-DATE-CYMD       
05391          PERFORM 3025-CALC-ELAPSED-MONTHS THRU 3026-CALC-EXIT     
05392          MOVE DC-ELAPSED-MONTHS        TO REIN-AH-CLM-MTH-3.      
05393                                                                   
05394      IF CR-DIS-INCUR-DT (4) NOT = ZEROS                           
05395          MOVE CR-DIS-INCUR-DT (4)    TO DC-GREG-DATE-CYMD         
05396          PERFORM 3025-CALC-ELAPSED-MONTHS THRU 3026-CALC-EXIT     
05397          MOVE DC-ELAPSED-MONTHS        TO REIN-AH-CLM-MTH-4.      
05398                                                                   
05399      IF CR-DIS-INCUR-DT (5) NOT = ZEROS                           
05400          MOVE CR-DIS-INCUR-DT (5)    TO DC-GREG-DATE-CYMD         
05401          PERFORM 3025-CALC-ELAPSED-MONTHS THRU 3026-CALC-EXIT     
05402          MOVE DC-ELAPSED-MONTHS        TO REIN-AH-CLM-MTH-5.      
05403                                                                   
05404      MOVE MONTHS-DIFF-AH               TO REIN-EARN-AH-TERM.      
05405                                                                   
05406                                                                   
05407      GO TO 3029-CONTINUE-REIN-CALC.                               
05408                                                                   
05409                                                                   
05410  3025-CALC-ELAPSED-MONTHS.                                        
05411                                                                   
05412      MOVE 'L'                          TO DC-OPTION-CODE.         
05413      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
05414      MOVE DC-BIN-DATE-1                TO DC-BIN-DATE-2.          
05415      MOVE WS-CR-BIN-DATE               TO DC-BIN-DATE-1.          
05416      MOVE '1'                          TO DC-OPTION-CODE.         
05417      MOVE ' '                          TO DC-CENTURY-ADJUSTMENT.  
05418      MOVE ZEROS                        TO DC-ELAPSED-MONTHS       
05419                                           DC-ODD-DAYS-OVER        
05420                                           DC-ELAPSED-DAYS.        
05421      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
05422                                                                   
05423  3026-CALC-EXIT.                                                  
05424      EXIT.                                                        
05425                                                                   
05426  EJECT                                                            
05427  3029-CONTINUE-REIN-CALC.                                         
05428                                                                   
05429                              COPY ECSRTPFM.                       
05430                                                                   
05431      MOVE 'X'                  TO REIN-RT-SW.                     
05432      PERFORM 3200-REINSURE-CANCEL THRU 3299-REINSURE-CANCEL-X.    
05433      PERFORM 3300-REINSURE-CLAIM THRU 3399-REINSURE-CLAIM-X.      
05434      MOVE ' '                  TO REIN-RT-SW.                     
05435                                                                   
05436  3030-RE-CALCULATE-REINSURANCE.                                   
05437                                                                   
05438      IF AM-RECALC-REIN = 'Y'                                      
05439          IF CR-REIN-TABLE NOT = AM-REI-TABLE  AND                 
05440             CR-REIN-SPEC = SPACE  AND                             
05441             CR-ENTRY-DATE NOT = EOM-RUN-DATE                      
05442              PERFORM 3600-RC-REINSURANCE                          
05443                  THRU 3799-RC-REINSURANCE-X.                      
05444                                                                   
05445      PERFORM 3040-REIN-REC-CNT THRU 3049-RRC-XIT                  
05446          VARYING SUB1 FROM 1 BY 1                                 
05447              UNTIL REIN-COMP (SUB1) = SPACES.                     
05448                                                                   
05449      GO TO 3099-REINSURE-ROUTINE-X.                               
05450                                                                   
05451  3040-REIN-REC-CNT.                                               
05452                                                                   
05453      IF REIN-LF-AH-FLGS (SUB1) = SPACES                           
05454          GO TO 3049-RRC-XIT.                                      
05455                                                                   
05456      PERFORM 3050-REIN-COMP-ADD THRU 3059-RCA-XIT                 
05457          VARYING B-SUB FROM 1 BY 1                                
05458              UNTIL B-SUB GREATER THAN 50.                         
05459                                                                   
05460  3049-RRC-XIT.                                                    
05461      EXIT.                                                        
05462                                                                   
05463  3050-REIN-COMP-ADD.                                              
05464                                                                   
05465      IF RB-COMPANY (B-SUB) = SPACES                               
05466          MOVE REIN-COMP (SUB1) TO RB-COMPANY (B-SUB).             
05467      IF RB-COMPANY (B-SUB) = REIN-COMP (SUB1)                     
05468          MOVE 1   TO RB-COUNT (B-SUB)                             
05469          MOVE +50 TO B-SUB.                                       
05470                                                                   
05471  3059-RCA-XIT.                                                    
05472      EXIT.                                                        
05473                                                                   
05474  3099-REINSURE-ROUTINE-X.                                         
05475      EXIT.                                                        
05476                                                                   
05477  EJECT                                                            
05478 ******************************************************************
05479 ***                R E I N S U R E   I S S U E S               ***
05480 ******************************************************************
05481                                                                   
05482  3100-REINSURE-ISSUE.                                             
05483                                                                   
05484      IF  REIN-COMP (1) = SPACES                                   
05485          GO TO 3199-REINSURE-ISSUE-X.                             
05486                                                                   
05487      PERFORM 2000-INITIALIZE-DETAIL-EXTRACT THRU 2099-INIT-DTL-X. 
05488      MOVE ZEROS    TO RW-ACCUM-LF  RW-ACCUM-AH.                   
05489      MOVE 'I'      TO DE-TRANS.                                   
05490      MOVE 'R'      TO DE-REIN.                                    
05491 *****PERFORM EXTRACT-DATE-RELOAD.                                 
05492      MOVE DETAIL-EXTRACT TO SAVE-EXTRACT.                         
05493      MOVE +1       TO SUB1.                                       
05494                                                                   
05495  3110-REINSURE-ISSUE-LOOP.                                        
05496                                                                   
05497      IF REIN-COMP (SUB1) = SPACES                                 
05498          GO TO 3119-REINSURE-ISSUE-LOOP-END.                      
05499                                                                   
05500      IF UPDATING-RISK-PREMIUMS                                    
05501        IF REIN-REM-SW (SUB1) NOT = 'I'                            
05502          ADD +1 TO SUB1                                           
05503          GO TO 3110-REINSURE-ISSUE-LOOP.                          
05504                                                                   
05505      IF REIN-REM-SW (SUB1) = 'Z'                                  
05506          ADD +1 TO SUB1                                           
05507          GO TO 3110-REINSURE-ISSUE-LOOP.                          
05508                                                                   
05509      MOVE REIN-COMP (SUB1)      TO DE-REI-COMP.                   
05510      MOVE REIN-WORK-FLDS (SUB1) TO RWF-FIELDS.                    
05511      MOVE RWF-LFAMT             TO DE-REI-LFAMT.                  
05512      MOVE RWF-LFPRM             TO DE-REI-LFPRM.                  
05513      MOVE RWF-AHAMT             TO DE-REI-AHAMT.                  
05514      MOVE RWF-AHPRM             TO DE-REI-AHPRM.                  
05515                                                                   
05516      IF REIN-REM-SW (SUB1) NOT = 'I'                              
05517          GO TO 3115-CONTINUE-ISSUE-LOOP.                          
05518                                                                   
05519      IF RWF-LFPRM = ZEROS  AND                                    
05520         RWF-AHPRM = ZEROS                                         
05521          GO TO 3115-CONTINUE-ISSUE-LOOP.                          
05522                                                                   
05523      IF RWF-LFPRM NOT = ZEROS                                     
05524          COMPUTE DE-REI-LFPRM = RWF-LFPRM - CR-LF-REI-RISK-PRM    
05525          MOVE RWF-LFPRM         TO CR-LF-REI-RISK-PRM.            
05526                                                                   
05527      IF RWF-AHPRM NOT = ZEROS                                     
05528          COMPUTE DE-REI-AHPRM = RWF-AHPRM - CR-AH-REI-RISK-PRM    
05529          MOVE RWF-AHPRM         TO CR-AH-REI-RISK-PRM.            
05530                                                                   
05531  3115-CONTINUE-ISSUE-LOOP.                                        
05532                                                                   
05533      ADD DE-REI-LFPRM           TO RW-ACCUM-LF.                   
05534      ADD DE-REI-AHPRM           TO RW-ACCUM-AH.                   
05535                                                                   
05536      IF DE-REI-LFAMT NOT = ZERO                                   
05537         OR DE-REI-LFPRM NOT = ZERO                                
05538         OR DE-REI-AHAMT NOT = ZERO                                
05539         OR DE-REI-AHPRM NOT = ZERO                                
05540          PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU                  
05541                  2599-WRT-DETAIL-X                                
05542          ADD +1 TO BC-R-ISS.                                      
05543                                                                   
05544      ADD +1 TO SUB1.                                              
05545      MOVE SAVE-EXTRACT TO DETAIL-EXTRACT.                         
05546      PERFORM EXTRACT-DATE-LOAD.                                   
05547                                                                   
05548      GO TO 3110-REINSURE-ISSUE-LOOP.                              
05549                                                                   
05550  3119-REINSURE-ISSUE-LOOP-END.                                    
05551      EXIT.                                                        
05552                                                                   
05553  3190-REINSURE-ISSUE-PRINT.                                       
05554                                                                   
05555      PERFORM 8600-SETUP-DETAIL-LINE THRU 8689-SETUP-DETAIL-X.     
05556      IF RW-ACCUM-LF NOT = ZERO                                    
05557          MOVE CLAS-I-AB3 (CLAS-INDEXL) TO TRD-LFTYP               
05558          MOVE CR-LF-TERM               TO TRD-LFTRM               
05559          MOVE RW-ACCUM-LF         TO TRD-LFPRM-RFND               
05560          MOVE 'REIN CEDED'        TO TRD-LF-ACTION.               
05561      IF RW-ACCUM-AH NOT = ZERO                                    
05562          MOVE CLAS-I-AB3 (CLAS-INDEXA) TO TRD-AHTYP               
05563          MOVE CR-AH-TERM               TO TRD-AHTRM               
05564          MOVE RW-ACCUM-AH         TO TRD-AHPRM-RFND               
05565          MOVE 'REIN CEDED'        TO TRD-AH-ACTION.               
05566      IF RW-ACCUM-LF NOT = ZERO  OR  RW-ACCUM-AH NOT = ZERO        
05567          PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.   
05568                                                                   
05569  3199-REINSURE-ISSUE-X.                                           
05570      EXIT.                                                        
05571  EJECT                                                            
05572 ******************************************************************
05573 ***               R E I N S U R E   C A N C E L S              ***
05574 ******************************************************************
05575                                                                   
05576  3200-REINSURE-CANCEL.                                            
05577                                                                   
05578      IF REIN-COMP (1) = SPACES                                    
05579          GO TO 3299-REINSURE-CANCEL-X.                            
05580                                                                   
05581      IF CR-LFRFND = ZERO                                          
05582         AND CR-AHRFND = ZERO                                      
05583          GO TO 3299-REINSURE-CANCEL-X.                            
05584                                                                   
05585      PERFORM REINSURE-CALC-CANCEL THRU REINSURE-CALC-CANCEL-X     
05586              VARYING SUB1 FROM +1 BY +1 UNTIL                     
05587                        REIN-COMP (SUB1) = SPACES.                 
05588                                                                   
05589      IF REIN-RT-SW = 'X'                                          
05590          GO TO 3299-REINSURE-CANCEL-X.                            
05591                                                                   
05592      PERFORM 2200-BUILD-CANCEL-EXTRACT THRU 2259-END-CANCEL-BUILD.
05593      MOVE ZEROS    TO RW-ACCUM-LF  RW-ACCUM-AH.                   
05594      MOVE 'R'      TO DE-REIN.                                    
05595 *****PERFORM EXTRACT-DATE-RELOAD.                                 
05596      MOVE DETAIL-EXTRACT TO SAVE-EXTRACT.                         
05597      MOVE +1       TO SUB1.                                       
05598                                                                   
05599  3210-REINSURE-CANCEL-LOOP.                                       
05600                                                                   
05601      IF REIN-COMP (SUB1) = SPACES                                 
05602          GO TO 3219-REINSURE-CANCEL-LOOP-END.                     
05603                                                                   
05604      IF REIN-REM-SW (SUB1) = 'Z'                                  
05605          ADD +1        TO SUB1                                    
05606          MOVE SAVE-EXTRACT TO DETAIL-EXTRACT                      
05607          PERFORM EXTRACT-DATE-LOAD                                
05608          GO TO 3210-REINSURE-CANCEL-LOOP.                         
05609                                                                   
05610      MOVE REIN-COMP (SUB1)      TO DE-REI-COMP.                   
05611      MOVE REIN-WORK-FLDS (SUB1) TO RWF-FIELDS.                    
05612                                                                   
05613      MOVE RWF-LFRFND            TO DE-REI-LFRFND.                 
05614      MOVE RWF-AHRFND            TO DE-REI-AHRFND.                 
05615      MOVE RWF-LFAMT             TO DE-REI-LFAMT.                  
05616      MOVE RWF-LFPRM             TO DE-REI-LFPRM.                  
05617      MOVE RWF-AHAMT             TO DE-REI-AHAMT.                  
05618      MOVE RWF-AHPRM             TO DE-REI-AHPRM.                  
05619      ADD RWF-LFRFND             TO RW-ACCUM-LF.                   
05620      ADD RWF-AHRFND             TO RW-ACCUM-AH.                   
05621      COMPUTE DE-REI-CNAMT = RWF-LFAMT * TEMP-5.                   
05622                                                                   
05623      IF REIN-REM-SW (SUB1) NOT = 'I'                              
05624          GO TO 3215-CONTINUE-CANCEL-LOOP.                         
05625                                                                   
05626      IF CR-LF-CANCEL-EXIT-DATE = EOM-RUN-DATE  OR                 
05627         VOIDING-LF-CANCEL                                         
05628          NEXT SENTENCE                                            
05629      ELSE                                                         
05630          MOVE ZEROS             TO DE-REI-LFRFND.                 
05631                                                                   
05632      IF CR-AH-CANCEL-EXIT-DATE = EOM-RUN-DATE  OR                 
05633         VOIDING-AH-CANCEL                                         
05634          NEXT SENTENCE                                            
05635      ELSE                                                         
05636          MOVE ZEROS             TO DE-REI-AHRFND.                 
05637                                                                   
05638  3215-CONTINUE-CANCEL-LOOP.                                       
05639                                                                   
05640      IF RWF-LF-RUNOFF-SW = 'N'                                    
05641          IF DE-LF-CANC-DTE NOT LESS THAN RWF-EP-STOP-DATE         
05642              MOVE ZEROS         TO DE-LF-TYPE                     
05643                                    DE-REI-LFAMT                   
05644                                    DE-REI-LFPRM                   
05645                                    DE-REI-LFRFND                  
05646                                    DE-REI-CNAMT.                  
05647                                                                   
05648      IF RWF-AH-RUNOFF-SW = 'N'                                    
05649          IF DE-AH-CANC-DTE NOT LESS THAN RWF-EP-STOP-DATE         
05650              MOVE ZEROS         TO DE-AH-TYPE                     
05651                                    DE-REI-AHAMT                   
05652                                    DE-REI-AHPRM                   
05653                                    DE-REI-AHRFND.                 
05654                                                                   
05655      IF DE-REI-LFAMT NOT = ZERO                                   
05656         OR DE-REI-LFPRM NOT = ZERO                                
05657         OR DE-REI-AHAMT NOT = ZERO                                
05658         OR DE-REI-AHPRM NOT = ZERO                                
05659         OR DE-REI-LFRFND NOT = ZERO                               
05660         OR DE-REI-AHRFND NOT = ZERO                               
05661         OR DE-REI-CNAMT NOT = ZERO                                
05662          PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU                  
05663                                             2599-WRT-DETAIL-X     
05664          ADD +1 TO BC-R-CNC.                                      
05665                                                                   
05666      ADD +1 TO SUB1.                                              
05667      MOVE SAVE-EXTRACT TO DETAIL-EXTRACT.                         
05668      PERFORM EXTRACT-DATE-LOAD.                                   
05669      GO TO 3210-REINSURE-CANCEL-LOOP.                             
05670                                                                   
05671  3219-REINSURE-CANCEL-LOOP-END.                                   
05672      EXIT.                                                        
05673                                                                   
05674  3290-REINSURE-CANCEL-PRINT.                                      
05675                                                                   
05676      PERFORM 8600-SETUP-DETAIL-LINE THRU 8689-SETUP-DETAIL-X.     
05677      IF RW-ACCUM-LF NOT = ZERO                                    
05678          MOVE CLAS-I-AB3 (CLAS-INDEXL) TO TRD-LFTYP               
05679          MOVE CR-LF-TERM               TO TRD-LFTRM               
05680          MOVE RW-ACCUM-LF         TO TRD-LFPRM-RFND               
05681          MOVE 'REIN CANCL'        TO TRD-LF-ACTION.               
05682      IF RW-ACCUM-AH NOT = ZERO                                    
05683          MOVE CLAS-I-AB3 (CLAS-INDEXA) TO TRD-AHTYP               
05684          MOVE CR-AH-TERM               TO TRD-AHTRM               
05685          MOVE RW-ACCUM-AH         TO TRD-AHPRM-RFND               
05686          MOVE 'REIN CANCL'        TO TRD-AH-ACTION.               
05687      IF RW-ACCUM-LF NOT = ZERO  OR  RW-ACCUM-AH NOT = ZERO        
05688          PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.   
05689                                                                   
05690  3299-REINSURE-CANCEL-X.                                          
05691      EXIT.                                                        
05692  EJECT                                                            
05693 ******************************************************************
05694 ***                R E I N S U R E   C L A I M S               ***
05695 ******************************************************************
05696                                                                   
05697  3300-REINSURE-CLAIM.                                             
05698                                                                   
05699 *    IF DTE-CLIENT = '???'                                        
05700 *       IF (REIN-RT-SW NOT = 'X')  AND                            
05701 *          (PC-PAYMENT-TYPE = '5' OR '6')                         
05702 *        GO TO 3399-REINSURE-CLAIM-X.                             
05703                                                                   
05704      IF REIN-COMP (1) = SPACES                                    
05705          GO TO 3399-REINSURE-CLAIM-X.                             
05706                                                                   
05707      IF REIN-RT-SW = 'X'                                          
05708          MOVE CR-DTHAMT   TO RW-LFCLMWK                           
05709          ADD  CR-DTHEXP   TO RW-LFCLMWK                           
05710          MOVE CR-DISAMT   TO RW-AHCLMWK                           
05711          ADD  CR-DISEXP   TO RW-AHCLMWK                           
05712      ELSE                                                         
05713          IF PC-CLAIM-TYPE  = 1 OR 3                               
05714              MOVE PC-CLAIM-PAYMENT    TO RW-LFCLMWK               
05715              MOVE +0                  TO RW-AHCLMWK               
05716          ELSE                                                     
05717              MOVE PC-CLAIM-PAYMENT    TO RW-AHCLMWK               
05718              MOVE +0                  TO RW-LFCLMWK.              
05719                                                                   
05720      MOVE +0                          TO RW-LFCLM                 
05721                                          RW-AHCLM.                
05722      IF RW-LFCLMWK = ZERO                                         
05723         AND RW-AHCLMWK = ZERO                                     
05724          GO TO 3399-REINSURE-CLAIM-X.                             
05725                                                                   
05726      PERFORM REINSURE-CALC-CLAIM THRU REINSURE-CALC-CLAIM-X       
05727          VARYING SUB1 FROM +1 BY +1                               
05728              UNTIL REIN-COMP (SUB1) = SPACES.                     
05729                                                                   
05730      IF REIN-RT-SW = 'X'                                          
05731          GO TO 3399-REINSURE-CLAIM-X.                             
05732                                                                   
05733      PERFORM 2300-BUILD-CLAIM-EXTRACT.                            
05734      MOVE ZEROS                TO RW-ACCUM-CLM.                   
05735      MOVE 'R'                  TO DE-REIN.                        
05736 *****PERFORM EXTRACT-DATE-RELOAD.                                 
05737      MOVE DETAIL-EXTRACT       TO SAVE-EXTRACT.                   
05738      MOVE +1                   TO SUB1.                           
05739                                                                   
05740  3310-REINSURE-CLAIM-LOOP.                                        
05741                                                                   
05742      IF REIN-COMP (SUB1) = SPACES                                 
05743          GO TO 3390-REINSURE-CLAIM-PRINT.                         
05744                                                                   
05745      IF REIN-REM-SW (SUB1) = 'Z'                                  
05746          ADD +1        TO SUB1                                    
05747          MOVE SAVE-EXTRACT TO DETAIL-EXTRACT                      
05748          PERFORM EXTRACT-DATE-LOAD                                
05749          GO TO 3310-REINSURE-CLAIM-LOOP.                          
05750                                                                   
05751      MOVE REIN-COMP (SUB1)      TO DE-REI-COMP.                   
05752      MOVE REIN-WORK-FLDS (SUB1) TO RWF-FIELDS.                    
05753                                                                   
05754      IF PC-CLAIM-TYPE  = 1 OR 3                                   
05755          IF RWF-LF-RUNOFF-SW = 'N'                                
05756            IF CR-DTH-DT NOT LESS THAN RWF-EP-STOP-DATE            
05757                GO TO 3330-REINSURE-CLAIM-LOOP-ADD.                
05758                                                                   
05759      IF PC-CLAIM-TYPE  = 2 OR 4                                   
05760          IF RWF-AH-RUNOFF-SW = 'N'                                
05761            IF CR-DIS-DT NOT LESS THAN RWF-EP-STOP-DATE            
05762                GO TO 3330-REINSURE-CLAIM-LOOP-ADD.                
05763                                                                   
05764      MOVE RWF-LFAMT             TO DE-REI-LFAMT.                  
05765      MOVE RWF-LFPRM             TO DE-REI-LFPRM.                  
05766      MOVE RWF-AHAMT             TO DE-REI-AHAMT.                  
05767      MOVE RWF-AHPRM             TO DE-REI-AHPRM.                  
05768                                                                   
05769      IF PC-CLAIM-TYPE  = 1 OR 3                                   
05770          MOVE RWF-LFCLML TO DE-REI-CLAIM-AMT                      
05771          ADD RWF-LFCLML  TO RW-ACCUM-CLM                          
05772      ELSE                                                         
05773          MOVE RWF-AHCLML TO DE-REI-CLAIM-AMT                      
05774          ADD RWF-AHCLML  TO RW-ACCUM-CLM.                         
05775                                                                   
05776      IF PC-CLAIM-TYPE = 1 OR 3                                    
05777          IF +0 = DE-REI-LFPRM AND DE-REI-LFAMT                    
05778             AND DE-REI-CLAIM-AMT                                  
05779              GO TO 3330-REINSURE-CLAIM-LOOP-ADD                   
05780          ELSE                                                     
05781              GO TO 3320-REINSURE-CLAIM-LOOP-GO                    
05782      ELSE                                                         
05783          IF +0 = DE-REI-AHPRM AND DE-REI-AHAMT                    
05784             AND DE-REI-CLAIM-AMT                                  
05785              GO TO 3330-REINSURE-CLAIM-LOOP-ADD                   
05786          ELSE                                                     
05787              GO TO 3320-REINSURE-CLAIM-LOOP-GO.                   
05788                                                                   
05789  3320-REINSURE-CLAIM-LOOP-GO.                                     
05790                                                                   
05791      PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X.   
05792      ADD +1                     TO BC-R-CLM.                      
05793                                                                   
05794  3330-REINSURE-CLAIM-LOOP-ADD.                                    
05795                                                                   
05796      ADD +1                     TO SUB1.                          
05797      MOVE SAVE-EXTRACT          TO DETAIL-EXTRACT.                
05798      PERFORM EXTRACT-DATE-LOAD.                                   
05799      GO TO 3310-REINSURE-CLAIM-LOOP.                              
05800                                                                   
05801  3390-REINSURE-CLAIM-PRINT.                                       
05802                                                                   
05803      PERFORM 8600-SETUP-DETAIL-LINE THRU 8689-SETUP-DETAIL-X.     
05804      IF PC-CLAIM-TYPE = '1'  OR  '3'                              
05805          MOVE CLAS-I-AB3 (CLAS-INDEXL) TO TRD-LFTYP               
05806          MOVE CR-LF-TERM               TO TRD-LFTRM               
05807          MOVE RW-ACCUM-CLM     TO TRD-LFCOM-CLM                   
05808          MOVE 'REIN CLAIM'     TO TRD-LF-ACTION                   
05809      ELSE                                                         
05810          MOVE CLAS-I-AB3 (CLAS-INDEXA) TO TRD-AHTYP               
05811          MOVE CR-AH-TERM               TO TRD-AHTRM               
05812          MOVE RW-ACCUM-CLM     TO TRD-AHCOM-CLM                   
05813          MOVE CR-DIS-MO        TO TRD-AHACTMO                     
05814          MOVE CR-DIS-DA        TO TRD-AHACTDA                     
05815          MOVE CR-DIS-YR        TO TRD-AHACTYR                     
05816          MOVE '/'              TO TRD-AH-S1  TRD-AH-S2            
05817          MOVE 'REIN CLAIM'     TO TRD-AH-ACTION.                  
05818      PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.       
05819                                                                   
05820  3399-REINSURE-CLAIM-X.                                           
05821      EXIT.                                                        
05822  EJECT                                                            
05823 ******************************************************************
05824 ***              R E I N S U R E   R E S E R V E S             ***
05825 ******************************************************************
05826                                                                   
05827  3400-REINSURE-RESERVE.                                           
05828                                                                   
05829      IF REIN-COMP (1) = SPACES                                    
05830          GO TO 3499-REINSURE-RESERVE-X.                           
05831                                                                   
05832      IF PC-CLAIM-TYPE = '1' OR '3'                                
05833          MOVE PC-IBNR-RESERVE-AMT    TO RW-LFIBNRWK               
05834          MOVE PC-PTC-RESERVE-AMT     TO RW-LFPAYCURWK             
05835          MOVE PC-FUTURE-RESERVE-AMT  TO RW-LFFUTRSVWK             
05836          ADD  PC-MANUAL-RESERVE-AMT  TO RW-LFFUTRSVWK             
05837          MOVE +0 TO RW-AHIBNRWK  RW-AHPAYCURWK  RW-AHFUTRSVWK     
05838      ELSE                                                         
05839          MOVE PC-IBNR-RESERVE-AMT    TO RW-AHIBNRWK               
05840          MOVE PC-PTC-RESERVE-AMT     TO RW-AHPAYCURWK             
05841          MOVE PC-FUTURE-RESERVE-AMT  TO RW-AHFUTRSVWK             
05842          ADD  PC-MANUAL-RESERVE-AMT  TO RW-AHFUTRSVWK             
05843          MOVE +0 TO RW-LFIBNRWK  RW-LFPAYCURWK  RW-LFFUTRSVWK.    
05844                                                                   
05845      MOVE +0 TO RW-LFIBNR  RW-LFPAYCUR  RW-LFFUTRSV               
05846                 RW-AHIBNR  RW-AHPAYCUR  RW-AHFUTRSV.              
05847      IF +0 = RW-LFIBNRWK AND RW-LFPAYCURWK AND RW-LFFUTRSVWK      
05848          AND RW-AHIBNRWK AND RW-AHPAYCURWK AND RW-AHFUTRSVWK      
05849           GO TO 3499-REINSURE-RESERVE-X.                          
05850                                                                   
05851      IF DTE-CLIENT NOT = 'MIC'                                    
05852          GO TO 3405-REINSURE-RESERVE-CONT.                        
05853                                                                   
05854      IF PC-CLAIM-TYPE = '1'  OR  '3'                              
05855          GO TO 3405-REINSURE-RESERVE-CONT.                        
05856                                                                   
05857      MOVE PC-CERT-EFF-DT             TO CP-CERT-EFF-DT.           
05858      MOVE BIN-VALUATION-DT           TO CP-VALUATION-DT.          
05859      MOVE PC-PAID-THRU-DT            TO CP-PAID-THRU-DT.          
05860      MOVE 'A'                        TO CP-BENEFIT-TYPE           
05861                                         CP-CLAIM-TYPE.            
05862      MOVE PC-INCURRED-DT             TO CP-INCURRED-DT.           
05863      MOVE PC-REPORTED-DT             TO CP-REPORTED-DT.           
05864      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
05865      MOVE CR-AGE                     TO CP-ISSUE-AGE.             
05866      MOVE ZEROS                      TO CP-CDT-PERCENT.           
05867      MOVE '3'                        TO CP-CDT-METHOD.            
05868      MOVE CR-AH-TERM                 TO CP-ORIGINAL-TERM.         
05869      MOVE CR-AHAMT                   TO CP-ORIGINAL-BENEFIT       
05870                                         CP-RATING-BENEFIT-AMT.    
05871      MOVE PC-REMAINING-TERM          TO CP-REMAINING-TERM.        
05872      MOVE 'O'                        TO CP-CLAIM-STATUS.          
05873                                                                   
05874  3405-REINSURE-RESERVE-CONT.                                      
05875                                                                   
05876      PERFORM 3500-REINSURE-CALC-RESERVE                           
05877          THRU 3599-REINSURE-CALC-RESERVE-X                        
05878              VARYING SUB1 FROM +1 BY +1                           
05879                  UNTIL REIN-COMP (SUB1) = SPACES.                 
05880                                                                   
05881      PERFORM 2400-BUILD-RESERVE-EXTRACT.                          
05882      MOVE +0 TO RW-ACCUM-IBNR  RW-ACCUM-PAYCUR  RW-ACCUM-FUTRSV.  
05883      MOVE 'R'            TO DE-REIN.                              
05884 *****PERFORM EXTRACT-DATE-RELOAD.                                 
05885      MOVE DETAIL-EXTRACT TO SAVE-EXTRACT.                         
05886      MOVE +1             TO SUB1.                                 
05887                                                                   
05888  3410-REINSURE-RESERVE-LOOP.                                      
05889                                                                   
05890      IF REIN-COMP (SUB1) = SPACES                                 
05891          GO TO 3490-REINSURE-RESERVE-PRINT.                       
05892                                                                   
05893      IF REIN-REM-SW (SUB1) = 'Z'                                  
05894          ADD +1 TO SUB1                                           
05895          GO TO 3410-REINSURE-RESERVE-LOOP.                        
05896                                                                   
05897      MOVE REIN-COMP (SUB1)      TO DE-REI-COMP.                   
05898      MOVE REIN-WORK-FLDS (SUB1) TO RWF-FIELDS.                    
05899                                                                   
05900      IF PC-CLAIM-TYPE  = 1 OR 3                                   
05901          IF RWF-LF-RUNOFF-SW = 'N'                                
05902            IF CR-DTH-DT NOT LESS THAN RWF-EP-STOP-DATE            
05903                GO TO 3415-REINSURE-RESERVE-LOOP-ADD.              
05904                                                                   
05905      IF PC-CLAIM-TYPE  = 2 OR 4                                   
05906          IF RWF-AH-RUNOFF-SW = 'N'                                
05907            IF CR-DIS-DT NOT LESS THAN RWF-EP-STOP-DATE            
05908                GO TO 3415-REINSURE-RESERVE-LOOP-ADD.              
05909                                                                   
05910      MOVE RWF-LFAMT             TO DE-REI-LFAMT.                  
05911      MOVE RWF-LFPRM             TO DE-REI-LFPRM.                  
05912      MOVE RWF-AHAMT             TO DE-REI-AHAMT.                  
05913      MOVE RWF-AHPRM             TO DE-REI-AHPRM.                  
05914                                                                   
05915      IF PC-CLAIM-TYPE = 1 OR 3                                    
05916          MOVE RWF-DTH-IBNRL   TO DE-REI-IBNR                      
05917          MOVE RWF-DTH-PAYCURL TO DE-REI-PAYCUR                    
05918          MOVE RWF-DTH-FUTRSVL TO DE-REI-FUTRSV                    
05919      ELSE                                                         
05920          MOVE RWF-DIS-IBNRL   TO DE-REI-IBNR                      
05921          MOVE RWF-DIS-PAYCURL TO DE-REI-PAYCUR                    
05922          MOVE RWF-DIS-FUTRSVL TO DE-REI-FUTRSV.                   
05923                                                                   
05924      ADD DE-REI-IBNR            TO RW-ACCUM-IBNR.                 
05925      ADD DE-REI-PAYCUR          TO RW-ACCUM-PAYCUR.               
05926      ADD DE-REI-FUTRSV          TO RW-ACCUM-FUTRSV.               
05927                                                                   
05928      IF +0 NOT = DE-REI-LFAMT OR DE-REI-LFPRM OR DE-REI-AHAMT     
05929         OR DE-REI-AHPRM OR DE-REI-IBNR OR DE-REI-PAYCUR           
05930         OR DE-REI-FUTRSV                                          
05931          PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU                  
05932                                             2599-WRT-DETAIL-X     
05933          ADD +1 TO BC-R-CRS.                                      
05934                                                                   
05935  3415-REINSURE-RESERVE-LOOP-ADD.                                  
05936                                                                   
05937      ADD +1 TO SUB1.                                              
05938      MOVE SAVE-EXTRACT TO DETAIL-EXTRACT.                         
05939      PERFORM EXTRACT-DATE-LOAD.                                   
05940      GO TO 3410-REINSURE-RESERVE-LOOP.                            
05941                                                                   
05942  3490-REINSURE-RESERVE-PRINT.                                     
05943                                                                   
05944      IF RW-ACCUM-IBNR   = ZERO  AND                               
05945         RW-ACCUM-PAYCUR = ZERO  AND                               
05946         RW-ACCUM-FUTRSV = ZERO                                    
05947          GO TO 3499-REINSURE-RESERVE-X.                           
05948                                                                   
05949      PERFORM 8600-SETUP-DETAIL-LINE THRU 8689-SETUP-DETAIL-X.     
05950      MOVE RW-ACCUM-IBNR             TO TRD-IBNR.                  
05951      MOVE 'IBNR'                    TO TRD-IBNRX.                 
05952      MOVE RW-ACCUM-PAYCUR           TO TRD-PAYCUR.                
05953      MOVE 'PAY CURR'                TO TRD-PAYCURX.               
05954      MOVE RW-ACCUM-FUTRSV           TO TRD-FUTRSV.                
05955      MOVE 'FUTURE'                  TO TRD-FUTRSVX.               
05956                                                                   
05957      IF PC-CLAIM-TYPE = 1                                         
05958          MOVE LIFE-OVERRIDE-L6      TO TRD-RSV-DESC               
05959      ELSE                                                         
05960          MOVE AH-OVERRIDE-L6        TO TRD-RSV-DESC.              
05961      MOVE ' REIN. CLAIM RESERVES'   TO TRD-RSV-ACTION.            
05962                                                                   
05963      PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.       
05964                                                                   
05965  3499-REINSURE-RESERVE-X.                                         
05966      EXIT.                                                        
05967  EJECT                                                            
05968 ******************************************************************
05969 ***        C A L C U L A T E   R E I N   R E S E R V E S       ***
05970 ******************************************************************
05971                                                                   
05972  3500-REINSURE-CALC-RESERVE.                                      
05973                                                                   
05974      MOVE REIN-WORK-FLDS (SUB1) TO RWF-FIELDS.                    
05975      MOVE +0 TO RWF-DTH-IBNRL RWF-DTH-PAYCURL  RWF-DTH-FUTRSVL    
05976                 RWF-DIS-IBNRL  RWF-DIS-PAYCURL  RWF-DIS-FUTRSVL.  
05977                                                                   
05978      PERFORM REIN-FIND-CO-LOOP THRU FIND-REI-CO-EXIT              
05979         VARYING CO-SUB FROM 1 BY 1 UNTIL                          
05980           (RCT-REIN-CO (CO-SUB) = REIN-COMP (SUB1))               
05981        OR (CO-SUB GREATER REIN-CO-TABLE-ENT-CNT).                 
05982                                                                   
05983      IF CO-SUB GREATER REIN-CO-TABLE-ENT-CNT                      
05984         MOVE ZERO TO CO-SUB.                                      
05985                                                                   
05986      IF CO-SUB NOT = ZERO                                         
05987         IF RCT-CLM-CUTOFF-DT (CO-SUB) NOT = ZERO                  
05988            IF PC-CLAIM-TYPE = '1' OR '3'                          
05989              IF CR-DTH-DT LESS THAN RCT-CLM-CUTOFF-DT (CO-SUB)    
05990                  GO TO 3540-CONT-REIN-CALC-RESERVE.               
05991                                                                   
05992      IF PC-CLAIM-TYPE  = 1 OR 3                                   
05993          IF RWF-LF-RUNOFF-SW = 'N'                                
05994            IF CR-DTH-DT NOT LESS THAN RWF-EP-STOP-DATE            
05995                GO TO 3540-CONT-REIN-CALC-RESERVE.                 
05996                                                                   
05997      IF CO-SUB NOT = ZERO                                         
05998         IF RCT-CLM-CUTOFF-DT (CO-SUB) NOT = ZERO                  
05999            IF PC-CLAIM-TYPE = '2' OR '4'                          
06000              IF CR-DIS-DT LESS THAN RCT-CLM-CUTOFF-DT (CO-SUB)    
06001                  GO TO 3540-CONT-REIN-CALC-RESERVE.               
06002                                                                   
06003      IF PC-CLAIM-TYPE  = 2 OR 4                                   
06004          IF RWF-AH-RUNOFF-SW = 'N'                                
06005            IF CR-DIS-DT NOT LESS THAN RWF-EP-STOP-DATE            
06006                GO TO 3540-CONT-REIN-CALC-RESERVE.                 
06007                                                                   
06008      IF DTE-CLIENT NOT = 'MIC'                                    
06009          GO TO 3505-REIN-CALC-RESERVE-CONT.                       
06010                                                                   
06011      IF PC-CLAIM-TYPE = '1'  OR  '3'                              
06012          GO TO 3505-REIN-CALC-RESERVE-CONT.                       
06013                                                                   
06014      MOVE PC-FUTURE-RESERVE-AMT        TO RW-AHFUTRSVWK.          
06015                                                                   
06016      IF CR-AHTYP = ZERO  OR                                       
06017         CO-SUB = ZERO                                             
06018          GO TO 3505-REIN-CALC-RESERVE-CONT                        
06019      ELSE                                                         
06020          IF RCT-AH-CLM-MAX (CO-SUB) = ZERO                        
06021              GO TO 3505-REIN-CALC-RESERVE-CONT.                   
06022                                                                   
06023      COMPUTE MAX-REM-TERM ROUNDED =                               
06024                          RCT-AH-CLM-MAX (CO-SUB) / CR-AHAMT.      
06025                                                                   
06026      IF MAX-REM-TERM NOT LESS THAN CP-REMAINING-TERM              
06027          GO TO 3505-REIN-CALC-RESERVE-CONT.                       
06028                                                                   
06029      COMPUTE CP-ORIGINAL-TERM =                                   
06030                  CR-AH-TERM - CP-REMAINING-TERM + MAX-REM-TERM.   
06031                                                                   
06032      CALL 'ELRESVX' USING CALCULATION-PASS-AREA.                  
06033                                                                   
06034      IF  CP-RETURN-CODE = ZERO                                    
06035          MOVE CP-FUTURE-RESERVE    TO RW-AHFUTRSVWK.              
06036                                                                   
06037  3505-REIN-CALC-RESERVE-CONT.                                     
06038                                                                   
06039      IF SUB1 = RE-100-COMP                                        
06040          GO TO 3520-REIN-REMAINDER-RSRV.                          
06041                                                                   
06042      IF CR-LFAMT NOT = ZERO  AND                                  
06043         RWF-LFAMT NOT = ZERO                                      
06044          IF DTE-CLIENT NOT = 'MIC'                                
06045              COMPUTE RWF-DTH-IBNRL = (RW-LFIBNRWK *               
06046                          RWF-LFAMT) / (CR-LFAMT + CR-LFAMT-ALT)   
06047              COMPUTE RWF-DTH-PAYCURL = (RW-LFPAYCURWK *           
06048                          RWF-LFAMT) / (CR-LFAMT + CR-LFAMT-ALT)   
06049              COMPUTE RWF-DTH-FUTRSVL = (RW-LFFUTRSVWK *           
06050                          RWF-LFAMT) / (CR-LFAMT + CR-LFAMT-ALT)   
06051          ELSE                                                     
06052              MOVE RW-LFIBNRWK         TO RWF-DTH-IBNRL            
06053              MOVE RW-LFPAYCURWK       TO RWF-DTH-PAYCURL          
06054              MOVE RW-LFFUTRSVWK       TO RWF-DTH-FUTRSVL.         
06055                                                                   
06056      IF CR-AHAMT NOT = ZERO  AND                                  
06057         RWF-AHAMT NOT = ZERO                                      
06058          IF DTE-CLIENT NOT = 'MIC'                                
06059              COMPUTE RWF-DIS-IBNRL = (RW-AHIBNRWK *               
06060                          RWF-AHAMT) / CR-AHAMT                    
06061              COMPUTE RWF-DIS-PAYCURL = (RW-AHPAYCURWK *           
06062                          RWF-AHAMT) / CR-AHAMT                    
06063              COMPUTE RWF-DIS-FUTRSVL = (RW-AHFUTRSVWK *           
06064                          RWF-AHAMT) / CR-AHAMT                    
06065          ELSE                                                     
06066              MOVE RW-AHIBNRWK         TO RWF-DIS-IBNRL            
06067              MOVE RW-AHPAYCURWK       TO RWF-DIS-PAYCURL          
06068              MOVE RW-AHFUTRSVWK       TO RWF-DIS-FUTRSVL.         
06069                                                                   
06070  3510-REINSURE-CALC-RESERVE-ADD.                                  
06071                                                                   
06072      ADD RWF-DTH-IBNRL   TO RWF-DTH-IBNR.                         
06073      ADD RWF-DTH-PAYCURL TO RWF-DTH-PAYCUR.                       
06074      ADD RWF-DTH-FUTRSVL TO RWF-DTH-FUTRSV.                       
06075      ADD RWF-DIS-IBNRL   TO RWF-DIS-IBNR.                         
06076      ADD RWF-DIS-PAYCURL TO RWF-DIS-PAYCUR.                       
06077      ADD RWF-DIS-FUTRSVL TO RWF-DIS-FUTRSV.                       
06078                                                                   
06079      IF DTE-CLIENT NOT = 'MIC'  AND  'MCC'                        
06080          GO TO 3519-REINSURE-CALC-RESERVE-END.                    
06081                                                                   
06082      IF CO-SUB = ZERO                                             
06083          GO TO 3519-REINSURE-CALC-RESERVE-END.                    
06084                                                                   
06085  3513-REIN-CHK-LF-RSRV-LIMIT.                                     
06086      IF RCT-LF-CLM-MAX (CO-SUB) = ZERO                            
06087          GO TO 3515-REIN-CHK-AH-RSRV-LIMIT.                       
06088                                                                   
06089      IF RWF-DTH-IBNR GREATER THAN RCT-LF-CLM-MAX (CO-SUB)         
06090          MOVE RCT-LF-CLM-MAX (CO-SUB) TO RWF-DTH-IBNR.            
06091                                                                   
06092      COMPUTE EXCESS-RESERVE = RCT-LF-CLM-MAX (CO-SUB) -           
06093                                       (RWF-LFCLM +                
06094                                        RWF-DTH-PAYCUR +           
06095                                        RWF-DTH-FUTRSV).           
06096                                                                   
06097      IF EXCESS-RESERVE NOT LESS THAN ZERO                         
06098          GO TO 3515-REIN-CHK-AH-RSRV-LIMIT.                       
06099                                                                   
06100      ADD EXCESS-RESERVE TO RWF-DTH-PAYCURL.                       
06101                                                                   
06102      IF RWF-DTH-PAYCURL LESS THAN ZERO                            
06103          ADD RWF-DTH-PAYCURL TO RWF-DTH-FUTRSVL                   
06104          MOVE ZEROS TO RWF-DTH-PAYCURL.                           
06105      IF RWF-DTH-FUTRSVL LESS THAN ZERO                            
06106          MOVE ZEROS TO RWF-DTH-FUTRSVL.                           
06107                                                                   
06108      ADD EXCESS-RESERVE TO RWF-DTH-PAYCUR.                        
06109                                                                   
06110      IF RWF-DTH-PAYCUR LESS THAN ZERO                             
06111          ADD RWF-DTH-PAYCUR TO RWF-DTH-FUTRSV                     
06112          MOVE ZEROS TO RWF-DTH-PAYCUR.                            
06113      IF RWF-DTH-FUTRSV LESS THAN ZERO                             
06114          MOVE ZEROS TO RWF-DTH-FUTRSV.                            
06115                                                                   
06116  3515-REIN-CHK-AH-RSRV-LIMIT.                                     
06117      IF RCT-AH-CLM-MAX (CO-SUB) = ZERO                            
06118          GO TO 3519-REINSURE-CALC-RESERVE-END.                    
06119                                                                   
06120      IF RWF-DIS-IBNR GREATER THAN RCT-AH-CLM-MAX (CO-SUB)         
06121          MOVE RCT-AH-CLM-MAX (CO-SUB) TO RWF-DIS-IBNR.            
06122                                                                   
06123      COMPUTE EXCESS-RESERVE = RCT-AH-CLM-MAX (CO-SUB) -           
06124                                       (RWF-AHCLM +                
06125                                        RWF-DIS-PAYCUR +           
06126                                        RWF-DIS-FUTRSV).           
06127                                                                   
06128      IF EXCESS-RESERVE NOT LESS THAN ZERO                         
06129          GO TO 3519-REINSURE-CALC-RESERVE-END.                    
06130                                                                   
06131      ADD EXCESS-RESERVE TO RWF-DIS-PAYCURL.                       
06132                                                                   
06133      IF RWF-DIS-PAYCURL LESS THAN ZERO                            
06134          ADD RWF-DIS-PAYCURL TO RWF-DIS-FUTRSVL                   
06135          MOVE ZEROS TO RWF-DIS-PAYCURL.                           
06136      IF RWF-DIS-FUTRSVL LESS THAN ZERO                            
06137          MOVE ZEROS TO RWF-DIS-FUTRSVL.                           
06138                                                                   
06139      ADD EXCESS-RESERVE TO RWF-DIS-PAYCUR.                        
06140                                                                   
06141      IF RWF-DIS-PAYCUR LESS THAN ZERO                             
06142          ADD RWF-DIS-PAYCUR TO RWF-DIS-FUTRSV                     
06143          MOVE ZEROS TO RWF-DIS-PAYCUR.                            
06144      IF RWF-DIS-FUTRSV LESS THAN ZERO                             
06145          MOVE ZEROS TO RWF-DIS-FUTRSV.                            
06146                                                                   
06147  3519-REINSURE-CALC-RESERVE-END.                                  
06148                                                                   
06149      IF +0 NOT = RWF-DTH-IBNR OR RWF-DTH-PAYCUR                   
06150         OR RWF-DTH-FUTRSV                                         
06151          MOVE 'X' TO REIN-LF-FLG (SUB1).                          
06152                                                                   
06153      IF +0 NOT = RWF-DIS-IBNR OR RWF-DIS-PAYCUR                   
06154         OR RWF-DIS-FUTRSV                                         
06155          MOVE 'X' TO REIN-AH-FLG (SUB1).                          
06156                                                                   
06157      GO TO 3540-CONT-REIN-CALC-RESERVE.                           
06158                                                                   
06159  3520-REIN-REMAINDER-RSRV.                                        
06160                                                                   
06161      MOVE RW-LFIBNRWK   TO RWF-DTH-IBNRL.                         
06162      MOVE RW-LFPAYCURWK TO RWF-DTH-PAYCURL.                       
06163      MOVE RW-LFFUTRSVWK TO RWF-DTH-FUTRSVL.                       
06164      MOVE RW-AHIBNRWK   TO RWF-DIS-IBNRL.                         
06165      MOVE RW-AHPAYCURWK TO RWF-DIS-PAYCURL.                       
06166      MOVE RW-AHFUTRSVWK TO RWF-DIS-FUTRSVL.                       
06167                                                                   
06168      PERFORM 3530-REIN-SUB-RSRV                                   
06169          THRU 3539-REIN-SUB-RSRV-X                                
06170              VARYING SUB2 FROM +1 BY +1 UNTIL SUB2 = SUB1.        
06171                                                                   
06172      GO TO 3510-REINSURE-CALC-RESERVE-ADD.                        
06173                                                                   
06174  3530-REIN-SUB-RSRV.                                              
06175                                                                   
06176      SUBTRACT REIN-DTH-IBNRL (SUB2)   FROM RWF-DTH-IBNRL.         
06177      SUBTRACT REIN-DTH-PAYCURL (SUB2) FROM RWF-DTH-PAYCURL.       
06178      SUBTRACT REIN-DTH-FUTRSVL (SUB2) FROM RWF-DTH-FUTRSVL.       
06179      SUBTRACT REIN-DIS-IBNRL (SUB2)   FROM RWF-DIS-IBNRL.         
06180      SUBTRACT REIN-DIS-PAYCURL (SUB2) FROM RWF-DIS-PAYCURL.       
06181      SUBTRACT REIN-DIS-FUTRSVL (SUB2) FROM RWF-DIS-FUTRSVL.       
06182                                                                   
06183  3539-REIN-SUB-RSRV-X.                                            
06184      EXIT.                                                        
06185                                                                   
06186  3540-CONT-REIN-CALC-RESERVE.                                     
06187                                                                   
06188      IF REIN-LF-AH-FLGS (SUB1) NOT = SPACES                       
06189          MOVE 'X' TO REIN-BUS-FLAG.                               
06190      MOVE RWF-FIELDS TO REIN-WORK-FLDS (SUB1).                    
06191                                                                   
06192  3599-REINSURE-CALC-RESERVE-X.                                    
06193      EXIT.                                                        
06194  EJECT                                                            
06195 ******************************************************************
06196 ***       R E I N S U R A N C E   C A L C U L A T I O N S      ***
06197 ******************************************************************
06198                                                                   
06199 *REINSURANCE-CALCULATIONS.                                        
06200                              COPY ECSRIRTN.                    
06201  EJECT                                                            
06202 ******************************************************************
06203 ***   R E C A L C U L A T I O N   O F   R E I N S U R A N C E  ***
06204 ******************************************************************
06205                                                                   
06206  3600-RC-REINSURANCE.                                             
06207                                                                   
06208      IF DTE-PGM-OPT NOT = '1'                                     
06209          GO TO 3799-RC-REINSURANCE-X.                             
06210                                                                   
06211      IF DTE-CLIENT = 'WSL'                                        
06212          IF CR-REIN-TABLE = '00A'                                 
06213              GO TO 3799-RC-REINSURANCE-X.                         
06214                                                                   
06215      IF CR-REIN-TABLE = SPACES OR ZEROS                           
06216          PERFORM CLEAR-REIN-HOLD THRU CLEAR-REIN-HOLD-X.          
06217                                                                   
06218  3610-CLEAR-RC-REIN.                                              
06219                                                                   
06220      MOVE SPACES TO RC-REIN-LEVELS (1).                           
06221      MOVE ZEROS  TO RC-REIN-LFAMT (1)                             
06222                     RC-REIN-LFPRM (1)                             
06223                     RC-REIN-AHAMT (1)                             
06224                     RC-REIN-AHPRM (1)                             
06225                     RC-REIN-LFRFND (1)                            
06226                     RC-REIN-AHRFND (1)                            
06227                     RC-REIN-LFCLM (1)                             
06228                     RC-REIN-AHCLM (1).                            
06229                                                                   
06230      PERFORM 3620-CLEAR-RC-LEVELS VARYING SUB1 FROM +2 BY +1      
06231          UNTIL RC-REIN-COMP (SUB1) = SPACES.                      
06232                                                                   
06233      MOVE +1 TO SUB1.                                             
06234      MOVE +0 TO SUB2.                                             
06235                                                                   
06236      GO TO 3630-RC-REIN-LOOP-1.                                   
06237                                                                   
06238  3620-CLEAR-RC-LEVELS.                                            
06239                                                                   
06240      MOVE RC-REIN-LEVELS (1) TO RC-REIN-LEVELS (SUB1).            
06241                                                                   
06242                                                                   
06243  3630-RC-REIN-LOOP-1.                                             
06244                                                                   
06245      IF REIN-LF-AH-FLGS (SUB1) NOT = SPACES                       
06246          ADD +1 TO SUB2                                           
06247          MOVE REIN-COMP (SUB1)   TO RC-REIN-COMP (SUB2)           
06248          MOVE REIN-LFAMT (SUB1)  TO RC-REIN-LFAMT (SUB2)          
06249          MOVE REIN-LFPRM (SUB1)  TO RC-REIN-LFPRM (SUB2)          
06250          MOVE REIN-AHAMT (SUB1)  TO RC-REIN-AHAMT (SUB2)          
06251          MOVE REIN-AHPRM (SUB1)  TO RC-REIN-AHPRM (SUB2)          
06252          MOVE REIN-LFRFND (SUB1) TO RC-REIN-LFRFND (SUB2)         
06253          MOVE REIN-AHRFND (SUB1) TO RC-REIN-AHRFND (SUB2)         
06254          MOVE REIN-LFCLM (SUB1)  TO RC-REIN-LFCLM (SUB2)          
06255          MOVE REIN-AHCLM (SUB1)  TO RC-REIN-AHCLM (SUB2)          
06256          MOVE REIN-REM-SW (SUB1) TO RC-REIN-REM-SW (SUB2).        
06257                                                                   
06258      ADD +1 TO SUB1.                                              
06259      IF REIN-COMP (SUB1) NOT = SPACES                             
06260          GO TO 3630-RC-REIN-LOOP-1.                               
06261                                                                   
06262      PERFORM CLEAR-REIN-HOLD THRU CLEAR-REIN-HOLD-X.              
06263                                                                   
06264      MOVE AM-REI-TABLE TO CR-REIN-TABLE.                          
06265      IF CR-REIN-TABLE = SPACES OR ZEROS                           
06266          MOVE +1                     TO SUB1                      
06267          GO TO 3710-RC-REIN-LOOP-2.                               
06268                                                                   
06269      PERFORM 3010-RR-READ-REIN THRU 3019-RR-READ-REIN-X.          
06270                                                                   
06271  3640-NSP-REIN-RATING-ROUTINE.                                    
06272                                                                   
06273      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
06274      MOVE 'L'                        TO DC-OPTION-CODE.           
06275      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
06276      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
06277      MOVE SPACES                     TO CP-ACCT-FLD-5.            
06278      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
06279      MOVE CR-AGE                     TO CP-ISSUE-AGE.             
06280      MOVE CR-APR                     TO CP-LOAN-APR.              
06281      MOVE CR-PMT-FREQ                TO CP-PAY-FREQUENCY.         
06282      MOVE CR-LOAN-TERM               TO CP-LOAN-TERM.             
06283      MOVE ZEROS                      TO CP-DEVIATION-CODE.        
06284      MOVE ZEROS                      TO CP-CLASS-CODE.            
06285                                                                   
06286      IF DTE-CLIENT = 'TIC'                                        
06287         IF CR-REIN-TABLE = '01T' OR '02T' OR '03T'                
06288             MOVE ZERO                TO CP-CLASS-CODE             
06289         ELSE                                                      
06290             MOVE CR-RATING-CLASS     TO CP-CLASS-CODE.            
06291                                                                   
06292      MOVE '3'                        TO CP-PROCESS-TYPE.          
06293      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
06294      MOVE CR-PMT-EXTENSION-DAYS      TO CP-TERM-OR-EXT-DAYS.      
06295                                                                   
06296  3650-NSP-REIN-LIFE.                                              
06297                                                                   
06298      IF CR-LFTYP = ZERO                                           
06299          GO TO 3660-NSP-REIN-AH.                                  
06300      IF (RE-NSP-ST-CD-LF = ZEROS OR SPACES) 
              OR (DTE-CLIENT = 'DCC')
06301          GO TO 3660-NSP-REIN-AH.                                  
06302      IF DTE-CLIENT NOT = 'LBL'                                    
06303          IF (CLAS-I-BAL (CLAS-INDEXL) = 'B')                      
06304              GO TO 3660-NSP-REIN-AH.                              
06305                                                                   
06306      IF RE-NSP-ST-CD-LF NOT = STATE-L                             
06307          MOVE RE-NSP-ST-CD-LF        TO STATE-L                   
06308          PERFORM 8200-STATE-CODE-LOOKUP THRU 8299-STATE-LOOKUP-X. 
06309                                                                   
06310      MOVE CLAS-I-RL-AH (CLAS-INDEXL) TO CP-BENEFIT-TYPE.          
06311      MOVE CR-LF-TERM                 TO CP-ORIGINAL-TERM.         
06312      MOVE CR-LFAMT                   TO CP-ORIGINAL-BENEFIT       
06313                                         CP-RATING-BENEFIT-AMT.    
06314      MOVE CLAS-I-EP (CLAS-INDEXL)    TO CP-EARNING-METHOD.        
06315      MOVE CLAS-I-BAL (CLAS-INDEXL)   TO CP-SPECIAL-CALC-CD.       
06316      MOVE CR-LFTYP                   TO CP-BENEFIT-CD.            
06317      MOVE LIFE-OVERRIDE-L1           TO CP-LIFE-OVERRIDE-CODE.    
06318      MOVE AM-LF-DEVIATION-PCT        TO CP-RATE-DEV-PCT.          
06319      MOVE RE-NSP-ST-CD-LF            TO CP-STATE.                 
06320      MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV.       
06321                                                                   
06322      IF CP-STATE-STD-ABBRV = 'OR'                                 
06323          COMPUTE CP-RATING-BENEFIT-AMT = CR-LFAMT + CR-LFAMT-ALT. 
06324                                                                   
06325      PERFORM 8100-GET-RATE THRU 8199-GET-RATE-X.                  
06326                                                                   
06327      IF CP-ERROR-RATE-NOT-FOUND  OR                               
06328         CP-ERROR-RATE-IS-ZERO                                     
06329          MOVE 'LIFE NET SINGLE PREMIUM RATE NOT FOUND'            
06330                                      TO WS-ABEND-MESSAGE          
06331          MOVE 0302                   TO WS-ABEND-CODE             
CIDMOD         DISPLAY '******************************'
CIDMOD         DISPLAY '***** ERROR LOCATION 018 *****'
CIDMOD         DISPLAY '******************************'
06332          MOVE WS-ABEND-CODE        TO WS-RETURN-CODE              
06333          GO TO ABEND-PGM.                                         
06334                                                                   
103102     MOVE CP-CALC-PREMIUM          TO CR-LF-NSP-PRM.           
103102
103102*    COMPUTE CR-LF-NSP-PRM ROUNDED =                              
103102*                      (CR-LFAMT / 100) * CP-PREMIUM-RATE.        
06337                                                                   
06338      COMPUTE CR-LF-NSP-PRM ROUNDED =  CR-LF-NSP-PRM +             
06339                    ((CR-LFAMT-ALT / 100) * CP-PREMIUM-RATE).      
06340                                                                   
06341      MOVE CP-PREMIUM-RATE            TO CR-LF-NSP-PRM-RATE.       
06342                                                                   
06343  3660-NSP-REIN-AH.                                                
06344                                                                   
06345      IF CR-AHTYP = ZERO                                           
06346          GO TO 3669-NSP-REIN-X.                                   
06347      IF (RE-NSP-ST-CD-AH = ZEROS OR SPACES)
              OR (DTE-CLIENT = 'DCC')
06348          GO TO 3669-NSP-REIN-X.                                   
06349      IF DTE-CLIENT NOT = 'LBL'                                    
06350          IF (CLAS-I-BAL (CLAS-INDEXA) = 'B')                      
06351              GO TO 3669-NSP-REIN-X.                               
06352                                                                   
06353      IF RE-NSP-ST-CD-AH NOT = STATE-L                             
06354          MOVE RE-NSP-ST-CD-AH        TO STATE-L                   
06355          PERFORM 8200-STATE-CODE-LOOKUP THRU 8299-STATE-LOOKUP-X. 
06356                                                                   
06357      MOVE CLAS-I-RL-AH (CLAS-INDEXA) TO CP-BENEFIT-TYPE.          
06358      MOVE CR-AH-TERM                 TO CP-ORIGINAL-TERM.         
06359      MOVE CR-AHAMT                   TO CP-ORIGINAL-BENEFIT       
06360                                         CP-RATING-BENEFIT-AMT.    
06361      MOVE CLAS-I-EP (CLAS-INDEXA)    TO CP-EARNING-METHOD.        
06362      MOVE CLAS-I-BAL (CLAS-INDEXA)   TO CP-SPECIAL-CALC-CD.       
06363      MOVE CR-AHTYP                   TO CP-BENEFIT-CD.            
06364      MOVE AH-OVERRIDE-L1             TO CP-AH-OVERRIDE-CODE.      
06365      MOVE AM-AH-DEVIATION-PCT        TO CP-RATE-DEV-PCT.          
06366      MOVE RE-NSP-ST-CD-AH            TO CP-STATE.                 
06367      MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV.       
06368                                                                   
06369      IF CP-STATE-STD-ABBRV = 'OR'                                 
06370          COMPUTE CP-RATING-BENEFIT-AMT = CR-AHAMT * CR-AH-TERM.   
06371                                                                   
06372      PERFORM 8100-GET-RATE THRU 8199-GET-RATE-X.                  
06373                                                                   
06374      IF CP-ERROR-RATE-NOT-FOUND  OR                               
06375         CP-ERROR-RATE-IS-ZERO                                     
06376          MOVE 'A&H NET SINGLE PREMIUM RATE NOT FOUND'             
06377                                      TO WS-ABEND-MESSAGE          
06378          MOVE 0302                   TO WS-ABEND-CODE             
CIDMOD         DISPLAY '******************************'
CIDMOD         DISPLAY '***** ERROR LOCATION 019 *****'
CIDMOD         DISPLAY '******************************'
06379          MOVE WS-ABEND-CODE        TO WS-RETURN-CODE              
06380          GO TO ABEND-PGM.                                         
06381                                                                   
06382      COMPUTE CR-AH-NSP-PRM ROUNDED =                              
06383              ((CR-AHAMT * CR-AH-TERM) / 100) * CP-PREMIUM-RATE.   
06384                                                                   
06385      MOVE CP-PREMIUM-RATE            TO CR-AH-NSP-PRM-RATE.       
06386                                                                   
06387  3669-NSP-REIN-X.                                                 
06388      EXIT.                                                        
06389  EJECT                                                            
06390 ******************************************************************
06391 ***        R E C A L C U L A T E   R E I N S U R A N C E       ***
06392 ******************************************************************
06393                                                                   
06394  3700-RC-REIN-PERF-CALC.                                          
06395                                                                   
06396                              COPY ECSRTPFM.                       
06397                                                                   
06398      IF CR-STATE NOT = STATE-L                                    
06399          MOVE CR-STATE               TO STATE-L                   
06400          PERFORM 8200-STATE-CODE-LOOKUP THRU 8299-STATE-LOOKUP-X. 
06401                                                                   
06402      MOVE 'X'                     TO REIN-RT-SW.                  
06403      PERFORM 3200-REINSURE-CANCEL THRU 3299-REINSURE-CANCEL-X.    
06404      PERFORM 3300-REINSURE-CLAIM THRU 3399-REINSURE-CLAIM-X.      
06405      MOVE ' '                     TO REIN-RT-SW.                  
06406                                                                   
06407  3710-RC-REIN-LOOP-2.                                             
06408                                                                   
06409      IF REIN-COMP (SUB1) = SPACES                                 
06410          GO TO 3730-RC-REIN-EXTRACTS.                             
06411                                                                   
06412      IF REIN-LF-AH-FLGS (SUB1) = SPACES                           
06413         ADD +1 TO SUB1                                            
06414          GO TO 3710-RC-REIN-LOOP-2.                               
06415                                                                   
06416      MOVE +1 TO SUB2.                                             
06417                                                                   
06418  3720-RC-REIN-LOOP-3.                                             
06419                                                                   
06420      IF RC-REIN-COMP (SUB2) = SPACES                              
06421          ADD +1 TO SUB1                                           
06422          GO TO 3710-RC-REIN-LOOP-2.                               
06423                                                                   
06424      IF REIN-COMP (SUB1) NOT = RC-REIN-COMP (SUB2)                
06425          ADD +1 TO SUB2                                           
06426          GO TO 3720-RC-REIN-LOOP-3.                               
06427                                                                   
06428      IF REIN-LFAMT (SUB1) = RC-REIN-LFAMT (SUB2)                  
06429         AND REIN-LFPRM (SUB1) = RC-REIN-LFPRM (SUB2)              
06430         AND REIN-AHAMT (SUB1) = RC-REIN-AHAMT (SUB2)              
06431         AND REIN-AHPRM (SUB1) = RC-REIN-AHPRM (SUB2)              
06432          MOVE 'X' TO RC-OLD-ISS (SUB2)                            
06433          MOVE 'X' TO RC-NEW-ISS (SUB1).                           
06434                                                                   
06435      IF REIN-LFRFND (SUB1) = RC-REIN-LFRFND (SUB2)                
06436         AND REIN-AHRFND (SUB1) = RC-REIN-AHRFND (SUB2)            
06437          MOVE 'X' TO RC-OLD-CNC (SUB2)                            
06438          MOVE 'X' TO RC-NEW-CNC (SUB1).                           
06439                                                                   
06440      IF REIN-LFCLM (SUB1) = RC-REIN-LFCLM (SUB2)                  
06441          MOVE 'X' TO RC-OLD-LF-CLM (SUB2)                         
06442          MOVE 'X' TO RC-NEW-LF-CLM (SUB1).                        
06443                                                                   
06444      IF REIN-AHCLM (SUB1) = RC-REIN-AHCLM (SUB2)                  
06445          MOVE 'X' TO RC-OLD-AH-CLM (SUB2)                         
06446          MOVE 'X' TO RC-NEW-AH-CLM (SUB1).                        
06447                                                                   
06448      ADD +1 TO SUB2.                                              
06449      GO TO 3720-RC-REIN-LOOP-3.                                   
06450                                                                   
06451  3730-RC-REIN-EXTRACTS.                                           
06452                                                                   
06453      PERFORM 2000-INITIALIZE-DETAIL-EXTRACT THRU 2099-INIT-DTL-X. 
06454      MOVE 'R' TO DE-REIN.                                         
06455      MOVE 'J' TO DE-TRANS.                                        
06456 *****PERFORM EXTRACT-DATE-RELOAD.                                 
06457      MOVE DETAIL-EXTRACT TO SAVE-EXTRACT.                         
06458      MOVE +1 TO SUB1.                                             
06459                                                                   
06460  3740-RC-REIN-OLD-ISSUE.                                          
06461                                                                   
06462      IF RC-REIN-COMP (SUB1) = SPACES                              
06463          MOVE +1 TO SUB1                                          
06464          GO TO 3745-RC-REIN-NEW-ISSUE.                            
06465                                                                   
06466      IF RC-REIN-REM-SW (SUB1) = 'Z'                               
06467          ADD +1 TO SUB1                                           
06468          GO TO 3740-RC-REIN-OLD-ISSUE.                            
06469                                                                   
06470      IF RC-OLD-ISS (SUB1) = 'X'                                   
06471          ADD +1 TO SUB1                                           
06472          GO TO 3740-RC-REIN-OLD-ISSUE.                            
06473                                                                   
06474      IF RC-REIN-LFAMT (SUB1) = ZERO                               
06475         AND RC-REIN-LFPRM (SUB1) = ZERO                           
06476         AND RC-REIN-AHAMT (SUB1) = ZERO                           
06477         AND RC-REIN-AHPRM (SUB1) = ZERO                           
06478          ADD +1 TO SUB1                                           
06479          GO TO 3740-RC-REIN-OLD-ISSUE.                            
06480                                                                   
06481      MOVE SAVE-EXTRACT TO DETAIL-EXTRACT.                         
06482      PERFORM EXTRACT-DATE-LOAD.                                   
06483      MOVE RC-REIN-COMP (SUB1) TO DE-REI-COMP.                     
06484      MULTIPLY RC-REIN-LFAMT (SUB1) BY -1 GIVING DE-REI-LFAMT.     
06485      MULTIPLY RC-REIN-LFPRM (SUB1) BY -1 GIVING DE-REI-LFPRM.     
06486      MULTIPLY RC-REIN-AHAMT (SUB1) BY -1 GIVING DE-REI-AHAMT.     
06487      MULTIPLY RC-REIN-AHPRM (SUB1) BY -1 GIVING DE-REI-AHPRM.     
06488      PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X.   
06489                                                                   
06490      ADD +1 TO BC-R-ISS.                                          
06491      ADD +1 TO SUB1.                                              
06492      GO TO 3740-RC-REIN-OLD-ISSUE.                                
06493                                                                   
06494  3745-RC-REIN-NEW-ISSUE.                                          
06495                                                                   
06496      IF REIN-COMP (SUB1) = SPACES                                 
06497          PERFORM 2200-BUILD-CANCEL-EXTRACT THRU                   
06498                                          2259-END-CANCEL-BUILD    
06499          MOVE 'R'      TO DE-REIN                                 
06500          MOVE 'K'      TO DE-TRANS                                
06501 *********PERFORM EXTRACT-DATE-RELOAD                              
06502          MOVE DETAIL-EXTRACT TO SAVE-EXTRACT                      
06503          MOVE +1       TO SUB1                                    
06504          GO TO 3750-RC-REIN-OLD-CANCEL.                           
06505                                                                   
06506      IF REIN-REM-SW (SUB1) = 'Z'                                  
06507          ADD +1 TO SUB1                                           
06508          GO TO 3745-RC-REIN-NEW-ISSUE.                            
06509                                                                   
06510      IF RC-NEW-ISS (SUB1) = 'X'                                   
06511          ADD +1 TO SUB1                                           
06512          GO TO 3745-RC-REIN-NEW-ISSUE.                            
06513                                                                   
06514      IF REIN-LFAMT (SUB1) = ZERO                                  
06515         AND REIN-LFPRM (SUB1) = ZERO                              
06516         AND REIN-AHAMT (SUB1) = ZERO                              
06517         AND REIN-AHPRM (SUB1) = ZERO                              
06518          ADD +1 TO SUB1                                           
06519          GO TO 3745-RC-REIN-NEW-ISSUE.                            
06520                                                                   
06521      MOVE SAVE-EXTRACT      TO DETAIL-EXTRACT.                    
06522      PERFORM EXTRACT-DATE-LOAD.                                   
06523      MOVE REIN-COMP (SUB1)  TO DE-REI-COMP.                       
06524      MOVE REIN-LFAMT (SUB1) TO DE-REI-LFAMT.                      
06525      MOVE REIN-LFPRM (SUB1) TO DE-REI-LFPRM.                      
06526      MOVE REIN-AHAMT (SUB1) TO DE-REI-AHAMT.                      
06527      MOVE REIN-AHPRM (SUB1) TO DE-REI-AHPRM.                      
06528      PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X.   
06529                                                                   
06530      ADD +1 TO BC-R-ISS.                                          
06531      ADD +1 TO SUB1.                                              
06532      GO TO 3745-RC-REIN-NEW-ISSUE.                                
06533                                                                   
06534  3750-RC-REIN-OLD-CANCEL.                                         
06535                                                                   
06536      IF RC-REIN-COMP (SUB1) = SPACES                              
06537          MOVE +1 TO SUB1                                          
06538          GO TO 3755-RC-REIN-NEW-CANCEL.                           
06539                                                                   
06540      IF RC-REIN-REM-SW (SUB1) = 'Z'                               
06541          ADD +1 TO SUB1                                           
06542          GO TO 3750-RC-REIN-OLD-CANCEL.                           
06543                                                                   
06544      IF RC-OLD-CNC (SUB1) = 'X'                                   
06545          ADD +1 TO SUB1                                           
06546          GO TO 3750-RC-REIN-OLD-CANCEL.                           
06547                                                                   
06548      IF RC-REIN-LFRFND (SUB1) = ZERO                              
06549         AND RC-REIN-AHRFND (SUB1) = ZERO                          
06550          ADD +1 TO SUB1                                           
06551          GO TO 3750-RC-REIN-OLD-CANCEL.                           
06552                                                                   
06553      MOVE SAVE-EXTRACT        TO DETAIL-EXTRACT.                  
06554      PERFORM EXTRACT-DATE-LOAD.                                   
06555      MOVE RC-REIN-COMP (SUB1) TO DE-REI-COMP.                     
06556      MULTIPLY RC-REIN-LFRFND (SUB1) BY -1 GIVING DE-REI-LFRFND.   
06557      MULTIPLY RC-REIN-AHRFND (SUB1) BY -1 GIVING DE-REI-AHRFND.   
06558      PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X.   
06559                                                                   
06560      ADD +1 TO BC-R-CNC.                                          
06561      ADD +1 TO SUB1.                                              
06562      GO TO 3750-RC-REIN-OLD-CANCEL.                               
06563                                                                   
06564  3755-RC-REIN-NEW-CANCEL.                                         
06565                                                                   
06566      IF REIN-COMP (SUB1) = SPACES                                 
06567          PERFORM 2000-INITIALIZE-DETAIL-EXTRACT                   
06568          MOVE 'R' TO DE-REIN                                      
06569          MOVE 'L' TO DE-TRANS                                     
06570 *********PERFORM EXTRACT-DATE-RELOAD                              
06571          MOVE DETAIL-EXTRACT TO SAVE-EXTRACT                      
06572          MOVE +1 TO SUB1                                          
06573          GO TO 3760-RC-REIN-OLD-CLAIM.                            
06574                                                                   
06575      IF REIN-REM-SW (SUB1) = 'Z'                                  
06576          ADD +1 TO SUB1                                           
06577          GO TO 3755-RC-REIN-NEW-CANCEL.                           
06578                                                                   
06579      IF RC-NEW-CNC (SUB1) = 'X'                                   
06580          ADD +1 TO SUB1                                           
06581          GO TO 3755-RC-REIN-NEW-CANCEL.                           
06582                                                                   
06583      IF REIN-LFRFND (SUB1) = ZERO                                 
06584         AND REIN-AHRFND (SUB1) = ZERO                             
06585          ADD +1 TO SUB1                                           
06586          GO TO 3755-RC-REIN-NEW-CANCEL.                           
06587                                                                   
06588      MOVE SAVE-EXTRACT       TO DETAIL-EXTRACT.                   
06589      PERFORM EXTRACT-DATE-LOAD.                                   
06590      MOVE REIN-COMP (SUB1)   TO DE-REI-COMP.                      
06591      MOVE REIN-LFRFND (SUB1) TO DE-REI-LFRFND.                    
06592      MOVE REIN-AHRFND (SUB1) TO DE-REI-AHRFND.                    
06593      PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X.   
06594                                                                   
06595      ADD +1 TO BC-R-CNC.                                          
06596      ADD +1 TO SUB1.                                              
06597      GO TO 3755-RC-REIN-NEW-CANCEL.                               
06598                                                                   
06599  3760-RC-REIN-OLD-CLAIM.                                          
06600                                                                   
06601      IF RC-REIN-COMP (SUB1) = SPACES                              
06602          MOVE +1 TO SUB1                                          
06603          GO TO 3780-RC-REIN-NEW-CLAIM.                            
06604                                                                   
06605      IF RC-REIN-REM-SW (SUB1) = 'Z'                               
06606          ADD +1 TO SUB1                                           
06607          GO TO 3760-RC-REIN-OLD-CLAIM.                            
06608                                                                   
06609      IF RC-OLD-LF-CLM (SUB1) = 'X'                                
06610          GO TO 3770-RC-REIN-OLD-AH-CLM.                           
06611                                                                   
06612      IF RC-REIN-LFCLM (SUB1) = ZERO                               
06613          GO TO 3770-RC-REIN-OLD-AH-CLM.                           
06614                                                                   
06615      MOVE SAVE-EXTRACT         TO DETAIL-EXTRACT.                 
06616      PERFORM EXTRACT-DATE-LOAD.                                   
06617      MOVE RC-REIN-COMP (SUB1)  TO DE-REI-COMP.                    
06618      MOVE RC-REIN-LFAMT (SUB1) TO DE-REI-LFAMT.                   
06619      MOVE RC-REIN-LFPRM (SUB1) TO DE-REI-LFPRM.                   
06620      MOVE RC-REIN-AHAMT (SUB1) TO DE-REI-AHAMT.                   
06621      MOVE RC-REIN-AHPRM (SUB1) TO DE-REI-AHPRM.                   
06622      MULTIPLY RC-REIN-LFCLM (SUB1) BY -1 GIVING DE-REI-CLAIM-AMT. 
06623      MOVE CR-DTHAMT     TO DE-CLAIM-AMT.                          
06624      ADD  CR-DTHEXP     TO DE-CLAIM-AMT.                          
06625      MOVE CR-DTH-PAY-DT TO DE-PAY                                 
06626                            WS-DE-PAY-N.                           
06627                                                                   
06628      IF CLAS-I-BAL (CLAS-INDEXL) = 'B'                            
06629          MOVE '3' TO DE-TYPE                                      
06630      ELSE                                                         
06631          MOVE '1' TO DE-TYPE.                                     
06632                                                                   
06633      PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X.   
06634                                                                   
06635      ADD +1 TO BC-R-CLM.                                          
06636                                                                   
06637  3770-RC-REIN-OLD-AH-CLM.                                         
06638                                                                   
06639      IF RC-OLD-AH-CLM (SUB1) = 'X'                                
06640          ADD +1 TO SUB1                                           
06641          GO TO 3760-RC-REIN-OLD-CLAIM.                            
06642                                                                   
06643      IF RC-REIN-AHCLM (SUB1) = ZERO                               
06644          ADD +1 TO SUB1                                           
06645          GO TO 3760-RC-REIN-OLD-CLAIM.                            
06646                                                                   
06647      MOVE SAVE-EXTRACT         TO DETAIL-EXTRACT.                 
06648      PERFORM EXTRACT-DATE-LOAD.                                   
06649      MOVE RC-REIN-COMP (SUB1)  TO DE-REI-COMP.                    
06650      MOVE RC-REIN-LFAMT (SUB1) TO DE-REI-LFAMT.                   
06651      MOVE RC-REIN-LFPRM (SUB1) TO DE-REI-LFPRM.                   
06652      MOVE RC-REIN-AHAMT (SUB1) TO DE-REI-AHAMT.                   
06653      MOVE RC-REIN-AHPRM (SUB1) TO DE-REI-AHPRM.                   
06654      MULTIPLY RC-REIN-AHCLM (SUB1) BY -1 GIVING DE-REI-CLAIM-AMT. 
06655      MOVE CR-DISAMT     TO DE-CLAIM-AMT.                          
06656      ADD  CR-DISEXP     TO DE-CLAIM-AMT.                          
06657      MOVE CR-DIS-PAY-DT TO DE-PAY                                 
06658                            WS-DE-PAY-N.                           
06659                                                                   
06660      IF CLAS-I-BAL (CLAS-INDEXA) = 'B'                            
06661          MOVE '4' TO DE-TYPE                                      
06662      ELSE                                                         
06663          MOVE '2' TO DE-TYPE.                                     
06664                                                                   
06665      PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X.   
06666                                                                   
06667      ADD +1 TO BC-R-CLM.                                          
06668      ADD +1 TO SUB1.                                              
06669                                                                   
06670      GO TO 3760-RC-REIN-OLD-CLAIM.                                
06671                                                                   
06672  3780-RC-REIN-NEW-CLAIM.                                          
06673                                                                   
06674      IF REIN-COMP (SUB1) = SPACES                                 
06675          GO TO 3799-RC-REINSURANCE-X.                             
06676                                                                   
06677      IF REIN-REM-SW (SUB1) = 'Z'                                  
06678          ADD +1 TO SUB1                                           
06679          GO TO 3780-RC-REIN-NEW-CLAIM.                            
06680                                                                   
06681      IF RC-NEW-LF-CLM (SUB1) = 'X'                                
06682          GO TO 3790-RC-REIN-NEW-AH-CLM.                           
06683                                                                   
06684      IF REIN-LFCLM (SUB1) = ZERO                                  
06685          GO TO 3790-RC-REIN-NEW-AH-CLM.                           
06686                                                                   
06687      MOVE SAVE-EXTRACT      TO DETAIL-EXTRACT.                    
06688      PERFORM EXTRACT-DATE-LOAD.                                   
06689      MOVE REIN-COMP (SUB1)  TO DE-REI-COMP.                       
06690      MOVE REIN-LFAMT (SUB1) TO DE-REI-LFAMT.                      
06691      MOVE REIN-LFPRM (SUB1) TO DE-REI-LFPRM.                      
06692      MOVE REIN-AHAMT (SUB1) TO DE-REI-AHAMT.                      
06693      MOVE REIN-AHPRM (SUB1) TO DE-REI-AHPRM.                      
06694      MOVE REIN-LFCLM (SUB1) TO DE-REI-CLAIM-AMT.                  
06695      MOVE CR-DTHAMT         TO DE-CLAIM-AMT.                      
06696      ADD  CR-DTHEXP         TO DE-CLAIM-AMT.                      
06697      MOVE CR-DTH-PAY-DT     TO DE-PAY                             
06698                                WS-DE-PAY-N.                       
06699                                                                   
06700      IF CLAS-I-BAL (CLAS-INDEXL) = 'B'                            
06701          MOVE '3' TO DE-TYPE                                      
06702      ELSE                                                         
06703          MOVE '1' TO DE-TYPE.                                     
06704                                                                   
06705      PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X.   
06706                                                                   
06707      ADD +1 TO BC-R-CLM.                                          
06708                                                                   
06709  3790-RC-REIN-NEW-AH-CLM.                                         
06710                                                                   
06711      IF RC-NEW-AH-CLM (SUB1) = 'X'                                
06712          ADD +1 TO SUB1                                           
06713          GO TO 3780-RC-REIN-NEW-CLAIM.                            
06714                                                                   
06715      IF REIN-AHCLM (SUB1) = ZERO                                  
06716          ADD +1 TO SUB1                                           
06717          GO TO 3780-RC-REIN-NEW-CLAIM.                            
06718                                                                   
06719      MOVE SAVE-EXTRACT      TO DETAIL-EXTRACT.                    
06720      PERFORM EXTRACT-DATE-LOAD.                                   
06721      MOVE REIN-COMP (SUB1)  TO DE-REI-COMP.                       
06722      MOVE REIN-LFAMT (SUB1) TO DE-REI-LFAMT.                      
06723      MOVE REIN-LFPRM (SUB1) TO DE-REI-LFPRM.                      
06724      MOVE REIN-AHAMT (SUB1) TO DE-REI-AHAMT.                      
06725      MOVE REIN-AHPRM (SUB1) TO DE-REI-AHPRM.                      
06726      MOVE REIN-AHCLM (SUB1) TO DE-REI-CLAIM-AMT.                  
06727      MOVE CR-DISAMT         TO DE-CLAIM-AMT.                      
06728      ADD  CR-DISEXP         TO DE-CLAIM-AMT.                      
06729      MOVE CR-DIS-PAY-DT     TO DE-PAY                             
06730                                WS-DE-PAY-N.                       
06731                                                                   
06732      IF CLAS-I-BAL (CLAS-INDEXA) = 'B'                            
06733          MOVE '4' TO DE-TYPE                                      
06734      ELSE                                                         
06735          MOVE '2' TO DE-TYPE.                                     
06736                                                                   
06737      PERFORM 2500-WRITE-DETAIL-EXTRACTS THRU 2599-WRT-DETAIL-X.   
06738                                                                   
06739      ADD +1 TO BC-R-CLM.                                          
06740      ADD +1 TO SUB1.                                              
06741                                                                   
06742      GO TO 3780-RC-REIN-NEW-CLAIM.                                
06743                                                                   
06744  3799-RC-REINSURANCE-X.                                           
06745      EXIT.                                                        
06746  EJECT                                                            
06747 ******************************************************************
06748 ***    E A R N E D   P R E M I U M   C A L C U L A T I O N S   ***
06749 ******************************************************************
06750                                                                   
06751  4000-EP-BUMP-RTN.                                                
06752                                                                   
06753      IF AM-RECALC-COMM = ('Y' OR '1')
06754         PERFORM 1600-RECALC-COMM-EXTR
06755                                  THRU 1899-RECALC-COMM-EXTR-X
060105        MOVE ' '                 TO RECALC-TYPE
060105     END-IF
06756                                                                   
06757      MOVE '1'    TO EP-FLG.                                       
06758      ADD +1      TO BC-G-CERTS.                                   
06759                                                                   
06760      MOVE CR-DT               TO HIGH-CERT.                       
06761      IF CR-ACCT-CONTROL NOT = LOW-CTL                             
06762          MOVE CR-DT           TO LOW-CERT                         
06763          MOVE CR-ACCT-CONTROL TO LOW-CTL.                         
06764                                                                   
06765      IF CR-POLICY-IS-REIN-ONLY                                    
06766          MOVE +0           TO GROSS-ISS-BEN-LF   GROSS-ISS-BEN-AH 
06767                               GROSS-CNC-BEN-LF   GROSS-CNC-BEN-AH 
06768                               GROSS-ISS-PRM-LF   GROSS-ISS-PRM-AH 
06769                               GROSS-CNC-PRM-LF   GROSS-CNC-PRM-AH 
06770                               DTH-IBNR           DIS-IBNR         
06771                               DTH-PAYCUR         DIS-PAYCUR       
06772                               DTH-FUTRSV         DIS-FUTRSV       
06773          GO TO 4050-EP-DO-REINSURE.                               
06774                                                                   
06775      MOVE CR-LFAMT       TO WE-LFAMT.                             
06776      MOVE CR-LFPRM       TO WE-LFPRM.                             
06777      IF CR-LFTYP NOT = ZEROS                                      
06778          IF CLAS-I-EP (CLAS-INDEXL) = 'B'                         
06779              MOVE CR-LFAMT-ALT        TO WE-LFAMT-ALT             
06780              MOVE CR-LFPRM-ALT        TO WE-LFPRM-ALT             
06781          ELSE                                                     
06782              MOVE ZEROS               TO WE-LFAMT-ALT             
06783                                          WE-LFPRM-ALT.            
06784                                                                   
06785      MOVE SPACES         TO REIN-EP-FLG                           
06786                             WE-REIN-CO.                           
06787      MOVE CR-LFRFND      TO WE-LFRFND.                            
06788      MOVE CR-DTHAMT      TO WE-DTHAMT.                            
06789      ADD  CR-DTHEXP      TO WE-DTHAMT.                            
06790      MOVE CR-DTHEXP      TO WE-DTHEXP.                            
06791      MOVE CR-NUM-DTH-CLM TO WE-NUM-DTH-CLM.                       
06792      MOVE DTH-IBNR       TO WE-DTH-IBNR.                          
06793      MOVE DTH-PAYCUR     TO WE-DTH-PAYCUR.                        
06794      MOVE DTH-FUTRSV     TO WE-DTH-FUTRSV.                        
06795                                                                   
06796      MOVE CR-AHPRM       TO WE-AHPRM.                             
06797      MOVE CR-AHAMT       TO WE-AHAMT.                             
06798      MOVE CR-AHRFND      TO WE-AHRFND.                            
06799      MOVE CR-DISAMT      TO WE-DSPY-AMT.                          
06800      ADD  CR-DISEXP      TO WE-DSPY-AMT.                          
06801      MOVE CR-DISEXP      TO WE-DSPY-EXP.                          
06802      MOVE CR-NUM-DIS-CLM TO WE-NUM-DIS-CLM.                       
06803      MOVE DIS-IBNR       TO WE-DIS-IBNR.                          
06804      MOVE DIS-PAYCUR     TO WE-DIS-PAYCUR.                        
06805      MOVE DIS-FUTRSV     TO WE-DIS-FUTRSV.                        
06806                                                                   
06807      IF CR-POLICY-IS-RESTORE  OR                                  
06808         CR-POLICY-IS-REISSUE                                      
122002        OR CR-POLICY-IS-MONTHLY
06809          MOVE ZEROS      TO WE-LFPRM                              
06810                             WE-LFPRM-ALT                          
06811                             WE-AHPRM.                             
06812                                                                   
06813      MOVE +0               TO GROSS-ISS-BEN-LF   GROSS-ISS-BEN-AH 
06814                               GROSS-CNC-BEN-LF   GROSS-CNC-BEN-AH 
06815                               GROSS-ISS-PRM-LF   GROSS-ISS-PRM-AH 
06816                               GROSS-CNC-PRM-LF   GROSS-CNC-PRM-AH 
06817                               DTH-IBNR           DIS-IBNR         
06818                               DTH-PAYCUR         DIS-PAYCUR       
06819                               DTH-FUTRSV         DIS-FUTRSV.      
06820                                                                   
06821      PERFORM 4100-EP-DO-LF THRU 4299-EPBX.                        
06822                                                                   
06823      IF CR-POLICY-IS-DECLINED  OR                                 
06824         CR-POLICY-IS-VOID                                         
06825          GO TO 4099-EP-BUMP-RTN-X.                                
06826                                                                   
06827  4050-EP-DO-REINSURE.                                             
06828                                                                   
06829      MOVE 'R' TO REIN-EP-FLG.                                     
06830      MOVE +1  TO SUB5.                                            
06831                                                                   
06832  4090-EP-DO-REINSURE-LOOP.                                        
06833                                                                   
06834      IF REIN-COMP (SUB5) = SPACES                                 
06835          MOVE SPACE TO REIN-EP-FLG                                
06836          GO TO 4099-EP-BUMP-RTN-X.                                
06837                                                                   
06838      IF REIN-REM-SW (SUB5) = 'Z'  OR                              
06839         REIN-LF-AH-FLGS (SUB5) = SPACES                           
06840          ADD +1 TO SUB5                                           
06841          GO TO 4090-EP-DO-REINSURE-LOOP.                          
06842                                                                   
06843      PERFORM REIN-FIND-CO-LOOP THRU FIND-REI-CO-EXIT              
06844         VARYING CO-SUB FROM 1 BY 1 UNTIL                          
06845           (RCT-REIN-CO (CO-SUB) = REIN-COMP (SUB5))  OR           
06846           (CO-SUB GREATER REIN-CO-TABLE-ENT-CNT)     OR           
06847           (RCT-REIN-CO (CO-SUB) = SPACES).                        
06848                                                                   
06849      IF (CO-SUB GREATER REIN-CO-TABLE-ENT-CNT)  OR                
06850         (RCT-REIN-CO (CO-SUB) = SPACES)                           
06851         MOVE ZERO TO CO-SUB.                                      
06852                                                                   
06853      ADD +1 TO BC-R-CERTS.                                        
06854      MOVE REIN-COMP (SUB5)      TO WE-REIN-CO.                    
06855      MOVE REIN-WORK-FLDS (SUB5) TO RWF-FIELDS.                    
070203     MOVE RE-COMP-INFO (SUB5)   TO WT-COMP-INFO
070203
030603     IF DTE-CLIENT = 'DCC'
030603        COMPUTE WS-CEDE-FACT =
030603           RWF-LFAMT / (CR-LFAMT + CR-LFAMT-ALT)
030603        IF (AM-RECALC-COMM = ('Y' OR '1'))
060105           AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA)
092705                                     NOT = 'G' AND 'L')
030603           COMPUTE RWF-LFPRM = CR-LF-NSP-PRM * WS-CEDE-FACT
030603*          COMPUTE RWF-LFRFND ROUNDED = CR-LF-NSP-PRM
030603*          / (CR-LFPRM + CR-LFPRM-ALT) * CR-LFRFND
030603           COMPUTE RWF-LFRFND ROUNDED = (CR-LFRFND
030603           / (CR-LFPRM + CR-LFPRM-ALT)) * CR-LF-NSP-PRM
030603        END-IF
030603     END-IF

06857      MOVE RWF-LFPRM      TO WE-LFPRM.                             
06858      MOVE RWF-LFAMT      TO WE-LFAMT.                             
06859      MOVE RWF-LFRFND     TO WE-LFRFND.                            
06860      MOVE RWF-LFCLM      TO WE-DTHAMT.                            
06861      MOVE RWF-DTH-IBNR   TO WE-DTH-IBNR.                          
06862      MOVE RWF-DTH-PAYCUR TO WE-DTH-PAYCUR.                        
06863      MOVE RWF-DTH-FUTRSV TO WE-DTH-FUTRSV.                        
06864      MOVE +0             TO WE-LFAMT-ALT                          
06865                             WE-LFPRM-ALT                          
06866                             WE-DTHEXP                             
06867                             WE-NUM-DTH-CLM.                       
06868                                                                   
06869      IF (DTE-CLIENT EQUAL 'NSL') AND                              
06870         (CR-REIN-TABLE EQUAL 'NAR')                               
052203        CONTINUE
06872      ELSE                                                         
06873       IF CLAS-I-EP (CLAS-INDEXL) EQUAL 'B'
06874        IF (CR-LFAMT + CR-LFAMT-ALT) GREATER THAN +0
06875         COMPUTE WS-CEDE-FACT ROUNDED EQUAL
06876             RWF-LFAMT / (CR-LFAMT + CR-LFAMT-ALT)
06877         COMPUTE WE-LFAMT ROUNDED EQUAL
06878                            CR-LFAMT * WS-CEDE-FACT
06879         COMPUTE WE-LFAMT-ALT ROUNDED EQUAL
06880                            CR-LFAMT-ALT * WS-CEDE-FACT
123003        IF (DTE-CLIENT = 'CID')
123003           AND (CR-LF-NSP-PRM > +0)
123003           AND (CR-ENTRY-DATE > 20031130)
123003           AND (WT-LF-QC NOT = '1' AND '3')
123003           COMPUTE WE-LFPRM ROUNDED = (CR-LF-NSP-PRM *
123003              RWF-LFAMT) / (CR-LFAMT + CR-LFAMT-ALT)
123003           MOVE +0            TO WE-LFPRM-ALT
123003        ELSE           
052203          IF (DTE-CLIENT = 'CID')
052203             AND (CR-LF-NSP-PRM > +0)
052203             AND (CR-ENTRY-DATE > 20030430)
052203             AND (WT-LF-QC NOT = '1' AND '3')
052203             COMPUTE WE-LFPRM = CR-LF-NSP-PRM * WS-CEDE-FACT
052203             MOVE +0            TO WE-LFPRM-ALT
052203          ELSE
06881              COMPUTE WE-LFPRM ROUNDED EQUAL
06882                            CR-LFPRM * WS-CEDE-FACT
06883              COMPUTE WE-LFPRM-ALT ROUNDED EQUAL
06884                            CR-LFPRM-ALT * WS-CEDE-FACT
052203          END-IF 
123003        END-IF
123003        IF (DTE-CLIENT = 'CID')
123003           AND (CR-LF-NSP-PRM > +0)
                 AND (CR-LFRFND-CALC > +0)
                 AND (CR-LF-CANCEL-EXIT-DATE > 20031130)
                 AND (WT-LF-QC NOT = '1' AND '4')
                 COMPUTE REIN-FACTOR ROUNDED =  CR-LFRFND-CALC /
                    (CR-LFPRM + CR-LFPRM-ALT)
                 COMPUTE WE-LFRFND =
                    REIN-FACTOR * REIN-CALCED-LIFE
                 COMPUTE WE-LFRFND ROUNDED =  (WE-LFRFND
                    * RWF-LFAMT) / (CR-LFAMT + CR-LFAMT-ALT)
      *          COMPUTE WE-LFRFND = CR-LFRFND-CALC / (CR-LFPRM
      *             + CR-LFPRM-ALT) * CR-LF-NSP-PRM 
      *             * RWF-LFAMT / (CR-LFAMT + CR-LFAMT-ALT)
              ELSE              

                IF (DTE-CLIENT = 'CID')
                 AND (CR-LF-NSP-PRM > +0)
                 AND (CR-LFRFND-CALC > +0)
                 AND (CR-LF-CANCEL-EXIT-DATE > 20030430)
                 AND (WT-LF-QC NOT = '1' AND '4')
052203           COMPUTE WS-WE-LFPRM = CR-LF-NSP-PRM * WS-CEDE-FACT
                 COMPUTE WE-LFRFND = (CR-LFRFND-CALC / (CR-LFPRM
                    + CR-LFPRM-ALT)) * (WS-WE-LFPRM)

                END-IF
              END-IF
06885        ELSE
06886          MOVE +0 TO WS-CEDE-FACT
052203       END-IF
052203      END-IF
052203     END-IF
06887                                                                   
030603     IF DTE-CLIENT = 'DCC'
030603        IF (AM-RECALC-COMM = ('Y' OR '1'))
060105           AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA)
092705                                   NOT = 'G' AND 'L')
030603           COMPUTE WS-CEDE-FACT =
030603               RWF-AHAMT / CR-AHAMT
030603           COMPUTE RWF-AHPRM = CR-AH-NSP-PRM * WS-CEDE-FACT
030603*          COMPUTE RWF-AHRFND ROUNDED = CR-AH-NSP-PRM
030603*          / CR-AHPRM * CR-AHRFND
022406           COMPUTE RWF-AHRFND ROUNDED = (CR-AHRFND
022406           / CR-AHPRM) * CR-AH-NSP-PRM
030603        END-IF
030603     END-IF

06888      MOVE RWF-AHPRM      TO WE-AHPRM.                             
PEMMOD
06889      MOVE RWF-AHAMT      TO WE-AHAMT                              
06890      MOVE RWF-AHRFND     TO WE-AHRFND.                            
06891      MOVE RWF-AHCLM      TO WE-DSPY-AMT.                          
06892      MOVE +0             TO WE-DSPY-EXP                           
06893                             WE-NUM-DIS-CLM.                       
06894      MOVE RWF-DIS-IBNR   TO WE-DIS-IBNR.                          
06895      MOVE RWF-DIS-PAYCUR TO WE-DIS-PAYCUR.                        
06896      MOVE RWF-DIS-FUTRSV TO WE-DIS-FUTRSV.                        
06897                                                                   
06898      IF  RWF-LF-RUNOFF-SW = 'N'                                   
06899              OR                                                   
06900          RWF-AH-RUNOFF-SW = 'N'                                   
06901          PERFORM 8000R-CONVERT-STOP-DATE THRU 8099R-STOP-DATE-X.  
06902                                                                   
06903      IF  RWF-LF-RUNOFF-SW = 'N'                                   
06904          MOVE EARNING-STOP-DATE    TO STOP-LF-DATE                
06905      ELSE                                                         
06906          MOVE 99999999             TO STOP-LF-DATE.               
06907                                                                   
06908      IF  RWF-AH-RUNOFF-SW = 'N'                                   
06909          MOVE EARNING-STOP-DATE    TO STOP-AH-DATE                
06910      ELSE                                                         
06911          MOVE 99999999             TO STOP-AH-DATE.               
06912                                                                   
06913      IF CR-POLICY-IS-RESTORE  OR                                  
06914         CR-POLICY-IS-REISSUE                                      
122002        OR CR-POLICY-IS-MONTHLY
06915          MOVE ZEROS      TO WE-LFPRM                              
06916                             WE-LFPRM-ALT                          
06917                             WE-AHPRM.                             
06918                                                                   
06919      PERFORM 4100-EP-DO-LF THRU 4299-EPBX.                        
06920                                                                   
06921      IF CO-SUB NOT = ZERO                                         
06922        IF RCT-EARNING-START-DT (CO-SUB) NOT = ZERO                
06923          IF CR-DT LESS THAN RCT-EARNING-START-DT (CO-SUB)         
06924              PERFORM S-4100-EP-DO-LF THRU S-4299-EPBX.            
06925                                                                   
06926      ADD +1 TO SUB5.                                              
06927      GO TO 4090-EP-DO-REINSURE-LOOP.                              
06928                                                                   
06929  4099-EP-BUMP-RTN-X.                                              
06930      EXIT.                                                        
06931  EJECT                                                            
06932 ******************************************************************
06933 ***  C A L C U L A T E   L I F E   E A R N E D   P R E M I U M ***
06934 ******************************************************************
06935                                                                   
06936  4100-EP-DO-LF.                                                   
06937                                                                   
06938      MOVE +0                         TO ISS-BEN  EPR-R78          
06939                                         CNC-BEN  EPR-PRO          
06940                                         ISS-PRM  EPR-ST           
PEMMOD                                        ISS-PRMTAX
06941                                         CNC-PRM  ACTIVE-COUNT.    
06942      MOVE SPACE                      TO LIFE-EARNED-SWITCH.       
06943                                                                   
06944      IF CR-LFTYP = ZERO                                           
06945          GO TO 4200-EP-DO-AH.                                     
06946                                                                   
06947      IF REIN-EP-FLG = 'R'                                         
06948          IF REIN-LF-FLG (SUB5) = SPACE                            
06949              GO TO 4200-EP-DO-AH.                                 
06950                                                                   
06951      IF CR-APR NOT NUMERIC                                        
06952          MOVE ZERO TO CR-APR.                                     
06953                                                                   
06954      MOVE LIFE-OVERRIDE-L1       TO WE-LF-AH.                     
06955      MOVE CR-LFTYP               TO WE-BEN-TYPE.                  
06956      PERFORM 4400-EP-FIND-TAB-ENT THRU 4499-EP-FIND-TAB-ENT-X.    
06957                                                                   
06958      IF CR-POLICY-IS-DECLINED  OR                                 
06959         CR-POLICY-IS-VOID                                         
06960          GO TO 4191-CONTINUE.                                     
06961                                                                   
06962      MOVE CR-LF-TERM             TO ORIG-TERM.                    
06963      MOVE CLAS-INDEXL            TO W-NDX.                        
06964                                                                   
06965      IF CR-DT GREATER EW-COV-DTE                                  
06966          MOVE CR-DT              TO EW-COV-DTE.                   
06967                                                                   
06968      IF (NOT CR-POLICY-IS-RESTORE) AND                            
06969         (NOT CR-POLICY-IS-REISSUE)                                
122002        AND (NOT CR-POLICY-IS-MONTHLY)
06970          COMPUTE ISS-BEN = WE-LFAMT + WE-LFAMT-ALT                
06971          IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                  
06972              ADD CR-LIVES TO EW-ISS-CNT                           
06973              COMPUTE EW-ORIG-TERM = EW-ORIG-TERM +                
06974                                     (ORIG-TERM * CR-LIVES)        
06975              COMPUTE EW-AGE = EW-AGE + (CR-AGE * CR-LIVES)        
06976              COMPUTE EW-AGE-BEN = EW-AGE-BEN + (CR-AGE * ISS-BEN) 
06977              COMPUTE EW-TERM-BEN = EW-TERM-BEN +                  
06978                                    (ORIG-TERM * ISS-BEN)          
06979          ELSE                                                     
06980              ADD +1 TO EW-ISS-CNT                                 
06981              COMPUTE EW-ORIG-TERM = EW-ORIG-TERM + ORIG-TERM      
06982              COMPUTE EW-AGE = EW-AGE + CR-AGE                     
06983              COMPUTE EW-AGE-BEN = EW-AGE-BEN + (CR-AGE * ISS-BEN) 
06984              COMPUTE EW-TERM-BEN = EW-TERM-BEN +                  
06985                                    (ORIG-TERM * ISS-BEN).         
06986                                                                   
06987      MOVE RUN-DATE        TO VALUATION-DT.                        
06988      MOVE '2'             TO W-VAL-SOURCE.                        
06989                                                                   
06990      IF REIN-EP-FLG = 'R'                                         
06991          IF REIN-LF-RUNOFF-SW (SUB5) = 'N' AND                    
06992             EARNING-STOP-DATE LESS THAN VALUATION-DT              
06993              MOVE EARNING-STOP-DATE TO VALUATION-DT               
06994              MOVE '10'              TO W-VAL-SOURCE.              
06995                                                                   
06996      PERFORM 8500-REMAINING-TERM-ROUTINE THRU 8599-REM-TERM-X.    
06997                                                                   
06998      IF (CR-LF-CURRENT-STATUS = '1' OR '4') AND                   
06999         (REM-TRM3 GREATER ZERO)                                   
07000         IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                   
07001             COMPUTE EW-INFORCE-CNT = EW-INFORCE-CNT +             
07002                                   (CR-LIVES - CR-SUM-CAN-CNT-ITD) 
07003             COMPUTE EW-REM-TERM = EW-REM-TERM +                   
07004                      (REM-TRM3 * (CR-LIVES - CR-SUM-CAN-CNT-ITD)) 
07005           ELSE                                                    
07006             COMPUTE EW-INFORCE-CNT = EW-INFORCE-CNT + 1           
07007             COMPUTE EW-REM-TERM = EW-REM-TERM + REM-TRM3.         
07008                                                                   
07009      COMPUTE ISS-PRM = WE-LFPRM + WE-LFPRM-ALT.                   
07010                                                                   
07011      IF DTE-CLIENT = 'GIC'                                        
07012          IF REIN-EP-FLG NOT = 'R'  AND                            
07013             CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'  AND             
122002            (CR-ENTRY-STATUS NOT = '5' AND '3' AND 'M')           
07015             MOVE CR-LFPRM-CALC TO ISS-PRM.                        
07016                                                                   
07017      IF  DTE-OPT-RESERVE-METHOD-UNAUTH                            
07018          GO TO 4105-EP-DO-LF-REIN-CHK.                            
07019                                                                   
07020      IF  CR-DT NOT GREATER THAN EOM-DATE (13)                     
07021              AND                                                  
07022          CR-LF-EXPIRE-DATE GREATER THAN EOM-DATE (1)              
07023              AND                                                  
07024          (CR-LF-CANC-DT GREATER THAN EOM-DATE (1)                 
07025                  OR                                               
07026              CR-LF-CANC-DT EQUAL ZEROS)                           
07027              AND                                                  
07028          (CR-LF-CLAIM-EXIT-DATE GREATER THAN EOM-DATE (1)         
07029                  OR                                               
07030              CR-LF-CLAIM-EXIT-DATE EQUAL ZEROS)                   
07031          MOVE 'Y'               TO  EPEC-LIFE-CTR-USED-IND (SUB3) 
07032          PERFORM 4194-SLOT-INTO-EFFECTED-MONTHS THRU 4194-EXIT    
07033                  VARYING                                          
07034              SUBM FROM +1 BY +1                                   
07035                  UNTIL                                            
07036              SUBM GREATER THAN +13.                               
07037                                                                   
07038      MOVE EW-SUB-KEY             TO EPEC-SUB-KEY-OPT (SUB3).      
07039      MOVE EW-REIN-CO             TO EPEC-REIN-CO-OPT (SUB3).      
07040                                                                   
07041  4105-EP-DO-LF-REIN-CHK.                                          
07042                                                                   
07043      IF REIN-EP-FLG = 'R'  AND                                    
07044         REIN-LF-RUNOFF-SW (SUB5) = 'N'                            
07045          NEXT SENTENCE                                            
07046      ELSE                                                         
07047          GO TO 4110-EP-DO-LF-CLM.                                 
07048                                                                   
07049      IF CR-LF-STATUS-AT-CANCEL NOT = ' '                          
07050          IF CR-LF-CANC-DT GREATER THAN EARNING-STOP-DATE          
07051              GO TO 4130-EP-DO-LF-EP.                              
07052                                                                   
07053      IF CR-LF-STATUS-AT-DEATH NOT = ' '                           
07054          IF CR-DTH-DT GREATER THAN EARNING-STOP-DATE              
07055              GO TO 4130-EP-DO-LF-EP.                              
07056                                                                   
07057  4110-EP-DO-LF-CLM.                                               
07058                                                                   
CIDMOD     IF  EPEC-CLM-EXP (SUB3)  NUMERIC                             
CIDMOD         NEXT  SENTENCE                                           
CIDMOD       ELSE                                                       
CIDMOD         MOVE ZEROS TO EPEC-CLM-EXP (SUB3).                       
CIDMOD                                                                  
07059      ADD WE-DTHAMT      TO EW-CLM-AMT.                            
07060      ADD WE-DTHEXP      TO EPEC-CLM-EXP (SUB3).                   
07061                                                                   
07062      IF CR-LF-STATUS-AT-DEATH = ' '                               
07063          IF CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'B'              
07064              GO TO 4115-EP-DO-LF-CLM-EPR.                         
07065                                                                   
07066      ADD WE-NUM-DTH-CLM TO EW-CLM-CNT.                            
07067      ADD +1             TO EW-CLM-CRT.                            
07068                                                                   
07069  4115-EP-DO-LF-CLM-EPR.                                           
07070                                                                   
07071      IF CR-LF-STATUS-AT-DEATH NOT = ' '                           
07072          COMPUTE EPR-R78 = WE-LFPRM + WE-LFPRM-ALT                
07073          MOVE EPR-R78 TO EPR-PRO  EPR-ST                          
PEMMOD     END-IF
07074      .                                                            
07075  4120-EP-DO-LF-CNCL.                                              
07076                                                                   
07077      IF CR-LF-STATUS-AT-CANCEL = ' '  AND                         
07078         CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'Z'                  
07079          IF CR-LF-STATUS-AT-DEATH NOT = ' '                       
07080              GO TO 4190-EP-DO-LF-COMM                             
07081          ELSE                                                     
07082              GO TO 4130-EP-DO-LF-EP.                              
07083                                                                   
07084      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                      
07085          ADD CR-SUM-CAN-CNT-ITD TO EW-CNC-CNT                     
07086          GO TO 4124-EP-DO-LF-CNCL-SUM.                            
07087                                                                   
07088      ADD +1                       TO EW-CNC-CNT.                  
07089                                                                   
07090      MOVE WE-LFRFND               TO CNC-PRM.                     
07091                                                                   
07092      IF DTE-CLIENT = 'GIC'                                        
07093          IF REIN-EP-FLG NOT = 'R'  AND                            
07094             CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'                  
07095              MOVE CR-LFRFND-CALC TO CNC-PRM.                      
07096                                                                   
PEMTMP*    MOVE CR-DT                 TO  DC-GREG-DATE-CYMD.            
PEMTMP*    MOVE 'L'                   TO  DC-OPTION-CODE.               
PEMTMP*    PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
PEMTMP*    MOVE DC-BIN-DATE-1         TO  WS-CR-BIN-DATE.               
07101                                                                   
07102      MOVE CR-LF-CANC-DT         TO  DC-GREG-DATE-CYMD.            
07103      MOVE 'L'                   TO  DC-OPTION-CODE.               
07104      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
07105      MOVE DC-BIN-DATE-1         TO  WS-BIN-CANCEL-DATE.           
07106                                                                   
07107      MOVE WS-CR-BIN-DATE          TO  DC-BIN-DATE-1.              
07108      MOVE WS-BIN-CANCEL-DATE      TO  DC-BIN-DATE-2.              
07109      MOVE '1'                     TO  DC-OPTION-CODE.             
07110      MOVE ZEROS                   TO  MONTHS-DIFF-LF.             
07111      MOVE ' '                     TO  DC-CENTURY-ADJUSTMENT.      
07112      MOVE ZEROS                   TO  DC-ELAPSED-MONTHS           
07113                                       DC-ODD-DAYS-OVER            
07114                                       DC-ELAPSED-DAYS.            
07115      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
07116      MOVE DC-ELAPSED-MONTHS       TO  MONTHS-DIFF-LF.             
07117                                                                   
07118      IF DC-ODD-DAYS-OVER   GREATER THAN  ZERO                     
07119          ADD  +1                  TO  MONTHS-DIFF-LF.             
07120                                                                   
07121      MOVE  'YYYYYYYYYY'           TO  WS-LF-CHARGEBACK-LEVELS.    
07122                                                                   
07123      IF DTE-CLIENT = 'ACE'                                        
07124          GO TO 4120-CONTINUE-LIFE-CANCEL.                         
07125                                                                   
07126      MOVE  ZEROS                  TO  SUB.                        
07127                                                                   
07128  4120-LIFE-CHARGEBACK-LOOP.                                       
07129                                                                   
07130      ADD    +1                    TO  SUB.                        
07131                                                                   
07132      IF  SUB  GREATER THAN  +10                                   
07133          GO TO  4120-CONTINUE-LIFE-CANCEL.                        
07134                                                                   
07135      IF  AM-COMM-CHARGEBACK (SUB)  NOT  NUMERIC  OR               
07136          AM-COMM-CHARGEBACK (SUB)  =  ZEROS                       
07137          GO TO  4120-LIFE-CHARGEBACK-LOOP.                        
07138                                                                   
07139      IF  AM-COMM-CHARGEBACK (SUB)  EQUAL    '99'                  
07140          MOVE  'N'    TO  WS-LF-CHARGEBACK-SW (SUB).              
07141                                                                   
07142      IF  MONTHS-DIFF-LF GREATER THAN AM-COMM-CHARGEBACK (SUB)     
07143          MOVE  'N'        TO  WS-LF-CHARGEBACK-SW (SUB).          
07144                                                                   
07145      GO TO  4120-LIFE-CHARGEBACK-LOOP.                            
07146                                                                   
07147                                                                   
07148  4120-CONTINUE-LIFE-CANCEL.                                       
07149                                                                   
07150      COMPUTE EPR-R78 = (WE-LFPRM + WE-LFPRM-ALT) - WE-LFRFND.     
07151      MOVE EPR-R78 TO EPR-PRO  EPR-ST.                             
07152                                                                   
07153      IF WE-LFRFND = +0 AND WE-DTHAMT NOT = +0                     
07154          GO TO 4190-EP-DO-LF-COMM.                                
07155                                                                   
07156      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'                   
07157          COMPUTE CNC-BEN = WE-LFAMT + WE-LFAMT-ALT                
07158          GO TO 4190-EP-DO-LF-COMM.                                
07159                                                                   
07160      MOVE CR-LF-CANC-DT TO VALUATION-DT.                          
07161      MOVE '3'           TO W-VAL-SOURCE.                          
07162      PERFORM 8500-REMAINING-TERM-ROUTINE THRU 8599-REM-TERM-X.    
07163                                                                   
07164      IF REM-TRM3 = CR-LF-TERM                                     
07165          COMPUTE CNC-BEN = WE-LFAMT + WE-LFAMT-ALT                
07166          GO TO 4190-EP-DO-LF-COMM.                                
07167                                                                   
07168      IF CLAS-I-EP (CLAS-INDEXL) = 'T'                             
07169          GO TO 4122-EP-DO-LF-CNCL-TEX.                            
07170                                                                   
07171      IF DTE-CLIENT = 'NCL'                                        
07172          IF CR-DT LESS THAN 19910101                              
07173              GO TO 4121-EP-DO-LF-CNC-BEN.                         
07174                                                                   
07175      IF CLAS-I-EP (CLAS-INDEXL) = 'N'                             
07176          GO TO 4126-EP-DO-LF-CNCL-NP.                             
07177                                                                   
07178      IF DTE-CLIENT = 'FUL'                                        
07179          GO TO 4128-EP-DO-LF-CNCL-FUL.                            
07180                                                                   
CIDMOD*    IF STATE-ABBR (CLAS-INDEXS) = 'OH'                           
CIDMOD     IF (STATE-ABBR (CLAS-INDEXS) = 'OH') AND                     
CIDMOD        (CR-RATING-CLASS NOT = 'L ')
07182          IF (CR-APR GREATER THAN ZERO)  AND                       
07183             (CR-LF-TERM GREATER THAN 060)  AND                    
07184             (CR-DT GREATER THAN 19831031)                         
07185               GO TO 4126-EP-DO-LF-CNCL-NP.                        
07186                                                                   
07187      IF STATE-ABBR (CLAS-INDEXS) = 'MT'                           
07188          IF (CR-APR GREATER THAN ZERO)  AND                       
07189             (CR-LF-TERM GREATER THAN 061)  AND                    
07190             (CR-DT GREATER THAN 19830318)                         
07191               GO TO 4126-EP-DO-LF-CNCL-NP.                        
07192                                                                   
07193      IF STATE-ABBR (CLAS-INDEXS) = 'UT'                           
07194          IF (CR-APR GREATER THAN ZERO)  AND                       
07195             (CR-LF-TERM GREATER THAN 062)  AND                    
07196             (CR-DT GREATER THAN 19810831)  AND                    
07197             (CR-DT LESS THAN 19830901)                            
07198               GO TO 4126-EP-DO-LF-CNCL-NP.                        
07199                                                                   
07200      IF STATE-ABBR (CLAS-INDEXS) = 'RI'                           
07201          IF (CR-APR GREATER THAN ZERO)  AND                       
07202             (CR-LF-TERM GREATER THAN 060)  AND                    
07203             (CR-DT GREATER THAN 19831231)                         
07204               GO TO 4126-EP-DO-LF-CNCL-NP.                        
07205                                                                   
07206  4121-EP-DO-LF-CNC-BEN.                                           
07207                                                                   
07208      COMPUTE TEMP-4 ROUNDED EQUAL REM-TRM3 / CR-LF-TERM.          
07209                                                                   
07210      COMPUTE TEMP-3 ROUNDED EQUAL                                 
07211          (WE-LFAMT + WE-LFAMT-ALT) * TEMP-4.                      
07212                                                                   
07213      MOVE TEMP-3                 TO CNC-BEN.                      
07214                                                                   
07215      GO TO 4190-EP-DO-LF-COMM.                                    
07216                                                                   
07217  4122-EP-DO-LF-CNCL-TEX.                                          
07218                                                                   
07219      DIVIDE WE-LFAMT BY CR-LF-TERM                                
07220          GIVING TEX-FACT-1.                                       
07221      DIVIDE REM-TRM3 BY CR-PMT-FREQ                               
07222          GIVING TEX-FACT-2                                        
07223              REMAINDER TEX-FACT-3.                                
07224      IF TEX-FACT-3 NOT = ZERO                                     
07225          ADD +1 TO TEX-FACT-2.                                    
07226                                                                   
07227      COMPUTE CNC-BEN = (TEX-FACT-1 * (TEX-FACT-2 * CR-PMT-FREQ)). 
07228                                                                   
07229      GO TO 4190-EP-DO-LF-COMM.                                    
07230                                                                   
07231  4124-EP-DO-LF-CNCL-SUM.                                          
07232                                                                   
07233      IF WE-LFRFND = ZERO                                          
07234          GO TO 4130-EP-DO-LF-EP.                                  
07235                                                                   
07236      MOVE WE-LFRFND TO CNC-PRM.                                   
07237                                                                   
07238      MOVE  'YYYYYYYYYY'           TO  WS-LF-CHARGEBACK-LEVELS.    
07239                                                                   
07240      IF WE-LFRFND NOT LESS WE-LFPRM                               
07241          MOVE WE-LFAMT TO CNC-BEN                                 
07242           GO TO 4130-EP-DO-LF-EP.                                 
07243                                                                   
07244                                                                   
07245      COMPUTE TEMP-7 ROUNDED = WE-LFAMT * (WE-LFRFND / WE-LFPRM).  
07246                                                                   
07247      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'                   
07248          MOVE TEMP-7 TO CNC-BEN                                   
07249           GO TO 4130-EP-DO-LF-EP.                                 
07250                                                                   
07251      MOVE CR-LF-CANC-DT TO VALUATION-DT.                          
07252      MOVE '4'           TO W-VAL-SOURCE.                          
07253      PERFORM 8500-REMAINING-TERM-ROUTINE THRU 8599-REM-TERM-X.    
07254                                                                   
07255      IF CLAS-I-EP (CLAS-INDEXL) NOT = 'N'                         
07256          COMPUTE CNC-BEN = TEMP-7 * (REM-TRM3 / CR-LF-TERM)       
07257          GO TO 4130-EP-DO-LF-EP.                                  
07258                                                                   
07259  4126-EP-DO-LF-CNCL-NP.                                           
07260                                                                   
07261      IF WE-LFAMT = ZERO                                           
07262         GO TO 4190-EP-DO-LF-COMM.                                 
07263                                                                   
07264      MOVE CR-DT                      TO  DC-GREG-DATE-CYMD.       
07265      MOVE 'L'                        TO  DC-OPTION-CODE.          
07266      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
07267      MOVE DC-BIN-DATE-1              TO  CP-CERT-EFF-DT.          
07268                                                                   
07269      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                            
07270        MOVE CR-LOAN-1ST-PMT-DT       TO  DC-GREG-DATE-1-YMD       
07271        MOVE '3'                      TO  DC-OPTION-CODE           
07272        PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X 
07273        MOVE DC-BIN-DATE-1            TO  CP-FIRST-PAY-DATE        
07274      ELSE                                                         
07275        MOVE CP-CERT-EFF-DT           TO  DC-BIN-DATE-1            
07276        MOVE '6'                      TO  DC-OPTION-CODE           
07277        MOVE +1                       TO  DC-ELAPSED-MONTHS        
07278        MOVE +0                       TO  DC-ELAPSED-DAYS          
07279        PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X 
07280        MOVE DC-BIN-DATE-2            TO  CP-FIRST-PAY-DATE.       
07281                                                                   
07282      MOVE CR-LFAMT                   TO  CP-ORIGINAL-BENEFIT.     
07283      MOVE CR-APR                     TO  CP-LOAN-APR.             
07284      MOVE CR-LF-TERM                 TO  CP-ORIGINAL-TERM.        
07285      MOVE CR-LOAN-TERM               TO  CP-LOAN-TERM.            
07286      MOVE REM-TRM3                   TO  CP-REMAINING-TERM.       
07287      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL) TO  CP-SPECIAL-CALC-CD.  
07288      MOVE CLAS-I-RL-AH     (CLAS-INDEXL) TO  CP-BENEFIT-TYPE.     
07289      MOVE CLAS-I-EP        (CLAS-INDEXL) TO  CP-EARNING-METHOD.   
07290                                                                   
07291      CALL 'ELRAMTX' USING CALCULATION-PASS-AREA.                  
07292                                                                   
07293      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                      
07294        COMPUTE CNC-BEN = (CP-REMAMT-FACTOR * (TEMP-7 / +1000))    
07295        GO TO 4130-EP-DO-LF-EP                                     
07296      ELSE                                                         
07297        COMPUTE CNC-BEN = (CP-REMAMT-FACTOR * (WE-LFAMT / +1000)). 
07298                                                                   
07299      GO TO 4190-EP-DO-LF-COMM.                                    
07300                                                                   
07301  4128-EP-DO-LF-CNCL-FUL.                                          
07302                                                                   
07303      IF WE-LFAMT = ZERO                                           
07304         GO TO 4190-EP-DO-LF-COMM.                                 
07305                                                                   
07306      MOVE CR-APR              TO NP-APR.                          
07307      MOVE CR-LF-TERM          TO NP-ORIG.                         
07308      MOVE REM-TRM3            TO NP-REM.                          
07309      MOVE '2'                 TO NP-OPT.                          
07310      MOVE CR-AHPRM            TO NP-AHPRM.                        
07311      MOVE CR-ACCOUNT          TO NP-ACCOUNT.                      
07312      MOVE CR-LFAMT            TO NP-BENEFIT.                      
07313                                                                   
07314 *    CALL 'FULNETRM'                                              
07315 *        USING NP-APR NP-ORIG NP-REM NP-OPT NP-BENEFIT NP-AHPRM   
07316 *              NP-ACCOUNT NP-REMAINING.                           
07317                                                                   
07318      MOVE NP-REMAINING TO CNC-BEN.                                
07319                                                                   
07320      GO TO 4190-EP-DO-LF-COMM.                                    
07321                                                                   
07322  4130-EP-DO-LF-EP.                                                
07323                                                                   
07324      MOVE SPACE                  TO STATUTORY-SWITCH.             
07325                                                                   
07326      MOVE EOM-RUN-DATE TO VALUATION-DT.                           
07327      MOVE '5'           TO W-VAL-SOURCE.                          
07328                                                                   
07329      IF REIN-EP-FLG = 'R'                                         
07330        IF REIN-REM-SW (SUB5) = 'I'                                
07331            COMPUTE EPR-R78 = WE-LFPRM + WE-LFPRM-ALT              
07332            MOVE EPR-R78             TO EPR-PRO  EPR-ST            
07333            GO TO 4190-EP-DO-LF-COMM.                              
07334                                                                   
07335      IF REIN-EP-FLG = 'R'                                         
07336          IF REIN-LF-RUNOFF-SW (SUB5) = 'N'  AND                   
07337             EARNING-STOP-DATE LESS THAN VALUATION-DT              
07338              MOVE '*'               TO LIFE-EARNED-SWITCH         
07339              MOVE EARNING-STOP-DATE TO VALUATION-DT               
07340              MOVE '10'              TO W-VAL-SOURCE.              
07341                                                                   
07342      PERFORM 8500-REMAINING-TERM-ROUTINE THRU 8599-REM-TERM-X.    
07343                                                                   
07344      IF REM-TRM2 GREATER CR-LF-TERM                               
07345          MOVE +0 TO EPR-R78  EPR-PRO  EPR-ST                      
07346          GO TO 4190-EP-DO-LF-COMM.                                
07347                                                                   
07348      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                      
07349          GO TO 4180-EP-DO-LF-EP-SUM.                              
07350                                                                   
07351      IF CLAS-I-EP (CLAS-INDEXL) = 'B'                             
07352          IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'L'                  
07353              IF REM-TRM2 NOT GREATER THAN +0                      
07354                  COMPUTE EPR-R78 = WE-LFPRM + WE-LFPRM-ALT        
07355                  MOVE EPR-R78 TO EPR-PRO  EPR-ST                  
07356                  GO TO 4190-EP-DO-LF-COMM                         
07357              ELSE                                                 
07358                  NEXT SENTENCE                                    
07359          ELSE                                                     
07360              IF W-BAL-REMTRM NOT GREATER +0                       
07361                  COMPUTE EPR-R78 = WE-LFPRM + WE-LFPRM-ALT        
07362                  MOVE EPR-R78 TO EPR-PRO  EPR-ST                  
07363                  GO TO 4190-EP-DO-LF-COMM                         
07364              ELSE                                                 
07365                  NEXT SENTENCE                                    
07366      ELSE                                                         
07367         IF DTE-CLIENT = 'NCL'  AND                                
07368            CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'                   
07369             MOVE WE-LFPRM TO EPR-R78  EPR-PRO  EPR-ST             
07370         ELSE                                                      
07371          IF REM-TRM2 NOT GREATER +0  OR                           
07372             CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'                  
07373              MOVE WE-LFPRM TO EPR-R78  EPR-PRO  EPR-ST            
07374              GO TO 4190-EP-DO-LF-COMM.                            
07375                                                                   
07376      MOVE +1 TO ACTIVE-COUNT.                                     
07377                                                                   
07378      IF CLAS-I-EP (CLAS-INDEXL) = 'T'                             
07379          GO TO 4140-EP-DO-LF-EP-TEX.                              
07380                                                                   
07381      IF CLAS-I-EP (CLAS-INDEXL) = 'N'                             
07382          GO TO 4150-EP-DO-LF-EP-NP.                               
07383                                                                   
CIDMOD*    IF STATE-ABBR (CLAS-INDEXS) = 'OH'                           
CIDMOD     IF (STATE-ABBR (CLAS-INDEXS) = 'OH') AND                     
CIDMOD        (CR-RATING-CLASS NOT = 'L ')
07385          IF (CR-APR GREATER THAN ZERO)  AND                       
07386             (CR-LF-TERM GREATER THAN 060)  AND                    
07387             (CR-DT GREATER THAN 19831031)                         
07388              MOVE '*'             TO STATUTORY-SWITCH             
07389               GO TO 4150-EP-DO-LF-EP-NP.                          
07390                                                                   
07391      IF STATE-ABBR (CLAS-INDEXS) = 'MT'                           
07392          IF (CR-APR GREATER THAN ZERO)  AND                       
07393             (CR-LF-TERM GREATER THAN 061)  AND                    
07394             (CR-DT GREATER THAN 19830318)                         
07395              MOVE '*'             TO STATUTORY-SWITCH             
07396               GO TO 4150-EP-DO-LF-EP-NP.                          
07397                                                                   
07398      IF STATE-ABBR (CLAS-INDEXS) = 'UT'                           
07399          IF (CR-APR GREATER THAN ZERO)  AND                       
07400             (CR-LF-TERM GREATER THAN 062)  AND                    
07401             (CR-DT GREATER THAN 19810831)  AND                    
07402             (CR-DT LESS THAN 19830901)                            
07403              MOVE '*'             TO STATUTORY-SWITCH             
07404               GO TO 4150-EP-DO-LF-EP-NP.                          
07405                                                                   
07406      IF STATE-ABBR (CLAS-INDEXS) = 'RI'                           
07407          IF (CR-APR GREATER THAN ZERO)  AND                       
07408             (CR-LF-TERM GREATER THAN 060)  AND                    
07409             (CR-DT GREATER THAN 19831231)                         
07410              MOVE '*'             TO STATUTORY-SWITCH             
07411               GO TO 4150-EP-DO-LF-EP-NP.                          
07412                                                                   
07413      GO TO 4160-EP-DO-LF-EP-R78.                                  
07414                                                                   
07415  4140-EP-DO-LF-EP-TEX.                                            
07416                                                                   
CIDMOD     IF CR-PMT-FREQ = 0                                           
CIDMOD       DISPLAY '*************************************'            
CIDMOD       DISPLAY '***** CR-PMT-FREQ EQUAL TO ZERO *****'            
CIDMOD       DISPLAY '*************************************'            
CIDMOD       DISPLAY '***** CR-FULL-CONTROL -->' CR-FULL-CONTROL        
CIDMOD       DISPLAY '*************************************'            
CIDMOD       COMPUTE TEX-FACT-4 =                                       
CIDMOD          (CR-LF-TERM * CR-LF-TERM) +                             
CIDMOD                               (WS-CR-PMT-FREQ * CR-LF-TERM)      
CIDMOD     ELSE                                                         
              COMPUTE TEX-FACT-4 =                                      
07418          (CR-LF-TERM * CR-LF-TERM) + (CR-PMT-FREQ * CR-LF-TERM).  
CIDMOD
CIDMOD     IF CR-PMT-FREQ = 0                                           
CIDMOD       DIVIDE REM-TRM1 BY WS-CR-PMT-FREQ                          
CIDMOD           GIVING TEX-FACT-5                                      
CIDMOD               REMAINDER TEX-FACT-6                               
CIDMOD     ELSE                                                         
              DIVIDE REM-TRM1 BY CR-PMT-FREQ                            
                  GIVING TEX-FACT-5                                     
                      REMAINDER TEX-FACT-6.                             

CIDMOD     IF CR-PMT-FREQ > 0                                           
CIDMOD        MOVE CR-PMT-FREQ TO WS-CR-PMT-FREQ.                       

07422      COMPUTE TEX-FACT-5 = TEX-FACT-5 * CR-PMT-FREQ.               
07423      COMPUTE TEX-FACT-7 = (TEX-FACT-5 * TEX-FACT-5) +             
07424          (TEX-FACT-5 * CR-PMT-FREQ) +                             
07425          (2 * (TEX-FACT-6 * (TEX-FACT-5 + CR-PMT-FREQ))).         
07426      COMPUTE TEX-FACT-8 ROUNDED = TEX-FACT-7 / TEX-FACT-4.        
07427                                                                   
07428      COMPUTE EPR-R78 ROUNDED = WE-LFPRM - (WE-LFPRM * TEX-FACT-8).
07429                                                                   
07430      GO TO 4170-EP-DO-LF-EP-PRO.                                  
07431                                                                   
07432  4150-EP-DO-LF-EP-NP.                                             
07433                                                                   
07434      IF WE-LFPRM = ZERO                                           
07435          GO TO 4160-EP-DO-LF-EP-R78.                              
07436                                                                   
07437      IF DTE-CLIENT = 'CRI'                                        
07438        IF STATE-ABBR (CLAS-INDEXS) = ('AB' OR 'BC' OR 'MB' OR     
07439              'NS' OR 'ON' OR 'NB' OR 'NF' OR 'QB' OR 'SK' OR 'PE')
07440            GO TO 4160-EP-DO-LF-EP-R78.                            
07441                                                                   
07442      IF DTE-CLIENT = 'NCL'                                        
07443        IF CR-DT LESS THAN 19910101                                
07444            GO TO 4160-EP-DO-LF-EP-R78.                            
07445                                                                   
07446      IF REM-TRM1 = CR-LF-TERM                                     
07447          MOVE +0 TO EPR-R78  EPR-PRO  EPR-ST                      
07448          GO TO 4190-EP-DO-LF-COMM.                                
07449                                                                   
07450      MOVE CR-APR TO NP-APR.                                       
07451      MOVE CR-LF-TERM TO NP-ORIG  NP-CAP.                          
07452      MOVE REM-TRM1 TO NP-REM.                                     
07453      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL) TO NP-OPT.               
07454      IF NP-OPT = 'T' OR 'U' OR 'V' OR 'W' OR 'X'                  
07455          MOVE CR-LOAN-TERM TO NP-ORIG.                            
042904
042904     MOVE 'R' TO NP-OPT.                                          
07457      CALL 'ECSNETRM'                                              
07458          USING NP-APR NP-ORIG NP-REM NP-OPT NP-CAP NP-FACTOR.     
07459                                                                   
07460      COMPUTE EPR-R78 ROUNDED = WE-LFPRM - (NP-FACTOR * WE-LFPRM). 
07461                                                                   
07462      IF STATUTORY-REQUIREMENT                                     
07463          MOVE EPR-R78                TO EPR-ST.                   
07464                                                                   
07465  4160-EP-DO-LF-EP-R78.                                            
07466                                                                   
07467      IF (CLAS-I-CALC-TYPE (CLAS-INDEXL) EQUAL 'B') AND            
07468         (DTE-CLIENT EQUAL 'NCL')                                  
07469         IF (CR-CCYY EQUAL EOM-CCYY) AND                           
07470            (CR-MO EQUAL EOM-MO)                                   
07471            MOVE '*'            TO OB-DATE-1-SWITCH                
07472            MOVE EW-REIN-CO     TO WS-OB-REIN-CO (SUB3)            
07473            MOVE LIFE-OVERRIDE-L1 TO WS-OB-LF-AH (SUB3)            
07474            MOVE CR-LFTYP       TO WS-OB-BENEFIT-CODE (SUB3)       
07475            COMPUTE WS-EPR-R78 (SUB3 1) =                          
07476                    WS-EPR-R78 (SUB3 1) + (WE-LFPRM * .5)          
07477            GO TO 4190-EP-DO-LF-COMM                               
07478         ELSE                                                      
07479         IF (CR-CCYY EQUAL OB-CCYY (1)) AND                        
07480            (CR-MO EQUAL OB-MO (1))                                
07481            MOVE '*'            TO OB-DATE-2-SWITCH                
07482            MOVE EW-REIN-CO     TO WS-OB-REIN-CO (SUB3)            
07483            MOVE LIFE-OVERRIDE-L1 TO WS-OB-LF-AH (SUB3)            
07484            MOVE CR-LFTYP       TO WS-OB-BENEFIT-CODE (SUB3)       
07485            COMPUTE WS-EPR-R78 (SUB3 2) =                          
07486                    WS-EPR-R78 (SUB3 2) + (WE-LFPRM * .5)          
07487            GO TO 4190-EP-DO-LF-COMM                               
07488         ELSE                                                      
07489         IF (CR-CCYY EQUAL OB-CCYY (2)) AND                        
07490            (CR-MO EQUAL OB-MO (2))                                
07491            MOVE '*'            TO OB-DATE-3-SWITCH                
07492            MOVE EW-REIN-CO     TO WS-OB-REIN-CO (SUB3)            
07493            MOVE LIFE-OVERRIDE-L1 TO WS-OB-LF-AH (SUB3)            
07494            MOVE CR-LFTYP       TO WS-OB-BENEFIT-CODE (SUB3)       
07495            COMPUTE WS-EPR-R78 (SUB3 3) =                          
07496                    WS-EPR-R78 (SUB3 3) + (WE-LFPRM * .5)          
07497            GO TO 4190-EP-DO-LF-COMM                               
07498         ELSE                                                      
07499            GO TO 4190-EP-DO-LF-COMM.                              
07500                                                                   
07501      COMPUTE TEMP-1 = CR-LF-TERM * (CR-LF-TERM + 1).              
07502                                                                   
07503      IF DTE-R78 = '1'                                             
07504          COMPUTE TEMP-2 = REM-TRM1 * REM-TRM1                     
07505      ELSE                                                         
07506          COMPUTE TEMP-2 = REM-TRM1 * (REM-TRM1 + 1).              
07507                                                                   
07508      COMPUTE TEMP-4 ROUNDED EQUAL TEMP-2 / TEMP-1.                
07509                                                                   
07510      IF REM-TRM1 GREATER THAN +0                                  
07511          COMPUTE TEMP-7 ROUNDED EQUAL                             
07512              WE-LFPRM - (WE-LFPRM * TEMP-4)                       
07513      ELSE                                                         
07514          MOVE WE-LFPRM                TO TEMP-7.                  
07515                                                                   
07516      IF CLAS-I-EP (CLAS-INDEXL) NOT = 'B'                         
07517          MOVE ZEROS                  TO TEMP-8                    
07518          GO TO 4165-END-LF-EP-R78.                                
07519                                                                   
07520      IF EPR-ST = ZEROS                                            
07521          MOVE TEMP-7                 TO EPR-ST.                   
07522                                                                   
07523      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                  
07524          COMPUTE TEMP-1 = (CR-LF-TERM + 1) * (CR-LF-TERM + 2)     
07525          COMPUTE TEMP-2 = (REM-TRM1 + 1) * (REM-TRM1 + 2)         
07526          IF DTE-R78 = '1'                                         
07527              COMPUTE TEMP-2 = (REM-TRM1 + 1) * (REM-TRM1 + 1).    
07528                                                                   
07529      COMPUTE TEMP-4 ROUNDED EQUAL TEMP-2 / TEMP-1.                
07530                                                                   
07531      COMPUTE TEMP-8 ROUNDED =                                     
07532                WE-LFPRM-ALT - (WE-LFPRM-ALT * TEMP-4).            
07533                                                                   
07534  4165-END-LF-EP-R78.                                              
07535                                                                   
07536      IF EPR-R78 = ZEROS                                           
07537          COMPUTE EPR-R78 = TEMP-7 + TEMP-8                        
07538      ELSE                                                         
07539          COMPUTE EPR-R78 = EPR-R78 + TEMP-8.                      
07540                                                                   
07541  4170-EP-DO-LF-EP-PRO.                                            
07542                                                                   
07543      IF DTE-CLIENT = 'MIC' OR 'MCC'                               
07544          COMPUTE TEMP-5 ROUNDED = (REM-TRM1 - .5) / CR-LF-TERM    
07545      ELSE                                                         
07546          COMPUTE TEMP-5 ROUNDED = REM-TRM1 / CR-LF-TERM.          
07547                                                                   
07548      IF REM-TRM1 GREATER THAN +0                                  
07549          COMPUTE TEMP-7 ROUNDED = WE-LFPRM - (WE-LFPRM * TEMP-5)  
07550      ELSE                                                         
07551          MOVE WE-LFPRM                  TO TEMP-7.                
07552                                                                   
07553      IF CLAS-I-EP (CLAS-INDEXL) NOT = 'B'                         
07554          MOVE ZEROS                     TO TEMP-8                 
07555          GO TO 4175-END-LF-EP-PRO.                                
07556                                                                   
07557      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                  
07558          IF DTE-CLIENT = 'MIC' OR 'MCC'                           
07559              COMPUTE TEMP-5 ROUNDED =                             
07560                            (REM-TRM1 + .5) / (CR-LF-TERM + 1)     
07561          ELSE                                                     
07562              COMPUTE TEMP-5 ROUNDED =                             
07563                            (REM-TRM1 + 1) / (CR-LF-TERM + 1).     
07564                                                                   
07565      COMPUTE TEMP-8 ROUNDED =                                     
07566                    WE-LFPRM-ALT - (WE-LFPRM-ALT * TEMP-5).        
07567                                                                   
07568      COMPUTE EPR-ST = EPR-ST + TEMP-8.                            
07569                                                                   
07570  4175-END-LF-EP-PRO.                                              
07571                                                                   
07572      COMPUTE EPR-PRO = TEMP-7 + TEMP-8.                           
07573                                                                   
07574      GO TO 4185-EP-DO-LF-STATUTORY.                               
07575                                                                   
07576  4180-EP-DO-LF-EP-SUM.                                            
07577                                                                   
07578      IF REM-TRM1 NOT GREATER +0                                   
07579          COMPUTE EPR-R78 = (WE-LFPRM + WE-LFPRM-ALT) - WE-LFRFND  
07580          MOVE EPR-R78 TO EPR-PRO  EPR-ST                          
07581          GO TO 4190-EP-DO-LF-COMM.                                
07582                                                                   
07583      COMPUTE ACTIVE-COUNT = CR-LIVES - CR-SUM-CAN-CNT-ITD.        
07584                                                                   
07585      IF WE-LFPRM = ZERO  AND  WE-LFPRM-ALT = ZERO                 
07586          MOVE ZEROS                 TO TEMP-7                     
07587      ELSE                                                         
07588          COMPUTE TEMP-7 ROUNDED = WE-LFPRM - (WE-LFRFND *         
07589                        (WE-LFPRM / (WE-LFPRM + WE-LFPRM-ALT))).   
07590                                                                   
07591      IF TEMP-7 NOT GREATER ZERO                                   
07592          IF CLAS-I-EP (CLAS-INDEXL) NOT = 'B'                     
07593              MOVE ZEROS             TO EPR-R78  EPR-PRO  EPR-ST   
07594              GO TO 4190-EP-DO-LF-COMM                             
07595          ELSE                                                     
07596              MOVE ZEROS             TO TEMP-7.                    
07597                                                                   
07598      COMPUTE TEMP-1 = CR-LF-TERM * (CR-LF-TERM + 1).              
07599      COMPUTE TEMP-2 = REM-TRM1 * (REM-TRM1 + 1).                  
07600      IF DTE-R78 = '1'                                             
07601          COMPUTE TEMP-2 = REM-TRM1 * REM-TRM1.                    
07602                                                                   
07603      COMPUTE TEMP-4 ROUNDED = TEMP-2 / TEMP-1.                    
07604      COMPUTE TEMP-5 ROUNDED = REM-TRM1 / CR-LF-TERM.              
07605                                                                   
07606      COMPUTE EPR-R78 ROUNDED = TEMP-7 - (TEMP-7 * TEMP-4).        
07607                                                                   
07608      COMPUTE EPR-PRO ROUNDED = TEMP-7 - (TEMP-7 * TEMP-5).        
07609                                                                   
07610      IF CLAS-I-EP (CLAS-INDEXL) NOT = 'B'                         
07611          GO TO 4185-EP-DO-LF-STATUTORY.                           
07612                                                                   
07613      MOVE EPR-R78                  TO EPR-ST.                     
07614                                                                   
07615      IF WE-LFPRM = ZERO  AND  WE-LFPRM-ALT = ZERO                 
07616          MOVE ZEROS                 TO TEMP-8                     
07617      ELSE                                                         
07618          COMPUTE TEMP-8 ROUNDED = WE-LFPRM-ALT - (WE-LFRFND *     
07619                      (WE-LFPRM-ALT / (WE-LFPRM + WE-LFPRM-ALT))). 
07620                                                                   
07621      IF TEMP-8 NOT GREATER ZERO                                   
07622          GO TO 4185-EP-DO-LF-STATUTORY.                           
07623                                                                   
07624      COMPUTE EPR-R78 ROUNDED = EPR-R78 +                          
07625                               (TEMP-8 - (TEMP-8 * TEMP-4)).       
07626                                                                   
07627      COMPUTE EPR-PRO ROUNDED = EPR-PRO +                          
07628                               (TEMP-8 - (TEMP-8 * TEMP-5)).       
07629                                                                   
07630      COMPUTE EPR-ST ROUNDED = EPR-ST +                            
07631                              (TEMP-8 - (TEMP-8 * TEMP-5)).        
07632                                                                   
07633  4185-EP-DO-LF-STATUTORY.                                         
07634                                                                   
07635      IF STATE-ABBR (CLAS-INDEXS) = 'AL'                           
07636          MOVE EPR-R78                TO EPR-ST.                   
07637      IF STATE-ABBR (CLAS-INDEXS) = 'WY'                           
07638          MOVE EPR-PRO                TO EPR-ST.                   
07639                                                                   
07640      IF EPR-ST NOT = ZEROS                                        
07641          GO TO 4190-EP-DO-LF-COMM.                                
07642                                                                   
07643      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'R'                          
07644          MOVE EPR-R78                TO EPR-ST                    
07645      ELSE                                                         
07646          MOVE EPR-PRO                TO EPR-ST.                   
07647                                                                   
07648  4190-EP-DO-LF-COMM.                                              
07649                                                                   
07650      MOVE +0 TO SUB1.                                             
07651      PERFORM 4300-EP-COMM-LOOP THRU 4350-EP-COMM-LOOP-X           
07652          10 TIMES.                                                
07653                                                                   
07654      IF REIN-EP-FLG NOT = 'R'  OR                                 
07655         CR-POLICY-IS-REIN-ONLY                                    
07656          MOVE ISS-BEN TO GROSS-ISS-BEN-LF                         
07657          MOVE CNC-BEN TO GROSS-CNC-BEN-LF                         
07658          MOVE ISS-PRM TO GROSS-ISS-PRM-LF                         
07659          MOVE CNC-PRM TO GROSS-CNC-PRM-LF.                        
07660                                                                   
07661      ADD ISS-BEN TO EW-ISS-BEN.                                   
07662      ADD GROSS-ISS-BEN-LF TO EW-ISS-BEN-GROSS.                    
07663      ADD CNC-BEN TO EW-CNC-BEN.                                   
07664      ADD GROSS-CNC-BEN-LF TO EW-CNC-BEN-GROSS.                    
07665      ADD ISS-PRM TO EW-ISS-PRM.                                   
07666      ADD GROSS-ISS-PRM-LF TO EW-ISS-PRM-GROSS.                    
07667      ADD CNC-PRM TO EW-CNC-PRM.                                   
07668      ADD GROSS-CNC-PRM-LF TO EW-CNC-PRM-GROSS.                    
07669                                                                   
07670      COMPUTE EW-EPR-R78 = EW-EPR-R78 + EPR-R78.                   
07671      COMPUTE EW-EPR-PRO = EW-EPR-PRO + EPR-PRO.                   
07672      COMPUTE EW-EPR-ST  = EW-EPR-ST  + EPR-ST.                    
07673                                                                   
PEMMOD     IF CR-LF-ISS-PREM-TAX NOT NUMERIC
PEMMOD        MOVE +0                  TO CR-LF-ISS-PREM-TAX
PEMMOD     END-IF
PEMMOD     IF CR-LF-CNC-PREM-TAX NOT NUMERIC
PEMMOD        MOVE +0                  TO CR-LF-CNC-PREM-TAX
PEMMOD     END-IF
PEMMOD
PEMMOD     COMPUTE ISS-PRMTAX =
PEMMOD        (ISS-PRM * CR-LF-ISS-PREM-TAX) -
PEMMOD        (CNC-PRM * CR-LF-CNC-PREM-TAX)
PEMMOD
PEMMOD     COMPUTE EW-ISS-PRM-TAX = EW-ISS-PRM-TAX + ISS-PRMTAX
PEMMOD
07674      IF LIFE-FULLY-EARNED                                         
07675          COMPUTE EW-EPR-R78-ADJ = EW-EPR-R78-ADJ +                
07676                                             (ISS-PRM - EPR-R78)   
07677          COMPUTE EW-EPR-PRO-ADJ = EW-EPR-PRO-ADJ +                
07678                                             (ISS-PRM - EPR-PRO)   
07679          COMPUTE EW-EPR-ST-ADJ  = EW-EPR-ST-ADJ +                 
07680                                             (ISS-PRM - EPR-ST).   
07681                                                                   
07682      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
07683          IF  REIN-EP-FLG NOT = 'R'                                
07684                  OR                                               
07685              CR-POLICY-IS-REIN-ONLY                               
07686              MOVE 'L'      TO W-CLAIM-TYPE                        
07687              MOVE EPR-R78  TO W-OPT-R78                           
07688              MOVE EPR-PRO  TO W-OPT-PRO                           
07689              MOVE EPR-ST   TO W-OPT-ST                            
07690              PERFORM 4600-GET-OPT-IBNR THRU 4600-EXIT             
07691              MOVE ZEROS    TO EW-CLM-IBNR                         
07692          ELSE                                                     
07693              IF  STOP-LF-DATE LESS THAN EOM-RUN-DATE              
07694                  MOVE 'N'        TO EPEC-LINE-NOT-USED-IND (SUB3) 
07695                  MOVE ZEROS      TO EW-CLM-IBNR                   
07696              ELSE                                                 
07697                  MOVE ZEROS      TO EW-CLM-IBNR                   
07698      ELSE                                                         
07699          IF  DTE-CLIENT NOT EQUAL 'ITG'                           
07700              ADD WE-DTH-IBNR                                      
07701                        TO EW-CLM-IBNR                             
07702          ELSE                                                     
07703              IF  CLAS-I-BAL (CLAS-INDEXL) = 'B'                   
07704                      AND                                          
07705                  CR-DT GREATER THAN LAST-YRS-DATE                 
07706                  COMPUTE  EW-CLM-IBNR                             
07707                      = EW-CLM-IBNR + (WE-LFAMT * .0000500).       
07708                                                                   
07709      ADD WE-DTH-PAYCUR TO EW-CLM-PAYCUR.                          
07710      ADD WE-DTH-FUTRSV TO EW-CLM-FUTRSV.                          
07711                                                                   
07712  4191-CONTINUE.                                                   
07713                                                                   
07714      MOVE 'Y'          TO EW-LEVEL-USED.                          
07715                                                                   
07716      IF CR-POLICY-UNDERWRITTEN                                    
07717          MOVE EPEC-WORK       TO EPEC-LEVELS-II (SUB3)            
07718          MOVE EPEC-OPT-WORK-TABLE                                 
07719                               TO EPEC-UNDW-OPT-DATA-TABLE         
07720      ELSE                                                         
07721          MOVE EPEC-WORK       TO EPEC-LEVELS (SUB3)               
07722          MOVE EPEC-OPT-WORK-TABLE                                 
07723                               TO EPEC-OPT-DATA-TABLE.             
07724                                                                   
07725      GO TO 4200-EP-DO-AH.                                         
07726  EJECT                                                            
07727  4194-SLOT-INTO-EFFECTED-MONTHS.                                  
07728                                                                   
07729      IF  CR-DT NOT GREATER THAN EOM-DATE (SUBM)                   
07730              AND                                                  
07731          CR-LF-EXPIRE-DATE GREATER THAN EOM-DATE (SUBM)           
07732              AND                                                  
07733          (CR-LF-CANC-DT GREATER THAN EOM-DATE (SUBM)              
07734                  OR                                               
07735           CR-LF-CANC-DT EQUAL ZEROS)                              
07736              AND                                                  
07737          (CR-LF-CLAIM-EXIT-DATE GREATER THAN EOM-DATE (SUBM)      
07738                  OR                                               
07739           CR-LF-CLAIM-EXIT-DATE EQUAL ZEROS)                      
07740          MOVE 'Y'                 TO W-TABLE-USED-SW              
07741          IF  CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                 
07742              COMPUTE EPEC-LIFE-CTR (SUB3 SUBM) ROUNDED            
07743                  = EPEC-LIFE-CTR (SUB3 SUBM)                      
07744                  + CR-LIVES                                       
07745          ELSE                                                     
07746              COMPUTE EPEC-LIFE-CTR (SUB3 SUBM)                    
07747                  = EPEC-LIFE-CTR (SUB3 SUBM) + 1.                 
07748                                                                   
07749  4194-EXIT.                                                       
07750      EXIT.                                                        
07751  EJECT                                                            
07752 ******************************************************************
07753 ***   C A L C U L A T E   A & H   E A R N E D   P R E M I U M  ***
07754 ******************************************************************
07755                                                                   
07756  4200-EP-DO-AH.                                                   
07757                                                                   
07758      MOVE +0                         TO ISS-BEN  EPR-R78          
07759                                         CNC-BEN  EPR-PRO          
07760                                         ISS-PRM  EPR-ST           
07761                                         CNC-PRM                   
07762                                         WX-CLM-AMT                
07763                                         WX-CLM-EXP                
07764                                         WX-CLM-CNT                
07765                                         WX-CLM-CRT                
07766                                         WX-ACTIVE-COUNT           
PEMMOD                                        ISS-PRMTAX
07767                                         WX-CNC-CNT.               
07768      MOVE SPACE                      TO A-H-EARNED-SWITCH.        
07769                                                                   
07770      IF CR-AHTYP = ZERO                                           
07771          GO TO 4295-EP-DO-COUNT.                                  
07772                                                                   
07773      IF REIN-EP-FLG = 'R'                                         
07774          IF REIN-AH-FLG (SUB5) = SPACE                            
07775              GO TO 4295-EP-DO-COUNT.                              
07776                                                                   
07777      MOVE AH-OVERRIDE-L1       TO WE-LF-AH.                       
07778      MOVE CR-AHTYP             TO WE-BEN-TYPE.                    
07779      PERFORM 4400-EP-FIND-TAB-ENT THRU 4499-EP-FIND-TAB-ENT-X.    
07780                                                                   
07781      IF CR-POLICY-IS-DECLINED  OR                                 
07782         CR-POLICY-IS-VOID                                         
07783          GO TO 4290-CONTINUE.                                     
07784                                                                   
07785      MOVE CR-AH-TERM           TO ORIG-TERM.                      
07786                                                                   
07787      IF CR-DT GREATER EW-COV-DTE                                  
07788          MOVE CR-DT            TO EW-COV-DTE.                     
07789                                                                   
07790      IF NOT CR-POLICY-IS-RESTORE  AND                             
07791         NOT CR-POLICY-IS-REISSUE                                  
122002        AND NOT CR-POLICY-IS-MONTHLY
07792          MOVE WE-AHAMT         TO ISS-BEN                         
07793          COMPUTE EW-ORIG-TERM = EW-ORIG-TERM + ORIG-TERM          
07794          COMPUTE EW-AGE = EW-AGE + CR-AGE                         
07795          COMPUTE EW-AGE-BEN = EW-AGE-BEN + (CR-AGE * ISS-BEN)     
07796          COMPUTE EW-TERM-BEN = EW-TERM-BEN +                      
07797                                    (ORIG-TERM * ISS-BEN).         
07798                                                                   
07799      MOVE CLAS-INDEXA     TO W-NDX.                               
07800      MOVE RUN-DATE        TO VALUATION-DT                         
07801      MOVE '6'             TO W-VAL-SOURCE.                        
07802                                                                   
07803      IF REIN-EP-FLG = 'R'                                         
07804          IF REIN-AH-RUNOFF-SW (SUB5) = 'N'  AND                   
07805             EARNING-STOP-DATE LESS THAN VALUATION-DT              
07806              MOVE EARNING-STOP-DATE TO VALUATION-DT               
07807              MOVE '10'              TO W-VAL-SOURCE.              
07808                                                                   
07809      PERFORM 8500-REMAINING-TERM-ROUTINE THRU 8599-REM-TERM-X.    
07810                                                                   
07811      IF (CR-AH-CURRENT-STATUS = '1' OR '4') AND                   
07812         (REM-TRM3 GREATER ZERO)                                   
07813         IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'Z'                   
07814             COMPUTE EW-INFORCE-CNT = EW-INFORCE-CNT +             
07815                                   (CR-LIVES - CR-SUM-CAN-CNT-ITD) 
07816             COMPUTE EW-REM-TERM = EW-REM-TERM +                   
07817                      (REM-TRM3 * (CR-LIVES - CR-SUM-CAN-CNT-ITD)) 
07818           ELSE                                                    
07819             COMPUTE EW-INFORCE-CNT = EW-INFORCE-CNT + 1           
07820             COMPUTE EW-REM-TERM = EW-REM-TERM + REM-TRM3.         
07821                                                                   
07822      IF  DTE-OPT-RESERVE-METHOD-UNAUTH                            
07823          GO TO 4210-BY-OPT-LOGIC.                                 
07824                                                                   
07825      IF  CR-DT NOT GREATER THAN EOM-DATE (13)                     
07826              AND                                                  
07827          CR-AH-EXPIRE-DATE GREATER THAN EOM-DATE (1)              
07828              AND                                                  
07829          (CR-AH-CANC-DT GREATER THAN EOM-DATE (1)                 
07830                  OR                                               
07831           CR-AH-CANC-DT EQUAL ZEROS)                              
07832          MOVE 'Y'                TO EPEC-LIFE-CTR-USED-IND (SUB3) 
07833          PERFORM 4296-SLOT-INTO-EFFECTED-MONTHS THRU 4296-EXIT    
07834                  VARYING                                          
07835              SUBM FROM +1 BY +1                                   
07836                  UNTIL                                            
07837              SUBM GREATER THAN +13.                               
07838                                                                   
07839      MOVE EW-SUB-KEY             TO EPEC-SUB-KEY-OPT (SUB3).      
07840      MOVE EW-REIN-CO             TO EPEC-REIN-CO-OPT (SUB3).      
07841                                                                   
07842  4210-BY-OPT-LOGIC.                                               
07843                                                                   
07844      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'Z'                      
07845          ADD CR-LIVES TO EW-ISS-CNT                               
07846          MOVE WE-AHPRM TO ISS-PRM                                 
07847          COMPUTE EW-ORIG-TERM = EW-ORIG-TERM +                    
07848                                 (ORIG-TERM * CR-LIVES)            
07849          COMPUTE EW-AGE = EW-AGE + (CR-AGE * CR-LIVES)            
07850          COMPUTE EW-AGE-BEN = EW-AGE-BEN + (CR-AGE * ISS-BEN)     
07851          COMPUTE EW-TERM-BEN = EW-TERM-BEN +                      
07852                                    (ORIG-TERM * ISS-BEN)          
07853          PERFORM 4825-EP-DO-AH-CNCL-SUM THRU 4899-EXIT            
07854          GO TO 4225-CONTINUE.                                     
07855                                                                   
07856      IF (NOT CR-POLICY-IS-RESTORE) AND                            
07857         (NOT CR-POLICY-IS-REISSUE)                                
122002        AND (NOT CR-POLICY-IS-MONTHLY)
07858         ADD +1 TO EW-ISS-CNT.                                     
07859                                                                   
07860      PERFORM 4800-CALC-EARNED-PREMIUM-AH THRU 4899-EXIT.          
07861                                                                   
07862  4225-CONTINUE.                                                   
CIDMOD                                                                  
CIDMOD     IF  EPEC-CLM-EXP (SUB3)  NUMERIC                             
CIDMOD         NEXT  SENTENCE                                           
CIDMOD       ELSE                                                       
CIDMOD         MOVE ZEROS TO EPEC-CLM-EXP (SUB3).                       
07863                                                                   
07864      ADD WX-CLM-AMT              TO EW-CLM-AMT.                   
07865      ADD WX-CLM-EXP              TO EPEC-CLM-EXP (SUB3).          
07866      ADD WX-CLM-CNT              TO EW-CLM-CNT.                   
07867      ADD WX-CLM-CRT              TO EW-CLM-CRT.                   
07868      ADD WX-ACTIVE-COUNT         TO ACTIVE-COUNT.                 
07869      ADD WX-CNC-CNT              TO EW-CNC-CNT.                   
07870                                                                   
07871      MOVE +0           TO SUB1.                                   
07872      PERFORM 4300-EP-COMM-LOOP THRU 4350-EP-COMM-LOOP-X           
07873          10 TIMES.                                                
           MOVE +0                     TO SUB16
           PERFORM 4375-EP-BANK-COMM   THRU 4375-EXIT VARYING
              WS-SUB1 FROM +1 BY +1 UNTIL WS-SUB1 > +10
07874                                                                   
07875      IF REIN-EP-FLG NOT = 'R'  OR                                 
07876         CR-POLICY-IS-REIN-ONLY                                    
07877          MOVE ISS-BEN TO GROSS-ISS-BEN-AH                         
07878          MOVE CNC-BEN TO GROSS-CNC-BEN-AH                         
07879          MOVE ISS-PRM TO GROSS-ISS-PRM-AH                         
07880          MOVE CNC-PRM TO GROSS-CNC-PRM-AH.                        
07881                                                                   
07882      ADD ISS-BEN TO EW-ISS-BEN.                                   
07883      ADD GROSS-ISS-BEN-AH TO EW-ISS-BEN-GROSS.                    
07884      ADD CNC-BEN TO EW-CNC-BEN.                                   
07885      ADD GROSS-CNC-BEN-AH TO EW-CNC-BEN-GROSS.                    
07886      ADD ISS-PRM TO EW-ISS-PRM.                                   
07887      ADD GROSS-ISS-PRM-AH TO EW-ISS-PRM-GROSS.                    
07888      ADD CNC-PRM TO EW-CNC-PRM.                                   
07889      ADD GROSS-CNC-PRM-AH TO EW-CNC-PRM-GROSS.                    
07890                                                                   
07891      COMPUTE EW-EPR-R78 = EW-EPR-R78 + EPR-R78.                   
07892      COMPUTE EW-EPR-PRO = EW-EPR-PRO + EPR-PRO.                   
07893      COMPUTE EW-EPR-ST  = EW-EPR-ST  + EPR-ST.                    
07894                                                                   
PEMMOD     IF CR-AH-ISS-PREM-TAX NOT NUMERIC
PEMMOD        MOVE ZEROS               TO CR-AH-ISS-PREM-TAX
PEMMOD     END-IF
PEMMOD     IF CR-AH-CNC-PREM-TAX NOT NUMERIC
PEMMOD        MOVE ZEROS               TO CR-AH-CNC-PREM-TAX
PEMMOD     END-IF
PEMMOD     COMPUTE ISS-PRMTAX =
PEMMOD        (ISS-PRM * CR-AH-ISS-PREM-TAX) -
PEMMOD        (CNC-PRM * CR-AH-CNC-PREM-TAX)
PEMMOD     COMPUTE EW-ISS-PRM-TAX = EW-ISS-PRM-TAX + ISS-PRMTAX
PEMMOD
07895      IF A-H-FULLY-EARNED                                          
07896          COMPUTE EW-EPR-R78-ADJ = EW-EPR-R78-ADJ +                
07897                                             (ISS-PRM - EPR-R78)   
07898          COMPUTE EW-EPR-PRO-ADJ = EW-EPR-PRO-ADJ +                
07899                                             (ISS-PRM - EPR-PRO)   
07900          COMPUTE EW-EPR-ST-ADJ  = EW-EPR-ST-ADJ +                 
07901                                             (ISS-PRM - EPR-ST).   
07902                                                                   
07903      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
07904          IF  REIN-EP-FLG NOT = 'R'                                
07905                  OR                                               
07906              CR-POLICY-IS-REIN-ONLY                               
07907              MOVE 'A'      TO W-CLAIM-TYPE                        
07908              MOVE EPR-R78  TO W-OPT-R78                           
07909              MOVE EPR-PRO  TO W-OPT-PRO                           
07910              MOVE EPR-ST   TO W-OPT-ST                            
07911              PERFORM 4600-GET-OPT-IBNR THRU 4600-EXIT             
07912              MOVE ZEROS    TO EW-CLM-IBNR                         
07913          ELSE                                                     
07914              IF  STOP-AH-DATE LESS THAN EOM-RUN-DATE              
07915                  MOVE 'N'   TO EPEC-LINE-NOT-USED-IND (SUB3)      
07916                  MOVE ZEROS TO EW-CLM-IBNR                        
07917              ELSE                                                 
07918                  MOVE ZEROS TO EW-CLM-IBNR                        
07919      ELSE                                                         
07920          IF  DTE-CLIENT NOT EQUAL 'ITG'                           
07921              ADD WE-DIS-IBNR     TO EW-CLM-IBNR                   
07922          ELSE                                                     
07923              IF  CLAS-I-BAL (CLAS-INDEXA) NOT EQUAL 'B'           
07924                  IF  CR-AH-POLICY-IS-ACTIVE                       
07925                      COMPUTE  EW-CLM-IBNR  =  EW-CLM-IBNR         
07926                               + ((WE-AHPRM - EPR-PRO) * .0730000) 
07927                  ELSE                                             
07928                      NEXT SENTENCE                                
07929              ELSE                                                 
07930                  IF  CR-DT GREATER THAN LAST-YRS-DATE             
07931                      COMPUTE  EW-CLM-IBNR                         
07932                          = EW-CLM-IBNR + (WE-AHPRM * .0600000).   
07933                                                                   
07934      ADD WE-DIS-PAYCUR TO EW-CLM-PAYCUR.                          
07935      ADD WE-DIS-FUTRSV TO EW-CLM-FUTRSV.                          
07936                                                                   
07937  4290-CONTINUE.                                                   
07938                                                                   
07939      MOVE 'Y'          TO EW-LEVEL-USED.                          
07940                                                                   
07941      IF CR-POLICY-UNDERWRITTEN                                    
07942          MOVE EPEC-WORK       TO EPEC-LEVELS-II (SUB3)            
07943          MOVE EPEC-OPT-WORK-TABLE                                 
07944                               TO EPEC-UNDW-OPT-DATA-TABLE         
07945      ELSE                                                         
07946          MOVE EPEC-WORK       TO EPEC-LEVELS (SUB3)               
07947          MOVE EPEC-OPT-WORK-TABLE                                 
07948                               TO EPEC-OPT-DATA-TABLE.             
07949                                                                   
07950                                                                   
07951  4295-EP-DO-COUNT.                                                
07952                                                                   
07953      IF REIN-EP-FLG = 'R'                                         
07954          ADD ACTIVE-COUNT TO BC-R-A-CERTS                         
07955      ELSE                                                         
07956          ADD ACTIVE-COUNT TO BC-G-A-CERTS.                        
07957                                                                   
07958      GO TO 4299-EPBX.                                             
07959  EJECT                                                            
07960  4296-SLOT-INTO-EFFECTED-MONTHS.                                  
07961                                                                   
07962      IF  CR-DT NOT GREATER THAN EOM-DATE (SUBM)                   
07963              AND                                                  
07964          CR-AH-EXPIRE-DATE GREATER THAN EOM-DATE (SUBM)           
07965              AND                                                  
07966          (CR-AH-CANC-DT GREATER THAN EOM-DATE (SUBM)              
07967                  OR                                               
07968           CR-AH-CANC-DT EQUAL ZEROS)                              
07969          MOVE 'Y'                 TO W-TABLE-USED-SW              
07970          IF  CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                 
07971              COMPUTE EPEC-LIFE-CTR (SUB3 SUBM) ROUNDED            
07972                  = EPEC-LIFE-CTR (SUB3 SUBM)                      
07973                  + CR-LIVES                                       
07974          ELSE                                                     
07975              COMPUTE EPEC-LIFE-CTR (SUB3 SUBM)                    
07976                  = EPEC-LIFE-CTR (SUB3 SUBM) + 1.                 
07977                                                                   
07978  4296-EXIT.                                                       
07979      EXIT.                                                        
07980                                                                   
07981  4299-EPBX.                                                       
07982      EXIT.                                                        
07983  EJECT                                                            
07984 ******************************************************************
07985 ***    C A L C U L A T E   E A R N E D   C O M M I S S I O N   ***
07986 ******************************************************************
07987                                                                   
07988  4300-EP-COMM-LOOP.                                               
07989      IF DTE-CLIENT = 'DMD'                                        
07990          GO TO 4350-EP-COMM-LOOP-X.                               
07991                                                                   
07992      ADD +1                         TO SUB1.                      
07993      MOVE ZEROS                     TO SUB16.                     
07994                                                                   
040504     IF CR-ADDL-CLP NOT NUMERIC
040504        MOVE ZEROS               TO CR-ADDL-CLP
040504     END-IF
07995      IF CR-COM-AGT (SUB1) = SPACES OR ZEROS                       
07996          GO TO 4350-EP-COMM-LOOP-X.                               
07997                                                                   
07998  4300-AGENT-LOOP.                                                 
07999                                                                   
08000      ADD +1                         TO SUB16.                     
08001                                                                   
08002      IF SUB16 GREATER THAN WS-LOOP-MAX                            
08003          DISPLAY 'ERROR OCCURRED - MAXIMUM AGENT LEVELS EXCEEDED' 
08004          DISPLAY 'CONTROL= ' CR-FULL-CONTROL                      
08005          MOVE 'ERR0R OCCURRED - MAXIMUM AGENT LEVELS EXCEEDED'    
08006                                      TO WS-ABEND-MESSAGE          
08007          MOVE DC-ERROR-CODE          TO WS-ABEND-FILE-STATUS      
08008          MOVE 4001                   TO WS-ABEND-CODE             
08009          GO TO ABEND-PGM.                                         
08010                                                                   
08011      IF EW-AGT-NO (SUB16) = SPACES                                
08012          MOVE CR-COM-AGT  (SUB1)     TO EW-AGT-NO   (SUB16)       
08013          MOVE CR-AGT-TYPE (SUB1)     TO EW-AGT-TYPE (SUB16)       
08014      ELSE                                                         
08015          IF (CR-COM-AGT (SUB1)  = EW-AGT-NO   (SUB16)) AND        
08016             (CR-AGT-TYPE (SUB1) = EW-AGT-TYPE (SUB16))            
08017              NEXT SENTENCE                                        
08018          ELSE                                                     
08019              GO TO 4300-AGENT-LOOP.                               
08020                                                                   
08021      IF EW-LF-AH = AH-OVERRIDE-L1                                 
08022          GO TO 4350-EP-COMM-LOOP-A.                               
08023                                                                   
032003     IF (DTE-CLIENT = 'DCC')
032003        AND (REIN-EP-FLG = 'R')
032003        AND (CR-AGT-TYPE (SUB1) = 'P' OR 'T')
032003        AND (WE-LFPRM NOT = ZEROS)
032003        COMPUTE WS-CEDE-FACT = RWF-LFAMT /
032003            (CR-LFAMT + CR-LFAMT-ALT)
032003*       COMPUTE ISS-WK ROUNDED = ((WE-LFPRM + WE-LFPRM-ALT)
032003        COMPUTE ISS-WK ROUNDED = ((CR-LFPRM + CR-LFPRM-ALT)
032003           * WS-CEDE-FACT) * CR-LCOM-L (SUB1)
032003     ELSE
100703        IF CR-AGT-TYPE (SUB1) = 'B' OR 'I' OR 'K' OR 'L'
052306                             OR 'J'
100703           COMPUTE ISS-WK = CR-LCOM-L (SUB1) * +1000
100703        ELSE
032003           COMPUTE ISS-WK  ROUNDED = ISS-PRM * CR-LCOM-L (SUB1)
100703        END-IF
032003     END-IF
032003
CIDMOD     COMPUTE EW-ISS-COM (SUB16) = EW-ISS-COM (SUB16) + ISS-WK.    
08026                                                                   
CIDMOD     MOVE ZEROS                  TO CNC-WK
100703                                    CNC-FACT
08027      IF DTE-CLIENT EQUAL 'XXX'                                    
08028          IF NOT CR-NO-LF-CHARGEBACK                               
CIDMOD            COMPUTE CNC-WK  ROUNDED = CNC-PRM * CR-LCOM-L (SUB1)  
08030             COMPUTE EW-CNC-COM (SUB16) = EW-CNC-COM (SUB16)       
CIDMOD                                + CNC-WK                          
08032             GO TO 4300-CONTINUE-LOOP.                             
08033                                                                   
           
08034      IF (WS-LEVELS-CHARGEBACK-SWITCHES = SPACES) OR               
08035         (WS-LF-CHARGEBACK-SW (SUB1) EQUAL 'Y')
100703        IF (CR-AGT-TYPE (SUB1) = 'K')
100703           AND (CNC-PRM > +0)
100703           COMPUTE CNC-WK = CR-LCOM-L (SUB1) * +1000
100703        ELSE                   
052306           IF (CR-AGT-TYPE (SUB1) = 'B' OR 'I' OR 'L' OR 'J')
100703              AND (CNC-PRM > +0)
100703              COMPUTE CNC-FACT = CNC-PRM / ISS-PRM
100703              COMPUTE CNC-WK = CNC-FACT *
100703                 (CR-LCOM-L (SUB1) * +1000)
100703           ELSE                   
092705             IF (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
100703                AND (CR-AGT-TYPE (SUB1) = 'C' OR 'D')
100703                COMPUTE CNC-FACT = CNC-PRM / ISS-PRM
100703                COMPUTE CNC-WK = CNC-FACT *
100703                   (CR-AHPRM - (CR-LFPRM-ALT
040504                   + CR-ADDL-CLP))
100703             ELSE
CIDMOD                COMPUTE CNC-WK  ROUNDED =
100703                   CNC-PRM * CR-LCOM-L (SUB1)
                   END-IF
100703           END-IF
100703        END-IF
08037         COMPUTE EW-CNC-COM (SUB16) =                           
CIDMOD                                  EW-CNC-COM (SUB16) + CNC-WK     
100703     END-IF
08040      .                                                             
08041  4300-CONTINUE-LOOP.                                              
08042                                                                   
CIDMOD     IF ((ISS-PRM - CNC-PRM) = ZEROS)
CIDMOD               OR
CIDMOD        ((ISS-WK - CNC-WK) = ZEROS)
CIDMOD        MOVE ZEROS              TO COMM-WK
CIDMOD     ELSE
CIDMOD        COMPUTE COMM-WK ROUNDED = EPR-R78 /
CIDMOD        (ISS-PRM - CNC-PRM) * (ISS-WK - CNC-WK)
CIDMOD     END-IF
CIDMOD
08044      COMPUTE EW-ECR-R78 (SUB16) = EW-ECR-R78 (SUB16) + COMM-WK.   
08045                                                                   
CIDMOD     IF ((ISS-PRM - CNC-PRM) = ZEROS)
CIDMOD               OR
CIDMOD        ((ISS-WK - CNC-WK) = ZEROS)
CIDMOD        MOVE ZEROS              TO COMM-WK
CIDMOD     ELSE
CIDMOD        COMPUTE COMM-WK ROUNDED = EPR-PRO /
CIDMOD        (ISS-PRM - CNC-PRM) * (ISS-WK - CNC-WK)
CIDMOD     END-IF
08047      COMPUTE EW-ECR-PRO (SUB16) = EW-ECR-PRO (SUB16) + COMM-WK.   
08048                                                                   
CIDMOD     IF ((ISS-PRM - CNC-PRM) = ZEROS)
CIDMOD               OR
CIDMOD        ((ISS-WK - CNC-WK) = ZEROS)
CIDMOD        MOVE ZEROS              TO COMM-WK
CIDMOD     ELSE
CIDMOD        COMPUTE COMM-WK ROUNDED = EPR-ST /
CIDMOD        (ISS-PRM - CNC-PRM) * (ISS-WK - CNC-WK)
CIDMOD     END-IF
CIDMOD
08050      COMPUTE EW-ECR-ST (SUB16) = EW-ECR-ST (SUB16) + COMM-WK.     
08051                                                                   
08052      IF LIFE-FULLY-EARNED                                         
08053          COMPUTE COMM-WK ROUNDED = (ISS-PRM - EPR-R78) *          
08054                                                 CR-LCOM-L (SUB1)  
08055          COMPUTE EW-ECR-R78-ADJ (SUB16) =                         
08056                                EW-ECR-R78-ADJ (SUB16) + COMM-WK   
08057          COMPUTE COMM-WK ROUNDED = (ISS-PRM - EPR-PRO) *          
08058                                                 CR-LCOM-L (SUB1)  
08059          COMPUTE EW-ECR-PRO-ADJ (SUB16) =                         
08060                                EW-ECR-PRO-ADJ (SUB16) + COMM-WK.  
08061                                                                   
08062      GO TO 4350-EP-COMM-LOOP-X.                                   
08063                                                                   
08064  4350-EP-COMM-LOOP-A.                                             
08065                                                                   
032003     IF (DTE-CLIENT = 'DCC')
032003        AND (REIN-EP-FLG = 'R')
032003        AND (CR-AGT-TYPE (SUB1) = 'P' OR 'T')
032003        AND (WE-AHPRM NOT = ZEROS)
032003        COMPUTE WS-CEDE-FACT = RWF-AHAMT / CR-AHAMT
032003        COMPUTE ISS-WK ROUNDED = (CR-AHPRM * WS-CEDE-FACT)
032003*       COMPUTE ISS-WK ROUNDED = (WE-AHPRM * WS-CEDE-FACT)
032003           * CR-LCOM-AH (SUB1)
032003     ELSE
052306        IF CR-AGT-TYPE (SUB1) = 'B' OR 'I' OR 'K' OR 'L' OR 'J'
100703           COMPUTE ISS-WK = CR-LCOM-AH (SUB1) * +1000
100703        ELSE
100703           IF (CR-AGT-TYPE (SUB1) = 'D' OR 'C')
092705             AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA)) = 'G' OR 'L'
100703              COMPUTE ISS-WK = CR-AHPRM - (CR-LFPRM-ALT
040504              + CR-ADDL-CLP)
100703           ELSE
032003              COMPUTE ISS-WK  ROUNDED =
100703                    ISS-PRM * CR-LCOM-AH (SUB1)
100703           END-IF
100703        END-IF
032003     END-IF
032003
08067      COMPUTE EW-ISS-COM (SUB16) =                                 
CIDMOD             EW-ISS-COM (SUB16) + ISS-WK.                         
08069                                                                   
CIDMOD     MOVE ZEROS                  TO CNC-WK
08070      IF DTE-CLIENT EQUAL 'XXX'                                    
08071          IF NOT CR-NO-LF-CHARGEBACK                               
CIDMOD           COMPUTE CNC-WK  ROUNDED = CNC-PRM * CR-LCOM-AH (SUB1)  
08073            COMPUTE EW-CNC-COM (SUB16) =                           
CIDMOD                   EW-CNC-COM (SUB16) + CNC-WK                    
08075            GO TO 4300-CONTINUE-LOOP-AH.                           
08076                                                                   

08077      IF (WS-LEVELS-CHARGEBACK-SWITCHES = SPACES) OR               
08078         (WS-AH-CHARGEBACK-SW (SUB1) EQUAL 'Y')
022406      IF (DTE-CLIENT = 'DCC')
022406         AND (REIN-EP-FLG = 'R')
022406         AND (CR-AGT-TYPE (SUB1) = 'P' OR 'T')
022406         AND (WE-AHRFND NOT = ZEROS)
022406         COMPUTE WS-CEDE-FACT = RWF-AHAMT / CR-AHAMT
022406         COMPUTE CNC-WK ROUNDED = (CR-AHRFND * WS-CEDE-FACT)
022406            * CR-LCOM-AH (SUB1)
            ELSE



100703        IF (CR-AGT-TYPE (SUB1) = 'K')
100703           AND (CNC-PRM > +0)
100703           COMPUTE CNC-WK = CR-LCOM-AH (SUB1) * +1000
100703        ELSE
052306           IF (CR-AGT-TYPE (SUB1) = 'B' OR 'I' OR 'L' OR 'J')
100703              AND (CNC-PRM > +0)
100703              COMPUTE CNC-FACT = CNC-PRM / ISS-PRM
100703              COMPUTE CNC-WK = CNC-FACT *
100703                 (CR-LCOM-AH (SUB1) * +1000)
100703           ELSE
092705              IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L'
100703                 COMPUTE CNC-FACT = CR-AHRFND / CR-AHPRM
100703                 COMPUTE CNC-WK = CNC-FACT *
100703                    (CR-AHPRM - (CR-LFPRM-ALT
040504                    + CR-ADDL-CLP))
100703              ELSE
CIDMOD                 COMPUTE CNC-WK  ROUNDED =
                          CNC-PRM * CR-LCOM-AH (SUB1)
                    END-IF
                 END-IF
100703        END-IF
            END-IF
08080         COMPUTE EW-CNC-COM (SUB16) =                           
CIDMOD                   EW-CNC-COM (SUB16) + CNC-WK
100703     END-IF

08083      .
08084  4300-CONTINUE-LOOP-AH.                                           
08085                                                                   
CIDMOD     IF ((ISS-PRM - CNC-PRM) = ZEROS)
CIDMOD                OR
CIDMOD        ((ISS-WK - CNC-WK) = ZEROS)
CIDMOD        MOVE ZEROS          TO COMM-WK
CIDMOD     ELSE
CIDMOD        COMPUTE COMM-WK ROUNDED = EPR-R78 /
CIDMOD        (ISS-PRM - CNC-PRM) * (ISS-WK - CNC-WK)
CIDMOD     END-IF
CIDMOD
08087      COMPUTE EW-ECR-R78 (SUB16) = EW-ECR-R78 (SUB16) + COMM-WK.   
08088                                                                   
CIDMOD     IF ((ISS-PRM - CNC-PRM) = ZEROS)
CIDMOD                OR
CIDMOD        ((ISS-WK - CNC-WK) = ZEROS)
CIDMOD        MOVE ZEROS          TO COMM-WK
CIDMOD     ELSE
CIDMOD        COMPUTE COMM-WK ROUNDED = EPR-PRO /
CIDMOD        (ISS-PRM - CNC-PRM) * (ISS-WK - CNC-WK)
CIDMOD     END-IF
CIDMOD
08090      COMPUTE EW-ECR-PRO (SUB16) = EW-ECR-PRO (SUB16) + COMM-WK.   
08091                                                                   
CIDMOD     IF ((ISS-PRM - CNC-PRM) = ZEROS)
CIDMOD                OR
CIDMOD        ((ISS-WK - CNC-WK) = ZEROS)
CIDMOD        MOVE ZEROS          TO COMM-WK
CIDMOD     ELSE
CIDMOD        COMPUTE COMM-WK ROUNDED = EPR-ST  /
CIDMOD        (ISS-PRM - CNC-PRM) * (ISS-WK - CNC-WK)
CIDMOD     END-IF
CIDMOD
08093      COMPUTE EW-ECR-ST (SUB16) = EW-ECR-ST (SUB16) + COMM-WK.     
08094                                                                   
08095      IF A-H-FULLY-EARNED                                          
08096          COMPUTE COMM-WK ROUNDED = (ISS-PRM - EPR-R78) *          
08097                                                 CR-LCOM-AH (SUB1) 
08098          COMPUTE EW-ECR-R78-ADJ (SUB16) =                         
08099                                EW-ECR-R78-ADJ (SUB16) + COMM-WK   
08100          COMPUTE COMM-WK ROUNDED = (ISS-PRM - EPR-PRO) *          
08101                                                 CR-LCOM-AH (SUB1) 
08102          COMPUTE EW-ECR-PRO-ADJ (SUB16) =                         
08103                                EW-ECR-PRO-ADJ (SUB16) + COMM-WK.  
08104                                                                   
08105  4350-EP-COMM-LOOP-X.                                             
08106      EXIT.                                                        




111204******************************************************************
111204***    C A L C U L A T E   E A R N E D  BANK COMMISSION        ***
111204******************************************************************
111204                                                                  
111204 4375-EP-BANK-COMM.                                               
111204                                                                  
111204     IF CS-AGT (SUB1) = SPACES OR ZEROS                       
111204        GO TO 4375-EXIT
111204     END-IF
111204     .                                                            
111204 4375-BANK-LOOP.                                                 
111204                                                                  
111204     ADD +1                      TO SUB16
111204                                                                  
111204     IF EW-LF-AH NOT = AH-OVERRIDE-L1                                 
111204        GO TO 4375-EXIT
111204     END-IF
111204                                                                  
111204     IF SUB16 GREATER THAN WS-LOOP-MAX                            
111204         DISPLAY 'ERROR OCCURRED - MAXIMUM BANK  LEVELS EXCEEDED' 
111204         DISPLAY 'CONTROL= ' CR-FULL-CONTROL                      
111204         MOVE 'ERR0R OCCURRED - MAXIMUM AGENT LEVELS EXCEEDED'    
111204                                     TO WS-ABEND-MESSAGE          
111204         MOVE DC-ERROR-CODE          TO WS-ABEND-FILE-STATUS      
111204         MOVE 4001                   TO WS-ABEND-CODE             
111204         PERFORM ABEND-PGM
111204     END-IF
111204                                                                  
111204     IF EW-AGT-NO (SUB16) = SPACES                                
111204        MOVE CS-AGT (SUB1)       TO EW-AGT-NO   (SUB16)       
111204        MOVE CS-COM-TYP (SUB1)   TO EW-AGT-TYPE (SUB16)       
111204     ELSE                                                         
111204        IF (CS-AGT (SUB1) = EW-AGT-NO   (SUB16)) AND        
111204           (CS-COM-TYP (SUB1) = EW-AGT-TYPE (SUB16))            
111204           CONTINUE
111204        ELSE                                                     
111204           GO TO 4375-BANK-LOOP
111204        END-IF
111204     END-IF
111204                                                                  
111204     IF CS-COM-TYP  (SUB1) = 'B'
111204        MOVE CS-SPP-FEES (SUB1)  TO ISS-WK
111204     END-IF
111204
111204     COMPUTE EW-ISS-COM (SUB16) = EW-ISS-COM (SUB16) + ISS-WK.    
111204                                                                  
111204     MOVE ZEROS                  TO CNC-WK
111204                                    CNC-FACT

111204     IF (CS-COM-TYP (SUB1) = 'B')
111204        AND (CNC-PRM > +0)
111204        COMPUTE CNC-FACT = CNC-PRM / ISS-PRM
111204        COMPUTE CNC-WK = CNC-FACT * CS-SPP-FEES (SUB1)
111204     END-IF

111204     COMPUTE EW-CNC-COM (SUB16) =                           
111204                  EW-CNC-COM (SUB16) + CNC-WK

111204     IF ((ISS-PRM - CNC-PRM) = ZEROS)
111204                OR
111204        ((ISS-WK - CNC-WK) = ZEROS)
111204        MOVE ZEROS          TO COMM-WK
111204     ELSE
111204        COMPUTE COMM-WK ROUNDED = EPR-R78 /
111204        (ISS-PRM - CNC-PRM) * (ISS-WK - CNC-WK)
111204     END-IF
111204
111204     COMPUTE EW-ECR-R78 (SUB16) = EW-ECR-R78 (SUB16) + COMM-WK.   
111204                                                                  
111204     IF ((ISS-PRM - CNC-PRM) = ZEROS)
111204                OR
111204        ((ISS-WK - CNC-WK) = ZEROS)
111204        MOVE ZEROS          TO COMM-WK
111204     ELSE
111204        COMPUTE COMM-WK ROUNDED = EPR-PRO /
111204        (ISS-PRM - CNC-PRM) * (ISS-WK - CNC-WK)
111204     END-IF
111204
111204     COMPUTE EW-ECR-PRO (SUB16) = EW-ECR-PRO (SUB16) + COMM-WK.   
111204                                                                  
111204     IF ((ISS-PRM - CNC-PRM) = ZEROS)
111204                OR
111204        ((ISS-WK - CNC-WK) = ZEROS)
111204        MOVE ZEROS          TO COMM-WK
111204     ELSE
111204        COMPUTE COMM-WK ROUNDED = EPR-ST  /
111204        (ISS-PRM - CNC-PRM) * (ISS-WK - CNC-WK)
111204     END-IF
111204
111204     COMPUTE EW-ECR-ST (SUB16) = EW-ECR-ST (SUB16) + COMM-WK.     
111204                                                                  
111204     IF A-H-FULLY-EARNED                                          
111204        MOVE CS-SPP-FEES (SUB1)  TO COMM-WK
111204        COMPUTE EW-ECR-R78-ADJ (SUB16) =                         
111204                               EW-ECR-R78-ADJ (SUB16) + COMM-WK   
111204        COMPUTE EW-ECR-PRO-ADJ (SUB16) =                         
111204                               EW-ECR-PRO-ADJ (SUB16) + COMM-WK
111204     END-IF

111204     .
111204 4375-EXIT.
111204     EXIT.                                                        

08108 ******************************************************************
08109 ******************************************************************
08110 ******************************************************************
08111 ******************************************************************
08112 ******************************************************************
08113 ******************************************************************
08114 ******************************************************************
08115 ******************************************************************
08116 ******************************************************************
08117 ******************************************************************
08118 ***  C A L C U L A T E   L I F E   S T A R T I N G  E A R N E D***
08119 ******************************************************************
08120                                                                   
08121  S-4100-EP-DO-LF.                                                 
08122                                                                   
08123      MOVE +0                         TO EPR-R78  EARNED-BY-CANCEL 
08124                                         EPR-PRO                   
08125                                         EPR-ST.                   
08126                                                                   
08127      MOVE RCT-EARNING-START-DT (CO-SUB) TO DC-GREG-DATE-CYMD.     
08128      MOVE 'L'                    TO DC-OPTION-CODE.               
08129      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
08130                                                                   
08131      MOVE +0                     TO DC-ELAPSED-MONTHS.            
08132      MOVE -1                     TO DC-ELAPSED-DAYS.              
08133      MOVE '6'                    TO DC-OPTION-CODE.               
08134      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
08135                                                                   
08136      MOVE +0                     TO DC-ELAPSED-MONTHS             
08137                                     DC-ELAPSED-DAYS.              
08138                                                                   
08139      MOVE DC-BIN-DATE-2          TO DC-BIN-DATE-1.                
08140      MOVE ' '                    TO DC-OPTION-CODE.               
08141      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
08142                                                                   
08143      IF NO-CONVERSION-ERROR                                       
08144         MOVE DC-GREG-DATE-CYMD   TO EARNING-START-DATE            
08145      ELSE                                                         
08146          DISPLAY 'ERROR OCCURED CONVERTING EARNING START DATE '   
08147                  'FOR REIN. CO. ' RCT-REIN-CO (CO-SUB)            
08148                  '  START DATE:' RCT-EARNING-START-DT (CO-SUB)    
08149          MOVE 'ERROR OCCURED CONVERTING EARNING START DATE '      
08150                                      TO WS-ABEND-MESSAGE          
08151          MOVE DC-ERROR-CODE          TO WS-ABEND-FILE-STATUS      
08152          MOVE 4001                   TO WS-ABEND-CODE             
08153          GO TO ABEND-PGM.                                         
08154                                                                   
08155      IF CR-LFTYP = ZERO                                           
08156          GO TO S-4200-EP-DO-AH.                                   
08157                                                                   
08158      IF REIN-LF-FLG (SUB5) = SPACE                                
08159          GO TO S-4200-EP-DO-AH.                                   
08160                                                                   
08161      MOVE LIFE-OVERRIDE-L1       TO WE-LF-AH.                     
08162      MOVE CR-LFTYP               TO WE-BEN-TYPE.                  
08163      PERFORM 4400-EP-FIND-TAB-ENT THRU 4499-EP-FIND-TAB-ENT-X.    
08164                                                                   
08165      MOVE CR-LF-TERM             TO ORIG-TERM.                    
08166      MOVE CLAS-INDEXL            TO W-NDX.                        
08167                                                                   
08168  S-4120-EP-DO-LF-CNCL.                                            
08169                                                                   
08170      IF CR-LF-STATUS-AT-CANCEL = ' '  AND                         
08171         CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'Z'                  
08172          GO TO S-4130-EP-DO-LF-EP.                                
08173                                                                   
08174      COMPUTE EARNED-BY-CANCEL =                                   
08175                        (WE-LFPRM + WE-LFPRM-ALT) - WE-LFRFND.     
08176                                                                   
08177  S-4130-EP-DO-LF-EP.                                              
08178                                                                   
08179      MOVE SPACE                  TO STATUTORY-SWITCH.             
08180                                                                   
08181      MOVE EARNING-START-DATE     TO VALUATION-DT.                 
08182      MOVE '9'                    TO W-VAL-SOURCE.                 
08183      PERFORM 8500-REMAINING-TERM-ROUTINE THRU 8599-REM-TERM-X.    
08184                                                                   
08185      IF REM-TRM2 GREATER CR-LF-TERM                               
08186          MOVE +0 TO EPR-R78  EPR-PRO  EPR-ST                      
08187          GO TO S-4190-EP-DO-LF-COMM.                              
08188                                                                   
08189      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                      
08190          GO TO S-4180-EP-DO-LF-EP-SUM.                            
08191                                                                   
08192      IF CLAS-I-EP (CLAS-INDEXL) = 'B'                             
08193          IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'L'                  
08194              IF REM-TRM2 NOT GREATER THAN +0                      
08195                  COMPUTE EPR-R78 = WE-LFPRM + WE-LFPRM-ALT        
08196                  MOVE EPR-R78 TO EPR-PRO  EPR-ST                  
08197                  GO TO S-4190-EP-DO-LF-COMM                       
08198              ELSE                                                 
08199                  NEXT SENTENCE                                    
08200          ELSE                                                     
08201              IF W-BAL-REMTRM NOT GREATER +0                       
08202                  COMPUTE EPR-R78 = WE-LFPRM + WE-LFPRM-ALT        
08203                  MOVE EPR-R78 TO EPR-PRO  EPR-ST                  
08204                  GO TO S-4190-EP-DO-LF-COMM                       
08205              ELSE                                                 
08206                  NEXT SENTENCE                                    
08207      ELSE                                                         
08208         IF (DTE-CLIENT EQUAL 'NCL') AND                           
08209            (CLAS-I-CALC-TYPE (CLAS-INDEXL) EQUAL 'B')             
08210              MOVE WE-LFPRM TO EPR-R78  EPR-PRO  EPR-ST            
08211         ELSE                                                      
08212          IF REM-TRM2 NOT GREATER +0  OR                           
08213             CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'                  
08214              MOVE WE-LFPRM TO EPR-R78  EPR-PRO  EPR-ST            
08215              GO TO S-4190-EP-DO-LF-COMM.                          
08216                                                                   
08217      IF CLAS-I-EP (CLAS-INDEXL) = 'T'                             
08218          GO TO S-4140-EP-DO-LF-EP-TEX.                            
08219                                                                   
08220      IF CLAS-I-EP (CLAS-INDEXL) = 'N'                             
08221          GO TO S-4150-EP-DO-LF-EP-NP.                             
08222                                                                   
CIDMOD*    IF STATE-ABBR (CLAS-INDEXS) = 'OH'                           
CIDMOD     IF (STATE-ABBR (CLAS-INDEXS) = 'OH') AND                     
CIDMOD        (CR-RATING-CLASS NOT = 'L ')
08224          IF (CR-APR GREATER THAN ZERO)  AND                       
08225             (CR-LF-TERM GREATER THAN 060)  AND                    
08226             (CR-DT GREATER THAN 19831031)                         
08227              MOVE '*'             TO STATUTORY-SWITCH             
08228               GO TO S-4150-EP-DO-LF-EP-NP.                        
08229                                                                   
08230      IF STATE-ABBR (CLAS-INDEXS) = 'MT'                           
08231          IF (CR-APR GREATER THAN ZERO)  AND                       
08232             (CR-LF-TERM GREATER THAN 061)  AND                    
08233             (CR-DT GREATER THAN 19830318)                         
08234              MOVE '*'             TO STATUTORY-SWITCH             
08235               GO TO S-4150-EP-DO-LF-EP-NP.                        
08236                                                                   
08237      IF STATE-ABBR (CLAS-INDEXS) = 'UT'                           
08238          IF (CR-APR GREATER THAN ZERO)  AND                       
08239             (CR-LF-TERM GREATER THAN 062)  AND                    
08240             (CR-DT GREATER THAN 19810831)  AND                    
08241             (CR-DT LESS THAN 19830901)                            
08242              MOVE '*'             TO STATUTORY-SWITCH             
08243               GO TO S-4150-EP-DO-LF-EP-NP.                        
08244                                                                   
08245      IF STATE-ABBR (CLAS-INDEXS) = 'RI'                           
08246          IF (CR-APR GREATER THAN ZERO)  AND                       
08247             (CR-LF-TERM GREATER THAN 060)  AND                    
08248             (CR-DT GREATER THAN 19831231)                         
08249              MOVE '*'             TO STATUTORY-SWITCH             
08250               GO TO S-4150-EP-DO-LF-EP-NP.                        
08251                                                                   
08252      GO TO S-4160-EP-DO-LF-EP-R78.                                
08253                                                                   
08254  S-4140-EP-DO-LF-EP-TEX.                                          
08255                                                                   
08256      COMPUTE TEX-FACT-4 =                                         
08257          (CR-LF-TERM * CR-LF-TERM) + (CR-PMT-FREQ * CR-LF-TERM).  
08258      DIVIDE REM-TRM1 BY CR-PMT-FREQ                               
08259          GIVING TEX-FACT-5                                        
08260              REMAINDER TEX-FACT-6.                                
08261      COMPUTE TEX-FACT-5 = TEX-FACT-5 * CR-PMT-FREQ.               
08262      COMPUTE TEX-FACT-7 = (TEX-FACT-5 * TEX-FACT-5) +             
08263          (TEX-FACT-5 * CR-PMT-FREQ) +                             
08264          (2 * (TEX-FACT-6 * (TEX-FACT-5 + CR-PMT-FREQ))).         
08265      COMPUTE TEX-FACT-8 ROUNDED = TEX-FACT-7 / TEX-FACT-4.        
08266                                                                   
08267      COMPUTE EPR-R78 ROUNDED = WE-LFPRM - (WE-LFPRM * TEX-FACT-8).
08268                                                                   
08269      GO TO S-4170-EP-DO-LF-EP-PRO.                                
08270                                                                   
08271  S-4150-EP-DO-LF-EP-NP.                                           
08272                                                                   
08273      IF WE-LFPRM = ZERO                                           
08274          GO TO S-4160-EP-DO-LF-EP-R78.                            
08275                                                                   
08276      IF DTE-CLIENT = 'CRI'                                        
08277        IF STATE-ABBR (CLAS-INDEXS) = ('AB' OR 'BC' OR 'MB' OR     
08278              'NS' OR 'ON' OR 'NB' OR 'NF' OR 'QB' OR 'SK' OR 'PE')
08279            GO TO S-4160-EP-DO-LF-EP-R78.                          
08280                                                                   
08281      IF DTE-CLIENT = 'NCL'                                        
08282        IF CR-DT LESS THAN 19910101                                
08283            GO TO S-4160-EP-DO-LF-EP-R78.                          
08284                                                                   
08285      IF REM-TRM1 = CR-LF-TERM                                     
08286          MOVE +0 TO EPR-R78  EPR-PRO  EPR-ST                      
08287          GO TO S-4190-EP-DO-LF-COMM.                              
08288                                                                   
08289      MOVE CR-APR TO NP-APR.                                       
08290      MOVE CR-LF-TERM TO NP-ORIG  NP-CAP.                          
08291      MOVE REM-TRM1 TO NP-REM.                                     
08292      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL) TO NP-OPT.               
08293      IF NP-OPT = 'T' OR 'U' OR 'V' OR 'W' OR 'X'                  
08294          MOVE CR-LOAN-TERM TO NP-ORIG.                            
08295      MOVE 'R' TO NP-OPT.                                          
08296      CALL 'ECSNETRM'                                              
08297          USING NP-APR NP-ORIG NP-REM NP-OPT NP-CAP NP-FACTOR.     
08298                                                                   
08299      COMPUTE EPR-R78 ROUNDED = WE-LFPRM - (NP-FACTOR * WE-LFPRM). 
08300                                                                   
08301      IF STATUTORY-REQUIREMENT                                     
08302          MOVE EPR-R78                TO EPR-ST.                   
08303                                                                   
08304  S-4160-EP-DO-LF-EP-R78.                                          
08305                                                                   
08306      IF (CLAS-I-CALC-TYPE (CLAS-INDEXL) EQUAL 'B') AND            
08307         (DTE-CLIENT EQUAL 'NCL')                                  
08308         IF (CR-CCYY EQUAL EOM-CCYY) AND                           
08309            (CR-MO EQUAL EOM-MO)                                   
08310            MOVE '*'            TO OB-DATE-1-SWITCH                
08311            MOVE EW-REIN-CO     TO WS-OB-REIN-CO (SUB3)            
08312            MOVE LIFE-OVERRIDE-L1 TO WS-OB-LF-AH (SUB3)            
08313            MOVE CR-LFTYP       TO WS-OB-BENEFIT-CODE (SUB3)       
08314            COMPUTE WS-EPR-R78 (SUB3 1) =                          
08315                    WS-EPR-R78 (SUB3 1) - (WE-LFPRM * .5)          
08316            GO TO S-4190-EP-DO-LF-COMM                             
08317         ELSE                                                      
08318         IF (CR-CCYY EQUAL OB-CCYY (1)) AND                        
08319            (CR-MO EQUAL OB-MO (1))                                
08320            MOVE '*'            TO OB-DATE-2-SWITCH                
08321            MOVE EW-REIN-CO     TO WS-OB-REIN-CO (SUB3)            
08322            MOVE LIFE-OVERRIDE-L1 TO WS-OB-LF-AH (SUB3)            
08323            MOVE CR-LFTYP       TO WS-OB-BENEFIT-CODE (SUB3)       
08324            COMPUTE WS-EPR-R78 (SUB3 2) =                          
08325                    WS-EPR-R78 (SUB3 2) - (WE-LFPRM * .5)          
08326            GO TO S-4190-EP-DO-LF-COMM                             
08327         ELSE                                                      
08328         IF (CR-CCYY EQUAL OB-CCYY (2)) AND                        
08329            (CR-MO EQUAL OB-MO (2))                                
08330            MOVE '*'            TO OB-DATE-3-SWITCH                
08331            MOVE EW-REIN-CO     TO WS-OB-REIN-CO (SUB3)            
08332            MOVE LIFE-OVERRIDE-L1 TO WS-OB-LF-AH (SUB3)            
08333            MOVE CR-LFTYP       TO WS-OB-BENEFIT-CODE (SUB3)       
08334            COMPUTE WS-EPR-R78 (SUB3 3) =                          
08335                    WS-EPR-R78 (SUB3 3) - (WE-LFPRM * .5)          
08336            GO TO S-4190-EP-DO-LF-COMM                             
08337         ELSE                                                      
08338            GO TO S-4190-EP-DO-LF-COMM.                            
08339                                                                   
08340      COMPUTE TEMP-1 = CR-LF-TERM * (CR-LF-TERM + 1).              
08341                                                                   
08342      IF DTE-R78 = '1'                                             
08343          COMPUTE TEMP-2 = REM-TRM1 * REM-TRM1                     
08344      ELSE                                                         
08345          COMPUTE TEMP-2 = REM-TRM1 * (REM-TRM1 + 1).              
08346                                                                   
08347      COMPUTE TEMP-4 ROUNDED EQUAL TEMP-2 / TEMP-1.                
08348                                                                   
08349      IF REM-TRM1 GREATER THAN +0                                  
08350          COMPUTE TEMP-7 ROUNDED EQUAL                             
08351              WE-LFPRM - (WE-LFPRM * TEMP-4)                       
08352      ELSE                                                         
08353          MOVE WE-LFPRM                TO TEMP-7.                  
08354                                                                   
08355      IF CLAS-I-EP (CLAS-INDEXL) NOT = 'B'                         
08356          MOVE ZEROS                  TO TEMP-8                    
08357          GO TO S-4165-END-LF-EP-R78.                              
08358                                                                   
08359      IF EPR-ST NOT = ZEROS                                        
08360          MOVE TEMP-7                 TO EPR-ST.                   
08361                                                                   
08362      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                  
08363          COMPUTE TEMP-1 = (CR-LF-TERM + 1) * (CR-LF-TERM + 2)     
08364          COMPUTE TEMP-2 = (REM-TRM1 + 1) * (REM-TRM1 + 2)         
08365          IF DTE-R78 = '1'                                         
08366              COMPUTE TEMP-2 = (REM-TRM1 + 1) * (REM-TRM1 + 1).    
08367                                                                   
08368      COMPUTE TEMP-4 ROUNDED EQUAL TEMP-2 / TEMP-1.                
08369                                                                   
08370      COMPUTE TEMP-8 ROUNDED =                                     
08371                WE-LFPRM-ALT - (WE-LFPRM-ALT * TEMP-4).            
08372                                                                   
08373  S-4165-END-LF-EP-R78.                                            
08374                                                                   
08375      IF EPR-R78 = ZEROS                                           
08376          COMPUTE EPR-R78 = TEMP-7 + TEMP-8                        
08377      ELSE                                                         
08378          COMPUTE EPR-R78 = EPR-R78 + TEMP-8.                      
08379                                                                   
08380  S-4170-EP-DO-LF-EP-PRO.                                          
08381                                                                   
08382      IF DTE-CLIENT = 'MIC' OR 'MCC'                               
08383          COMPUTE TEMP-5 ROUNDED = (REM-TRM1 - .5) / CR-LF-TERM    
08384      ELSE                                                         
08385          COMPUTE TEMP-5 ROUNDED = REM-TRM1 / CR-LF-TERM.          
08386                                                                   
08387      IF REM-TRM1 GREATER THAN +0                                  
08388          COMPUTE TEMP-7 ROUNDED = WE-LFPRM - (WE-LFPRM * TEMP-5)  
08389      ELSE                                                         
08390          MOVE WE-LFPRM                  TO TEMP-7.                
08391                                                                   
08392      IF CLAS-I-EP (CLAS-INDEXL) NOT = 'B'                         
08393          MOVE ZEROS                     TO TEMP-8                 
08394          GO TO S-4175-END-LF-EP-PRO.                              
08395                                                                   
08396      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                  
08397          IF DTE-CLIENT = 'MIC' OR 'MCC'                           
08398              COMPUTE TEMP-5 ROUNDED =                             
08399                            (REM-TRM1 + .5) / (CR-LF-TERM + 1)     
08400          ELSE                                                     
08401              COMPUTE TEMP-5 ROUNDED =                             
08402                            (REM-TRM1 + 1) / (CR-LF-TERM + 1).     
08403                                                                   
08404      COMPUTE TEMP-8 ROUNDED =                                     
08405                    WE-LFPRM-ALT - (WE-LFPRM-ALT * TEMP-5).        
08406                                                                   
08407      COMPUTE EPR-ST = EPR-ST + TEMP-8.                            
08408                                                                   
08409  S-4175-END-LF-EP-PRO.                                            
08410                                                                   
08411      COMPUTE EPR-PRO = TEMP-7 + TEMP-8.                           
08412                                                                   
08413      GO TO S-4185-EP-DO-LF-STATUTORY.                             
08414                                                                   
08415  S-4180-EP-DO-LF-EP-SUM.                                          
08416                                                                   
08417      IF REM-TRM1 NOT GREATER +0                                   
08418          COMPUTE EPR-R78 = (WE-LFPRM + WE-LFPRM-ALT) - WE-LFRFND  
08419          MOVE EPR-R78 TO EPR-PRO  EPR-ST                          
08420          GO TO S-4190-EP-DO-LF-COMM.                              
08421                                                                   
08422      IF WE-LFPRM = ZERO  AND  WE-LFPRM-ALT = ZERO                 
08423          MOVE ZEROS                 TO TEMP-7                     
08424      ELSE                                                         
08425          COMPUTE TEMP-7 ROUNDED = WE-LFPRM - (WE-LFRFND *         
08426                        (WE-LFPRM / (WE-LFPRM + WE-LFPRM-ALT))).   
08427                                                                   
08428      IF TEMP-7 NOT GREATER ZERO                                   
08429          IF CLAS-I-EP (CLAS-INDEXL) NOT = 'B'                     
08430              MOVE ZEROS             TO EPR-R78  EPR-PRO  EPR-ST   
08431              GO TO S-4190-EP-DO-LF-COMM                           
08432          ELSE                                                     
08433              MOVE ZEROS             TO TEMP-7.                    
08434                                                                   
08435      COMPUTE TEMP-1 = CR-LF-TERM * (CR-LF-TERM + 1).              
08436      COMPUTE TEMP-2 = REM-TRM1 * (REM-TRM1 + 1).                  
08437      IF DTE-R78 = '1'                                             
08438          COMPUTE TEMP-2 = REM-TRM1 * REM-TRM1.                    
08439                                                                   
08440      COMPUTE TEMP-4 ROUNDED = TEMP-2 / TEMP-1.                    
08441      COMPUTE TEMP-5 ROUNDED = REM-TRM1 / CR-LF-TERM.              
08442                                                                   
08443      COMPUTE EPR-R78 ROUNDED = TEMP-7 - (TEMP-7 * TEMP-4).        
08444                                                                   
08445      COMPUTE EPR-PRO ROUNDED = TEMP-7 - (TEMP-7 * TEMP-5).        
08446                                                                   
08447      IF CLAS-I-EP (CLAS-INDEXL) NOT = 'B'                         
08448          GO TO S-4185-EP-DO-LF-STATUTORY.                         
08449                                                                   
08450      MOVE EPR-R78                  TO EPR-ST.                     
08451                                                                   
08452      IF WE-LFPRM = ZERO  AND  WE-LFPRM-ALT = ZERO                 
08453          MOVE ZEROS                 TO TEMP-8                     
08454      ELSE                                                         
08455          COMPUTE TEMP-8 ROUNDED = WE-LFPRM-ALT - (WE-LFRFND *     
08456                      (WE-LFPRM-ALT / (WE-LFPRM + WE-LFPRM-ALT))). 
08457                                                                   
08458      IF TEMP-8 NOT GREATER ZERO                                   
08459          GO TO S-4185-EP-DO-LF-STATUTORY.                         
08460                                                                   
08461      COMPUTE EPR-R78 ROUNDED = EPR-R78 +                          
08462                               (TEMP-8 - (TEMP-8 * TEMP-4)).       
08463                                                                   
08464      COMPUTE EPR-PRO ROUNDED = EPR-PRO +                          
08465                               (TEMP-8 - (TEMP-8 * TEMP-5)).       
08466                                                                   
08467      COMPUTE EPR-ST ROUNDED = EPR-ST +                            
08468                              (TEMP-8 - (TEMP-8 * TEMP-5)).        
08469                                                                   
08470  S-4185-EP-DO-LF-STATUTORY.                                       
08471                                                                   
08472      IF STATE-ABBR (CLAS-INDEXS) = 'AL'                           
08473          MOVE EPR-R78                TO EPR-ST.                   
08474                                                                   
08475      IF STATE-ABBR (CLAS-INDEXS) = 'WY'                           
08476          MOVE EPR-PRO                TO EPR-ST.                   
08477                                                                   
08478      IF EPR-ST NOT = ZEROS                                        
08479          GO TO S-4190-EP-DO-LF-COMM.                              
08480                                                                   
08481      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'R'                          
08482          MOVE EPR-R78                TO EPR-ST                    
08483      ELSE                                                         
08484          MOVE EPR-PRO                TO EPR-ST.                   
08485                                                                   
08486  S-4190-EP-DO-LF-COMM.                                            
08487                                                                   
08488      IF EARNED-BY-CANCEL = ZEROS                                  
08489          GO TO S-4195-LF-COMM-CONTINUE.                           
08490                                                                   
08491      IF EPR-R78 GREATER THAN EARNED-BY-CANCEL                     
08492          MOVE EARNED-BY-CANCEL     TO EPR-R78.                    
08493                                                                   
08494      IF EPR-PRO GREATER THAN EARNED-BY-CANCEL                     
08495          MOVE EARNED-BY-CANCEL     TO EPR-PRO.                    
08496                                                                   
08497      IF EPR-ST  GREATER THAN EARNED-BY-CANCEL                     
08498          MOVE EARNED-BY-CANCEL     TO EPR-ST.                     
08499                                                                   
08500  S-4195-LF-COMM-CONTINUE.                                         
08501                                                                   
08502      MOVE +0 TO SUB1.                                             
08503      PERFORM S-4300-EP-COMM-LOOP THRU S-4399-EP-COMM-LOOP-X       
08504          10 TIMES.                                                
08505                                                                   
08506      COMPUTE EW-EPR-R78 = EW-EPR-R78 - EPR-R78.                   
08507      COMPUTE EW-EPR-PRO = EW-EPR-PRO - EPR-PRO.                   
08508      COMPUTE EW-EPR-ST  = EW-EPR-ST  - EPR-ST.                    
08509                                                                   
08510      COMPUTE EW-EPR-R78-ADJ = EW-EPR-R78-ADJ + EPR-R78.           
08511      COMPUTE EW-EPR-PRO-ADJ = EW-EPR-PRO-ADJ + EPR-PRO.           
08512      COMPUTE EW-EPR-ST-ADJ  = EW-EPR-ST-ADJ  + EPR-ST.            
08513                                                                   
08514      MOVE 'Y'          TO EW-LEVEL-USED.                          
08515                                                                   
08516      IF CR-POLICY-UNDERWRITTEN                                    
08517          MOVE EPEC-WORK       TO EPEC-LEVELS-II (SUB3)            
08518                                                                   
08519      ELSE                                                         
08520          MOVE EPEC-WORK       TO EPEC-LEVELS (SUB3).              
08521                                                                   
08522      GO TO S-4200-EP-DO-AH.                                       
08523                                                                   
08524  EJECT                                                            
08525 ******************************************************************
08526 ***  C A L C U L A T E   A & H   S T A R T   E A R N E D       ***
08527 ******************************************************************
08528                                                                   
08529  S-4200-EP-DO-AH.                                                 
08530                                                                   
08531      MOVE +0                         TO EPR-R78  EARNED-BY-CANCEL 
08532                                         EPR-PRO                   
08533                                         EPR-ST.                   
08534                                                                   
08535      IF CR-AHTYP = ZERO                                           
08536          GO TO S-4299-EPBX.                                       
08537                                                                   
08538      IF REIN-AH-FLG (SUB5) = SPACE                                
08539          GO TO S-4299-EPBX.                                       
08540                                                                   
08541      MOVE AH-OVERRIDE-L1       TO WE-LF-AH.                       
08542      MOVE CR-AHTYP             TO WE-BEN-TYPE.                    
08543      PERFORM 4400-EP-FIND-TAB-ENT THRU 4499-EP-FIND-TAB-ENT-X.    
08544                                                                   
08545      MOVE CR-AH-TERM           TO ORIG-TERM.                      
08546      MOVE CLAS-INDEXA          TO W-NDX.                          
08547                                                                   
08548      MOVE EARNING-START-DATE     TO VALUATION-DT.                 
08549      MOVE '9'                    TO W-VAL-SOURCE.                 
08550      PERFORM 8500-REMAINING-TERM-ROUTINE THRU 8599-REM-TERM-X.    
08551                                                                   
08552  S-4220-EP-DO-AH-CNCL.                                            
08553                                                                   
08554      IF CR-AH-STATUS-AT-CANCEL NOT = ' '                          
08555          COMPUTE EARNED-BY-CANCEL = WE-AHPRM - WE-AHRFND.         
08556                                                                   
08557  S-4230-EP-DO-AH-EP.                                              
08558                                                                   
08559      IF REM-TRM2 GREATER CR-AH-TERM                               
08560          MOVE +0 TO EPR-R78  EPR-PRO  EPR-ST                      
08561          GO TO S-4290-EP-DO-AH-COMM.                              
08562                                                                   
08563      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'Z'                      
08564          COMPUTE WE-AHPRM = WE-AHPRM - WE-AHRFND.                 
08565                                                                   
08566  S-4235-CHECK-OB-AH-NCL.                                          
08567                                                                   
08568      IF (DTE-CLIENT EQUAL 'NCL') AND                              
08569         (CLAS-I-CALC-TYPE (CLAS-INDEXA) EQUAL 'B')                
08570            MOVE WE-AHPRM         TO EPR-R78                       
08571                                     EPR-PRO                       
08572                                     EPR-ST                        
08573         IF (CR-CCYY EQUAL EOM-CCYY) AND                           
08574            (CR-MO EQUAL EOM-MO)                                   
08575            MOVE '*'            TO OB-DATE-1-SWITCH                
08576            MOVE EW-REIN-CO     TO WS-OB-REIN-CO (SUB3)            
08577            MOVE AH-OVERRIDE-L1 TO WS-OB-LF-AH (SUB3)              
08578            MOVE CR-AHTYP       TO WS-OB-BENEFIT-CODE (SUB3)       
08579            COMPUTE WS-EPR-R78 (SUB3 1) =                          
08580                    WS-EPR-R78 (SUB3 1) - (WE-AHPRM * .5)          
08581             GO TO S-4290-EP-DO-AH-COMM                            
08582         ELSE                                                      
08583         IF (CR-CCYY EQUAL OB-CCYY (1)) AND                        
08584            (CR-MO EQUAL OB-MO (1))                                
08585            MOVE '*'            TO OB-DATE-2-SWITCH                
08586            MOVE EW-REIN-CO     TO WS-OB-REIN-CO (SUB3)            
08587            MOVE AH-OVERRIDE-L1 TO WS-OB-LF-AH (SUB3)              
08588            MOVE CR-AHTYP       TO WS-OB-BENEFIT-CODE (SUB3)       
08589            COMPUTE WS-EPR-R78 (SUB3 2) =                          
08590                    WS-EPR-R78 (SUB3 2) - (WE-AHPRM * .5)          
08591             GO TO S-4290-EP-DO-AH-COMM                            
08592         ELSE                                                      
08593         IF (CR-CCYY EQUAL OB-CCYY (2)) AND                        
08594            (CR-MO EQUAL OB-MO (2))                                
08595            MOVE '*'            TO OB-DATE-3-SWITCH                
08596            MOVE EW-REIN-CO     TO WS-OB-REIN-CO (SUB3)            
08597            MOVE AH-OVERRIDE-L1 TO WS-OB-LF-AH (SUB3)              
08598            MOVE CR-AHTYP       TO WS-OB-BENEFIT-CODE (SUB3)       
08599            COMPUTE WS-EPR-R78 (SUB3 3) =                          
08600                    WS-EPR-R78 (SUB3 3) - (WE-AHPRM * .5)          
08601             GO TO S-4290-EP-DO-AH-COMM                            
08602         ELSE                                                      
08603             GO TO S-4290-EP-DO-AH-COMM.                           
08604                                                                   
08605      IF REM-TRM2 NOT GREATER +0                                   
08606          MOVE WE-AHPRM TO EPR-R78  EPR-PRO  EPR-ST                
08607          GO TO S-4290-EP-DO-AH-COMM.                              
08608                                                                   
08609      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'B'                      
08610          MOVE WE-AHPRM TO EPR-R78  EPR-PRO  EPR-ST                
08611          GO TO S-4290-EP-DO-AH-COMM.                              
08612                                                                   
08613      COMPUTE TEMP-1 = CR-AH-TERM * (CR-AH-TERM + 1).              
08614      COMPUTE TEMP-2 = REM-TRM1 * (REM-TRM1 + 1).                  
08615      IF DTE-R78 = '1'                                             
08616          COMPUTE TEMP-2 = REM-TRM1 * REM-TRM1.                    
08617      COMPUTE TEMP-4 ROUNDED = TEMP-2 / TEMP-1.                    
08618                                                                   
08619      IF DTE-CLIENT = 'MIC' OR 'MCC'                               
08620          COMPUTE TEMP-5 ROUNDED = (REM-TRM1 - .5) / CR-AH-TERM    
08621      ELSE                                                         
08622          COMPUTE TEMP-5 ROUNDED = REM-TRM1 / CR-AH-TERM.          
08623                                                                   
08624      COMPUTE EPR-R78 ROUNDED = WE-AHPRM * TEMP-4.                 
08625      COMPUTE EPR-R78 = WE-AHPRM - EPR-R78.                        
08626                                                                   
08627      COMPUTE EPR-PRO ROUNDED = WE-AHPRM * TEMP-5.                 
08628      COMPUTE EPR-PRO = WE-AHPRM - EPR-PRO.                        
08629                                                                   
08630      MOVE  CLAS-I-EP (CLAS-INDEXA) TO AH-EARN-METHOD.             
08631                                                                   
PEMMOD*    IF AM-EARN-METHOD-A NOT EQUAL SPACES AND ZEROS               
PEMMOD*        MOVE AM-EARN-METHOD-A TO AH-EARN-METHOD.                 
08634                                                                   
08635      MOVE SPACES TO STATUTORY-SWITCH-AH.                          
08636                                                                   
08637      IF (STATE-ABBR (CLAS-INDEXS) = 'VA') AND                     
08638         (CR-DT GREATER THAN  19921231)                            
08639          IF CR-AH-TERM GREATER THAN  +61                          
08640              MOVE 'A' TO AH-EARN-METHOD                           
08641              MOVE '*' TO STATUTORY-SWITCH-AH                      
08642              GO TO S-4245-EP-DO-AH-EP-AN                          
08643          ELSE                                                     
08644              GO TO S-4250-EP-DO-AH-STATUTORY.                     
08645                                                                   
08646      IF AH-EARN-METHOD EQUAL 'N'                                  
08647          GO TO S-4240-EP-DO-AH-EP-NP.                             
08648                                                                   
08649      IF AH-EARN-METHOD EQUAL 'A' OR 'C'                           
08650          MOVE 'A' TO AH-EARN-METHOD                               
08651          GO TO S-4245-EP-DO-AH-EP-AN.                             
08652                                                                   
08653      GO TO S-4250-EP-DO-AH-STATUTORY.                             
08654                                                                   
08655  S-4240-EP-DO-AH-EP-NP.                                           
08656                                                                   
08657      IF WE-AHPRM = ZERO                                           
08658          GO TO S-4250-EP-DO-AH-STATUTORY.                         
08659                                                                   
08660      IF CR-APR GREATER THAN +0                                    
08661          NEXT SENTENCE                                            
08662      ELSE                                                         
08663          GO TO S-4250-EP-DO-AH-STATUTORY.                         
08664                                                                   
08665      IF DTE-CLIENT = 'NCL'                                        
08666        IF CR-DT LESS THAN 19910101                                
08667            GO TO S-4250-EP-DO-AH-STATUTORY.                       
08668                                                                   
08669      IF REM-TRM1 = CR-AH-TERM                                     
08670          MOVE +0 TO EPR-R78  EPR-PRO  EPR-ST                      
08671          GO TO S-4290-EP-DO-AH-COMM.                              
08672                                                                   
08673      MOVE CR-APR TO NP-APR.                                       
08674      MOVE CR-AH-TERM TO NP-ORIG  NP-CAP.                          
08675      MOVE REM-TRM1 TO NP-REM.                                     
08676      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXA) TO NP-OPT.               
08677      IF NP-OPT = 'T' OR 'U' OR 'V' OR 'W' OR 'X'                  
08678          MOVE CR-LOAN-TERM TO NP-ORIG.                            
042904     MOVE 'R' TO NP-OPT.                                          
08680      CALL 'ECSNETRM'                                              
08681          USING NP-APR NP-ORIG NP-REM NP-OPT NP-CAP NP-FACTOR.     
08682                                                                   
08683      COMPUTE EPR-R78 ROUNDED = WE-AHPRM - (NP-FACTOR * WE-AHPRM). 
08684                                                                   
08685      GO TO S-4250-EP-DO-AH-STATUTORY.                             
08686                                                                   
08687  S-4245-EP-DO-AH-EP-AN.                                           
08688      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
08689      MOVE 'L'                        TO DC-OPTION-CODE.           
08690      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
08691      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
08692      MOVE CLAS-I-RL-AH (CLAS-INDEXA) TO CP-BENEFIT-TYPE.          
08693      MOVE SPACES                     TO CP-ACCT-FLD-5.            
08694      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
08695      MOVE STATE-SUB (CLAS-INDEXS)    TO CP-STATE.                 
08696      MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV.       
08697      MOVE CR-AGE                     TO CP-ISSUE-AGE.             
08698      MOVE REM-TRM1                   TO CP-ORIGINAL-TERM.         
08699      MOVE CR-AHAMT                   TO CP-ORIGINAL-BENEFIT       
08700                                         CP-RATING-BENEFIT-AMT.    
08701      IF CP-STATE-STD-ABBRV = 'OR'                                 
08702          COMPUTE CP-RATING-BENEFIT-AMT =                          
08703                                   CR-AHAMT * REM-TRM1.            
08704      MOVE CR-APR                     TO CP-LOAN-APR.              
08705      MOVE CR-PMT-FREQ                TO CP-PAY-FREQUENCY.         
08706      MOVE AH-EARN-METHOD             TO CP-EARNING-METHOD.        
08707      MOVE '3'                        TO CP-PROCESS-TYPE.          
08708      MOVE CLAS-I-BAL (CLAS-INDEXA)   TO CP-SPECIAL-CALC-CD.       
08709      MOVE CR-LOAN-TERM               TO CP-LOAN-TERM.             
08710                                                                   
08711      IF CR-RATING-CLASS NOT = SPACE AND ZERO                      
08712          MOVE CR-RATING-CLASS        TO CP-CLASS-CODE             
08713      ELSE                                                         
08714          MOVE AM-CAL-TABLE           TO CP-CLASS-CODE.            
08715                                                                   
08716      MOVE AM-AH-DEVIATION            TO CP-DEVIATION-CODE.        
08717      MOVE CR-AHTYP                   TO CP-BENEFIT-CD.            
08718      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
08719      MOVE CR-PMT-EXTENSION-DAYS      TO CP-TERM-OR-EXT-DAYS.      
08720      MOVE AH-OVERRIDE-L1             TO CP-AH-OVERRIDE-CODE.      
08721      MOVE AM-AH-DEVIATION-PCT        TO CP-RATE-DEV-PCT.          
08722                                                                   
08723      PERFORM 8100-GET-RATE THRU 8199-GET-RATE-X.                  
08724                                                                   
08725      IF CP-ERROR-RATE-NOT-FOUND  OR                               
08726         CP-ERROR-RATE-IS-ZERO  OR                                 
08727         CP-ERROR-IN-DATES                                         
08728          GO TO S-4250-EP-DO-AH-STATUTORY.                         
08729                                                                   
08730      COMPUTE EPR-R78 ROUNDED =                                    
08731              ((REM-TRM1 * WE-AHAMT) / +100) *                     
08732                CP-PREMIUM-RATE.                                   
08733                                                                   
08734      COMPUTE EPR-R78 = WE-AHPRM - EPR-R78.                        
08735                                                                   
08736      IF STATUTORY-REQUIREMENT-AH                                  
08737          MOVE EPR-R78                TO EPR-ST                    
08738          GO TO S-4290-EP-DO-AH-COMM.                              
08739                                                                   
08740  S-4250-EP-DO-AH-STATUTORY.                                       
08741      MOVE EPR-R78                    TO EPR-ST.                   
08742                                                                   
08743      MOVE +0                         TO SUB1.                     
08744                                                                   
08745  S-4255-EP-AH-STAT-LOOP.                                          
08746      ADD +1 TO SUB1.                                              
08747                                                                   
08748      IF STATE-ABBR (CLAS-INDEXS) = AH-RES-STATE-ENT (SUB1)        
08749          MOVE EPR-PRO                TO EPR-ST                    
08750      ELSE                                                         
08751          IF AH-RES-STATE-ENT (SUB1) NOT = HIGH-VALUES             
08752              GO TO S-4255-EP-AH-STAT-LOOP.                        
08753                                                                   
08754      IF (STATE-ABBR (CLAS-INDEXS) = 'OH'  AND                     
08755          CR-DT GREATER THAN 19831031)                             
08756                     OR                                            
08757         (STATE-ABBR (CLAS-INDEXS) = 'OK'  AND                     
08758          CR-DT GREATER THAN 19820629)                             
08759                     OR                                            
08760         (STATE-ABBR (CLAS-INDEXS) = 'TX'  AND                     
08761          CR-DT GREATER THAN 19830831)                             
08762              COMPUTE EPR-ST = (EPR-R78 + EPR-PRO) / 2.            
08763                                                                   
08764      IF DTE-CLIENT = 'CSO'  OR  'CID'                             
08765        IF STATE-ABBR (CLAS-INDEXS) = 'CO'                         
08766          IF CR-DT LESS THAN 19970101                              
08767              MOVE EPR-R78            TO EPR-ST                    
08768          ELSE                                                     
08769              COMPUTE EPR-ST = (EPR-R78 + EPR-PRO) / 2.            
08770                                                                   
08771  S-4290-EP-DO-AH-COMM.                                            
08772                                                                   
08773      IF EARNED-BY-CANCEL = ZEROS                                  
08774          GO TO S-4295-AH-COMM-CONTINUE.                           
08775                                                                   
08776      IF EPR-R78 GREATER THAN EARNED-BY-CANCEL                     
08777          MOVE EARNED-BY-CANCEL     TO EPR-R78.                    
08778                                                                   
08779      IF EPR-PRO GREATER THAN EARNED-BY-CANCEL                     
08780          MOVE EARNED-BY-CANCEL     TO EPR-PRO.                    
08781                                                                   
08782      IF EPR-ST  GREATER THAN EARNED-BY-CANCEL                     
08783          MOVE EARNED-BY-CANCEL     TO EPR-ST.                     
08784                                                                   
08785  S-4295-AH-COMM-CONTINUE.                                         
08786                                                                   
08787      MOVE +0           TO SUB1.                                   
08788      PERFORM S-4300-EP-COMM-LOOP THRU S-4399-EP-COMM-LOOP-X       
08789          10 TIMES.                                                
08790                                                                   
08791      COMPUTE EW-EPR-R78 = EW-EPR-R78 - EPR-R78.                   
08792      COMPUTE EW-EPR-PRO = EW-EPR-PRO - EPR-PRO.                   
08793      COMPUTE EW-EPR-ST  = EW-EPR-ST  - EPR-ST.                    
08794                                                                   
08795      COMPUTE EW-EPR-R78-ADJ = EW-EPR-R78-ADJ + EPR-R78.           
08796      COMPUTE EW-EPR-PRO-ADJ = EW-EPR-PRO-ADJ + EPR-PRO.           
08797      COMPUTE EW-EPR-ST-ADJ  = EW-EPR-ST-ADJ  + EPR-ST.            
08798                                                                   
08799      MOVE 'Y'          TO EW-LEVEL-USED.                          
08800                                                                   
08801      IF CR-POLICY-UNDERWRITTEN                                    
08802          MOVE EPEC-WORK       TO EPEC-LEVELS-II (SUB3)            
08803      ELSE                                                         
08804          MOVE EPEC-WORK       TO EPEC-LEVELS (SUB3).              
08805                                                                   
08806                                                                   
08807  S-4299-EPBX.                                                     
08808      EXIT.                                                        
08809  EJECT                                                            
08810 ******************************************************************
08811 ***    C A L C U L A T E   S T A R T   E A R N E D   C O M M   ***
08812 ******************************************************************
08813                                                                   
08814  S-4300-EP-COMM-LOOP.                                             
08815                                                                   
08816      ADD +1 TO SUB1.                                              
08817      IF CR-COM-AGT (SUB1) = SPACES OR ZEROS                       
08818          GO TO S-4399-EP-COMM-LOOP-X.                             
08819                                                                   
08820      IF EW-LF-AH = AH-OVERRIDE-L1                                 
08821          GO TO S-4350-EP-COMM-LOOP-A.                             
08822                                                                   
100703*    IF CR-AGT-TYPE (SUB1) = 'K'
100703*       COMPUTE COMM-WK = 
08823      COMPUTE COMM-WK ROUNDED = EPR-R78 * CR-LCOM-L (SUB1).        
08824      COMPUTE EW-ECR-R78 (SUB1) = EW-ECR-R78 (SUB1) - COMM-WK.     
08825      COMPUTE EW-ECR-R78-ADJ (SUB1) =                              
08826                                  EW-ECR-R78-ADJ (SUB1) + COMM-WK. 
08827                                                                   
08828      COMPUTE COMM-WK ROUNDED = EPR-PRO * CR-LCOM-L (SUB1).        
08829      COMPUTE EW-ECR-PRO (SUB1) = EW-ECR-PRO (SUB1) - COMM-WK.     
08830      COMPUTE EW-ECR-PRO-ADJ (SUB1) =                              
08831                                  EW-ECR-PRO-ADJ (SUB1) + COMM-WK. 
08832                                                                   
08833      COMPUTE COMM-WK ROUNDED = EPR-ST * CR-LCOM-L (SUB1).         
08834      COMPUTE EW-ECR-ST (SUB1) = EW-ECR-ST (SUB1) - COMM-WK.       
08835                                                                   
08836      GO TO S-4399-EP-COMM-LOOP-X.                                 
08837                                                                   
08838  S-4350-EP-COMM-LOOP-A.                                           
08839                                                                   
08840      COMPUTE COMM-WK ROUNDED = EPR-R78 * CR-LCOM-AH (SUB1).       
08841      COMPUTE EW-ECR-R78 (SUB1) = EW-ECR-R78 (SUB1) - COMM-WK.     
08842      COMPUTE EW-ECR-R78-ADJ (SUB1) =                              
08843                                  EW-ECR-R78-ADJ (SUB1) + COMM-WK. 
08844                                                                   
08845      COMPUTE COMM-WK ROUNDED = EPR-PRO * CR-LCOM-AH (SUB1).       
08846      COMPUTE EW-ECR-PRO (SUB1) = EW-ECR-PRO (SUB1) - COMM-WK.     
08847      COMPUTE EW-ECR-PRO-ADJ (SUB1) =                              
08848                                  EW-ECR-PRO-ADJ (SUB1) + COMM-WK. 
08849                                                                   
08850      COMPUTE COMM-WK ROUNDED = EPR-ST * CR-LCOM-AH (SUB1).        
08851      COMPUTE EW-ECR-ST (SUB1) = EW-ECR-ST (SUB1) - COMM-WK.       
08852                                                                   
08853  S-4399-EP-COMM-LOOP-X.                                           
08854      EXIT.                                                        
08855                                                                   
08856 ******************************************************************
08857 ******************************************************************
08858 ******************************************************************
08859 ******************************************************************
08860 ******************************************************************
08861 ******************************************************************
08862 ******************************************************************
08863 ******************************************************************
08864 ******************************************************************
08865 ******************************************************************
08866  EJECT                                                            
08867 ******************************************************************
08868 ***     F I N D   R E I N S U R A N C E   T A B L E   S U B    ***
08869 ******************************************************************
08870                                                                   
08871  4400-EP-FIND-TAB-ENT.                                            
08872                                                                   
08873      IF WE-REIN-CO = EPEC-SUB-REIN (SUB9)                         
08874          GO TO 4490-EP-FIND-TAB-SUB.                              
08875      MOVE +1 TO SUB9.                                             
08876                                                                   
08877  4450-EP-FIND-TAB-LOOP.                                           
08878                                                                   
08879      IF WE-REIN-CO = EPEC-SUB-REIN (SUB9)                         
08880          GO TO 4490-EP-FIND-TAB-SUB.                              
08881      IF EPEC-SUB-REIN (SUB9) = ZEROS                              
08882          MOVE WE-REIN-CO TO EPEC-SUB-REIN (SUB9)                  
08883          GO TO 4490-EP-FIND-TAB-SUB.                              
08884                                                                   
08885      ADD +1 TO SUB9.                                              
08886      IF SUB9 GREATER THAN +31                                     
08887          DISPLAY 'EPEC SUBSCRIPT TABLE FULL ' AM-MSTR-CNTRL       
08888          MOVE 0201 TO WS-ABEND-CODE                               
CIDMOD         DISPLAY '******************************'
CIDMOD         DISPLAY '***** ERROR LOCATION 020 *****'
CIDMOD         DISPLAY '******************************'
08889          MOVE WS-ABEND-CODE        TO WS-RETURN-CODE              
08890          GO TO ABEND-PGM.                                         
08891                                                                   
08892      GO TO 4450-EP-FIND-TAB-LOOP.                                 
08893                                                                   
08894  4490-EP-FIND-TAB-SUB.                                            
08895                                                                   
08896      IF WE-LF-AH = LIFE-OVERRIDE-L1                               
08897          IF EPEC-SUB-LF (SUB9 CLAS-INDEXL) = ZERO                 
08898              ADD +1 TO SUB10                                      
08899              MOVE SUB10 TO SUB3                                   
08900              MOVE SUB10 TO EPEC-SUB-LF (SUB9 CLAS-INDEXL)         
08901          ELSE                                                     
08902              MOVE EPEC-SUB-LF (SUB9 CLAS-INDEXL) TO SUB3.         
08903                                                                   
08904      IF WE-LF-AH = AH-OVERRIDE-L1                                 
08905          IF EPEC-SUB-AH (SUB9 CLAS-INDEXA) = ZERO                 
08906              ADD +1 TO SUB10                                      
08907              MOVE SUB10 TO SUB3                                   
08908              MOVE SUB10 TO EPEC-SUB-AH (SUB9 CLAS-INDEXA)         
08909          ELSE                                                     
08910              MOVE EPEC-SUB-AH (SUB9 CLAS-INDEXA) TO SUB3.         
08911                                                                   
CIDMOD*    IF SUB3 GREATER THAN +220                                    
092602*    IF SUB3 GREATER THAN +300                                    
092602     IF SUB3 GREATER THAN +900                                    
CIDMOD         DISPLAY 'SUB3 VALUE IS - ' SUB3                          
08913          DISPLAY 'EPEC WORK TABLE FULL ' AM-MSTR-CNTRL            
08914          MOVE 0202 TO WS-ABEND-CODE                               
CIDMOD         DISPLAY '******************************'
CIDMOD         DISPLAY '***** ERROR LOCATION 021 *****'
CIDMOD         DISPLAY '******************************'
08915          MOVE WS-ABEND-CODE        TO WS-RETURN-CODE              
08916          GO TO ABEND-PGM.                                         
08917                                                                   
08918      IF CR-POLICY-UNDERWRITTEN                                    
08919          MOVE EPEC-LEVELS-II (SUB3)  TO EPEC-WORK                 
08920          MOVE EPEC-UNDW-OPT-DATA-TABLE                            
08921                                      TO EPEC-OPT-WORK-TABLE       
08922      ELSE                                                         
08923          MOVE EPEC-LEVELS (SUB3)     TO EPEC-WORK                 
08924          MOVE EPEC-OPT-DATA-TABLE    TO EPEC-OPT-WORK-TABLE.      
08925                                                                   
08926      MOVE WE-REIN-CO         TO EW-REIN-CO                        
08927                                 EPEC-REIN-CO (SUB3)               
08928                                 EPEC-REIN-CO-II (SUB3).           
08929      MOVE WE-LF-AH           TO EW-LF-AH                          
08930                                 EPEC-LF-AH (SUB3)                 
08931                                 EPEC-LF-AH-II (SUB3).             
08932      MOVE WE-BEN-TYPE        TO EW-BEN-TYPE                       
08933                                 EPEC-BEN-TYPE (SUB3)              
08934                                 EPEC-BEN-TYPE-II (SUB3).          
08935                                                                   
08936  4499-EP-FIND-TAB-ENT-X.                                          
08937      EXIT.                                                        
08938  EJECT                                                            
08939 ******************************************************************
08940 ***         B U I L D   S U M M A R Y   E X T R A C T S        ***
08941 ******************************************************************
08942                                                                   
08943  4500-BUILD-EPEC-EXTRACT.                                         
08944                                                                   
102204     IF DTE-CLIENT = 'DCC'
102204        PERFORM 4541-CALC-IBNR   THRU 4541-EXIT
102204     END-IF

08945      PERFORM 4510-BUILD-EPEC-EXT THRU 4519-BUILD-EPEC-EXT-X       
08946          VARYING SUB2 FROM +1 BY +1                               
08947              UNTIL EPEC-REIN-CO (SUB2) = ZEROS.                   
08948                                                                   
08949      PERFORM 4550-CLEAR-EPEC-TOTALS THRU 4589-CLEAR-EPEC-TOTALS-X.
08950                                                                   
08951      GO TO 4599-BUILD-EPEC-EXTRACT-X.                             
08952                                                                   
08953                                                                   
08954  4510-BUILD-EPEC-EXT.                                             
08955                                                                   
08956      MOVE EPEC-LEVELS (SUB2)            TO EPEC-WORK.             
08957      MOVE EPEC-OPT-DATA-TABLE           TO EPEC-OPT-WORK-TABLE.   
08958      IF EW-LEVEL-USED = 'Y'                                       
08959          MOVE SPACE                     TO W-UNDERWRITING-SW      
08960          PERFORM 4511-BUILD-EPEC-RECORD THRU                      
08961                  4518-BUILD-EPEC-RCD-X.                           
08962                                                                   
08963      MOVE EPEC-LEVELS-II (SUB2)         TO EPEC-WORK.             
08964      MOVE EPEC-UNDW-OPT-DATA-TABLE      TO EPEC-OPT-WORK-TABLE.   
08965      IF EW-LEVEL-USED = 'Y'                                       
08966          MOVE '*'                       TO W-UNDERWRITING-SW      
08967          PERFORM 4511-BUILD-EPEC-RECORD THRU                      
08968                  4518-BUILD-EPEC-RCD-X.                           
08969                                                                   
08970      GO TO 4519-BUILD-EPEC-EXT-X.                                 
08971                                                                   
08972  4511-BUILD-EPEC-RECORD.                                          
08973                                                                   
08974      MOVE SPACES                        TO EP-RECORD.             
08975      MOVE 'EP'                          TO EP-RECORD-ID.          
08976      MOVE DTE-CLASIC-COMPANY-CD         TO EP-COMPANY-CD.         
08977      MOVE AM-MSTR-CNTRL                 TO EP-CONTROL.            
08978      MOVE AM-EXPIRE-DT                  TO WS-EP-EXP-DTE-N.       
08979      MOVE AM-EFFECT-DT                  TO EP-EFF-DTE             
08980                                            WS-EP-EFF-DTE-N.       
08981      MOVE EW-REIN-CO                    TO EP-REI-CO.             
08982      IF EP-REI-CO NOT = SPACES                                    
08983          MOVE 'R'                       TO EP-REIN.               
08984      MOVE EW-LF-AH                      TO EP-RCD-TYPE.           
08985      MOVE EW-BEN-TYPE                   TO EP-BEN-CODE.           
08986                                                                   
08987      MOVE EW-ISS-CNT                    TO EP-ISS-CNT.            
08988      MOVE EW-ISS-BEN                    TO EP-ISS-BEN.            
08989      MOVE EW-ISS-BEN-GROSS              TO EP-ISS-BEN-GROSS.      
08990                                                                   
08991      MOVE EW-CNC-CNT                    TO EP-CNC-CNT.            
08992      MOVE EW-CNC-BEN                    TO EP-CNC-BEN.            
08993      MOVE EW-CNC-BEN-GROSS              TO EP-CNC-BEN-GROSS.      
08994                                                                   
08995      MOVE EW-ISS-PRM                    TO EP-ISS-PRM.            
08996      MOVE EW-ISS-PRM-GROSS              TO EP-ISS-PRM-GROSS.      
08997      MOVE EW-CNC-PRM                    TO EP-CNC-PRM.            
08998      MOVE EW-CNC-PRM-GROSS              TO EP-CNC-PRM-GROSS.      
08999                                                                   
09000      IF DTE-CLIENT NOT = 'NCL'                                    
09001          GO TO 4512-BUILD-EPEC-CONTINUE.                          
09002                                                                   
09003      IF W-UNDERWRITTEN-DATA                                       
09004          GO TO 4512-BUILD-EPEC-CONTINUE.                          
09005                                                                   
09006      IF AM-EXPIRE-DT NOT GREATER THAN EOM-RUN-DATE                
09007          GO TO 4512-BUILD-EPEC-CONTINUE.                          
09008                                                                   
09009      IF WS-OB-REIN-CO (SUB2) = EW-REIN-CO  AND                    
09010         WS-OB-LF-AH (SUB2) = EW-LF-AH  AND                        
09011         WS-OB-BENEFIT-CODE (SUB2) = EW-BEN-TYPE                   
09012          NEXT SENTENCE                                            
09013      ELSE                                                         
09014          GO TO 4512-BUILD-EPEC-CONTINUE.                          
09015                                                                   
09016      IF OB-DATE-1-USED  OR                                        
09017         OB-DATE-2-USED  OR                                        
09018         OB-DATE-3-USED                                            
09019          NEXT SENTENCE                                            
09020      ELSE                                                         
09021          GO TO 4512-BUILD-EPEC-CONTINUE.                          
09022                                                                   
09023      IF OB-DATE-1-USED                                            
09024          IF WS-EPR-R78 (SUB2 1) NOT EQUAL +0                      
09025              COMPUTE EW-EPR-R78 = EW-ISS-PRM - EW-CNC-PRM -       
09026                            EW-EPR-R78-ADJ - WS-EPR-R78 (SUB2 1)   
09027              MOVE EW-EPR-R78    TO EW-EPR-PRO                     
09028                                    EW-EPR-ST                      
09029              GO TO 4512-BUILD-EPEC-CONTINUE                       
09030          ELSE                                                     
09031              GO TO 4512-BUILD-EPEC-CONTINUE.                      
09032                                                                   
09033      IF OB-DATE-2-USED                                            
09034          IF WS-EPR-R78 (SUB2 2) NOT EQUAL +0                      
09035              COMPUTE EW-EPR-R78 = EW-ISS-PRM - EW-CNC-PRM -       
09036                            EW-EPR-R78-ADJ - WS-EPR-R78 (SUB2 2)   
09037              MOVE EW-EPR-R78    TO EW-EPR-PRO                     
09038                                    EW-EPR-ST                      
09039              GO TO 4512-BUILD-EPEC-CONTINUE                       
09040          ELSE                                                     
09041              GO TO 4512-BUILD-EPEC-CONTINUE.                      
09042                                                                   
09043      IF OB-DATE-3-USED                                            
09044          IF WS-EPR-R78 (SUB2 3) NOT EQUAL +0                      
09045              COMPUTE EW-EPR-R78 = EW-ISS-PRM - EW-CNC-PRM -       
09046                            EW-EPR-R78-ADJ - WS-EPR-R78 (SUB2 3)   
09047              MOVE EW-EPR-R78    TO EW-EPR-PRO                     
09048                                    EW-EPR-ST                      
09049              GO TO 4512-BUILD-EPEC-CONTINUE                       
09050          ELSE                                                     
09051              GO TO 4512-BUILD-EPEC-CONTINUE.                      
09052                                                                   
09053  4512-BUILD-EPEC-CONTINUE.                                        
09054                                                                   
09055      MOVE EW-EPR-R78                    TO EP-PRM-78.             
09056      MOVE EW-EPR-PRO                    TO EP-PRM-PR.             
09057      MOVE EW-EPR-ST                     TO EP-PRM-ST.             
09058                                                                   
09059      MOVE EW-EPR-R78-ADJ                TO EP-PRM-78-ADJ.         
09060      MOVE EW-EPR-PRO-ADJ                TO EP-PRM-PR-ADJ.         
09061      MOVE EW-EPR-ST-ADJ                 TO EP-PRM-ST-ADJ.         
09062                                                                   
PEMMOD     MOVE EW-ISS-PRM-TAX                TO EP-PRM-TAX
PEMMOD
09063      MOVE EW-CLM-AMT                    TO EP-CLM-AMT.            
09064      MOVE EW-CLM-CNT                    TO EP-CLM-CNT.            
09065      MOVE EW-CLM-CRT                    TO EP-CLM-CRT.            
09066                                                                   
09067      MOVE EW-CLM-PAYCUR                 TO EP-CLM-DU.             
09068      MOVE ZEROS                         TO EP-CLM-PV.             
09069                                                                   
09070      IF EP-RCD-TYPE = 'A' OR 'U'                                  
09071          IF CARRIER-UEP-PCT (CLAS-INDEXCN) NOT = ZEROS            
09072              PERFORM 4599A-CALC-IBNR THRU 4599A-CALC-EXIT.        
09073                                                                   
09074      MOVE EW-CLM-IBNR                   TO EP-CLM-IBNR.           
09075                                                                   
09076      MOVE EW-CLM-FUTRSV                 TO EP-LOSS-RESV.          
09077      MOVE ZEROS                         TO EP-CLAIM-ADJ.          
09078                                                                   
09079      MOVE EW-INFORCE-CNT                TO EP-INFORCE-CNT.        
09080                                                                   
09081      IF EW-ISS-CNT = ZERO                                         
09082          MOVE ZERO                      TO EP-AVG-ORIG-TERM       
09083                                            EP-AVG-AGE             
09084        ELSE                                                       
09085          COMPUTE EP-AVG-ORIG-TERM ROUNDED =                       
09086                                 EW-ORIG-TERM / EW-ISS-CNT         
09087          COMPUTE EP-AVG-AGE ROUNDED = EW-AGE / EW-ISS-CNT.        
09088                                                                   
09089      IF EW-INFORCE-CNT = ZERO                                     
09090          MOVE ZERO                      TO EP-AVG-REM-TERM        
09091        ELSE                                                       
09092          COMPUTE EP-AVG-REM-TERM ROUNDED =                        
09093                              EW-REM-TERM / EW-INFORCE-CNT.        
09094                                                                   
09095      IF EW-ISS-BEN = ZERO                                         
09096          MOVE ZERO                      TO EP-WEIGHTED-ORIG-TERM  
09097                                            EP-WEIGHTED-AGE        
09098        ELSE                                                       
09099          COMPUTE EP-WEIGHTED-ORIG-TERM ROUNDED =                  
09100                                   EW-TERM-BEN / EW-ISS-BEN        
09101          COMPUTE EP-WEIGHTED-AGE ROUNDED =                        
09102                                    EW-AGE-BEN / EW-ISS-BEN.       
09103                                                                   
09104      MOVE ZEROS                         TO                        
09105          EP-RETRO-EXPENSES  EP-RETRO-PAYMENTS  EP-RETRO-OTH-COMM  
09106          EP-MORT-RESV       EP-IN-FORCE        EP-ADJUST.         
09107                                                                   
09108      MOVE EW-COV-DTE                    TO EP-HI-COV-DT           
09109                                            WS-EP-HI-COV-DT-N.     
09110      MOVE HIGH-CERT                     TO EP-HI-CERT             
09111                                            WS-EP-HI-CERT-N.       
09112      MOVE LOW-CERT                      TO EP-LO-CERT             
09113                                            WS-EP-LO-CERT-N.       
09114      IF DTE-PGM-OPT = '3'                                         
09115          MOVE 'P'                       TO EP-PURGE.              
09116                                                                   
09117      IF  DTE-OPT-RESERVE-METHOD-UNAUTH                            
09118          GO TO 4514-CONTINUE.                                     
09119                                                                   
09120      MOVE EPEC-CLM-EXP (SUB2)    TO EP-CLM-EXP.                   
09121                                                                   
09122      IF  EPEC-LIFE-CTR-NOT-USED (SUB2)                            
09123          MOVE +1                 TO EP-LIFE-YEARS                 
09124      ELSE                                                         
09125          MOVE ZEROS              TO W-LIFE-YEARS                  
09126          PERFORM 4515-DETERMINE-LIFE-YEARS THRU 4515-EXIT         
09127                  VARYING                                          
09128              SUBM FROM +1 BY +1                                   
09129                  UNTIL                                            
09130              SUBM GREATER THAN +13                                
09131          COMPUTE W-LIFE-YEARS-INT ROUNDED                         
09132              = W-LIFE-YEARS / 12                                  
09133              - (EPEC-LIFE-CTR (SUB2 1)                            
09134              + EPEC-LIFE-CTR (SUB2 13)) / 24                      
09135          COMPUTE EP-LIFE-YEARS ROUNDED = W-LIFE-YEARS-INT         
09136          IF  EP-LIFE-YEARS LESS THAN +1                           
09137              MOVE +1             TO EP-LIFE-YEARS.                
09138                                                                   
09139  4514-CONTINUE.                                                   
09140                                                                   
09141      PERFORM 4900-WRITE-SUMMARY-EXTRACTS THRU 4999-WRT-SUMMARY-X. 
09142                                                                   
09143      MOVE SPACES                        TO EC-RECORD.             
09144      MOVE '1'                           TO WS-SEQ-NBR.            
09145      MOVE +0                            TO SUB3.                  
09146      MOVE +0                            TO SUB4.                  
09147      PERFORM 4520-BUILD-EPEC-LOAD-COMM                            
09148          THRU 4539-BUILD-EPEC-LOAD-COMM-X                         
09149          UNTIL SUB4  GREATER THAN WS-LOOP-MAX.                    
09150                                                                   
09151      GO TO 4518-BUILD-EPEC-RCD-X.                                 
09152                                                                   
09153  4515-DETERMINE-LIFE-YEARS.                                       
09154                                                                   
09155      ADD EPEC-LIFE-CTR (SUB2 SUBM)      TO W-LIFE-YEARS.          
09156                                                                   
09157  4515-EXIT.                                                       
09158      EXIT.                                                        
09159                                                                   
09160  4518-BUILD-EPEC-RCD-X.                                           
09161      EXIT.                                                        
09162                                                                   
09163  4519-BUILD-EPEC-EXT-X.                                           
09164      EXIT.                                                        
09165                                                                   
09166  4520-BUILD-EPEC-LOAD-COMM.                                       
09167                                                                   
09168      PERFORM 4545-BUILD-BASE  THRU  4545-BUILD-BASE-X.            
09169                                                                   
09170      ADD +1 TO SUB3  SUB4.                                        
09171      MOVE ZEROS                  TO LEVELS-BUILT.                 
09172                                                                   
09173  4530-BUILD-EPEC-LOAD-COMM-LOOP.                                  
09174                                                                   
09175      IF SUB4 GREATER THAN WS-LOOP-MAX                             
09176          IF AGT-SW = 'X'                                          
09177              PERFORM 4900-WRITE-SUMMARY-EXTRACTS                  
09178                  THRU 4999-WRT-SUMMARY-X                          
09179              MOVE ' '            TO AGT-SW                        
09180              MOVE WS-LOOP-MAX    TO  SUB4                         
09181              GO TO 4539-BUILD-EPEC-LOAD-COMM-X                    
09182          ELSE                                                     
09183              GO TO 4539-BUILD-EPEC-LOAD-COMM-X.                   
09184                                                                   
09185      IF LEVELS-BUILT EQUAL +5                                     
09186          IF AGT-SW = 'X'                                          
09187              PERFORM 4900-WRITE-SUMMARY-EXTRACTS                  
09188                  THRU 4999-WRT-SUMMARY-X                          
09189              MOVE ' '            TO AGT-SW                        
09190              MOVE ZEROS          TO LEVELS-BUILT                  
09191              ADD 1 TO WS-SEQ-NBR                                  
09192              PERFORM 4545-BUILD-BASE  THRU  4545-BUILD-BASE-X     
09193              PERFORM 4540-BUILD-EPEC-CLEAR  THRU                  
09194                      4540-BUILD-CLEAR-X                           
09195                  VARYING SUB3 FROM +1 BY +1                       
09196                  UNTIL SUB3 GREATER THAN +5                       
09197              MOVE +1             TO SUB3                          
09198              GO TO 4531-BUILD-EPEC-LOAD-COMM                      
09199          ELSE                                                     
09200              MOVE +1             TO SUB3                          
09201              MOVE ZEROS          TO LEVELS-BUILT                  
09202              GO TO 4531-BUILD-EPEC-LOAD-COMM.                     
09203                                                                   
09204      IF SUB4 GREATER THAN WS-LOOP-MAX                             
09205          IF AGT-SW = 'X'                                          
09206              PERFORM 4900-WRITE-SUMMARY-EXTRACTS                  
09207                  THRU 4999-WRT-SUMMARY-X                          
09208              MOVE ' '                   TO AGT-SW                 
09209              GO TO 4539-BUILD-EPEC-LOAD-COMM-X                    
09210          ELSE                                                     
09211              GO TO 4539-BUILD-EPEC-LOAD-COMM-X.                   
09212                                                                   
09213  4531-BUILD-EPEC-LOAD-COMM.                                       
09214                                                                   
CIDMOD     IF (EW-AGT-NO (SUB4) = SPACES)                               
CIDMOD                  OR
CIDMOD        ((EW-ISS-COM (SUB4) = ZEROS) AND
CIDMOD         (EW-CNC-COM (SUB4) = ZEROS) AND
CIDMOD         (EW-ECR-R78 (SUB4) = ZEROS) AND
CIDMOD         (EW-ECR-R78-ADJ (SUB4) = ZEROS) AND
CIDMOD         (EW-ECR-PRO (SUB4) = ZEROS) AND
CIDMOD         (EW-ECR-PRO-ADJ (SUB4) = ZEROS) AND
CIDMOD         (EW-ECR-ST (SUB4) = ZEROS))
09216          PERFORM 4540-BUILD-EPEC-CLEAR                            
09217              THRU 4540-BUILD-CLEAR-X                              
09218          ADD +1 TO SUB4                                           
09219          GO TO 4530-BUILD-EPEC-LOAD-COMM-LOOP.                    
09220                                                                   
09221      MOVE 'X'                           TO AGT-SW.                
09222      MOVE EW-AGT-NO (SUB4)              TO EC-AGT-NO (SUB3).      
09223      MOVE EW-AGT-TYPE (SUB4)            TO EC-AGT-TYPE (SUB3).    
09224      MOVE EW-ISS-COM (SUB4)             TO EC-ISS-COMM (SUB3).    
09225      MOVE EW-CNC-COM (SUB4)             TO EC-CNC-COMM (SUB3).    
09226      MOVE EW-ECR-R78 (SUB4)             TO EC-COMM-78 (SUB3).     
09227      MOVE EW-ECR-PRO (SUB4)             TO EC-COMM-PR (SUB3).     
09228      MOVE EW-ECR-ST (SUB4)              TO EC-COMM-ST (SUB3).     
09229      MOVE EW-ECR-R78-ADJ (SUB4)         TO EC-COMM-78-ADJ (SUB3). 
09230      MOVE EW-ECR-PRO-ADJ (SUB4)         TO EC-COMM-PR-ADJ (SUB3). 
09231                                                                   
09232      ADD +1 TO SUB3 SUB4 LEVELS-BUILT.                            
09233                                                                   
09234      GO TO 4530-BUILD-EPEC-LOAD-COMM-LOOP.                        
09235                                                                   
09236  4539-BUILD-EPEC-LOAD-COMM-X.                                     
09237      EXIT.                                                        
09238                                                                   
09239  4540-BUILD-EPEC-CLEAR.                                           
09240                                                                   
09241      MOVE ZEROS                         TO EC-AGT-NO (SUB3).      
09242      MOVE SPACE                         TO EC-AGT-TYPE (SUB3).    
09243      MOVE ZEROS                         TO EC-ISS-COMM (SUB3)     
09244                                            EC-CNC-COMM (SUB3)     
09245                                            EC-COMM-78 (SUB3)      
09246                                            EC-COMM-PR (SUB3)      
09247                                            EC-COMM-ST (SUB3)      
09248                                            EC-COMM-78-ADJ (SUB3)  
09249                                            EC-COMM-PR-ADJ (SUB3). 
09250                                                                   
09251  4540-BUILD-CLEAR-X.                                              
09252      EXIT.                                                        

102204 4541-CALC-IBNR.
102204 
           MOVE +0                    TO TOT-AH-IBNR
                                         TOT-AH-PRM
                                         TOT-AH-PD-CLMS
                                         TOT-AH-CLMRSV
                                         TOT-LF-IBNR
                                         TOT-LF-PRM
                                         TOT-LF-PD-CLMS
                                         TOT-LF-CLMRSV

           MOVE +0                    TO RTOT-AH-IBNR
                                         RTOT-AH-PRM
                                         RTOT-AH-PD-CLMS
                                         RTOT-AH-CLMRSV
                                         RTOT-LF-IBNR
                                         RTOT-LF-PRM
                                         RTOT-LF-PD-CLMS
                                         RTOT-LF-CLMRSV
                                         
102204     PERFORM VARYING SUB2 FROM +1 BY +1 UNTIL
102204        (SUB2 > +900)
102204        OR (EPEC-REIN-CO (SUB2) = ZEROS)
102204*       IF W-UNDERWRITTEN-DATA
102204*          MOVE EPEC-LEVELS-II (SUB2)
102204*                                TO EPEC-WORK
102204*       ELSE
102204           MOVE EPEC-LEVELS (SUB2)
102204                                 TO EPEC-WORK
102204*       END-IF
              IF EW-REIN-CO = SPACES
102204           IF EW-LF-AH = 'L'
102204              COMPUTE TOT-LF-PRM = TOT-LF-PRM
102204                 + EW-EPR-ST
102204              COMPUTE TOT-LF-PD-CLMS = TOT-LF-PD-CLMS
102204                 + EW-CLM-AMT
102204              COMPUTE TOT-LF-CLMRSV = TOT-LF-CLMRSV
102204                 + EW-CLM-PAYCUR + EW-CLM-FUTRSV
102204           ELSE
102204              COMPUTE TOT-AH-PRM = TOT-AH-PRM
102204                 + EW-EPR-ST
102204              COMPUTE TOT-AH-PD-CLMS = TOT-AH-PD-CLMS
102204                 + EW-CLM-AMT
102204              COMPUTE TOT-AH-CLMRSV = TOT-AH-CLMRSV
102204                 + EW-CLM-PAYCUR + EW-CLM-FUTRSV
              ELSE
102204           IF EW-LF-AH = 'L'
102204              COMPUTE RTOT-LF-PRM = RTOT-LF-PRM
102204                 + EW-EPR-ST
102204              COMPUTE RTOT-LF-PD-CLMS = RTOT-LF-PD-CLMS
102204                 + EW-CLM-AMT
102204              COMPUTE RTOT-LF-CLMRSV = RTOT-LF-CLMRSV
102204                 + EW-CLM-PAYCUR + EW-CLM-FUTRSV
102204           ELSE
102204              COMPUTE RTOT-AH-PRM = RTOT-AH-PRM
102204                 + EW-EPR-ST
102204              COMPUTE RTOT-AH-PD-CLMS = RTOT-AH-PD-CLMS
102204                 + EW-CLM-AMT
102204              COMPUTE RTOT-AH-CLMRSV = RTOT-AH-CLMRSV
102204                 + EW-CLM-PAYCUR + EW-CLM-FUTRSV
                 END-IF
102204        END-IF
102204     END-PERFORM

102204     COMPUTE TOT-AH-IBNR = (TOT-AH-PRM * .6)
102204        - TOT-AH-PD-CLMS - TOT-AH-CLMRSV
102204     IF TOT-AH-IBNR < ZEROS
102204        COMPUTE TOT-AH-IBNR = TOT-AH-PRM * .15
102204     END-IF
102204     COMPUTE TOT-LF-IBNR = (TOT-LF-PRM * .6)
102204        - TOT-LF-PD-CLMS - TOT-LF-CLMRSV
102204     IF TOT-LF-IBNR < ZEROS
102204        COMPUTE TOT-LF-IBNR = TOT-LF-PRM * .15
102204     END-IF

102204     COMPUTE RTOT-AH-IBNR = (RTOT-AH-PRM * .6)
102204        - RTOT-AH-PD-CLMS - RTOT-AH-CLMRSV
102204     IF RTOT-AH-IBNR < ZEROS
102204        COMPUTE RTOT-AH-IBNR = RTOT-AH-PRM * .15
102204     END-IF
102204     COMPUTE RTOT-LF-IBNR = (RTOT-LF-PRM * .6)
102204        - RTOT-LF-PD-CLMS - RTOT-LF-CLMRSV
102204     IF RTOT-LF-IBNR < ZEROS
102204        COMPUTE RTOT-LF-IBNR = RTOT-LF-PRM * .15
102204     END-IF

102204     PERFORM VARYING SUB2 FROM +1 BY +1 UNTIL
102204        (SUB2 > +900)
102204        OR (EPEC-REIN-CO (SUB2) = ZEROS)
102204*       IF W-UNDERWRITTEN-DATA
102204*          MOVE EPEC-LEVELS-II (SUB2)
102204*                                TO EPEC-WORK
102204*       ELSE
102204           MOVE EPEC-LEVELS (SUB2)
102204                                 TO EPEC-WORK
102204*       END-IF
              IF EW-REIN-CO = SPACES
102204           IF EW-LF-AH = 'L'
                    COMPUTE IBNR-FACT ROUNDED = EW-EPR-ST
                       / TOT-LF-PRM
                    COMPUTE EW-CLM-IBNR ROUNDED = IBNR-FACT
                       * RTOT-LF-IBNR
      *                * TOT-LF-IBNR
102204           ELSE
                    COMPUTE IBNR-FACT ROUNDED = EW-EPR-ST
                       / TOT-AH-PRM
                    COMPUTE EW-CLM-IBNR ROUNDED = IBNR-FACT
                       * RTOT-AH-IBNR
      *                * TOT-AH-IBNR
102204           END-IF
              ELSE
102204           IF EW-LF-AH = 'L'
                    COMPUTE IBNR-FACT ROUNDED = EW-EPR-ST
                       / RTOT-LF-PRM
                    COMPUTE EW-CLM-IBNR ROUNDED = IBNR-FACT
                       * RTOT-LF-IBNR
102204           ELSE
                    COMPUTE IBNR-FACT ROUNDED = EW-EPR-ST
                       / RTOT-AH-PRM
                    COMPUTE EW-CLM-IBNR ROUNDED = IBNR-FACT
                       * RTOT-AH-IBNR
102204           END-IF
              END-IF
102204*       IF W-UNDERWRITTEN-DATA
102204*          MOVE EPEC-WORK        TO EPEC-LEVELS-II (SUB2)
102204*       ELSE
102204           MOVE EPEC-WORK        TO EPEC-LEVELS (SUB2)
102204*       END-IF
102204     END-PERFORM

102204     .
102204 4541-EXIT.
102204     EXIT.
09253                                                                   
09254  4545-BUILD-BASE.                                                 
09255                                                                   
09256      MOVE 'EC'                   TO EC-RECORD-ID.                 
09257      MOVE DTE-CLASIC-COMPANY-CD  TO EP-COMPANY-CD.                
09258      MOVE AM-MSTR-CNTRL          TO EC-CONTROL.                   
09259      MOVE AM-EXPIRE-DT           TO WS-EC-EXP-DTE-N.              
09260      MOVE AM-EFFECT-DT           TO EC-EFF-DTE                    
09261                                     WS-EC-EFF-DTE.                
09262      MOVE EW-REIN-CO             TO EC-REI-CO.                    
09263      IF EC-REI-CO NOT = SPACES                                    
09264          MOVE 'R'                TO EC-REIN.                      
09265      MOVE EW-LF-AH               TO EC-RCD-TYPE.                  
09266      MOVE EW-BEN-TYPE            TO EC-BEN-CODE.                  
09267      IF DTE-PGM-OPT = '3'                                         
09268          MOVE 'P'                TO EC-PURGE.                     
09269      MOVE WS-SEQ-NBR             TO EC-SEQ-NBR.                   
09270                                                                   
09271  4545-BUILD-BASE-X.                                               
09272      EXIT.                                                        
09273                                                                   
09274 ******************************************************************
09275 ***        CLEAR EARNED PREMIUM  AND  REINSURANCE TOTALS       ***
09276 ******************************************************************
09277                                                                   
09278  4550-CLEAR-EPEC-TOTALS.                                          
09279                                                                   
09280      MOVE LOW-CERT             TO SV-LO-CERT.                     
09281      MOVE HIGH-CERT            TO SV-HI-CERT.                     
09282      MOVE SPACES               TO LOW-CTL.                        
09283      MOVE ZEROS                TO LOW-CERT  HIGH-CERT.            
09284                                                                   
09285      MOVE +1                   TO SUB9.                           
09286      MOVE +0                   TO SUB10.                          
09287      MOVE ZERO                 TO EW-REIN-CO                      
09288                                   EW-BEN-TYPE.                    
09289      MOVE SPACE                TO EP-FLG                          
09290                                   EW-LF-AH                        
09291                                   EW-LEVEL-USED                   
09292                                   W-TABLE-USED-SW.                
09293                                                                   
09294      MOVE +0                   TO EW-ISS-BEN     EW-ISS-BEN-GROSS 
09295                                   EW-CNC-BEN     EW-CNC-BEN-GROSS 
09296                                   EW-ISS-PRM     EW-ISS-PRM-GROSS 
09297                                   EW-CNC-PRM     EW-CNC-PRM-GROSS 
09298                                   EW-EPR-R78     EW-EPR-R78-ADJ   
09299                                   EW-EPR-PRO     EW-EPR-PRO-ADJ   
09300                                   EW-EPR-ST      EW-EPR-ST-ADJ    
09301                                   EW-CLM-AMT     EW-COV-DTE       
09302                                   EW-CLM-IBNR    EW-CLM-PAYCUR    
09303                                   EW-CLM-FUTRSV  EW-CLM-CNT       
09304                                   EW-CLM-CRT     EW-ISS-CNT       
09305                                   EW-CNC-CNT     EW-AGE           
09306                                   EW-ORIG-TERM   EW-INFORCE-CNT   
09307                                   EW-AGE-BEN     EW-TERM-BEN      
PEMMOD                                  EW-ISS-PRM-TAX
09308                                   EW-REM-TERM.                    
09309                                                                   
09310      MOVE +1 TO SUB1.                                             
09311      PERFORM 4555-CLEAR-EPEC-COMM WS-LOOP-MAX TIMES.              
09312      PERFORM 4560-CLEAR-EPEC-ALL VARYING SUB1 FROM +1 BY +1       
09313          UNTIL EPEC-REIN-CO (SUB1) = ZEROS.                       
09314      MOVE EPEC-OPT-WORK-TABLE    TO EPEC-OPT-DATA-TABLE           
09315                                     EPEC-UNDW-OPT-DATA-TABLE.     
09316      PERFORM 4570-CLEAR-EPEC-SUB-A VARYING SUB1 FROM +1 BY +1     
09317          UNTIL EPEC-SUB-REIN (SUB1) = ZEROS.                      
09318                                                                   
09319      PERFORM 4590-INIT-OB THRU 4590-EXIT                          
09320          VARYING SUB3 FROM +1 BY +1                               
CIDMOD*        UNTIL SUB3 GREATER THAN +300.                            
092602*        UNTIL SUB3 GREATER THAN +220.                            
092602         UNTIL SUB3 GREATER THAN +900.                            
09322                                                                   
09323      GO TO 4589-CLEAR-EPEC-TOTALS-X.                              
09324                                                                   
09325  4555-CLEAR-EPEC-COMM.                                            
09326                                                                   
09327      MOVE +0 TO EW-ISS-COM (SUB1)  EW-CNC-COM (SUB1)              
09328                 EW-ECR-R78 (SUB1)  EW-ECR-R78-ADJ (SUB1)          
09329                 EW-ECR-PRO (SUB1)  EW-ECR-PRO-ADJ (SUB1)          
09330                 EW-ECR-ST  (SUB1).                                
09331      MOVE SPACES                 TO EW-AGT-NO (SUB1)              
09332                                     EW-AGT-TYPE (SUB1).           
09333                                                                   
09334      ADD +1 TO SUB1.                                              
09335                                                                   
09336  4560-CLEAR-EPEC-ALL.                                             
09337                                                                   
09338      MOVE EPEC-WORK              TO EPEC-LEVELS (SUB1)            
09339                                     EPEC-LEVELS-II (SUB1).        
09340      MOVE W-LIFE-YEARS-ZERO      TO EPEC-OPT-GRP (SUB1).          
09341                                                                   
09342  4570-CLEAR-EPEC-SUB-A.                                           
09343                                                                   
09344      MOVE ZEROS TO EPEC-SUB-REIN (SUB1).                          
09345      PERFORM 4580-CLEAR-EPEC-SUB-B VARYING SUB2 FROM +1 BY +1     
092602*        UNTIL SUB2 GREATER +300.                                 
092602         UNTIL SUB2 GREATER +900.                                 
09347                                                                   
09348  4580-CLEAR-EPEC-SUB-B.                                           
09349                                                                   
09350      MOVE ZEROS TO EPEC-SUB-LF (SUB1 SUB2).                       
09351      MOVE ZEROS TO EPEC-SUB-AH (SUB1 SUB2).                       
09352                                                                   
09353  4589-CLEAR-EPEC-TOTALS-X.                                        
09354      EXIT.                                                        
09355                                                                   
09356  4590-INIT-OB.                                                    
09357                                                                   
09358      MOVE +0 TO WS-EPR-R78 (SUB3 1).                              
09359      MOVE +0 TO WS-EPR-R78 (SUB3 2).                              
09360      MOVE +0 TO WS-EPR-R78 (SUB3 3).                              
09361                                                                   
09362  4590-EXIT.                                                       
09363      EXIT.                                                        
09364                                                                   
09365  4599-BUILD-EPEC-EXTRACT-X.                                       
09366      EXIT.                                                        
09367                                                                   
09368 *********************************************************         
09369 *  THIS ROUTINE IS USED TO CALCULATE THE A&H IBNR       *         
09370 *  RESERVES USING A PERCENTAGE OF THE UNEARNED PREMIUM. *         
09371 *  THESE PERCENTAGES ARE ENTERED THROUGH SCREEN EL105A  *         
09372 *  (CARRIER CONTROLS) AND PASSED THROUGHOUT THE SYSTEM  *         
09373 *  IN THE DATE CARD.                                    *         
09374 *  'CARRIER-UEP-PCT' IS THE ACTUAL PERCENT PER CARRIER. *         
09375 *********************************************************         
09376                                                                   
09377  4599A-CALC-IBNR.                                                 
09378                                                                   
09379      COMPUTE EW-CLM-IBNR =  ((EP-ISS-PRM - EP-CNC-PRM) -          
09380         (((EP-PRM-78 + EP-PRM-78-ADJ)                             
09381                               * CARRIER-R78-PCT (CLAS-INDEXCN)) + 
09382          ((EP-PRM-PR + EP-PRM-PR-ADJ)                             
09383                               * CARRIER-PRO-PCT (CLAS-INDEXCN)))) 
09384                          * CARRIER-UEP-PCT (CLAS-INDEXCN).        
09385                                                                   
09386      IF EW-CLM-IBNR NOT GREATER THAN ZEROS                        
09387          MOVE ZEROS                 TO EW-CLM-IBNR                
09388          GO TO 4599A-CALC-EXIT.                                   
09389                                                                   
09390      IF EP-REIN = 'R'                                             
09391          MOVE 'Y'                   TO W-REIN-PROCESS-IND.        
09392                                                                   
09393      PERFORM 4740-CREATE-DETAIL-EXTR THRU 4740-EXIT.              
09394                                                                   
09395  4599A-CALC-EXIT.                                                 
09396       EXIT.                                                       
09397  4599B-SEARCH.                                                    
09398       EXIT.                                                       
09399                                  EJECT                            
09400 ******************************************************************
09401 ***         CALCULATE OPTIONAL METHOD I B N R                  ***
09402 ******************************************************************
09403                                                                   
09404  4600-GET-OPT-IBNR.                                               
09405                                                                   
09406      IF  CR-POLICY-IS-ACTIVE                                      
09407          IF  CR-POLICY-IS-REISSUE                                 
122002             OR CR-POLICY-IS-MONTHLY
09408              IF  W-LIFE                                           
09409                  GO TO 4600-EXIT                                  
09410              ELSE                                                 
09411                  NEXT SENTENCE                                    
09412          ELSE                                                     
09413              NEXT SENTENCE                                        
09414      ELSE                                                         
09415          IF  W-LIFE                                               
09416              GO TO 4600-EXIT.                                     
09417                                                                   
09418      IF  W-LIFE                                                   
09419          PERFORM 4620-DETERMINE-LIFE-IBNR THRU 4620-EXIT          
09420      ELSE                                                         
09421          PERFORM 4640-GET-A-H-EARNED-PREMIUM THRU 4640-EXIT.      
09422                                                                   
09423  4600-EXIT.                                                       
09424      EXIT.                                                        
09425                                                                   
09426  4620-DETERMINE-LIFE-IBNR.                                        
09427                                                                   
09428      IF  CR-LF-POLICY-IS-ACTIVE                                   
09429          ADD +1                  TO W-LF-ACTIVE                   
09430          IF  CR-LF-POLICY-IS-REISSUE                              
122002             OR CR-LF-POLICY-IS-MONTHLY
09431              MOVE ZEROS          TO W-WK-IBNR                     
09432              ADD +1              TO W-LF-REISSUE                  
09433              GO TO 4620-EXIT                                      
09434          ELSE                                                     
09435              NEXT SENTENCE                                        
09436      ELSE                                                         
09437          ADD +1                  TO W-LF-INACTIVE                 
09438          MOVE ZEROS              TO W-WK-IBNR                     
09439          GO TO 4620-EXIT.                                         
09440                                                                   
09441      IF  CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'                     
09442          MOVE ZEROS              TO W-WK-IBNR                     
09443          ADD +1                  TO W-LF-CALC-B                   
09444          GO TO 4620-EXIT.                                         
09445                                                                   
09446      IF  (CLAS-I-EP (CLAS-INDEXL) = 'B'                           
09447                  AND                                              
09448              CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'             
09449                  AND                                              
09450              W-BAL-REMTRM NOT GREATER THAN ZERO)                  
09451              OR                                                   
09452          (CLAS-I-EP (CLAS-INDEXL) = 'B'                           
09453                  AND                                              
09454              CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'L'                 
09455                  AND                                              
09456              REM-TRM2 NOT GREATER THAN ZERO)                      
09457          MOVE ZEROS              TO W-WK-IBNR                     
09458          ADD +1                  TO W-LF-BALLOON                  
09459          GO TO 4620-EXIT                                          
09460      ELSE                                                         
09461          IF  (CLAS-I-EP (CLAS-INDEXL) NOT = 'B'                   
09462                  AND                                              
09463              REM-TRM2 NOT GREATER THAN ZERO)                      
09464              ADD +1              TO W-LF-REMTERM-0                
09465              MOVE ZEROS          TO W-WK-IBNR                     
09466              GO TO 4620-EXIT.                                     
09467                                                                   
09468      PERFORM 4680-CALC-REMAINING-AMT-LF THRU 4680-EXIT.           
09469                                                                   
09470      IF  CP-REMAINING-AMT NOT GREATER THAN ZEROS                  
09471          ADD +1                  TO W-LF-REMAMT-0                 
09472          MOVE ZEROS              TO W-WK-IBNR                     
09473          GO TO 4620-EXIT.                                         
09474                                                                   
09475      IF  AM-LIFE-IBNR-PCT NUMERIC                                 
09476              AND                                                  
09477          AM-LIFE-IBNR-PCT GREATER THAN ZEROS                      
09478          COMPUTE W-WK-IBNR ROUNDED                                
09479              = CP-REMAINING-AMT                                   
09480              / 1000                                               
09481              * AM-LIFE-IBNR-PCT                                   
09482      ELSE                                                         
09483          COMPUTE W-WK-IBNR ROUNDED                                
09484              = CP-REMAINING-AMT                                   
09485              / 1000                                               
09486              * COMPANY-IBNR-LIFE-FACTOR.                          
09487                                                                   
09488      IF  CR-LF-IS-REIN-ONLY                                       
09489          ADD +1                  TO W-LF-REIN                     
09490      ELSE                                                         
09491          ADD +1                  TO W-LF-DATA-USED                
09492          ADD W-WK-IBNR           TO W-LIFE-IBNR-IN                
09493          ADD CP-REMAINING-AMT    TO W-LIFE-REMAINING-AMT          
09494          IF  CR-LFTYP = '01'                                      
09495              ADD CP-REMAINING-AMT                                 
09496                                  TO W-01-REMAMT                   
09497              ADD +1              TO W-01-DATA-USED                
09498          ELSE                                                     
09499          IF  CR-LFTYP = '02'                                      
09500              ADD CP-REMAINING-AMT                                 
09501                                  TO W-02-REMAMT                   
09502              ADD +1              TO W-02-DATA-USED                
09503          ELSE                                                     
09504          IF  CR-LFTYP = '03'                                      
09505              ADD CP-REMAINING-AMT                                 
09506                                  TO W-03-REMAMT                   
09507              ADD +1              TO W-03-DATA-USED                
09508          ELSE                                                     
09509          IF  CR-LFTYP = '04'                                      
09510              ADD CP-REMAINING-AMT                                 
09511                                  TO W-04-REMAMT                   
09512              ADD +1              TO W-04-DATA-USED                
09513          ELSE                                                     
09514          IF  CR-LFTYP = '05'                                      
09515              ADD CP-REMAINING-AMT                                 
09516                                  TO W-05-REMAMT                   
09517              ADD +1              TO W-05-DATA-USED                
09518          ELSE                                                     
09519          IF  CR-LFTYP = '06'                                      
09520              ADD CP-REMAINING-AMT                                 
09521                                  TO W-06-REMAMT                   
09522              ADD +1              TO W-06-DATA-USED                
09523          ELSE                                                     
09524          IF  CR-LFTYP = '30'                                      
09525              ADD CP-REMAINING-AMT                                 
09526                                  TO W-30-REMAMT                   
09527              ADD +1              TO W-30-DATA-USED                
09528          ELSE                                                     
09529          IF  CR-LFTYP = '31'                                      
09530              ADD CP-REMAINING-AMT                                 
09531                                  TO W-31-REMAMT                   
09532              ADD +1              TO W-31-DATA-USED                
09533          ELSE                                                     
09534              ADD CP-REMAINING-AMT                                 
09535                                  TO W-XX-REMAMT                   
09536              ADD +1              TO W-XX-DATA-USED.               
09537                                                                   
09538      IF  CR-POLICY-IS-REIN-ONLY                                   
09539              OR                                                   
09540          CR-LF-IS-REIN-ONLY                                       
09541          ADD ISS-BEN             TO EPEC-REIN-ISS-BEN (SUB3)      
09542          MOVE 'R'                TO EPEC-REIN-ONLY-IND (SUB3)     
09543          ADD W-WK-IBNR           TO EPEC-LIFE-IBNR (SUB3)         
09544      ELSE                                                         
09545          ADD W-WK-IBNR           TO EPEC-LIFE-IBNR (SUB3).        
09546                                                                   
09547  4620-EXIT.                                                       
09548      EXIT.                                                        
09549                                  EJECT                            
09550  4640-GET-A-H-EARNED-PREMIUM.                                     
09551                                                                   
09552      IF  CR-AH-POLICY-IS-ACTIVE                                   
09553          ADD +1                  TO W-AH-ACTIVE                   
09554          IF  CR-AH-POLICY-IS-REISSUE                              
122002             OR CR-AH-POLICY-IS-MONTHLY
09555              ADD +1              TO W-AH-REISSUE                  
09556          ELSE                                                     
09557              NEXT SENTENCE                                        
09558      ELSE                                                         
09559          ADD +1                  TO W-AH-INACTIVE.                
09560                                                                   
09561      IF  CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'B'                     
09562          ADD +1                  TO W-AH-CALC-B                   
09563          GO TO 4640-EXIT.                                         
09564                                                                   
09565      IF  REM-TRM2 NOT GREATER THAN ZEROS                          
09566          MOVE 'Y'                TO W-RTRM-0-SW                   
09567      ELSE                                                         
09568          MOVE SPACES             TO W-RTRM-0-SW.                  
09569                                                                   
09570      PERFORM 4660-DETERMINE-EARNED-PREMIUM THRU 4660-EXIT.        
09571                                                                   
09572 ****** NOTE:  THERE IS AN ASSUMPTION THAT STATISTICALLY ONLY ONE  
09573 ******        ACTUALLY CLAIM PER 90 DAY PERIOD AND THAT THE CLAIM 
09574 ******        ITD FIELD (CR-DISAMT) CONTAINS ONLY THE AMOUNT OF   
09575 ******        THAT CLAIM.                                         
09576                                                                   
09577      PERFORM 4650-DETERMINE-CLM-AMT-AH THRU 4650-EXIT.            
09578                                                                   
09579      IF  CR-POLICY-IS-REIN-ONLY                                   
09580               OR                                                  
09581          CR-AH-IS-REIN-ONLY                                       
09582          ADD +1                  TO W-AH-REIN                     
09583          ADD ISS-BEN             TO EPEC-REIN-ISS-BEN (SUB3)      
09584          MOVE 'R'                TO EPEC-REIN-ONLY-IND (SUB3)     
09585          ADD W-CLMAMT            TO EPEC-CLAIMS-MADE-PAID (SUB3)  
09586          ADD W-EARNED-PREMIUM    TO EPEC-EARNED-PREMIUM-AH (SUB3) 
09587          GO TO 4640-EXIT.                                         
09588                                                                   
09589      ADD W-EARNED-PREMIUM        TO EPEC-EARNED-PREMIUM-AH (SUB3) 
09590                                     W-EPEC-EP-IN.                 
09591      ADD W-CLMAMT                TO EPEC-CLAIMS-MADE-PAID (SUB3)  
09592                                     W-CLAIMS-MADE-PAID-IN.        
09593      ADD +1                      TO W-AH-DATA-USED.               
09594      ADD W-EARNED-PREMIUM-1      TO W-EPEC-EP-1.                  
09595      ADD W-EARNED-PREMIUM-2      TO W-EPEC-EP-2.                  
09596                                                                   
09597      IF  W-CLMAMT GREATER THAN ZEROS                              
09598          ADD +1                  TO W-AH-CLAIM-CTR                
09599          ADD W-EARNED-PREMIUM    TO W-AH-CLAIM-EP.                
09600                                                                   
09601      IF  W-RTRM-0                                                 
09602              AND                                                  
09603          NOT CR-POLICY-IS-REIN-ONLY                               
09604              AND                                                  
09605          NOT CR-AH-IS-REIN-ONLY                                   
09606          ADD W-CLMAMT            TO W-RTRM-0-CLMAMT               
09607          ADD W-EARNED-PREMIUM-1  TO W-RTRM-EP-1                   
09608          ADD W-EARNED-PREMIUM-2  TO W-RTRM-EP-2                   
09609          ADD W-EARNED-PREMIUM    TO W-RTRM-EP                     
09610          ADD +1                  TO W-AH-REMTERM-0.               
09611                                                                   
09612  4640-EXIT.                                                       
09613      EXIT.                                                        
09614                                  EJECT                            
09615  4650-DETERMINE-CLM-AMT-AH.                                       
09616                                                                   
09617      MOVE ZEROS                  TO W-CLMAMT.                     
09618                                                                   
09619      IF  CR-DIS-DT NOT GREATER THAN ZEROS                         
09620              OR                                                   
09621          CR-DIS-DT NOT GREATER THAN W-CUTOFF-DATE                 
09622          GO TO 4650-EXIT.                                         
09623                                                                   
09624      IF  CR-DIS-PTO-DT GREATER THAN ZEROS                         
09625              AND                                                  
09626          CR-DIS-PTO-DT GREATER THAN CR-DIS-DT                     
09627          MOVE CR-DIS-DT              TO DC-GREG-DATE-CYMD         
09628          MOVE 'L'                    TO DC-OPTION-CODE            
09629          PERFORM 8000-DATE-CONVERT-ROUTINE                        
09630              THRU 8099-DATE-CONVERT-X                             
09631          MOVE DC-BIN-DATE-1          TO W-DIS-DT                  
09632          MOVE CR-DIS-PTO-DT          TO DC-GREG-DATE-CYMD         
09633          MOVE 'L'                    TO DC-OPTION-CODE            
09634          PERFORM 8000-DATE-CONVERT-ROUTINE                        
09635              THRU 8099-DATE-CONVERT-X                             
09636          MOVE DC-BIN-DATE-1          TO W-DIS-PTO-DT              
09637          IF  DATE-CONVERSION-ERROR                                
09638              DISPLAY 'CERT DATES ERROR' CR-FULL-CONTROL           
09639              MOVE 'CERT DATES ERROR' TO WS-ABEND-MESSAGE          
09640              MOVE DC-ERROR-CODE      TO WS-ABEND-FILE-STATUS      
09641              MOVE 0303               TO WS-ABEND-CODE             
09642              MOVE WS-ABEND-CODE      TO WS-RETURN-CODE            
09643              GO TO ABEND-PGM                                      
09644          ELSE                                                     
09645              MOVE W-DIS-DT           TO DC-BIN-DATE-1             
09646              MOVE W-DIS-PTO-DT       TO DC-BIN-DATE-2             
09647              MOVE '1'                TO DC-OPTION-CODE            
09648              MOVE ZEROS              TO DC-ELAPSED-MONTHS         
09649                                         DC-ELAPSED-DAYS           
09650              PERFORM 8000-DATE-CONVERT-ROUTINE                    
09651                  THRU 8099-DATE-CONVERT-X                         
09652              IF  DATE-CONVERSION-ERROR                            
09653                  DISPLAY 'DIS TO PTO DATE ERROR '                 
09654                      CR-FULL-CONTROL                              
09655                  MOVE 'DIS TO PTO DATE ERROR '                    
09656                                      TO WS-ABEND-MESSAGE          
09657                  MOVE DC-ERROR-CODE  TO WS-ABEND-FILE-STATUS      
09658                  MOVE 0303           TO WS-ABEND-CODE             
09659                  MOVE WS-ABEND-CODE  TO WS-RETURN-CODE            
09660                  GO TO ABEND-PGM                                  
09661              ELSE                                                 
09662                  COMPUTE W-CLMAMT ROUNDED                         
09663                      = CR-AHAMT                                   
09664                      * (DC-ELAPSED-DAYS / 30)                     
09665                  GO TO 4650-EXIT.                                 
09666                                                                   
09667      IF  CR-DIS-PAY-DT NOT GREATER THAN ZEROS                     
09668              OR                                                   
09669          CR-DIS-PAY-DT LESS THAN CR-DIS-DT                        
09670          GO TO 4650-EXIT.                                         
09671                                                                   
09672      COMPUTE W-90-DAY-MAX ROUNDED                                 
09673          = CR-AHAMT * 3.                                          
09674                                                                   
09675      IF  CR-DISAMT NOT GREATER THAN W-90-DAY-MAX                  
09676          MOVE CR-DISAMT          TO W-CLMAMT                      
09677      ELSE                                                         
09678          MOVE W-90-DAY-MAX       TO W-CLMAMT.                     
09679                                                                   
09680      IF  NOT CR-POLICY-IS-REIN-ONLY                               
09681               AND                                                 
09682          NOT CR-AH-IS-REIN-ONLY                                   
09683          ADD +1          TO W-CLM-EXCEPT-CNT                      
09684          ADD W-CLMAMT    TO W-CLM-EXCEPT-AMT.                     
09685                                                                   
09686  4650-EXIT.                                                       
09687      EXIT.                                                        
09688                                  EJECT                            
09689  4660-DETERMINE-EARNED-PREMIUM.                                   
09690                                                                   
09691      MOVE ZEROS                  TO W-EARNED-PREMIUM.             
09692                                                                   
09693      PERFORM 4670-CALC-EP THRU 4670-EXIT.                         
09694                                                                   
09695      MOVE W-EARNED-PREMIUM       TO W-EARNED-PREMIUM-1.           
09696                                                                   
09697      IF  CR-ENTRY-DATE NOT GREATER THAN W-CUTOFF-DATE             
09698          MOVE ZEROS              TO W-EARNED-PREMIUM              
09699          MOVE W-CUTOFF-DATE      TO VALUATION-DT                  
09700          MOVE '7'                TO W-VAL-SOURCE                  
09701          PERFORM 8500-REMAINING-TERM-ROUTINE                      
09702              THRU 8599-REM-TERM-X                                 
09703          PERFORM 4666-SAVE-AHCR-DATA THRU 4666-EXIT               
09704          PERFORM 4800-CALC-EARNED-PREMIUM-AH THRU 4899-EXIT       
09705          PERFORM 4668-RESTORE-AHCR-DATA THRU 4668-EXIT            
09706          MOVE EPR-R78            TO W-OPT-R78                     
09707          MOVE EPR-PRO            TO W-OPT-PRO                     
09708          MOVE EPR-ST             TO W-OPT-ST                      
09709          PERFORM 4670-CALC-EP THRU 4670-EXIT                      
09710          MOVE W-EARNED-PREMIUM   TO W-EARNED-PREMIUM-2            
09711      ELSE                                                         
09712          MOVE ZEROS              TO W-EARNED-PREMIUM-2.           
09713                                                                   
09714      COMPUTE W-EARNED-PREMIUM ROUNDED                             
09715          = W-EARNED-PREMIUM-1 - W-EARNED-PREMIUM-2.               
09716                                                                   
09717      IF  W-EARNED-PREMIUM LESS THAN ZERO                          
09718          ADD +1                  TO W-EP-LESS-THAN-0              
09719          MOVE ZERO               TO W-EARNED-PREMIUM.             
09720                                                                   
09721  4660-EXIT.                                                       
09722      EXIT.                                                        
09723                                  EJECT                            
09724  4666-SAVE-AHCR-DATA.                                             
09725                                                                   
09726      MOVE SPACES                 TO WX-STATUS-AT-CANCEL           
09727                                     WX-STATUS-AT-SETTLEMENT.      
09728                                                                   
09729      MOVE ZEROS                  TO WX-CANC-DT                    
09730                                     WX-EOM-RUN-DATE               
09731                                     WX-SETTLEMENT-EXIT-DATE.      
09732                                                                   
09733      IF  CR-AH-CANC-DT GREATER THAN W-CUTOFF-DATE                 
09734          MOVE CR-AH-CANC-DT      TO WX-CANC-DT                    
09735          MOVE CR-AH-STATUS-AT-CANCEL                              
09736                                  TO WX-STATUS-AT-CANCEL           
09737          MOVE ZEROS              TO CR-AH-CANC-DT                 
09738                                     WS-CR-AH-CANC-DT-N            
09739          MOVE SPACES             TO CR-AH-STATUS-AT-CANCEL.       
09740                                                                   
09741      IF  CR-AH-SETTLEMENT-EXIT-DATE GREATER THAN W-CUTOFF-DATE    
09742          MOVE CR-AH-SETTLEMENT-EXIT-DATE                          
09743                                  TO WX-SETTLEMENT-EXIT-DATE       
09744          MOVE CR-AH-STATUS-AT-SETTLEMENT                          
09745                                  TO WX-STATUS-AT-SETTLEMENT       
09746          MOVE ZEROS              TO CR-AH-SETTLEMENT-EXIT-DATE    
09747                                  WS-CR-AH-SETTLEMENT-EXIT-DATEN   
09748          MOVE SPACES             TO CR-AH-STATUS-AT-SETTLEMENT.   
09749                                                                   
09750      MOVE EOM-RUN-DATE           TO WX-EOM-RUN-DATE.              
09751      MOVE W-CUTOFF-DATE          TO EOM-RUN-DATE.                 
09752                                                                   
09753  4666-EXIT.                                                       
09754      EXIT.                                                        
09755                                                                   
09756  4668-RESTORE-AHCR-DATA.                                          
09757                                                                   
09758      IF  WX-CANC-DT GREATER THAN W-CUTOFF-DATE                    
09759          MOVE WX-CANC-DT         TO CR-AH-CANC-DT                 
09760                                     WS-CR-AH-CANC-DT-N            
09761          MOVE WX-STATUS-AT-CANCEL                                 
09762                                  TO CR-AH-STATUS-AT-CANCEL.       
09763                                                                   
09764      IF  WX-SETTLEMENT-EXIT-DATE GREATER THAN W-CUTOFF-DATE       
09765          MOVE WX-SETTLEMENT-EXIT-DATE                             
09766                                  TO CR-AH-SETTLEMENT-EXIT-DATE    
09767                                  WS-CR-AH-SETTLEMENT-EXIT-DATEN   
09768          MOVE WX-STATUS-AT-SETTLEMENT                             
09769                                  TO CR-AH-STATUS-AT-SETTLEMENT.   
09770                                                                   
09771      MOVE WX-EOM-RUN-DATE        TO EOM-RUN-DATE.                 
09772                                                                   
09773  4668-EXIT.                                                       
09774      EXIT.                                                        
09775                                  EJECT                            
09776  4670-CALC-EP.                                                    
09777                                                                   
09778      IF  STATE-ABBR (CLAS-INDEXS) = 'WY'                          
09779          MOVE W-OPT-PRO          TO W-EARNED-PREMIUM              
09780          ADD W-EARNED-PREMIUM    TO W-WY-EP                       
09781          GO TO 4670-EXIT.                                         
09782                                                                   
09783      IF  STATE-ABBR (CLAS-INDEXS) = 'VA'                          
09784          MOVE W-OPT-ST           TO W-EARNED-PREMIUM              
09785          ADD W-EARNED-PREMIUM    TO W-VA-EP                       
09786          GO TO 4670-EXIT.                                         
09787                                                                   
09788 ***** NOTE THIS ASSUMES THAT ONLY A&H COVERAGES ARE PROCESSED HERE
09789                                                                   
09790      IF  CLAS-I-EP (CLAS-INDEXA) = 'M'                            
09791          COMPUTE W-EARNED-PREMIUM                                 
09792              = (W-OPT-PRO + W-OPT-R78) / +2                       
09793          ADD W-EARNED-PREMIUM    TO W-M-EP                        
09794      ELSE                                                         
09795          IF  CLAS-I-EP (CLAS-INDEXA) = 'R'                        
09796              MOVE W-OPT-R78      TO W-EARNED-PREMIUM              
09797              ADD W-EARNED-PREMIUM                                 
09798                                  TO W-R-EP                        
09799          ELSE                                                     
09800              IF  CLAS-I-EP (CLAS-INDEXA) = 'P'                    
09801                  MOVE W-OPT-PRO  TO W-EARNED-PREMIUM              
09802                  ADD W-EARNED-PREMIUM                             
09803                                  TO W-P-EP                        
09804              ELSE                                                 
09805                  IF  CLAS-I-EP (CLAS-INDEXA) = '1'                
09806                      COMPUTE W-EARNED-PREMIUM ROUNDED             
09807                          = (W-OPT-PRO * +.6667)                   
09808                          + (W-OPT-R78 * +.3333)                   
09809                      ADD W-EARNED-PREMIUM                         
09810                                  TO W-1-EP                        
09811                  ELSE                                             
09812                      ADD +1      TO W-OTHERS-COUNT                
09813                      MOVE ZEROS  TO W-EARNED-PREMIUM.             
09814                                                                   
09815  4670-EXIT.                                                       
09816      EXIT.                                                        
09817                                  EJECT                            
09818  4680-CALC-REMAINING-AMT-LF.                                      
09819                                                                   
09820      MOVE LOW-VALUES             TO CP-INSURED-BIRTH-DT.          
09821      MOVE CR-AGE                 TO CP-ISSUE-AGE.                 
09822      MOVE CR-APR                 TO CP-LOAN-APR.                  
09823      MOVE CR-LOAN-TERM           TO CP-LOAN-TERM.                 
09824                                                                   
09825      IF  CR-PMT-FREQ NOT GREATER THAN ZERO                        
09826          MOVE +1                 TO CR-PMT-FREQ.                  
09827                                                                   
09828      MOVE CR-PMT-FREQ            TO CP-PAY-FREQUENCY.             
09829                                                                   
09830      MOVE REM-TRM2               TO CP-REMAINING-TERM.            
09831      MOVE CR-LFAMT               TO CP-ORIGINAL-BENEFIT           
09832                                     CP-RATING-BENEFIT-AMT.        
09833      IF CP-STATE-STD-ABBRV = 'OR'                                 
09834          COMPUTE CP-RATING-BENEFIT-AMT = CR-LFAMT + CR-LFAMT-ALT. 
09835      MOVE CR-LFAMT-ALT           TO CP-ALTERNATE-BENEFIT.         
09836      MOVE CR-LFPRM               TO CP-ORIGINAL-PREMIUM.          
09837      MOVE ZEROS                  TO CP-REMAINING-AMT              
09838                                     CP-REMAINING-AMT-PRV          
09839                                     CP-RETURN-CODE                
09840                                     CP-RETURN-CODE-2.             
09841                                                                   
09842                                                                   
09843      CALL 'ELRAMTX' USING CALCULATION-PASS-AREA                   
09844                                                                   
09845      IF  CP-RETURN-CODE NOT = ZERO                                
09846          MOVE 'ERROR OCCURED REMAINING AMT CALCULATIONS'          
09847                              TO WS-ABEND-MESSAGE                  
09848          MOVE CP-RETURN-CODE TO WS-ABEND-FILE-STATUS              
09849          MOVE 2001           TO WS-ABEND-CODE                     
09850          GO TO ABEND-PGM.                                         
09851                                                                   
09852  4680-EXIT.                                                       
09853      EXIT.                                                        
09854                                  EJECT                            
09855 ******************************************************************
09856 ***  BUILDS ACCOUNT LEVEL IBNR (DIRECT AND REIN)               ***
09857 ******************************************************************
09858                                                                   
09859  4700-CALC-OPT-IBNR.                                              
09860                                                                   
09861      MOVE EPEC-OPT-DATA-TABLE    TO EPEC-OPT-WORK-TABLE.          
09862      MOVE SPACE                  TO W-UNDERWRITING-SW.            
09863                                                                   
09864      PERFORM 4710-PROCESS-THRU-EPEC-TABLE THRU 4710-EXIT          
09865              VARYING                                              
09866          SUB2 FROM +1 BY +1                                       
09867              UNTIL                                                
CIDMOD*        SUB2 GREATER THAN 300.                                   
092602*        SUB2 GREATER THAN 220.                                   
092602         SUB2 GREATER THAN 900.                                   
09869                                                                   
09870      MOVE EPEC-UNDW-OPT-DATA-TABLE                                
09871                                  TO EPEC-OPT-WORK-TABLE.          
09872      MOVE '*'                    TO W-UNDERWRITING-SW.            
09873                                                                   
09874      PERFORM 4710-PROCESS-THRU-EPEC-TABLE THRU 4710-EXIT          
09875              VARYING                                              
09876          SUB2 FROM +1 BY +1                                       
09877              UNTIL                                                
CIDMOD*        SUB2 GREATER THAN 300.                                   
092602*        SUB2 GREATER THAN 220.                                   
092602         SUB2 GREATER THAN 900.                                   
09879                                                                   
09880  4700-EXIT.                                                       
09881      EXIT.                                                        
09882                                  EJECT                            
09883  4710-PROCESS-THRU-EPEC-TABLE.                                    
09884                                                                   
09885      IF  EPEC-REIN-CO-OPT (SUB2) = ZEROS                          
09886          GO TO 4710-EXIT.                                         
09887                                                                   
09888      IF  W-UNDERWRITTEN-DATA                                      
09889          MOVE EPEC-LEVELS-II (SUB2)  TO EPEC-WORK                 
09890      ELSE                                                         
09891          MOVE EPEC-LEVELS (SUB2)     TO EPEC-WORK.                
09892                                                                   
09893      IF EW-LEVEL-USED NOT = 'Y'                                   
09894          GO TO 4710-EXIT.                                         
09895                                                                   
09896      IF  EW-REIN-CO GREATER THAN SPACES                           
09897          MOVE 'Y'                TO W-REIN-PROCESS-SW             
09898          IF  EPEC-REIN-ONLY-DATA (SUB2)                           
09899              PERFORM 4760-PROCESS-REIN-ONLY THRU 4760-EXIT        
09900              IF  W-WK-IBNR GREATER THAN ZEROS                     
09901                  PERFORM 4770-CREATE-REIN-DETAIL THRU 4770-EXIT   
09902                  GO TO 4710-EXIT                                  
09903              ELSE                                                 
09904                  GO TO 4710-EXIT                                  
09905          ELSE                                                     
09906              IF  EW-CLM-IBNR GREATER THAN ZEROS                   
09907                  PERFORM 4770-CREATE-REIN-DETAIL THRU 4770-EXIT   
09908                  GO TO 4710-EXIT                                  
09909              ELSE                                                 
09910                  GO TO 4710-EXIT.                                 
09911                                                                   
09912      MOVE SPACES                 TO W-REIN-PROCESS-SW.            
09913                                                                   
09914      IF  EW-LF-AH EQUAL 'L'                                       
09915          IF  EPEC-LIFE-IBNR (SUB2) EQUAL ZEROS                    
09916              GO TO 4710-EXIT                                      
09917          ELSE                                                     
09918              PERFORM 4720-PROCESS-LIFE THRU 4720-EXIT             
09919              ADD W-WK-IBNR       TO W-LIFE-IBNR                   
09920      ELSE                                                         
09921          IF  EPEC-EARNED-PREMIUM-AH (SUB2)                        
09922                  GREATER THAN ZEROS                               
09923              PERFORM 4730-PROCESS-A-H THRU 4730-EXIT              
09924              ADD W-WK-IBNR       TO W-AH-IBNR                     
09925          ELSE                                                     
09926              GO TO 4710-EXIT.                                     
09927                                                                   
09928      MOVE W-WK-IBNR              TO EW-CLM-IBNR.                  
09929      MOVE EW-ISS-BEN             TO W-ISS-BEN.                    
09930      MOVE EW-ISS-BEN-GROSS       TO W-ISS-BEN-GROSS.              
09931                                                                   
09932      IF  W-UNDERWRITTEN-DATA                                      
09933          MOVE EPEC-WORK          TO EPEC-LEVELS-II (SUB2)         
09934      ELSE                                                         
09935          MOVE EPEC-WORK          TO EPEC-LEVELS (SUB2).           
09936                                                                   
09937                                                                   
09938      PERFORM 4740-CREATE-DETAIL-EXTR THRU 4740-EXIT.              
09939      PERFORM 4750-UPDATE-REIN THRU 4750-EXIT.                     
09940                                                                   
09941  4710-EXIT.                                                       
09942      EXIT.                                                        
09943                                  EJECT                            
09944  4720-PROCESS-LIFE.                                               
09945                                                                   
09946      MOVE EPEC-LIFE-IBNR (SUB2)  TO W-WK-IBNR.                    
09947                                                                   
09948  4720-EXIT.                                                       
09949      EXIT.                                                        
09950                                  EJECT                            
09951  4730-PROCESS-A-H.                                                
09952                                                                   
09953      IF  AM-TARGET-LOSS-RATIO NUMERIC                             
09954              AND                                                  
09955          AM-TARGET-LOSS-RATIO NOT EQUAL ZERO                      
09956          MOVE AM-TARGET-LOSS-RATIO                                
09957                                  TO W-TARGET-LOSS-RATIO           
09958      ELSE                                                         
09959          IF  CLAS-INDEXS NOT GREATER THAN CLAS-MAXS               
09960              MOVE STATE-TARGET-LOSS-RATIO (CLAS-INDEXS)           
09961                                  TO W-TARGET-LOSS-RATIO           
09962              PERFORM 4735-GET-BUSINESS-ADJUSTMENT THRU 4735-EXIT. 
09963                                                                   
09964      IF  W-NOT-REIN-PROCESS                                       
09965          ADD EPEC-EARNED-PREMIUM-AH (SUB2) TO W-EPEC-EP           
09966          ADD EPEC-CLAIMS-MADE-PAID (SUB2)  TO W-CLAIMS-MADE-PAID. 
09967                                                                   
09968      COMPUTE W-IBNR-1 ROUNDED                                     
09969          = EPEC-EARNED-PREMIUM-AH (SUB2)                          
09970          * COMPANY-IBNR-AH-FACTOR                                 
09971          * W-TARGET-LOSS-RATIO.                                   
09972                                                                   
09973      COMPUTE W-IBNR-2 ROUNDED                                     
09974          = (EPEC-EARNED-PREMIUM-AH (SUB2)                         
09975          * W-TARGET-LOSS-RATIO)                                   
09976          - EPEC-CLAIMS-MADE-PAID (SUB2).                          
09977                                                                   
09978      IF  W-IBNR-1 GREATER THAN W-IBNR-2                           
09979          MOVE W-IBNR-1           TO W-WK-IBNR                     
09980          IF  W-NOT-REIN-PROCESS                                   
09981              ADD +1              TO W-AH-IBNR-1-CNT               
09982              ADD W-IBNR-1        TO W-AH-IBNR-1                   
09983          ELSE                                                     
09984              NEXT SENTENCE                                        
09985      ELSE                                                         
09986          MOVE W-IBNR-2           TO W-WK-IBNR                     
09987          IF  W-NOT-REIN-PROCESS                                   
09988              ADD +1              TO W-AH-IBNR-2-CNT               
09989              ADD W-IBNR-2        TO W-AH-IBNR-2.                  
09990                                                                   
09991  4730-EXIT.                                                       
09992      EXIT.                                                        
09993                                  EJECT                            
09994  4735-GET-BUSINESS-ADJUSTMENT.                                    
09995                                                                   
09996      IF  CR-GRPTYP NOT NUMERIC                                    
09997          MOVE '99'               TO INDEXBS                       
09998      ELSE                                                         
09999          MOVE CR-GRPTYP          TO INDEXBS.                      
10000                                                                   
10001      IF  BUS-TRGT-LOSS-RATIO-MOD (INDEXBS) NUMERIC                
10002              AND                                                  
10003          BUS-TRGT-LOSS-RATIO-MOD (INDEXBS)                        
10004              NOT EQUAL ZEROS                                      
10005              AND                                                  
10006          BUS-TRGT-LOSS-RATIO-MOD (INDEXBS)                        
10007              NOT EQUAL +1.0000                                    
10008          MOVE INDEXBS           TO W-EDIT-7                       
10009          COMPUTE W-TARGET-LOSS-RATIO ROUNDED                      
10010              = W-TARGET-LOSS-RATIO                                
10011              * BUS-TRGT-LOSS-RATIO-MOD (INDEXBS).                 
10012                                                                   
10013  4735-EXIT.                                                       
10014      EXIT.                                                        
10015                                                                   
10016  4740-CREATE-DETAIL-EXTR.                                         
10017                                                                   
10018      IF DTE-PGM-OPT NOT = '1'                                     
10019          GO TO 4740-EXIT.                                         
10020                                                                   
10021      IF  EW-CLM-IBNR NOT GREATER THAN ZEROS                       
10022          GO TO 4740-EXIT.                                         
10023                                                                   
10024      MOVE EOM-RUN-DATE           TO INCURRED-DATE.                
10025      MOVE ZEROS                  TO REPORTED-DATE                 
10026                                     PAY-TO-DATE.                  
10027                                                                   
10028      MOVE SPACES                 TO TR-DTL2.                      
10029      MOVE 'ACTBENIBNR'           TO TRD-CERT.                     
10030                                                                   
10031      MOVE EOM-YR                 TO TRD-ISYR.                     
10032      MOVE EOM-MO                 TO TRD-ISMO.                     
10033      MOVE EOM-DA                 TO TRD-ISDA.                     
10034      MOVE ZERO                   TO TRD-AGE.                      
10035                                                                   
10036      IF  EW-LF-AH EQUAL 'L'                                       
10037          MOVE LIFE-OVERRIDE-L6   TO TRD-RSV-DESC                  
10038      ELSE                                                         
10039          MOVE AH-OVERRIDE-L6     TO TRD-RSV-DESC.                 
10040                                                                   
10041      MOVE SPACES                 TO TRD-PAYCURX                   
10042                                     TRD-PAYCUR-SPACE              
10043                                     TRD-FUTRSV-SPACE              
10044                                     TRD-FUTRSVX.                  
10045                                                                   
10046      MOVE EW-CLM-IBNR            TO TRD-IBNR.                     
10047      MOVE 'IBNR'                 TO TRD-IBNRX.                    
10048      MOVE EW-BEN-TYPE            TO TRD-BENEFIT-CODE.             
10049      MOVE 'BENEFIT - '           TO TRD-BENEFIT-X.                
10050                                                                   
10051      IF  W-UNDERWRITTEN-DATA                                      
10052          IF  W-REIN-IN-PROCESS                                    
10053              MOVE ' ACCT/BEN REIN RSV UW'                         
10054                                  TO TRD-RSV-ACTION                
10055          ELSE                                                     
10056              MOVE ' ACCT/BEN RESERVES UW'                         
10057                                  TO TRD-RSV-ACTION                
10058      ELSE                                                         
10059          IF  W-REIN-IN-PROCESS                                    
10060              MOVE ' ACCT/BEN REIN RSV'                            
10061                                  TO TRD-RSV-ACTION                
10062          ELSE                                                     
10063              MOVE ' ACCT/BEN RESERVES'                            
10064                                  TO TRD-RSV-ACTION.               
10065                                                                   
10066      PERFORM 8690-PRINT-DETAIL-LINE THRU 8699-PRT-DETAIL-X.       
10067                                                                   
10068      MOVE SPACES                 TO DETAIL-EXTRACT.               
10069                                                                   
10070      IF  W-REIN-IN-PROCESS                                        
10071          MOVE EW-REIN-CO         TO DE-REI-COMP                   
10072          MOVE 'R'                TO DE-REIN                       
10073          MOVE EW-CLM-IBNR        TO DE-REI-IBNR                   
10074          MOVE ZEROS              TO DE-IBNR                       
10075      ELSE                                                         
10076          MOVE EW-CLM-IBNR        TO DE-IBNR                       
10077          MOVE ZEROS              TO DE-REI-IBNR.                  
10078                                                                   
10079      MOVE 'DE'                   TO DE-RECORD-ID.                 
10080      MOVE 'Y'                    TO DE-TRANS.                     
10081      MOVE 'IBNRRSV'              TO DE-CLMNO.                     
10082      MOVE '1'                    TO DE-ENTRY-STATUS.              
10083      MOVE 'ACTBENIBNR'           TO DE-CERT.                      
10084                                                                   
10085      MOVE EOM-RUN-DATE           TO DE-EFF                        
10086                                     WS-DE-EFF-N                   
10087                                     DE-ENTRY-DTE                  
10088                                     WS-DE-ENTRY-DTE-N.            
10089      MOVE INCURRED-DATE          TO DE-RSV-INCUR                  
10090                                     WS-DE-RSV-INCUR-N.            
10091                                                                   
10092      MOVE AM-COMPANY-CD          TO DE-COMPANY-CD.                
10093      MOVE AM-MSTR-CNTRL          TO DE-CNTRL1.                    
10094      MOVE AM-EXPIRE-DT           TO DE-ACC-EXP-DTE-RSV            
10095                                     WS-DE-ACC-EXP-DTE-RSV-N.      
10096      MOVE AM-EFFECT-DT           TO DE-ACC-EFF-DTE-RSV            
10097                                     WS-DE-ACC-EFF-DTE-RSV-N.      
10098                                                                   
10099      MOVE AM-NAME                TO DE-RSV-ACC-NAME.              
10100      MOVE AM-RECALC-COMM         TO DE-RECALC-CODE.               
10101      MOVE AM-REMIT-TO            TO DE-REMIT-TO.                  
10102      MOVE AM-REPORT-CODE-1       TO DE-REPORT-CODE-1.             
10103      MOVE AM-REPORT-CODE-2       TO DE-REPORT-CODE-2.             

042607     IF (CR-ENTRY-BATCH (1:2) = 'VA')
              OR (CR-LF-EXIT-BATCH (1:2) = 'VA')
              OR (CR-AH-EXIT-BATCH (1:2) = 'VA')
              MOVE 'VA '                 TO DE-NCL-POOL-CODE
           END-IF

10104 *    MOVE AM-POOL-PRIME          TO DE-NCL-POOL-CODE.             
10105      MOVE AM-IG                  TO DE-IG.                        
10106                                                                   
10107      IF W-UNDERWRITTEN-DATA                                       
10108          MOVE 'Y'                TO DE-UNDERWRITING-CODE          
10109      ELSE                                                         
10110          MOVE 'N'                TO DE-UNDERWRITING-CODE.         
10111                                                                   
10112      IF  EW-LF-AH EQUAL 'L'                                       
10113          MOVE EW-BEN-TYPE        TO DE-LF-TYPE                    
10114          MOVE '1'                TO DE-RESERVE-TYPE               
10115          MOVE ZEROS              TO DE-AH-TYPE                    
10116      ELSE                                                         
10117          MOVE EW-BEN-TYPE        TO DE-AH-TYPE                    
10118          MOVE '2'                TO DE-RESERVE-TYPE               
10119          MOVE ZEROS              TO DE-LF-TYPE.                   
10120                                                                   
10121      MOVE SPACES                 TO DE-NAME                       
10122                                     DE-ACC-GPCD                   
10123                                     DE-AH-STAT-CDE                
10124                                     DE-LF-STAT-CDE                
10125                                     DE-MEMBER-NO                  
10126                                     DE-NAME                       
10127                                     DE-SEX.                       
10128      MOVE ZEROS                  TO DE-AGE                        
10129                                     DE-AH-BEN                     
10130                                     DE-AH-CANC-DTE                
10131                                     WS-DE-AH-CANC-DTE-N           
10132                                     DE-AH-CANC-EXIT-DT            
10133                                     WS-DE-AH-CANC-EXIT-DT-N       
10134                                     DE-AH-PRM                     
10135                                     DE-AH-RFND                    
10136                                     DE-AH-TERM                    
10137                                     DE-APR                        
10138                                     DE-CANCEL-CNT-ITD             
10139                                     DE-CANCEL-CNT-YTD             
10140                                     DE-FUTRSV                     
10141                                     DE-LF-BEN                     
10142                                     DE-LF-BEN-ALT                 
10143                                     DE-LF-CANC-DTE                
10144                                     WS-DE-LF-CANC-DTE-N           
10145                                     DE-LF-CANC-EXIT-DT            
10146                                     WS-DE-LF-CANC-EXIT-DT-N       
10147                                     DE-LF-CNBEN                   
10148                                     DE-LF-CNBEN-ALT               
10149                                     DE-LF-PRM                     
10150                                     DE-LF-PRM-ALT                 
10151                                     DE-LF-RFND                    
10152                                     DE-LF-TERM                    
10153                                     DE-LIVES                      
10154                                     DE-PAYCUR                     
10155                                     DE-PMT-FREQ                   
10156                                     DE-REI-AHAMT                  
10157                                     DE-REI-AHPRM                  
10158                                     DE-REI-AHRFND                 
10159                                     DE-REI-CNAMT                  
10160                                     DE-REI-FUTRSV                 
10161                                     DE-REI-LFAMT                  
10162                                     DE-REI-LFPRM                  
10163                                     DE-REI-LFRFND                 
10164                                     DE-REI-PAYCUR                 
10165                                     DE-RSV-REPORTED               
10166                                     DE-RSV-PAYTO                  
10167                                     WS-DE-RSV-PAYTO-N             
011904                                    DE-MOB-NET-TOT-FEES
10168                                     DE-SOC-SEC-NO.                
10169                                                                   
10170      MOVE SPACES                 TO W-REIN-PROCESS-IND.           
10171                                                                   
10172      PERFORM 2500-WRITE-DETAIL-EXTRACTS                           
10173          THRU 2599-WRT-DETAIL-X.                                  
10174      ADD +1                      TO BC-G-CRS.                     
10175                                                                   
10176  4740-EXIT.                                                       
10177      EXIT.                                                        
10178                                  EJECT                            
10179  4750-UPDATE-REIN.                                                
10180                                                                   
10181      COMPUTE SUB3 = SUB2 + 1.                                     
10182                                                                   
10183      IF  W-UNDERWRITTEN-DATA                                      
10184          MOVE EPEC-SUB-KEY-II (SUB2)                              
10185                                  TO W-SUB-KEY                     
10186      ELSE                                                         
10187          MOVE EPEC-SUB-KEY (SUB2)                                 
10188                                  TO W-SUB-KEY.                    
10189                                                                   
10190      PERFORM 4755-MATCH-REIN-EPEC THRU 4755-EXIT                  
10191              VARYING                                              
10192          SUB3 FROM SUB3 BY +1                                     
10193              UNTIL                                                
092602         SUB3 GREATER THAN +900                                   
092602*        SUB3 GREATER THAN +220                                   
CIDMOD*        SUB3 GREATER THAN +300                                   
10195              OR                                                   
10196          EPEC-LF-AH-OPT (SUB3) EQUAL SPACES.                      
10197                                                                   
10198  4750-EXIT.                                                       
10199      EXIT.                                                        
10200                                  EJECT                            
10201  4755-MATCH-REIN-EPEC.                                            
10202                                                                   
10203      IF  W-UNDERWRITTEN-DATA                                      
10204          MOVE EPEC-LEVELS-II (SUB3)                               
10205                                  TO EPEC-WORK                     
10206      ELSE                                                         
10207          MOVE EPEC-LEVELS (SUB3) TO EPEC-WORK.                    
10208                                                                   
10209      IF  EPEC-LINE-NOT-USED (SUB3)                                
10210          GO TO 4755-EXIT.                                         
10211                                                                   
10212      IF  EW-SUB-KEY NOT EQUAL W-SUB-KEY                           
10213              OR                                                   
10214          EW-REIN-CO EQUAL SPACES                                  
10215          GO TO 4755-EXIT.                                         
10216                                                                   
10217      IF  W-ISS-BEN GREATER THAN ZEROS                             
10218          IF  EPEC-REIN-ONLY-DATA (SUB3)                           
10219              COMPUTE W-FACTOR ROUNDED                             
10220                  = (EW-ISS-BEN                                    
10221                  - EPEC-REIN-ISS-BEN (SUB3))                      
10222                  / W-ISS-BEN                                      
10223          ELSE                                                     
10224              COMPUTE W-FACTOR ROUNDED                             
10225                  = EW-ISS-BEN                                     
10226                  / W-ISS-BEN.                                     
10227                                                                   
10228      COMPUTE EW-CLM-IBNR ROUNDED                                  
10229          = EW-CLM-IBNR                                            
10230          + (W-WK-IBNR * W-FACTOR).                                
10231                                                                   
10232      IF  W-UNDERWRITTEN-DATA                                      
10233          MOVE EPEC-WORK          TO EPEC-LEVELS-II (SUB3)         
10234      ELSE                                                         
10235          MOVE EPEC-WORK          TO EPEC-LEVELS (SUB3).           
10236                                                                   
10237  4755-EXIT.                                                       
10238      EXIT.                                                        
10239                                  EJECT                            
10240  4760-PROCESS-REIN-ONLY.                                          
10241                                                                   
10242      MOVE ZEROS                  TO W-WK-IBNR.                    
10243                                                                   
10244      IF  EPEC-LINE-NOT-USED (SUB2)                                
10245          GO TO 4760-EXIT.                                         
10246                                                                   
10247      IF  EW-LF-AH EQUAL 'L'                                       
10248          IF  EPEC-LIFE-IBNR (SUB2) EQUAL ZEROS                    
10249              GO TO 4760-EXIT                                      
10250          ELSE                                                     
10251              PERFORM 4720-PROCESS-LIFE THRU 4720-EXIT             
10252              ADD W-WK-IBNR       TO EW-CLM-IBNR                   
10253      ELSE                                                         
10254          IF  EPEC-EARNED-PREMIUM-AH (SUB2) EQUAL ZEROS            
10255              GO TO 4760-EXIT                                      
10256          ELSE                                                     
10257              PERFORM 4730-PROCESS-A-H THRU 4730-EXIT              
10258              ADD W-WK-IBNR       TO EW-CLM-IBNR.                  
10259                                                                   
10260  4760-EXIT.                                                       
10261      EXIT.                                                        
10262                                  EJECT                            
10263  4770-CREATE-REIN-DETAIL.                                         
10264                                                                   
10265      MOVE 'Y'                    TO W-REIN-PROCESS-IND.           
10266      PERFORM 4740-CREATE-DETAIL-EXTR THRU 4740-EXIT.              
10267                                                                   
10268  4770-EXIT.                                                       
10269      EXIT.                                                        
10270                                                                   
10271  4799-EXIT.                                                       
10272      EXIT.                                                        
10273  EJECT                                                            
10274  4800-CALC-EARNED-PREMIUM-AH.                                     
10275                                                                   
10276      MOVE +0                         TO CNC-BEN  EPR-R78          
10277                                         ISS-PRM  EPR-PRO          
10278                                         CNC-PRM  EPR-ST           
10279                                         WX-CLM-AMT                
10280                                         WX-CLM-EXP                
10281                                         WX-CLM-CNT                
10282                                         WX-CLM-CRT                
10283                                         WX-CNC-CNT                
PEMMOD                                        ISS-PRMTAX
10284                                         WX-ACTIVE-COUNT.          
10285      MOVE SPACE                      TO A-H-EARNED-SWITCH.        
10286                                                                   
10287      MOVE WE-AHPRM TO ISS-PRM.                                    
10288                                                                   
10289      IF DTE-CLIENT = 'GIC'                                        
10290          IF REIN-EP-FLG NOT = 'R'  AND                            
10291             CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'B'  AND             
122002            (CR-ENTRY-STATUS NOT = '5' AND '3' AND 'M')           
10293             MOVE CR-AHPRM-CALC TO ISS-PRM.                        
10294                                                                   
10295      IF REIN-EP-FLG = 'R'  AND                                    
10296         REIN-AH-RUNOFF-SW (SUB5) = 'N'                            
10297          NEXT SENTENCE                                            
10298      ELSE                                                         
10299          GO TO 4810-EP-DO-AH-CLM.                                 
10300                                                                   
10301      IF CR-DIS-DT NOT GREATER THAN EARNING-STOP-DATE              
10302          IF WE-DSPY-AMT NOT = +0                                  
10303              ADD WE-DSPY-AMT    TO WX-CLM-AMT                     
10304              ADD WE-DSPY-EXP    TO WX-CLM-EXP                     
10305              ADD CR-NUM-DIS-CLM TO WX-CLM-CNT                     
10306              ADD +1             TO WX-CLM-CNT.                    
10307                                                                   
10308      IF CR-AH-STATUS-AT-SETTLEMENT NOT = ' '                      
10309          IF CR-DIS-DT GREATER THAN EARNING-STOP-DATE              
10310              GO TO 4830-EP-DO-AH-EP.                              
10311                                                                   
10312      IF CR-AH-STATUS-AT-CANCEL NOT = ' '                          
10313          IF CR-AH-CANC-DT GREATER THAN EARNING-STOP-DATE          
10314              GO TO 4830-EP-DO-AH-EP.                              
10315                                                                   
10316      GO TO 4815-EP-DO-AH-CLM-EPR.                                 
10317                                                                   
10318  4810-EP-DO-AH-CLM.                                               
10319                                                                   
10320      IF WE-DSPY-AMT NOT = +0                                      
10321          ADD WE-DSPY-AMT    TO WX-CLM-AMT                         
10322          ADD WE-DSPY-EXP    TO WX-CLM-EXP                         
10323          ADD CR-NUM-DIS-CLM TO WX-CLM-CNT                         
10324          ADD +1             TO WX-CLM-CNT.                        
10325                                                                   
10326  4815-EP-DO-AH-CLM-EPR.                                           
10327                                                                   
10328      IF CR-AH-STATUS-AT-SETTLEMENT NOT = ' '                      
10329          MOVE WE-AHPRM TO EPR-R78  EPR-PRO  EPR-ST.               
10330                                                                   
10331  4820-EP-DO-AH-CNCL.                                              
10332                                                                   
10333      IF CR-AH-STATUS-AT-CANCEL = ' '                              
10334          IF CR-AH-STATUS-AT-SETTLEMENT NOT = ' '  AND             
10335             CLAS-I-CALC-TYPE (CLAS-INDEXA) NOT = 'Z'              
10336              GO TO 4899-EXIT                                      
10337          ELSE                                                     
10338              GO TO 4830-EP-DO-AH-EP.                              
10339                                                                   
           IF (DTE-CLIENT = 'DCC')
092705        AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
              IF CR-AHRFND NOT = ZEROS
                 ADD +1                TO WX-CNC-CNT
              END-IF
           ELSE
10340         ADD +1                   TO WX-CNC-CNT
           END-IF
10341                                                                   
10342      MOVE WE-AHRFND TO CNC-PRM.                                   
10343                                                                   
PEMTMP*    MOVE CR-DT                 TO  DC-GREG-DATE-CYMD.            
PEMTMP*    MOVE 'L'                   TO  DC-OPTION-CODE.               
PEMTMP*    PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
PEMTMP*    MOVE DC-BIN-DATE-1         TO  WS-CR-BIN-DATE.               
10348                                                                   
10349      MOVE CR-AH-CANC-DT         TO DC-GREG-DATE-CYMD.             
10350      MOVE 'L'                   TO  DC-OPTION-CODE.               
10351      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
10352      MOVE DC-BIN-DATE-1         TO  WS-BIN-CANCEL-DATE.           
10353                                                                   
10354      MOVE WS-CR-BIN-DATE          TO  DC-BIN-DATE-1.              
10355      MOVE WS-BIN-CANCEL-DATE      TO  DC-BIN-DATE-2.              
10356      MOVE '1'                     TO  DC-OPTION-CODE.             
10357      MOVE ZEROS                   TO  MONTHS-DIFF-AH.             
10358      MOVE ZEROS                   TO  DC-ELAPSED-MONTHS           
10359                                       DC-ODD-DAYS-OVER            
10360                                       DC-ELAPSED-DAYS.            
10361      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
10362      MOVE DC-ELAPSED-MONTHS       TO  MONTHS-DIFF-AH.             
10363                                                                   
10364      IF DC-ODD-DAYS-OVER   GREATER THAN  ZERO                     
10365          ADD  +1                  TO  MONTHS-DIFF-AH.             
10366                                                                   
10367      MOVE  'YYYYYYYYYY'           TO  WS-AH-CHARGEBACK-LEVELS.    
10368                                                                   
10369      IF DTE-CLIENT = 'ACE'                                        
10370          GO TO 4820-CONTINUE-AH-CANCEL.                           
10371                                                                   
10372      MOVE  ZEROS                  TO  SUB.                        
10373                                                                   
10374  4820-AH-CHARGEBACK-LOOP.                                         
10375                                                                   
10376      ADD    +1                    TO  SUB.                        
10377                                                                   
10378      IF  SUB  GREATER THAN  +10                                   
10379          GO TO  4820-CONTINUE-AH-CANCEL.                          
10380                                                                   
10381      IF  AM-COMM-CHARGEBACK (SUB)  NOT  NUMERIC  OR               
10382          AM-COMM-CHARGEBACK (SUB)  =  ZEROS                       
10383          GO TO  4820-AH-CHARGEBACK-LOOP.                          
10384                                                                   
10385      IF  AM-COMM-CHARGEBACK (SUB)  EQUAL    '99'                  
10386          MOVE  'N'    TO  WS-AH-CHARGEBACK-SW (SUB).              
10387                                                                   
10388      IF  (MONTHS-DIFF-AH GREATER THAN AM-COMM-CHARGEBACK (SUB))
               OR (CR-AHRFND = ZEROS)
10389          MOVE  'N'        TO  WS-AH-CHARGEBACK-SW (SUB)
           END-IF
10390                                                                   
10391      GO TO  4820-AH-CHARGEBACK-LOOP.                              
10392                                                                   
10393                                                                   
10394  4820-CONTINUE-AH-CANCEL.                                         
10395                                                                   
10396      IF DTE-CLIENT = 'GIC'                                        
10397          IF REIN-EP-FLG NOT = 'R'  AND                            
10398             CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'B'                  
10399             MOVE CR-AHRFND-CALC TO CNC-PRM.                       
10400                                                                   
10401      COMPUTE EPR-R78 = WE-AHPRM - WE-AHRFND.                      
10402      MOVE EPR-R78 TO EPR-PRO  EPR-ST.                             
10403                                                                   
10404      IF CR-AH-LUMP-SUM-DISAB                                      
10405          GO TO 4899-EXIT.                                         
10406                                                                   
10407      MOVE WE-AHAMT TO CNC-BEN.                                    
10408                                                                   
10409      GO TO 4899-EXIT.                                             
10410                                                                   
10411  4825-EP-DO-AH-CNCL-SUM.                                          
10412                                                                   
10413      IF WE-DSPY-AMT NOT = +0                                      
10414          ADD WE-DSPY-AMT    TO WX-CLM-AMT                         
10415          ADD WE-DSPY-EXP    TO WX-CLM-EXP                         
10416          ADD CR-NUM-DIS-CLM TO WX-CLM-CNT                         
10417          ADD +1             TO WX-CLM-CNT.                        
10418                                                                   
10419      IF WE-AHRFND = ZERO                                          
10420         GO TO 4830-EP-DO-AH-EP.                                   
10421                                                                   
10422      MOVE WE-AHRFND TO CNC-PRM.                                   
10423                                                                   
10424      ADD CR-SUM-CAN-CNT-ITD TO WX-CNC-CNT.                        
10425                                                                   
10426      MOVE  'YYYYYYYYYY'           TO  WS-AH-CHARGEBACK-LEVELS.    
10427                                                                   
10428      IF WE-AHRFND NOT LESS WE-AHPRM                               
10429          MOVE WE-AHAMT TO CNC-BEN                                 
10430           GO TO 4830-EP-DO-AH-EP.                                 
10431                                                                   
10432      COMPUTE TEMP-6 ROUNDED = WE-AHRFND / WE-AHPRM.               
10433      COMPUTE TEMP-7 ROUNDED = TEMP-6 * WE-AHAMT.                  
10434      MOVE TEMP-7 TO CNC-BEN.                                      
10435                                                                   
10436  4830-EP-DO-AH-EP.                                                
10437                                                                   
10438      MOVE EOM-RUN-DATE TO VALUATION-DT.                           
10439      MOVE '8'           TO W-VAL-SOURCE.                          
10440                                                                   
10441      IF REIN-EP-FLG = 'R'                                         
10442        IF REIN-REM-SW (SUB5) = 'I'                                
10443            MOVE WE-AHPRM            TO EPR-R78  EPR-PRO  EPR-ST   
10444            GO TO 4899-EXIT.                                       
10445                                                                   
10446      IF REIN-EP-FLG = 'R'                                         
10447          IF REIN-AH-RUNOFF-SW (SUB5) = 'N'  AND                   
10448             EARNING-STOP-DATE LESS THAN VALUATION-DT              
10449              MOVE '*'               TO A-H-EARNED-SWITCH          
10450              MOVE EARNING-STOP-DATE TO VALUATION-DT               
10451              MOVE '10'              TO W-VAL-SOURCE.              
10452                                                                   
10453      PERFORM 8500-REMAINING-TERM-ROUTINE THRU 8599-REM-TERM-X.    
10454                                                                   
10455      IF REM-TRM2 GREATER CR-AH-TERM                               
10456          MOVE +0 TO EPR-R78  EPR-PRO  EPR-ST                      
10457          GO TO 4899-EXIT.                                         
10458                                                                   
10459      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'Z'                      
10460          COMPUTE WE-AHPRM = WE-AHPRM - WE-AHRFND.                 
10461 *                                                                 
10462  4835-CHECK-OB-AH-NCL.                                            
10463                                                                   
10464      IF (DTE-CLIENT EQUAL 'NCL') AND                              
10465         (CLAS-I-CALC-TYPE (CLAS-INDEXA) EQUAL 'B')                
10466            MOVE WE-AHPRM         TO EPR-R78                       
10467                                     EPR-PRO                       
10468                                     EPR-ST                        
10469         IF (CR-CCYY EQUAL EOM-CCYY) AND                           
10470            (CR-MO EQUAL EOM-MO)                                   
10471            MOVE '*'            TO OB-DATE-1-SWITCH                
10472            MOVE EW-REIN-CO     TO WS-OB-REIN-CO (SUB3)            
10473            MOVE AH-OVERRIDE-L1 TO WS-OB-LF-AH (SUB3)              
10474            MOVE CR-AHTYP       TO WS-OB-BENEFIT-CODE (SUB3)       
10475            COMPUTE WS-EPR-R78 (SUB3 1) =                          
10476                    WS-EPR-R78 (SUB3 1) + (WE-AHPRM * .5)          
10477            GO TO 4899-EXIT                                        
10478         ELSE                                                      
10479         IF (CR-CCYY EQUAL OB-CCYY (1)) AND                        
10480            (CR-MO EQUAL OB-MO (1))                                
10481            MOVE '*'            TO OB-DATE-2-SWITCH                
10482            MOVE EW-REIN-CO     TO WS-OB-REIN-CO (SUB3)            
10483            MOVE AH-OVERRIDE-L1 TO WS-OB-LF-AH (SUB3)              
10484            MOVE CR-AHTYP       TO WS-OB-BENEFIT-CODE (SUB3)       
10485            COMPUTE WS-EPR-R78 (SUB3 2) =                          
10486                    WS-EPR-R78 (SUB3 2) + (WE-AHPRM * .5)          
10487            GO TO 4899-EXIT                                        
10488         ELSE                                                      
10489         IF (CR-CCYY EQUAL OB-CCYY (2)) AND                        
10490            (CR-MO EQUAL OB-MO (2))                                
10491            MOVE '*'            TO OB-DATE-3-SWITCH                
10492            MOVE EW-REIN-CO     TO WS-OB-REIN-CO (SUB3)            
10493            MOVE AH-OVERRIDE-L1 TO WS-OB-LF-AH (SUB3)              
10494            MOVE CR-AHTYP       TO WS-OB-BENEFIT-CODE (SUB3)       
10495            COMPUTE WS-EPR-R78 (SUB3 3) =                          
10496                    WS-EPR-R78 (SUB3 3) + (WE-AHPRM * .5)          
10497            GO TO 4899-EXIT                                        
10498         ELSE                                                      
10499            GO TO 4899-EXIT.                                       
10500                                                                   
10501      IF REM-TRM2 NOT GREATER +0                                   
10502          MOVE WE-AHPRM TO EPR-R78  EPR-PRO  EPR-ST                
10503          GO TO 4899-EXIT.                                         
10504                                                                   
10505      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'B'                      
10506          MOVE WE-AHPRM TO EPR-R78  EPR-PRO  EPR-ST                
10507          GO TO 4899-EXIT.                                         
10508                                                                   
10509      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) NOT = 'Z'                  
10510          MOVE +1 TO WX-ACTIVE-COUNT                               
10511      ELSE                                                         
10512          COMPUTE WX-ACTIVE-COUNT = CR-LIVES - CR-SUM-CAN-CNT-ITD. 
10513                                                                   
10514      COMPUTE TEMP-1 = CR-AH-TERM * (CR-AH-TERM + 1).              
10515      COMPUTE TEMP-2 = REM-TRM1 * (REM-TRM1 + 1).                  
10516      IF DTE-R78 = '1'                                             
10517          COMPUTE TEMP-2 = REM-TRM1 * REM-TRM1.                    
10518      COMPUTE TEMP-4 ROUNDED = TEMP-2 / TEMP-1.                    
10519                                                                   
10520      IF DTE-CLIENT = 'MIC' OR 'MCC'                               
10521          COMPUTE TEMP-5 ROUNDED = (REM-TRM1 - .5) / CR-AH-TERM    
10522      ELSE                                                         
10523          COMPUTE TEMP-5 ROUNDED = REM-TRM1 / CR-AH-TERM.          
10524                                                                   
10525      COMPUTE EPR-R78 ROUNDED = WE-AHPRM * TEMP-4.                 
10526      COMPUTE EPR-R78 = WE-AHPRM - EPR-R78.                        
10527                                                                   
10528      COMPUTE EPR-PRO ROUNDED = WE-AHPRM * TEMP-5.                 
10529      COMPUTE EPR-PRO = WE-AHPRM - EPR-PRO.                        
10530                                                                   
10531      MOVE CLAS-I-EP (CLAS-INDEXA) TO AH-EARN-METHOD.              
10532                                                                   
PEMMOD*    IF AM-EARN-METHOD-A NOT EQUAL SPACES AND ZEROS               
PEMMOD*        MOVE AM-EARN-METHOD-A TO AH-EARN-METHOD.                 
10535                                                                   
10536      MOVE SPACES TO STATUTORY-SWITCH-AH.                          
10537                                                                   
10538      IF (STATE-ABBR (CLAS-INDEXS) = 'VA') AND                     
10539         (CR-DT GREATER THAN  19921231)                            
040504        AND (DTE-CLIENT NOT = 'DCC')
10540          IF CR-AH-TERM GREATER THAN  +61                          
10541              MOVE 'A' TO AH-EARN-METHOD                           
10542              MOVE '*' TO STATUTORY-SWITCH-AH                      
10543              GO TO 4845-EP-DO-AH-EP-AN                            
10544          ELSE                                                     
10545              GO TO 4860-EP-DO-AH-STATUTORY.                       
10546                                                                   
10547      IF (AH-EARN-METHOD EQUAL 'N')
                        OR
              ((DTE-CLIENT = 'DCC')
092705         AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
               AND (CLAS-I-REFUND-METHOD (CLAS-INDEXA) NOT = 'G'))
10548          GO TO 4840-EP-DO-AH-EP-NP
           END-IF
10549                                                                   
10550      IF AH-EARN-METHOD EQUAL 'A' OR 'C'                           
10551          MOVE 'A' TO AH-EARN-METHOD                               
10552          GO TO 4845-EP-DO-AH-EP-AN.                               
10553                                                                   
10554      GO TO 4860-EP-DO-AH-STATUTORY.                               
10555                                                                   
10556  4840-EP-DO-AH-EP-NP.                                             
10557                                                                   
10558      IF WE-AHPRM = ZERO                                           
10559          GO TO 4860-EP-DO-AH-STATUTORY.                           
10560                                                                   
10561      IF CR-APR GREATER THAN +0                                    
10562          NEXT SENTENCE                                            
10563      ELSE                                                         
10564          GO TO 4860-EP-DO-AH-STATUTORY.                           
10565                                                                   
10566      IF DTE-CLIENT = 'NCL'                                        
10567        IF CR-DT LESS THAN 19910101                                
10568            GO TO 4860-EP-DO-AH-STATUTORY.                         
10569                                                                   
10570      IF REM-TRM1 = CR-AH-TERM                                     
10571          MOVE +0 TO EPR-R78  EPR-PRO  EPR-ST                      
10572          GO TO 4899-EXIT.                                         
10573                                                                   
10574      MOVE CR-APR TO NP-APR.                                       
10575      MOVE CR-AH-TERM TO NP-ORIG  NP-CAP.                          
10576      MOVE REM-TRM1 TO NP-REM.                                     
10577      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXA) TO NP-OPT.               
10578      IF NP-OPT = 'T' OR 'U' OR 'V' OR 'W' OR 'X'                  
10579          MOVE CR-LOAN-TERM TO NP-ORIG.                            

042904     IF (DTE-CLIENT = 'DCC')
092705        AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
042904        MOVE 'S'                 TO NP-OPT
042904     ELSE
042904        MOVE 'R'                 TO NP-OPT
042904     END-IF
042904

042904*    MOVE 'R' TO NP-OPT.                                          
10581      CALL 'ECSNETRM'                                              
10582          USING NP-APR NP-ORIG NP-REM NP-OPT NP-CAP NP-FACTOR.     
10583                                                                   
042904     IF (DTE-CLIENT = 'DCC')
092705        AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
              COMPUTE EPR-ST ROUNDED =
                 WE-AHPRM - (NP-FACTOR * WE-AHPRM)
              GO TO 4899-EXIT
           ELSE
10584         COMPUTE EPR-R78 ROUNDED =
                 WE-AHPRM - (NP-FACTOR * WE-AHPRM)
           END-IF
10585                                                                   
10586      GO TO 4860-EP-DO-AH-STATUTORY.                               
10587                                                                   
10588  4845-EP-DO-AH-EP-AN.                                             
10589      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
10590      MOVE 'L'                        TO DC-OPTION-CODE.           
10591      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
10592      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
10593      MOVE CLAS-I-RL-AH (CLAS-INDEXA) TO CP-BENEFIT-TYPE.          
10594      MOVE SPACES                     TO CP-ACCT-FLD-5.            
10595      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
10596      MOVE STATE-SUB (CLAS-INDEXS)    TO CP-STATE.                 
10597      MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV.       
10598      MOVE CR-AGE                     TO CP-ISSUE-AGE.             
10599      MOVE REM-TRM1                   TO CP-ORIGINAL-TERM.         
10600      MOVE CR-AHAMT                   TO CP-ORIGINAL-BENEFIT       
10601                                         CP-RATING-BENEFIT-AMT.    
10602      IF CP-STATE-STD-ABBRV = 'OR'                                 
10603          COMPUTE CP-RATING-BENEFIT-AMT =                          
10604                                   CR-AHAMT * REM-TRM1.            
10605      MOVE CR-APR                     TO CP-LOAN-APR.              
10606      MOVE CR-PMT-FREQ                TO CP-PAY-FREQUENCY.         
10607      MOVE AH-EARN-METHOD             TO CP-EARNING-METHOD.        
10608      MOVE '3'                        TO CP-PROCESS-TYPE.          
10609      MOVE CLAS-I-BAL (CLAS-INDEXA)   TO CP-SPECIAL-CALC-CD.       
10610      MOVE CR-LOAN-TERM               TO CP-LOAN-TERM.             
10611      IF CR-RATING-CLASS NOT = SPACE AND ZERO                      
10612          MOVE CR-RATING-CLASS        TO CP-CLASS-CODE             
10613      ELSE                                                         
10614          MOVE AM-CAL-TABLE           TO CP-CLASS-CODE.            
10615      MOVE AM-AH-DEVIATION            TO CP-DEVIATION-CODE.        
10616      MOVE CR-AHTYP                   TO CP-BENEFIT-CD.            
10617      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
10618      MOVE CR-PMT-EXTENSION-DAYS      TO CP-TERM-OR-EXT-DAYS.      
10619      MOVE AH-OVERRIDE-L1             TO CP-AH-OVERRIDE-CODE.      
10620      MOVE AM-AH-DEVIATION-PCT        TO CP-RATE-DEV-PCT.          
10621                                                                   
10622      PERFORM 8100-GET-RATE THRU 8199-GET-RATE-X.                  
10623                                                                   
10624      IF CP-ERROR-RATE-NOT-FOUND  OR                               
10625         CP-ERROR-RATE-IS-ZERO  OR                                 
10626         CP-ERROR-IN-DATES                                         
10627          GO TO 4860-EP-DO-AH-STATUTORY.                           
10628                                                                   
10629      COMPUTE EPR-R78 ROUNDED =                                    
10630              ((REM-TRM1 * WE-AHAMT) / +100) *                     
10631                CP-PREMIUM-RATE.                                   
10632                                                                   
10633      COMPUTE EPR-R78 = WE-AHPRM - EPR-R78.                        
10634                                                                   
10635      IF STATUTORY-REQUIREMENT-AH                                  
10636          MOVE EPR-R78                TO EPR-ST                    
10637          GO TO 4899-EXIT.                                         
10638                                                                   
10639  4860-EP-DO-AH-STATUTORY.                                         

           IF (DTE-CLIENT = 'DCC')
092705        AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
              AND (CLAS-I-REFUND-METHOD (CLAS-INDEXA) = 'G')
              MOVE EPR-R78             TO EPR-ST
              GO TO 4899-EXIT
           END-IF

10640      MOVE EPR-R78                    TO EPR-ST.                   
10641                                                                   
10642      MOVE +0                         TO SUB1.                     
10643                                                                   
10644  4865-EP-AH-STAT-LOOP.                                            
10645      ADD +1 TO SUB1.                                              
10646                                                                   
10647      IF STATE-ABBR (CLAS-INDEXS) = AH-RES-STATE-ENT (SUB1)        
10648          MOVE EPR-PRO                TO EPR-ST                    
10649      ELSE                                                         
10650          IF AH-RES-STATE-ENT (SUB1) NOT = HIGH-VALUES             
10651              GO TO 4865-EP-AH-STAT-LOOP.                          
10652                                                                   
10653      IF (STATE-ABBR (CLAS-INDEXS) = 'OH'  AND                     
10654          CR-DT GREATER THAN 19831031)  OR                         
10655                                                                   
10656         (STATE-ABBR (CLAS-INDEXS) = 'OK'  AND                     
10657          CR-DT GREATER THAN 19820629)  OR                         
10658                                                                   
10659         (STATE-ABBR (CLAS-INDEXS) = 'TX'  AND                     
10660          CR-DT GREATER THAN 19830831)                             
10661                                                                   
10662          COMPUTE EPR-ST = (EPR-R78 + EPR-PRO) / 2.                
10663                                                                   
10664      IF DTE-CLIENT = 'CSO'  OR  'CID'                             
10665        IF STATE-ABBR (CLAS-INDEXS) = 'CO'                         
10666          IF CR-DT LESS THAN 19970101                              
10667              MOVE EPR-R78            TO EPR-ST                    
10668          ELSE                                                     
10669              COMPUTE EPR-ST = (EPR-R78 + EPR-PRO) / 2.            
10670                                                                   
           IF DTE-CLIENT = 'DCC'
              IF CLAS-I-BAL (CLAS-INDEXA) = 'C'
                 MOVE EPR-PRO          TO EPR-ST
              ELSE
                 COMPUTE EPR-ST = (EPR-R78 + EPR-PRO) / 2
              END-IF
           END-IF     
           .
10671  4899-EXIT.                                                       
10672      EXIT.                                                        
10673 ******************************************************************
10674 ***         W R I T E   S U M M A R Y   E X T R A C T S        ***
10675 ******************************************************************
10676                                                                   
10677  4900-WRITE-SUMMARY-EXTRACTS.                                     
10678                                                                   
10679      IF DTE-CLIENT = 'HER'                                        
10680          IF EP-RECORD-ID = 'EC'  AND                              
10681             EP-REINCO = 'LNL'                                     
10682              GO TO 4999-WRT-SUMMARY-X.                            
10683                                                                   
10684      MOVE EOM-RUN-DATE            TO EP-RUN-DTE                   
10685                                      WS-EP-RUN-DTE-N.             
10686                                                                   
10687      IF W-UNDERWRITTEN-DATA                                       
10688          MOVE 'Y'                 TO EP-UNDERWRITING-CODE         
10689      ELSE                                                         
10690          MOVE 'N'                 TO EP-UNDERWRITING-CODE.        
10691                                                                   
10692      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
10693          IF VALID-EP-ID                                           
10694              MOVE 'Y'             TO EP-NEW-DATA-IND.             
10695                                                                   
10696      WRITE EP-RECORD.                                             
10697                                                                   
10698      ADD +1                       TO EP-CNT.                      
10699                                                                   
10700  4999-WRT-SUMMARY-X.                                              
10701      EXIT.                                                        
10702  EJECT                                                            
10703 ******************************************************************
10704 ***       T O T A L   P A G E   S E T U P   R O U T I N E      ***
10705 ******************************************************************
10706                                                                   
10707  5000-DATE-RANGE-BREAK.                                           
10708                                                                   
10709      IF NO-TRANSACTION-WAS-PROCESSED                              
10710          GO TO 5099-DATE-RANGE-BREAK-X.                           
10711                                                                   
10712      ADD +1  TO RANGE-CNT.                                        
10713      MOVE +1 TO SUB6.                                             
10714      MOVE +2 TO SUB7.                                             
10715      PERFORM 5800-ADD-PAGES-LEVELS THRU 5899-ADD-PAGES-LEVELS-X.  
10716      IF LN-CNT GREATER +50                                        
10717          PERFORM 8700-PRINT-REPORT-HEADINGS THRU                  
10718                  8750-PRT-RANGE-HEADING.                          
10719                                                                   
10720      MOVE SPACES                     TO PRT.                      
10721      PERFORM 8800-PRINT-ROUTINE.                                  
10722      MOVE 'DATE RANGE'               TO TPH2-DESC.                
10723      MOVE '  TOTALS'                 TO TPH3-DESC.                
10724      PERFORM 6100-ACCT-TOTAL-HEADINGS THRU 6199-PAGE-HEADINGS-X.  
10725      PERFORM 6500-PRINT-TOTAL-PAGE THRU 6599-TOTAL-PAGE-PRINT-X.  
10726      PERFORM 5900-CLEAR-TOTAL-PAGES                               
10727          THRU 5999-CLEAR-TOTAL-PAGES-X                            
10728              VARYING SUB6 FROM 1 BY 1 UNTIL SUB6 GREATER 1.       
10729                                                                   
10730  5099-DATE-RANGE-BREAK-X.                                         
10731      EXIT.                                                        
10732                                                                   
10733  5100-ACCOUNT-BREAK.                                              
10734                                                                   
10735      IF RANGE-CNT NOT = +0                                        
10736          PERFORM 5000-DATE-RANGE-BREAK                            
10737              THRU 5099-DATE-RANGE-BREAK-X                         
10738      ELSE                                                         
10739          MOVE +1 TO SUB6                                          
10740          MOVE +2 TO SUB7                                          
10741          PERFORM 5800-ADD-PAGES-LEVELS                            
10742              THRU 5899-ADD-PAGES-LEVELS-X                         
10743          PERFORM 5900-CLEAR-TOTAL-PAGES                           
10744              THRU 5999-CLEAR-TOTAL-PAGES-X                        
10745                  VARYING SUB6 FROM 1 BY 1 UNTIL SUB6 GREATER 1.   
10746                                                                   
10747      MOVE +0 TO RANGE-CNT.                                        
10748      PERFORM 7500-ADD-UP-REIN-CO THRU 7599-ADD-UP-REIN-X          
10749          VARYING B-SUB FROM 1 BY 1                                
10750              UNTIL B-SUB GREATER THAN 50  OR                      
10751                    RB-COMPANY (B-SUB) = SPACES.                   
10752      PERFORM 7900-CLEAR-BILLING-TABLE THRU 7999-CLEAR-BILLING-X   
10753          VARYING B-SUB FROM 1 BY 1                                
10754              UNTIL B-SUB GREATER THAN 50.                         
10755                                                                   
10756      IF ACCT-FLG = SPACE                                          
10757          GO TO 5199-ACCOUNT-BREAK-X.                              
10758                                                                   
10759      MOVE SPACE TO ACCT-FLG.                                      
10760      MOVE +2 TO SUB6.                                             
10761      MOVE +3 TO SUB7.                                             
10762      PERFORM 5800-ADD-PAGES-LEVELS THRU 5899-ADD-PAGES-LEVELS-X.  
10763      IF LN-CNT GREATER +50                                        
10764          PERFORM 8700-PRINT-REPORT-HEADINGS THRU                  
10765                  8725-PRT-ACCT-HEADING.                           
10766                                                                   
10767      MOVE SPACES                     TO PRT.                      
10768      PERFORM 8800-PRINT-ROUTINE.                                  
10769      MOVE 'ACCOUNT'                  TO TPH2-DESC.                
10770      MOVE ' TOTALS'                  TO TPH3-DESC.                
10771      PERFORM 6100-ACCT-TOTAL-HEADINGS THRU 6199-PAGE-HEADINGS-X.  
10772      PERFORM 6500-PRINT-TOTAL-PAGE THRU 6599-TOTAL-PAGE-PRINT-X.  
10773      PERFORM 5900-CLEAR-TOTAL-PAGES                               
10774          THRU 5999-CLEAR-TOTAL-PAGES-X                            
10775              VARYING SUB6 FROM 2 BY 1 UNTIL SUB6 GREATER 2.       
10776                                                                   
10777  5199-ACCOUNT-BREAK-X.                                            
10778      EXIT.                                                        
10779                                                                   
10780  5200-STATE-BREAK.                                                
10781                                                                   
10782      PERFORM 5100-ACCOUNT-BREAK THRU 5199-ACCOUNT-BREAK-X.        
10783      MOVE ZEROS TO SV-LO-CERT  SV-HI-CERT.                        
10784      MOVE +3 TO SUB6.                                             
10785      MOVE +4 TO SUB7.                                             
10786      PERFORM 5800-ADD-PAGES-LEVELS THRU 5899-ADD-PAGES-LEVELS-X.  
10787                                                                   
10788      IF STATE-FLG = SPACE                                         
10789          GO TO 5299-STATE-BREAK-X.                                
10790                                                                   
10791      MOVE SPACE         TO STATE-FLG.                             
10792      PERFORM 8700-PRINT-REPORT-HEADINGS.                          
10793      MOVE 'STATE'       TO TPH1-TYPE.                             
10794      MOVE HLD-STATE     TO STATE-L.                               
10795                                                                   
10796      PERFORM 8200-STATE-CODE-LOOKUP THRU 8299-STATE-LOOKUP-X.     
10797                                                                   
10798      IF HLD-STATE NOT = STATE-SUB (CLAS-INDEXS)                   
10799          MOVE SPACES          TO TPH1-DESC                        
10800          MOVE HLD-STATE       TO TPH1-ST-CODE                     
10801          MOVE 'UNKNOWN STATE' TO TPH1-ST-DESC                     
10802      ELSE                                                         
10803          MOVE SPACES TO TPH1-DESC                                 
10804          MOVE STATE-SUB (CLAS-INDEXS) TO TPH1-ST-CODE             
10805          MOVE STATE-PIC (CLAS-INDEXS) TO TPH1-ST-DESC.            
10806                                                                   
10807      MOVE 'STATE'  TO TPH2-DESC.                                  
10808      MOVE 'TOTALS' TO TPH3-DESC.                                  
10809      PERFORM 6000-TOTAL-PAGE-HEADINGS THRU 6199-PAGE-HEADINGS-X.  
10810      PERFORM 6500-PRINT-TOTAL-PAGE THRU 6599-TOTAL-PAGE-PRINT-X.  
10811      PERFORM 5900-CLEAR-TOTAL-PAGES                               
10812          THRU 5999-CLEAR-TOTAL-PAGES-X                            
10813              VARYING SUB6 FROM 3 BY 1 UNTIL SUB6 GREATER 3.       
10814                                                                   
10815  5299-STATE-BREAK-X.                                              
10816      EXIT.                                                        
10817                                                                   
10818  5300-COMP-BREAK.                                                 
10819                                                                   
10820      PERFORM 5200-STATE-BREAK THRU 5299-STATE-BREAK-X.            
10821      MOVE +4 TO SUB6.                                             
10822      MOVE +5 TO SUB7.                                             
10823      PERFORM 5800-ADD-PAGES-LEVELS THRU 5899-ADD-PAGES-LEVELS-X.  
10824                                                                   
10825      IF COMP-FLG = SPACE                                          
10826          GO TO 5399-COMP-BREAK-X.                                 
10827                                                                   
10828      MOVE ' ' TO COMP-FLG.                                        
10829      PERFORM 8700-PRINT-REPORT-HEADINGS.                          
10830      MOVE 'COMPANY'                   TO TPH1-TYPE.               
10831      MOVE HLD-GROUPING                TO TPH1-DESC.               
10832      MOVE 'COMPANY'                   TO TPH2-DESC.               
10833      MOVE ' TOTALS'                   TO TPH3-DESC.               
10834      PERFORM 6000-TOTAL-PAGE-HEADINGS THRU 6199-PAGE-HEADINGS-X.  
10835      PERFORM 6500-PRINT-TOTAL-PAGE THRU 6599-TOTAL-PAGE-PRINT-X.  
10836      PERFORM 5900-CLEAR-TOTAL-PAGES                               
10837          THRU 5999-CLEAR-TOTAL-PAGES-X                            
10838              VARYING SUB6 FROM 4 BY 1 UNTIL SUB6 GREATER 4.       
10839                                                                   
10840  5399-COMP-BREAK-X.                                               
10841      EXIT.                                                        
10842                                                                   
10843  5400-CARR-BREAK.                                                 
10844                                                                   
10845      PERFORM 5300-COMP-BREAK THRU 5399-COMP-BREAK-X.              
10846      MOVE +5 TO SUB6.                                             
10847      MOVE +6 TO SUB7.                                             
10848      PERFORM 5800-ADD-PAGES-LEVELS THRU 5899-ADD-PAGES-LEVELS-X.  
10849                                                                   
10850      IF CARR-FLG = SPACE                                          
10851          GO TO 5499-CARR-BREAK-X.                                 
10852                                                                   
10853      MOVE ' ' TO CARR-FLG.                                        
10854      PERFORM 8700-PRINT-REPORT-HEADINGS.                          
10855      MOVE 'CARRIER'               TO TPH1-TYPE.                   
10856      MOVE HLD-CARRIER             TO TPH1-DESC.                   
10857      MOVE 'CARRIER'               TO TPH2-DESC.                   
10858      MOVE ' TOTALS'               TO TPH3-DESC.                   
10859      PERFORM 6000-TOTAL-PAGE-HEADINGS THRU 6199-PAGE-HEADINGS-X.  
10860      PERFORM 6500-PRINT-TOTAL-PAGE THRU 6599-TOTAL-PAGE-PRINT-X.  
10861      PERFORM 5900-CLEAR-TOTAL-PAGES                               
10862          THRU 5999-CLEAR-TOTAL-PAGES-X                            
10863              VARYING SUB6 FROM 5 BY 1 UNTIL SUB6 GREATER 5.       
10864                                                                   
10865  5499-CARR-BREAK-X.                                               
10866      EXIT.                                                        
10867                                                                   
10868  5500-FINAL-BREAK.                                                
10869                                                                   
10870      PERFORM 5400-CARR-BREAK THRU 5499-CARR-BREAK-X.              
10871                                                                   
10872      IF FINAL-FLG = SPACE                                         
10873          GO TO 5599-FINAL-BREAK-X.                                
10874                                                                   
10875      IF DTE-FICH = '1'                                            
10876          MOVE '2'  TO  DTE-FICH.                                  
10877      MOVE +6 TO SUB6.                                             
10878      PERFORM 8700-PRINT-REPORT-HEADINGS.                          
10879      MOVE SPACES   TO TPH1-TYPE.                                  
10880      MOVE SPACES   TO TPH1-DESC.                                  
10881      MOVE 'FINAL'  TO TPH2-DESC.                                  
10882      MOVE 'TOTALS' TO TPH3-DESC.                                  
10883      PERFORM 6000-TOTAL-PAGE-HEADINGS THRU 6199-PAGE-HEADINGS-X.  
10884      PERFORM 6500-PRINT-TOTAL-PAGE THRU 6599-TOTAL-PAGE-PRINT-X.  
10885                                                                   
10886  5599-FINAL-BREAK-X.                                              
10887      EXIT.                                                        
10888                                                                   
10889  5800-ADD-PAGES-LEVELS.                                           
10890                                                                   
10891      ADD TPL-ISS-PRM (SUB6)  TO TPL-ISS-PRM (SUB7).               
10892      ADD TPA-ISS-PRM (SUB6)  TO TPA-ISS-PRM (SUB7).               
10893      ADD TPL-CNC-PRM (SUB6)  TO TPL-CNC-PRM (SUB7).               
10894      ADD TPA-CNC-PRM (SUB6)  TO TPA-CNC-PRM (SUB7).               
10895      ADD TPL-ACCT-COM (SUB6) TO TPL-ACCT-COM (SUB7).              
10896      ADD TPA-ACCT-COM (SUB6) TO TPA-ACCT-COM (SUB7).              
10897      ADD TPL-CLAIMS (SUB6)   TO TPL-CLAIMS (SUB7).                
10898      ADD TPA-CLAIMS (SUB6)   TO TPA-CLAIMS (SUB7).                
10899                                                                   
10900  5899-ADD-PAGES-LEVELS-X.                                         
10901      EXIT.                                                        
10902                                                                   
10903  5900-CLEAR-TOTAL-PAGES.                                          
10904                                                                   
10905      MOVE +0 TO TPL-ISS-PRM (SUB6)  TPA-ISS-PRM (SUB6)            
10906              TPL-CNC-PRM (SUB6)     TPA-CNC-PRM (SUB6)            
10907              TPL-ACCT-COM (SUB6)    TPA-ACCT-COM (SUB6)           
10908              TPL-CLAIMS (SUB6)      TPA-CLAIMS (SUB6).            
10909                                                                   
10910  5999-CLEAR-TOTAL-PAGES-X.                                        
10911      EXIT.                                                        
10912  EJECT                                                            
10913 ******************************************************************
10914 ***       T O T A L   P A G E   P R I N T   R O U T I N E      ***
10915 ******************************************************************
10916                                                                   
10917  6000-TOTAL-PAGE-HEADINGS.                                        
10918                                                                   
10919      MOVE TOTAL-PAGE-HD1 TO PRT.                                  
10920      PERFORM 8800-PRINT-ROUTINE.                                  
10921                                                                   
10922  6100-ACCT-TOTAL-HEADINGS.                                        
10923                                                                   
10924      MOVE TOTAL-PAGE-HD2 TO PRT.                                  
10925      MOVE '0' TO P-CTL.                                           
10926      PERFORM 8800-PRINT-ROUTINE.                                  
10927      MOVE TOTAL-PAGE-HD3 TO PRT.                                  
10928      MOVE ' ' TO P-CTL.                                           
10929      PERFORM 8800-PRINT-ROUTINE.                                  
10930      PERFORM 8800-PRINT-ROUTINE.                                  
10931      MOVE +81 TO LN-CNT.                                          
10932                                                                   
10933  6199-PAGE-HEADINGS-X.                                            
10934      EXIT.                                                        
10935                                                                   
10936  6500-PRINT-TOTAL-PAGE.                                           
10937                                                                   
10938      MOVE SPACES             TO TOTAL-PAGE-DETAIL.                
10939      MOVE LIFE-OVERRIDE-L6   TO TPD-DESC.                         
10940      MOVE TPL-ISS-PRM (SUB6) TO TPD-ISS-PRM.                      
10941                                                                   
10942      IF SUB6 = 6 AND ME-DO-UPDATE                                 
10943          MOVE TPL-ISS-PRM (6) TO ME-010-PREM-L.                   
10944                                                                   
10945      MOVE TPL-CNC-PRM (SUB6) TO TPD-CNC-PRM.                      
10946                                                                   
10947      IF SUB6 = 6 AND ME-DO-UPDATE                                 
10948          MOVE TPL-CNC-PRM (6) TO ME-010-REF-L.                    
10949                                                                   
10950      COMPUTE TPD-NET-PRM =                                        
10951          TPL-ISS-PRM (SUB6) - TPL-CNC-PRM (SUB6).                 
10952                                                                   
10953      IF SUB6 = 6 AND ME-DO-UPDATE                                 
10954          SUBTRACT TPL-CNC-PRM (6) FROM TPL-ISS-PRM (6)            
10955          GIVING ME-010-NET-L.                                     
10956                                                                   
10957      MOVE TPL-ACCT-COM (SUB6) TO TPD-ACCT-COM.                    
10958                                                                   
10959      IF SUB6 = 6 AND ME-DO-UPDATE                                 
10960          MOVE TPL-ACCT-COM (6) TO ME-010-COMM-L.                  
10961                                                                   
10962      COMPUTE TPD-PREM-COMP =                                      
10963          TPL-ISS-PRM (SUB6) - TPL-CNC-PRM (SUB6)                  
10964                             - TPL-ACCT-COM (SUB6).                
10965      MOVE TPL-CLAIMS (SUB6)   TO TPD-CLAIMS.                      
10966                                                                   
10967      IF SUB6 = 6 AND ME-DO-UPDATE                                 
10968          MOVE TPL-CLAIMS (6) TO ME-010-PMT-L.                     

062104     IF SUB6 = 6
062104         MOVE TPL-CLAIMS (6)          TO WS-ME-BAL-AMT
062104         MOVE WS-BALANCE-DESCRIPTION1 TO WS-ME-BAL-DESCRIP
062104         WRITE ME-ECS010-BALANCE-REC  FROM WS-ME-BALANCE-REC
062104     END-IF.
10969                                                                   
10970      MOVE TOTAL-PAGE-DETAIL   TO PRT.                             
10971      PERFORM 8800-PRINT-ROUTINE.                                  
10972      MOVE SPACES             TO TOTAL-PAGE-DETAIL.                
10973      MOVE AH-OVERRIDE-L6     TO TPD-DESC.                         
10974      MOVE TPA-ISS-PRM (SUB6) TO TPD-ISS-PRM.                      
10975      MOVE TPA-CNC-PRM (SUB6) TO TPD-CNC-PRM.                      
10976                                                                   
10977      IF SUB6 = 6 AND ME-DO-UPDATE                                 
10978          MOVE TPA-ISS-PRM (6) TO ME-010-PREM-AH                   
10979          MOVE TPA-CNC-PRM (6) TO ME-010-REF-AH.                   
10980                                                                   
10981      COMPUTE TPD-NET-PRM =                                        
10982          TPA-ISS-PRM (SUB6) - TPA-CNC-PRM (SUB6).                 
10983                                                                   
10984      IF SUB6 = 6 AND ME-DO-UPDATE                                 
10985          SUBTRACT TPA-CNC-PRM (6) FROM TPA-ISS-PRM (6)            
10986          GIVING ME-010-NET-AH.                                    
10987                                                                   
10988      MOVE TPA-ACCT-COM (SUB6) TO TPD-ACCT-COM.                    
10989                                                                   
10990      IF SUB6 = 6 AND ME-DO-UPDATE                                 
10991          MOVE TPA-ACCT-COM (6) TO ME-010-COMM-AH.                 
10992                                                                   
10993      COMPUTE TPD-PREM-COMP =                                      
10994          TPA-ISS-PRM (SUB6) - TPA-CNC-PRM (SUB6)                  
10995                             - TPA-ACCT-COM (SUB6).                
10996      MOVE TPA-CLAIMS (SUB6)   TO TPD-CLAIMS.                      
10997                                                                   
10998      IF SUB6 = 6 AND ME-DO-UPDATE                                 
10999          MOVE TPA-CLAIMS (6) TO ME-010-PMT-AH.                    

062104     IF SUB6 = 6 
062104         MOVE TPA-CLAIMS (6)          TO WS-ME-BAL-AMT
062104         MOVE WS-BALANCE-DESCRIPTION2 TO WS-ME-BAL-DESCRIP
062104         WRITE ME-ECS010-BALANCE-REC  FROM WS-ME-BALANCE-REC
062104     END-IF.
11000                                                                   
11001      MOVE TOTAL-PAGE-DETAIL   TO PRT.                             
11002      PERFORM 8800-PRINT-ROUTINE.                                  
11003                                                                   
11004      MOVE 'TOTAL' TO TPD-DESC.                                    
11005                                                                   
11006      COMPUTE TPD-ISS-PRM =                                        
11007          TPL-ISS-PRM (SUB6) + TPA-ISS-PRM (SUB6).                 
11008                                                                   
11009      COMPUTE TPD-CNC-PRM =                                        
11010          TPL-CNC-PRM (SUB6) + TPA-CNC-PRM (SUB6).                 
11011                                                                   
11012      COMPUTE TPD-NET-PRM =                                        
11013          TPL-ISS-PRM (SUB6) + TPA-ISS-PRM (SUB6)                  
11014          - TPL-CNC-PRM (SUB6) - TPA-CNC-PRM (SUB6).               
11015                                                                   
11016      COMPUTE TPD-ACCT-COM =                                       
11017          TPL-ACCT-COM (SUB6) + TPA-ACCT-COM (SUB6).               
11018                                                                   
11019      COMPUTE TPD-PREM-COMP =                                      
11020          TPL-ISS-PRM (SUB6) + TPA-ISS-PRM (SUB6)                  
11021          - TPL-CNC-PRM (SUB6) - TPA-CNC-PRM (SUB6)                
11022          - TPL-ACCT-COM (SUB6) - TPA-ACCT-COM (SUB6).             
11023                                                                   
11024      COMPUTE TPD-CLAIMS =                                         
11025          TPL-CLAIMS (SUB6) + TPA-CLAIMS (SUB6).                   
11026                                                                   
11027      IF SV-LO-CERT = ZEROS  AND  SV-HI-CERT = ZEROS               
11028          NEXT SENTENCE                                            
11029      ELSE                                                         
11030          MOVE SV-LO-CERT (6:6) TO TPD-LO-CERT                     
11031          MOVE SV-HI-CERT (6:6) TO TPD-HI-CERT                     
11032          MOVE ZEROS TO SV-LO-CERT  SV-HI-CERT.                    
11033 *        MOVE SV-LO-CERT (4:8)    TO TPD-LO-CERT                  
11034 *        MOVE SV-HI-CERT (4:8)    TO TPD-HI-CERT                  
11035 *        MOVE ZEROS               TO SV-LO-CERT                   
11036 *                                    SV-HI-CERT.                  
11037                                                                   
11038      MOVE TOTAL-PAGE-DETAIL       TO PRT.                         
11039      MOVE '0'                     TO P-CTL.                       
11040      PERFORM 8800-PRINT-ROUTINE.                                  
11041      PERFORM 8800-PRINT-ROUTINE.                                  
11042                                                                   
11043  6599-TOTAL-PAGE-PRINT-X.                                         
11044      EXIT.                                                        
11045 *                                                                 
11046 *                                                                 
11047 *                                                                 
11048 **START***********************************************************
11049 *      CUSTOM CODING FOR CLIENT "DMD"            PROJECT# 6475    
11050 *            PARAGRAPHS  XXXX THRU 6700-COMM-EXIT                 
11051 ******************************************************************
11052 *                                                                 
11053  6650-GET-RES-STATE-COMM.                                         
11054 *6650-BROWSE-FORWARD.                                             
11055                                                                   
11056      IF DTE-CLIENT NOT = 'DMD'                                    
11057          GO TO 6700-COMM-EXIT.                                    
11058                                                                   
11059      MOVE CR-COMPANY-CD       TO WS-ERRESC-COMPANY-CD.            
11060      MOVE CR-CARRIER          TO WS-ERRESC-CARRIER.               
11061      MOVE CR-GROUPING         TO WS-ERRESC-GROUP.                 
11062      MOVE CR-STATE            TO WS-ERRESC-STATE.                 
11063      MOVE CR-ACCOUNT          TO WS-ERRESC-ACCOUNT.               
11064      MOVE WK-AGT              TO WS-ERRESC-AGENT.                 
11065      MOVE CR-CERT-NO (1:2)    TO WS-ERRESC-RES-STATE.             
11066      MOVE CR-DT               TO WS-YYYYMMDD-DT.                  
11067                                                                   
11068      MOVE WS-YYYYMMDD-DT      TO WS-ERRESC-EXPIRE-DT.             
11069                                                                   
11070 *POSITION FILE AT STARTING KEY RECORD(COMPANY, CARRIER, GROUP, STA
11071 *ACCOUNT,AGENT, RESIDENT STATE) AND RESC-EXPIRE-DATE SET BY THE   
11072 *VALUE OF PB-CERT-EFF-DT                                          
11073 *                                                                 
11074      MOVE WS-ERRESC-KEY        TO ERRESC-RECORD-KEY.              
11075                                                                   
070102     START ERRESC-IN KEY NOT LESS THAN ERRESC-RECORD-KEY.
11077                                                                   
11078      IF ERRESC-FILE-STATUS NOT EQUAL '00'                         
11079          GO TO 6700-COMM-EXIT.                                    
11080                                                                   
11081 *GET RECORD WHERE POSITIONED                                      
11082                                                                   
11083  6650-READ-ERRESC-NEXT.                                           
070102     READ ERRESC-IN NEXT RECORD.
11085                                                                   
11086      IF ERRESC-FILE-STATUS NOT EQUAL '00'                         
11087          GO TO 6700-COMM-EXIT.                                    
11088                                                                   
11089 *GOT GOOD RECORD ??    FIRST 32 POSITIONS MUST MATCH  !!!!!!      
11090                                                                   
11091      IF ERRESC-RECORD-KEY (1:32) EQUAL TO WS-ERRESC-KEY (1:32)    
11092         NEXT SENTENCE                                             
11093      ELSE                                                         
11094          GO TO 6700-COMM-EXIT.                                    
11095                                                                   
11096      IF WS-YYYYMMDD-DT < RESC-EXPIRE-DATE                         
11097          NEXT SENTENCE                                            
11098      ELSE                                                         
11099          GO TO 6650-READ-ERRESC-NEXT.                             
11100                                                                   
11101 *  CR-DT  = WS-YYYYMMDD-DT                                        
11102 *CHECK TO SEE IF CERTIFICATE EFFECTIVE DATE IS WITHIN ACCOUNT     
11103 *RESIDENT STATE COMMISSION RECORD'S EFFECTIVE TIME PERIOD???      
11104                                                                   
11105      IF WS-YYYYMMDD-DT IS LESS THAN RESC-EFFECTIVE-DATE           
11106          GO TO 6700-COMM-EXIT.                                    
11107                                                                   
11108 * SEARCH FOR MATCHING CATEGORY CODE ENTRY                         
11109                                                                   
11110      MOVE ZEROS               TO WS-SUB WS-SUB1.                  
11111                                                                   
11112      PERFORM 6680-SEARCH-CATEGORY THRU 6680-EXIT.                 
11113                                                                   
11114      IF WS-SUB1 IS GREATER THAN ZERO                              
11115          IF RESC-COMMISSION-PER (WS-SUB) NOT NUMERIC              
11116              PERFORM 6680-SET-TABLE-CODE                          
11117          ELSE                                                     
11118              PERFORM 6680-SET-COMMISSION.                         
11119                                                                   
11120      GO TO 6700-COMM-EXIT.                                        
11121                                                                   
11122  6680-SET-TABLE-CODE.                                             
11123      IF RESC-COMMISSION-TAB (WS-SUB) EQUAL SPACES                 
11124         MOVE ZEROS             TO WS-SUB1                         
11125      ELSE                                                         
11126 * DEFAULT TO AH-COVERAGE TO READ COMMISSION TABLE(CTBL)           
11127 * TO COMPUTE AH COMMISSION.                                       
11128         MOVE CR-AHTYP          TO CTBL-BEN-CODE                   
11129         MOVE CR-AH-TERM        TO WS-COMM-TERM                    
11130         MOVE AH-OVERRIDE-L1    TO CTBL-BEN-TYPE                   
11131         MOVE RESC-COMMISSION-TAB (WS-SUB)                         
11132                                TO CTBL-TABLE-CD                   
11133         PERFORM 6680-READ-CTBL-TABLE                              
11134         IF ERCTBL-FILE-STATUS NOT EQUAL '00'                      
11135            MOVE ZEROS          TO WS-SUB1.                        
11136                                                                   
11137  6680-READ-CTBL-TABLE.                                            
11138 *                                                                 
11139 * WHEN CR-AHTYP = ZEROS READ COMMISSION TABLE(CTBL)               
11140 * TO COMPUTE LIFE COMMISSION                                      
11141                                                                   
11142      IF CR-AHTYP = ZERO                                           
11143         MOVE LIFE-OVERRIDE-L1  TO CTBL-BEN-TYPE                   
11144         MOVE CR-LFTYP          TO CTBL-BEN-CODE                   
11145         MOVE CR-LF-TERM        TO WS-COMM-TERM.                   
11146                                                                   
11147      PERFORM 1300-READ-ERCTBL-FILE THRU 1399-READ-ERCTBL-X.       
11148                                                                   
11149      IF ERCTBL-FILE-STATUS EQUAL '00'                             
11150 * COMPUTE A-H COMMISSION WHEN CR-AHTYP IS NOT ZEROS               
11151         IF CR-AHTYP NOT = ZERO                                    
11152              COMPUTE COMM-CK-AMT = CR-AHAMT * CR-AH-TERM          
11153              MOVE CR-AH-TERM        TO WS-COMM-TERM               
11154              PERFORM 1400-GET-COMM-PERCENT THRU 1489-GET-PERCENT-X
11155              MOVE WK-RATE           TO WS-COMMISSION              
11156                                        PB-I-AH-COMMISSION         
11157         ELSE                                                      
11158 * COMPUTE LIFE COMMISSION WHEN CR-AHTYP IS ZEROS                  
11159              MOVE CR-LFAMT    TO COMM-CK-AMT                      
11160              MOVE CR-LF-TERM  TO WS-COMM-TERM                     
11161              PERFORM 1400-GET-COMM-PERCENT THRU 1489-GET-PERCENT-X
11162              MOVE WK-RATE     TO WS-COMMISSION                    
11163                                  PB-I-LIFE-COMMISSION.            
11164                                                                   
11165  6680-SET-COMMISSION.                                             
11166      IF RESC-COMMISSION-PER (WS-SUB) EQUAL ZEROS                  
11167            MOVE ZEROS         TO WS-SUB1                          
11168        ELSE                                                       
11169            MOVE RESC-COMMISSION-PER (WS-SUB)                      
11170                               TO WS-COMMISSION WK-RATE            
11171            IF CR-AHTYP NOT = ZERO                                 
11172                 MOVE WS-COMMISSION TO PB-I-AH-COMMISSION          
11173            ELSE                                                   
11174                 MOVE WS-COMMISSION TO PB-I-LIFE-COMMISSION.       
11175                                                                   
11176  6680-SEARCH-CATEGORY.                                            
11177      ADD +1 TO WS-SUB.                                            
11178                                                                   
11179      IF WS-SUB IS GREATER THAN +12                                
11180          GO TO 6680-EXIT.                                         
11181                                                                   
11182      IF CR-CERT-NO (4:1) = RESC-COVERAGE-CAT (WS-SUB)             
11183           MOVE WS-SUB         TO WS-SUB1                          
11184           GO TO 6680-EXIT.                                        
11185                                                                   
11186      GO TO 6680-SEARCH-CATEGORY.                                  
11187                                                                   
11188  6680-EXIT.                                                       
11189      EXIT.                                                        
11190                                                                   
11191  6700-COMM-EXIT.                                                  
11192      EXIT.                                                        
11193 *                                                                 
11194 *END***************CUSTOM CODE FOR CLIENT "DMD"*************      
11195                                                                   
11196  EJECT                                                            
11197 ******************************************************************
11198 ***         C L I E N T   B I L L I N G   R O U T I N E        ***
11199 ******************************************************************
11200                                                                   
11201  7000-CLIENT-BILLING-ROUTINE.                                     
11202 *                                                                 
11203 *                                                                 
11204 * LEAVE THE ABOVE 2 LINES IN TO PREVENT LOSING THE LINE ABOVE     
11238                                                                   
11239      MOVE +0 TO BC-NET-WRIT BC-NET-WRIT-OB BC-NET-WRIT-SP         
11240                 BC-G-ISS      BC-G-CNC   BC-G-CLM                 
11241                 BC-G-CRS      BC-G-CTN   BC-R-ISS      BC-R-CNC   
11242                 BC-R-CLM      BC-R-CRS   BC-R-CTN      BC-COMM    
11243                 BC-NEW-ACCTS  BC-ACCTS   BC-ACT-ACCTS             
11244                 BC-G-CERTS    BC-G-A-CERTS  BC-R-CERTS            
11245                 BC-R-A-CERTS  BC-ER-ACCTS   BC-CS-ACCTS.          
11246                                                                   
11247  7099-BILL-X.                                                     
11248      EXIT.                                                        
11249                                                                   
11250                                                                   
11251  7500-ADD-UP-REIN-CO.                                             
11252                                                                   
11253      IF RB-COMPANY (B-SUB) NOT = SPACES                           
11254          ADD RB-COUNT (B-SUB) TO BC-CS-ACCTS.                     
11255                                                                   
11256  7599-ADD-UP-REIN-X.                                              
11257      EXIT.                                                        
11258                                                                   
11259  7900-CLEAR-BILLING-TABLE.                                        
11260                                                                   
11261      MOVE SPACES TO RB-COMPANY (B-SUB).                           
11262      MOVE 0      TO RB-COUNT (B-SUB).                             
11263                                                                   
11264  7999-CLEAR-BILLING-X.                                            
11265      EXIT.                                                        
11266  EJECT                                                            
11267 ******************************************************************
11268 ***         M I S C E L L A N E O U S   R O U T I N E S        ***
11269 ******************************************************************
11270                                                                   
11271  8000-DATE-CONVERT-ROUTINE.                                       
11272                                                                   
11273      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   
11274                                                                   
11275  8099-DATE-CONVERT-X.                                             
11276      EXIT.                                                        
11277                                                                   
11278  8000R-CONVERT-STOP-DATE.                                         
11279                                                                   
11280      MOVE RWF-EP-STOP-DATE        TO DC-GREG-DATE-CYMD.           
11281      MOVE 'L'                     TO DC-OPTION-CODE.              
11282      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
11283      MOVE +0                      TO DC-ELAPSED-MONTHS.           
11284      MOVE -1                      TO DC-ELAPSED-DAYS.             
11285      MOVE '6'                     TO DC-OPTION-CODE.              
11286      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
11287      MOVE +0                      TO DC-ELAPSED-MONTHS            
11288                                      DC-ELAPSED-DAYS.             
11289      MOVE DC-BIN-DATE-2           TO DC-BIN-DATE-1.               
11290      MOVE ' '                     TO DC-OPTION-CODE.              
11291      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
11292                                                                   
11293      IF NO-CONVERSION-ERROR                                       
11294          MOVE DC-GREG-DATE-CYMD   TO EARNING-STOP-DATE            
11295      ELSE                                                         
11296          DISPLAY 'ERROR OCCURED CONVERTING EARNING STOP '         
11297                  'DATE IN REIN TABLE: ' CR-REIN-TABLE             
11298                  '  STOP DATE: ' RWF-EP-STOP-DATE                 
11299          MOVE 'ERROR OCCURED CONVERTING EARNING STOP DATE '       
11300                                   TO WS-ABEND-MESSAGE             
11301          MOVE DC-ERROR-CODE       TO WS-ABEND-FILE-STATUS         
11302          MOVE 4002                TO WS-ABEND-CODE                
11303          GO TO ABEND-PGM.                                         
11304                                                                   
11305  8099R-STOP-DATE-X.                                               
11306      EXIT.                                                        
11307                                                                   
11308                                                                   
11309  8100-GET-RATE.                                                   
11310                                                                   
11311      CALL 'ELRATEX' USING CALCULATION-PASS-AREA.                  
11312                                                                   
11313  8199-GET-RATE-X.                                                 
11314      EXIT.                                                        
11315                                                                   
11316                                                                   
11317  8200-STATE-CODE-LOOKUP.                                          
11318                                                                   
11319      IF CLAS-INDEXS NOT GREATER CLAS-MAXS  AND                    
11320         CLAS-INDEXS NOT = ZERO                                    
11321          IF STATE-L = STATE-SUB (CLAS-INDEXS)                     
11322              GO TO 8299-STATE-LOOKUP-X.                           
11323                                                                   
11324      MOVE CLAS-STARTS TO CLAS-INDEXS.                             
11325                                                                   
11326  8250-STATE-LOOKUP-LOOP.                                          
11327                                                                   
11328      IF CLAS-INDEXS GREATER CLAS-MAXS  OR  CLAS-INDEXS = ZERO     
11329          MOVE 0302 TO WS-ABEND-CODE                               
11330          DISPLAY 'STATE CODE ' STATE-L ' NOT IN STATE TABLE'      
10074          DISPLAY '******************************'                 
10075          DISPLAY '***** ERROR LOCATION 022 *****'                 
10076          DISPLAY '******************************'                 
11331          MOVE WS-ABEND-CODE        TO WS-RETURN-CODE              
11332          GO TO ABEND-PGM.                                         
11333                                                                   
11334      IF STATE-L NOT = STATE-SUB (CLAS-INDEXS)                     
11335          ADD +1 TO CLAS-INDEXS                                    
11336          GO TO 8250-STATE-LOOKUP-LOOP.                            
11337                                                                   
11338  8299-STATE-LOOKUP-X.                                             
11339      EXIT.                                                        
11340                                                                   
11341                                                                   
11342  8300-LIFE-TYPE-LOOKUP.                                           
11343                                                                   
11344      IF CLAS-INDEXL NOT GREATER CLAS-MAXL  AND                    
11345         CLAS-INDEXL NOT = ZERO                                    
11346          IF CLAS-LOOK = CLAS-I-BEN (CLAS-INDEXL)                  
11347              GO TO 8399-LIFE-LOOKUP-X.                            
11348                                                                   
11349      MOVE CLAS-STARTL TO CLAS-INDEXL.                             
11350                                                                   
11351  8350-LIFE-LOOKUP-LOOP.                                           
11352                                                                   
11353      IF CLAS-INDEXL GREATER CLAS-MAXL  OR  CLAS-INDEXL = ZERO     
11354          MOVE 0401 TO WS-ABEND-CODE                               
11355          DISPLAY 'LIFE TYPE ' CLAS-LOOK ' NOT IN TYPE TABLE'      
11356          DISPLAY '    PENDING CONTROL = ' PENDING-MATCH-CONTROL   
11357          DISPLAY '       CERT CONTROL = ' CR-FULL-CONTROL         
10074          DISPLAY '******************************'                 
10075          DISPLAY '***** ERROR LOCATION 023 *****'                 
10076          DISPLAY '******************************'                 
11358          MOVE WS-ABEND-CODE        TO WS-RETURN-CODE              
11359          IF DTE-CLIENT = 'NCL'                                    
CIDMOD                      OR 'CID'                                    
11360              MOVE CLAS-STARTL      TO CLAS-INDEXL                 
11361              MOVE '05'             TO CLAS-LOOK                   
11362              GO TO 8350-LIFE-LOOKUP-LOOP                          
11363          ELSE                                                     
11364              GO TO ABEND-PGM.                                     
11365                                                                   
11366      IF CLAS-LOOK NOT = CLAS-I-BEN (CLAS-INDEXL)                  
11367          ADD +1 TO CLAS-INDEXL                                    
11368          GO TO 8350-LIFE-LOOKUP-LOOP.                             
11369                                                                   
11370      IF CLAS-I-EP (CLAS-INDEXL) = 'U'                             
11371          MOVE 'R'                    TO CLAS-I-EP (CLAS-INDEXL).  
11372                                                                   
11373  8399-LIFE-LOOKUP-X.                                              
11374      EXIT.                                                        
11375                                                                   
11376                                                                   
11377  8400-A-H-TYPE-LOOKUP.                                            
11378                                                                   
11379      IF CLAS-INDEXA NOT GREATER CLAS-MAXA  AND                    
11380         CLAS-INDEXA NOT = ZERO                                    
11381          IF CLAS-LOOK = CLAS-I-BEN (CLAS-INDEXA)                  
11382              GO TO 8499-A-H-LOOKUP-X.                             
11383                                                                   
11384      MOVE CLAS-STARTA TO CLAS-INDEXA.                             
11385                                                                   
11386  8450-A-H-LOOKUP-LOOP.                                            
11387                                                                   
11388      IF CLAS-INDEXA GREATER CLAS-MAXA  OR  CLAS-INDEXA = ZERO     
11389          MOVE 0402 TO WS-ABEND-CODE                               
11390          DISPLAY 'A&H TYPE ' CLAS-LOOK ' NOT IN TYPE TABLE'       
11391          DISPLAY '    PENDING CONTROL = ' PENDING-MATCH-CONTROL   
11392          DISPLAY '       CERT CONTROL = ' CR-FULL-CONTROL         
               DISPLAY '******************************'
               DISPLAY '***** ERROR LOCATION 024 *****'
               DISPLAY '******************************'
11393          MOVE WS-ABEND-CODE        TO WS-RETURN-CODE              
11394          IF DTE-CLIENT = 'NCL'                                    
11395              MOVE CLAS-STARTA      TO CLAS-INDEXA                 
11396              MOVE '05'             TO CLAS-LOOK                   
11397              GO TO 8450-A-H-LOOKUP-LOOP                           
11398          ELSE                                                     
11399              GO TO ABEND-PGM.                                     
11400                                                                   
11401      IF CLAS-LOOK NOT = CLAS-I-BEN (CLAS-INDEXA)                  
11402          ADD +1 TO CLAS-INDEXA                                    
11403          GO TO 8450-A-H-LOOKUP-LOOP.                              
11404                                                                   
11405  8499-A-H-LOOKUP-X.                                               
11406      EXIT.                                                        
11407                                                                   
11408  8500-REMAINING-TERM-ROUTINE.                                     
11409                                                                   
11410      MOVE ZEROS                      TO CP-REMAINING-TERM         
11411                                         CP-REMAINING-TERM-1       
11412                                         CP-REMAINING-TERM-2       
11413                                         CP-REMAINING-TERM-3       
11414                                         CP-RETURN-CODE            
11415                                         CP-RETURN-CODE-2          
11416                                         REM-TRM1                  
11417                                         REM-TRM2                  
11418                                         REM-TRM3                  
11419                                         TEM-TRM1.                 
11420      MOVE SPACES                     TO CP-REM-TERM-METHOD.       
11421                                                                   
11422      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
11423      MOVE 'L'                        TO DC-OPTION-CODE.           
11424      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
11425                                                                   
11426      IF  NO-CONVERSION-ERROR                                      
11427          MOVE DC-BIN-DATE-1          TO CP-CERT-EFF-DT            
11428      ELSE                                                         
11429          MOVE 'ERROR OCCURED CONVERTING CR-DT'                    
11430                                      TO WS-ABEND-MESSAGE          
11431          MOVE DC-ERROR-CODE          TO WS-ABEND-FILE-STATUS      
11432          MOVE 3001                   TO WS-ABEND-CODE             
11433          GO TO ABEND-PGM.                                         
11434                                                                   
11435      MOVE LOW-VALUES                 TO CP-FIRST-PAY-DATE         
11436                                                                   
11437      IF  CR-LOAN-1ST-PMT-DT NOT NUMERIC                           
11438          MOVE ZEROS                  TO CR-LOAN-1ST-PMT-DT.       
11439                                                                   
11440      IF  CR-LOAN-1ST-PMT-DT NOT = ZEROS                           
11441          MOVE CR-LOAN-1ST-PMT-DT     TO DC-GREG-DATE-1-YMD        
11442          MOVE ZEROS                  TO DC-ELAPSED-MONTHS         
11443                                         DC-ELAPSED-DAYS           
11444          MOVE '3'                    TO DC-OPTION-CODE            
11445          PERFORM 8000-DATE-CONVERT-ROUTINE THRU                   
11446                  8099-DATE-CONVERT-X                              
11447          IF  NO-CONVERSION-ERROR                                  
11448              MOVE DC-BIN-DATE-1      TO CP-FIRST-PAY-DATE         
11449          ELSE                                                     
11450              MOVE ZEROS              TO CR-LOAN-1ST-PMT-DT.       
11451                                                                   
11452      IF  CP-FIRST-PAY-DATE LESS THAN CP-CERT-EFF-DT               
11453          MOVE ZEROS                  TO CR-LOAN-1ST-PMT-DT.       
11454                                                                   
11455      IF  CR-LOAN-1ST-PMT-DT = ZEROS                               
11456          MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1             
11457          MOVE +1                     TO DC-ELAPSED-MONTHS         
11458          MOVE ZEROS                  TO DC-ELAPSED-DAYS           
11459          MOVE '6'                    TO DC-OPTION-CODE            
11460          PERFORM 8000-DATE-CONVERT-ROUTINE THRU                   
11461                  8099-DATE-CONVERT-X                              
11462          IF  NO-CONVERSION-ERROR                                  
11463              MOVE DC-BIN-DATE-2      TO CP-FIRST-PAY-DATE         
11464              MOVE DC-GREG-DATE-1-YMD TO CR-LOAN-1ST-PMT-DT        
11465          ELSE                                                     
11466              MOVE 'ERROR OCCURED DETERMINING 1ST PAY DATE '       
11467                                      TO WS-ABEND-MESSAGE          
11468              MOVE DC-ERROR-CODE      TO WS-ABEND-FILE-STATUS      
11469              MOVE 3002               TO WS-ABEND-CODE             
11470              GO TO ABEND-PGM.                                     
11471                                                                   
11472      MOVE VALUATION-DT               TO DC-GREG-DATE-CYMD.        
11473      MOVE 'L'                        TO DC-OPTION-CODE.           
11474      PERFORM 8000-DATE-CONVERT-ROUTINE THRU 8099-DATE-CONVERT-X.  
11475                                                                   
11476      IF  NO-CONVERSION-ERROR                                      
11477          MOVE DC-BIN-DATE-1          TO CP-VALUATION-DT           
11478      ELSE                                                         
11479          DISPLAY 'ERROR CONVERTING VALUATION DATE, SOURCE - '     
11480              W-VAL-ERROR (W-VAL-SOURCE)                           
11481          DISPLAY '   KEY - ' CR-FULL-CONTROL                      
11482              ' VALUATION DT - ' VALUATION-DT ' ERROR - '          
11483              DC-ERROR-CODE                                        
11484          DISPLAY '   STATUS: ' CR-ENTRY-STATUS ' LF: ' CR-LFTYP   
11485          ' ' CR-LF-CURRENT-STATUS '-' CR-LF-STATUS-AT-CANCEL '-'  
11486              CR-LF-STATUS-AT-DEATH ' AH: ' CR-AHTYP ' '           
11487              CR-AH-CURRENT-STATUS '-' CR-AH-STATUS-AT-CANCEL '-'  
11488              CR-AH-STATUS-AT-SETTLEMENT.                          
11489 *        MOVE 'ERROR OCCURED CONVERTING VALUATION DATE'           
11490 *                                    TO WS-ABEND-MESSAGE          
11491 *        MOVE DC-ERROR-CODE          TO WS-ABEND-FILE-STATUS      
11492 *        MOVE 3003                   TO WS-ABEND-CODE             
11493 *        GO TO ABEND-PGM.                                         
11494                                                                   
11495      MOVE STATE-SUB (CLAS-INDEXS)    TO CP-STATE.                 
11496      MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV.       
11497      MOVE '3'                        TO CP-PROCESS-TYPE.          
11498      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
11499      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
11500      MOVE SPACES                     TO CP-ACCT-FLD-5.            
11501                                                                   
11502 *    MOVE CLAS-CO-REM-TERM-CALC (W-NDX)                           
11503 *                                    TO CP-REM-TERM-METHOD.       
11504                                                                   
11505      IF  DTE-CLIENT = 'FIM'                                       
11506          MOVE '5'                    TO CP-REM-TERM-METHOD.       
11507                                                                   
11508      IF DTE-CLIENT = 'FLI' OR 'FLU'                               
11509          IF WE-LF-AH = AH-OVERRIDE-L1                             
11510              MOVE '2'                TO CP-REM-TERM-METHOD        
11511          ELSE                                                     
11512              IF CR-LFTYP = '01' OR '03'                           
11513                  MOVE '4'            TO CP-REM-TERM-METHOD        
11514              ELSE                                                 
11515                  IF CR-LFTYP = '02' OR '04'                       
11516                      MOVE '2'        TO CP-REM-TERM-METHOD.       
11517                                                                   
11518      IF  DTE-CLIENT = 'CSL'                                       
11519          IF  CR-CARRIER = '2'                                     
11520              IF  CR-LFTYP = '07' OR  '08'                         
11521                  MOVE '4'            TO CP-REM-TERM-METHOD.       
11522                                                                   
11523      IF  DTE-CLIENT = 'POS'                                       
11524          IF  CR-CARRIER = '1'                                     
11525              MOVE '1'                TO CP-REM-TERM-METHOD        
11526          ELSE                                                     
11527              MOVE '2'                TO CP-REM-TERM-METHOD.       
11528                                                                   
11529      IF  CP-REM-TERM-METHOD = SPACES OR LOW-VALUES                
11530          MOVE DTE-REM-TRM            TO CP-REM-TERM-METHOD.       
11531                                                                   
11532      IF  WE-LF-AH EQUAL AH-OVERRIDE-L1                            
11533          GO TO 8540-CALC-REM-TERM-AH.                             
11534                                                                   
11535  8520-CALC-REM-TERM-LIFE.                                         
11536                                                                   
11537      MOVE CLAS-I-EP (CLAS-INDEXL)    TO CP-EARNING-METHOD.        
11538      MOVE CLAS-I-RL-AH (CLAS-INDEXL) TO CP-BENEFIT-TYPE.          
11539      MOVE CLAS-I-BAL (CLAS-INDEXL)   TO CP-SPECIAL-CALC-CD.       
11540 *    MOVE ORIG-TERM                  TO CP-ORIGINAL-TERM.         
11541      MOVE CR-LF-TERM                 TO CP-ORIGINAL-TERM          
11542                                         CP-LOAN-TERM.             
11543                                                                   
11544      IF  CLAS-I-EP (CLAS-INDEXL) EQUAL 'B'                        
11545              AND                                                  
11546          CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT EQUAL 'L'             
11547          ADD +1                      TO CP-ORIGINAL-TERM          
11548                                         CP-LOAN-TERM.             
11549                                                                   
11550      IF  CP-TERM-IS-DAYS                                          
11551          MOVE CR-LF-TERM-IN-DAYS     TO CP-TERM-OR-EXT-DAYS       
11552      ELSE                                                         
11553          MOVE ZEROS                  TO CP-TERM-OR-EXT-DAYS.      
11554                                                                   
11555      MOVE DTE-REM-TRM-CALC-OPTION    TO CP-REM-TRM-CALC-OPTION.   
11556                                                                   
11557      IF  CP-ORIGINAL-TERM NOT GREATER THAN ZEROS                  
11558          ADD +1                      TO W-ORGTRM-0-LF             
11559          DISPLAY 'LF TRM NOT GREATER THAN +0 - '                  
11560              CR-FULL-CONTROL                                      
11561          GO TO 8599-REM-TERM-X.                                   
11562                                                                   
11563      CALL 'ELRTRMX' USING CALCULATION-PASS-AREA.                  
11564                                                                   
11565      IF  CP-RETURN-CODE NOT EQUAL ZEROS                           
11566          ADD +1                      TO W-RET-CODE-LF             
11567          GO TO 8599-REM-TERM-X.                                   
11568 *                                                                 
11569 *        MOVE 'ERROR OCCURED REMAINING TERM CALCULATION - LF'     
11570 *                                    TO WS-ABEND-MESSAGE          
11571 *        MOVE CP-RETURN-CODE         TO WS-ABEND-FILE-STATUS      
11572 *        MOVE 2002                   TO WS-ABEND-CODE             
11573 *        GO TO ABEND-PGM.                                         
11574                                                                   
11575      IF  CLAS-I-EP (CLAS-INDEXL) EQUAL 'B'                        
11576              AND                                                  
11577          CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT EQUAL 'L'             
11578          MOVE CP-REMAINING-TERM-2    TO W-BAL-REMTRM              
11579          COMPUTE CP-REMAINING-TERM-1 = CP-REMAINING-TERM-1 - 1    
11580          COMPUTE CP-REMAINING-TERM-2 = CP-REMAINING-TERM-2 - 1.   
11581                                                                   
11582      IF  CP-REMAINING-TERM-1 NEGATIVE                             
11583          MOVE ZEROS                  TO CP-REMAINING-TERM-1.      
11584                                                                   
11585      IF  CP-REMAINING-TERM-2 NEGATIVE                             
11586          MOVE ZEROS                  TO CP-REMAINING-TERM-2.      
11587                                                                   
11588      IF DTE-CLIENT = 'NCL'                                        
11589          MOVE CP-REMAINING-TERM-1    TO CP-REMAINING-TERM-2       
11590                                         CP-REMAINING-TERM-3.      
11591                                                                   
11592      MOVE CP-REMAINING-TERM-1        TO REM-TRM1.                 
11593      MOVE CP-REMAINING-TERM-2        TO REM-TRM2.                 
11594      MOVE CP-REMAINING-TERM-3        TO REM-TRM3.                 
11595      MOVE CP-ODD-DAYS                TO TEM-TRM1.                 
11596                                                                   
11597      GO TO 8599-REM-TERM-X.                                       
11598                                                                   
11599  8520-EXIT.                                                       
11600      EXIT.                                                        
11601                                  EJECT                            
11602  8540-CALC-REM-TERM-AH.                                           
11603                                                                   
11604      MOVE CLAS-I-EP (CLAS-INDEXA)    TO CP-EARNING-METHOD.        
11605      MOVE CLAS-I-RL-AH (CLAS-INDEXA) TO CP-BENEFIT-TYPE.          
11606      MOVE CLAS-I-BAL (CLAS-INDEXA)   TO CP-SPECIAL-CALC-CD.       
11607      MOVE CR-AH-TERM                 TO CP-ORIGINAL-TERM          
11608                                         CP-LOAN-TERM.             
11609      MOVE ZEROS                      TO CP-TERM-OR-EXT-DAYS.      
11610                                                                   
11611      MOVE DTE-REM-TRM-CALC-OPTION    TO CP-REM-TRM-CALC-OPTION.   
11612                                                                   
11613      IF  CP-ORIGINAL-TERM NOT GREATER THAN ZEROS                  
11614          DISPLAY 'AH TRM NOT GREATER THAN +0 - '                  
11615              CR-FULL-CONTROL                                      
11616          ADD +1                      TO W-ORGTRM-0-AH             
11617          GO TO 8599-REM-TERM-X.                                   
11618                                                                   
11619      CALL 'ELRTRMX' USING CALCULATION-PASS-AREA.                  
11620                                                                   
11621      IF  CP-RETURN-CODE NOT EQUAL ZEROS                           
11622          ADD +1                      TO W-RET-CODE-AH             
11623          GO TO 8599-REM-TERM-X.                                   
11624 *                                                                 
11625 *        MOVE 'ERROR OCCURED REMAINING TERM CALCULATION - AH'     
11626 *                                    TO WS-ABEND-MESSAGE          
11627 *        MOVE CP-RETURN-CODE         TO WS-ABEND-FILE-STATUS      
11628 *        MOVE 2001                   TO WS-ABEND-CODE             
11629 *        GO TO ABEND-PGM.                                         
11630                                                                   
11631      IF DTE-CLIENT = 'NCL'                                        
11632          MOVE CP-REMAINING-TERM-1    TO CP-REMAINING-TERM-2       
11633                                         CP-REMAINING-TERM-3.      
11634                                                                   
11635      MOVE CP-REMAINING-TERM-1        TO REM-TRM1.                 
11636      MOVE CP-REMAINING-TERM-2        TO REM-TRM2.                 
11637      MOVE CP-REMAINING-TERM-3        TO REM-TRM3.                 
11638      MOVE CP-ODD-DAYS                TO TEM-TRM1.                 
11639                                                                   
11640  8599-REM-TERM-X.                                                 
11641      EXIT.                                                        
11642  EJECT                                                            
CIDMOD
CIDMOD******************************************************************
CIDMOD***     " D I S P L A Y "  P R I N T   R O U T I N E           ***
CIDMOD******************************************************************
CIDMOD                                                                  
CIDMOD 8600-DISPLAY-PRT.                                                
CIDMOD                                                                  
CIDMOD     IF  DIS-LINE-CNT GREATER THAN 59                             
CIDMOD         PERFORM 8600-DISPLAY-HD THRU                             
CIDMOD             8600-HD-EXIT.                                        
CIDMOD                                                                  
CIDMOD     ADD  1  TO DIS-LINE-CNT.                                     
CIDMOD     WRITE DISPLAY-REC FROM DISPLAY-LINE.                         
CIDMOD     MOVE  SPACES TO DISPLAY-REC                                  
CIDMOD                     DISPLAY-LINE.                                
CIDMOD                                                                  
CIDMOD 8600-DISPLAY-EXIT.                                               
CIDMOD     EXIT.                                                        
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD 8600-DISPLAY-HD.                                                 
CIDMOD                                                                  
CIDMOD     MOVE '1'  TO  DISPLAY-CC.                                    
CIDMOD     MOVE ZEROS TO DIS-LINE-CNT.                                  
CIDMOD     WRITE DISPLAY-REC FROM DISPLAY-HD-1.                         
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     MOVE ' '  TO  DISPLAY-CC.                                    
CIDMOD     MOVE SPACES TO DISPLAY-REC.                                  
CIDMOD     WRITE DISPLAY-REC.                                           
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     MOVE WS-CURRENT-DATE  TO  DIS-DATE.                          
CIDMOD     WRITE DISPLAY-REC FROM DISPLAY-HD-2.                         
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     MOVE SPACES TO DISPLAY-REC.                                  
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     WRITE DISPLAY-REC.                                           
CIDMOD                                                                  
CIDMOD 8600-HD-EXIT.                                                    
CIDMOD     EXIT.                                                        
CIDMOD                                                                  
11643 ******************************************************************
11644 ***      T R A N S A C T I O N   P R I N T   R O U T I N E     ***
11645 ******************************************************************
11646                                                                   
11647  8600-SETUP-DETAIL-LINE.                                          
11648                                                                   
11649      MOVE SPACES                TO TR-DTL2.                       
11650                                                                   
11651      MOVE CR-CERT-NO            TO TRD-CERT.                      
11652      MOVE CR-YR                 TO TRD-ISYR.                      
11653      MOVE CR-MO                 TO TRD-ISMO.                      
11654      MOVE CR-DA                 TO TRD-ISDA.                      
11655      MOVE CR-AGE                TO TRD-AGE.                       
11656                                                                   
11657  8689-SETUP-DETAIL-X.                                             
11658      EXIT.                                                        
11659                                                                   
11660  8690-PRINT-DETAIL-LINE.                                          
11661                                                                   
11662      IF LN-CNT GREATER THAN LN-MX                                 
11663          PERFORM 8700-PRINT-REPORT-HEADINGS THRU                  
11664                  8799-PRT-HEADINGS-X.                             
11665      MOVE TRD-CERT TO SV-TRD-CERT.                                
11666      MOVE TRD-ISDT TO SV-TRD-ISDT.                                
11667                                                                   
11668      IF TRD-CERT = LST-TRD-CERT  AND  TRD-ISDT = LST-TRD-ISDT     
11669          MOVE SPACES TO TRD-CERT  TRD-ISDT  TRD-AGE.              
11670                                                                   
11671      MOVE SV-TRD-CERT TO LST-TRD-CERT.                            
11672      MOVE SV-TRD-ISDT TO LST-TRD-ISDT.                            
11673      MOVE TR-DTL TO PRT.                                          
11674                                                                   
11675      IF NOT WS-CHANGE-REC                                         
11676          MOVE '/'                TO TRD-IS-S1  TRD-IS-S2          
11677      ELSE                                                         
11678          MOVE 'N'                TO  WS-CHANGE-REC-SW.            
11679                                                                   
11680      PERFORM 8800-PRINT-ROUTINE.                                  
11681                                                                   
11682  8699-PRT-DETAIL-X.                                               
11683      EXIT.                                                        
11684                                                                   
11685  8700-PRINT-REPORT-HEADINGS.                                      
11686      ADD +1 TO PG-CNT.                                            
11687      MOVE PG-CNT TO TR-PG.                                        
11688      MOVE +0 TO LN-CNT.                                           
11689      MOVE TR-HD-1 TO PRT.                                         
11690      PERFORM 8800-PRINT-ROUTINE.                                  
11691      MOVE TR-HD-2 TO PRT.                                         
11692      PERFORM 8800-PRINT-ROUTINE.                                  
11693      MOVE TR-HD-3 TO PRT.                                         
11694      PERFORM 8800-PRINT-ROUTINE.                                  
11695      ADD +1 TO LN-CNT.                                            
11696                                                                   
11697  8725-PRT-ACCT-HEADING.                                           
11698      MOVE TR-HD-4 TO PRT.                                         
11699      PERFORM 8800-PRINT-ROUTINE.                                  
11700      MOVE TR-HD-5 TO PRT.                                         
11701      PERFORM 8800-PRINT-ROUTINE.                                  
11702                                                                   
11703  8750-PRT-RANGE-HEADING.                                          
11704      MOVE TR-HD-6 TO PRT.                                         
11705      PERFORM 8800-PRINT-ROUTINE.                                  
11706                                                                   
11707  8775-PRT-COLUMN-HEADINGS.                                        
11708      MOVE TR-HD-7 TO PRT.                                         
11709      PERFORM 8800-PRINT-ROUTINE.                                  
11710      MOVE TR-HD-8 TO PRT.                                         
11711      PERFORM 8800-PRINT-ROUTINE.                                  
11712      MOVE TR-HD-9 TO PRT.                                         
11713      PERFORM 8800-PRINT-ROUTINE.                                  
11714      PERFORM 8800-PRINT-ROUTINE.                                  
11715                                                                   
11716  8799-PRT-HEADINGS-X.                                             
11717      EXIT.                                                        
11718                                                                   
11719  8800-PRINT-ROUTINE.                                              
11720      MOVE P-CTL TO X.                                             
11721      PERFORM 8900-PRINT-A-LINE THRU 8999-PRT-LINE-X.              
11722      ADD +1 TO LN-CNT.                                            
11723      MOVE SPACES TO PRT.                                          
11724                                                                   
11725  8900-PRINT-A-LINE.                                               
11726                              COPY ELCPRT2.                        
11727                                                                   
11728  8999-PRT-LINE-X.                                                 
11729      EXIT.                                                        
11730  EJECT                                                            
11731 ******************************************************************
11732 ***          E N D   O F   J O B   P R O C E S S I N G         ***
11733 ******************************************************************
11734                                                                   
11735  9910-EOJ-1.                                                      
11736      IF TR-CNT = ZERO                                             
11737          IF (DTE-PGM-OPT = '2' OR '3') OR                         
11738             (DTE-CLIENT = 'LGX' OR 'FNA' OR 'HER' OR 'NCL' OR     
11739                           'ITY' OR 'DAC' OR 'FLC')                
11740              NEXT SENTENCE                                        
11741          ELSE                                                     
11742              MOVE 'NO INPUT TRANSACTIONS' TO WS-ABEND-MESSAGE     
11743              MOVE 0703                    TO WS-ABEND-CODE        
CIDMOD             DISPLAY '******************************'             
CIDMOD             DISPLAY '***** ERROR LOCATION 025 *****'             
CIDMOD             DISPLAY '******************************'             
11744              MOVE WS-ABEND-CODE           TO WS-RETURN-CODE       
11745              GO TO ABEND-PGM.                                     
11746                                                                   
11747      MOVE '*'         TO TRANS-EOF-FLAG.                          
11748      MOVE HIGH-VALUES TO PENDING-MATCH-CONTROL.                   
11749                                                                   
11750      IF EOF-ON-CERT                                               
11751          PERFORM 0400-WRITE-CERT-RECORD THRU 0499-WRITE-CERT-X    
11752          GO TO 9940-EOJ-FIN.                                      
11753                                                                   
11754      GO TO 0330-MATCH-PENDING-TO-CERT.                            
11755                                                                   
11756  9920-EOJ-2.                                                      
11757      MOVE '*'         TO CERT-EOF-FLAG.                           
11758      MOVE HIGH-VALUES TO CIR-CONTROL.                             
11759                                                                   
11760      IF EOF-ON-TRANS                                              
11761          PERFORM 0400-WRITE-CERT-RECORD THRU 0499-WRITE-CERT-X    
11762          MOVE HIGH-VALUES TO CERTIFICATE-RECORD                   
11763          GO TO 9940-EOJ-FIN.                                      
11764                                                                   
11765      GO TO 0299-READ-CERT-X.                                      
11766                                                                   
11767  9930-EOJ-3.                                                      
11768      IF PROCESSING-COMPLETED                                      
11769          GO TO 9950-EOJ-FIN-GO.                                   
11770                                                                   
11771      DISPLAY 'MISSING ACCOUNT MASTER - '  CR-ACCT-CONTROL.        
11772      MOVE 'MISSING ACCOUNT MASTER'                                
11773                                TO WS-ABEND-MESSAGE.               
11774      MOVE 0302                 TO WS-ABEND-CODE.                  
CIDMOD     DISPLAY '******************************'                     
CIDMOD     DISPLAY '***** ERROR LOCATION 026 *****'                     
CIDMOD     DISPLAY '******************************'                     
11775      MOVE WS-ABEND-CODE        TO WS-RETURN-CODE.                 
11776      GO TO ABEND-PGM.                                             
11777                                                                   
11778  9940-EOJ-FIN.                                                    
11779      MOVE '*'        TO PROCESSING-FLAG.                          
11780      MOVE LOW-VALUES TO CERTIFICATE-RECORD.                       
11781                                                                   
11782      PERFORM 0100-ACCT-MASTER-READ-ROUTINE THRU                   
11783              0199-READ-ACCOUNT-X.                                 
11784      GO TO 9940-EOJ-FIN.                                          
11785                                                                   
11786  9950-EOJ-FIN-GO.                                                 
11787      PERFORM 5500-FINAL-BREAK THRU 5599-FINAL-BREAK-X.            
11788      MOVE HIGH-VALUES TO ACCOUNT-MASTER.                          
11789 *                                                                 
11790 *                                                                 
11791 * LEAVE THE ABOVE 2 LINE IN TO PREVENT LOSING THE LINE ABOVE      
11795      PERFORM 7000-CLIENT-BILLING-ROUTINE THRU 7099-BILL-X.        
11796                                                                   
11797  9960-NO-TOTS.                                                    
11798                                                                   
11799      MOVE +15                    TO LN-CNT.                       
11800      MOVE CMIN-CNT               TO TTL-PT-CNT.                   
11801      IF ME-DO-UPDATE                                              
11802          MOVE CMIN-CNT           TO ME-010-CERT-IN.               

062104     MOVE CMIN-CNT                TO WS-ME-BAL-AMT.
062104     MOVE WS-BALANCE-DESCRIPTION3 TO WS-ME-BAL-DESCRIP.
062104     WRITE ME-ECS010-BALANCE-REC  FROM WS-ME-BALANCE-REC.

11803                                                                   
11804      MOVE 'INPUT CERTIFICATE MASTER RECORDS' TO TTL-HD-DS.        
11805      MOVE TR-TTL-HD              TO PRT.                          
11806      PERFORM 8800-PRINT-ROUTINE.                                  
11807      ADD +1 TO LN-CNT.                                            
11808      MOVE TR-CNT                TO TTL-PT-CNT.                    
11809      MOVE 'TRANSACTION RECORDS' TO TTL-HD-DS.                     
11810      MOVE TR-TTL-HD             TO PRT.                           
11811      PERFORM 8800-PRINT-ROUTINE.                                  
11812      ADD +1 TO LN-CNT.                                            
11813                                                                   
11814      IF DUP-CNT = ZERO                                            
11815          GO TO 9969-CHECK-MISMATCHED-TOTS.                        
11816                                                                   
11817      MOVE DUP-CNT                 TO TTL-PT-CNT.                  
11818                                                                   
11819      IF ME-DO-UPDATE                                              
11820          MOVE DUP-CNT            TO ME-010-DUP-ISS.               
11821                                                                   
11822      MOVE 'DUPE ISSUES DROPPED  ' TO TTL-HD-DS.                   
11823      MOVE TR-TTL-HD               TO PRT.                         
11824      PERFORM 8800-PRINT-ROUTINE.                                  
11825      PERFORM 8800-PRINT-ROUTINE.                                  
11826      ADD +1 TO LN-CNT.                                            
11827                                                                   
11828      MOVE '1'                           TO  DTL-REC-TYPE.         
11829      MOVE 'ISS'                         TO  DTL-REC-TYPE-DESC.    
11830      MOVE SPACES                        TO  DTL-CERT-IN-CONTROL.  
11831      MOVE 'DUPLICATE CERTIFICATE RECORD WILL BE'                  
11832                                         TO  DTL-CERT-OUT-CONTROL. 
11833      MOVE SPACES                        TO  MISMATCH-DETAIL-2.    
11834      MOVE ' LIFE PREMIUM = '            TO  MD2-LF-PREM-DESC.     
11835      MOVE ' A&H  PREMIUM = '            TO  MD2-LF-ALT-PREM-DESC. 
11836                                                                   
11837      MOVE +1 TO SUB3.                                             
11838  9965-PRT-DUPES.                                                  
11839                                                                   
11840      IF  MISMATCH-LINE-COUNT GREATER THAN +56                     
11841          ADD +1                  TO  MISMATCH-PAGE-NUMBER         
11842          MOVE MISMATCH-PAGE-NUMBER                                
11843                                  TO  TR-PG                        
11844          MOVE MISMATCH-HEADING-1 TO  MISMATCH-RPT                 
11845          MOVE MR-CTL             TO  X                            
11846          MOVE X TO LCP-ASA                                        
11847          PERFORM LCP-WRITE-POS-MISMATCH-RPT                       
11848              THRU LCP-WRITE-END-MISMATCH-RPT                      
11849          MOVE TR-HD-2            TO  MISMATCH-RPT                 
11850          MOVE MR-CTL             TO  X                            
11851          MOVE X TO LCP-ASA                                        
11852          PERFORM LCP-WRITE-POS-MISMATCH-RPT                       
11853              THRU LCP-WRITE-END-MISMATCH-RPT                      
11854          MOVE TR-HD-3            TO  MISMATCH-RPT                 
11855          MOVE MR-CTL             TO  X                            
11856          MOVE X TO LCP-ASA                                        
11857          PERFORM LCP-WRITE-POS-MISMATCH-RPT                       
11858              THRU LCP-WRITE-END-MISMATCH-RPT                      
11859          MOVE MISMATCH-HEADING-2 TO  MISMATCH-RPT                 
11860          MOVE MR-CTL             TO  X                            
11861          MOVE X TO LCP-ASA                                        
11862          PERFORM LCP-WRITE-POS-MISMATCH-RPT                       
11863              THRU LCP-WRITE-END-MISMATCH-RPT                      
11864          MOVE +6                 TO  MISMATCH-LINE-COUNT.         
11865                                                                   
11866      MOVE DUPE-ENTRY (SUB3)             TO  DTL-MATCH-CONTROL.    
11867                                                                   
11868      MOVE MISMATCH-DETAIL               TO  MISMATCH-RPT.         
11869      MOVE MR-CTL                        TO  X.                    
11870      ADD +2                             TO  MISMATCH-LINE-COUNT.  
11871                                                                   
11872      MOVE X TO LCP-ASA                                            
11873      PERFORM LCP-WRITE-POS-MISMATCH-RPT                           
11874          THRU LCP-WRITE-END-MISMATCH-RPT.                         
11875                                                                   
11876      MOVE  DUPE-LF-PRM (SUB3)           TO  MD2-LF-PREM.          
11877      MOVE  DUPE-AH-PRM (SUB3)           TO  MD2-LF-ALT-PREM.      
11878                                                                   
11879      ADD  DUPE-LF-PRM (SUB3)            TO  W-MM-LF-PRM.          
11880      ADD  DUPE-AH-PRM (SUB3)            TO  W-MM-AH-PRM.          
11881                                                                   
11882      MOVE DUPE-STATUS (SUB3)            TO  MD2-AH-PREM-DESC.     
11883      IF  DUPE-STATUS (SUB3)  =  '2'                               
11884          MOVE  'POLICY IS PENDING '     TO  MD2-AH-PREM-DESC.     
11885      IF  DUPE-STATUS (SUB3)  =  '5'                               
11886          MOVE  'POLICY IS REISSUE '     TO  MD2-AH-PREM-DESC.     
122002     IF  DUPE-STATUS (SUB3)  =  'M'                               
11886          MOVE  'POLICY IS MONTHLY '     TO  MD2-AH-PREM-DESC.     
11887      IF  DUPE-STATUS (SUB3)  =  '9'                               
11888          MOVE  'POLICY REIN ONLY  '     TO  MD2-AH-PREM-DESC.     
11889      IF  DUPE-STATUS (SUB3)  =  'D'                               
11890          MOVE  'POLICY IS DECLINED'     TO  MD2-AH-PREM-DESC.     
11891      IF  DUPE-STATUS (SUB3)  =  'V'                               
11892          MOVE  'POLICY IS VOIDED  '     TO  MD2-AH-PREM-DESC.     
11893      IF  DUPE-STATUS (SUB3)  =  'P'                               
11894          MOVE  'POLICY PREM ACCTNG'     TO  MD2-AH-PREM-DESC.     
11895                                                                   
11896      MOVE MISMATCH-DETAIL-2             TO  MISMATCH-RPT.         
11897      MOVE MR-CTL                        TO  X.                    
11898      ADD +1                             TO  MISMATCH-LINE-COUNT.  
11899                                                                   
11900      MOVE X TO LCP-ASA                                            
11901      PERFORM LCP-WRITE-POS-MISMATCH-RPT                           
11902          THRU LCP-WRITE-END-MISMATCH-RPT.                         
11903                                                                   
11904      IF SUB3 LESS THAN DUP-CNT                                    
11905          ADD +1 TO SUB3                                           
11906          GO TO 9965-PRT-DUPES.                                    
11907                                                                   
11908  9969-CHECK-MISMATCHED-TOTS.                                      
11909                                                                   
11910      IF W-MM-LF-PRM = ZEROS  AND                                  
11911         W-MM-AH-PRM = ZEROS  AND                                  
11912         W-MM-LF-RFD = ZEROS  AND                                  
11913         W-MM-AH-RFD = ZEROS  AND                                  
11914         W-MM-LF-CLM = ZEROS  AND                                  
11915         W-MM-AH-CLM = ZEROS                                       
11916          GO TO 9970-FIN-TOTS.                                     
11917                                                                   
11918      MOVE SPACES                        TO  MISMATCH-RPT.         
11919      MOVE '0'                           TO  MR-CTL  X.            
11920      MOVE X TO LCP-ASA                                            
11921      PERFORM LCP-WRITE-POS-MISMATCH-RPT                           
11922          THRU LCP-WRITE-END-MISMATCH-RPT.                         
11923                                                                   
11924      MOVE 'TOT LF PREMS  = '            TO  MD2-LF-PREM-DESC.     
11925      MOVE 'TOT AH PREMS  = '            TO  MD2-LF-ALT-PREM-DESC. 
11926      MOVE ' TOTAL PREMIUMS = '          TO  MD2-AH-PREM-DESC.     
11927                                                                   
11928      MOVE W-MM-LF-PRM                   TO MD2-LF-PREM.           
11929      MOVE W-MM-AH-PRM                   TO MD2-LF-ALT-PREM.       
11930      COMPUTE MD2-AH-PREM = W-MM-LF-PRM + W-MM-AH-PRM.             
11931                                                                   
11932      MOVE MISMATCH-DETAIL-2             TO  MISMATCH-RPT.         
11933      MOVE '0'                           TO  MR-CTL  X.            
11934      MOVE X TO LCP-ASA                                            
11935      PERFORM LCP-WRITE-POS-MISMATCH-RPT                           
11936          THRU LCP-WRITE-END-MISMATCH-RPT.                         
11937                                                                   
11938      MOVE 'TOT LF RFNDS  = '            TO  MD2-LF-PREM-DESC.     
11939      MOVE 'TOT AH RFNDS  = '            TO  MD2-LF-ALT-PREM-DESC. 
11940      MOVE ' TOTAL REFUNDS  = '          TO  MD2-AH-PREM-DESC.     
11941                                                                   
11942      MOVE W-MM-LF-RFD                   TO MD2-LF-PREM.           
11943      MOVE W-MM-AH-RFD                   TO MD2-LF-ALT-PREM.       
11944      COMPUTE MD2-AH-PREM = W-MM-LF-RFD + W-MM-AH-RFD.             
11945                                                                   
11946      MOVE MISMATCH-DETAIL-2             TO  MISMATCH-RPT.         
11947      MOVE '0'                           TO  MR-CTL  X.            
11948      MOVE X TO LCP-ASA                                            
11949      PERFORM LCP-WRITE-POS-MISMATCH-RPT                           
11950          THRU LCP-WRITE-END-MISMATCH-RPT.                         
11951                                                                   
11952      MOVE 'TOT LF CLAIMS = '            TO  MD2-LF-PREM-DESC.     
11953      MOVE 'TOT AH CLAIMS = '            TO  MD2-LF-ALT-PREM-DESC. 
11954      MOVE ' TOTAL CLAIMS   = '          TO  MD2-AH-PREM-DESC.     
11955                                                                   
11956      MOVE W-MM-LF-CLM                   TO MD2-LF-PREM.           
11957      MOVE W-MM-AH-CLM                   TO MD2-LF-ALT-PREM.       
11958      COMPUTE MD2-AH-PREM = W-MM-LF-CLM + W-MM-AH-CLM.             
11959                                                                   
11960      MOVE MISMATCH-DETAIL-2             TO  MISMATCH-RPT.         
11961      MOVE '0'                           TO  MR-CTL  X.            
11962      MOVE X TO LCP-ASA                                            
11963      PERFORM LCP-WRITE-POS-MISMATCH-RPT                           
11964          THRU LCP-WRITE-END-MISMATCH-RPT.                         
11965                                                                   
11966  9970-FIN-TOTS.                                                   
11967                                                                   
11968      MOVE CMOT-CNT                TO TTL-PT-CNT.                  
11969                                                                   
11970      IF ME-DO-UPDATE                                              
11971          MOVE CMOT-CNT           TO ME-010-CERT-OUT.              
11972                                                                   
11973      MOVE 'OUTPUT CERTIFICATE MASTER RECORDS' TO TTL-HD-DS.       
11974      MOVE TR-TTL-HD               TO PRT.                         
11975      PERFORM 8800-PRINT-ROUTINE.                                  
11976                                                                   
062104     MOVE CMOT-CNT                TO WS-ME50-BAL-AMT-LOW.
062104     MOVE CMOT-CNT                TO WS-ME50-BAL-AMT-HIGH.
062104     MOVE WS-BAL50-DESCRIPTION    TO WS-ME50-BAL-DESCRIP.
062104     WRITE ME50-ECS010-BALANCE-REC FROM WS-ME50-BALANCE-REC.
11976                                                                   
11977      MOVE DE-CNT                  TO TTL-PT-CNT.                  
11978      MOVE 'DETAIL EXTRACT RECORDS' TO TTL-HD-DS.                  
11979      MOVE TR-TTL-HD               TO PRT.                         
11980      PERFORM 8800-PRINT-ROUTINE.                                  
11981                                                                   
11982      MOVE EP-CNT                  TO TTL-PT-CNT.                  
11983      MOVE 'SUMMARY EXTRACTS RECORDS' TO TTL-HD-DS.                
11984      MOVE TR-TTL-HD               TO PRT.                         
11985      PERFORM 8800-PRINT-ROUTINE.                                  
11986      PERFORM 8800-PRINT-ROUTINE.                                  
11987                                                                   
11988      MOVE ERACCTT-READS           TO TTL-PT-CNT.                  
11989      MOVE 'VSAM ACCOUNT MASTER FILE READS' TO TTL-HD-DS.          
11990      MOVE TR-TTL-HD               TO PRT.                         
11991      PERFORM 8800-PRINT-ROUTINE.                                  
11992                                                                   
11993      MOVE ERRTBL-READS            TO TTL-PT-CNT.                  
11994      MOVE 'VSAM REINSURANCE TABLE FILE READS' TO TTL-HD-DS.       
11995      MOVE TR-TTL-HD               TO PRT.                         
11996      PERFORM 8800-PRINT-ROUTINE.                                  
11997                                                                   
11998      MOVE ERCTBL-READS            TO TTL-PT-CNT.                  
11999      MOVE 'VSAM COMMISSION TABLE FILE READS' TO TTL-HD-DS.        
12000      MOVE TR-TTL-HD               TO PRT.                         
12001      PERFORM 8800-PRINT-ROUTINE.                                  
12002                                                                   
12003      IF  DTE-OPT-RESERVE-METHOD-UNAUTH                            
12004          GO TO 9990-FINAL-CLOSE.                                  
12005                                                                   
12006 **** MOST OF THE LINES WRITTEN BELOW ARE USED TO VERIFY           
12007 **** REPORT TOTALS, ETC, THAT CANNOT BE VERIFIED ANY OTHER        
12008 **** WAY.  MOST OF THESE FIELDS WILL BE DELETED WHEN ALL          
12009 **** PILOT COMPANIES HAVE BEEN SUCCESSFULLY PROCESSED.            
12010                                                                   
12011      MOVE W-POLICY-ACTIVE-CTR     TO TTL-PT-CNT.                  
12012      MOVE 'ACTIVE POLICIES'       TO TTL-HD-DS.                   
12013      MOVE TR-TTL-HD               TO PRT.                         
12014      PERFORM 8800-PRINT-ROUTINE.                                  
12015                                                                   
12016      MOVE W-POLICY-INACTIVE-CTR   TO TTL-PT-CNT.                  
12017      MOVE 'INACTIVE POLICIES'     TO TTL-HD-DS.                   
12018      MOVE TR-TTL-HD               TO PRT.                         
12019      PERFORM 8800-PRINT-ROUTINE.                                  
12020                                                                   
12021      MOVE W-POLICY-REISSUE-CTR    TO TTL-PT-CNT.                  
12022      MOVE 'REISSUE POLICIES'      TO TTL-HD-DS.                   
12023      MOVE TR-TTL-HD               TO PRT.                         
12024      PERFORM 8800-PRINT-ROUTINE.                                  
12025                                                                   
12026      MOVE W-POLICY-REIN-CTR       TO TTL-PT-CNT.                  
12027      MOVE 'REINSURANCE ONLY POLICIES' TO TTL-HD-DS.               
12028      MOVE TR-TTL-HD               TO PRT.                         
12029      PERFORM 8800-PRINT-ROUTINE.                                  
12030                                                                   
12031      MOVE W-POL-ISSUES            TO TTL-PT-CNT.                  
12032      MOVE 'ISSUE TRANSACTIONS'    TO TTL-HD-DS.                   
12033      MOVE TR-TTL-HD               TO PRT.                         
12034      PERFORM 8800-PRINT-ROUTINE.                                  
12035                                                                   
12036      MOVE W-AH-ISSUES             TO TTL-PT-CNT.                  
12037      MOVE 'AH ISSUE TRANSACTIONS' TO TTL-HD-DS.                   
12038      MOVE TR-TTL-HD               TO PRT.                         
12039      PERFORM 8800-PRINT-ROUTINE.                                  
12040                                                                   
12041      MOVE W-LF-ISSUES             TO TTL-PT-CNT.                  
12042      MOVE 'LF ISSUE TRANSACTIONS' TO TTL-HD-DS.                   
12043      MOVE TR-TTL-HD               TO PRT.                         
12044      PERFORM 8800-PRINT-ROUTINE.                                  
12045                                                                   
12046      MOVE W-PENDING-CLMS          TO TTL-PT-CNT.                  
12047      MOVE 'CLAIM TRANSACTIONS'    TO TTL-HD-DS.                   
12048      MOVE TR-TTL-HD               TO PRT.                         
12049      PERFORM 8800-PRINT-ROUTINE.                                  
12050                                                                   
12051      MOVE W-AH-CLAIMS             TO TTL-PT-CNT.                  
12052      MOVE 'AH CLAIMS TRANSACTIONS' TO TTL-HD-DS.                  
12053      MOVE TR-TTL-HD               TO PRT.                         
12054      PERFORM 8800-PRINT-ROUTINE.                                  
12055                                                                   
12056      MOVE W-LF-CLAIMS             TO TTL-PT-CNT.                  
12057      MOVE 'LF CLAIMS TRANSACTIONS' TO TTL-HD-DS.                  
12058      MOVE TR-TTL-HD               TO PRT.                         
12059      PERFORM 8800-PRINT-ROUTINE.                                  
12060                                                                   
12061      MOVE W-AH-RESERVES           TO TTL-PT-CNT.                  
12062      MOVE 'AH RESERVES TRANSACTIONS' TO TTL-HD-DS.                
12063      MOVE TR-TTL-HD               TO PRT.                         
12064      PERFORM 8800-PRINT-ROUTINE.                                  
12065                                                                   
12066      MOVE W-LF-RESERVES           TO TTL-PT-CNT.                  
12067      MOVE 'LF RESERVES TRANSACTIONS' TO TTL-HD-DS.                
12068      MOVE TR-TTL-HD               TO PRT.                         
12069      PERFORM 8800-PRINT-ROUTINE.                                  
12070                                                                   
12071      MOVE W-AH-ACTIVE             TO TTL-PT-CNT.                  
12072      MOVE 'AH RECORDS ACTIVE '    TO TTL-HD-DS.                   
12073      MOVE TR-TTL-HD               TO PRT.                         
12074      PERFORM 8800-PRINT-ROUTINE.                                  
12075                                                                   
12076      MOVE W-AH-INACTIVE           TO TTL-PT-CNT.                  
12077      MOVE 'AH RECORDS INACTIVE'   TO TTL-HD-DS.                   
12078      MOVE TR-TTL-HD               TO PRT.                         
12079      PERFORM 8800-PRINT-ROUTINE.                                  
12080                                                                   
12081      MOVE W-AH-REISSUE            TO TTL-PT-CNT.                  
12082      MOVE 'AH REISSUE REJECTS'    TO TTL-HD-DS.                   
12083      MOVE TR-TTL-HD               TO PRT.                         
12084      PERFORM 8800-PRINT-ROUTINE.                                  
12085                                                                   
12086      MOVE W-AH-REIN               TO TTL-PT-CNT.                  
12087      MOVE 'AH REIN ONLY      '    TO TTL-HD-DS.                   
12088      MOVE TR-TTL-HD               TO PRT.                         
12089      PERFORM 8800-PRINT-ROUTINE.                                  
12090                                                                   
12091      MOVE W-AH-CALC-B             TO TTL-PT-CNT.                  
12092      MOVE 'AH CALC B REJECTS'     TO TTL-HD-DS.                   
12093      MOVE TR-TTL-HD               TO PRT.                         
12094      PERFORM 8800-PRINT-ROUTINE.                                  
12095                                                                   
12096      MOVE W-AHTYP-99              TO TTL-PT-CNT.                  
12097      MOVE 'AH TYPE CHANGED TO 99' TO TTL-HD-DS.                   
12098      MOVE TR-TTL-HD               TO PRT.                         
12099      PERFORM 8800-PRINT-ROUTINE.                                  
12100                                                                   
12101      MOVE W-ORGTRM-0-AH           TO TTL-PT-CNT.                  
12102      MOVE 'AH ORIGINAL TERM = 0 ' TO TTL-HD-DS.                   
12103      MOVE TR-TTL-HD               TO PRT.                         
12104      PERFORM 8800-PRINT-ROUTINE.                                  
12105                                                                   
12106      MOVE W-RET-CODE-AH           TO TTL-PT-CNT.                  
12107      MOVE 'AH RET CODE TERM = 0 ' TO TTL-HD-DS.                   
12108      MOVE TR-TTL-HD               TO PRT.                         
12109      PERFORM 8800-PRINT-ROUTINE.                                  
12110                                                                   
12111      MOVE W-AH-REMTERM-0          TO TTL-PT-CNT.                  
12112      MOVE 'AH REMAINING TERM = 0' TO TTL-HD-DS.                   
12113      MOVE TR-TTL-HD               TO PRT.                         
12114      PERFORM 8800-PRINT-ROUTINE.                                  
12115                                                                   
12116      MOVE W-AH-DATA-USED          TO TTL-PT-CNT.                  
12117      MOVE 'AH RECORDS ACCEPTED DURING PROCESSING' TO TTL-HD-DS.   
12118      MOVE TR-TTL-HD               TO PRT.                         
12119      PERFORM 8800-PRINT-ROUTINE.                                  
12120                                                                   
12121      MOVE W-LF-ACTIVE             TO TTL-PT-CNT.                  
12122      MOVE 'LF RECORDS ACTIVE '    TO TTL-HD-DS.                   
12123      MOVE TR-TTL-HD               TO PRT.                         
12124      PERFORM 8800-PRINT-ROUTINE.                                  
12125                                                                   
12126      MOVE W-LF-INACTIVE           TO TTL-PT-CNT.                  
12127      MOVE 'LF RECORDS INACTIVE'   TO TTL-HD-DS.                   
12128      MOVE TR-TTL-HD               TO PRT.                         
12129      PERFORM 8800-PRINT-ROUTINE.                                  
12130                                                                   
12131      MOVE W-LF-BALLOON            TO TTL-PT-CNT.                  
12132      MOVE 'LF BALLOON REJECTS'    TO TTL-HD-DS.                   
12133      MOVE TR-TTL-HD               TO PRT.                         
12134      PERFORM 8800-PRINT-ROUTINE.                                  
12135                                                                   
12136      MOVE W-LF-REISSUE            TO TTL-PT-CNT.                  
12137      MOVE 'LF REISSUE REJECTS'    TO TTL-HD-DS.                   
12138      MOVE TR-TTL-HD               TO PRT.                         
12139      PERFORM 8800-PRINT-ROUTINE.                                  
12140                                                                   
12141      MOVE W-LF-REIN               TO TTL-PT-CNT.                  
12142      MOVE 'LF REIN ONLY      '    TO TTL-HD-DS.                   
12143      MOVE TR-TTL-HD               TO PRT.                         
12144      PERFORM 8800-PRINT-ROUTINE.                                  
12145                                                                   
12146      MOVE W-LF-CALC-B             TO TTL-PT-CNT.                  
12147      MOVE 'LF CALC B REJECTS'     TO TTL-HD-DS.                   
12148      MOVE TR-TTL-HD               TO PRT.                         
12149      PERFORM 8800-PRINT-ROUTINE.                                  
12150                                                                   
12151      MOVE W-ORGTRM-0-LF           TO TTL-PT-CNT.                  
12152      MOVE 'LF ORIGINAL TERM = 0 ' TO TTL-HD-DS.                   
12153      MOVE TR-TTL-HD               TO PRT.                         
12154      PERFORM 8800-PRINT-ROUTINE.                                  
12155                                                                   
12156      MOVE W-RET-CODE-LF           TO TTL-PT-CNT.                  
12157      MOVE 'LF RET CODE TERM = 0 ' TO TTL-HD-DS.                   
12158      MOVE TR-TTL-HD               TO PRT.                         
12159      PERFORM 8800-PRINT-ROUTINE.                                  
12160                                                                   
12161      MOVE W-LF-REMTERM-0          TO TTL-PT-CNT.                  
12162      MOVE 'LF REMTERM = 0 REJECTS' TO TTL-HD-DS.                  
12163      MOVE TR-TTL-HD               TO PRT.                         
12164      PERFORM 8800-PRINT-ROUTINE.                                  
12165                                                                   
12166      MOVE W-LF-REMAMT-0           TO TTL-PT-CNT.                  
12167      MOVE 'LF REMAMT = 0 REJECTS' TO TTL-HD-DS.                   
12168      MOVE TR-TTL-HD               TO PRT.                         
12169      PERFORM 8800-PRINT-ROUTINE.                                  
12170                                                                   
12171      MOVE W-LFTYP-99              TO TTL-PT-CNT.                  
12172      MOVE 'LF TYPE CHANGED TO 99' TO TTL-HD-DS.                   
12173      MOVE TR-TTL-HD               TO PRT.                         
12174      PERFORM 8800-PRINT-ROUTINE.                                  
12175                                                                   
12176      MOVE W-LF-DATA-USED          TO TTL-PT-CNT.                  
12177      MOVE 'LF RECORDS ACCEPTED DURING PROCESSING' TO TTL-HD-DS.   
12178      MOVE TR-TTL-HD               TO PRT.                         
12179      PERFORM 8800-PRINT-ROUTINE.                                  
12180                                                                   
12181      MOVE W-EPEC-EP-IN            TO TTL-PT-CNT.                  
12182      MOVE 'AH EARNED PREMIUM LAST 90 DAYS - DET ' TO TTL-HD-DS.   
12183      MOVE TR-TTL-HD               TO PRT.                         
12184      PERFORM 8800-PRINT-ROUTINE.                                  
12185                                                                   
12186      MOVE W-EPEC-EP               TO TTL-PT-CNT.                  
12187      MOVE 'AH EARNED PREMIUM LAST 90 DAYS       ' TO TTL-HD-DS.   
12188      MOVE TR-TTL-HD               TO PRT.                         
12189      PERFORM 8800-PRINT-ROUTINE.                                  
12190                                                                   
12191      MOVE W-EPEC-EP-2             TO TTL-PT-CNT.                  
12192      MOVE 'AH EARNED PREMIUM 90 DAYS AGO        ' TO TTL-HD-DS.   
12193      MOVE TR-TTL-HD               TO PRT.                         
12194      PERFORM 8800-PRINT-ROUTINE.                                  
12195                                                                   
12196      MOVE W-EP-LESS-THAN-0        TO TTL-PT-CNT.                  
12197      MOVE 'AH RECORDS WITH 90 DAY EP < +0       ' TO TTL-HD-DS.   
12198      MOVE TR-TTL-HD               TO PRT.                         
12199      PERFORM 8800-PRINT-ROUTINE.                                  
12200                                                                   
12201      MOVE W-EPEC-EP-1             TO TTL-PT-CNT.                  
12202      MOVE 'AH EARNED PREMIUM TODAY              ' TO TTL-HD-DS.   
12203      MOVE TR-TTL-HD               TO PRT.                         
12204      PERFORM 8800-PRINT-ROUTINE.                                  
12205                                                                   
12206      MOVE W-AH-CLAIM-CTR          TO TTL-PT-CNT.                  
12207      MOVE 'AH COVERAGES WITH QUALIFIED CLAIMS'                    
12208                                   TO TTL-HD-DS.                   
12209      MOVE TR-TTL-HD               TO PRT.                         
12210      PERFORM 8800-PRINT-ROUTINE.                                  
12211                                                                   
12212      MOVE W-AH-CLAIM-EP           TO TTL-PT-CNT.                  
12213      MOVE 'AH EARNED PREM WITH QUALIFIED CLAIMS'                  
12214                                   TO TTL-HD-DS.                   
12215      MOVE TR-TTL-HD               TO PRT.                         
12216      PERFORM 8800-PRINT-ROUTINE.                                  
12217                                                                   
12218      MOVE W-CLAIMS-MADE-PAID-IN   TO TTL-PT-CNT.                  
12219      MOVE 'AH CLAIMS MADE AND PAID LAST 90 DAYS- DET'             
12220                                   TO TTL-HD-DS.                   
12221      MOVE TR-TTL-HD               TO PRT.                         
12222      PERFORM 8800-PRINT-ROUTINE.                                  
12223                                                                   
12224      MOVE W-CLAIMS-MADE-PAID      TO TTL-PT-CNT.                  
12225      MOVE 'AH CLAIMS MADE AND PAID LAST 90 DAYS'                  
12226                                   TO TTL-HD-DS.                   
12227      MOVE TR-TTL-HD               TO PRT.                         
12228      PERFORM 8800-PRINT-ROUTINE.                                  
12229                                                                   
12230      MOVE W-CLM-EXCEPT-CNT        TO TTL-PT-CNT.                  
12231      MOVE 'AH CLAIMS CALCD. USING ITD AMOUNT'                     
12232                                   TO TTL-HD-DS.                   
12233      MOVE TR-TTL-HD               TO PRT.                         
12234      PERFORM 8800-PRINT-ROUTINE.                                  
12235                                                                   
12236      MOVE W-CLM-EXCEPT-AMT        TO TTL-PT-CNT.                  
12237      MOVE 'AH CLAIMS AMT USING ITD AMOUNT'                        
12238                                   TO TTL-HD-DS.                   
12239      MOVE TR-TTL-HD               TO PRT.                         
12240      PERFORM 8800-PRINT-ROUTINE.                                  
12241                                                                   
12242      MOVE W-AH-IBNR-1             TO TTL-PT-CNT.                  
12243      MOVE 'A&H IBNR-1'            TO TTL-HD-DS.                   
12244      MOVE TR-TTL-HD               TO PRT.                         
12245      PERFORM 8800-PRINT-ROUTINE.                                  
12246                                                                   
12247      MOVE W-AH-IBNR-2             TO TTL-PT-CNT.                  
12248      MOVE 'A&H IBNR-2'            TO TTL-HD-DS.                   
12249      MOVE TR-TTL-HD               TO PRT.                         
12250      PERFORM 8800-PRINT-ROUTINE.                                  
12251                                                                   
12252      MOVE W-LIFE-IBNR-IN          TO TTL-PT-CNT.                  
12253      MOVE 'LIFE IBNR - DETAIL'    TO TTL-HD-DS.                   
12254      MOVE TR-TTL-HD               TO PRT.                         
12255      PERFORM 8800-PRINT-ROUTINE.                                  
12256                                                                   
12257      MOVE W-LIFE-IBNR             TO TTL-PT-CNT.                  
12258      MOVE 'LIFE IBNR'             TO TTL-HD-DS.                   
12259      MOVE TR-TTL-HD               TO PRT.                         
12260      PERFORM 8800-PRINT-ROUTINE.                                  
12261                                                                   
12262      MOVE W-LIFE-REMAINING-AMT    TO TTL-PT-CNT.                  
12263      MOVE 'LIFE REMAINING-AMT '   TO TTL-HD-DS.                   
12264      MOVE TR-TTL-HD               TO PRT.                         
12265      PERFORM 8800-PRINT-ROUTINE.                                  
12266                                                                   
12267      MOVE W-01-REMAMT             TO TTL-PT-CNT.                  
12268      MOVE '01 REMAINING-AMT '     TO TTL-HD-DS.                   
12269      MOVE TR-TTL-HD               TO PRT.                         
12270      PERFORM 8800-PRINT-ROUTINE.                                  
12271                                                                   
12272      MOVE W-01-DATA-USED          TO TTL-PT-CNT.                  
12273      MOVE '01 CERT COUNT    '     TO TTL-HD-DS.                   
12274      MOVE TR-TTL-HD               TO PRT.                         
12275      PERFORM 8800-PRINT-ROUTINE.                                  
12276                                                                   
12277      MOVE W-02-REMAMT             TO TTL-PT-CNT.                  
12278      MOVE '02 REMAINING-AMT '     TO TTL-HD-DS.                   
12279      MOVE TR-TTL-HD               TO PRT.                         
12280      PERFORM 8800-PRINT-ROUTINE.                                  
12281                                                                   
12282      MOVE W-02-DATA-USED          TO TTL-PT-CNT.                  
12283      MOVE '02 CERT COUNT    '     TO TTL-HD-DS.                   
12284      MOVE TR-TTL-HD               TO PRT.                         
12285      PERFORM 8800-PRINT-ROUTINE.                                  
12286                                                                   
12287      MOVE W-03-REMAMT             TO TTL-PT-CNT.                  
12288      MOVE '03 REMAINING-AMT '     TO TTL-HD-DS.                   
12289      MOVE TR-TTL-HD               TO PRT.                         
12290      PERFORM 8800-PRINT-ROUTINE.                                  
12291                                                                   
12292      MOVE W-03-DATA-USED          TO TTL-PT-CNT.                  
12293      MOVE '03 CERT COUNT    '     TO TTL-HD-DS.                   
12294      MOVE TR-TTL-HD               TO PRT.                         
12295      PERFORM 8800-PRINT-ROUTINE.                                  
12296                                                                   
12297      MOVE W-04-REMAMT             TO TTL-PT-CNT.                  
12298      MOVE '04 REMAINING-AMT '     TO TTL-HD-DS.                   
12299      MOVE TR-TTL-HD               TO PRT.                         
12300      PERFORM 8800-PRINT-ROUTINE.                                  
12301                                                                   
12302      MOVE W-04-DATA-USED          TO TTL-PT-CNT.                  
12303      MOVE '04 CERT COUNT    '     TO TTL-HD-DS.                   
12304      MOVE TR-TTL-HD               TO PRT.                         
12305      PERFORM 8800-PRINT-ROUTINE.                                  
12306                                                                   
12307      MOVE W-05-REMAMT             TO TTL-PT-CNT.                  
12308      MOVE '05 REMAINING-AMT '     TO TTL-HD-DS.                   
12309      MOVE TR-TTL-HD               TO PRT.                         
12310      PERFORM 8800-PRINT-ROUTINE.                                  
12311                                                                   
12312      MOVE W-05-DATA-USED          TO TTL-PT-CNT.                  
12313      MOVE '05 CERT COUNT    '     TO TTL-HD-DS.                   
12314      MOVE TR-TTL-HD               TO PRT.                         
12315      PERFORM 8800-PRINT-ROUTINE.                                  
12316                                                                   
12317      MOVE W-06-REMAMT             TO TTL-PT-CNT.                  
12318      MOVE '06 REMAINING-AMT '     TO TTL-HD-DS.                   
12319      MOVE TR-TTL-HD               TO PRT.                         
12320      PERFORM 8800-PRINT-ROUTINE.                                  
12321                                                                   
12322      MOVE W-06-DATA-USED          TO TTL-PT-CNT.                  
12323      MOVE '06 CERT COUNT    '     TO TTL-HD-DS.                   
12324      MOVE TR-TTL-HD               TO PRT.                         
12325      PERFORM 8800-PRINT-ROUTINE.                                  
12326                                                                   
12327      MOVE W-30-REMAMT             TO TTL-PT-CNT.                  
12328      MOVE '30 REMAINING-AMT '     TO TTL-HD-DS.                   
12329      MOVE TR-TTL-HD               TO PRT.                         
12330      PERFORM 8800-PRINT-ROUTINE.                                  
12331                                                                   
12332      MOVE W-30-DATA-USED          TO TTL-PT-CNT.                  
12333      MOVE '30 CERT COUNT    '     TO TTL-HD-DS.                   
12334      MOVE TR-TTL-HD               TO PRT.                         
12335      PERFORM 8800-PRINT-ROUTINE.                                  
12336                                                                   
12337      MOVE W-31-REMAMT             TO TTL-PT-CNT.                  
12338      MOVE '31 REMAINING-AMT '     TO TTL-HD-DS.                   
12339      MOVE TR-TTL-HD               TO PRT.                         
12340      PERFORM 8800-PRINT-ROUTINE.                                  
12341                                                                   
12342      MOVE W-31-DATA-USED          TO TTL-PT-CNT.                  
12343      MOVE '31 CERT COUNT    '     TO TTL-HD-DS.                   
12344      MOVE TR-TTL-HD               TO PRT.                         
12345      PERFORM 8800-PRINT-ROUTINE.                                  
12346                                                                   
12347      MOVE W-XX-REMAMT             TO TTL-PT-CNT.                  
12348      MOVE 'XX REMAINING-AMT '     TO TTL-HD-DS.                   
12349      MOVE TR-TTL-HD               TO PRT.                         
12350      PERFORM 8800-PRINT-ROUTINE.                                  
12351                                                                   
12352      MOVE W-XX-DATA-USED          TO TTL-PT-CNT.                  
12353      MOVE 'XX CERT COUNT    '     TO TTL-HD-DS.                   
12354      MOVE TR-TTL-HD               TO PRT.                         
12355      PERFORM 8800-PRINT-ROUTINE.                                  
12356                                                                   
12357      MOVE W-1-EP                  TO TTL-PT-CNT.                  
12358      MOVE 'TYPE 1 EP        '     TO TTL-HD-DS.                   
12359      MOVE TR-TTL-HD               TO PRT.                         
12360      PERFORM 8800-PRINT-ROUTINE.                                  
12361                                                                   
12362      MOVE W-P-EP                  TO TTL-PT-CNT.                  
12363      MOVE 'TYPE P EP        '     TO TTL-HD-DS.                   
12364      MOVE TR-TTL-HD               TO PRT.                         
12365      PERFORM 8800-PRINT-ROUTINE.                                  
12366                                                                   
12367      MOVE W-M-EP                  TO TTL-PT-CNT.                  
12368      MOVE 'TYPE M EP        '     TO TTL-HD-DS.                   
12369      MOVE TR-TTL-HD               TO PRT.                         
12370      PERFORM 8800-PRINT-ROUTINE.                                  
12371                                                                   
12372      MOVE W-R-EP                  TO TTL-PT-CNT.                  
12373      MOVE 'TYPE R EP        '     TO TTL-HD-DS.                   
12374      MOVE TR-TTL-HD               TO PRT.                         
12375      PERFORM 8800-PRINT-ROUTINE.                                  
12376                                                                   
12377      MOVE W-WY-EP                 TO TTL-PT-CNT.                  
12378      MOVE 'EP FOR STATE WY  '     TO TTL-HD-DS.                   
12379      MOVE TR-TTL-HD               TO PRT.                         
12380      PERFORM 8800-PRINT-ROUTINE.                                  
12381                                                                   
12382      MOVE W-VA-EP                 TO TTL-PT-CNT.                  
12383      MOVE 'EP FOR STATE VA  '     TO TTL-HD-DS.                   
12384      MOVE TR-TTL-HD               TO PRT.                         
12385      PERFORM 8800-PRINT-ROUTINE.                                  
12386                                                                   
12387      MOVE W-OTHERS-COUNT          TO TTL-PT-CNT.                  
12388      MOVE 'AH COUNT OF OTHERS'    TO TTL-HD-DS.                   
12389      MOVE TR-TTL-HD               TO PRT.                         
12390      PERFORM 8800-PRINT-ROUTINE.                                  
12391                                                                   
12392      MOVE W-RTRM-0-CLMAMT         TO TTL-PT-CNT.                  
12393      MOVE 'CLM AMT OF THOSE CERTS WITH RTRM=0'    TO TTL-HD-DS.   
12394      MOVE TR-TTL-HD               TO PRT.                         
12395      PERFORM 8800-PRINT-ROUTINE.                                  
12396                                                                   
12397      MOVE W-RTRM-EP-1             TO TTL-PT-CNT.                  
12398      MOVE 'EP1 OF THOSE CERTS WITH RTRM=0'    TO TTL-HD-DS.       
12399      MOVE TR-TTL-HD               TO PRT.                         
12400      PERFORM 8800-PRINT-ROUTINE.                                  
12401                                                                   
12402      MOVE W-RTRM-EP-2             TO TTL-PT-CNT.                  
12403      MOVE 'EP2 OF THOSE CERTS WITH RTRM=0'    TO TTL-HD-DS.       
12404      MOVE TR-TTL-HD               TO PRT.                         
12405      PERFORM 8800-PRINT-ROUTINE.                                  
12406                                                                   
12407      MOVE W-RTRM-EP               TO TTL-PT-CNT.                  
12408      MOVE 'EP OF THOSE CERTS WITH RTRM=0'    TO TTL-HD-DS.        
12409      MOVE TR-TTL-HD               TO PRT.                         
12410      PERFORM 8800-PRINT-ROUTINE.                                  
12411                                                                   
12412      MOVE W-AH-IBNR-1             TO TTL-PT-CNT.                  
12413      MOVE 'IBNR1 FROM EQUATION 1' TO TTL-HD-DS.                   
12414      MOVE TR-TTL-HD               TO PRT.                         
12415      PERFORM 8800-PRINT-ROUTINE.                                  
12416                                                                   
12417      MOVE W-AH-IBNR-1-CNT         TO TTL-PT-CNT.                  
12418      MOVE 'REC CNT FROM EQUATION 1' TO TTL-HD-DS.                 
12419      MOVE TR-TTL-HD               TO PRT.                         
12420      PERFORM 8800-PRINT-ROUTINE.                                  
12421                                                                   
12422      MOVE W-AH-IBNR-2             TO TTL-PT-CNT.                  
12423      MOVE 'IBNR2 FROM EQUATION 2' TO TTL-HD-DS.                   
12424      MOVE TR-TTL-HD               TO PRT.                         
12425      PERFORM 8800-PRINT-ROUTINE.                                  
12426                                                                   
12427      MOVE W-AH-IBNR-2-CNT         TO TTL-PT-CNT.                  
12428      MOVE 'REC CNT FROM EQUATION 2' TO TTL-HD-DS.                 
12429      MOVE TR-TTL-HD               TO PRT.                         
12430      PERFORM 8800-PRINT-ROUTINE.                                  
12431                                                                   
12432  9990-FINAL-CLOSE.                                                
12433                              COPY ELCPRTC.                        
12434                                                                   
12435      IF DTE-CLIENT = 'DMD'                                        
070102         CLOSE ERRESC-IN.
12437                                                                   
CIDMOD     PERFORM 8600-DISPLAY-HD   THRU  8600-HD-EXIT.                
CIDMOD                                                                  
CIDMOD     DISPLAY ' '.                                                 
CIDMOD     DISPLAY '* * * * * * * * * * * * * * * * * *'.               
CIDMOD     DISPLAY ' DISPLAY ERROR COUNT = '  ERROR-COUNT.              
CIDMOD     DISPLAY '* * * * * * * * * * * * * * * * * *'.               
CIDMOD     DISPLAY ' '.                                                 
CIDMOD                                                                  
CIDMOD     MOVE    ' DISPLAY ERROR COUNT = '                            
CIDMOD       TO  DIS-LINE-REASON.                                       
CIDMOD     MOVE  ERROR-COUNT                                            
CIDMOD       TO  DIS-LINE-REC.                                          
CIDMOD         PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT.       
CIDMOD                                                                  
070102     CLOSE ERACCTT-IN  CERT-MASTER-IN  PRINT-COPY  MISMATCH-REPORT
CIDMOD           DISPLAY-PRT
062104           ME-ECS010-BALANCE
062104           ME50-ECS010-BALANCE
12439            PENDING-EXTRACT  SUMMARY-EXTR.                         
12440                                                                   
12441      IF DTE-PGM-OPT = '1'                                         
12442          CLOSE CERT-MASTER-OUT                                    
12443                DETAIL-EXTR.                                       
12444                                                                   
12445      IF ERACCTT-FILE-STATUS NOT = '00'                            
12446          MOVE '12'                TO ABEND-CODE-1                 
12447          MOVE ERACCTT-FILE-STATUS TO ABEND-CODE-2                 
CIDMOD         DISPLAY '******************************'                 
CIDMOD         DISPLAY '***** ERROR LOCATION 027 *****'                 
CIDMOD         DISPLAY '******************************'                 
12448          MOVE WS-ABEND-CODE        TO WS-RETURN-CODE              
12449            GO TO ABEND-PGM.                                       
12450                                                                   
12451      IF COMM-OPEN-SW = 'X'                                        
12452          CLOSE ERCTBL                                             
12453          IF ERCTBL-FILE-STATUS NOT = '00'                         
12454              MOVE '42'               TO ABEND-CODE-1              
12455              MOVE ERCTBL-FILE-STATUS TO ABEND-CODE-2              
CIDMOD             DISPLAY '******************************'             
CIDMOD             DISPLAY '***** ERROR LOCATION 028 *****'             
CIDMOD             DISPLAY '******************************'             
12456              MOVE WS-ABEND-CODE        TO WS-RETURN-CODE          
12457              GO TO ABEND-PGM.                                     
12458                                                                   
12459      IF REIN-OPEN-SW = 'X'                                        
070102         CLOSE ERRTBL-IN
12461          IF ERRTBL-FILE-STATUS NOT = '00'                         
12462              MOVE '22'               TO ABEND-CODE-1              
12463              MOVE ERRTBL-FILE-STATUS TO ABEND-CODE-2              
CIDMOD             DISPLAY '******************************'             
CIDMOD             DISPLAY '***** ERROR LOCATION 029 *****'             
CIDMOD             DISPLAY '******************************'             
12464              MOVE WS-ABEND-CODE        TO WS-RETURN-CODE          
12465              GO TO ABEND-PGM.                                     
12466                                                                   
12467      MOVE 'C'                         TO CP-IO-FUNCTION.          
12468      PERFORM 8100-GET-RATE THRU 8199-GET-RATE-X.                  
12469      IF IO-ERROR                                                  
12470          MOVE 'ERROR OCCURED CLOSING - ELRATE'                    
12471                                       TO WS-ABEND-MESSAGE         
12472          MOVE 0302                    TO WS-RETURN-CODE           
CIDMOD         DISPLAY '******************************'                 
CIDMOD         DISPLAY '***** ERROR LOCATION 030 *****'                 
CIDMOD         DISPLAY '******************************'                 
12473          MOVE WS-ABEND-CODE        TO WS-RETURN-CODE              
12474          GO TO ABEND-PGM.                                         
12475                                                                   
12476      IF ME-DO-UPDATE                                              
12477            MOVE ME-START-TIME        TO ME-010-START              
12478            MOVE ME-CNDS-DATE         TO ME-010-RUN-DT             
12479            ACCEPT WS-TIME-OF-DAY   FROM TIME                      
12480            MOVE WS-TIME              TO ME-010-END                
12481            ADD 1 TO ME-010-RUN-CT                                 
12482            REWRITE MONTH-END-BALANCES                             
12483            DISPLAY 'MONTH-END BALANCES POSTED'                    
070102           CLOSE ERMEBL-INOUT
12485      ELSE                                                         
12486          DISPLAY 'MONTH-END BALANCES NOT POSTED'.                 
12487                                                                   
12488      GO TO 9999-END-THE-JOB.                                      
12489                                                                   
12490  CERT-DATE-LOAD.                                                  
12491      COPY ELCCRTM1.                                               
12492                                                                   
12493  EXTRACT-DATE-LOAD.                                               
12494      COPY ELCEXTM1.                                               
12495                                                                   
12496  EXTRACT-DATE-RELOAD.                                             
12497      COPY ELCEXTM2.                                               
12498                                                                   
12499      EJECT                                                        
12500  ABEND-PGM.                                                       
CIDMOD     DISPLAY '******************************'                     
CIDMOD     DISPLAY '***** ABEND PGM **************'                     
CIDMOD     DISPLAY '******************************'                     
12501                              COPY ELCABEND.                       
12502      EJECT                                                        
12503  9999-END-THE-JOB.                                                
12504                                                                   
12505      DISPLAY ' '.                                                 
12506      DISPLAY '****** THE END OF MESSAGES CREATED BY '             
12507          'ECS010 ******'                                          
12508      DISPLAY ' '.                                                 
12509      GOBACK.                                                      
12510 /                                                                 
12511  LCP-WRITE-POS-MISMATCH-RPT SECTION.                              
12512      IF LCP-ASA = '+'                                             
12513          WRITE MISMATCH-RPT AFTER 0 LINE                          
12514      ELSE                                                         
12515      IF LCP-ASA = ' '                                             
12516          WRITE MISMATCH-RPT AFTER ADVANCING 1 LINE                
12517      ELSE                                                         
12518      IF LCP-ASA = '0'                                             
12519          WRITE MISMATCH-RPT AFTER ADVANCING 2 LINE                
12520      ELSE                                                         
12521      IF LCP-ASA = '-'                                             
12522          WRITE MISMATCH-RPT AFTER ADVANCING 3 LINE                
12523      ELSE                                                         
12524      IF LCP-ASA = '1'                                             
12525          WRITE MISMATCH-RPT AFTER ADVANCING PAGE                  
12526      ELSE                                                         
12527      IF LCP-ASA = '2'                                             
12528          WRITE MISMATCH-RPT AFTER ADVANCING LCP-CH2               
12529      ELSE                                                         
12530      IF LCP-ASA = '3'                                             
12531          WRITE MISMATCH-RPT AFTER ADVANCING LCP-CH3               
12532      ELSE                                                         
12533      IF LCP-ASA = '4'                                             
12534          WRITE MISMATCH-RPT AFTER ADVANCING LCP-CH4               
12535      ELSE                                                         
12536      IF LCP-ASA = '5'                                             
12537          WRITE MISMATCH-RPT AFTER ADVANCING LCP-CH5               
12538      ELSE                                                         
12539      IF LCP-ASA = '6'                                             
12540          WRITE MISMATCH-RPT AFTER ADVANCING LCP-CH6               
12541      ELSE                                                         
12542      IF LCP-ASA = '7'                                             
12543          WRITE MISMATCH-RPT AFTER ADVANCING LCP-CH7               
12544      ELSE                                                         
12545      IF LCP-ASA = '8'                                             
12546          WRITE MISMATCH-RPT AFTER ADVANCING LCP-CH8               
12547      ELSE                                                         
12548      IF LCP-ASA = '9'                                             
12549          WRITE MISMATCH-RPT AFTER ADVANCING LCP-CH9               
12550      ELSE                                                         
12551      IF LCP-ASA = 'A'                                             
12552          WRITE MISMATCH-RPT AFTER ADVANCING LCP-CH10              
12553      ELSE                                                         
12554      IF LCP-ASA = 'B'                                             
12555          WRITE MISMATCH-RPT AFTER ADVANCING LCP-CH11              
12556      ELSE                                                         
12557      IF LCP-ASA = 'C'                                             
12558          WRITE MISMATCH-RPT AFTER ADVANCING LCP-CH12              
12559      ELSE                                                         
12560      IF LCP-ASA = 'V'                                             
12561          WRITE MISMATCH-RPT AFTER ADVANCING LCP-P01               
12562      ELSE                                                         
12563      IF LCP-ASA = 'W'                                             
12564          WRITE MISMATCH-RPT AFTER ADVANCING LCP-P02               
12565      ELSE                                                         
12566      DISPLAY 'ASA CODE ERROR'.                                    
12567  LCP-WRITE-END-MISMATCH-RPT.                                      
12568      EXIT.                                                        
