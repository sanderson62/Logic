       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIB012MN.
       AUTHOR.     PABLO.
       DATE-COMPILED.

      *REMARKS.
      *        THIS PROGRAM READS THE FICHE FILE OUT OF EL562
      *        AND SEPARATES THE MN STATEMENTS FROM ALL THE OTHERS
      
100808******************************************************************
100808*                   C H A N G E   L O G
100808*
100808* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100808*-----------------------------------------------------------------
100808*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100808* EFFECTIVE    NUMBER
100808*-----------------------------------------------------------------
100808* 100808  CR2007100800003  PEMA  NEW PROGRAM
100808******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                                                                        
           SELECT ERCOMP    ASSIGN TO ERCOMP
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS CO-CONTROL-PRIMARY
                  FILE STATUS IS ERCOMP-FILE-STATUS.

           SELECT ERACCT    ASSIGN TO ERACCT3
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS AM-VG-KEY3
                  FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT BILLING-STATEMENTS ASSIGN TO SYS010.

           SELECT DISK-DATE          ASSIGN TO SYS019.

           SELECT OTHER-STATEMENTS   ASSIGN TO SYS011.

           SELECT REMIT-STMTS        ASSIGN TO SYS012.

           SELECT SCAN-EXT         ASSIGN TO SCANOT
                                   ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.                                                   

       FILE SECTION.                                                    
                                                                        
       FD  BILLING-STATEMENTS                                           
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  STMT-RECORD                 PIC X(133).
                                                                        
       FD  ERCOMP.
                                       COPY ERCCOMP.

       FD  ERACCT.

00026  01  ACCOUNT-MASTER.                                              ERCACCT
00027      12  AM-RECORD-ID                      PIC XX.                ERCACCT
00028          88  VALID-AM-ID                      VALUE 'AM'.         ERCACCT
00029                                                                   ERCACCT
00030      12  AM-CONTROL-PRIMARY.                                      ERCACCT
00031          16  AM-COMPANY-CD                 PIC X.                 ERCACCT
00032          16  AM-MSTR-CNTRL.                                       ERCACCT
00033              20  AM-CONTROL-A.                                    ERCACCT
00034                  24  AM-CARRIER            PIC X.                 ERCACCT
00035                  24  AM-GROUPING.                                 ERCACCT
00036                      28 AM-GROUPING-PREFIX PIC XXX.               ERCACCT
00037                      28 AM-GROUPING-PRIME  PIC XXX.               ERCACCT
00038                  24  AM-STATE              PIC XX.                ERCACCT
00039                  24  AM-ACCOUNT.                                  ERCACCT
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).              ERCACCT
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).              ERCACCT
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A            ERCACCT
00043                                            PIC X(19).             ERCACCT
00044              20  AM-CNTRL-B.                                      ERCACCT
00045                  24  AM-EXPIRATION-DT      PIC XX.                ERCACCT
00046                  24  FILLER                PIC X(4).              ERCACCT
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.                 ERCACCT
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.     ERCACCT
00049                                                                   ERCACCT
00050      12  AM-CONTROL-BY-VAR-GRP.                                   ERCACCT
00051          16  AM-COMPANY-CD-A1              PIC X.                 ERCACCT
00052          16  AM-VG-CARRIER                 PIC X.                 ERCACCT
00053          16  AM-VG-GROUPING                PIC X(6).              ERCACCT
00054          16  AM-VG-STATE                   PIC XX.                ERCACCT
               16  AM-VG-KEY2-END.
00055            18  AM-VG-ACCOUNT               PIC X(10).             ERCACCT
00056            18  AM-VG-DATE.                                        ERCACCT
00057              20  AM-VG-EXPIRATION-DT       PIC XX.                ERCACCT
00058              20  FILLER                    PIC X(4).              ERCACCT
00059            18  AM-VG-EXP-DATE REDEFINES AM-VG-DATE                ERCACCT
00060                                            PIC 9(11)      COMP-3. ERCACCT
               16  AM-VG-KEY3-R REDEFINES AM-VG-KEY2-END.
                   20  AM-VG-KEY3.
                       25  AM-VG-KEY3-ACCOUNT    PIC X(10).
                       25  AM-VG-KEY3-EXP-DT     PIC X(2).
                   20  FILLER                    PIC X(4).
                   
00061      12  AM-MAINT-INFORMATION.                                    ERCACCT
00062          16  AM-LAST-MAINT-DT              PIC XX.                ERCACCT
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3. ERCACCT
00064          16  AM-LAST-MAINT-USER            PIC X(4).              ERCACCT
00065          16  FILLER                        PIC XX.                ERCACCT
00066                                                                   ERCACCT
00067      12  AM-EFFECTIVE-DT                   PIC XX.                ERCACCT
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3. ERCACCT
00069                                                                   ERCACCT
00070      12  AM-PREV-DATES  COMP-3.                                   ERCACCT
00071          16  AM-PREV-EXP-DT                PIC 9(11).             ERCACCT
00072          16  AM-PREV-EFF-DT                PIC 9(11).             ERCACCT
00073                                                                   ERCACCT
00074      12  AM-REPORT-CODE-1                  PIC X(10).             ERCACCT
00075      12  AM-REPORT-CODE-2                  PIC X(10).             ERCACCT
00076                                                                   ERCACCT
00077      12  AM-CITY-CODE                      PIC X(4).              ERCACCT
00078      12  AM-COUNTY-PARISH                  PIC X(6).              ERCACCT
00079                                                                   ERCACCT
00080      12  AM-NAME                           PIC X(30).             ERCACCT
00081      12  AM-PERSON                         PIC X(30).             ERCACCT
00082      12  AM-ADDRS                          PIC X(30).             ERCACCT
00083      12  AM-CITY                           PIC X(30).             ERCACCT
00084      12  AM-ZIP.                                                  ERCACCT
00085          16  AM-ZIP-PRIME.                                        ERCACCT
00086              20  AM-ZIP-PRI-1ST            PIC X.                 ERCACCT
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'. ERCACCT
00088              20  FILLER                    PIC X(4).              ERCACCT
00089          16  AM-ZIP-PLUS4                  PIC X(4).              ERCACCT
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.              ERCACCT
00091          16  AM-CAN-POSTAL-1               PIC XXX.               ERCACCT
00092          16  AM-CAN-POSTAL-2               PIC XXX.               ERCACCT
00093          16  FILLER                        PIC XXX.               ERCACCT
00094      12  AM-TEL-NO.                                               ERCACCT
00095          16  AM-AREA-CODE                  PIC 999.               ERCACCT
00096          16  AM-TEL-PRE                    PIC 999.               ERCACCT
00097          16  AM-TEL-NBR                    PIC 9(4).              ERCACCT
00098      12  AM-TEL-LOC                        PIC X.                 ERCACCT
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.          ERCACCT
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.          ERCACCT
00101                                                                   ERCACCT
00102      12  AM-COMM-STRUCTURE.                                       ERCACCT
00103          16  AM-DEFN-1.                                           ERCACCT
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.              ERCACCT
00105                  24  AM-AGT.                                      ERCACCT
00106                      28  AM-AGT-PREFIX     PIC X(4).              ERCACCT
00107                      28  AM-AGT-PRIME      PIC X(6).              ERCACCT
00108                  24  AM-COM-TYP            PIC X.                 ERCACCT
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3. ERCACCT
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3. ERCACCT
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3. ERCACCT
00112                  24  AM-RECALC-LV-INDIC    PIC X.                 ERCACCT
00113                  24  AM-RETRO-LV-INDIC     PIC X.                 ERCACCT
00114                  24  AM-GL-CODES           PIC X.                 ERCACCT
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).             ERCACCT
00116                  24  FILLER                PIC X(01).             ERCACCT
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.                   ERCACCT
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.              ERCACCT
00119                  24  FILLER                PIC X(11).             ERCACCT
00120                  24  AM-L-COMA             PIC XXX.               ERCACCT
00121                  24  AM-J-COMA             PIC XXX.               ERCACCT
00122                  24  AM-A-COMA             PIC XXX.               ERCACCT
00123                  24  FILLER                PIC X(6).              ERCACCT
00124                                                                   ERCACCT
00125      12  AM-COMM-CHANGE-STATUS             PIC X.                 ERCACCT
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.          ERCACCT
00127                                                                   ERCACCT
00128      12  AM-CSR-CODE                       PIC X(4).              ERCACCT
00129                                                                   ERCACCT
00130      12  AM-BILLING-STATUS                 PIC X.                 ERCACCT
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.          ERCACCT
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.          ERCACCT
00133      12  AM-AUTO-REFUND-SW                 PIC X.                 ERCACCT
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.          ERCACCT
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.      ERCACCT
00136      12  AM-GPCD                           PIC 99.                ERCACCT
00137      12  AM-IG                             PIC X.                 ERCACCT
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.          ERCACCT
00139          88  AM-HAS-GROUP                     VALUE '2'.          ERCACCT
00140      12  AM-STATUS                         PIC X.                 ERCACCT
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.          ERCACCT
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.          ERCACCT
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.          ERCACCT
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
00144      12  AM-REMIT-TO                       PIC 99.                ERCACCT
00145      12  AM-ID-NO                          PIC X(11).             ERCACCT
00146                                                                   ERCACCT
00147      12  AM-CAL-TABLE                      PIC XX.                ERCACCT
00148      12  AM-LF-DEVIATION                   PIC XXX.               ERCACCT
00149      12  AM-AH-DEVIATION                   PIC XXX.               ERCACCT
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3. ERCACCT
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3. ERCACCT
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3. ERCACCT
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3. ERCACCT
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3. ERCACCT
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3. ERCACCT
00156                                                                   ERCACCT
00157      12  AM-USER-FIELDS.                                          ERCACCT
00158          16  AM-FLD-1                      PIC XX.                ERCACCT
00159          16  AM-FLD-2                      PIC XX.                ERCACCT
00160          16  AM-FLD-3                      PIC XX.                ERCACCT
00161          16  AM-FLD-4                      PIC XX.                ERCACCT
00162          16  AM-FLD-5                      PIC XX.                ERCACCT
00163                                                                   ERCACCT
00164      12  AM-1ST-PROD-DATE.                                        ERCACCT
00165          16  AM-1ST-PROD-YR                PIC XX.                ERCACCT
00166          16  AM-1ST-PROD-MO                PIC XX.                ERCACCT
00167          16  AM-1ST-PROD-DA                PIC XX.                ERCACCT
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.     ERCACCT
00169      12  AM-CERTS-PURGED-DATE.                                    ERCACCT
00170          16  AM-PUR-YR                     PIC XX.                ERCACCT
00171          16  AM-PUR-MO                     PIC XX.                ERCACCT
00172          16  AM-PUR-DA                     PIC XX.                ERCACCT
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.     ERCACCT
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.     ERCACCT
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.     ERCACCT
00176      12  AM-INACTIVE-DATE.                                        ERCACCT
00177          16  AM-INA-MO                     PIC 99.                ERCACCT
00178          16  AM-INA-DA                     PIC 99.                ERCACCT
00179          16  AM-INA-YR                     PIC 99.                ERCACCT
00180      12  AM-AR-HI-CERT-DATE                PIC XX.                ERCACCT
00181                                                                   ERCACCT
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3. ERCACCT
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3. ERCACCT
00184                                                                   ERCACCT
00185      12  AM-OB-PAYMENT-MODE                PIC X.                 ERCACCT
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.      ERCACCT
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.          ERCACCT
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.          ERCACCT
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.          ERCACCT
00190                                                                   ERCACCT
00191      12  AM-AH-ONLY-INDICATOR              PIC X.                 ERCACCT
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.      ERCACCT
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.          ERCACCT
00194                                                                   ERCACCT
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).             ERCACCT
00196                                                                   ERCACCT
00197      12  AM-OVER-SHORT.                                           ERCACCT
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3. ERCACCT
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3. ERCACCT
00200                                                                   ERCACCT
00201      12  FILLER                            PIC X(05).             ERCACCT
00202                                                                   ERCACCT
00203      12  AM-RECALC-COMM                    PIC X.                 ERCACCT
00204      12  AM-RECALC-REIN                    PIC X.                 ERCACCT
00205                                                                   ERCACCT
00206      12  AM-REI-TABLE                      PIC XXX.               ERCACCT
00207      12  AM-REI-ET-LF                      PIC X.                 ERCACCT
00208      12  AM-REI-ET-AH                      PIC X.                 ERCACCT
00209      12  AM-REI-PE-LF                      PIC X.                 ERCACCT
00210      12  AM-REI-PE-AH                      PIC X.                 ERCACCT
00211      12  AM-REI-PRT-ST                     PIC X.                 ERCACCT
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3. ERCACCT
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3. ERCACCT
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3. ERCACCT
00215      12  AM-REI-GROUP-A                    PIC X(6).              ERCACCT
00216      12  AM-REI-MORT                       PIC X(4).              ERCACCT
00217      12  AM-REI-PRT-OW                     PIC X.                 ERCACCT
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3. ERCACCT
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3. ERCACCT
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3. ERCACCT
00221      12  AM-REI-GROUP-B                    PIC X(6).              ERCACCT
00222                                                                   ERCACCT
00223      12  AM-TRUST-TYPE                     PIC X(2).              ERCACCT
00224                                                                   ERCACCT
00225      12  AM-EMPLOYER-STMT-USED             PIC X.                 ERCACCT
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.                 ERCACCT
00227                                                                   ERCACCT
00228      12  AM-STD-AH-TYPE                    PIC XX.                ERCACCT
00229      12  AM-EARN-METHODS.                                         ERCACCT
00230          16  AM-EARN-METHOD-R              PIC X.                 ERCACCT
00231              88 AM-REF-RL-R78                 VALUE 'R'.          ERCACCT
00232              88 AM-REF-RL-PR                  VALUE 'P'.          ERCACCT
00233              88 AM-REF-RL-MEAN                VALUE 'M'.          ERCACCT
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.          ERCACCT
00235          16  AM-EARN-METHOD-L              PIC X.                 ERCACCT
00236              88 AM-REF-LL-R78                 VALUE 'R'.          ERCACCT
00237              88 AM-REF-LL-PR                  VALUE 'P'.          ERCACCT
00238              88 AM-REF-LL-MEAN                VALUE 'M'.          ERCACCT
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.          ERCACCT
00240          16  AM-EARN-METHOD-A              PIC X.                 ERCACCT
00241              88 AM-REF-AH-R78                 VALUE 'R'.          ERCACCT
00242              88 AM-REF-AH-PR                  VALUE 'P'.          ERCACCT
00243              88 AM-REF-AH-MEAN                VALUE 'M'.          ERCACCT
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.          ERCACCT
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.          ERCACCT
00246              88 AM-REF-AH-NET                 VALUE 'N'.          ERCACCT
00247                                                                   ERCACCT
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3. ERCACCT
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3. ERCACCT
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3. ERCACCT
00251                                                                   ERCACCT
00252      12  AM-RET-Y-N                        PIC X.                 ERCACCT
00253      12  AM-RET-P-E                        PIC X.                 ERCACCT
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3. ERCACCT
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3. ERCACCT
00256      12  AM-RET-GRP                        PIC X(6).              ERCACCT
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.                    ERCACCT
00258          16  AM-POOL-PRIME                 PIC XXX.               ERCACCT
00259          16  AM-POOL-SUB                   PIC XXX.               ERCACCT
00260      12  AM-RETRO-EARNINGS.                                       ERCACCT
00261          16  AM-RET-EARN-R                 PIC X.                 ERCACCT
00262          16  AM-RET-EARN-L                 PIC X.                 ERCACCT
00263          16  AM-RET-EARN-A                 PIC X.                 ERCACCT
00264      12  AM-RET-ST-TAX-USE                 PIC X.                 ERCACCT
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.  ERCACCT
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.      ERCACCT
00267      12  AM-RETRO-BEG-EARNINGS.                                   ERCACCT
00268          16  AM-RET-BEG-EARN-R             PIC X.                 ERCACCT
00269          16  AM-RET-BEG-EARN-L             PIC X.                 ERCACCT
00270          16  AM-RET-BEG-EARN-A             PIC X.                 ERCACCT
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3. ERCACCT
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3. ERCACCT
00273                                                                   ERCACCT
00274      12  AM-USER-SELECT-OPTIONS.                                  ERCACCT
00275          16  AM-USER-SELECT-1              PIC X(10).             ERCACCT
00276          16  AM-USER-SELECT-2              PIC X(10).             ERCACCT
00277          16  AM-USER-SELECT-3              PIC X(10).             ERCACCT
00278          16  AM-USER-SELECT-4              PIC X(10).             ERCACCT
00279          16  AM-USER-SELECT-5              PIC X(10).             ERCACCT
00280                                                                   ERCACCT
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3. ERCACCT
00282                                                                   ERCACCT
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3. ERCACCT
00284                                                                   ERCACCT
00285      12  AM-RPT045A-SWITCH                 PIC X.                 ERCACCT
00286          88  RPT045A-OFF                   VALUE 'N'.             ERCACCT
00287                                                                   ERCACCT
00288      12  AM-INSURANCE-LIMITS.                                     ERCACCT
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3. ERCACCT
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3. ERCACCT
00291                                                                   ERCACCT
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.                 ERCACCT
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.          ERCACCT
00294                                                                   ERCACCT
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.                 ERCACCT
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.          ERCACCT
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.          ERCACCT
00298                                                                   ERCACCT
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3. ERCACCT
00300                                                                   ERCACCT
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3. ERCACCT
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
090803     12  FILLER                            PIC X(18).
090803*    12  FILLER                            PIC X(22).             ERCACCT
00303                                                                   ERCACCT
00304      12  AM-RESERVE-DATE.                                         ERCACCT
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.    ERCACCT
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.    ERCACCT
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.    ERCACCT
00308                                                                   ERCACCT
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.                ERCACCT
00310      12  AM-NOTIFICATION-TYPES.                                   ERCACCT
00311          16  AM-NOTIF-OF-LETTERS           PIC X.                 ERCACCT
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.                 ERCACCT
00313          16  AM-NOTIF-OF-REPORTS           PIC X.                 ERCACCT
00314          16  AM-NOTIF-OF-STATUS            PIC X.                 ERCACCT
00315                                                                   ERCACCT
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.                 ERCACCT
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.          ERCACCT
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.          ERCACCT
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.          ERCACCT
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.     ERCACCT
00321                                                                   ERCACCT
00322      12  AM-BENEFIT-CONTROLS.                                     ERCACCT
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.            ERCACCT
00324              20  AM-BENEFIT-CODE           PIC XX.                ERCACCT
00325              20  AM-BENEFIT-TYPE           PIC X.                 ERCACCT
00326              20  AM-BENEFIT-REVISION       PIC XXX.               ERCACCT
00327              20  AM-BENEFIT-REM-TERM       PIC X.                 ERCACCT
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.                 ERCACCT
00329              20  FILLER                    PIC XX.                ERCACCT
00330          16  FILLER                        PIC X(80).             ERCACCT
00331                                                                   ERCACCT
00332      12  AM-TRANSFER-DATA.                                        ERCACCT
00333          16  AM-TRANSFERRED-FROM.                                 ERCACCT
00334              20  AM-TRNFROM-CARRIER        PIC X.                 ERCACCT
00335              20  AM-TRNFROM-GROUPING.                             ERCACCT
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.               ERCACCT
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.               ERCACCT
00338              20  AM-TRNFROM-STATE          PIC XX.                ERCACCT
00339              20  AM-TRNFROM-ACCOUNT.                              ERCACCT
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).             ERCACCT
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).              ERCACCT
00342              20  AM-TRNFROM-DTE            PIC XX.                ERCACCT
00343          16  AM-TRANSFERRED-TO.                                   ERCACCT
00344              20  AM-TRNTO-CARRIER          PIC X.                 ERCACCT
00345              20  AM-TRNTO-GROUPING.                               ERCACCT
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.               ERCACCT
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.               ERCACCT
00348              20  AM-TRNTO-STATE            PIC XX.                ERCACCT
00349              20  AM-TRNTO-ACCOUNT.                                ERCACCT
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).              ERCACCT
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).              ERCACCT
00352              20  AM-TRNTO-DTE              PIC XX.                ERCACCT
00353          16  FILLER                        PIC X(10).             ERCACCT
00354                                                                   ERCACCT
00355      12  AM-SAVED-REMIT-TO                 PIC 99.                ERCACCT
00356                                                                   ERCACCT
00357      12  AM-COMM-STRUCTURE-SAVED.                                 ERCACCT
00358          16  AM-DEFN-1-SAVED.                                     ERCACCT
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.           ERCACCT
00360                  24  AM-AGT-SV             PIC X(10).             ERCACCT
00361                  24  AM-COM-TYP-SV         PIC X.                 ERCACCT
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3. ERCACCT
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3. ERCACCT
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3. ERCACCT
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.                 ERCACCT
00366                  24  FILLER                PIC X.                 ERCACCT
00367                  24  AM-GL-CODES-SV        PIC X.                 ERCACCT
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.                ERCACCT
00369                  24  FILLER                PIC X.                 ERCACCT
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.       ERCACCT
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.            ERCACCT
00372                  24  FILLER                PIC X(11).             ERCACCT
00373                  24  AM-L-COMA-SV          PIC XXX.               ERCACCT
00374                  24  AM-J-COMA-SV          PIC XXX.               ERCACCT
00375                  24  AM-A-COMA-SV          PIC XXX.               ERCACCT
00376                  24  FILLER                PIC X(6).              ERCACCT
00377                                                                   ERCACCT
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.                            ERCACCT
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.                 ERCACCT
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3. ERCACCT
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3. ERCACCT
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3. ERCACCT
00383                                                                   ERCACCT
00384      12  FILLER                            PIC X(130).            ERCACCT
00385                                                                   ERCACCT
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.                               ERCACCT
00387          16  AM-CONTROL-NAME               PIC X(30).             ERCACCT
00388          16  AM-EXECUTIVE-ONE.                                    ERCACCT
00389              20  AM-EXEC1-NAME             PIC X(15).             ERCACCT
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)       ERCACCT
00391                                                           COMP-3. ERCACCT
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)       ERCACCT
00393                                                           COMP-3. ERCACCT
00394          16  AM-EXECUTIVE-TWO.                                    ERCACCT
00395              20  AM-EXEC2-NAME             PIC X(15).             ERCACCT
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)       ERCACCT
00397                                                           COMP-3. ERCACCT
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)       ERCACCT
00399                                                           COMP-3. ERCACCT
00400                                                                   ERCACCT
00401      12  AM-RETRO-ADDITIONAL-DATA.                                ERCACCT
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3. ERCACCT
00403          16  AM-RETRO-PREM-P-E             PIC X.                 ERCACCT
00404          16  AM-RETRO-CLMS-P-I             PIC X.                 ERCACCT
00405          16  AM-RETRO-RET-BRACKET-LF.                             ERCACCT
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.                 ERCACCT
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.      ERCACCT
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.          ERCACCT
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.                 ERCACCT
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.      ERCACCT
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.          ERCACCT
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.           ERCACCT
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3. ERCACCT
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3. ERCACCT
00415          16  AM-RETRO-RET-BRACKET-AH.                             ERCACCT
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.                 ERCACCT
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.      ERCACCT
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.          ERCACCT
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.          ERCACCT
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.                 ERCACCT
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.      ERCACCT
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.          ERCACCT
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.           ERCACCT
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3. ERCACCT
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3. ERCACCT
00426                                                                   ERCACCT
00427      12  AM-COMMENTS.                                             ERCACCT
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.ERCACCT
00429                                                                   ERCACCT
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.         ERCACCT
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.                 ERCACCT
00432          16  AM-FLI-BILLING-CODE           PIC X.                 ERCACCT
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.                ERCACCT
00434          16  AM-FLI-UNITED-IDENT           PIC X.                 ERCACCT
00435          16  AM-FLI-INTEREST-LOST-DATA.                           ERCACCT
00436              20  AM-FLI-BANK-NO            PIC X(5).              ERCACCT
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3. ERCACCT
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3. ERCACCT
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3. ERCACCT
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.            ERCACCT
00441              20  AM-FLI-AGT                PIC X(9).              ERCACCT
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.                 ERCACCT
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3. ERCACCT
00444          16  FILLER                        PIC X(102).            ERCACCT
00445                                                                   ERCACCT
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.         ERCACCT
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.          ERCACCT
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.              ERCACCT
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.               ERCACCT
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.             ERCACCT
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.               ERCACCT
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.               ERCACCT
00453          16  FILLER                          PIC X(10).           ERCACCT
00454 ******************************************************************ERCACCT

       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.
                                                                        
       FD  OTHER-STATEMENTS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  OTHER-STMT-REC              PIC X(133).

       FD  REMIT-STMTS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REMIT-STMT-REC                  PIC X(133).
                                                                        
       FD  SCAN-EXT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.                                            

       01  SCAN-EXT-RECORD             PIC X(55).


       WORKING-STORAGE SECTION.                                         
       copy "ctypes.cpy".

       77  WS-HEADING-SW               PIC X   VALUE SPACES.
           88  WS-HEADING                  VALUE 'Y'.
       77  WS-REMIT-STMT-SW            PIC X   VALUE SPACES.
           88  WS-REMIT-STMT               VALUE 'Y'.
       77  WS-NEW-ACCT-SW              PIC X   VALUE SPACES.
           88  WS-NEW-ACCT                 VALUE 'Y'.
       77  WS-EOF-SW                   PIC X   VALUE SPACES.
           88  END-OF-INPUT                VALUE 'Y'.
       77  ERCOMP-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERACCT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-IN-CNT                   PIC 9(7)  VALUE ZEROS.
       77  WS-OUT-CNT                  PIC 9(7)  VALUE ZEROS.
       77  A1                          PIC S999  VALUE +0 COMP-3.
       77  WS-SAVE-STATE               PIC XX   VALUE SPACES.
       77  WS-USER-SELECT-2            PIC X(10)  VALUE SPACES.
       77  WS-REPORT-CODE-1            PIC X(10)  VALUE SPACES.
       77  WS-ACCOUNT-FOUND-SW         PIC X    VALUE SPACES.
           88  FOUND-ACCOUNT              VALUE 'Y'.
       77  WS-SAVE-ERACCT-KEY          PIC X(19)  VALUE LOW-VALUES.
       77  WS-DATE-RANGE-FOUND-SW      PIC X   VALUE SPACES.
          88  DATE-RANGE-FOUND            VALUE 'Y'.
       77  WS-ADDR-SEQ-NO              PIC 9(7)  VALUE ZEROS.
       77  WS-DIS-ADDR-SEQ-NO          PIC ZZZZZZ9  BLANK WHEN ZERO.
       77  WS-DISPLAY-AMT              PIC ---,--9.99.
       77  WS-ERACCT-SW                PIC X  VALUE ' '.
           88  I-SAY-STOP                  VALUE 'Y'.
       77  WS-TEST-STATE               PIC XX.
       01  WS-COMPARE-KEY.
           05  WS-CK-CARRIER           PIC X.
           05  WS-CK-GROUP             PIC X(6).
           05  WS-CK-RESP              PIC X(10).
           05  WS-CK-ACCOUNT           PIC X(10).
       01  WS-PREV-KEY                 PIC X(27)  VALUE LOW-VALUES.
       01  WS-MISC.
           05  WS-SUMMARY-SW           PIC X   VALUE SPACES.
               88  LAST-PAGE-SUMMARY          VALUE 'Y'.
               88  LAST-PAGE-DETAIL           VALUE 'D'.
           05  WS-SRCH-STATE           PIC X(30)  VALUE SPACES.
           05  WS-HOLD-HEAD  OCCURS 4  PIC X(133).

       01  WS-COVER-SHEET.
           05  PD-CC               PIC X.
           05  PD-ID               PIC X(4).
           05  PD-DATE1            PIC X(18).
           05  PD-CUR              PIC ---,--9.99.
           05  PD-OV30             PIC ----,--9.99.
           05  PD-OV60             PIC ----,--9.99.
           05  PD-OV90             PIC ----,--9.99.
           05  PD-END-BAL          PIC ----,--9.99.
           05  PD-ACCOUNT          PIC X(10).
           05  PD-PMT              PIC ----,--9.99.
           05  PD-BAL-FWD          PIC ----,--9.99.
           05  PD-USER-SELECT-2    PIC X(14).
           05  PD-REPORT-CODE-1    PIC X(10).
       01  REDEFINES WS-COVER-SHEET.
           05  FILLER              PIC X.
           05  PD-BARCODE1         PIC X(50).
           05  FILLER              PIC X(10).
           05  PD-BARCODE2         PIC X(25).
       01  REDEFINES WS-COVER-SHEET.
           05  FILLER           PIC X.
           05  PD-ADDRESS       PIC X(132).

       01  FILLER.
           05  SAVE-BARCODE1       PIC X(50)   VALUE SPACE.
           05  SAVE-BARCODE2       PIC X(25)   VALUE SPACE.
           05  WS-WORK-DATE            PIC 9(11)  VALUE ZEROS.
           05  WS-WORK-DATER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-SCAN-CCYYMM      PIC X(6).
               10  FILLER              PIC XX.
           05  WS-RETURN-CODE          PIC S9(4)  COMP  VALUE ZEROS.    
           05  WS-ZERO                 PIC S9     VALUE +0  COMP-3.
           05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.         
           05  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZEROS.          
           05  PGM-SUB             PIC S999   COMP-3  VALUE +061.   
           05  S0C7                PIC X       VALUE SPACE.             
           05  FORCE-DUMP REDEFINES S0C7 PIC S9 COMP-3.                 
           05  ERCOMP-STATUS       PIC X(2)    VALUE SPACE.             
           05  PREV-CO-CONTROL     PIC X(29)   VALUE SPACE.             
           05  REMIT-PAGE-NO       PIC 9(6)    VALUE ZERO.              
           05  WS-DATE             PIC 9(8)    VALUE ZERO.              
           05  WS-HDG  OCCURS 4 TIMES PIC X(133).                       
           05  WK-ADDR OCCURS 7 TIMES PIC X(30).                        
           05  WK-AMT              PIC ZZZ,ZZZ,ZZZ.99/.                 
                                                                        
       01  FILLER               COMP-3.                                 
           05  STRT             PIC S9(3)   VALUE +0.                   
           05  S1               PIC S9(3)   VALUE +0.                   
           05  WK1              PIC S9(7)   VALUE +0.                   
           05  WK2              PIC S9(7)   VALUE +0.                   

       01  WS-SAVE-SCAN-REC        PIC X(55).
       01  CID-SCAN-REC.
           05  SAV-CID-CC              PIC 99.
           05  SAV-CID-YY              PIC 99.
           05  SAV-CID-MM              PIC 99.
           05  CID-SCAN-SEQ-NO         PIC 9(7).
           05  SR-DEL1                 PIC X.
           05  CID-CARRIER             PIC X.
           05  SR-DEL2                 PIC X.
           05  CID-GROUP               PIC X(6).
           05  SR-DEL3                 PIC X.
           05  CID-FIN-RESP            PIC X(10).
           05  SR-DEL4                 PIC X.
           05  CID-ACCOUNT             PIC X(10).
           05  SR-DEL5                 PIC X.
           05  CID-AMT-DUE             PIC 9(7).99.

      ***************************************************************** 
      *  BARCODE ROUTINE                                              * 
      ***************************************************************** 
       01  AGEB16-WORKAREA.
      *     05  AGEB16-LEN       PIC 9(4)    VALUE ZERO.
           05  AGEB16-LEN       short.
           05  AGEB16-INPUT     PIC X(28)   VALUE SPACE.
           05  AGEB16-OUTPUT    PIC X(128)  VALUE SPACE.

       01  BARCODE1.
           05  BC-ENCL-CODE        PIC X(2)    VALUE ZERO.
           05  BC-MAIL-CODE        PIC X       VALUE '1'.
           05  BC-DIV-CODE         PIC X       VALUE '1'.
           05  BC-ACCT-NO.
               10  BC-CARR         PIC X(07)   VALUE ZERO.
               10  BC-ACCT         PIC X(10)   VALUE ZERO.
           05  BC-SEQ-NO           PIC 9(4)    VALUE ZERO.

       01  BARCODE2.
           05  BC1-DATE            PIC X(6).
           05  BC1-SEQ-NO          PIC 9(7)    VALUE ZERO.


                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.                                              
                                                                        
                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
              END-OF-INPUT

           PERFORM 4000-CLOSE-FILES    THRU 4000-EXIT
           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT BILLING-STATEMENTS
                                                                        
           OPEN INPUT ERCOMP                                            
           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ERCOMP - OPEN ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           OPEN INPUT ERACCT
           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ERACCT - OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           OPEN OUTPUT OTHER-STATEMENTS SCAN-EXT
                       REMIT-STMTS

           .
       0010-EXIT.
           EXIT.

       0020-INIT.

           MOVE RUN-DATE               TO WS-WORK-DATE
           MOVE SPACES                 TO WS-COVER-SHEET

           MOVE SPACES                 TO CID-SCAN-REC
           MOVE RUN-DATE               TO WS-WORK-DATE
           MOVE WS-SCAN-CCYYMM         TO BC1-DATE
                                          CID-SCAN-REC (1:6)
           MOVE ZEROS                  TO CID-SCAN-SEQ-NO
           MOVE ZEROS                  TO CID-AMT-DUE
           MOVE ';'                    TO SR-DEL1
                                          SR-DEL2
                                          SR-DEL3
                                          SR-DEL4
                                          SR-DEL5
           MOVE CID-SCAN-REC           TO WS-SAVE-SCAN-REC

           PERFORM 0040-READ-INPUT     THRU 0040-EXIT

           .                                                            
       0020-EXIT.                                                       
           EXIT.                                                        

       0040-READ-INPUT.

           READ BILLING-STATEMENTS AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO WS-IN-CNT
           END-IF

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

           IF STMT-RECORD (1:1) = '1'
              SET WS-HEADING TO TRUE
              PERFORM 0060-PROCESS-HEADING THRU 0060-EXIT
           END-IF

           IF STMT-RECORD (4:17) = 'PLEASE RETURN ONE'
              SET LAST-PAGE-SUMMARY    TO TRUE
           END-IF

           IF WS-REMIT-STMT
              PERFORM 0080-WRITE-REMIT-STMT THRU 0080-EXIT
           ELSE
              PERFORM 0090-WRITE-OTHER-STMT THRU 0090-EXIT
           END-IF

           PERFORM 0040-READ-INPUT     THRU 0040-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-PROCESS-HEADING.

           IF WS-REMIT-STMT
      *        DISPLAY ' YES REMIT STMT ' CO-CARRIER ' ' CO-RESP-NO ' '
      *           CO-ACCOUNT ' ' CO-ADDR-STATE
              MOVE SPACES                 TO WS-COVER-SHEET
              IF LAST-PAGE-SUMMARY
      *          DISPLAY ' YES LAST PAGE SUMMARY ' CO-CARRIER ' '
      *             CO-RESP-NO ' ' CO-ACCOUNT ' ' CO-ADDR-STATE
                 SET LAST-PAGE-DETAIL     TO TRUE
                 MOVE 'C'                 TO PD-CC
                 MOVE SAVE-BARCODE2       TO PD-BARCODE2
              END-IF
              MOVE WS-HOLD-HEAD (3)(126:6) TO REMIT-PAGE-NO
              DIVIDE REMIT-PAGE-NO BY 2 GIVING WK1 REMAINDER WK2        
              IF WK2 = 1
                 MOVE 'C'                 TO PD-CC
                 PERFORM 2100-BUILD-BARCODE1 THRU 2100-EXIT              
                 MOVE SAVE-BARCODE1       TO PD-BARCODE1
              END-IF
      *       IF WS-COVER-SHEET NOT = SPACES
      *          WRITE REMIT-STMT-REC         FROM WS-COVER-SHEET
      *       END-IF
           END-IF

           MOVE STMT-RECORD            TO WS-HOLD-HEAD (1)
           PERFORM 0040-READ-INPUT     THRU 0040-EXIT
           MOVE STMT-RECORD            TO WS-HOLD-HEAD (2)
           PERFORM 0040-READ-INPUT     THRU 0040-EXIT
           MOVE STMT-RECORD            TO WS-HOLD-HEAD (3)
           PERFORM 0040-READ-INPUT     THRU 0040-EXIT
           MOVE STMT-RECORD            TO WS-HOLD-HEAD (4)
           PERFORM 0040-READ-INPUT     THRU 0040-EXIT

           IF WS-HOLD-HEAD (1) (61:13) = 'OVERALL RECAP'
              GO TO 0060-EXIT
           END-IF

           MOVE WS-HOLD-HEAD (4)(16:01)
                                       TO WS-CK-CARRIER
           MOVE WS-HOLD-HEAD (4)(17:06)
                                       TO WS-CK-GROUP
           MOVE WS-HOLD-HEAD (4)(90:10)
                                       TO WS-CK-RESP
           MOVE WS-HOLD-HEAD (4)(24:10)
                                       TO WS-CK-ACCOUNT

           IF WS-COMPARE-KEY NOT = WS-PREV-KEY
              SET WS-NEW-ACCT          TO TRUE
              PERFORM 0070-READ-ERCOMP THRU 0070-EXIT
              MOVE WS-COMPARE-KEY      TO WS-PREV-KEY
           ELSE
              MOVE ' '                 TO WS-NEW-ACCT-SW
           END-IF

           .
       0060-EXIT.
           EXIT.

       0070-READ-ERCOMP.                                                

           MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD

           MOVE WS-HOLD-HEAD (4)(16:01) TO CO-CARRIER                          
           MOVE WS-HOLD-HEAD (4)(17:06) TO CO-GROUPING                         
           MOVE WS-HOLD-HEAD (4)(90:10) TO CO-RESP-NO                          
           MOVE WS-HOLD-HEAD (4)(24:10) TO CO-ACCOUNT                          
           MOVE 'A'                    TO CO-TYPE                             

           IF CO-RESP-NO = SPACES                                       
              MOVE CO-ACCOUNT          TO CO-RESP-NO
           END-IF

           IF CO-CONTROL-PRIMARY = PREV-CO-CONTROL
              GO TO 0070-EXIT
           END-IF
                                                                        
           IF (CO-CARRIER = SPACES)
              AND (CO-GROUPING = SPACES)
              DISPLAY ' CAR AND GRP SPACES ' CO-ACCOUNT
              GO TO 0070-EXIT
           END-IF

           READ ERCOMP
           IF ERCOMP-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERCOMP - READ ' ERCOMP-FILE-STATUS
                 ' KEY=' CO-CONTROL-PRIMARY (2:27)
              PERFORM ABEND-PGM
           END-IF

           MOVE CO-CONTROL-PRIMARY     TO PREV-CO-CONTROL
           MOVE '  '                   TO WS-TEST-STATE

           PERFORM VARYING A1 FROM +1 BY +1 UNTIL
              (A1 > +28)
              OR (CO-ACCT-NAME (A1:3) = '*MN')
           END-PERFORM

           IF A1 > +28
              CONTINUE
           ELSE
              MOVE 'MN'                TO WS-TEST-STATE
           END-IF

           IF (WS-TEST-STATE = 'MN')
              AND (CO-ADDR-STATE NOT = 'MN')
              DISPLAY ' NAME MN BUT NOT MAILING - CHECK ' CO-ACCOUNT
           END-IF

           IF (CO-ADDR-STATE = 'MN')
              OR (WS-TEST-STATE = 'MN')
              SET WS-REMIT-STMT TO TRUE
           ELSE
              MOVE ' '                 TO WS-REMIT-STMT-SW
           END-IF

      * I'M JUST CONFIRMING THAT IT IS MN HERE

           MOVE LOW-VALUES             TO AM-VG-KEY3
           MOVE CO-ACCOUNT             TO AM-VG-KEY3-ACCOUNT
           MOVE ' '                    TO WS-ERACCT-SW
           START ERACCT KEY IS >= AM-VG-KEY3
           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERACCT - BAD START ' ERACCT-FILE-STATUS
                 ' ' CO-ACCOUNT
              PERFORM ABEND-PGM
           END-IF

           PERFORM UNTIL I-SAY-STOP
              READ ERACCT NEXT RECORD
              IF (ERACCT-FILE-STATUS = '00')
                 IF (AM-ACCOUNT = CO-ACCOUNT)
                    IF AM-STATE = CO-ADDR-STATE
                       SET I-SAY-STOP  TO TRUE
                    END-IF
                    IF (AM-STATE = 'MN')
                       AND (NOT WS-REMIT-STMT)
                       DISPLAY ' FOUND MN ACCT BUT COMP NOT MN '
                          CO-ACCOUNT ' ' CO-ADDR-STATE
                    END-IF
                 ELSE
                    DISPLAY ' ERACCT NOT FOUND ' CO-ACCOUNT
                       ' ' CO-ADDR-STATE ' ' CO-ACCT-NAME
                    SET I-SAY-STOP     TO TRUE
                 END-IF
              ELSE
                 SET I-SAY-STOP        TO TRUE
                 DISPLAY ' ERACCT3 READ NEXT ' ERACCT-FILE-STATUS ' '
                    CO-ACCOUNT ' ' CO-ADDR-STATE
              END-IF
           END-PERFORM

      *    IF (CO-BILL-SW = 'B' OR 'C')
      *       AND (CO-CURRENT-END-BAL IS POSITIVE)
      *       MOVE CO-CURRENT-END-BAL TO WS-DISPLAY-AMT
      *       DISPLAY ' REMIT STMT ' CO-CARRIER ' ' CO-RESP-NO ' '
      *          CO-ACCOUNT ' ' CO-BILL-SW ' ' WS-DISPLAY-AMT
      *       SET WS-REMIT-STMT  TO TRUE
      *    ELSE
      *       MOVE ' '                 TO WS-REMIT-STMT-SW
      *       MOVE CO-CURRENT-END-BAL TO WS-DISPLAY-AMT
      *       DISPLAY ' NOT REMIT STMT ' CO-CARRIER ' ' CO-RESP-NO ' '
      *          CO-ACCOUNT ' ' CO-BILL-SW ' ' WS-DISPLAY-AMT
      *    END-IF

PEMTST*    IF (CO-CARRIER = '9')
PEMTST*       AND (CO-RESP-NO = '0000016480')
PEMTST*       SET END-OF-INPUT TO TRUE
PEMTST*    END-IF

           .
       0070-EXIT.                                                       
           EXIT.                                                        

       0080-WRITE-REMIT-STMT.
       
           IF WS-HEADING
      *       IF WS-NEW-ACCT
      *          PERFORM 0100-PRINT-COVER-SHEET
      *                                THRU 0100-EXIT
      *       END-IF
              WRITE REMIT-STMT-REC         FROM WS-HOLD-HEAD (1)
              WRITE REMIT-STMT-REC         FROM WS-HOLD-HEAD (2)
              WRITE REMIT-STMT-REC         FROM WS-HOLD-HEAD (3)
              WRITE REMIT-STMT-REC         FROM WS-HOLD-HEAD (4)
              MOVE ' ' TO WS-HEADING-SW
           END-IF

           WRITE REMIT-STMT-REC            FROM STMT-RECORD

           .
       0080-EXIT.                                                       
           EXIT.                                                        

       0090-WRITE-OTHER-STMT.
              
           IF WS-HEADING
              WRITE OTHER-STMT-REC FROM WS-HOLD-HEAD (1)
              WRITE OTHER-STMT-REC FROM WS-HOLD-HEAD (2)
              WRITE OTHER-STMT-REC FROM WS-HOLD-HEAD (3)
              WRITE OTHER-STMT-REC FROM WS-HOLD-HEAD (4)
              MOVE ' ' TO WS-HEADING-SW
           END-IF

           WRITE OTHER-STMT-REC FROM STMT-RECORD

           .
       0090-EXIT.                                                       
           EXIT.                                                        

       0100-PRINT-COVER-SHEET.

           SET LAST-PAGE-DETAIL TO TRUE
           PERFORM 0200-GET-ERACCT     THRU 0200-EXIT

           MOVE '1'                    TO PD-CC
           MOVE 'PDUE'                 TO PD-ID

           MOVE +0                     TO STRT
           INSPECT ALPH-DATE TALLYING STRT FOR LEADING ' '
           MOVE ALPH-DATE(STRT + 1:)   TO PD-DATE1
           MOVE CO-ACCOUNT             TO PD-ACCOUNT
           MOVE CO-CUR                 TO PD-CUR
           MOVE CO-OV30                TO PD-OV30
           MOVE CO-OV60                TO PD-OV60
           MOVE CO-OV90                TO PD-OV90
           MOVE CO-END-BAL             TO PD-END-BAL
           MOVE CO-CUR-PMT             TO PD-PMT
           MOVE CO-BAL-FWD             TO PD-BAL-FWD
           IF WS-USER-SELECT-2 NOT = SPACES
              STRING 'CC: ' WS-USER-SELECT-2 DELIMITED BY SIZE
                 INTO PD-USER-SELECT-2
              END-STRING
           ELSE
              MOVE SPACES              TO PD-USER-SELECT-2
           END-IF
           IF WS-REPORT-CODE-1 NOT = SPACES
              MOVE WS-REPORT-CODE-1    TO PD-REPORT-CODE-1
           ELSE
              MOVE SPACES              TO PD-REPORT-CODE-1
           END-IF

           WRITE REMIT-STMT-REC            FROM WS-COVER-SHEET

           MOVE SPACES                 TO WS-COVER-SHEET
           PERFORM 2100-BUILD-BARCODE1 THRU 2100-EXIT
           MOVE SAVE-BARCODE1          TO PD-BARCODE1
           PERFORM 2150-BUILD-BARCODE2 THRU 2150-EXIT
           MOVE SAVE-BARCODE2          TO PD-BARCODE2
           WRITE REMIT-STMT-REC        FROM WS-COVER-SHEET

      *    MOVE SPACES                 TO WS-COVER-SHEET
      *    PERFORM 2150-BUILD-BARCODE2 THRU 2150-EXIT
      *    MOVE SAVE-BARCODE2          TO PD-BARCODE2
      *    WRITE REMIT-STMT-REC            FROM WS-COVER-SHEET


           MOVE SPACES                 TO WS-COVER-SHEET
           ADD 1                       TO WS-ADDR-SEQ-NO
           MOVE WS-ADDR-SEQ-NO         TO WS-DIS-ADDR-SEQ-NO
           MOVE WS-DIS-ADDR-SEQ-NO     TO WK-ADDR (1)
           MOVE CO-CONTROL-NAME        TO WK-ADDR(2)
           MOVE CO-ACCT-NAME           TO WK-ADDR(3)
           MOVE CO-ADDR-1              TO WK-ADDR(4)
           MOVE CO-ADDR-2              TO WK-ADDR(5)
051810     MOVE SPACES                 TO WK-ADDR(6)
051810     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810        DELIMITED BY '  ' INTO WK-ADDR (6)
051810     END-STRING
           IF CO-ZIP-PLUS4 = SPACE
              MOVE CO-ZIP-PRIME        TO WK-ADDR(6)(26:5)
           ELSE
              MOVE CO-ZIP              TO WK-ADDR(6)(22:9)
           END-IF

           IF WK-ADDR(2) = WK-ADDR(3)
              MOVE SPACES TO WK-ADDR(3)                                 
           END-IF                                                       

           PERFORM 2 TIMES

           IF WK-ADDR(1) = SPACES                                       
              MOVE WK-ADDR(2) TO WK-ADDR(1)                             
              MOVE WK-ADDR(3) TO WK-ADDR(2)                             
              MOVE WK-ADDR(4) TO WK-ADDR(3)                             
              MOVE WK-ADDR(5) TO WK-ADDR(4)                             
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       
                                                                        
           IF WK-ADDR(2) = SPACES                                       
              MOVE WK-ADDR(3) TO WK-ADDR(2)                             
              MOVE WK-ADDR(4) TO WK-ADDR(3)                             
              MOVE WK-ADDR(5) TO WK-ADDR(4)                             
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       
                                                                        
           IF WK-ADDR(3) = SPACES                                       
              MOVE WK-ADDR(4) TO WK-ADDR(3)                             
              MOVE WK-ADDR(5) TO WK-ADDR(4)                             
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       
                                                                        
           IF WK-ADDR(4) = SPACES                                       
              MOVE WK-ADDR(5) TO WK-ADDR(4)                             
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       
                                                                        
           IF WK-ADDR(5) = SPACES                                       
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       

           END-PERFORM

           PERFORM VARYING A1 FROM 1 BY 1 UNTIL A1 > 7
              MOVE WK-ADDR(A1)         TO PD-ADDRESS                           
              WRITE REMIT-STMT-REC         FROM WS-COVER-SHEET
           END-PERFORM                                                  

           MOVE WS-SAVE-SCAN-REC       TO CID-SCAN-REC
           MOVE BC1-SEQ-NO             TO CID-SCAN-SEQ-NO
           MOVE CO-CARRIER             TO CID-CARRIER
           MOVE CO-GROUPING            TO CID-GROUP
           MOVE CO-RESP-NO             TO CID-FIN-RESP
           MOVE CO-ACCOUNT             TO CID-ACCOUNT
           MOVE CO-CURRENT-END-BAL     TO CID-AMT-DUE
           WRITE SCAN-EXT-RECORD       FROM CID-SCAN-REC

           .                                                            
       0100-EXIT.                                                       
           EXIT.                                                        

       0200-GET-ERACCT.

           MOVE SPACES                 TO WS-ACCOUNT-FOUND-SW
                                          WS-USER-SELECT-2
                                          WS-REPORT-CODE-1

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY        
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD
           MOVE CO-CARRIER             TO AM-CARRIER
           MOVE CO-GROUPING            TO AM-GROUPING
           MOVE CO-ACCOUNT             TO AM-ACCOUNT
           MOVE LOW-VALUES             TO AM-EXPIRATION-DT

      *    PERFORM 0210-GET-STATE      THRU 0210-EXIT
      *    MOVE WS-SAVE-STATE          TO AM-STATE
           MOVE CO-ADDR-STATE          TO AM-STATE

           MOVE AM-CONTROL-A           TO WS-SAVE-ERACCT-KEY
      *    START ERACCT KEY >= AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERACCT - START ' ERACCT-FILE-STATUS
                 ' KEY=' AM-CONTROL-PRIMARY (2:19)
              PERFORM ABEND-PGM
           END-IF

           MOVE SPACES                 TO WS-DATE-RANGE-FOUND-SW
           PERFORM UNTIL DATE-RANGE-FOUND
              READ ERACCT NEXT
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERACCT - READ ' ERACCT-FILE-STATUS
                    ' KEY=' AM-CONTROL-PRIMARY (2:19)
                 PERFORM ABEND-PGM
              END-IF
              
              IF (AM-CONTROL-A = WS-SAVE-ERACCT-KEY)
                 SET FOUND-ACCOUNT        TO TRUE
                 MOVE AM-USER-SELECT-2    TO WS-USER-SELECT-2
                 MOVE AM-REPORT-CODE-1    TO WS-REPORT-CODE-1
              ELSE
                 SET DATE-RANGE-FOUND     TO TRUE
                 DISPLAY ' ERACCT = ' AM-CONTROL-PRIMARY (2:19)
                    ' ERCOMP = ' CO-CONTROL-PRIMARY (2:27)
                    ' STATE = ' WS-SAVE-STATE
              END-IF
           END-PERFORM

           .
       0200-EXIT.
           EXIT.

       0210-GET-STATE.

      *  WE REALLY DON'T NEED THIS PARA BECAUSE THE STATE
      *  IS SEPARATE NOW, BUT I LEFT IT IN ANYWAY
           MOVE CO-ADDR-3              TO WS-SRCH-STATE
      
           PERFORM VARYING S1 FROM +29 BY -1 UNTIL
              (S1 < +1) OR
              ((WS-SRCH-STATE (S1:2) ALPHABETIC) AND
              (WS-SRCH-STATE (S1:2) NOT = SPACES AND LOW-VALUES)
              AND
              (WS-SRCH-STATE (S1 + 1:1) NOT = ' ' AND ',' AND
                       '.' AND LOW-VALUES) AND
              (WS-SRCH-STATE (S1:1) NOT = ' ' AND ',' AND
                       '.' AND LOW-VALUES))
           END-PERFORM
      
           IF S1 NOT < +1
              MOVE WS-SRCH-STATE (S1:2)
                                       TO WS-SAVE-STATE
           ELSE
              MOVE SPACES              TO WS-SAVE-STATE
           END-IF
      
           .
       0210-EXIT.
           EXIT.


       2100-BUILD-BARCODE1.                                              

           MOVE CO-CARR-GROUP          TO BC-CARR
           MOVE CO-ACCOUNT             TO BC-ACCT
           ADD 1                       TO BC-SEQ-NO
           MOVE 28                     TO AGEB16-LEN
           MOVE BARCODE1               TO AGEB16-INPUT
           MOVE SPACES                 TO AGEB16-OUTPUT

           CALL 'AGEB16'   USING AGEB16-LEN
                                 AGEB16-INPUT
                                 AGEB16-OUTPUT

           IF AGEB16-LEN = +128
              DISPLAY 'AGEB16 - BARCODE ROUTINE ERROR'
           ELSE
              MOVE AGEB16-OUTPUT       TO SAVE-BARCODE1
           END-IF
PEMTST*    DISPLAY  BARCODE1

           .
       2100-EXIT.
           EXIT.

       2150-BUILD-BARCODE2.

           ADD 1                       TO BC1-SEQ-NO
           MOVE 13                     TO AGEB16-LEN
           MOVE BARCODE2               TO AGEB16-INPUT
           MOVE SPACES                 TO AGEB16-OUTPUT
           CALL 'AGEB16' USING AGEB16-LEN
                               AGEB16-INPUT
                               AGEB16-OUTPUT
           IF AGEB16-LEN = +128
              DISPLAY 'AGEB16 - BARCODE ROUTINE ERROR'
           ELSE
              MOVE AGEB16-OUTPUT       TO SAVE-BARCODE2
           END-IF

           .
       2150-EXIT.
           EXIT.

       4000-CLOSE-FILES.

           CLOSE BILLING-STATEMENTS ERCOMP ERACCT SCAN-EXT
              OTHER-STATEMENTS REMIT-STMTS

           .
       4000-EXIT.
           EXIT.

       ABEND-PGM.
                           COPY ELCABEND.
                                                                        
