       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ECS0633.
       AUTHOR.        AJRA.
       DATE-COMPILED.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
032806* 032806  CR2006022800001  AJRA  NEW WRITING ACCOUNT REPORT
031811* 031811 CR2011012700001   PEMA  ADD ACCT STATUS S - SUSPENDED
091911* 091911 IR2011090700001   PEMA  REMOVE 'B' PROCESSING
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PRNTR            ASSIGN TO SYS008.  
           
           SELECT COMM-MSTR-IN     ASSIGN TO SYS010.  

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT SORT-WORK        ASSIGN TO SYS001.

           SELECT ERACCT3          ASSIGN TO SYS016
                                     ORGANIZATION  INDEXED
                                     ACCESS        DYNAMIC
                                     RECORD KEY    AM-VG-KEY3 
                                     FILE STATUS   ACCT-STATUS.

           

       DATA DIVISION.
       FILE SECTION.

       FD  PRNTR                                                        
                                   COPY ELCPRTFD.                       

       FD  COMM-MSTR-IN                                                 
                                   COPY ECSCOIFD.                       

       FD  DISK-DATE
                                   COPY ELCDTEFD.

       SD  SORT-WORK.
       01  SORT-REC.
           12  SORT-KEY             PIC  X(13).
           12  SORT-DETAIL          PIC  X(30).

       FD  ERACCT3.

      *     COPY ERCACCT.
00001 ******************************************************************04/19/98
00002 *                                                                *ERCACCT
00002 *                                                                *ERCACCT
00003 *                            ERCACCT                             *   LV031
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE               CL*31
00005 *                            VMOD=2.031                          *ERCACCT
00006 *                                                                *ERCACCT
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *ERCACCT
00008 *                                                                *ERCACCT
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *ERCACCT
00010 *   VSAM ACCOUNT MASTER FILES.                                   *ERCACCT
00011 *                                                                *ERCACCT
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *ERCACCT
00013 *                                                                *ERCACCT
00014 *   FILE TYPE = VSAM,KSDS                                        *ERCACCT
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *ERCACCT
00016 *                                                                *ERCACCT
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *ERCACCT
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *ERCACCT
00019 *                                                                *ERCACCT
00020 *   LOG = NO                                                     *ERCACCT
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *ERCACCT
00022 *                                                                *ERCACCT
00023 *                                                                *ERCACCT
00024 ******************************************************************ERCACCT
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
102004******************************************************************
00025                                                                   ERCACCT
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
031811         88  AM-ACCOUNT-FROZEN                VALUE '4'.
031811         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
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

     
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   ECS0633  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  S1                          PIC S999 VALUE +0 COMP-3.

       01  ACCT-STATUS              PIC XX             VALUE '00'.
       01  WS-HOLD-STATE            PIC XX             VALUE SPACES.
       01  WS-LINE-COUNT            COMP-3  PIC S9(03) VALUE +60.
       01  WS-LINE-COUNT-MAX        COMP-3  PIC S9(03) VALUE +55.
       01  WS-PAGE                  COMP-3  PIC S9(05) VALUE +0.
       01  WS-RELEASE-SORT-COUNT    COMP-3  PIC S9(05) VALUE +0.
       01  WS-RETURN-SORT-COUNT     COMP-3  PIC S9(05) VALUE +0.
       01  WS-EOF-SW                        PIC  X(01) VALUE SPACE.
           88  END-OF-COMM-IN                          VALUE 'Y'.

       01  WS-EOF-SW2                       PIC  X(01) VALUE SPACE.
           88  END-OF-SORT-FILE                        VALUE 'Y'.
       01  WS-ACCT-REC-FOUND                PIC  X(01) VALUE 'N'.
           88  ACCT-REC-FOUND                          VALUE 'Y'.

       01  COMM-RECS-IN                     PIC  9(09) VALUE ZEROS.
       01  SUB1                             PIC S9(05) VALUE +0 COMP-3.
       01  PGM-SUB                   COMP-3 PIC S9(03) VALUE +0.

       01  WS-ABEND-FIELDS.
           05  WS-RETURN-CODE               PIC S9(04) VALUE ZERO.
           05  WS-ZERO                      PIC S9(01) VALUE ZERO.
           05  WS-ABEND-MESSAGE             PIC  X(80) VALUE SPACES.
           05  WS-ABEND-FILE-STATUS         PIC  X(02) VALUE ZERO.

      ***** SORT WORK AREA  *****  
       01  WS-SORT-REC.
           05  WS-SORT-KEY.
               10  WS-SORT-CARRIER          PIC  X(01) VALUE SPACE.
               10  WS-SORT-STATE            PIC  X(02) VALUE SPACES.
               10  WS-SORT-ACCOUNT-NUM      PIC  X(10) VALUE SPACES.
           05  WS-SORT-ACCOUNT-NAME         PIC  X(30) VALUE SPACES.           
       
       01  HD1.              
           12  FILLER              PIC  X(01)      VALUE '1'.                                         
           12  FILLER              PIC  X(43)      VALUE SPACES.     
           12  FILLER              PIC  X(44)      VALUE             
                   '      ACCOUNTS WRITING FOR FIRST TIME       '.   
           12  FILLER              PIC  X(26)      VALUE SPACES.     
           12  FILLER              PIC  X(08)      VALUE 'ECS0633'.   
           12  FILLER              PIC  X(05)      VALUE SPACES.     
                                                                     
       01  HD2.                                                      
           12  FILLER              PIC  X(51)      VALUE SPACES.     
           12  HD-CO               PIC  X(30).                       
           12  FILLER              PIC  X(33)      VALUE SPACES.     
           12  HD-RUN-DT           PIC  X(08)      VALUE SPACES.     
           12  FILLER              PIC  X(05)      VALUE SPACES.     
                                                                     
       01  HD3.                                                      
           12  FILLER              PIC  X(57)      VALUE SPACES.     
           12  HD-DT               PIC  X(18).                       
           12  FILLER              PIC  X(39)      VALUE SPACES.     
           12  FILLER              PIC  X(05)      VALUE 'PAGE '.    
           12  HD-PG               PIC ZZ,ZZ9.                       
           12  FILLER              PIC  X(05)      VALUE SPACES.     
                                                                     
       01  HD4.                                                                                                                    
           12  FILLER              PIC  X(01)   VALUE '0'.
           12  FILLER              PIC  X(31)   VALUE SPACES.
           12  FILLER              PIC  X(07)   VALUE 'CARRIER'.
           12  FILLER              PIC  X(02)   VALUE SPACES.
           12  FILLER              PIC  X(05)   VALUE 'STATE'.
           12  FILLER              PIC  X(03)   VALUE SPACES.
           12  FILLER              PIC  X(11)   VALUE 'ACCOUNT NUM'.
           12  FILLER              PIC  X(05)   VALUE SPACES.
           12  FILLER              PIC  X(12)   VALUE 'ACCOUNT NAME'.
           12  FILLER              PIC  X(56)   VALUE SPACES.                   

       01  DETAIL-LINE.
           05  FILLER              PIC  X(35)      VALUE SPACES.
           05  DET-CARRIER         PIC  X(01).
           05  FILLER              PIC  X(07)      VALUE SPACES.
           05  DET-STATE           PIC  X(02).
           05  FILLER              PIC  X(05)      VALUE SPACES.
           05  DET-ACCOUNT-NUM     PIC  X(10).
           05  FILLER              PIC  X(05)      VALUE SPACES.
           05  DET-ACCOUNT-NAME    PIC  X(30).
           05  FILLER              PIC  X(40)      VALUE SPACES.   
           
       01  DET-NO-NEW.
           05  FILLER              PIC X(35)       VALUE SPACES.
           05  FILLER              PIC X(50)       VALUE
                'NO ACCOUNTS WRITING FOR THE FIRST TIME THIS MONTH'. 
           05  FILLER              PIC X(48)       VALUE SPACES.

       01  BLANK-LINE              PIC  X(133)     VALUE SPACES.
       
       COPY ERCCOMP.
       COPY ELCDATE.
       COPY ELCDTECX.
       COPY ELCDTEVR.

       PROCEDURE DIVISION.

      ******************************************************************
      ***        D A T E   C A R D   L O A D   R O U T I N E         ***
      ******************************************************************
     
           COPY ELCDTERX.
      ******************************************************************

           PERFORM 0400-OPEN-FILES       THRU 0400-EXIT
           PERFORM 0600-INITIALIZE       THRU 0600-EXIT

           SORT SORT-WORK ASCENDING KEY  SORT-KEY
               INPUT PROCEDURE 
                   0100-INPUT-ROUTINE    THRU 0100-EXIT
               OUTPUT PROCEDURE 
                   0200-OUTPUT-ROUTINE   THRU 0200-EXIT
      
           IF SORT-RETURN NOT = ZERO
               MOVE 'SORT FAILED'        TO WS-ABEND-MESSAGE
               MOVE SORT-RETURN          TO WS-RETURN-CODE
               PERFORM ABEND-PGM         THRU APS-EXIT
           END-IF

           PERFORM 0500-CLOSE-FILES      THRU 0500-EXIT

           DISPLAY ' COMM RECORDS READ    '  COMM-RECS-IN
           DISPLAY ' SORT RECORDS RELEASED '  WS-RELEASE-SORT-COUNT
           DISPLAY ' SORT RECORDS RETURNED '  WS-RETURN-SORT-COUNT
           DISPLAY ' WS LINE COUNT '  WS-LINE-COUNT
         
           GOBACK

           .
       0100-INPUT-ROUTINE.
            
           PERFORM 0110-PROCESS-COMM-MSTR-IN     THRU 0110-EXIT
               UNTIL END-OF-COMM-IN

           .
       0100-EXIT.
           EXIT.


       0110-PROCESS-COMM-MSTR-IN.

           READ COMM-MSTR-IN
               AT END
                   SET END-OF-COMM-IN  TO TRUE
                   GO TO 0110-EXIT
           END-READ

           ADD +1                      TO COMM-RECS-IN

           MOVE COMP-IN-RECORD TO COMPENSATION-MASTER

091911     IF CO-TYPE = 'A'
              CONTINUE
           ELSE
              GO TO 0110-EXIT
           END-IF
           
           IF CO-FIRST-WRITTEN-DT <> BIN-RUN-DATE
               GO TO 0110-EXIT
           END-IF
           
           PERFORM 0150-GET-ACCT-STATE THRU 0150-EXIT
           
           IF WS-ACCT-REC-FOUND = 'N'
               DISPLAY "ACCOUNT REC NOT FOUND ON ACCT MSTR - " 
                        CO-RESP-NO
               GO TO 0110-EXIT
           END-IF
           	
           MOVE CO-CARRIER             TO WS-SORT-CARRIER
           MOVE WS-HOLD-STATE          TO WS-SORT-STATE
      *     MOVE CO-ACCOUNT             TO WS-SORT-ACCOUNT-NUM
           MOVE CO-RESP-NO             TO WS-SORT-ACCOUNT-NUM
           MOVE CO-ACCT-NAME           TO WS-SORT-ACCOUNT-NAME	
           
           RELEASE SORT-REC            FROM WS-SORT-REC
           ADD +1                      TO WS-RELEASE-SORT-COUNT

           .
       0110-EXIT.
           EXIT.

       0150-GET-ACCT-STATE.
      
           MOVE CO-RESP-NO  TO AM-VG-KEY3-ACCOUNT
           MOVE LOW-VALUES  TO AM-VG-KEY3-EXP-DT
           
           START ERACCT3 KEY IS NOT < AM-VG-KEY3
           EVALUATE ACCT-STATUS
               WHEN '00'
                 CONTINUE
               WHEN '23'
                 DISPLAY "ACCT MSTR REC NOT FOUND ON START - " 
                         CO-RESP-NO
                 MOVE 'N' TO WS-ACCT-REC-FOUND
                 GO TO 0150-EXIT
               WHEN OTHER
                 MOVE ACCT-STATUS TO WS-ABEND-FILE-STATUS
                 MOVE 'START ERROR ON ERACCT FILE' TO WS-ABEND-MESSAGE
                 PERFORM ABEND-PGM
           END-EVALUATE
           
           READ ERACCT3 NEXT RECORD
           EVALUATE ACCT-STATUS
               WHEN '00'
                 CONTINUE
               WHEN '10'
                 DISPLAY "ACCT MSTR NOT FOUND ON READ - " CO-RESP-NO
                 MOVE 'N' TO WS-ACCT-REC-FOUND
                 GO TO 0150-EXIT
               WHEN OTHER
                 MOVE ACCT-STATUS TO WS-ABEND-FILE-STATUS
                 MOVE 'READ NEXT ERROR ON ERACCT FILE'
                       TO WS-ABEND-MESSAGE
                 PERFORM ABEND-PGM
           END-EVALUATE

           IF (AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              OR (AM-CARRIER    NOT = CO-CARRIER)
              OR (AM-GROUPING   NOT = CO-GROUPING)
              OR (AM-ACCOUNT    NOT = CO-RESP-NO)
                  DISPLAY "ACCT NOT MATCHED - AM=" AM-ACCOUNT
                          " CO=" CO-RESP-NO
                  MOVE 'N' TO WS-ACCT-REC-FOUND
           ELSE
               MOVE AM-STATE TO WS-HOLD-STATE
               MOVE 'Y' TO WS-ACCT-REC-FOUND
           END-IF

           .
       0150-EXIT.
           EXIT.




       0200-OUTPUT-ROUTINE.

           PERFORM 0210-RETURN-SORT    THRU 0210-EXIT
               UNTIL END-OF-SORT-FILE
               
           IF WS-RETURN-SORT-COUNT = 0
               MOVE DET-NO-NEW        TO DETAIL-LINE
               PERFORM 0300-PRINT  THRU 0300-EXIT
           END-IF
           
           . 
       0200-EXIT.
           EXIT.


       0210-RETURN-SORT.

           RETURN SORT-WORK                INTO WS-SORT-REC 
               AT END
                   SET END-OF-SORT-FILE    TO TRUE
                   GO TO 0210-EXIT
           END-RETURN

           ADD +1                          TO WS-RETURN-SORT-COUNT

           MOVE WS-SORT-CARRIER            TO DET-CARRIER
           MOVE WS-SORT-STATE              TO DET-STATE
           MOVE WS-SORT-ACCOUNT-NUM        TO DET-ACCOUNT-NUM
           MOVE WS-SORT-ACCOUNT-NAME       TO DET-ACCOUNT-NAME
           
           PERFORM 0300-PRINT              THRU 0300-EXIT

           .
       0210-EXIT.
           EXIT.

       0300-PRINT.
           
           IF WS-LINE-COUNT > WS-LINE-COUNT-MAX 
               MOVE +0                     TO WS-LINE-COUNT
               ADD +1                      TO WS-PAGE 
               MOVE WS-PAGE                TO HD-PG
               WRITE PRT                   FROM HD1
               WRITE PRT                   FROM HD2
               WRITE PRT                   FROM HD3
               WRITE PRT                   FROM HD4
               WRITE PRT                   FROM BLANK-LINE
               ADD +7                      TO WS-LINE-COUNT
           END-IF 
                 
           WRITE PRT                       FROM DETAIL-LINE
           ADD +1                          TO WS-LINE-COUNT
       
           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT COMM-MSTR-IN
               OUTPUT PRNTR
               
           OPEN INPUT ERACCT3
           IF ACCT-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE ACCT-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'OPEN ERROR ON ERACCT FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF
               
           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE COMM-MSTR-IN
                 ERACCT3
                 PRNTR
           .

       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE COMPANY-NAME           TO HD-CO

           ACCEPT WS-ACCEPT-DATE       FROM DATE
           MOVE WS-AD-MM               TO WS-CD-MM
           MOVE WS-AD-DD               TO WS-CD-DD
           MOVE WS-AD-YY               TO WS-CD-YY
           MOVE WS-CURRENT-DATE        TO HD-RUN-DT

           MOVE ALPH-DATE              TO HD-DT

           .
       0600-EXIT.
           EXIT.

      
       8510-DATE-CONVERSION.
      
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA
      
           .
      
       8590-EXIT.
           EXIT.

       ABEND-PGM SECTION.
                                     COPY ELCABEND.

