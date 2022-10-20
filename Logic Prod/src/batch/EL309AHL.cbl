00001  IDENTIFICATION DIVISION.                                         03/11/98
00002                                                                   EL309
00003  PROGRAM-ID.                 EL309 .                                 LV004
00004 *              PROGRAM CONVERTED BY                               EL309
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL309
00006 *              CONVERSION DATE 05/06/94 09:18:02.                 EL309
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL309
00008 *                            VMOD=2.024.                          EL309
00009                                                                   EL309
00009                                                                   EL309
00010 *AUTHOR.     LOGIC, INC.                                          EL309
00011 *            DALLAS, TEXAS.                                       EL309
00012                                                                   EL309
00013 *DATE-COMPILED.                                                   EL309
00014                                                                   EL309
00015 *SECURITY.   *****************************************************EL309
00016 *            *                                                   *EL309
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL309
00018 *            *                                                   *EL309
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL309
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL309
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL309
00022 *            *                                                   *EL309
00023 *            *****************************************************EL309
00024                                                                   EL309
00025 *REMARKS.                                                         EL309
00026 *    ANY MODIFICATIONS MADE TO THE LOGIC OF THIS PROGRAM          EL309
00027 *    SHOULD ALSO BE MADE TO ELGX309 (FIA ONLY) AND EL309DUM       EL309
00028 *        THIS PROGRAM IS RUN ONCE A MONTH TO CREATE A CLAIM       EL309
00029 *    HISTORY EXTRACT TAPE USED AS INPUT TO CLAIMS MONTH END       EL309
00030 *    REPORTING.                                                   EL309
00031 *        EACH COMPANY'S OPTIONS FOR PROGRAM EL309 ARE READ DURING EL309
00032 *    THE EXECUTION OF THIS PROGRAM.  ONLY OPTIONS WITH A 'NONE'   EL309
00033 *    FREQUENCY WILL BE RECOGNIZED.                                EL309
00034                                                                   EL309
00035 *    INPUT FILES  - ELCNTL - CONTROL FILE                         EL309
00036 *                   ELCERT - CERTIFICIATE MASTER                  EL309
00037 *                   ELMSTR - CLAIM MASTER                         EL309
00038 *                   ELTRLR - ACTIVITY TRAILERS                    EL309
00039 *                   ELACCT - ACCOUNT MASTER                       EL309
00040 *                   ELCHKQ - CHECK QUE                            EL309
00041 *                   ELPGMS - PROGRAM OPTIONS SELECTED             EL309
00042 *                   ELARCH - LETTER ARCHIVE                       EL309
00043 *                   ELRETR - RETRIEVE FILE                        EL309
00044 *                   MPPLCY - CONVENIENCE POLICY MASTER            EL309
00045 *                                                                 EL309
00046 *    OUTPUT FILES - ELHIST - HISTORY ARCHIVE FILE                 EL309
00047 *                   ELMSTR - CLAIM MASTER                         EL309
00048                                                                   EL309
00049      EJECT                                                        EL309
00050  ENVIRONMENT DIVISION.                                            EL309
00051                                                                   EL309
00052  INPUT-OUTPUT SECTION.                                            EL309
00053                                                                   EL309
00054  FILE-CONTROL.                                                    EL309
00055                                                                   EL309
00056      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL309
00057                                                                   EL309
00058      SELECT ELCNTL           ASSIGN TO SYS021-FBA1-ELCNTL         EL309
00059                              ORGANIZATION IS INDEXED              EL309
00060                              ACCESS IS DYNAMIC                    EL309
00061                              RECORD KEY IS CF-CONTROL-PRIMARY     EL309
00062                              FILE STATUS IS ELCNTL-FILE-STATUS.   EL309
00063                                                                   EL309
00064      SELECT ELCERT           ASSIGN TO SYS022-FBA1-ELCERT         EL309
00065                              ORGANIZATION IS INDEXED              EL309
00066                              ACCESS IS DYNAMIC                    EL309
00067                              RECORD KEY IS CM-CONTROL-PRIMARY     EL309
00068                              FILE STATUS IS ELCERT-FILE-STATUS.   EL309
00069                                                                   EL309
00070      SELECT MPPLCY           ASSIGN TO SYS025-FBA1-MPPLCY         EL309
00071                              ORGANIZATION IS INDEXED              EL309
00072                              ACCESS IS DYNAMIC                    EL309
00073                              RECORD KEY IS PM-CONTROL-PRIMARY     EL309
00074                              FILE STATUS IS MPPLCY-FILE-STATUS.   EL309
00075                                                                   EL309
00076      SELECT ELMSTR           ASSIGN TO SYS023-FBA1-ELMSTR         EL309
00077                              ORGANIZATION IS INDEXED              EL309
00078                              ACCESS IS DYNAMIC                    EL309
00079                              RECORD KEY IS CL-CONTROL-PRIMARY     EL309
00080                              FILE STATUS IS ELMSTR-FILE-STATUS.   EL309
00081                                                                   EL309
00082      SELECT ELTRLR           ASSIGN TO SYS024-FBA1-ELTRLR         EL309
00083                              ORGANIZATION IS INDEXED              EL309
00084                              ACCESS IS DYNAMIC                    EL309
00085                              RECORD KEY IS AT-CONTROL-PRIMARY     EL309
00086                              FILE STATUS IS ELTRLR-FILE-STATUS.   EL309
00087                                                                   EL309
00088      SELECT ELPGMS           ASSIGN TO SYS027-FBA1-ELPGMS         EL309
00089                              ORGANIZATION IS INDEXED              EL309
00090                              ACCESS IS DYNAMIC                    EL309
00091                              RECORD KEY IS PS-CONTROL-PRIMARY     EL309
00092                              FILE STATUS IS ELPGMS-FILE-STATUS.   EL309
00093                                                                   EL309
00094      SELECT ELARCH           ASSIGN TO SYS028-FBA1-ELARCH         EL309
00095                              ORGANIZATION IS INDEXED              EL309
00096                              ACCESS IS DYNAMIC                    EL309
00097                              RECORD KEY IS LA-CONTROL-PRIMARY     EL309
00098                              FILE STATUS IS ELARCH-FILE-STATUS.   EL309
00099                                                                   EL309
00100      SELECT ELRETR           ASSIGN TO SYS029-FBA1-ELRETR         EL309
00101                              ORGANIZATION IS INDEXED              EL309
00102                              ACCESS IS DYNAMIC                    EL309
00103                              RECORD KEY IS RL-CONTROL-PRIMARY     EL309
00104                              FILE STATUS IS ELRETR-FILE-STATUS.   EL309
00105                                                                   EL309
00106      SELECT HISTORY-INPUT-FILE                                    EL309
00107          ASSIGN TO SYS011-UT-2400-S-SYS011.                       EL309
00108                                                                   EL309
00109      SELECT HISTORY-OUTPUT-FILE                                   EL309
00110          ASSIGN TO SYS012-UT-2400-S-SYS012.                       EL309
00111                                                                   EL309
00112      EJECT                                                        EL309
00113  DATA DIVISION.                                                   EL309
00114                                                                   EL309
00115  FILE SECTION.                                                    EL309
00116                                                                   EL309
00117  FD  ELCNTL.                                                      EL309
00118                                                                   EL309
00119                                  COPY ELCCNTL.                    EL309
00120                                                                   EL309
00121      EJECT                                                        EL309
00122  FD  ELCERT.                                                      EL309
00123                                                                   EL309
00124                                  COPY ELCCERT.                    EL309
00125                                                                   EL309
00126      EJECT                                                        EL309
00127  FD  MPPLCY.                                                      EL309
00128                                                                   EL309
00129                                  COPY MPCPLCY.                    EL309
00130                                                                   EL309
00131      EJECT                                                        EL309
00132  FD  ELMSTR.                                                      EL309
00133                                                                   EL309
00134                                  COPY ELCMSTR.                    EL309
00135                                                                   EL309
00136      EJECT                                                        EL309
00137  FD  ELTRLR.                                                      EL309
00138                                                                   EL309
00139                                  COPY ELCTRLR.                    EL309
00140                                                                   EL309
00141      EJECT                                                        EL309
00142  FD  ELPGMS.                                                      EL309
00143                                                                   EL309
00144                                  COPY ELCPGMS.                    EL309
00145                                                                   EL309
00146      EJECT                                                        EL309
00147  FD  ELARCH.                                                      EL309
00148                                                                   EL309
00149                                  COPY ELCARCH.                    EL309
00150                                                                   EL309
00151      EJECT                                                        EL309
00152  FD  ELRETR.                                                      EL309
00153                                                                   EL309
00154                                  COPY ELCRETR.                    EL309
00155                                                                   EL309
00156      EJECT                                                        EL309
00157  FD  PRNTR                       COPY ELCPRTFD.                   EL309
00158                                                                   EL309
00159      EJECT                                                        EL309
00160  FD  HISTORY-INPUT-FILE          COPY ELCHAF.                     EL309
00161                                                                   EL309
00162      EJECT                                                        EL309
00163  FD  HISTORY-OUTPUT-FILE         COPY ELCHAF REPLACING            EL309
00164      HISTORY-INPUT-RECORD        BY  HISTORY-OUTPUT-RECORD        EL309
00165      HISTORY-INPUT-CLAIM-RECORD  BY  HISTORY-OUTPUT-CLAIM-RECORD  EL309
00166      HISTORY-INPUT-CERT-RECORD   BY  HISTORY-OUTPUT-CERT-RECORD   EL309
pemuni*    HISTORY-INPUT-POLICY-RECORD BY  HISTORY-OUTPUT-POLICY-RECORD EL309
00168      HISTORY-INPUT-TRAILER-RECORD BY HISTORY-OUTPUT-TRAILER-RECORDEL309
00169      HISTORY-INPUT-LETTER-RECORD BY  HISTORY-OUTPUT-LETTER-RECORD EL309
00170      HIR-COMPANY-ID              BY  HOR-COMPANY-ID               EL309
00171      HIR-CLAIM-KEY               BY  HOR-CLAIM-KEY                EL309
00172      HIR-DATE-ARCHIVED           BY  HOR-DATE-ARCHIVED            EL309
00173      HIR-PURGED-CLAIM            BY  HOR-PURGED-CLAIM             EL309
00174      HIR-RECORD-ID               BY  HOR-RECORD-ID                EL309
00175      HIR-CLAIM-RECORD            BY  HOR-CLAIM-RECORD             EL309
00176      HIR-CERTIFICATE-RECORD      BY  HOR-CERTIFICATE-RECORD       EL309
pemuni*    HIR-POLICY-RECORD           BY  HOR-POLICY-RECORD            EL309
00178      HIR-ACTIVITY-TRAILER-RECORD BY  HOR-ACTIVITY-TRAILER-RECORD  EL309
00179      HIR-LETTER-ARCHIVE-RECORD   BY  HOR-LETTER-ARCHIVE-RECORD.   EL309
00180                                                                   EL309
pemuni 01  HISTORY-OUTPUT-RECORD2          PIC X(476).                  EL309
pemuni*01  HISTORY-OUTPUT-RECORD2          PIC X(1226).                 EL309
00182                                                                   EL309
00183      EJECT                                                        EL309
00184  WORKING-STORAGE SECTION.                                         EL309
00185  77  FILLER  PIC X(32)   VALUE '********************************'.EL309
00186  77  FILLER  PIC X(32)   VALUE '*     EL309  WORKING STORAGE   *'.EL309
00187  77  FILLER   PIC X(32) VALUE  '******** VMOD=2.024 ************'.EL309
00188                                                                   EL309
00189  01  FILLER                          COMP-3.                      EL309
00190      05  WS-LINE-COUNT               PIC S9(3)   VALUE +99.       EL309
00191      05  WS-LINE-COUNT-MAX           PIC S9(3)   VALUE +60.       EL309
00192      05  WS-PAGE                     PIC S9(5)   VALUE ZERO.      EL309
00193      05  WS-REPORT-SW                PIC S9      VALUE +1.        EL309
00194      05  WS-RECORD-COUNT             PIC S9(9)   VALUE ZERO.      EL309
00195      05  WS-RETURN-CODE              PIC S9(3)   VALUE ZERO.      EL309
00196      05  WS-ZERO                     PIC S9      VALUE ZERO.      EL309
00197      05  WS-BYPASS-SW                PIC S9      VALUE ZERO.      EL309
00198      05  WS-DUP-SW                   PIC S9      VALUE ZERO.      EL309
00199      05  WS-CURRENT-TIME             PIC S9(7)   VALUE ZERO.      EL309
00200                                                                   EL309
00201      05  WS-WORK                     PIC S9(7)   VALUE ZERO.      EL309
00202      05  WS-CONTROL-TOTAL            PIC S9(9)   VALUE ZERO.      EL309
00203      05  WS-DIFF                     PIC S9(9)   VALUE ZERO.      EL309
00204      05  WS-NEXT-CL-HISTORY          PIC S9      VALUE ZERO.      EL309
00205      05  WS-NEXT-PURGED-IPT          PIC S9      VALUE ZERO.      EL309
00206      05  WS-NEXT-OPEN-IPT            PIC S9      VALUE ZERO.      EL309
00207      05  WS-NEXT-CLOSED-IPT          PIC S9      VALUE ZERO.      EL309
00208      05  WS-REMAINDER                PIC S9(5)   VALUE ZERO.      EL309
00209                                                                   EL309
00210      05  WS-CLAIM-COUNT              PIC S9(7)   VALUE ZERO.      EL309
00211      05  WS-TRAILER-COUNT            PIC S9(7)   VALUE ZERO.      EL309
00212                                                                   EL309
00213      05  WS-TOTAL-CLAIM-COUNT        PIC S9(7)   VALUE ZERO.      EL309
00214      05  WS-TOTAL-TRAILER-COUNT      PIC S9(7)   VALUE ZERO.      EL309
00215                                                                   EL309
00216      05  WS-HISTORY-FILE-OPEN        PIC S9      VALUE +1.        EL309
00217          88  HISTORY-FILE-OPEN                   VALUE +1, +2, +3.EL309
00218          88  END-OF-HISTORY-FILE                 VALUE +3.        EL309
00219                                                                   EL309
00220      05  WS-HISTORY-INPUT-COUNT      PIC S9(9)   VALUE ZERO.      EL309
00221      05  WS-HISTORY-OUTPUT-COUNT     PIC S9(9)   VALUE ZERO.      EL309
00222                                                                   EL309
00223      05  WS-TOTAL-HISTORY-INPUT      PIC S9(9)   VALUE ZERO.      EL309
00224      05  WS-TOTAL-HISTORY-OUTPUT     PIC S9(9)   VALUE ZERO.      EL309
00225                                                                   EL309
00226      05  WS-CL-HISTORY-INPUT         PIC S9(9)   VALUE ZERO.      EL309
00227      05  WS-CL-HISTORY-OUTPUT        PIC S9(9)   VALUE ZERO.      EL309
00228      05  WS-CM-HISTORY-INPUT         PIC S9(9)   VALUE ZERO.      EL309
00229      05  WS-CM-HISTORY-OUTPUT        PIC S9(9)   VALUE ZERO.      EL309
00230      05  WS-PM-HISTORY-INPUT         PIC S9(9)   VALUE ZERO.      EL309
00231      05  WS-PM-HISTORY-OUTPUT        PIC S9(9)   VALUE ZERO.      EL309
00232      05  WS-AT-HISTORY-INPUT         PIC S9(9)   VALUE ZERO.      EL309
00233      05  WS-AT-HISTORY-OUTPUT        PIC S9(9)   VALUE ZERO.      EL309
00234      05  WS-LA-HISTORY-INPUT         PIC S9(9)   VALUE ZERO.      EL309
00235      05  WS-LA-HISTORY-OUTPUT        PIC S9(9)   VALUE ZERO.      EL309
00236      05  WS-OTHER-HISTORY-INPUT      PIC S9(9)   VALUE ZERO.      EL309
00237      05  WS-OTHER-HISTORY-OUTPUT     PIC S9(9)   VALUE ZERO.      EL309
00238      05  WS-OPEN-CLAIMS              PIC S9(9)   VALUE ZERO.      EL309
00239      05  WS-CLOSED-CLAIMS            PIC S9(9)   VALUE ZERO.      EL309
00240      05  WS-PURGED-CLAIMS            PIC S9(9)   VALUE ZERO.      EL309
00241      05  WS-RETRIEVE-CLAIMS-FOUND    PIC S9(9)   VALUE ZERO.      EL309
00242                                                                   EL309
00243      05  WS-OPEN-CLAIMS-IPT          PIC S9(9)   VALUE ZERO.      EL309
00244      05  WS-CLOSED-CLAIMS-IPT        PIC S9(9)   VALUE ZERO.      EL309
00245      05  WS-PURGED-CLAIMS-IPT        PIC S9(9)   VALUE ZERO.      EL309
00246                                                                   EL309
00247      05  WS-OPEN-CLAIMS-OPT          PIC S9(9)   VALUE ZERO.      EL309
00248      05  WS-CLOSED-CLAIMS-OPT        PIC S9(9)   VALUE ZERO.      EL309
00249      05  WS-PURGED-CLAIMS-OPT        PIC S9(9)   VALUE ZERO.      EL309
00250                                                                   EL309
00251      05  WS-CL-VALID-DEL             PIC S9(9)   VALUE ZERO.      EL309
00252      05  WS-CL-NEW-PURGED            PIC S9(9)   VALUE ZERO.      EL309
00253                                                                   EL309
00254      05  WS-CL-HISTORY-DROPPED       PIC S9(9)   VALUE ZERO.      EL309
00255      05  WS-CM-HISTORY-DROPPED       PIC S9(9)   VALUE ZERO.      EL309
00256      05  WS-PM-HISTORY-DROPPED       PIC S9(9)   VALUE ZERO.      EL309
00257      05  WS-AT-HISTORY-DROPPED       PIC S9(9)   VALUE ZERO.      EL309
00258      05  WS-LA-HISTORY-DROPPED       PIC S9(9)   VALUE ZERO.      EL309
00259      05  WS-TOTAL-HISTORY-DROPPED    PIC S9(9)   VALUE ZERO.      EL309
00260                                                                   EL309
00261      05  WS-CL-HISTORY-ADDED         PIC S9(9)   VALUE ZERO.      EL309
00262      05  WS-CM-HISTORY-ADDED         PIC S9(9)   VALUE ZERO.      EL309
00263      05  WS-PM-HISTORY-ADDED         PIC S9(9)   VALUE ZERO.      EL309
00264      05  WS-AT-HISTORY-ADDED         PIC S9(9)   VALUE ZERO.      EL309
00265      05  WS-LA-HISTORY-ADDED         PIC S9(9)   VALUE ZERO.      EL309
00266      05  WS-TOTAL-HISTORY-ADDED      PIC S9(9)   VALUE ZERO.      EL309
00267      EJECT                                                        EL309
00268  01  FILLER   COMP   SYNC.                                        EL309
00269      05  PGM-SUB                     PIC S9(4)   VALUE +309.      EL309
00270      05  WS-INDEX                    PIC S9(4)   VALUE ZERO.      EL309
00271      05  WS-LENGTH                   REDEFINES                    EL309
00272          WS-INDEX                    PIC S9(4).                   EL309
00273      05  MAX-TABLE-INDX              PIC S9(4)   VALUE +100.      EL309
00274                                                                   EL309
00275  01  FILLER.                                                      EL309
00276      05  WS-LETTER-MESSAGE.                                       EL309
00277          10  FILLER              PIC X(25)  VALUE                 EL309
00278                   'ARCHIVE LETTER NOT FOUND '.                    EL309
00279          10  WS-NUMBER-DISPLAY   PIC ZZZZZZZ9  VALUE ZEROS.       EL309
00280          10  FILLER              PIC X(27)  VALUE SPACES.         EL309
00281                                                                   EL309
00282      05  WS-LAST-COMPANY-ID      PIC X(3) VALUE LOW-VALUES.       EL309
00283      05  WS-LAST-CLAIM-KEY       PIC X(20) VALUE LOW-VALUES.      EL309
00284      05  WS-LAST-CL-KEY          PIC X(20) VALUE LOW-VALUES.      EL309
00285      05  WS-LAST-RECORD-ID       PIC XX          VALUE 'AT'.      EL309
00286      05  WS-LAST-CLAIM-STATUS    PIC X     VALUE SPACES.          EL309
00287      05  WS-EXTRACT-ERROR-SW     PIC X     VALUE SPACES.          EL309
00288      05  WS-SAVE-CLAIM           PIC X(350) VALUE SPACES.         EL309
00289                                                                   EL309
00290      05  WS-CLAIM-MASTER.                                         EL309
00291          10  FILLER                  PIC X(114).                  EL309
00292          10  WS-STATUS               PIC X.                       EL309
00293          10  FILLER                  PIC X(235).                  EL309
00294                                                                   EL309
00295      05  WS-ACTIVITY-TRAILER.                                     EL309
00296          10  WS-VALID-ID             PIC XX.                      EL309
00297          10  WS-CONTROL-PRIMARY.                                  EL309
00298              20  WS-COMP-CD              PIC X.                   EL309
00299              20  WS-CARRIER              PIC X.                   EL309
00300              20  WS-CLAIM-NO             PIC X(7).                EL309
00301              20  WS-CERT-NO              PIC X(11).               EL309
00302              20  WS-SEQUENCE-NO          PIC S9(4)    COMP.       EL309
00303          10  FILLER                      PIC X(176).              EL309
00304                                                                   EL309
00305      05  WS-LETTER-ARCHIVE.                                       EL309
00306          10  WS-LETTER-ID                PIC XX.                  EL309
00307          10  WS-LETTER-CD                PIC X.                   EL309
00308          10  WS-LETTER-ARCHIVE-NO        PIC S9(8)    COMP.       EL309
00309          10  FILLER                      PIC X(83).               EL309
00310                                                                   EL309
00311      05  WS-DISPLAY-TIME             PIC 99B99B99.                EL309
00312                                                                   EL309
00313      05  WS-ACCEPT-TIME.                                          EL309
00314          10  WS-ACCEPT-HHMMSS        PIC 9(06)   VALUE ZEROS.     EL309
00315          10  FILLER                  PIC 9(02)   VALUE ZEROS.     EL309
00316                                                                   EL309
00317      05  X                           PIC X.                       EL309
00318      05  ABEND-CODE                  PIC X(4).                    EL309
00319      05  ABEND-OPTION                PIC X.                       EL309
00320      05  OLC-REPORT-NAME             PIC X(8) VALUE 'EL- 309'.    EL309
00321                                                                   EL309
00322      05  WS-SAVE-PRINT-RECORD        PIC X(133)  VALUE SPACES.    EL309
00323                                                                   EL309
00324      05  WS-LAST-COMPANY-CD          PIC X VALUE LOW-VALUES.      EL309
00325      05  WS-LAST-CARRIER             PIC X VALUE LOW-VALUES.      EL309
00326      05  WS-PURGE-CLAIM-SW           PIC X VALUE SPACES.          EL309
00327                                                                   EL309
00328      05  WS-CARRIER-CONTROL          PIC X       VALUE SPACES.    EL309
00329        88  EQUAL-CONDITION                       VALUE '1'.       EL309
00330                                                                   EL309
00331      05  WS-MATCH-FOUND-SW           PIC X       VALUE SPACES.    EL309
00332        88  MATCH-FOUND                           VALUE '1'.       EL309
00333                                                                   EL309
00334      05  WS-PURGE-SW                 PIC X       VALUE SPACES.    EL309
00335        88  PURGED                                VALUE '1'.       EL309
00336                                                                   EL309
00337      05  WS-FOUND-RETRIEVE-SW        PIC X       VALUE SPACES.    EL309
00338        88  RETRIEVE-NOT-RESEARCHED               VALUE ' '.       EL309
00339        88  RETRIEVE-FOUND                        VALUE '1'.       EL309
00340        88  RETRIEVE-FOUND-AND-WAS-PURGED         VALUE '2'.       EL309
00341        88  RETRIEVE-RECORD-NOT-FOUND             VALUE '3'.       EL309
00342                                                                   EL309
00343      05  WS-FOUND-LETTER-SW          PIC X       VALUE SPACES.    EL309
00344        88  LETTER-NOT-FOUND                      VALUE SPACES.    EL309
00345        88  LETTER-FOUND                          VALUE '1'.       EL309
00346                                                                   EL309
00347      05  WS-FOUND-EQUAL-SW           PIC X       VALUE SPACES.    EL309
00348        88  EQUAL-KEYS                            VALUE '1'.       EL309
00349        88  NEW-RECORD                            VALUE SPACES.    EL309
00350                                                                   EL309
00351      05  WS-DISPLAY-AMOUNT           PIC Z,ZZZ,ZZ9.99-.           EL309
00352      05  WS-DISPLAY-COUNT            PIC Z,ZZZ,ZZ9-.              EL309
00353                                                                   EL309
00354      05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    EL309
00355                                                                   EL309
00356      05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      EL309
00357                                                                   EL309
00358      05  ELCNTL-FILE-STATUS          PIC XX      VALUE ZERO.      EL309
00359      05  ELCERT-FILE-STATUS          PIC XX      VALUE ZERO.      EL309
00360      05  MPPLCY-FILE-STATUS          PIC XX      VALUE ZERO.      EL309
00361      05  ELMSTR-FILE-STATUS          PIC XX      VALUE ZERO.      EL309
00362      05  ELTRLR-FILE-STATUS          PIC XX      VALUE ZERO.      EL309
00363      05  ELACCT-FILE-STATUS          PIC XX      VALUE ZERO.      EL309
00364      05  ELCHKQ-FILE-STATUS          PIC XX      VALUE ZERO.      EL309
00365      05  ELPGMS-FILE-STATUS          PIC XX      VALUE ZERO.      EL309
00366      05  ELARCH-FILE-STATUS          PIC XX      VALUE ZERO.      EL309
00367      05  ELACTQ-FILE-STATUS          PIC XX      VALUE ZERO.      EL309
00368      05  ELRETR-FILE-STATUS          PIC XX      VALUE ZERO.      EL309
00369                                                                   EL309
00370      05  WS-FILE-ERROR-MESSAGE.                                   EL309
00371          10  FILLER                  PIC X(24)   VALUE            EL309
00372              'ERROR OCCURED OPENING - '.                          EL309
00373          10  WS-FEM-FILE-NAME        PIC X(8).                    EL309
00374                                                                   EL309
00375      05  WS-CURRENT-DATE             PIC XX  VALUE LOW-VALUES.    EL309
00376      05  WS-RUN-DATE                 PIC XX  VALUE LOW-VALUES.    EL309
00377                                                                   EL309
00378      05  WS-COMPANY-ID.                                           EL309
00379          10 WS-FIRST-TWO             PIC X(2).                    EL309
00380          10 WS-COMPANY-INCREMENT     PIC X.                       EL309
00381      05  WS-COMPANY-CD               PIC X.                       EL309
00382                                                                   EL309
00383      05  WS-PROGRAM-OPTIONS.                                      EL309
00384          10  WS-FREQUENCY            PIC X(4).                    EL309
00385          10  WS-PRINT-OPTION         PIC X.                       EL309
00386          10  WS-FORMAT-OPTION        PIC X.                       EL309
00387          10  WS-PROCESS-OPTION       PIC X.                       EL309
00388          10  WS-TOTAL-OPTION         PIC X.                       EL309
00389                                                                   EL309
00390      05  WS-COMPANY-NAME.                                         EL309
00391          10  WS-CN-CHAR              PIC X                        EL309
00392              OCCURS 30 TIMES         INDEXED BY CN1.              EL309
00393                                                                   EL309
00394      05  WS-COMPANY-NAME2.                                        EL309
00395          10  WS-CN2-CHAR             PIC X                        EL309
00396              OCCURS 30 TIMES         INDEXED BY CN2.              EL309
00397                                                                   EL309
00398      05  WS-AMOUNT                   PIC ZZZ,ZZZ,ZZ9.99-.         EL309
00399      05  WS-COUNT                    PIC ZZZ,ZZZ,ZZ9-.            EL309
00400                                                                   EL309
00401      05  WS-INITIALS.                                             EL309
00402          10  WS-INITIAL1             PIC X.                       EL309
00403          10  WS-INITIAL2             PIC X.                       EL309
00404                                                                   EL309
00405      EJECT                                                        EL309
00406  01  WS-HEADING1.                                                 EL309
00407      05  FILLER                      PIC X(49)   VALUE '1'.       EL309
00408      05  WS-H1-TITLE                 PIC X(71)   VALUE            EL309
00409          '  MONTHLY CLAIM ARCHIVE & MERGE'.                       EL309
00410      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL309'.      EL309
00411                                                                   EL309
00412  01  WS-HEADING2.                                                 EL309
00413      05  FILLER                      PIC X       VALUE SPACES.    EL309
00414      05  WS-H2-COMPANY-ID            PIC XXX     VALUE 'XXX'.     EL309
00415      05  FILLER                      PIC X(3)    VALUE ' - '.     EL309
00416      05  WS-H2-COMPANY-NAME          PIC X(30)   VALUE SPACES.    EL309
00417      05  FILLER                      PIC X(16)   VALUE SPACES.    EL309
00418      05  WS-D2-TITLE                 PIC X(64)   VALUE SPACES.    EL309
00419      05  FILLER                      PIC X(03)   VALUE SPACES.    EL309
00420      05  FILLER                      PIC X(5)    VALUE 'PAGE'.    EL309
00421      05  WS-H2-PAGE                  PIC ZZ,ZZ9.                  EL309
00422      05  FILLER                      PIC X(02)   VALUE SPACES.    EL309
00423                                                                   EL309
00424  01  WS-HEADING3.                                                 EL309
00425      05  FILLER                      PIC X(56)   VALUE SPACES.    EL309
00426      05  WS-H3-DATE                  PIC X(20)   VALUE SPACES.    EL309
00427      05  FILLER                      PIC X(57)   VALUE SPACES.    EL309
00428                                                                   EL309
00429  01  WS-HEADING4.                                                 EL309
00430      05  FILLER                      PIC X(133)  VALUE            EL309
00431          '0          CLAIM      CERTIFICATE   EFFECTIVE'.         EL309
00432                                                                   EL309
00433  01  WS-HEADING5.                                                 EL309
00434      05  FILLER                      PIC X(72)   VALUE            EL309
00435          ' CARRIER   NUMBER           NUMBER       DATE      INSUREL309
00436 -        'RED'.                                                   EL309
00437      05  FILLER                      PIC X(61)   VALUE            EL309
00438          'MESSAGE'.                                               EL309
00439                                                                   EL309
00440      EJECT                                                        EL309
00441  01  WS-DETAIL1.                                                  EL309
00442      05  FILLER                      PIC X(4).                    EL309
00443      05  WS-D1-CARRIER               PIC X.                       EL309
00444      05  FILLER                      PIC X(6).                    EL309
00445      05  WS-D1-CLAIM-NO              PIC X(7).                    EL309
00446      05  FILLER                      PIC X(4).                    EL309
00447      05  WS-D1-CERT-NO               PIC X(11).                   EL309
00448      05  FILLER                      PIC X(3).                    EL309
00449      05  WS-D1-CERT-EFF-DT           PIC X(8).                    EL309
00450      05  FILLER                      PIC X(3).                    EL309
00451      05  WS-D1-INSURED-NAME          PIC X(20).                   EL309
00452      05  FILLER                      PIC X(4).                    EL309
00453      05  WS-D1-MESSAGE               PIC X(60).                   EL309
00454                                                                   EL309
00455      05  FILLER                      REDEFINES                    EL309
00456          WS-D1-MESSAGE.                                           EL309
00457          10  FILLER                  PIC X(35).                   EL309
00458          10  WS-D1-BIRTH-DATE        PIC X(8).                    EL309
00459          10  FILLER                  PIC X(17).                   EL309
00460                                                                   EL309
00461      05  FILLER                      REDEFINES                    EL309
00462          WS-D1-MESSAGE.                                           EL309
00463          10  FILLER                  PIC X(23).                   EL309
00464          10  WS-D1-TRAILER-SEQ       PIC 9(4).                    EL309
00465          10  FILLER                  PIC X(6).                    EL309
00466          10  WS-D1-TRAILER-TYPE      PIC X.                       EL309
00467          10  FILLER                  PIC X(26).                   EL309
00468                                                                   EL309
00469      05  FILLER                      REDEFINES                    EL309
00470          WS-D1-MESSAGE.                                           EL309
00471          10  FILLER                  PIC X(22).                   EL309
00472          10  WS-D1-MSG               PIC X(20).                   EL309
00473          10  FILLER                  PIC X(18).                   EL309
00474                                                                   EL309
00475      05  FILLER                      PIC X(2).                    EL309
00476                                                                   EL309
00477  01  WS-DETAIL2                      REDEFINES                    EL309
00478      WS-DETAIL1.                                                  EL309
00479      05  FILLER                      PIC X.                       EL309
00480      05  WS-D2-DESC                  PIC X(30).                   EL309
00481      05  WS-D2-COUNT                 PIC ZZZ,ZZZ,ZZ9-.            EL309
00482      05  FILLER                      PIC X(88).                   EL309
00483                                                                   EL309
00484  01  WS-DETAIL3.                                                  EL309
00485      05  FILLER                      PIC X           VALUE ' '.   EL309
00486      05  WS-D3-DESC1                 PIC X(12)       VALUE        EL309
00487                               'PROCESSED - '.                     EL309
00488      05  WS-D3-COUNT1                PIC  ZZZ,ZZZ,ZZ9-.           EL309
00489      05  FILLER                      PIC X(2).                    EL309
00490      05  WS-D3-DESC2                 PIC X(7)        VALUE        EL309
00491                               'OPEN - '.                          EL309
00492      05  WS-D3-COUNT2                PIC ZZZ,ZZZ,ZZ9-.            EL309
00493      05  FILLER                      PIC X(2).                    EL309
00494                                                                   EL309
00495      05  WS-D3-DESC3                 PIC X(9)        VALUE        EL309
00496                               'CLOSED - '.                        EL309
00497      05  WS-D3-COUNT3                PIC ZZZ,ZZZ,ZZ9-.            EL309
00498      05  FILLER                      PIC X(2).                    EL309
00499      05  WS-D3-DESC4                 PIC X(9)        VALUE        EL309
00500                               'PURGED - '.                        EL309
00501      05  WS-D3-COUNT4                PIC ZZZ,ZZZ,ZZ9-.            EL309
00502                                                                   EL309
00503  01  WS-DETAIL4.                                                  EL309
00504      05  FILLER                      PIC X           VALUE ' '.   EL309
00505      05  WS-D4-DESC1                 PIC X(12)       VALUE        EL309
00506                               'PROCESSED - '.                     EL309
00507      05  WS-D4-COUNT                 PIC  ZZZ,ZZZ,ZZ9-.           EL309
00508                                                                   EL309
00509  01  WS-DETAIL5.                                                  EL309
00510      05  FILLER                      PIC X        VALUE SPACE.    EL309
00511      05  WS-D5-DESC1                 PIC X(30)    VALUE SPACES.   EL309
00512      05  WS-D5-COUNT1                PIC  ZZZ,ZZZ,ZZ9-.           EL309
00513                                                                   EL309
00514  01  WS-HEADER.                                                   EL309
00515      05  FILLER                      PIC X(37)    VALUE SPACE.    EL309
00516      05  WS-HEAD-DESC                PIC X(25)    VALUE SPACES.   EL309
00517                                                                   EL309
00518                                  COPY ELCDATE.                    EL309
00519                                                                   EL309
00520                                  COPY ELCNWA.                     EL309
00521                                                                   EL309
00522      EJECT                                                        EL309
00523  PROCEDURE DIVISION.                                              EL309
00524                                                                   EL309
00525  0000-MAIN-LOGIC SECTION.                                         EL309
00526                                                                   EL309
pemuni     open output history-output-file
00527      PERFORM OPEN-FILES.                                          EL309
00528                                                                   EL309
00529      PERFORM 1000-PROCESS-HISTORY-FILE.                           EL309
00530                                                                   EL309
00531      PERFORM CLOSE-FILES                                          EL309
00532                                                                   EL309
00533      GOBACK.                                                      EL309
00534                                                                   EL309
00535  1000-PROCESS-HISTORY-FILE SECTION.                               EL309
00536                                                                   EL309
00537 *    NOTE ******************************************************* EL309
00538 *         *                                                     * EL309
00539 *         *      THE PROCESSING LOGIC OF THIS PROGRAM IS AS     * EL309
00540 *         *  FOLLOWS:                                           * EL309
00541 *         *                                                     * EL309
00542 *         *      1.  READ THE CONTROL FILE SEQUENTIALLY         * EL309
00543 *         *          PROCESSING EACH COMPANY RECORD.            * EL309
00544 *         *                                                     * EL309
00545 *         *      2.  SET THE OPTIONS FOR THIS COMPANY           * EL309
00546 *         *                                                     * EL309
00547 *         *      3.  PROCESS ALL OF THE CLAIMS FOR THIS         * EL309
00548 *         *          COMPANY                                    * EL309
00549 *         *                                                     * EL309
00550 *         *          A.  READ THE CLAIM RECORD                  * EL309
00551 *         *          B.  READ THE CERTIFICATE                   * EL309
00552 *         *          C.  READ THE ACCOUNT                       * EL309
00553 *         *          D.  READ ALL THE ACTIVITY TRAILERS FOR     * EL309
00554 *         *              THIS CLAIM                             * EL309
00555 *         *                                                     * EL309
00556 *         *      4.  GO TO STEP 1.                              * EL309
00557 *         *******************************************************.EL309
00558                                                                   EL309
00559 ***************************************************************** EL309
00560 *   READS THE CONTROL FILE SEQUENTIALLY PROCESSING EACH         * EL309
00561 *   COMPANY.                                                    * EL309
00562 ***************************************************************** EL309
00563      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          EL309
00564                                                                   EL309
00565      START ELCNTL                                                 EL309
00566          KEY IS GREATER THAN CF-CONTROL-PRIMARY                   EL309
00567                                                                   EL309
00568      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL309
00569          MOVE 'ERROR OCCURED START INITIAL - ELCNTL'              EL309
00570                                  TO  WS-ABEND-MESSAGE             EL309
00571          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
00572          PERFORM ABEND-PGM.                                       EL309
00573                                                                   EL309
00574      EJECT                                                        EL309
00575  1010-READ-CONTROL-FILE.                                          EL309
00576      READ ELCNTL NEXT                                             EL309
00577                                                                   EL309
00578      IF ELCNTL-FILE-STATUS = '10'                                 EL309
00579          GO TO 1990-EXIT-TO-TOTALS.                               EL309
00580                                                                   EL309
00581      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL309
00582          MOVE 'ERROR OCCURED READNEXT - ELCNTL'                   EL309
00583                                  TO  WS-ABEND-MESSAGE             EL309
00584          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
00585          PERFORM ABEND-PGM.                                       EL309
00586                                                                   EL309
00587      IF CF-RECORD-TYPE NOT = '1'                                  EL309
00588          GO TO 1010-READ-CONTROL-FILE.                            EL309
00589                                                                   EL309
042312     if cf-company-id not = 'AHL'
042312        go to 1010-read-control-file
042312     end-if

00590 *    IF CF-COMPANY-ID EQUAL 'OFL' OR 'CGL' OR 'FGL'               EL309
00591 *        OR 'OUL' OR 'ADL' OR 'FIA' OR 'ABL' OR 'SEN'             EL309
00592 *        OR 'CAB' OR 'OFI' OR 'OFJ' OR 'IST' OR 'NLI'             EL309
00593 *        OR 'PLI' OR 'TIC' OR 'IFL' OR 'COM' OR 'FND'             EL309
00594 *        OR 'FFL' OR 'SBF' OR 'ZLL' OR 'OFT' OR 'UAL'             EL309
00595 *        OR 'BNE' OR 'GLB' OR 'NCL' OR 'BAL' OR 'WDS'             EL309
00596 *        OR 'CDC' OR 'PEM' OR 'LGX' OR 'SAW' OR 'FRH'             EL309
00597 *        OR 'FRI' OR 'FRN' OR 'FRO' OR 'FRS' OR 'FRT'             EL309
00598 *        OR 'KSA' OR 'POS' OR 'TII' OR 'NCX'                      EL309
00599 ********* ADDED 05/94                                             EL309
00600 *        OR 'AFC' OR 'AFL' OR 'HER' OR 'HSL' OR 'KSM'             EL309
00601 *        OR 'REL' OR 'RIC' OR 'SLI' OR 'SRL' OR 'TIH'             EL309
00602 ********* ADDED 09/95                                             EL309
00603 *        OR 'ACM' OR 'AIG' OR 'BOA' OR 'COL' OR 'CRI'             EL309
00604 *        OR 'FMC' OR 'FNT' OR 'JAN' OR 'JHM' OR 'NGI'             EL309
00605 *        OR 'UAL'                                                 EL309
00606 *        GO TO 1010-READ-CONTROL-FILE.                            EL309
00607                                                                   EL309
00608      MOVE CF-CL-MAIL-TO-NAME     TO  WS-COMPANY-NAME              EL309
00609                                      WS-H2-COMPANY-NAME.          EL309
00610      MOVE CF-COMPANY-ID          TO  WS-COMPANY-ID                EL309
00611                                      WS-H2-COMPANY-ID.            EL309
00612      MOVE CF-COMPANY-CD          TO  WS-COMPANY-CD.               EL309
00613                                                                   EL309
00614      ACCEPT WS-ACCEPT-TIME FROM TIME.                             EL309
00615      MOVE WS-ACCEPT-HHMMSS       TO  WS-DISPLAY-TIME.             EL309
00616      INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'.            EL309
00617      DISPLAY 'EL309-BEGIN PROCESSING ' WS-H2-COMPANY-NAME ' AT '  EL309
00618              WS-DISPLAY-TIME UPON CONSOLE.                        EL309
00619                                                                   EL309
00620      MOVE SPACES                 TO  WS-COMPANY-NAME2.            EL309
00621      SET CN1                     TO +30.                          EL309
00622                                                                   EL309
00623      PERFORM 5200-MOVE-NAME      THRU 5300-EXIT.                  EL309
00624      EJECT                                                        EL309
00625 *NOTE *********************************************************** EL309
00626 *   READS THE PROGRAMS FILE TO SEE IF THE COMPANY HAS           * EL309
00627 *   REQUESTED EL309 TO RUN.  IF THE PROGRAM OPTION              * EL309
00628 *   INDICATES THAT THE COMPANY RECORDS ARE NOT TO BE            * EL309
00629 *   PROCESSED,  THE WS-COMPANY-ID IS ADJUSTED TO                * EL309
00630 *   ENABLE THE RECORDS ALREADY ON THE CLAIMS HISTORY            * EL309
00631 *   FILE FOR THAT COMPANY TO BE REWRITTEN TO THE CLAIMS         * EL309
00632 *   HISTORY FILE.  A NOTE IS ALSO PRINTED ON THE FINAL          * EL309
00633 *   REPORT INDICATING THAT THE OPTION IS NOT TURNED ON          * EL309
00634 *   AND ARCHIVED RECORDS WERE NOT UPDATED FOR                   * EL309
00635 *   THE COMPANY.                                                * EL309
00636 ***************************************************************** EL309
00637                                                                   EL309
00638  1040-GET-PGM-OPTIONS.                                            EL309
00639                                                                   EL309
00640      MOVE WS-COMPANY-CD          TO  PS-CONTROL-PRIMARY.          EL309
00641      MOVE 'EL309'                TO  PS-PROGRAM-NUMBER.           EL309
00642                                                                   EL309
00643      READ ELPGMS.                                                 EL309
00644                                                                   EL309
00645      IF ELPGMS-FILE-STATUS = '23'                                 EL309
00646          GO TO 1900-EXIT-TO-COMPANY-TOTAL.                        EL309
00647                                                                   EL309
00648      IF ELPGMS-FILE-STATUS NOT = ZERO                             EL309
00649          MOVE 'ERROR OCCURED READ - ELPGMS'                       EL309
00650                                  TO  WS-ABEND-MESSAGE             EL309
00651          MOVE ELPGMS-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
00652          PERFORM ABEND-PGM.                                       EL309
00653                                                                   EL309
00654      MOVE PS-PROGRAM-OPTIONS (1) TO  WS-PROGRAM-OPTIONS.          EL309
00655                                                                   EL309
00656      IF PS-FREQUENCY-CODE (1) NOT = 'NONE'                        EL309
00657          MOVE '9' TO WS-COMPANY-INCREMENT                         EL309
00658          GO TO 1900-EXIT-TO-COMPANY-TOTAL.                        EL309
00659                                                                   EL309
00660      EJECT                                                        EL309
00661                                                                   EL309
00662  1100-POSITION-CLAIM-MSTR.                                        EL309
00663                                                                   EL309
00664 *    NOTE ******************************************************* EL309
00665 *         *      POSITION THE CLAIM MASTER AT THE BEGINNING     * EL309
00666 *         *  OF THE COMPANY THEN PROCESS ALL OF THE CLAIMS.     * EL309
00667 *         *******************************************************.EL309
00668                                                                   EL309
00669      MOVE WS-COMPANY-CD          TO  CL-COMPANY-CD.               EL309
00670                                                                   EL309
00671      START ELMSTR                                                 EL309
00672          KEY IS EQUAL TO CL-COMPANY-CD                            EL309
00673                                                                   EL309
00674      IF ELMSTR-FILE-STATUS = '23'                                 EL309
00675          GO TO 1900-EXIT-TO-COMPANY-TOTAL.                        EL309
00676                                                                   EL309
00677      IF ELMSTR-FILE-STATUS NOT = ZERO                             EL309
00678          MOVE 'ERROR OCCURED START - ELMSTR'                      EL309
00679                                  TO  WS-ABEND-MESSAGE             EL309
00680          MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
00681          PERFORM ABEND-PGM.                                       EL309
00682                                                                   EL309
00683      EJECT                                                        EL309
00684  1120-READ-NEXT-CLAIM.                                            EL309
00685      READ ELMSTR NEXT.                                            EL309
00686                                                                   EL309
00687      IF ELMSTR-FILE-STATUS = '10'                                 EL309
00688          GO TO 1900-EXIT-TO-COMPANY-TOTAL.                        EL309
00689                                                                   EL309
00690      IF ELMSTR-FILE-STATUS NOT = ZERO                             EL309
00691          MOVE 'ERROR OCCURED READNEXT - ELMSTR'                   EL309
00692                                  TO  WS-ABEND-MESSAGE             EL309
00693          MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
00694          PERFORM ABEND-PGM.                                       EL309
00695                                                                   EL309
00696      IF CL-COMPANY-CD NOT = WS-COMPANY-CD                         EL309
00697          GO TO 1900-EXIT-TO-COMPANY-TOTAL.                        EL309
00698                                                                   EL309
pemuni     if cl-claim-no = '0234172'
              continue
           end-if

00699      IF CL-CLAIM-STATUS = 'O'                                     EL309
00700          ADD +1  TO  WS-OPEN-CLAIMS                               EL309
00701      ELSE                                                         EL309
00702          IF CL-PURGED-DT NOT EQUAL LOW-VALUES AND SPACES          EL309
00703              ADD +1  TO  WS-PURGED-CLAIMS                         EL309
00704          ELSE                                                     EL309
00705              ADD +1  TO  WS-CLOSED-CLAIMS.                        EL309
00706                                                                   EL309
00707      ADD +1                      TO  WS-CLAIM-COUNT.              EL309
00708                                                                   EL309
00709      DIVIDE WS-CLAIM-COUNT BY +5000                               EL309
00710          GIVING WS-WORK REMAINDER WS-REMAINDER                    EL309
00711                                                                   EL309
00712      IF WS-REMAINDER = ZERO                                       EL309
00713          ACCEPT WS-ACCEPT-TIME FROM TIME                          EL309
00714          MOVE WS-ACCEPT-HHMMSS   TO  WS-DISPLAY-TIME              EL309
00715          INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'         EL309
00716          MOVE WS-CLAIM-COUNT     TO  WS-COUNT                     EL309
00717          DISPLAY 'PROCESSED ' WS-COUNT WS-COMPANY-ID ' CLAIMS '   EL309
00718                                  WS-DISPLAY-TIME UPON CONSOLE.    EL309
00719                                                                   EL309
00720      MOVE '0'                    TO  WS-DETAIL1.                  EL309
00721                                                                   EL309
00722      MOVE CL-CARRIER             TO  WS-D1-CARRIER.               EL309
00723      MOVE CL-CLAIM-NO            TO  WS-D1-CLAIM-NO.              EL309
00724      MOVE CL-CERT-NO             TO  WS-D1-CERT-NO.               EL309
00725                                                                   EL309
00726      MOVE CL-CERT-EFF-DT         TO  DC-BIN-DATE-1.               EL309
00727      MOVE SPACES                 TO  DC-OPTION-CODE.              EL309
00728      PERFORM 8500-DATE-CONVERSION.                                EL309
00729      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-CERT-EFF-DT.           EL309
00730                                                                   EL309
00731      PERFORM 5000-MOVE-NAME.                                      EL309
00732      MOVE WS-NAME-WORK           TO  WS-D1-INSURED-NAME.          EL309
00733                                                                   EL309
00734      MOVE SPACES                 TO WS-FOUND-LETTER-SW            EL309
00735                                     WS-MATCH-FOUND-SW             EL309
00736                                     WS-FOUND-EQUAL-SW.            EL309
00737                                                                   EL309
00738      PERFORM 6000-PROCESS-HISTORY-FILE THRU 6390-EXIT.            EL309
00739                                                                   EL309
00740      IF WS-BYPASS-SW = +1                                         EL309
00741          MOVE ZERO               TO  WS-BYPASS-SW                 EL309
00742          GO TO 1120-READ-NEXT-CLAIM.                              EL309
00743      EJECT                                                        EL309
00744 ***************************************************************** EL309
00745 * IF A NEW CLAIM APPEARS ON THE ON-LINE CLAIMS MASTER, OR IF    * EL309
00746 * EQUAL CLAIMS WERE FOUND AND CHANGES HAD BEEN MADE TO THE      * EL309
00747 * CLAIM, A NEW CLAIMS HISTORY SEQUENCE NEEDS TO BE CREATED.     * EL309
00748 ***************************************************************** EL309
00749                                                                   EL309
00750      MOVE WS-COMPANY-ID          TO  HOR-COMPANY-ID.              EL309
00751      MOVE CL-CONTROL-PRIMARY     TO  HOR-CLAIM-KEY.               EL309
00752      MOVE WS-CURRENT-DATE        TO  HOR-DATE-ARCHIVED.           EL309
00753      MOVE SPACES                 TO  HOR-PURGED-CLAIM.            EL309
00754      MOVE CLAIM-MASTER           TO  HOR-CLAIM-RECORD.            EL309
00755                                                                   EL309
00756      IF NEW-RECORD                                                EL309
00757          PERFORM 6901-COUNT-ADD-RECORD THRU 6901-EXIT.            EL309
00758                                                                   EL309
00759      PERFORM 7400-WRITE-HISTORY  THRU 7400-EXIT.                  EL309
00760                                                                   EL309
00761      IF EQUAL-KEYS                                                EL309
00762          PERFORM 6000-PROCESS-HISTORY-FILE                        EL309
00763                                  THRU 6390-EXIT                   EL309
00764          IF (HIR-RECORD-ID NOT EQUAL 'CM' AND 'PM')               EL309
00765              DISPLAY 'LOGIC FLAW' ' '                             EL309
00766              HIR-RECORD-ID.                                       EL309
00767                                                                   EL309
00768      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                     EL309
00769          GO TO 1125-READ-POLICY-MASTER.                           EL309
00770                                                                   EL309
00771      MOVE WS-COMPANY-CD          TO  CM-COMPANY-CD.               EL309
00772      MOVE CL-CERT-CARRIER        TO  CM-CARRIER.                  EL309
00773      MOVE CL-CERT-GROUPING       TO  CM-GROUPING.                 EL309
00774      MOVE CL-CERT-STATE          TO  CM-STATE.                    EL309
00775      MOVE CL-CERT-ACCOUNT        TO  CM-ACCOUNT.                  EL309
00776      MOVE CL-CERT-EFF-DT         TO  CM-CERT-EFF-DT.              EL309
00777      MOVE CL-CERT-NO             TO  CM-CERT-NO.                  EL309
00778      READ ELCERT.                                                 EL309
00779                                                                   EL309
00780      IF ELCERT-FILE-STATUS = '23'                                 EL309
00781          MOVE 'Y'                TO WS-EXTRACT-ERROR-SW           EL309
00782          MOVE 'CERTIFICATE NOT FOUND'  TO  WS-D1-MESSAGE          EL309
00783          MOVE WS-DETAIL1         TO  PRT                          EL309
00784          PERFORM WRITE-A-LINE                                     EL309
00785          MOVE SPACES             TO  WS-DETAIL1                   EL309
00786        ELSE                                                       EL309
00787      IF ELCERT-FILE-STATUS NOT = ZERO                             EL309
00788          MOVE 'ERROR OCCURED READ - ELCERT'                       EL309
00789                                  TO  WS-ABEND-MESSAGE             EL309
00790          MOVE ELCERT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
00791          PERFORM ABEND-PGM.                                       EL309
00792                                                                   EL309
00793      MOVE WS-COMPANY-ID          TO  HOR-COMPANY-ID.              EL309
00794      MOVE CL-CONTROL-PRIMARY     TO  HOR-CLAIM-KEY.               EL309
00795      MOVE WS-CURRENT-DATE        TO  HOR-DATE-ARCHIVED.           EL309
00796      MOVE SPACES                 TO  HOR-PURGED-CLAIM.            EL309
00797      MOVE CERTIFICATE-MASTER     TO  HOR-CERTIFICATE-RECORD.      EL309
00798                                                                   EL309
00799      GO TO 1130-CONT-READ-NEXT-CLAIM.                             EL309
00800                                                                   EL309
00801  1125-READ-POLICY-MASTER.                                         EL309
00802                                                                   EL309
00803      MOVE WS-COMPANY-CD          TO  PM-COMPANY-CD.               EL309
00804      MOVE CL-CERT-CARRIER        TO  PM-CARRIER.                  EL309
00805      MOVE CL-CERT-GROUPING       TO  PM-GROUPING.                 EL309
00806      MOVE CL-CERT-STATE          TO  PM-STATE.                    EL309
00807      MOVE CL-CERT-ACCOUNT        TO  PM-PRODUCER.                 EL309
00808      MOVE CL-CERT-EFF-DT         TO  PM-POLICY-EFF-DT.            EL309
00809      MOVE CL-CV-REFERENCE-NO     TO  PM-REFERENCE-NUMBER.         EL309
00810                                                                   EL309
00811      READ MPPLCY.                                                 EL309
00812                                                                   EL309
00813      IF MPPLCY-FILE-STATUS IS EQUAL TO '23'                       EL309
00814          MOVE 'Y'                TO  WS-EXTRACT-ERROR-SW          EL309
00815          MOVE 'POLICY NOT FOUND' TO  WS-D1-MESSAGE                EL309
00816          MOVE WS-DETAIL1         TO  PRT                          EL309
00817          PERFORM WRITE-A-LINE                                     EL309
00818          MOVE SPACES             TO  WS-DETAIL1                   EL309
00819      ELSE                                                         EL309
00820          IF MPPLCY-FILE-STATUS IS NOT EQUAL TO '00'               EL309
00821              MOVE 'ERROR OCCURED READ - MPPLCY'                   EL309
00822                                  TO  WS-ABEND-MESSAGE             EL309
00823              MOVE MPPLCY-FILE-STATUS                              EL309
00824                                  TO  WS-ABEND-FILE-STATUS         EL309
00825              PERFORM ABEND-PGM.                                   EL309
00826                                                                   EL309
00827      MOVE WS-COMPANY-ID          TO  HOR-COMPANY-ID.              EL309
00828      MOVE CL-CONTROL-PRIMARY     TO  HOR-CLAIM-KEY.               EL309
00829      MOVE WS-CURRENT-DATE        TO  HOR-DATE-ARCHIVED.           EL309
00830      MOVE SPACES                 TO  HOR-PURGED-CLAIM.            EL309
pemuni*    MOVE POLICY-MASTER          TO  HOR-POLICY-RECORD.           EL309
00832                                                                   EL309
00833  1130-CONT-READ-NEXT-CLAIM.                                       EL309
00834                                                                   EL309
00835      IF NEW-RECORD                                                EL309
00836          PERFORM 6901-COUNT-ADD-RECORD THRU 6901-EXIT.            EL309
00837                                                                   EL309
00838      PERFORM 7400-WRITE-HISTORY  THRU 7400-EXIT.                  EL309
00839                                                                   EL309
00840      PERFORM 6400-READ-WRITE-AT-RECS THRU 6455-EXIT.              EL309
00841                                                                   EL309
00842      IF CL-HISTORY-ARCHIVE-DT = LOW-VALUES OR SPACES OR ZEROS     EL309
00843         MOVE WS-CURRENT-DATE        TO  CL-HISTORY-ARCHIVE-DT     EL309
00844         REWRITE CLAIM-MASTER                                      EL309
00845         IF ELMSTR-FILE-STATUS NOT = ZERO                          EL309
00846            MOVE 'ERROR OCCURED REWRITE - ELMSTR'                  EL309
00847                                     TO WS-ABEND-MESSAGE           EL309
00848            MOVE ELMSTR-FILE-STATUS  TO  WS-ABEND-FILE-STATUS      EL309
00849            PERFORM ABEND-PGM.                                     EL309
00850                                                                   EL309
00851 ***************************************************************** EL309
00852 * READ THROUGH THE CLAIMS HISTORY UNTIL THE NEXT CLAIM IS FOUND * EL309
00853 ***************************************************************** EL309
00854                                                                   EL309
00855      IF EQUAL-KEYS                                                EL309
00856         IF HIR-RECORD-ID NOT EQUAL 'CL'                           EL309
00857             PERFORM 6700-READ-UNTIL-NEXT                          EL309
00858                                     THRU 6700-EXIT.               EL309
00859                                                                   EL309
00860      MOVE SPACES                    TO WS-FOUND-EQUAL-SW.         EL309
00861                                                                   EL309
00862      GO TO 1120-READ-NEXT-CLAIM.                                  EL309
00863                                                                   EL309
00864      EJECT                                                        EL309
00865                                                                   EL309
00866  1900-EXIT-TO-COMPANY-TOTAL.                                      EL309
00867                                                                   EL309
00868 *    NOTE ******************************************************* EL309
00869 *         *                  COMPANY TOTALS                     * EL309
00870 *         *******************************************************.EL309
00871 *NOTE************************************************************ EL309
00872 *   THE COMPANY TOTALS WILL LIST HOW MANY OF EACH TYPE OF       * EL309
00873 *  RECORD WAS INPUT, ADDED, DROPPED, AND WRITTEN.  A CONTROL    * EL309
00874 *  TOTAL IS CALCULATED TO INDICATE AT THE BOTTOM OF THE         * EL309
00875 *  REPORT THAT A LOGIC PROBLEM WITH THE RUN EXISTED.            * EL309
00876 ***************************************************************** EL309
00877                                                                   EL309
00878      MOVE SPACES                 TO  WS-EXTRACT-ERROR-SW.         EL309
00879      MOVE +99                    TO  WS-LINE-COUNT.               EL309
00880      MOVE '   *ON-LINE STATISTICS*  '  TO  WS-HEAD-DESC.          EL309
00881      MOVE WS-HEADER              TO  PRT.                         EL309
00882      PERFORM WRITE-A-LINE.                                        EL309
00883                                                                   EL309
00884      MOVE '0CLAIMS   :               '  TO  WS-DETAIL2.           EL309
00885      MOVE WS-DETAIL2             TO  PRT.                         EL309
00886      PERFORM WRITE-A-LINE.                                        EL309
00887                                                                   EL309
00888      MOVE WS-CLAIM-COUNT         TO  WS-D3-COUNT1.                EL309
00889      MOVE WS-OPEN-CLAIMS         TO  WS-D3-COUNT2.                EL309
00890      MOVE WS-CLOSED-CLAIMS       TO  WS-D3-COUNT3.                EL309
00891      MOVE WS-PURGED-CLAIMS       TO  WS-D3-COUNT4.                EL309
00892      MOVE WS-DETAIL3             TO  PRT.                         EL309
00893      PERFORM WRITE-A-LINE.                                        EL309
00894                                                                   EL309
00895      MOVE '0TRAILERS :               '  TO  WS-DETAIL2.           EL309
00896      MOVE WS-DETAIL2             TO  PRT.                         EL309
00897      PERFORM WRITE-A-LINE.                                        EL309
00898                                                                   EL309
00899      MOVE WS-TRAILER-COUNT       TO  WS-D4-COUNT.                 EL309
00900      MOVE WS-DETAIL4             TO  PRT.                         EL309
00901      PERFORM WRITE-A-LINE.                                        EL309
00902                                                                   EL309
00903      ADD WS-CLAIM-COUNT          TO  WS-TOTAL-CLAIM-COUNT.        EL309
00904      ADD WS-TRAILER-COUNT        TO  WS-TOTAL-TRAILER-COUNT.      EL309
00905                                                                   EL309
00906      MOVE ZERO                   TO  WS-CLAIM-COUNT               EL309
00907                                      WS-TRAILER-COUNT.            EL309
00908                                                                   EL309
00909      MOVE HIGH-VALUES            TO  CL-CONTROL-PRIMARY.          EL309
00910                                                                   EL309
00911      PERFORM 6000-PROCESS-HISTORY-FILE  THRU 6390-EXIT.           EL309
00912      EJECT                                                        EL309
00913 *NOTE************************************************************ EL309
00914 *   THE FOLLOWING ROUTINE IS FOR REPORTING ONLY.  THE HISTORY   * EL309
00915 *  ARCHIVE FILE HAS BEEN READ AND TOTALED ONE CLAIM BEYOND THE  * EL309
00916 *  COMPANY BEING REPORTED.  SINCE THE TOTALS ON THE REPORT      * EL309
00917 *  ARE SO DETAILED AND CRITICAL, THE FOLLOWING MUST BE          * EL309
00918 *  PROCESSED.                                                   * EL309
00919 ***************************************************************** EL309
00920                                                                   EL309
00921                                                                   EL309
00922      IF WS-NEXT-CL-HISTORY GREATER THAN ZERO                      EL309
00923         ADD +1                   TO WS-HISTORY-INPUT-COUNT        EL309
00924                                     WS-CL-HISTORY-INPUT           EL309
00925         ADD WS-NEXT-PURGED-IPT   TO WS-PURGED-CLAIMS-IPT          EL309
00926         ADD WS-NEXT-CLOSED-IPT   TO WS-CLOSED-CLAIMS-IPT          EL309
00927         ADD WS-NEXT-OPEN-IPT     TO WS-OPEN-CLAIMS-IPT.           EL309
00928                                                                   EL309
00929         MOVE ZERO                TO WS-NEXT-CL-HISTORY            EL309
00930                                     WS-NEXT-PURGED-IPT            EL309
00931                                     WS-NEXT-CLOSED-IPT            EL309
00932                                     WS-NEXT-OPEN-IPT.             EL309
00933                                                                   EL309
00934      COMPUTE WS-AT-HISTORY-ADDED =                                EL309
00935          WS-AT-HISTORY-OUTPUT - WS-AT-HISTORY-INPUT +             EL309
00936          WS-AT-HISTORY-DROPPED.                                   EL309
00937                                                                   EL309
00938      COMPUTE WS-LA-HISTORY-ADDED =                                EL309
00939          WS-LA-HISTORY-OUTPUT - WS-LA-HISTORY-INPUT +             EL309
00940          WS-LA-HISTORY-DROPPED.                                   EL309
00941                                                                   EL309
00942      COMPUTE WS-CONTROL-TOTAL    =                                EL309
00943          WS-CL-HISTORY-INPUT     -                                EL309
00944          WS-CL-HISTORY-DROPPED   +                                EL309
00945          WS-CL-HISTORY-ADDED     +                                EL309
00946          WS-CM-HISTORY-INPUT     -                                EL309
00947          WS-CM-HISTORY-DROPPED   +                                EL309
00948          WS-CM-HISTORY-ADDED     +                                EL309
00949          WS-PM-HISTORY-INPUT     -                                EL309
00950          WS-PM-HISTORY-DROPPED   +                                EL309
00951          WS-PM-HISTORY-ADDED     +                                EL309
00952          WS-AT-HISTORY-INPUT     -                                EL309
00953          WS-AT-HISTORY-DROPPED   +                                EL309
00954          WS-AT-HISTORY-ADDED     +                                EL309
00955          WS-LA-HISTORY-INPUT     -                                EL309
00956          WS-LA-HISTORY-DROPPED   +                                EL309
00957          WS-LA-HISTORY-ADDED.                                     EL309
00958                                                                   EL309
00959                                                                   EL309
00960      COMPUTE WS-DIFF             =                                EL309
00961          WS-HISTORY-OUTPUT-COUNT -                                EL309
00962          WS-CONTROL-TOTAL.                                        EL309
00963                                                                   EL309
00964      IF WS-DIFF   EQUAL +1 OR -1                                  EL309
00965          IF WS-DIFF EQUAL -1                                      EL309
00966              ADD  -1             TO WS-HISTORY-INPUT-COUNT        EL309
00967              ADD  -1             TO WS-CL-HISTORY-INPUT           EL309
00968              MOVE +1             TO WS-NEXT-CL-HISTORY            EL309
00969              PERFORM 6904-DECREMENT-TOTALS THRU 6904-EXIT         EL309
00970          ELSE                                                     EL309
00971              ADD  +1             TO WS-HISTORY-INPUT-COUNT        EL309
00972              ADD  +1             TO WS-CL-HISTORY-INPUT           EL309
00973              MOVE -1             TO WS-NEXT-CL-HISTORY            EL309
00974              PERFORM 6904-DECREMENT-TOTALS THRU 6904-EXIT         EL309
00975      ELSE                                                         EL309
00976          MOVE ZERO               TO WS-NEXT-CL-HISTORY.           EL309
00977                                                                   EL309
00978      MOVE '*HISTORY FILE STATISTICS*'  TO  WS-HEAD-DESC.          EL309
00979      MOVE WS-HEADER              TO  PRT.                         EL309
00980      PERFORM WRITE-A-LINE.                                        EL309
00981                                                                   EL309
00982      MOVE '0CLAIMS   - INPUT         '  TO  WS-DETAIL2.           EL309
00983      MOVE WS-DETAIL2             TO  PRT.                         EL309
00984      PERFORM WRITE-A-LINE.                                        EL309
00985                                                                   EL309
00986      MOVE WS-CL-HISTORY-INPUT    TO  WS-D3-COUNT1.                EL309
00987      MOVE WS-OPEN-CLAIMS-IPT     TO  WS-D3-COUNT2.                EL309
00988      MOVE WS-CLOSED-CLAIMS-IPT   TO  WS-D3-COUNT3.                EL309
00989      MOVE WS-PURGED-CLAIMS-IPT   TO  WS-D3-COUNT4.                EL309
00990      MOVE WS-DETAIL3             TO  PRT.                         EL309
00991      PERFORM WRITE-A-LINE.                                        EL309
00992                                                                   EL309
00993      MOVE '0CLAIMS   - OUTPUT        '  TO  WS-DETAIL2.           EL309
00994      MOVE WS-DETAIL2             TO  PRT.                         EL309
00995      PERFORM WRITE-A-LINE.                                        EL309
00996                                                                   EL309
00997      MOVE WS-CL-HISTORY-OUTPUT    TO  WS-D3-COUNT1.               EL309
00998      MOVE WS-OPEN-CLAIMS-OPT     TO  WS-D3-COUNT2.                EL309
00999      MOVE WS-CLOSED-CLAIMS-OPT   TO  WS-D3-COUNT3.                EL309
01000      MOVE WS-PURGED-CLAIMS-OPT   TO  WS-D3-COUNT4.                EL309
01001      MOVE WS-DETAIL3             TO  PRT.                         EL309
01002      PERFORM WRITE-A-LINE.                                        EL309
01003                                                                   EL309
01004      MOVE '0CLAIM ACTIVITY INDICATED '  TO  WS-DETAIL2.           EL309
01005      MOVE WS-DETAIL2             TO  PRT.                         EL309
01006      PERFORM WRITE-A-LINE.                                        EL309
01007                                                                   EL309
01008      MOVE ' DELETED FROM ON-LINE  -'  TO  WS-DETAIL2.             EL309
01009      MOVE WS-DETAIL2             TO  PRT.                         EL309
01010      PERFORM WRITE-A-LINE.                                        EL309
01011                                                                   EL309
01012      MOVE '  CLOSED,PURGED,DELETED-'                              EL309
01013                                  TO  WS-D5-DESC1                  EL309
01014      MOVE WS-CL-VALID-DEL        TO  WS-D5-COUNT1.                EL309
01015      MOVE WS-DETAIL5             TO  PRT.                         EL309
01016      PERFORM WRITE-A-LINE.                                        EL309
01017                                                                   EL309
01018      MOVE '            REATTACHED -'                              EL309
01019                                  TO  WS-D5-DESC1                  EL309
01020      MOVE WS-CL-HISTORY-DROPPED  TO  WS-D5-COUNT1.                EL309
01021      MOVE WS-DETAIL5             TO  PRT.                         EL309
01022      PERFORM WRITE-A-LINE.                                        EL309
01023                                                                   EL309
01024      MOVE 'NEW CLAIMS             -'                              EL309
01025                                  TO  WS-D5-DESC1                  EL309
01026      MOVE WS-CL-HISTORY-ADDED    TO  WS-D5-COUNT1.                EL309
01027      MOVE WS-DETAIL5             TO  PRT.                         EL309
01028      PERFORM WRITE-A-LINE.                                        EL309
01029                                                                   EL309
01030      MOVE 'PURGED CLAIMS          -'                              EL309
01031                                  TO  WS-D5-DESC1                  EL309
01032      MOVE WS-CL-NEW-PURGED       TO  WS-D5-COUNT1.                EL309
01033      MOVE WS-DETAIL5             TO  PRT.                         EL309
01034      PERFORM WRITE-A-LINE.                                        EL309
01035                                                                   EL309
01036      MOVE 'CLAIMS FOUND ON RETRIEVE'                              EL309
01037                                  TO  WS-D5-DESC1                  EL309
01038      MOVE WS-RETRIEVE-CLAIMS-FOUND TO  WS-D5-COUNT1.              EL309
01039      MOVE WS-DETAIL5             TO  PRT.                         EL309
01040      PERFORM WRITE-A-LINE.                                        EL309
01041                                                                   EL309
01042      MOVE '0CLAIMS INPUT          -'  TO  WS-DETAIL2.             EL309
01043      MOVE WS-CL-HISTORY-INPUT    TO  WS-D2-COUNT.                 EL309
01044      MOVE WS-DETAIL2             TO  PRT.                         EL309
01045      PERFORM WRITE-A-LINE.                                        EL309
01046                                                                   EL309
01047      MOVE ' CLAIMS DROPPED        -'  TO  WS-DETAIL2.             EL309
01048      MOVE WS-CL-HISTORY-DROPPED  TO  WS-D2-COUNT.                 EL309
01049      MOVE WS-DETAIL2             TO  PRT.                         EL309
01050      PERFORM WRITE-A-LINE.                                        EL309
01051                                                                   EL309
01052      MOVE ' CLAIMS ADDED          -'  TO  WS-DETAIL2.             EL309
01053      MOVE WS-CL-HISTORY-ADDED    TO  WS-D2-COUNT.                 EL309
01054      MOVE WS-DETAIL2             TO  PRT.                         EL309
01055      PERFORM WRITE-A-LINE.                                        EL309
01056                                                                   EL309
01057      MOVE ' CLAIMS OUTPUT         -'  TO  WS-DETAIL2.             EL309
01058      MOVE WS-CL-HISTORY-OUTPUT   TO  WS-D2-COUNT.                 EL309
01059      MOVE WS-DETAIL2             TO  PRT.                         EL309
01060      PERFORM WRITE-A-LINE.                                        EL309
01061                                                                   EL309
01062      MOVE '0CERTS INPUT           -'  TO  WS-DETAIL2.             EL309
01063      MOVE WS-CM-HISTORY-INPUT TO WS-D2-COUNT.                     EL309
01064      MOVE WS-DETAIL2             TO  PRT.                         EL309
01065      PERFORM WRITE-A-LINE.                                        EL309
01066                                                                   EL309
01067      MOVE ' CERTS DROPPED         -'  TO  WS-DETAIL2.             EL309
01068      MOVE WS-CM-HISTORY-DROPPED  TO  WS-D2-COUNT.                 EL309
01069      MOVE WS-DETAIL2             TO  PRT.                         EL309
01070      PERFORM WRITE-A-LINE.                                        EL309
01071                                                                   EL309
01072      MOVE ' CERTS ADDED           -'  TO  WS-DETAIL2.             EL309
01073      MOVE WS-CM-HISTORY-ADDED    TO  WS-D2-COUNT.                 EL309
01074      MOVE WS-DETAIL2             TO  PRT.                         EL309
01075      PERFORM WRITE-A-LINE.                                        EL309
01076                                                                   EL309
01077      MOVE ' CERTS OUTPUT          -'  TO  WS-DETAIL2.             EL309
01078      MOVE WS-CM-HISTORY-OUTPUT   TO  WS-D2-COUNT.                 EL309
01079      MOVE WS-DETAIL2             TO  PRT.                         EL309
01080      PERFORM WRITE-A-LINE.                                        EL309
01081                                                                   EL309
01082      MOVE '0POLICIES INPUT        -'  TO  WS-DETAIL2.             EL309
01083      MOVE WS-PM-HISTORY-INPUT TO WS-D2-COUNT.                     EL309
01084      MOVE WS-DETAIL2             TO  PRT.                         EL309
01085      PERFORM WRITE-A-LINE.                                        EL309
01086                                                                   EL309
01087      MOVE ' POLICIES DROPPED      -'  TO  WS-DETAIL2.             EL309
01088      MOVE WS-PM-HISTORY-DROPPED  TO  WS-D2-COUNT.                 EL309
01089      MOVE WS-DETAIL2             TO  PRT.                         EL309
01090      PERFORM WRITE-A-LINE.                                        EL309
01091                                                                   EL309
01092      MOVE ' POLICIES ADDED        -'  TO  WS-DETAIL2.             EL309
01093      MOVE WS-PM-HISTORY-ADDED    TO  WS-D2-COUNT.                 EL309
01094      MOVE WS-DETAIL2             TO  PRT.                         EL309
01095      PERFORM WRITE-A-LINE.                                        EL309
01096                                                                   EL309
01097      MOVE ' POLICIES OUTPUT       -'  TO  WS-DETAIL2.             EL309
01098      MOVE WS-PM-HISTORY-OUTPUT   TO  WS-D2-COUNT.                 EL309
01099      MOVE WS-DETAIL2             TO  PRT.                         EL309
01100      PERFORM WRITE-A-LINE.                                        EL309
01101                                                                   EL309
01102      MOVE '0TRAILERS INPUT        -'  TO  WS-DETAIL2.             EL309
01103      MOVE WS-AT-HISTORY-INPUT TO WS-D2-COUNT.                     EL309
01104      MOVE WS-DETAIL2             TO  PRT.                         EL309
01105      PERFORM WRITE-A-LINE.                                        EL309
01106                                                                   EL309
01107      MOVE ' TRAILERS DROPPED      -'  TO  WS-DETAIL2.             EL309
01108      MOVE WS-AT-HISTORY-DROPPED  TO  WS-D2-COUNT.                 EL309
01109      MOVE WS-DETAIL2             TO  PRT.                         EL309
01110      PERFORM WRITE-A-LINE.                                        EL309
01111                                                                   EL309
01112      MOVE ' TRAILERS ADDED        -'  TO  WS-DETAIL2.             EL309
01113      MOVE WS-AT-HISTORY-ADDED    TO  WS-D2-COUNT.                 EL309
01114      MOVE WS-DETAIL2             TO  PRT.                         EL309
01115      PERFORM WRITE-A-LINE.                                        EL309
01116                                                                   EL309
01117      MOVE ' TRAILERS OUTPUT       -'  TO  WS-DETAIL2.             EL309
01118      MOVE WS-AT-HISTORY-OUTPUT   TO  WS-D2-COUNT.                 EL309
01119      MOVE WS-DETAIL2             TO  PRT.                         EL309
01120      PERFORM WRITE-A-LINE.                                        EL309
01121                                                                   EL309
01122      MOVE '0LETTERS INPUT         -'  TO  WS-DETAIL2.             EL309
01123      MOVE WS-LA-HISTORY-INPUT TO WS-D2-COUNT.                     EL309
01124      MOVE WS-DETAIL2             TO  PRT.                         EL309
01125      PERFORM WRITE-A-LINE.                                        EL309
01126                                                                   EL309
01127      MOVE ' LETTERS DROPPED       -'  TO  WS-DETAIL2.             EL309
01128      MOVE WS-LA-HISTORY-DROPPED  TO  WS-D2-COUNT.                 EL309
01129      MOVE WS-DETAIL2             TO  PRT.                         EL309
01130      PERFORM WRITE-A-LINE.                                        EL309
01131                                                                   EL309
01132      MOVE ' LETTERS  ADDED        -'  TO  WS-DETAIL2.             EL309
01133      MOVE WS-LA-HISTORY-ADDED    TO  WS-D2-COUNT.                 EL309
01134      MOVE WS-DETAIL2             TO  PRT.                         EL309
01135      PERFORM WRITE-A-LINE.                                        EL309
01136                                                                   EL309
01137      MOVE ' LETTERS OUTPUT        -'  TO  WS-DETAIL2.             EL309
01138      MOVE WS-LA-HISTORY-OUTPUT   TO  WS-D2-COUNT.                 EL309
01139      MOVE WS-DETAIL2             TO  PRT.                         EL309
01140      PERFORM WRITE-A-LINE.                                        EL309
01141                                                                   EL309
01142      MOVE '0HISTORY RECORDS INPUT -'  TO  WS-DETAIL2.             EL309
01143                                                                   EL309
01144      MOVE WS-HISTORY-INPUT-COUNT TO  WS-D2-COUNT.                 EL309
01145      MOVE WS-DETAIL2             TO  PRT.                         EL309
01146      PERFORM WRITE-A-LINE.                                        EL309
01147                                                                   EL309
01148      MOVE ' HISTORY RECORDS OUTPUT-'  TO  WS-DETAIL2.             EL309
01149      MOVE WS-HISTORY-OUTPUT-COUNT TO  WS-D2-COUNT.                EL309
01150      MOVE WS-DETAIL2             TO  PRT.                         EL309
01151      PERFORM WRITE-A-LINE.                                        EL309
01152                                                                   EL309
01153      IF WS-DIFF GREATER THAN +1                                   EL309
01154          MOVE '0TOTALS OUT OF CONTROL -'  TO  WS-DETAIL2          EL309
01155          MOVE WS-DIFF                  TO  WS-D2-COUNT            EL309
01156          MOVE WS-DETAIL2             TO  PRT                      EL309
01157          PERFORM WRITE-A-LINE.                                    EL309
01158                                                                   EL309
01159      IF WS-FREQUENCY NOT EQUAL 'NONE'                             EL309
01160          MOVE '0WARNING*****      '  TO  WS-DETAIL2               EL309
01161          MOVE WS-DETAIL2             TO  PRT                      EL309
01162          PERFORM WRITE-A-LINE                                     EL309
01163          MOVE ' OPTION FOR EL-309 NOT SET' TO WS-DETAIL2          EL309
01164          MOVE WS-DETAIL2             TO  PRT                      EL309
01165          PERFORM WRITE-A-LINE.                                    EL309
01166                                                                   EL309
01167      ADD WS-HISTORY-INPUT-COUNT  TO  WS-TOTAL-HISTORY-INPUT.      EL309
01168      ADD WS-HISTORY-OUTPUT-COUNT TO  WS-TOTAL-HISTORY-OUTPUT.     EL309
01169                                                                   EL309
01170      MOVE ZERO                   TO  WS-HISTORY-INPUT-COUNT       EL309
01171                                      WS-HISTORY-OUTPUT-COUNT      EL309
01172                                      WS-DIFF                      EL309
01173                                      WS-CONTROL-TOTAL             EL309
01174                                      WS-CL-HISTORY-INPUT          EL309
01175                                      WS-CL-HISTORY-OUTPUT         EL309
01176                                      WS-CL-HISTORY-ADDED          EL309
01177                                      WS-CL-HISTORY-DROPPED        EL309
01178                                      WS-CM-HISTORY-INPUT          EL309
01179                                      WS-CM-HISTORY-OUTPUT         EL309
01180                                      WS-CM-HISTORY-ADDED          EL309
01181                                      WS-CM-HISTORY-DROPPED        EL309
01182                                      WS-PM-HISTORY-INPUT          EL309
01183                                      WS-PM-HISTORY-OUTPUT         EL309
01184                                      WS-PM-HISTORY-ADDED          EL309
01185                                      WS-PM-HISTORY-DROPPED        EL309
01186                                      WS-AT-HISTORY-INPUT          EL309
01187                                      WS-AT-HISTORY-OUTPUT         EL309
01188                                      WS-AT-HISTORY-DROPPED        EL309
01189                                      WS-AT-HISTORY-ADDED          EL309
01190                                      WS-LA-HISTORY-INPUT          EL309
01191                                      WS-LA-HISTORY-OUTPUT         EL309
01192                                      WS-LA-HISTORY-DROPPED        EL309
01193                                      WS-LA-HISTORY-ADDED          EL309
01194                                      WS-OPEN-CLAIMS-IPT           EL309
01195                                      WS-OPEN-CLAIMS-OPT           EL309
01196                                      WS-PURGED-CLAIMS-IPT         EL309
01197                                      WS-PURGED-CLAIMS-OPT         EL309
01198                                      WS-CLOSED-CLAIMS-IPT         EL309
01199                                      WS-CLOSED-CLAIMS-OPT         EL309
01200                                      WS-CLAIM-COUNT               EL309
01201                                      WS-OPEN-CLAIMS               EL309
01202                                      WS-CLOSED-CLAIMS             EL309
01203                                      WS-PURGED-CLAIMS             EL309
01204                                      WS-RETRIEVE-CLAIMS-FOUND     EL309
01205                                      WS-CL-VALID-DEL              EL309
01206                                      WS-CL-NEW-PURGED.            EL309
01207                                                                   EL309
01208      MOVE +99                    TO  WS-LINE-COUNT.               EL309
01209                                                                   EL309
01210      GO TO 1010-READ-CONTROL-FILE.                                EL309
01211                                                                   EL309
01212      EJECT                                                        EL309
01213  1990-EXIT-TO-TOTALS.                                             EL309
01214 *    NOTE ******************************************************* EL309
01215 *         *                   FINAL TOTALS                      * EL309
01216 *         *******************************************************.EL309
01217                                                                   EL309
01218      ACCEPT WS-ACCEPT-TIME FROM TIME.                             EL309
01219      MOVE WS-ACCEPT-HHMMSS       TO  WS-DISPLAY-TIME.             EL309
01220      INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'.            EL309
01221      DISPLAY 'END OF PROCESSING '                                 EL309
01222              WS-DISPLAY-TIME UPON CONSOLE.                        EL309
01223                                                                   EL309
01224      MOVE +99                    TO  WS-LINE-COUNT.               EL309
01225      MOVE SPACES                 TO  WS-H2-COMPANY-ID.            EL309
01226      MOVE '*** FINAL TOTALS ***' TO  WS-H2-COMPANY-NAME.          EL309
01227                                                                   EL309
01228      MOVE HIGH-VALUES            TO  WS-COMPANY-ID                EL309
01229                                      CL-CONTROL-PRIMARY.          EL309
01230                                                                   EL309
01231      PERFORM 6000-PROCESS-HISTORY-FILE THRU 6390-EXIT.            EL309
01232                                                                   EL309
01233      MOVE '0HISTORY RECORDS INPUT -'  TO  WS-DETAIL2.             EL309
01234      MOVE WS-TOTAL-HISTORY-INPUT TO  WS-D2-COUNT.                 EL309
01235      MOVE WS-DETAIL2             TO  PRT.                         EL309
01236      PERFORM WRITE-A-LINE.                                        EL309
01237                                                                   EL309
01238      MOVE ' HISTORY RECORDS OUTPUT-'  TO  WS-DETAIL2.             EL309
01239      MOVE WS-TOTAL-HISTORY-OUTPUT  TO  WS-D2-COUNT.               EL309
01240      MOVE WS-DETAIL2             TO  PRT.                         EL309
01241      PERFORM WRITE-A-LINE.                                        EL309
01242                                                                   EL309
01243  1999-EXIT-FROM-SECTION.                                          EL309
01244      EXIT.                                                        EL309
01245      EJECT                                                        EL309
01246 ***************************************************************** EL309
01247 *      5 0 0 0  -  INITIALIZE AND FORMAT                        * EL309
01248 ***************************************************************** EL309
01249  5000-MOVE-NAME SECTION.         COPY ELCMNS.                     EL309
01250                                                                   EL309
01251      EJECT                                                        EL309
01252  5200-MOVE-NAME.                                                  EL309
01253      IF WS-CN-CHAR (CN1) = SPACES                                 EL309
01254          IF CN1 GREATER THAN +1                                   EL309
01255              SET CN1             DOWN BY +1                       EL309
01256              GO TO 5200-MOVE-NAME                                 EL309
01257            ELSE                                                   EL309
01258              GO TO 5300-EXIT.                                     EL309
01259                                                                   EL309
01260      SET WS-LENGTH               TO CN1.                          EL309
01261                                                                   EL309
01262      SUBTRACT WS-LENGTH FROM +30 GIVING WS-LENGTH.                EL309
01263      DIVIDE +2 INTO WS-LENGTH ROUNDED.                            EL309
01264                                                                   EL309
01265      IF WS-LENGTH NOT GREATER THAN ZERO                           EL309
01266          GO TO 5300-EXIT.                                         EL309
01267                                                                   EL309
01268      SET CN2                     TO CN1.                          EL309
01269      SET CN2                     UP BY WS-LENGTH.                 EL309
01270                                                                   EL309
01271  5300-MOVE-NAME.                                                  EL309
01272      MOVE WS-CN-CHAR (CN1)       TO WS-CN2-CHAR (CN2).            EL309
01273                                                                   EL309
01274      IF CN1 GREATER THAN +1                                       EL309
01275          SET CN1                                                  EL309
01276              CN2                 DOWN BY +1                       EL309
01277          GO TO 5300-MOVE-NAME.                                    EL309
01278                                                                   EL309
01279      MOVE WS-COMPANY-NAME2       TO  WS-COMPANY-NAME.             EL309
01280                                                                      CL**4
01281  5300-EXIT.                                                       EL309
01282       EXIT.                                                          CL**4
01283      EJECT                                                        EL309
01284                                                                   EL309
01285 ***************************************************************** EL309
01286 *      6 0 0 0  -  READ, COMPARE, AND WRITE HISTORY RECORDS     * EL309
01287 ***************************************************************** EL309
01288 ***************************************************************** EL309
01289 *               PROCESS AND READ HISTORY FILE                   * EL309
01290 *****************************************************************.EL309
01291  6000-PROCESS-HISTORY-FILE SECTION.                               EL309
01292                                                                   EL309
01293      IF END-OF-HISTORY-FILE                                       EL309
01294          GO TO 6390-EXIT.                                         EL309
01295                                                                   EL309
01296  6309-VERIFY-READ.                                                EL309
01297      MOVE ZERO                   TO  WS-BYPASS-SW.                EL309
01298      MOVE SPACES                 TO  WS-PURGE-SW.                 EL309
01299                                                                   EL309
01300 *    NOTE ******************************************************* EL309
01301 *         *      IF OPEN SW = +2 A RECORD HAS ALREADY           * EL309
01302 *         *  BEEN READ.  BYPASS READING A RECORD AND GO TO      * EL309
01303 *         *  THE MATCH ROUTINE                                  * EL309
01304 *         *******************************************************.EL309
01305                                                                   EL309
01306      IF WS-HISTORY-FILE-OPEN = +2                                 EL309
01307          GO TO 6330-COMPARE-CLAIM-HISTORY.                        EL309
01308                                                                   EL309
01309  6320-READ-HISTORY-FILE.                                          EL309
01310      READ HISTORY-INPUT-FILE INTO HISTORY-OUTPUT-RECORD           EL309
01311          AT END                                                   EL309
01312              MOVE +3             TO  WS-HISTORY-FILE-OPEN         EL309
01313              MOVE HIGH-VALUES    TO  HIR-COMPANY-ID               EL309
01314                                      HIR-RECORD-ID                EL309
01315                                      HIR-CLAIM-KEY                EL309
01316              GO TO 6390-EXIT.                                     EL309
01317                                                                   EL309
01318                                                                   EL309
01319      IF HIR-RECORD-ID = 'CL'                                      EL309
01320          IF HIR-CLAIM-KEY = WS-LAST-CL-KEY                        EL309
01321              MOVE +1             TO  WS-DUP-SW                    EL309
01322            ELSE                                                   EL309
01323              MOVE HIR-CLAIM-KEY  TO  WS-LAST-CL-KEY               EL309
01324              MOVE ZERO           TO  WS-DUP-SW.                   EL309
01325                                                                   EL309
01326      IF WS-DUP-SW = +1                                            EL309
01327          GO TO 6320-READ-HISTORY-FILE.                            EL309
01328  EJECT                                                            EL309
01329 *    NOTE ******************************************************* EL309
01330 *         *          SEQUENCE CHECK THE HISTORY FILE            * EL309
01331 *         *******************************************************.EL309
01332                                                                   EL309
01333      IF HIR-COMPANY-ID LESS THAN WS-LAST-COMPANY-ID               EL309
01334        OR                                                         EL309
01335          (HIR-COMPANY-ID = WS-LAST-COMPANY-ID AND                 EL309
01336           HIR-CLAIM-KEY LESS THAN WS-LAST-CLAIM-KEY)              EL309
01337        OR                                                         EL309
01338          ((HIR-RECORD-ID = 'CL' AND                               EL309
01339            WS-LAST-RECORD-ID = ('CL' OR 'CM' OR 'PM'))            EL309
01340         OR                                                        EL309
01341           ((HIR-RECORD-ID = 'CM' OR 'PM') AND                     EL309
01342            WS-LAST-RECORD-ID NOT = 'CL')                          EL309
01343         OR                                                        EL309
01344           (HIR-RECORD-ID = 'AT' AND                               EL309
01345            WS-LAST-RECORD-ID = 'CL'))                             EL309
01346        OR                                                         EL309
01347          (HIR-CLAIM-KEY NOT = WS-LAST-CLAIM-KEY AND               EL309
01348           HIR-RECORD-ID NOT = 'CL')                               EL309
01349              DISPLAY 'OUT OF SEQ' ' ' HIR-CLAIM-KEY               EL309
01350              MOVE 'HISTORY FILE OUT OF SEQUENCE'                  EL309
01351                                  TO  WS-ABEND-MESSAGE             EL309
01352              PERFORM ABEND-PGM.                                   EL309
01353                                                                   EL309
01354      MOVE HIR-COMPANY-ID         TO  WS-LAST-COMPANY-ID.          EL309
01355      MOVE HIR-CLAIM-KEY          TO  WS-LAST-CLAIM-KEY.           EL309
01356      MOVE HIR-RECORD-ID          TO  WS-LAST-RECORD-ID.           EL309
01357                                                                   EL309
01358      ADD +1                      TO  WS-HISTORY-INPUT-COUNT.      EL309
01359                                                                   EL309
01360      IF HIR-RECORD-ID = 'CL'                                      EL309
01361          ADD +1                  TO  WS-CL-HISTORY-INPUT          EL309
01362          PERFORM  6902-COUNT-INPUT THRU 6902-EXIT                 EL309
01363          MOVE ' '                TO WS-FOUND-RETRIEVE-SW          EL309
01364        ELSE                                                       EL309
01365      IF HIR-RECORD-ID = 'CM'                                      EL309
01366          ADD +1                  TO  WS-CM-HISTORY-INPUT          EL309
01367        ELSE                                                       EL309
01368      IF HIR-RECORD-ID = 'PM'                                      EL309
01369          ADD +1                  TO  WS-PM-HISTORY-INPUT          EL309
01370        ELSE                                                       EL309
01371      IF HIR-RECORD-ID = 'AT'                                      EL309
01372          ADD +1                  TO  WS-AT-HISTORY-INPUT          EL309
01373        ELSE                                                       EL309
01374      IF HIR-RECORD-ID = 'LA'                                      EL309
01375          ADD +1                  TO  WS-LA-HISTORY-INPUT          EL309
01376        ELSE                                                       EL309
01377          DISPLAY 'NOT VALID ID ' HIR-COMPANY-ID ' ' HIR-CLAIM-KEY EL309
01378          ADD +1  TO  WS-OTHER-HISTORY-INPUT.                      EL309
01379                                                                   EL309
01380      MOVE +1                     TO  WS-HISTORY-FILE-OPEN.        EL309
01381                                                                   EL309
01382      IF EQUAL-KEYS                                                EL309
01383          GO TO 6390-EXIT.                                         EL309
01384      EJECT                                                        EL309
01385  6330-COMPARE-CLAIM-HISTORY.                                      EL309
01386                                                                   EL309
01387  6330-LESS-THAN.                                                  EL309
01388                                                                   EL309
01389 ***************************************************************** EL309
01390 *  IF THE HIR-COMPANY-ID IS LESS THAN THE WS-COMPANY-ID,        * EL309
01391 *  THE COMPANY IS EITHER NO LONGER IN EXISTENCE OR THE          * EL309
01392 *  WS-COMPANY-ID WAS ADJUSTED (PROCESS OPTION NOT TURNED ON).   * EL309
01393 *  ALL RECORDS EXISTING ON THE CLAIMS HISTORY FILE WILL BE      * EL309
01394 *  REWRITTEN.                                                   * EL309
01395 ***************************************************************** EL309
01396                                                                   EL309
01397      IF HIR-COMPANY-ID LESS THAN WS-COMPANY-ID                    EL309
01398          PERFORM 7400-WRITE-HISTORY THRU 7400-EXIT                EL309
01399          GO TO 6320-READ-HISTORY-FILE.                            EL309
01400                                                                   EL309
01401   EJECT                                                           EL309
01402 ***************************************************************** EL309
01403 *  IF THE HIR-COMPANY-ID IS EQUAL TO  THE WS-COMPANY-ID,        * EL309
01404 *  AND THE INPUT CLAIM-KEY IS LESS THAN THE CURRENT ONLINE      * EL309
01405 *  CLAIM, THREE CONDITIONS COULD EXIST :                        * EL309
01406 *                                                               * EL309
01407 *   -THE RECORD HAS BEEN MOVED TO THE RETRIEVE FILE             * EL309
01408 *                                                               * EL309
01409 *   -THE RECORD HAS GONE THROUGH A NORMAL EXISTENCE AND HAS     * EL309
01410 *    BEEN PURGED FROM THE ONLINE FILE                           * EL309
01411 *                                                               * EL309
01412 *   -THE ORIGINAL CLAIM WAS IN ERROR.  EL131 HAS DELETED IT     * EL309
01413 *    AND REATTACHED IT TO ANOTHER CERTIFICATE.                  * EL309
01414 *                                                               * EL309
01415 *  ONLY THOSE THAT HAVE BEEN SUCCESSFULLY PURGED WILL BE        * EL309
01416 *  REWRITTEN.  THE CLAIMS IN ERROR WILL NOT BE REWRITTEN        * EL309
01417 *  TO THE CLAIMS HISTORY FILE.                                  * EL309
01418 ***************************************************************** EL309
01419                                                                   EL309
01420      IF HIR-COMPANY-ID = WS-COMPANY-ID                            EL309
01421        IF HIR-CLAIM-KEY LESS THAN CL-CONTROL-PRIMARY              EL309
01422             NEXT SENTENCE                                         EL309
01423        ELSE                                                       EL309
01424             GO TO 6330-GREATER-THAN                               EL309
01425      ELSE                                                         EL309
01426        GO TO 6330-GREATER-THAN.                                   EL309
01427                                                                   EL309
01428      IF HIR-PURGED-CLAIM EQUAL 'Y'                                EL309
01429          PERFORM 6905-COUNT-VALID-DEL THRU 6905-EXIT              EL309
01430          PERFORM 7400-WRITE-HISTORY THRU 7400-EXIT                EL309
01431          GO TO 6320-READ-HISTORY-FILE.                            EL309
01432                                                                   EL309
01433      IF HIR-RECORD-ID EQUAL 'CL'                                  EL309
01434          PERFORM 6910-READ-RETRIEVE-FILE THRU                     EL309
01435                           6910-READ-RETRIEVE-FILE-EXIT.           EL309
01436                                                                   EL309
01437      IF RETRIEVE-RECORD-NOT-FOUND                                 EL309
01438          PERFORM 6900-COUNT-RECORDS THRU 6900-EXIT                EL309
01439              GO TO 6320-READ-HISTORY-FILE.                        EL309
01440                                                                   EL309
01441      IF RETRIEVE-FOUND                                            EL309
01442         PERFORM 6905-COUNT-VALID-DEL THRU 6905-EXIT               EL309
01443         PERFORM 7400-WRITE-HISTORY THRU 7400-EXIT                 EL309
01444         GO TO 6320-READ-HISTORY-FILE                              EL309
01445      ELSE                                                         EL309
01446         IF RETRIEVE-FOUND-AND-WAS-PURGED                          EL309
01447            MOVE 'Y'               TO HOR-PURGED-CLAIM             EL309
01448            PERFORM 6905-COUNT-VALID-DEL THRU 6905-EXIT            EL309
01449            PERFORM 7400-WRITE-HISTORY THRU 7400-EXIT              EL309
01450            GO TO 6320-READ-HISTORY-FILE.                          EL309
01451                                                                   EL309
01452  6330-GREATER-THAN.                                               EL309
01453                                                                   EL309
01454 ***************************************************************** EL309
01455 *   IF THE INPUT CLAIM KEY IS GREATER THAN THE CURRENT          * EL309
01456 *   ON-LINE CLAIM-KEY, ALL RECORDS FOR THAT CLAIM HAVE          * EL309
01457 *   BEEN PROCESSED FROM THE CLAIMS HISTORY FILE. A FLAG IS      * EL309
01458 *   TURNED ON TO INDICATE THAT THE CLAIMS HISTORY FILE IS       * EL309
01459 *   NOT TO BE ADVANCED AND THIS ROUTINE IS EXITED.              * EL309
01460 ***************************************************************** EL309
01461                                                                   EL309
01462      IF HIR-CLAIM-KEY NOT = CL-CONTROL-PRIMARY                    EL309
01463          MOVE +2                 TO  WS-HISTORY-FILE-OPEN         EL309
01464          GO TO 6390-EXIT                                          EL309
01465      ELSE                                                         EL309
01466          MOVE +1                 TO  WS-HISTORY-FILE-OPEN.        EL309
01467  EJECT                                                            EL309
01468  6330-EQUAL-TO.                                                   EL309
01469                                                                   EL309
01470 ***************************************************************** EL309
01471 *  THE WS-BYPASS-SW IS A READ/WRITE LOOPING INDICATOR.  THE     * EL309
01472 *  SWITCH IS SET IN THE COMPARISON OF THE 'CL' RECORDS          * EL309
01473 *  AND INDICATES THAT THE LOGIC FOLLOWING THE STATEMENT         * EL309
01474 *  IS TO BE BYPASSED.  THE PURGED SWITCH IS SET THE FIRST TIME  * EL309
01475 *  A RECORD COMES THROUGH AS A PURGED CLAIM.                    * EL309
01476 *  AT THIS POINT IN THE LOGIC, IF THE FLAG/FLAGS HAVE BEEN SET  * EL309
01477 *  THE PROGRAM IS READING AND WRITTING THE ACCOMPANING 'CM',    * EL309
01478 *  'AT, AND 'LA' RECORDS.                                       * EL309
01479 ***************************************************************** EL309
01480                                                                   EL309
01481      IF WS-BYPASS-SW = +1                                         EL309
01482          IF PURGED                                                EL309
01483              MOVE 'Y'            TO HOR-PURGED-CLAIM              EL309
01484              PERFORM 7400-WRITE-HISTORY THRU 7400-EXIT            EL309
01485              GO TO 6320-READ-HISTORY-FILE                         EL309
01486          ELSE                                                     EL309
01487              PERFORM 7400-WRITE-HISTORY THRU 7400-EXIT            EL309
01488              GO TO 6320-READ-HISTORY-FILE.                        EL309
01489                                                                   EL309
01490                                                                   EL309
01491 ***************************************************************** EL309
01492 *   THE BYPASS SWITCH HAS NOT BEEN SET, AND THE RECORD IS       * EL309
01493 *   NOT A 'CL' RECORD.  THE RECORDS ARE TO BE BYPASSED.  I.E    * EL309
01494 *   NOT REWRITTEN TO THE CLAIMS HISTORY FILE.                   * EL309
01495 ***************************************************************** EL309
01496                                                                   EL309
01497      IF HIR-RECORD-ID NOT = 'CL'                                  EL309
01498          GO TO 6320-READ-HISTORY-FILE.                            EL309
01499                                                                   EL309
01500                                                                   EL309
01501 ***************************************************************** EL309
01502 *   A MODIFICATION TO EL326 MOVES LOW-VALUES TO THE CL-PURGED-DT* EL309
01503 *     IF THE CLAIM IS BROUGHT BACK TO THE ONLINE FILE FOR       * EL309
01504 *     ADDITIONAL ACTIVITY.                                        EL309
01505 *   IF THE CLAIM HAS A PURGED STATUS AND HAS NOT BEEN           * EL309
01506 *   RE-ESTABLISHED AS AN OPEN CLAIM VIA EL326 PROGRAM,          * EL309
01507 *   ALL RECORDS FROM THE CLAIM HISTORY FILE ARE REWRITTEN       * EL309
01508 *   TO THE OUTPUT FILE. (WS-BYPASS-SW IS SET)                   * EL309
01509 *****************************************************************.EL309
01510                                                                   EL309
01511      IF HIR-PURGED-CLAIM EQUAL 'Y'                                EL309
01512          IF CL-PURGED-DT NOT EQUAL LOW-VALUES AND SPACES          EL309
01513              MOVE +1                 TO  WS-BYPASS-SW             EL309
01514              MOVE CLAIM-MASTER       TO HOR-CLAIM-RECORD          EL309
01515              PERFORM 7400-WRITE-HISTORY THRU 7400-EXIT            EL309
01516              GO TO 6320-READ-HISTORY-FILE.                        EL309
01517                                                                   EL309
01518 ***************************************************************** EL309
01519 *   IF THIS IS THE FIRST TIME THROUGH AS A PURGED CLAIM,        * EL309
01520 *   THE PURGED FLAG IS SET, WS-BYPASS-SW IS SET AND ALL         * EL309
01521 *   RECORDS FROM THE CLAIMS HISTORY FILE ARE REWRITTEN          * EL309
01522 *****************************************************************.EL309
01523                                                                   EL309
01524      IF CL-PURGED-DT NOT EQUAL LOW-VALUES AND SPACES              EL309
01525          ADD +1                  TO WS-CL-NEW-PURGED              EL309
01526          MOVE +1                 TO WS-BYPASS-SW                  EL309
01527                                     WS-PURGE-SW                   EL309
01528          MOVE 'Y'                TO HOR-PURGED-CLAIM              EL309
01529          MOVE CLAIM-MASTER       TO HOR-CLAIM-RECORD              EL309
01530          PERFORM 7400-WRITE-HISTORY THRU 7400-EXIT                EL309
01531          GO TO 6320-READ-HISTORY-FILE.                            EL309
01532                                                                   EL309
01533 ***************************************************************** EL309
01534 * EQUAL CLAIMS ARE FOUND WITH UPDATES TO THE CLAIM              * EL309
01535 *  AS OF 09/88  THIS PROGRAM WILL RECREATE ALL ACTIVITY         * EL309
01536 * EACH MONTH, NOT JUST WHEN THE CLIAM RECORD INDICATES THAT     * EL309
01537 * THERE HAS BEEN ACTIVITY.                                        EL309
01538 ***************************************************************** EL309
01539                                                                   EL309
01540      MOVE +1                     TO WS-HISTORY-FILE-OPEN.         EL309
01541      MOVE +1                     TO WS-FOUND-EQUAL-SW.            EL309
01542                                                                   EL309
01543  6390-EXIT.                                                       EL309
01544      EXIT.                                                        EL309
01545      EJECT                                                        EL309
01546 ***************************************************************** EL309
01547 *               READ AND PROCESS ACTIVITY RECORDS               * EL309
01548 *****************************************************************.EL309
01549  6400-READ-WRITE-AT-RECS.                                         EL309
01550      MOVE SPACES                 TO  AT-CONTROL-PRIMARY.          EL309
01551                                                                   EL309
01552      MOVE CL-COMPANY-CD          TO  AT-COMPANY-CD.               EL309
01553      MOVE CL-CARRIER             TO  AT-CARRIER.                  EL309
01554      MOVE CL-CLAIM-NO            TO  AT-CLAIM-NO.                 EL309
01555      MOVE CL-CERT-NO             TO  AT-CERT-NO.                  EL309
01556      MOVE ZERO                   TO  AT-SEQUENCE-NO.              EL309
01557                                                                   EL309
01558      START ELTRLR                                                 EL309
01559          KEY IS EQUAL TO AT-CONTROL-PRIMARY.                      EL309
01560                                                                   EL309
01561      IF ELTRLR-FILE-STATUS = '23' OR '10'                         EL309
01562          MOVE 'Y'                TO WS-EXTRACT-ERROR-SW           EL309
01563          MOVE 'ZERO TRAILER NOT FOUND'  TO  WS-D1-MESSAGE         EL309
01564          MOVE WS-DETAIL1         TO  PRT                          EL309
01565          PERFORM WRITE-A-LINE                                     EL309
01566          MOVE SPACES             TO  WS-DETAIL1                   EL309
01567        ELSE                                                       EL309
01568      IF ELTRLR-FILE-STATUS NOT = ZERO                             EL309
01569          MOVE 'ERROR OCCURED START ZERO TRAILER - ELTRLR'         EL309
01570                                  TO  WS-ABEND-MESSAGE             EL309
01571          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
01572          PERFORM ABEND-PGM.                                       EL309
01573                                                                   EL309
01574      EJECT                                                        EL309
01575  6400-READ-AT.                                                    EL309
01576      READ ELTRLR NEXT.                                            EL309
01577                                                                   EL309
01578      IF ELTRLR-FILE-STATUS = '10'                                 EL309
01579          GO TO 6455-EXIT.                                         EL309
01580                                                                   EL309
01581      IF ELTRLR-FILE-STATUS NOT = ZERO                             EL309
pemuni         display 'claim key = ' cl-control-primary
               display 'trlr  key = ' at-control-primary
01582          MOVE 'ERROR OCCURED READNEXT TRAILER RECORD - ELTRLR'    EL309
01583                                  TO  WS-ABEND-MESSAGE             EL309
01584          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
01585          PERFORM ABEND-PGM.                                       EL309
01586                                                                   EL309
01587      IF CL-COMPANY-CD NOT = AT-COMPANY-CD  OR                     EL309
01588         CL-CARRIER    NOT = AT-CARRIER     OR                     EL309
01589         CL-CLAIM-NO   NOT = AT-CLAIM-NO    OR                     EL309
01590         CL-CERT-NO    NOT = AT-CERT-NO                            EL309
01591          GO TO 6455-EXIT.                                         EL309
01592                                                                   EL309
01593      ADD +1  TO  WS-TRAILER-COUNT.                                EL309
01594                                                                   EL309
01595      MOVE WS-COMPANY-ID          TO  HOR-COMPANY-ID.              EL309
01596      MOVE CL-CONTROL-PRIMARY     TO  HOR-CLAIM-KEY.               EL309
01597      MOVE WS-CURRENT-DATE        TO  HOR-DATE-ARCHIVED.           EL309
01598      MOVE SPACES                 TO  HOR-PURGED-CLAIM.            EL309
01599      MOVE ACTIVITY-TRAILERS      TO  HOR-ACTIVITY-TRAILER-RECORD. EL309
01600                                                                   EL309
01601      PERFORM 7400-WRITE-HISTORY THRU 7400-EXIT.                   EL309
01602                                                                   EL309
01603      IF AT-TRAILER-TYPE = '4'                                     EL309
01604        IF AT-LETTER-ARCHIVE-NO NOT EQUAL +0                       EL309
01605            PERFORM 6500-ARCHIVE-LETTER THRU 6690-EXIT.            EL309
01606                                                                   EL309
01607      GO TO 6400-READ-AT.                                          EL309
01608                                                                   EL309
01609  6455-EXIT.                                                       EL309
01610       EXIT.                                                          CL**3
01611      EJECT                                                        EL309
01612 ***************************************************************** EL309
01613 *               READ AND PROCESS ARCHIVE LETTERS                * EL309
01614 *****************************************************************.EL309
01615  6500-ARCHIVE-LETTER SECTION.                                     EL309
01616                                                                   EL309
01617      MOVE LOW-VALUES             TO  LA-CONTROL-PRIMARY.          EL309
01618      MOVE WS-COMPANY-CD          TO  LA-COMPANY-CD.               EL309
01619      MOVE AT-LETTER-ARCHIVE-NO   TO  LA-ARCHIVE-NO.               EL309
01620                                                                   EL309
01621      START ELARCH                                                 EL309
01622          KEY IS NOT LESS THAN LA-CONTROL-PRIMARY.                 EL309
01623                                                                   EL309
01624      IF ELARCH-FILE-STATUS = '23'                                 EL309
01625          IF EQUAL-KEYS                                            EL309
01626              GO TO 6520-SEARCH-FOR-LETTER                         EL309
01627          ELSE                                                     EL309
01628              MOVE 'Y'            TO WS-EXTRACT-ERROR-SW           EL309
01629              MOVE 'ARCH LETTER NOT FOUND '                        EL309
01630                                  TO WS-D1-MESSAGE                 EL309
01631              MOVE WS-DETAIL1         TO  PRT                      EL309
01632              PERFORM WRITE-A-LINE                                 EL309
01633              MOVE SPACES             TO  WS-DETAIL1               EL309
01634              GO TO 6690-EXIT.                                     EL309
01635                                                                   EL309
01636      IF ELARCH-FILE-STATUS NOT = ZERO                             EL309
01637          MOVE 'ERROR OCCURED START - ELARCH'                      EL309
01638                                  TO  WS-ABEND-MESSAGE             EL309
01639          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
01640          PERFORM ABEND-PGM.                                       EL309
01641                                                                   EL309
01642      MOVE SPACES                 TO  WS-FOUND-LETTER-SW.          EL309
01643                                                                   EL309
01644  6510-READ-ARCHIVE.                                               EL309
01645      READ ELARCH NEXT.                                            EL309
01646                                                                   EL309
01647      IF ELARCH-FILE-STATUS = '10'                                 EL309
01648          IF LETTER-NOT-FOUND AND EQUAL-KEYS                       EL309
01649              GO TO 6520-SEARCH-FOR-LETTER                         EL309
01650          ELSE                                                     EL309
01651              GO TO 6690-EXIT.                                     EL309
01652                                                                   EL309
01653      IF ELARCH-FILE-STATUS NOT = ZERO                             EL309
01654          MOVE 'ERROR OCCURED READNEXT - ELARCH'                   EL309
01655                                  TO  WS-ABEND-MESSAGE             EL309
01656          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
01657          PERFORM ABEND-PGM.                                       EL309
01658                                                                   EL309
01659      IF LA-COMPANY-CD        NOT = WS-COMPANY-CD OR               EL309
01660         AT-LETTER-ARCHIVE-NO NOT = LA-ARCHIVE-NO                  EL309
01661          IF LETTER-NOT-FOUND AND EQUAL-KEYS                       EL309
01662              GO TO 6520-SEARCH-FOR-LETTER                         EL309
01663          ELSE                                                     EL309
01664              GO TO 6690-EXIT.                                     EL309
01665                                                                   EL309
01666      MOVE '1'                    TO  WS-FOUND-LETTER-SW.          EL309
01667      MOVE WS-COMPANY-ID          TO  HOR-COMPANY-ID.              EL309
01668      MOVE CL-CONTROL-PRIMARY     TO  HOR-CLAIM-KEY.               EL309
01669      MOVE WS-CURRENT-DATE        TO  HOR-DATE-ARCHIVED.           EL309
01670      MOVE SPACES                 TO  HOR-PURGED-CLAIM.            EL309
01671      MOVE LETTER-ARCHIVE         TO  HOR-LETTER-ARCHIVE-RECORD.   EL309
01672                                                                   EL309
01673      PERFORM 7400-WRITE-HISTORY THRU 7400-EXIT.                   EL309
01674                                                                   EL309
01675      GO TO 6510-READ-ARCHIVE.                                     EL309
01676  EJECT                                                            EL309
01677 ***************************************************************** EL309
01678 *  READS THROUGH THE CLAIMS HISTORY FILE SEARCHING FOR THE      * EL309
01679 * ACTIVITY RECORD EQUAL TO THE ACTIVITY RECORD FOUND ON-LINE.   * EL309
01680 * IF A MATCH IS FOUND, A FLAG IS TURNED ON AND THE ACCOMPANYING * EL309
01681 * 'LA' RECORDS ARE REVIEWED AND WRITTEN TO THE CLAIMS HISTORY   * EL309
01682 * OUTPUT FILE.  A SWITCH IS TURNED ON TO INDICATE THAT THE      * EL309
01683 * LETTER WAS SUCCESSFULLY FOUND.                                * EL309
01684 *  EXITS FROM THIS ROUTINE WHEN THE NEXT CLAIMS RECORD IS FOUND * EL309
01685 * OR THE ARCHIVE LETTER CONTROL PRIMARY IS GREATER THAN THE     * EL309
01686 * ON-LINE RECORD.                                               * EL309
01687 *****************************************************************.EL309
01688                                                                   EL309
01689  6520-SEARCH-FOR-LETTER.                                          EL309
01690                                                                   EL309
01691      IF WS-LAST-RECORD-ID  EQUAL 'AT'                             EL309
01692          IF WS-CONTROL-PRIMARY EQUAL AT-CONTROL-PRIMARY           EL309
01693              MOVE +1                 TO WS-MATCH-FOUND-SW.        EL309
01694                                                                   EL309
01695      IF END-OF-HISTORY-FILE                                       EL309
01696          GO TO 6690-EXIT                                          EL309
01697      ELSE                                                         EL309
01698          PERFORM 6000-PROCESS-HISTORY-FILE THRU 6390-EXIT.        EL309
01699                                                                   EL309
01700      IF HIR-RECORD-ID EQUAL 'CL'                                  EL309
01701          IF LETTER-NOT-FOUND                                      EL309
01702             MOVE 'Y'            TO WS-EXTRACT-ERROR-SW            EL309
01703             MOVE AT-LETTER-ARCHIVE-NO TO WS-NUMBER-DISPLAY        EL309
01704             MOVE WS-LETTER-MESSAGE    TO WS-D1-MESSAGE            EL309
01705             MOVE WS-DETAIL1         TO  PRT                       EL309
01706             PERFORM WRITE-A-LINE                                  EL309
01707             MOVE SPACES             TO  WS-DETAIL1                EL309
01708             MOVE +2              TO WS-HISTORY-FILE-OPEN          EL309
01709             GO TO 6690-EXIT                                       EL309
01710          ELSE                                                     EL309
01711             MOVE +2              TO WS-HISTORY-FILE-OPEN          EL309
01712             GO TO 6690-EXIT.                                      EL309
01713                                                                   EL309
01714      IF MATCH-FOUND                                               EL309
01715         IF HIR-RECORD-ID EQUAL 'LA'                               EL309
01716            GO TO 6520-CONTINUE-SEARCH.                            EL309
01717                                                                   EL309
01718      IF HIR-RECORD-ID  EQUAL 'AT'                                 EL309
01719          MOVE HIR-ACTIVITY-TRAILER-RECORD                         EL309
01720                                  TO WS-ACTIVITY-TRAILER.          EL309
01721                                                                   EL309
01722      IF WS-CONTROL-PRIMARY EQUAL AT-CONTROL-PRIMARY               EL309
01723          MOVE +1                 TO WS-MATCH-FOUND-SW             EL309
01724          GO TO 6520-SEARCH-FOR-LETTER.                            EL309
01725                                                                   EL309
01726      IF WS-CONTROL-PRIMARY GREATER THAN AT-CONTROL-PRIMARY        EL309
01727          IF LETTER-NOT-FOUND                                      EL309
01728             MOVE 'Y'            TO WS-EXTRACT-ERROR-SW            EL309
01729             MOVE AT-LETTER-ARCHIVE-NO TO WS-NUMBER-DISPLAY        EL309
01730             MOVE WS-LETTER-MESSAGE    TO WS-D1-MESSAGE            EL309
01731             MOVE WS-DETAIL1      TO  PRT                          EL309
01732             PERFORM WRITE-A-LINE                                  EL309
01733             MOVE SPACES          TO WS-DETAIL1                    EL309
01734             MOVE SPACES          TO WS-MATCH-FOUND-SW             EL309
01735             GO TO 6690-EXIT                                       EL309
01736          ELSE                                                     EL309
01737             MOVE SPACES          TO WS-MATCH-FOUND-SW             EL309
01738             MOVE SPACES          TO WS-FOUND-LETTER-SW            EL309
01739             GO TO 6690-EXIT.                                      EL309
01740                                                                   EL309
01741      GO TO 6520-SEARCH-FOR-LETTER.                                EL309
01742                                                                   EL309
01743  6520-CONTINUE-SEARCH.                                            EL309
01744      MOVE HIR-LETTER-ARCHIVE-RECORD TO WS-LETTER-ARCHIVE.         EL309
01745      IF AT-LETTER-ARCHIVE-NO EQUAL TO                             EL309
01746                                WS-LETTER-ARCHIVE-NO               EL309
01747          MOVE '1'                  TO WS-FOUND-LETTER-SW          EL309
01748          PERFORM 7400-WRITE-HISTORY THRU 7400-EXIT.               EL309
01749                                                                   EL309
01750      GO TO 6520-SEARCH-FOR-LETTER.                                EL309
01751                                                                   EL309
01752  6690-EXIT.                                                       EL309
01753      EXIT.                                                        EL309
01754      EJECT                                                        EL309
01755 ***************************************************************** EL309
01756 * ADVANCES THE CLAIMS HISTORY FILE UNTIL THE NEXT CLAIMS        * EL309
01757 * RECORD IS FOUND.                                              * EL309
01758 ***************************************************************** EL309
01759                                                                   EL309
01760  6700-READ-UNTIL-NEXT.                                            EL309
01761                                                                   EL309
01762      PERFORM 6000-PROCESS-HISTORY-FILE THRU 6390-EXIT.            EL309
01763                                                                   EL309
01764      IF END-OF-HISTORY-FILE                                       EL309
01765         GO TO 6700-EXIT.                                          EL309
01766                                                                   EL309
01767      IF HIR-RECORD-ID EQUAL 'CL'                                  EL309
01768         MOVE +2              TO WS-HISTORY-FILE-OPEN              EL309
01769         GO TO 6700-EXIT                                           EL309
01770      ELSE                                                         EL309
01771         GO TO 6700-READ-UNTIL-NEXT.                               EL309
01772                                                                   EL309
01773  6700-EXIT.                                                       EL309
01774      EXIT.                                                        EL309
01775      EJECT                                                        EL309
01776 ***************************************************************** EL309
01777 * COUNTS THE RECORDS DROPPED FROM THE CLAIMS HISTORY FILE       * EL309
01778 ***************************************************************** EL309
01779                                                                   EL309
01780  6900-COUNT-RECORDS.                                              EL309
01781      DISPLAY 'DROPPED ' HIR-COMPANY-ID ' ' HIR-CLAIM-KEY ' '      EL309
01782         HIR-RECORD-ID.                                            EL309
01783                                                                   EL309
01784      IF HIR-RECORD-ID = 'AT'                                      EL309
01785          ADD +1                  TO  WS-AT-HISTORY-DROPPED        EL309
01786        ELSE                                                       EL309
01787      IF HIR-RECORD-ID = 'LA'                                      EL309
01788          ADD +1                  TO  WS-LA-HISTORY-DROPPED        EL309
01789        ELSE                                                       EL309
01790      IF HIR-RECORD-ID = 'CM'                                      EL309
01791          ADD +1                  TO  WS-CM-HISTORY-DROPPED        EL309
01792        ELSE                                                       EL309
01793      IF HIR-RECORD-ID = 'PM'                                      EL309
01794          ADD +1                  TO  WS-PM-HISTORY-DROPPED        EL309
01795        ELSE                                                       EL309
01796      IF HIR-RECORD-ID = 'CL'                                      EL309
01797          ADD +1                  TO  WS-CL-HISTORY-DROPPED.       EL309
01798                                                                   EL309
01799      ADD +1                      TO  WS-TOTAL-HISTORY-DROPPED.    EL309
01800                                                                   EL309
01801  6900-EXIT.                                                       EL309
01802       EXIT.                                                          CL**3
01803  EJECT                                                            EL309
01804 ***************************************************************** EL309
01805 * COUNTS THE RECORDS ADDED TO THE CLAIMS HISTORY FILE           * EL309
01806 * FOR REPORTING PURPOSES.                                       * EL309
01807 ***************************************************************** EL309
01808                                                                   EL309
01809  6901-COUNT-ADD-RECORD.                                           EL309
01810                                                                   EL309
01811      IF HOR-RECORD-ID = 'CM'                                      EL309
01812          ADD +1                  TO  WS-CM-HISTORY-ADDED          EL309
01813        ELSE                                                       EL309
01814      IF HOR-RECORD-ID = 'PM'                                      EL309
01815          ADD +1                  TO  WS-PM-HISTORY-ADDED          EL309
01816        ELSE                                                       EL309
01817      IF HOR-RECORD-ID = 'CL'                                      EL309
01818          ADD +1                  TO  WS-CL-HISTORY-ADDED.         EL309
01819                                                                   EL309
01820      ADD +1                      TO  WS-TOTAL-HISTORY-ADDED.      EL309
01821                                                                   EL309
01822  6901-EXIT.                                                       EL309
01823       EXIT.                                                          CL**3
01824      EJECT                                                        EL309
01825 ***************************************************************** EL309
01826 * CHECKS THE STATUS OF INPUT RECORDS FOR REPORTING PURPOSES.    * EL309
01827 ***************************************************************** EL309
01828                                                                   EL309
01829  6902-COUNT-INPUT.                                                EL309
01830                                                                   EL309
01831      IF HIR-PURGED-CLAIM EQUAL 'Y'                                EL309
01832          MOVE 'P'                TO  WS-LAST-CLAIM-STATUS         EL309
01833          ADD +1                  TO  WS-PURGED-CLAIMS-IPT         EL309
01834          GO TO 6902-EXIT.                                         EL309
01835                                                                   EL309
01836      MOVE HIR-CLAIM-RECORD       TO WS-CLAIM-MASTER.              EL309
01837                                                                   EL309
01838      IF WS-STATUS = 'O'                                           EL309
01839          MOVE 'O'                TO  WS-LAST-CLAIM-STATUS         EL309
01840          ADD +1                  TO  WS-OPEN-CLAIMS-IPT           EL309
01841      ELSE                                                         EL309
01842          MOVE 'C'                TO  WS-LAST-CLAIM-STATUS         EL309
01843          ADD +1                  TO  WS-CLOSED-CLAIMS-IPT.        EL309
01844                                                                   EL309
01845  6902-EXIT.                                                       EL309
01846       EXIT.                                                          CL**3
01847      EJECT                                                        EL309
01848 ***************************************************************** EL309
01849 * CHECKS FOR STATUS OF OUTPUT RECORD FOR REPORTING PURPOSES.    * EL309
01850 ***************************************************************** EL309
01851                                                                   EL309
01852  6903-COUNT-OUTPUT.                                               EL309
01853                                                                   EL309
01854      IF HOR-PURGED-CLAIM EQUAL 'Y'                                EL309
01855          ADD +1                  TO WS-PURGED-CLAIMS-OPT          EL309
01856          GO TO 6903-EXIT.                                         EL309
01857                                                                   EL309
01858      MOVE HOR-CLAIM-RECORD       TO WS-CLAIM-MASTER.              EL309
01859                                                                   EL309
01860      IF WS-STATUS = 'O'                                           EL309
01861          ADD +1                  TO  WS-OPEN-CLAIMS-OPT           EL309
01862      ELSE                                                         EL309
01863          ADD +1                  TO  WS-CLOSED-CLAIMS-OPT.        EL309
01864                                                                   EL309
01865  6903-EXIT.                                                       EL309
01866       EXIT.                                                          CL**3
01867      EJECT                                                        EL309
01868 ***************************************************************** EL309
01869 * DECREMENTS THE TOTAL FOR REPORT.                              * EL309
01870 ***************************************************************** EL309
01871                                                                   EL309
01872  6904-DECREMENT-TOTALS.                                           EL309
01873      IF WS-DIFF EQUAL +1                                          EL309
01874          IF WS-LAST-CLAIM-STATUS EQUAL 'P'                        EL309
01875              ADD +1              TO WS-PURGED-CLAIMS-IPT          EL309
01876              MOVE -1             TO WS-NEXT-PURGED-IPT            EL309
01877              GO TO 6904-EXIT                                      EL309
01878          ELSE                                                     EL309
01879              IF WS-LAST-CLAIM-STATUS EQUAL 'O'                    EL309
01880                  ADD +1          TO WS-OPEN-CLAIMS-IPT            EL309
01881                  MOVE -1         TO WS-NEXT-OPEN-IPT              EL309
01882                  GO TO 6904-EXIT                                  EL309
01883              ELSE                                                 EL309
01884                  ADD +1          TO WS-CLOSED-CLAIMS-IPT          EL309
01885                  MOVE -1         TO WS-NEXT-CLOSED-IPT            EL309
01886                  GO TO 6904-EXIT.                                 EL309
01887                                                                   EL309
01888      IF WS-LAST-CLAIM-STATUS  EQUAL 'P'                           EL309
01889          ADD  -1                 TO WS-PURGED-CLAIMS-IPT          EL309
01890          MOVE +1                 TO WS-NEXT-PURGED-IPT            EL309
01891          GO TO 6904-EXIT.                                         EL309
01892                                                                   EL309
01893      IF WS-LAST-CLAIM-STATUS  EQUAL 'O'                           EL309
01894          ADD  -1                 TO WS-OPEN-CLAIMS-IPT            EL309
01895          MOVE +1                 TO WS-NEXT-OPEN-IPT              EL309
01896      ELSE                                                         EL309
01897          ADD  -1                 TO WS-CLOSED-CLAIMS-IPT          EL309
01898          MOVE +1                 TO WS-NEXT-CLOSED-IPT.           EL309
01899                                                                   EL309
01900  6904-EXIT.                                                       EL309
01901       EXIT.                                                          CL**3
01902      EJECT                                                        EL309
01903 ***************************************************************** EL309
01904 * COUNTS CLAIMS DELETED FROM THE ON-LINE CLAIMS MASTER          * EL309
01905 ***************************************************************** EL309
01906                                                                   EL309
01907  6905-COUNT-VALID-DEL.                                            EL309
01908                                                                   EL309
01909      IF HIR-RECORD-ID = 'CL'                                      EL309
01910          ADD +1                  TO WS-CL-VALID-DEL.              EL309
01911                                                                   EL309
01912  6905-EXIT.                                                       EL309
01913       EXIT.                                                          CL**3
01914  EJECT                                                            EL309
01915 ***************************************************************** EL309
01916 *               6 9 1 0  -  READ RETRIEVE FILE                  * EL309
01917 *       READ THE RETRIEVE FILE TO SEE IF THE CLAIM HAS BEEN     * EL309
01918 *   REMOVED FROM THE ELMSTR FILE.  IF THE RETRIEVE RECORD IS    * EL309
01919 *   FOUND, AND THE PURGED DATE IS SET TO SOMETHING BESIDES LOW- * EL309
01920 *   VALUES OR SPACES,  FLAG THE CLAIM AS BEING PURGED...        * EL309
01921 ***************************************************************** EL309
01922  6910-READ-RETRIEVE-FILE.                                         EL309
01923      IF RETRIEVE-NOT-RESEARCHED                                   EL309
01924          NEXT SENTENCE                                            EL309
01925      ELSE                                                         EL309
01926          GO TO 6910-READ-RETRIEVE-FILE-EXIT.                      EL309
01927                                                                   EL309
01928      MOVE HIR-CLAIM-KEY            TO RL-CONTROL-PRIMARY.         EL309
01929                                                                   EL309
01930      READ ELRETR.                                                 EL309
01931                                                                   EL309
01932      IF ELRETR-FILE-STATUS = '23' OR '10'                         EL309
01933          MOVE '3'                TO  WS-FOUND-RETRIEVE-SW         EL309
01934          GO TO 6910-READ-RETRIEVE-FILE-EXIT                       EL309
01935        ELSE                                                       EL309
01936      IF ELRETR-FILE-STATUS NOT = ZERO AND '02'                    EL309
01937          MOVE 'ERROR OCCURED READ - ELRETR'                       EL309
01938                                  TO  WS-ABEND-MESSAGE             EL309
01939          MOVE ELRETR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
01940          PERFORM ABEND-PGM.                                       EL309
01941                                                                   EL309
01942      IF HIR-CLAIM-KEY EQUAL RL-CONTROL-PRIMARY                    EL309
01943          ADD +1                  TO WS-RETRIEVE-CLAIMS-FOUND      EL309
01944          MOVE '1'                TO WS-FOUND-RETRIEVE-SW.         EL309
01945                                                                   EL309
01946      IF RL-PURGED-DT NOT EQUAL LOW-VALUES AND SPACES              EL309
01947          MOVE '2'                TO WS-FOUND-RETRIEVE-SW          EL309
01948          ADD  +1                 TO WS-CL-NEW-PURGED              EL309
01949          MOVE +1                 TO WS-BYPASS-SW                  EL309
01950                                     WS-PURGE-SW                   EL309
01951          MOVE 'Y'                TO HOR-PURGED-CLAIM              EL309
01952          MOVE 'CL'               TO RL-RECORD-ID                  EL309
01953          MOVE RETRIEVE-MASTER    TO HOR-CLAIM-RECORD.             EL309
01954                                                                   EL309
01955      IF WS-FOUND-RETRIEVE-SW EQUAL '1' OR '2'                     EL309
01956          NEXT SENTENCE                                            EL309
01957      ELSE                                                         EL309
01958          DISPLAY ' NOT FOUND ' HIR-CLAIM-KEY.                     EL309
01959                                                                   EL309
01960  6910-READ-RETRIEVE-FILE-EXIT.                                    EL309
01961      EXIT.                                                           CL**3
01962  EJECT                                                            EL309
01963                                                                   EL309
01964 ***************************************************************** EL309
01965 *      7 0 0 0  -  WRITE HISTORY TO OUTFILE                     * EL309
01966 ***************************************************************** EL309
01967 ***************************************************************** EL309
01968 *                    WRITE HISTORY RECORDS                      * EL309
01969 *****************************************************************.EL309
01970  7400-WRITE-HISTORY SECTION.                                      EL309
01971                                                                   EL309
01972      IF HOR-RECORD-ID = 'AT'                                      EL309
01973          ADD +1                  TO  WS-AT-HISTORY-OUTPUT         EL309
01974          WRITE HISTORY-OUTPUT-TRAILER-RECORD                      EL309
01975        ELSE                                                       EL309
01976      IF HOR-RECORD-ID = 'LA'                                      EL309
01977          ADD +1                  TO  WS-LA-HISTORY-OUTPUT         EL309
01978          WRITE HISTORY-OUTPUT-LETTER-RECORD                       EL309
01979        ELSE                                                       EL309
01980      IF HOR-RECORD-ID = 'CM'                                      EL309
01981          ADD +1                  TO  WS-CM-HISTORY-OUTPUT         EL309
01982          WRITE HISTORY-OUTPUT-CERT-RECORD                         EL309
01983        ELSE                                                       EL309
01984      IF HOR-RECORD-ID = 'PM'                                      EL309
01985          ADD +1                  TO  WS-PM-HISTORY-OUTPUT         EL309
pemuni*        WRITE HISTORY-OUTPUT-POLICY-RECORD                       EL309
01987        ELSE                                                       EL309
01988      IF HOR-RECORD-ID = 'CL'                                      EL309
01989          ADD +1                  TO  WS-CL-HISTORY-OUTPUT         EL309
01990          PERFORM 6903-COUNT-OUTPUT THRU 6903-EXIT                 EL309
01991          WRITE HISTORY-OUTPUT-CLAIM-RECORD                        EL309
01992        ELSE                                                       EL309
01993          ADD +1                  TO  WS-OTHER-HISTORY-OUTPUT      EL309
01994          WRITE HISTORY-OUTPUT-CLAIM-RECORD.                       EL309
01995                                                                   EL309
01996      MOVE LOW-VALUES TO  HISTORY-OUTPUT-RECORD2.                  EL309
01997                                                                   EL309
01998      ADD +1                      TO  WS-HISTORY-OUTPUT-COUNT.     EL309
01999                                                                   EL309
02000      IF HIR-RECORD-ID = 'AT'                                      EL309
02001          MOVE HISTORY-INPUT-TRAILER-RECORD                        EL309
02002                                  TO  HISTORY-OUTPUT-TRAILER-RECORDEL309
02003        ELSE                                                       EL309
02004      IF HIR-RECORD-ID = 'LA'                                      EL309
02005          MOVE HISTORY-INPUT-LETTER-RECORD                         EL309
02006                                  TO  HISTORY-OUTPUT-LETTER-RECORD EL309
02007        ELSE                                                       EL309
02008      IF HIR-RECORD-ID = 'CM'                                      EL309
02009          MOVE HISTORY-INPUT-CERT-RECORD                           EL309
02010                                  TO  HISTORY-OUTPUT-CERT-RECORD   EL309
02011        ELSE                                                       EL309
02012      IF HIR-RECORD-ID = 'PM'                                      EL309
pemuni*        MOVE HISTORY-INPUT-POLICY-RECORD                         EL309
pemuni*                                TO  HISTORY-OUTPUT-POLICY-RECORD EL309
pemuni         continue
02015        ELSE                                                       EL309
02016          MOVE HISTORY-INPUT-CLAIM-RECORD                          EL309
02017                                  TO  HISTORY-OUTPUT-CLAIM-RECORD  EL309
pemuni       end-if
             end-if
             end-if
             end-if
             .
02018                                                                   EL309
02019  7400-EXIT.                                                       EL309
02020      EXIT.                                                        EL309
02021      EJECT                                                        EL309
02022 ***************************************************************** EL309
02023 *                        DATE CONVERSION                        * EL309
02024 *****************************************************************.EL309
02025  8500-DATE-CONVERSION SECTION.   COPY ELCDCS.                     EL309
02026                                                                   EL309
02027      EJECT                                                        EL309
02028  WRITE-A-LINE SECTION.           COPY ELCWAL.                     EL309
02029                                                                   EL309
02030      EJECT                                                        EL309
02031  WRITE-HEADINGS SECTION.                                          EL309
02032                                                                   EL309
02033 *    NOTE ******************************************************* EL309
02034 *         *      THIS SECTION CONTROLS THE WRITING OF THE       * EL309
02035 *         *  HEADINGS.                                          * EL309
02036 *         *******************************************************.EL309
02037                                                                   EL309
02038  WHS-010.                                                         EL309
02039      ADD +1                      TO  WS-PAGE.                     EL309
02040      MOVE WS-PAGE                TO  WS-H2-PAGE.                  EL309
02041                                                                   EL309
02042      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL309
02043                                                                   EL309
02044      MOVE WS-HEADING1            TO  PRT.                         EL309
02045      PERFORM WRITE-PRINTER.                                       EL309
02046                                                                   EL309
02047      IF WS-EXTRACT-ERROR-SW EQUAL 'Y'                             EL309
02048          MOVE  'EXTRACT ERROR REGISTER'  TO WS-D2-TITLE           EL309
02049      ELSE                                                         EL309
02050          MOVE  '  EXTRACT STATISTICS  '  TO WS-D2-TITLE.          EL309
02051                                                                   EL309
02052      MOVE WS-HEADING2            TO  PRT.                         EL309
02053      PERFORM WRITE-PRINTER.                                       EL309
02054                                                                   EL309
02055      MOVE WS-HEADING3            TO  PRT.                         EL309
02056      PERFORM WRITE-PRINTER.                                       EL309
02057                                                                   EL309
02058      IF WS-EXTRACT-ERROR-SW EQUAL 'Y'                             EL309
02059          MOVE WS-HEADING4        TO  PRT                          EL309
02060          PERFORM WRITE-PRINTER                                    EL309
02061          MOVE WS-HEADING5        TO  PRT                          EL309
02062          PERFORM WRITE-PRINTER                                    EL309
02063          MOVE +6                 TO  WS-LINE-COUNT                EL309
02064      ELSE                                                         EL309
02065          MOVE +4                 TO  WS-LINE-COUNT.               EL309
02066                                                                   EL309
02067      MOVE SPACES                 TO  WS-EXTRACT-ERROR-SW.         EL309
02068                                                                   EL309
02069  WHS-020.                        COPY ELCWHS2.                    EL309
02070                                                                   EL309
02071      EJECT                                                        EL309
02072  WRITE-PRINTER SECTION.          COPY ELCWPS.                     EL309
02073                                                                   EL309
02074      IF P-CTL   EQUAL ' '                                         EL309
02075          WRITE PRT   AFTER ADVANCING 1 LINE                       EL309
02076      ELSE                                                         EL309
02077          IF P-CTL   EQUAL '0'                                     EL309
02078              WRITE PRT   AFTER ADVANCING 2 LINE                   EL309
02079          ELSE                                                     EL309
02080              IF P-CTL   EQUAL '-'                                 EL309
02081                  WRITE PRT   AFTER ADVANCING 3 LINE               EL309
02082              ELSE                                                 EL309
02083                  WRITE PRT   AFTER ADVANCING PAGE.                EL309
02084                                                                   EL309
02085  WPS-EXIT.                                                        EL309
02086      EXIT.                                                        EL309
02087                                                                   EL309
02088      EJECT                                                        EL309
02089 ***************************************************************** EL309
02090 *                          OPENS  FILES                         * EL309
02091 *****************************************************************.EL309
02092  OPEN-FILES SECTION.                                              EL309
02093                                                                   EL309
02094  OFS-010.                                                         EL309
02095      OPEN INPUT ELPGMS                                            EL309
02096                 ELCERT                                            EL309
02097                 MPPLCY                                            EL309
02098                 ELCNTL                                            EL309
02099                 ELTRLR                                            EL309
02100                 ELARCH                                            EL309
02101                 ELRETR                                            EL309
02102                 HISTORY-INPUT-FILE                                EL309
02103          I-O    ELMSTR                                            EL309
pemuni*        OUTPUT HISTORY-OUTPUT-FILE                               EL309
pemuni     open output PRNTR.                                           EL309
02106                                                                   EL309
02107      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL309
02108          NEXT SENTENCE                                            EL309
02109        ELSE                                                       EL309
02110          MOVE 'ELCNTL  '         TO  WS-FEM-FILE-NAME             EL309
02111          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL309
02112          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02113          PERFORM ABEND-PGM.                                       EL309
02114                                                                   EL309
02115      IF ELCERT-FILE-STATUS  = '00' OR '97'                        EL309
02116          NEXT SENTENCE                                            EL309
02117        ELSE                                                       EL309
02118          MOVE 'ELCERT  '         TO  WS-FEM-FILE-NAME             EL309
02119          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL309
02120          MOVE ELCERT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02121          PERFORM ABEND-PGM.                                       EL309
02122                                                                   EL309
02123      IF (MPPLCY-FILE-STATUS IS EQUAL TO '00' OR '97' OR '9%'      EL309
               OR '9+')
02124          NEXT SENTENCE                                            EL309
02125      ELSE                                                         EL309
02126          MOVE 'MPPLCY'           TO  WS-FEM-FILE-NAME             EL309
02127          MOVE WS-FILE-ERROR-MESSAGE  TO  WS-ABEND-MESSAGE         EL309
02128          MOVE MPPLCY-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02129          PERFORM ABEND-PGM.                                       EL309
02130                                                                   EL309
02131      IF ELMSTR-FILE-STATUS  = '00' OR '97'                        EL309
02132          NEXT SENTENCE                                            EL309
02133        ELSE                                                       EL309
02134          MOVE 'ELMSTR  '         TO  WS-FEM-FILE-NAME             EL309
02135          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL309
02136          MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02137          PERFORM ABEND-PGM.                                       EL309
02138                                                                   EL309
02139      IF ELTRLR-FILE-STATUS  = '00' OR '97'                        EL309
02140          NEXT SENTENCE                                            EL309
02141        ELSE                                                       EL309
02142          MOVE 'ELTRLR  '         TO  WS-FEM-FILE-NAME             EL309
02143          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL309
02144          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02145          PERFORM ABEND-PGM.                                       EL309
02146                                                                   EL309
02147      IF ELPGMS-FILE-STATUS  = '00' OR '97'                        EL309
02148          NEXT SENTENCE                                            EL309
02149        ELSE                                                       EL309
02150          MOVE 'ELPGMS  '         TO  WS-FEM-FILE-NAME             EL309
02151          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL309
02152          MOVE ELPGMS-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02153          PERFORM ABEND-PGM.                                       EL309
02154                                                                   EL309
02155      IF ELARCH-FILE-STATUS  = '00' OR '97'                        EL309
02156          NEXT SENTENCE                                            EL309
02157        ELSE                                                       EL309
02158          MOVE 'ELARCH  '         TO  WS-FEM-FILE-NAME             EL309
02159          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL309
02160          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02161          PERFORM ABEND-PGM.                                       EL309
02162                                                                   EL309
02163      IF ELRETR-FILE-STATUS  = '00' OR '97'                        EL309
02164          NEXT SENTENCE                                            EL309
02165        ELSE                                                       EL309
02166          MOVE 'ELRETR  '         TO  WS-FEM-FILE-NAME             EL309
02167          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL309
02168          MOVE ELRETR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02169          PERFORM ABEND-PGM.                                       EL309
02170                                                                   EL309
02171      ACCEPT DC-GREG-DATE-1-YMD FROM DATE.                         EL309
02172      MOVE '3'                    TO  DC-OPTION-CODE.              EL309
02173      PERFORM 8500-DATE-CONVERSION.                                EL309
02174      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE              EL309
02175                                      WS-RUN-DATE.                 EL309
02176      MOVE DC-GREG-DATE-1-ALPHA   TO  WS-H3-DATE.                  EL309
02177                                                                   EL309
02178      ACCEPT WS-CURRENT-TIME FROM TIME.                            EL309
02179                                                                   EL309
02180      MOVE +99                    TO  WS-LINE-COUNT.               EL309
02181                                                                   EL309
02182  OFS-EXIT.                                                        EL309
02183      EXIT.                                                        EL309
02184                                                                   EL309
02185      EJECT                                                        EL309
02186 ***************************************************************** EL309
02187 *                          CLOSE  FILES                         * EL309
02188 *****************************************************************.EL309
02189  CLOSE-FILES SECTION.                                             EL309
02190                                                                   EL309
02191  CFS-010.                                                         EL309
02192      CLOSE ELCNTL                                                 EL309
02193            ELPGMS                                                 EL309
02194            ELCERT                                                 EL309
02195            MPPLCY                                                 EL309
02196            ELMSTR                                                 EL309
02197            ELTRLR                                                 EL309
02198            ELARCH                                                 EL309
02199            ELRETR                                                 EL309
02200            HISTORY-INPUT-FILE                                     EL309
02201            HISTORY-OUTPUT-FILE                                    EL309
02202            PRNTR.                                                 EL309
02203                                                                   EL309
02204      MOVE 'ERROR OCCURED CLOSING -'  TO  WS-FILE-ERROR-MESSAGE.   EL309
02205                                                                   EL309
02206      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL309
02207          MOVE 'ELCNTL  '         TO  WS-FEM-FILE-NAME             EL309
02208          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL309
02209          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02210          PERFORM ABEND-PGM.                                       EL309
02211                                                                   EL309
02212      IF ELCERT-FILE-STATUS NOT = ZERO                             EL309
02213          MOVE 'ELCERT  '         TO  WS-FEM-FILE-NAME             EL309
02214          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL309
02215          MOVE ELCERT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02216          PERFORM ABEND-PGM.                                       EL309
02217                                                                   EL309
unix       MOVE '00'  TO MPPLCY-FILE-STATUS
02218      IF MPPLCY-FILE-STATUS IS NOT EQUAL TO ZERO                   EL309
02219          MOVE 'MPPLCY  '         TO  WS-FEM-FILE-NAME             EL309
02220          MOVE WS-FILE-ERROR-MESSAGE  TO  WS-ABEND-MESSAGE         EL309
02221          MOVE MPPLCY-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02222          PERFORM ABEND-PGM.                                       EL309
02223                                                                   EL309
02224      IF ELMSTR-FILE-STATUS NOT = ZERO                             EL309
02225          MOVE 'ELMSTR  '         TO  WS-FEM-FILE-NAME             EL309
02226          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL309
02227          MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02228          PERFORM ABEND-PGM.                                       EL309
02229                                                                   EL309
02230      IF ELTRLR-FILE-STATUS NOT = ZERO                             EL309
02231          MOVE 'ELTRLR  '         TO  WS-FEM-FILE-NAME             EL309
02232          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL309
02233          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02234          PERFORM ABEND-PGM.                                       EL309
02235                                                                   EL309
02236      IF ELPGMS-FILE-STATUS NOT = ZERO                             EL309
02237          MOVE 'ELPGMS  '         TO  WS-FEM-FILE-NAME             EL309
02238          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL309
02239          MOVE ELPGMS-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02240          PERFORM ABEND-PGM.                                       EL309
02241                                                                   EL309
02242      IF ELARCH-FILE-STATUS NOT = ZERO                             EL309
02243          MOVE 'ELARCH  '         TO  WS-FEM-FILE-NAME             EL309
02244          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL309
02245          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL309
02246          PERFORM ABEND-PGM.                                       EL309
02247                                                                   EL309
02248  CFS-EXIT.                                                        EL309
02249      EXIT.                                                        EL309
02250                                                                   EL309
02251  ABEND-PGM SECTION.              COPY ELCABEND SUPPRESS.          EL309
02252                                                                   EL309
