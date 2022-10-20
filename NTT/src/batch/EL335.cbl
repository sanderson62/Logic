00001  IDENTIFICATION DIVISION.                                         09/24/97
00002                                                                   EL335
00003  PROGRAM-ID.                 EL335 .                                 LV001
00004 *              PROGRAM CONVERTED BY                               EL335
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL335
00006 *              CONVERSION DATE 09/26/95 08:03:46.                 EL335
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL335
00008 *                            VMOD=2.015                           EL335
00009                                                                   EL335
00009                                                                   EL335
00010 *AUTHOR.     LOGIC INC.                                           EL335
00011 *            DALLAS, TEXAS.                                       EL335
00012                                                                   EL335
00013 *DATE-COMPILED.                                                   EL335
00014                                                                   EL335
00015 *SECURITY.   *****************************************************EL335
00016 *            *                                                   *EL335
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL335
00018 *            *                                                   *EL335
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL335
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL335
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL335
00022 *            *                                                   *EL335
00023 *            *****************************************************EL335
00024                                                                   EL335
00025 *REMARKS.                                                         EL335
00026                                                                   EL335
00027 *         THIS PROGRAM IS USED TO LOAD THE BASE CLUSTERS AND      EL335
00028 *       BUILD A WORK FILE FOR LOADING THE ALTERNATE INDEXES.      EL335
00029                                                                   EL335
00030      EJECT                                                        EL335
00031  ENVIRONMENT DIVISION.                                            EL335
00032  CONFIGURATION SECTION.                                           EL335
00033  SPECIAL-NAMES.                                                   EL335
00034      C02 IS LCP-CH2                                               EL335
00035      C03 IS LCP-CH3                                               EL335
00036      C04 IS LCP-CH4                                               EL335
00037      C05 IS LCP-CH5                                               EL335
00038      C06 IS LCP-CH6                                               EL335
00039      C07 IS LCP-CH7                                               EL335
00040      C08 IS LCP-CH8                                               EL335
00041      C09 IS LCP-CH9                                               EL335
00042      C10 IS LCP-CH10                                              EL335
00043      C11 IS LCP-CH11                                              EL335
00044      C12 IS LCP-CH12                                              EL335
00045      S01 IS LCP-P01                                               EL335
00046      S02 IS LCP-P02.                                              EL335
00047                                                                   EL335
00048  INPUT-OUTPUT SECTION.                                            EL335
00049                                                                   EL335
00050  FILE-CONTROL.                                                    EL335
00051                                                                   EL335
00052      SELECT ELARCH-INPUT-FILE                                     EL335
00053          ASSIGN TO SYS010-UT-2400-S-SYS010.                       EL335
00054                                                                   EL335
00055      SELECT ELARCH-MASTER-FILE                                    EL335
00056          ASSIGN TO SYS023-FBA1-ELARCH                             EL335
00057          ORGANIZATION IS INDEXED                                  EL335
00058          ACCESS IS DYNAMIC                                        EL335
00059          RECORD KEY IS LA-CONTROL-PRIMARY IN ELARCH-MASTER-FILE   EL335
00060          FILE STATUS IS ELARCH-FILE-STATUS.                       EL335
00061                                                                   EL335
00062      SELECT ELCERT-INPUT-FILE                                     EL335
00063          ASSIGN TO SYS010-UT-2400-S-SYS010.                       EL335
00064                                                                   EL335
00065      SELECT ELCERT-MASTER-FILE                                    EL335
00066          ASSIGN TO SYS024-FBA1-ELCERT                             EL335
00067          ORGANIZATION IS INDEXED                                  EL335
00068          ACCESS IS SEQUENTIAL                                     EL335
00069          RECORD KEY IS CM-CONTROL-PRIMARY IN ELCERT-MASTER-FILE   EL335
00070          FILE STATUS IS ELCERT-FILE-STATUS.                       EL335
00071                                                                   EL335
00072      SELECT ELMSTR-INPUT-FILE                                     EL335
00073          ASSIGN TO SYS010-UT-2400-S-SYS010.                       EL335
00074                                                                   EL335
00075      SELECT ELMSTR-MASTER-FILE                                    EL335
00076          ASSIGN TO SYS025-FBA1-ELMSTR                             EL335
00077          ORGANIZATION IS INDEXED                                  EL335
00078          ACCESS IS SEQUENTIAL                                     EL335
00079          RECORD KEY IS CL-CONTROL-PRIMARY IN ELMSTR-MASTER-FILE   EL335
00080          FILE STATUS IS ELMSTR-FILE-STATUS.                       EL335
00081                                                                   EL335
00082      SELECT ERACCT-INPUT-FILE                                     EL335
00083          ASSIGN TO SYS010-UT-2400-S-SYS010.                       EL335
00084                                                                   EL335
00085      SELECT ERACCT-MASTER-FILE                                    EL335
00086          ASSIGN TO SYS026-FBA1-ERACCT                             EL335
00087          ORGANIZATION IS INDEXED                                  EL335
00088          ACCESS IS DYNAMIC                                        EL335
00089          RECORD KEY IS AM-CONTROL-PRIMARY IN ERACCT-MASTER-FILE   EL335
00090          FILE STATUS IS ERACCT-FILE-STATUS.                       EL335
00091                                                                   EL335
00092      SELECT ERPNDB-INPUT-FILE                                     EL335
00093          ASSIGN TO SYS010-UT-2400-S-SYS010.                       EL335
00094                                                                   EL335
00095      SELECT ERPNDB-MASTER-FILE                                    EL335
00096          ASSIGN TO SYS027-FBA1-ERPNDB                             EL335
00097          ORGANIZATION IS INDEXED                                  EL335
00098          ACCESS IS DYNAMIC                                        EL335
00099          RECORD KEY IS PB-CONTROL-PRIMARY IN ERPNDB-MASTER-FILE   EL335
00100          FILE STATUS IS ERPNDB-FILE-STATUS.                       EL335
00101                                                                   EL335
00102      SELECT CARD-FILE                                             EL335
00103          ASSIGN TO SYS009-UR-2540R-S-SYS009.                      EL335
00104                                                                   EL335
00105      SELECT COMPANY-SORT-WORK-FILE                                EL335
00106          ASSIGN TO  SYS001-UT-FBA1-S-SORTWK1.                     EL335
00107                                                                   EL335
00108      SELECT SORT-WORK-FILE                                        EL335
00109          ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.                      EL335
00110                                                                   EL335
00111      SELECT ELCNTL-BACKUP-FILE                                    EL335
00112          ASSIGN TO SYS011-UT-FBA1-S-SYS011.                       EL335
00113                                                                   EL335
00114      SELECT COMPANY-NAME-WORK-FILE                                EL335
00115          ASSIGN TO SYS012-UT-FBA1-S-SYS012.                       EL335
00116                                                                   EL335
00117      SELECT SORT-OUTPUT-FILE                                      EL335
00118          ASSIGN TO SYS018-UT-2400-S-SYS018.                       EL335
00119                                                                   EL335
00120      SELECT SORT-OUTPUT-FILE2                                     EL335
00121          ASSIGN TO SYS017-UT-2400-S-SYS017.                       EL335
00122                                                                   EL335
00123      SELECT CERT-INDEX-2                                          EL335
00124          ASSIGN TO SYS002-UT-2400-S-SYS002.                       EL335
00125                                                                   EL335
00126      SELECT CERT-INDEX-3                                          EL335
00127          ASSIGN TO SYS003-UT-2400-S-SYS003.                       EL335
00128                                                                   EL335
00129      SELECT CERT-INDEX-5                                          EL335
00130          ASSIGN TO SYS005-UT-2400-S-SYS005.                       EL335
00131                                                                   EL335
00132      SELECT CERT-INDEX-6                                          EL335
00133          ASSIGN TO SYS006-UT-2400-S-SYS006.                       EL335
00134                                                                   EL335
00135      SELECT PRNTR                                                 EL335
00136          ASSIGN TO SYS008-UR-1403-S-SYS008.                       EL335
00137                                                                   EL335
00138      EJECT                                                        EL335
00139  DATA DIVISION.                                                   EL335
00140                                                                   EL335
00141  FILE SECTION.                                                    EL335
00142                                                                   EL335
00143  FD  ELARCH-INPUT-FILE                                            EL335
00144      BLOCK CONTAINS 0 RECORDS
00145      RECORDING MODE F.                                            EL335
00146                                                                   EL335
00147  01  ELARCH-INPUT-RECORD            PIC X(90).                    EL335
00148                                                                   EL335
00149  FD  ELARCH-MASTER-FILE.                                          EL335
00150                                                                   EL335
00151                     COPY ELCARCH.                                 EL335
00152                                                                   EL335
00153  FD  ELCERT-INPUT-FILE                                            EL335
00154      BLOCK CONTAINS 0 RECORDS
00155      RECORDING MODE F.                                            EL335
00156                                                                   EL335
00157  01  ELCERT-INPUT-RECORD            PIC X(450).                   EL335
00158                                                                   EL335
00159  FD  ELCERT-MASTER-FILE.                                          EL335
00160                                                                   EL335
00161                     COPY ELCCERT.                                 EL335
00162                                                                   EL335
00163      EJECT                                                        EL335
00164  FD  ELCNTL-BACKUP-FILE                                           EL335
00165      BLOCK CONTAINS 0 RECORDS
00166      RECORDING MODE F.                                            EL335
00167                                                                   EL335
00168                   COPY ELCCNTL.                                   EL335
00169                                                                   EL335
00170  FD  ELMSTR-INPUT-FILE                                            EL335
00171      BLOCK CONTAINS 0 RECORDS
00172      RECORDING MODE F.                                            EL335
00173                                                                   EL335
00174  01  ELMSTR-INPUT-RECORD            PIC X(350).                   EL335
00175                                                                   EL335
00176  FD  ELMSTR-MASTER-FILE.                                          EL335
00177                                                                   EL335
00178                         COPY ELCMSTR.                             EL335
00179                                                                   EL335
00180      EJECT                                                        EL335
00181  FD  ERACCT-INPUT-FILE                                            EL335
00182      BLOCK CONTAINS 0 RECORDS
00183      RECORDING MODE F.                                            EL335
00184                                                                   EL335
00185  01  ERACCT-INPUT-RECORD            PIC X(2000).                  EL335
00186                                                                   EL335
00187  FD  ERACCT-MASTER-FILE.                                          EL335
00188                                                                   EL335
00189                         COPY ERCACCT.                             EL335
00190                                                                   EL335
00191      EJECT                                                        EL335
00192  FD  ERPNDB-INPUT-FILE                                            EL335
00193      BLOCK CONTAINS 0 RECORDS
00194      RECORDING MODE F.                                            EL335
00195                                                                   EL335
00196  01  ERPNDB-INPUT-RECORD            PIC X(585).                   EL335
00197                                                                   EL335
00198  FD  ERPNDB-MASTER-FILE.                                          EL335
00199                                                                   EL335
00200                         COPY ERCPNDB.                             EL335
00201                                                                   EL335
00202      EJECT                                                        EL335
00203  SD  SORT-WORK-FILE.                                              EL335
00204                                                                   EL335
00205  01  SORT-RECORD                     PIC X(53).                   EL335
00206                                                                   EL335
00207  SD  COMPANY-SORT-WORK-FILE.                                      EL335
00208                                                                   EL335
00209  01  COMPANY-SORT-WORK-RECORD.                                    EL335
00210      05  SWR-COMPANY-CODE            PIC X.                       EL335
00211      05  SWR-COMPANY-ID              PIC X(3).                    EL335
00212      05  SWR-COMPANY-NAME            PIC X(30).                   EL335
00213                                                                   EL335
00214  FD  COMPANY-NAME-WORK-FILE COPY ELC335F2.                        EL335
00215                                                                   EL335
00216  FD  SORT-OUTPUT-FILE COPY ELC335F1.                              EL335
00217                                                                   EL335
00218  FD  SORT-OUTPUT-FILE2 COPY ELC335F1 REPLACING                    EL335
00219      SORT-OUTPUT-RECORD          BY  SORT-OUTPUT-RECORD2          EL335
00220      SOR-AIX-NUMBER              BY  SOR2-AIX-NUMBER              EL335
00221      SOR-KEYS                    BY  SOR2-KEYS2                   EL335
00222      SOR-AIX-KEY                 BY  SOR2-AIX-KEY                 EL335
00223      SOR-PRIME-KEY               BY  SOR2-PRIME-KEY               EL335
00224      SOR-AIX-KEY2                BY  SOR2-AIX-KEY2                EL335
00225      SOR-PRIME-KEY2              BY  SOR2-PRIME-KEY2.             EL335
00226                                                                   EL335
00227  FD  CARD-FILE                                                    EL335
00228      RECORDING MODE F.                                            EL335
00229                                                                   EL335
00230  01  CARD-RECORD.                                                 EL335
00231      05  CR-OPTION                   PIC X(3).                    EL335
00232      05  CR-AIX.                                                  EL335
00233          10  CR-AIX02                PIC X.                       EL335
00234          10  CR-AIX03                PIC X.                       EL335
00235          10  CR-AIX04                PIC X.                       EL335
00236          10  CR-AIX05                PIC X.                       EL335
00237          10  CR-AIX06                PIC X.                       EL335
00238      05  CR-DATASET                  PIC X(6).                    EL335
00239      05  CR-RECORDS-INPUT-LIMIT      PIC S9(9).                   EL335
00240      05  CR-KEYRANGE-COUNT           PIC S9(9).                   EL335
00241      05  FILLER                      PIC X(48).                   EL335
00242                                                                   EL335
00243                                                                   EL335
00244  FD  PRNTR                   COPY ELCPRTFD SUPPRESS.              EL335
00245                                                                   EL335
00246  FD  CERT-INDEX-2                                                 EL335
00247      BLOCK CONTAINS 0 RECORDS
00248      RECORDING MODE F.                                            EL335
00249                                                                   EL335
00250  01  AIX02-RECORD                    PIC X(53).                   EL335
00251                                                                   EL335
00252  FD  CERT-INDEX-3                                                 EL335
00253      BLOCK CONTAINS 0 RECORDS
00254      RECORDING MODE F.                                            EL335
00255                                                                   EL335
00256  01  AIX03-RECORD                    PIC X(53).                   EL335
00257                                                                   EL335
00258  FD  CERT-INDEX-5                                                 EL335
00259      BLOCK CONTAINS 0 RECORDS
00260      RECORDING MODE F.                                            EL335
00261                                                                   EL335
00262  01  AIX05-RECORD                    PIC X(53).                   EL335
00263                                                                   EL335
00264  FD  CERT-INDEX-6                                                 EL335
00265      BLOCK CONTAINS 0 RECORDS
00266      RECORDING MODE F.                                            EL335
00267                                                                   EL335
00268  01  AIX06-RECORD                    PIC X(53).                   EL335
00269                                                                   EL335
00270      EJECT                                                        EL335
00271  WORKING-STORAGE SECTION.                                         EL335
00272  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL335
00273  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.   EL335
00274  77  LCP-ONCTR-02                  PIC S9(8) COMP-3 VALUE ZERO.   EL335
00275  77  LCP-ONCTR-03                  PIC S9(8) COMP-3 VALUE ZERO.   EL335
00276  77  LCP-ONCTR-04                  PIC S9(8) COMP-3 VALUE ZERO.   EL335
00277  77  LCP-ONCTR-05                  PIC S9(8) COMP-3 VALUE ZERO.   EL335
00278  77  LCP-ASA                       PIC X.                         EL335
00279                                                                   EL335
00280  77  FILLER  PIC X(32) VALUE '********************************'.  EL335
00281  77  FILLER  PIC X(32) VALUE '    EL335 WORKING-STORAGE       '.  EL335
00282  77  FILLER  PIC X(32) VALUE '*********VMOD=2.015 ************'.  EL335
00283                                                                   EL335
00284  01  FILLER                          COMP-3.                      EL335
00285    02  FILLER.                                                    EL335
00286      05  WS-RECORDS-INPUT-LIMIT      PIC S9(9)       VALUE ZERO.  EL335
00287      05  WS-KEYRANGE-LIMIT           PIC S9(9)       VALUE ZERO.  EL335
00288      05  WS-RECORDS-INPUT            PIC S9(9)       VALUE ZERO.  EL335
00289      05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.  EL335
00290      05  WS-RECORDS-OUTPUT           PIC S9(9)       VALUE ZERO.  EL335
00291      05  WS-RECORDS-RELEASED         PIC S9(9)       VALUE ZERO.  EL335
00292      05  WS-RECORDS-RETURNED         PIC S9(9)       VALUE ZERO.  EL335
00293      05  WS-WORK                     PIC S9(9)       VALUE ZERO.  EL335
00294      05  WS-REMAINDER                PIC S9(9)       VALUE ZERO.  EL335
00295      05  WS-OUT-OF-SEQUENCE-RECORDS  PIC S9(9)       VALUE ZERO.  EL335
00296      05  WS-OVERFLOW-COUNT           PIC S9(9)       VALUE ZERO.  EL335
00297      05  WS-ZERO                     PIC S9          VALUE ZERO.  EL335
00298      05  WS-OPTION                   PIC S9          VALUE ZERO.  EL335
00299      05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   EL335
00300                                                                   EL335
00301    02  WS-MESSAGE-OCCURRENCE.                                     EL335
00302      05  ELARCH-MESSAGE-OCCURRENCE   PIC S9(5)      VALUE +10000. EL335
00303      05  ELCERT-MESSAGE-OCCURRENCE   PIC S9(5)      VALUE +50000. EL335
00304      05  ELMSTR-MESSAGE-OCCURRENCE   PIC S9(5)      VALUE +10000. EL335
00305      05  ERACCT-MESSAGE-OCCURRENCE   PIC S9(5)      VALUE +10000. EL335
00306      05  ERPNDB-MESSAGE-OCCURRENCE   PIC S9(5)      VALUE +10000. EL335
00307    02  WS-MSG-OCCURRENCE             REDEFINES                    EL335
00308        WS-MESSAGE-OCCURRENCE         PIC S9(5)      OCCURS 5.     EL335
00309                                                                   EL335
00310  01  FILLER                          COMP SYNC.                   EL335
00311      05  WS-RETURN-CODE              PIC S9(4)       VALUE ZERO.  EL335
00312      05  WS-DATASET                  PIC S9(4)       VALUE ZERO.  EL335
00313        88  PROCESSING-ELARCH-FILE                    VALUE +1.    EL335
00314        88  PROCESSING-ELCERT-FILE                    VALUE +2.    EL335
00315        88  PROCESSING-ELMSTR-FILE                    VALUE +3.    EL335
00316        88  PROCESSING-ERACCT-FILE                    VALUE +4.    EL335
00317        88  PROCESSING-ERPNDB-FILE                    VALUE +5.    EL335
00318                                                                   EL335
00319  01  FILLER.                                                      EL335
00320      05  HAN-WORK-KEY.                                            EL335
00321          10  FILLER                  PIC X(01).                   EL335
00322          10  HAN-LAST-NAME.                                       EL335
00323              15  HAN-EFF-DT          PIC X(06).                   EL335
00324              15  FILLER              PIC X(03).                   EL335
00325              15  HAN-SEQ-NO          PIC X(06).                   EL335
00326          10  FILLER                  PIC X(02).                   EL335
00327      05  WS-HAN-SEQ-NO               PIC 9(06) VALUE ZEROS.       EL335
00328      05  WS-COMPANY-NUMBER           PIC S9(4) COMP  VALUE ZERO.  EL335
00329                                                                   EL335
00330      05  FILLER REDEFINES WS-COMPANY-NUMBER.                      EL335
00331          10  FILLER                  PIC X.                       EL335
00332          10  WS-COMPANY-CD           PIC X.                       EL335
00333                                                                   EL335
00334      05  WS-HEX-17    COMP           PIC S9(4)       VALUE +105.  EL335
00335      05  FILLER REDEFINES WS-HEX-17.                              EL335
00336          10  FILLER                  PIC X.                       EL335
00337          10  WS-NCB                  PIC X.                       EL335
00338                                                                   EL335
00339      05  WS-HEX-6C    COMP           PIC S9(4)       VALUE +108.  EL335
00340      05  FILLER REDEFINES WS-HEX-6C.                              EL335
00341          10  FILLER                  PIC X.                       EL335
00342          10  WS-HAN                  PIC X.                       EL335
00343                                                                   EL335
00344      05  WS-LAST-COMPANY-CD          PIC X      VALUE LOW-VALUES. EL335
00345                                                                   EL335
00346      05  ELARCH-FILE-STATUS          PIC XX          VALUE ZERO.  EL335
00347      05  ELCERT-FILE-STATUS          PIC XX          VALUE ZERO.  EL335
00348      05  ELMSTR-FILE-STATUS          PIC XX          VALUE ZERO.  EL335
00349      05  ERACCT-FILE-STATUS          PIC XX          VALUE ZERO.  EL335
00350      05  ERPNDB-FILE-STATUS          PIC XX          VALUE ZERO.  EL335
00351                                                                   EL335
00352      05  ELARCH-FILE-NAME            PIC X(8) VALUE 'ELARCH'.     EL335
00353      05  ELCERT-FILE-NAME            PIC X(8) VALUE 'ELCERT'.     EL335
00354      05  ELMSTR-FILE-NAME            PIC X(8) VALUE 'ELMSTR'.     EL335
00355      05  ERACCT-FILE-NAME            PIC X(8) VALUE 'ERACCT'.     EL335
00356      05  ERPNDB-FILE-NAME            PIC X(8) VALUE 'ERPNDB'.     EL335
00357                                                                   EL335
00358                                                                   EL335
00359      05  WS-ABEND-MESSAGE            PIC X(80) VALUE SPACES.      EL335
00360      05  WS-ABEND-FILE-STATUS        PIC XX    VALUE ZERO.        EL335
00361                                                                   EL335
00362      05  WS-AIX                                     VALUE ALL 'Y'.EL335
00363          10  WS-AIX02                PIC X.                       EL335
00364          10  WS-AIX03                PIC X.                       EL335
00365          10  WS-AIX04                PIC X.                       EL335
00366          10  WS-AIX05                PIC X.                       EL335
00367          10  WS-AIX06                PIC X.                       EL335
00368                                                                   EL335
00369      05  WS-FILE-ERROR-MESSAGE.                                   EL335
00370          10  FILLER                  PIC X           VALUE ZERO.  EL335
00371          10  WS-FEM-ERROR-MESSAGE   PIC X(24)       VALUE         EL335
00372              'ERROR OCCURED OPENING - '.                          EL335
00373          10  WS-FEM-FILE-NAME        PIC X(8).                    EL335
00374                                                                   EL335
00375  01  SORT-WORK-RECORD.                                            EL335
00376      05  SWR-AIX-NUMBER              PIC S9(4) COMP.              EL335
00377      05  SWR-KEYS.                                                EL335
00378          10  SWR-AIX-KEY             PIC X(18).                   EL335
00379          10  SWR-PRIME-KEY           PIC X(33).                   EL335
00380      05  FILLER REDEFINES SWR-KEYS.                               EL335
00381          10  SWR-AIX-KEY2            PIC X(36).                   EL335
00382          10  SWR-PRIME-KEY2          PIC X(15).                   EL335
00383      05  FILLER REDEFINES SWR-KEYS.                               EL335
00384          10  SWR-AIX-KEY3            PIC X(29).                   EL335
00385          10  SWR-PRIME-KEY3          PIC X(22).                   EL335
00386      05  FILLER REDEFINES SWR-KEYS.                               EL335
00387          10  SWR-COMPANY-CD          PIC X(01).                   EL335
00388          10  FILLER                  PIC X(50).                   EL335
00389                                                                   EL335
00390             COPY ELC335W1.                                        EL335
00391                                                                   EL335
00392  01  WS-DETAIL-LINE1                 PIC X(133) VALUE SPACES.     EL335
00393                                                                   EL335
00394  01  FILLER REDEFINES WS-DETAIL-LINE1.                            EL335
00395      05  FILLER                      PIC XX.                      EL335
00396      05  WS-D2-MESSAGE               PIC X(20).                   EL335
00397      05  FILLER                      PIC X(5).                    EL335
00398      05  WS-D2-COUNT                 PIC ZZZ,ZZZ,ZZ9-.            EL335
00399                                                                   EL335
00400  01  FILLER REDEFINES WS-DETAIL-LINE1.                            EL335
00401      05  FILLER                      PIC XX.                      EL335
00402      05  WS-D3-COMPANY-NUMBER        PIC ZZZ9.                    EL335
00403      05  FILLER                      PIC XX.                      EL335
00404      05  WS-D3-COMPANY-CD1           PIC X.                       EL335
00405      05  WS-D3-COMPANY-CD2           PIC X.                       EL335
00406      05  FILLER                      PIC XX.                      EL335
00407      05  WS-D3-COMPANY-ID            PIC X(3).                    EL335
00408      05  FILLER                      PIC XX.                      EL335
00409      05  WS-D3-COMPANY-NAME          PIC X(30).                   EL335
00410      05  FILLER                      PIC XX.                      EL335
00411      05  WS-D3-MESSAGE               PIC X(20).                   EL335
00412      05  FILLER                      PIC X(5).                    EL335
00413      05  WS-D3-COUNT                 PIC ZZZ,ZZZ,ZZ9-.            EL335
00414                                                                   EL335
00415  01  WS-DISPLAY1.                                                 EL335
00416      05  WS-D1-TIME                  PIC 99B99B99.                EL335
00417      05  FILLER                      PIC X           VALUE SPACES.EL335
00418      05  WS-D1-MESSAGE               PIC X(30)       VALUE        EL335
00419          'RECORDS INPUT='.                                        EL335
00420      05  WS-D1-COUNT                 PIC ZZZ,ZZZ,ZZ9-.            EL335
00421                                                                   EL335
00422  01  WS-DATE-AND-TIME.                                            EL335
00423      12  WS-ACCEPT-DATE.                                          EL335
00424          16  WS-AD-YY                PIC  99.                     EL335
00425          16  WS-AD-MM                PIC  99.                     EL335
00426          16  WS-AD-DD                PIC  99.                     EL335
00427      12  WS-CURRENT-DATE.                                         EL335
00428          16  WS-CD-MM                PIC  99.                     EL335
00429          16  FILLER                  PIC  X          VALUE '/'.   EL335
00430          16  WS-CD-DD                PIC  99.                     EL335
00431          16  FILLER                  PIC  X          VALUE '/'.   EL335
00432          16  WS-CD-YY                PIC  99.                     EL335
00433      12  WS-TIME-OF-DAY.                                          EL335
00434          16  WS-TIME                 PIC  9(6).                   EL335
00435          16  WS-HUN-SEC              PIC  99.                     EL335
00436      EJECT                                                        EL335
00437  PROCEDURE DIVISION.                                              EL335
00469 *        open output CERT-INDEX-5                                 EL335
00470 *        open output CERT-INDEX-6.                                EL335
00467 *        OPEN OUTPUT CERT-INDEX-2                                 EL335
00468 *        open output CERT-INDEX-3                                 EL335


00438                                                                   EL335
00439      ACCEPT WS-ACCEPT-DATE FROM DATE.                             EL335
00440                                                                   EL335
00441      MOVE WS-AD-YY               TO  WS-CD-YY.                    EL335
00442      MOVE WS-AD-MM               TO  WS-CD-MM.                    EL335
00443      MOVE WS-AD-DD               TO  WS-CD-DD.                    EL335
00444                                                                   EL335
00445      OPEN INPUT CARD-FILE.                                        EL335
00449                                                                   EL335
00450                                                                   EL335
00451 *    SORT COMPANY-SORT-WORK-FILE                                  EL335
00452 *        ON ASCENDING KEY SWR-COMPANY-CODE                        EL335
00453 *            INPUT PROCEDURE  4000-SORT-INPUT-PROCEDURE           EL335
00454 *            OUTPUT PROCEDURE 4100-SORT-OUTPUT-PROCEDURE.         EL335
00455 *                                                                 EL335
00456 *    IF SORT-RETURN GREATER ZERO                                  EL335
00457 *        MOVE SORT-RETURN                 TO  WS-RETURN-CODE      EL335
00458 *        MOVE 'COMPANY TABLE SORT FAILED' TO  WS-ABEND-MESSAGE    EL335
00459 *        GO TO ABEND-PGM.                                         EL335
00460                                                                   EL335
00461  MLS-010.                                                         EL335
00462      READ CARD-FILE                                               EL335
00463          AT END                                                   EL335
00464              GO TO MLS-EXIT.                                      EL335
           display ' card record ' card-record
00465                                                                   EL335
00466 *    IF CR-DATASET = ELCERT-FILE-NAME                             EL335
00467 *        OPEN OUTPUT CERT-INDEX-2                                 EL335
00468 *                    CERT-INDEX-3                                 EL335
00469 *                    CERT-INDEX-5                                 EL335
00470 *                    CERT-INDEX-6.                                EL335
00471                                                                   EL335
00472      IF CR-OPTION = 'YES'                                         EL335
00473          MOVE +1                  TO  WS-OPTION.                  EL335
00474                                                                   EL335
00475      MOVE CR-AIX                  TO  WS-AIX.                     EL335
00476      INSPECT WS-AIX CONVERTING SPACES TO 'Y'.                     EL335
00477                                                                   EL335
00478      IF CR-RECORDS-INPUT-LIMIT NUMERIC                            EL335
00479          MOVE CR-RECORDS-INPUT-LIMIT  TO  WS-RECORDS-INPUT-LIMIT  EL335
00480        ELSE                                                       EL335
00481          MOVE +999999999              TO  WS-RECORDS-INPUT-LIMIT. EL335
00482                                                                   EL335
00483      IF CR-DATASET = SPACES OR ZEROS                              EL335
00484         MOVE ELCERT-FILE-NAME TO CR-DATASET.                      EL335
00485                                                                   EL335
00486      IF CR-DATASET = ELARCH-FILE-NAME                             EL335
00487          MOVE +1                 TO  WS-DATASET                   EL335
00488          GO TO MLS-020.                                           EL335
00489                                                                   EL335
00490      IF CR-DATASET = ELCERT-FILE-NAME                             EL335
00491          MOVE +2                 TO  WS-DATASET                   EL335
00492          GO TO MLS-020.                                           EL335
00493                                                                   EL335
00494      IF CR-DATASET = ELMSTR-FILE-NAME                             EL335
00495          MOVE +3                 TO  WS-DATASET                   EL335
00496          GO TO MLS-020.                                           EL335
00497                                                                   EL335
00498      IF CR-DATASET = ERACCT-FILE-NAME                             EL335
00499          MOVE +4                 TO  WS-DATASET                   EL335
00500          GO TO MLS-020.                                           EL335
00501                                                                   EL335
00502      IF CR-DATASET = ERPNDB-FILE-NAME                             EL335
00503          MOVE +5                 TO  WS-DATASET                   EL335
00504          GO TO MLS-020.                                           EL335
00505                                                                   EL335
00506      MOVE 'INVALID DATASET NAME'  TO  WS-ABEND-MESSAGE            EL335
00507                                                                   EL335
00508      GO TO ABEND-PGM.                                             EL335
00509                                                                   EL335
00510      EJECT                                                        EL335
00511  MLS-020.                                                         EL335

00445 *    OPEN INPUT CARD-FILE                                         EL335
00446      open input ELCNTL-BACKUP-FILE                                EL335
00447           OUTPUT PRNTR                                            EL335
00448                  COMPANY-NAME-WORK-FILE.                          EL335





00451      SORT COMPANY-SORT-WORK-FILE                                  EL335
00452          ON ASCENDING KEY SWR-COMPANY-CODE                        EL335
00453              INPUT PROCEDURE  4000-SORT-INPUT-PROCEDURE           EL335
00454              OUTPUT PROCEDURE 4100-SORT-OUTPUT-PROCEDURE.         EL335
00455                                                                   EL335
00456      IF SORT-RETURN GREATER ZERO                                  EL335
00457          MOVE SORT-RETURN                 TO  WS-RETURN-CODE      EL335
00458          MOVE 'COMPANY TABLE SORT FAILED' TO  WS-ABEND-MESSAGE    EL335
00459          GO TO ABEND-PGM.                                         EL335
00460                                                                   EL335



00512      PERFORM 3000-OPEN-FILES.                                     EL335
00513                                                                   EL335
00514      IF CR-OPTION = 'CNT'                                         EL335
00515          PERFORM 1000-SORT-INPUT-PROCEDURE                        EL335
00516        ELSE                                                       EL335
00517          IF CR-DATASET = ELCERT-FILE-NAME                         EL335
00518              PERFORM 1000-SORT-INPUT-PROCEDURE                    EL335
00519          ELSE                                                     EL335
NTTDel*            MOVE +7000000       TO SORT-FILE-SIZE                EL335
NTTIns*            MOVE +2097152       TO SORT-CORE-SIZE                EL335
00522              SORT SORT-WORK-FILE                                  EL335
00523                  ON ASCENDING KEY SORT-RECORD                     EL335
00524                  INPUT PROCEDURE  1000-SORT-INPUT-PROCEDURE       EL335
00525                  OUTPUT PROCEDURE 5000-SORT-OUTPUT-PROCEDURE      EL335
00526                                                                   EL335
00527              IF SORT-RETURN NOT = ZERO                            EL335
00528                  MOVE SORT-RETURN    TO  WS-RETURN-CODE           EL335
00529                  MOVE 'SORT FAILED'  TO  WS-ABEND-MESSAGE         EL335
00530                  GO TO ABEND-PGM.                                 EL335
00531                                                                   EL335
00532      MOVE '0'                    TO  WS-DETAIL-LINE1.             EL335
00533                                                                   EL335
00534      MOVE 'RECORDS INPUT'        TO  WS-D2-MESSAGE.               EL335
00535      MOVE WS-RECORDS-INPUT       TO  WS-D2-COUNT.                 EL335
00536      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL335
00537      MOVE P-CTL TO LCP-ASA                                        EL335
00538      PERFORM LCP-WRITE-POS-PRT                                    EL335
00539          THRU LCP-WRITE-END-PRT.                                  EL335
00540                                                                   EL335
00541      MOVE 'RECORDS OUTPUT'       TO  WS-D2-MESSAGE.               EL335
00542      MOVE WS-RECORDS-OUTPUT      TO  WS-D2-COUNT.                 EL335
00543      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL335
00544      MOVE P-CTL TO LCP-ASA                                        EL335
00545      PERFORM LCP-WRITE-POS-PRT                                    EL335
00546          THRU LCP-WRITE-END-PRT.                                  EL335
00547                                                                   EL335
00548      MOVE 'RECORDS RELEASED'     TO  WS-D2-MESSAGE.               EL335
00549      MOVE WS-RECORDS-RELEASED    TO  WS-D2-COUNT.                 EL335
00550      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL335
00551      MOVE P-CTL TO LCP-ASA                                        EL335
00552      PERFORM LCP-WRITE-POS-PRT                                    EL335
00553          THRU LCP-WRITE-END-PRT.                                  EL335
00554                                                                   EL335
00555      GO TO MLS-010.                                               EL335
00556                                                                   EL335
00557  MLS-EXIT.                                                        EL335
00558      CLOSE PRNTR                                                  EL335
00559          CARD-FILE                                                EL335
00560          ELCNTL-BACKUP-FILE                                       EL335
00561          COMPANY-NAME-WORK-FILE.                                  EL335
00562                                                                   EL335
00563      GOBACK.                                                      EL335
00564                                                                   EL335
00565      EJECT                                                        EL335
00566  1000-SORT-INPUT-PROCEDURE SECTION.                               EL335
00567                                                                   EL335
00568      MOVE 'RECORDS INPUT='       TO  WS-D1-MESSAGE.               EL335
00569                                                                   EL335
00570      MOVE ZERO                   TO  WS-RECORDS-OUTPUT            EL335
00571                                      WS-RECORDS-INPUT             EL335
00572                                      WS-RECORDS-RELEASED          EL335
00573                                      WS-RECORD-COUNT.             EL335
00574                                                                   EL335
00575  1100-SIP.                                                        EL335
00576      GO TO 1200-SIP-ELARCH                                        EL335
00577            1300-SIP-ELCERT                                        EL335
00578            1400-SIP-ELMSTR                                        EL335
00579            1500-SIP-ERACCT                                        EL335
00580            1600-SIP-ERPNDB DEPENDING ON WS-DATASET.               EL335
00581                                                                   EL335
00582  1200-SIP-ELARCH. COPY ELC335P1 REPLACING                         EL335
00583      ELCERT-INPUT-FILE           BY  ELARCH-INPUT-FILE            EL335
00584      CERTIFICATE-MASTER          BY  LETTER-ARCHIVE               EL335
00585      ELCERT-MASTER-FILE          BY  ELARCH-MASTER-FILE           EL335
00586      ELCERT-MESSAGE-OCCURRENCE   BY  ELARCH-MESSAGE-OCCURRENCE    EL335
00587      CM-COMPANY-CD               BY  LA-COMPANY-CD.               EL335
00588                                                                   EL335
00589      MOVE +1                     TO  SWR-AIX-NUMBER.              EL335
00590      MOVE LA-CONTROL-BY-TYPE     IN ELARCH-MASTER-FILE            EL335
00591                                  TO  SWR-AIX-KEY.                 EL335
00592      MOVE LA-CONTROL-PRIMARY IN  ELARCH-MASTER-FILE               EL335
00593                                  TO  SWR-PRIME-KEY.               EL335
00594      PERFORM 2000-RELEASE.                                        EL335
00595                                                                   EL335
00596  1290-SIP-ELARCH. COPY ELC335P2 REPLACING                         EL335
00597      CERTIFICATE-MASTER          BY  LETTER-ARCHIVE               EL335
00598      ELCERT-MASTER-FILE          BY  ELARCH-MASTER-FILE           EL335
00599      ELCERT-FILE-STATUS          BY  ELARCH-FILE-STATUS           EL335
00600      ELCERT-FILE-NAME            BY  ELARCH-FILE-NAME.            EL335
00601                                                                   EL335
00602      EJECT                                                        EL335
00603  1300-SIP-ELCERT. *> COPY ELC335P1.

00001 ***************************************************************** 04/19/98
00002 *                                                               * ELC335P1
00003 *                            ELC335P1.                           *   LV002
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE               CL**2
00005 *                            VMOD=2.003                          *   CL**2
00006 *****************************************************************.ELC335P1
00007                                                                   ELC335P1
00008      READ ELCERT-INPUT-FILE INTO CERTIFICATE-MASTER               ELC335P1
00009          AT END                                                   ELC335P1
00010              GO TO 1900-SIP.                                      ELC335P1
00011                                                                   ELC335P1
00012      IF LCP-ONCTR-01 = +0                                         ELC335P1
00013          MOVE +1                 TO  LCP-ONCTR-01                 ELC335P1
00014          MOVE CM-COMPANY-CD      TO  WS-LAST-COMPANY-CD.          ELC335P1
00015                                                                   ELC335P1
00016      IF CM-COMPANY-CD NOT = WS-LAST-COMPANY-CD                    ELC335P1
00017          PERFORM 2200-PRINT-COMPANY-TOTALS                        ELC335P1
00018          MOVE CM-COMPANY-CD      TO  WS-LAST-COMPANY-CD.          ELC335P1
00019                                                                   ELC335P1
00020      ADD +1  TO  WS-RECORD-COUNT                                  ELC335P1
00021                  WS-RECORDS-INPUT.                                ELC335P1
00022                                                                   ELC335P1
00023      IF WS-RECORDS-INPUT GREATER THAN WS-RECORDS-INPUT-LIMIT      ELC335P1
00024          GO TO 1900-SIP.                                          ELC335P1
00025                                                                   ELC335P1
00026      DIVIDE WS-RECORDS-INPUT BY WS-MSG-OCCURRENCE (WS-DATASET)    ELC335P1
00027          GIVING WS-WORK REMAINDER WS-REMAINDER                    ELC335P1
00028                                                                   ELC335P1
00029      IF WS-REMAINDER = ZERO                                       ELC335P1
00030          PERFORM 2300-DISPLAY-UPON-CONSOLE.                       ELC335P1
00604                                                                   EL335
00605      IF CM-INSURED-INITIALS = SPACES                              EL335
00606          MOVE '**'               TO  CM-INSURED-INITIALS.         EL335
00607                                                                   EL335
00608      IF CM-INSURED-LAST-NAME = SPACES                             EL335
00609          MOVE CM-CERT-NO         TO  CM-INSURED-LAST-NAME.        EL335
00610                                                                   EL335
00611      IF CM-SOC-SEC-NO = (SPACES OR ZERO OR                        EL335
00612                                 '1' OR '2' OR '3' OR '4' OR       EL335
00613                                 '000000000  ' OR                  EL335
00614                                 '   -  -    ')                    EL335
00615          MOVE CM-STATE             TO  CM-SSN-STATE               EL335
00616          MOVE CM-ACCOUNT-PRIME     TO  CM-SSN-ACCOUNT             EL335
00617          MOVE CM-INSURED-INITIALS  TO  CM-INSURED-INITIALS-A2     EL335
00618          MOVE CM-INSURED-LAST-NAME TO  CM-PART-LAST-NAME-A2.      EL335
00619                                                                   EL335
00620      IF WS-AIX02 = 'Y'                                            EL335
00621          IF CM-INSURED-LAST-NAME = SPACES            OR           EL335
00622                                    'BALANCE        ' OR           EL335
00623                                    'BALANCEL       ' OR           EL335
00624                                    'BALANCEH       ' OR           EL335
00625                                    'NO NAME        ' OR           EL335
00626                                    'VOID           ' OR           EL335
00627                                    'SUMMARY CERT   '              EL335
00628              NEXT SENTENCE                                        EL335
00629            ELSE                                                   EL335
00630              MOVE +1                     TO  SWR-AIX-NUMBER       EL335
00631              MOVE CM-CONTROL-BY-NAME     TO  SWR-AIX-KEY          EL335
00632              MOVE CM-CONTROL-PRIMARY     TO  SWR-PRIME-KEY        EL335
00633              PERFORM 2000-RELEASE.                                EL335
00634                                                                   EL335
00635      IF WS-AIX03 = 'Y'                                            EL335
00636          IF CM-SSN-STATE   = CM-STATE   AND                       EL335
00637             CM-SSN-ACCOUNT = CM-ACCOUNT-PRIME                     EL335
00638              NEXT SENTENCE                                        EL335
00639            ELSE                                                   EL335
00640              MOVE +2                     TO  SWR-AIX-NUMBER       EL335
00641              MOVE CM-CONTROL-BY-SSN      TO  SWR-AIX-KEY          EL335
00642              MOVE CM-CONTROL-PRIMARY     TO  SWR-PRIME-KEY        EL335
00643              PERFORM 2000-RELEASE.                                EL335
00644                                                                   EL335
00645      IF WS-AIX05 = 'Y'                                            EL335
00646          MOVE +4                     TO  SWR-AIX-NUMBER           EL335
00647          MOVE CM-CONTROL-BY-CERT-NO  TO  SWR-AIX-KEY              EL335
00648          MOVE CM-CONTROL-PRIMARY     TO  SWR-PRIME-KEY            EL335
00649          PERFORM 2000-RELEASE.                                    EL335
00650                                                                   EL335
00651      IF WS-AIX06 = 'Y'                                            EL335
00652          IF CM-MEMB-STATE = CM-STATE                              EL335
00653            AND CM-MEMB-ACCOUNT = CM-ACCOUNT                       EL335
00654              NEXT SENTENCE                                        EL335
00655            ELSE                                                   EL335
00656              MOVE +5                     TO  SWR-AIX-NUMBER       EL335
00657              MOVE CM-CONTROL-BY-MEMB     TO  SWR-AIX-KEY          EL335
00658              MOVE CM-CONTROL-PRIMARY     TO  SWR-PRIME-KEY        EL335
00659              PERFORM 2000-RELEASE.                                EL335
00660                                                                   EL335
00661  1390-SIP-ELCERT. *> COPY ELC335P2.
00001 ***************************************************************** 04/19/98
00002 *                                                               * ELC335P2
00003 *                            ELC335P2.                           *   LV002
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE               CL**2
00005 *                            VMOD=2.002                          *   CL**2
00006 *                                                               * ELC335P2
00007 *               WRITE ROUTINE FOR THE BUILD INDEX ROUTINE.      * ELC335P2
00008 *****************************************************************.ELC335P2
00009                                                                   ELC335P2
00010      IF WS-OPTION = +1                                            ELC335P2
00011          ADD +1  TO  WS-RECORDS-OUTPUT                            ELC335P2
00012          WRITE CERTIFICATE-MASTER IN ELCERT-MASTER-FILE           ELC335P2
00013          IF ELCERT-FILE-STATUS NOT = ZERO
                  display ' elcert - error - write ' elcert-file-status
                  display ' cert ' cm-record-id ' '
                  cm-control-primary(2:19) ' ' cm-cert-no ' '
                  cm-insured-last-name ' ' cm-soc-sec-no ' ' ws-aix
00014              MOVE 'ERROR OCCURED WRITE -' TO WS-FEM-ERROR-MESSAGE ELC335P2
00015              MOVE ELCERT-FILE-NAME        TO WS-FEM-FILE-NAME     ELC335P2
00016              MOVE WS-FILE-ERROR-MESSAGE   TO WS-ABEND-MESSAGE     ELC335P2
00017              MOVE ELCERT-FILE-STATUS      TO WS-ABEND-FILE-STATUS
                   perform 2300-DISPLAY-UPON-CONSOLE thru 2390-exit
                   MOVE WS-RECORDS-OUTPUT      TO  WS-D2-COUNT
                   display ' records output ' ws-d2-count
00018              PERFORM ABEND-PGM.                                   ELC335P2
00019                                                                   ELC335P2
00020      GO TO 1100-SIP.                                              ELC335P2
00021                                                                   ELC335P2


00662                                                                   EL335
00663      EJECT                                                        EL335
00664  1400-SIP-ELMSTR. COPY ELC335P1 REPLACING                         EL335
00665      ELCERT-INPUT-FILE           BY  ELMSTR-INPUT-FILE            EL335
00666      CERTIFICATE-MASTER          BY  CLAIM-MASTER                 EL335
00667      ELCERT-MASTER-FILE          BY  ELMSTR-MASTER-FILE           EL335
00668      ELCERT-MESSAGE-OCCURRENCE   BY  ELMSTR-MESSAGE-OCCURRENCE    EL335
00669      CM-COMPANY-CD               BY  CL-COMPANY-CD.               EL335
00670                                                                   EL335
00671      IF WS-AIX02 = 'Y'                                            EL335
00672        AND CL-INSURED-LAST-NAME NOT = SPACES                      EL335
00673          MOVE +1                     TO  SWR-AIX-NUMBER           EL335
00674          MOVE CL-CONTROL-BY-NAME     TO  SWR-AIX-KEY3             EL335
00675          MOVE CL-CONTROL-PRIMARY     TO  SWR-PRIME-KEY3           EL335
00676          PERFORM 2000-RELEASE.                                    EL335
00677                                                                   EL335
00678      IF CL-SOC-SEC-NO = (SPACES OR ZERO OR                        EL335
00679                          '1' OR '2' OR '3' OR '4' OR              EL335
00680                              '000000000  ' OR                     EL335
00681                              '   -  -    ')                       EL335
00682          MOVE CL-CERT-STATE        TO  CL-SSN-STATE               EL335
00683          MOVE CL-CERT-ACCOUNT      TO  CL-SSN-ACCOUNT             EL335
00684          MOVE CL-INSURED-LAST-NAME TO  CL-SSN-LN3.                EL335
00685                                                                   EL335
00686      IF WS-AIX03 = 'Y'                                            EL335
00687          IF CL-SSN-STATE   = CL-CERT-STATE  AND                   EL335
00688             CL-SSN-ACCOUNT = CL-CERT-ACCOUNT                      EL335
00689              NEXT SENTENCE                                        EL335
00690            ELSE                                                   EL335
00691              MOVE +2                     TO  SWR-AIX-NUMBER       EL335
00692              MOVE CL-CONTROL-BY-SSN      TO  SWR-AIX-KEY          EL335
00693              MOVE CL-CONTROL-PRIMARY     TO  SWR-PRIME-KEY        EL335
00694              PERFORM 2000-RELEASE.                                EL335
00695                                                                   EL335
00696      IF WS-AIX05 = 'Y'                                            EL335
00697          MOVE +4                     TO  SWR-AIX-NUMBER           EL335
00698          MOVE CL-CONTROL-BY-CERT-NO  TO  SWR-AIX-KEY              EL335
00699          MOVE CL-CONTROL-PRIMARY     TO  SWR-PRIME-KEY            EL335
00700          PERFORM 2000-RELEASE.                                    EL335
00701                                                                   EL335
00702  1490-SIP-ELMSTR. COPY ELC335P2 REPLACING                         EL335
00703      CERTIFICATE-MASTER          BY  CLAIM-MASTER                 EL335
00704      ELCERT-MASTER-FILE          BY  ELMSTR-MASTER-FILE           EL335
00705      ELCERT-FILE-STATUS          BY  ELMSTR-FILE-STATUS           EL335
00706      ELCERT-FILE-NAME            BY  ELMSTR-FILE-NAME.            EL335
00707                                                                   EL335
00708      EJECT                                                        EL335
00709  1500-SIP-ERACCT. COPY ELC335P1 REPLACING                         EL335
00710      ELCERT-INPUT-FILE           BY  ERACCT-INPUT-FILE            EL335
00711      CERTIFICATE-MASTER          BY  ACCOUNT-MASTER               EL335
00712      ELCERT-MASTER-FILE          BY  ERACCT-MASTER-FILE           EL335
00713      ELCERT-MESSAGE-OCCURRENCE   BY  ERACCT-MESSAGE-OCCURRENCE    EL335
00714      CM-COMPANY-CD               BY  AM-COMPANY-CD.               EL335
00715                                                                   EL335
00716      MOVE +1                     TO  SWR-AIX-NUMBER.              EL335
00717      MOVE AM-CONTROL-BY-VAR-GRP IN ERACCT-MASTER-FILE             EL335
00718                                  TO  SWR-AIX-KEY.                 EL335
00719      MOVE AM-CONTROL-PRIMARY IN ERACCT-MASTER-FILE                EL335
00720                                  TO  SWR-PRIME-KEY.               EL335
00721      PERFORM 2000-RELEASE.                                        EL335
00722                                                                   EL335
00723  1590-SIP-ERACCT. COPY ELC335P2 REPLACING                         EL335
00724      CERTIFICATE-MASTER          BY  ACCOUNT-MASTER               EL335
00725      ELCERT-MASTER-FILE          BY  ERACCT-MASTER-FILE           EL335
00726      ELCERT-FILE-STATUS          BY  ERACCT-FILE-STATUS           EL335
00727      ELCERT-FILE-NAME            BY  ERACCT-FILE-NAME.            EL335
00728                                                                   EL335
00729      EJECT                                                        EL335
00730  1600-SIP-ERPNDB. COPY ELC335P1 REPLACING                         EL335
00731      ELCERT-INPUT-FILE           BY  ERPNDB-INPUT-FILE            EL335
00732      CERTIFICATE-MASTER          BY  PENDING-BUSINESS             EL335
00733      ELCERT-MASTER-FILE          BY  ERPNDB-MASTER-FILE           EL335
00734      ELCERT-MESSAGE-OCCURRENCE   BY  ERPNDB-MESSAGE-OCCURRENCE    EL335
00735      CM-COMPANY-CD               BY  PB-COMPANY-CD.               EL335
00736                                                                   EL335
00737      MOVE +1                     TO  SWR-AIX-NUMBER.              EL335
00738      MOVE PB-CONTROL-BY-ACCOUNT IN ERPNDB-MASTER-FILE             EL335
00739                                  TO  SWR-AIX-KEY2.                EL335
00740      MOVE PB-CONTROL-PRIMARY IN ERPNDB-MASTER-FILE                EL335
00741                                  TO  SWR-PRIME-KEY2.              EL335
00742      PERFORM 2000-RELEASE.                                        EL335
00743                                                                   EL335
00744  1690-SIP-ERPNDB. COPY ELC335P2 REPLACING                         EL335
00745      CERTIFICATE-MASTER          BY  PENDING-BUSINESS             EL335
00746      ELCERT-MASTER-FILE          BY  ERPNDB-MASTER-FILE           EL335
00747      ELCERT-FILE-STATUS          BY  ERPNDB-FILE-STATUS           EL335
00748      ELCERT-FILE-NAME            BY  ERPNDB-FILE-NAME.            EL335
00749                                                                   EL335
00750  1900-SIP.                                                        EL335
00751      PERFORM 3100-CLOSE-MASTER-FILE.                              EL335
00752                                                                   EL335
00753      PERFORM 2200-PRINT-COMPANY-TOTALS.                           EL335
00754                                                                   EL335
00755      MOVE 'TOTAL INPUT='       TO  WS-D1-MESSAGE.                 EL335
00756      PERFORM 2300-DISPLAY-UPON-CONSOLE.                           EL335
00757                                                                   EL335
00758  1990-EXIT.                                                       EL335
00759      EXIT.                                                        EL335
00760                                                                   EL335
00761      EJECT                                                        EL335
00762  2000-RELEASE SECTION.                                            EL335
00763                                                                   EL335
00764      IF CR-OPTION = 'CNT'                                         EL335
00765          GO TO 2090-EXIT.                                         EL335
00766                                                                   EL335
00767      ADD +1  TO  WS-RECORDS-RELEASED.                             EL335
00768                                                                   EL335
00769      IF CR-DATASET NOT = ELCERT-FILE-NAME                         EL335
00770         GO TO 2000-DO-RELEASE.                                    EL335
00771                                                                   EL335
00772                                                                   EL335
00773      IF SWR-AIX-NUMBER = +1                                       EL335
00774          WRITE AIX02-RECORD FROM SORT-WORK-RECORD                 EL335
00775          GO TO 2090-EXIT.                                         EL335
00776      IF SWR-AIX-NUMBER = +2                                       EL335
00777          WRITE AIX03-RECORD FROM SORT-WORK-RECORD                 EL335
00778          GO TO 2090-EXIT.                                         EL335
00779      IF SWR-AIX-NUMBER = +4                                       EL335
00780          WRITE AIX05-RECORD FROM SORT-WORK-RECORD                 EL335
00781          GO TO 2090-EXIT.                                         EL335
00782      IF SWR-AIX-NUMBER = +5                                       EL335
00783          WRITE AIX06-RECORD FROM SORT-WORK-RECORD                 EL335
00784          GO TO 2090-EXIT.                                         EL335
00785                                                                   EL335
00786  2000-DO-RELEASE.                                                 EL335
00787                                                                   EL335
00788      RELEASE SORT-RECORD FROM SORT-WORK-RECORD.                   EL335
00789                                                                   EL335
00790      IF SORT-RETURN NOT = ZERO                                    EL335
00791          MOVE 'SORT FAILED - RELEASE'  TO  WS-ABEND-MESSAGE       EL335
00792          MOVE SORT-RETURN              TO  WS-RETURN-CODE         EL335
00793          GO TO ABEND-PGM.                                         EL335
00794                                                                   EL335
00795  2090-EXIT.                                                       EL335
00796      EXIT.                                                        EL335
00797                                                                   EL335
00798  2200-PRINT-COMPANY-TOTALS SECTION.                               EL335
00799                                                                   EL335
00800      MOVE '0'                    TO  WS-DETAIL-LINE1.             EL335
00801      MOVE WS-LAST-COMPANY-CD     TO  WS-COMPANY-CD.               EL335
00802      MOVE WS-COMPANY-NUMBER      TO  WS-D3-COMPANY-NUMBER.        EL335
00803                                                                   EL335
00804      DIVIDE WS-COMPANY-NUMBER BY +16                              EL335
00805          GIVING WS-WORK REMAINDER WS-REMAINDER.                   EL335
00806                                                                   EL335
00807      IF WS-WORK GREATER ZERO                                      EL335
00808          MOVE WS-HEX-CHAR (WS-WORK) TO WS-D3-COMPANY-CD1          EL335
00809        ELSE                                                       EL335
00810          MOVE ZERO                  TO WS-D3-COMPANY-CD1.         EL335
00811                                                                   EL335
00812      IF WS-REMAINDER GREATER ZERO                                 EL335
00813          MOVE WS-HEX-CHAR (WS-REMAINDER) TO WS-D3-COMPANY-CD2     EL335
00814        ELSE                                                       EL335
00815          MOVE ZERO                       TO WS-D3-COMPANY-CD2.    EL335
00816                                                                   EL335
00817      SET CI TO +1.                                                EL335
00818                                                                   EL335
00819  2220-PRINT-COMPANY-TOTALS.                                       EL335
00820      IF WS-COMPANY-CD = WS-COMPANY-CODE (CI)                      EL335
00821          MOVE WS-COMPANY-ID (CI)   TO WS-D3-COMPANY-ID            EL335
00822          MOVE WS-COMPANY-NAME (CI) TO WS-D3-COMPANY-NAME          EL335
00823          GO TO 2230-PRINT-COMPANY-TOTALS.                         EL335
00824                                                                   EL335
00825      IF CI LESS CI-MAX                                            EL335
00826          SET CI UP BY +1                                          EL335
00827          GO TO 2220-PRINT-COMPANY-TOTALS.                         EL335
00828                                                                   EL335
00829      MOVE '*** UNKNOWN COMPANY CODE ***' TO WS-D3-COMPANY-NAME.   EL335
00830                                                                   EL335
00831  2230-PRINT-COMPANY-TOTALS.                                       EL335
00832      MOVE 'RECORDS='             TO  WS-D3-MESSAGE.               EL335
00833      MOVE WS-RECORD-COUNT        TO  WS-D3-COUNT.                 EL335
00834      MOVE ZERO                   TO  WS-RECORD-COUNT.             EL335
00835                                                                   EL335
00836      MOVE WS-DETAIL-LINE1 TO PRT                                  EL335
00837      MOVE P-CTL TO LCP-ASA                                        EL335
00838      PERFORM LCP-WRITE-POS-PRT                                    EL335
00839          THRU LCP-WRITE-END-PRT.                                  EL335
00840                                                                   EL335
00841      MOVE CM-COMPANY-CD          TO  WS-LAST-COMPANY-CD.          EL335
00842                                                                   EL335
00843  2290-EXIT.                                                       EL335
00844      EXIT.                                                        EL335
00845                                                                   EL335
00846  2300-DISPLAY-UPON-CONSOLE SECTION.                               EL335
00847                                                                   EL335
00848      MOVE WS-RECORDS-INPUT     TO  WS-D1-COUNT.                   EL335
00849                                                                   EL335
00850      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL335
00851                                                                   EL335
00852      MOVE WS-TIME              TO  WS-D1-TIME.                    EL335
00853      INSPECT WS-D1-TIME CONVERTING SPACES TO '.'.                 EL335
00854      DISPLAY WS-DISPLAY1 UPON CONSOLE.                            EL335
00855                                                                   EL335
00856  2390-EXIT.                                                       EL335
00857      EXIT.                                                        EL335
00858                                                                   EL335
00859      EJECT                                                        EL335
00860  3000-OPEN-FILES SECTION.                                         EL335
00861                                                                   EL335
00862      MOVE 'ERROR OCCURED OPEN - ' TO  WS-FEM-ERROR-MESSAGE.       EL335
00863                                                                   EL335
00864      GO TO 3020-OPEN-ELARCH                                       EL335
00865            3030-OPEN-ELCERT                                       EL335
00866            3040-OPEN-ELMSTR                                       EL335
00867            3050-OPEN-ERACCT                                       EL335
00868            3060-OPEN-ERPNDB DEPENDING ON WS-DATASET.              EL335
00869                                                                   EL335
00870  3020-OPEN-ELARCH. COPY ELC335P3 REPLACING                        EL335
00871      ELCERT-MASTER-FILE          BY  ELARCH-MASTER-FILE           EL335
00872      ELCERT-INPUT-FILE           BY  ELARCH-INPUT-FILE            EL335
00873      ELCERT-FILE-STATUS          BY  ELARCH-FILE-STATUS           EL335
00874      ELCERT-FILE-NAME            BY  ELARCH-FILE-NAME.            EL335
00875                                                                   EL335
00876      EJECT                                                        EL335
00877  3030-OPEN-ELCERT.                                                EL335
00878                                                                   EL335
00879 *     IF WS-OPTION = +1                                           EL335
00880 *         OPEN OUTPUT CERT-INDEX-2                                EL335
00881 *                     CERT-INDEX-3                                EL335
00882 *                     CERT-INDEX-5                                EL335
00883 *                     CERT-INDEX-6.                               EL335
00884 *      COPY ELC335P3.
00001 ***************************************************************** 04/19/98
00002 *                                                               * ELC335P3
00003 *                            ELC335P3                            *   LV002
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE               CL**2
00005 *                            VMOD=2.002                          *   CL**2
00006 *                                                               * ELC335P3
00007 *                OPEN ROUTINE FOR THE BUILD INDEX ROUTINE.      * ELC335P3
00008 *****************************************************************.ELC335P3
00009                                                                   ELC335P3
00010      OPEN INPUT ELCERT-INPUT-FILE.                                ELC335P3
00011                                                                   ELC335P3
           display ' open file status ' elcert-file-status

00012      IF WS-OPTION = +1                                            ELC335P3
00013          OPEN OUTPUT ELCERT-MASTER-FILE                           ELC335P3
00013 *        OPEN I-O    ELCERT-MASTER-FILE                           ELC335P3
00014          IF ELCERT-FILE-STATUS NOT = '00' AND '97'                ELC335P3
00015              MOVE ELCERT-FILE-NAME       TO WS-FEM-FILE-NAME      ELC335P3
00016              MOVE WS-FILE-ERROR-MESSAGE  TO WS-ABEND-MESSAGE      ELC335P3
00017              MOVE ELCERT-FILE-STATUS     TO WS-ABEND-FILE-STATUS  ELC335P3
00018              PERFORM ABEND-PGM.                                   ELC335P3
00019                                                                   ELC335P3
00020      GO TO 3090-EXIT.                                             ELC335P3
00885                                                                   EL335
00886      EJECT                                                        EL335
00887  3040-OPEN-ELMSTR. COPY ELC335P3 REPLACING                        EL335
00888      ELCERT-MASTER-FILE          BY  ELMSTR-MASTER-FILE           EL335
00889      ELCERT-INPUT-FILE           BY  ELMSTR-INPUT-FILE            EL335
00890      ELCERT-FILE-STATUS          BY  ELMSTR-FILE-STATUS           EL335
00891      ELCERT-FILE-NAME            BY  ELMSTR-FILE-NAME.            EL335
00892                                                                   EL335
00893      EJECT                                                        EL335
00894  3050-OPEN-ERACCT. COPY ELC335P3 REPLACING                        EL335
00895      ELCERT-MASTER-FILE          BY  ERACCT-MASTER-FILE           EL335
00896      ELCERT-INPUT-FILE           BY  ERACCT-INPUT-FILE            EL335
00897      ELCERT-FILE-STATUS          BY  ERACCT-FILE-STATUS           EL335
00898      ELCERT-FILE-NAME            BY  ERACCT-FILE-NAME.            EL335
00899                                                                   EL335
00900      EJECT                                                        EL335
00901  3060-OPEN-ERPNDB. COPY ELC335P3 REPLACING                        EL335
00902      ELCERT-MASTER-FILE          BY  ERPNDB-MASTER-FILE           EL335
00903      ELCERT-INPUT-FILE           BY  ERPNDB-INPUT-FILE            EL335
00904      ELCERT-FILE-STATUS          BY  ERPNDB-FILE-STATUS           EL335
00905      ELCERT-FILE-NAME            BY  ERPNDB-FILE-NAME.            EL335
00906                                                                   EL335
00907  3090-EXIT.                                                       EL335
00908      EXIT.                                                        EL335
00909                                                                   EL335
00910      EJECT                                                        EL335
00911  3100-CLOSE-MASTER-FILE SECTION.                                  EL335
00912                                                                   EL335
00913      MOVE 'ERROR OCCURED CLOSE - ' TO  WS-FEM-ERROR-MESSAGE.      EL335
00914                                                                   EL335
00915      GO TO 3120-CLOSE-ELARCH                                      EL335
00916            3130-CLOSE-ELCERT                                      EL335
00917            3140-CLOSE-ELMSTR                                      EL335
00918            3150-CLOSE-ERACCT                                      EL335
00919            3160-CLOSE-ERPNDB DEPENDING ON WS-DATASET.             EL335
00920                                                                   EL335
00921  3120-CLOSE-ELARCH. COPY ELC335P4 REPLACING                       EL335
00922      ELCERT-MASTER-FILE          BY  ELARCH-MASTER-FILE           EL335
00923      ELCERT-INPUT-FILE           BY  ELARCH-INPUT-FILE            EL335
00924      ELCERT-FILE-STATUS          BY  ELARCH-FILE-STATUS           EL335
00925      ELCERT-FILE-NAME            BY  ELARCH-FILE-NAME.            EL335
00926                                                                   EL335
00927      EJECT                                                        EL335
00928  3130-CLOSE-ELCERT.                                               EL335
00929 *        CLOSE CERT-INDEX-2                                       EL335
00930 *              CERT-INDEX-3                                       EL335
00931 *              CERT-INDEX-5                                       EL335
00932 *              CERT-INDEX-6.                                      EL335
00933       COPY ELC335P4.                                              EL335
00934                                                                   EL335
00935      EJECT                                                        EL335
00936  3140-CLOSE-ELMSTR. COPY ELC335P4 REPLACING                       EL335
00937      ELCERT-MASTER-FILE          BY  ELMSTR-MASTER-FILE           EL335
00938      ELCERT-INPUT-FILE           BY  ELMSTR-INPUT-FILE            EL335
00939      ELCERT-FILE-STATUS          BY  ELMSTR-FILE-STATUS           EL335
00940      ELCERT-FILE-NAME            BY  ELMSTR-FILE-NAME.            EL335
00941                                                                   EL335
00942      EJECT                                                        EL335
00943  3150-CLOSE-ERACCT. COPY ELC335P4 REPLACING                       EL335
00944      ELCERT-MASTER-FILE          BY  ERACCT-MASTER-FILE           EL335
00945      ELCERT-INPUT-FILE           BY  ERACCT-INPUT-FILE            EL335
00946      ELCERT-FILE-STATUS          BY  ERACCT-FILE-STATUS           EL335
00947      ELCERT-FILE-NAME            BY  ERACCT-FILE-NAME.            EL335
00948                                                                   EL335
00949      EJECT                                                        EL335
00950  3160-CLOSE-ERPNDB. COPY ELC335P4 REPLACING                       EL335
00951      ELCERT-MASTER-FILE          BY  ERPNDB-MASTER-FILE           EL335
00952      ELCERT-INPUT-FILE           BY  ERPNDB-INPUT-FILE            EL335
00953      ELCERT-FILE-STATUS          BY  ERPNDB-FILE-STATUS           EL335
00954      ELCERT-FILE-NAME            BY  ERPNDB-FILE-NAME.            EL335
00955                                                                   EL335
00956  3190-EXIT.                                                       EL335
00957      EXIT.                                                        EL335
00958                                                                   EL335
00959      EJECT                                                        EL335
00960  4000-SORT-INPUT-PROCEDURE SECTION.                               EL335
00961                                                                   EL335
00962  4010-SIP.                                                        EL335
00963      READ ELCNTL-BACKUP-FILE                                      EL335
00964          AT END                                                   EL335
00965              GO TO 4090-EXIT.                                     EL335
00966                                                                   EL335
00967      IF CF-RECORD-TYPE NOT = '1'                                  EL335
00968          GO TO 4010-SIP.                                          EL335
00969                                                                   EL335
00970      MOVE CF-COMPANY-CD          TO  SWR-COMPANY-CODE.            EL335
00971      MOVE CF-COMPANY-ID          TO  SWR-COMPANY-ID.              EL335
00972      MOVE CF-CL-MAIL-TO-NAME     TO  SWR-COMPANY-NAME.            EL335
00973                                                                   EL335
00974      RELEASE COMPANY-SORT-WORK-RECORD.                            EL335
00975                                                                   EL335
00976      IF SORT-RETURN GREATER ZERO                                  EL335
00977          MOVE SORT-RETURN                 TO  WS-RETURN-CODE      EL335
00978          MOVE 'COMPANY NAME SORT FAILED'  TO  WS-ABEND-MESSAGE    EL335
00979          GO TO ABEND-PGM.                                         EL335
00980                                                                   EL335
00981      GO TO 4010-SIP.                                              EL335
00982                                                                   EL335
00983  4090-EXIT.                                                       EL335
00984      EXIT.                                                        EL335
00985                                                                   EL335
00986      EJECT                                                        EL335
00987  4100-SORT-OUTPUT-PROCEDURE SECTION.                              EL335
00988                                                                   EL335
00989      SET CI TO +1.                                                EL335
00990                                                                   EL335
00991  4110-SOP.                                                        EL335
00992      RETURN COMPANY-SORT-WORK-FILE                                EL335
00993          AT END                                                   EL335
00994              SET CI-MAX TO CI                                     EL335
00995              SET CI-MAX DOWN BY +1                                EL335
00996              GO TO 4190-EXIT.                                     EL335
00997                                                                   EL335
00998      MOVE COMPANY-SORT-WORK-RECORD TO WS-COMPANY-NAME-TABLE (CI)  EL335
00999                                  COMPANY-NAME-WORK-RECORD.        EL335
01000                                                                   EL335
01001      WRITE COMPANY-NAME-WORK-RECORD.                              EL335
01002                                                                   EL335
01003      IF CI LESS WS-CI-MAX                                         EL335
01004          SET CI UP BY +1                                          EL335
01005          GO TO 4110-SOP.                                          EL335
01006                                                                   EL335
01007  4190-EXIT.                                                       EL335
01008      EXIT.                                                        EL335
01009                                                                   EL335
01010      EJECT                                                        EL335
01011  5000-SORT-OUTPUT-PROCEDURE SECTION.                              EL335
01012                                                                   EL335
01013      OPEN OUTPUT SORT-OUTPUT-FILE                                 EL335
01014                  SORT-OUTPUT-FILE2.                               EL335
01015                                                                   EL335
01016  5100-SOP.                                                        EL335
01017      RETURN SORT-WORK-FILE                                        EL335
01018          AT END                                                   EL335
01019              GO TO 5900-SOP.                                      EL335
01020                                                                   EL335
01021      IF SORT-RETURN NOT = ZERO                                    EL335
01022          MOVE SORT-RETURN          TO WS-RETURN-CODE              EL335
01023          MOVE 'SORT FAILED RETURN' TO WS-ABEND-MESSAGE            EL335
01024          GO TO ABEND-PGM.                                         EL335
01025                                                                   EL335
01026      MOVE SORT-RECORD            TO  SORT-OUTPUT-RECORD           EL335
01027                                      SORT-OUTPUT-RECORD2.         EL335
01028                                                                   EL335
01029      WRITE SORT-OUTPUT-RECORD.                                    EL335
01030      WRITE SORT-OUTPUT-RECORD2.                                   EL335
01031                                                                   EL335
01032      GO TO 5100-SOP.                                              EL335
01033                                                                   EL335
01034  5900-SOP.                                                        EL335
01035      CLOSE SORT-OUTPUT-FILE                                       EL335
01036            SORT-OUTPUT-FILE2.                                     EL335
01037                                                                   EL335
01038  5999-EXIT.                                                       EL335
01039      EXIT.                                                        EL335
01040                                                                   EL335
01041      EJECT                                                        EL335
01042  ABEND-PGM SECTION. COPY ELCABEND.                                EL335
01043 /                                                                 EL335
01044  LCP-WRITE-POS-PRT SECTION.                                       EL335
01045      IF LCP-ASA = '+'                                             EL335
01046          WRITE PRT AFTER 0 LINE                                   EL335
01047      ELSE                                                         EL335
01048      IF LCP-ASA = ' '                                             EL335
01049          WRITE PRT AFTER ADVANCING 1 LINE                         EL335
01050      ELSE                                                         EL335
01051      IF LCP-ASA = '0'                                             EL335
01052          WRITE PRT AFTER ADVANCING 2 LINE                         EL335
01053      ELSE                                                         EL335
01054      IF LCP-ASA = '-'                                             EL335
01055          WRITE PRT AFTER ADVANCING 3 LINE                         EL335
01056      ELSE                                                         EL335
01057      IF LCP-ASA = '1'                                             EL335
01058          WRITE PRT AFTER ADVANCING PAGE                           EL335
01059      ELSE                                                         EL335
01060      IF LCP-ASA = '2'                                             EL335
01061          WRITE PRT AFTER ADVANCING LCP-CH2                        EL335
01062      ELSE                                                         EL335
01063      IF LCP-ASA = '3'                                             EL335
01064          WRITE PRT AFTER ADVANCING LCP-CH3                        EL335
01065      ELSE                                                         EL335
01066      IF LCP-ASA = '4'                                             EL335
01067          WRITE PRT AFTER ADVANCING LCP-CH4                        EL335
01068      ELSE                                                         EL335
01069      IF LCP-ASA = '5'                                             EL335
01070          WRITE PRT AFTER ADVANCING LCP-CH5                        EL335
01071      ELSE                                                         EL335
01072      IF LCP-ASA = '6'                                             EL335
01073          WRITE PRT AFTER ADVANCING LCP-CH6                        EL335
01074      ELSE                                                         EL335
01075      IF LCP-ASA = '7'                                             EL335
01076          WRITE PRT AFTER ADVANCING LCP-CH7                        EL335
01077      ELSE                                                         EL335
01078      IF LCP-ASA = '8'                                             EL335
01079          WRITE PRT AFTER ADVANCING LCP-CH8                        EL335
01080      ELSE                                                         EL335
01081      IF LCP-ASA = '9'                                             EL335
01082          WRITE PRT AFTER ADVANCING LCP-CH9                        EL335
01083      ELSE                                                         EL335
01084      IF LCP-ASA = 'A'                                             EL335
01085          WRITE PRT AFTER ADVANCING LCP-CH10                       EL335
01086      ELSE                                                         EL335
01087      IF LCP-ASA = 'B'                                             EL335
01088          WRITE PRT AFTER ADVANCING LCP-CH11                       EL335
01089      ELSE                                                         EL335
01090      IF LCP-ASA = 'C'                                             EL335
01091          WRITE PRT AFTER ADVANCING LCP-CH12                       EL335
01092      ELSE                                                         EL335
01093      IF LCP-ASA = 'V'                                             EL335
01094          WRITE PRT AFTER ADVANCING LCP-P01                        EL335
01095      ELSE                                                         EL335
01096      IF LCP-ASA = 'W'                                             EL335
01097          WRITE PRT AFTER ADVANCING LCP-P02                        EL335
01098      ELSE                                                         EL335
01099      DISPLAY 'ASA CODE ERROR'.                                    EL335
01100  LCP-WRITE-END-PRT.                                               EL335
01101      EXIT.                                                        EL335
01102                                                                   EL335
