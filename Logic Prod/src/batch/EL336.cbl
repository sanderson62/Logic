00001  IDENTIFICATION DIVISION.                                         09/24/97
00002                                                                   EL336
00003  PROGRAM-ID.                 EL336 .                                 LV001
00004 *              PROGRAM CONVERTED BY                               EL336
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL336
00006 *              CONVERSION DATE 03/21/95 15:34:51.                 EL336
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL336
00008 *                            VMOD=2.012.                          EL336
00009                                                                   EL336
00009                                                                   EL336
00010 *AUTHOR.     LOGIC INC.                                           EL336
00011 *            DALLAS, TEXAS.                                       EL336
00012                                                                   EL336
00013 *DATE-COMPILED.                                                   EL336
00014                                                                   EL336
00015 *SECURITY.   *****************************************************EL336
00016 *            *                                                   *EL336
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL336
00018 *            *                                                   *EL336
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL336
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL336
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL336
00022 *            *                                                   *EL336
00023 *            *****************************************************EL336
00024                                                                   EL336
00025 *REMARKS.                                                         EL336
00026                                                                   EL336
00027 *         THIS PROGRAM IS USED TO LOAD THE VSAM MASTER FILE       EL336
00028 *       ALTERNATE INDEXES.                                        EL336
00029                                                                   EL336
00030 *                ALTERNATE INDEX RECORD LAYOUT                    EL336
00031                                                                   EL336
00032 *        OFFSET      BYTES &    FIELD                             EL336
00033 *    DECIMAL  HEXBIT PATTERN    NAME     DESCRIPTION              EL336
00034                                                                   EL336
00035 *       0      0       1        AIXFG    FLAG BYTE                EL336
00036 *                   XXXX XXX.            RESERVED                 EL336
00037 *                   .... ...1            PRIME KEY POINTERS USED  EL336
00038 *                   .... ...0            RBA POINTERS ARE USED    EL336
00039 *       1      1       1        AIXPKP   POINTER LENGTH (BINARY-74EL336
00040 *                                                              )  EL336
00041 *       2      2       2        AIXPC    NUMBER OF POINTERS IN    EL336
00042 *                                        THIS RECORD (IN BINARY-74EL336
00043 *                                                              )  EL336
00044 *       4      4       1        AIXKL    LENGTH OF ALTERNATE KEY  EL336
00045 *                                        (IN BINARY-74)           EL336
00046 *       5      5                                                  EL336
00047 *                    NOTE 1     AIXKY    ALTERNATE KEY            EL336
00048 *       NOTE 2       NOTE 3     AIXPT    FIRST POINTER TO BASE    EL336
00049 *                                        CLUSTER                  EL336
00050                                                                   EL336
00051 *     NOTE 1:  THE LENGTH OF THIS FIELD IS SPECIFIED IN AIXKL     EL336
00052 *     NOTE 2:  THE DISPLACEMENT OF THIS FIELD IS 5 + THE LENGTH   EL336
00053 *              OF AIXKY                                           EL336
00054 *     NOTE 3:  THE LENGTH OF THIS FIELD IS SPECIFIED IN AIXPL     EL336
00055                                                                   EL336
00056 *        THE ABOVE WAS COPIED FROM THE VSAM LOGIC MANUAL.         EL336
00057                                                                   EL336
00058  ENVIRONMENT DIVISION.                                            EL336
00059                                                                   EL336
00060  INPUT-OUTPUT SECTION.                                            EL336
00061                                                                   EL336
00062  FILE-CONTROL.                                                    EL336
00063                                                                   EL336
00064      SELECT ELARCH-AIX02                                          EL336
00065          ASSIGN TO SYS013-FBA1-ELARCH2                            EL336
00066          ORGANIZATION IS INDEXED                                  EL336
00067          ACCESS IS SEQUENTIAL                                     EL336
00068          RECORD KEY IS ELARCH-AIX02-KEY                           EL336
00069          FILE STATUS IS ELARCH-AIX02-FILE-STATUS.                 EL336
00070                                                                   EL336
00071      EJECT                                                        EL336
00072      SELECT ELCERT-AIX02                                          EL336
00073          ASSIGN TO SYS013-FBA1-ELCERT2                            EL336
00074          ORGANIZATION IS INDEXED                                  EL336
00075          ACCESS IS SEQUENTIAL                                     EL336
00076          RECORD KEY IS ELCERT-AIX02-KEY                           EL336
00077          FILE STATUS IS ELCERT-AIX02-FILE-STATUS.                 EL336
00078                                                                   EL336
00079      SELECT ELCERT-AIX03                                          EL336
00080          ASSIGN TO SYS014-FBA1-ELCERT3                            EL336
00081          ORGANIZATION IS INDEXED                                  EL336
00082          ACCESS IS SEQUENTIAL                                     EL336
00083          RECORD KEY IS ELCERT-AIX03-KEY                           EL336
00084          FILE STATUS IS ELCERT-AIX03-FILE-STATUS.                 EL336
00085                                                                   EL336
00086      SELECT ELCERT-AIX05                                          EL336
00087          ASSIGN TO SYS016-FBA1-ELCERT5                            EL336
00088          ORGANIZATION IS INDEXED                                  EL336
00089          ACCESS IS SEQUENTIAL                                     EL336
00090          RECORD KEY IS ELCERT-AIX05-KEY                           EL336
00091          FILE STATUS IS ELCERT-AIX05-FILE-STATUS.                 EL336
00092                                                                   EL336
00093      SELECT ELCERT-AIX06                                          EL336
00094          ASSIGN TO SYS017-FBA1-ELCERT6                            EL336
00095          ORGANIZATION IS INDEXED                                  EL336
00096          ACCESS IS SEQUENTIAL                                     EL336
00097          RECORD KEY IS ELCERT-AIX06-KEY                           EL336
00098          FILE STATUS IS ELCERT-AIX06-FILE-STATUS.                 EL336
00099                                                                   EL336
00100      EJECT                                                        EL336
00101      SELECT ELMSTR-AIX02                                          EL336
00102          ASSIGN TO SYS013-FBA1-ELMSTR2                            EL336
00103          ORGANIZATION IS INDEXED                                  EL336
00104          ACCESS IS SEQUENTIAL                                     EL336
00105          RECORD KEY IS ELMSTR-AIX02-KEY                           EL336
00106          FILE STATUS IS ELMSTR-AIX02-FILE-STATUS.                 EL336
00107                                                                   EL336
00108      SELECT ELMSTR-AIX03                                          EL336
00109          ASSIGN TO SYS014-FBA1-ELMSTR3                            EL336
00110          ORGANIZATION IS INDEXED                                  EL336
00111          ACCESS IS SEQUENTIAL                                     EL336
00112          RECORD KEY IS ELMSTR-AIX03-KEY                           EL336
00113          FILE STATUS IS ELMSTR-AIX03-FILE-STATUS.                 EL336
00114                                                                   EL336
00115      SELECT ELMSTR-AIX05                                          EL336
00116          ASSIGN TO SYS016-FBA1-ELMSTR5                            EL336
00117          ORGANIZATION IS INDEXED                                  EL336
00118          ACCESS IS SEQUENTIAL                                     EL336
00119          RECORD KEY IS ELMSTR-AIX05-KEY                           EL336
00120          FILE STATUS IS ELMSTR-AIX05-FILE-STATUS.                 EL336
00121                                                                   EL336
00122      SELECT ERACCT-AIX02                                          EL336
00123          ASSIGN TO SYS013-FBA1-ERACCT2                            EL336
00124          ORGANIZATION IS INDEXED                                  EL336
00125          ACCESS IS SEQUENTIAL                                     EL336
00126          RECORD KEY IS ERACCT-AIX02-KEY                           EL336
00127          FILE STATUS IS ERACCT-AIX02-FILE-STATUS.                 EL336
00128                                                                   EL336
00129      SELECT ERPNDB-AIX02                                          EL336
00130          ASSIGN TO SYS013-FBA1-ERPNDB2                            EL336
00131          ORGANIZATION IS INDEXED                                  EL336
00132          ACCESS IS SEQUENTIAL                                     EL336
00133          RECORD KEY IS ERPNDB-AIX02-KEY                           EL336
00134          FILE STATUS IS ERPNDB-AIX02-FILE-STATUS.                 EL336
00135                                                                   EL336
00136      SELECT CARD-FILE                                             EL336
00137          ASSIGN TO SYS006-UR-2540R-S-SYS006.                      EL336
00138                                                                   EL336
00139      SELECT COMPANY-NAME-WORK-FILE                                EL336
00140          ASSIGN TO SYS012-UT-FBA1-S-SYS012.                       EL336
00141                                                                   EL336
00142      SELECT SORT-OUTPUT-FILE                                      EL336
00143          ASSIGN TO SYS018-UT-2400-S-SYS018.                       EL336
00144                                                                   EL336
00145      SELECT SORT-WORK-FILE                                        EL336
00146          ASSIGN TO SYS001-UT-3380-S-SORTWK1.                      EL336
00147                                                                   EL336
00148      SELECT PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.    EL336
00149                                                                   EL336
00150      EJECT                                                        EL336
00151  DATA DIVISION.                                                   EL336
00152                                                                   EL336
00153  FILE SECTION.                                                    EL336
00154                                                                   EL336
00155  FD  CARD-FILE                                                    EL336
00156      BLOCK CONTAINS 0 RECORDS
00157      RECORDING MODE IS F.                                         EL336
00158                                                                   EL336
00159  01  CARD-RECORD.                                                 EL336
00160      05  CR-DATASET                  PIC X(6).                    EL336
00161      05  FILLER                      PIC X.                       EL336
00162      05  CR-RESTART-POINT.                                        EL336
00163          10  FILLER                  PIC X(6).                    EL336
00164          10  CR-AIX-NUMBER           PIC 9.                       EL336
00165          10  FILLER                  PIC X.                       EL336
00166      05  FILLER                      PIC X(65).                   EL336
00167                                                                   EL336
00168  FD  ELARCH-AIX02.                                                EL336
00169                                                                   EL336
00170  01  ELARCH-AIX02-RECORD.                                         EL336
00171      05  ELARCH-AIX02-FLAG-BYTE      PIC X.                       EL336
00172      05  ELARCH-AIX02-POINTER-LENGTH PIC X.                       EL336
00173      05  ELARCH-AIX02-NUMBER-OF-KEYS PIC S9(4) COMP.              EL336
00174      05  ELARCH-AIX02-ALT-KEY-LENGTH PIC X.                       EL336
00175      05  ELARCH-AIX02-KEY            PIC X(8).                    EL336
00176      05  ELARCH-AIX02-PRIME-KEY      PIC X(8)                     EL336
00177          OCCURS 1 TIMES                                           EL336
00178          INDEXED BY ELARCH-AIX02-INDEX.                           EL336
00179                                                                   EL336
00180      EJECT                                                        EL336
00181  FD  ELCERT-AIX02.                                                EL336
00182                                                                   EL336
00183  01  ELCERT-AIX02-RECORD.                                         EL336
00184      05  ELCERT-AIX02-FLAG-BYTE      PIC X.                       EL336
00185      05  ELCERT-AIX02-POINTER-LENGTH PIC X.                       EL336
00186      05  ELCERT-AIX02-NUMBER-OF-KEYS PIC S9(4) COMP.              EL336
00187      05  ELCERT-AIX02-ALT-KEY-LENGTH PIC X.                       EL336
00188      05  ELCERT-AIX02-KEY            PIC X(18).                   EL336
00189      05  ELCERT-AIX02-PRIME-KEY      PIC X(33)                    EL336
00190          OCCURS  500  TIMES                                       EL336
00191          DEPENDING ON ELCERT-AIX02-NUMBER-OF-KEYS                 EL336
00192          INDEXED BY ELCERT-AIX02-INDEX.                           EL336
00193                                                                   EL336
00194  FD  ELCERT-AIX03.                                                EL336
00195                                                                   EL336
00196  01  ELCERT-AIX03-RECORD.                                         EL336
00197      05  ELCERT-AIX03-FLAG-BYTE      PIC X.                       EL336
00198      05  ELCERT-AIX03-POINTER-LENGTH PIC X.                       EL336
00199      05  ELCERT-AIX03-NUMBER-OF-KEYS PIC S9(4) COMP.              EL336
00200      05  ELCERT-AIX03-ALT-KEY-LENGTH PIC X.                       EL336
00201      05  ELCERT-AIX03-KEY            PIC X(12).                   EL336
00202      05  ELCERT-AIX03-PRIME-KEY      PIC X(33)                    EL336
00203          OCCURS 500 TIMES                                         EL336
00204          DEPENDING ON ELCERT-AIX03-NUMBER-OF-KEYS                 EL336
00205          INDEXED BY ELCERT-AIX03-INDEX.                           EL336
00206                                                                   EL336
00207  FD  ELCERT-AIX05.                                                EL336
00208                                                                   EL336
00209  01  ELCERT-AIX05-RECORD.                                         EL336
00210      05  ELCERT-AIX05-FLAG-BYTE      PIC X.                       EL336
00211      05  ELCERT-AIX05-POINTER-LENGTH PIC X.                       EL336
00212      05  ELCERT-AIX05-NUMBER-OF-KEYS PIC S9(4) COMP.              EL336
00213      05  ELCERT-AIX05-ALT-KEY-LENGTH PIC X.                       EL336
00214      05  ELCERT-AIX05-KEY            PIC X(12).                   EL336
00215      05  ELCERT-AIX05-PRIME-KEY      PIC X(33)                    EL336
00216          OCCURS 500 TIMES                                         EL336
00217          DEPENDING ON ELCERT-AIX05-NUMBER-OF-KEYS                 EL336
00218          INDEXED BY ELCERT-AIX05-INDEX.                           EL336
00219                                                                   EL336
00220  FD  ELCERT-AIX06.                                                EL336
00221                                                                   EL336
00222  01  ELCERT-AIX06-RECORD.                                         EL336
00223      05  ELCERT-AIX06-FLAG-BYTE      PIC X.                       EL336
00224      05  ELCERT-AIX06-POINTER-LENGTH PIC X.                       EL336
00225      05  ELCERT-AIX06-NUMBER-OF-KEYS PIC S9(4) COMP.              EL336
00226      05  ELCERT-AIX06-ALT-KEY-LENGTH PIC X.                       EL336
00227      05  ELCERT-AIX06-KEY            PIC X(13).                   EL336
00228      05  ELCERT-AIX06-PRIME-KEY      PIC X(33)                    EL336
00229          OCCURS 500  TIMES                                        EL336
00230          DEPENDING ON ELCERT-AIX06-NUMBER-OF-KEYS                 EL336
00231          INDEXED BY ELCERT-AIX06-INDEX.                           EL336
00232                                                                   EL336
00233      EJECT                                                        EL336
00234  FD  ELMSTR-AIX02.                                                EL336
00235                                                                   EL336
00236  01  ELMSTR-AIX02-RECORD.                                         EL336
00237      05  ELMSTR-AIX02-FLAG-BYTE      PIC X.                       EL336
00238      05  ELMSTR-AIX02-POINTER-LENGTH PIC X.                       EL336
00239      05  ELMSTR-AIX02-NUMBER-OF-KEYS PIC S9(4) COMP.              EL336
00240      05  ELMSTR-AIX02-ALT-KEY-LENGTH PIC X.                       EL336
00241      05  ELMSTR-AIX02-KEY            PIC X(29).                   EL336
00242      05  ELMSTR-AIX02-PRIME-KEY      PIC X(20)                    EL336
00243          OCCURS 1145 TIMES                                        EL336
00244          DEPENDING ON ELMSTR-AIX02-NUMBER-OF-KEYS                 EL336
00245          INDEXED BY ELMSTR-AIX02-INDEX.                           EL336
00246                                                                   EL336
00247  FD  ELMSTR-AIX03.                                                EL336
00248                                                                   EL336
00249  01  ELMSTR-AIX03-RECORD.                                         EL336
00250      05  ELMSTR-AIX03-FLAG-BYTE      PIC X.                       EL336
00251      05  ELMSTR-AIX03-POINTER-LENGTH PIC X.                       EL336
00252      05  ELMSTR-AIX03-NUMBER-OF-KEYS PIC S9(4) COMP.              EL336
00253      05  ELMSTR-AIX03-ALT-KEY-LENGTH PIC X.                       EL336
00254      05  ELMSTR-AIX03-KEY            PIC X(12).                   EL336
00255      05  ELMSTR-AIX03-PRIME-KEY      PIC X(20)                    EL336
00256          OCCURS 285 TIMES                                         EL336
00257          DEPENDING ON ELMSTR-AIX03-NUMBER-OF-KEYS                 EL336
00258          INDEXED BY ELMSTR-AIX03-INDEX.                           EL336
00259                                                                   EL336
00260  FD  ELMSTR-AIX05.                                                EL336
00261                                                                   EL336
00262  01  ELMSTR-AIX05-RECORD.                                         EL336
00263      05  ELMSTR-AIX05-FLAG-BYTE      PIC X.                       EL336
00264      05  ELMSTR-AIX05-POINTER-LENGTH PIC X.                       EL336
00265      05  ELMSTR-AIX05-NUMBER-OF-KEYS PIC S9(4) COMP.              EL336
00266      05  ELMSTR-AIX05-ALT-KEY-LENGTH PIC X.                       EL336
00267      05  ELMSTR-AIX05-KEY            PIC X(12).                   EL336
00268      05  ELMSTR-AIX05-PRIME-KEY      PIC X(20)                    EL336
00269          OCCURS 285 TIMES                                         EL336
00270          DEPENDING ON ELMSTR-AIX05-NUMBER-OF-KEYS                 EL336
00271          INDEXED BY ELMSTR-AIX05-INDEX.                           EL336
00272                                                                   EL336
00273      EJECT                                                        EL336
00274  FD  ERACCT-AIX02.                                                EL336
00275                                                                   EL336
00276  01  ERACCT-AIX02-RECORD.                                         EL336
00277      05  ERACCT-AIX02-FLAG-BYTE      PIC X.                       EL336
00278      05  ERACCT-AIX02-POINTER-LENGTH PIC X.                       EL336
00279      05  ERACCT-AIX02-NUMBER-OF-KEYS PIC S9(4) COMP.              EL336
00280      05  ERACCT-AIX02-ALT-KEY-LENGTH PIC X.                       EL336
00281      05  ERACCT-AIX02-KEY            PIC X(26).                   EL336
00282      05  ERACCT-AIX02-PRIME-KEY      PIC X(26)                    EL336
00283          OCCURS 1 TIMES                                           EL336
00284          INDEXED BY ERACCT-AIX02-INDEX.                           EL336
00285                                                                   EL336
00286  FD  ERPNDB-AIX02.                                                EL336
00287                                                                   EL336
00288  01  ERPNDB-AIX02-RECORD.                                         EL336
00289      05  ERPNDB-AIX02-FLAG-BYTE      PIC X.                       EL336
00290      05  ERPNDB-AIX02-POINTER-LENGTH PIC X.                       EL336
00291      05  ERPNDB-AIX02-NUMBER-OF-KEYS PIC S9(4) COMP.              EL336
00292      05  ERPNDB-AIX02-ALT-KEY-LENGTH PIC X.                       EL336
00293      05  ERPNDB-AIX02-KEY            PIC X(36).                   EL336
00294      05  ERPNDB-AIX02-PRIME-KEY      PIC X(11)                    EL336
00295          OCCURS 1 TIMES                                           EL336
00296          INDEXED BY ERPNDB-AIX02-INDEX.                           EL336
00297                                                                   EL336
00298      EJECT                                                        EL336
00299  FD  COMPANY-NAME-WORK-FILE COPY ELC335F2.                        EL336
00300                                                                   EL336
00301  FD  SORT-OUTPUT-FILE COPY ELC335F1.                              EL336
00302                                                                   EL336
00303  FD  PRNTR COPY ELCPRTFD.                                         EL336
00304                                                                   EL336
00305  SD  SORT-WORK-FILE                                               EL336
00306                      .                                            EL336
00307                                                                   EL336
00308  01  SORT-WORK-RECORD.                                            EL336
00309      05  SWR-OCCURS              PIC S9(3) COMP-3.                EL336
00310      05  SWR-FREQ                PIC S9(9) COMP-3.                EL336
00311                                                                   EL336
00312      EJECT                                                        EL336
00313  WORKING-STORAGE SECTION.                                         EL336
00314  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL336
00315                                                                   EL336
00316  77  FILLER  PIC X(32) VALUE '********************************'.  EL336
00317  77  FILLER  PIC X(32) VALUE '     EL336 WORKING-STORAGE      '.  EL336
00318  77  FILLER  PIC X(32) VALUE '********* V/M 2.012 ************'.  EL336
00319                                                                   EL336
00320  01  FILLER                          COMP-3.                      EL336
00321      05  WS-RECORDS-RETURNED         PIC S9(9)       VALUE ZERO.  EL336
00322      05  WS-AIX02-RECORDS-INPUT      PIC S9(9)       VALUE ZERO.  EL336
00323      05  WS-AIX03-RECORDS-INPUT      PIC S9(9)       VALUE ZERO.  EL336
00324      05  WS-AIX04-RECORDS-INPUT      PIC S9(9)       VALUE ZERO.  EL336
00325      05  WS-AIX05-RECORDS-INPUT      PIC S9(9)       VALUE ZERO.  EL336
00326      05  WS-AIX06-RECORDS-INPUT      PIC S9(9)       VALUE ZERO.  EL336
00327      05  WS-AIX02-RECORDS-OUTPUT     PIC S9(9)       VALUE ZERO.  EL336
00328      05  WS-AIX03-RECORDS-OUTPUT     PIC S9(9)       VALUE ZERO.  EL336
00329      05  WS-AIX04-RECORDS-OUTPUT     PIC S9(9)       VALUE ZERO.  EL336
00330      05  WS-AIX05-RECORDS-OUTPUT     PIC S9(9)       VALUE ZERO.  EL336
00331      05  WS-AIX06-RECORDS-OUTPUT     PIC S9(9)       VALUE ZERO.  EL336
00332      05  WS-RECORDS-INPUT            PIC S9(9)       VALUE ZERO.  EL336
00333      05  WS-RECORDS-OUTPUT           PIC S9(9)       VALUE ZERO.  EL336
00334      05  WS-WORK                     PIC S9(9)       VALUE ZERO.  EL336
00335      05  WS-REMAINDER                PIC S9(9)       VALUE ZERO.  EL336
00336      05  WS-OVERFLOW-COUNT           PIC S9(9)       VALUE ZERO.  EL336
00337        88  OVERFLOW-HAS-NOT-OCCURED                  VALUE ZERO.  EL336
00338      05  WS-ZERO                     PIC S9          VALUE ZERO.  EL336
00339      05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   EL336
00340                                                                   EL336
00341      05  WS-MESSAGE-OCCURRENCE-AREA.                              EL336
00342        07  ELARCH-MESSAGE-OCCURRENCE PIC S9(7)      VALUE +10000. EL336
00343        07  ELCERT-MESSAGE-OCCURRENCE PIC S9(7)      VALUE +100000.EL336
00344        07  ELMSTR-MESSAGE-OCCURRENCE PIC S9(7)      VALUE +10000. EL336
00345        07  ERACCT-MESSAGE-OCCURRENCE PIC S9(7)      VALUE +10000. EL336
00346        07  ERPNDB-MESSAGE-OCCURRENCE PIC S9(7)      VALUE +10000. EL336
00347      05  WS-MESSAGE-OCCURRENCE       REDEFINES                    EL336
00348          WS-MESSAGE-OCCURRENCE-AREA  PIC S9(7)                    EL336
00349          OCCURS 5 TIMES.                                          EL336
00350                                                                   EL336
00351  01  FILLER                          COMP SYNC.                   EL336
00352      05  WS-RETURN-CODE              PIC S9(4)       VALUE ZERO.  EL336
00353      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  EL336
00354      05  WS-DATASET                  PIC S9(4)       VALUE ZERO.  EL336
00355        88  PROCESSING-ELARCH-FILE                    VALUE +1.    EL336
00356        88  PROCESSING-ELCERT-FILE                    VALUE +2.    EL336
00357        88  PROCESSING-ELMSTR-FILE                    VALUE +3.    EL336
00358        88  PROCESSING-ERACCT-FILE                    VALUE +4.    EL336
00359        88  PROCESSING-ERPNDB-FILE                    VALUE +5.    EL336
00360      05  WS-LAST-AIX-NUMBER          PIC S9(4)       VALUE ZERO.  EL336
00361      05  WS-RESTART-POINT            PIC S9(4)       VALUE ZERO.  EL336
00362                                                                   EL336
00363      05  ELARCH-AIX02-MAX          PIC S9(4)       VALUE +1.      EL336
00364      05  ELCERT-AIX02-MAX          PIC S9(4)       VALUE +500.    EL336
00365      05  ELCERT-AIX03-MAX          PIC S9(4)       VALUE +500.    EL336
00366      05  ELCERT-AIX05-MAX          PIC S9(4)       VALUE +500.    EL336
00367      05  ELCERT-AIX06-MAX          PIC S9(4)       VALUE +500.    EL336
00368      05  ELMSTR-AIX02-MAX          PIC S9(4)       VALUE +1145.   EL336
00369      05  ELMSTR-AIX03-MAX          PIC S9(4)       VALUE +285.    EL336
00370      05  ELMSTR-AIX05-MAX          PIC S9(4)       VALUE +285.    EL336
00371      05  ERACCT-AIX02-MAX          PIC S9(4)       VALUE +1.      EL336
00372      05  ERPNDB-AIX02-MAX          PIC S9(4)       VALUE +1.      EL336
00373                                                                   EL336
00374      05  WS-KEY-LENGTH-AREA.                                      EL336
00375        07  ELARCH-KEY-LENGTH         PIC S9(4)       VALUE +8.    EL336
00376        07  ELARCH2-KEY-LENGTH        PIC S9(4)       VALUE +8.    EL336
00377        07  FILLER                    PIC S9(4)       VALUE ZERO.  EL336
00378        07  FILLER                    PIC S9(4)       VALUE ZERO.  EL336
00379        07  FILLER                    PIC S9(4)       VALUE ZERO.  EL336
00380        07  FILLER                    PIC S9(4)       VALUE ZERO.  EL336
00381        07  ELCERT-KEY-LENGTH         PIC S9(4)       VALUE +33.   EL336
00382        07  ELCERT2-KEY-LENGTH        PIC S9(4)       VALUE +18.   EL336
00383        07  ELCERT3-KEY-LENGTH        PIC S9(4)       VALUE +12.   EL336
00384        07  FILLER                    PIC S9(4)       VALUE +0.    EL336
00385        07  ELCERT5-KEY-LENGTH        PIC S9(4)       VALUE +12.   EL336
00386        07  ELCERT6-KEY-LENGTH        PIC S9(4)       VALUE +13.   EL336
00387        07  ELMSTR-KEY-LENGTH         PIC S9(4)       VALUE +20.   EL336
00388        07  ELMSTR2-KEY-LENGTH        PIC S9(4)       VALUE +29.   EL336
00389        07  ELMSTR3-KEY-LENGTH        PIC S9(4)       VALUE +12.   EL336
00390        07  FILLER                    PIC S9(4)       VALUE ZERO.  EL336
00391        07  ELMSTR5-KEY-LENGTH        PIC S9(4)       VALUE +12.   EL336
00392        07  FILLER                    PIC S9(4)       VALUE ZERO.  EL336
00393        07  ERACCT-KEY-LENGTH         PIC S9(4)       VALUE +26.   EL336
00394        07  ERACCT2-KEY-LENGTH        PIC S9(4)       VALUE +26.   EL336
00395        07  FILLER                    PIC S9(4)       VALUE ZERO.  EL336
00396        07  FILLER                    PIC S9(4)       VALUE ZERO.  EL336
00397        07  FILLER                    PIC S9(4)       VALUE ZERO.  EL336
00398        07  FILLER                    PIC S9(4)       VALUE ZERO.  EL336
00399        07  ERPNDB-KEY-LENGTH         PIC S9(4)       VALUE +11.   EL336
00400        07  ERPNDB2-KEY-LENGTH        PIC S9(4)       VALUE +36.   EL336
00401        07  FILLER                    PIC S9(4)       VALUE ZERO.  EL336
00402        07  FILLER                    PIC S9(4)       VALUE ZERO.  EL336
00403        07  FILLER                    PIC S9(4)       VALUE ZERO.  EL336
00404        07  FILLER                    PIC S9(4)       VALUE ZERO.  EL336
00405                                                                   EL336
00406      05  FILLER                      REDEFINES                    EL336
00407          WS-KEY-LENGTH-AREA          OCCURS 5 TIMES.              EL336
00408          10  WS-BASE-KEY-LENGTH      PIC S9(4).                   EL336
00409          10  WS-AIX-KEY-LENGTH       PIC S9(4)                    EL336
00410              OCCURS 5 TIMES.                                      EL336
00411                                                                   EL336
00412  01  FILLER.                                                      EL336
00413      05  WS-LAST-AIX-KEY2        PIC X(36) VALUE LOW-VALUES.      EL336
00414      05  WS-LAST-AIX-KEY         REDEFINES                        EL336
00415          WS-LAST-AIX-KEY2        PIC X(26).                       EL336
00416      05  WS-KEY-CHAR             REDEFINES                        EL336
00417          WS-LAST-AIX-KEY2        PIC X         OCCURS 36 TIMES.   EL336
00418                                                                   EL336
00419      05  WS-COMPANY-NUMBER           PIC S9(4) COMP  VALUE ZERO.  EL336
00420                                                                   EL336
00421      05  FILLER                      REDEFINES                    EL336
00422          WS-COMPANY-NUMBER.                                       EL336
00423          10  FILLER                  PIC X.                       EL336
00424          10  WS-COMPANY-CD           PIC X.                       EL336
00425                                                                   EL336
00426      05  WS-NUMBER                   REDEFINES                    EL336
00427          WS-COMPANY-NUMBER           PIC S9(4)       COMP.        EL336
00428                                                                   EL336
00429      05  FILLER                      REDEFINES                    EL336
00430          WS-COMPANY-NUMBER.                                       EL336
00431          10  FILLER                  PIC X.                       EL336
00432          10  WS-CHAR                 PIC X.                       EL336
00433                                                                   EL336
00434      05  WS-KEY-LENGTH2              REDEFINES                    EL336
00435          WS-COMPANY-NUMBER           PIC S9(4)       COMP.        EL336
00436                                                                   EL336
00437      05  FILLER                      REDEFINES                    EL336
00438          WS-COMPANY-NUMBER.                                       EL336
00439          10  FILLER                  PIC X.                       EL336
00440          10  WS-KEY-LENGTH           PIC X.                       EL336
00441                                                                   EL336
00442      05  WS-LAST-COMPANY-CD          PIC X      VALUE LOW-VALUES. EL336
00443                                                                   EL336
00444      05  ELARCH-AIX02-FILE-STATUS    PIC XX          VALUE ZERO.  EL336
00445      05  ELCERT-AIX02-FILE-STATUS    PIC XX          VALUE ZERO.  EL336
00446      05  ELCERT-AIX03-FILE-STATUS    PIC XX          VALUE ZERO.  EL336
00447      05  ELCERT-AIX05-FILE-STATUS    PIC XX          VALUE ZERO.  EL336
00448      05  ELCERT-AIX06-FILE-STATUS    PIC XX          VALUE ZERO.  EL336
00449      05  ELMSTR-AIX02-FILE-STATUS    PIC XX          VALUE ZERO.  EL336
00450      05  ELMSTR-AIX03-FILE-STATUS    PIC XX          VALUE ZERO.  EL336
00451      05  ELMSTR-AIX05-FILE-STATUS    PIC XX          VALUE ZERO.  EL336
00452      05  ERACCT-AIX02-FILE-STATUS    PIC XX          VALUE ZERO.  EL336
00453      05  ERPNDB-AIX02-FILE-STATUS    PIC XX          VALUE ZERO.  EL336
00454                                                                   EL336
00455      05  ELARCH-FILE-NAME            PIC X(8) VALUE 'ELARCH'.     EL336
00456      05  ELCERT-FILE-NAME            PIC X(8) VALUE 'ELCERT'.     EL336
00457      05  ELMSTR-FILE-NAME            PIC X(8) VALUE 'ELMSTR'.     EL336
00458      05  ERACCT-FILE-NAME            PIC X(8) VALUE 'ERACCT'.     EL336
00459      05  ERPNDB-FILE-NAME            PIC X(8) VALUE 'ERPNDB'.     EL336
00460                                                                   EL336
00461      05  WS-ERROR-MESSAGE.                                        EL336
00462          10  FILLER                  PIC X(14)       VALUE        EL336
00463              'ERROR OCCURED '.                                    EL336
00464          10  WS-EM-OPERATION         PIC X(6) VALUE 'WRITE'.      EL336
00465          10  WS-EM-FILE-NAME         PIC X(6) VALUE 'ELCERT'.     EL336
00466          10  WS-EM-AIX               PIC 9           VALUE ZERO.  EL336
00467          10  FILLER                  PIC X           VALUE 'I'.   EL336
00468                                                                   EL336
00469      05  WS-LOADED-MESSAGE.                                       EL336
00470          10  WS-LM-FILE-NAME         PIC X(6) VALUE 'ELCERT'.     EL336
00471          10  WS-LM-AIX               PIC 9           VALUE ZERO.  EL336
00472          10  FILLER                  PIC X(20)       VALUE        EL336
00473              ' SUCCESSFULLY LOADED'.                              EL336
00474                                                                   EL336
00475      05  WS-HEX-01                   PIC S9(4) COMP  VALUE +1.    EL336
00476                                                                   EL336
00477      05  FILLER                      REDEFINES                    EL336
00478          WS-HEX-01.                                               EL336
00479          10  FILLER                  PIC X.                       EL336
00480          10  WS-FLAG-BYTE            PIC X.                       EL336
00481                                                                   EL336
00482      05  FILLER.                                                  EL336
00483          10  BASE-KEY-LENGTH         PIC X.                       EL336
00484          10  AIX-KEY-LENGTHS.                                     EL336
00485              15  AIX02-KEY-LENGTH    PIC X.                       EL336
00486              15  AIX03-KEY-LENGTH    PIC X.                       EL336
00487              15  AIX04-KEY-LENGTH    PIC X.                       EL336
00488              15  AIX05-KEY-LENGTH    PIC X.                       EL336
00489              15  AIX06-KEY-LENGTH    PIC X.                       EL336
00490                                                                   EL336
00491          10  AIX-KEY-LENGTH          REDEFINES                    EL336
00492              AIX-KEY-LENGTHS         PIC X                        EL336
00493              OCCURS 5 TIMES.                                      EL336
00494                                                                   EL336
00495      05  WS-ABEND-MESSAGE        PIC X(80)           VALUE SPACES.EL336
00496      05  WS-ABEND-FILE-STATUS    PIC XX              VALUE ZERO.  EL336
00497                                                                   EL336
00498  01  WS-AIX-COUNT-AREA             COMP-3.                        EL336
00499      05  WS-AIX-COUNT-ARRAY  OCCURS 500 TIMES INDEXED BY CI1.     EL336
00500          10  WS-AIX-COUNT-OCCURS  PIC S9(3).                      EL336
00501          10  WS-AIX-COUNT-FREQ    PIC S9(9).                      EL336
00502                                                                   EL336
00503  01  WS-TOTAL-COUNT-AREA           COMP-3.                        EL336
00504      05  WS-TOTAL-COUNT-ARRAY  OCCURS 500 TIMES INDEXED BY TI1.   EL336
00505          10  WS-TOTAL-COUNT-OCCURS  PIC S9(3).                    EL336
00506          10  WS-TOTAL-COUNT-FREQ    PIC S9(9).                    EL336
00507                                                                   EL336
00508  01  WS-COUNT-AREA-INITALIZED      PIC X(3500).                   EL336
00509                                                                   EL336
00510             COPY ELC335W1.                                        EL336
00511                                                                   EL336
00512      05  WS-DIGET-CHARACTERS                         VALUE SPACES.EL336
00513          10  WS-DIGET                PIC X                        EL336
00514              OCCURS 50 TIMES.                                     EL336
00515                                                                   EL336
00516  01  WS-DETAIL-LINE1.                                             EL336
00517      05  FILLER                      PIC XX.                      EL336
00518      05  WS-D2-MESSAGE               PIC X(20).                   EL336
00519      05  FILLER                      PIC X(5).                    EL336
00520      05  WS-D2-COUNT                 PIC ZZZ,ZZZ,ZZ9-.            EL336
00521      05 FILLER                       PIC X(94).                   EL336
00522                                                                   EL336
00523  01  FILLER                          REDEFINES                    EL336
00524      WS-DETAIL-LINE1.                                             EL336
00525                                                                   EL336
00526      05  FILLER                      PIC XX.                      EL336
00527      05  WS-D3-AIX                   PIC 9.                       EL336
00528      05  FILLER                      PIC XX.                      EL336
00529      05  WS-D3-COMPANY               PIC 999.                     EL336
00530      05  FILLER                      PIC XX.                      EL336
00531      05  WS-D3-AIX-KEY.                                           EL336
00532          10  WS-D3-CHAR              PIC X                        EL336
00533              OCCURS 36 TIMES         INDEXED BY CHAR-INDEX.       EL336
00534                                                                   EL336
00535      05  FILLER                      PIC XX.                      EL336
00536      05  WS-D3-MESSAGE               PIC X(18).                   EL336
00537      05  FILLER                      PIC X(5).                    EL336
00538      05  WS-D3-COUNT                 PIC ZZZ,ZZZ,ZZ9-.            EL336
00539                                                                   EL336
00540  01  FILLER                          REDEFINES                    EL336
00541      WS-DETAIL-LINE1.                                             EL336
00542                                                                   EL336
00543      05  FILLER                      PIC XX.                      EL336
00544      05  WS-D4-FILE-NAME             PIC X(6).                    EL336
00545      05  WS-D4-AIX-NUMBER            PIC 9.                       EL336
00546      05  FILLER                      PIC XX.                      EL336
00547      05  WS-D4-COMPANY-NUMBER        PIC 999.                     EL336
00548      05  FILLER                      PIC XX.                      EL336
00549      05  WS-D4-COMPANY-ID            PIC X(3).                    EL336
00550      05  FILLER                      PIC XX.                      EL336
00551      05  WS-D4-COMPANY-CD1           PIC X.                       EL336
00552      05  WS-D4-COMPANY-CD2           PIC X.                       EL336
00553      05  FILLER                      PIC XX.                      EL336
00554      05  WS-D4-COMPANY-NAME          PIC X(30).                   EL336
00555      05  FILLER                      PIC XX.                      EL336
00556      05  WS-D4-MESSAGE               PIC X(20).                   EL336
00557      05  FILLER                      PIC X(5).                    EL336
00558      05  WS-D4-COUNT                 PIC ZZZ,ZZZ,ZZ9-.            EL336
00559                                                                   EL336
00560  01  FILLER                          REDEFINES                    EL336
00561      WS-DETAIL-LINE1.                                             EL336
00562      05  FILLER                      PIC X(25).                   EL336
00563      05  WS-D5-AIX-OCCURS            PIC ZZ9-.                    EL336
00564      05  WS-D5-AIX-OCCURS-FREQ       PIC ZZZ,ZZZ,ZZ9-.            EL336
00565                                                                   EL336
00566  01  WS-DISPLAY1.                                                 EL336
00567      05  WS-D1-TIME                  PIC 99B99B99.                EL336
00568      05  FILLER                      PIC X           VALUE SPACES.EL336
00569      05  WS-D1-MESSAGE               PIC X(30).                   EL336
00570      05  WS-D1-COUNT                 PIC ZZZ,ZZZ,ZZ9-.            EL336
00571                                                                   EL336
00572  01  WS-DATE-AND-TIME.                                            EL336
00573      12  WS-ACCEPT-DATE.                                          EL336
00574          16  WS-AD-YY                PIC  99.                     EL336
00575          16  WS-AD-MM                PIC  99.                     EL336
00576          16  WS-AD-DD                PIC  99.                     EL336
00577      12  WS-CURRENT-DATE.                                         EL336
00578          16  WS-CD-MM                PIC  99.                     EL336
00579          16  FILLER                  PIC  X          VALUE '/'.   EL336
00580          16  WS-CD-DD                PIC  99.                     EL336
00581          16  FILLER                  PIC  X          VALUE '/'.   EL336
00582          16  WS-CD-YY                PIC  99.                     EL336
00583      12  WS-TIME-OF-DAY.                                          EL336
00584          16  WS-TIME                 PIC  9(6).                   EL336
00585          16  WS-HUN-SEC              PIC  99.                     EL336
00586      EJECT                                                        EL336
00587  PROCEDURE DIVISION.                                              EL336
00588                                                                   EL336
00589      OPEN INPUT CARD-FILE                                         EL336
00590                 COMPANY-NAME-WORK-FILE                            EL336
00591           OUTPUT PRNTR.                                           EL336
00592                                                                   EL336
00593      ACCEPT WS-ACCEPT-DATE       FROM  DATE.                      EL336
00594                                                                   EL336
00595      MOVE WS-AD-YY               TO  WS-CD-YY.                    EL336
00596      MOVE WS-AD-MM               TO  WS-CD-MM.                    EL336
00597      MOVE WS-AD-DD               TO  WS-CD-DD.                    EL336
00598                                                                   EL336
00599 *    NOTE ******************************************************* EL336
00600 *         *          LOAD THE COMPANY NAME TABLE                * EL336
00601 *         *******************************************************.EL336
00602                                                                   EL336
00603      SET CI TO +1.                                                EL336
00604                                                                   EL336
00605   0010-MAIN-LOGIC.                                                EL336
00606      READ COMPANY-NAME-WORK-FILE INTO WS-COMPANY-NAME-TABLE (CI)  EL336
00607          AT END                                                   EL336
00608              CLOSE COMPANY-NAME-WORK-FILE                         EL336
00609              SET CI-MAX TO CI                                     EL336
00610              SET CI-MAX DOWN BY +1                                EL336
00611              GO TO 0020-MAIN-LOGIC.                               EL336
00612                                                                   EL336
00613      IF CI LESS THAN WS-CI-MAX                                    EL336
00614          SET CI UP BY +1                                          EL336
00615          GO TO 0010-MAIN-LOGIC.                                   EL336
00616                                                                   EL336
00617      EJECT                                                        EL336
00618  0020-MAIN-LOGIC.                                                 EL336
00619 *    NOTE ******************************************************* EL336
00620 *         *      READ THE CONTROL CARDS TO SEE WHICH ALTERNATE  * EL336
00621 *         *  INDEXES TO LOAD AND THEN SET THE BASE KEY LENGTH   * EL336
00622 *         *  AND ALL OF THE ALTERNATE KEY LENGTHS.              * EL336
00623 *         *******************************************************.EL336
00624                                                                   EL336
00625      READ CARD-FILE                                               EL336
00626          AT END                                                   EL336
00627              GO TO 0900-MAIN-LOGIC.                               EL336
00628                                                                   EL336
00629      MOVE ZERO                   TO  WS-RECORDS-RETURNED.         EL336
00630                                                                   EL336
00631      IF CR-DATASET = ELARCH-FILE-NAME                             EL336
00632          MOVE +1                 TO  WS-DATASET                   EL336
00633          MOVE ELARCH-FILE-NAME   TO  WS-EM-FILE-NAME              EL336
00634                                      WS-LM-FILE-NAME              EL336
00635          GO TO 0100-MAIN-LOGIC.                                   EL336
00636                                                                   EL336
00637      IF CR-DATASET = ELCERT-FILE-NAME                             EL336
00638          MOVE +2                 TO  WS-DATASET                   EL336
00639          MOVE ELCERT-FILE-NAME   TO  WS-EM-FILE-NAME              EL336
00640                                      WS-LM-FILE-NAME              EL336
00641          GO TO 0100-MAIN-LOGIC.                                   EL336
00642                                                                   EL336
00643      IF CR-DATASET = ELMSTR-FILE-NAME                             EL336
00644          MOVE +3                 TO  WS-DATASET                   EL336
00645          MOVE ELMSTR-FILE-NAME   TO  WS-EM-FILE-NAME              EL336
00646                                      WS-LM-FILE-NAME              EL336
00647          GO TO 0100-MAIN-LOGIC.                                   EL336
00648                                                                   EL336
00649      IF CR-DATASET = ERACCT-FILE-NAME                             EL336
00650          MOVE +4                 TO  WS-DATASET                   EL336
00651          MOVE ERACCT-FILE-NAME   TO  WS-EM-FILE-NAME              EL336
00652                                      WS-LM-FILE-NAME              EL336
00653          GO TO 0100-MAIN-LOGIC.                                   EL336
00654                                                                   EL336
00655      IF CR-DATASET = ERPNDB-FILE-NAME                             EL336
00656          MOVE +5                 TO  WS-DATASET                   EL336
00657          MOVE ERPNDB-FILE-NAME   TO  WS-EM-FILE-NAME              EL336
00658                                      WS-LM-FILE-NAME              EL336
00659          GO TO 0100-MAIN-LOGIC.                                   EL336
00660                                                                   EL336
00661      MOVE 'INVALID DATASET NAME'  TO  WS-ABEND-MESSAGE.           EL336
00662      GO TO ABEND-PGM.                                             EL336
00663                                                                   EL336
00664      EJECT                                                        EL336
00665  0100-MAIN-LOGIC.                                                 EL336
00666      MOVE WS-BASE-KEY-LENGTH (WS-DATASET) TO  WS-KEY-LENGTH2.     EL336
00667      MOVE WS-KEY-LENGTH          TO  BASE-KEY-LENGTH.             EL336
00668                                                                   EL336
00669      MOVE +1                     TO  WS-INDEX.                    EL336
00670                                                                   EL336
00671  0110-MAIN-LOGIC.                                                 EL336
00672      MOVE WS-AIX-KEY-LENGTH (WS-DATASET WS-INDEX)                 EL336
00673                                  TO  WS-KEY-LENGTH2.              EL336
00674      MOVE WS-KEY-LENGTH      TO  AIX-KEY-LENGTH (WS-INDEX).       EL336
00675                                                                   EL336
00676      IF WS-INDEX LESS THAN +5                                     EL336
00677          ADD +1  TO  WS-INDEX                                     EL336
00678          GO TO 0110-MAIN-LOGIC.                                   EL336
00679                                                                   EL336
00680      SET CI1 TO +1.                                               EL336
00681                                                                   EL336
00682  0120-MAIN-LOGIC.                                                 EL336
00683      MOVE ZERO  TO  WS-AIX-COUNT-OCCURS (CI1)                     EL336
00684                     WS-AIX-COUNT-FREQ (CI1).                      EL336
00685                                                                   EL336
00686      IF CI1 IS LESS THAN +500                                     EL336
00687          SET CI1 UP BY +1                                         EL336
00688          GO TO 0120-MAIN-LOGIC.                                   EL336
00689                                                                   EL336
00690      SET CI1 TI1 TO +1.                                           EL336
00691                                                                   EL336
00692      MOVE WS-AIX-COUNT-AREA  TO  WS-COUNT-AREA-INITALIZED.        EL336
00693      MOVE WS-COUNT-AREA-INITALIZED TO WS-TOTAL-COUNT-AREA.        EL336
00694                                                                   EL336
00695      EJECT                                                        EL336
00696 *    NOTE ******************************************************* EL336
00697 *         *      CHECK TO SEE IF THERE IS A RESTART POINT.      * EL336
00698 *         *                                                     * EL336
00699 *         *      THAT IS,YOU HAVE SUCCESSFULLY LOADED THE FIRST * EL336
00700 *         *  ALTERNATE INDEX AND SOMETHING HAPPENED DURING THE  * EL336
00701 *         *  LOADING OF THE SECOND ALTERNATE INDEX.SO SINCE THE * EL336
00702 *         *  LOADING OF THE FIRST AIX WAS SUCCESSFUL, THEN YOU  * EL336
00703 *         *  CAN BYPASS THE LOADING OF THE FIRST AIX WITH A     * EL336
00704 *         *  RESTART POINT.                                     * EL336
00705 *         *******************************************************.EL336
00706                                                                   EL336
00707      MOVE +1                     TO  WS-RESTART-POINT.            EL336
00708                                                                   EL336
00709      IF CR-RESTART-POINT = SPACES                                 EL336
00710          GO TO 0200-MAIN-LOGIC.                                   EL336
00711                                                                   EL336
00712      IF (CR-RESTART-POINT = 'ELARCH2 ' AND                        EL336
00713          CR-DATASET = ELARCH-FILE-NAME)                           EL336
00714        OR                                                         EL336
00715         (CR-RESTART-POINT = ('ELCERT2 ' OR                        EL336
00716                                     'ELCERT3 ' OR                 EL336
00717                                     'ELCERT5 ' OR                 EL336
00718                                     'ELCERT6 ') AND               EL336
00719          CR-DATASET = ELCERT-FILE-NAME)                           EL336
00720        OR                                                         EL336
00721         (CR-RESTART-POINT = ('ELMSTR2 ' OR                        EL336
00722                                     'ELMSTR3 ' OR                 EL336
00723                                     'ELMSTR5 ') AND               EL336
00724          CR-DATASET = ELMSTR-FILE-NAME)                           EL336
00725        OR                                                         EL336
00726         (CR-RESTART-POINT = 'ERACCT2 ' AND                        EL336
00727          CR-DATASET = ERACCT-FILE-NAME)                           EL336
00728        OR                                                         EL336
00729         (CR-RESTART-POINT = 'ERPNDB2 ' AND                        EL336
00730          CR-DATASET = ERPNDB-FILE-NAME)                           EL336
00731              MOVE CR-AIX-NUMBER      TO  WS-RESTART-POINT         EL336
00732              SUBTRACT +1 FROM WS-RESTART-POINT                    EL336
00733            ELSE                                                   EL336
00734              MOVE 'RESTART POINT NOT VALID'  TO  WS-ABEND-MESSAGE EL336
00735              GO TO ABEND-PGM.                                     EL336
00736                                                                   EL336
00737      EJECT                                                        EL336
00738  0200-MAIN-LOGIC.                                                 EL336
00739      PERFORM 3000-SORT-OUTPUT-PROCEDURE.                          EL336
00740                                                                   EL336
00741      MOVE '0'                    TO  WS-DETAIL-LINE1.             EL336
00742                                                                   EL336
00743      MOVE 'RECORDS RETURNED'     TO  WS-D2-MESSAGE.               EL336
00744      MOVE WS-RECORDS-RETURNED    TO  WS-D2-COUNT.                 EL336
00745      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL336
00746      PERFORM WRITE-PRINTER.                                       EL336
00747                                                                   EL336
00748      MOVE 'AIX02 RECORDS INPUT'  TO  WS-D2-MESSAGE.               EL336
00749      MOVE WS-AIX02-RECORDS-INPUT TO  WS-D2-COUNT.                 EL336
00750      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL336
00751      PERFORM WRITE-PRINTER.                                       EL336
00752                                                                   EL336
00753      MOVE 'AIX02 RECORDS OUTPUT' TO  WS-D2-MESSAGE.               EL336
00754      MOVE WS-AIX02-RECORDS-OUTPUT TO  WS-D2-COUNT.                EL336
00755      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL336
00756      PERFORM WRITE-PRINTER.                                       EL336
00757                                                                   EL336
00758      MOVE 'AIX03 RECORDS INPUT'  TO  WS-D2-MESSAGE.               EL336
00759      MOVE WS-AIX03-RECORDS-INPUT TO  WS-D2-COUNT.                 EL336
00760      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL336
00761      PERFORM WRITE-PRINTER.                                       EL336
00762                                                                   EL336
00763      MOVE 'AIX03 RECORDS OUTPUT' TO  WS-D2-MESSAGE.               EL336
00764      MOVE WS-AIX03-RECORDS-OUTPUT TO  WS-D2-COUNT.                EL336
00765      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL336
00766      PERFORM WRITE-PRINTER.                                       EL336
00767                                                                   EL336
00768      MOVE 'AIX04 RECORDS INPUT'  TO  WS-D2-MESSAGE.               EL336
00769      MOVE WS-AIX04-RECORDS-INPUT TO  WS-D2-COUNT.                 EL336
00770      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL336
00771      PERFORM WRITE-PRINTER.                                       EL336
00772                                                                   EL336
00773      MOVE 'AIX04 RECORDS OUTPUT' TO  WS-D2-MESSAGE.               EL336
00774      MOVE WS-AIX04-RECORDS-OUTPUT TO  WS-D2-COUNT.                EL336
00775      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL336
00776      PERFORM WRITE-PRINTER.                                       EL336
00777                                                                   EL336
00778      MOVE 'AIX05 RECORDS INPUT'  TO  WS-D2-MESSAGE.               EL336
00779      MOVE WS-AIX05-RECORDS-INPUT TO  WS-D2-COUNT.                 EL336
00780      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL336
00781      PERFORM WRITE-PRINTER.                                       EL336
00782                                                                   EL336
00783      MOVE 'AIX05 RECORDS OUTPUT' TO  WS-D2-MESSAGE.               EL336
00784      MOVE WS-AIX05-RECORDS-OUTPUT TO  WS-D2-COUNT.                EL336
00785      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL336
00786      PERFORM WRITE-PRINTER.                                       EL336
00787                                                                   EL336
00788      MOVE 'AIX06 RECORDS INPUT'  TO  WS-D2-MESSAGE.               EL336
00789      MOVE WS-AIX06-RECORDS-INPUT TO  WS-D2-COUNT.                 EL336
00790      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL336
00791      PERFORM WRITE-PRINTER.                                       EL336
00792                                                                   EL336
00793      MOVE 'AIX06 RECORDS OUTPUT' TO  WS-D2-MESSAGE.               EL336
00794      MOVE WS-AIX06-RECORDS-OUTPUT TO  WS-D2-COUNT.                EL336
00795      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL336
00796      PERFORM WRITE-PRINTER.                                       EL336
00797                                                                   EL336
00798      GO TO 0020-MAIN-LOGIC.                                       EL336
00799                                                                   EL336
00800  0900-MAIN-LOGIC.                                                 EL336
00801      CLOSE CARD-FILE                                              EL336
00802            PRNTR.                                                 EL336
00803                                                                   EL336
00804      GOBACK.                                                      EL336
00805      EJECT                                                        EL336
00806  3000-SORT-OUTPUT-PROCEDURE SECTION.                              EL336
00807                                                                   EL336
00808 *    NOTE ******************************************************* EL336
00809 *         *                                                     * EL336
00810 *         *      THIS SECTION READS THE SORTED ALTERNATE INDEX  * EL336
00811 *         *  KEYS AND LOADS THEM TO THE ALTERNATE INDEX RECORDS * EL336
00812 *         *                                                     * EL336
00813 *         *      ON THE FIRST RECORD OF EACH ALTERNATE INDEX    * EL336
00814 *         *  THE ALTERNATE INDEX VSAM FILE WILL BE OPENED.      * EL336
00815 *         *                                                     * EL336
00816 *         *      AS EACH RECORD IS BEING LOADED,WHEN THE RECORD * EL336
00817 *         *  IS FULL OR THE AIX KEY CHANGES,THE RECORD WILL BE  * EL336
00818 *         *  OUTPUT.  IF MORE ALTERNATE KEYS ARE AVAILABLE THAN * EL336
00819 *         *  WILL FIT IN THE ALTERNATE INDEX RECORD,THE AIX KEY * EL336
00820 *         *  THAT OVERFLOWED WILL BE PRINTED WITH THE NUMBER OF * EL336
00821 *         *  TIMES IT OVERFLOWED.                               * EL336
00822 *         *******************************************************.EL336
00823                                                                   EL336
00824      OPEN INPUT SORT-OUTPUT-FILE.                                 EL336
00825                                                                   EL336
00826  3100-SOP.                                                        EL336
00827      READ SORT-OUTPUT-FILE                                        EL336
00828          AT END                                                   EL336
00829              PERFORM 4000-WRITE-AIX                               EL336
00830              PERFORM 6000-CLOSE-AIX                               EL336
00831              PERFORM 7000-PRINT-COMPANY-TOTALS                    EL336
00832              CLOSE SORT-OUTPUT-FILE                               EL336
00833              GO TO 3900-EXIT.                                     EL336
00834                                                                   EL336
00835      ADD +1  TO  WS-RECORDS-RETURNED.                             EL336
00836                                                                   EL336
00837      DIVIDE WS-RECORDS-RETURNED                                   EL336
00838          BY WS-MESSAGE-OCCURRENCE (WS-DATASET)                    EL336
00839              GIVING WS-WORK REMAINDER WS-REMAINDER.               EL336
00840                                                                   EL336
00841      IF WS-REMAINDER = ZERO                                       EL336
00842          MOVE WS-RECORDS-RETURNED  TO  WS-D1-COUNT                EL336
00843          ACCEPT WS-TIME-OF-DAY     FROM  TIME                     EL336
00844          MOVE WS-TIME              TO  WS-D1-TIME                 EL336
00845          INSPECT WS-D1-TIME REPLACING ALL SPACES BY '.'           EL336
00846          MOVE 'RECORDS RETURNED='  TO  WS-D1-MESSAGE              EL336
00847          DISPLAY WS-DISPLAY1 UPON CONSOLE.                        EL336
00848                                                                   EL336
00849      IF SOR-AIX-NUMBER LESS THAN WS-RESTART-POINT                 EL336
00850          GO TO 3100-SOP.                                          EL336
00851                                                                   EL336
00852      EJECT                                                        EL336
00853      IF WS-RECORDS-RETURNED = +1                                  EL336
00854          MOVE SOR-AIX-NUMBER     TO  WS-LAST-AIX-NUMBER           EL336
00855          MOVE SOR-AIX-KEY        TO  WS-LAST-COMPANY-CD.          EL336
00856                                                                   EL336
00857      IF SOR-AIX-NUMBER NOT = WS-LAST-AIX-NUMBER                   EL336
00858          PERFORM 4000-WRITE-AIX                                   EL336
00859          PERFORM 7000-PRINT-COMPANY-TOTALS                        EL336
00860          MOVE SOR-AIX-KEY        TO  WS-LAST-COMPANY-CD           EL336
00861          PERFORM 6000-CLOSE-AIX                                   EL336
00862        ELSE                                                       EL336
00863          MOVE SOR-AIX-KEY        TO  WS-COMPANY-CD                EL336
00864          IF WS-COMPANY-CD NOT = WS-LAST-COMPANY-CD                EL336
00865              PERFORM 7000-PRINT-COMPANY-TOTALS                    EL336
00866              MOVE SOR-AIX-KEY    TO  WS-LAST-COMPANY-CD.          EL336
00867                                                                   EL336
00868      ADD +1  TO  WS-RECORDS-INPUT.                                EL336
00869                                                                   EL336
00870      MOVE SOR-AIX-NUMBER         TO  WS-LAST-AIX-NUMBER.          EL336
00871                                                                   EL336
00872      GO TO 3300-SOP-ELARCH                                        EL336
00873            3400-SOP-ELCERT                                        EL336
00874            3500-SOP-ELMSTR                                        EL336
00875            3600-SOP-ERACCT                                        EL336
00876            3700-SOP-ERPNDB DEPENDING ON WS-DATASET.               EL336
00877                                                                   EL336
00878      EJECT                                                        EL336
00879  3300-SOP-ELARCH. COPY ELC336P1 REPLACING                         EL336
00880      AIX02-INDEX                 BY  ELARCH-AIX02-INDEX           EL336
00881      AIX02-FLAG-BYTE             BY  ELARCH-AIX02-FLAG-BYTE       EL336
00882      AIX02-POINTER-LENGTH        BY  ELARCH-AIX02-POINTER-LENGTH  EL336
00883      AIX02-ALT-KEY-LENGTH        BY  ELARCH-AIX02-ALT-KEY-LENGTH  EL336
00884      AIX02-KEY                   BY  ELARCH-AIX02-KEY             EL336
00885      AIX02-PRIME-KEY             BY  ELARCH-AIX02-PRIME-KEY       EL336
00886      WS-POINTER-LENGTH           BY  BASE-KEY-LENGTH              EL336
00887      WS-AIX02-KEY-LENGTH         BY  AIX02-KEY-LENGTH             EL336
00888      WS-AIX02-RECORDS-INPUT      BY  WS-AIX02-RECORDS-INPUT       EL336
00889      WS-AIX02-MAX                BY  ELARCH-AIX02-MAX.            EL336
00890                                                                   EL336
00891      EJECT                                                        EL336
00892  3400-SOP-ELCERT.                                                 EL336
00893      GO TO 3420-SOP-ELCERT2                                       EL336
00894            3430-SOP-ELCERT3                                       EL336
00895            3440-SOP-ELCERT4                                       EL336
00896            3450-SOP-ELCERT5                                       EL336
00897            3460-SOP-ELCERT6 DEPENDING ON SOR-AIX-NUMBER.          EL336
00898                                                                   EL336
00899  3420-SOP-ELCERT2. COPY ELC336P1 REPLACING                        EL336
00900      AIX02-INDEX                 BY  ELCERT-AIX02-INDEX           EL336
00901      AIX02-FLAG-BYTE             BY  ELCERT-AIX02-FLAG-BYTE       EL336
00902      AIX02-POINTER-LENGTH        BY  ELCERT-AIX02-POINTER-LENGTH  EL336
00903      AIX02-ALT-KEY-LENGTH        BY  ELCERT-AIX02-ALT-KEY-LENGTH  EL336
00904      AIX02-KEY                   BY  ELCERT-AIX02-KEY             EL336
00905      AIX02-PRIME-KEY             BY  ELCERT-AIX02-PRIME-KEY       EL336
00906      WS-POINTER-LENGTH           BY  BASE-KEY-LENGTH              EL336
00907      WS-AIX02-KEY-LENGTH         BY  AIX02-KEY-LENGTH             EL336
00908      WS-AIX02-RECORDS-INPUT      BY  WS-AIX02-RECORDS-INPUT       EL336
00909      WS-AIX02-MAX                BY  ELCERT-AIX02-MAX.            EL336
00910                                                                   EL336
00911      EJECT                                                        EL336
00912  3430-SOP-ELCERT3. COPY ELC336P1 REPLACING                        EL336
00913      AIX02-INDEX                 BY  ELCERT-AIX03-INDEX           EL336
00914      AIX02-FLAG-BYTE             BY  ELCERT-AIX03-FLAG-BYTE       EL336
00915      AIX02-POINTER-LENGTH        BY  ELCERT-AIX03-POINTER-LENGTH  EL336
00916      AIX02-ALT-KEY-LENGTH        BY  ELCERT-AIX03-ALT-KEY-LENGTH  EL336
00917      AIX02-KEY                   BY  ELCERT-AIX03-KEY             EL336
00918      AIX02-PRIME-KEY             BY  ELCERT-AIX03-PRIME-KEY       EL336
00919      WS-POINTER-LENGTH           BY  BASE-KEY-LENGTH              EL336
00920      WS-AIX02-KEY-LENGTH         BY  AIX03-KEY-LENGTH             EL336
00921      WS-AIX02-RECORDS-INPUT      BY  WS-AIX03-RECORDS-INPUT       EL336
00922      WS-AIX02-MAX                BY  ELCERT-AIX03-MAX.            EL336
00923                                                                   EL336
00924      EJECT                                                        EL336
00925  3440-SOP-ELCERT4.                                                EL336
00926      GO TO 3100-SOP.                                              EL336
00927                                                                   EL336
00928  3450-SOP-ELCERT5. COPY ELC336P1 REPLACING                        EL336
00929      AIX02-INDEX                 BY  ELCERT-AIX05-INDEX           EL336
00930      AIX02-FLAG-BYTE             BY  ELCERT-AIX05-FLAG-BYTE       EL336
00931      AIX02-POINTER-LENGTH        BY  ELCERT-AIX05-POINTER-LENGTH  EL336
00932      AIX02-ALT-KEY-LENGTH        BY  ELCERT-AIX05-ALT-KEY-LENGTH  EL336
00933      AIX02-KEY                   BY  ELCERT-AIX05-KEY             EL336
00934      AIX02-PRIME-KEY             BY  ELCERT-AIX05-PRIME-KEY       EL336
00935      WS-POINTER-LENGTH           BY  BASE-KEY-LENGTH              EL336
00936      WS-AIX02-KEY-LENGTH         BY  AIX05-KEY-LENGTH             EL336
00937      WS-AIX02-RECORDS-INPUT      BY  WS-AIX05-RECORDS-INPUT       EL336
00938      WS-AIX02-MAX                BY  ELCERT-AIX05-MAX.            EL336
00939                                                                   EL336
00940      EJECT                                                        EL336
00941  3460-SOP-ELCERT6. COPY ELC336P1 REPLACING                        EL336
00942      AIX02-INDEX                 BY  ELCERT-AIX06-INDEX           EL336
00943      AIX02-FLAG-BYTE             BY  ELCERT-AIX06-FLAG-BYTE       EL336
00944      AIX02-POINTER-LENGTH        BY  ELCERT-AIX06-POINTER-LENGTH  EL336
00945      AIX02-ALT-KEY-LENGTH        BY  ELCERT-AIX06-ALT-KEY-LENGTH  EL336
00946      AIX02-KEY                   BY  ELCERT-AIX06-KEY             EL336
00947      AIX02-PRIME-KEY             BY  ELCERT-AIX06-PRIME-KEY       EL336
00948      WS-POINTER-LENGTH           BY  BASE-KEY-LENGTH              EL336
00949      WS-AIX02-KEY-LENGTH         BY  AIX06-KEY-LENGTH             EL336
00950      WS-AIX02-RECORDS-INPUT      BY  WS-AIX06-RECORDS-INPUT       EL336
00951      WS-AIX02-MAX                BY  ELCERT-AIX06-MAX.            EL336
00952                                                                   EL336
00953      EJECT                                                        EL336
00954  3500-SOP-ELMSTR.                                                 EL336
00955      GO TO 3520-SOP-ELMSTR2                                       EL336
00956            3530-SOP-ELMSTR3                                       EL336
00957            3540-SOP-ELMSTR4                                       EL336
00958            3550-SOP-ELMSTR5 DEPENDING ON SOR-AIX-NUMBER.          EL336
00959                                                                   EL336
00960  3520-SOP-ELMSTR2. COPY ELC336P1 REPLACING                        EL336
00961      AIX02-INDEX                 BY  ELMSTR-AIX02-INDEX           EL336
00962      AIX02-FLAG-BYTE             BY  ELMSTR-AIX02-FLAG-BYTE       EL336
00963      AIX02-POINTER-LENGTH        BY  ELMSTR-AIX02-POINTER-LENGTH  EL336
00964      AIX02-ALT-KEY-LENGTH        BY  ELMSTR-AIX02-ALT-KEY-LENGTH  EL336
00965      AIX02-KEY                   BY  ELMSTR-AIX02-KEY             EL336
00966      AIX02-PRIME-KEY             BY  ELMSTR-AIX02-PRIME-KEY       EL336
00967      WS-POINTER-LENGTH           BY  BASE-KEY-LENGTH              EL336
00968      WS-AIX02-KEY-LENGTH         BY  AIX02-KEY-LENGTH             EL336
00969      WS-AIX02-RECORDS-INPUT      BY  WS-AIX02-RECORDS-INPUT       EL336
00970      WS-AIX02-MAX                BY  ELMSTR-AIX02-MAX             EL336
00971      SOR-AIX-KEY                 BY SOR-AIX-KEY3                  EL336
00972      SOR-PRIME-KEY               BY SOR-PRIME-KEY3                EL336
00973      WS-LAST-AIX-KEY             BY WS-LAST-AIX-KEY2.             EL336
00974                                                                   EL336
00975      EJECT                                                        EL336
00976  3530-SOP-ELMSTR3. COPY ELC336P1 REPLACING                        EL336
00977      AIX02-INDEX                 BY  ELMSTR-AIX03-INDEX           EL336
00978      AIX02-FLAG-BYTE             BY  ELMSTR-AIX03-FLAG-BYTE       EL336
00979      AIX02-POINTER-LENGTH        BY  ELMSTR-AIX03-POINTER-LENGTH  EL336
00980      AIX02-ALT-KEY-LENGTH        BY  ELMSTR-AIX03-ALT-KEY-LENGTH  EL336
00981      AIX02-KEY                   BY  ELMSTR-AIX03-KEY             EL336
00982      AIX02-PRIME-KEY             BY  ELMSTR-AIX03-PRIME-KEY       EL336
00983      WS-POINTER-LENGTH           BY  BASE-KEY-LENGTH              EL336
00984      WS-AIX02-KEY-LENGTH         BY  AIX03-KEY-LENGTH             EL336
00985      WS-AIX02-RECORDS-INPUT      BY  WS-AIX03-RECORDS-INPUT       EL336
00986      WS-AIX02-MAX                BY  ELMSTR-AIX03-MAX.            EL336
00987                                                                   EL336
00988      EJECT                                                        EL336
00989  3540-SOP-ELMSTR4.                                                EL336
00990 *    NOTE ******************************************************* EL336
00991 *         *          THERE IS NO ELMSTR AIX 4                   * EL336
00992 *         *******************************************************.EL336
00993                                                                   EL336
00994      GO TO 3100-SOP.                                              EL336
00995                                                                   EL336
00996      EJECT                                                        EL336
00997  3550-SOP-ELMSTR5. COPY ELC336P1 REPLACING                        EL336
00998      AIX02-INDEX                 BY  ELMSTR-AIX05-INDEX           EL336
00999      AIX02-FLAG-BYTE             BY  ELMSTR-AIX05-FLAG-BYTE       EL336
01000      AIX02-POINTER-LENGTH        BY  ELMSTR-AIX05-POINTER-LENGTH  EL336
01001      AIX02-ALT-KEY-LENGTH        BY  ELMSTR-AIX05-ALT-KEY-LENGTH  EL336
01002      AIX02-KEY                   BY  ELMSTR-AIX05-KEY             EL336
01003      AIX02-PRIME-KEY             BY  ELMSTR-AIX05-PRIME-KEY       EL336
01004      WS-POINTER-LENGTH           BY  BASE-KEY-LENGTH              EL336
01005      WS-AIX02-KEY-LENGTH         BY  AIX05-KEY-LENGTH             EL336
01006      WS-AIX02-RECORDS-INPUT      BY  WS-AIX05-RECORDS-INPUT       EL336
01007      WS-AIX02-MAX                BY  ELMSTR-AIX05-MAX.            EL336
01008                                                                   EL336
01009      EJECT                                                        EL336
01010  3600-SOP-ERACCT. COPY ELC336P1 REPLACING                         EL336
01011      AIX02-INDEX                 BY  ERACCT-AIX02-INDEX           EL336
01012      AIX02-FLAG-BYTE             BY  ERACCT-AIX02-FLAG-BYTE       EL336
01013      AIX02-POINTER-LENGTH        BY  ERACCT-AIX02-POINTER-LENGTH  EL336
01014      AIX02-ALT-KEY-LENGTH        BY  ERACCT-AIX02-ALT-KEY-LENGTH  EL336
01015      AIX02-KEY                   BY  ERACCT-AIX02-KEY             EL336
01016      AIX02-PRIME-KEY             BY  ERACCT-AIX02-PRIME-KEY       EL336
01017      WS-POINTER-LENGTH           BY  BASE-KEY-LENGTH              EL336
01018      WS-AIX02-KEY-LENGTH         BY  AIX02-KEY-LENGTH             EL336
01019      WS-AIX02-RECORDS-INPUT      BY  WS-AIX02-RECORDS-INPUT       EL336
01020      WS-AIX02-MAX                BY  ERACCT-AIX02-MAX.            EL336
01021                                                                   EL336
01022      EJECT                                                        EL336
01023  3700-SOP-ERPNDB. COPY ELC336P1 REPLACING                         EL336
01024      AIX02-INDEX                 BY  ERPNDB-AIX02-INDEX           EL336
01025      AIX02-FLAG-BYTE             BY  ERPNDB-AIX02-FLAG-BYTE       EL336
01026      AIX02-POINTER-LENGTH        BY  ERPNDB-AIX02-POINTER-LENGTH  EL336
01027      AIX02-ALT-KEY-LENGTH        BY  ERPNDB-AIX02-ALT-KEY-LENGTH  EL336
01028      AIX02-KEY                   BY  ERPNDB-AIX02-KEY             EL336
01029      AIX02-PRIME-KEY             BY  ERPNDB-AIX02-PRIME-KEY       EL336
01030      WS-POINTER-LENGTH           BY  BASE-KEY-LENGTH              EL336
01031      WS-AIX02-KEY-LENGTH         BY  AIX02-KEY-LENGTH             EL336
01032      WS-AIX02-RECORDS-INPUT      BY  WS-AIX02-RECORDS-INPUT       EL336
01033      WS-AIX02-MAX                BY  ERPNDB-AIX02-MAX             EL336
01034      SOR-AIX-KEY                 BY  SOR-AIX-KEY2                 EL336
01035      SOR-PRIME-KEY               BY  SOR-PRIME-KEY2               EL336
01036      WS-LAST-AIX-KEY             BY  WS-LAST-AIX-KEY2.            EL336
01037                                                                   EL336
01038  3900-EXIT.                                                       EL336
01039      EXIT.                                                        EL336
01040                                                                   EL336
01041      EJECT                                                        EL336
01042  4000-WRITE-AIX SECTION.                                          EL336
01043                                                                   EL336
01044      IF WS-OVERFLOW-COUNT NOT GREATER THAN ZERO                   EL336
01045          GO TO 4100-WRITE-AIX.                                    EL336
01046                                                                   EL336
01047      MOVE '0'                    TO  WS-DETAIL-LINE1.             EL336
01048                                                                   EL336
01049      IF PROCESSING-ERPNDB-FILE                                    EL336
01050          MOVE WS-LAST-AIX-KEY2   TO  WS-D3-AIX-KEY                EL336
01051                                      WS-COMPANY-CD                EL336
01052        ELSE                                                       EL336
01053          MOVE WS-LAST-AIX-KEY    TO  WS-D3-AIX-KEY                EL336
01054                                      WS-COMPANY-CD.               EL336
01055                                                                   EL336
01056      ADD  +1  WS-LAST-AIX-NUMBER  GIVING  WS-D3-AIX.              EL336
01057      MOVE WS-COMPANY-NUMBER      TO  WS-D3-COMPANY.               EL336
01058      MOVE 'AIX KEY OVERFLOW'     TO  WS-D3-MESSAGE.               EL336
01059      MOVE WS-OVERFLOW-COUNT      TO  WS-D3-COUNT.                 EL336
01060      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL336
01061      PERFORM WRITE-PRINTER.                                       EL336
01062                                                                   EL336
01063      MOVE SPACES                 TO  WS-DETAIL-LINE1.             EL336
01064                                                                   EL336
01065      SET CHAR-INDEX TO +1.                                        EL336
01066      MOVE SPACES                 TO  WS-DIGET-CHARACTERS.         EL336
01067                                                                   EL336
01068  4020-WRITE-AIX.                                                  EL336
01069      MOVE WS-KEY-CHAR (CHAR-INDEX) TO WS-CHAR.                    EL336
01070                                                                   EL336
01071      IF WS-NUMBER = ZERO                                          EL336
01072          MOVE ZERO               TO  WS-D3-CHAR (CHAR-INDEX)      EL336
01073                                      WS-DIGET (CHAR-INDEX)        EL336
01074          GO TO 4030-WRITE-AIX.                                    EL336
01075                                                                   EL336
01076      DIVIDE WS-NUMBER BY +16 GIVING WS-NUMBER                     EL336
01077          REMAINDER WS-REMAINDER.                                  EL336
01078                                                                   EL336
01079      IF WS-NUMBER NOT = ZERO                                      EL336
01080          MOVE WS-HEX-CHAR (WS-NUMBER) TO  WS-D3-CHAR (CHAR-INDEX) EL336
01081        ELSE                                                       EL336
01082          MOVE ZERO               TO  WS-D3-CHAR (CHAR-INDEX).     EL336
01083                                                                   EL336
01084      IF WS-REMAINDER NOT = ZERO                                   EL336
01085          MOVE WS-HEX-CHAR (WS-REMAINDER) TO  WS-DIGET (CHAR-INDEX)EL336
01086        ELSE                                                       EL336
01087          MOVE ZERO               TO  WS-DIGET (CHAR-INDEX).       EL336
01088                                                                   EL336
01089  4030-WRITE-AIX.                                                  EL336
01090      IF CHAR-INDEX LESS THAN WS-AIX-KEY-LENGTH                    EL336
01091                                   (WS-DATASET, WS-LAST-AIX-NUMBER)EL336
01092          SET CHAR-INDEX UP BY +1                                  EL336
01093          GO TO 4020-WRITE-AIX.                                    EL336
01094                                                                   EL336
01095      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL336
01096      PERFORM WRITE-PRINTER.                                       EL336
01097                                                                   EL336
01098      MOVE WS-DIGET-CHARACTERS    TO  WS-D3-AIX-KEY.               EL336
01099                                                                   EL336
01100      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL336
01101      PERFORM WRITE-PRINTER.                                       EL336
01102                                                                   EL336
01103      MOVE ZERO                   TO  WS-OVERFLOW-COUNT.           EL336
01104                                                                   EL336
01105  4100-WRITE-AIX.                                                  EL336
01106      ADD +1 TO WS-RECORDS-OUTPUT.                                 EL336
01107                                                                   EL336
01108      GO TO 4300-WRITE-ELARCH                                      EL336
01109            4400-WRITE-ELCERT                                      EL336
01110            4500-WRITE-ELMSTR                                      EL336
01111            4600-WRITE-ERACCT                                      EL336
01112            4700-WRITE-ERPNDB DEPENDING ON WS-DATASET.             EL336
01113                                                                   EL336
01114      EJECT                                                        EL336
01115  4300-WRITE-ELARCH. COPY ELC336P2 REPLACING                       EL336
01116      AIX02-INDEX                 BY  ELARCH-AIX02-INDEX           EL336
01117      AIX02-NUMBER-OF-KEYS        BY  ELARCH-AIX02-NUMBER-OF-KEYS  EL336
01118      AIX02-RECORD                BY  ELARCH-AIX02-RECORD          EL336
01119      AIX02-FILE-STATUS           BY  ELARCH-AIX02-FILE-STATUS     EL336
01120      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX02-RECORDS-OUTPUT.     EL336
01121                                                                   EL336
01122      EJECT                                                        EL336
01123  4400-WRITE-ELCERT.                                               EL336
01124      GO TO 4420-WRITE-ELCERT2                                     EL336
01125            4430-WRITE-ELCERT3                                     EL336
01126            4440-WRITE-ELCERT4                                     EL336
01127            4450-WRITE-ELCERT5                                     EL336
01128            4460-WRITE-ELCERT6 DEPENDING ON WS-LAST-AIX-NUMBER.    EL336
01129                                                                   EL336
01130      EJECT                                                        EL336
01131  4420-WRITE-ELCERT2. COPY ELC336P2 REPLACING                      EL336
01132      AIX02-INDEX                 BY  ELCERT-AIX02-INDEX           EL336
01133      AIX02-NUMBER-OF-KEYS        BY  ELCERT-AIX02-NUMBER-OF-KEYS  EL336
01134      AIX02-RECORD                BY  ELCERT-AIX02-RECORD          EL336
01135      AIX02-FILE-STATUS           BY  ELCERT-AIX02-FILE-STATUS     EL336
01136      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX02-RECORDS-OUTPUT.     EL336
01137                                                                   EL336
01138      EJECT                                                        EL336
01139  4430-WRITE-ELCERT3. COPY ELC336P2 REPLACING                      EL336
01140      AIX02-INDEX                 BY  ELCERT-AIX03-INDEX           EL336
01141      AIX02-NUMBER-OF-KEYS        BY  ELCERT-AIX03-NUMBER-OF-KEYS  EL336
01142      AIX02-RECORD                BY  ELCERT-AIX03-RECORD          EL336
01143      AIX02-FILE-STATUS           BY  ELCERT-AIX03-FILE-STATUS     EL336
01144      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX03-RECORDS-OUTPUT.     EL336
01145                                                                   EL336
01146      EJECT                                                        EL336
01147  4440-WRITE-ELCERT4.                                              EL336
01148      GO TO 4900-EXIT.                                             EL336
01149                                                                   EL336
01150  4450-WRITE-ELCERT5. COPY ELC336P2 REPLACING                      EL336
01151      AIX02-INDEX                 BY  ELCERT-AIX05-INDEX           EL336
01152      AIX02-NUMBER-OF-KEYS        BY  ELCERT-AIX05-NUMBER-OF-KEYS  EL336
01153      AIX02-RECORD                BY  ELCERT-AIX05-RECORD          EL336
01154      AIX02-FILE-STATUS           BY  ELCERT-AIX05-FILE-STATUS     EL336
01155      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX05-RECORDS-OUTPUT.     EL336
01156                                                                   EL336
01157      EJECT                                                        EL336
01158  4460-WRITE-ELCERT6. COPY ELC336P2 REPLACING                      EL336
01159      AIX02-INDEX                 BY  ELCERT-AIX06-INDEX           EL336
01160      AIX02-NUMBER-OF-KEYS        BY  ELCERT-AIX06-NUMBER-OF-KEYS  EL336
01161      AIX02-RECORD                BY  ELCERT-AIX06-RECORD          EL336
01162      AIX02-FILE-STATUS           BY  ELCERT-AIX06-FILE-STATUS     EL336
01163      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX06-RECORDS-OUTPUT.     EL336
01164                                                                   EL336
01165      EJECT                                                        EL336
01166  4500-WRITE-ELMSTR.                                               EL336
01167      GO TO 4520-WRITE-ELMSTR2                                     EL336
01168            4530-WRITE-ELMSTR3                                     EL336
01169            4540-WRITE-ELMSTR4                                     EL336
01170            4550-WRITE-ELMSTR5 DEPENDING ON WS-LAST-AIX-NUMBER.    EL336
01171                                                                   EL336
01172      EJECT                                                        EL336
01173  4520-WRITE-ELMSTR2. COPY ELC336P2 REPLACING                      EL336
01174      AIX02-INDEX                 BY  ELMSTR-AIX02-INDEX           EL336
01175      AIX02-NUMBER-OF-KEYS        BY  ELMSTR-AIX02-NUMBER-OF-KEYS  EL336
01176      AIX02-RECORD                BY  ELMSTR-AIX02-RECORD          EL336
01177      AIX02-FILE-STATUS           BY  ELMSTR-AIX02-FILE-STATUS     EL336
01178      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX02-RECORDS-OUTPUT.     EL336
01179                                                                   EL336
01180      EJECT                                                        EL336
01181  4530-WRITE-ELMSTR3. COPY ELC336P2 REPLACING                      EL336
01182      AIX02-INDEX                 BY  ELMSTR-AIX03-INDEX           EL336
01183      AIX02-NUMBER-OF-KEYS        BY  ELMSTR-AIX03-NUMBER-OF-KEYS  EL336
01184      AIX02-RECORD                BY  ELMSTR-AIX03-RECORD          EL336
01185      AIX02-FILE-STATUS           BY  ELMSTR-AIX03-FILE-STATUS     EL336
01186      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX03-RECORDS-OUTPUT.     EL336
01187                                                                   EL336
01188      EJECT                                                        EL336
01189  4540-WRITE-ELMSTR4.                                              EL336
01190 *    NOTE ******************************************************* EL336
01191 *         *        THERE IS NO CLAIM MASTER AIX FOUR            * EL336
01192 *         *******************************************************.EL336
01193                                                                   EL336
01194      GO TO 4900-EXIT.                                             EL336
01195                                                                   EL336
01196      EJECT                                                        EL336
01197  4550-WRITE-ELMSTR5. COPY ELC336P2 REPLACING                      EL336
01198      AIX02-INDEX                 BY  ELMSTR-AIX05-INDEX           EL336
01199      AIX02-NUMBER-OF-KEYS        BY  ELMSTR-AIX05-NUMBER-OF-KEYS  EL336
01200      AIX02-RECORD                BY  ELMSTR-AIX05-RECORD          EL336
01201      AIX02-FILE-STATUS           BY  ELMSTR-AIX05-FILE-STATUS     EL336
01202      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX05-RECORDS-OUTPUT.     EL336
01203                                                                   EL336
01204      EJECT                                                        EL336
01205  4600-WRITE-ERACCT. COPY ELC336P2 REPLACING                       EL336
01206      AIX02-INDEX                 BY  ERACCT-AIX02-INDEX           EL336
01207      AIX02-NUMBER-OF-KEYS        BY  ERACCT-AIX02-NUMBER-OF-KEYS  EL336
01208      AIX02-RECORD                BY  ERACCT-AIX02-RECORD          EL336
01209      AIX02-FILE-STATUS           BY  ERACCT-AIX02-FILE-STATUS     EL336
01210      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX02-RECORDS-OUTPUT.     EL336
01211                                                                   EL336
01212      EJECT                                                        EL336
01213  4700-WRITE-ERPNDB. COPY ELC336P2 REPLACING                       EL336
01214      AIX02-INDEX                 BY  ERPNDB-AIX02-INDEX           EL336
01215      AIX02-NUMBER-OF-KEYS        BY  ERPNDB-AIX02-NUMBER-OF-KEYS  EL336
01216      AIX02-RECORD                BY  ERPNDB-AIX02-RECORD          EL336
01217      AIX02-FILE-STATUS           BY  ERPNDB-AIX02-FILE-STATUS     EL336
01218      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX02-RECORDS-OUTPUT.     EL336
01219                                                                   EL336
01220  4900-EXIT.                                                       EL336
01221      EXIT.                                                        EL336
01222                                                                   EL336
01223      EJECT                                                        EL336
01224  5000-OPEN-AIX SECTION.                                           EL336
01225                                                                   EL336
01226      GO TO 5300-OPEN-ELARCH                                       EL336
01227            5400-OPEN-ELCERT                                       EL336
01228            5500-OPEN-ELMSTR                                       EL336
01229            5600-OPEN-ERACCT                                       EL336
01230            5700-OPEN-ERPNDB DEPENDING ON WS-DATASET.              EL336
01231                                                                   EL336
01232  5300-OPEN-ELARCH. COPY ELC336P3 REPLACING                        EL336
01233      CERTIFICATE-MASTER-AIX02    BY  ELARCH-AIX02                 EL336
01234      AIX02-FILE-STATUS           BY  ELARCH-AIX02-FILE-STATUS.    EL336
01235                                                                   EL336
01236      EJECT                                                        EL336
01237  5400-OPEN-ELCERT.                                                EL336
01238      GO TO 5420-OPEN-ELCERT2                                      EL336
01239            5430-OPEN-ELCERT3                                      EL336
01240            5440-OPEN-ELCERT4                                      EL336
01241            5450-OPEN-ELCERT5                                      EL336
01242            5460-OPEN-ELCERT6 DEPENDING ON WS-LAST-AIX-NUMBER.     EL336
01243                                                                   EL336
01244  5420-OPEN-ELCERT2. COPY ELC336P3 REPLACING                       EL336
01245      CERTIFICATE-MASTER-AIX02    BY  ELCERT-AIX02                 EL336
01246      AIX02-FILE-STATUS           BY  ELCERT-AIX02-FILE-STATUS.    EL336
01247                                                                   EL336
01248  5430-OPEN-ELCERT3. COPY ELC336P3 REPLACING                       EL336
01249      CERTIFICATE-MASTER-AIX02    BY  ELCERT-AIX03                 EL336
01250      AIX02-FILE-STATUS           BY  ELCERT-AIX03-FILE-STATUS.    EL336
01251                                                                   EL336
01252      EJECT                                                        EL336
01253  5440-OPEN-ELCERT4.                                               EL336
01254      GO TO 5900-EXIT.                                             EL336
01255                                                                   EL336
01256  5450-OPEN-ELCERT5. COPY ELC336P3 REPLACING                       EL336
01257      CERTIFICATE-MASTER-AIX02    BY  ELCERT-AIX05                 EL336
01258      AIX02-FILE-STATUS           BY  ELCERT-AIX05-FILE-STATUS.    EL336
01259                                                                   EL336
01260  5460-OPEN-ELCERT6. COPY ELC336P3 REPLACING                       EL336
01261      CERTIFICATE-MASTER-AIX02    BY  ELCERT-AIX06                 EL336
01262      AIX02-FILE-STATUS           BY  ELCERT-AIX06-FILE-STATUS.    EL336
01263                                                                   EL336
01264      EJECT                                                        EL336
01265  5500-OPEN-ELMSTR.                                                EL336
01266      GO TO 5520-OPEN-ELMSTR2                                      EL336
01267            5530-OPEN-ELMSTR3                                      EL336
01268            5540-OPEN-ELMSTR4                                      EL336
01269            5550-OPEN-ELMSTR5 DEPENDING ON WS-LAST-AIX-NUMBER.     EL336
01270                                                                   EL336
01271  5520-OPEN-ELMSTR2. COPY ELC336P3 REPLACING                       EL336
01272      CERTIFICATE-MASTER-AIX02    BY  ELMSTR-AIX02                 EL336
01273      AIX02-FILE-STATUS           BY  ELMSTR-AIX02-FILE-STATUS.    EL336
01274                                                                   EL336
01275  5530-OPEN-ELMSTR3. COPY ELC336P3 REPLACING                       EL336
01276      CERTIFICATE-MASTER-AIX02    BY  ELMSTR-AIX03                 EL336
01277      AIX02-FILE-STATUS           BY  ELMSTR-AIX03-FILE-STATUS.    EL336
01278                                                                   EL336
01279      EJECT                                                        EL336
01280  5540-OPEN-ELMSTR4.                                               EL336
01281 *    NOTE ******************************************************* EL336
01282 *         *        THERE IS NO CLAIM MASTER AIX FOUR            * EL336
01283 *         *******************************************************.EL336
01284                                                                   EL336
01285      GO TO 5900-EXIT.                                             EL336
01286                                                                   EL336
01287  5550-OPEN-ELMSTR5. COPY ELC336P3 REPLACING                       EL336
01288      CERTIFICATE-MASTER-AIX02    BY  ELMSTR-AIX05                 EL336
01289      AIX02-FILE-STATUS           BY  ELMSTR-AIX05-FILE-STATUS.    EL336
01290                                                                   EL336
01291      EJECT                                                        EL336
01292  5600-OPEN-ERACCT. COPY ELC336P3 REPLACING                        EL336
01293      CERTIFICATE-MASTER-AIX02    BY  ERACCT-AIX02                 EL336
01294      AIX02-FILE-STATUS           BY  ERACCT-AIX02-FILE-STATUS.    EL336
01295                                                                   EL336
01296  5700-OPEN-ERPNDB. COPY ELC336P3 REPLACING                        EL336
01297      CERTIFICATE-MASTER-AIX02    BY  ERPNDB-AIX02                 EL336
01298      AIX02-FILE-STATUS           BY  ERPNDB-AIX02-FILE-STATUS.    EL336
01299                                                                   EL336
01300  5900-EXIT.                                                       EL336
01301      EXIT.                                                        EL336
01302                                                                   EL336
01303      EJECT                                                        EL336
01304  6000-CLOSE-AIX SECTION.                                          EL336
01305                                                                   EL336
01306      MOVE '0AIX TOTALS'          TO  PRT.                         EL336
01307      PERFORM WRITE-PRINTER.                                       EL336
01308                                                                   EL336
01309  6010-CLOSE-AIX.                                                  EL336
01310      MOVE WS-TOTAL-COUNT-AREA  TO  WS-AIX-COUNT-AREA.             EL336
01311                                                                   EL336
01312      SORT SORT-WORK-FILE                                          EL336
01313          ON DESCENDING KEY SWR-FREQ                               EL336
01314          INPUT PROCEDURE IS 8000-SORT-INPUT-PROCEDURE             EL336
01315          OUTPUT PROCEDURE IS 8100-SORT-OUTPUT-PROCEDURE           EL336
01316                                                                   EL336
01317      MOVE WS-COUNT-AREA-INITALIZED TO WS-TOTAL-COUNT-AREA         EL336
01318                                  WS-AIX-COUNT-AREA.               EL336
01319                                                                   EL336
01320  6020-CLOSE-AIX.                                                  EL336
01321      GO TO 6300-CLOSE-ELARCH                                      EL336
01322            6400-CLOSE-ELCERT                                      EL336
01323            6500-CLOSE-ELMSTR                                      EL336
01324            6600-CLOSE-ERACCT                                      EL336
01325            6700-CLOSE-ERPNDB DEPENDING ON WS-DATASET.             EL336
01326                                                                   EL336
01327      EJECT                                                        EL336
01328  6300-CLOSE-ELARCH. COPY ELC336P4 REPLACING                       EL336
01329      CERTIFICATE-MASTER-AIX02    BY  ELARCH-AIX02                 EL336
01330      AIX02-FILE-STATUS           BY  ELARCH-AIX02-FILE-STATUS.    EL336
01331                                                                   EL336
01332      EJECT                                                        EL336
01333  6400-CLOSE-ELCERT.                                               EL336
01334      GO TO 6420-CLOSE-ELCERT2                                     EL336
01335            6430-CLOSE-ELCERT3                                     EL336
01336            6440-CLOSE-ELCERT4                                     EL336
01337            6450-CLOSE-ELCERT5                                     EL336
01338            6460-CLOSE-ELCERT6 DEPENDING ON WS-LAST-AIX-NUMBER.    EL336
01339                                                                   EL336
01340  6420-CLOSE-ELCERT2. COPY ELC336P4 REPLACING                      EL336
01341      CERTIFICATE-MASTER-AIX02    BY  ELCERT-AIX02                 EL336
01342      AIX02-FILE-STATUS           BY  ELCERT-AIX02-FILE-STATUS.    EL336
01343                                                                   EL336
01344      EJECT                                                        EL336
01345  6430-CLOSE-ELCERT3. COPY ELC336P4 REPLACING                      EL336
01346      CERTIFICATE-MASTER-AIX02    BY  ELCERT-AIX03                 EL336
01347      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX03-RECORDS-OUTPUT      EL336
01348      AIX02-FILE-STATUS           BY  ELCERT-AIX03-FILE-STATUS.    EL336
01349                                                                   EL336
01350      EJECT                                                        EL336
01351  6440-CLOSE-ELCERT4.                                              EL336
01352      GO TO 6900-EXIT.                                             EL336
01353                                                                   EL336
01354  6450-CLOSE-ELCERT5. COPY ELC336P4 REPLACING                      EL336
01355      CERTIFICATE-MASTER-AIX02    BY  ELCERT-AIX05                 EL336
01356      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX05-RECORDS-OUTPUT      EL336
01357      AIX02-FILE-STATUS           BY  ELCERT-AIX05-FILE-STATUS.    EL336
01358                                                                   EL336
01359      EJECT                                                        EL336
01360  6460-CLOSE-ELCERT6. COPY ELC336P4 REPLACING                      EL336
01361      CERTIFICATE-MASTER-AIX02    BY  ELCERT-AIX06                 EL336
01362      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX06-RECORDS-OUTPUT      EL336
01363      AIX02-FILE-STATUS           BY  ELCERT-AIX06-FILE-STATUS.    EL336
01364                                                                   EL336
01365      EJECT                                                        EL336
01366  6500-CLOSE-ELMSTR.                                               EL336
01367      GO TO 6520-CLOSE-ELMSTR2                                     EL336
01368            6530-CLOSE-ELMSTR3                                     EL336
01369            6540-CLOSE-ELMSTR4                                     EL336
01370            6550-CLOSE-ELMSTR5 DEPENDING ON WS-LAST-AIX-NUMBER.    EL336
01371                                                                   EL336
01372  6520-CLOSE-ELMSTR2. COPY ELC336P4 REPLACING                      EL336
01373      CERTIFICATE-MASTER-AIX02    BY  ELMSTR-AIX02                 EL336
01374      AIX02-FILE-STATUS           BY  ELMSTR-AIX02-FILE-STATUS.    EL336
01375                                                                   EL336
01376      EJECT                                                        EL336
01377  6530-CLOSE-ELMSTR3. COPY ELC336P4 REPLACING                      EL336
01378      CERTIFICATE-MASTER-AIX02    BY  ELMSTR-AIX03                 EL336
01379      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX03-RECORDS-OUTPUT      EL336
01380      AIX02-FILE-STATUS           BY  ELMSTR-AIX03-FILE-STATUS.    EL336
01381                                                                   EL336
01382  6540-CLOSE-ELMSTR4.                                              EL336
01383 *    NOTE ******************************************************* EL336
01384 *         *        THERE IS NO CLAIM MASTER AIX FOUR            * EL336
01385 *         *******************************************************.EL336
01386                                                                   EL336
01387      GO TO 6900-EXIT.                                             EL336
01388                                                                   EL336
01389      EJECT                                                        EL336
01390  6550-CLOSE-ELMSTR5. COPY ELC336P4 REPLACING                      EL336
01391      CERTIFICATE-MASTER-AIX02    BY  ELMSTR-AIX05                 EL336
01392      WS-AIX02-RECORDS-OUTPUT     BY  WS-AIX03-RECORDS-OUTPUT      EL336
01393      AIX02-FILE-STATUS           BY  ELMSTR-AIX05-FILE-STATUS.    EL336
01394                                                                   EL336
01395      EJECT                                                        EL336
01396  6600-CLOSE-ERACCT. COPY ELC336P4 REPLACING                       EL336
01397      CERTIFICATE-MASTER-AIX02    BY  ERACCT-AIX02                 EL336
01398      AIX02-FILE-STATUS           BY  ERACCT-AIX02-FILE-STATUS.    EL336
01399                                                                   EL336
01400      EJECT                                                        EL336
01401  6700-CLOSE-ERPNDB. COPY ELC336P4 REPLACING                       EL336
01402      CERTIFICATE-MASTER-AIX02    BY  ERPNDB-AIX02                 EL336
01403      AIX02-FILE-STATUS           BY  ERPNDB-AIX02-FILE-STATUS.    EL336
01404                                                                   EL336
01405  6900-EXIT.                                                       EL336
01406      EXIT.                                                        EL336
01407                                                                   EL336
01408      EJECT                                                        EL336
01409  7000-PRINT-COMPANY-TOTALS SECTION.                               EL336
01410                                                                   EL336
01411      MOVE '0'                TO  WS-DETAIL-LINE1.                 EL336
01412      MOVE WS-EM-FILE-NAME    TO  WS-D4-FILE-NAME.                 EL336
01413      ADD +1 WS-LAST-AIX-NUMBER GIVING WS-D4-AIX-NUMBER.           EL336
01414      MOVE WS-LAST-COMPANY-CD TO  WS-COMPANY-CD.                   EL336
01415      MOVE WS-COMPANY-NUMBER  TO  WS-D4-COMPANY-NUMBER.            EL336
01416                                                                   EL336
01417      DIVIDE WS-COMPANY-NUMBER BY +16                              EL336
01418          GIVING WS-WORK REMAINDER WS-REMAINDER.                   EL336
01419                                                                   EL336
01420      IF WS-WORK GREATER THAN ZERO                                 EL336
01421          MOVE WS-HEX-CHAR (WS-WORK) TO WS-D4-COMPANY-CD1          EL336
01422        ELSE                                                       EL336
01423          MOVE ZERO               TO  WS-D4-COMPANY-CD1.           EL336
01424                                                                   EL336
01425      IF WS-REMAINDER GREATER THAN ZERO                            EL336
01426          MOVE WS-HEX-CHAR (WS-REMAINDER) TO WS-D4-COMPANY-CD2     EL336
01427        ELSE                                                       EL336
01428          MOVE ZERO               TO  WS-D4-COMPANY-CD2.           EL336
01429                                                                   EL336
01430      SET CI TO +1.                                                EL336
01431                                                                   EL336
01432  7020-PRINT-COMPANY-TOTALS.                                       EL336
01433      IF WS-COMPANY-CD = WS-COMPANY-CODE (CI)                      EL336
01434          MOVE WS-COMPANY-ID   (CI) TO WS-D4-COMPANY-ID            EL336
01435          MOVE WS-COMPANY-NAME (CI) TO WS-D4-COMPANY-NAME          EL336
01436          GO TO 7030-PRINT-COMPANY-TOTALS.                         EL336
01437                                                                   EL336
01438      IF CI LESS THAN CI-MAX                                       EL336
01439          SET CI UP BY +1                                          EL336
01440          GO TO 7020-PRINT-COMPANY-TOTALS.                         EL336
01441                                                                   EL336
01442      MOVE '*** UNKNOWN COMPANY CODE ***' TO WS-D4-COMPANY-NAME.   EL336
01443                                                                   EL336
01444      EJECT                                                        EL336
01445  7030-PRINT-COMPANY-TOTALS.                                       EL336
01446      MOVE 'RECORDS OUTPUT='  TO  WS-D4-MESSAGE.                   EL336
01447      MOVE WS-RECORDS-OUTPUT  TO  WS-D4-COUNT.                     EL336
01448      MOVE ZERO               TO  WS-RECORDS-OUTPUT.               EL336
01449      MOVE WS-DETAIL-LINE1    TO  PRT.                             EL336
01450      PERFORM WRITE-PRINTER.                                       EL336
01451                                                                   EL336
01452      SORT SORT-WORK-FILE                                          EL336
01453          ON DESCENDING KEY SWR-FREQ                               EL336
01454          INPUT PROCEDURE IS 8000-SORT-INPUT-PROCEDURE             EL336
01455          OUTPUT PROCEDURE IS 8100-SORT-OUTPUT-PROCEDURE           EL336
01456                                                                   EL336
01457      SET CI1 TO +1.                                               EL336
01458                                                                   EL336
01459  7040-PRINT-COMPANY-TOTALS.                                       EL336
01460      IF WS-AIX-COUNT-OCCURS (CI1) = ZERO                          EL336
01461          MOVE WS-COUNT-AREA-INITALIZED TO WS-AIX-COUNT-AREA       EL336
01462          SET CI1 TO +1                                            EL336
01463          GO TO 7090-EXIT.                                         EL336
01464                                                                   EL336
01465      IF WS-TOTAL-COUNT-OCCURS (TI1) = WS-AIX-COUNT-OCCURS (CI1)   EL336
01466          ADD WS-AIX-COUNT-FREQ (CI1) TO WS-TOTAL-COUNT-FREQ (TI1) EL336
01467        ELSE                                                       EL336
01468          SET TI1 TO +1                                            EL336
01469          SEARCH WS-TOTAL-COUNT-ARRAY VARYING TI1                  EL336
01470              WHEN WS-TOTAL-COUNT-OCCURS (TI1) =                   EL336
01471                                  WS-AIX-COUNT-OCCURS (CI1)        EL336
01472                  ADD WS-AIX-COUNT-FREQ (CI1)                      EL336
01473                                  TO  WS-TOTAL-COUNT-FREQ (TI1)    EL336
01474              WHEN WS-TOTAL-COUNT-OCCURS (TI1) = ZERO              EL336
01475                  MOVE WS-AIX-COUNT-OCCURS (CI1)                   EL336
01476                                  TO  WS-TOTAL-COUNT-OCCURS (TI1)  EL336
01477                  ADD WS-AIX-COUNT-FREQ (CI1)                      EL336
01478                                  TO  WS-TOTAL-COUNT-FREQ (TI1).   EL336
01479                                                                   EL336
01480      IF CI1 IS LESS THAN +500                                     EL336
01481          SET CI1 UP BY +1                                         EL336
01482          GO TO 7040-PRINT-COMPANY-TOTALS.                         EL336
01483                                                                   EL336
01484  7090-EXIT.                                                       EL336
01485      EXIT.                                                        EL336
01486                                                                   EL336
01487      EJECT                                                        EL336
01488  8000-SORT-INPUT-PROCEDURE SECTION.                               EL336
01489                                                                   EL336
01490      SET CI1 TO +1.                                               EL336
01491      MOVE '0'                    TO  WS-DETAIL-LINE1.             EL336
01492                                                                   EL336
01493  8010-SIP.                                                        EL336
01494      IF WS-AIX-COUNT-OCCURS (CI1) = ZERO                          EL336
01495          GO TO 8099-EXIT.                                         EL336
01496                                                                   EL336
01497      MOVE WS-AIX-COUNT-OCCURS (CI1) TO SWR-OCCURS.                EL336
01498      MOVE WS-AIX-COUNT-FREQ   (CI1) TO SWR-FREQ.                  EL336
01499                                                                   EL336
01500      RELEASE SORT-WORK-RECORD.                                    EL336
01501                                                                   EL336
01502      IF CI1 IS LESS THAN +500                                     EL336
01503          SET CI1 UP BY +1                                         EL336
01504          GO TO 8010-SIP.                                          EL336
01505                                                                   EL336
01506  8099-EXIT.                                                       EL336
01507      EXIT.                                                        EL336
01508                                                                   EL336
01509      EJECT                                                        EL336
01510  8100-SORT-OUTPUT-PROCEDURE SECTION.                              EL336
01511                                                                   EL336
01512  8110-SOP.                                                        EL336
01513      RETURN SORT-WORK-FILE                                        EL336
01514          AT END                                                   EL336
01515              GO TO 8199-EXIT.                                     EL336
01516                                                                   EL336
01517      MOVE SWR-OCCURS       TO WS-D5-AIX-OCCURS.                   EL336
01518      MOVE SWR-FREQ         TO WS-D5-AIX-OCCURS-FREQ.              EL336
01519                                                                   EL336
01520      MOVE WS-DETAIL-LINE1 TO  PRT.                                EL336
01521      PERFORM WRITE-PRINTER.                                       EL336
01522                                                                   EL336
01523      MOVE SPACES           TO  WS-DETAIL-LINE1.                   EL336
01524      GO TO 8110-SOP.                                              EL336
01525                                                                   EL336
01526  8199-EXIT.                                                       EL336
01527      EXIT.                                                        EL336
01528                                                                   EL336
01529      EJECT                                                        EL336
01530  WRITE-PRINTER SECTION.                                           EL336
01531                                                                   EL336
01532 *    NOTE ******************************************************* EL336
01533 *         * THIS SECTION CONTROLS WRITES TO THE PRINTER         * EL336
01534 *         *******************************************************.EL336
01535                                                                   EL336
01536  WPS-010.                                                         EL336
01537      WRITE PRT.                                                   EL336
01538                                                                   EL336
01539  WPS-EXIT.                                                        EL336
01540      EXIT.                                                        EL336
01541                                                                   EL336
01542      EJECT                                                        EL336
01543  ABEND-PGM SECTION. COPY ELCABEND.                                EL336
01544                                                                   EL336
