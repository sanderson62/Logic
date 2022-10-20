00001  IDENTIFICATION DIVISION.                                         04/21/98
00002                                                                   ECS191
00003  PROGRAM-ID.                 ECS191.                                 LV005
00004 *              PROGRAM CONVERTED BY                               ECS191
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS191
00006 *              CONVERSION DATE 04/21/98 09:20:19.                 ECS191
00007 *               PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE         ECS191
00008 *                            VMOD=2.007.                          ECS191
00009 *                                                                 ECS191
00010 *AUTHOR.        LOGIC INC.                                        ECS191
00011 *               DALLAS, TEXAS.                                    ECS191
00012 *                                                                 ECS191
00013 *DATE-COMPILED.                                                   ECS191
00014 *                                                                 ECS191
00015 *SECURITY.   *****************************************************ECS191
00016 *            *                                                   *ECS191
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC INC.      *ECS191
00018 *            *                                                   *ECS191
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS191
00020 *            *   OF LOGIC INC. IS EXPRESSLY PROHIBITED WITHOUT   *ECS191
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS191
00022 *            *                                                   *ECS191
00023 *            *****************************************************ECS191
00024 *                                                                 ECS191
00025 *REMARKS.                                                         ECS191
00026 *        READS EXTR FROM ECS010 AND CREATES GENERAL LEDGER        ECS191
00027 *        INTERFACE RECORDS FOR CLAIMS (80 BYTE BLOCKED 10).       ECS191
00028                                                                   ECS191
00029  ENVIRONMENT DIVISION.                                            ECS191
00030  INPUT-OUTPUT SECTION.                                            ECS191
00031  FILE-CONTROL.                                                    ECS191
00032                                                                   ECS191
00033      SELECT SORT-FILE        ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  ECS191
00034      SELECT PRINT-FILE       ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS191
00035      SELECT EXTRACTS         ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS191
00036      SELECT ABC-CLAIMS       ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS191
00037      SELECT ELTRLR           ASSIGN TO SYS017-FBA1-ELTRLR         ECS191
00038                              ORGANIZATION IS INDEXED              ECS191
00039                              ACCESS IS DYNAMIC                    ECS191
00040                              RECORD KEY IS AT-CONTROL-PRIMARY     ECS191
00041                              FILE STATUS IS ELTRLR-FILE-STATUS.   ECS191
00042      SELECT ELCNTL           ASSIGN TO SYS018-FBA1-ELCNTL         ECS191
00043                              ORGANIZATION IS INDEXED              ECS191
00044                              ACCESS IS DYNAMIC                    ECS191
00045                              RECORD KEY IS CF-CONTROL-PRIMARY     ECS191
00046                              FILE STATUS IS ELCNTL-FILE-STATUS.   ECS191
00047      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS191
00048      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS191
00049  EJECT                                                            ECS191
00050  DATA DIVISION.                                                   ECS191
00051  FILE SECTION.                                                    ECS191
00052                                                                   ECS191
00053  SD  SORT-FILE                                                    ECS191
00054                      .                                            ECS191
00055  01  SORT-REC.                                                    ECS191
00056      12  FILLER              PIC  X(4).                           ECS191
00057      12  SORT-KEY1           PIC  X(7).                           ECS191
00058      12  SORT-KEY2           PIC  XX.                             ECS191
00059      12  SORT-KEY3           PIC  X(10).                          ECS191
00060      12  FILLER              PIC  X(6).                           ECS191
00061      12  SORT-KEY4           PIC  X(11).                          ECS191
00062      12  FILLER              PIC  X(331).                         ECS191
00063      12  SORT-KEY5           PIC  X(7).                           ECS191
00064      12  FILLER              PIC  X(132).                         ECS191
00065  EJECT                                                            ECS191
00066  FD  PRINT-FILE                                                   ECS191
00067                              COPY ELCPRTFD.                       ECS191
00068  EJECT                                                            ECS191
00069  FD  EXTRACTS                                                     ECS191
00070      RECORDING MODE F                                             ECS191
00071      BLOCK CONTAINS 0 RECORDS
00072                                    .                              ECS191
00073  01  DET-EXTR                PIC  X(510).                         ECS191
00074  EJECT                                                            ECS191
00075  FD  ELCNTL                                                       ECS191
00076                                .                                  ECS191
00077                                                                   ECS191
00078                              COPY ELCCNTL.                        ECS191
00079  EJECT                                                            ECS191
00080  FD  ELTRLR                                                       ECS191
00081                                .                                  ECS191
00082                                                                   ECS191
00083                              COPY ELCTRLR.                        ECS191
00084  EJECT                                                            ECS191
00085  FD  ABC-CLAIMS                                                   ECS191
00086      RECORDING MODE F                                             ECS191
00087      BLOCK CONTAINS 0 RECORDS
00088                                   .                               ECS191
00089  01  ABC-CLAIMS-REC          PIC  X(80).                          ECS191
00090  EJECT                                                            ECS191
00091  FD  DISK-DATE                                                    ECS191
00092                              COPY ELCDTEFD.                       ECS191
00093  EJECT                                                            ECS191
00094  FD  FICH                                                         ECS191
00095                              COPY ELCFCHFD.                       ECS191
00096  EJECT                                                            ECS191
00097  WORKING-STORAGE SECTION.                                         ECS191
00098  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS191
00099  77  FILLER  PIC  X(32) VALUE '********************************'. ECS191
00100  77  FILLER  PIC  X(32) VALUE '     ECS191 WORKING-STORAGE     '. ECS191
00101  77  FILLER  PIC  X(32) VALUE '*********VMOD=2.007*************'. ECS191
00102                                                                   ECS191
00103  77  S1                      PIC S9(4)   COMP    VALUE +00.       ECS191
00104  77  S2                      PIC S9(4)   COMP    VALUE +00.       ECS191
00105  77  LINE-CNT                PIC S9(3)   COMP-3  VALUE +99.       ECS191
00106  77  PAGE-CNT                PIC S9(3)   COMP-3  VALUE +0.        ECS191
00107  77  PGM-SUB                 PIC S9(3)   COMP-3  VALUE +076.      ECS191
00108  77  X                       PIC  X              VALUE SPACE.     ECS191
00109  77  SPACE-NP                PIC  X              VALUE '1'.       ECS191
00110  77  SPACE-1                 PIC  X              VALUE ' '.       ECS191
00111  77  SPACE-2                 PIC  X              VALUE '0'.       ECS191
00112  77  SPACE-3                 PIC  X              VALUE '-'.       ECS191
00113  77  PREV-CARR               PIC  X              VALUE LOW-VALUE. ECS191
00114  77  PREV-COMP               PIC  X(6)           VALUE LOW-VALUE. ECS191
00115  77  PREV-STATE              PIC  XX             VALUE LOW-VALUE. ECS191
00116  77  PREV-ACCT               PIC  X(10)          VALUE LOW-VALUE. ECS191
00117  77  HOLD-STATE              PIC  XX.                             ECS191
00118                                                                   ECS191
00119  01  REQUIRED-STORAGE.                                            ECS191
00120      12  WS-RETURN-CODE          PIC S9(4)              COMP.     ECS191
00121      12  WS-ABEND-MESSAGE        PIC  X(80).                      ECS191
00122      12  WS-ABEND-FILE-STATUS    PIC  X(2)   VALUE ZEROS.         ECS191
00123      12  WS-ZERO                 PIC S9(1)   VALUE ZERO COMP-3.   ECS191
00124      12  3800-DATE-1.                                             ECS191
00125          16  FILLER              PIC  999.                        ECS191
00126          16  3800-CC             PIC  99.                         ECS191
00127          16  3800-YEAR           PIC  99.                         ECS191
00128          16  3800-MONTH          PIC  99.                         ECS191
00129          16  3800-DAY            PIC  99.                         ECS191
00130  EJECT                                                            ECS191
00131 ******************************************************************ECS191
00132 ******************************************************************ECS191
00133 **                                                              **ECS191
00134 **    THIS IS THE FORMAT OF THE INPUT TO THE GENERAL LEDGER     **ECS191
00135 **    SYSTEM.  IT MAY BE NECESSARY TO MODIFY THIS FORMAT TO     **ECS191
00136 **    MEET THE REQUIREMENTS OF YOUR GENERAL LEDGER SYSTEM.      **ECS191
00137 **                                                              **ECS191
00138 ******************************************************************ECS191
00139 ******************************************************************ECS191
00140                                                                   ECS191
00141  01  WS-3800-CARD.                                                ECS191
00142      12  3800-LITERAL            PIC  X(6)       VALUE '0LG00 '.  ECS191
00143      12  3800-DATE               PIC  9(11)  COMP-3.              ECS191
00144      12  3800-VOUCHER-ID         PIC  X(5)       VALUE '*****'.   ECS191
00145      12  3800-JULIAN-DATE COMP-3.                                 ECS191
00146          16  FILLER              PIC  99.                         ECS191
00147          16  3800-JULIAN-CC      PIC  99.                         ECS191
00148          16  3800-JULIAN-YEAR    PIC  99.                         ECS191
00149          16  3800-JULIAN-DAY     PIC  9(3).                       ECS191
00150      12  3800-GENERAL-LEDGER     PIC  X(7).                       ECS191
00151      12  3800-U-LITERAL          PIC  X          VALUE 'U'.       ECS191
00152      12  3800-AMOUNT             PIC  9(7)V99.                    ECS191
00153      12  3800-DEBIT-CREDIT       PIC  X.                          ECS191
00154      12  3800-CHECK-NUMBER       PIC  X(6).                       ECS191
00155      12  3800-DESCRIPTION.                                        ECS191
00156          16  3800-ACCOUNT        PIC  X(10).                      ECS191
00157          16  3800-CERT-NO        PIC  X(11).                      ECS191
00158          16  3800-DESC-L-H       PIC  X.                          ECS191
00159          16  3800-DESC-C-U-P     PIC  X          VALUE 'C'.       ECS191
00160          16  3800-SYSTEM         PIC  X          VALUE 'C'.       ECS191
00161      12  3800-STATE              PIC  XX         VALUE SPACES.    ECS191
00162      12  3800-COMPANY            PIC  X(3).                       ECS191
00163      12  FILLER                  PIC  X          VALUE SPACES.    ECS191
00164      12  3800-MISC-MONTH-YEAR    PIC  X(4)       VALUE '0000'.    ECS191
00165  EJECT                                                            ECS191
00166 ******************************************************************ECS191
00167 ******************************************************************ECS191
00168 **                                                              **ECS191
00169 **    THIS TABLE CONTAINS THE GENERAL LEDGER ACCOUNT NUMBERS    **ECS191
00170 **    FOR THE CASH CLEARING, CLAIMS, AND EXPENSE ACCOUNTS.      **ECS191
00171 **                                                              **ECS191
00172 **    BASED UPON CARRIER AND GROUPING CODE MODIFY THIS TABLE    **ECS191
00173 **    BY ADDING THE ACCOUNT NUMBERS FROM YOUR PARTICULAR        **ECS191
00174 **    CHART OF ACCOUNTS.                                        **ECS191
00175 **                                                              **ECS191
00176 ******************************************************************ECS191
00177 ******************************************************************ECS191
00178                                                                   ECS191
00179  01  GENERAL-LEDGER-TABLE.                                        ECS191
00180      12  FILLER              PIC  X(39)          VALUE            ECS191
00181              'U00123109485010702511070253008005300902'.           ECS191
00182      12  FILLER              PIC  X(39)          VALUE            ECS191
00183              'U00223109485010700511070053008005300902'.           ECS191
00184      12  FILLER              PIC  X(39)          VALUE            ECS191
00185              'U00323109485010740511074053008005300902'.           ECS191
00186      12  FILLER              PIC  X(39)          VALUE            ECS191
00187              'A00123109485010700511080053008005300902'.           ECS191
00188      12  FILLER              PIC  X(39)          VALUE            ECS191
00189              'A00223109485010700511080053008005300902'.           ECS191
00190      12  FILLER              PIC  X(39)          VALUE            ECS191
00191              'A00323109485010700511080053008005300902'.           ECS191
00192      12  FILLER              PIC  X(39)          VALUE            ECS191
00193              'F00110709257040890704589053008005300902'.           ECS191
00194      12  FILLER              PIC  X(39)          VALUE            ECS191
00195              'F00210709257040890704589053008005300902'.           ECS191
00196      12  FILLER              PIC  X(39)          VALUE            ECS191
00197              'F00310709257040890704589053008005300902'.           ECS191
00198                                                                   ECS191
00199  01  LEDGER-NUMBER-TABLE  REDEFINES  GENERAL-LEDGER-TABLE.        ECS191
00200      12  LEDGER-NUMBERS  OCCURS  09  TIMES   INDEXED  BY  L.      ECS191
00201          16  LEDGER-CARR-CO          PIC  X(4).                   ECS191
00202          16  LEDGER-ACCT-NUMBERS     PIC  X(35).                  ECS191
00203                                                                   ECS191
00204  01  GENERAL-LEDGER-NUMBERS.                                      ECS191
00205      12  GL-CASH-CLEAR-NO    PIC  X(7).                           ECS191
00206      12  GL-LF-CLM-NO        PIC  X(7).                           ECS191
00207      12  GL-AH-CLM-NO        PIC  X(7).                           ECS191
00208      12  GL-EXP-1            PIC  X(7).                           ECS191
00209      12  GL-EXP-2            PIC  X(7).                           ECS191
00210  EJECT                                                            ECS191
00211  01  ACCOUNT-TOTALS      COMP-3.                                  ECS191
00212      12  ACCT-CLEAR          PIC S9(9)V99        VALUE ZEROS.     ECS191
00213      12  ACCT-CLEAR-DB       PIC S9(9)V99        VALUE ZEROS.     ECS191
00214      12  ACCT-CLEAR-CR       PIC S9(9)V99        VALUE ZEROS.     ECS191
00215      12  ACCT-LIFE           PIC S9(9)V99        VALUE ZEROS.     ECS191
00216      12  ACCT-LIFE-DB        PIC S9(9)V99        VALUE ZEROS.     ECS191
00217      12  ACCT-LIFE-CR        PIC S9(9)V99        VALUE ZEROS.     ECS191
00218      12  ACCT-AH             PIC S9(9)V99        VALUE ZEROS.     ECS191
00219      12  ACCT-AH-DB          PIC S9(9)V99        VALUE ZEROS.     ECS191
00220      12  ACCT-AH-CR          PIC S9(9)V99        VALUE ZEROS.     ECS191
00221      12  ACCT-EXP-1          PIC S9(9)V99        VALUE ZEROS.     ECS191
00222      12  ACCT-EXP-1-DB       PIC S9(9)V99        VALUE ZEROS.     ECS191
00223      12  ACCT-EXP-1-CR       PIC S9(9)V99        VALUE ZEROS.     ECS191
00224      12  ACCT-EXP-2          PIC S9(9)V99        VALUE ZEROS.     ECS191
00225      12  ACCT-EXP-2-DB       PIC S9(9)V99        VALUE ZEROS.     ECS191
00226      12  ACCT-EXP-2-CR       PIC S9(9)V99        VALUE ZEROS.     ECS191
00227                                                                   ECS191
00228  01  COMPANY-TOTALS      COMP-3.                                  ECS191
00229      12  COMP-CLEAR          PIC S9(9)V99        VALUE ZEROS.     ECS191
00230      12  COMP-CLEAR-DB       PIC S9(9)V99        VALUE ZEROS.     ECS191
00231      12  COMP-CLEAR-CR       PIC S9(9)V99        VALUE ZEROS.     ECS191
00232      12  COMP-LIFE           PIC S9(9)V99        VALUE ZEROS.     ECS191
00233      12  COMP-LIFE-DB        PIC S9(9)V99        VALUE ZEROS.     ECS191
00234      12  COMP-LIFE-CR        PIC S9(9)V99        VALUE ZEROS.     ECS191
00235      12  COMP-AH             PIC S9(9)V99        VALUE ZEROS.     ECS191
00236      12  COMP-AH-DB          PIC S9(9)V99        VALUE ZEROS.     ECS191
00237      12  COMP-AH-CR          PIC S9(9)V99        VALUE ZEROS.     ECS191
00238      12  COMP-EXP-1          PIC S9(9)V99        VALUE ZEROS.     ECS191
00239      12  COMP-EXP-1-DB       PIC S9(9)V99        VALUE ZEROS.     ECS191
00240      12  COMP-EXP-1-CR       PIC S9(9)V99        VALUE ZEROS.     ECS191
00241      12  COMP-EXP-2          PIC S9(9)V99        VALUE ZEROS.     ECS191
00242      12  COMP-EXP-2-DB       PIC S9(9)V99        VALUE ZEROS.     ECS191
00243      12  COMP-EXP-2-CR       PIC S9(9)V99        VALUE ZEROS.     ECS191
00244  EJECT                                                            ECS191
00245  01  CARRIER-TOTALS      COMP-3.                                  ECS191
00246      12  CARR-CLEAR          PIC S9(9)V99        VALUE ZEROS.     ECS191
00247      12  CARR-CLEAR-DB       PIC S9(9)V99        VALUE ZEROS.     ECS191
00248      12  CARR-CLEAR-CR       PIC S9(9)V99        VALUE ZEROS.     ECS191
00249      12  CARR-LIFE           PIC S9(9)V99        VALUE ZEROS.     ECS191
00250      12  CARR-LIFE-DB        PIC S9(9)V99        VALUE ZEROS.     ECS191
00251      12  CARR-LIFE-CR        PIC S9(9)V99        VALUE ZEROS.     ECS191
00252      12  CARR-AH             PIC S9(9)V99        VALUE ZEROS.     ECS191
00253      12  CARR-AH-DB          PIC S9(9)V99        VALUE ZEROS.     ECS191
00254      12  CARR-AH-CR          PIC S9(9)V99        VALUE ZEROS.     ECS191
00255      12  CARR-EXP-1          PIC S9(9)V99        VALUE ZEROS.     ECS191
00256      12  CARR-EXP-1-DB       PIC S9(9)V99        VALUE ZEROS.     ECS191
00257      12  CARR-EXP-1-CR       PIC S9(9)V99        VALUE ZEROS.     ECS191
00258      12  CARR-EXP-2          PIC S9(9)V99        VALUE ZEROS.     ECS191
00259      12  CARR-EXP-2-DB       PIC S9(9)V99        VALUE ZEROS.     ECS191
00260      12  CARR-EXP-2-CR       PIC S9(9)V99        VALUE ZEROS.     ECS191
00261                                                                   ECS191
00262  01  GRAND-TOTALS        COMP-3.                                  ECS191
00263      12  GRND-CLEAR          PIC S9(9)V99        VALUE ZEROS.     ECS191
00264      12  GRND-CLEAR-DB       PIC S9(9)V99        VALUE ZEROS.     ECS191
00265      12  GRND-CLEAR-CR       PIC S9(9)V99        VALUE ZEROS.     ECS191
00266      12  GRND-LIFE           PIC S9(9)V99        VALUE ZEROS.     ECS191
00267      12  GRND-LIFE-DB        PIC S9(9)V99        VALUE ZEROS.     ECS191
00268      12  GRND-LIFE-CR        PIC S9(9)V99        VALUE ZEROS.     ECS191
00269      12  GRND-AH             PIC S9(9)V99        VALUE ZEROS.     ECS191
00270      12  GRND-AH-DB          PIC S9(9)V99        VALUE ZEROS.     ECS191
00271      12  GRND-AH-CR          PIC S9(9)V99        VALUE ZEROS.     ECS191
00272      12  GRND-EXP-1          PIC S9(9)V99        VALUE ZEROS.     ECS191
00273      12  GRND-EXP-1-DB       PIC S9(9)V99        VALUE ZEROS.     ECS191
00274      12  GRND-EXP-1-CR       PIC S9(9)V99        VALUE ZEROS.     ECS191
00275      12  GRND-EXP-2          PIC S9(9)V99        VALUE ZEROS.     ECS191
00276      12  GRND-EXP-2-DB       PIC S9(9)V99        VALUE ZEROS.     ECS191
00277      12  GRND-EXP-2-CR       PIC S9(9)V99        VALUE ZEROS.     ECS191
00278  EJECT                                                            ECS191
00279  01  COMP-ZEROS       COMP-3.                                     ECS191
00280      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00281      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00282      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00283      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00284      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00285      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00286      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00287      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00288      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00289      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00290      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00291      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00292      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00293      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00294      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS191
00295                                                                   ECS191
00296  01  SEARCH-CARR-CO.                                              ECS191
00297      12  SEARCH-CARR         PIC  X.                              ECS191
00298      12  SEARCH-COMP         PIC  X(6).                           ECS191
00299                                                                   ECS191
00300  01  WS-CERT-NO.                                                  ECS191
00301      12  WSL-CERT-NO         PIC  X(11).                          ECS191
00302                                                                   ECS191
00303  01  WS-CAUSE-CD.                                                 ECS191
00304      12  WS-CAUSE-CD-1       PIC  X.                              ECS191
00305      12  WS-CAUSE-CD-2       PIC  X.                              ECS191
00306                                                                   ECS191
00307  01  FILLER.                                                      ECS191
00308      12  ELCNTL-FILE-STATUS.                                      ECS191
00309          16  ELCNTL-FILE-STATUS1     PIC  X.                      ECS191
00310          16  ELCNTL-FILE-STATUS2     PIC  X.                      ECS191
00311      12  ELTRLR-FILE-STATUS.                                      ECS191
00312          16  ELTRLR-FILE-STATUS1     PIC  X.                      ECS191
00313          16  ELTRLR-FILE-STATUS2     PIC  X.                      ECS191
00314                                                                   ECS191
00315  01  WS-CHECK-NO.                                                 ECS191
00316      12  FILLER                  PIC  X.                          ECS191
00317      12  WS-CHECK-6.                                              ECS191
00318          16  WS-CHECK-CON        PIC  X.                          ECS191
00319          16  WS-CHECK-5          PIC  X(5).                       ECS191
00320                                                                   ECS191
00321  01  WS-AT-CONTROL-PRIMARY.                                       ECS191
00322      12  WS-AT-COMPANY-CD        PIC  X.                          ECS191
00323      12  WS-AT-CARRIER           PIC  X.                          ECS191
00324      12  WS-AT-CLAIM-NO          PIC  X(7).                       ECS191
00325      12  WS-AT-CERT-NO.                                           ECS191
00326          16  WS-AT-CERT-PRIME    PIC  X(10).                      ECS191
00327          16  WS-AT-CERT-SFX      PIC  X.                          ECS191
00328      12  WS-AT-SEQUENCE-NO       PIC S9(4) COMP VALUE +1.         ECS191
00329  EJECT                                                            ECS191
00330                              COPY ECSEXT01.                       ECS191
00331  EJECT                                                            ECS191
00332                              COPY ELCEXTVR.                       ECS191
00333  EJECT                                                            ECS191
00334  01  HEAD-LINE-1.                                                 ECS191
00335      12  FILLER              PIC  X(46)          VALUE SPACES.    ECS191
00336      12  FILLER              PIC  X(33)          VALUE            ECS191
00337              'GENERAL LEDGER INTERFACE (CLAIMS)'.                 ECS191
00338      12  FILLER              PIC  X(45)          VALUE SPACES.    ECS191
00339      12  FILLER              PIC  X(7)           VALUE 'ECS-191'. ECS191
00340      12  FILLER              PIC  X              VALUE SPACE.     ECS191
00341                                                                   ECS191
00342  01  HEAD-LINE-2.                                                 ECS191
00343      12  FILLER              PIC  X(47)          VALUE SPACES.    ECS191
00344      12  HD-CLIENT           PIC  X(30).                          ECS191
00345      12  FILLER              PIC  X(47)          VALUE SPACES.    ECS191
00346      12  HD-RUN              PIC  X(8).                           ECS191
00347                                                                   ECS191
00348  01  HEAD-LINE-3.                                                 ECS191
00349      12  FILLER              PIC  X(53)          VALUE SPACES.    ECS191
00350      12  HD-DATE             PIC  X(18).                          ECS191
00351      12  FILLER              PIC  X(41)          VALUE SPACES.    ECS191
00352      12  FILLER              PIC  X(5)           VALUE 'PAGE'.    ECS191
00353      12  HD-PAGE             PIC ZZ,ZZZ.                          ECS191
00354      12  FILLER              PIC  X(9)           VALUE SPACES.    ECS191
00355                                                                   ECS191
00356  01  HEAD-LINE-4.                                                 ECS191
00357      12  FILLER              PIC  X(44)          VALUE            ECS191
00358              '                                         CLA'.      ECS191
00359      12  FILLER              PIC  X(44)          VALUE            ECS191
00360              'IM  CHECK        CASH              LIFE     '.      ECS191
00361      12  FILLER              PIC  X(44)          VALUE            ECS191
00362              '     HEALTH       EXPENSE      EXPENSE      '.      ECS191
00363                                                                   ECS191
00364  01  HEAD-LINE-5.                                                 ECS191
00365      12  FILLER              PIC  X(44)          VALUE            ECS191
00366              '               CAR GROUP  ST   ACCOUNT    NU'.      ECS191
00367      12  FILLER              PIC  X(44)          VALUE            ECS191
00368              'M    NUM       CLEARING           CLAIM     '.      ECS191
00369      12  FILLER              PIC  X(44)          VALUE            ECS191
00370              '      CLAIM       TYPE 01      TYPE 02      '.      ECS191
00371                                                                   ECS191
00372  01  DETAIL-LINE.                                                 ECS191
00373      12  FILLER              PIC  X(16)          VALUE SPACES.    ECS191
00374      12  DET-CARR            PIC  X.                              ECS191
00375      12  FILLER              PIC  XX             VALUE SPACES.    ECS191
00376      12  DET-COMP            PIC  X(6).                           ECS191
00377      12  FILLER              PIC  X              VALUE SPACES.    ECS191
00378      12  DET-ST              PIC  XX.                             ECS191
00379      12  FILLER              PIC  X              VALUE SPACES.    ECS191
00380      12  DET-ACCT            PIC  X(10).                          ECS191
00381      12  DET-DESC.                                                ECS191
00382          16  FILLER          PIC  X              VALUE SPACES.    ECS191
00383          16  DET-CLAIM       PIC  X(7).                           ECS191
00384          16  FILLER          PIC  X              VALUE SPACES.    ECS191
00385          16  DET-CHECK       PIC  X(6).                           ECS191
00386      12  FILLER              PIC  X              VALUE SPACES.    ECS191
00387      12  DET-AMT-CASH-CLEAR  PIC ZZZ,ZZZ,ZZZ.99-.                 ECS191
00388      12  FILLER              PIC  X              VALUE SPACES.    ECS191
00389      12  DET-AMT-LIFE        PIC ZZZ,ZZZ,ZZZ.99-.                 ECS191
00390      12  FILLER              PIC  X              VALUE SPACES.    ECS191
00391      12  DET-AMT-AH          PIC ZZZ,ZZZ,ZZZ.99-.                 ECS191
00392      12  FILLER              PIC  XX             VALUE SPACES.    ECS191
00393      12  DET-AMT-EXP-1       PIC ZZZ,ZZZ.99-.                     ECS191
00394      12  FILLER              PIC  XX             VALUE SPACES.    ECS191
00395      12  DET-AMT-EXP-2       PIC ZZZ,ZZZ.99-.                     ECS191
00396      12  FILLER              PIC  XX             VALUE SPACES.    ECS191
00397      12  DET-FLAG            PIC  X              VALUE SPACES.    ECS191
00398      12  FILLER              PIC  X              VALUE SPACES.    ECS191
00399                                                                   ECS191
00400  01  DETAIL-GL-LINE.                                              ECS191
00401      12  FILLER              PIC  X(40)          VALUE SPACES.    ECS191
00402      12  FILLER              PIC  X(18)          VALUE            ECS191
00403              '**********      **'.                                ECS191
00404      12  DET-CC-GLNO         PIC  X(7).                           ECS191
00405      12  FILLER              PIC  X(9)           VALUE            ECS191
00406              '**     **'.                                         ECS191
00407      12  DET-LC-GLNO         PIC  X(7).                           ECS191
00408      12  FILLER              PIC  X(9)           VALUE            ECS191
00409              '**     **'.                                         ECS191
00410      12  DET-AC-GLNO         PIC  X(7).                           ECS191
00411      12  FILLER              PIC  X(10)          VALUE            ECS191
00412              '**      **'.                                        ECS191
00413      12  DET-E1-GLNO         PIC  X(7).                           ECS191
00414      12  FILLER              PIC  X(7)           VALUE '**   **'. ECS191
00415      12  DET-E2-GLNO         PIC  X(7).                           ECS191
00416      12  FILLER              PIC  X(4)           VALUE '**  '.    ECS191
00417  EJECT                                                            ECS191
00418  01  STATE-TABLE.                                                 ECS191
00419      12  FILLER              PIC  X(4)       VALUE 'AL01'.        ECS191
00420      12  FILLER              PIC  X(4)       VALUE 'AZ02'.        ECS191
00421      12  FILLER              PIC  X(4)       VALUE 'AR03'.        ECS191
00422      12  FILLER              PIC  X(4)       VALUE 'CA04'.        ECS191
00423      12  FILLER              PIC  X(4)       VALUE 'CO05'.        ECS191
00424      12  FILLER              PIC  X(4)       VALUE 'CT06'.        ECS191
00425      12  FILLER              PIC  X(4)       VALUE 'DE07'.        ECS191
00426      12  FILLER              PIC  X(4)       VALUE 'DC08'.        ECS191
00427      12  FILLER              PIC  X(4)       VALUE 'FL09'.        ECS191
00428      12  FILLER              PIC  X(4)       VALUE 'GA10'.        ECS191
00429      12  FILLER              PIC  X(4)       VALUE 'ID11'.        ECS191
00430      12  FILLER              PIC  X(4)       VALUE 'IL12'.        ECS191
00431      12  FILLER              PIC  X(4)       VALUE 'IN13'.        ECS191
00432      12  FILLER              PIC  X(4)       VALUE 'IA14'.        ECS191
00433      12  FILLER              PIC  X(4)       VALUE 'KS15'.        ECS191
00434      12  FILLER              PIC  X(4)       VALUE 'KY16'.        ECS191
00435      12  FILLER              PIC  X(4)       VALUE 'LA17'.        ECS191
00436      12  FILLER              PIC  X(4)       VALUE 'ME18'.        ECS191
00437      12  FILLER              PIC  X(4)       VALUE 'MD19'.        ECS191
00438      12  FILLER              PIC  X(4)       VALUE 'MA20'.        ECS191
00439      12  FILLER              PIC  X(4)       VALUE 'MI21'.        ECS191
00440      12  FILLER              PIC  X(4)       VALUE 'MN22'.        ECS191
00441      12  FILLER              PIC  X(4)       VALUE 'MS23'.        ECS191
00442      12  FILLER              PIC  X(4)       VALUE 'MO24'.        ECS191
00443      12  FILLER              PIC  X(4)       VALUE 'MT25'.        ECS191
00444      12  FILLER              PIC  X(4)       VALUE 'NE26'.        ECS191
00445      12  FILLER              PIC  X(4)       VALUE 'NV27'.        ECS191
00446      12  FILLER              PIC  X(4)       VALUE 'NH28'.        ECS191
00447      12  FILLER              PIC  X(4)       VALUE 'NJ29'.        ECS191
00448      12  FILLER              PIC  X(4)       VALUE 'NM30'.        ECS191
00449      12  FILLER              PIC  X(4)       VALUE 'NY31'.        ECS191
00450      12  FILLER              PIC  X(4)       VALUE 'NC32'.        ECS191
00451      12  FILLER              PIC  X(4)       VALUE 'ND33'.        ECS191
00452      12  FILLER              PIC  X(4)       VALUE 'OH34'.        ECS191
00453      12  FILLER              PIC  X(4)       VALUE 'OK35'.        ECS191
00454      12  FILLER              PIC  X(4)       VALUE 'OR36'.        ECS191
00455      12  FILLER              PIC  X(4)       VALUE 'PA37'.        ECS191
00456      12  FILLER              PIC  X(4)       VALUE 'RI38'.        ECS191
00457      12  FILLER              PIC  X(4)       VALUE 'SC39'.        ECS191
00458      12  FILLER              PIC  X(4)       VALUE 'SD40'.        ECS191
00459      12  FILLER              PIC  X(4)       VALUE 'TN41'.        ECS191
00460      12  FILLER              PIC  X(4)       VALUE 'TX42'.        ECS191
00461      12  FILLER              PIC  X(4)       VALUE 'UT43'.        ECS191
00462      12  FILLER              PIC  X(4)       VALUE 'VT44'.        ECS191
00463      12  FILLER              PIC  X(4)       VALUE 'VA45'.        ECS191
00464      12  FILLER              PIC  X(4)       VALUE 'WA46'.        ECS191
00465      12  FILLER              PIC  X(4)       VALUE 'WV47'.        ECS191
00466      12  FILLER              PIC  X(4)       VALUE 'WI48'.        ECS191
00467      12  FILLER              PIC  X(4)       VALUE 'WY49'.        ECS191
00468      12  FILLER              PIC  X(4)       VALUE 'AK50'.        ECS191
00469      12  FILLER              PIC  X(4)       VALUE 'HI55'.        ECS191
00470      12  FILLER              PIC  X(4)       VALUE HIGH-VALUES.   ECS191
00471                                                                   ECS191
00472  01  STATE-TBL  REDEFINES  STATE-TABLE.                           ECS191
00473      12  STATE-ENTRY     OCCURS  52  TIMES.                       ECS191
00474          16  STATE-CDE       PIC  XX.                             ECS191
00475          16  STATE-ARG       PIC  XX.                             ECS191
00476  EJECT                                                            ECS191
00477                              COPY ELCDATE.                        ECS191
00478                                                                   ECS191
00479                              COPY ELCDTECX.                       ECS191
00480                                                                   ECS191
00481                              COPY ELCDTEVR.                       ECS191
00482  EJECT                                                            ECS191
00483  PROCEDURE DIVISION.                                              ECS191
00484                                                                   ECS191
00485  0000-INITIALIZE SECTION.                                         ECS191
00486                                                                   ECS191
00487  0000-GET-DATE.                                                   ECS191
00488                              COPY ELCDTERX.                       ECS191
00489  EJECT                                                            ECS191
00490      MOVE ALPH-DATE              TO  HD-DATE.                     ECS191
00491      MOVE WS-CURRENT-DATE        TO  HD-RUN.                      ECS191
00492      MOVE COMPANY-NAME           TO  HD-CLIENT.                   ECS191
00493      MOVE RUN-DATE               TO  3800-DATE                    ECS191
00494                                      3800-DATE-1.                 ECS191
00495      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               ECS191
00496      MOVE ' '                    TO  DC-OPTION-CODE.              ECS191
00497                                                                   ECS191
00498      CALL 'ELDATCX'  USING  DATE-CONVERSION-DATA.                 ECS191
00499                                                                   ECS191
00500      MOVE DC-JULIAN-DATE         TO  3800-JULIAN-DATE.            ECS191
00501      MOVE DC-ALPHA-CEN-N         TO  3800-JULIAN-CC.              ECS191
00502      MOVE DTE-CLIENT             TO  3800-COMPANY.                ECS191
00503                                                                   ECS191
00504      IF DTE-CLIENT = 'CIM'                                        ECS191
00505          MOVE 'WCO'              TO  3800-COMPANY.                ECS191
00506                                                                   ECS191
00507      IF DTE-CLIENT = 'WFL'                                        ECS191
00508          MOVE 'FLA'              TO  3800-COMPANY.                ECS191
00509                                                                   ECS191
00510      IF DTE-CLIENT = 'UFL' OR 'UFR'                               ECS191
00511          MOVE 'UFL'              TO  3800-COMPANY.                ECS191
00512                                                                   ECS191
00513      OPEN INPUT ELCNTL.                                           ECS191
00514                                                                   ECS191
00515      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        ECS191
00516          NEXT SENTENCE                                            ECS191
00517        ELSE                                                       ECS191
00518          DISPLAY 'ERROR OCCURED OPEN - ELCNTL'                    ECS191
00519          DISPLAY 'ERROR CODE RETURNED = '  ELCNTL-FILE-STATUS     ECS191
00520          DISPLAY 'CNTRL ST ERR= '  ELCNTL-FILE-STATUS             ECS191
00521              UPON CONSOLE                                         ECS191
00522          MOVE '0302'             TO  WS-RETURN-CODE               ECS191
00523          MOVE 'ERROR OCCURED OPEN - ELCNTL'                       ECS191
00524                                  TO  WS-ABEND-MESSAGE             ECS191
00525          GO TO ABEND-PGM.                                         ECS191
00526                                                                   ECS191
00527      MOVE SPACES                 TO  CF-CONTROL-PRIMARY.          ECS191
00528      MOVE DTE-CLIENT             TO  CF-COMPANY-ID.               ECS191
00529      MOVE '0'                    TO  CF-RECORD-TYPE.              ECS191
00530                                                                   ECS191
00531      IF DTE-CLIENT = 'LOK'                                        ECS191
00532          GO TO 0100-SORT-ROUTINE.                                 ECS191
00533                                                                   ECS191
00534  0001-READ-COMPANY-CONTROL.                                       ECS191
00535      START ELCNTL  KEY NOT LESS CF-CONTROL-PRIMARY                ECS191
00536                                                                   ECS191
00537      IF ELCNTL-FILE-STATUS NOT = ZERO                             ECS191
00538          DISPLAY 'ERROR OCCURED START - ELCNTL'                   ECS191
00539          DISPLAY 'ERROR CODE RETURNED = '  ELCNTL-FILE-STATUS     ECS191
00540          DISPLAY 'CNTRL OP ERR= '  ELCNTL-FILE-STATUS             ECS191
00541              UPON CONSOLE                                         ECS191
00542          MOVE '0302'             TO  WS-RETURN-CODE               ECS191
00543          MOVE 'ERROR OCCURED START - ELCNTL'                      ECS191
00544                                  TO  WS-ABEND-MESSAGE             ECS191
00545          GO TO ABEND-PGM.                                         ECS191
00546                                                                   ECS191
00547  0002-READ-NEXT-CONTROL.                                          ECS191
00548      READ ELCNTL  NEXT RECORD.                                    ECS191
00549                                                                   ECS191
00550      IF (NOT CF-COMPANY-MASTER)                                   ECS191
00551        OR  (DTE-CLIENT NOT = CF-COMPANY-ID)                       ECS191
00552          DISPLAY 'ONLINE COMPANY MASTER RECORD NOT FOUND'         ECS191
00553          DISPLAY '**************************************'         ECS191
00554          MOVE '0302'             TO  WS-RETURN-CODE               ECS191
00555          MOVE 'ONLINE COMPANY MASTER RECORD NOT FOUND'            ECS191
00556                                  TO  WS-ABEND-MESSAGE             ECS191
00557          GO TO ABEND-PGM.                                         ECS191
00558                                                                   ECS191
00559  0100-SORT-ROUTINE.                                               ECS191
00560      SORT SORT-FILE  ON ASCENDING KEY  SORT-KEY1                  ECS191
00561                                        SORT-KEY3                  ECS191
00562                                        SORT-KEY5                  ECS191
00563          INPUT PROCEDURE  SORT-CLAIM-TRANSACTIONS                 ECS191
00564          OUTPUT PROCEDURE CREATE-GEN-LEDGER-TRANSACTIONS.         ECS191
00565                                                                   ECS191
00566      IF SORT-RETURN NOT = ZEROS                                   ECS191
00567          MOVE '0101'             TO  WS-RETURN-CODE               ECS191
00568          MOVE 'BAD RETURN CODE FROM INTERNAL SORT'                ECS191
00569                                  TO  WS-ABEND-MESSAGE             ECS191
00570          GO TO ABEND-PGM.                                         ECS191
00571                                                                   ECS191
00572  0199-END-OF-JOB.                                                 ECS191
00573                              COPY ELCPRTC.                        ECS191
00574                                                                   ECS191
00575      CLOSE ABC-CLAIMS                                             ECS191
00576            PRINT-FILE                                             ECS191
00577            ELCNTL                                                 ECS191
00578            ELTRLR.                                                ECS191
00579                                                                   ECS191
00580      GOBACK.                                                      ECS191
00581  EJECT                                                            ECS191
00582  SORT-CLAIM-TRANSACTIONS SECTION.                                 ECS191
00583                                                                   ECS191
00584  0200-INPUT-ROUTINE.                                              ECS191
00585      OPEN INPUT EXTRACTS                                          ECS191
00586                 ELTRLR.                                           ECS191
00587                                                                   ECS191
00588      IF ELTRLR-FILE-STATUS  = '00' OR '97'                        ECS191
00589          NEXT SENTENCE                                            ECS191
00590        ELSE                                                       ECS191
00591          DISPLAY 'ERROR OCCURED OPEN  - ELTRLR'                   ECS191
00592          DISPLAY 'ERROR CODE RETURNED = '  ELTRLR-FILE-STATUS     ECS191
00593          MOVE '0302'             TO  WS-RETURN-CODE               ECS191
00594          MOVE 'ERROR OCCURED OPEN - ELTRLR'                       ECS191
00595                                  TO  WS-ABEND-MESSAGE             ECS191
00596          GO TO ABEND-PGM.                                         ECS191
00597                                                                   ECS191
00598  0210-READ-EXTRACT.                                               ECS191
00599      READ EXTRACTS  INTO  DETAIL-EXTRACT  AT END                  ECS191
00600          GO TO 0290-END-INPUT.                                    ECS191
00601                                                                   ECS191
00602      IF NOT VALID-DE-ID                                           ECS191
00603          GO TO 0210-READ-EXTRACT.                                 ECS191
00604                                                                   ECS191
00605      IF DE-REIN = 'R'                                             ECS191
00606          GO TO 0210-READ-EXTRACT.                                 ECS191
00607                                                                   ECS191
00608      IF NOT DE-CLAIM                                              ECS191
00609          GO TO 0210-READ-EXTRACT.                                 ECS191
00610                                                                   ECS191
00611      COPY ELCEXTM1.                                               ECS191
00612                                                                   ECS191
00613      IF DTE-CLIENT = 'ADL' OR  'LOK'  OR  'WFL'                   ECS191
00614                            OR  'FLB'  OR  'ALA'                   ECS191
00615          MOVE DE-STATE           TO  HOLD-STATE                   ECS191
00616          GO TO 0230-CHECK-CLAIM-TYPE.                             ECS191
00617                                                                   ECS191
00618      IF DE-STATE = PREV-STATE                                     ECS191
00619          GO TO 0230-CHECK-CLAIM-TYPE.                             ECS191
00620                                                                   ECS191
00621      MOVE +0                     TO  S1.                          ECS191
00622      MOVE DE-STATE               TO  PREV-STATE.                  ECS191
00623                                                                   ECS191
00624  0220-LOOP.                                                       ECS191
00625      ADD +1                      TO  S1.                          ECS191
00626                                                                   ECS191
00627      IF STATE-ARG (S1) = HIGH-VALUES                              ECS191
00628          MOVE 'XX'               TO  HOLD-STATE                   ECS191
00629          GO TO 0230-CHECK-CLAIM-TYPE.                             ECS191
00630                                                                   ECS191
00631      IF DE-STATE = STATE-ARG (S1)                                 ECS191
00632          MOVE STATE-CDE (S1)     TO  HOLD-STATE                   ECS191
00633          GO TO 0230-CHECK-CLAIM-TYPE.                             ECS191
00634                                                                   ECS191
00635      GO TO 0220-LOOP.                                             ECS191
00636                                                                   ECS191
00637  0230-CHECK-CLAIM-TYPE.                                           ECS191
00638      MOVE HOLD-STATE             TO  DE-STATE.                    ECS191
00639                                                                   ECS191
00640      IF DE-OB-DTH                                                 ECS191
00641          MOVE '1'                TO  DE-TYPE.                     ECS191
00642                                                                   ECS191
00643      IF DE-OB-AH                                                  ECS191
00644          MOVE '2'                TO  DE-TYPE.                     ECS191
00645                                                                   ECS191
00646 *** ??????????????????????                                        ECS191
00647      MOVE DE-CARRIER             TO  DE-RECORD-ID.                ECS191
00648                                                                   ECS191
00649      IF DTE-CLIENT = 'ADL' OR  'LOK' OR 'FLB' OR 'ALA'            ECS191
00650          GO TO 0250-RELEASE-EXTRACT.                              ECS191
00651                                                                   ECS191
00652      IF DTE-CLIENT = 'CIM'                                        ECS191
00653          MOVE 'W'                TO  DE-CARRIER.                  ECS191
00654                                                                   ECS191
00655      IF DTE-CLIENT = 'WFL'                                        ECS191
00656          MOVE 'F'                TO  DE-CARRIER.                  ECS191
00657                                                                   ECS191
00658      IF DTE-CLIENT = 'UFL'                                        ECS191
00659          MOVE 'U'                TO  DE-CARRIER.                  ECS191
00660                                                                   ECS191
00661      IF DTE-CLIENT = 'UFR'                                        ECS191
00662          MOVE 'R'                TO  DE-CARRIER.                  ECS191
00663                                                                   ECS191
00664      MOVE '000001'               TO  DE-GROUPING.                 ECS191
00665                                                                   ECS191
00666      IF DE-ACC-GPCD = '02'              OR  '05'                  ECS191
00667          MOVE '000002'           TO  DE-GROUPING                  ECS191
00668      ELSE                                                         ECS191
00669          IF DE-ACC-GPCD = '04'                                    ECS191
00670              MOVE '000003'       TO  DE-GROUPING.                 ECS191
00671                                                                   ECS191
00672      IF DTE-CLIENT = 'CIM'                                        ECS191
00673          MOVE '000002'           TO  DE-GROUPING                  ECS191
00674          IF DE-ACC-GPCD = '01' OR  '09'                           ECS191
00675                                OR  '15'  OR  '18'                 ECS191
00676              MOVE '000001'       TO  DE-GROUPING.                 ECS191
00677                                                                   ECS191
00678  0250-RELEASE-EXTRACT.                                            ECS191
00679      COPY ELCEXTM2.                                               ECS191
00680      RELEASE SORT-REC  FROM  DETAIL-EXTRACT.                      ECS191
00681                                                                   ECS191
00682      GO TO 0210-READ-EXTRACT.                                     ECS191
00683                                                                   ECS191
00684  0290-END-INPUT.                                                  ECS191
00685      CLOSE EXTRACTS.                                              ECS191
00686                                                                   ECS191
00687  0299-XIT.                                                        ECS191
00688      EXIT.                                                        ECS191
00689  EJECT                                                            ECS191
00690  CREATE-GEN-LEDGER-TRANSACTIONS SECTION.                          ECS191
00691                                                                   ECS191
00692  1000-OUTPUT-ROUTINE.                                             ECS191
00693      OPEN OUTPUT ABC-CLAIMS  PRINT-FILE.                          ECS191
00694                                                                   ECS191
00695      IF DTE-CLIENT = 'LOK'                                        ECS191
00696          MOVE SPACES             TO  ACTIVITY-TRAILERS.           ECS191
00697                                                                   ECS191
00698      PERFORM 1100-RETURN-SORTED-RECS.                             ECS191
00699                                                                   ECS191
00700      MOVE DE-CARRIER             TO  PREV-CARR.                   ECS191
00701      MOVE DE-GROUPING            TO  PREV-COMP.                   ECS191
00702      MOVE DE-STATE               TO  PREV-STATE.                  ECS191
00703      MOVE DE-ACCOUNT             TO  PREV-ACCT.                   ECS191
00704                                                                   ECS191
00705      PERFORM 2500-HEADING-ROUTINE  THRU  2599-XIT.                ECS191
00706                                                                   ECS191
00707      GO TO 1250-READ-PAYMENT.                                     ECS191
00708                                                                   ECS191
00709  1100-RETURN-SORTED-RECS.                                         ECS191
00710      RETURN SORT-FILE  INTO  DETAIL-EXTRACT  AT END               ECS191
00711          GO TO 2100-ACCT-BREAK.                                   ECS191
00712                                                                   ECS191
00713      COPY ELCEXTM1.                                               ECS191
00714                                                                   ECS191
00715  1200-CHECK-BREAKS.                                               ECS191
00716      IF DE-CARRIER NOT = PREV-CARR                                ECS191
00717          PERFORM 2100-ACCT-BREAK  THRU  2199-XIT                  ECS191
00718          PERFORM 2200-COMP-BREAK  THRU  2299-XIT                  ECS191
00719          PERFORM 2300-CARR-BREAK  THRU  2399-XIT                  ECS191
00720          PERFORM 2500-HEADING-ROUTINE  THRU  2599-XIT.            ECS191
00721                                                                   ECS191
00722      IF DE-GROUPING NOT = PREV-COMP                               ECS191
00723          PERFORM 2100-ACCT-BREAK  THRU  2199-XIT                  ECS191
00724          PERFORM 2200-COMP-BREAK  THRU  2299-XIT                  ECS191
00725          PERFORM 2500-HEADING-ROUTINE  THRU  2599-XIT.            ECS191
00726                                                                   ECS191
00727      IF DE-ACCOUNT NOT = PREV-ACCT                                ECS191
00728          PERFORM 2100-ACCT-BREAK  THRU  2199-XIT.                 ECS191
00729                                                                   ECS191
00730  1250-READ-PAYMENT.                                               ECS191
00731      MOVE SPACES                 TO  DETAIL-LINE.                 ECS191
00732      MOVE LOW-VALUE              TO  WS-AT-CONTROL-PRIMARY.       ECS191
00733      MOVE CF-COMPANY-CD          TO  WS-AT-COMPANY-CD.            ECS191
00734      MOVE DE-CNUM                TO  WS-AT-CLAIM-NO.              ECS191
00735      MOVE DE-CERT                TO  WS-AT-CERT-NO.               ECS191
00736 *** ??????????????????????                                        ECS191
00737      MOVE DE-RECORD-ID           TO  WS-AT-CARRIER                ECS191
00738      MOVE ZERO                   TO  WS-AT-SEQUENCE-NO.           ECS191
00739      MOVE WS-AT-CONTROL-PRIMARY  TO  AT-CONTROL-PRIMARY.          ECS191
00740                                                                   ECS191
00741      IF DTE-CLIENT = 'LOK'                                        ECS191
00742          GO TO 1258-NOT-FOUND.                                    ECS191
00743                                                                   ECS191
00744  1252-START.                                                      ECS191
00745      START ELTRLR  KEY NOT LESS AT-CONTROL-PRIMARY.               ECS191
00746                                                                   ECS191
00747      IF ELTRLR-FILE-STATUS = '23' OR  '10'                        ECS191
00748           GO TO 1258-NOT-FOUND.                                   ECS191
00749                                                                   ECS191
00750      IF ELTRLR-FILE-STATUS NOT = ZERO                             ECS191
00751          DISPLAY 'ERROR OCCURED START- ELTRLR'                    ECS191
00752          DISPLAY 'ERROR CODE RETURNED = '  ELTRLR-FILE-STATUS     ECS191
00753          MOVE '0302'             TO  WS-RETURN-CODE               ECS191
00754          MOVE 'ERROR OCCURED START - ELTRLR'                      ECS191
00755                                  TO  WS-ABEND-MESSAGE             ECS191
00756          GO TO ABEND-PGM.                                         ECS191
00757                                                                   ECS191
00758  1254-READ-NEXT.                                                  ECS191
00759      READ ELTRLR  NEXT RECORD.                                    ECS191
00760                                                                   ECS191
00761      IF ELTRLR-FILE-STATUS1 = '1'                                 ECS191
00762           GO TO 1258-NOT-FOUND.                                   ECS191
00763                                                                   ECS191
00764      IF ELTRLR-FILE-STATUS1 NOT = ZERO                            ECS191
00765          DISPLAY 'ERROR OCCURED READ - ELTRLR'                    ECS191
00766          DISPLAY 'ERROR CODE RETURNED = '  ELTRLR-FILE-STATUS     ECS191
00767          MOVE '0302'             TO  WS-RETURN-CODE               ECS191
00768          MOVE 'ERROR OCCURED READ - ELTRLR'                       ECS191
00769                                  TO  WS-ABEND-MESSAGE             ECS191
00770          GO TO ABEND-PGM.                                         ECS191
00771                                                                   ECS191
00772      IF CF-COMPANY-CD NOT = AT-COMPANY-CD                         ECS191
00773          GO TO 1258-NOT-FOUND.                                    ECS191
00774                                                                   ECS191
00775      IF NOT PAYMENT-TR                                            ECS191
00776          GO TO 1254-READ-NEXT.                                    ECS191
00777                                                                   ECS191
00778      IF WS-AT-CERT-NO NOT = AT-CERT-NO                            ECS191
00779          GO TO 1258-NOT-FOUND.                                    ECS191
00780                                                                   ECS191
00781      IF WS-AT-CLAIM-NO NOT = AT-CLAIM-NO                          ECS191
00782          GO TO 1258-NOT-FOUND.                                    ECS191
00783                                                                   ECS191
00784      MOVE DE-PTO-CC              TO  DC-ALPHA-CEN-N.              ECS191
00785      MOVE DE-PTO-YR              TO  DC-YMD-YEAR.                 ECS191
00786      MOVE DE-PTO-MO              TO  DC-YMD-MONTH.                ECS191
00787      MOVE DE-PTO-DA              TO  DC-YMD-DAY.                  ECS191
00788      MOVE '3'                    TO  DC-OPTION-CODE.              ECS191
00789                                                                   ECS191
00790      CALL 'ELDATCX'  USING  DATE-CONVERSION-DATA.                 ECS191
00791                                                                   ECS191
00792      MOVE AT-CHECK-NO            TO  WS-CHECK-NO.                 ECS191
00793                                                                   ECS191
00794      IF ((DE-CLAIM-AMT = AT-AMOUNT-PAID)                          ECS191
00795        OR  (DE-CLAIM-AMT + AT-AMOUNT-PAID = ZERO))                ECS191
00796          AND  ((DE-CHECK = WS-CHECK-5)                            ECS191
00797            OR  (DC-BIN-DATE-1 = AT-PAID-THRU-DT))                 ECS191
00798              NEXT SENTENCE                                        ECS191
00799      ELSE                                                         ECS191
00800          GO TO 1254-READ-NEXT.                                    ECS191
00801                                                                   ECS191
00802      MOVE WS-CHECK-6             TO  3800-CHECK-NUMBER            ECS191
00803                                      DET-CHECK.                   ECS191
00804                                                                   ECS191
00805      GO TO 1300-ACCUMULATE-DETAIL.                                ECS191
00806                                                                   ECS191
00807  1258-NOT-FOUND.                                                  ECS191
00808      MOVE DE-CHECK               TO  WS-CHECK-5.                  ECS191
00809                                                                   ECS191
00810      IF DTE-CLIENT = 'LOK'                                        ECS191
00811          MOVE '0'                TO  WS-CHECK-CON                 ECS191
00812      ELSE                                                         ECS191
00813          MOVE '1'                TO  WS-CHECK-CON                 ECS191
00814          MOVE '*'                TO  DET-FLAG.                    ECS191
00815                                                                   ECS191
00816      MOVE WS-CHECK-6             TO  3800-CHECK-NUMBER            ECS191
00817                                      DET-CHECK.                   ECS191
00818                                                                   ECS191
00819  1300-ACCUMULATE-DETAIL.                                          ECS191
00820      MOVE DE-CARRIER             TO  DET-CARR  SEARCH-CARR.       ECS191
00821      MOVE DE-GROUPING            TO  DET-COMP  SEARCH-COMP.       ECS191
00822                                                                   ECS191
00823      IF DTE-CLIENT = 'ADL' OR  'FLB'  OR  'ALA'                   ECS191
00824          MOVE 'A'                TO  SEARCH-CARR.                 ECS191
00825                                                                   ECS191
00826      PERFORM 3000-LEDGER-TABLE-LOOKUP  THRU  3099-XIT.            ECS191
00827                                                                   ECS191
00828      MOVE DE-STATE               TO  3800-STATE  DET-ST.          ECS191
00829      MOVE DE-ACCOUNT             TO  3800-ACCOUNT  DET-ACCT.      ECS191
00830      MOVE DE-CNUM                TO  DET-CLAIM.                   ECS191
00831      MOVE DE-CERT-NO             TO  WS-CERT-NO.                  ECS191
00832      MOVE WSL-CERT-NO            TO  3800-CERT-NO.                ECS191
00833                                                                   ECS191
00834      COMPUTE ACCT-CLEAR = ACCT-CLEAR + DE-CLAIM-AMT.              ECS191
00835                                                                   ECS191
00836      IF AT-EXPENSE-TYPE = '1'                                     ECS191
00837          COMPUTE ACCT-EXP-1 = ACCT-EXP-1 + DE-CLAIM-AMT           ECS191
00838          GO TO 1550-GL-EXPENSE-1.                                 ECS191
00839                                                                   ECS191
00840      IF AT-EXPENSE-TYPE = '2'                                     ECS191
00841          COMPUTE ACCT-EXP-2 = ACCT-EXP-2 + DE-CLAIM-AMT           ECS191
00842          GO TO 1575-GL-EXPENSE-2.                                 ECS191
00843                                                                   ECS191
00844      IF DE-TYPE = '1'                                             ECS191
00845          COMPUTE ACCT-LIFE = ACCT-LIFE + DE-CLAIM-AMT             ECS191
00846          GO TO 1400-GL-LIFE                                       ECS191
00847      ELSE                                                         ECS191
00848          COMPUTE ACCT-AH = ACCT-AH + DE-CLAIM-AMT                 ECS191
00849          GO TO 1500-GL-AH.                                        ECS191
00850                                                                   ECS191
00851  1400-GL-LIFE.                                                    ECS191
00852      MOVE GL-CASH-CLEAR-NO       TO  3800-GENERAL-LEDGER.         ECS191
00853      MOVE 'L'                    TO  3800-DESC-L-H.               ECS191
00854      MOVE DE-CLAIM-AMT           TO  3800-AMOUNT                  ECS191
00855                                      DET-AMT-CASH-CLEAR.          ECS191
00856                                                                   ECS191
00857      IF DE-CLAIM-AMT NOT LESS ZERO                                ECS191
00858          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS191
00859          ADD 3800-AMOUNT         TO  ACCT-CLEAR-CR                ECS191
00860      ELSE                                                         ECS191
00861          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS191
00862          ADD 3800-AMOUNT         TO  ACCT-CLEAR-DB.               ECS191
00863                                                                   ECS191
00864      IF DE-CLAIM-AMT NOT = ZERO                                   ECS191
00865          PERFORM 1700-WRITE-GL-CARD  THRU  1799-XIT.              ECS191
00866                                                                   ECS191
00867      MOVE GL-LF-CLM-NO           TO  3800-GENERAL-LEDGER.         ECS191
00868      MOVE DE-CLAIM-AMT           TO  3800-AMOUNT  DET-AMT-LIFE.   ECS191
00869                                                                   ECS191
00870      IF DE-CLAIM-AMT NOT LESS ZERO                                ECS191
00871          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS191
00872          ADD 3800-AMOUNT         TO  ACCT-LIFE-DB                 ECS191
00873      ELSE                                                         ECS191
00874          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS191
00875          ADD 3800-AMOUNT TO ACCT-LIFE-CR.                         ECS191
00876                                                                   ECS191
00877      IF DE-CLAIM-AMT NOT = ZERO                                   ECS191
00878          PERFORM 1700-WRITE-GL-CARD  THRU  1799-XIT.              ECS191
00879                                                                   ECS191
00880      GO TO 1600-PRINT-DETAIL.                                     ECS191
00881                                                                   ECS191
00882  1500-GL-AH.                                                      ECS191
00883      MOVE GL-CASH-CLEAR-NO       TO  3800-GENERAL-LEDGER.         ECS191
00884      MOVE 'H'                    TO  3800-DESC-L-H.               ECS191
00885      MOVE DE-CLAIM-AMT           TO  3800-AMOUNT                  ECS191
00886                                      DET-AMT-CASH-CLEAR.          ECS191
00887      IF DE-CLAIM-AMT NOT LESS ZERO                                ECS191
00888          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS191
00889          ADD 3800-AMOUNT         TO  ACCT-CLEAR-CR                ECS191
00890      ELSE                                                         ECS191
00891          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS191
00892          ADD 3800-AMOUNT         TO  ACCT-CLEAR-DB.               ECS191
00893                                                                   ECS191
00894      IF DE-CLAIM-AMT NOT = ZERO                                   ECS191
00895          PERFORM 1700-WRITE-GL-CARD  THRU  1799-XIT.              ECS191
00896                                                                   ECS191
00897      MOVE GL-AH-CLM-NO           TO  3800-GENERAL-LEDGER.         ECS191
00898      MOVE DE-CLAIM-AMT           TO  3800-AMOUNT  DET-AMT-AH.     ECS191
00899                                                                   ECS191
00900      IF DE-CLAIM-AMT NOT LESS ZERO                                ECS191
00901          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS191
00902          ADD 3800-AMOUNT         TO  ACCT-AH-DB                   ECS191
00903      ELSE                                                         ECS191
00904          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS191
00905          ADD 3800-AMOUNT         TO  ACCT-AH-CR.                  ECS191
00906                                                                   ECS191
00907      IF DE-CLAIM-AMT NOT = ZERO                                   ECS191
00908          PERFORM 1700-WRITE-GL-CARD  THRU  1799-XIT.              ECS191
00909                                                                   ECS191
00910      GO TO 1600-PRINT-DETAIL.                                     ECS191
00911                                                                   ECS191
00912  1550-GL-EXPENSE-1.                                               ECS191
00913      MOVE GL-CASH-CLEAR-NO       TO  3800-GENERAL-LEDGER.         ECS191
00914      MOVE 'E'                    TO  3800-DESC-L-H.               ECS191
00915      MOVE DE-CLAIM-AMT           TO  3800-AMOUNT                  ECS191
00916                                      DET-AMT-CASH-CLEAR.          ECS191
00917                                                                   ECS191
00918      IF DE-CLAIM-AMT NOT LESS ZERO                                ECS191
00919          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS191
00920          ADD 3800-AMOUNT         TO  ACCT-CLEAR-CR                ECS191
00921      ELSE                                                         ECS191
00922          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS191
00923          ADD 3800-AMOUNT         TO  ACCT-CLEAR-DB.               ECS191
00924                                                                   ECS191
00925      IF DE-CLAIM-AMT NOT = ZERO                                   ECS191
00926          PERFORM 1700-WRITE-GL-CARD  THRU  1799-XIT.              ECS191
00927                                                                   ECS191
00928      MOVE GL-EXP-1               TO  3800-GENERAL-LEDGER.         ECS191
00929      MOVE DE-CLAIM-AMT           TO  3800-AMOUNT  DET-AMT-EXP-1.  ECS191
00930                                                                   ECS191
00931      IF DE-CLAIM-AMT NOT LESS ZERO                                ECS191
00932          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS191
00933          ADD 3800-AMOUNT         TO  ACCT-EXP-1-DB                ECS191
00934      ELSE                                                         ECS191
00935          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS191
00936          ADD 3800-AMOUNT         TO  ACCT-EXP-1-CR.               ECS191
00937                                                                   ECS191
00938      IF DE-CLAIM-AMT NOT = ZERO                                   ECS191
00939          PERFORM 1700-WRITE-GL-CARD  THRU  1799-XIT.              ECS191
00940                                                                   ECS191
00941      GO TO 1600-PRINT-DETAIL.                                     ECS191
00942                                                                   ECS191
00943  1575-GL-EXPENSE-2.                                               ECS191
00944      MOVE GL-CASH-CLEAR-NO       TO  3800-GENERAL-LEDGER.         ECS191
00945      MOVE 'E'                    TO  3800-DESC-L-H.               ECS191
00946      MOVE DE-CLAIM-AMT           TO  3800-AMOUNT                  ECS191
00947                                      DET-AMT-CASH-CLEAR.          ECS191
00948                                                                   ECS191
00949      IF DE-CLAIM-AMT NOT LESS ZERO                                ECS191
00950          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS191
00951          ADD 3800-AMOUNT         TO  ACCT-CLEAR-CR                ECS191
00952      ELSE                                                         ECS191
00953          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS191
00954          ADD 3800-AMOUNT         TO  ACCT-CLEAR-DB.               ECS191
00955                                                                   ECS191
00956      IF DE-CLAIM-AMT NOT = ZERO                                   ECS191
00957          PERFORM 1700-WRITE-GL-CARD  THRU  1799-XIT.              ECS191
00958                                                                   ECS191
00959      MOVE GL-EXP-2               TO  3800-GENERAL-LEDGER.         ECS191
00960      MOVE DE-CLAIM-AMT           TO  3800-AMOUNT  DET-AMT-EXP-2.  ECS191
00961                                                                   ECS191
00962      IF DE-CLAIM-AMT NOT LESS ZERO                                ECS191
00963          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS191
00964          ADD 3800-AMOUNT         TO  ACCT-EXP-2-DB                ECS191
00965      ELSE                                                         ECS191
00966          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS191
00967          ADD 3800-AMOUNT         TO  ACCT-EXP-2-CR.               ECS191
00968                                                                   ECS191
00969      IF DE-CLAIM-AMT NOT = ZERO                                   ECS191
00970          PERFORM 1700-WRITE-GL-CARD  THRU  1799-XIT.              ECS191
00971                                                                   ECS191
00972  1600-PRINT-DETAIL.                                               ECS191
00973      MOVE SPACE-1                TO  X.                           ECS191
00974      MOVE DETAIL-LINE            TO  P-DATA.                      ECS191
00975                                                                   ECS191
00976      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
00977                                                                   ECS191
00978      GO TO 1100-RETURN-SORTED-RECS.                               ECS191
00979                                                                   ECS191
00980  1700-WRITE-GL-CARD.                                              ECS191
00981      WRITE ABC-CLAIMS-REC  FROM  WS-3800-CARD.                    ECS191
00982                                                                   ECS191
00983  1799-XIT.                                                        ECS191
00984      EXIT.                                                        ECS191
00985  EJECT                                                            ECS191
00986  2100-ACCT-BREAK.                                                 ECS191
00987      MOVE SPACES                 TO  DETAIL-LINE.                 ECS191
00988      MOVE GL-CASH-CLEAR-NO       TO  DET-CC-GLNO.                 ECS191
00989      MOVE GL-LF-CLM-NO           TO  DET-LC-GLNO.                 ECS191
00990      MOVE GL-AH-CLM-NO           TO  DET-AC-GLNO.                 ECS191
00991      MOVE GL-EXP-1               TO  DET-E1-GLNO.                 ECS191
00992      MOVE GL-EXP-2               TO  DET-E2-GLNO.                 ECS191
00993      MOVE SPACE-2                TO  X.                           ECS191
00994      MOVE DETAIL-GL-LINE         TO  P-DATA.                      ECS191
00995                                                                   ECS191
00996      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
00997                                                                   ECS191
00998      MOVE PREV-ACCT              TO  DET-ACCT.                    ECS191
00999      MOVE ACCT-CLEAR             TO  DET-AMT-CASH-CLEAR.          ECS191
01000      MOVE ACCT-LIFE              TO  DET-AMT-LIFE.                ECS191
01001      MOVE ACCT-AH                TO  DET-AMT-AH.                  ECS191
01002      MOVE ACCT-EXP-1             TO  DET-AMT-EXP-1.               ECS191
01003      MOVE ACCT-EXP-2             TO  DET-AMT-EXP-2.               ECS191
01004      MOVE SPACE-1                TO  X.                           ECS191
01005      MOVE DETAIL-LINE            TO  P-DATA.                      ECS191
01006                                                                   ECS191
01007      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01008                                                                   ECS191
01009      MOVE SPACES                 TO  P-DATA.                      ECS191
01010                                                                   ECS191
01011      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01012                                                                   ECS191
01013      COMPUTE COMP-CLEAR = COMP-CLEAR + ACCT-CLEAR.                ECS191
01014      COMPUTE COMP-CLEAR-DB = COMP-CLEAR-DB + ACCT-CLEAR-DB.       ECS191
01015      COMPUTE COMP-CLEAR-CR = COMP-CLEAR-CR + ACCT-CLEAR-CR.       ECS191
01016                                                                   ECS191
01017      COMPUTE COMP-LIFE = COMP-LIFE + ACCT-LIFE.                   ECS191
01018      COMPUTE COMP-LIFE-DB = COMP-LIFE-DB + ACCT-LIFE-DB.          ECS191
01019      COMPUTE COMP-LIFE-CR = COMP-LIFE-CR + ACCT-LIFE-CR.          ECS191
01020                                                                   ECS191
01021      COMPUTE COMP-AH = COMP-AH + ACCT-AH.                         ECS191
01022      COMPUTE COMP-AH-DB = COMP-AH-DB + ACCT-AH-DB.                ECS191
01023      COMPUTE COMP-AH-CR = COMP-AH-CR + ACCT-AH-CR.                ECS191
01024                                                                   ECS191
01025      COMPUTE COMP-EXP-1 = COMP-EXP-1 + ACCT-EXP-1.                ECS191
01026      COMPUTE COMP-EXP-1-DB = COMP-EXP-1-DB + ACCT-EXP-1-DB.       ECS191
01027      COMPUTE COMP-EXP-1-CR = COMP-EXP-1-CR + ACCT-EXP-1-CR.       ECS191
01028                                                                   ECS191
01029      COMPUTE COMP-EXP-2 = COMP-EXP-2 + ACCT-EXP-2.                ECS191
01030      COMPUTE COMP-EXP-2-DB = COMP-EXP-2-DB + ACCT-EXP-2-DB.       ECS191
01031      COMPUTE COMP-EXP-2-CR = COMP-EXP-2-CR + ACCT-EXP-2-CR.       ECS191
01032                                                                   ECS191
01033      MOVE COMP-ZEROS             TO  ACCOUNT-TOTALS.              ECS191
01034      MOVE DE-STATE               TO  PREV-STATE.                  ECS191
01035      MOVE DE-ACCOUNT             TO  PREV-ACCT.                   ECS191
01036                                                                   ECS191
01037  2199-XIT.                                                        ECS191
01038      EXIT.                                                        ECS191
01039                                                                   ECS191
01040  2200-COMP-BREAK.                                                 ECS191
01041      MOVE SPACE-2                TO  X.                           ECS191
01042      MOVE DETAIL-GL-LINE         TO  P-DATA.                      ECS191
01043                                                                   ECS191
01044      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01045                                                                   ECS191
01046      MOVE SPACES                 TO  DETAIL-LINE.                 ECS191
01047      MOVE PREV-COMP              TO  DET-COMP.                    ECS191
01048      MOVE PREV-CARR              TO  DET-CARR.                    ECS191
01049      MOVE COMP-CLEAR             TO  DET-AMT-CASH-CLEAR.          ECS191
01050      MOVE COMP-LIFE              TO  DET-AMT-LIFE.                ECS191
01051      MOVE COMP-AH                TO  DET-AMT-AH.                  ECS191
01052      MOVE COMP-EXP-1             TO  DET-AMT-EXP-1.               ECS191
01053      MOVE COMP-EXP-2             TO  DET-AMT-EXP-2.               ECS191
01054      MOVE SPACE-1                TO  X.                           ECS191
01055      MOVE DETAIL-LINE            TO  P-DATA.                      ECS191
01056                                                                   ECS191
01057      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01058                                                                   ECS191
01059      MOVE SPACES                 TO  DETAIL-LINE.                 ECS191
01060      MOVE '  TOTAL DEBITS '      TO  DET-DESC.                    ECS191
01061      MOVE COMP-CLEAR-DB          TO  DET-AMT-CASH-CLEAR.          ECS191
01062      MOVE COMP-LIFE-DB           TO  DET-AMT-LIFE.                ECS191
01063      MOVE COMP-AH-DB             TO  DET-AMT-AH.                  ECS191
01064      MOVE COMP-EXP-1-DB          TO  DET-AMT-EXP-1.               ECS191
01065      MOVE COMP-EXP-2-DB          TO  DET-AMT-EXP-2.               ECS191
01066      MOVE SPACE-2                TO  X.                           ECS191
01067      MOVE DETAIL-LINE            TO  P-DATA.                      ECS191
01068                                                                   ECS191
01069      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01070                                                                   ECS191
01071      MOVE SPACES                 TO  DETAIL-LINE.                 ECS191
01072      MOVE '  TOTAL CREDITS'      TO  DET-DESC.                    ECS191
01073      MOVE COMP-CLEAR-CR          TO  DET-AMT-CASH-CLEAR.          ECS191
01074      MOVE COMP-LIFE-CR           TO  DET-AMT-LIFE.                ECS191
01075      MOVE COMP-AH-CR             TO  DET-AMT-AH.                  ECS191
01076      MOVE COMP-EXP-1-CR          TO  DET-AMT-EXP-1.               ECS191
01077      MOVE COMP-EXP-2-CR          TO  DET-AMT-EXP-2.               ECS191
01078      MOVE SPACE-1                TO  X.                           ECS191
01079      MOVE DETAIL-LINE            TO  P-DATA.                      ECS191
01080                                                                   ECS191
01081      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01082                                                                   ECS191
01083      COMPUTE CARR-CLEAR = CARR-CLEAR + COMP-CLEAR.                ECS191
01084      COMPUTE CARR-CLEAR-DB = CARR-CLEAR-DB + COMP-CLEAR-DB.       ECS191
01085      COMPUTE CARR-CLEAR-CR = CARR-CLEAR-CR + COMP-CLEAR-CR.       ECS191
01086                                                                   ECS191
01087      COMPUTE CARR-LIFE = CARR-LIFE + COMP-LIFE.                   ECS191
01088      COMPUTE CARR-LIFE-DB = CARR-LIFE-DB + COMP-LIFE-DB.          ECS191
01089      COMPUTE CARR-LIFE-CR = CARR-LIFE-CR + COMP-LIFE-CR.          ECS191
01090                                                                   ECS191
01091      COMPUTE CARR-AH = CARR-AH + COMP-AH.                         ECS191
01092      COMPUTE CARR-AH-DB = CARR-AH-DB + COMP-AH-DB.                ECS191
01093      COMPUTE CARR-AH-CR = CARR-AH-CR + COMP-AH-CR.                ECS191
01094                                                                   ECS191
01095      COMPUTE CARR-EXP-1 = CARR-EXP-1 + COMP-EXP-1.                ECS191
01096      COMPUTE CARR-EXP-1-DB = CARR-EXP-1-DB + COMP-EXP-1-DB.       ECS191
01097      COMPUTE CARR-EXP-1-CR = CARR-EXP-1-CR + COMP-EXP-1-CR.       ECS191
01098                                                                   ECS191
01099      COMPUTE CARR-EXP-2 = CARR-EXP-2 + COMP-EXP-2.                ECS191
01100      COMPUTE CARR-EXP-2-DB = CARR-EXP-2-DB + COMP-EXP-2-DB.       ECS191
01101      COMPUTE CARR-EXP-2-CR = CARR-EXP-2-CR + COMP-EXP-2-CR.       ECS191
01102                                                                   ECS191
01103      MOVE COMP-ZEROS             TO  COMPANY-TOTALS.              ECS191
01104      MOVE DE-GROUPING            TO  PREV-COMP.                   ECS191
01105                                                                   ECS191
01106  2299-XIT.                                                        ECS191
01107      EXIT.                                                        ECS191
01108                                                                   ECS191
01109  2300-CARR-BREAK.                                                 ECS191
01110      MOVE SPACES                 TO  DETAIL-LINE.                 ECS191
01111      MOVE PREV-CARR              TO  DET-CARR.                    ECS191
01112      MOVE CARR-CLEAR             TO  DET-AMT-CASH-CLEAR.          ECS191
01113      MOVE CARR-LIFE              TO  DET-AMT-LIFE.                ECS191
01114      MOVE CARR-AH                TO  DET-AMT-AH.                  ECS191
01115      MOVE CARR-EXP-1             TO  DET-AMT-EXP-1.               ECS191
01116      MOVE CARR-EXP-2             TO  DET-AMT-EXP-2.               ECS191
01117      MOVE SPACE-3                TO  X.                           ECS191
01118      MOVE DETAIL-LINE            TO  P-DATA.                      ECS191
01119                                                                   ECS191
01120      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01121                                                                   ECS191
01122      MOVE SPACES                 TO  DETAIL-LINE.                 ECS191
01123      MOVE '  TOTAL DEBITS '      TO  DET-DESC.                    ECS191
01124      MOVE CARR-CLEAR-DB          TO  DET-AMT-CASH-CLEAR.          ECS191
01125      MOVE CARR-LIFE-DB           TO  DET-AMT-LIFE.                ECS191
01126      MOVE CARR-AH-DB             TO  DET-AMT-AH.                  ECS191
01127      MOVE CARR-EXP-1-DB          TO  DET-AMT-EXP-1.               ECS191
01128      MOVE CARR-EXP-2-DB          TO  DET-AMT-EXP-2.               ECS191
01129      MOVE SPACE-2                TO  X.                           ECS191
01130      MOVE DETAIL-LINE            TO  P-DATA.                      ECS191
01131                                                                   ECS191
01132      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01133                                                                   ECS191
01134      MOVE SPACES                 TO  DETAIL-LINE.                 ECS191
01135      MOVE '  TOTAL CREDITS'      TO  DET-DESC.                    ECS191
01136      MOVE CARR-CLEAR-CR          TO  DET-AMT-CASH-CLEAR.          ECS191
01137      MOVE CARR-LIFE-CR           TO  DET-AMT-LIFE.                ECS191
01138      MOVE CARR-AH-CR             TO  DET-AMT-AH.                  ECS191
01139      MOVE CARR-EXP-1-CR          TO  DET-AMT-EXP-1.               ECS191
01140      MOVE CARR-EXP-2-CR          TO  DET-AMT-EXP-2.               ECS191
01141      MOVE SPACE-1                TO  X.                           ECS191
01142      MOVE DETAIL-LINE            TO  P-DATA.                      ECS191
01143                                                                   ECS191
01144      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01145                                                                   ECS191
01146      COMPUTE GRND-CLEAR = GRND-CLEAR + CARR-CLEAR.                ECS191
01147      COMPUTE GRND-CLEAR-DB = GRND-CLEAR-DB + CARR-CLEAR-DB.       ECS191
01148      COMPUTE GRND-CLEAR-CR = GRND-CLEAR-CR + CARR-CLEAR-CR.       ECS191
01149                                                                   ECS191
01150      COMPUTE GRND-LIFE = GRND-LIFE + CARR-LIFE.                   ECS191
01151      COMPUTE GRND-LIFE-DB = GRND-LIFE-DB + CARR-LIFE-DB.          ECS191
01152      COMPUTE GRND-LIFE-CR = GRND-LIFE-CR + CARR-LIFE-CR.          ECS191
01153                                                                   ECS191
01154      COMPUTE GRND-AH = GRND-AH + CARR-AH.                         ECS191
01155      COMPUTE GRND-AH-DB = GRND-AH-DB + CARR-AH-DB.                ECS191
01156      COMPUTE GRND-AH-CR = GRND-AH-CR + CARR-AH-CR.                ECS191
01157                                                                   ECS191
01158      COMPUTE GRND-EXP-1 = GRND-EXP-1 + CARR-EXP-1.                ECS191
01159      COMPUTE GRND-EXP-1-DB = GRND-EXP-1-DB + CARR-EXP-1-DB.       ECS191
01160      COMPUTE GRND-EXP-1-CR = GRND-EXP-1-CR + CARR-EXP-1-CR.       ECS191
01161                                                                   ECS191
01162      COMPUTE GRND-EXP-2 = GRND-EXP-2 + CARR-EXP-2.                ECS191
01163      COMPUTE GRND-EXP-2-DB = GRND-EXP-2-DB + CARR-EXP-2-DB.       ECS191
01164      COMPUTE GRND-EXP-2-CR = GRND-EXP-2-CR + CARR-EXP-2-CR.       ECS191
01165                                                                   ECS191
01166      MOVE COMP-ZEROS             TO  CARRIER-TOTALS.              ECS191
01167      MOVE DE-CARRIER             TO  PREV-CARR.                   ECS191
01168                                                                   ECS191
01169  2399-XIT.                                                        ECS191
01170      EXIT.                                                        ECS191
01171                                                                   ECS191
01172  2400-GRAND-TOTALS.                                               ECS191
01173      PERFORM 2500-HEADING-ROUTINE  THRU  2599-XIT.                ECS191
01174                                                                   ECS191
01175      MOVE SPACES                 TO  DETAIL-LINE.                 ECS191
01176      MOVE 'GRAND TOTALS   '      TO  DET-DESC.                    ECS191
01177      MOVE GRND-CLEAR             TO  DET-AMT-CASH-CLEAR.          ECS191
01178      MOVE GRND-LIFE              TO  DET-AMT-LIFE.                ECS191
01179      MOVE GRND-AH                TO  DET-AMT-AH.                  ECS191
01180      MOVE GRND-EXP-1             TO  DET-AMT-EXP-1.               ECS191
01181      MOVE GRND-EXP-2             TO  DET-AMT-EXP-2.               ECS191
01182      MOVE SPACE-3                TO  X.                           ECS191
01183      MOVE DETAIL-LINE            TO  P-DATA.                      ECS191
01184                                                                   ECS191
01185      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01186                                                                   ECS191
01187      MOVE SPACES                 TO  DETAIL-LINE.                 ECS191
01188      MOVE '  TOTAL DEBITS '      TO  DET-DESC.                    ECS191
01189      MOVE GRND-CLEAR-DB          TO  DET-AMT-CASH-CLEAR.          ECS191
01190      MOVE GRND-LIFE-DB           TO  DET-AMT-LIFE.                ECS191
01191      MOVE GRND-AH-DB             TO  DET-AMT-AH.                  ECS191
01192      MOVE GRND-EXP-1-DB          TO  DET-AMT-EXP-1.               ECS191
01193      MOVE GRND-EXP-2-DB          TO  DET-AMT-EXP-2.               ECS191
01194      MOVE SPACE-2                TO  X.                           ECS191
01195      MOVE DETAIL-LINE            TO  P-DATA.                      ECS191
01196                                                                   ECS191
01197      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01198                                                                   ECS191
01199      MOVE SPACES                 TO  DETAIL-LINE.                 ECS191
01200      MOVE '  TOTAL CREDITS'      TO  DET-DESC.                    ECS191
01201      MOVE GRND-CLEAR-CR          TO  DET-AMT-CASH-CLEAR.          ECS191
01202      MOVE GRND-LIFE-CR           TO  DET-AMT-LIFE.                ECS191
01203      MOVE GRND-AH-CR             TO  DET-AMT-AH.                  ECS191
01204      MOVE GRND-EXP-1-CR          TO  DET-AMT-EXP-1.               ECS191
01205      MOVE GRND-EXP-2-CR          TO  DET-AMT-EXP-2.               ECS191
01206      MOVE SPACE-1                TO  X.                           ECS191
01207      MOVE DETAIL-LINE            TO  P-DATA.                      ECS191
01208                                                                   ECS191
01209      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01210                                                                   ECS191
01211  2499-XIT.                                                        ECS191
01212      EXIT.                                                        ECS191
01213  EJECT                                                            ECS191
01214  PERFORMED-PROCEDURES SECTION.                                    ECS191
01215                                                                   ECS191
01216  2500-HEADING-ROUTINE.                                            ECS191
01217      MOVE ZEROS                  TO  LINE-CNT.                    ECS191
01218                                                                   ECS191
01219      ADD 1                       TO  PAGE-CNT.                    ECS191
01220                                                                   ECS191
01221      MOVE SPACE-NP               TO  X.                           ECS191
01222      MOVE PAGE-CNT               TO  HD-PAGE.                     ECS191
01223      MOVE HEAD-LINE-1            TO  P-DATA.                      ECS191
01224                                                                   ECS191
01225      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01226                                                                   ECS191
01227      MOVE SPACE-1                TO  X.                           ECS191
01228      MOVE HEAD-LINE-2            TO  P-DATA.                      ECS191
01229                                                                   ECS191
01230      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01231                                                                   ECS191
01232      MOVE HEAD-LINE-3            TO  P-DATA.                      ECS191
01233                                                                   ECS191
01234      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01235                                                                   ECS191
01236      MOVE SPACE-2                TO  X.                           ECS191
01237      MOVE HEAD-LINE-4            TO  P-DATA.                      ECS191
01238                                                                   ECS191
01239      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01240                                                                   ECS191
01241      MOVE SPACE-1                TO  X.                           ECS191
01242      MOVE HEAD-LINE-5            TO  P-DATA.                      ECS191
01243                                                                   ECS191
01244      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01245                                                                   ECS191
01246      MOVE SPACE-2                TO  X.                           ECS191
01247      MOVE SPACES                 TO  P-DATA.                      ECS191
01248                                                                   ECS191
01249      PERFORM 2600-PRINT-ROUTINE  THRU  2699-XIT.                  ECS191
01250                                                                   ECS191
01251  2599-XIT.                                                        ECS191
01252      EXIT.                                                        ECS191
01253                                                                   ECS191
01254  2600-PRINT-ROUTINE.                                              ECS191
01255                              COPY ELCPRT2.                        ECS191
01256                                                                   ECS191
01257      IF X = SPACE-1                                               ECS191
01258          ADD +1                  TO  LINE-CNT                     ECS191
01259      ELSE                                                         ECS191
01260          IF X = SPACE-2                                           ECS191
01261              ADD +2              TO  LINE-CNT                     ECS191
01262          ELSE                                                     ECS191
01263              IF X = SPACE-3                                       ECS191
01264                  ADD +3          TO  LINE-CNT                     ECS191
01265              ELSE                                                 ECS191
01266                  MOVE +0         TO  LINE-CNT.                    ECS191
01267                                                                   ECS191
01268      IF LINE-CNT  IS GREATER THAN  +52                            ECS191
01269          PERFORM 2500-HEADING-ROUTINE  THRU  2599-XIT.            ECS191
01270                                                                   ECS191
01271  2699-XIT.                                                        ECS191
01272      EXIT.                                                        ECS191
01273                                                                   ECS191
01274  3000-LEDGER-TABLE-LOOKUP.                                        ECS191
01275      SET L                       TO  1.                           ECS191
01276                                                                   ECS191
01277      SEARCH LEDGER-NUMBERS  VARYING  L  AT END                    ECS191
01278          GO TO 3090-LOOKUP-DEFAULT                                ECS191
01279          WHEN SEARCH-CARR-CO = LEDGER-CARR-CO (L)                 ECS191
01280              MOVE LEDGER-ACCT-NUMBERS (L)                         ECS191
01281                                  TO  GENERAL-LEDGER-NUMBERS       ECS191
01282              GO TO 3099-XIT.                                      ECS191
01283                                                                   ECS191
01284  3090-LOOKUP-DEFAULT.                                             ECS191
01285      IF SEARCH-COMP = '000001'                                    ECS191
01286            MOVE LEDGER-ACCT-NUMBERS (1)                           ECS191
01287                                  TO  GENERAL-LEDGER-NUMBERS.      ECS191
01288      IF SEARCH-COMP = '000002'                                    ECS191
01289            MOVE LEDGER-ACCT-NUMBERS (2)                           ECS191
01290                                  TO  GENERAL-LEDGER-NUMBERS.      ECS191
01291      IF SEARCH-COMP = '000003'                                    ECS191
01292            MOVE LEDGER-ACCT-NUMBERS (3)                           ECS191
01293                                  TO  GENERAL-LEDGER-NUMBERS.      ECS191
01294                                                                   ECS191
01295  3099-XIT.                                                        ECS191
01296      EXIT.                                                        ECS191
01297                                                                   ECS191
01298  ABEND-PGM SECTION.                                               ECS191
01299                              COPY ELCABEND.                       ECS191
