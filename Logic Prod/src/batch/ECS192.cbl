00001  IDENTIFICATION DIVISION.                                         04/21/98
00002                                                                   ECS192
00003  PROGRAM-ID.                 ECS192.                                 LV006
00004 *              PROGRAM CONVERTED BY                               ECS192
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS192
00006 *              CONVERSION DATE 04/21/98 09:16:28.                 ECS192
00007 *               PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE         ECS192
00008 *                            VMOD=2.006.                          ECS192
00009 *AUTHOR.        LOGIC INC.                                        ECS192
00010 *               DALLAS, TEXAS.                                    ECS192
00011 *                                                                 ECS192
00012 *DATE-COMPILED.                                                   ECS192
00013 *                                                                 ECS192
00014 *SECURITY.   *****************************************************ECS192
00015 *            *                                                   *ECS192
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC INC.      *ECS192
00017 *            *                                                   *ECS192
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS192
00019 *            *   OF LOGIC INC. IS EXPRESSLY PROHIBITED WITHOUT   *ECS192
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS192
00021 *            *                                                   *ECS192
00022 *            *****************************************************ECS192
00023 *                                                                 ECS192
00024 *REMARKS.                                                         ECS192
00025 *        READS PAYMENT AND ADJUSTMENT CARDS FROM ECS061 AND       ECS192
00026 *        GENERATES GENERAL LEDGER INTERFACE RECORDS.              ECS192
00027                                                                   ECS192
00028  ENVIRONMENT DIVISION.                                            ECS192
00029  CONFIGURATION SECTION.                                           ECS192
00030  SPECIAL-NAMES.                                                   ECS192
00031      C02 IS LCP-CH2                                               ECS192
00032      C03 IS LCP-CH3                                               ECS192
00033      C04 IS LCP-CH4                                               ECS192
00034      C05 IS LCP-CH5                                               ECS192
00035      C06 IS LCP-CH6                                               ECS192
00036      C07 IS LCP-CH7                                               ECS192
00037      C08 IS LCP-CH8                                               ECS192
00038      C09 IS LCP-CH9                                               ECS192
00039      C10 IS LCP-CH10                                              ECS192
00040      C11 IS LCP-CH11                                              ECS192
00041      C12 IS LCP-CH12                                              ECS192
00042      S01 IS LCP-P01                                               ECS192
00043      S02 IS LCP-P02.                                              ECS192
00044  INPUT-OUTPUT SECTION.                                            ECS192
00045  FILE-CONTROL.                                                    ECS192
00046                                                                   ECS192
00047      SELECT PAYADJ-CARDS     ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS192
00048      SELECT ABC-PAYMENTS     ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS192
00049      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS192
00050      SELECT PRINT-FILE       ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS192
00051      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS192
00052      SELECT SORT-FILE        ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  ECS192
00053  EJECT                                                            ECS192
00054  DATA DIVISION.                                                   ECS192
00055  FILE SECTION.                                                    ECS192
00056                                                                   ECS192
00057  FD  PAYADJ-CARDS                                                 ECS192
00058      RECORDING MODE IS F                                          ECS192
00059      BLOCK CONTAINS 0 RECORDS
00060                                .                                  ECS192
00061                                                                   ECS192
00062  01  CARD-RECORD             PIC  X(80).                          ECS192
00063  EJECT                                                            ECS192
00064  FD  ABC-PAYMENTS                                                 ECS192
00065      RECORDING MODE IS F                                          ECS192
00066      BLOCK CONTAINS 0 RECORDS
00067                                   .                               ECS192
00068                                                                   ECS192
00069  01  ABC-PAYMNT-REC          PIC  X(80).                          ECS192
00070  EJECT                                                            ECS192
00071  SD  SORT-FILE                                                    ECS192
00072 *    LABEL RECORDS ARE STANDARD                                   ECS192
00073                                .                                  ECS192
00074                                                                   ECS192
00075  01  SORT-RECORD.                                                 ECS192
00076      12  SRT-CARD-TYPE       PIC  X(3).                           ECS192
00077          88  CLA-CARD                            VALUE 'CLA'.     ECS192
00078      12  SRT-CARRIER         PIC  X.                              ECS192
00079      12  SRT-COMPANY         PIC  X(6).                           ECS192
00080      12  SRT-FIN-RESP        PIC  X(10).                          ECS192
00081      12  SRT-ACCOUNT         PIC  X(10).                          ECS192
00082      12  FILLER              PIC  X(8).                           ECS192
00083      12  SRT-ENTRY-DATE      PIC  X(6).                           ECS192
00084      12  FILLER              PIC  X(24).                          ECS192
00085      12  SRT-PYADJ-TYPE      PIC  X.                              ECS192
00086          88  RECEIVED                            VALUE 'R'.       ECS192
00087          88  CHARGED                             VALUE 'C'.       ECS192
00088      12  SRT-AMOUNT          PIC  9(7)V99.                        ECS192
00089      12  FILLER              PIC  X.                              ECS192
00090      12  SRT-ERROR-FLAG      PIC  X.                              ECS192
00091  EJECT                                                            ECS192
00092  FD  DISK-DATE                                                    ECS192
00093                              COPY ELCDTEFD.                       ECS192
00094  EJECT                                                            ECS192
00095  FD  PRINT-FILE                                                   ECS192
00096                              COPY ELCPRTFD.                       ECS192
00097  EJECT                                                            ECS192
00098  FD  FICH                                                         ECS192
00099                              COPY ELCFCHFD.                       ECS192
00100  EJECT                                                            ECS192
00101  WORKING-STORAGE SECTION.                                         ECS192
00102  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS192
00103  77  LCP-ASA                       PIC X.                         ECS192
00104  77  FILLER  PIC  X(32) VALUE '********************************'. ECS192
00105  77  FILLER  PIC  X(32) VALUE '     ECS192 WORKING-STORAGE     '. ECS192
00106  77  FILLER  PIC  X(32) VALUE '*********VMOD=2.006*************'. ECS192
00107                                                                   ECS192
00108  77  LINE-CNT                PIC S9(3)   COMP-3  VALUE +99.       ECS192
00109  77  PAGE-CNT                PIC S9(3)   COMP-3  VALUE +0.        ECS192
00110  77  PGM-SUB                 PIC S9(3)   COMP-3  VALUE +192.      ECS192
00111  77  X                       PIC  X              VALUE SPACE.     ECS192
00112  77  PREV-CARR               PIC  X              VALUE LOW-VALUE. ECS192
00113  77  PREV-COMP               PIC  X(6)           VALUE LOW-VALUE. ECS192
00114                                                                   ECS192
00115  01  REQUIRED-STORAGE.                                            ECS192
00116      12  WS-RETURN-CODE          PIC S9(4)              COMP.     ECS192
00117      12  WS-ABEND-MESSAGE        PIC  X(80).                      ECS192
00118      12  WS-ABEND-FILE-STATUS    PIC  XX     VALUE ZEROS.         ECS192
00119      12  WS-ZERO                 PIC S9      VALUE ZERO COMP-3.   ECS192
00120  EJECT                                                            ECS192
00121 ******************************************************************ECS192
00122 ******************************************************************ECS192
00123 **                                                              **ECS192
00124 **    THIS IS THE FORMAT OF THE INPUT TO THE GENERAL LEDGER     **ECS192
00125 **    SYSTEM.  IT MAY BE NECESSARY TO MODIFY THIS FORMAT TO     **ECS192
00126 **    MEET THE REQUIREMENTS OF YOUR GENERAL LEDGER SYSTEM.      **ECS192
00127 **                                                              **ECS192
00128 ******************************************************************ECS192
00129 ******************************************************************ECS192
00130                                                                   ECS192
00131  01  WS-3800-CARD.                                                ECS192
00132      12  3800-LITERAL            PIC  X(6)       VALUE '0LG00 '.  ECS192
00133      12  3800-DATE-1 COMP-3.                                      ECS192
00134          16  FILLER              PIC  99.                         ECS192
00135          16  3800-CC             PIC  999.                        ECS192
00136          16  3800-YEAR           PIC  99.                         ECS192
00137          16  3800-MONTH          PIC  99.                         ECS192
00138          16  3800-DAY            PIC  99.                         ECS192
00139      12  3800-VOUCHER-ID         PIC  X(5)       VALUE '*****'.   ECS192
00140      12  3800-JULIAN-DATE  COMP-3.                                ECS192
00141          16  FILLER              PIC  99.                         ECS192
00142          16  3800-JULIAN-CC      PIC  99.                         ECS192
00143          16  3800-JULIAN-YEAR    PIC  99.                         ECS192
00144          16  3800-JULIAN-DAY     PIC  9(3).                       ECS192
00145      12  3800-GENERAL-LEDGER     PIC  X(7).                       ECS192
00146      12  3800-U-LITERAL          PIC  X          VALUE 'U'.       ECS192
00147      12  3800-AMOUNT             PIC  9(7)V99.                    ECS192
00148      12  3800-DEBIT-CREDIT       PIC  X.                          ECS192
00149      12  3800-DATE-2             PIC  X(6).                       ECS192
00150      12  3800-DESCRIPTION.                                        ECS192
00151          16  3800-ACCOUNT        PIC  X(10).                      ECS192
00152          16  3800-DESC           PIC  X(8)       VALUE            ECS192
00153                  'PMT/CR  '.                                      ECS192
00154          16  3800-SYSTEM         PIC  X          VALUE 'C'.       ECS192
00155      12  FILLER                  PIC  XX         VALUE SPACES.    ECS192
00156      12  3800-STATE              PIC  XX         VALUE SPACES.    ECS192
00157      12  3800-COMPANY            PIC  X(6).                       ECS192
00158      12  FILLER                  PIC  X          VALUE SPACES.    ECS192
00159      12  3800-MISC-MONTH-YEAR    PIC  X(4)       VALUE '0000'.    ECS192
00160  EJECT                                                            ECS192
00161 ******************************************************************ECS192
00162 ******************************************************************ECS192
00163 **                                                              **ECS192
00164 **    THIS TABLE CONTAINS THE GENERAL LEDGER ACCOUNT NUMBERS    **ECS192
00165 **    FOR THE ACCTS RECEIVABLE, AND CASH CLEARING ACCOUNTS.     **ECS192
00166 **                                                              **ECS192
00167 **    BASED UPON CARRIER AND GROUPING CODE MODIFY THIS TABLE    **ECS192
00168 **    BY ADDING THE ACCOUNT NUMBERS FROM YOUR PARTICULAR        **ECS192
00169 **    CHART OF ACCOUNTS.                                        **ECS192
00170 **                                                              **ECS192
00171 ******************************************************************ECS192
00172 ******************************************************************ECS192
00173                                                                   ECS192
00174  01  GENERAL-LEDGER-TABLE.                                        ECS192
00175      12  FILLER              PIC  X(21)          VALUE            ECS192
00176              'U00000116300002310920'.                             ECS192
00177      12  FILLER              PIC  X(21)          VALUE            ECS192
00178              'U00000216300002310920'.                             ECS192
00179      12  FILLER              PIC  X(21)          VALUE            ECS192
00180              'U00000316304002310940'.                             ECS192
00181      12  FILLER              PIC  X(21)          VALUE            ECS192
00182              'A00000116300002310920'.                             ECS192
00183      12  FILLER              PIC  X(21)          VALUE            ECS192
00184              'A00000216300002310920'.                             ECS192
00185      12  FILLER              PIC  X(21)          VALUE            ECS192
00186              'A00000316300002310920'.                             ECS192
00187      12  FILLER              PIC  X(21)          VALUE            ECS192
00188              'F00000111558902212890'.                             ECS192
00189      12  FILLER              PIC  X(21)          VALUE            ECS192
00190              'F00000211558902212890'.                             ECS192
00191      12  FILLER              PIC  X(21)          VALUE            ECS192
00192              'F00000311558902212890'.                             ECS192
00193                                                                   ECS192
00194  01  LEDGER-NUMBER-TABLE  REDEFINES  GENERAL-LEDGER-TABLE.        ECS192
00195      12  LEDGER-NUMBERS  OCCURS  09  TIMES   INDEXED BY  L.       ECS192
00196          16  LEDGER-CARR-CO          PIC  X(7).                   ECS192
00197          16  LEDGER-ACCT-NUMBERS     PIC  X(14).                  ECS192
00198                                                                   ECS192
00199  01  GENERAL-LEDGER-NUMBERS.                                      ECS192
00200      12  GL-ACCT-REC-NO      PIC  X(7).                           ECS192
00201      12  GL-CASH-CLEAR-NO    PIC  X(7).                           ECS192
00202  EJECT                                                            ECS192
00203  01  FIELD-REPRESENTATIVE    PIC  X(10).                          ECS192
00204      88  FIELD-REP     VALUE '0000005500' '0000005511'            ECS192
00205                              '0000005544' '0000005545'            ECS192
00206                              '0000005602' '0000005627'            ECS192
00207                              '0000005729' '0000005751'            ECS192
00208                              '0000005759' '0000005761'            ECS192
00209                              '0000005769' '0000005770'            ECS192
00210                              '0000005785' '0000005699'            ECS192
00211                              '00000A0006' '00000A0007'            ECS192
00212                              '0000005795' '0000005797'            ECS192
00213                              '0000777777'.                        ECS192
00214                                                                   ECS192
00215  01  COMPANY-TOTALS      COMP-3.                                  ECS192
00216      12  COMP-PYMTS          PIC S9(9)V99        VALUE ZEROS.     ECS192
00217      12  COMP-PYMTS-DB       PIC S9(9)V99        VALUE ZEROS.     ECS192
00218      12  COMP-PYMTS-CR       PIC S9(9)V99        VALUE ZEROS.     ECS192
00219                                                                   ECS192
00220  01  CARRIER-TOTALS      COMP-3.                                  ECS192
00221      12  CARR-PYMTS          PIC S9(9)V99        VALUE ZEROS.     ECS192
00222      12  CARR-PYMTS-DB       PIC S9(9)V99        VALUE ZEROS.     ECS192
00223      12  CARR-PYMTS-CR       PIC S9(9)V99        VALUE ZEROS.     ECS192
00224                                                                   ECS192
00225  01  GRAND-TOTALS        COMP-3.                                  ECS192
00226      12  GRND-PYMTS          PIC S9(9)V99        VALUE ZEROS.     ECS192
00227      12  GRND-PYMTS-DB       PIC S9(9)V99        VALUE ZEROS.     ECS192
00228      12  GRND-PYMTS-CR       PIC S9(9)V99        VALUE ZEROS.     ECS192
00229                                                                   ECS192
00230  01  COMP-ZEROS          COMP-3.                                  ECS192
00231      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS192
00232      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS192
00233      12  FILLER              PIC S9(9)V99        VALUE ZEROS.     ECS192
00234                                                                   ECS192
00235  01  SEARCH-CARR-CO.                                              ECS192
00236      12  SEARCH-CARR         PIC  X.                              ECS192
00237      12  SEARCH-COMP         PIC  X(6).                           ECS192
00238  EJECT                                                            ECS192
00239  01  HEAD-LINE-1.                                                 ECS192
00240      12  FILLER              PIC  X(44)          VALUE SPACES.    ECS192
00241      12  FILLER              PIC  X(36)          VALUE            ECS192
00242              'GENERAL LEDGER INTERFACE  (PAYMENTS)'.              ECS192
00243      12  FILLER              PIC  X(44)          VALUE SPACES.    ECS192
00244      12  FILLER              PIC  X(7)           VALUE 'ECS-192'. ECS192
00245      12  FILLER              PIC  X              VALUE SPACE.     ECS192
00246                                                                   ECS192
00247  01  HEAD-LINE-2.                                                 ECS192
00248      12  FILLER              PIC  X(47)          VALUE SPACES.    ECS192
00249      12  HD-CLIENT           PIC  X(30).                          ECS192
00250      12  FILLER              PIC  X(47)          VALUE SPACES.    ECS192
00251      12  HD-RUN              PIC  X(8).                           ECS192
00252                                                                   ECS192
00253  01  HEAD-LINE-3.                                                 ECS192
00254      12  FILLER              PIC  X(53)          VALUE SPACES.    ECS192
00255      12  HD-DATE             PIC  X(18).                          ECS192
00256      12  FILLER              PIC  X(41)          VALUE SPACES.    ECS192
00257      12  FILLER              PIC  X(5)           VALUE 'PAGE'.    ECS192
00258      12  HD-PAGE             PIC ZZ,ZZZ.                          ECS192
00259      12  FILLER              PIC  X(9)           VALUE SPACES.    ECS192
00260                                                                   ECS192
00261  01  HEAD-LINE-4.                                                 ECS192
00262      12  FILLER              PIC  X(51)          VALUE SPACES.    ECS192
00263      12  FILLER              PIC  X(22)          VALUE            ECS192
00264              'CREDIT           DEBIT'.                            ECS192
00265                                                                   ECS192
00266  01  HEAD-LINE-5.                                                 ECS192
00267      12  FILLER              PIC  X(44)          VALUE            ECS192
00268              '    CAR  GROUP    FIN. RESP.       ACCOUNT  '.      ECS192
00269      12  FILLER              PIC  X(34)          VALUE            ECS192
00270              '  ACCTS RECEIVABLE   CASH CLEARING'.                ECS192
00271                                                                   ECS192
00272  01  DETAIL-LINE.                                                 ECS192
00273      12  FILLER              PIC  X(5).                           ECS192
00274      12  DET-CARR            PIC  X.                              ECS192
00275      12  FILLER              PIC  X(3).                           ECS192
00276      12  DET-COMP            PIC  X(6).                           ECS192
00277      12  DET-DESC.                                                ECS192
00278          16  FILLER          PIC  X(3).                           ECS192
00279          16  DET-RESP        PIC  X(10).                          ECS192
00280          16  FILLER          PIC  XX.                             ECS192
00281          16  DET-FLD-REP     PIC  X.                              ECS192
00282          16  FILLER          PIC  XX.                             ECS192
00283          16  DET-ACCT        PIC  X(10).                          ECS192
00284      12  FILLER              PIC  X(3).                           ECS192
00285      12  DET-AMOUNT-CR       PIC ZZZ,ZZZ,ZZZ.99.                  ECS192
00286      12  DET-MINUS1          PIC  X.                              ECS192
00287      12  FILLER              PIC  XX.                             ECS192
00288      12  DET-AMOUNT-DB       PIC ZZZ,ZZZ,ZZZ.99.                  ECS192
00289      12  DET-MINUS2          PIC  X.                              ECS192
00290      12  FILLER              PIC  X(54).                          ECS192
00291                                                                   ECS192
00292  01  DETAIL-GL-LINE.                                              ECS192
00293      12  FILLER              PIC  X(20)          VALUE SPACES.    ECS192
00294      12  FILLER              PIC  X(18)          VALUE            ECS192
00295              '**********      **'.                                ECS192
00296      12  DET-AR-GLNO         PIC  X(7).                           ECS192
00297      12  FILLER              PIC  X(10)          VALUE            ECS192
00298              '**      **'.                                        ECS192
00299      12  DET-CC-GLNO         PIC  X(7).                           ECS192
00300      12  FILLER              PIC  XX             VALUE '**'.      ECS192
00301      12  FILLER              PIC  X(68)          VALUE SPACES.    ECS192
00302  EJECT                                                            ECS192
00303                             COPY ELCDATE.                         ECS192
00304                                                                   ECS192
00305                              COPY ELCDTECX.                       ECS192
00306                                                                   ECS192
00307                              COPY ELCDTEVR.                       ECS192
00308  EJECT                                                            ECS192
00309  PROCEDURE DIVISION.                                              ECS192
00310                                                                   ECS192
00311  0000-GET-DATE.                                                   ECS192
00312                              COPY ELCDTERX.                       ECS192
00313                                                                   ECS192
00314      MOVE ALPH-DATE              TO  HD-DATE.                     ECS192
00315      MOVE WS-CURRENT-DATE        TO  HD-RUN.                      ECS192
00316      MOVE COMPANY-NAME           TO  HD-CLIENT.                   ECS192
00317      MOVE RUN-DATE               TO  3800-DATE-1.                 ECS192
00318      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               ECS192
00319      MOVE ' '                    TO  DC-OPTION-CODE.              ECS192
00320                                                                   ECS192
00321      CALL 'ELDATCX'  USING  DATE-CONVERSION-DATA.                 ECS192
00322                                                                   ECS192
00323      MOVE DC-JULIAN-DATE         TO  3800-JULIAN-DATE.            ECS192
00324      MOVE DC-ALPHA-CEN-N         TO  3800-JULIAN-CC.              ECS192
00325      MOVE DTE-CLIENT             TO  3800-COMPANY.                ECS192
00326                                                                   ECS192
00327      IF DTE-CLIENT = 'CIM'                                        ECS192
00328          MOVE 'WCO'              TO  3800-COMPANY.                ECS192
00329                                                                   ECS192
00330      IF DTE-CLIENT = 'WFL'                                        ECS192
00331          MOVE 'FLA'              TO  3800-COMPANY.                ECS192
00332                                                                   ECS192
00333      IF DTE-CLIENT = 'UFL'  OR  'UFR'                             ECS192
00334          MOVE 'UFL'              TO  3800-COMPANY.                ECS192
00335                                                                   ECS192
00336  0500-SORT-ROUTINE.                                               ECS192
00337      SORT SORT-FILE  ON ASCENDING KEY  SRT-CARRIER                ECS192
00338                                        SRT-COMPANY                ECS192
00339                                        SRT-FIN-RESP               ECS192
00340                                        SRT-ACCOUNT                ECS192
00341          INPUT PROCEDURE  SORT-PAYMENT-ADJUSTMENT-CARDS           ECS192
00342          OUTPUT PROCEDURE CREATE-GEN-LEDGER-TRANSACTIONS.         ECS192
00343                                                                   ECS192
00344      IF SORT-RETURN NOT = ZEROS                                   ECS192
00345          MOVE '0101'             TO  WS-RETURN-CODE               ECS192
00346          MOVE 'BAD RETURN CODE ON INTERNAL SORT'                  ECS192
00347                                  TO  WS-ABEND-MESSAGE             ECS192
00348          GO TO ABEND-PGM.                                         ECS192
00349                                                                   ECS192
00350  0999-END-OF-JOB.                                                 ECS192
00351                              COPY ELCPRTC.                        ECS192
00352                                                                   ECS192
00353      CLOSE ABC-PAYMENTS  PRINT-FILE.                              ECS192
00354                                                                   ECS192
00355      GOBACK.                                                      ECS192
00356  EJECT                                                            ECS192
00357  SORT-PAYMENT-ADJUSTMENT-CARDS SECTION.                           ECS192
00358                                                                   ECS192
00359  1000-OPEN-INPUT.                                                 ECS192
00360      OPEN INPUT PAYADJ-CARDS.                                     ECS192
00361                                                                   ECS192
00362  1010-READ-PYADJ-CARD.                                            ECS192
00363      READ PAYADJ-CARDS  INTO  SORT-RECORD  AT END                 ECS192
00364          GO TO 1090-CLOSE-INPUT.                                  ECS192
00365                                                                   ECS192
00366      IF CLA-CARD  AND  (RECEIVED  OR  CHARGED)                    ECS192
00367          NEXT SENTENCE                                            ECS192
00368      ELSE                                                         ECS192
00369          GO TO 1010-READ-PYADJ-CARD.                              ECS192
00370                                                                   ECS192
00371      IF SRT-ERROR-FLAG NOT = SPACE                                ECS192
00372          GO TO 1010-READ-PYADJ-CARD.                              ECS192
00373                                                                   ECS192
00374  1030-RELEASE-TO-SORT.                                            ECS192
00375      IF DTE-CLIENT = 'CIM'                                        ECS192
00376          MOVE 'W'                TO  SRT-CARRIER.                 ECS192
00377                                                                   ECS192
00378      IF DTE-CLIENT = 'WFL'                                        ECS192
00379          MOVE 'F'                TO  SRT-CARRIER.                 ECS192
00380                                                                   ECS192
00381      IF DTE-CLIENT = 'UFL'                                        ECS192
00382          MOVE 'U'                TO  SRT-CARRIER.                 ECS192
00383                                                                   ECS192
00384      IF DTE-CLIENT = 'UFR'                                        ECS192
00385          MOVE 'R'                TO  SRT-CARRIER.                 ECS192
00386                                                                   ECS192
00387      IF SRT-COMPANY = '000001'  OR  '000002'  OR  '000003'        ECS192
00388          NEXT SENTENCE                                            ECS192
00389      ELSE                                                         ECS192
00390          MOVE '000001'              TO  SRT-COMPANY.              ECS192
00391                                                                   ECS192
00392      IF DTE-CLIENT = 'CIM'                                        ECS192
00393          MOVE '000002'           TO  SRT-COMPANY.                 ECS192
00394                                                                   ECS192
00395      RELEASE SORT-RECORD.                                         ECS192
00396                                                                   ECS192
00397        GO TO 1010-READ-PYADJ-CARD.                                ECS192
00398                                                                   ECS192
00399  1090-CLOSE-INPUT.                                                ECS192
00400      CLOSE PAYADJ-CARDS.                                          ECS192
00401                                                                   ECS192
00402  1099-INPUT-EXIT.                                                 ECS192
00403      EXIT.                                                        ECS192
00404  EJECT                                                            ECS192
00405  CREATE-GEN-LEDGER-TRANSACTIONS SECTION.                          ECS192
00406                                                                   ECS192
00407  2000-OPEN-OUTPUT.                                                ECS192
00408      OPEN OUTPUT ABC-PAYMENTS  PRINT-FILE.                        ECS192
00409                                                                   ECS192
00410      PERFORM 3100-HEADING-ROUTINE  THRU  3199-XIT.                ECS192
00411                                                                   ECS192
00412      PERFORM 2010-RETURN-PYADJ-CARD.                              ECS192
00413                                                                   ECS192
00414      MOVE SRT-CARRIER            TO  PREV-CARR  SEARCH-CARR.      ECS192
00415      MOVE SRT-COMPANY            TO  PREV-COMP  SEARCH-COMP.      ECS192
00416                                                                   ECS192
00417      PERFORM 3000-LEDGER-TABLE-LOOKUP  THRU  3099-XIT.            ECS192
00418                                                                   ECS192
00419      GO TO 2030-PRINT-DETAIL.                                     ECS192
00420                                                                   ECS192
00421  2010-RETURN-PYADJ-CARD.                                          ECS192
00422      RETURN SORT-FILE  AT END                                     ECS192
00423          GO TO 2100-COMP-BREAK.                                   ECS192
00424                                                                   ECS192
00425  2020-CHECK-BREAKS.                                               ECS192
00426      IF SRT-CARRIER NOT = PREV-CARR                               ECS192
00427          PERFORM 2100-COMP-BREAK  THRU  2199-XIT                  ECS192
00428          PERFORM 2200-CARR-BREAK  THRU  2299-XIT                  ECS192
00429          PERFORM 3000-LEDGER-TABLE-LOOKUP  THRU  3099-XIT         ECS192
00430          PERFORM 3100-HEADING-ROUTINE  THRU  3199-XIT.            ECS192
00431                                                                   ECS192
00432      IF SRT-COMPANY NOT = PREV-COMP                               ECS192
00433          PERFORM 2100-COMP-BREAK  THRU  2199-XIT                  ECS192
00434          PERFORM 3000-LEDGER-TABLE-LOOKUP  THRU  3099-XIT         ECS192
00435          PERFORM 3100-HEADING-ROUTINE  THRU  3199-XIT.            ECS192
00436                                                                   ECS192
00437  2030-PRINT-DETAIL.                                               ECS192
00438      MOVE SPACES                 TO  DETAIL-LINE.                 ECS192
00439      MOVE SRT-CARRIER            TO  DET-CARR.                    ECS192
00440      MOVE SRT-COMPANY            TO  DET-COMP.                    ECS192
00441      MOVE SRT-FIN-RESP           TO  DET-RESP.                    ECS192
00442      MOVE SRT-ACCOUNT            TO  DET-ACCT.                    ECS192
00443                                                                   ECS192
00444      IF SRT-ACCOUNT NOT = SPACES AND LOW-VALUES                   ECS192
00445          MOVE SRT-ACCOUNT        TO  3800-ACCOUNT                 ECS192
00446                                      FIELD-REPRESENTATIVE         ECS192
00447      ELSE                                                         ECS192
00448          MOVE SRT-FIN-RESP       TO  3800-ACCOUNT                 ECS192
00449                                      FIELD-REPRESENTATIVE.        ECS192
00450                                                                   ECS192
00451      MOVE GL-ACCT-REC-NO         TO  3800-GENERAL-LEDGER.         ECS192
00452                                                                   ECS192
00453      IF DTE-CLIENT = 'UFL'  OR  'UFR'  OR  'WFL'                  ECS192
00454          IF FIELD-REP                                             ECS192
00455              MOVE '1420250'      TO  3800-GENERAL-LEDGER          ECS192
00456              MOVE '*'            TO  DET-FLD-REP.                 ECS192
00457                                                                   ECS192
00458      MOVE SRT-AMOUNT             TO  DET-AMOUNT-CR                ECS192
00459                                      DET-AMOUNT-DB.               ECS192
00460      IF CHARGED                                                   ECS192
00461          MOVE '-'                TO  DET-MINUS1  DET-MINUS2.      ECS192
00462                                                                   ECS192
00463      MOVE ' '                    TO  X.                           ECS192
00464      MOVE DETAIL-LINE            TO  P-DATA.                      ECS192
00465                                                                   ECS192
00466      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00467                                                                   ECS192
00468  2040-CREATE-3800-CARD.                                           ECS192
00469      MOVE SRT-ENTRY-DATE         TO  3800-DATE-2.                 ECS192
00470      MOVE SRT-AMOUNT             TO  3800-AMOUNT.                 ECS192
00471                                                                   ECS192
00472      IF RECEIVED                                                  ECS192
00473          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS192
00474          ADD 3800-AMOUNT         TO  COMP-PYMTS-CR                ECS192
00475      ELSE                                                         ECS192
00476          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS192
00477          ADD 3800-AMOUNT         TO  COMP-PYMTS-DB.               ECS192
00478                                                                   ECS192
00479      PERFORM 2050-WRITE-ABC-REC  THRU  2059-XIT.                  ECS192
00480                                                                   ECS192
00481      MOVE GL-CASH-CLEAR-NO       TO  3800-GENERAL-LEDGER.         ECS192
00482                                                                   ECS192
00483      IF RECEIVED                                                  ECS192
00484          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS192
00485      ELSE                                                         ECS192
00486          MOVE 'C'                TO  3800-DEBIT-CREDIT.           ECS192
00487                                                                   ECS192
00488      PERFORM 2050-WRITE-ABC-REC  THRU  2059-XIT.                  ECS192
00489                                                                   ECS192
00490      IF RECEIVED                                                  ECS192
00491          COMPUTE COMP-PYMTS = COMP-PYMTS + SRT-AMOUNT             ECS192
00492      ELSE                                                         ECS192
00493          COMPUTE COMP-PYMTS = COMP-PYMTS - SRT-AMOUNT.            ECS192
00494                                                                   ECS192
00495      GO TO 2010-RETURN-PYADJ-CARD.                                ECS192
00496                                                                   ECS192
00497  2050-WRITE-ABC-REC.                                              ECS192
00498      WRITE ABC-PAYMNT-REC  FROM  WS-3800-CARD.                    ECS192
00499                                                                   ECS192
00500  2059-XIT.                                                        ECS192
00501      EXIT.                                                        ECS192
00502  EJECT                                                            ECS192
00503  2100-COMP-BREAK.                                                 ECS192
00504      MOVE GL-ACCT-REC-NO         TO  DET-AR-GLNO.                 ECS192
00505      MOVE GL-CASH-CLEAR-NO       TO  DET-CC-GLNO.                 ECS192
00506      MOVE '0'                    TO  X.                           ECS192
00507      MOVE DETAIL-GL-LINE         TO  P-DATA.                      ECS192
00508                                                                   ECS192
00509      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00510                                                                   ECS192
00511      MOVE SPACES                 TO  DETAIL-LINE.                 ECS192
00512      MOVE PREV-CARR              TO  DET-CARR.                    ECS192
00513      MOVE PREV-COMP              TO  DET-COMP.                    ECS192
00514      MOVE COMP-PYMTS             TO  DET-AMOUNT-CR                ECS192
00515                                      DET-AMOUNT-DB.               ECS192
00516      MOVE ' '                    TO  X.                           ECS192
00517      MOVE DETAIL-LINE            TO  P-DATA.                      ECS192
00518                                                                   ECS192
00519      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00520                                                                   ECS192
00521      MOVE SPACES                 TO  DETAIL-LINE.                 ECS192
00522      MOVE '     TOTAL CREDITS'   TO  DET-DESC.                    ECS192
00523      MOVE COMP-PYMTS-CR          TO  DET-AMOUNT-CR.               ECS192
00524      MOVE COMP-PYMTS-DB          TO  DET-AMOUNT-DB.               ECS192
00525      MOVE '0'                    TO  X.                           ECS192
00526      MOVE DETAIL-LINE            TO  P-DATA.                      ECS192
00527                                                                   ECS192
00528      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00529                                                                   ECS192
00530      MOVE SPACES                 TO  DETAIL-LINE.                 ECS192
00531      MOVE '     TOTAL DEBITS '   TO  DET-DESC.                    ECS192
00532      MOVE COMP-PYMTS-DB          TO  DET-AMOUNT-CR.               ECS192
00533      MOVE COMP-PYMTS-CR          TO  DET-AMOUNT-DB.               ECS192
00534      MOVE ' '                    TO  X.                           ECS192
00535      MOVE DETAIL-LINE            TO  P-DATA.                      ECS192
00536                                                                   ECS192
00537      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00538                                                                   ECS192
00539      MOVE SRT-COMPANY            TO  PREV-COMP  SEARCH-COMP.      ECS192
00540                                                                   ECS192
00541      COMPUTE CARR-PYMTS = CARR-PYMTS + COMP-PYMTS.                ECS192
00542      COMPUTE CARR-PYMTS-DB = CARR-PYMTS-DB + COMP-PYMTS-DB.       ECS192
00543      COMPUTE CARR-PYMTS-CR = CARR-PYMTS-CR + COMP-PYMTS-CR.       ECS192
00544                                                                   ECS192
00545      MOVE COMP-ZEROS             TO  COMPANY-TOTALS.              ECS192
00546                                                                   ECS192
00547  2199-XIT.                                                        ECS192
00548      EXIT.                                                        ECS192
00549                                                                   ECS192
00550  2200-CARR-BREAK.                                                 ECS192
00551      MOVE SPACES                 TO  DETAIL-LINE.                 ECS192
00552      MOVE PREV-CARR              TO  DET-CARR.                    ECS192
00553      MOVE CARR-PYMTS             TO  DET-AMOUNT-CR                ECS192
00554                                      DET-AMOUNT-DB.               ECS192
00555      MOVE '-'                    TO  X.                           ECS192
00556      MOVE DETAIL-LINE            TO  P-DATA.                      ECS192
00557                                                                   ECS192
00558      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00559                                                                   ECS192
00560      MOVE SPACES                 TO  DETAIL-LINE.                 ECS192
00561      MOVE '     TOTAL CREDITS'   TO  DET-DESC.                    ECS192
00562      MOVE CARR-PYMTS-CR          TO  DET-AMOUNT-CR.               ECS192
00563      MOVE CARR-PYMTS-DB          TO  DET-AMOUNT-DB.               ECS192
00564      MOVE '0'                    TO  X.                           ECS192
00565      MOVE DETAIL-LINE            TO  P-DATA.                      ECS192
00566                                                                   ECS192
00567      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00568                                                                   ECS192
00569      MOVE SPACES                 TO  DETAIL-LINE.                 ECS192
00570      MOVE '     TOTAL DEBITS '   TO  DET-DESC.                    ECS192
00571      MOVE CARR-PYMTS-DB          TO  DET-AMOUNT-CR.               ECS192
00572      MOVE CARR-PYMTS-CR          TO  DET-AMOUNT-DB.               ECS192
00573      MOVE ' '                    TO  X.                           ECS192
00574      MOVE DETAIL-LINE            TO  P-DATA.                      ECS192
00575                                                                   ECS192
00576      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00577                                                                   ECS192
00578      MOVE SRT-CARRIER            TO  PREV-CARR  SEARCH-CARR.      ECS192
00579                                                                   ECS192
00580      COMPUTE GRND-PYMTS = GRND-PYMTS + CARR-PYMTS.                ECS192
00581      COMPUTE GRND-PYMTS-DB = GRND-PYMTS-DB + CARR-PYMTS-DB.       ECS192
00582      COMPUTE GRND-PYMTS-CR = GRND-PYMTS-CR + CARR-PYMTS-CR.       ECS192
00583                                                                   ECS192
00584      MOVE COMP-ZEROS             TO  CARRIER-TOTALS.              ECS192
00585                                                                   ECS192
00586  2299-XIT.                                                        ECS192
00587      EXIT.                                                        ECS192
00588                                                                   ECS192
00589  2300-GRAND-TOTALS.                                               ECS192
00590      PERFORM 3100-HEADING-ROUTINE  THRU  3199-XIT.                ECS192
00591                                                                   ECS192
00592      MOVE SPACES                 TO  DETAIL-LINE.                 ECS192
00593      MOVE 'GRAND TOTALS      '   TO  DET-DESC.                    ECS192
00594      MOVE GRND-PYMTS             TO  DET-AMOUNT-CR                ECS192
00595                                      DET-AMOUNT-DB.               ECS192
00596      MOVE '-'                    TO  X.                           ECS192
00597      MOVE DETAIL-LINE            TO  P-DATA.                      ECS192
00598                                                                   ECS192
00599      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00600                                                                   ECS192
00601      MOVE SPACES                 TO  DETAIL-LINE.                 ECS192
00602      MOVE '     TOTAL CREDITS'   TO  DET-DESC.                    ECS192
00603      MOVE GRND-PYMTS-CR          TO  DET-AMOUNT-CR.               ECS192
00604      MOVE GRND-PYMTS-DB          TO  DET-AMOUNT-DB.               ECS192
00605      MOVE '0'                    TO  X.                           ECS192
00606      MOVE DETAIL-LINE            TO  P-DATA.                      ECS192
00607                                                                   ECS192
00608      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00609                                                                   ECS192
00610      MOVE SPACES                 TO  DETAIL-LINE.                 ECS192
00611      MOVE '     TOTAL DEBITS '   TO  DET-DESC.                    ECS192
00612      MOVE GRND-PYMTS-DB          TO  DET-AMOUNT-CR.               ECS192
00613      MOVE GRND-PYMTS-CR          TO  DET-AMOUNT-DB.               ECS192
00614      MOVE ' '                    TO  X.                           ECS192
00615      MOVE DETAIL-LINE            TO  P-DATA.                      ECS192
00616                                                                   ECS192
00617      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00618                                                                   ECS192
00619  2399-XIT.                                                        ECS192
00620      EXIT.                                                        ECS192
00621                                                                   ECS192
00622  9999-END-OF-JOB.                                                 ECS192
00623      EXIT.                                                        ECS192
00624  EJECT                                                            ECS192
00625  PERFORMED-PROCEDURES SECTION.                                    ECS192
00626                                                                   ECS192
00627  3000-LEDGER-TABLE-LOOKUP.                                        ECS192
00628      IF DTE-CLIENT = 'ADL'  OR  'FLB'  OR  'ALA'                  ECS192
00629          MOVE 'A'                TO  SEARCH-CARR.                 ECS192
00630                                                                   ECS192
00631      SET L                       TO  1.                           ECS192
00632                                                                   ECS192
00633      SEARCH LEDGER-NUMBERS  VARYING  L  AT END                    ECS192
00634          GO TO 3090-LOOKUP-DEFAULT                                ECS192
00635          WHEN SEARCH-CARR-CO = LEDGER-CARR-CO (L)                 ECS192
00636              MOVE LEDGER-ACCT-NUMBERS (L)                         ECS192
00637                                  TO  GENERAL-LEDGER-NUMBERS       ECS192
00638              GO TO 3099-XIT.                                      ECS192
00639                                                                   ECS192
00640  3090-LOOKUP-DEFAULT.                                             ECS192
00641      IF SEARCH-COMP = '001'                                       ECS192
00642          MOVE LEDGER-ACCT-NUMBERS (1)                             ECS192
00643                                  TO  GENERAL-LEDGER-NUMBERS.      ECS192
00644                                                                   ECS192
00645      IF SEARCH-COMP = '002'                                       ECS192
00646          MOVE LEDGER-ACCT-NUMBERS (2)                             ECS192
00647                                  TO  GENERAL-LEDGER-NUMBERS.      ECS192
00648                                                                   ECS192
00649      IF SEARCH-COMP = '003'                                       ECS192
00650          MOVE LEDGER-ACCT-NUMBERS (3)                             ECS192
00651                                  TO  GENERAL-LEDGER-NUMBERS.      ECS192
00652                                                                   ECS192
00653  3099-XIT.                                                        ECS192
00654      EXIT.                                                        ECS192
00655                                                                   ECS192
00656  3100-HEADING-ROUTINE.                                            ECS192
00657      MOVE ZEROS                  TO  LINE-CNT.                    ECS192
00658                                                                   ECS192
00659      ADD 1                       TO  PAGE-CNT.                    ECS192
00660                                                                   ECS192
00661      MOVE '1'                    TO  X.                           ECS192
00662      MOVE PAGE-CNT               TO  HD-PAGE.                     ECS192
00663      MOVE HEAD-LINE-1            TO  P-DATA.                      ECS192
00664      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00665                                                                   ECS192
00666      MOVE ' '                    TO  X.                           ECS192
00667      MOVE HEAD-LINE-2            TO  P-DATA.                      ECS192
00668      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00669                                                                   ECS192
00670      MOVE HEAD-LINE-3            TO  P-DATA.                      ECS192
00671      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00672                                                                   ECS192
00673      MOVE '-'                    TO  X.                           ECS192
00674      MOVE HEAD-LINE-4            TO  P-DATA.                      ECS192
00675      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00676                                                                   ECS192
00677      MOVE ' '                    TO  X.                           ECS192
00678      MOVE HEAD-LINE-5            TO  P-DATA.                      ECS192
00679      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00680                                                                   ECS192
00681      MOVE '0'                    TO  X.                           ECS192
00682      MOVE SPACES                 TO  P-DATA.                      ECS192
00683      PERFORM 3200-PRINT-ROUTINE  THRU  3299-XIT.                  ECS192
00684                                                                   ECS192
00685  3199-XIT.                                                        ECS192
00686      EXIT.                                                        ECS192
00687                                                                   ECS192
00688  3200-PRINT-ROUTINE.                                              ECS192
00689                              COPY ELCPRT2.                           CL**6
00690                                                                   ECS192
00691      IF X = ' '                                                   ECS192
00692          ADD +1                  TO  LINE-CNT                     ECS192
00693      ELSE                                                         ECS192
00694          IF X = '0'                                               ECS192
00695              ADD +2              TO  LINE-CNT                     ECS192
00696          ELSE                                                     ECS192
00697              IF X = '-'                                           ECS192
00698                  ADD +3          TO  LINE-CNT                     ECS192
00699              ELSE                                                 ECS192
00700                  MOVE +0         TO  LINE-CNT.                    ECS192
00701                                                                   ECS192
00702      IF LINE-CNT GREATER +52                                      ECS192
00703          PERFORM 3100-HEADING-ROUTINE  THRU  3199-XIT.            ECS192
00704                                                                   ECS192
00705  3299-XIT.                                                        ECS192
00706      EXIT.                                                        ECS192
00707                                                                   ECS192
00708  ABEND-PGM SECTION.                                               ECS192
00709                              COPY ELCABEND.                       ECS192
00710 /                                                                 ECS192
00711  LCP-WRITE-POS-PRT SECTION.                                       ECS192
00712      IF LCP-ASA = '+'                                             ECS192
00713          WRITE PRT AFTER 0 LINE                                   ECS192
00714      ELSE                                                         ECS192
00715      IF LCP-ASA = ' '                                             ECS192
00716          WRITE PRT AFTER ADVANCING 1 LINE                         ECS192
00717      ELSE                                                         ECS192
00718      IF LCP-ASA = '0'                                             ECS192
00719          WRITE PRT AFTER ADVANCING 2 LINE                         ECS192
00720      ELSE                                                         ECS192
00721      IF LCP-ASA = '-'                                             ECS192
00722          WRITE PRT AFTER ADVANCING 3 LINE                         ECS192
00723      ELSE                                                         ECS192
00724      IF LCP-ASA = '1'                                             ECS192
00725          WRITE PRT AFTER ADVANCING PAGE                           ECS192
00726      ELSE                                                         ECS192
00727      IF LCP-ASA = '2'                                             ECS192
00728          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS192
00729      ELSE                                                         ECS192
00730      IF LCP-ASA = '3'                                             ECS192
00731          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS192
00732      ELSE                                                         ECS192
00733      IF LCP-ASA = '4'                                             ECS192
00734          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS192
00735      ELSE                                                         ECS192
00736      IF LCP-ASA = '5'                                             ECS192
00737          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS192
00738      ELSE                                                         ECS192
00739      IF LCP-ASA = '6'                                             ECS192
00740          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS192
00741      ELSE                                                         ECS192
00742      IF LCP-ASA = '7'                                             ECS192
00743          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS192
00744      ELSE                                                         ECS192
00745      IF LCP-ASA = '8'                                             ECS192
00746          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS192
00747      ELSE                                                         ECS192
00748      IF LCP-ASA = '9'                                             ECS192
00749          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS192
00750      ELSE                                                         ECS192
00751      IF LCP-ASA = 'A'                                             ECS192
00752          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS192
00753      ELSE                                                         ECS192
00754      IF LCP-ASA = 'B'                                             ECS192
00755          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS192
00756      ELSE                                                         ECS192
00757      IF LCP-ASA = 'C'                                             ECS192
00758          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS192
00759      ELSE                                                         ECS192
00760      IF LCP-ASA = 'V'                                             ECS192
00761          WRITE PRT AFTER ADVANCING LCP-P01                        ECS192
00762      ELSE                                                         ECS192
00763      IF LCP-ASA = 'W'                                             ECS192
00764          WRITE PRT AFTER ADVANCING LCP-P02                        ECS192
00765      ELSE                                                         ECS192
00766      DISPLAY 'ASA CODE ERROR'.                                    ECS192
00767  LCP-WRITE-END-PRT.                                               ECS192
00768      EXIT.                                                        ECS192
