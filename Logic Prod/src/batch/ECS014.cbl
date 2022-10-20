00001  IDENTIFICATION DIVISION.                                         03/23/98
00002                                                                   ECS014
00003  PROGRAM-ID.                ECS014.                                  LV003
00004 *              PROGRAM CONVERTED BY                               ECS014
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS014
00006 *              CONVERSION DATE 11/28/95 10:55:54.                 ECS014
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS014
00008 *                           VMOD=2.005.                           ECS014
00009                                                                   ECS014
00010 *AUTHOR.        LOGIC, INC.                                       ECS014
00011 *               DALLAS, TEXAS.                                    ECS014
00012                                                                   ECS014
00013 *DATE-COMPILED.                                                   ECS014
00014                                                                   ECS014
00015 *SECURITY.   *****************************************************ECS014
00016 *            *                                                   *ECS014
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS014
00018 *            *                                                   *ECS014
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS014
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS014
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS014
00022 *            *                                                   *ECS014
00023 *            *****************************************************ECS014
00024                                                                   ECS014
00025 *REMARKS.                                                         ECS014
00026 *        PROGRAM PRINTS RECAP OF ACTIVITY CONCERNING REINSURANCE  ECS014
00027 *        IN THE CURRENT UPDATE TRANSACTIONS.                      ECS014
00028                                                                   ECS014
00029  ENVIRONMENT DIVISION.                                            ECS014
00030  CONFIGURATION SECTION.                                           ECS014
00031  SPECIAL-NAMES.                                                   ECS014
00032      C02 IS LCP-CH2                                               ECS014
00033      C03 IS LCP-CH3                                               ECS014
00034      C04 IS LCP-CH4                                               ECS014
00035      C05 IS LCP-CH5                                               ECS014
00036      C06 IS LCP-CH6                                               ECS014
00037      C07 IS LCP-CH7                                               ECS014
00038      C08 IS LCP-CH8                                               ECS014
00039      C09 IS LCP-CH9                                               ECS014
00040      C10 IS LCP-CH10                                              ECS014
00041      C11 IS LCP-CH11                                              ECS014
00042      C12 IS LCP-CH12                                              ECS014
00043      S01 IS LCP-P01                                               ECS014
00044      S02 IS LCP-P02.                                              ECS014
00045  INPUT-OUTPUT SECTION.                                            ECS014
00046  FILE-CONTROL.                                                    ECS014
00047                                                                   ECS014
00048      SELECT SRT-EXT          ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  ECS014
00049      SELECT PRNT-OUT         ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS014
00050      SELECT RTBL-FILE        ASSIGN TO SYS014                     ECS014
00051                              ACCESS IS RANDOM                     ECS014
00052                              ORGANIZATION IS INDEXED              ECS014
00053                              FILE STATUS IS REIN-FILE-STATUS      ECS014
00054                              RECORD KEY IS RE-CONTROL-PRIMARY.    ECS014
00055      SELECT CUR-REIN         ASSIGN TO SYS018-UT-2400-S-SYS018.   ECS014
00056      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS014
00057      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS014
00058                                                                   ECS014
00059  EJECT                                                            ECS014
00060  DATA DIVISION.                                                   ECS014
00061  FILE SECTION.                                                    ECS014
00062                                                                   ECS014
00063  SD  SRT-EXT.                                                     ECS014
00064                                                                   ECS014
00065  01  S-REC.                                                       ECS014
00066      12  FILLER              PIC X(4).                            ECS014
00067      12  S-CTL               PIC X(36).                           ECS014
00068      12  FILLER              PIC X(470).                          ECS014
00069      12  S-TYPE              PIC X.                               ECS014
00070      12  S-FROM              PIC X(30).                           ECS014
00071      12  S-TO                PIC X(30).                           ECS014
00072                                                                   ECS014
00073  EJECT                                                            ECS014
00074  FD  PRNT-OUT                                                     ECS014
00075                              COPY ELCPRTFD.                       ECS014
00076                                                                   ECS014
00077  EJECT                                                            ECS014
00078  FD  RTBL-FILE                                                    ECS014
00079                              COPY ECSRTFDD.                       ECS014
00080                                                                   ECS014
00081                              COPY ERCREIN.                        ECS014
00082                                                                   ECS014
00083  EJECT                                                            ECS014
00084  FD  CUR-REIN                                                     ECS014
00085                              COPY ECSEXTFD.                       ECS014
00086                                                                   ECS014
00087  01  DE-EXTRACT-RECORD.                                           ECS014
00088      12  FILLER              PIC X(510).                          ECS014
00089                                                                   ECS014
00090  EJECT                                                            ECS014
00091  FD  DISK-DATE                                                    ECS014
00092                              COPY ELCDTEFD.                       ECS014
00093                                                                   ECS014
00094  EJECT                                                            ECS014
00095  FD  FICH                                                         ECS014
00096                              COPY ELCFCHFD.                       ECS014
00097                                                                   ECS014
00098  EJECT                                                            ECS014
00099  WORKING-STORAGE SECTION.                                         ECS014
00100  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS014
00101  77  LCP-ASA                       PIC X.                         ECS014
00102  77  FILLER  PIC X(32) VALUE '********************************'.  ECS014
00103  77  FILLER  PIC X(32) VALUE '     ECS014 WORKING STORAGE     '.  ECS014
00104  77  FILLER  PIC X(32) VALUE '******* VMOD=2.005 *************'.  ECS014
00105                                                                   ECS014
00106  77  X1                      PIC S9(5)   COMP    VALUE +0.        ECS014
00107  77  SA                      PIC S9(5)   COMP    VALUE +0.        ECS014
00108  77  PAGER                   PIC S999    COMP-3  VALUE +0.        ECS014
00109  77  K-1                     PIC S999    COMP-3  VALUE -1.        ECS014
00110  77  SELECT-COUNT            PIC S9(9)   COMP-3  VALUE +0.        ECS014
00111  77  L-C                     PIC 99              VALUE 99.        ECS014
00112  77  L-ST                    PIC XX.                              ECS014
00113  77  X                       PIC X.                               ECS014
00114  77  F-SW                    PIC X               VALUE '1'.       ECS014
00115  77  LAS-ACCT                PIC X(6)            VALUE SPACES.    ECS014
00116                                                                   ECS014
00117  01  WS-ABEND-FIELDS.                                             ECS014
00118      12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      ECS014
00119      12  WS-ZERO                 PIC S9          VALUE ZERO.      ECS014
00120      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    ECS014
00121      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      ECS014
00122                                                                   ECS014
00123  01  REIN-FILE-STATUS.                                            ECS014
00124      12  REIN-FILE-STATUS-1      PIC X.                           ECS014
00125      12  REIN-FILE-STATUS-2      PIC X.                           ECS014
00126                                                                   ECS014
00127  01  HEAD-A  SYNC.                                                ECS014
00128      12  FILLER              PIC X(48)           VALUE SPACES.    ECS014
00129      12  FILLER              PIC X(24)           VALUE            ECS014
00130              'REINSURANCE ACTIVITY REC'.                          ECS014
00131      12  FILLER              PIC XXX             VALUE 'AP '.     ECS014
00132      12  FILLER              PIC X(45)           VALUE SPACES.    ECS014
00133      12  FILLER              PIC X(8)            VALUE 'ECS014'.  ECS014
00134                                                                   ECS014
00135  01  HEAD-B  SYNC.                                                ECS014
00136      12  FILLER              PIC X(47)           VALUE SPACES.    ECS014
00137      12  HA-1                PIC X(30).                           ECS014
00138      12  FILLER              PIC X(43)           VALUE SPACES.    ECS014
00139      12  HD-RD               PIC X(8).                            ECS014
00140                                                                   ECS014
00141  01  HEAD-BA SYNC.                                                ECS014
00142      12  FILLER              PIC X(53)           VALUE SPACES.    ECS014
00143      12  HB-DATE             PIC X(18).                           ECS014
00144      12  FILLER              PIC X(41)           VALUE SPACES.    ECS014
00145      12  FILLER              PIC X(5)            VALUE 'PAGE '.   ECS014
00146      12  PGNO                PIC ZZ,ZZ9.                          ECS014
00147                                                                   ECS014
00148  01  HEAD-C SYNC.                                                 ECS014
00149      12  HC-DESC             PIC X(11)           VALUE            ECS014
00150              ' COMPANY - '.                                       ECS014
00151      12  HC-2                PIC X(30).                           ECS014
00152                                                                   ECS014
00153  01  HEAD-D SYNC.                                                 ECS014
00154      12  FILLER              PIC X(44)           VALUE            ECS014
00155              ' NET REINSURANCE                     CARRIER'.      ECS014
00156      12  FILLER              PIC X(44)           VALUE            ECS014
00157              '  GROUP    BENEFIT          BENEFIT        P'.      ECS014
00158      12  FILLER              PIC X(44)           VALUE            ECS014
00159              'REMIUM        COMMISSION     CLAIM AMOUNT   '.      ECS014
00160                                                                   ECS014
00161  01  HEAD-E SYNC.                                                 ECS014
00162      12  FILLER              PIC X(44)           VALUE SPACES.    ECS014
00163      12  FILLER              PIC X(44)           VALUE            ECS014
00164              '             KIND                           '.      ECS014
00165      12  FILLER              PIC X(44)           VALUE SPACES.    ECS014
00166                                                                   ECS014
00167  01  TOT-1.                                                       ECS014
00168      12  FILLER              PIC X.                               ECS014
00169      12  T1-FT               PIC X(4).                            ECS014
00170      12  FILLER              PIC XX.                              ECS014
00171      12  T1-TO               PIC X(30).                           ECS014
00172      12  FILLER              PIC XXX.                             ECS014
00173      12  T1-CARR             PIC X.                               ECS014
00174      12  FILLER              PIC XXXX.                            ECS014
00175      12  T1-GROUP            PIC X(6).                            ECS014
00176      12  FILLER              PIC X(5).                            ECS014
00177      12  T1-AREA-1.                                               ECS014
00178          16  T1-LKIND        PIC X(6).                            ECS014
00179          16  FILLER          PIC X(3).                            ECS014
00180          16  T1-LBEN         PIC ZZ,ZZZ,ZZZ,ZZZ.99-.              ECS014
00181          16  T1-LPRM         PIC ZZZ,ZZZ,ZZZ.99-.                 ECS014
00182          16  T1-LCOM         PIC ZZZ,ZZZ,ZZZ.99-.                 ECS014
00183          16  T1-LCLAIMS      PIC ZZ,ZZZ,ZZZ,ZZZ.99-.              ECS014
00184                                                                   ECS014
00185  01  TOT-2.                                                       ECS014
00186      12  FILLER              PIC X(56).                           ECS014
00187      12  T2-AREA-1.                                               ECS014
00188          16  T2-AKIND        PIC X(6).                            ECS014
00189          16  FILLER          PIC X(3).                            ECS014
00190          16  T2-ABEN         PIC ZZ,ZZZ,ZZZ,ZZZ.99-.              ECS014
00191          16  T2-APRM         PIC ZZZ,ZZZ,ZZZ.99-.                 ECS014
00192          16  T2-ACOM         PIC ZZZ,ZZZ,ZZZ.99-.                 ECS014
00193          16  T2-ACLAIMS      PIC ZZ,ZZZ,ZZZ,ZZZ.99-.              ECS014
00194                                                                   ECS014
00195  01  ACCUM-WORK-AREAS    COMP-3  SYNC.                            ECS014
00196      12  T-LPREM             PIC S9(09)V99        VALUE +0.       ECS014
00197      12  G-LPREM             PIC S9(09)V99        VALUE +0.       ECS014
00198      12  T-APREM             PIC S9(09)V99        VALUE +0.       ECS014
00199      12  G-APREM             PIC S9(09)V99        VALUE +0.       ECS014
00200      12  T-LBEN              PIC S9(11)V99        VALUE +0.       ECS014
00201      12  G-LBEN              PIC S9(11)V99        VALUE +0.       ECS014
00202      12  T-ABEN              PIC S9(11)V99        VALUE +0.       ECS014
00203      12  G-ABEN              PIC S9(11)V99        VALUE +0.       ECS014
00204      12  T-CANCL             PIC S9(09)V99        VALUE +0.       ECS014
00205      12  T-CANCA             PIC S9(09)V99        VALUE +0.       ECS014
00206      12  G-CANCL             PIC S9(09)V99        VALUE +0.       ECS014
00207      12  G-CANCA             PIC S9(09)V99        VALUE +0.       ECS014
00208      12  T-DEATH             PIC S9(11)V99        VALUE +0.       ECS014
00209      12  G-DEATH             PIC S9(11)V99        VALUE +0.       ECS014
00210      12  T-DISAB             PIC S9(09)V99        VALUE +0.       ECS014
00211      12  G-DISAB             PIC S9(09)V99        VALUE +0.       ECS014
00212      12  T-LCOM              PIC S9(07)V99        VALUE +0.       ECS014
00213      12  G-LCOM              PIC S9(07)V99        VALUE +0.       ECS014
00214      12  T-ACOM              PIC S9(07)V99        VALUE +0.       ECS014
00215      12  G-ACOM              PIC S9(07)V99        VALUE +0.       ECS014
00216                                                                   ECS014
00217      12  C-LPREM             PIC S9(09)V99        VALUE +0.       ECS014
00218      12  F-LPREM             PIC S9(09)V99        VALUE +0.       ECS014
00219      12  C-APREM             PIC S9(09)V99        VALUE +0.       ECS014
00220      12  F-APREM             PIC S9(09)V99        VALUE +0.       ECS014
00221      12  C-LBEN              PIC S9(11)V99        VALUE +0.       ECS014
00222      12  F-LBEN              PIC S9(11)V99        VALUE +0.       ECS014
00223      12  C-ABEN              PIC S9(09)V99        VALUE +0.       ECS014
00224      12  F-ABEN              PIC S9(09)V99        VALUE +0.       ECS014
00225      12  C-CANCL             PIC S9(09)V99        VALUE +0.       ECS014
00226      12  C-CANCA             PIC S9(09)V99        VALUE +0.       ECS014
00227      12  F-CANCL             PIC S9(09)V99        VALUE +0.       ECS014
00228      12  F-CANCA             PIC S9(09)V99        VALUE +0.       ECS014
00229      12  C-DEATH             PIC S9(11)V99        VALUE +0.       ECS014
00230      12  F-DEATH             PIC S9(11)V99        VALUE +0.       ECS014
00231      12  C-DISAB             PIC S9(09)V99        VALUE +0.       ECS014
00232      12  F-DISAB             PIC S9(09)V99        VALUE +0.       ECS014
00233      12  C-LCOM              PIC S9(09)V99        VALUE +0.       ECS014
00234      12  F-LCOM              PIC S9(07)V99        VALUE +0.       ECS014
00235      12  C-ACOM              PIC S9(07)V99        VALUE +0.       ECS014
00236      12  F-ACOM              PIC S9(07)V99        VALUE +0.       ECS014
00237                                                                   ECS014
00238  01  ACCOUNT-ACCUMS      COMP-3.                                  ECS014
00239      12  A-LBEN              PIC S9(11)V99.                       ECS014
00240      12  A-ABEN              PIC S9(11)V99.                       ECS014
00241      12  A-LPREM             PIC S9(09)V99.                       ECS014
00242      12  A-APREM             PIC S9(09)V99.                       ECS014
00243      12  A-CANCL             PIC S9(09)V99.                       ECS014
00244      12  A-CANCA             PIC S9(09)V99.                       ECS014
00245      12  A-DEATH             PIC S9(11)V99.                       ECS014
00246      12  A-DISAB             PIC S9(09)V99.                       ECS014
00247      12  A-LCOM              PIC S9(09)V99.                       ECS014
00248      12  A-ACOM              PIC S9(09)V99.                       ECS014
00249                                                                   ECS014
00250  01  MISC-WORK-AREAS.                                             ECS014
00251      12  L-CO.                                                    ECS014
00252          16  L-CARR          PIC X.                               ECS014
00253          16  L-GRP           PIC X(6).                            ECS014
00254      12  L-CARRIER           PIC X.                               ECS014
00255      12  L-FROM              PIC X(30).                           ECS014
00256      12  L-TO                PIC X(30).                           ECS014
00257      12  L-TYPE              PIC X.                               ECS014
00258      12  W-ST.                                                    ECS014
00259          16  W-S             PIC 99.                              ECS014
00260      12  WS-AGT              PIC X(10).                           ECS014
00261      12  REIN-SEARCH.                                             ECS014
00262          16  REIN-CODE       PIC X               VALUE 'B'.       ECS014
00263          16  REIN-SRCH       PIC X(6).                            ECS014
00264      12  LAST-REIN-SEARCH    PIC X(7)            VALUE LOW-VALUE. ECS014
00265      12  REIN-NAME           PIC X(30).                           ECS014
00266      12  CEDE-NAME           PIC X(30).                           ECS014
00267                                                                   ECS014
00268  01  MISC.                                                        ECS014
00269      05  PGM-SUB             PIC S999    COMP    VALUE +014.      ECS014
00270  EJECT                                                            ECS014
00271                              COPY ECSEXT01.                       ECS014
00272  EJECT                                                            ECS014
00273                              COPY ELCDTECX.                       ECS014
00274  EJECT                                                            ECS014
00275                              COPY ELCDTEVR.                       ECS014
00276  EJECT                                                            ECS014
00277  PROCEDURE DIVISION.                                              ECS014
00278                                                                   ECS014
00279  0100-INITIALIZE-RTN SECTION.                                     ECS014
00280                                                                   ECS014
00281  0110-STANDARD-RTN.                                               ECS014
00282                              COPY ELCDTERX.                       ECS014
00283                                                                   ECS014
00284      MOVE ALPH-DATE              TO HB-DATE.                      ECS014
00285      MOVE COMPANY-NAME           TO HA-1.                         ECS014
00286      MOVE WS-CURRENT-DATE        TO HD-RD.                        ECS014
00287      MOVE DTE-CLASIC-COMPANY-CD  TO RE-COMPANY-CD.                ECS014
00288  EJECT                                                            ECS014
00289  0130-SORT-REINSURANCE SECTION.                                   ECS014
00290                                                                   ECS014
00291      OPEN INPUT CUR-REIN                                          ECS014
00292                 RTBL-FILE.                                        ECS014
00293                                                                   ECS014
00294      IF REIN-FILE-STATUS  = '35'                                  ECS014
00295          CLOSE CUR-REIN                                           ECS014
00296          GO TO 0690-E-O-J.                                        ECS014
00297                                                                   ECS014
00298      IF REIN-FILE-STATUS  = '00' OR '97'                          ECS014
00299          NEXT SENTENCE                                            ECS014
00300        ELSE                                                       ECS014
00301          MOVE 'OPEN ERROR - REIN '   TO WS-ABEND-MESSAGE          ECS014
00302          MOVE REIN-FILE-STATUS   TO WS-ABEND-FILE-STATUS          ECS014
00303          GO TO ABEND-PGM.                                         ECS014
00304                                                                   ECS014
00305      SORT SRT-EXT ON ASCENDING KEY S-FROM                         ECS014
00306                                    S-TO                           ECS014
00307                                    S-TYPE                         ECS014
00308                                    S-CTL                          ECS014
00309          INPUT PROCEDURE IS 0160-SELECT-REINS THRU                ECS014
00310              0200-SET-SELECT-XIT                                  ECS014
00311          OUTPUT PROCEDURE IS 0220-PRINT-REINS THRU                ECS014
00312              0660-EXIT-REINS.                                     ECS014
00313                                                                   ECS014
00314      IF SORT-RETURN GREATER THAN 4                                ECS014
00315          MOVE 'SORT ERROR '    TO WS-ABEND-MESSAGE                ECS014
00316          MOVE 0101             TO WS-RETURN-CODE                  ECS014
00317          GO TO ABEND-PGM.                                         ECS014
00318                                                                   ECS014
00319      GO TO 0690-E-O-J.                                            ECS014
00320                                                                   ECS014
00321  EJECT                                                            ECS014
00322  0160-SELECT-REINS SECTION.                                       ECS014
00323                                                                   ECS014
00324  0180-ML1.                                                        ECS014
00325      READ CUR-REIN  INTO  DETAIL-EXTRACT                          ECS014
00326               AT END GO TO 0190-END-SELECT.                       ECS014
00327                                                                   ECS014
00328      IF DE-RECORD-ID NOT = 'DE'                                   ECS014
00329          GO TO 0180-ML1.                                          ECS014
00330                                                                   ECS014
00331      IF DE-REIN  NOT = 'R'                                        ECS014
00332          GO TO 0180-ML1.                                          ECS014
00333                                                                   ECS014
00334      IF NOT DE-ISSUE                                              ECS014
00335         AND NOT DE-CANCEL                                         ECS014
00336         AND NOT DE-CLAIM                                          ECS014
00337         AND NOT DE-RC-ISSUE                                       ECS014
00338         AND NOT DE-RC-CANCEL                                      ECS014
00339         AND NOT DE-RR-RC-ISS                                      ECS014
00340         AND NOT DE-RR-RC-CNC                                      ECS014
00341         AND NOT DE-RR-RC-CLM                                      ECS014
00342          GO TO  0180-ML1.                                         ECS014
00343                                                                   ECS014
00344      IF (DE-RC-ISSUE OR DE-RC-CANCEL)                             ECS014
00345         AND DTE-CLIENT = 'FLA'                                    ECS014
00346          GO TO 0180-ML1.                                          ECS014
00347                                                                   ECS014
00348      IF DE-REI-LFAMT NOT NUMERIC                                  ECS014
00349          MOVE ZEROS              TO DE-REI-LFAMT.                 ECS014
00350                                                                   ECS014
00351      IF DE-REI-LFPRM NOT NUMERIC                                  ECS014
00352          MOVE ZEROS              TO DE-REI-LFPRM.                 ECS014
00353                                                                   ECS014
00354      IF DE-REI-LFRFND NOT NUMERIC                                 ECS014
00355          MOVE ZEROS              TO DE-REI-LFRFND.                ECS014
00356                                                                   ECS014
00357      IF DE-REI-AHAMT NOT NUMERIC                                  ECS014
00358          MOVE ZEROS              TO DE-REI-AHAMT.                 ECS014
00359                                                                   ECS014
00360      IF DE-REI-AHPRM NOT NUMERIC                                  ECS014
00361          MOVE ZEROS              TO DE-REI-AHPRM.                 ECS014
00362                                                                   ECS014
00363      IF DE-REI-AHRFND NOT NUMERIC                                 ECS014
00364          MOVE ZEROS              TO DE-REI-AHRFND.                ECS014
00365                                                                   ECS014
00366      IF DE-REI-CNAMT NOT NUMERIC                                  ECS014
00367          MOVE ZEROS              TO DE-REI-CNAMT.                 ECS014
00368                                                                   ECS014
00369      IF DE-CLAIM  OR  DE-RR-RC-CLM                                ECS014
00370          IF DE-REI-CLAIM-AMT NOT NUMERIC                          ECS014
00371              MOVE ZEROS          TO DE-REI-CLAIM-AMT.             ECS014
00372                                                                   ECS014
00373      MOVE DE-REI-COMP            TO DE-GROUPING.                  ECS014
00374      MOVE DETAIL-EXTRACT         TO S-REC.                        ECS014
00375      MOVE DE-GROUPING            TO REIN-SRCH.                    ECS014
00376      MOVE REIN-SEARCH            TO RE-KEY.                       ECS014
00377      PERFORM 0500-READ-REIN-TBL THRU 0510-READ-REIN-TBL-X.        ECS014
00378                                                                   ECS014
00379      MOVE REIN-NAME              TO S-FROM.                       ECS014
00380      MOVE CEDE-NAME              TO S-TO.                         ECS014
00381      MOVE 'T'                    TO S-TYPE.                       ECS014
00382      RELEASE S-REC.                                               ECS014
00383      ADD +1 TO SELECT-COUNT.                                      ECS014
00384                                                                   ECS014
00385      COMPUTE DE-REI-CNAMT = DE-REI-CNAMT * -1.                    ECS014
00386      COMPUTE DE-REI-LFAMT = DE-REI-LFAMT * -1.                    ECS014
00387      COMPUTE DE-REI-LFRFND = DE-REI-LFRFND * -1.                  ECS014
00388      COMPUTE DE-REI-LFPRM = DE-REI-LFPRM * -1.                    ECS014
00389      COMPUTE DE-REI-AHAMT = DE-REI-AHAMT * -1.                    ECS014
00390      COMPUTE DE-REI-AHRFND = DE-REI-AHRFND * -1.                  ECS014
00391      COMPUTE DE-REI-AHPRM = DE-REI-AHPRM * -1.                    ECS014
00392                                                                   ECS014
00393      IF DE-CLAIM  OR  DE-RR-RC-CLM                                ECS014
00394          COMPUTE DE-REI-CLAIM-AMT = DE-REI-CLAIM-AMT * -1.        ECS014
00395                                                                   ECS014
00396      MOVE DETAIL-EXTRACT         TO S-REC.                        ECS014
00397      MOVE REIN-NAME              TO S-TO.                         ECS014
00398      MOVE CEDE-NAME              TO S-FROM.                       ECS014
00399      MOVE 'F'                    TO S-TYPE.                       ECS014
00400      RELEASE S-REC.                                               ECS014
00401      ADD +1 TO SELECT-COUNT.                                      ECS014
00402                                                                   ECS014
00403      GO TO 0180-ML1.                                              ECS014
00404                                                                   ECS014
00405  0190-END-SELECT.                                                 ECS014
00406      CLOSE CUR-REIN.                                              ECS014
00407                                                                   ECS014
00408  0200-SET-SELECT-XIT.                                             ECS014
00409      EXIT.                                                        ECS014
00410  EJECT                                                            ECS014
00411  0220-PRINT-REINS SECTION.                                        ECS014
00412                                                                   ECS014
00413      OPEN OUTPUT PRNT-OUT.                                        ECS014
00414                                                                   ECS014
00415      IF SELECT-COUNT = +0                                         ECS014
00416          GO TO 0630-NO-INPUT-RTN.                                 ECS014
00417                                                                   ECS014
00418  0230-R-LOOP.                                                     ECS014
00419                                                                   ECS014
00420      RETURN SRT-EXT INTO DETAIL-EXTRACT AT END                    ECS014
00421          GO TO 0640-END-PRINT.                                    ECS014
00422                                                                   ECS014
00423      IF F-SW = '1'                                                ECS014
00424          PERFORM 0550-T-O-X                                       ECS014
00425          MOVE 99                 TO L-C.                          ECS014
00426                                                                   ECS014
00427      IF S-FROM NOT = L-FROM OR                                    ECS014
00428         S-TO NOT = L-TO OR                                        ECS014
00429         DE-CARRIER NOT = L-CARR OR                                ECS014
00430         DE-GROUPING NOT = L-GRP OR                                ECS014
00431         DE-CARRIER NOT = L-CARRIER                                ECS014
00432          GO TO 0520-TOTS-OUT.                                     ECS014
00433                                                                   ECS014
00434  0240-SET-DATAL.                                                  ECS014
00435                                                                   ECS014
00436      IF DE-CANCEL                                                 ECS014
00437         OR DE-RC-CANCEL                                           ECS014
00438         OR DE-RR-RC-CNC                                           ECS014
00439          COMPUTE DE-REI-CNAMT = DE-REI-CNAMT * -1                 ECS014
00440          COMPUTE DE-REI-LFRFND = DE-REI-LFRFND * -1               ECS014
00441          COMPUTE DE-REI-AHAMT = DE-REI-AHAMT * -1                 ECS014
00442          COMPUTE DE-REI-AHRFND = DE-REI-AHRFND * -1.              ECS014
00443                                                                   ECS014
00444      IF DE-ISSUE                                                  ECS014
00445         OR DE-RC-ISSUE                                            ECS014
00446         OR DE-RR-RC-ISS                                           ECS014
00447          PERFORM 0290-ADD-ISSUE THRU 0320-ADD-ISSUE-X.            ECS014
00448                                                                   ECS014
00449      IF DE-ISSUE                                                  ECS014
00450         OR DE-RR-RC-ISS                                           ECS014
00451          ADD DE-REI-LFAMT TO T-LBEN  A-LBEN                       ECS014
00452          ADD DE-REI-AHAMT TO T-ABEN  A-ABEN                       ECS014
00453          ADD DE-REI-LFPRM TO T-LPREM  A-LPREM                     ECS014
00454          ADD DE-REI-AHPRM TO T-APREM  A-APREM.                    ECS014
00455                                                                   ECS014
00456      IF DE-CANCEL                                                 ECS014
00457         OR DE-RC-CANCEL                                           ECS014
00458         OR DE-RR-RC-CNC                                           ECS014
00459          PERFORM 0330-ADD-CANCEL THRU 0360-ADD-CANCEL-X.          ECS014
00460                                                                   ECS014
00461      IF DE-CANCEL                                                 ECS014
00462         OR DE-RR-RC-CNC                                           ECS014
00463          ADD DE-REI-LFRFND TO T-CANCL  A-CANCL                    ECS014
00464          ADD DE-REI-AHRFND TO T-CANCA  A-CANCA                    ECS014
00465          ADD DE-REI-CNAMT TO T-LBEN  A-LBEN                       ECS014
00466          ADD DE-REI-AHAMT TO T-ABEN  A-ABEN.                      ECS014
00467                                                                   ECS014
00468      IF DE-ISSUE                                                  ECS014
00469          GO TO 0280-WRITE-OUT.                                    ECS014
00470                                                                   ECS014
00471      IF DE-CANCEL                                                 ECS014
00472          GO TO 0280-WRITE-OUT.                                    ECS014
00473                                                                   ECS014
00474      IF DE-RC-ISSUE                                               ECS014
00475         OR DE-RC-CANCEL                                           ECS014
00476          GO TO 0280-WRITE-OUT.                                    ECS014
00477                                                                   ECS014
00478      IF DE-RR-RC-ISS                                              ECS014
00479          GO TO 0280-WRITE-OUT.                                    ECS014
00480                                                                   ECS014
00481      IF DE-RR-RC-CNC                                              ECS014
00482          GO TO 0280-WRITE-OUT.                                    ECS014
00483                                                                   ECS014
00484      IF DE-CLAIM                                                  ECS014
00485          IF DE-DTH  OR  DE-OB-DTH                                 ECS014
00486              ADD DE-REI-CLAIM-AMT TO T-DEATH  A-DEATH             ECS014
00487          ELSE                                                     ECS014
00488              ADD DE-REI-CLAIM-AMT TO T-DISAB  A-DISAB.            ECS014
00489                                                                   ECS014
00490      IF DE-RR-RC-CLM                                              ECS014
00491          IF DE-DTH  OR  DE-OB-DTH                                 ECS014
00492              ADD DE-REI-CLAIM-AMT TO T-DEATH  A-DEATH             ECS014
00493          ELSE                                                     ECS014
00494              ADD DE-REI-CLAIM-AMT TO T-DISAB  A-DISAB.            ECS014
00495                                                                   ECS014
00496  0280-WRITE-OUT.                                                  ECS014
00497                                                                   ECS014
00498      IF L-C IS GREATER THAN 54                                    ECS014
00499          PERFORM 0470-HEADS-OUT THRU 0490-HEADS-OUT-X.            ECS014
00500                                                                   ECS014
00501      MOVE S-FROM                 TO L-FROM.                       ECS014
00502      MOVE S-TO                   TO L-TO.                         ECS014
00503      MOVE S-TYPE                 TO L-TYPE.                       ECS014
00504      MOVE DE-CARRIER             TO L-CARR.                       ECS014
00505      MOVE DE-GROUPING            TO L-GRP.                        ECS014
00506      GO TO 0230-R-LOOP.                                           ECS014
00507                                                                   ECS014
00508  0290-ADD-ISSUE.                                                  ECS014
00509                                                                   ECS014
00510      MOVE +1                     TO SA.                           ECS014
00511                                                                   ECS014
00512  0300-ADD-ISSUE-LOOP.                                             ECS014
00513                                                                   ECS014
00514      IF DE-AGT-TYPE (SA) = 'W'                                    ECS014
00515          IF DE-AGT-PRIME (SA) NOT = DE-REI-COMP                   ECS014
00516              GO TO 0310-ADD-ISS-INCR.                             ECS014
00517                                                                   ECS014
00518      IF DE-AGT-TYPE (SA) = ('D' OR 'R' OR 'W' OR 'P' OR 'T')      ECS014
00519          COMPUTE T-LCOM ROUNDED = T-LCOM +                        ECS014
00520                                 (DE-REI-LFPRM * DE-L-PC (SA))     ECS014
00521          COMPUTE A-LCOM ROUNDED = A-LCOM +                        ECS014
00522                                 (DE-REI-LFPRM * DE-L-PC (SA))     ECS014
00523          COMPUTE T-ACOM ROUNDED = T-ACOM +                        ECS014
00524                                 (DE-REI-AHPRM * DE-A-PC (SA))     ECS014
00525          COMPUTE A-ACOM ROUNDED = A-ACOM +                        ECS014
00526                                 (DE-REI-AHPRM * DE-A-PC (SA)).    ECS014
00527                                                                   ECS014
00528  0310-ADD-ISS-INCR.                                               ECS014
00529                                                                   ECS014
00530      ADD +1 TO SA.                                                ECS014
00531                                                                   ECS014
00532      IF SA LESS +11                                               ECS014
00533          GO TO 0300-ADD-ISSUE-LOOP.                               ECS014
00534                                                                   ECS014
00535  0320-ADD-ISSUE-X.                                                ECS014
00536      EXIT.                                                        ECS014
00537                                                                   ECS014
00538  0330-ADD-CANCEL.                                                 ECS014
00539                                                                   ECS014
00540      MOVE +1                     TO SA.                           ECS014
00541                                                                   ECS014
00542  0340-ADD-CANCEL-LOOP.                                            ECS014
00543                                                                   ECS014
00544      IF DE-AGT-TYPE (SA) = 'W'                                    ECS014
00545          IF DE-AGT-PRIME (SA) NOT = DE-REI-COMP                   ECS014
00546              GO TO 0350-ADD-CANCEL-INCR.                          ECS014
00547                                                                   ECS014
00548      IF DE-AGT-TYPE (SA) = ('D' OR 'R' OR 'W' OR 'P' OR 'T')      ECS014
00549          COMPUTE T-LCOM ROUNDED = T-LCOM +                        ECS014
00550                        (DE-REI-LFRFND * DE-L-PC (SA))             ECS014
00551          COMPUTE A-LCOM ROUNDED = A-LCOM +                        ECS014
00552                        (DE-REI-LFRFND * DE-L-PC (SA))             ECS014
00553          COMPUTE T-ACOM ROUNDED = T-ACOM +                        ECS014
00554                        (DE-REI-AHRFND * DE-A-PC (SA))             ECS014
00555          COMPUTE A-ACOM ROUNDED = A-ACOM +                        ECS014
00556                        (DE-REI-AHRFND * DE-A-PC (SA)).            ECS014
00557                                                                   ECS014
00558  0350-ADD-CANCEL-INCR.                                            ECS014
00559                                                                   ECS014
00560      ADD +1 TO SA.                                                ECS014
00561                                                                   ECS014
00562      IF SA LESS +11                                               ECS014
00563          GO TO 0340-ADD-CANCEL-LOOP.                              ECS014
00564                                                                   ECS014
00565  0360-ADD-CANCEL-X.                                               ECS014
00566      EXIT.                                                        ECS014
00567                                                                   ECS014
00568  EJECT                                                            ECS014
00569 ***************************************                           ECS014
00570 *                                     *                           ECS014
00571 *     PRINT   ROUTINE                 *                           ECS014
00572 *                                     *                           ECS014
00573 ***************************************                           ECS014
00574  0370-PRXX.                                                       ECS014
00575                              COPY ELCPRT2.                        ECS014
00576                                                                   ECS014
00577  0380-PRXX-EXIT.                                                  ECS014
00578      EXIT.                                                        ECS014
00579                                                                   ECS014
00580  EJECT                                                            ECS014
00581 ***************************************                           ECS014
00582 *                                     *                           ECS014
00583 *      HEADING  ROUTINES              *                           ECS014
00584 *                                     *                           ECS014
00585 ***************************************                           ECS014
00586  0390-PRINT-HEAD-A.                                               ECS014
00587                                                                   ECS014
00588      ADD 1 TO PAGER.                                              ECS014
00589      MOVE PAGER                  TO PGNO.                         ECS014
00590      MOVE '1'                    TO X.                            ECS014
00591      MOVE HEAD-A                 TO PRT.                          ECS014
00592      PERFORM 0370-PRXX THRU 0380-PRXX-EXIT.                       ECS014
00593      MOVE ' '                    TO X.                            ECS014
00594      MOVE HEAD-B                 TO PRT.                          ECS014
00595      PERFORM 0370-PRXX THRU 0380-PRXX-EXIT.                       ECS014
00596      MOVE HEAD-BA                TO PRT.                          ECS014
00597      PERFORM 0370-PRXX THRU 0380-PRXX-EXIT.                       ECS014
00598                                                                   ECS014
00599  0400-PRINT-HEAD-A-X.                                             ECS014
00600      EXIT.                                                        ECS014
00601                                                                   ECS014
00602  0410-PRINT-HEAD-B.                                               ECS014
00603                                                                   ECS014
00604      MOVE ' '                    TO X.                            ECS014
00605      MOVE HEAD-C                 TO PRT.                          ECS014
00606      PERFORM 0370-PRXX THRU 0380-PRXX-EXIT.                       ECS014
00607                                                                   ECS014
00608  0420-PRINT-HEAD-B-X.                                             ECS014
00609      EXIT.                                                        ECS014
00610                                                                   ECS014
00611  0430-PRINT-HEAD-C.                                               ECS014
00612                                                                   ECS014
00613      MOVE '0'                    TO X.                            ECS014
00614      MOVE HEAD-D                 TO PRT.                          ECS014
00615      PERFORM 0370-PRXX THRU 0380-PRXX-EXIT.                       ECS014
00616      MOVE ' '                    TO X.                            ECS014
00617      MOVE HEAD-E                 TO PRT.                          ECS014
00618      PERFORM 0370-PRXX THRU 0380-PRXX-EXIT.                       ECS014
00619      MOVE SPACES                 TO PRT.                          ECS014
00620                                                                   ECS014
00621  0440-PRINT-HEAD-C-X.                                             ECS014
00622      EXIT.                                                        ECS014
00623                                                                   ECS014
00624  EJECT                                                            ECS014
00625 ***************************************                           ECS014
00626 *                                     *                           ECS014
00627 *   ROUTINE TO SET UP THE HEADINGS    *                           ECS014
00628 *                                     *                           ECS014
00629 ***************************************                           ECS014
00630  0470-HEADS-OUT.                                                  ECS014
00631                                                                   ECS014
00632      IF L-TYPE = 'F'                                                 CL**2
00633          MOVE L-FROM             TO HC-2.                         ECS014
00634                                                                   ECS014
00635      IF L-TYPE = 'T'                                                 CL**2
00636          MOVE L-FROM             TO HC-2.                            CL**3
00637                                                                      CL**2
00638      PERFORM 0390-PRINT-HEAD-A THRU 0400-PRINT-HEAD-A-X.          ECS014
00639      PERFORM 0410-PRINT-HEAD-B THRU 0420-PRINT-HEAD-B-X.          ECS014
00640      PERFORM 0430-PRINT-HEAD-C THRU 0440-PRINT-HEAD-C-X.          ECS014
00641      MOVE ' '                    TO X.                            ECS014
00642      MOVE SPACES                 TO PRT.                          ECS014
00643      PERFORM 0370-PRXX THRU 0380-PRXX-EXIT.                       ECS014
00644      MOVE 11                     TO L-C.                          ECS014
00645                                                                   ECS014
00646  0490-HEADS-OUT-X.                                                ECS014
00647      EXIT.                                                        ECS014
00648                                                                   ECS014
00649  EJECT                                                            ECS014
00650 ***************************************                           ECS014
00651 *                                     *                           ECS014
00652 *   ROUTINE TO READ THE REIN TBL FILE *                           ECS014
00653 *                                     *                           ECS014
00654 ***************************************                           ECS014
00655  0500-READ-REIN-TBL.                                              ECS014
00656                                                                   ECS014
00657      IF REIN-SEARCH = LAST-REIN-SEARCH                            ECS014
00658          GO TO 0510-READ-REIN-TBL-X.                              ECS014
00659                                                                   ECS014
00660      MOVE REIN-SEARCH            TO LAST-REIN-SEARCH.             ECS014
00661      READ RTBL-FILE.                                              ECS014
00662                                                                   ECS014
00663 ***      INVALID KEY                                              ECS014
00664      IF REIN-FILE-STATUS = '23'                                   ECS014
00665          MOVE SPACES             TO REIN-NAME                     ECS014
00666          MOVE SPACES             TO CEDE-NAME                     ECS014
00667          GO TO 0510-READ-REIN-TBL-X.                              ECS014
00668                                                                   ECS014
00669      IF REIN-FILE-STATUS NOT = '00'                               ECS014
00670          MOVE 'INVALID READ - REIN' TO WS-ABEND-MESSAGE           ECS014
00671          MOVE REIN-FILE-STATUS   TO WS-ABEND-FILE-STATUS          ECS014
00672          GO TO ABEND-PGM.                                         ECS014
00673                                                                   ECS014
00674      MOVE RE-NAME                TO REIN-NAME.                    ECS014
00675      MOVE RE-CEDE-NAME           TO CEDE-NAME.                    ECS014
00676                                                                   ECS014
00677  0510-READ-REIN-TBL-X.                                            ECS014
00678      EXIT.                                                        ECS014
00679                                                                   ECS014
00680  EJECT                                                            ECS014
00681 ***************************************                           ECS014
00682 *                                     *                           ECS014
00683 *   ROUTINE TO PRINT THE TOTALS       *                           ECS014
00684 *                                     *                           ECS014
00685 ***************************************                           ECS014
00686  0520-TOTS-OUT.                                                   ECS014
00687                                                                   ECS014
00688      MOVE SPACES                 TO PRT.                          ECS014
00689      PERFORM 0370-PRXX THRU 0380-PRXX-EXIT.                       ECS014
00690      ADD T-LPREM                 TO G-LPREM.                      ECS014
00691      ADD T-APREM                 TO G-APREM.                      ECS014
00692      ADD T-LBEN                  TO G-LBEN.                       ECS014
00693      ADD T-ABEN                  TO G-ABEN.                       ECS014
00694      ADD T-CANCL                 TO G-CANCL.                      ECS014
00695      ADD T-CANCA                 TO G-CANCA.                      ECS014
00696      ADD T-DISAB                 TO G-DISAB.                      ECS014
00697      ADD T-DEATH                 TO G-DEATH.                      ECS014
00698      ADD T-LCOM                  TO G-LCOM.                       ECS014
00699      ADD T-ACOM                  TO G-ACOM.                       ECS014
00700      MOVE SPACES                 TO TOT-1, TOT-2.                 ECS014
00701      MOVE T-LBEN                 TO T1-LBEN.                      ECS014
00702      MOVE T-ABEN                 TO T2-ABEN.                      ECS014
00703      MOVE T-LCOM                 TO T1-LCOM.                      ECS014
00704      MOVE T-DEATH                TO T1-LCLAIMS.                   ECS014
00705      MOVE T-DISAB                TO T2-ACLAIMS.                   ECS014
00706      MOVE L-TO                   TO T1-TO.                        ECS014
00707      ADD T-LPREM  T-CANCL        GIVING  T1-LPRM.                 ECS014
00708      ADD T-APREM  T-CANCA        GIVING  T2-APRM.                 ECS014
00709      MOVE T-ACOM                 TO T2-ACOM.                      ECS014
00710      MOVE L-GRP                  TO T1-GROUP.                     ECS014
00711      MOVE L-CARR                 TO T1-CARR.                      ECS014
00712      MOVE LIFE-OVERRIDE-L6       TO T1-LKIND.                     ECS014
00713      MOVE   AH-OVERRIDE-L6       TO T2-AKIND.                     ECS014
00714                                                                   ECS014
00715      IF L-TYPE = 'F'                                              ECS014
00716          MOVE '  TO'             TO T1-FT.                        ECS014
00717                                                                   ECS014
00718      IF L-TYPE = 'T'                                              ECS014
00719          MOVE 'FROM'             TO T1-FT.                        ECS014
00720                                                                   ECS014
00721      IF L-TYPE = ' '                                              ECS014
00722          MOVE SPACES             TO T1-GROUP  T1-CARR             ECS014
00723          MOVE 'NET TOTAL'        TO T1-TO.                        ECS014
00724                                                                   ECS014
00725      MOVE '0'                    TO X.                            ECS014
00726      MOVE TOT-1                  TO PRT.                          ECS014
00727      PERFORM 0370-PRXX THRU 0380-PRXX-EXIT.                       ECS014
00728      MOVE ' '                    TO X.                            ECS014
00729      MOVE TOT-2                  TO PRT.                          ECS014
00730      PERFORM 0370-PRXX THRU 0380-PRXX-EXIT.                       ECS014
00731      ADD +3 TO L-C.                                               ECS014
00732                                                                   ECS014
00733  0530-TOTS-OUT-X.                                                 ECS014
00734      EXIT.                                                        ECS014
00735                                                                   ECS014
00736  0540-T-GP-TST.                                                   ECS014
00737                                                                   ECS014
00738      IF L-FROM NOT = S-FROM                                       ECS014
00739          GO TO 0570-GROUP-TOT-OUT.                                ECS014
00740                                                                   ECS014
00741  0550-T-O-X.                                                      ECS014
00742                                                                   ECS014
00743      MOVE DE-STATE               TO W-ST.                         ECS014
00744      MOVE ' '                    TO F-SW.                         ECS014
00745      MOVE DE-CARRIER             TO L-CARR.                       ECS014
00746      MOVE DE-GROUPING            TO L-GRP.                        ECS014
00747      MOVE DE-STATE               TO L-ST.                         ECS014
00748      MOVE DE-ACCOUNT             TO LAS-ACCT.                     ECS014
00749      MOVE DE-CARRIER             TO L-CARRIER.                    ECS014
00750      MOVE S-FROM                 TO L-FROM.                       ECS014
00751      MOVE S-TO                   TO L-TO.                         ECS014
00752      MOVE S-TYPE                 TO L-TYPE.                       ECS014
00753      MOVE ZERO               TO T-LBEN  T-ABEN  T-LPREM  T-APREM  ECS014
00754                                 T-CANCL T-DEATH T-DISAB  T-CANCA  ECS014
00755                                 T-LCOM  T-ACOM.                   ECS014
00756      MOVE ZERO      TO A-LBEN  A-ABEN  A-LPREM  A-APREM  A-DEATH  ECS014
00757                        A-LCOM  A-ACOM  A-CANCL  A-CANCA  A-DISAB. ECS014
00758                                                                   ECS014
00759  0560-E-TOTS.                                                     ECS014
00760                                                                   ECS014
00761      GO TO 0240-SET-DATAL.                                        ECS014
00762                                                                   ECS014
00763  EJECT                                                            ECS014
00764 ***************************************                           ECS014
00765 *                                     *                           ECS014
00766 *   REINSURANCE COMPANY BREAK ROUTINE *                           ECS014
00767 *                                     *                           ECS014
00768 ***************************************                           ECS014
00769  0570-GROUP-TOT-OUT.                                              ECS014
00770                                                                   ECS014
00771      MOVE G-LPREM                TO T-LPREM.                      ECS014
00772      MOVE G-APREM                TO T-APREM.                      ECS014
00773      MOVE G-LBEN                 TO T-LBEN.                       ECS014
00774      MOVE G-ABEN                 TO T-ABEN.                       ECS014
00775      MOVE G-CANCL                TO T-CANCL.                      ECS014
00776      MOVE G-DEATH                TO T-DEATH.                      ECS014
00777      MOVE G-DISAB                TO T-DISAB.                      ECS014
00778      MOVE G-CANCA                TO T-CANCA.                      ECS014
00779      MOVE G-LCOM                 TO T-LCOM.                       ECS014
00780      MOVE G-ACOM                 TO T-ACOM.                       ECS014
00781      ADD G-LPREM                 TO C-LPREM.                      ECS014
00782      ADD G-APREM                 TO C-APREM.                      ECS014
00783      ADD G-LBEN                  TO C-LBEN.                       ECS014
00784      ADD G-ABEN                  TO C-ABEN.                       ECS014
00785      ADD G-CANCL                 TO C-CANCL.                      ECS014
00786      ADD G-DEATH                 TO C-DEATH.                      ECS014
00787      ADD G-DISAB                 TO C-DISAB.                      ECS014
00788      ADD G-CANCA                 TO C-CANCA.                      ECS014
00789      ADD G-LCOM                  TO C-LCOM.                       ECS014
00790      ADD G-ACOM                  TO C-ACOM.                       ECS014
00791      MOVE SPACES                 TO L-TYPE.                       ECS014
00792      PERFORM 0520-TOTS-OUT.                                       ECS014
00793      MOVE ZEROS TO G-LPREM  G-APREM  G-LBEN   G-ABEN  G-DEATH     ECS014
00794                    G-DISAB  G-CANCL  G-CANCA  G-LCOM  G-ACOM.     ECS014
00795      MOVE 99                     TO L-C.                          ECS014
00796                                                                   ECS014
00797  0580-G-T-O-X.                                                    ECS014
00798                                                                   ECS014
00799      GO TO 0550-T-O-X.                                            ECS014
00800                                                                   ECS014
00801  EJECT                                                            ECS014
00802 ***************************************                           ECS014
00803 *                                     *                           ECS014
00804 *    END OF FILE ROUTINES             *                           ECS014
00805 *                                     *                           ECS014
00806 ***************************************                           ECS014
00807  0630-NO-INPUT-RTN.                                               ECS014
00808                                                                   ECS014
00809      PERFORM 0390-PRINT-HEAD-A THRU 0400-PRINT-HEAD-A-X.          ECS014
00810      MOVE ' NO REINSURANCE RECORDS RECEIVED THIS CYCLE ' TO PRT.  ECS014
00811      MOVE '0'                    TO X.                            ECS014
00812      PERFORM 0370-PRXX THRU 0380-PRXX-EXIT.                       ECS014
00813      GO TO 0650-CLOSE-FICH.                                       ECS014
00814                                                                   ECS014
00815  0640-END-PRINT.                                                  ECS014
00816                                                                   ECS014
00817      PERFORM 0520-TOTS-OUT THRU 0530-TOTS-OUT-X.                  ECS014
00818      PERFORM 0570-GROUP-TOT-OUT.                                  ECS014
00819      MOVE '1'                    TO X.                            ECS014
00820      MOVE SPACES                 TO PRT.                          ECS014
00821      PERFORM 0370-PRXX THRU 0380-PRXX-EXIT.                       ECS014
00822      CLOSE RTBL-FILE.                                             ECS014
00823                                                                   ECS014
00824      IF REIN-FILE-STATUS NOT = '00'                               ECS014
00825          MOVE 'CLOSE ERROR - REIN'    TO WS-ABEND-MESSAGE         ECS014
00826          MOVE REIN-FILE-STATUS   TO WS-ABEND-FILE-STATUS          ECS014
00827          GO TO ABEND-PGM.                                         ECS014
00828                                                                   ECS014
00829  0650-CLOSE-FICH.                                                 ECS014
00830                                                                   ECS014
00831                              COPY ELCPRTC.                        ECS014
00832                                                                   ECS014
00833      CLOSE PRNT-OUT.                                              ECS014
00834                                                                   ECS014
00835  0660-EXIT-REINS.                                                 ECS014
00836      EXIT.                                                        ECS014
00837                                                                   ECS014
00838  EJECT                                                            ECS014
00839  0680-END-OF-JOB SECTION.                                         ECS014
00840                                                                   ECS014
00841  0690-E-O-J.                                                      ECS014
00842      GOBACK.                                                      ECS014
00843                                                                   ECS014
00844  ABEND-PGM SECTION.              COPY ELCABEND.                   ECS014
00845 /                                                                 ECS014
00846  LCP-WRITE-POS-PRT SECTION.                                       ECS014
00847      IF LCP-ASA = '+'                                             ECS014
00848          WRITE PRT AFTER 0 LINE                                   ECS014
00849      ELSE                                                         ECS014
00850      IF LCP-ASA = ' '                                             ECS014
00851          WRITE PRT AFTER ADVANCING 1 LINE                         ECS014
00852      ELSE                                                         ECS014
00853      IF LCP-ASA = '0'                                             ECS014
00854          WRITE PRT AFTER ADVANCING 2 LINE                         ECS014
00855      ELSE                                                         ECS014
00856      IF LCP-ASA = '-'                                             ECS014
00857          WRITE PRT AFTER ADVANCING 3 LINE                         ECS014
00858      ELSE                                                         ECS014
00859      IF LCP-ASA = '1'                                             ECS014
00860          WRITE PRT AFTER ADVANCING PAGE                           ECS014
00861      ELSE                                                         ECS014
00862      IF LCP-ASA = '2'                                             ECS014
00863          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS014
00864      ELSE                                                         ECS014
00865      IF LCP-ASA = '3'                                             ECS014
00866          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS014
00867      ELSE                                                         ECS014
00868      IF LCP-ASA = '4'                                             ECS014
00869          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS014
00870      ELSE                                                         ECS014
00871      IF LCP-ASA = '5'                                             ECS014
00872          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS014
00873      ELSE                                                         ECS014
00874      IF LCP-ASA = '6'                                             ECS014
00875          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS014
00876      ELSE                                                         ECS014
00877      IF LCP-ASA = '7'                                             ECS014
00878          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS014
00879      ELSE                                                         ECS014
00880      IF LCP-ASA = '8'                                             ECS014
00881          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS014
00882      ELSE                                                         ECS014
00883      IF LCP-ASA = '9'                                             ECS014
00884          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS014
00885      ELSE                                                         ECS014
00886      IF LCP-ASA = 'A'                                             ECS014
00887          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS014
00888      ELSE                                                         ECS014
00889      IF LCP-ASA = 'B'                                             ECS014
00890          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS014
00891      ELSE                                                         ECS014
00892      IF LCP-ASA = 'C'                                             ECS014
00893          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS014
00894      ELSE                                                         ECS014
00895      IF LCP-ASA = 'V'                                             ECS014
00896          WRITE PRT AFTER ADVANCING LCP-P01                        ECS014
00897      ELSE                                                         ECS014
00898      IF LCP-ASA = 'W'                                             ECS014
00899          WRITE PRT AFTER ADVANCING LCP-P02                        ECS014
00900      ELSE                                                         ECS014
00901      DISPLAY 'ASA CODE ERROR'.                                    ECS014
00902  LCP-WRITE-END-PRT.                                               ECS014
00903      EXIT.                                                        ECS014
