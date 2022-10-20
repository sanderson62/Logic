00001  ID DIVISION.                                                     01/14/97
00002                                                                   EL6507
00003  PROGRAM-ID.                 EL6507.                                 LV009
00004 *              PROGRAM CONVERTED BY                                  CL**8
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**8
00006 *              CONVERSION DATE 11/27/95 09:07:19.                    CL**8
00007 *                            VMOD=2.009                              CL**9
00008 *                                                                 EL6507
00009 *AUTHOR.     LOGIC,INC.                                              CL**8
00010 *            DALLAS, TEXAS.                                          CL**8
00011                                                                   EL6507
00012 *DATE-COMPILED.                                                      CL**8
00013 *SECURITY.   *****************************************************   CL**8
00014 *            *                                                   *   CL**8
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**8
00016 *            *                                                   *   CL**8
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**8
00018 *            *                                                   *   CL**8
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**8
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**8
00021 *            *                                                   *   CL**8
00022 *            *****************************************************   CL**8
00023 *                                                                 EL6507
00024 *REMARKS.    TRANSACTION - EXD3 - ACCOUNT PLAN MASTER MAINT.      EL6507
00025 *                                                                 EL6507
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL6507AI FILLER
101101******************************************************************

00026  ENVIRONMENT DIVISION.                                            EL6507
00027                                                                   EL6507
00028      EJECT                                                        EL6507
00029  DATA DIVISION.                                                   EL6507
00030  WORKING-STORAGE SECTION.                                         EL6507
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL6507
00032  77  FILLER  PIC X(32)  VALUE '*    EL6507 WORKING STORAGE    *'. EL6507
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.009 *********'.    CL**9
00034                                                                   EL6507
00035  01  WS-DATE-AREA.                                                EL6507
00036      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    EL6507
00037      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.    EL6507
00038                                                                   EL6507
00039  01  STANDARD-AREAS.                                              EL6507
00040      12  RETURNED-FROM               PIC X(08).                   EL6507
00041      12  WS-PLAN-KEY.                                             EL6507
00042          16  WS-PLAN-ACCT-KEY.                                    EL6507
00043              20  WS-PLAN-COMPANY-CD  PIC X.                       EL6507
00044              20  WS-PLAN-CARRIER     PIC X.                       EL6507
00045              20  WS-PLAN-GROUP       PIC X(6).                    EL6507
00046              20  WS-PLAN-STATE       PIC X(2).                    EL6507
00047              20  WS-PLAN-ACCOUNT     PIC X(10).                   EL6507
00048          16  WS-PLAN-BEN-TYPE        PIC X.                       EL6507
00049          16  WS-PLAN-BEN             PIC XX.                      EL6507
00050          16  WS-PLAN-REVISION        PIC X(3).                    EL6507
00051      12  GETMAIN-SPACE               PIC X(01) VALUE SPACES.      EL6507
00052      12  SUB                         PIC S9(04) COMP VALUE +0.    EL6507
00053      12  SUB1                        PIC S9(4)   VALUE +0    COMP.EL6507
00054      12  SUB2                        PIC S9(4)   VALUE +0    COMP.EL6507
00055      12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1500.  EL6507
00056      12  QID1.                                                    EL6507
00057          16  QID1-TERM               PIC X(4).                    EL6507
00058          16  FILLER                  PIC X(4)    VALUE '650H'.    EL6507
00059      12  QID2.                                                    EL6507
00060          16  QID2-TERM               PIC X(4).                    EL6507
00061          16  FILLER                  PIC X(4)    VALUE '650I'.    EL6507
00062      12  WS-MAP-LENGTH               PIC S9(4) COMP VALUE +656.   EL6507
00063      12  MAP-NAME                    PIC X(8)    VALUE 'EL6507A'. EL6507
00064      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL6507S'. EL6507
00065      12  SCREEN-NUMBER               PIC X(4)    VALUE '650H'.    EL6507
00066      12  TRANS-ID                    PIC X(4)    VALUE 'EXD3'.    EL6507
00067      12  THIS-PGM                    PIC X(8)    VALUE 'EL6507'.  EL6507
00068      12  PGM-NAME                    PIC X(8).                    EL6507
00069      12  TIME-IN                     PIC S9(7).                   EL6507
00070      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL6507
00071          16  FILLER                  PIC X.                       EL6507
00072          16  TIME-OUT                PIC 99V99.                   EL6507
00073          16  FILLER                  PIC XX.                      EL6507
00074      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.   EL6507
00075      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.   EL6507
00076      12  XCTL-106                    PIC X(8)    VALUE 'EL106'.   EL6507
00077      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.   EL6507
00078      12  XCTL-650                    PIC X(8)    VALUE 'EL650'.   EL6507
00079      12  XCTL-6501                   PIC X(8)    VALUE 'EL6501'.  EL6507
00080      12  XCTL-6502                   PIC X(8)    VALUE 'EL6502'.  EL6507
00081      12  XCTL-6503                   PIC X(8)    VALUE 'EL6503'.  EL6507
00082      12  XCTL-6504                   PIC X(8)    VALUE 'EL6504'.  EL6507
00083      12  XCTL-6505                   PIC X(8)    VALUE 'EL6505'.  EL6507
00084      12  XCTL-6506                   PIC X(8)    VALUE 'EL6505'.  EL6507
00085      12  XCTL-656                    PIC X(8)    VALUE 'EL656'.   EL6507
00086      12  XCTL-6565                   PIC X(8)    VALUE 'EL6565'.  EL6507
00087      12  LINK-001                    PIC X(8)    VALUE 'EL001'.   EL6507
00088      12  LINK-004                    PIC X(8)    VALUE 'EL004'.   EL6507
00089      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL6507
00090      12  FILE-ID                     PIC X(8)    VALUE SPACES.    EL6507
00091      12  ERACCT-FILE                 PIC X(8)    VALUE 'ERACCT'.  EL6507
00092      12  ERPLAN-FILE                 PIC X(8)    VALUE 'ERPLAN'.  EL6507
00093      12  ERRATE-FILE                 PIC X(8)    VALUE 'ERRATE'.  EL6507
00094      12  ELCNTL-FILE                 PIC X(8)    VALUE 'ELCNTL'.  EL6507
00095      12  BIN-CURRENT-SAVE            PIC XX      VALUE SPACES.    EL6507
00096      12  YMD-CURRENT-SAVE            PIC X(6)    VALUE SPACES.    EL6507
00097                                                                   EL6507
00098      12  JOURNAL-LENGTH              PIC S9(4)   VALUE +23   COMP.EL6507
00099      12  ERPLAN-LENGTH               PIC S9(4)   VALUE +420  COMP.EL6507
00100      12  ELCNTL-LENGTH               PIC S9(4)   VALUE +750  COMP.   CL**5
00101      12  SC-ITEM                     PIC S9(4)   VALUE +1    COMP.EL6507
00102      12  WS-JOURNAL-FILE-LENGTH      PIC S9(4)   VALUE +0    COMP.EL6507
00103                                                                   EL6507
00104      12  DEEDIT-FIELD                PIC X(15).                   EL6507
00105      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      EL6507
00106      12  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(14)V9.    EL6507
00107      12  DEEDIT-FIELD-V2  REDEFINES DEEDIT-FIELD PIC S9(13)V99.   EL6507
00108      12  DEEDIT-FIELD-V3  REDEFINES DEEDIT-FIELD PIC S9(12)V999.  EL6507
00109      12  DEEDIT-FIELD-V4  REDEFINES DEEDIT-FIELD PIC S9(11)V9(4). EL6507
00110      12  DEEDIT-FIELD-V5  REDEFINES DEEDIT-FIELD PIC S9(10)V9(5). EL6507
00111      12  DEEDIT-FIELD-V6  REDEFINES DEEDIT-FIELD PIC S9(9)V9(6).  EL6507
00112                                                                   EL6507
00113      12  WS-EDIT-FIELD-CONV          PIC S9(4)   VALUE +0.        EL6507
00114                                                                   EL6507
00115      12  RATE-KEY.                                                EL6507
00116          16  RATE-COMP-CD            PIC X.                       EL6507
00117          16  RATE-STATE              PIC XX.                      EL6507
00118          16  RATE-CLASS              PIC XX.                      EL6507
00119          16  RATE-DEV                PIC XXX.                     EL6507
00120          16  FILLER                  PIC X(20).                   EL6507
00121                                                                   EL6507
00122      12  FILLER OCCURS 8 TIMES.                                   EL6507
00123          16  WS-MOBENI               PIC S9(04).                  EL6507
00124          16  WS-TOTBENI              PIC S9(06).                  EL6507
00125      12  WS-PRMPCTI                  PIC S9V9(04)   VALUE ZEROS.  EL6507
00126      12  WS-REFPCTI                  PIC S9V9(04)   VALUE ZEROS.  EL6507
00127      12  WS-PRMAMTI                  PIC S9(03)V99  VALUE ZEROS.  EL6507
00128      12  WS-REFAMTI                  PIC S9(03)V99  VALUE ZEROS.  EL6507
00129      12  WS-CLMAMTI                  PIC S9(03)V99  VALUE ZEROS.  EL6507
00130      12  WS-STTAXI                   PIC S9V9(04)   VALUE ZEROS.  EL6507
00131      12  WS-POLFEEI                  PIC S99V999    VALUE ZEROS.  EL6507
00132      12  WS-CORETI                   PIC S9V9(04)   VALUE ZEROS.  EL6507
00133      12  WS-DEVPCTI                  PIC S9V9(06)   VALUE ZEROS.  EL6507
00134      12  WS-SALTAXI                  PIC S9V9(04)   VALUE ZEROS.     CL**9
00135      12  WS-OVSAMTI                  PIC S9(03)V99  VALUE ZEROS.     CL**9
00136      12  WS-OVSPCTI                  PIC S9V9(4)    VALUE ZEROS.     CL**9
00137      EJECT                                                        EL6507
00138      12  ERROR-MESSAGES.                                          EL6507
00139          16  ER-0000                 PIC X(4)    VALUE '0000'.    EL6507
00140          16  ER-0002                 PIC X(4)    VALUE '0002'.    EL6507
00141          16  ER-0004                 PIC X(4)    VALUE '0004'.    EL6507
00142          16  ER-0008                 PIC X(4)    VALUE '0008'.    EL6507
00143          16  ER-0029                 PIC X(4)    VALUE '0029'.    EL6507
00144          16  ER-0066                 PIC X(4)    VALUE '0066'.    EL6507
00145          16  ER-0067                 PIC X(4)    VALUE '0067'.    EL6507
00146          16  ER-0068                 PIC X(4)    VALUE '0068'.    EL6507
00147          16  ER-0070                 PIC X(4)    VALUE '0070'.    EL6507
00148          16  ER-0132                 PIC X(4)    VALUE '0132'.    EL6507
00149          16  ER-0627                 PIC X(4)    VALUE '0627'.    EL6507
00150          16  ER-2039                 PIC X(4)    VALUE '2039'.    EL6507
00151          16  ER-2056                 PIC X(4)    VALUE '2056'.    EL6507
00152          16  ER-2106                 PIC X(4)    VALUE '2106'.    EL6507
00153          16  ER-2107                 PIC X(4)    VALUE '2107'.    EL6507
00154          16  ER-2152                 PIC X(4)    VALUE '2152'.    EL6507
00155          16  ER-2154                 PIC X(4)    VALUE '2154'.    EL6507
00156          16  ER-2168                 PIC X(4)    VALUE '2168'.    EL6507
00157          16  ER-2169                 PIC X(4)    VALUE '2169'.    EL6507
00158          16  ER-2170                 PIC X(4)    VALUE '2170'.    EL6507
00159          16  ER-2273                 PIC X(4)    VALUE '2273'.    EL6507
00160          16  ER-2274                 PIC X(4)    VALUE '2274'.    EL6507
00161          16  ER-2275                 PIC X(4)    VALUE '2275'.    EL6507
00162          16  ER-2276                 PIC X(4)    VALUE '2276'.    EL6507
00163          16  ER-2572                 PIC X(4)    VALUE '2572'.    EL6507
00164          16  ER-2937                 PIC X(4)    VALUE '2937'.    EL6507
00165          16  ER-2938                 PIC X(4)    VALUE '2938'.    EL6507
00166          16  ER-2939                 PIC X(4)    VALUE '2939'.    EL6507
00167          16  ER-2948                 PIC X(4)    VALUE '2948'.       CL**2
00168          16  ER-3126                 PIC X(4)    VALUE '3126'.    EL6507
00169          16  ER-3127                 PIC X(4)    VALUE '3127'.    EL6507
00170          16  ER-3128                 PIC X(4)    VALUE '3128'.    EL6507
00171          16  ER-7208                 PIC X(4)    VALUE '7208'.    EL6507
00172          16  ER-9055                 PIC X(4)    VALUE '9055'.    EL6507
00173          16  ER-9407                 PIC X(4)    VALUE '9407'.    EL6507
00174                                                                   EL6507
00175      12  ELCNTL-KEY.                                              EL6507
00176          16  CNTL-COMP-ID            PIC X(3)    VALUE SPACES.    EL6507
00177          16  CNTL-REC-TYPE           PIC X       VALUE SPACES.    EL6507
00178          16  CNTL-ACCESS             PIC X(4)    VALUE SPACES.    EL6507
00179          16  CNTL-SEQ-NO             PIC S9(4)   VALUE +0  COMP.  EL6507
00180                                                                   EL6507
00181      EJECT                                                        EL6507
00182                            COPY ELCSCTM.                          EL6507
00183      EJECT                                                        EL6507
00184                            COPY ELCSCRTY.                         EL6507
00185      EJECT                                                        EL6507
00186                            COPY ELCLOGOF.                         EL6507
00187      EJECT                                                        EL6507
00188                            COPY ELCDATE.                          EL6507
00189      EJECT                                                        EL6507
00190                            COPY ELCATTR.                          EL6507
00191      EJECT                                                        EL6507
00192                            COPY ELCEMIB.                          EL6507
00193      EJECT                                                        EL6507
00194                            COPY ELCINTF.                          EL6507
00195      EJECT                                                        EL6507
00196                            COPY ELC650PI.                         EL6507
00197      EJECT                                                        EL6507
00198                            COPY ELCJPFX.                          EL6507
00199                                          PIC X(2000).             EL6507
00200                                                                   EL6507
00201      EJECT                                                        EL6507
00202                                      COPY ELCAID.                    CL**6
00203  01  FILLER    REDEFINES DFHAID.                                  EL6507
00204      12  FILLER                      PIC X(8).                    EL6507
00205      12  PF-VALUES                   PIC X       OCCURS 2.        EL6507
00206                                                                   EL6507
00207      EJECT                                                        EL6507
00208                                      COPY EL6507S.                EL6507
00209  01  FILLER REDEFINES EL6507AI.                                   EL6507
101101     12  FILLER                      PIC X(124).                  EL6507
00211      12  WS-MAP-LIMITS OCCURS 8 TIMES.                            EL6507
00212          16  EL6507H-AGEL            PIC S9(04) COMP.             EL6507
00213          16  EL6507H-AGEA            PIC X(01).                   EL6507
00214          16  EL6507H-AGEI            PIC 9(02).                   EL6507
00215 *        16  EL6507H-AGEO REDEFINES                               EL6507
00216 *            EL6507H-AGEI            PIC ZZ.                      EL6507
00217          16  EL6507H-TERML           PIC S9(04) COMP.             EL6507
00218          16  EL6507H-TERMA           PIC X(01).                   EL6507
00219          16  EL6507H-TERMI           PIC 9(03).                   EL6507
00220 *        16  EL6507H-TERMO REDEFINES                              EL6507
00221 *            EL6507H-TERMI           PIC ZZZ.                     EL6507
00222          16  EL6507H-TOTBENL         PIC S9(04) COMP.             EL6507
00223          16  EL6507H-TOTBENA         PIC X(01).                   EL6507
00224          16  EL6507H-TOTBENI         PIC X(07).                      CL**7
00225          16  EL6507H-TOTBENO REDEFINES                            EL6507
00226              EL6507H-TOTBENI         PIC ZZZ,ZZZ.                    CL**7
00227          16  EL6507H-MOBENL          PIC S9(04) COMP.             EL6507
00228          16  EL6507H-MOBENA          PIC X(01).                   EL6507
00229          16  EL6507H-MOBENI          PIC X(05).                      CL**7
00230          16  EL6507H-MOBENO REDEFINES                             EL6507
00231              EL6507H-MOBENI          PIC Z,ZZZ.                      CL**7
00232                                                                   EL6507
00233      EJECT                                                        EL6507
00234  LINKAGE SECTION.                                                 EL6507
00235  01  DFHCOMMAREA                     PIC X(1500).                 EL6507
00236                                                                   EL6507
00237      EJECT                                                        EL6507
00238 *01 PARMLIST .                                                       CL**8
00239 *    02  FILLER                      PIC S9(8)   COMP.               CL**8
00240 *    02  ERPLAN-POINTER              PIC S9(8)   COMP.               CL**8
00241 *    02  ERRATE-POINTER              PIC S9(8)   COMP.               CL**8
00242 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.               CL**8
00243 *    02  ERACCT-POINTER              PIC S9(8)   COMP.               CL**8
00244                                                                   EL6507
00245                                      COPY ERCPLAN.                EL6507
00246      EJECT                                                        EL6507
00247                                      COPY ERCRATE.                EL6507
00248      EJECT                                                        EL6507
00249                                      COPY ELCCNTL.                EL6507
00250      EJECT                                                        EL6507
00251                                      COPY ERCACCT.                EL6507
00252      EJECT                                                        EL6507
00253                                                                   EL6507
00254  PROCEDURE DIVISION.                                              EL6507
00255                                                                   EL6507
00256      MOVE EIBDATE                TO DC-JULIAN-YYDDD               EL6507
00257      MOVE '5'                    TO DC-OPTION-CODE                EL6507
00258      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT                EL6507
00259      MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE                     EL6507
00260      MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE                 EL6507
00261      MOVE DC-GREG-DATE-1-YMD     TO YMD-CURRENT-SAVE              EL6507
00262                                                                   EL6507
00263      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK       EL6507
00264                                                                   EL6507
00265      MOVE EIBTRMID               TO QID1-TERM                     EL6507
00266                                      QID2-TERM.                   EL6507
00267                                                                   EL6507
00268      IF EIBCALEN = 0                                              EL6507
00269          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6507
00270                                                                   EL6507
00271      IF PI-RETURN-TO-PROGRAM EQUAL THIS-PGM                       EL6507
00272         MOVE PI-CALLING-PROGRAM  TO RETURNED-FROM.                EL6507
00273                                                                   EL6507
00274      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6507
00275         MOVE THIS-PGM            TO PI-CALLING-PROGRAM.           EL6507
00276                                                                   EL6507
00277      MOVE LOW-VALUES             TO EL6507AI.                     EL6507
00278                                                                   EL6507
00279      IF EIBTRNID NOT = TRANS-ID                                   EL6507
00280         IF RETURNED-FROM = XCTL-6565 OR XCTL-106                  EL6507
00281            MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM        EL6507
00282            MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM      EL6507
00283            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1        EL6507
00284            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2        EL6507
00285            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3        EL6507
00286            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4        EL6507
00287            MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5        EL6507
00288            MOVE SPACES               TO PI-SAVED-PROGRAM-6        EL6507
00289            PERFORM 6200-RECOVER-TEMP-STORAGE THRU 6200-EXIT.      EL6507
00290                                                                   EL6507
00291      IF EIBTRNID NOT = TRANS-ID                                   EL6507
00292         MOVE PI-PLAN-KEY         TO WS-PLAN-KEY                   EL6507
00293         GO TO 4000-SHOW.                                          EL6507
00294                                                                   EL6507
00295      EXEC CICS HANDLE CONDITION                                   EL6507
00296           PGMIDERR  (9600-PGMID-ERROR)                            EL6507
00297           ERROR     (9990-ABEND)                                  EL6507
00298      END-EXEC.                                                    EL6507
00299                                                                   EL6507
00300      IF EIBAID = DFHCLEAR                                         EL6507
00301          GO TO 9400-CLEAR.                                        EL6507
00302                                                                   EL6507
00303      EJECT                                                        EL6507
00304  0200-RECEIVE.                                                    EL6507
00305                                                                   EL6507
00306      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6507
00307          MOVE ER-0008            TO EMI-ERROR                     EL6507
00308          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6507
00309          MOVE -1                 TO PFKEYL                        EL6507
00310          GO TO 8200-SEND-DATAONLY.                                EL6507
00311                                                                   EL6507
00312      EXEC CICS RECEIVE                                            EL6507
00313          MAP      (MAP-NAME)                                      EL6507
00314          MAPSET   (MAPSET-NAME)                                   EL6507
00315          INTO     (EL6507AI)                                      EL6507
00316      END-EXEC.                                                    EL6507
00317                                                                   EL6507
00318      IF PFKEYL EQUAL +0                                           EL6507
00319          GO TO 0300-CHECK-PFKEYS.                                 EL6507
00320                                                                   EL6507
00321      IF EIBAID NOT = DFHENTER                                     EL6507
00322          MOVE ER-0004            TO EMI-ERROR                     EL6507
00323          GO TO 0320-INPUT-ERROR.                                  EL6507
00324                                                                   EL6507
00325      IF (PFKEYI NUMERIC) AND (PFKEYI GREATER 0 AND LESS 25)       EL6507
00326          MOVE PF-VALUES (PFKEYI) TO EIBAID                        EL6507
00327      ELSE                                                         EL6507
00328          MOVE ER-0029            TO EMI-ERROR                     EL6507
00329          GO TO 0320-INPUT-ERROR.                                  EL6507
00330                                                                   EL6507
00331      EJECT                                                        EL6507
00332  0300-CHECK-PFKEYS.                                               EL6507
00333                                                                   EL6507
00334      IF EIBAID = DFHPF23                                          EL6507
00335          GO TO 8810-PF23.                                         EL6507
00336                                                                   EL6507
00337      IF EIBAID = DFHPF24                                          EL6507
00338          GO TO 9200-RETURN-MAIN-MENU.                             EL6507
00339                                                                   EL6507
00340      IF EIBAID = DFHPF12                                          EL6507
00341          GO TO 9500-PF12.                                         EL6507
00342                                                                   EL6507
00343      IF EIBAID = DFHPF1                                           EL6507
00344          GO TO 0400-BROWSE-FORWARD.                               EL6507
00345                                                                   EL6507
00346      IF EIBAID = DFHPF2                                           EL6507
00347          GO TO 0500-BROWSE-BACKWARD.                              EL6507
00348                                                                   EL6507
00349      IF EIBAID = DFHPF4                                           EL6507
00350         MOVE PI-ACCT-STATE      TO PI-WS-STATE                    EL6507
00351         PERFORM 6100-CREATE-TEMP-STORAGE THRU 6100-EXIT           EL6507
00352         MOVE XCTL-106           TO PGM-NAME                       EL6507
00353         GO TO 9300-XCTL.                                          EL6507
00354                                                                   EL6507
00355      IF EIBAID = DFHPF3                                           EL6507
00356         MOVE PI-ACCT-STATE      TO PI-WS-STATE                    EL6507
00357         MOVE PI-PLAN-BEN        TO PI-WS-PLAN                     EL6507
00358         MOVE PI-PLAN-BEN-TYPE   TO PI-WS-TYPE                     EL6507
00359         MOVE '00'               TO PI-WS-CLASS                    EL6507
00360         PERFORM 6100-CREATE-TEMP-STORAGE THRU 6100-EXIT           EL6507
00361         MOVE XCTL-6565          TO PGM-NAME                       EL6507
00362         GO TO 9300-XCTL.                                          EL6507
00363                                                                   EL6507
00364      IF EIBAID = DFHENTER                                         EL6507
00365         GO TO 0330-CHECK-MAINTYP.                                 EL6507
00366                                                                   EL6507
00367      MOVE ER-0029                TO EMI-ERROR.                    EL6507
00368                                                                   EL6507
00369  0320-INPUT-ERROR.                                                EL6507
00370                                                                   EL6507
00371      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL6507
00372      MOVE AL-UNBON               TO PFKEYA                        EL6507
00373      MOVE -1                     TO PFKEYL                        EL6507
00374      GO TO 8200-SEND-DATAONLY.                                    EL6507
00375                                                                   EL6507
00376  0330-CHECK-MAINTYP.                                              EL6507
00377                                                                   EL6507
00378      IF MAINTL GREATER ZERO                                       EL6507
00379         IF MAINTI = 'S' OR 'C' OR 'A' OR 'D'                      EL6507
00380            MOVE AL-UANON         TO MAINTA                        EL6507
00381         ELSE                                                      EL6507
00382            MOVE -1               TO MAINTL                        EL6507
00383            MOVE AL-UABON         TO MAINTA                        EL6507
00384            MOVE ER-2039          TO EMI-ERROR                     EL6507
00385            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL6507
00386            GO TO 8200-SEND-DATAONLY                               EL6507
00387      ELSE                                                         EL6507
00388         MOVE -1                  TO MAINTL                        EL6507
00389         MOVE AL-UABON            TO MAINTA                        EL6507
00390         MOVE ER-2039             TO EMI-ERROR                     EL6507
00391         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6507
00392         GO TO 8200-SEND-DATAONLY.                                 EL6507
00393                                                                   EL6507
00394      PERFORM 7400-BUILD-KEY THRU 7499-EXIT                        EL6507
00395                                                                   EL6507
00396      IF MAINTI EQUAL 'S'                                          EL6507
00397         GO TO 4000-SHOW.                                          EL6507
00398                                                                   EL6507
00399      PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT                 EL6507
00400                                                                   EL6507
00401      IF EMI-ERROR NOT = ZEROS                                     EL6507
00402         MOVE -1                  TO MAINTL                        EL6507
00403         GO TO 8200-SEND-DATAONLY.                                 EL6507
00404                                                                   EL6507
00405      IF MAINTI EQUAL 'A'                                          EL6507
00406         GO TO 4100-ADD.                                           EL6507
00407                                                                   EL6507
00408      IF MAINTI EQUAL 'D'                                          EL6507
00409         GO TO 4500-DELETE.                                        EL6507
00410                                                                   EL6507
00411      GO TO 4200-CHANGE.                                           EL6507
00412                                                                   EL6507
00413      EJECT                                                        EL6507
00414                                                                   EL6507
00415  0400-BROWSE-FORWARD.                                             EL6507
00416                                                                   EL6507
00417      PERFORM 7400-BUILD-KEY THRU 7499-EXIT                        EL6507
00418                                                                   EL6507
00419      MOVE WS-PLAN-KEY            TO PI-PLAN-KEY                   EL6507
00420                                                                   EL6507
00421      EXEC CICS HANDLE CONDITION                                   EL6507
00422          NOTFND   (0420-END-FILE)                                 EL6507
00423          ENDFILE  (0420-END-FILE)                                 EL6507
00424      END-EXEC                                                     EL6507
00425                                                                   EL6507
00426      EXEC CICS STARTBR                                            EL6507
00427           DATASET  (ERPLAN-FILE)                                  EL6507
00428           RIDFLD   (WS-PLAN-KEY)                                  EL6507
00429           GTEQ                                                    EL6507
00430      END-EXEC.                                                    EL6507
00431                                                                   EL6507
00432  0410-READ-ERPLAN-NEXT.                                           EL6507
00433                                                                   EL6507
00434      EXEC CICS READNEXT                                           EL6507
00435           DATASET  (ERPLAN-FILE)                                  EL6507
00436           SET      (ADDRESS OF PLAN-MASTER)                          CL**8
00437           RIDFLD   (WS-PLAN-KEY)                                  EL6507
00438      END-EXEC                                                     EL6507
00439                                                                   EL6507
00440      IF PI-PLAN-KEY EQUAL WS-PLAN-KEY                             EL6507
00441         GO TO 0410-READ-ERPLAN-NEXT.                              EL6507
00442                                                                   EL6507
00443      IF WS-PLAN-ACCT-KEY EQUAL PI-PLAN-ACCT-KEY                   EL6507
00444         MOVE WS-PLAN-KEY         TO PI-PLAN-KEY                   EL6507
00445         MOVE LOW-VALUES          TO EL6507AO                      EL6507
00446         GO TO 5000-BUILD-INITIAL-SCREEN.                          EL6507
00447                                                                   EL6507
00448  0420-END-FILE.                                                   EL6507
00449                                                                   EL6507
00450      MOVE -1                     TO MAINTL                        EL6507
00451      MOVE ER-0066                TO EMI-ERROR                     EL6507
00452      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL6507
00453      GO TO 8200-SEND-DATAONLY.                                    EL6507
00454                                                                   EL6507
00455  0500-BROWSE-BACKWARD.                                            EL6507
00456                                                                   EL6507
00457      PERFORM 7400-BUILD-KEY THRU 7499-EXIT                        EL6507
00458                                                                   EL6507
00459      MOVE WS-PLAN-KEY            TO PI-PLAN-KEY                   EL6507
00460                                                                   EL6507
00461      EXEC CICS HANDLE CONDITION                                   EL6507
00462          NOTFND   (0520-END-FILE)                                 EL6507
00463          ENDFILE  (0520-END-FILE)                                 EL6507
00464      END-EXEC                                                     EL6507
00465                                                                   EL6507
00466      EXEC CICS STARTBR                                            EL6507
00467           DATASET  (ERPLAN-FILE)                                  EL6507
00468           RIDFLD   (WS-PLAN-KEY)                                  EL6507
00469           GTEQ                                                    EL6507
00470      END-EXEC                                                     EL6507
00471                                                                   EL6507
00472      EXEC CICS READNEXT                                           EL6507
00473           DATASET  (ERPLAN-FILE)                                  EL6507
00474           SET      (ADDRESS OF PLAN-MASTER)                          CL**8
00475           RIDFLD   (WS-PLAN-KEY)                                  EL6507
00476      END-EXEC.                                                    EL6507
00477                                                                   EL6507
00478  0510-READ-ERPLAN-PREV.                                           EL6507
00479                                                                   EL6507
00480      EXEC CICS READPREV                                           EL6507
00481           DATASET  (ERPLAN-FILE)                                  EL6507
00482           SET      (ADDRESS OF PLAN-MASTER)                          CL**8
00483           RIDFLD   (WS-PLAN-KEY)                                  EL6507
00484      END-EXEC                                                     EL6507
00485                                                                   EL6507
00486      IF PI-PLAN-KEY EQUAL WS-PLAN-KEY                             EL6507
00487         GO TO 0510-READ-ERPLAN-PREV.                              EL6507
00488                                                                   EL6507
00489      IF WS-PLAN-ACCT-KEY EQUAL PI-PLAN-ACCT-KEY                   EL6507
00490         MOVE WS-PLAN-KEY         TO PI-PLAN-KEY                   EL6507
00491         MOVE LOW-VALUES          TO EL6507AO                      EL6507
00492         GO TO 5000-BUILD-INITIAL-SCREEN.                          EL6507
00493                                                                   EL6507
00494  0520-END-FILE.                                                   EL6507
00495                                                                   EL6507
00496      MOVE -1                     TO MAINTL                        EL6507
00497      MOVE ER-0067                TO EMI-ERROR                     EL6507
00498      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL6507
00499      GO TO 8200-SEND-DATAONLY.                                    EL6507
00500                                                                   EL6507
00501  4000-SHOW.                                                       EL6507
00502                                                                   EL6507
00503      MOVE WS-PLAN-KEY            TO PI-PLAN-KEY                   EL6507
00504      PERFORM 7150-READ-ERPLAN THRU 7199-EXIT                      EL6507
00505                                                                   EL6507
00506      IF EMI-ERROR EQUAL ER-2938                                   EL6507
00507         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL6507
00508                                                                   EL6507
00509      IF EMI-NO-ERRORS                                             EL6507
00510         NEXT SENTENCE                                             EL6507
00511      ELSE                                                         EL6507
00512         GO TO 8100-SEND-INITIAL-MAP.                              EL6507
00513                                                                   EL6507
00514      MOVE PI-PLAN-KEY            TO WS-PLAN-KEY                   EL6507
00515      MOVE LOW-VALUES             TO EL6507AO                      EL6507
00516      GO TO 5000-BUILD-INITIAL-SCREEN.                             EL6507
00517                                                                   EL6507
00518      EJECT                                                        EL6507
00519  4100-ADD.                                                        EL6507
00520                                                                   EL6507
00521      IF NOT MODIFY-CAP                                            EL6507
00522         MOVE 'UPDATE'            TO SM-READ                       EL6507
00523         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT            EL6507
00524         MOVE ER-0070             TO EMI-ERROR                     EL6507
00525         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6507
00526         GO TO 8100-SEND-INITIAL-MAP.                              EL6507
00527                                                                   EL6507
00528      MOVE WS-PLAN-KEY            TO PI-PLAN-KEY                   EL6507
00529      PERFORM 7150-READ-ERPLAN THRU 7199-EXIT                      EL6507
00530                                                                   EL6507
00531      IF EMI-ERROR EQUAL ER-2938                                   EL6507
00532         MOVE ZEROS               TO EMI-ERROR                     EL6507
00533      ELSE                                                         EL6507
00534         MOVE ER-0132             TO EMI-ERROR                     EL6507
00535         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6507
00536         GO TO 8100-SEND-INITIAL-MAP.                              EL6507
00537                                                                   EL6507
00538      MOVE WS-PLAN-KEY            TO PI-PLAN-KEY                   EL6507
00539      PERFORM 7000-EDIT THRU 7099-EXIT                             EL6507
00540                                                                   EL6507
00541      IF EMI-NO-ERRORS                                             EL6507
00542         NEXT SENTENCE                                             EL6507
00543      ELSE                                                         EL6507
00544         IF EMI-FORCABLE OR EMI-FATAL                              EL6507
00545            GO TO 8200-SEND-DATAONLY.                              EL6507
00546                                                                   EL6507
00547      EXEC CICS GETMAIN                                            EL6507
00548           LENGTH    (ERPLAN-LENGTH)                               EL6507
00549           SET       (ADDRESS OF PLAN-MASTER)                         CL**8
00550           INITIMG   (GETMAIN-SPACE)                               EL6507
00551      END-EXEC.                                                    EL6507
00552                                                                   EL6507
00553      MOVE +0                 TO PL-ATT-AGE                        EL6507
00554                                 PL-LM-AGE (1)                     EL6507
00555                                 PL-LM-DUR (1)                     EL6507
00556                                 PL-LM-MOA (1)                     EL6507
00557                                 PL-LM-AMT (1)                     EL6507
00558                                 PL-TOL-PREM-AMT                   EL6507
00559                                 PL-TOL-REF-AMT                    EL6507
00560                                 PL-TOL-CLM-AMT                    EL6507
00561                                 PL-TOL-PREM-PCT                   EL6507
00562                                 PL-TOL-REF-PCT                    EL6507
00563                                 PL-TOL-CLM-PCT                    EL6507
00564                                 PL-POLICY-FEE                     EL6507
00565                                 PL-STATE-TAX                      EL6507
00566                                 PL-RETRO-RET                      EL6507
00567                                 PL-SALES-TAX                         CL**9
00568                                 PL-DEV-PCT.                       EL6507
00569                                                                   EL6507
00570      MOVE PL-L-LIMITS (1)    TO PL-L-LIMITS (2)                   EL6507
00571                                 PL-L-LIMITS (3)                   EL6507
00572                                 PL-L-LIMITS (4)                   EL6507
00573                                 PL-L-LIMITS (5)                   EL6507
00574                                 PL-L-LIMITS (6)                   EL6507
00575                                 PL-L-LIMITS (7)                   EL6507
00576                                 PL-L-LIMITS (8).                  EL6507
00577                                                                   EL6507
00578      MOVE 'PL'               TO PL-RECORD-ID                      EL6507
00579      MOVE PI-PLAN-KEY        TO PL-CONTROL-PRIMARY                EL6507
00580      MOVE SAVE-BIN-DATE      TO PL-LAST-MAINT-DT                  EL6507
00581      MOVE EIBTIME            TO PL-LAST-MAINT-HHMMSS              EL6507
00582      MOVE PI-PROCESSOR-ID    TO PL-LAST-MAINT-USER                EL6507
00583                                                                   EL6507
00584      PERFORM 6000-CHECK-FOR-UPDATE   THRU 6049-EXIT               EL6507
00585      PERFORM 5100-CHECK-REASONABILITY THRU 5599-EXIT              EL6507
00586      PERFORM 7500-READ-ERACCT THRU 7699-EXIT                      EL6507
00587                                                                   EL6507
00588      IF EMI-NO-ERRORS                                             EL6507
00589         NEXT SENTENCE                                             EL6507
00590      ELSE                                                         EL6507
00591         EXEC CICS FREEMAIN                                        EL6507
00592              DATA      (PLAN-MASTER)                              EL6507
00593         END-EXEC                                                  EL6507
00594         GO TO 8200-SEND-DATAONLY.                                 EL6507
00595                                                                   EL6507
00596      MOVE PLAN-MASTER            TO JP-RECORD-AREA                EL6507
00597                                                                   EL6507
00598      EXEC CICS WRITE                                              EL6507
00599          DATASET  (ERPLAN-FILE)                                   EL6507
00600          RIDFLD   (PI-PLAN-KEY)                                   EL6507
00601          FROM     (PLAN-MASTER)                                   EL6507
00602      END-EXEC.                                                    EL6507
00603                                                                   EL6507
00604      MOVE 'A'                    TO JP-RECORD-TYPE                EL6507
00605      MOVE ERPLAN-FILE            TO FILE-ID                       EL6507
00606      COMPUTE WS-JOURNAL-FILE-LENGTH EQUAL                         EL6507
00607         ERPLAN-LENGTH + JOURNAL-LENGTH.                           EL6507
00608      PERFORM 8400-LOG-JOURNAL-RECORD                              EL6507
00609                                                                   EL6507
00610      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT                EL6507
00611      MOVE ER-0000                TO EMI-ERROR                     EL6507
00612      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL6507
00613                                                                   EL6507
00614      GO TO 4000-SHOW.                                             EL6507
00615                                                                   EL6507
00616  4200-CHANGE.                                                     EL6507
00617                                                                   EL6507
00618      IF NOT MODIFY-CAP                                            EL6507
00619         MOVE 'UPDATE'            TO SM-READ                       EL6507
00620         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT            EL6507
00621         MOVE ER-0070             TO EMI-ERROR                     EL6507
00622         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6507
00623         GO TO 8100-SEND-INITIAL-MAP.                              EL6507
00624                                                                   EL6507
00625      IF PI-PLAN-KEY NOT EQUAL WS-PLAN-KEY                         EL6507
00626         MOVE ER-2056             TO EMI-ERROR                     EL6507
00627         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6507
00628         MOVE 'S'                 TO MAINTO                        EL6507
00629         MOVE -1                  TO MAINTL                        EL6507
00630         MOVE WS-PLAN-KEY         TO PI-PLAN-KEY                   EL6507
00631         GO TO 8200-SEND-DATAONLY.                                 EL6507
00632                                                                   EL6507
00633      MOVE WS-PLAN-KEY            TO PI-PLAN-KEY                   EL6507
00634      PERFORM 7000-EDIT THRU 7099-EXIT                             EL6507
00635                                                                   EL6507
00636      IF EMI-NO-ERRORS                                             EL6507
00637         NEXT SENTENCE                                             EL6507
00638      ELSE                                                         EL6507
00639         IF EMI-FORCABLE OR EMI-FATAL                              EL6507
00640            GO TO 8200-SEND-DATAONLY.                              EL6507
00641                                                                   EL6507
00642      PERFORM 7300-READ-ERPLAN-UPDATE THRU 7399-EXIT               EL6507
00643      MOVE PLAN-MASTER            TO JP-RECORD-AREA                EL6507
00644      MOVE ERPLAN-FILE            TO FILE-ID                       EL6507
00645      MOVE 'B'                    TO JP-RECORD-TYPE                EL6507
00646      COMPUTE WS-JOURNAL-FILE-LENGTH EQUAL                         EL6507
00647         ERPLAN-LENGTH + JOURNAL-LENGTH                            EL6507
00648                                                                   EL6507
00649      PERFORM 6000-CHECK-FOR-UPDATE   THRU 6049-EXIT               EL6507
00650      PERFORM 5100-CHECK-REASONABILITY THRU 5599-EXIT              EL6507
00651                                                                   EL6507
00652      IF EMI-NO-ERRORS                                             EL6507
00653         NEXT SENTENCE                                             EL6507
00654      ELSE                                                         EL6507
00655         EXEC CICS UNLOCK                                          EL6507
00656              DATASET  (ERPLAN-FILE)                               EL6507
00657         END-EXEC                                                  EL6507
00658         GO TO 8200-SEND-DATAONLY.                                 EL6507
00659                                                                   EL6507
00660      IF PL-LAST-MAINT-USER   = PI-UPDATE-BY AND                   EL6507
00661         PL-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL6507
00662         NEXT SENTENCE                                             EL6507
00663      ELSE                                                         EL6507
00664         EXEC CICS UNLOCK                                          EL6507
00665              DATASET  (ERPLAN-FILE)                               EL6507
00666         END-EXEC                                                  EL6507
00667         MOVE ER-0068            TO EMI-ERROR                      EL6507
00668         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6507
00669         PERFORM 7150-READ-ERPLAN  THRU 7199-EXIT                  EL6507
00670         MOVE LOW-VALUES         TO EL6507AO                       EL6507
00671         GO TO 5000-BUILD-INITIAL-SCREEN.                          EL6507
00672                                                                   EL6507
00673      MOVE PI-PROCESSOR-ID        TO PL-LAST-MAINT-USER            EL6507
00674      MOVE EIBTIME                TO PL-LAST-MAINT-HHMMSS          EL6507
00675      MOVE EIBDATE                TO DC-JULIAN-YYDDD               EL6507
00676      MOVE '5'                    TO DC-OPTION-CODE                EL6507
00677      MOVE LINK-ELDATCV           TO PGM-NAME                      EL6507
00678                                                                   EL6507
00679      EXEC CICS LINK                                               EL6507
00680          PROGRAM (PGM-NAME)                                       EL6507
00681          COMMAREA(DATE-CONVERSION-DATA)                           EL6507
00682          LENGTH  (DC-COMM-LENGTH)                                 EL6507
00683      END-EXEC.                                                    EL6507
00684                                                                   EL6507
00685      MOVE DC-BIN-DATE-1          TO PL-LAST-MAINT-DT              EL6507
00686                                      BIN-CURRENT-SAVE             EL6507
00687      PERFORM 8400-LOG-JOURNAL-RECORD                              EL6507
00688      MOVE PLAN-MASTER            TO JP-RECORD-AREA                EL6507
00689                                                                   EL6507
00690      EXEC CICS REWRITE                                            EL6507
00691          DATASET  (ERPLAN-FILE)                                   EL6507
00692          FROM     (PLAN-MASTER)                                   EL6507
00693      END-EXEC.                                                    EL6507
00694                                                                   EL6507
00695      MOVE 'C'                    TO JP-RECORD-TYPE                EL6507
00696      MOVE ERPLAN-FILE            TO FILE-ID                       EL6507
00697      COMPUTE WS-JOURNAL-FILE-LENGTH EQUAL                         EL6507
00698         ERPLAN-LENGTH + JOURNAL-LENGTH                            EL6507
00699      PERFORM 8400-LOG-JOURNAL-RECORD                              EL6507
00700                                                                   EL6507
00701      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT                EL6507
00702      MOVE ER-0000                TO EMI-ERROR                     EL6507
00703      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL6507
00704                                                                   EL6507
00705      PERFORM 7150-READ-ERPLAN THRU 7199-EXIT                      EL6507
00706      MOVE LOW-VALUES             TO EL6507AO                      EL6507
00707                                                                   EL6507
00708      GO TO 5000-BUILD-INITIAL-SCREEN.                             EL6507
00709                                                                   EL6507
00710      EJECT                                                        EL6507
00711                                                                   EL6507
00712  4500-DELETE.                                                     EL6507
00713                                                                   EL6507
00714      IF NOT MODIFY-CAP                                            EL6507
00715         MOVE 'UPDATE'            TO SM-READ                       EL6507
00716         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT            EL6507
00717         MOVE ER-0070             TO EMI-ERROR                     EL6507
00718         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6507
00719         GO TO 8100-SEND-INITIAL-MAP.                              EL6507
00720                                                                   EL6507
00721      IF PI-PLAN-KEY NOT EQUAL WS-PLAN-KEY                         EL6507
00722         MOVE ER-2056             TO EMI-ERROR                     EL6507
00723         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6507
00724         MOVE 'S'                 TO MAINTO                        EL6507
00725         MOVE -1                  TO MAINTL                        EL6507
00726         MOVE WS-PLAN-KEY         TO PI-PLAN-KEY                   EL6507
00727         GO TO 8200-SEND-DATAONLY.                                 EL6507
00728                                                                   EL6507
00729      PERFORM 7300-READ-ERPLAN-UPDATE THRU 7399-EXIT               EL6507
00730                                                                   EL6507
00731      IF PL-LAST-MAINT-USER   = PI-UPDATE-BY AND                   EL6507
00732         PL-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL6507
00733         NEXT SENTENCE                                             EL6507
00734      ELSE                                                         EL6507
00735         EXEC CICS UNLOCK                                          EL6507
00736              DATASET  (ERPLAN-FILE)                               EL6507
00737         END-EXEC                                                  EL6507
00738         MOVE ER-0068            TO EMI-ERROR                      EL6507
00739         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6507
00740         PERFORM 7150-READ-ERPLAN  THRU 7199-EXIT                  EL6507
00741         MOVE LOW-VALUES         TO EL6507AO                       EL6507
00742         GO TO 5000-BUILD-INITIAL-SCREEN.                          EL6507
00743                                                                   EL6507
00744      MOVE PLAN-MASTER            TO JP-RECORD-AREA                EL6507
00745      MOVE ERPLAN-FILE            TO FILE-ID                       EL6507
00746      MOVE 'D'                    TO JP-RECORD-TYPE                EL6507
00747      COMPUTE WS-JOURNAL-FILE-LENGTH EQUAL                         EL6507
00748         ERPLAN-LENGTH + JOURNAL-LENGTH                            EL6507
00749                                                                   EL6507
00750      EXEC CICS DELETE                                             EL6507
00751          DATASET  (ERPLAN-FILE)                                   EL6507
00752      END-EXEC.                                                    EL6507
00753                                                                   EL6507
00754      PERFORM 8400-LOG-JOURNAL-RECORD                              EL6507
00755      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT                EL6507
00756      MOVE ER-0000                TO EMI-ERROR                     EL6507
00757      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL6507
00758                                                                   EL6507
00759      MOVE LOW-VALUES             TO EL6507AO                      EL6507
00760      MOVE 'S'                    TO MAINTO                        EL6507
00761      GO TO 8100-SEND-INITIAL-MAP.                                 EL6507
00762                                                                   EL6507
00763  5000-BUILD-INITIAL-SCREEN.                                       EL6507
00764                                                                   EL6507
00765      MOVE 'S'                    TO MAINTO                        EL6507
00766      MOVE AL-UANON               TO MAINTA                        EL6507
00767      MOVE -1                     TO MAINTL                        EL6507
00768      MOVE PI-PLAN-CARRIER        TO CARRO                         EL6507
00769      MOVE PI-PLAN-GROUP          TO GROUPO                        EL6507
00770      MOVE PI-PLAN-STATE          TO STATEO                        EL6507
00771      MOVE PI-PLAN-ACCOUNT        TO ACCTO                         EL6507
00772      MOVE PI-PLAN-BEN-TYPE       TO TYPEO                         EL6507
00773      MOVE PI-PLAN-BEN            TO BENEO                         EL6507
00774      MOVE PI-PLAN-REVISION       TO REVISEO                       EL6507
00775                                                                   EL6507
00776      MOVE PL-LAST-MAINT-USER     TO PI-UPDATE-BY                  EL6507
00777      MOVE PL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS              EL6507
00778                                                                   EL6507
00779      IF PL-ATT-AGE NUMERIC AND                                    EL6507
00780         PL-ATT-AGE GREATER THAN ZEROS                             EL6507
00781         MOVE PL-ATT-AGE          TO ATTAGEO                          CL**6
00782         MOVE AL-UNNON            TO ATTAGEA                          CL**6
00783         MOVE +2                  TO ATTAGEL.                         CL**6
00784                                                                   EL6507
00785      IF PL-BENEFIT-TYPE EQUAL PI-LIFE-OVERRIDE-L1                 EL6507
00786         MOVE AL-SADOF            TO BENEC1A                       EL6507
00787                                     BENEC2A                       EL6507
00788                                     EL6507H-MOBENA (1)            EL6507
00789                                     EL6507H-MOBENA (2)            EL6507
00790                                     EL6507H-MOBENA (3)            EL6507
00791                                     EL6507H-MOBENA (4)            EL6507
00792                                     EL6507H-MOBENA (5)            EL6507
00793                                     EL6507H-MOBENA (6)            EL6507
00794                                     EL6507H-MOBENA (7)            EL6507
00795                                     EL6507H-MOBENA (8).           EL6507
00796                                                                   EL6507
00797      MOVE PL-CALC-METHOD         TO CALCTPO                       EL6507
00798      MOVE +1 TO SUB.                                              EL6507
00799                                                                   EL6507
00800  5010-BUILD-LIMITS.                                               EL6507
00801                                                                   EL6507
00802      IF SUB GREATER THAN +8                                       EL6507
00803         GO TO 5020-END-LIMITS.                                    EL6507
00804                                                                   EL6507
00805      IF (PL-LM-AGE (SUB) NUMERIC) AND                             EL6507
00806         (PL-LM-AGE (SUB) GREATER THAN ZEROS)                      EL6507
00807         MOVE PL-LM-AGE (SUB)     TO EL6507H-AGEI    (SUB)         EL6507
00808         MOVE AL-UNNON            TO EL6507H-AGEA    (SUB)            CL**6
00809         MOVE +2                  TO EL6507H-AGEL    (SUB)            CL**6
00810         MOVE PL-LM-DUR (SUB)     TO EL6507H-TERMI   (SUB)         EL6507
00811         MOVE AL-UNNON            TO EL6507H-TERMA   (SUB)            CL**6
00812         MOVE +3                  TO EL6507H-TERML   (SUB)            CL**6
00813         MOVE PL-LM-AMT (SUB)     TO EL6507H-TOTBENO (SUB)            CL**6
00814         MOVE AL-UANON            TO EL6507H-TOTBENA (SUB)            CL**6
00815         MOVE +7                  TO EL6507H-TOTBENL (SUB).           CL**7
00816                                                                   EL6507
00817      IF PL-BENEFIT-TYPE EQUAL PI-AH-OVERRIDE-L1                   EL6507
00818         MOVE PL-LM-MOA (SUB)     TO EL6507H-MOBENO  (SUB)            CL**6
00819         MOVE AL-UNNON            TO EL6507H-MOBENA  (SUB)            CL**6
00820         MOVE +6                  TO EL6507H-MOBENL  (SUB).           CL**7
00821                                                                   EL6507
00822      ADD +1 TO SUB                                                EL6507
00823      GO TO 5010-BUILD-LIMITS.                                     EL6507
00824                                                                   EL6507
00825  5020-END-LIMITS.                                                 EL6507
00826                                                                   EL6507
00827      MOVE PL-PREMIUM-GL          TO PRMACTO                       EL6507
00828      MOVE AL-UANON               TO PRMACTA                          CL**6
00829      MOVE +8                     TO PRMACTL                          CL**6
00830                                                                      CL**6
00831      MOVE PL-COMM-GL             TO COMACTO                       EL6507
00832      MOVE AL-UANON               TO COMACTA                          CL**6
00833      MOVE +8                     TO COMACTL                          CL**6
00834                                                                      CL**6
00835      MOVE PL-CLAIM-GL            TO CLMACTO                       EL6507
00836      MOVE AL-UANON               TO CLMACTA                          CL**6
00837      MOVE +8                     TO CLMACTL                          CL**6
00838                                                                      CL**6
00839      MOVE PL-TOL-PREM-AMT        TO PRMAMTO                       EL6507
00840      MOVE AL-UANON               TO PRMAMTA                          CL**6
00841      MOVE +6                     TO PRMAMTL                          CL**6
00842                                                                      CL**6
00843      MOVE PL-TOL-REF-AMT         TO REFAMTO                       EL6507
00844      MOVE AL-UANON               TO REFAMTA                          CL**6
00845      MOVE +6                     TO REFAMTL                          CL**6
00846                                                                      CL**6
00847      MOVE PL-TOL-CLM-AMT         TO CLMAMTO                       EL6507
00848      MOVE AL-UANON               TO CLMAMTA                          CL**6
00849      MOVE +8                     TO CLMAMTL                          CL**6
00850                                                                      CL**6
00851      MOVE PL-TOL-PREM-PCT        TO PRMPCTO                       EL6507
00852      MOVE AL-UANON               TO PRMPCTA                          CL**6
00853      MOVE +6                     TO PRMPCTL                          CL**6
00854                                                                      CL**6
00855      MOVE PL-TOL-REF-PCT         TO REFPCTO                       EL6507
00856      MOVE AL-UANON               TO REFPCTA                          CL**6
00857      MOVE +6                     TO REFPCTL                          CL**6
00858                                                                      CL**6
00859      MOVE PL-STATE-TAX           TO STTAXO                        EL6507
00860      MOVE AL-UANON               TO STTAXA                           CL**6
00861      MOVE +6                     TO STTAXL                           CL**6
00862                                                                      CL**6
00863      MOVE PL-POLICY-FEE          TO POLFEEO                       EL6507
00864      MOVE AL-UANON               TO POLFEEA                          CL**6
00865      MOVE +6                     TO POLFEEL                          CL**6
00866                                                                   EL6507
00867      IF PL-OVER-SHORT-AMT  NUMERIC                                   CL**9
00868         MOVE PL-OVER-SHORT-AMT   TO OVSAMTO                          CL**9
00869         MOVE +6                     TO OVSAMTL                       CL**9
00870         MOVE AL-UANON               TO OVSAMTA                       CL**9
00871      END-IF.                                                         CL**9
00872                                                                      CL**9
00873      IF PL-OVER-SHORT-PCT  NUMERIC                                   CL**9
00874         MOVE PL-OVER-SHORT-PCT TO OVSPCTO                            CL**9
00875         MOVE +6                     TO OVSPCTL                       CL**9
00876         MOVE AL-UANON  TO OVSPCTA                                    CL**9
00877      END-IF.                                                         CL**9
00878                                                                      CL**9
00879      IF PL-POLICY-FORM GREATER THAN SPACES                        EL6507
00880          MOVE PL-POLICY-FORM     TO POLFORMO                         CL**6
00881          MOVE AL-UANON           TO POLFORMA                         CL**6
00882          MOVE +6                 TO POLFORML.                        CL**6
00883                                                                   EL6507
00884      MOVE PL-EDIT-FOR-FORM       TO POLEDTO                       EL6507
00885      MOVE AL-UANON               TO POLEDTA                          CL**6
00886      MOVE +1                     TO POLEDTL                          CL**6
00887                                                                      CL**6
00888      MOVE PL-RETRO-RET           TO CORETO                        EL6507
00889      MOVE AL-UANON               TO CORETA                           CL**6
00890      MOVE +6                     TO CORETL                           CL**6
00891                                                                      CL**6
00892      MOVE PL-DEV-CODE            TO DEVCDEO                       EL6507
00893      IF PL-DEV-CODE NOT = SPACES                                     CL**6
00894          MOVE AL-UANON           TO DEVCDEA                          CL**6
00895          MOVE +3                 TO DEVCDEL.                         CL**6
00896                                                                      CL**6
00897      MOVE PL-DEV-PCT             TO DEVPCTO                       EL6507
00898      MOVE AL-UANON               TO DEVPCTA                          CL**6
00899      MOVE +8                     TO DEVPCTL                          CL**6
00900                                                                      CL**6
00901      MOVE PL-BENEFIT-GROUP       TO BENGRPO                       EL6507
00902      MOVE AL-UANON               TO BENGRPA                          CL**6
00903      MOVE +5                     TO BENGRPL                          CL**6
00904                                                                      CL**6
00905      MOVE PL-IG                  TO INDGRPO                       EL6507
00906                                                                      CL**6
00907      IF INDGRPI = 'I' OR 'G'                                         CL**6
00908          MOVE AL-UANON           TO INDGRPA                          CL**6
00909          MOVE +1                 TO INDGRPL.                         CL**6
00910                                                                      CL**6
00911      MOVE PL-DEV-CODE            TO PI-WS-DEV                     EL6507
00912                                                                   EL6507
00913      IF PI-WS-DEV EQUAL SPACES                                    EL6507
00914         MOVE '000'               TO PI-WS-DEV.                    EL6507
00915                                                                      CL**9
00916      MOVE PL-SALES-TAX           TO SALTAXO                          CL**9
00917      MOVE AL-UANON               TO SALTAXA                          CL**9
00918      MOVE +6                     TO SALTAXL.                         CL**9
00919                                                                   EL6507
00920      GO TO 8100-SEND-INITIAL-MAP.                                 EL6507
00921                                                                   EL6507
00922  5099-EXIT.                                                       EL6507
00923      EXIT.                                                        EL6507
00924                                                                   EL6507
00925      EJECT                                                        EL6507
00926  5100-CHECK-REASONABILITY.                                        EL6507
00927                                                                   EL6507
00928      MOVE +1                      TO SUB.                         EL6507
00929                                                                   EL6507
00930  5200-CHECK-NUMERIC.                                              EL6507
00931                                                                   EL6507
00932      IF SUB GREATER THAN +8                                       EL6507
00933         MOVE +1                   TO SUB                          EL6507
00934         GO TO 5300-LIMITS-NUMERIC.                                EL6507
00935                                                                   EL6507
00936      IF PL-LM-AGE (SUB) NOT NUMERIC                               EL6507
00937         MOVE +0                   TO PL-LM-AGE (SUB).             EL6507
00938                                                                   EL6507
00939      IF PL-LM-DUR (SUB) NOT NUMERIC                               EL6507
00940         MOVE +0                   TO PL-LM-DUR (SUB).             EL6507
00941                                                                   EL6507
00942      IF PL-LM-MOA (SUB) NOT NUMERIC                               EL6507
00943         MOVE +0                   TO PL-LM-MOA (SUB).             EL6507
00944                                                                   EL6507
00945      IF PL-LM-AMT (SUB) NOT NUMERIC                               EL6507
00946         MOVE +0                   TO PL-LM-AMT (SUB).             EL6507
00947                                                                   EL6507
00948      ADD +1 TO SUB                                                EL6507
00949      GO TO 5200-CHECK-NUMERIC.                                    EL6507
00950                                                                   EL6507
00951  5300-LIMITS-NUMERIC.                                             EL6507
00952                                                                   EL6507
00953      IF SUB GREATER THAN +8                                       EL6507
00954         MOVE +1 TO SUB                                            EL6507
00955         MOVE +2 TO SUB1                                           EL6507
00956         GO TO 5500-CONTINUE.                                      EL6507
00957                                                                   EL6507
00958      IF PL-BENEFIT-TYPE EQUAL PI-AH-OVERRIDE-L1                   EL6507
00959         GO TO 5400-AH-LIMITS.                                     EL6507
00960                                                                   EL6507
00961      IF (+0 LESS THAN PL-LM-AGE (SUB) OR                          EL6507
00962                       PL-LM-DUR (SUB) OR                          EL6507
00963                       PL-LM-AMT (SUB))                            EL6507
00964         IF (PL-LM-AGE (SUB) GREATER THAN +0) AND                  EL6507
00965            (PL-LM-DUR (SUB) GREATER THAN +0) AND                  EL6507
00966            (PL-LM-AMT (SUB) GREATER THAN +0)                      EL6507
00967            NEXT SENTENCE                                          EL6507
00968         ELSE                                                      EL6507
00969            MOVE -1              TO EL6507H-AGEL (SUB)             EL6507
00970            MOVE AL-UABON        TO EL6507H-AGEA (SUB)             EL6507
00971            MOVE ER-2106         TO EMI-ERROR                      EL6507
00972            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL6507
00973            MOVE +1 TO SUB                                         EL6507
00974            MOVE +2 TO SUB1                                        EL6507
00975            GO TO 5500-CONTINUE.                                   EL6507
00976                                                                   EL6507
00977      ADD +1 TO SUB                                                EL6507
00978      GO TO 5300-LIMITS-NUMERIC.                                   EL6507
00979                                                                   EL6507
00980  5400-AH-LIMITS.                                                  EL6507
00981                                                                   EL6507
00982      IF SUB GREATER THAN +8                                       EL6507
00983         MOVE +1 TO SUB                                            EL6507
00984         MOVE +2 TO SUB1                                           EL6507
00985         GO TO 5500-CONTINUE.                                      EL6507
00986                                                                   EL6507
00987      IF (+0 LESS THAN PL-LM-AGE (SUB) OR                          EL6507
00988                       PL-LM-DUR (SUB) OR                          EL6507
00989                       PL-LM-AMT (SUB) OR                          EL6507
00990                       PL-LM-MOA (SUB))                            EL6507
00991         IF (PL-LM-AGE (SUB) GREATER THAN +0) AND                  EL6507
00992            (PL-LM-DUR (SUB) GREATER THAN +0) AND                  EL6507
00993            (PL-LM-AMT (SUB) GREATER THAN +0) AND                  EL6507
00994            (PL-LM-MOA (SUB) GREATER THAN +0)                      EL6507
00995            NEXT SENTENCE                                          EL6507
00996         ELSE                                                      EL6507
00997            MOVE -1              TO EL6507H-AGEL (SUB)             EL6507
00998            MOVE AL-UABON        TO EL6507H-AGEA (SUB)             EL6507
00999            MOVE ER-2107         TO EMI-ERROR                      EL6507
01000            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL6507
01001            MOVE +1 TO SUB                                         EL6507
01002            MOVE +2 TO SUB1                                        EL6507
01003            GO TO 5500-CONTINUE.                                   EL6507
01004                                                                   EL6507
01005      ADD +1 TO SUB                                                EL6507
01006      GO TO 5400-AH-LIMITS.                                        EL6507
01007                                                                   EL6507
01008  5500-CONTINUE.                                                   EL6507
01009                                                                   EL6507
01010      IF EMI-NO-ERRORS                                             EL6507
01011         NEXT SENTENCE                                             EL6507
01012      ELSE                                                         EL6507
01013         GO TO 5599-EXIT.                                          EL6507
01014                                                                   EL6507
01015      IF SUB1 GREATER THAN +8                                      EL6507
01016         GO TO 5599-EXIT.                                          EL6507
01017                                                                   EL6507
01018      IF (PL-LM-AGE (SUB) GREATER THAN +0) AND                     EL6507
01019         (PL-LM-AGE (SUB1) GREATER THAN +0)                        EL6507
01020         IF PL-LM-AGE (SUB1) LESS THAN PL-LM-AGE (SUB)             EL6507
01021            MOVE -1              TO EL6507H-AGEL (SUB1)            EL6507
01022            MOVE AL-UABON        TO EL6507H-AGEA (SUB1)            EL6507
01023            MOVE ER-2274         TO EMI-ERROR                      EL6507
01024            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01025                                                                   EL6507
01026      ADD +1 TO SUB SUB1                                           EL6507
01027      GO TO 5500-CONTINUE.                                         EL6507
01028                                                                   EL6507
01029  5599-EXIT.                                                       EL6507
01030      EXIT.                                                        EL6507
01031                                                                   EL6507
01032      EJECT                                                        EL6507
01033  6000-CHECK-FOR-UPDATE.                                           EL6507
01034                                                                   EL6507
01035      IF ATTAGEL GREATER THAN +0                                   EL6507
01036         MOVE ATTAGEI              TO PL-ATT-AGE.                  EL6507
01037                                                                   EL6507
01038      MOVE +1 TO SUB.                                              EL6507
01039                                                                   EL6507
01040  6010-CHECK-LIMITS.                                               EL6507
01041                                                                   EL6507
01042      IF SUB GREATER THAN +8                                       EL6507
01043         GO TO 6020-END-CHECK-LIMITS.                              EL6507
01044                                                                   EL6507
01045      IF EL6507H-AGEL (SUB) GREATER +0                             EL6507
01046         MOVE EL6507H-AGEI (SUB)   TO PL-LM-AGE (SUB).             EL6507
01047                                                                   EL6507
01048      IF EL6507H-TERML (SUB) GREATER +0                            EL6507
01049         MOVE EL6507H-TERMI (SUB)  TO PL-LM-DUR (SUB).             EL6507
01050                                                                   EL6507
01051      IF EL6507H-MOBENL (SUB) GREATER +0                           EL6507
01052         MOVE WS-MOBENI (SUB)      TO PL-LM-MOA (SUB).             EL6507
01053                                                                   EL6507
01054      IF EL6507H-TOTBENL (SUB) GREATER +0                          EL6507
01055         MOVE WS-TOTBENI (SUB)     TO PL-LM-AMT (SUB).             EL6507
01056                                                                   EL6507
01057      ADD +1 TO SUB                                                EL6507
01058      GO TO 6010-CHECK-LIMITS.                                     EL6507
01059                                                                   EL6507
01060  6020-END-CHECK-LIMITS.                                           EL6507
01061                                                                   EL6507
01062      IF PRMACTL GREATER THAN +0                                   EL6507
01063         MOVE PRMACTI              TO PL-PREMIUM-GL.               EL6507
01064                                                                   EL6507
01065      IF COMACTL GREATER THAN +0                                   EL6507
01066         MOVE COMACTI              TO PL-COMM-GL.                  EL6507
01067                                                                   EL6507
01068      IF CLMACTL GREATER THAN +0                                   EL6507
01069         MOVE CLMACTI              TO PL-CLAIM-GL.                 EL6507
01070                                                                   EL6507
01071      IF PRMAMTL GREATER THAN +0                                   EL6507
01072         MOVE WS-PRMAMTI           TO PL-TOL-PREM-AMT.             EL6507
01073                                                                   EL6507
01074      IF REFAMTL GREATER THAN +0                                   EL6507
01075         MOVE WS-REFAMTI           TO PL-TOL-REF-AMT.              EL6507
01076                                                                   EL6507
01077      IF OVSAMTL > +0                                                 CL**9
01078         MOVE WS-OVSAMTI TO PL-OVER-SHORT-AMT                         CL**9
01079      END-IF.                                                         CL**9
01080                                                                      CL**9
01081      IF CLMAMTL GREATER THAN +0                                   EL6507
01082         MOVE WS-CLMAMTI           TO PL-TOL-CLM-AMT.              EL6507
01083                                                                   EL6507
01084      IF PRMPCTL GREATER THAN +0                                   EL6507
01085         MOVE WS-PRMPCTI           TO PL-TOL-PREM-PCT.             EL6507
01086                                                                   EL6507
01087      IF REFPCTL GREATER THAN +0                                   EL6507
01088         MOVE WS-REFPCTI           TO PL-TOL-REF-PCT.              EL6507
01089                                                                      CL**9
01090      IF OVSPCTL > +0                                                 CL**9
01091         MOVE WS-OVSPCTI TO PL-OVER-SHORT-PCT                         CL**9
01092      END-IF.                                                         CL**9
01093                                                                   EL6507
01094      IF POLFEEL GREATER THAN +0                                   EL6507
01095         MOVE WS-POLFEEI           TO PL-POLICY-FEE.               EL6507
01096                                                                   EL6507
01097      IF STTAXL GREATER THAN +0                                    EL6507
01098         MOVE WS-STTAXI            TO PL-STATE-TAX.                EL6507
01099                                                                   EL6507
01100      IF INDGRPL GREATER THAN +0                                   EL6507
01101         MOVE INDGRPI              TO PL-IG.                       EL6507
01102                                                                   EL6507
01103      IF CORETL GREATER THAN +0                                    EL6507
01104         MOVE WS-CORETI            TO PL-RETRO-RET.                EL6507
01105                                                                   EL6507
01106      IF POLFORML GREATER THAN +0                                  EL6507
01107         MOVE POLFORMI             TO PL-POLICY-FORM.              EL6507
01108                                                                   EL6507
01109      IF POLEDTL GREATER THAN +0                                   EL6507
01110         MOVE POLEDTI              TO PL-EDIT-FOR-FORM.            EL6507
01111                                                                   EL6507
01112      IF DEVCDEL GREATER THAN +0                                   EL6507
01113         MOVE DEVCDEI              TO PL-DEV-CODE.                 EL6507
01114                                                                   EL6507
01115      IF DEVPCTL GREATER THAN +0                                   EL6507
01116         MOVE WS-DEVPCTI           TO PL-DEV-PCT.                  EL6507
01117                                                                   EL6507
01118      IF CALCTPL GREATER THAN +0                                   EL6507
01119         MOVE CALCTPI              TO PL-CALC-METHOD.              EL6507
01120                                                                   EL6507
01121      IF BENGRPL GREATER THAN +0                                   EL6507
01122         MOVE BENGRPI              TO PL-BENEFIT-GROUP.            EL6507
01123                                                                      CL**9
01124      IF SALTAXL GREATER THAN +0                                      CL**9
01125         MOVE WS-SALTAXI           TO PL-SALES-TAX.                   CL**9
01126                                                                   EL6507
01127  6049-EXIT.                                                       EL6507
01128      EXIT.                                                        EL6507
01129      EJECT                                                        EL6507
01130                                                                   EL6507
01131  6100-CREATE-TEMP-STORAGE.                                        EL6507
01132                                                                   EL6507
01133      PERFORM 6300-DELETE-TEMP-STORAGE THRU 6300-EXIT              EL6507
01134                                                                   EL6507
01135      EXEC CICS WRITEQ TS                                          EL6507
01136          QUEUE   (QID1)                                           EL6507
01137          FROM    (EL6507AO)                                       EL6507
01138          LENGTH  (WS-MAP-LENGTH)                                  EL6507
01139      END-EXEC.                                                    EL6507
01140                                                                   EL6507
01141      EXEC CICS WRITEQ TS                                          EL6507
01142          QUEUE   (QID2)                                           EL6507
01143          FROM    (PROGRAM-INTERFACE-BLOCK)                        EL6507
01144          LENGTH  (WS-COMM-LENGTH)                                 EL6507
01145      END-EXEC.                                                    EL6507
01146                                                                   EL6507
01147  6100-EXIT.                                                       EL6507
01148       EXIT.                                                       EL6507
01149                                                                   EL6507
01150  6200-RECOVER-TEMP-STORAGE.                                       EL6507
01151      EXEC CICS READQ TS                                           EL6507
01152          QUEUE   (QID1)                                           EL6507
01153          INTO    (EL6507AO)                                       EL6507
01154          LENGTH  (WS-MAP-LENGTH)                                  EL6507
01155      END-EXEC.                                                    EL6507
01156                                                                   EL6507
01157      EXEC CICS READQ TS                                           EL6507
01158          QUEUE   (QID2)                                           EL6507
01159          INTO    (PROGRAM-INTERFACE-BLOCK)                        EL6507
01160          LENGTH  (WS-COMM-LENGTH)                                 EL6507
01161      END-EXEC.                                                    EL6507
01162                                                                   EL6507
01163      PERFORM 6300-DELETE-TEMP-STORAGE THRU 6300-EXIT.             EL6507
01164                                                                   EL6507
01165  6200-EXIT.                                                       EL6507
01166       EXIT.                                                       EL6507
01167                                                                   EL6507
01168  6300-DELETE-TEMP-STORAGE.                                        EL6507
01169                                                                   EL6507
01170      EXEC CICS HANDLE CONDITION                                   EL6507
01171          QIDERR  (6300-EXIT)                                      EL6507
01172      END-EXEC.                                                    EL6507
01173                                                                   EL6507
01174      EXEC CICS DELETEQ TS                                         EL6507
01175          QUEUE  (QID1)                                            EL6507
01176      END-EXEC.                                                    EL6507
01177                                                                   EL6507
01178      EXEC CICS DELETEQ TS                                         EL6507
01179          QUEUE  (QID2)                                            EL6507
01180      END-EXEC.                                                    EL6507
01181                                                                   EL6507
01182  6300-EXIT.                                                       EL6507
01183       EXIT.                                                       EL6507
01184                                                                   EL6507
01185      EJECT                                                        EL6507
01186  7000-EDIT.                                                       EL6507
01187                                                                   EL6507
01188      IF ATTAGEL GREATER THAN +0                                   EL6507
01189         IF ATTAGEI NUMERIC                                        EL6507
01190            MOVE AL-UNNON          TO ATTAGEA                      EL6507
01191         ELSE                                                      EL6507
01192            MOVE -1                TO ATTAGEL                      EL6507
01193            MOVE AL-UNBON          TO ATTAGEA                      EL6507
01194            MOVE ER-2273           TO EMI-ERROR                    EL6507
01195            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01196                                                                   EL6507
01197      MOVE +1 TO SUB.                                              EL6507
01198                                                                   EL6507
01199  7010-EDIT-LIMITS.                                                EL6507
01200                                                                   EL6507
01201      IF SUB GREATER THAN +8                                       EL6507
01202         GO TO 7020-END-EDIT-LIMITS.                               EL6507
01203                                                                   EL6507
01204      IF (EL6507H-AGEL (SUB) EQUAL +0) AND                         EL6507
01205         (EL6507H-TERML (SUB) EQUAL +0) AND                        EL6507
01206         (EL6507H-MOBENL (SUB) EQUAL +0) AND                       EL6507
01207         (EL6507H-TOTBENL (SUB) EQUAL +0)                          EL6507
01208         ADD +1 TO SUB                                             EL6507
01209         GO TO 7010-EDIT-LIMITS.                                   EL6507
01210                                                                   EL6507
01211      IF EL6507H-AGEL (SUB) GREATER +0                             EL6507
01212         IF EL6507H-AGEI (SUB) NUMERIC                             EL6507
01213            MOVE AL-UNNON          TO EL6507H-AGEA (SUB)           EL6507
01214         ELSE                                                      EL6507
01215            MOVE -1                TO EL6507H-AGEL (SUB)           EL6507
01216            MOVE AL-UNBON          TO EL6507H-AGEA (SUB)           EL6507
01217            MOVE ER-2275           TO EMI-ERROR                    EL6507
01218            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01219                                                                   EL6507
01220      IF EL6507H-TERML (SUB) GREATER +0                            EL6507
01221         IF EL6507H-TERMI (SUB) NUMERIC                            EL6507
01222            MOVE AL-UNNON          TO EL6507H-TERMA (SUB)          EL6507
01223         ELSE                                                      EL6507
01224            MOVE -1                TO EL6507H-TERML (SUB)          EL6507
01225            MOVE AL-UNBON          TO EL6507H-TERMA (SUB)          EL6507
01226            MOVE ER-2276           TO EMI-ERROR                    EL6507
01227            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01228                                                                   EL6507
01229      IF EL6507H-MOBENL (SUB) GREATER +0                           EL6507
01230         MOVE EL6507H-MOBENI (SUB) TO DEEDIT-FIELD                 EL6507
01231         PERFORM 8600-DEEDIT                                       EL6507
01232         IF DEEDIT-FIELD-V0 NUMERIC                                EL6507
01233            MOVE DEEDIT-FIELD-V0   TO WS-MOBENI (SUB)              EL6507
01234                                      EL6507H-MOBENO (SUB)         EL6507
01235            MOVE AL-UANON          TO EL6507H-MOBENA (SUB)         EL6507
01236         ELSE                                                      EL6507
01237            MOVE -1                TO EL6507H-MOBENL (SUB)         EL6507
01238            MOVE AL-UABON          TO EL6507H-MOBENA (SUB)         EL6507
01239            MOVE ER-3127           TO EMI-ERROR                    EL6507
01240            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01241                                                                   EL6507
01242      IF EL6507H-TOTBENL (SUB) GREATER +0                          EL6507
01243         MOVE EL6507H-TOTBENI (SUB) TO DEEDIT-FIELD                EL6507
01244         PERFORM 8600-DEEDIT                                       EL6507
01245         IF DEEDIT-FIELD-V0 NUMERIC                                EL6507
01246            MOVE DEEDIT-FIELD-V0   TO WS-TOTBENI (SUB)             EL6507
01247                                      EL6507H-TOTBENO (SUB)        EL6507
01248            MOVE AL-UANON          TO EL6507H-TOTBENA (SUB)        EL6507
01249         ELSE                                                      EL6507
01250            MOVE -1                TO EL6507H-TOTBENL (SUB)        EL6507
01251            MOVE AL-UABON          TO EL6507H-TOTBENA (SUB)        EL6507
01252            MOVE ER-3128           TO EMI-ERROR                    EL6507
01253            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01254                                                                   EL6507
01255      ADD +1 TO SUB                                                EL6507
01256      GO TO 7010-EDIT-LIMITS.                                      EL6507
01257                                                                   EL6507
01258  7020-END-EDIT-LIMITS.                                            EL6507
01259                                                                   EL6507
01260      IF PRMPCTL GREATER THAN +0                                   EL6507
01261         MOVE PRMPCTI              TO DEEDIT-FIELD                 EL6507
01262         PERFORM 8600-DEEDIT                                       EL6507
01263         IF DEEDIT-FIELD-V4 NUMERIC                                EL6507
01264            MOVE DEEDIT-FIELD-V4   TO WS-PRMPCTI                   EL6507
01265                                      PRMPCTO                      EL6507
01266            MOVE AL-UANON          TO PRMPCTA                      EL6507
01267         ELSE                                                      EL6507
01268            MOVE -1                TO PRMPCTL                      EL6507
01269            MOVE AL-UABON          TO PRMPCTA                      EL6507
01270            MOVE ER-2168           TO EMI-ERROR                    EL6507
01271            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01272                                                                   EL6507
01273      IF PRMAMTL GREATER THAN +0                                   EL6507
01274         MOVE PRMAMTI              TO DEEDIT-FIELD                 EL6507
01275         PERFORM 8600-DEEDIT                                       EL6507
01276         IF DEEDIT-FIELD-V2 NUMERIC                                EL6507
01277            MOVE DEEDIT-FIELD-V2   TO WS-PRMAMTI                   EL6507
01278                                      PRMAMTO                      EL6507
01279            MOVE AL-UANON          TO PRMAMTA                      EL6507
01280         ELSE                                                      EL6507
01281            MOVE -1                TO PRMAMTL                      EL6507
01282            MOVE AL-UABON          TO PRMAMTA                      EL6507
01283            MOVE ER-2168           TO EMI-ERROR                    EL6507
01284            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01285                                                                   EL6507
01286      IF REFPCTL GREATER THAN +0                                   EL6507
01287         MOVE REFPCTI              TO DEEDIT-FIELD                 EL6507
01288         PERFORM 8600-DEEDIT                                       EL6507
01289         IF DEEDIT-FIELD-V4 NUMERIC                                EL6507
01290            MOVE DEEDIT-FIELD-V4   TO WS-REFPCTI                   EL6507
01291                                      REFPCTO                      EL6507
01292            MOVE AL-UANON          TO REFPCTA                      EL6507
01293         ELSE                                                      EL6507
01294            MOVE -1                TO REFPCTL                      EL6507
01295            MOVE AL-UABON          TO REFPCTA                      EL6507
01296            MOVE ER-2169           TO EMI-ERROR                    EL6507
01297            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01298                                                                   EL6507
01299      IF REFAMTL GREATER THAN +0                                   EL6507
01300         MOVE REFAMTI              TO DEEDIT-FIELD                 EL6507
01301         PERFORM 8600-DEEDIT                                       EL6507
01302         IF DEEDIT-FIELD-V2 NUMERIC                                EL6507
01303            MOVE DEEDIT-FIELD-V2   TO WS-REFAMTI                   EL6507
01304                                      REFAMTO                      EL6507
01305            MOVE AL-UANON          TO REFAMTA                      EL6507
01306         ELSE                                                      EL6507
01307            MOVE -1                TO REFAMTL                      EL6507
01308            MOVE AL-UABON          TO REFAMTA                      EL6507
01309            MOVE ER-2169           TO EMI-ERROR                    EL6507
01310            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01311                                                                   EL6507
01312      IF OVSPCTL GREATER THAN +0                                      CL**9
01313         MOVE OVSPCTI              TO  DEEDIT-FIELD                   CL**9
01314         PERFORM 8600-DEEDIT                                          CL**9
01315         IF DEEDIT-FIELD-V4 NUMERIC                                   CL**9
01316            MOVE DEEDIT-FIELD-V4   TO WS-OVSPCTI                      CL**9
01317                                      OVSPCTO                         CL**9
01318            MOVE AL-UANON          TO OVSPCTA                         CL**9
01319         ELSE                                                         CL**9
01320            MOVE -1                TO OVSPCTL                         CL**9
01321            MOVE AL-UABON          TO OVSPCTA                         CL**9
01322            MOVE ER-2169           TO EMI-ERROR                       CL**9
01323            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**9
01324                                                                      CL**9
01325      IF OVSAMTL GREATER THAN +0                                      CL**9
01326         MOVE OVSAMTI              TO  DEEDIT-FIELD                   CL**9
01327         PERFORM 8600-DEEDIT                                          CL**9
01328         IF DEEDIT-FIELD-V2 NUMERIC                                   CL**9
01329            MOVE DEEDIT-FIELD-V2   TO WS-OVSAMTI                      CL**9
01330                                      OVSAMTO                         CL**9
01331            MOVE AL-UANON          TO OVSAMTA                         CL**9
01332         ELSE                                                         CL**9
01333            MOVE -1                TO OVSAMTL                         CL**9
01334            MOVE AL-UABON          TO OVSAMTA                         CL**9
01335            MOVE ER-2169           TO EMI-ERROR                       CL**9
01336            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**9
01337                                                                      CL**9
01338      IF DEVPCTI GREATER THAN +0                                      CL**9
01339         MOVE DEVPCTI              TO DEEDIT-FIELD                    CL**9
01340         PERFORM 8600-DEEDIT                                          CL**9
01341         IF DEEDIT-FIELD-V6 NUMERIC                                   CL**9
01342            MOVE DEEDIT-FIELD-V5   TO WS-DEVPCTI                      CL**9
01343                                      DEVPCTO                         CL**9
01344            MOVE AL-UANON          TO DEVPCTA                         CL**9
01345         ELSE                                                         CL**9
01346            MOVE -1                TO DEVPCTL                         CL**9
01347            MOVE AL-UABON          TO DEVPCTA                         CL**9
01348            MOVE ER-3126           TO EMI-ERROR                       CL**9
01349            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**9
01350                                                                      CL**9
01351      IF CLMAMTL GREATER THAN +0                                   EL6507
01352         MOVE CLMAMTI              TO DEEDIT-FIELD                 EL6507
01353         PERFORM 8600-DEEDIT                                       EL6507
01354         IF DEEDIT-FIELD-V2 NUMERIC                                EL6507
01355            MOVE DEEDIT-FIELD-V2   TO WS-CLMAMTI                   EL6507
01356                                      CLMAMTO                      EL6507
01357            MOVE AL-UANON          TO CLMAMTA                      EL6507
01358         ELSE                                                      EL6507
01359            MOVE -1                TO CLMAMTL                      EL6507
01360            MOVE AL-UABON          TO CLMAMTA                      EL6507
01361            MOVE ER-2170           TO EMI-ERROR                    EL6507
01362            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01363                                                                   EL6507
01364      IF STTAXL GREATER THAN +0                                    EL6507
01365         MOVE STTAXI               TO DEEDIT-FIELD                 EL6507
01366         PERFORM 8600-DEEDIT                                       EL6507
01367         IF DEEDIT-FIELD-V4 NUMERIC                                EL6507
01368            MOVE DEEDIT-FIELD-V4   TO WS-STTAXI                    EL6507
01369                                      STTAXO                       EL6507
01370            MOVE AL-UANON          TO STTAXA                       EL6507
01371         ELSE                                                      EL6507
01372            MOVE -1                TO STTAXL                       EL6507
01373            MOVE AL-UABON          TO STTAXA                       EL6507
01374            MOVE ER-7208           TO EMI-ERROR                    EL6507
01375            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01376                                                                   EL6507
01377      IF POLFEEL GREATER THAN +0                                   EL6507
01378         MOVE POLFEEI              TO DEEDIT-FIELD                 EL6507
01379         PERFORM 8600-DEEDIT                                       EL6507
01380         IF DEEDIT-FIELD-V3 NUMERIC                                EL6507
01381            MOVE DEEDIT-FIELD-V3   TO WS-POLFEEI                   EL6507
01382                                      POLFEEO                      EL6507
01383            MOVE AL-UANON          TO POLFEEA                      EL6507
01384         ELSE                                                      EL6507
01385            MOVE -1                TO POLFEEL                      EL6507
01386            MOVE AL-UABON          TO POLFEEA                      EL6507
01387            MOVE ER-9407           TO EMI-ERROR                    EL6507
01388            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01389                                                                   EL6507
01390      IF INDGRPL GREATER THAN +0                                   EL6507
01391         IF INDGRPI EQUAL 'I' OR 'G'                               EL6507
01392            MOVE AL-UANON          TO INDGRPA                      EL6507
01393         ELSE                                                      EL6507
01394            MOVE -1                TO INDGRPL                      EL6507
01395            MOVE AL-UABON          TO INDGRPA                      EL6507
01396            MOVE ER-2152           TO EMI-ERROR                    EL6507
01397            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01398                                                                   EL6507
01399      IF POLEDTL GREATER +0                                        EL6507
01400         IF POLEDTI EQUAL 'Y' OR 'N' OR ' '                        EL6507
01401            MOVE AL-UANON          TO POLEDTA                      EL6507
01402         ELSE                                                      EL6507
01403            MOVE -1                TO POLEDTL                      EL6507
01404            MOVE AL-UABON          TO POLEDTA                      EL6507
01405            MOVE ER-0627           TO EMI-ERROR                    EL6507
01406            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01407                                                                   EL6507
01408      IF (POLEDTL GREATER THAN +0) AND                             EL6507
01409         (POLEDTI EQUAL 'Y')                                       EL6507
01410         IF POLFORML EQUAL +0                                      EL6507
01411            MOVE -1                TO POLFORML                     EL6507
01412            MOVE AL-UNBON          TO POLFORMA                     EL6507
01413            MOVE ER-2937           TO EMI-ERROR                    EL6507
01414            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01415                                                                   EL6507
01416      IF CORETL GREATER THAN +0                                    EL6507
01417         MOVE CORETI               TO DEEDIT-FIELD                 EL6507
01418         PERFORM 8600-DEEDIT                                       EL6507
01419         IF DEEDIT-FIELD-V4 NUMERIC                                EL6507
01420            MOVE DEEDIT-FIELD-V4   TO WS-CORETI                    EL6507
01421                                      CORETO                       EL6507
01422            MOVE AL-UANON          TO CORETA                       EL6507
01423         ELSE                                                      EL6507
01424            MOVE -1                TO CORETL                       EL6507
01425            MOVE AL-UABON          TO CORETA                       EL6507
01426            MOVE ER-9055           TO EMI-ERROR                    EL6507
01427            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01428                                                                   EL6507
01429      IF SALTAXL GREATER THAN +0                                      CL**9
01430         MOVE SALTAXI              TO DEEDIT-FIELD                    CL**9
01431         PERFORM 8600-DEEDIT                                       EL6507
01432         IF DEEDIT-FIELD-V4 NUMERIC                                   CL**9
01433            MOVE DEEDIT-FIELD-V4   TO WS-SALTAXI                      CL**9
01434                                      SALTAXO                         CL**9
01435            MOVE AL-UANON          TO SALTAXA                         CL**9
01436         ELSE                                                      EL6507
01437            MOVE -1                TO SALTAXL                         CL**9
01438            MOVE AL-UABON          TO SALTAXA                         CL**9
01439            MOVE ER-7208           TO EMI-ERROR                       CL**9
01440            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL6507
01441                                                                   EL6507
01442  7030-CHECK-LF-DEVIATION.                                         EL6507
01443                                                                   EL6507
01444      IF (DEVCDEL GREATER THAN +0) AND                             EL6507
01445         (TYPEI EQUAL PI-LIFE-OVERRIDE-L1)                         EL6507
01446         MOVE AL-UANON             TO DEVCDEA                      EL6507
01447      ELSE                                                         EL6507
01448         GO TO 7040-CHECK-AH-DEVIATION.                            EL6507
01449                                                                   EL6507
01450      MOVE LOW-VALUES             TO RATE-KEY                      EL6507
01451                                                                   EL6507
01452      MOVE PI-COMPANY-CD          TO RATE-COMP-CD                  EL6507
01453      MOVE PI-ACCT-STATE          TO RATE-STATE                    EL6507
01454      MOVE ZEROS                  TO RATE-CLASS                    EL6507
01455      MOVE DEVCDEI                TO RATE-DEV                      EL6507
01456                                                                   EL6507
01457      EXEC CICS HANDLE CONDITION                                   EL6507
01458          NOTFND   (7039-LIFE-DEV-ERROR)                           EL6507
01459      END-EXEC.                                                    EL6507
01460                                                                   EL6507
01461      EXEC CICS READ                                               EL6507
01462          GTEQ                                                     EL6507
01463          DATASET   (ERRATE-FILE)                                  EL6507
01464          SET       (ADDRESS OF RATE-RECORD)                          CL**8
01465          RIDFLD    (RATE-KEY)                                     EL6507
01466      END-EXEC.                                                    EL6507
01467                                                                   EL6507
01468      IF RT-COMPANY-CD = RATE-COMP-CD AND                          EL6507
01469         RT-ST-CODE    = RATE-STATE   AND                          EL6507
01470         RT-ST-CLASS   = RATE-CLASS   AND                          EL6507
01471         RT-ST-DEV     = RATE-DEV                                  EL6507
01472            GO TO 7040-CHECK-AH-DEVIATION.                         EL6507
01473                                                                   EL6507
01474  7039-LIFE-DEV-ERROR.                                             EL6507
01475                                                                   EL6507
01476      MOVE -1                     TO DEVCDEL                       EL6507
01477      MOVE AL-UABON               TO DEVCDEA                       EL6507
01478      MOVE ER-2154                TO EMI-ERROR                     EL6507
01479      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6507
01480                                                                   EL6507
01481  7040-CHECK-AH-DEVIATION.                                         EL6507
01482                                                                   EL6507
01483      IF (DEVCDEL GREATER THAN +0) AND                             EL6507
01484         (TYPEI EQUAL PI-AH-OVERRIDE-L1)                           EL6507
01485         MOVE AL-UANON             TO DEVCDEA                      EL6507
01486      ELSE                                                         EL6507
01487         GO TO 7099-EXIT.                                          EL6507
01488                                                                   EL6507
01489      MOVE LOW-VALUES             TO RATE-KEY                      EL6507
01490                                                                   EL6507
01491      MOVE PI-COMPANY-CD          TO RATE-COMP-CD                  EL6507
01492      MOVE PI-ACCT-STATE          TO RATE-STATE                    EL6507
01493      MOVE ZEROS                  TO RATE-CLASS                    EL6507
01494      MOVE DEVCDEI                TO RATE-DEV                      EL6507
01495                                                                   EL6507
01496      EXEC CICS HANDLE CONDITION                                   EL6507
01497          NOTFND   (7049-AH-DEV-ERROR)                             EL6507
01498      END-EXEC.                                                    EL6507
01499                                                                   EL6507
01500      EXEC CICS READ                                               EL6507
01501          GTEQ                                                     EL6507
01502          DATASET   (ERRATE-FILE)                                  EL6507
01503          SET       (ADDRESS OF RATE-RECORD)                          CL**8
01504          RIDFLD    (RATE-KEY)                                     EL6507
01505      END-EXEC.                                                    EL6507
01506                                                                   EL6507
01507      IF RT-COMPANY-CD = RATE-COMP-CD AND                          EL6507
01508         RT-ST-CODE    = RATE-STATE   AND                          EL6507
01509         RT-ST-CLASS   = RATE-CLASS   AND                          EL6507
01510         RT-ST-DEV     = RATE-DEV                                  EL6507
01511            GO TO 7099-EXIT.                                       EL6507
01512                                                                   EL6507
01513  7049-AH-DEV-ERROR.                                               EL6507
01514                                                                   EL6507
01515      MOVE -1                     TO DEVCDEL                       EL6507
01516      MOVE AL-UABON               TO DEVCDEA                       EL6507
01517      MOVE ER-2154                TO EMI-ERROR                     EL6507
01518      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6507
01519                                                                   EL6507
01520  7099-EXIT.                                                       EL6507
01521      EXIT.                                                        EL6507
01522      EJECT                                                        EL6507
01523                                                                   EL6507
01524  7150-READ-ERPLAN.                                                EL6507
01525                                                                   EL6507
01526      EXEC CICS HANDLE CONDITION                                   EL6507
01527          NOTFND   (7120-NOTFND)                                   EL6507
01528      END-EXEC.                                                    EL6507
01529                                                                   EL6507
01530      EXEC CICS READ                                               EL6507
01531           DATASET  (ERPLAN-FILE)                                  EL6507
01532           SET      (ADDRESS OF PLAN-MASTER)                          CL**8
01533           RIDFLD   (PI-PLAN-KEY)                                  EL6507
01534      END-EXEC.                                                    EL6507
01535                                                                   EL6507
01536      MOVE PL-LAST-MAINT-USER     TO PI-UPDATE-BY                  EL6507
01537      MOVE PL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             EL6507
01538                                                                      CL**9
01539      IF PL-SALES-TAX NOT NUMERIC                                     CL**9
01540          MOVE ZEROS              TO PL-SALES-TAX.                    CL**9
01541                                                                   EL6507
01542      GO TO 7199-EXIT.                                             EL6507
01543                                                                   EL6507
01544  7120-NOTFND.                                                     EL6507
01545                                                                   EL6507
01546      MOVE -1                     TO MAINTL                        EL6507
01547      MOVE ER-2938                TO EMI-ERROR.                    EL6507
01548 *PEM PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL6507
01549                                                                   EL6507
01550  7199-EXIT.                                                       EL6507
01551      EXIT.                                                        EL6507
01552      EJECT                                                        EL6507
01553                                                                   EL6507
01554  7200-DEEDIT.                                                     EL6507
01555                                                                   EL6507
01556      EXEC CICS BIF                                                EL6507
01557           DEEDIT                                                  EL6507
01558           FIELD  (DEEDIT-FIELD)                                   EL6507
01559           LENGTH (15)                                             EL6507
01560      END-EXEC.                                                    EL6507
01561                                                                   EL6507
01562  7200-EXIT.                                                       EL6507
01563      EXIT.                                                        EL6507
01564      EJECT                                                        EL6507
01565  7300-READ-ERPLAN-UPDATE.                                         EL6507
01566                                                                   EL6507
01567      EXEC CICS HANDLE CONDITION                                      CL**3
01568          NOTFND   (7320-NOTFND)                                      CL**3
01569      END-EXEC.                                                       CL**3
01570                                                                      CL**3
01571      EXEC CICS READ                                               EL6507
01572           DATASET  (ERPLAN-FILE)                                  EL6507
01573           SET      (ADDRESS OF PLAN-MASTER)                          CL**8
01574           RIDFLD   (PI-PLAN-KEY)                                  EL6507
01575           UPDATE                                                  EL6507
01576      END-EXEC.                                                    EL6507
01577                                                                      CL**3
01578      GO TO 7399-EXIT.                                                CL**3
01579                                                                      CL**3
01580  7320-NOTFND.                                                        CL**3
01581                                                                      CL**3
01582      MOVE -1                     TO MAINTL                           CL**3
01583      MOVE ER-2938                TO EMI-ERROR.                       CL**3
01584      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                        CL**3
01585      GO TO 8200-SEND-DATAONLY.                                       CL**3
01586                                                                   EL6507
01587  7399-EXIT.                                                       EL6507
01588      EXIT.                                                        EL6507
01589      EJECT                                                        EL6507
01590  7400-BUILD-KEY.                                                  EL6507
01591                                                                   EL6507
01592      MOVE PI-COMPANY-CD          TO WS-PLAN-COMPANY-CD            EL6507
01593                                                                   EL6507
01594      IF CARRL GREATER THAN +0                                     EL6507
01595         MOVE CARRI               TO WS-PLAN-CARRIER.              EL6507
01596                                                                   EL6507
01597      IF GROUPL GREATER THAN +0                                    EL6507
01598         MOVE GROUPI              TO WS-PLAN-GROUP.                EL6507
01599                                                                   EL6507
01600      IF STATEL GREATER THAN +0                                    EL6507
01601         MOVE STATEI              TO WS-PLAN-STATE.                EL6507
01602                                                                   EL6507
01603      IF ACCTL GREATER THAN +0                                     EL6507
01604         MOVE ACCTI               TO WS-PLAN-ACCOUNT.              EL6507
01605                                                                   EL6507
01606      IF TYPEL GREATER THAN +0                                     EL6507
01607         MOVE TYPEI               TO WS-PLAN-BEN-TYPE.             EL6507
01608                                                                   EL6507
01609      IF BENEL GREATER THAN +0                                     EL6507
01610         MOVE BENEI               TO WS-PLAN-BEN.                  EL6507
01611                                                                   EL6507
01612      IF REVISEL GREATER THAN +0                                   EL6507
01613         MOVE REVISEI             TO WS-PLAN-REVISION.             EL6507
01614                                                                   EL6507
01615      IF (REVISEL GREATER THAN +0) AND                             EL6507
01616         (REVISEI EQUAL SPACES)                                    EL6507
01617         MOVE ER-2948             TO EMI-ERROR                        CL**2
01618         MOVE -1                  TO REVISEL                       EL6507
01619         MOVE AL-UABON            TO REVISEA                       EL6507
01620         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6507
01621         GO TO 8200-SEND-DATAONLY.                                 EL6507
01622                                                                   EL6507
01623  7499-EXIT.                                                       EL6507
01624      EXIT.                                                        EL6507
01625      EJECT                                                        EL6507
01626  7500-READ-ERACCT.                                                EL6507
01627                                                                   EL6507
01628      EXEC CICS HANDLE CONDITION                                   EL6507
01629          NOTFND   (7555-NOTFND)                                      CL**8
01630      END-EXEC.                                                    EL6507
01631                                                                   EL6507
01632      EXEC CICS READ                                               EL6507
01633           DATASET  (ERACCT-FILE)                                  EL6507
01634           SET      (ADDRESS OF ACCOUNT-MASTER)                       CL**8
01635           RIDFLD   (PI-ACCT-KEY)                                  EL6507
01636      END-EXEC.                                                    EL6507
01637                                                                   EL6507
01638      MOVE +0                    TO SUB2.                             CL**8
01639                                                                   EL6507
01640  7520-CHECK-IF-EQUAL.                                                CL**8
01641                                                                   EL6507
01642      ADD +1                     TO SUB2                              CL**8
01643                                                                   EL6507
01644      IF SUB2 GREATER THAN +20                                     EL6507
01645         IF PI-COMPANY-ID EQUAL 'DMD'                                 CL**8
01646             MOVE +0                 TO SUB2                          CL**8
01647             GO TO 7525-CHECK-EXTRA                                   CL**8
01648         ELSE                                                         CL**8
01649             GO TO 7555-NOTFND.                                       CL**8
01650                                                                   EL6507
01651      IF PL-BENEFIT-TYPE EQUAL AM-BENEFIT-TYPE (SUB2) AND          EL6507
01652         PL-BENEFIT-CODE EQUAL AM-BENEFIT-CODE (SUB2) AND          EL6507
01653         PL-REVISION-NO  EQUAL AM-BENEFIT-REVISION  (SUB2)         EL6507
01654         GO TO 7699-EXIT                                           EL6507
01655      ELSE                                                         EL6507
01656         GO TO 7520-CHECK-IF-EQUAL.                                   CL**8
01657                                                                   EL6507
01658  7525-CHECK-EXTRA.                                                   CL**8
01659                                                                      CL**8
01660      ADD +1                     TO SUB2                              CL**8
01661                                                                      CL**8
01662      IF SUB2 GREATER THAN +30                                        CL**8
01663         GO TO 7555-NOTFND.                                           CL**8
01664                                                                      CL**8
01665      IF PL-BENEFIT-TYPE EQUAL AM-BENEFIT-DMD-TYPE (SUB2) AND         CL**8
01666         PL-BENEFIT-CODE EQUAL AM-BENEFIT-DMD-CODE (SUB2) AND         CL**8
01667         PL-REVISION-NO  EQUAL AM-BENEFIT-DMD-REVISION  (SUB2)        CL**8
01668         GO TO 7699-EXIT                                              CL**8
01669      ELSE                                                            CL**8
01670         GO TO 7525-CHECK-EXTRA.                                      CL**8
01671                                                                      CL**8
01672  7555-NOTFND.                                                        CL**8
01673                                                                   EL6507
01674      MOVE -1                     TO MAINTL                        EL6507
01675      MOVE ER-2939                TO EMI-ERROR                     EL6507
01676      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6507
01677                                                                   EL6507
01678  7699-EXIT.                                                       EL6507
01679      EXIT.                                                        EL6507
01680      EJECT                                                        EL6507
01681                                                                   EL6507
01682  7800-COMPANY-REC-READ.                                           EL6507
01683                                                                   EL6507
01684      MOVE SPACES                 TO ELCNTL-KEY                    EL6507
01685      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID                  EL6507
01686      MOVE '1'                    TO CNTL-REC-TYPE                 EL6507
01687      MOVE +0                     TO CNTL-SEQ-NO                   EL6507
01688                                                                   EL6507
01689      EXEC CICS HANDLE CONDITION                                   EL6507
01690          NOTFND   (7880-NO-COMP)                                  EL6507
01691      END-EXEC.                                                    EL6507
01692                                                                   EL6507
01693      EXEC CICS READ                                               EL6507
01694          DATASET   (ELCNTL-FILE)                                  EL6507
01695          SET       (ADDRESS OF CONTROL-FILE)                         CL**8
01696          RIDFLD    (ELCNTL-KEY)                                   EL6507
01697      END-EXEC.                                                    EL6507
01698                                                                   EL6507
01699      IF CF-ACCOUNT-MSTR-MAINT-DT = LOW-VALUES                     EL6507
01700          MOVE ER-2572            TO EMI-ERROR                     EL6507
01701          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL6507
01702                                                                   EL6507
01703      GO TO 7899-EXIT.                                             EL6507
01704                                                                   EL6507
01705  7880-NO-COMP.                                                    EL6507
01706                                                                   EL6507
01707      MOVE ER-0002                TO EMI-ERROR                     EL6507
01708      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6507
01709                                                                   EL6507
01710  7899-EXIT.                                                       EL6507
01711      EXIT.                                                        EL6507
01712      EJECT                                                        EL6507
01713  8000-UPDATE-MAINT-DATE.                                          EL6507
01714                                                                   EL6507
01715      MOVE SPACES                 TO ELCNTL-KEY                    EL6507
01716                                                                   EL6507
01717      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID                  EL6507
01718      MOVE '1'                    TO CNTL-REC-TYPE                 EL6507
01719      MOVE +0                     TO CNTL-SEQ-NO                   EL6507
01720                                                                   EL6507
01721      EXEC CICS HANDLE CONDITION                                   EL6507
01722          NOTFND   (8000-EXIT)                                     EL6507
01723      END-EXEC.                                                    EL6507
01724                                                                   EL6507
01725      EXEC CICS READ                                               EL6507
01726          UPDATE                                                   EL6507
01727          DATASET   (ELCNTL-FILE)                                  EL6507
01728          SET       (ADDRESS OF CONTROL-FILE)                         CL**8
01729          RIDFLD    (ELCNTL-KEY)                                   EL6507
01730      END-EXEC.                                                    EL6507
01731                                                                   EL6507
01732      MOVE CONTROL-FILE           TO JP-RECORD-AREA                EL6507
01733      MOVE 'B'                    TO JP-RECORD-TYPE                EL6507
01734      COMPUTE WS-JOURNAL-FILE-LENGTH EQUAL                         EL6507
01735           ELCNTL-LENGTH + JOURNAL-LENGTH                          EL6507
01736      MOVE ELCNTL-FILE            TO FILE-ID                       EL6507
01737      PERFORM 8400-LOG-JOURNAL-RECORD                              EL6507
01738                                                                   EL6507
01739      MOVE BIN-CURRENT-SAVE       TO CF-ACCOUNT-MSTR-MAINT-DT      EL6507
01740                                                                   EL6507
01741      MOVE CONTROL-FILE           TO JP-RECORD-AREA                EL6507
01742      MOVE 'C'                    TO JP-RECORD-TYPE                EL6507
01743      MOVE ELCNTL-FILE            TO FILE-ID                       EL6507
01744                                                                   EL6507
01745      EXEC CICS REWRITE                                            EL6507
01746          DATASET   (ELCNTL-FILE)                                  EL6507
01747          FROM      (CONTROL-FILE)                                 EL6507
01748      END-EXEC.                                                    EL6507
01749                                                                   EL6507
01750      COMPUTE WS-JOURNAL-FILE-LENGTH EQUAL                         EL6507
01751           ELCNTL-LENGTH + JOURNAL-LENGTH                          EL6507
01752      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6507
01753                                                                   EL6507
01754  8000-EXIT.                                                       EL6507
01755       EXIT.                                                       EL6507
01756      EJECT                                                        EL6507
01757                                                                   EL6507
01758  8100-SEND-INITIAL-MAP.                                           EL6507
01759                                                                   EL6507
01760      MOVE SAVE-DATE              TO DATEO                         EL6507
01761      MOVE EIBTIME                TO TIME-IN                       EL6507
01762      MOVE TIME-OUT               TO TIMEO                         EL6507
101101     MOVE PI-COMPANY-ID          TO CMPNYIDO
101101     MOVE PI-PROCESSOR-ID        TO USERIDO
01763      MOVE -1                     TO MAINTL                        EL6507
01764      MOVE PI-PLAN-CARRIER        TO CARRO                         EL6507
01765      MOVE PI-PLAN-GROUP          TO GROUPO                        EL6507
01766      MOVE PI-PLAN-STATE          TO STATEO                        EL6507
01767      MOVE PI-PLAN-ACCOUNT        TO ACCTO                         EL6507
01768      MOVE PI-PLAN-BEN-TYPE       TO TYPEO                         EL6507
01769      MOVE PI-PLAN-BEN            TO BENEO                         EL6507
01770      MOVE PI-PLAN-REVISION       TO REVISEO                       EL6507
01771      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO                       EL6507
01772      MOVE AL-SABOF               TO ERRMSGA                       EL6507
01773                                                                   EL6507
01774      EXEC CICS SEND                                               EL6507
01775          MAP      (MAP-NAME)                                      EL6507
01776          MAPSET   (MAPSET-NAME)                                   EL6507
01777          FROM     (EL6507AO)                                      EL6507
01778          ERASE                                                    EL6507
01779          CURSOR                                                   EL6507
01780      END-EXEC.                                                    EL6507
01781                                                                   EL6507
01782      GO TO 9100-RETURN-TRAN.                                      EL6507
01783                                                                   EL6507
01784  8200-SEND-DATAONLY.                                              EL6507
01785                                                                   EL6507
01786      MOVE SAVE-DATE              TO DATEO                         EL6507
01787      MOVE EIBTIME                TO TIME-IN                       EL6507
01788      MOVE TIME-OUT               TO TIMEO                         EL6507
101101     MOVE PI-COMPANY-ID          TO CMPNYIDO
101101     MOVE PI-PROCESSOR-ID        TO USERIDO
01789      MOVE PI-PLAN-CARRIER        TO CARRO                         EL6507
01790      MOVE PI-PLAN-GROUP          TO GROUPO                        EL6507
01791      MOVE PI-PLAN-STATE          TO STATEO                        EL6507
01792      MOVE PI-PLAN-ACCOUNT        TO ACCTO                         EL6507
01793      MOVE PI-PLAN-BEN-TYPE       TO TYPEO                         EL6507
01794      MOVE PI-PLAN-BEN            TO BENEO                         EL6507
01795      MOVE PI-PLAN-REVISION       TO REVISEO                       EL6507
01796      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO                       EL6507
01797      MOVE AL-SABOF               TO ERRMSGA                       EL6507
01798                                                                   EL6507
01799      EXEC CICS SEND                                               EL6507
01800          MAP      (MAP-NAME)                                      EL6507
01801          MAPSET   (MAPSET-NAME)                                   EL6507
01802          FROM     (EL6507AO)                                      EL6507
01803          DATAONLY                                                 EL6507
01804          CURSOR                                                   EL6507
01805      END-EXEC.                                                    EL6507
01806                                                                   EL6507
01807      GO TO 9100-RETURN-TRAN.                                      EL6507
01808                                                                   EL6507
01809  8300-SEND-TEXT.                                                  EL6507
01810                                                                   EL6507
01811      EXEC CICS SEND TEXT                                          EL6507
01812          FROM     (LOGOFF-TEXT)                                   EL6507
01813          LENGTH   (LOGOFF-LENGTH)                                 EL6507
01814          ERASE                                                    EL6507
01815          FREEKB                                                   EL6507
01816      END-EXEC.                                                    EL6507
01817                                                                   EL6507
01818      EXEC CICS RETURN                                             EL6507
01819      END-EXEC.                                                    EL6507
01820                                                                   EL6507
01821  8400-LOG-JOURNAL-RECORD.                                         EL6507
01822                                                                   EL6507
01823      MOVE PI-PROCESSOR-ID        TO JP-USER-ID                    EL6507
01824      MOVE FILE-ID                TO JP-FILE-ID                    EL6507
01825      MOVE THIS-PGM               TO JP-PROGRAM-ID.                EL6507
01826                                                                   EL6507
pemuni*    IF PI-JOURNAL-FILE-ID NOT = ZERO                             EL6507
pemuni*        EXEC CICS JOURNAL                                        EL6507
pemuni*            JFILEID     (PI-JOURNAL-FILE-ID)                     EL6507
pemuni*            JTYPEID     ('ER')                                   EL6507
pemuni*            FROM        (JOURNAL-RECORD)                         EL6507
pemuni*            LENGTH      (WS-JOURNAL-FILE-LENGTH)                 EL6507
pemuni*        END-EXEC.                                                EL6507
01834                                                                   EL6507
01835  8600-DEEDIT.                                                     EL6507
01836                                                                   EL6507
01837      EXEC CICS BIF DEEDIT                                         EL6507
01838           FIELD(DEEDIT-FIELD)                                     EL6507
01839           LENGTH(15)                                              EL6507
01840      END-EXEC.                                                    EL6507
01841                                                                   EL6507
01842  8800-UNAUTHORIZED-ACCESS.                                        EL6507
01843                                                                   EL6507
01844      MOVE UNACCESS-MSG           TO LOGOFF-MSG                    EL6507
01845      GO TO 8300-SEND-TEXT.                                        EL6507
01846                                                                   EL6507
01847  8810-PF23.                                                       EL6507
01848                                                                   EL6507
01849      MOVE EIBAID                 TO PI-ENTRY-CD-1                 EL6507
01850      MOVE XCTL-005               TO PGM-NAME                      EL6507
01851      GO TO 9300-XCTL.                                             EL6507
01852                                                                   EL6507
01853  9000-RETURN-CICS.                                                EL6507
01854                                                                   EL6507
01855      EXEC CICS RETURN                                             EL6507
01856      END-EXEC.                                                    EL6507
01857                                                                   EL6507
01858  9100-RETURN-TRAN.                                                EL6507
01859                                                                   EL6507
01860      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO              EL6507
01861      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO          EL6507
01862                                                                   EL6507
01863      EXEC CICS RETURN                                             EL6507
01864          TRANSID    (TRANS-ID)                                    EL6507
01865          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6507
01866          LENGTH     (WS-COMM-LENGTH)                              EL6507
01867      END-EXEC.                                                    EL6507
01868                                                                   EL6507
01869  9200-RETURN-MAIN-MENU.                                           EL6507
01870                                                                   EL6507
01871      MOVE XCTL-626               TO PGM-NAME                      EL6507
01872      GO TO 9300-XCTL.                                             EL6507
01873                                                                   EL6507
01874  9300-XCTL.                                                       EL6507
01875                                                                   EL6507
01876      EXEC CICS XCTL                                               EL6507
01877          PROGRAM    (PGM-NAME)                                    EL6507
01878          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6507
01879          LENGTH     (WS-COMM-LENGTH)                              EL6507
01880      END-EXEC.                                                    EL6507
01881                                                                   EL6507
01882  9400-CLEAR.                                                      EL6507
01883                                                                   EL6507
01884      MOVE XCTL-6505              TO PGM-NAME                      EL6507
01885      GO TO 9300-XCTL.                                             EL6507
01886                                                                   EL6507
01887  9500-PF12.                                                       EL6507
01888                                                                   EL6507
01889      MOVE XCTL-010               TO PGM-NAME                      EL6507
01890      GO TO 9300-XCTL.                                             EL6507
01891                                                                   EL6507
01892  9600-PGMID-ERROR.                                                EL6507
01893                                                                   EL6507
01894      EXEC CICS HANDLE CONDITION                                   EL6507
01895          PGMIDERR    (8300-SEND-TEXT)                             EL6507
01896      END-EXEC.                                                    EL6507
01897                                                                   EL6507
01898      MOVE PGM-NAME               TO PI-CALLING-PROGRAM            EL6507
01899      MOVE ' '                    TO PI-ENTRY-CD-1                 EL6507
01900      MOVE XCTL-005               TO PGM-NAME                      EL6507
01901      MOVE PGM-NAME               TO LOGOFF-PGM                    EL6507
01902      MOVE PGMIDERR-MSG           TO LOGOFF-FILL                   EL6507
01903      GO TO 9300-XCTL.                                             EL6507
01904                                                                   EL6507
01905  9700-LINK-DATE-CONVERT.                                          EL6507
01906                                                                   EL6507
01907      EXEC CICS LINK                                               EL6507
01908          PROGRAM    ('ELDATCV')                                   EL6507
01909          COMMAREA   (DATE-CONVERSION-DATA)                        EL6507
01910          LENGTH     (DC-COMM-LENGTH)                              EL6507
01911      END-EXEC.                                                    EL6507
01912                                                                   EL6507
01913  9700-EXIT.                                                       EL6507
01914      EXIT.                                                        EL6507
01915                                                                   EL6507
01916  9900-ERROR-FORMAT.                                               EL6507
01917                                                                   EL6507
01918      IF NOT EMI-ERRORS-COMPLETE                                   EL6507
01919          MOVE LINK-001           TO PGM-NAME                      EL6507
01920          EXEC CICS LINK                                           EL6507
01921              PROGRAM    (PGM-NAME)                                EL6507
01922              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL6507
01923              LENGTH     (EMI-COMM-LENGTH)                         EL6507
01924          END-EXEC.                                                EL6507
01925                                                                   EL6507
01926  9900-EXIT.                                                       EL6507
01927      EXIT.                                                        EL6507
01928                                                                   EL6507
01929  9990-ABEND.                                                      EL6507
01930                                                                   EL6507
01931      MOVE LINK-004               TO PGM-NAME                      EL6507
01932      MOVE DFHEIBLK               TO EMI-LINE1                     EL6507
01933      EXEC CICS LINK                                               EL6507
01934          PROGRAM   (PGM-NAME)                                     EL6507
01935          COMMAREA  (EMI-LINE1)                                    EL6507
01936          LENGTH    (72)                                           EL6507
01937      END-EXEC.                                                    EL6507
01938                                                                   EL6507
01939      MOVE -1                     TO MAINTL                        EL6507
01940      GO TO 8200-SEND-DATAONLY                                     EL6507
01941      GOBACK.                                                      EL6507
01942                                                                   EL6507
01943  9995-SECURITY-VIOLATION.                                         EL6507
01944             COPY ELCSCTP.                                         EL6507
01945  9995-EXIT.                                                       EL6507
01946       EXIT.                                                       EL6507
