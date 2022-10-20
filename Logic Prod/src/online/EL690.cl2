00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL690
00003  PROGRAM-ID.                 EL690 .                                 LV005
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/12/96 10:00:53.                    CL**5
00007 *                            VMOD=2.005                              CL**5
00008 *                                                                 EL690
00008 *                                                                 EL690
00009 *AUTHOR.        LOGIC,INC.                                           CL**5
00010 *               DALLAS, TEXAS.                                       CL**5
00011                                                                   EL690
00012 *DATE-COMPILED.                                                      CL**5
00013                                                                   EL690
00014 *SECURITY.   *****************************************************   CL**5
00015 *            *                                                   *   CL**5
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**5
00017 *            *                                                   *   CL**5
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**5
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**5
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**5
00021 *            *                                                   *   CL**5
00022 *            *****************************************************   CL**5
00023 *                                                                 EL690
00024 *REMARKS.                                                         EL690
00025 *       TRANSACTION - EXM3 - CREDIT SYSTEM CORRESPONDENCE REVIEW  EL690
00023 *                                                                 EL690
101101******************************************************************
101101*    0012447420    C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL690AI FILLER
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
070711* 070711    2011022800001  AJRA  NAPERSOFT CHANGES
012312* 012312    2011022800001  AJRA  NAPERSOFT CHANGES
101512* 101512    2011022800001  AJRA  FIX LIST FOR SAME CERT DIFF EFF DT
121112* 121112    2012101700002  AJRA  ADD PF6 TO CERT NOTES
102020* 102020 IR2020101300001   PEMA  Correct billing notes
101101******************************************************************

00026                                                                   EL690
00027  ENVIRONMENT DIVISION.                                            EL690
00028  DATA DIVISION.                                                   EL690
00029                                  EJECT                            EL690
00030  WORKING-STORAGE SECTION.                                         EL690
00031  77  FILLER   PIC  X(32) VALUE '*******************************'. EL690
00032  77  FILLER   PIC  X(32) VALUE '*    EL690 WORKING STORAGE    *'. EL690
00033  77  FILLER   PIC  X(32) VALUE '********* VMOD=2.005 **********'.    CL**5
00034                                                                   EL690
00035  01  W-PROGRAM-CONSTANTS.                                         EL690
00036      12  FILLER                  PIC  X(18)                       EL690
00037                                  VALUE 'PROGRAM CONSTANTS:'.      EL690
00038      12  W-APPL-SCRTY-NDX        PIC S9(04)  VALUE +3    COMP.    EL690
00039                                                                   EL690
00040      12  W-ARCH-FILE-ID          PIC  X(08)  VALUE 'ERARCH'.      EL690
00041      12  W-ARCH2-FILE-ID         PIC  X(08)  VALUE 'ERARCH2'.     EL690
00042      12  W-ARCH3-FILE-ID         PIC  X(08)  VALUE 'ERARCH3'.     EL690
00043      12  W-ARCH4-FILE-ID         PIC  X(08)  VALUE 'ERARCH4'.     EL690
00044      12  W-ARCH5-FILE-ID         PIC  X(08)  VALUE 'ERARCH5'.     EL690
00045      12  W-ARCH6-FILE-ID         PIC  X(08)  VALUE 'ERARCH6'.     EL690
00046      12  W-ARCT-FILE-ID          PIC  X(08)  VALUE 'ERARCT'.      EL690
00047      12  W-CNTL-FILE-ID          PIC  X(08)  VALUE 'ELCNTL'.      EL690
00048      12  W-LINK-001              PIC  X(08)  VALUE 'EL001'.       EL690
00049      12  W-LINK-004              PIC  X(08)  VALUE 'EL004'.       EL690
00050      12  W-MAP                   PIC  X(08)  VALUE 'EL690A'.      EL690
00051      12  W-MAP-REDEFINE  REDEFINES   W-MAP.                       EL690
00052          16  FILLER              PIC  X(02).                      EL690
00053          16  W-MAP-NUM           PIC  X(06).                      EL690
00054      12  W-MAPSET                PIC  X(08)  VALUE 'EL690S'.      EL690
00055      12  W-THIS-PGM              PIC  X(08)  VALUE 'EL690'.       EL690
00056      12  W-TRANSACTION           PIC  X(04)  VALUE 'EXM3'.        EL690
00057      12  W-XCTL-005              PIC  X(08)  VALUE 'EL005'.       EL690
00058      12  W-XCTL-010              PIC  X(08)  VALUE 'EL010'.       EL690
00059      12  W-XCTL-153              PIC  X(08)  VALUE 'EL690'.       EL690
00060      12  W-XCTL-626              PIC  X(08)  VALUE 'EL626'.       EL690
00061      12  W-XCTL-689              PIC  X(08)  VALUE 'EL689'.       EL690
070711     12  W-XCTL-691              PIC  X(08)  VALUE 'EL691'.
121112     12  W-XCTL-1279             PIC  X(08)  VALUE 'EL1279'.
00062                                                                   EL690
00063  01  W-WORK-AREAS.                                                EL690
00064      12  FILLER                  PIC  X(11)                       EL690
00065                                       VALUE 'WORK AREAS:'.        EL690
00066                                                                   EL690
00067      12  W-ARCH-NDX              PIC S9(04)  COMP.                EL690
00068      12  W-ARCH-NDX2             PIC S9(04)  COMP.                EL690
00069      12  W-ARCH-NDX3             PIC S9(04)  COMP.                EL690
00070      12  W-ARCH-NUMBER           PIC S9(08)  COMP.                EL690
00071      12  W-HOLD-ARCHIVE          PIC S9(08)  COMP.                EL690
00072      12  W-KEY-NDX               PIC S9(04)  COMP.                EL690
00073      12  W-LAST-ERROR            PIC  9(04)  VALUE 9999.          EL690
00074                                                                   EL690
00075      12  W-CALL-PGM              PIC  X(08)  VALUE SPACES.        EL690
00076                                                                   EL690
00077      12  W-DEEDIT-FIELD          PIC  X(15).                      EL690
00078      12  W-DEEDIT-FIELD-V0 REDEFINES W-DEEDIT-FIELD               EL690
00079                                  PIC S9(15).                      EL690
00080                                                                   EL690
00081      12  W-HOLD-LINE             PIC X(100).                      EL690
00082      12  W-RETURNED-FROM         PIC  X(08)  VALUE SPACES.        EL690
00083      12  W-RESEND-DATE-1         PIC  X(02)  VALUE SPACES.        EL690
00084      12  W-RESEND-DATE-2         PIC  X(02)  VALUE SPACES.        EL690
00085      12  W-RESEND-DATE-3         PIC  X(02)  VALUE SPACES.        EL690
00086      12  W-REPLY-DATE            PIC  X(02)  VALUE SPACES.        EL690
00087      12  W-SAVE-BIN-DATE         PIC  X(02)  VALUE SPACES.        EL690
00088      12  W-SAVE-DATE             PIC  X(08)  VALUE SPACES.        EL690
00089      12  W-STATUS                PIC  X(01)  VALUE SPACES.        EL690
00090                                                                   EL690
00091      12  W-TIME-IN               PIC S9(07).                      EL690
00092      12  FILLER REDEFINES W-TIME-IN.                              EL690
00093          16  FILLER              PIC  X(01).                      EL690
00094          16  W-TIME-OUT          PIC  9(02)V9(02).                EL690
00095          16  FILLER              PIC  X(02).                      EL690
00096                                                                   EL690
00097  01  W-KEY-FIELDS.                                                EL690
00098      12  FILLER                  PIC  X(11)                       EL690
00099                                       VALUE 'KEY FIELDS:'.        EL690
00100      12  W-CNTL-KEY.                                              EL690
00101          16  W-CNTL-COMPANY-ID   PIC  X(03).                      EL690
00102          16  W-CNTL-RECORD-TYPE  PIC  X(01).                      EL690
00103          16  FILLER              PIC  X(04).                      EL690
00104          16  W-CNTL-SEQ          PIC S9(04) COMP.                 EL690
00105      12  W-QUID-KEY.                                              EL690
00106          16  W-QUID-TERMINAL     PIC  X(04).                      EL690
00107          16  W-QUID-MAP-NUM      PIC  X(04).                      EL690
00108 *                                                                 EL690
00109 *    12  W-SC-QUID-KEY.                                           EL690
00110 *        16  W-SC-QUID-TERMINAL  PIC  X(04).                      EL690
00111 *        16  W-SC-QUID-SYSTEM    PIC  X(04).                      EL690
00112                                                                   EL690
00113      12  W-ARCH-KEY.                                              EL690
00114          16  W-ARCH-COMPANY-CD   PIC  X(01).                      EL690
00115          16  W-ARCH-ARCHIVE-NO   PIC S9(08) COMP.                 EL690
00116                                                                   EL690
00117      12  W-ARCH2-KEY.                                             EL690
00118          16  W-ARCH2-COMPANY-CD  PIC  X(01).                      EL690
00119          16  W-ARCH2-CSCSAE-KEYLET.                               EL690
00120              20  W-ARCH2-CERT-PRIME                               EL690
00121                                  PIC  X(10).                      EL690
00122              20  W-ARCH2-SUFFIX  PIC  X(01).                      EL690
00123              20  W-ARCH2-CARRIER PIC  X(01).                      EL690
00124              20  W-ARCH2-GROUPING                                 EL690
00125                                  PIC  X(06).                      EL690
00126              20  W-ARCH2-STATE   PIC  X(02).                      EL690
00127              20  W-ARCH2-ACCOUNT PIC  X(10).                      EL690
00128              20  W-ARCH2-EFF-DTE PIC  X(02).                      EL690
00129          16  W-ARCH2-ARCHIVE-NO  PIC S9(08) COMP.                 EL690
00130                                                                   EL690
00131      12  W-ARCH3-KEY.                                             EL690
00132          16  W-ARCH3-COMPANY-CD  PIC  X(01).                      EL690
00133          16  W-ARCH3-FCGSA-KEYLET.                                EL690
00134              20  W-ARCH3-FORM    PIC  X(04).                      EL690
00135              20  W-ARCH3-CARRIER PIC  X(01).                      EL690
00136              20  W-ARCH3-GROUPING                                 EL690
00137                                  PIC  X(06).                      EL690
00138              20  W-ARCH3-STATE   PIC  X(02).                      EL690
00139              20  W-ARCH3-ACCOUNT PIC  X(10).                      EL690
00140          16  W-ARCH3-ARCHIVE-NO  PIC S9(08) COMP.                 EL690
00141                                                                   EL690
00142      12  W-ARCH4-KEY.                                             EL690
00143          16  W-ARCH4-COMPANY-CD  PIC  X(01).                      EL690
00144          16  W-ARCH4-PCGSA-KEYLET.                                EL690
00145              20  W-ARCH4-PROCESSOR                                EL690
00146                                  PIC  X(04).                      EL690
00147              20  W-ARCH4-CARRIER PIC  X(01).                      EL690
00148              20  W-ARCH4-GROUPING                                 EL690
00149                                  PIC  X(06).                      EL690
00150              20  W-ARCH4-STATE   PIC  X(02).                      EL690
00151              20  W-ARCH4-ACCOUNT PIC  X(10).                      EL690
00152          16  W-ARCH4-ARCHIVE-NO  PIC S9(08) COMP.                 EL690
00153                                                                   EL690
00154      12  W-ARCH5-KEY.                                             EL690
00155          16  W-ARCH5-COMPANY-CD  PIC  X(01).                      EL690
00156          16  W-ARCH5-CGSA-KEYLET.                                 EL690
00157              20  W-ARCH5-CARRIER PIC  X(01).                      EL690
00158              20  W-ARCH5-GROUPING                                 EL690
00159                                  PIC  X(06).                      EL690
00160              20  W-ARCH5-STATE   PIC  X(02).                      EL690
00161              20  W-ARCH5-ACCOUNT PIC  X(10).                      EL690
00162          16  W-ARCH5-ARCHIVE-NO  PIC S9(08) COMP.                 EL690
00163                                                                   EL690
00164      12  W-ARCH6-KEY.                                             EL690
00165          16  W-ARCH6-COMPANY-CD  PIC  X(01).                      EL690
00166          16  W-ARCH6-ENTRY.                                       EL690
00167              20  W-ARCH6-FILLER  PIC  X(02).                      EL690
00168              20  W-ARCH6-QUE-CONTROL                              EL690
00169                                  PIC S9(08) COMP.                 EL690
00170          16  W-ARCH6-ARCHIVE-NO  PIC S9(08) COMP.                 EL690
00171                                                                   EL690
00172      12  W-ARCT-KEY.                                              EL690
00173          16  W-ARCT-COMPANY-CD   PIC  X(01).                      EL690
00174          16  W-ARCT-ARCHIVE-NO   PIC S9(08) COMP.                 EL690
00175          16  W-ARCT-RECORD-TYPE  PIC  X(01).                      EL690
00176              88  W-ARCT-ADDRESS-DATA   VALUE '1'.                 EL690
00177              88  W-ARCT-TEXT-DATA      VALUE '2'.                 EL690
00178          16  W-ARCT-LINE-SEQ-NO  PIC S9(04) COMP.                 EL690
00179                                                                   EL690
00180      12  FILLER                      PIC  X(28).                  EL690
00181      12  LT-NUM-LINES-ON-RECORD      PIC S9(04)    COMP.          EL690
00182                                                                   EL690
00183      12  LT-TEXT-RECORD.                                          EL690
00184          16  LT-LETTER-TEXT OCCURS 20 TIMES                       EL690
00185                             INDEXED BY LT-NDX.                    EL690
00186              20  LT-TEXT-LINE        PIC  X(70).                  EL690
00187              20  LT-SKIP-CONTROL     PIC  X(02).                  EL690
00188                  88  LT-NO-LINES-SKIPPED             VALUE SPACES.EL690
00189                  88  LT-SKIP-TO-NEXT-PAGE            VALUE '99'.  EL690
00190              20  FILLER              PIC  X(08).                  EL690
00191                                                                   EL690
00192                                                                   EL690
00193  01  W-PROGRAM-SWITCHES.                                          EL690
00194      12  FILLER                  PIC  X(09)                       EL690
00195                                       VALUE 'SWITCHES:'.          EL690
00196      12  W-CHANGE-REQUESTED-IND  PIC  X(01)  VALUE SPACES.        EL690
00197          88  W-CHANGE-REQUESTED       VALUE 'Y'.                  EL690
00198          88  W-NO-CHANGES-MADE        VALUE SPACES.               EL690
00199      12  W-FIRST-CHANGE-IND      PIC  9(02)  VALUE ZEROS.         EL690
00200          88  W-FIRST-CHANGE-FOUND     VALUES 01 THRU 12.          EL690
00201          88  W-NO-FIRST-CHANGE        VALUE ZEROS.                EL690
00202      12  W-READ-PREV-TWICE-IND   PIC  X(01)  VALUE SPACES.        EL690
00203          88  W-READ-PREV-TWICE        VALUE 'Y'.                  EL690
00204      12  W-VALID-STATUS-SW       PIC  X(01).                      EL690
00205          88  W-ALLOWED-ENTRIES        VALUE 'A' 'H' '+' '#' '@'      CL**3
00206                                             'X' 'V'.              EL690
00207          88  W-VALID-STATUS           VALUE 'A' 'H'                  CL**4
00208                                             'P' 'X' 'V' 'C'.      EL690
00209          88  W-ACTIVE                 VALUE 'A'.                  EL690
00210          88  W-COMPLETED              VALUE 'C'.                  EL690
00211          88  W-VOIDED                 VALUE 'V'.                  EL690
00212          88  W-TO-BE-PURGED-STATUS    VALUE 'X'.                  EL690
00213          88  W-PURGED-STATUS          VALUE 'P'.                  EL690
00214          88  W-SPECIAL-STATUS         VALUE '+'.                  EL690
00215          88  W-CLEAR-INITIAL          VALUE '#'.                     CL**3
00216          88  W-CHECK-STATUS           VALUE '@'.                     CL**3
00217                                  EJECT                            EL690
00218  01  FILLER                      PIC  X(14)                       EL690
00219                                       VALUE 'PGRM MAP AREA:'.     EL690
00220      COPY EL690S.                                                 EL690
00221  01  FILLER REDEFINES EL690AI.                                    EL690
101101     16  FILLER                  PIC X(154).                         CL**3
00223      16  W-ARCH-GROUPS.                                           EL690
00224          20  W-ARCHGRP-DATA OCCURS 12 TIMES.                      EL690
00225              24  W-ARCNOL        PIC S9(04) COMP.                 EL690
00226              24  W-ARCNOA        PIC  X(01).                      EL690
00227              24  W-ARCNOO        PIC  9(08).                      EL690
070711             24  W-FORML         PIC S9(04) COMP.                 EL690
070711             24  W-FORMA         PIC  X(01).                      EL690
070711             24  W-FORMO         PIC  X(04).                      EL690
070711             24  W-USRIDL        PIC S9(04) COMP.                 EL690
070711             24  W-USRIDA        PIC  X(01).                      EL690
070711             24  W-USRIDO        PIC  X(04).                      EL690
070711             24  W-PRINTL        PIC S9(04) COMP.                 EL690
070711             24  W-PRINTA        PIC  X(01).                      EL690
070711             24  W-PRINTO        PIC  X(08).                      EL690
00243              24  W-STATUSL       PIC S9(04) COMP.                 EL690
00244              24  W-STATUSA       PIC  X(01).                      EL690
00245              24  W-STATUSO       PIC  X(08).                      EL690
070711             24  W-RSFRML        PIC S9(04) COMP.                 EL690
070711             24  W-RSFRMA        PIC  X(01).                      EL690
070711             24  W-RSFRMO        PIC  X(04).                      EL690
070711             24  W-RSDATL        PIC S9(04) COMP.                 EL690
070711             24  W-RSDATA        PIC  X(01).                      EL690
070711             24  W-RSDATO        PIC  X(08).                      EL690
070711     16  FILLER                  PIC  X(87).
00255                                  EJECT                            EL690
00256                                                                   EL690
00257  01  FILLER                      PIC  X(22)                       EL690
00258                                  VALUE 'PGRM INTERFACE STARTS:'.  EL690
00259      COPY ELCINTF.                                                EL690
00260      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   EL690
00261      COPY ELC1042.                                                EL690
00262      COPY ELC689PI.                                               EL690
00263          16  PI-690-WORK-AREA.                                    EL690
00264              20  PI-690-ARCHIVE-TABLE.                            EL690
00265                  24  PI-690-ARCHIVE-NUM OCCURS 12 TIMES           EL690
00266                                  PIC S9(08) COMP.                 EL690
00267              20  PI-690-CURSOR   PIC S9(04) COMP.                 EL690
00268              20  PI-690-FIRST-DATA.                               EL690
00269                  24  PI-690-FIRST-CERT-NO.                        EL690
00270                      28  PI-690-FIRST-CERT-PRIME                  EL690
00271                                  PIC  X(10).                      EL690
00272                      28  PI-690-FIRST-SUFFIX                      EL690
00273                                  PIC  X(01).                      EL690
00274                  24  PI-690-FIRST-CARRIER                         EL690
00275                                  PIC  X(01).                      EL690
00276                  24  PI-690-FIRST-GROUPING                        EL690
00277                                  PIC  X(06).                      EL690
00278                  24  PI-690-FIRST-STATE                           EL690
00279                                  PIC  X(02).                      EL690
00280                  24  PI-690-FIRST-ACCOUNT                         EL690
00281                                  PIC  X(10).                      EL690
00282                  24  PI-690-FIRST-EFFECT-DATE                     EL690
00283                                  PIC  X(02).                      EL690
00284                  24  PI-690-FIRST-ENTRY.                          EL690
00285                      28  PI-690-FIRST-CONTROL-PREFIX              EL690
00286                                  PIC  X(02).                      EL690
00287                      28  PI-690-FIRST-CONTROL                     EL690
00288                                  PIC S9(08) COMP.                 EL690
00289                  24  PI-690-FIRST-FORM                            EL690
00290                                  PIC  X(04).                      EL690
00291                  24  PI-690-FIRST-PROCESSOR                       EL690
00292                                  PIC  X(04).                      EL690
00293                  24  PI-690-FIRST-ARCHIVE-NO                      EL690
00294                                  PIC S9(08) COMP.                 EL690
00295              20  PI-690-INIT-DATA.                                EL690
00296                  24  PI-690-INIT-CERT-NO.                         EL690
00297                      28  PI-690-INIT-CERT-PRIME                   EL690
00298                                  PIC  X(10).                      EL690
00299                      28  PI-690-INIT-SUFFIX                       EL690
00300                                  PIC  X(01).                      EL690
00301                  24  PI-690-INIT-CARRIER                          EL690
00302                                  PIC  X(01).                      EL690
00303                  24  PI-690-INIT-GROUPING                         EL690
00304                                  PIC  X(06).                      EL690
00305                  24  PI-690-INIT-STATE                            EL690
00306                                  PIC  X(02).                      EL690
00307                  24  PI-690-INIT-ACCOUNT                          EL690
00308                                  PIC  X(10).                      EL690
00309                  24  PI-690-INIT-EFFECT-DATE                      EL690
00310                                  PIC  X(02).                      EL690
00311                  24  PI-690-INIT-EFF-DTE                          EL690
00312                                  PIC  X(08).                      EL690
00313                  24  PI-690-INIT-ENTRY.                           EL690
00314                      28  PI-690-INIT-CONTROL-PREFIX               EL690
00315                                  PIC  X(02).                      EL690
00316                      28  PI-690-INIT-CONTROL                      EL690
00317                                  PIC S9(08) COMP.                 EL690
00318                  24  PI-690-INIT-FORM                             EL690
00319                                  PIC  X(04).                      EL690
00320                  24  PI-690-INIT-PROCESSOR                        EL690
00321                                  PIC  X(04).                      EL690
00322                  24  PI-690-INIT-ARCHIVE-NO                       EL690
00323                                  PIC S9(08) COMP.                 EL690
00324              20  PI-690-LAST-DATA.                                EL690
00325                  24  PI-690-LAST-CERT-NO.                         EL690
00326                      28  PI-690-LAST-CERT-PRIME                   EL690
00327                                  PIC  X(10).                      EL690
00328                      28  PI-690-LAST-SUFFIX                       EL690
00329                                  PIC  X(01).                      EL690
00330                  24  PI-690-LAST-CARRIER                          EL690
00331                                  PIC  X(01).                      EL690
00332                  24  PI-690-LAST-GROUPING                         EL690
00333                                  PIC  X(06).                      EL690
00334                  24  PI-690-LAST-STATE                            EL690
00335                                  PIC  X(02).                      EL690
00336                  24  PI-690-LAST-ACCOUNT                          EL690
00337                                  PIC  X(10).                      EL690
00338                  24  PI-690-LAST-EFFECT-DATE                      EL690
00339                                  PIC  X(02).                      EL690
00340                  24  PI-690-LAST-ENTRY.                           EL690
00341                      28  PI-690-LAST-CONTROL-PREFIX               EL690
00342                                  PIC  X(02).                      EL690
00343                      28  PI-690-LAST-CONTROL                      EL690
00344                                  PIC S9(08) COMP.                 EL690
00345                  24  PI-690-LAST-FORM                             EL690
00346                                  PIC  X(04).                      EL690
00347                  24  PI-690-LAST-PROCESSOR                        EL690
00348                                  PIC  X(04).                      EL690
00349                  24  PI-690-LAST-ARCHIVE-NO                       EL690
00350                                  PIC S9(08) COMP.                 EL690
00351              20  PI-690-LAST-ARCH-NDX                             EL690
00352                                  PIC S9(04) COMP.                 EL690
00353              20  PI-690-BRWS-TYPE-IND                             EL690
00354                                  PIC  9(01).                      EL690
00355                  88  PI-690-BRWS-CERTRP               VALUE 1.    EL690
00356                  88  PI-690-BRWS-FORM                 VALUE 2.    EL690
00357                  88  PI-690-BRWS-PROCESSOR            VALUE 3.    EL690
00358                  88  PI-690-BRWS-ACCOUNT              VALUE 4.    EL690
00359                  88  PI-690-BRWS-ENTRY-CNTL           VALUE 5.    EL690
00360                  88  PI-690-BRWS-ARCHIVE              VALUE 6.    EL690
00361              20  PI-690-LAST-BROWSE-IND                           EL690
00362                                  PIC  X(01).                      EL690
00363                  88  PI-690-LAST-BRWS-FWRD            VALUE '1'.  EL690
00364                  88  PI-690-LAST-BRWS-BWRD            VALUE '2'.  EL690
00365              20  PI-690-STATUS-SELECTION-IND                         CL**3
00366                                  PIC  X(01).                         CL**3
00367                  88  PI-690-SELECT-ALL       VALUE 'N'.              CL**3
00368                  88  PI-690-VALID-SELECTION  VALUE 'A' 'C' 'H'       CL**3
00369                                                'X' 'P' 'V' 'N'.      CL**3
00370          16  FILLER              PIC X(67).                          CL**5
00371                                  EJECT                            EL690
00372  01  FILLER                      PIC  X(19)                       EL690
00373                                  VALUE ':END INTERFACE AREA'.     EL690
00374                                                                   EL690
00375  01  ERROR-MESSAGES.                                              EL690
00376      12  ER-0000                 PIC  X(04)  VALUE '0000'.        EL690
00377      12  ER-0004                 PIC  X(04)  VALUE '0004'.        EL690
00378      12  ER-0023                 PIC  X(04)  VALUE '0023'.        EL690
00379      12  ER-0029                 PIC  X(04)  VALUE '0029'.        EL690
00380      12  ER-0050                 PIC  X(04)  VALUE '0050'.        EL690
00381      12  ER-0068                 PIC  X(04)  VALUE '0068'.        EL690
00382      12  ER-0074                 PIC  X(04)  VALUE '0074'.        EL690
00383      12  ER-0085                 PIC  X(04)  VALUE '0085'.        EL690
00384      12  ER-0130                 PIC  X(04)  VALUE '0130'.        EL690
00385      12  ER-0131                 PIC  X(04)  VALUE '0131'.        EL690
00386      12  ER-0185                 PIC  X(04)  VALUE '0185'.        EL690
00387      12  ER-0190                 PIC  X(04)  VALUE '0190'.        EL690
00388      12  ER-0314                 PIC  X(04)  VALUE '0314'.        EL690
00389      12  ER-7008                 PIC  X(04)  VALUE '7008'.        EL690
00390      12  ER-7244                 PIC  X(04)  VALUE '7244'.           CL**3
00391      12  ER-7357                 PIC  X(04)  VALUE '7357'.        EL690
00392      12  ER-7358                 PIC  X(04)  VALUE '7358'.        EL690
00393      12  ER-7359                 PIC  X(04)  VALUE '7359'.        EL690
00394      12  ER-7360                 PIC  X(04)  VALUE '7360'.        EL690
00395      12  ER-7361                 PIC  X(04)  VALUE '7361'.        EL690
00396      12  ER-7362                 PIC  X(04)  VALUE '7362'.        EL690
00397      12  ER-7366                 PIC  X(04)  VALUE '7366'.        EL690
00398      12  ER-7384                 PIC  X(04)  VALUE '7384'.        EL690
00399      12  ER-7385                 PIC  X(04)  VALUE '7385'.        EL690
00400      12  ER-7386                 PIC  X(04)  VALUE '7386'.        EL690
00401      12  ER-7387                 PIC  X(04)  VALUE '7387'.        EL690
00402      12  ER-7388                 PIC  X(04)  VALUE '7388'.        EL690
00403 *    12  ER-7394                 PIC  X(04)  VALUE '7394'.        EL690
00404      12  ER-7396                 PIC  X(04)  VALUE '7396'.        EL690
00405      12  ER-7397                 PIC  X(04)  VALUE '7397'.        EL690
00406      12  ER-7399                 PIC  X(04)  VALUE '7399'.        EL690
00407      12  ER-9007                 PIC  X(04)  VALUE '9007'.        EL690
00408      12  ER-9008                 PIC  X(04)  VALUE '9008'.        EL690
00409      12  ER-9009                 PIC  X(04)  VALUE '9009'.        EL690
00410      12  ER-9010                 PIC  X(04)  VALUE '9010'.        EL690
00411      12  ER-9011                 PIC  X(04)  VALUE '9011'.        EL690
00412      12  ER-9012                 PIC  X(04)  VALUE '9012'.        EL690
00413      12  ER-9015                 PIC  X(04)  VALUE '9015'.        EL690
00414      12  ER-9016                 PIC  X(04)  VALUE '9016'.        EL690
00415      12  ER-9017                 PIC  X(04)  VALUE '9017'.        EL690
00416      12  ER-9042                 PIC  X(04)  VALUE '9042'.        EL690
00417      12  ER-9080                 PIC  X(04)  VALUE '9080'.        EL690
00418      12  ER-9096                 PIC  X(04)  VALUE '9096'.        EL690
00419      12  ER-9097                 PIC  X(04)  VALUE '9097'.        EL690
00420      12  ER-9129                 PIC  X(04)  VALUE '9129'.        EL690
00421      12  ER-9150                 PIC  X(04)  VALUE '9150'.        EL690
00422      12  ER-9196                 PIC  X(04)  VALUE '9196'.        EL690
00423      12  ER-9245                 PIC  X(04)  VALUE '9245'.        EL690
00424      12  ER-9281                 PIC  X(04)  VALUE '9281'.        EL690
00425      12  ER-9282                 PIC  X(04)  VALUE '9282'.        EL690
00426      12  ER-9293                 PIC  X(04)  VALUE '9293'.        EL690
00427      12  ER-9308                 PIC  X(04)  VALUE '9308'.        EL690
00428      12  ER-9310                 PIC  X(04)  VALUE '9310'.        EL690
00429      12  ER-9312                 PIC  X(04)  VALUE '9312'.        EL690
00430      12  ER-9323                 PIC  X(04)  VALUE '9323'.        EL690
00431                                  EJECT                            EL690
00432      COPY ELCAID REPLACING '01 DFHAID.' BY '01  DFHAID.'.         EL690
00433  01  FILLER  REDEFINES  DFHAID.                                   EL690
00434      12  FILLER                  PIC  X(08).                      EL690
00435      12  PF-VALUES               PIC  X(01)  OCCURS 2.            EL690
00436                                                                   EL690
00437                                  EJECT                            EL690
00438      COPY ELCATTR.                                                EL690
00439                                  EJECT                            EL690
00440      COPY ELCDATE.                                                EL690
00441                                  EJECT                            EL690
00442      COPY ELCEMIB.                                                EL690
00443                                  EJECT                            EL690
00444      COPY ELCLOGOF.                                               EL690
00445                                  EJECT                            EL690
00446      COPY ELCSCTM.                                                EL690
00447                                  EJECT                            EL690
00448      COPY ELCSCRTY.                                               EL690
00449                                  EJECT                            EL690
00450  LINKAGE SECTION.                                                 EL690
00451  01  DFHCOMMAREA                 PIC X(1024).                     EL690
00452                                                                   EL690
00453 *01 PARMLIST .                                                       CL**5
00454 *    12  FILLER                  PIC S9(8)   COMP.                   CL**5
00455 *    12  L-ARCH-POINTER          PIC S9(8)   COMP.                   CL**5
00456 *    12  L-ARCT-POINTER          PIC S9(8)   COMP.                   CL**5
00457 *    12  L-CNTL-POINTER          PIC S9(8)   COMP.                   CL**5
00458                                                                   EL690
00459                                  EJECT                            EL690
00460      COPY ERCARCH.                                                EL690
00461                                  EJECT                            EL690
00462      COPY ERCARCT.                                                EL690
00463                                  EJECT                            EL690
00464      COPY ELCCNTL.                                                EL690
00465                                                                   EL690
00466                                  EJECT                            EL690
00467  PROCEDURE DIVISION.                                              EL690
00468                                                                   EL690
00469      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL690
00470                                                                   EL690
00471      EXEC CICS HANDLE CONDITION                                   EL690
00472          QIDERR    (0110-PGM-INITIALIZATION)                      EL690
00473          MAPFAIL   (0110-PGM-INITIALIZATION)                      EL690
00474          NOTOPEN   (8070-NOTOPEN)                                 EL690
00475          NOTFND    (8080-ARCH-NOT-FOUND)                          EL690
00476          PGMIDERR  (9700-PGMID-ERROR)                             EL690
00477          ERROR     (9800-ABEND)                                   EL690
00478          END-EXEC.                                                EL690
00479                                                                   EL690
00480      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL690
00481      MOVE '5'                    TO DC-OPTION-CODE.               EL690
00482      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.               EL690
00483      MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-DATE.                  EL690
00484      MOVE DC-BIN-DATE-1          TO W-SAVE-BIN-DATE.              EL690
00485                                                                   EL690
00486      MOVE +1                     TO EMI-NUMBER-OF-LINES.          EL690
00487      MOVE '2'                    TO EMI-SWITCH2.                  EL690
00488                                                                   EL690
00489 ***************************************************************   EL690
00490 *       IF ATTEMPTING TO EXECUTE PROGRAM WITHOUT SIGNING ON   *   EL690
00491 *       (COMM LENGTH EQUAL ZERO), SEND ERROR MESSAGE.         *   EL690
00492 ***************************************************************   EL690
00493                                                                   EL690
00494      IF  EIBCALEN EQUAL 0                                         EL690
00495          GO TO 8000-UNAUTHORIZED-ACCESS.                          EL690
00496                                                                   EL690
00497      IF  PI-RETURN-TO-PROGRAM EQUAL W-THIS-PGM                    EL690
00498          MOVE PI-CALLING-PROGRAM TO W-RETURNED-FROM               EL690
00499                                                                   EL690
00500      ELSE                                                         EL690
00501          MOVE SPACES             TO W-RETURNED-FROM.              EL690
00502                                                                   EL690
00503      IF  PI-CALLING-PROGRAM NOT EQUAL W-THIS-PGM                  EL690
00504                                                                   EL690
00505          IF  PI-RETURN-TO-PROGRAM NOT EQUAL W-THIS-PGM            EL690
00506              MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-6     EL690
00507              MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-5     EL690
00508              MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-4     EL690
00509              MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-3     EL690
00510              MOVE PI-SAVED-PROGRAM-1    TO PI-SAVED-PROGRAM-2     EL690
00511              MOVE PI-RETURN-TO-PROGRAM  TO PI-SAVED-PROGRAM-1     EL690
00512              MOVE PI-CALLING-PROGRAM    TO PI-RETURN-TO-PROGRAM   EL690
00513              MOVE W-THIS-PGM            TO PI-CALLING-PROGRAM     EL690
00514              MOVE LOW-VALUES            TO PI-690-WORK-AREA       EL690
00515              MOVE HIGH-VALUES           TO PI-690-FIRST-DATA      EL690
PEMUNI             MOVE ZEROS                 TO PI-690-BRWS-TYPE-IND
00516                                                                   EL690
00517              IF  PI-RETURN-TO-PROGRAM EQUAL W-XCTL-626            EL690
00518                  MOVE LOW-VALUES        TO PI-CARRIER             EL690
00519                                            PI-GROUPING            EL690
00520                                            PI-STATE               EL690
00521                                            PI-ACCOUNT             EL690
00522                                            PI-CERT-EFF-DT         EL690
00523                                            PI-CERT-PRIME          EL690
00524                                            PI-CERT-SFX            EL690
00525                                            PI-689-WORK-AREA       EL690
00526                                                                   EL690
00527              ELSE                                                 EL690
00528                  PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT  EL690
00529                  GO TO 1200-SET-UP-COMPLETE-SCREEN                EL690
00530                                                                   EL690
00531          ELSE                                                     EL690
00532              MOVE PI-RETURN-TO-PROGRAM  TO PI-CALLING-PROGRAM     EL690
00533              MOVE PI-SAVED-PROGRAM-1    TO PI-RETURN-TO-PROGRAM   EL690
00534              MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-1     EL690
00535              MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-2     EL690
00536              MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-3     EL690
00537              MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-4     EL690
00538              MOVE PI-SAVED-PROGRAM-6    TO PI-SAVED-PROGRAM-5     EL690
00539              MOVE SPACES                TO PI-SAVED-PROGRAM-6.    EL690
00540                                  EJECT                            EL690
00541  0100-ENTRY-LOGIC.                                                EL690
00542                                                                   EL690
00543      IF  EIBTRNID EQUAL W-TRANSACTION                             EL690
00544                                                                   EL690
00545          IF  EIBAID EQUAL DFHCLEAR                                EL690
00546                  OR                                               EL690
00547              NOT DISPLAY-CAP                                      EL690
00548              MOVE PI-RETURN-TO-PROGRAM                            EL690
00549                                  TO W-CALL-PGM                    EL690
00550              PERFORM 9400-XCTL THRU 9400-EXIT                     EL690
00551                                                                   EL690
00552          ELSE                                                     EL690
00553              GO TO 0200-RECEIVE                                   EL690
00554                                                                   EL690
00555      ELSE                                                         EL690
00556          IF  W-RETURNED-FROM NOT EQUAL SPACES                     EL690
00557              GO TO 1000-RECOVER-LAST-SCREEN.                      EL690
00558                                                                   EL690
00559      MOVE LOW-VALUES             TO EL690AO.                      EL690
00560                                                                   EL690
00561  0110-PGM-INITIALIZATION.                                         EL690
00562                                                                   EL690
00563      PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT.             EL690
00564                                                                   EL690
00565      MOVE LOW-VALUES             TO EL690AO.                         CL**5
00566      MOVE -1                     TO TYPEBRL.                      EL690
00567      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
00568                                  EJECT                            EL690
00569  0200-RECEIVE.                                                    EL690
00570                                                                   EL690
00571      IF  EIBAID EQUAL DFHPA1                                      EL690
00572              OR                                                   EL690
00573          EIBAID EQUAL DFHPA2                                      EL690
00574              OR                                                   EL690
00575          EIBAID EQUAL DFHPA3                                      EL690
00576          MOVE ER-7008            TO EMI-ERROR                     EL690
00577          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL690
00578          MOVE -1                 TO PFKEYL                        EL690
00579          GO TO 8200-SEND-DATAONLY.                                EL690
00580                                                                   EL690
00581      EXEC CICS RECEIVE                                            EL690
00582          MAP     (W-MAP)                                          EL690
00583          MAPSET  (W-MAPSET)                                       EL690
00584          INTO    (EL690AI)                                        EL690
00585          END-EXEC.                                                EL690
00586                                                                   EL690
00587      IF  PFKEYL EQUAL ZERO                                        EL690
00588          GO TO 0300-CHECK-PFKEYS.                                 EL690
00589                                                                   EL690
00590      IF  EIBAID NOT EQUAL DFHENTER                                EL690
00591          MOVE ER-0004            TO EMI-ERROR                     EL690
00592          GO TO 0310-INPUT-ERROR.                                  EL690
00593                                                                   EL690
00594      IF  PFKEYI GREATER 0 AND LESS 25                             EL690
00595          MOVE PF-VALUES (PFKEYI) TO EIBAID                        EL690
00596                                                                   EL690
00597      ELSE                                                         EL690
00598          MOVE ER-0029            TO EMI-ERROR                     EL690
00599          GO TO 0310-INPUT-ERROR.                                  EL690
00600                                                                   EL690
00601                                  EJECT                            EL690
00602  0300-CHECK-PFKEYS.                                               EL690
00603                                                                   EL690
00604      IF  EIBAID EQUAL DFHPF23                                     EL690
00605          MOVE EIBAID             TO PI-ENTRY-CD-1                 EL690
00606          MOVE W-XCTL-005         TO W-CALL-PGM                    EL690
00607          PERFORM 9400-XCTL THRU 9400-EXIT.                        EL690
00608                                                                   EL690
00609      IF  EIBAID EQUAL DFHPF24                                     EL690
00610          MOVE W-XCTL-626         TO W-CALL-PGM                    EL690
00611          PERFORM 9400-XCTL THRU 9400-EXIT.                        EL690
00612                                                                   EL690
00613      IF  EIBAID EQUAL DFHPF12                                     EL690
00614          MOVE W-XCTL-010         TO W-CALL-PGM                    EL690
00615          PERFORM 9400-XCTL THRU 9400-EXIT.                        EL690
00616                                                                   EL690
00617      IF  MAINTI EQUAL 'C'                                         EL690
00618              AND                                                  EL690
00619          EIBAID EQUAL DFHENTER                                    EL690
00620              AND                                                  EL690
00621          CORRSELL GREATER THAN ZEROS                              EL690
00622          MOVE -1                 TO CORRSELL                      EL690
00623          MOVE AL-UNBON           TO CORRSELA                      EL690
00624                                     MAINTA                        EL690
00625          MOVE ER-9323            TO EMI-ERROR                     EL690
00626          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL690
00627          GO TO 8200-SEND-DATAONLY.                                EL690
00628                                                                   EL690
00629      IF  EIBAID EQUAL DFHPF3                                      EL690
012312*            OR                                                   EL690
012312*        (EIBAID EQUAL DFHENTER                                   EL690
012312*            AND                                                  EL690
012312*        CORRSELL GREATER THAN ZEROS)                             EL690
00634          MOVE '3'                TO EIBAID                        EL690
00635          PERFORM 1500-SELECT-ARCHIVE-REC THRU 1500-EXIT           EL690
00636          MOVE W-XCTL-689         TO W-CALL-PGM                    EL690
00637          GO TO 9400-XCTL.                                         EL690
00638                                                                   EL690
00639      IF  EIBAID EQUAL DFHPF4                                      EL690
00640          MOVE LOW-VALUES         TO EL690AO                       EL690
00641                                     PI-690-WORK-AREA              EL690
00642          MOVE -1                 TO TYPEBRL                       EL690
00643          GO TO 8100-SEND-INITIAL-MAP.                             EL690
070711
070711     IF  EIBAID EQUAL DFHPF5
012312             OR
012312         (EIBAID EQUAL DFHENTER
012312             AND
012312         CORRSELL GREATER THAN ZEROS)
012312         MOVE '5'                TO EIBAID
070711         PERFORM 1500-SELECT-ARCHIVE-REC THRU 1500-EXIT
070711         IF PI-689-ARCHIVE-NUMBER NOT GREATER ZERO
070711             MOVE -1             TO CORRSELL
070711             MOVE AL-UNBON       TO CORRSELA
070711             MOVE ER-9293        TO EMI-ERROR
070711             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
070711             GO TO 8200-SEND-DATAONLY
070711         ELSE
102020            MOVE PI-690-INIT-CARRIER
102020                                 TO PI-CARRIER
102020            MOVE PI-690-INIT-GROUPING
102020                                 TO PI-GROUPING
102020            MOVE PI-690-INIT-STATE
102020                                 TO PI-STATE
102020            MOVE PI-690-INIT-ACCOUNT
102020                                 TO PI-ACCOUNT
102020            MOVE PI-690-INIT-EFFECT-DATE
102020                                 TO PI-CERT-EFF-DT
102020            MOVE PI-690-INIT-CERT-PRIME
102020                                 TO PI-CERT-PRIME
102020            MOVE PI-690-INIT-SUFFIX
102020                                 TO PI-CERT-SFX
070711            MOVE W-XCTL-691      TO W-CALL-PGM
070711            GO TO 9400-XCTL
070711         END-IF
070711     END-IF.
121112
121112     IF  EIBAID EQUAL DFHPF6
121112         MOVE PI-690-INIT-CARRIER  TO PI-CARRIER
121112         MOVE PI-690-INIT-GROUPING TO PI-GROUPING
121112         MOVE PI-690-INIT-STATE    TO PI-STATE
121112         MOVE PI-690-INIT-ACCOUNT  TO PI-ACCOUNT
121112         MOVE PI-690-INIT-EFFECT-DATE TO PI-CERT-EFF-DT
121112         MOVE PI-690-INIT-CERT-PRIME TO PI-CERT-PRIME
121112         MOVE PI-690-INIT-SUFFIX   TO PI-CERT-SFX
102020         move pi-689-chg-seq-nox(1:1)
102020                                 to PI-PROGRAM-WORK-AREA(1:1)
121112         MOVE W-XCTL-1279        TO W-CALL-PGM
121112         GO TO 9400-XCTL
121112     END-IF
00644                                                                   EL690
00645      IF  MAINTI EQUAL 'C'                                         EL690
00646              AND                                                  EL690
00647          EIBAID EQUAL DFHENTER                                    EL690
00648          GO TO 0700-PROCESS-CHANGES.                              EL690
00649                                                                   EL690
00650      IF  EIBAID EQUAL DFHPF1                                      EL690
00651          PERFORM 0500-EDIT-SCREEN-DATA THRU 0500-EXIT             EL690
00652          MOVE LOW-VALUES         TO PI-690-FIRST-DATA             EL690
00653                                     W-ARCH-GROUPS                 EL690
00654          GO TO 2000-PAGE-FORWARD.                                 EL690
00655                                                                   EL690
00656      IF  EIBAID EQUAL DFHPF2                                      EL690
00657                                                                   EL690
00658          IF  PI-690-INIT-DATA EQUAL LOW-VALUES                    EL690
00659              MOVE HIGH-VALUES    TO PI-690-INIT-DATA              EL690
00660              PERFORM 0500-EDIT-SCREEN-DATA THRU 0500-EXIT         EL690
00661              MOVE HIGH-VALUES    TO PI-690-LAST-DATA              EL690
00662              MOVE LOW-VALUES     TO W-ARCH-GROUPS                 EL690
00663              GO TO 3000-PAGE-BACKWARD                             EL690
00664                                                                   EL690
00665          ELSE                                                     EL690
00666              PERFORM 0500-EDIT-SCREEN-DATA THRU 0500-EXIT         EL690
00667              MOVE HIGH-VALUES    TO PI-690-LAST-DATA              EL690
00668              MOVE LOW-VALUES     TO W-ARCH-GROUPS                 EL690
00669              GO TO 3000-PAGE-BACKWARD.                            EL690
00670                                                                   EL690
00671      MOVE ER-0029                TO EMI-ERROR.                    EL690
00672                                                                   EL690
00673  0310-INPUT-ERROR.                                                EL690
00674                                                                   EL690
00675      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
00676      MOVE -1                     TO PFKEYL                        EL690
00677      MOVE AL-UNBON               TO PFKEYA                        EL690
00678      GO TO 8200-SEND-DATAONLY.                                    EL690
00679                                  EJECT                            EL690
00680  0500-EDIT-SCREEN-DATA.                                           EL690
00681                                                                   EL690
00682      IF  TYPEBRL NOT EQUAL ZEROS                                  EL690
00683                                                                   EL690
00684          IF  TYPEBRI NOT EQUAL PI-690-BRWS-TYPE-IND               EL690
00685              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND        EL690
00686              MOVE TYPEBRI        TO PI-690-BRWS-TYPE-IND          EL690
00687              MOVE AL-UNNON       TO TYPEBRA                       EL690
00688                                                                   EL690
00689          ELSE                                                     EL690
00690              NEXT SENTENCE                                        EL690
00691                                                                   EL690
00692      ELSE                                                         EL690
00693          IF  PI-690-BRWS-TYPE-IND NOT NUMERIC                     EL690
00694                  OR                                               EL690
00695              PI-690-BRWS-TYPE-IND LESS THAN 1                     EL690
00696                  OR                                               EL690
00697              PI-690-BRWS-TYPE-IND GREATER THAN 6                  EL690
00698              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND        EL690
00699              MOVE 6              TO PI-690-BRWS-TYPE-IND          EL690
00700              MOVE +1             TO TYPEBRL                       EL690
00701              MOVE AL-UNNON       TO TYPEBRA                       EL690
00702                                                                   EL690
00703          ELSE                                                     EL690
00704              MOVE +1             TO TYPEBRL                       EL690
00705              MOVE AL-UNNON       TO TYPEBRA                       EL690
00706              MOVE PI-690-BRWS-TYPE-IND                            EL690
00707                                  TO TYPEBRO.                      EL690
00708                                                                   EL690
00709      IF  CERTRPL GREATER THAN ZEROS                               EL690
00710                                                                   EL690
00711          IF  CERTRPI NOT EQUAL PI-690-INIT-CERT-PRIME             EL690
00712              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND        EL690
00713              MOVE CERTRPI        TO PI-690-INIT-CERT-PRIME        EL690
00714              MOVE AL-UANON       TO CERTRPA                       EL690
00715                                                                   EL690
00716              IF  SUFFIXL GREATER THAN ZEROS                       EL690
00717                                                                   EL690
00718                  IF  SUFFIXI NOT EQUAL PI-690-INIT-SUFFIX         EL690
00719                      MOVE 'Y'    TO W-CHANGE-REQUESTED-IND        EL690
00720                                                                   EL690
00721                      IF  SUFFIXI NOT GREATER THAN SPACES          EL690
00722                          MOVE SPACES                              EL690
00723                                  TO SUFFIXI                       EL690
00724                                     PI-690-INIT-SUFFIX            EL690
00725                          MOVE AL-UANON                            EL690
00726                                  TO SUFFIXA                       EL690
00727                                                                   EL690
00728                      ELSE                                         EL690
00729                          MOVE AL-UANON                            EL690
00730                                  TO SUFFIXA                       EL690
00731                          MOVE SUFFIXI                             EL690
00732                                  TO PI-690-INIT-SUFFIX            EL690
00733                                                                   EL690
00734                  ELSE                                             EL690
00735                      NEXT SENTENCE                                EL690
00736                                                                   EL690
00737              ELSE                                                 EL690
00738                  IF  PI-690-INIT-SUFFIX EQUAL LOW-VALUES          EL690
00739                          OR                                       EL690
00740                      PI-690-INIT-SUFFIX EQUAL HIGH-VALUES         EL690
00741                      MOVE AL-UANON                                EL690
00742                                  TO SUFFIXA                       EL690
00743                      MOVE SPACES TO PI-690-INIT-SUFFIX            EL690
00744                                                                   EL690
00745                  ELSE                                             EL690
00746                      NEXT SENTENCE                                EL690
00747                                                                   EL690
00748          ELSE                                                     EL690
00749              NEXT SENTENCE                                        EL690
00750                                                                   EL690
00751      ELSE                                                         EL690
00752          IF  PI-690-INIT-CERT-PRIME GREATER THAN LOW-VALUES       EL690
00753                  AND                                              EL690
00754              PI-690-INIT-CERT-PRIME LESS THAN HIGH-VALUES         EL690
00755              MOVE PI-690-INIT-CERT-PRIME                          EL690
00756                                  TO CERTRPI                       EL690
00757              MOVE PI-690-INIT-SUFFIX                              EL690
00758                                  TO SUFFIXI                       EL690
00759              MOVE AL-UANON       TO CERTRPA                       EL690
00760                                     SUFFIXA                       EL690
00761              MOVE +10            TO CERTRPL                       EL690
00762              MOVE +1             TO SUFFIXL.                      EL690
00763                                                                   EL690
00764      IF  FORML GREATER THAN ZEROS                                 EL690
00765                                                                   EL690
00766          IF  FORMI NOT EQUAL PI-690-INIT-FORM                     EL690
00767              MOVE AL-UANON       TO FORMA                         EL690
00768              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND        EL690
00769              MOVE FORMI          TO PI-690-INIT-FORM              EL690
00770                                                                   EL690
00771          ELSE                                                     EL690
00772              NEXT SENTENCE                                        EL690
00773                                                                   EL690
00774      ELSE                                                         EL690
00775          IF  PI-690-INIT-FORM GREATER THAN LOW-VALUES             EL690
00776                  AND                                              EL690
00777              PI-690-INIT-FORM LESS THAN HIGH-VALUES               EL690
00778              MOVE AL-UANON       TO FORMA                         EL690
00779              MOVE +4             TO FORML                         EL690
00780              MOVE PI-690-INIT-FORM                                EL690
00781                                  TO FORMI.                        EL690
00782                                                                   EL690
00783      IF  ENTRYL GREATER THAN ZEROS                                EL690
00784              AND                                                  EL690
00785          CKCNTLL GREATER THAN ZEROS                               EL690
00786          MOVE -1                 TO CKCNTLL                       EL690
00787          MOVE AL-UABON           TO CKCNTLA                       EL690
00788          MOVE ER-7362            TO EMI-ERROR                     EL690
00789          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL690
00790          GO TO 8100-SEND-INITIAL-MAP.                             EL690
00791                                                                   EL690
00792      IF  ENTRYL GREATER THAN ZEROS                                EL690
00793                                                                   EL690
00794          IF  ENTRYI NOT EQUAL PI-690-INIT-ENTRY                   EL690
00795              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND        EL690
00796              MOVE ENTRYI         TO PI-690-INIT-ENTRY             EL690
00797                                                                   EL690
00798          ELSE                                                     EL690
00799              NEXT SENTENCE                                        EL690
00800                                                                   EL690
00801      ELSE                                                         EL690
00802          IF  PI-690-INIT-ENTRY GREATER THAN LOW-VALUES            EL690
00803                  AND                                              EL690
00804              PI-690-INIT-ENTRY LESS THAN HIGH-VALUES              EL690
00805                  AND                                              EL690
00806              PI-690-INIT-CONTROL-PREFIX NOT EQUAL 'CK'            EL690
00807              MOVE AL-UANON       TO ENTRYA                        EL690
00808              MOVE +6             TO ENTRYL                        EL690
00809              MOVE PI-690-INIT-ENTRY                               EL690
00810                                  TO ENTRYI.                       EL690
00811                                                                   EL690
00812      IF  CKCNTLL GREATER THAN ZEROS                               EL690
00813                                                                   EL690
00814          IF  CKCNTLI NUMERIC                                      EL690
00815                                                                   EL690
00816              IF  CKCNTLI NOT EQUAL PI-690-INIT-CONTROL            EL690
00817                  MOVE 'Y'        TO W-CHANGE-REQUESTED-IND        EL690
00818                  MOVE 'CK'       TO PI-690-INIT-CONTROL-PREFIX    EL690
00819                  MOVE CKCNTLI    TO PI-690-INIT-CONTROL           EL690
00820                                                                   EL690
00821              ELSE                                                 EL690
00822                  NEXT SENTENCE                                    EL690
00823                                                                   EL690
00824          ELSE                                                     EL690
00825              MOVE ZEROS          TO PI-690-INIT-CONTROL           EL690
00826                                     CKCNTLO                       EL690
00827                                                                   EL690
00828      ELSE                                                         EL690
00829          IF  PI-690-INIT-CONTROL GREATER THAN ZEROS               EL690
00830                  AND                                              EL690
00831              PI-690-INIT-CONTROL LESS THAN 99999999               EL690
00832                  AND                                              EL690
00833              PI-690-INIT-CONTROL-PREFIX EQUAL 'CK'                EL690
00834              MOVE AL-UANON       TO CKCNTLA                       EL690
00835              MOVE +8             TO CKCNTLL                       EL690
00836              MOVE PI-690-INIT-CONTROL                             EL690
00837                                  TO CKCNTLI.                      EL690
00838                                                                   EL690
00839      IF  CARRIERL GREATER THAN ZEROS                              EL690
00840                                                                   EL690
00841          IF  CARRIERI NOT EQUAL  TO PI-690-INIT-CARRIER           EL690
00842              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND        EL690
00843              MOVE CARRIERI       TO PI-690-INIT-CARRIER           EL690
00844                                                                   EL690
00845          ELSE                                                     EL690
00846              NEXT SENTENCE                                        EL690
00847                                                                   EL690
00848      ELSE                                                         EL690
00849          IF  PI-690-INIT-CARRIER GREATER THAN LOW-VALUES          EL690
00850                  AND                                              EL690
00851              PI-690-INIT-CARRIER LESS THAN HIGH-VALUES            EL690
00852              MOVE AL-UANON       TO CARRIERA                      EL690
00853              MOVE +1             TO CARRIERL                      EL690
00854              MOVE PI-690-INIT-CARRIER                             EL690
00855                                  TO CARRIERI.                     EL690
00856                                                                   EL690
00857      IF  GROUPL GREATER THAN ZEROS                                EL690
00858                                                                   EL690
00859          IF  GROUPI NOT EQUAL    TO PI-690-INIT-GROUPING          EL690
00860              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND        EL690
00861              MOVE GROUPI         TO PI-690-INIT-GROUPING          EL690
00862                                                                   EL690
00863          ELSE                                                     EL690
00864              NEXT SENTENCE                                        EL690
00865                                                                   EL690
00866      ELSE                                                         EL690
00867          IF  PI-690-INIT-GROUPING GREATER THAN LOW-VALUES         EL690
00868                  AND                                              EL690
00869              PI-690-INIT-GROUPING LESS THAN HIGH-VALUES           EL690
00870              MOVE AL-UANON       TO GROUPA                        EL690
00871              MOVE +6             TO GROUPL                        EL690
00872              MOVE PI-690-INIT-GROUPING                            EL690
00873                                  TO GROUPI.                       EL690
00874                                                                   EL690
00875      IF  STATEL GREATER THAN ZEROS                                EL690
00876                                                                   EL690
00877          IF  STATEI NOT EQUAL    TO PI-690-INIT-STATE             EL690
00878              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND        EL690
00879              MOVE STATEI         TO PI-690-INIT-STATE             EL690
00880                                                                   EL690
00881          ELSE                                                     EL690
00882              NEXT SENTENCE                                        EL690
00883                                                                   EL690
00884      ELSE                                                         EL690
00885          IF  PI-690-INIT-STATE GREATER THAN LOW-VALUES            EL690
00886                  AND                                              EL690
00887              PI-690-INIT-STATE LESS THAN HIGH-VALUES              EL690
00888              MOVE AL-UANON       TO STATEA                        EL690
00889              MOVE +2             TO STATEL                        EL690
00890              MOVE PI-690-INIT-STATE                               EL690
00891                                  TO STATEI.                       EL690
00892                                                                   EL690
00893      IF  ACCTL GREATER THAN ZEROS                                 EL690
00894                                                                   EL690
00895          IF  ACCTI NOT EQUAL     TO PI-690-INIT-ACCOUNT           EL690
00896              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND        EL690
00897              MOVE ACCTI          TO PI-690-INIT-ACCOUNT           EL690
00898                                                                   EL690
00899          ELSE                                                     EL690
00900              NEXT SENTENCE                                        EL690
00901                                                                   EL690
00902      ELSE                                                         EL690
00903          IF  PI-690-INIT-ACCOUNT GREATER THAN LOW-VALUES          EL690
00904                  AND                                              EL690
00905              PI-690-INIT-ACCOUNT LESS THAN HIGH-VALUES            EL690
00906              MOVE AL-UANON       TO ACCTA                         EL690
00907              MOVE +10            TO ACCTL                         EL690
00908              MOVE PI-690-INIT-ACCOUNT                             EL690
00909                                  TO ACCTI.                        EL690
00910                                                                   EL690
00911      IF  PROCSRL GREATER THAN ZEROS                               EL690
00912                                                                   EL690
00913          IF  PROCSRI NOT EQUAL   TO PI-690-INIT-PROCESSOR         EL690
00914              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND        EL690
00915              MOVE PROCSRI        TO PI-690-INIT-PROCESSOR         EL690
00916                                                                   EL690
00917          ELSE                                                     EL690
00918              NEXT SENTENCE                                        EL690
00919                                                                   EL690
00920      ELSE                                                         EL690
00921          IF  PI-690-INIT-PROCESSOR GREATER THAN LOW-VALUES        EL690
00922                  AND                                              EL690
00923              PI-690-INIT-PROCESSOR LESS THAN HIGH-VALUES          EL690
00924              MOVE AL-UANON       TO PROCSRA                       EL690
00925              MOVE +4             TO PROCSRL                       EL690
00926              MOVE PI-690-INIT-PROCESSOR                           EL690
00927                                  TO PROCSRI.                      EL690
00928                                                                   EL690
00929      IF  EFFDTEL GREATER THAN ZEROS                               EL690
00930          MOVE EFFDTEI            TO W-DEEDIT-FIELD                EL690
00931                                                                   EL690
00932          PERFORM 9200-DATE-EDIT THRU 9200-EXIT                    EL690
00933                                                                   EL690
00934          IF  DC-ERROR-CODE NOT EQUAL SPACES                       EL690
00935              MOVE -1             TO EFFDTEL                       EL690
00936              MOVE AL-UABON       TO EFFDTEA                       EL690
00937              MOVE ER-0314        TO EMI-ERROR                     EL690
00938              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL690
00939              GO TO 8100-SEND-INITIAL-MAP                          EL690
00940                                                                   EL690
00941          ELSE                                                     EL690
00942              IF  DC-BIN-DATE-1 NOT = PI-690-INIT-EFFECT-DATE
00943                  MOVE 'Y'        TO W-CHANGE-REQUESTED-IND        EL690
00944                  MOVE DC-BIN-DATE-1                               EL690
00945                                  TO PI-690-INIT-EFFECT-DATE       EL690
00946                  MOVE DC-GREG-DATE-1-EDIT                         EL690
00947                                  TO PI-690-INIT-EFF-DTE           EL690
00948                                                                   EL690
00949              ELSE                                                 EL690
00950                  IF  PI-690-INIT-EFFECT-DATE > LOW-VALUES
00952                          AND                                      EL690
00953                      PI-690-INIT-EFFECT-DATE < HIGH-VALUES
00954                      MOVE AL-UANON                                EL690
00955                                  TO EFFDTEA                       EL690
00956                      MOVE +8     TO EFFDTEL                       EL690
00957                      MOVE PI-690-INIT-EFF-DTE                     EL690
00958                                  TO EFFDTEI.                      EL690
00959                                                                   EL690
00960      IF  STATSELL GREATER THAN ZEROS                                 CL**3
00961          MOVE STATSELI           TO PI-690-STATUS-SELECTION-IND      CL**3
00962                                                                      CL**3
00963          IF  NOT PI-690-VALID-SELECTION                              CL**3
00964              MOVE -1             TO STATSELL                         CL**3
00965              MOVE AL-UABON       TO STATSELA                         CL**3
00966              MOVE ER-7244        TO EMI-ERROR                        CL**3
00967              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**3
00968              GO TO 8100-SEND-INITIAL-MAP                             CL**3
00969                                                                      CL**3
00970          ELSE                                                        CL**3
00971              NEXT SENTENCE                                           CL**3
00972                                                                      CL**3
00973      ELSE                                                            CL**3
00974          IF  PI-690-VALID-SELECTION                                  CL**3
00975              MOVE AL-UANON       TO STATSELA                         CL**3
00976              MOVE +1             TO STATSELL                         CL**3
00977              MOVE PI-690-STATUS-SELECTION-IND                        CL**3
00978                                  TO STATSELI                         CL**3
00979                                                                      CL**3
00980          ELSE                                                        CL**3
00981              MOVE AL-UANON       TO STATSELA                         CL**3
00982              MOVE +1             TO STATSELL                         CL**3
00983              MOVE 'N'            TO PI-690-STATUS-SELECTION-IND      CL**3
00984                                     STATSELI.                        CL**3
00985                                                                      CL**3
00986  0500-EXIT.                                                       EL690
00987      EXIT.                                                        EL690
00988                                  EJECT                            EL690
00989  0700-PROCESS-CHANGES.                                            EL690
00990                                                                   EL690
00991      IF  NOT MODIFY-CAP                                           EL690
00992          MOVE 'UPDATE'           TO SM-READ                       EL690
00993          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL690
00994          MOVE ER-9096            TO EMI-ERROR                     EL690
00995          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL690
00996          MOVE AL-UANON           TO MAINTA                        EL690
00997          MOVE -1                 TO MAINTL                        EL690
00998          GO TO 8200-SEND-DATAONLY.                                EL690
00999                                                                   EL690
01000      EXEC CICS HANDLE CONDITION                                   EL690
01001          NOTFND  (8080-ARCH-NOT-FOUND)                            EL690
01002          ENDFILE (8080-ARCH-NOT-FOUND)                            EL690
01003          END-EXEC.                                                EL690
01004                                                                   EL690
01005      MOVE ZEROS                  TO W-FIRST-CHANGE-IND.           EL690
01006                                                                   EL690
01007      PERFORM 0720-SEARCH-FOR-CHANGES THRU 0720-EXIT               EL690
01008              VARYING                                              EL690
01009          W-ARCH-NDX FROM 1 BY 1                                   EL690
01010              UNTIL                                                EL690
01011          W-ARCH-NDX GREATER THAN 12                               EL690
01012              OR                                                   EL690
01013          PI-690-ARCHIVE-NUM (W-ARCH-NDX) EQUAL ZEROS.             EL690
01014                                                                   EL690
01015      IF  NOT EMI-NO-ERRORS                                        EL690
01016          GO TO 8200-SEND-DATAONLY.                                EL690
01017                                                                   EL690
01018      MOVE SPACES                 TO MAINTO                        EL690
01019      MOVE -1                     TO MAINTL                        EL690
01020      MOVE ER-9308                TO EMI-ERROR                     EL690
01021      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL690
01022                                                                   EL690
01023      MOVE LOW-VALUES             TO EL690AO.                      EL690
01024                                                                   EL690
01025      IF  PI-690-LAST-BRWS-FWRD                                    EL690
01026          MOVE '1'                TO EIBAID                        EL690
01027          GO TO 2000-PAGE-FORWARD.                                 EL690
01028                                                                   EL690
01029      IF  PI-690-LAST-BRWS-BWRD                                    EL690
01030          MOVE '2'                TO EIBAID                        EL690
01031          GO TO 3000-PAGE-BACKWARD.                                EL690
01032                                                                   EL690
01033  0700-EXIT.                                                       EL690
01034      EXIT.                                                        EL690
01035                                                                   EL690
01036  0720-SEARCH-FOR-CHANGES.                                         EL690
01037                                                                   EL690
01038      MOVE SPACES                 TO W-REPLY-DATE                  EL690
01039                                     W-RESEND-DATE-1               EL690
01040                                     W-RESEND-DATE-2               EL690
01041                                     W-RESEND-DATE-3               EL690
01042                                     W-STATUS                         CL**3
01043                                     W-VALID-STATUS-SW.               CL**3
01044                                                                   EL690
070711*    IF  W-STATSL (W-ARCH-NDX) GREATER THAN ZEROS                 EL690
070711*        MOVE W-STATSO (W-ARCH-NDX)                               EL690
070711*                                TO W-VALID-STATUS-SW             EL690
070711*                                                                 EL690
070711*        IF  W-ALLOWED-ENTRIES                                    EL690
070711*            MOVE W-STATSO (W-ARCH-NDX)                           EL690
070711*                                TO W-STATUS                      EL690
070711*                                                                 EL690
070711*        ELSE                                                     EL690
070711*            MOVE ER-7358        TO EMI-ERROR                     EL690
070711*            MOVE AL-UANON       TO W-STATSA (W-ARCH-NDX)         EL690
070711*            MOVE -1             TO W-STATSL (W-ARCH-NDX)         EL690
070711*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL690
01058                                                                   EL690
070711     IF  W-RSDATL (W-ARCH-NDX) GREATER THAN ZEROS                 EL690
01060                                                                   EL690
070711         IF  W-RSDATO (W-ARCH-NDX) EQUAL SPACES                   EL690
01062                  OR                                               EL690
070711             W-RSDATO (W-ARCH-NDX) EQUAL ZEROES                   EL690
01064              MOVE LOW-VALUES     TO W-RESEND-DATE-1               EL690
01065                                                                   EL690
01066          ELSE                                                     EL690
070711             MOVE W-RSDATO (W-ARCH-NDX)                           EL690
01068                                  TO W-DEEDIT-FIELD                EL690
01069                                                                   EL690
01070              PERFORM 9200-DATE-EDIT THRU 9200-EXIT                EL690
01071                                                                   EL690
01072              IF  DC-ERROR-CODE EQUAL SPACES                       EL690
01073                  MOVE DC-BIN-DATE-1                               EL690
01074                                  TO W-RESEND-DATE-1               EL690
01075                  MOVE DC-GREG-DATE-1-EDIT                         EL690
070711                                 TO W-RSDATO (W-ARCH-NDX)         EL690
01077                                                                   EL690
01078              ELSE                                                 EL690
01079                  MOVE ER-0185    TO EMI-ERROR                     EL690
070711                 MOVE AL-UANON   TO W-RSDATA (W-ARCH-NDX)         EL690
070711                 MOVE -1         TO W-RSDATL (W-ARCH-NDX)         EL690
01082                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        EL690
01083                                                                   EL690
070711*    IF  W-RSFRML (W-ARCH-NDX) GREATER THAN ZEROS                 EL690
01085 *                                                                 EL690
070711*        IF  W-RSFRMO (W-ARCH-NDX) EQUAL SPACES                   EL690
01087 *                OR                                               EL690
070711*            W-RSFRMO (W-ARCH-NDX) EQUAL ZEROES                   EL690
01089 *            MOVE LOW-VALUES     TO W-RESEND-DATE-2               EL690
01090 *                                                                 EL690
01091 *        ELSE                                                     EL690
070711*            MOVE W-RSFRMO (W-ARCH-NDX)                           EL690
01093 *                                TO W-DEEDIT-FIELD                EL690
01094 *                                                                 EL690
01095 *            PERFORM 9200-DATE-EDIT THRU 9200-EXIT                EL690
01096 *                                                                 EL690
01097 *            IF  DC-ERROR-CODE EQUAL SPACES                       EL690
01098 *                MOVE DC-BIN-DATE-1                               EL690
01099 *                                TO W-RESEND-DATE-2               EL690
01100 *                MOVE DC-GREG-DATE-1-EDIT                         EL690
01101 *                                TO W-RSFRMO (W-ARCH-NDX)         EL690
01102 *                                                                 EL690
01103 *            ELSE                                                 EL690
01104 *                MOVE ER-0185    TO EMI-ERROR                     EL690
01105 *                MOVE AL-UANON   TO W-RSFRMA (W-ARCH-NDX)         EL690
01106 *                MOVE -1         TO W-RSFRML (W-ARCH-NDX)         EL690
01107 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        EL690
01108 *                                                                 EL690
01109 *    IF  W-RSSTSL (W-ARCH-NDX) GREATER THAN ZEROS                 EL690
01110 *                                                                 EL690
01111 *        IF  W-RSSTSO (W-ARCH-NDX) EQUAL SPACES                   EL690
01112 *                OR                                               EL690
01113 *            W-RSSTSO (W-ARCH-NDX) EQUAL ZEROES                   EL690
01114 *            MOVE LOW-VALUES     TO W-RESEND-DATE-3               EL690
01115 *                                                                 EL690
01116 *        ELSE                                                     EL690
01117 *            MOVE W-RSSTSO (W-ARCH-NDX)                           EL690
01118 *                                TO W-DEEDIT-FIELD                EL690
01119 *                                                                 EL690
01120 *            PERFORM 9200-DATE-EDIT THRU 9200-EXIT                EL690
01121 *                                                                 EL690
01122 *            IF  DC-ERROR-CODE EQUAL SPACES                       EL690
01123 *                MOVE DC-BIN-DATE-1                               EL690
01124 *                                TO W-RESEND-DATE-3               EL690
01125 *                MOVE DC-GREG-DATE-1-EDIT                         EL690
01126 *                                TO W-RSSTSO (W-ARCH-NDX)         EL690
01127 *                                                                 EL690
01128 *            ELSE                                                 EL690
01129 *                MOVE ER-0185    TO EMI-ERROR                     EL690
01130 *                MOVE AL-UANON   TO W-RSSTSA (W-ARCH-NDX)         EL690
01131 *                MOVE -1         TO W-RSSTSL (W-ARCH-NDX)         EL690
01132 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        EL690
01133                                                                   EL690
070711*    IF  W-REPLYL (W-ARCH-NDX) GREATER THAN ZEROS                 EL690
070711*                                                                 EL690
070711*        IF  W-REPLYO (W-ARCH-NDX) EQUAL SPACES                   EL690
070711*                OR                                               EL690
070711*            W-REPLYO (W-ARCH-NDX) EQUAL ZEROES                   EL690
070711*            MOVE LOW-VALUES     TO W-REPLY-DATE                  EL690
070711*                                                                 EL690
070711*        ELSE                                                     EL690
070711*            MOVE W-REPLYO (W-ARCH-NDX)                           EL690
070711*                                TO W-DEEDIT-FIELD                EL690
070711*                                                                 EL690
070711*            PERFORM 9200-DATE-EDIT THRU 9200-EXIT                EL690
070711*                                                                 EL690
070711*            IF  DC-ERROR-CODE EQUAL SPACES                       EL690
070711*                MOVE DC-BIN-DATE-1                               EL690
070711*                                TO W-REPLY-DATE                  EL690
070711*                MOVE DC-GREG-DATE-1-EDIT                         EL690
070711*                                TO W-REPLYO (W-ARCH-NDX)         EL690
070711*                                                                 EL690
070711*            ELSE                                                 EL690
070711*                MOVE ER-9245    TO EMI-ERROR                     EL690
070711*                MOVE AL-UANON   TO W-REPLYA (W-ARCH-NDX)         EL690
070711*                MOVE -1         TO W-REPLYL (W-ARCH-NDX)         EL690
070711*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        EL690
01158                                                                   EL690
070711*    IF  (W-REPLY-DATE NOT EQUAL SPACES                           EL690
070711*            OR                                                   EL690
070711*        W-RESEND-DATE-1 NOT EQUAL SPACES                         EL690
070711*            OR                                                   EL690
070711*        W-RESEND-DATE-2 NOT EQUAL SPACES                         EL690
070711*            OR                                                   EL690
070711*        W-RESEND-DATE-3 NOT EQUAL SPACES                         EL690
070711*            OR                                                   EL690
070711*        W-STATUS GREATER THAN SPACES)                            EL690
070711*            AND                                                  EL690
070711*        PI-690-ARCHIVE-NUM (W-ARCH-NDX) NOT EQUAL ZEROS          EL690
070711*        PERFORM 0740-UPDATE-ARCH-RECORD THRU 0740-EXIT.          EL690
01171                                                                   EL690
01172  0720-EXIT.                                                       EL690
01173      EXIT.                                                        EL690
01174                                                                   EL690
01175  0740-UPDATE-ARCH-RECORD.                                         EL690
01176                                                                   EL690
01177      MOVE LOW-VALUES             TO W-ARCT-KEY.                   EL690
01178      MOVE PI-690-ARCHIVE-NUM (W-ARCH-NDX)                         EL690
01179                                  TO W-ARCH-ARCHIVE-NO             EL690
01180                                     W-ARCT-ARCHIVE-NO             EL690
01181                                     W-HOLD-ARCHIVE.               EL690
01182      MOVE PI-COMPANY-CD          TO W-ARCH-COMPANY-CD             EL690
01183                                     W-ARCT-COMPANY-CD.            EL690
01184                                                                   EL690
01185      IF  W-SPECIAL-STATUS                                         EL690
01186          PERFORM 7800-DELETE-CYCLE THRU 7800-EXIT                 EL690
01187          GO TO 0740-EXIT.                                         EL690
01188                                                                   EL690
01189      EXEC CICS HANDLE CONDITION                                   EL690
01190          NOTFND  (8080-ARCH-NOT-FOUND)                            EL690
01191          ENDFILE (8080-ARCH-NOT-FOUND)                            EL690
01192          END-EXEC.                                                EL690
01193                                                                   EL690
01194      PERFORM 7600-READ-ARCH-FILE THRU 7600-EXIT.                  EL690
01195                                                                   EL690
01196      IF  W-REPLY-DATE NOT EQUAL SPACES                            EL690
01197                                                                   EL690
01198          IF  W-REPLY-DATE EQUAL LOW-VALUES                        EL690
01199              MOVE LOW-VALUES     TO LA-REPLY-DATE                 EL690
01200              MOVE 'A'            TO W-STATUS                      EL690
01201                                                                   EL690
01202          ELSE                                                     EL690
01203              MOVE W-REPLY-DATE   TO LA-REPLY-DATE                 EL690
01204              MOVE 'C'            TO W-STATUS.                     EL690
01205                                                                   EL690
01206      IF  W-RESEND-DATE-1 NOT EQUAL SPACES                         EL690
01207              AND                                                  EL690
01208          W-RESEND-DATE-1 NOT EQUAL LA-RESEND-DATE
01209                                                                   EL690
01210          IF  LA-SENT-DATE   GREATER THAN LOW-VALUES               EL690
01211              MOVE ER-7396        TO EMI-ERROR                     EL690
01212              MOVE AL-UANON       TO W-RSDATA (W-ARCH-NDX)         EL690
01213              MOVE -1             TO W-RSDATL (W-ARCH-NDX)         EL690
01214              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL690
01215                                                                   EL690
01216          ELSE                                                     EL690
01217              MOVE 'A'            TO W-STATUS                      EL690
01218              MOVE LOW-VALUES     TO LA-SENT-DATE                  EL690
01219              MOVE W-RESEND-DATE-1                                 EL690
01220                                  TO LA-RESEND-DATE.
01221                                                                   EL690
01222 *    IF  W-RESEND-DATE-2 NOT EQUAL SPACES                         EL690
01223 *            AND                                                  EL690
01224 *        W-RESEND-DATE-2 NOT EQUAL LA-RESEND-DATE-2               EL690
01225 *                                                                 EL690
01226 *        IF  LA-SENT-DATE-2 GREATER THAN LOW-VALUES               EL690
01227 *            MOVE ER-7396        TO EMI-ERROR                     EL690
01228 *            MOVE AL-UANON       TO W-RSFRMA (W-ARCH-NDX)         EL690
01229 *            MOVE -1             TO W-RSFRML (W-ARCH-NDX)         EL690
01230 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL690
01231 *                                                                 EL690
01232 *        ELSE                                                     EL690
01233 *            MOVE 'A'            TO W-STATUS                      EL690
01234 *            MOVE LOW-VALUES     TO LA-SENT-DATE-2                EL690
01235 *            MOVE W-RESEND-DATE-2                                 EL690
01236 *                                TO LA-RESEND-DATE-2.             EL690
01237                                                                   EL690
01238 *    IF  W-RESEND-DATE-3 NOT EQUAL SPACES                         EL690
01239 *            AND                                                  EL690
01240 *        W-RESEND-DATE-3 NOT EQUAL LA-RESEND-DATE-3               EL690
01241 *                                                                 EL690
01242 *        IF  LA-SENT-DATE-3 GREATER THAN LOW-VALUES               EL690
01243 *                                                                 EL690
01244 *            IF  PI-COMPANY-ID EQUAL 'LAP' OR 'RMC'               EL690
01245 *                MOVE ER-7396    TO EMI-ERROR                     EL690
01246 *                MOVE AL-UANON   TO W-RSSTSA (W-ARCH-NDX)         EL690
01247 *                MOVE -1         TO W-RSSTSL (W-ARCH-NDX)         EL690
01248 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL690
01249 *                                                                 EL690
01250 *            ELSE                                                 EL690
01251 *                MOVE LOW-VALUES TO LA-SENT-DATE-3                EL690
01252 *                MOVE W-RESEND-DATE-3                             EL690
01253 *                                TO LA-RESEND-DATE-3              EL690
01254 *                MOVE 'A'        TO W-STATUS                      EL690
01255 *                                                                 EL690
01256 *        ELSE                                                     EL690
01257 *            MOVE 'A'            TO W-STATUS                      EL690
01258 *            MOVE LOW-VALUES     TO LA-SENT-DATE-3                EL690
01259 *            MOVE W-RESEND-DATE-3                                 EL690
01260 *                                TO LA-RESEND-DATE-3.             EL690
01261                                                                   EL690
01262 *    IF  LA-RESEND-DATE-3 GREATER THAN LOW-VALUES                 EL690
01263 *                                                                 EL690
01264 *        IF  LA-RESEND-DATE-3 GREATER THAN LA-RESEND-DATE-2       EL690
01265 *            NEXT SENTENCE                                        EL690
01266 *                                                                 EL690
01267 *        ELSE                                                     EL690
01268 *            EXEC CICS UNLOCK                                     EL690
01269 *                DATASET (W-ARCH-FILE-ID)                         EL690
01270 *            END-EXEC                                             EL690
01271 *            MOVE ER-9281        TO EMI-ERROR                     EL690
01272 *            MOVE AL-UANON       TO W-RSFRMA (W-ARCH-NDX)         EL690
01273 *            MOVE -1             TO W-RSFRML (W-ARCH-NDX)         EL690
01274 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL690
01275 *            GO TO 0740-EXIT.                                     EL690
01276                                                                   EL690
01277 *    IF  LA-RESEND-DATE-2 GREATER THAN LOW-VALUES                 EL690
01278 *                                                                 EL690
01279 *        IF  LA-RESEND-DATE-2 GREATER THAN LA-RESEND-DATE
01280 *            NEXT SENTENCE                                        EL690
01281 *                                                                 EL690
01282 *        ELSE                                                     EL690
01283 *            EXEC CICS UNLOCK                                     EL690
01284 *                DATASET (W-ARCH-FILE-ID)                         EL690
01285 *            END-EXEC                                             EL690
01286 *                                                                 EL690
01287 *            MOVE ER-9281        TO EMI-ERROR                     EL690
01288 *            MOVE AL-UANON       TO W-RSDATA (W-ARCH-NDX)         EL690
01289 *            MOVE -1             TO W-RSDATL (W-ARCH-NDX)         EL690
01290 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL690
01291 *            GO TO 0740-EXIT.                                     EL690
01292                                                                   EL690
01293      IF  LA-STATUS EQUAL 'X'                                      EL690
01294              AND                                                  EL690
01295          W-ACTIVE                                                 EL690
01296                                                                   EL690
01297          IF  LA-VOIDED-DATE GREATER THAN LOW-VALUES               EL690
01298              MOVE 'V'            TO W-STATUS                      EL690
01299                                                                   EL690
01300          ELSE                                                     EL690
01301              IF  LA-REPLY-DATE GREATER THAN LOW-VALUES            EL690
01302                  MOVE 'C'        TO W-STATUS.                     EL690
01303                                                                   EL690
01304      IF  LA-STATUS EQUAL 'V'                                      EL690
01305              AND                                                  EL690
01306          W-ACTIVE                                                 EL690
01307          MOVE LOW-VALUES         TO LA-VOIDED-DATE.               EL690
01308                                                                   EL690
070711*    IF  LA-STATUS EQUAL 'C'                                      EL690
070711*            AND                                                  EL690
070711*        W-ACTIVE                                                 EL690
070711*        MOVE ER-7399            TO EMI-ERROR                     EL690
070711*        MOVE AL-UANON           TO W-STATSA (W-ARCH-NDX)         EL690
070711*        MOVE -1                 TO W-STATSL (W-ARCH-NDX)         EL690
070711*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL690
070711*        GO TO 0740-REWRITE.                                      EL690
01317                                                                   EL690
01318      IF  W-CLEAR-INITIAL                                             CL**3
01319          MOVE LOW-VALUES         TO LA-INITIAL-PRINT-DATE            CL**4
01320          MOVE 'A'                TO W-STATUS.                        CL**4
01321                                                                      CL**3
01322      IF  W-CHECK-STATUS                                              CL**3
01323          PERFORM 0750-CHECK-STATUS THRU 0750-EXIT                    CL**3
01324                                                                      CL**3
01325      ELSE                                                            CL**3
01326          IF  W-VALID-STATUS                                          CL**4
01327                  AND                                                 CL**4
01328              W-STATUS GREATER THAN SPACES                            CL**4
01329              MOVE W-STATUS       TO LA-STATUS.                       CL**3
01330                                                                   EL690
01331      IF  W-VOIDED                                                 EL690
01332          MOVE W-SAVE-BIN-DATE    TO LA-VOIDED-DATE.               EL690
01333                                                                   EL690
01334      IF  LA-REPLY-DATE GREATER THAN LOW-VALUES                    EL690
01335              AND                                                  EL690
01336          LA-STATUS EQUAL 'A'                                      EL690
01337          MOVE 'C'                TO LA-STATUS.                    EL690
01338                                                                   EL690
01339  0740-REWRITE.                                                    EL690
01340                                                                   EL690
01341      EXEC CICS REWRITE                                            EL690
01342          DATASET (W-ARCH-FILE-ID)                                 EL690
01343          FROM    (LETTER-ARCHIVE)                                 EL690
01344      END-EXEC.                                                    EL690
01345                                                                   EL690
01346      IF  W-NO-FIRST-CHANGE                                        EL690
01347          MOVE W-ARCH-NDX         TO W-FIRST-CHANGE-IND.           EL690
01348                                                                   EL690
01349  0740-EXIT.                                                       EL690
01350      EXIT.                                                        EL690
01351                                  EJECT                               CL**3
01352  0750-CHECK-STATUS.                                                  CL**3
01353                                                                      CL**4
01354      IF  LA-STATUS EQUAL '#'                                         CL**4
01355          MOVE 'A'                TO LA-STATUS                        CL**4
01356          GO TO 0750-EXIT.                                            CL**4
01357                                                                   EL690
01358      IF  LA-STATUS NOT EQUAL 'A' OR 'C'                              CL**3
01359          GO TO 0750-EXIT.                                            CL**3
01360                                                                      CL**3
01361      IF LA-INITIAL-PRINT-DATE > LOW-VALUES
01362         IF LA-RESEND-DATE > LOW-VALUES
01363            IF LA-SENT-DATE = LOW-VALUES
01364               MOVE 'A'           TO LA-STATUS
                 ELSE
                    MOVE 'C'           TO LA-STATUS
                 END-IF
              ELSE
                 MOVE 'C'              TO LA-STATUS
              END-IF
           ELSE
              MOVE 'A'                  TO LA-STATUS
           END-IF

01361 *    IF  LA-INITIAL-PRINT-DATE GREATER THAN LOW-VALUES               CL**3
01362 *        IF  LA-RESEND-DATE GREATER THAN LOW-VALUES
01363 *            IF  LA-SENT-DATE   NOT GREATER THAN LOW-VALUES          CL**3
01364 *                MOVE 'A'        TO LA-STATUS                        CL**3
01365 *                                                                    CL**3
01366 *            ELSE                                                    CL**3
01367 *                IF  LA-RESEND-DATE-2 GREATER THAN LOW-VALUES        CL**3
01368 *                                                                    CL**3
01369 *                    IF  LA-SENT-DATE-2 NOT GREATER THAN             CL**3
01370 *                            LOW-VALUES                              CL**3
01371 *                        MOVE 'A'        TO LA-STATUS                CL**3
01372 *                                                                    CL**3
01373 *                    ELSE                                            CL**3
01374 *                        IF  LA-RESEND-DATE-3 GREATER THAN           CL**3
01375 *                                LOW-VALUES                          CL**3
01376 *                                                                    CL**3
01377 *                            IF  LA-SENT-DATE-3 NOT GREATER          CL**3
01378 *                                    THAN LOW-VALUES                 CL**3
01379 *                                MOVE 'A' TO LA-STATUS               CL**3
01380 *                                                                    CL**3
01381 *                            ELSE                                    CL**3
01382 *                                MOVE 'C' TO LA-STATUS               CL**3
01383 *                                                                    CL**3
01384 *                        ELSE                                        CL**3
01385 *                            MOVE 'C'                                CL**3
01386 *                                TO LA-STATUS                        CL**3
01387 *                                                                    CL**3
01388 *                                                                    CL**3
01389 *                 ELSE                                               CL**3
01390 *                     MOVE 'C'   TO LA-STATUS                        CL**3
01391 *                                                                    CL**3
01392 *        ELSE                                                        CL**3
01393 *            MOVE 'C'            TO LA-STATUS                        CL**3
01394 *                                                                    CL**3
01395 *    ELSE                                                            CL**3
01396 *        MOVE 'A'                TO LA-STATUS.                       CL**3
01397                                                                      CL**3

           .
01398  0750-EXIT.                                                          CL**3
01399      EXIT.                                                           CL**3
01400                                  EJECT                               CL**3
01401  0800-CONVERT-EFFECT-DATE.                                        EL690
01402                                                                   EL690
01403       IF  PI-CERT-EFF-DT > LOW-VALUES
01404           MOVE PI-CERT-EFF-DT    TO DC-BIN-DATE-1                 EL690
01405                                     PI-690-INIT-EFFECT-DATE
01406           MOVE ' '               TO DC-OPTION-CODE                EL690
01407           PERFORM 9500-LINK-DATE-CONVERT                          EL690
01408               THRU 9500-EXIT                                      EL690
01409                                                                   EL690
01410           IF  DC-ERROR-CODE EQUAL SPACES                          EL690
01411               MOVE DC-GREG-DATE-1-EDIT                            EL690
01412                                  TO EFFDTEI                       EL690
01413                                     PI-690-INIT-EFF-DTE
01414               MOVE +8            TO EFFDTEL                       EL690
01415                                                                   EL690
01416           ELSE                                                    EL690
01417               MOVE LOW-VALUES    TO EFFDTEI                       EL690
01418               MOVE +8            TO EFFDTEL                       EL690
01419                                                                   EL690
01420       ELSE                                                        EL690
01421           MOVE LOW-VALUES        TO EFFDTEI                       EL690
01422           MOVE +8                TO EFFDTEL.                      EL690
01423                                                                   EL690
01424  0800-EXIT.                                                       EL690
01425      EXIT.                                                        EL690
01426                                  EJECT                            EL690
01427  1000-RECOVER-LAST-SCREEN.                                        EL690
01428                                                                   EL690
01429      MOVE LOW-VALUES             TO EL690AO.                      EL690
01430      MOVE 1                      TO W-FIRST-CHANGE-IND.           EL690
01431                                                                   EL690
01432      MOVE PI-690-BRWS-TYPE-IND   TO TYPEBRI.                      EL690
01433      MOVE +1                     TO TYPEBRL.                      EL690
01434                                                                   EL690
01435      IF  PI-690-INIT-CERT-PRIME GREATER THAN LOW-VALUES           EL690
01436              AND                                                  EL690
01437          PI-690-INIT-CERT-PRIME LESS THAN HIGH-VALUES             EL690
01438          MOVE PI-690-INIT-CERT-PRIME                              EL690
01439                                  TO CERTRPI                       EL690
01440          MOVE +10                TO CERTRPL                       EL690
01441          MOVE PI-690-INIT-SUFFIX TO SUFFIXI                       EL690
01442          MOVE +1                 TO SUFFIXL.                      EL690
01443                                                                   EL690
01444      IF  PI-690-INIT-CARRIER GREATER THAN LOW-VALUES              EL690
01445              AND                                                  EL690
01446          PI-690-INIT-CARRIER LESS THAN HIGH-VALUES                EL690
01447          MOVE PI-690-INIT-CARRIER                                 EL690
01448                                  TO CARRIERI                      EL690
01449          MOVE +1                 TO CARRIERL.                     EL690
01450                                                                   EL690
01451      IF  PI-690-INIT-GROUPING GREATER THAN LOW-VALUES             EL690
01452              AND                                                  EL690
01453          PI-690-INIT-GROUPING LESS THAN HIGH-VALUES               EL690
01454          MOVE PI-690-INIT-GROUPING                                EL690
01455                                  TO GROUPI                        EL690
01456          MOVE +6                 TO GROUPL.                       EL690
01457                                                                   EL690
01458      IF  PI-690-INIT-STATE GREATER THAN LOW-VALUES                EL690
01459              AND                                                  EL690
01460          PI-690-INIT-STATE LESS THAN HIGH-VALUES                  EL690
01461          MOVE PI-690-INIT-STATE  TO STATEI                        EL690
01462          MOVE +2                 TO STATEL.                       EL690
01463                                                                   EL690
01464      IF  PI-690-INIT-ACCOUNT GREATER THAN LOW-VALUES              EL690
01465              AND                                                  EL690
01466          PI-690-INIT-ACCOUNT LESS THAN HIGH-VALUES                EL690
01467          MOVE PI-690-INIT-ACCOUNT                                 EL690
01468                                  TO ACCTI                         EL690
01469          MOVE +10                TO ACCTL.                        EL690
01470                                                                   EL690
01471      IF  PI-690-INIT-FORM GREATER THAN LOW-VALUES                 EL690
01472              AND                                                  EL690
01473          PI-690-INIT-FORM LESS THAN HIGH-VALUES                   EL690
01474          MOVE PI-690-INIT-FORM   TO FORMI                         EL690
01475          MOVE +04                TO FORML.                        EL690
01476                                                                   EL690
01477      IF  PI-690-INIT-PROCESSOR GREATER THAN LOW-VALUES            EL690
01478              AND                                                  EL690
01479          PI-690-INIT-PROCESSOR LESS THAN HIGH-VALUES              EL690
01480          MOVE PI-690-INIT-PROCESSOR                               EL690
01481                                  TO PROCSRI                       EL690
01482          MOVE +04                TO PROCSRL.                      EL690
01483                                                                   EL690
01484      IF  PI-690-INIT-EFFECT-DATE > LOW-VALUES
01485              AND                                                  EL690
01486          PI-690-INIT-EFFECT-DATE < HIGH-VALUES
01487          MOVE PI-690-INIT-EFFECT-DATE
01488                                  TO DC-BIN-DATE-1                 EL690
01489          MOVE ' '                TO DC-OPTION-CODE                EL690
01490          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT            EL690
01491                                                                   EL690
01492          IF  DC-ERROR-CODE EQUAL SPACES                           EL690
01493              MOVE DC-GREG-DATE-1-EDIT                             EL690
01494                                  TO EFFDTEI                       EL690
01495              MOVE +8             TO EFFDTEL.                      EL690
01496                                                                   EL690
01497      IF  PI-690-INIT-ENTRY GREATER THAN LOW-VALUES                EL690
01498              AND                                                  EL690
01499          PI-690-INIT-ENTRY LESS THAN HIGH-VALUES                  EL690
01500              AND                                                  EL690
01501          PI-690-INIT-CONTROL-PREFIX NOT EQUAL 'CK'                EL690
01502          MOVE PI-690-INIT-ENTRY  TO ENTRYI                        EL690
01503          MOVE +6                 TO ENTRYL                        EL690
01504                                                                   EL690
01505      ELSE                                                         EL690
01506          IF  PI-690-INIT-CONTROL GREATER THAN ZEROS               EL690
01507                  AND                                              EL690
01508              PI-690-INIT-CONTROL LESS THAN 99999999               EL690
01509                  AND                                              EL690
01510              PI-690-INIT-CONTROL-PREFIX EQUAL 'CK'                EL690
01511              MOVE PI-690-INIT-CONTROL                             EL690
01512                                  TO CKCNTLI                       EL690
01513              MOVE +8             TO CKCNTLL.                      EL690
01514                                                                      CL**3
01515      IF  PI-690-VALID-SELECTION                                      CL**3
01516          MOVE PI-690-STATUS-SELECTION-IND                            CL**3
01517                                  TO STATSELI                         CL**3
01518          MOVE +1                 TO STATSELL                         CL**3
01519                                                                      CL**3
01520      ELSE                                                            CL**3
01521          MOVE 'N'                TO STATSELI                         CL**3
01522          MOVE +1                 TO STATSELL.                        CL**3
01523                                                                   EL690
01524      IF  PI-690-LAST-BRWS-FWRD                                    EL690
01525          MOVE '1'                TO EIBAID                        EL690
01526          GO TO 2000-PAGE-FORWARD.                                 EL690
01527                                                                   EL690
01528      IF  PI-690-LAST-BRWS-BWRD                                    EL690
01529          MOVE '2'                TO EIBAID                        EL690
01530          MOVE 'Y'                TO W-READ-PREV-TWICE-IND         EL690
01531          GO TO 3000-PAGE-BACKWARD.                                EL690
01532                                                                   EL690
01533      MOVE -1                     TO CORRSELL.                     EL690
01534      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
01535                                                                   EL690
01536  1000-EXIT.                                                       EL690
01537      EXIT.                                                        EL690
01538                                  EJECT                            EL690
01539  1200-SET-UP-COMPLETE-SCREEN.                                     EL690
01540                                                                      CL**5
01541      MOVE LOW-VALUES        TO EL690AO.                              CL**5
01542                                                                   EL690
01543      IF  PI-RETURN-TO-PROGRAM EQUAL 'EL6311'                      EL690
01544          GO TO 1220-COMING-FROM-EL6311.                           EL690
01545                                                                   EL690
01546      IF  PI-RETURN-TO-PROGRAM EQUAL 'EL652'                       EL690
01547          GO TO 1240-COMING-FROM-EL652.                            EL690
01548                                                                   EL690
01549      IF  PI-RETURN-TO-PROGRAM EQUAL 'EL1273'                      EL690
01550          MOVE LOW-VALUES         TO PI-689-WORK-AREA              EL690
01551          MOVE PI-CERT-PRIME      TO CERTRPI                       EL690
01552          MOVE +10                TO CERTRPL                       EL690
01553          MOVE PI-CERT-SFX        TO SUFFIXI                       EL690
01554          MOVE +1                 TO SUFFIXL                       EL690
01555          MOVE 1                  TO TYPEBRI                       EL690
01556          MOVE +1                 TO TYPEBRL                       EL690
01557                                                                   EL690
01558      ELSE                                                         EL690
01559          IF  PI-RETURN-TO-PROGRAM EQUAL 'EL6501'                  EL690
01560              MOVE 4              TO TYPEBRI                       EL690
01561              MOVE +1             TO TYPEBRL.                      EL690
01562                                                                   EL690
01563      MOVE PI-CARRIER             TO CARRIERI.                     EL690
01564      MOVE +1                     TO CARRIERL.                     EL690
01565      MOVE PI-GROUPING            TO GROUPI.                       EL690
01566      MOVE +6                     TO GROUPL.                       EL690
01567      MOVE PI-STATE               TO STATEI.                       EL690
01568      MOVE +2                     TO STATEL.                       EL690
01569      MOVE PI-ACCOUNT             TO ACCTI.                        EL690
01570      MOVE +10                    TO ACCTL.                        EL690
01571                                                                   EL690
01572      IF  PI-CERT-EFF-DT NOT EQUAL LOW-VALUES                      EL690
01573              AND                                                  EL690
01574          PI-CERT-EFF-DT NOT EQUAL SPACES                          EL690
01575          MOVE PI-CERT-EFF-DT     TO DC-BIN-DATE-1                 EL690
01576          MOVE ' '                TO DC-OPTION-CODE                EL690
01577          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT            EL690
01578                                                                   EL690
01579          IF  DC-ERROR-CODE EQUAL SPACES                           EL690
01580              MOVE DC-GREG-DATE-1-EDIT                             EL690
01581                                  TO EFFDTEI                       EL690
01582              MOVE +8             TO EFFDTEL.                      EL690
01583                                                                   EL690
01584      MOVE '1'                    TO EIBAID.                       EL690
01585                                                                   EL690
01586      PERFORM 0500-EDIT-SCREEN-DATA THRU 0500-EXIT.                EL690
01587      MOVE ER-7397                TO EMI-ERROR.                    EL690
01588      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
01589      GO TO 3000-PAGE-BACKWARD.                                    EL690
01590 *    GO TO 2000-PAGE-FORWARD.                                     EL690
01591                                                                   EL690
01592  1200-EXIT.                                                       EL690
01593      EXIT.                                                        EL690
01594                                  EJECT                            EL690
01595  1220-COMING-FROM-EL6311.                                         EL690
01596                                                                   EL690
01597      MOVE 1                      TO TYPEBRI.                         CL**2
01598      MOVE +1                     TO TYPEBRL.                      EL690
01599 *    MOVE PI-689-ENTRY-BATCH     TO ENTRYI.                          CL**4
01600 *    MOVE +6                     TO ENTRYL.                          CL**4
01601      MOVE PI-689-CERT-PRIME      TO CERTRPI.                      EL690
01602      MOVE +10                    TO CERTRPL.                      EL690
01603      MOVE PI-689-CERT-SFX        TO SUFFIXI.                      EL690
01604      MOVE +1                     TO SUFFIXL.                      EL690
01605      MOVE PI-689-CARRIER         TO CARRIERI.                     EL690
01606      MOVE +1                     TO CARRIERL.                     EL690
01607      MOVE PI-689-GROUPING        TO GROUPI.                       EL690
01608      MOVE +6                     TO GROUPL.                       EL690
01609      MOVE PI-689-STATE           TO STATEI.                       EL690
01610      MOVE +2                     TO STATEL.                       EL690
01611      MOVE PI-689-ACCOUNT         TO ACCTI.                        EL690
01612      MOVE +10                    TO ACCTL.                        EL690
01613                                                                   EL690
01614      IF  PI-689-EFF-DATE GREATER THAN LOW-VALUES                  EL690
01615          MOVE PI-689-EFF-DATE    TO DC-BIN-DATE-1                 EL690
01616          MOVE ' '                TO DC-OPTION-CODE                EL690
01617          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT            EL690
01618                                                                   EL690
01619          IF  DC-ERROR-CODE EQUAL SPACES                           EL690
01620              MOVE DC-GREG-DATE-1-EDIT                             EL690
01621                                  TO EFFDTEI                       EL690
01622              MOVE +8             TO EFFDTEL                       EL690
01623                                                                   EL690
01624          ELSE                                                     EL690
01625              MOVE LOW-VALUES     TO EFFDTEI                       EL690
01626              MOVE +8             TO EFFDTEL                       EL690
01627                                                                   EL690
01628      ELSE                                                         EL690
01629          MOVE LOW-VALUES         TO EFFDTEI                       EL690
01630          MOVE +8                 TO EFFDTEL.                      EL690
01631                                                                   EL690
01632      MOVE '1'                    TO EIBAID.                       EL690
01633                                                                   EL690
01634      PERFORM 0500-EDIT-SCREEN-DATA THRU 0500-EXIT.                EL690
01635      MOVE ER-7397                TO EMI-ERROR.                    EL690
01636      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
01637      GO TO 3000-PAGE-BACKWARD.                                    EL690
01638 *    GO TO 2000-PAGE-FORWARD.                                     EL690
01639                                                                   EL690
01640  1220-EXIT.                                                       EL690
01641      EXIT.                                                        EL690
01642                                  EJECT                            EL690
01643  1240-COMING-FROM-EL652.                                          EL690
01644                                                                   EL690
01645      MOVE 1                      TO TYPEBRI.                      EL690
01646      MOVE +1                     TO TYPEBRL.                      EL690
01647      MOVE PI-CR-FIN-RESP         TO CERTRPI.                      EL690
01648      MOVE +10                    TO CERTRPL.                      EL690
01649      MOVE PI-CR-TYPE             TO SUFFIXI.                      EL690
01650      MOVE +1                     TO SUFFIXL.                      EL690
01651      MOVE PI-CR-CARRIER          TO CARRIERI.                     EL690
01652      MOVE +1                     TO CARRIERL.                     EL690
01653      MOVE PI-CR-GROUPING         TO GROUPI.                       EL690
01654      MOVE +6                     TO GROUPL.                       EL690
01655      MOVE PI-CR-ACCOUNT          TO ACCTI.                        EL690
01656      MOVE +10                    TO ACCTL.                        EL690
01657                                                                   EL690
01658      MOVE AL-UANON               TO CARRIERA                      EL690
01659                                     GROUPA                        EL690
01660                                     ACCTA                         EL690
01661                                     TYPEBRA                       EL690
01662                                     CERTRPA.                      EL690
01663                                                                   EL690
01664      PERFORM 0500-EDIT-SCREEN-DATA THRU 0500-EXIT.                EL690
01665      MOVE ER-7397                TO EMI-ERROR.                    EL690
01666      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
01667      GO TO 3000-PAGE-BACKWARD.                                    EL690
01668 *    GO TO 2000-PAGE-FORWARD.                                     EL690
01669                                                                   EL690
01670  1240-EXIT.                                                       EL690
01671      EXIT.                                                        EL690
01672                                  EJECT                            EL690
01673  1500-SELECT-ARCHIVE-REC.                                         EL690
01674                                                                   EL690
01675      MOVE ZEROS                  TO PI-689-ARCHIVE-NUMBER.        EL690
01676                                                                   EL690
01677      IF  CORRSELL GREATER THAN +0                                 EL690
01678                                                                   EL690
01679          EXEC CICS BIF DEEDIT                                     EL690
01680              FIELD   (CORRSELI)                                   EL690
01681              LENGTH  (2)                                          EL690
01682              END-EXEC                                             EL690
01683                                                                   EL690
01684          IF  CORRSELI NUMERIC                                     EL690
01685                                                                   EL690
01686              IF  CORRSELI NOT LESS THAN 01                        EL690
01687                      AND                                          EL690
01688                  CORRSELI NOT GREATER THAN 12                     EL690
01689                  MOVE CORRSELI   TO W-ARCH-NDX                    EL690
01690                                                                   EL690
01691                  IF  PI-690-ARCHIVE-NUM (W-ARCH-NDX)              EL690
01692                           NOT EQUAL ZEROS                         EL690
01693                      MOVE PI-690-ARCHIVE-NUM (W-ARCH-NDX)         EL690
01694                                  TO PI-689-ARCHIVE-NUMBER         EL690
01695                      GO TO 1500-EXIT                              EL690
01696                                                                   EL690
01697                  ELSE                                             EL690
01698                      MOVE ER-9293                                 EL690
01699                                  TO EMI-ERROR                     EL690
01700                      MOVE -1     TO CORRSELL                      EL690
01701                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL690
01702                      GO TO 8200-SEND-DATAONLY                     EL690
01703                                                                   EL690
01704          ELSE                                                     EL690
01705              MOVE ER-9293        TO EMI-ERROR                     EL690
01706              MOVE -1             TO CORRSELL                      EL690
01707              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL690
01708              GO TO 8200-SEND-DATAONLY.                            EL690
01709                                                                   EL690
01710  1500-EXIT.                                                       EL690
01711      EXIT.                                                        EL690
01712                                  EJECT                            EL690
01713  2000-PAGE-FORWARD.                                               EL690
01714                                                                   EL690
01715      MOVE '1'                    TO PI-690-LAST-BROWSE-IND.       EL690
01716      MOVE +1                     TO W-ARCH-NDX.                   EL690
01717      MOVE ZEROS                  TO W-ARCH-NUMBER.                EL690
01718                                                                   EL690
01719      GO TO 2100-ARCH2-PROCESS                                     EL690
01720            2200-ARCH3-PROCESS                                     EL690
01721            2300-ARCH4-PROCESS                                     EL690
01722            2400-ARCH5-PROCESS                                     EL690
01723            2500-ARCH6-PROCESS                                     EL690
01724            2600-ARCH-PROCESS DEPENDING ON PI-690-BRWS-TYPE-IND.   EL690
01725                                                                   EL690
01726      MOVE ER-7384                TO EMI-ERROR.                    EL690
01727      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
01728      MOVE -1                     TO TYPEBRL.                      EL690
01729      MOVE AL-UABON               TO TYPEBRA.                      EL690
01730      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
01731                                                                   EL690
01732  2000-EXIT.                                                       EL690
01733      EXIT.                                                        EL690
01734                                  EJECT                            EL690
01735  2100-ARCH2-PROCESS.                                              EL690
01736                                                                   EL690
01737      IF  W-CHANGE-REQUESTED                                       EL690
01738          MOVE LOW-VALUES         TO W-ARCH2-KEY                   EL690
01739          MOVE PI-COMPANY-CD      TO W-ARCH2-COMPANY-CD            EL690
01740          MOVE PI-690-INIT-CERT-PRIME                              EL690
01741                                  TO W-ARCH2-CERT-PRIME            EL690
01742          MOVE PI-690-INIT-SUFFIX TO W-ARCH2-SUFFIX                EL690
01743          MOVE PI-690-INIT-CARRIER                                 EL690
01744                                  TO W-ARCH2-CARRIER               EL690
01745          MOVE PI-690-INIT-GROUPING                                EL690
01746                                  TO W-ARCH2-GROUPING              EL690
01747          MOVE PI-690-INIT-STATE  TO W-ARCH2-STATE                 EL690
01748          MOVE PI-690-INIT-ACCOUNT                                 EL690
01749                                  TO W-ARCH2-ACCOUNT               EL690
01750          MOVE PI-690-INIT-EFFECT-DATE                             EL690
01751                                  TO W-ARCH2-EFF-DTE               EL690
01752          MOVE PI-690-INIT-ARCHIVE-NO                              EL690
01753                                  TO W-ARCH2-ARCHIVE-NO            EL690
01754                                                                   EL690
01755      ELSE                                                         EL690
01756          IF  W-FIRST-CHANGE-FOUND                                 EL690
01757              MOVE LOW-VALUES     TO W-ARCH2-KEY                   EL690
01758              MOVE PI-COMPANY-CD  TO W-ARCH2-COMPANY-CD            EL690
01759              MOVE PI-690-FIRST-CERT-PRIME                         EL690
01760                                  TO W-ARCH2-CERT-PRIME            EL690
01761              MOVE PI-690-FIRST-SUFFIX                             EL690
01762                                  TO W-ARCH2-SUFFIX                EL690
01763              MOVE PI-690-FIRST-CARRIER                            EL690
01764                                  TO W-ARCH2-CARRIER               EL690
01765              MOVE PI-690-FIRST-GROUPING                           EL690
01766                                  TO W-ARCH2-GROUPING              EL690
01767              MOVE PI-690-FIRST-STATE                              EL690
01768                                  TO W-ARCH2-STATE                 EL690
01769              MOVE PI-690-FIRST-ACCOUNT                            EL690
01770                                  TO W-ARCH2-ACCOUNT               EL690
01771              MOVE PI-690-FIRST-EFFECT-DATE                        EL690
01772                                  TO W-ARCH2-EFF-DTE               EL690
01773              MOVE PI-690-FIRST-ARCHIVE-NO                         EL690
01774                                  TO W-ARCH2-ARCHIVE-NO            EL690
01775                                                                   EL690
01776          ELSE                                                     EL690
01777              MOVE LOW-VALUES     TO W-ARCH2-KEY                   EL690
01778              MOVE PI-COMPANY-CD  TO W-ARCH2-COMPANY-CD            EL690
01779              MOVE PI-690-LAST-CERT-PRIME                          EL690
01780                                  TO W-ARCH2-CERT-PRIME            EL690
01781              MOVE PI-690-LAST-SUFFIX                              EL690
01782                                  TO W-ARCH2-SUFFIX                EL690
01783              MOVE PI-690-LAST-CARRIER                             EL690
01784                                  TO W-ARCH2-CARRIER               EL690
01785              MOVE PI-690-LAST-GROUPING                            EL690
01786                                  TO W-ARCH2-GROUPING              EL690
01787              MOVE PI-690-LAST-STATE                               EL690
01788                                  TO W-ARCH2-STATE                 EL690
01789              MOVE PI-690-LAST-ACCOUNT                             EL690
01790                                  TO W-ARCH2-ACCOUNT               EL690
01791              MOVE PI-690-LAST-EFFECT-DATE                         EL690
01792                                  TO W-ARCH2-EFF-DTE               EL690
01793              MOVE PI-690-LAST-ARCHIVE-NO                          EL690
01794                                  TO W-ARCH2-ARCHIVE-NO.           EL690
01795                                                                   EL690
01796      EXEC CICS HANDLE CONDITION                                   EL690
01797          NOTFND  (8080-ARCH-NOT-FOUND)                            EL690
01798          ENDFILE (8080-ARCH-NOT-FOUND)                            EL690
01799          NOTOPEN (8000-ARCH2-NOT-OPEN)                            EL690
01800          END-EXEC.                                                EL690
01801                                                                   EL690
01802      EXEC CICS STARTBR                                            EL690
01803          DATASET  (W-ARCH2-FILE-ID)                               EL690
01804          RIDFLD   (W-ARCH2-KEY)                                   EL690
01805          GTEQ                                                     EL690
01806          END-EXEC.                                                EL690
01807                                                                   EL690
01808      PERFORM 7000-READ-ARCH2-FILE-NEXT THRU 7000-EXIT.            EL690
01809                                                                   EL690
01810      EXEC CICS HANDLE CONDITION                                   EL690
01811          NOTFND  (2100-END-BROWSE)                                EL690
01812          ENDFILE (2100-END-BROWSE)                                EL690
01813          END-EXEC.                                                EL690
01814                                                                   EL690
01815  2100-BUILD-ARCH2-LIST.                                           EL690
01816                                                                   EL690
01817      IF  LA-COMPANY-CD-A2 NOT EQUAL PI-COMPANY-CD                 EL690
01818          GO TO 2100-END-BROWSE.                                   EL690
01819                                                                   EL690
01820      IF  PI-690-INIT-CERT-NO EQUAL LOW-VALUES OR HIGH-VALUES      EL690
01821          GO TO 2100-CONTINUE.                                     EL690
01822                                                                   EL690
01823      IF  LA-CERT-NO-A2 NOT EQUAL PI-690-INIT-CERT-NO              EL690
01824          GO TO 2100-END-BROWSE.                                   EL690
01825                                                                   EL690
01826      IF  PI-690-INIT-CARRIER EQUAL LOW-VALUES OR HIGH-VALUES      EL690
01827          GO TO 2100-CONTINUE.                                     EL690
01828                                                                   EL690
01829      IF  LA-CARRIER-A2 NOT EQUAL PI-690-INIT-CARRIER              EL690
01830          GO TO 2100-END-BROWSE.                                   EL690
01831                                                                   EL690
01832      IF  PI-690-INIT-GROUPING EQUAL LOW-VALUES OR HIGH-VALUES     EL690
01833          GO TO 2100-CONTINUE.                                     EL690
01834                                                                   EL690
01835      IF  LA-GROUPING-A2 NOT EQUAL PI-690-INIT-GROUPING            EL690
01836          GO TO 2100-END-BROWSE.                                   EL690
01837                                                                   EL690
01838      IF  PI-690-INIT-STATE EQUAL LOW-VALUES OR HIGH-VALUES        EL690
01839          GO TO 2100-CONTINUE.                                     EL690
01840                                                                   EL690
01841      IF  LA-STATE-A2 NOT EQUAL PI-690-INIT-STATE                  EL690
01842          GO TO 2100-END-BROWSE.                                   EL690
01843                                                                   EL690
01844      IF  PI-690-INIT-ACCOUNT EQUAL LOW-VALUES OR HIGH-VALUES      EL690
01845          GO TO 2100-CONTINUE.                                     EL690
01846                                                                   EL690
01847      IF  LA-ACCOUNT-A2 NOT EQUAL PI-690-INIT-ACCOUNT              EL690
01848          GO TO 2100-END-BROWSE.                                   EL690
01849                                                                   EL690
01850      IF  PI-690-INIT-EFFECT-DATE = LOW-VALUES OR HIGH-VALUES
01851          GO TO 2100-CONTINUE.                                     EL690
01852                                                                   EL690
01853      IF  LA-EFFECT-DATE-A2 NOT EQUAL PI-690-INIT-EFFECT-DATE      EL690
01854          GO TO 2100-END-BROWSE.                                   EL690
01855                                                                   EL690
01856  2100-CONTINUE.                                                   EL690
01857                                                                   EL690
01858      IF  NOT PI-690-SELECT-ALL                                       CL**3
01859              AND                                                     CL**3
01860          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND             CL**3
01861          PERFORM 7000-READ-ARCH2-FILE-NEXT THRU 7000-EXIT            CL**3
01862          GO TO 2100-BUILD-ARCH2-LIST.                                CL**3
01863                                                                      CL**3
01864      IF  PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES                   EL690
01865                  AND                                              EL690
01866          PI-690-INIT-ENTRY LESS THAN HIGH-VALUES                  EL690
01867          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6              EL690
01868              PERFORM 7000-READ-ARCH2-FILE-NEXT THRU 7000-EXIT     EL690
01869              GO TO 2100-BUILD-ARCH2-LIST.                         EL690
01870                                                                   EL690
01871      IF  PI-690-INIT-FORM NOT EQUAL LOW-VALUES                    EL690
01872                  AND                                              EL690
01873          PI-690-INIT-FORM LESS THAN HIGH-VALUES                   EL690
01874          IF  PI-690-INIT-FORM NOT EQUAL LA-FORM-A3                EL690
01875              PERFORM 7000-READ-ARCH2-FILE-NEXT THRU 7000-EXIT     EL690
01876              GO TO 2100-BUILD-ARCH2-LIST.                         EL690
01877                                                                   EL690
01878      IF  PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES               EL690
01879                  AND                                              EL690
01880          PI-690-INIT-PROCESSOR LESS THAN HIGH-VALUES              EL690
01881          IF  PI-690-INIT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD      EL690
01882              PERFORM 7000-READ-ARCH2-FILE-NEXT THRU 7000-EXIT     EL690
01883              GO TO 2100-BUILD-ARCH2-LIST.                         EL690
01884                                                                   EL690
01885      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.                EL690
01886                                                                   EL690
01887      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.               EL690
01888                                                                   EL690
01889      IF  W-ARCH-NDX LESS THAN +13                                 EL690
01890          PERFORM 7000-READ-ARCH2-FILE-NEXT THRU 7000-EXIT         EL690
01891          GO TO 2100-BUILD-ARCH2-LIST.                             EL690
01892                                                                   EL690
01893  2100-END.                                                        EL690
01894                                                                   EL690
01895      MOVE -1                     TO CORRSELL.                     EL690
01896      MOVE ZEROS                  TO CORRSELO.                     EL690
01897      PERFORM 2100-END-BROWSE.                                     EL690
01898      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
01899                                                                   EL690
01900  2100-END-BROWSE.                                                 EL690
01901                                                                   EL690
01902      EXEC CICS ENDBR                                              EL690
01903          DATASET (W-ARCH2-FILE-ID)                                EL690
01904      END-EXEC.                                                    EL690
01905                                                                   EL690
01906  2100-NOT-FOUND.                                                  EL690
01907                                                                   EL690
01908      MOVE ER-0130                TO EMI-ERROR.                    EL690
01909      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
01910      MOVE -1                     TO CORRSELL.                     EL690
01911      MOVE ZEROS                  TO CORRSELO.                     EL690
01912      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
01913                                                                   EL690
01914  2100-EXIT.                                                       EL690
01915      EXIT.                                                        EL690
01916                                  EJECT                            EL690
01917  2200-ARCH3-PROCESS.                                              EL690
01918                                                                   EL690
01919      IF  W-CHANGE-REQUESTED                                       EL690
01920          MOVE LOW-VALUES         TO W-ARCH3-KEY                   EL690
01921          MOVE PI-COMPANY-CD      TO W-ARCH3-COMPANY-CD            EL690
01922          MOVE PI-690-INIT-FORM   TO W-ARCH3-FORM                  EL690
01923          MOVE PI-690-INIT-CARRIER                                 EL690
01924                                  TO W-ARCH3-CARRIER               EL690
01925          MOVE PI-690-INIT-GROUPING                                EL690
01926                                  TO W-ARCH3-GROUPING              EL690
01927          MOVE PI-690-INIT-STATE  TO W-ARCH3-STATE                 EL690
01928          MOVE PI-690-INIT-ACCOUNT                                 EL690
01929                                  TO W-ARCH3-ACCOUNT               EL690
01930          MOVE PI-690-INIT-ARCHIVE-NO                              EL690
01931                                  TO W-ARCH3-ARCHIVE-NO            EL690
01932                                                                   EL690
01933      ELSE                                                         EL690
01934          IF  W-FIRST-CHANGE-FOUND                                 EL690
01935              MOVE LOW-VALUES     TO W-ARCH3-KEY                   EL690
01936              MOVE PI-COMPANY-CD  TO W-ARCH3-COMPANY-CD            EL690
01937              MOVE PI-690-FIRST-FORM                               EL690
01938                                  TO W-ARCH3-FORM                  EL690
01939              MOVE PI-690-FIRST-CARRIER                            EL690
01940                                  TO W-ARCH3-CARRIER               EL690
01941              MOVE PI-690-FIRST-GROUPING                           EL690
01942                                  TO W-ARCH3-GROUPING              EL690
01943              MOVE PI-690-FIRST-STATE                              EL690
01944                                  TO W-ARCH3-STATE                 EL690
01945              MOVE PI-690-FIRST-ACCOUNT                            EL690
01946                                  TO W-ARCH3-ACCOUNT               EL690
01947              MOVE PI-690-FIRST-ARCHIVE-NO                         EL690
01948                                  TO W-ARCH3-ARCHIVE-NO            EL690
01949                                                                   EL690
01950          ELSE                                                     EL690
01951              MOVE LOW-VALUES     TO W-ARCH3-KEY                   EL690
01952              MOVE PI-COMPANY-CD  TO W-ARCH3-COMPANY-CD            EL690
01953              MOVE PI-690-LAST-FORM                                EL690
01954                                  TO W-ARCH3-FORM                  EL690
01955              MOVE PI-690-LAST-CARRIER                             EL690
01956                                  TO W-ARCH3-CARRIER               EL690
01957              MOVE PI-690-LAST-GROUPING                            EL690
01958                                  TO W-ARCH3-GROUPING              EL690
01959              MOVE PI-690-LAST-STATE                               EL690
01960                                  TO W-ARCH3-STATE                 EL690
01961              MOVE PI-690-LAST-ACCOUNT                             EL690
01962                                  TO W-ARCH3-ACCOUNT               EL690
01963              MOVE PI-690-LAST-ARCHIVE-NO                          EL690
01964                                  TO W-ARCH3-ARCHIVE-NO.           EL690
01965                                                                   EL690
01966      EXEC CICS HANDLE CONDITION                                   EL690
01967          NOTFND  (8080-ARCH-NOT-FOUND)                            EL690
01968          ENDFILE (8080-ARCH-NOT-FOUND)                            EL690
01969          NOTOPEN (8010-ARCH3-NOT-OPEN)                            EL690
01970          END-EXEC.                                                EL690
01971                                                                   EL690
01972      EXEC CICS STARTBR                                            EL690
01973          DATASET  (W-ARCH3-FILE-ID)                               EL690
01974          RIDFLD   (W-ARCH3-KEY)                                   EL690
01975          GTEQ                                                     EL690
01976      END-EXEC.                                                    EL690
01977                                                                   EL690
01978      PERFORM 7100-READ-ARCH3-FILE-NEXT THRU 7100-EXIT.            EL690
01979                                                                   EL690
01980      EXEC CICS HANDLE CONDITION                                   EL690
01981          NOTFND  (2200-END-BROWSE)                                EL690
01982          ENDFILE (2200-END-BROWSE)                                EL690
01983          END-EXEC.                                                EL690
01984                                                                   EL690
01985  2200-BUILD-ARCH3-LIST.                                           EL690
01986                                                                   EL690
01987      IF  LA-COMPANY-CD-A3 NOT EQUAL PI-COMPANY-CD                 EL690
01988          GO TO 2200-END-BROWSE.                                   EL690
01989                                                                   EL690
01990      IF  PI-690-INIT-FORM EQUAL LOW-VALUES OR HIGH-VALUES         EL690
01991          GO TO 2200-CONTINUE.                                     EL690
01992                                                                   EL690
01993      IF  LA-FORM-A3 NOT EQUAL PI-690-INIT-FORM                    EL690
01994          GO TO 2200-END-BROWSE.                                   EL690
01995                                                                   EL690
01996      IF  PI-690-INIT-CARRIER EQUAL LOW-VALUES OR HIGH-VALUES      EL690
01997          GO TO 2200-CONTINUE.                                     EL690
01998                                                                   EL690
01999      IF  LA-CARRIER-A3 NOT EQUAL PI-690-INIT-CARRIER              EL690
02000          GO TO 2200-END-BROWSE.                                   EL690
02001                                                                   EL690
02002      IF  PI-690-INIT-GROUPING EQUAL LOW-VALUES OR HIGH-VALUES     EL690
02003          GO TO 2200-CONTINUE.                                     EL690
02004                                                                   EL690
02005      IF  LA-GROUPING-A3 NOT EQUAL PI-690-INIT-GROUPING            EL690
02006          GO TO 2200-END-BROWSE.                                   EL690
02007                                                                   EL690
02008      IF  PI-690-INIT-STATE EQUAL LOW-VALUES OR HIGH-VALUES        EL690
02009          GO TO 2200-CONTINUE.                                     EL690
02010                                                                   EL690
02011      IF  LA-STATE-A3 NOT EQUAL PI-690-INIT-STATE                  EL690
02012          GO TO 2200-END-BROWSE.                                   EL690
02013                                                                   EL690
02014      IF  PI-690-INIT-ACCOUNT EQUAL LOW-VALUES OR HIGH-VALUES      EL690
02015          GO TO 2200-CONTINUE.                                     EL690
02016                                                                   EL690
02017      IF  LA-ACCOUNT-A3 NOT EQUAL PI-690-INIT-ACCOUNT              EL690
02018          GO TO 2200-END-BROWSE.                                   EL690
02019                                                                   EL690
02020  2200-CONTINUE.                                                   EL690
02021                                                                   EL690
02022      IF  NOT PI-690-SELECT-ALL                                       CL**3
02023              AND                                                     CL**3
02024          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND             CL**3
02025          PERFORM 7100-READ-ARCH3-FILE-NEXT THRU 7100-EXIT            CL**3
02026          GO TO 2200-BUILD-ARCH3-LIST.                                CL**3
02027                                                                      CL**3
02028      IF  PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES                   EL690
02029                  AND                                              EL690
02030          PI-690-INIT-ENTRY LESS THAN HIGH-VALUES                  EL690
02031          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6              EL690
02032              PERFORM 7100-READ-ARCH3-FILE-NEXT THRU 7100-EXIT     EL690
02033              GO TO 2200-BUILD-ARCH3-LIST.                         EL690
02034                                                                   EL690
02035      IF  PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES               EL690
02036                  AND                                              EL690
02037          PI-690-INIT-PROCESSOR LESS THAN HIGH-VALUES              EL690
02038          IF  PI-690-INIT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD      EL690
02039              PERFORM 7100-READ-ARCH3-FILE-NEXT THRU 7100-EXIT     EL690
02040              GO TO 2200-BUILD-ARCH3-LIST.                         EL690
02041                                                                   EL690
02042      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.                EL690
02043                                                                   EL690
02044      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.               EL690
02045                                                                   EL690
02046      IF  W-ARCH-NDX LESS THAN +13                                 EL690
02047          PERFORM 7100-READ-ARCH3-FILE-NEXT THRU 7100-EXIT         EL690
02048          GO TO 2200-BUILD-ARCH3-LIST.                             EL690
02049                                                                   EL690
02050  2200-END.                                                        EL690
02051                                                                   EL690
02052      MOVE -1                     TO CORRSELL.                     EL690
02053      MOVE ZEROS                  TO CORRSELO.                     EL690
02054      PERFORM 2200-END-BROWSE.                                     EL690
02055      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
02056                                                                   EL690
02057  2200-END-BROWSE.                                                 EL690
02058                                                                   EL690
02059      EXEC CICS ENDBR                                              EL690
02060          DATASET (W-ARCH3-FILE-ID)                                EL690
02061      END-EXEC.                                                    EL690
02062                                                                   EL690
02063  2200-ARCH3-NOT-FOUND.                                            EL690
02064                                                                   EL690
02065      MOVE ER-0130                TO EMI-ERROR.                    EL690
02066      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
02067      MOVE -1                     TO CORRSELL.                     EL690
02068      MOVE ZEROS                  TO CORRSELO.                     EL690
02069      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
02070                                                                   EL690
02071  2200-EXIT.                                                       EL690
02072      EXIT.                                                        EL690
02073                                  EJECT                            EL690
02074  2300-ARCH4-PROCESS.                                              EL690
02075                                                                   EL690
02076      IF  W-CHANGE-REQUESTED                                       EL690
02077          MOVE LOW-VALUES         TO W-ARCH4-KEY                   EL690
02078          MOVE PI-COMPANY-CD      TO W-ARCH4-COMPANY-CD            EL690
02079          MOVE PI-690-INIT-PROCESSOR                               EL690
02080                                  TO W-ARCH4-PROCESSOR             EL690
02081          MOVE PI-690-INIT-CARRIER                                 EL690
02082                                  TO W-ARCH4-CARRIER               EL690
02083          MOVE PI-690-INIT-GROUPING                                EL690
02084                                  TO W-ARCH4-GROUPING              EL690
02085          MOVE PI-690-INIT-STATE  TO W-ARCH4-STATE                 EL690
02086          MOVE PI-690-INIT-ACCOUNT                                 EL690
02087                                  TO W-ARCH4-ACCOUNT               EL690
02088          MOVE PI-690-INIT-ARCHIVE-NO                              EL690
02089                                  TO W-ARCH4-ARCHIVE-NO            EL690
02090                                                                   EL690
02091      ELSE                                                         EL690
02092          IF  W-FIRST-CHANGE-FOUND                                 EL690
02093              MOVE LOW-VALUES     TO W-ARCH4-KEY                   EL690
02094              MOVE PI-COMPANY-CD  TO W-ARCH4-COMPANY-CD            EL690
02095              MOVE PI-690-FIRST-PROCESSOR                          EL690
02096                                  TO W-ARCH4-PROCESSOR             EL690
02097              MOVE PI-690-FIRST-CARRIER                            EL690
02098                                  TO W-ARCH4-CARRIER               EL690
02099              MOVE PI-690-FIRST-GROUPING                           EL690
02100                                  TO W-ARCH4-GROUPING              EL690
02101              MOVE PI-690-FIRST-STATE                              EL690
02102                                  TO W-ARCH4-STATE                 EL690
02103              MOVE PI-690-FIRST-ACCOUNT                            EL690
02104                                  TO W-ARCH4-ACCOUNT               EL690
02105              MOVE PI-690-FIRST-ARCHIVE-NO                         EL690
02106                                  TO W-ARCH4-ARCHIVE-NO            EL690
02107                                                                   EL690
02108          ELSE                                                     EL690
02109              MOVE LOW-VALUES     TO W-ARCH4-KEY                   EL690
02110              MOVE PI-COMPANY-CD  TO W-ARCH4-COMPANY-CD            EL690
02111              MOVE PI-690-LAST-PROCESSOR                           EL690
02112                                  TO W-ARCH4-PROCESSOR             EL690
02113              MOVE PI-690-LAST-CARRIER                             EL690
02114                                  TO W-ARCH4-CARRIER               EL690
02115              MOVE PI-690-LAST-GROUPING                            EL690
02116                                  TO W-ARCH4-GROUPING              EL690
02117              MOVE PI-690-LAST-STATE                               EL690
02118                                  TO W-ARCH4-STATE                 EL690
02119              MOVE PI-690-LAST-ACCOUNT                             EL690
02120                                  TO W-ARCH4-ACCOUNT               EL690
02121              MOVE PI-690-LAST-ARCHIVE-NO                          EL690
02122                                  TO W-ARCH4-ARCHIVE-NO.           EL690
02123                                                                   EL690
02124      EXEC CICS HANDLE CONDITION                                   EL690
02125          NOTFND  (8080-ARCH-NOT-FOUND)                            EL690
02126          ENDFILE (8080-ARCH-NOT-FOUND)                            EL690
02127          NOTOPEN (8020-ARCH4-NOT-OPEN)                            EL690
02128          END-EXEC.                                                EL690
02129                                                                   EL690
02130      EXEC CICS STARTBR                                            EL690
02131          DATASET  (W-ARCH4-FILE-ID)                               EL690
02132          RIDFLD   (W-ARCH4-KEY)                                   EL690
02133          GTEQ                                                     EL690
02134      END-EXEC.                                                    EL690
02135                                                                   EL690
02136      PERFORM 7200-READ-ARCH4-FILE-NEXT THRU 7200-EXIT.            EL690
02137                                                                   EL690
02138      EXEC CICS HANDLE CONDITION                                   EL690
02139          NOTFND  (2300-END-BROWSE)                                EL690
02140          ENDFILE (2300-END-BROWSE)                                EL690
02141          END-EXEC.                                                EL690
02142                                                                   EL690
02143  2300-BUILD-ARCH4-LIST.                                           EL690
02144                                                                   EL690
02145      IF  LA-COMPANY-CD-A4 NOT EQUAL PI-COMPANY-CD                 EL690
02146          GO TO 2300-END-BROWSE.                                   EL690
02147                                                                   EL690
02148      IF  PI-690-INIT-PROCESSOR EQUAL LOW-VALUES OR HIGH-VALUES    EL690
02149          GO TO 2300-CONTINUE.                                     EL690
02150                                                                   EL690
02151      IF  LA-PROCESSOR-CD NOT EQUAL PI-690-INIT-PROCESSOR          EL690
02152          GO TO 2300-END-BROWSE.                                   EL690
02153                                                                   EL690
02154      IF  PI-690-INIT-CARRIER EQUAL LOW-VALUES OR HIGH-VALUES      EL690
02155          GO TO 2300-CONTINUE.                                     EL690
02156                                                                   EL690
02157      IF  LA-CARRIER-A4 NOT EQUAL PI-690-INIT-CARRIER              EL690
02158          GO TO 2300-END-BROWSE.                                   EL690
02159                                                                   EL690
02160      IF  PI-690-INIT-GROUPING EQUAL LOW-VALUES OR HIGH-VALUES     EL690
02161          GO TO 2300-CONTINUE.                                     EL690
02162                                                                   EL690
02163      IF  LA-GROUPING-A4 NOT EQUAL PI-690-INIT-GROUPING            EL690
02164          GO TO 2300-END-BROWSE.                                   EL690
02165                                                                   EL690
02166      IF  PI-690-INIT-STATE EQUAL LOW-VALUES OR HIGH-VALUES        EL690
02167          GO TO 2300-CONTINUE.                                     EL690
02168                                                                   EL690
02169      IF  LA-STATE-A4 NOT EQUAL PI-690-INIT-STATE                  EL690
02170          GO TO 2300-END-BROWSE.                                   EL690
02171                                                                   EL690
02172      IF  PI-690-INIT-ACCOUNT EQUAL LOW-VALUES OR HIGH-VALUES      EL690
02173          GO TO 2300-CONTINUE.                                     EL690
02174                                                                   EL690
02175      IF  LA-ACCOUNT-A4 NOT EQUAL PI-690-INIT-ACCOUNT              EL690
02176          GO TO 2300-END-BROWSE.                                   EL690
02177                                                                   EL690
02178  2300-CONTINUE.                                                   EL690
02179                                                                   EL690
02180      IF  NOT PI-690-SELECT-ALL                                       CL**3
02181              AND                                                     CL**3
02182          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND             CL**3
02183          PERFORM 7200-READ-ARCH4-FILE-NEXT THRU 7200-EXIT            CL**3
02184          GO TO 2300-BUILD-ARCH4-LIST.                                CL**3
02185                                                                      CL**3
02186      IF  PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES                   EL690
02187                  AND                                              EL690
02188          PI-690-INIT-ENTRY LESS THAN HIGH-VALUES                  EL690
02189          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6              EL690
02190              PERFORM 7200-READ-ARCH4-FILE-NEXT THRU 7200-EXIT     EL690
02191              GO TO 2300-BUILD-ARCH4-LIST.                         EL690
02192                                                                   EL690
02193      IF  PI-690-INIT-FORM NOT EQUAL LOW-VALUES                    EL690
02194                  AND                                              EL690
02195          PI-690-INIT-FORM LESS THAN HIGH-VALUES                   EL690
02196          IF  PI-690-INIT-FORM NOT EQUAL LA-FORM-A3                EL690
02197              PERFORM 7200-READ-ARCH4-FILE-NEXT THRU 7200-EXIT     EL690
02198              GO TO 2300-BUILD-ARCH4-LIST.                         EL690
02199                                                                   EL690
02200      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.                EL690
02201                                                                   EL690
02202      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.               EL690
02203                                                                   EL690
02204      IF  W-ARCH-NDX LESS THAN +13                                 EL690
02205          PERFORM 7200-READ-ARCH4-FILE-NEXT THRU 7200-EXIT         EL690
02206          GO TO 2300-BUILD-ARCH4-LIST.                             EL690
02207                                                                   EL690
02208  2300-END.                                                        EL690
02209                                                                   EL690
02210      MOVE -1                     TO CORRSELL.                     EL690
02211      MOVE ZEROS                  TO CORRSELO.                     EL690
02212      PERFORM 2300-END-BROWSE.                                     EL690
02213      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
02214                                                                   EL690
02215  2300-END-BROWSE.                                                 EL690
02216                                                                   EL690
02217      EXEC CICS ENDBR                                              EL690
02218          DATASET (W-ARCH4-FILE-ID)                                EL690
02219      END-EXEC.                                                    EL690
02220                                                                   EL690
02221  2300-ARCH4-NOT-FOUND.                                            EL690
02222                                                                   EL690
02223      MOVE ER-0130                TO EMI-ERROR.                    EL690
02224      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
02225      MOVE -1                     TO CORRSELL.                     EL690
02226      MOVE ZEROS                  TO CORRSELO.                     EL690
02227      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
02228                                                                   EL690
02229  2300-EXIT.                                                       EL690
02230      EXIT.                                                        EL690
02231                                  EJECT                            EL690
02232  2400-ARCH5-PROCESS.                                              EL690
02233                                                                   EL690
02234      IF  W-CHANGE-REQUESTED                                       EL690
02235          MOVE LOW-VALUES         TO W-ARCH5-KEY                   EL690
02236          MOVE PI-COMPANY-CD      TO W-ARCH5-COMPANY-CD            EL690
02237          MOVE PI-690-INIT-CARRIER                                 EL690
02238                                  TO W-ARCH5-CARRIER               EL690
02239          MOVE PI-690-INIT-GROUPING                                EL690
02240                                  TO W-ARCH5-GROUPING              EL690
02241          MOVE PI-690-INIT-STATE  TO W-ARCH5-STATE                 EL690
02242          MOVE PI-690-INIT-ACCOUNT                                 EL690
02243                                  TO W-ARCH5-ACCOUNT               EL690
02244          MOVE PI-690-INIT-ARCHIVE-NO                              EL690
02245                                  TO W-ARCH5-ARCHIVE-NO            EL690
02246                                                                   EL690
02247      ELSE                                                         EL690
02248          IF  W-FIRST-CHANGE-FOUND                                 EL690
02249              MOVE LOW-VALUES     TO W-ARCH5-KEY                   EL690
02250              MOVE PI-COMPANY-CD  TO W-ARCH5-COMPANY-CD            EL690
02251              MOVE PI-690-FIRST-CARRIER                            EL690
02252                                  TO W-ARCH5-CARRIER               EL690
02253              MOVE PI-690-FIRST-GROUPING                           EL690
02254                                  TO W-ARCH5-GROUPING              EL690
02255              MOVE PI-690-FIRST-STATE                              EL690
02256                                  TO W-ARCH5-STATE                 EL690
02257              MOVE PI-690-FIRST-ACCOUNT                            EL690
02258                                  TO W-ARCH5-ACCOUNT               EL690
02259              MOVE PI-690-FIRST-ARCHIVE-NO                         EL690
02260                                  TO W-ARCH5-ARCHIVE-NO            EL690
02261                                                                   EL690
02262          ELSE                                                     EL690
02263              MOVE LOW-VALUES     TO W-ARCH5-KEY                   EL690
02264              MOVE PI-COMPANY-CD  TO W-ARCH5-COMPANY-CD            EL690
02265              MOVE PI-690-LAST-CARRIER                             EL690
02266                                  TO W-ARCH5-CARRIER               EL690
02267              MOVE PI-690-LAST-GROUPING                            EL690
02268                                  TO W-ARCH5-GROUPING              EL690
02269              MOVE PI-690-LAST-STATE                               EL690
02270                                  TO W-ARCH5-STATE                 EL690
02271              MOVE PI-690-LAST-ACCOUNT                             EL690
02272                                  TO W-ARCH5-ACCOUNT               EL690
02273              MOVE PI-690-LAST-ARCHIVE-NO                          EL690
02274                                  TO W-ARCH5-ARCHIVE-NO.           EL690
02275                                                                   EL690
02276      EXEC CICS HANDLE CONDITION                                   EL690
02277          NOTFND  (8080-ARCH-NOT-FOUND)                            EL690
02278          ENDFILE (8080-ARCH-NOT-FOUND)                            EL690
02279          NOTOPEN (8030-ARCH5-NOT-OPEN)                            EL690
02280          END-EXEC.                                                EL690
02281                                                                   EL690
02282      EXEC CICS STARTBR                                            EL690
02283          DATASET  (W-ARCH5-FILE-ID)                               EL690
02284          RIDFLD   (W-ARCH5-KEY)                                   EL690
02285          GTEQ                                                     EL690
02286      END-EXEC.                                                    EL690
02287                                                                   EL690
02288      PERFORM 7300-READ-ARCH5-FILE-NEXT THRU 7300-EXIT.            EL690
02289                                                                   EL690
02290      EXEC CICS HANDLE CONDITION                                   EL690
02291          NOTFND  (2400-END-BROWSE)                                EL690
02292          ENDFILE (2400-END-BROWSE)                                EL690
02293          END-EXEC.                                                EL690
02294                                                                   EL690
02295  2400-BUILD-ARCH5-LIST.                                           EL690
02296                                                                   EL690
02297      IF  LA-COMPANY-CD-A5 NOT EQUAL PI-COMPANY-CD                 EL690
02298          GO TO 2400-END-BROWSE.                                   EL690
02299                                                                   EL690
02300      IF  PI-690-INIT-CARRIER EQUAL LOW-VALUES OR HIGH-VALUES      EL690
02301          GO TO 2400-CONTINUE.                                     EL690
02302                                                                   EL690
02303      IF  LA-CARRIER-A5 NOT EQUAL PI-690-INIT-CARRIER              EL690
02304          GO TO 2400-END-BROWSE.                                   EL690
02305                                                                   EL690
02306      IF  PI-690-INIT-GROUPING EQUAL LOW-VALUES OR HIGH-VALUES     EL690
02307          GO TO 2400-CONTINUE.                                     EL690
02308                                                                   EL690
02309      IF  LA-GROUPING-A5 NOT EQUAL PI-690-INIT-GROUPING            EL690
02310          GO TO 2400-END-BROWSE.                                   EL690
02311                                                                   EL690
02312      IF  PI-690-INIT-STATE EQUAL LOW-VALUES OR HIGH-VALUES        EL690
02313          GO TO 2400-CONTINUE.                                     EL690
02314                                                                   EL690
02315      IF  LA-STATE-A5 NOT EQUAL PI-690-INIT-STATE                  EL690
02316          GO TO 2400-END-BROWSE.                                   EL690
02317                                                                   EL690
02318      IF  PI-690-INIT-ACCOUNT EQUAL LOW-VALUES OR HIGH-VALUES      EL690
02319          GO TO 2400-CONTINUE.                                     EL690
02320                                                                   EL690
02321      IF  LA-ACCOUNT-A5 NOT EQUAL PI-690-INIT-ACCOUNT              EL690
02322          GO TO 2400-END-BROWSE.                                   EL690
02323                                                                   EL690
02324  2400-CONTINUE.                                                   EL690
02325                                                                   EL690
02326      IF  NOT PI-690-SELECT-ALL                                       CL**3
02327              AND                                                     CL**3
02328          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND             CL**3
02329          PERFORM 7300-READ-ARCH5-FILE-NEXT THRU 7300-EXIT            CL**3
02330          GO TO 2400-BUILD-ARCH5-LIST.                                CL**3
02331                                                                      CL**3
02332      IF  PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES                   EL690
02333                  AND                                              EL690
02334          PI-690-INIT-ENTRY LESS THAN HIGH-VALUES                  EL690
02335          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6              EL690
02336              PERFORM 7300-READ-ARCH5-FILE-NEXT THRU 7300-EXIT     EL690
02337              GO TO 2400-BUILD-ARCH5-LIST.                         EL690
02338                                                                   EL690
02339      IF  PI-690-INIT-FORM NOT EQUAL LOW-VALUES                    EL690
02340                  AND                                              EL690
02341          PI-690-INIT-FORM LESS THAN HIGH-VALUES                   EL690
02342          IF  PI-690-INIT-FORM NOT EQUAL LA-FORM-A3                EL690
02343              PERFORM 7300-READ-ARCH5-FILE-NEXT THRU 7300-EXIT     EL690
02344              GO TO 2400-BUILD-ARCH5-LIST.                         EL690
02345                                                                   EL690
02346      IF  PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES               EL690
02347                  AND                                              EL690
02348          PI-690-INIT-PROCESSOR LESS THAN HIGH-VALUES              EL690
02349          IF  PI-690-INIT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD      EL690
02350              PERFORM 7300-READ-ARCH5-FILE-NEXT THRU 7300-EXIT     EL690
02351              GO TO 2400-BUILD-ARCH5-LIST.                         EL690
02352                                                                   EL690
02353      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.                EL690
02354                                                                   EL690
02355      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.               EL690
02356                                                                   EL690
02357      IF  W-ARCH-NDX LESS THAN +13                                 EL690
02358          PERFORM 7300-READ-ARCH5-FILE-NEXT THRU 7300-EXIT         EL690
02359          GO TO 2400-BUILD-ARCH5-LIST.                             EL690
02360                                                                   EL690
02361  2400-END.                                                        EL690
02362                                                                   EL690
02363      MOVE -1                     TO CORRSELL.                     EL690
02364      MOVE ZEROS                  TO CORRSELO.                     EL690
02365      PERFORM 2400-END-BROWSE.                                     EL690
02366      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
02367                                                                   EL690
02368  2400-END-BROWSE.                                                 EL690
02369                                                                   EL690
02370      EXEC CICS ENDBR                                              EL690
02371          DATASET (W-ARCH5-FILE-ID)                                EL690
02372      END-EXEC.                                                    EL690
02373                                                                   EL690
02374  2400-ARCH5-NOT-FOUND.                                            EL690
02375                                                                   EL690
02376      MOVE ER-0130                TO EMI-ERROR.                    EL690
02377      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
02378      MOVE -1                     TO CORRSELL.                     EL690
02379      MOVE ZEROS                  TO CORRSELO.                     EL690
02380      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
02381                                                                   EL690
02382  2400-EXIT.                                                       EL690
02383      EXIT.                                                        EL690
02384                                  EJECT                            EL690
02385  2500-ARCH6-PROCESS.                                              EL690
02386                                                                   EL690
02387      IF  W-CHANGE-REQUESTED                                       EL690
02388          MOVE LOW-VALUES         TO W-ARCH6-KEY                   EL690
02389          MOVE PI-COMPANY-CD      TO W-ARCH6-COMPANY-CD            EL690
02390          MOVE PI-690-INIT-ENTRY  TO W-ARCH6-ENTRY                 EL690
02391          MOVE PI-690-INIT-ARCHIVE-NO                              EL690
02392                                  TO W-ARCH6-ARCHIVE-NO            EL690
02393                                                                   EL690
02394      ELSE                                                         EL690
02395          IF  W-FIRST-CHANGE-FOUND                                 EL690
02396              MOVE LOW-VALUES     TO W-ARCH6-KEY                   EL690
02397              MOVE PI-COMPANY-CD  TO W-ARCH6-COMPANY-CD            EL690
02398              MOVE PI-690-FIRST-ENTRY                              EL690
02399                                  TO W-ARCH6-ENTRY                 EL690
02400              MOVE PI-690-FIRST-ARCHIVE-NO                         EL690
02401                                  TO W-ARCH6-ARCHIVE-NO            EL690
02402                                                                   EL690
02403          ELSE                                                     EL690
02404              MOVE LOW-VALUES     TO W-ARCH6-KEY                   EL690
02405              MOVE PI-COMPANY-CD  TO W-ARCH6-COMPANY-CD            EL690
02406              MOVE PI-690-LAST-ENTRY                               EL690
02407                                  TO W-ARCH6-ENTRY                 EL690
02408              MOVE PI-690-LAST-ARCHIVE-NO                          EL690
02409                                  TO W-ARCH6-ARCHIVE-NO.           EL690
02410                                                                   EL690
02411      EXEC CICS HANDLE CONDITION                                   EL690
02412          NOTFND  (8080-ARCH-NOT-FOUND)                            EL690
02413          ENDFILE (8080-ARCH-NOT-FOUND)                            EL690
02414          NOTOPEN (8040-ARCH6-NOT-OPEN)                            EL690
02415          END-EXEC.                                                EL690
02416                                                                   EL690
02417      EXEC CICS STARTBR                                            EL690
02418          DATASET  (W-ARCH6-FILE-ID)                               EL690
02419          RIDFLD   (W-ARCH6-KEY)                                   EL690
02420          GTEQ                                                     EL690
02421      END-EXEC.                                                    EL690
02422                                                                   EL690
02423      PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT.            EL690
02424                                                                   EL690
02425      EXEC CICS HANDLE CONDITION                                   EL690
02426          NOTFND  (2500-END-BROWSE)                                EL690
02427          ENDFILE (2500-END-BROWSE)                                EL690
02428          END-EXEC.                                                EL690
02429                                                                   EL690
02430  2500-BUILD-ARCH6-LIST.                                           EL690
02431                                                                   EL690
02432      IF  LA-COMPANY-CD-A6 NOT EQUAL PI-COMPANY-CD                 EL690
02433          GO TO 2500-END-BROWSE.                                   EL690
02434                                                                   EL690
02435      IF  PI-690-INIT-ENTRY EQUAL LOW-VALUES OR HIGH-VALUES        EL690
02436          GO TO 2500-CONTINUE.                                     EL690
02437                                                                   EL690
02438      IF  LA-ENTRY-A6 NOT EQUAL PI-690-INIT-ENTRY                  EL690
02439          GO TO 2500-END-BROWSE.                                   EL690
02440                                                                   EL690
02441  2500-CONTINUE.                                                   EL690
02442                                                                      CL**3
02443      IF  NOT PI-690-SELECT-ALL                                       CL**3
02444              AND                                                     CL**3
02445          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND             CL**3
02446          PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT            CL**3
02447          GO TO 2500-BUILD-ARCH6-LIST.                                CL**3
02448                                                                   EL690
02449      IF  PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES               EL690
02450              AND                                                  EL690
02451          PI-690-INIT-PROCESSOR NOT EQUAL HIGH-VALUES              EL690
02452          IF  LA-ENTRY-A6 NOT EQUAL PI-690-INIT-PROCESSOR          EL690
02453              PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT     EL690
02454              GO TO 2500-BUILD-ARCH6-LIST.                         EL690
02455                                                                   EL690
02456      IF  PI-690-INIT-FORM NOT EQUAL LOW-VALUES                    EL690
02457              AND                                                  EL690
02458          PI-690-INIT-FORM NOT EQUAL HIGH-VALUES                   EL690
02459          IF  LA-FORM-A3 NOT EQUAL PI-690-INIT-FORM                EL690
02460              PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT     EL690
02461              GO TO 2500-BUILD-ARCH6-LIST.                         EL690
02462                                                                   EL690
02463      IF  PI-690-INIT-CARRIER NOT EQUAL LOW-VALUES                 EL690
02464              AND                                                  EL690
02465          PI-690-INIT-CARRIER NOT EQUAL HIGH-VALUES                EL690
02466          IF  LA-CARRIER-A2 NOT EQUAL PI-690-INIT-CARRIER          EL690
02467              PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT     EL690
02468              GO TO 2500-BUILD-ARCH6-LIST.                         EL690
02469                                                                   EL690
02470      IF  PI-690-INIT-GROUPING NOT EQUAL LOW-VALUES                EL690
02471              AND                                                  EL690
02472          PI-690-INIT-GROUPING NOT EQUAL HIGH-VALUES               EL690
02473          IF  LA-GROUPING-A2 NOT EQUAL PI-690-INIT-GROUPING        EL690
02474              PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT     EL690
02475              GO TO 2500-BUILD-ARCH6-LIST.                         EL690
02476                                                                   EL690
02477      IF  PI-690-INIT-STATE NOT EQUAL LOW-VALUES                   EL690
02478              AND                                                  EL690
02479          PI-690-INIT-STATE NOT EQUAL HIGH-VALUES                  EL690
02480          IF  LA-STATE-A2 NOT EQUAL PI-690-INIT-STATE              EL690
02481              PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT     EL690
02482              GO TO 2500-BUILD-ARCH6-LIST.                         EL690
02483                                                                   EL690
02484      IF  PI-690-INIT-ACCOUNT NOT EQUAL LOW-VALUES                 EL690
02485              AND                                                  EL690
02486          PI-690-INIT-ACCOUNT NOT EQUAL HIGH-VALUES                EL690
02487          IF  LA-ACCOUNT-A2 NOT EQUAL PI-690-INIT-ACCOUNT          EL690
02488              PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT     EL690
02489              GO TO 2500-BUILD-ARCH6-LIST.                         EL690
02490                                                                   EL690
02491      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.                EL690
02492                                                                   EL690
02493      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.               EL690
02494                                                                   EL690
02495      IF  W-ARCH-NDX LESS THAN +13                                 EL690
02496          PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT         EL690
02497          GO TO 2500-BUILD-ARCH6-LIST.                             EL690
02498                                                                   EL690
02499  2500-END.                                                        EL690
02500                                                                   EL690
02501      MOVE -1                     TO CORRSELL.                     EL690
02502      MOVE ZEROS                  TO CORRSELO.                     EL690
02503      PERFORM 2500-END-BROWSE.                                     EL690
02504      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
02505                                                                   EL690
02506  2500-END-BROWSE.                                                 EL690
02507                                                                   EL690
02508      EXEC CICS ENDBR                                              EL690
02509          DATASET (W-ARCH6-FILE-ID)                                EL690
02510      END-EXEC.                                                    EL690
02511                                                                   EL690
02512  2500-ARCH6-NOT-FOUND.                                            EL690
02513                                                                   EL690
02514      MOVE ER-0130                TO EMI-ERROR.                    EL690
02515      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
02516      MOVE -1                     TO CORRSELL.                     EL690
02517      MOVE ZEROS                  TO CORRSELO.                     EL690
02518      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
02519                                                                   EL690
02520  2500-EXIT.                                                       EL690
02521      EXIT.                                                        EL690
02522                                  EJECT                            EL690
02523  2600-ARCH-PROCESS.                                               EL690
02524                                                                   EL690
02525      IF  W-CHANGE-REQUESTED                                       EL690
02526          MOVE LOW-VALUES         TO W-ARCH-KEY                    EL690
02527          MOVE PI-COMPANY-CD      TO W-ARCH-COMPANY-CD             EL690
02528          MOVE PI-690-INIT-ARCHIVE-NO                              EL690
02529                                  TO W-ARCH-ARCHIVE-NO             EL690
02530                                                                   EL690
02531      ELSE                                                         EL690
02532          IF  W-FIRST-CHANGE-FOUND                                 EL690
02533              MOVE LOW-VALUES     TO W-ARCH-KEY                    EL690
02534              MOVE PI-COMPANY-CD  TO W-ARCH-COMPANY-CD             EL690
02535              MOVE PI-690-FIRST-ARCHIVE-NO                         EL690
02536                                  TO W-ARCH-ARCHIVE-NO             EL690
02537                                                                   EL690
02538          ELSE                                                     EL690
02539              MOVE LOW-VALUES     TO W-ARCH-KEY                    EL690
02540              MOVE PI-COMPANY-CD  TO W-ARCH-COMPANY-CD             EL690
02541              MOVE PI-690-LAST-ARCHIVE-NO                          EL690
02542                                  TO W-ARCH-ARCHIVE-NO.            EL690
02543                                                                   EL690
02544      EXEC CICS HANDLE CONDITION                                   EL690
02545          NOTFND  (8080-ARCH-NOT-FOUND)                            EL690
02546          ENDFILE (8080-ARCH-NOT-FOUND)                            EL690
02547          NOTOPEN (8050-ARCH-NOT-OPEN)                             EL690
02548          END-EXEC.                                                EL690
02549                                                                   EL690
02550      EXEC CICS STARTBR                                            EL690
02551          DATASET  (W-ARCH-FILE-ID)                                EL690
02552          RIDFLD   (W-ARCH-KEY)                                    EL690
02553          GTEQ                                                     EL690
02554      END-EXEC.                                                    EL690
02555                                                                   EL690
02556      PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT.             EL690
02557                                                                   EL690
02558      EXEC CICS HANDLE CONDITION                                   EL690
02559          NOTFND  (2600-END-BROWSE)                                EL690
02560          ENDFILE (2600-END-BROWSE)                                EL690
02561          END-EXEC.                                                EL690
02562                                                                   EL690
02563  2600-BUILD-ARCH-LIST.                                            EL690
02564                                                                   EL690
02565      IF  LA-COMPANY-CD NOT EQUAL PI-COMPANY-CD                    EL690
02566          GO TO 2600-END-BROWSE.                                   EL690
02567                                                                   EL690
02568      IF  PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES               EL690
02569              AND                                                  EL690
02570          PI-690-INIT-PROCESSOR NOT EQUAL HIGH-VALUES              EL690
02571          IF  LA-PROCESSOR-CD NOT EQUAL PI-690-INIT-PROCESSOR      EL690
02572              PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT      EL690
02573              GO TO 2600-BUILD-ARCH-LIST.                          EL690
02574                                                                   EL690
02575      IF  PI-690-INIT-FORM NOT EQUAL LOW-VALUES                    EL690
02576              AND                                                  EL690
02577          PI-690-INIT-FORM NOT EQUAL HIGH-VALUES                   EL690
02578          IF  LA-FORM-A3 NOT EQUAL PI-690-INIT-FORM                EL690
02579              PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT      EL690
02580              GO TO 2600-BUILD-ARCH-LIST.                          EL690
02581                                                                   EL690
02582      IF  PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES                   EL690
02583              AND                                                  EL690
02584          PI-690-INIT-ENTRY NOT EQUAL HIGH-VALUES                  EL690
02585          IF  LA-ENTRY-A6 NOT EQUAL PI-690-INIT-ENTRY              EL690
02586              PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT      EL690
02587              GO TO 2600-BUILD-ARCH-LIST.                          EL690
02588                                                                   EL690
02589      IF  PI-690-INIT-CARRIER NOT EQUAL LOW-VALUES                 EL690
02590              AND                                                  EL690
02591          PI-690-INIT-CARRIER NOT EQUAL HIGH-VALUES                EL690
02592          IF  LA-CARRIER-A2 NOT EQUAL PI-690-INIT-CARRIER          EL690
02593              PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT      EL690
02594              GO TO 2600-BUILD-ARCH-LIST.                          EL690
02595                                                                   EL690
02596      IF  PI-690-INIT-GROUPING NOT EQUAL LOW-VALUES                EL690
02597              AND                                                  EL690
02598          PI-690-INIT-GROUPING NOT EQUAL HIGH-VALUES               EL690
02599          IF  LA-GROUPING-A2 NOT EQUAL PI-690-INIT-GROUPING        EL690
02600              PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT      EL690
02601              GO TO 2600-BUILD-ARCH-LIST.                          EL690
02602                                                                   EL690
02603      IF  PI-690-INIT-STATE NOT EQUAL LOW-VALUES                   EL690
02604              AND                                                  EL690
02605          PI-690-INIT-STATE NOT EQUAL HIGH-VALUES                  EL690
02606          IF  LA-STATE-A2 NOT EQUAL PI-690-INIT-STATE              EL690
02607              PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT      EL690
02608              GO TO 2600-BUILD-ARCH-LIST.                          EL690
02609                                                                   EL690
02610      IF  PI-690-INIT-ACCOUNT NOT EQUAL LOW-VALUES                 EL690
02611              AND                                                  EL690
02612          PI-690-INIT-ACCOUNT NOT EQUAL HIGH-VALUES                EL690
02613          IF  LA-ACCOUNT-A2 NOT EQUAL PI-690-INIT-ACCOUNT          EL690
02614              PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT      EL690
02615              GO TO 2600-BUILD-ARCH-LIST.                          EL690
02616                                                                   EL690
02617  2600-CONTINUE.                                                   EL690
02618                                                                      CL**3
02619      IF  NOT PI-690-SELECT-ALL                                       CL**3
02620              AND                                                     CL**3
02621          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND             CL**3
02622          PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT             CL**3
02623          GO TO 2600-BUILD-ARCH-LIST.                                 CL**3
02624                                                                   EL690
02625      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.                EL690
02626                                                                   EL690
02627      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.               EL690
02628                                                                   EL690
02629      IF  W-ARCH-NDX LESS THAN +13                                 EL690
02630          PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT          EL690
02631          GO TO 2600-BUILD-ARCH-LIST.                              EL690
02632                                                                   EL690
02633  2600-END.                                                        EL690
02634                                                                   EL690
02635      MOVE -1                     TO CORRSELL.                     EL690
02636      MOVE ZEROS                  TO CORRSELO.                     EL690
02637      PERFORM 2600-END-BROWSE.                                     EL690
02638      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
02639                                                                   EL690
02640  2600-END-BROWSE.                                                 EL690
02641                                                                   EL690
02642      EXEC CICS ENDBR                                              EL690
02643          DATASET (W-ARCH-FILE-ID)                                 EL690
02644      END-EXEC.                                                    EL690
02645                                                                   EL690
02646  2600-ARCH-NOT-FOUND.                                             EL690
02647                                                                   EL690
02648      MOVE ER-0130                TO EMI-ERROR.                    EL690
02649      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
02650      MOVE -1                     TO CORRSELL.                     EL690
02651      MOVE ZEROS                  TO CORRSELO.                     EL690
02652      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
02653                                                                   EL690
02654  2600-EXIT.                                                       EL690
02655      EXIT.                                                        EL690
02656                                  EJECT                            EL690
02657  3000-PAGE-BACKWARD.                                              EL690
02658                                                                   EL690
02659      MOVE '2'                    TO PI-690-LAST-BROWSE-IND.       EL690
02660      MOVE +1                     TO W-ARCH-NDX                    EL690
02661                                     W-ARCH-NDX2.                  EL690
02662      MOVE ZEROS                  TO W-ARCH-NUMBER.                EL690
02663                                                                   EL690
02664      GO TO 3100-ARCH2-PROCESS                                     EL690
02665            3200-ARCH3-PROCESS                                     EL690
02666            3300-ARCH4-PROCESS                                     EL690
02667            3400-ARCH5-PROCESS                                     EL690
02668            3500-ARCH6-PROCESS                                     EL690
02669            3600-ARCH-PROCESS DEPENDING ON PI-690-BRWS-TYPE-IND.   EL690
02670                                                                   EL690
02671      MOVE ER-7384                TO EMI-ERROR.                    EL690
02672      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
02673      MOVE -1                     TO TYPEBRL.                      EL690
02674      MOVE AL-UABON               TO TYPEBRA.                      EL690
02675      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
02676                                                                   EL690
02677  3000-EXIT.                                                       EL690
02678      EXIT.                                                        EL690
02679                                  EJECT                            EL690
02680  3100-ARCH2-PROCESS.                                              EL690
02681                                                                   EL690
02682      IF  W-CHANGE-REQUESTED                                       EL690
02683          MOVE HIGH-VALUES        TO W-ARCH2-KEY                   EL690
02684          MOVE PI-COMPANY-CD      TO W-ARCH2-COMPANY-CD            EL690
02685          MOVE PI-690-INIT-CERT-PRIME                              EL690
02686                                  TO W-ARCH2-CERT-PRIME            EL690
02687          MOVE PI-690-INIT-SUFFIX TO W-ARCH2-SUFFIX                EL690
02688          MOVE PI-690-INIT-CARRIER                                 EL690
02689                                  TO W-ARCH2-CARRIER               EL690
02690          MOVE PI-690-INIT-GROUPING                                EL690
02691                                  TO W-ARCH2-GROUPING              EL690
02692          MOVE PI-690-INIT-STATE  TO W-ARCH2-STATE                 EL690
02693          MOVE PI-690-INIT-ACCOUNT                                 EL690
02694                                  TO W-ARCH2-ACCOUNT               EL690
02695          MOVE PI-690-INIT-EFFECT-DATE                             EL690
02696                                  TO W-ARCH2-EFF-DTE               EL690
02697                                                                   EL690
02698                                                                   EL690
02699      ELSE                                                         EL690
02700          IF  W-FIRST-CHANGE-FOUND                                 EL690
02701              MOVE HIGH-VALUES    TO W-ARCH2-KEY                   EL690
02702              MOVE PI-COMPANY-CD  TO W-ARCH2-COMPANY-CD            EL690
02703              MOVE PI-690-LAST-CERT-PRIME                          EL690
02704                                  TO W-ARCH2-CERT-PRIME            EL690
02705              MOVE PI-690-LAST-SUFFIX                              EL690
02706                                  TO W-ARCH2-SUFFIX                EL690
02707              MOVE PI-690-LAST-CARRIER                             EL690
02708                                  TO W-ARCH2-CARRIER               EL690
02709              MOVE PI-690-LAST-GROUPING                            EL690
02710                                  TO W-ARCH2-GROUPING              EL690
02711              MOVE PI-690-LAST-STATE                               EL690
02712                                  TO W-ARCH2-STATE                 EL690
02713              MOVE PI-690-LAST-ACCOUNT                             EL690
02714                                  TO W-ARCH2-ACCOUNT               EL690
02715              MOVE PI-690-LAST-EFFECT-DATE                         EL690
02716                                  TO W-ARCH2-EFF-DTE               EL690
02717                                                                   EL690
02718          ELSE                                                     EL690
02719              MOVE PI-COMPANY-CD  TO W-ARCH2-COMPANY-CD            EL690
02720              MOVE PI-690-FIRST-CERT-PRIME                         EL690
02721                                  TO W-ARCH2-CERT-PRIME            EL690
02722              MOVE PI-690-FIRST-SUFFIX                             EL690
02723                                  TO W-ARCH2-SUFFIX                EL690
02724              MOVE PI-690-FIRST-CARRIER                            EL690
02725                                  TO W-ARCH2-CARRIER               EL690
02726              MOVE PI-690-FIRST-GROUPING                           EL690
02727                                  TO W-ARCH2-GROUPING              EL690
02728              MOVE PI-690-FIRST-STATE                              EL690
02729                                  TO W-ARCH2-STATE                 EL690
02730              MOVE PI-690-FIRST-ACCOUNT                            EL690
02731                                  TO W-ARCH2-ACCOUNT               EL690
02732              MOVE PI-690-FIRST-EFFECT-DATE                        EL690
02733                                  TO W-ARCH2-EFF-DTE               EL690
02734              MOVE PI-690-FIRST-ARCHIVE-NO                         EL690
02735                                  TO W-ARCH2-ARCHIVE-NO.           EL690
02736                                                                   EL690
02737      EXEC CICS HANDLE CONDITION                                   EL690
02738          NOTFND  (8080-ARCH-NOT-FOUND)                            EL690
02739          ENDFILE (8080-ARCH-NOT-FOUND)                            EL690
02740          NOTOPEN (8000-ARCH2-NOT-OPEN)                            EL690
02741          END-EXEC.                                                EL690
02742                                                                   EL690
02743      EXEC CICS STARTBR                                            EL690
02744          DATASET  (W-ARCH2-FILE-ID)                               EL690
02745          RIDFLD   (W-ARCH2-KEY)                                   EL690
02746          GTEQ                                                     EL690
02747          END-EXEC.                                                EL690
02748                                                                   EL690
02749      EXEC CICS HANDLE CONDITION                                   EL690
02750          NOTFND  (3100-END-BROWSE)                                EL690
02751          ENDFILE (3100-END-BROWSE)                                EL690
02752          END-EXEC.                                                EL690
02753                                                                   EL690
02754      PERFORM 7000-READ-ARCH2-FILE-NEXT THRU 7000-EXIT.            EL690
02755                                                                   EL690
02756      IF  W-CHANGE-REQUESTED                                       EL690
02757              OR                                                   EL690
02758          W-READ-PREV-TWICE                                        EL690
02759          MOVE SPACES            TO W-READ-PREV-TWICE-IND          EL690
02760          PERFORM 7050-READ-PREV-ARCH2 THRU 7050-EXIT.             EL690
02761                                                                   EL690
02762  3100-BUILD-ARCH2-LIST-BACKWARD.                                  EL690
02763                                                                   EL690
02764      PERFORM 7050-READ-PREV-ARCH2 THRU 7050-EXIT.                 EL690
02765                                                                   EL690
02766      IF  LA-COMPANY-CD-A2 NOT EQUAL PI-COMPANY-CD                 EL690
02767          GO TO 3100-END.                                          EL690
02768                                                                   EL690
02769      IF  PI-690-INIT-CERT-NO EQUAL HIGH-VALUES OR LOW-VALUES      EL690
02770          GO TO 3100-CONTINUE.                                     EL690
02771                                                                   EL690
02772      IF  LA-CERT-NO-A2 NOT EQUAL PI-690-INIT-CERT-NO              EL690
02773          GO TO 3100-END-BROWSE.                                   EL690
02774                                                                   EL690
02775      IF  PI-690-INIT-CARRIER EQUAL HIGH-VALUES OR LOW-VALUES      EL690
02776          GO TO 3100-CONTINUE.                                     EL690
02777                                                                   EL690
02778      IF  LA-CARRIER-A2 NOT EQUAL PI-690-INIT-CARRIER              EL690
02779          GO TO 3100-END-BROWSE.                                   EL690
02780                                                                   EL690
02781      IF  PI-690-INIT-GROUPING EQUAL HIGH-VALUES OR LOW-VALUES     EL690
02782          GO TO 3100-CONTINUE.                                     EL690
02783                                                                   EL690
02784      IF  LA-GROUPING-A2 NOT EQUAL PI-690-INIT-GROUPING            EL690
02785          GO TO 3100-END-BROWSE.                                   EL690
02786                                                                   EL690
02787      IF  PI-690-INIT-STATE EQUAL HIGH-VALUES OR LOW-VALUES        EL690
02788          GO TO 3100-CONTINUE.                                     EL690
02789                                                                   EL690
02790      IF  LA-STATE-A2 NOT EQUAL PI-690-INIT-STATE                  EL690
02791          GO TO 3100-END-BROWSE.                                   EL690
02792                                                                   EL690
02793      IF  PI-690-INIT-ACCOUNT EQUAL HIGH-VALUES OR LOW-VALUES      EL690
02794          GO TO 3100-CONTINUE.                                     EL690
02795                                                                   EL690
02796      IF  LA-ACCOUNT-A2 NOT EQUAL PI-690-INIT-ACCOUNT              EL690
02797          GO TO 3100-END-BROWSE.                                   EL690
02798                                                                   EL690
101512     IF  PI-690-INIT-EFFECT-DATE = LOW-VALUES OR HIGH-VALUES
101512         GO TO 3100-CONTINUE.
101512
101512     IF  LA-EFFECT-DATE-A2 NOT EQUAL PI-690-INIT-EFFECT-DATE
101512         GO TO 3100-END-BROWSE.
101512
02799  3100-CONTINUE.                                                   EL690
02800                                                                   EL690
02801      IF  NOT PI-690-SELECT-ALL                                       CL**3
02802              AND                                                     CL**3
02803          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND             CL**3
02804          GO TO 3100-BUILD-ARCH2-LIST-BACKWARD.                       CL**3
02805                                                                      CL**3
02806      IF  PI-690-INIT-ENTRY NOT EQUAL HIGH-VALUES                  EL690
02807              AND                                                  EL690
02808          PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES                   EL690
02809          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6              EL690
02810              GO TO 3100-BUILD-ARCH2-LIST-BACKWARD.                EL690
02811                                                                   EL690
02812      IF  PI-690-INIT-FORM NOT EQUAL HIGH-VALUES                   EL690
02813              AND                                                  EL690
02814          PI-690-INIT-FORM NOT EQUAL LOW-VALUES                    EL690
02815          IF  PI-690-INIT-FORM NOT EQUAL LA-FORM-A3                EL690
02816              GO TO 3100-BUILD-ARCH2-LIST-BACKWARD.                EL690
02817                                                                   EL690
02818      IF  PI-690-INIT-PROCESSOR NOT EQUAL HIGH-VALUES              EL690
02819              AND                                                  EL690
02820          PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES               EL690
02821          IF  PI-690-INIT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD      EL690
02822              GO TO 3100-BUILD-ARCH2-LIST-BACKWARD.                EL690
02823                                                                   EL690
02824      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.                EL690
02825                                                                   EL690
02826      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.               EL690
02827                                                                   EL690
02828      IF  W-ARCH-NDX LESS THAN +13                                 EL690
02829          GO TO 3100-BUILD-ARCH2-LIST-BACKWARD.                    EL690
02830                                                                   EL690
02831  3100-END.                                                        EL690
02832                                                                   EL690
02833      MOVE -1                     TO CORRSELL.                     EL690
02834      MOVE ZEROS                  TO CORRSELO.                     EL690
02835      PERFORM 3100-END-BROWSE                                      EL690
02836      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
02837                                                                   EL690
02838  3100-END-BROWSE.                                                 EL690
02839                                                                   EL690
02840      MOVE W-ARCH-NDX             TO W-ARCH-NDX3.                  EL690
02841      SUBTRACT +1 FROM W-ARCH-NDX3.                                EL690
02842                                                                   EL690
02843      IF  W-ARCH-NDX3 GREATER THAN +1                              EL690
02844          MOVE +1 TO W-ARCH-NDX2                                   EL690
02845          PERFORM 6500-REVERSE-TABLE-DATA THRU 6500-EXIT           EL690
02846                  UNTIL                                            EL690
02847              W-ARCH-NDX2 NOT LESS W-ARCH-NDX3.                    EL690
02848                                                                   EL690
02849      EXEC CICS ENDBR                                              EL690
02850          DATASET (W-ARCH2-FILE-ID)                                EL690
02851      END-EXEC.                                                    EL690
02852                                                                   EL690
02853  3100-FRONT-OF-FILE.                                              EL690
02854                                                                   EL690
02855      MOVE ER-0131                TO EMI-ERROR.                    EL690
02856      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
02857      MOVE -1                     TO CORRSELL.                     EL690
02858      MOVE ZEROS                  TO CORRSELO.                     EL690
02859      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
02860                                                                   EL690
02861  3100-EXIT.                                                       EL690
02862      EXIT.                                                        EL690
02863                                  EJECT                            EL690
02864  3200-ARCH3-PROCESS.                                              EL690
02865                                                                   EL690
02866      IF  W-CHANGE-REQUESTED                                       EL690
02867          MOVE HIGH-VALUES        TO W-ARCH3-KEY                   EL690
02868          MOVE PI-COMPANY-CD      TO W-ARCH3-COMPANY-CD            EL690
02869          MOVE PI-690-INIT-FORM   TO W-ARCH3-FORM                  EL690
02870          MOVE PI-690-INIT-CARRIER                                 EL690
02871                                  TO W-ARCH3-CARRIER               EL690
02872          MOVE PI-690-INIT-GROUPING                                EL690
02873                                  TO W-ARCH3-GROUPING              EL690
02874          MOVE PI-690-INIT-STATE  TO W-ARCH3-STATE                 EL690
02875          MOVE PI-690-INIT-ACCOUNT                                 EL690
02876                                  TO W-ARCH3-ACCOUNT               EL690
02877                                                                   EL690
02878      ELSE                                                         EL690
02879          IF  W-FIRST-CHANGE-FOUND                                 EL690
02880              MOVE HIGH-VALUES    TO W-ARCH3-KEY                   EL690
02881              MOVE PI-COMPANY-CD  TO W-ARCH3-COMPANY-CD            EL690
02882              MOVE PI-690-LAST-FORM                                EL690
02883                                  TO W-ARCH3-FORM                  EL690
02884              MOVE PI-690-LAST-CARRIER                             EL690
02885                                  TO W-ARCH3-CARRIER               EL690
02886              MOVE PI-690-LAST-GROUPING                            EL690
02887                                  TO W-ARCH3-GROUPING              EL690
02888              MOVE PI-690-LAST-STATE                               EL690
02889                                  TO W-ARCH3-STATE                 EL690
02890              MOVE PI-690-LAST-ACCOUNT                             EL690
02891                                  TO W-ARCH3-ACCOUNT               EL690
02892                                                                   EL690
02893          ELSE                                                     EL690
02894              MOVE PI-COMPANY-CD  TO W-ARCH3-COMPANY-CD            EL690
02895              MOVE PI-690-FIRST-FORM                               EL690
02896                                  TO W-ARCH3-FORM                  EL690
02897              MOVE PI-690-FIRST-CARRIER                            EL690
02898                                  TO W-ARCH3-CARRIER               EL690
02899              MOVE PI-690-FIRST-GROUPING                           EL690
02900                                  TO W-ARCH3-GROUPING              EL690
02901              MOVE PI-690-FIRST-STATE                              EL690
02902                                  TO W-ARCH3-STATE                 EL690
02903              MOVE PI-690-FIRST-ACCOUNT                            EL690
02904                                  TO W-ARCH3-ACCOUNT               EL690
02905              MOVE PI-690-FIRST-ARCHIVE-NO                         EL690
02906                                  TO W-ARCH3-ARCHIVE-NO.           EL690
02907                                                                   EL690
02908      EXEC CICS HANDLE CONDITION                                   EL690
02909          NOTFND  (8080-ARCH-NOT-FOUND)                            EL690
02910          ENDFILE (8080-ARCH-NOT-FOUND)                            EL690
02911          NOTOPEN (8010-ARCH3-NOT-OPEN)                            EL690
02912          END-EXEC.                                                EL690
02913                                                                   EL690
02914      EXEC CICS STARTBR                                            EL690
02915          DATASET  (W-ARCH3-FILE-ID)                               EL690
02916          RIDFLD   (W-ARCH3-KEY)                                   EL690
02917          GTEQ                                                     EL690
02918          END-EXEC.                                                EL690
02919                                                                   EL690
02920      EXEC CICS HANDLE CONDITION                                   EL690
02921          NOTFND  (3200-END-BROWSE)                                EL690
02922          ENDFILE (3200-END-BROWSE)                                EL690
02923          END-EXEC.                                                EL690
02924                                                                   EL690
02925      PERFORM 7100-READ-ARCH3-FILE-NEXT THRU 7100-EXIT.            EL690
02926                                                                   EL690
02927      IF  W-CHANGE-REQUESTED                                       EL690
02928              OR                                                   EL690
02929          W-READ-PREV-TWICE                                        EL690
02930          MOVE SPACES            TO W-READ-PREV-TWICE-IND          EL690
02931          PERFORM 7150-READ-PREV-ARCH3 THRU 7150-EXIT.             EL690
02932                                                                   EL690
02933  3200-BUILD-ARCH3-LIST-BACKWARD.                                  EL690
02934                                                                   EL690
02935      PERFORM 7150-READ-PREV-ARCH3 THRU 7150-EXIT.                 EL690
02936                                                                   EL690
02937      IF  LA-COMPANY-CD-A3 NOT EQUAL PI-COMPANY-CD                 EL690
02938          GO TO 3200-END.                                          EL690
02939                                                                   EL690
02940      IF  PI-690-INIT-FORM EQUAL HIGH-VALUES OR LOW-VALUES         EL690
02941          GO TO 3200-CONTINUE.                                     EL690
02942                                                                   EL690
02943      IF  LA-FORM-A3 NOT EQUAL PI-690-INIT-FORM                    EL690
02944          GO TO 3200-END-BROWSE.                                   EL690
02945                                                                   EL690
02946      IF  PI-690-INIT-CARRIER EQUAL HIGH-VALUES OR LOW-VALUES      EL690
02947          GO TO 3200-CONTINUE.                                     EL690
02948                                                                   EL690
02949      IF  LA-CARRIER-A3 NOT EQUAL PI-690-INIT-CARRIER              EL690
02950          GO TO 3200-END-BROWSE.                                   EL690
02951                                                                   EL690
02952      IF  PI-690-INIT-GROUPING EQUAL HIGH-VALUES OR LOW-VALUES     EL690
02953          GO TO 3200-CONTINUE.                                     EL690
02954                                                                   EL690
02955      IF  LA-GROUPING-A3 NOT EQUAL PI-690-INIT-GROUPING            EL690
02956          GO TO 3200-END-BROWSE.                                   EL690
02957                                                                   EL690
02958      IF  PI-690-INIT-STATE EQUAL HIGH-VALUES OR LOW-VALUES        EL690
02959          GO TO 3200-CONTINUE.                                     EL690
02960                                                                   EL690
02961      IF  LA-STATE-A3 NOT EQUAL PI-690-INIT-STATE                  EL690
02962          GO TO 3200-END-BROWSE.                                   EL690
02963                                                                   EL690
02964      IF  PI-690-INIT-ACCOUNT EQUAL HIGH-VALUES OR LOW-VALUES      EL690
02965          GO TO 3200-CONTINUE.                                     EL690
02966                                                                   EL690
02967      IF  LA-ACCOUNT-A3 NOT EQUAL PI-690-INIT-ACCOUNT              EL690
02968          GO TO 3200-END-BROWSE.                                   EL690
02969                                                                   EL690
02970  3200-CONTINUE.                                                   EL690
02971                                                                   EL690
02972      IF  NOT PI-690-SELECT-ALL                                       CL**3
02973              AND                                                     CL**3
02974          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND             CL**3
02975          GO TO 3200-BUILD-ARCH3-LIST-BACKWARD.                       CL**3
02976                                                                      CL**3
02977      IF  PI-690-INIT-ENTRY NOT EQUAL HIGH-VALUES                  EL690
02978              AND                                                  EL690
02979          PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES                   EL690
02980          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6              EL690
02981              GO TO 3200-BUILD-ARCH3-LIST-BACKWARD.                EL690
02982                                                                   EL690
02983      IF  PI-690-INIT-PROCESSOR NOT EQUAL HIGH-VALUES              EL690
02984              AND                                                  EL690
02985          PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES               EL690
02986          IF  PI-690-INIT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD      EL690
02987              GO TO 3200-BUILD-ARCH3-LIST-BACKWARD.                EL690
02988                                                                   EL690
02989      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.                EL690
02990                                                                   EL690
02991      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.               EL690
02992                                                                   EL690
02993      IF  W-ARCH-NDX LESS THAN +13                                 EL690
02994          GO TO 3200-BUILD-ARCH3-LIST-BACKWARD.                    EL690
02995                                                                   EL690
02996  3200-END.                                                        EL690
02997                                                                   EL690
02998      MOVE -1                     TO CORRSELL.                     EL690
02999      MOVE ZEROS                  TO CORRSELO.                     EL690
03000      PERFORM 3200-END-BROWSE.                                     EL690
03001      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
03002                                                                   EL690
03003  3200-END-BROWSE.                                                 EL690
03004                                                                   EL690
03005      MOVE W-ARCH-NDX             TO W-ARCH-NDX3.                  EL690
03006      SUBTRACT +1 FROM W-ARCH-NDX3.                                EL690
03007                                                                   EL690
03008      IF  W-ARCH-NDX3 GREATER THAN +1                              EL690
03009          MOVE +1 TO W-ARCH-NDX2                                   EL690
03010          PERFORM 6500-REVERSE-TABLE-DATA THRU 6500-EXIT           EL690
03011                  UNTIL                                            EL690
03012              W-ARCH-NDX2 NOT LESS W-ARCH-NDX3.                    EL690
03013                                                                   EL690
03014      EXEC CICS ENDBR                                              EL690
03015          DATASET (W-ARCH3-FILE-ID)                                EL690
03016      END-EXEC.                                                    EL690
03017                                                                   EL690
03018  3200-FRONT-OF-FILE.                                              EL690
03019                                                                   EL690
03020      MOVE ER-0131                TO EMI-ERROR.                    EL690
03021      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
03022      MOVE -1                     TO CORRSELL.                     EL690
03023      MOVE ZEROS                  TO CORRSELO.                     EL690
03024      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
03025                                                                   EL690
03026  3200-EXIT.                                                       EL690
03027      EXIT.                                                        EL690
03028                                  EJECT                            EL690
03029  3300-ARCH4-PROCESS.                                              EL690
03030                                                                   EL690
03031      IF  W-CHANGE-REQUESTED                                       EL690
03032          MOVE HIGH-VALUES        TO W-ARCH4-KEY                   EL690
03033          MOVE PI-COMPANY-CD      TO W-ARCH4-COMPANY-CD            EL690
03034          MOVE PI-690-INIT-PROCESSOR                               EL690
03035                                  TO W-ARCH4-PROCESSOR             EL690
03036          MOVE PI-690-INIT-CARRIER                                 EL690
03037                                  TO W-ARCH4-CARRIER               EL690
03038          MOVE PI-690-INIT-GROUPING                                EL690
03039                                  TO W-ARCH4-GROUPING              EL690
03040          MOVE PI-690-INIT-STATE  TO W-ARCH4-STATE                 EL690
03041          MOVE PI-690-INIT-ACCOUNT                                 EL690
03042                                  TO W-ARCH4-ACCOUNT               EL690
03043                                                                   EL690
03044      ELSE                                                         EL690
03045          IF  W-FIRST-CHANGE-FOUND                                 EL690
03046              MOVE HIGH-VALUES    TO W-ARCH4-KEY                   EL690
03047              MOVE PI-COMPANY-CD  TO W-ARCH4-COMPANY-CD            EL690
03048              MOVE PI-690-LAST-PROCESSOR                           EL690
03049                                  TO W-ARCH4-PROCESSOR             EL690
03050              MOVE PI-690-LAST-CARRIER                             EL690
03051                                  TO W-ARCH4-CARRIER               EL690
03052              MOVE PI-690-LAST-GROUPING                            EL690
03053                                  TO W-ARCH4-GROUPING              EL690
03054              MOVE PI-690-LAST-STATE                               EL690
03055                                  TO W-ARCH4-STATE                 EL690
03056              MOVE PI-690-LAST-ACCOUNT                             EL690
03057                                  TO W-ARCH4-ACCOUNT               EL690
03058                                                                   EL690
03059          ELSE                                                     EL690
03060              MOVE PI-COMPANY-CD  TO W-ARCH4-COMPANY-CD            EL690
03061              MOVE PI-690-FIRST-PROCESSOR                          EL690
03062                                  TO W-ARCH4-PROCESSOR             EL690
03063              MOVE PI-690-FIRST-CARRIER                            EL690
03064                                  TO W-ARCH4-CARRIER               EL690
03065              MOVE PI-690-FIRST-GROUPING                           EL690
03066                                  TO W-ARCH4-GROUPING              EL690
03067              MOVE PI-690-FIRST-STATE                              EL690
03068                                  TO W-ARCH4-STATE                 EL690
03069              MOVE PI-690-FIRST-ACCOUNT                            EL690
03070                                  TO W-ARCH4-ACCOUNT               EL690
03071              MOVE PI-690-FIRST-ARCHIVE-NO                         EL690
03072                                  TO W-ARCH4-ARCHIVE-NO.           EL690
03073                                                                   EL690
03074      EXEC CICS HANDLE CONDITION                                   EL690
03075          NOTFND  (8080-ARCH-NOT-FOUND)                            EL690
03076          ENDFILE (8080-ARCH-NOT-FOUND)                            EL690
03077          NOTOPEN (8020-ARCH4-NOT-OPEN)                            EL690
03078          END-EXEC.                                                EL690
03079                                                                   EL690
03080      EXEC CICS STARTBR                                            EL690
03081          DATASET  (W-ARCH4-FILE-ID)                               EL690
03082          RIDFLD   (W-ARCH4-KEY)                                   EL690
03083          GTEQ                                                     EL690
03084          END-EXEC.                                                EL690
03085                                                                   EL690
03086      EXEC CICS HANDLE CONDITION                                   EL690
03087          NOTFND  (3300-END-BROWSE)                                EL690
03088          ENDFILE (3300-END-BROWSE)                                EL690
03089          END-EXEC.                                                EL690
03090                                                                   EL690
03091      PERFORM 7200-READ-ARCH4-FILE-NEXT THRU 7200-EXIT.            EL690
03092                                                                   EL690
03093      IF  W-CHANGE-REQUESTED                                       EL690
03094              OR                                                   EL690
03095          W-READ-PREV-TWICE                                        EL690
03096          MOVE SPACES            TO W-READ-PREV-TWICE-IND          EL690
03097          PERFORM 7250-READ-PREV-ARCH4 THRU 7250-EXIT.             EL690
03098                                                                   EL690
03099  3300-BUILD-ARCH4-LIST-BACKWARD.                                  EL690
03100                                                                   EL690
03101      PERFORM 7250-READ-PREV-ARCH4 THRU 7250-EXIT.                 EL690
03102                                                                   EL690
03103      IF  LA-COMPANY-CD-A3 NOT EQUAL PI-COMPANY-CD                 EL690
03104          GO TO 3300-END.                                          EL690
03105                                                                   EL690
03106      IF  PI-690-INIT-PROCESSOR EQUAL HIGH-VALUES OR LOW-VALUES    EL690
03107          GO TO 3300-CONTINUE.                                     EL690
03108                                                                   EL690
03109      IF  LA-PROCESSOR-CD NOT EQUAL PI-690-INIT-PROCESSOR          EL690
03110          GO TO 3300-END-BROWSE.                                   EL690
03111                                                                   EL690
03112      IF  PI-690-INIT-CARRIER EQUAL HIGH-VALUES OR LOW-VALUES      EL690
03113          GO TO 3300-CONTINUE.                                     EL690
03114                                                                   EL690
03115      IF  LA-CARRIER-A4 NOT EQUAL PI-690-INIT-CARRIER              EL690
03116          GO TO 3300-END-BROWSE.                                   EL690
03117                                                                   EL690
03118      IF  PI-690-INIT-GROUPING EQUAL HIGH-VALUES OR LOW-VALUES     EL690
03119          GO TO 3300-CONTINUE.                                     EL690
03120                                                                   EL690
03121      IF  LA-GROUPING-A4 NOT EQUAL PI-690-INIT-GROUPING            EL690
03122          GO TO 3300-END-BROWSE.                                   EL690
03123                                                                   EL690
03124      IF  PI-690-INIT-STATE EQUAL HIGH-VALUES OR LOW-VALUES        EL690
03125          GO TO 3300-CONTINUE.                                     EL690
03126                                                                   EL690
03127      IF  LA-STATE-A4 NOT EQUAL PI-690-INIT-STATE                  EL690
03128          GO TO 3300-END-BROWSE.                                   EL690
03129                                                                   EL690
03130      IF  PI-690-INIT-ACCOUNT EQUAL HIGH-VALUES OR LOW-VALUES      EL690
03131          GO TO 3300-CONTINUE.                                     EL690
03132                                                                   EL690
03133      IF  LA-ACCOUNT-A4 NOT EQUAL PI-690-INIT-ACCOUNT              EL690
03134          GO TO 3300-END-BROWSE.                                   EL690
03135                                                                   EL690
03136  3300-CONTINUE.                                                   EL690
03137                                                                   EL690
03138      IF  NOT PI-690-SELECT-ALL                                       CL**3
03139              AND                                                     CL**3
03140          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND             CL**3
03141          GO TO 3300-BUILD-ARCH4-LIST-BACKWARD.                       CL**3
03142                                                                      CL**3
03143      IF  PI-690-INIT-ENTRY NOT EQUAL HIGH-VALUES                  EL690
03144              AND                                                  EL690
03145          PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES                   EL690
03146          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6              EL690
03147              GO TO 3300-BUILD-ARCH4-LIST-BACKWARD.                EL690
03148                                                                   EL690
03149      IF  PI-690-INIT-FORM NOT EQUAL HIGH-VALUES                   EL690
03150              AND                                                  EL690
03151          PI-690-INIT-FORM NOT EQUAL LOW-VALUES                    EL690
03152          IF  PI-690-INIT-FORM NOT EQUAL LA-FORM-A3                EL690
03153              GO TO 3300-BUILD-ARCH4-LIST-BACKWARD.                EL690
03154                                                                   EL690
03155      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.                EL690
03156                                                                   EL690
03157      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.               EL690
03158                                                                   EL690
03159      IF  W-ARCH-NDX LESS THAN +13                                 EL690
03160          GO TO 3300-BUILD-ARCH4-LIST-BACKWARD.                    EL690
03161                                                                   EL690
03162  3300-END.                                                        EL690
03163                                                                   EL690
03164      MOVE -1                     TO CORRSELL.                     EL690
03165      MOVE ZEROS                  TO CORRSELO.                     EL690
03166      PERFORM 3300-END-BROWSE                                      EL690
03167      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
03168                                                                   EL690
03169  3300-END-BROWSE.                                                 EL690
03170                                                                   EL690
03171      MOVE W-ARCH-NDX             TO W-ARCH-NDX3.                  EL690
03172      SUBTRACT +1 FROM W-ARCH-NDX3.                                EL690
03173                                                                   EL690
03174      IF  W-ARCH-NDX3 GREATER THAN +1                              EL690
03175          MOVE +1 TO W-ARCH-NDX2                                   EL690
03176          PERFORM 6500-REVERSE-TABLE-DATA THRU 6500-EXIT           EL690
03177                  UNTIL                                            EL690
03178              W-ARCH-NDX2 NOT LESS W-ARCH-NDX3.                    EL690
03179                                                                   EL690
03180      EXEC CICS ENDBR                                              EL690
03181          DATASET (W-ARCH4-FILE-ID)                                EL690
03182      END-EXEC.                                                    EL690
03183                                                                   EL690
03184  3300-FRONT-OF-FILE.                                              EL690
03185                                                                   EL690
03186      MOVE ER-0131                TO EMI-ERROR.                    EL690
03187      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
03188      MOVE -1                     TO CORRSELL.                     EL690
03189      MOVE ZEROS                  TO CORRSELO.                     EL690
03190      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
03191                                                                   EL690
03192  3300-EXIT.                                                       EL690
03193      EXIT.                                                        EL690
03194                                  EJECT                            EL690
03195  3400-ARCH5-PROCESS.                                              EL690
03196                                                                   EL690
03197      IF  W-CHANGE-REQUESTED                                       EL690
03198          MOVE HIGH-VALUES        TO W-ARCH5-KEY                   EL690
03199          MOVE PI-COMPANY-CD      TO W-ARCH5-COMPANY-CD            EL690
03200          MOVE PI-690-INIT-CARRIER                                 EL690
03201                                  TO W-ARCH5-CARRIER               EL690
03202          MOVE PI-690-INIT-GROUPING                                EL690
03203                                  TO W-ARCH5-GROUPING              EL690
03204          MOVE PI-690-INIT-STATE  TO W-ARCH5-STATE                 EL690
03205          MOVE PI-690-INIT-ACCOUNT                                 EL690
03206                                  TO W-ARCH5-ACCOUNT               EL690
03207                                                                   EL690
03208      ELSE                                                         EL690
03209          IF  W-FIRST-CHANGE-FOUND                                 EL690
03210              MOVE SPACES         TO W-READ-PREV-TWICE-IND         EL690
03211              MOVE HIGH-VALUES    TO W-ARCH5-KEY                   EL690
03212              MOVE PI-COMPANY-CD  TO W-ARCH5-COMPANY-CD            EL690
03213              MOVE PI-690-LAST-CARRIER                             EL690
03214                                  TO W-ARCH5-CARRIER               EL690
03215              MOVE PI-690-LAST-GROUPING                            EL690
03216                                  TO W-ARCH5-GROUPING              EL690
03217              MOVE PI-690-LAST-STATE                               EL690
03218                                  TO W-ARCH5-STATE                 EL690
03219              MOVE PI-690-LAST-ACCOUNT                             EL690
03220                                  TO W-ARCH5-ACCOUNT               EL690
03221              MOVE PI-690-LAST-ARCHIVE-NO                          EL690
03222                                  TO W-ARCH5-ARCHIVE-NO            EL690
03223                                                                   EL690
03224          ELSE                                                     EL690
03225              MOVE PI-COMPANY-CD  TO W-ARCH5-COMPANY-CD            EL690
03226              MOVE PI-690-FIRST-CARRIER                            EL690
03227                                  TO W-ARCH5-CARRIER               EL690
03228              MOVE PI-690-FIRST-GROUPING                           EL690
03229                                  TO W-ARCH5-GROUPING              EL690
03230              MOVE PI-690-FIRST-STATE                              EL690
03231                                  TO W-ARCH5-STATE                 EL690
03232              MOVE PI-690-FIRST-ACCOUNT                            EL690
03233                                  TO W-ARCH5-ACCOUNT               EL690
03234              MOVE PI-690-FIRST-ARCHIVE-NO                         EL690
03235                                  TO W-ARCH5-ARCHIVE-NO.           EL690
03236                                                                   EL690
03237      EXEC CICS HANDLE CONDITION                                   EL690
03238          NOTFND  (8080-ARCH-NOT-FOUND)                            EL690
03239          ENDFILE (8080-ARCH-NOT-FOUND)                            EL690
03240          NOTOPEN (8030-ARCH5-NOT-OPEN)                            EL690
03241          END-EXEC.                                                EL690
03242                                                                   EL690
03243      EXEC CICS STARTBR                                            EL690
03244          DATASET  (W-ARCH5-FILE-ID)                               EL690
03245          RIDFLD   (W-ARCH5-KEY)                                   EL690
03246          GTEQ                                                     EL690
03247          END-EXEC.                                                EL690
03248                                                                   EL690
03249      EXEC CICS HANDLE CONDITION                                   EL690
03250          NOTFND  (3400-END-BROWSE)                                EL690
03251          ENDFILE (3400-END-BROWSE)                                EL690
03252          END-EXEC.                                                EL690
03253                                                                   EL690
03254      PERFORM 7300-READ-ARCH5-FILE-NEXT THRU 7300-EXIT.            EL690
03255                                                                   EL690
03256      IF  W-CHANGE-REQUESTED                                       EL690
03257 *            OR                                                   EL690
03258 *        W-READ-PREV-TWICE                                        EL690
03259 *        MOVE SPACES            TO W-READ-PREV-TWICE-IND          EL690
03260          PERFORM 7350-READ-PREV-ARCH5 THRU 7350-EXIT.             EL690
03261                                                                   EL690
03262  3400-BUILD-ARCH5-LIST-BACKWARD.                                  EL690
03263                                                                   EL690
03264      PERFORM 7350-READ-PREV-ARCH5 THRU 7350-EXIT.                 EL690
03265                                                                   EL690
03266      IF  LA-COMPANY-CD-A5 NOT EQUAL PI-COMPANY-CD                 EL690
03267          GO TO 3400-END.                                          EL690
03268                                                                   EL690
03269      IF  PI-690-INIT-CARRIER EQUAL HIGH-VALUES OR LOW-VALUES      EL690
03270          GO TO 3400-CONTINUE.                                     EL690
03271                                                                   EL690
03272      IF  LA-CARRIER-A5 NOT EQUAL PI-690-INIT-CARRIER              EL690
03273          GO TO 3400-END-BROWSE.                                   EL690
03274                                                                   EL690
03275      IF  PI-690-INIT-GROUPING EQUAL HIGH-VALUES OR LOW-VALUES     EL690
03276          GO TO 3400-CONTINUE.                                     EL690
03277                                                                   EL690
03278      IF  LA-GROUPING-A5 NOT EQUAL PI-690-INIT-GROUPING            EL690
03279          GO TO 3400-END-BROWSE.                                   EL690
03280                                                                   EL690
03281      IF  PI-690-INIT-STATE EQUAL HIGH-VALUES OR LOW-VALUES        EL690
03282          GO TO 3400-CONTINUE.                                     EL690
03283                                                                   EL690
03284      IF  LA-STATE-A5 NOT EQUAL PI-690-INIT-STATE                  EL690
03285          GO TO 3400-END-BROWSE.                                   EL690
03286                                                                   EL690
03287      IF  PI-690-INIT-ACCOUNT EQUAL HIGH-VALUES OR LOW-VALUES      EL690
03288          GO TO 3400-CONTINUE.                                     EL690
03289                                                                   EL690
03290      IF  LA-ACCOUNT-A5 NOT EQUAL PI-690-INIT-ACCOUNT              EL690
03291          GO TO 3400-END-BROWSE.                                   EL690
03292                                                                   EL690
03293  3400-CONTINUE.                                                   EL690
03294                                                                   EL690
03295      IF  NOT PI-690-SELECT-ALL                                       CL**3
03296              AND                                                     CL**3
03297          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND             CL**3
03298          GO TO 3400-BUILD-ARCH5-LIST-BACKWARD.                       CL**3
03299                                                                      CL**3
03300      IF  PI-690-INIT-ENTRY NOT EQUAL HIGH-VALUES                  EL690
03301              AND                                                  EL690
03302          PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES                   EL690
03303          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6              EL690
03304              GO TO 3400-BUILD-ARCH5-LIST-BACKWARD.                EL690
03305                                                                   EL690
03306      IF  PI-690-INIT-PROCESSOR NOT EQUAL HIGH-VALUES              EL690
03307              AND                                                  EL690
03308          PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES               EL690
03309          IF  PI-690-INIT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD      EL690
03310              GO TO 3400-BUILD-ARCH5-LIST-BACKWARD.                EL690
03311                                                                   EL690
03312      IF  PI-690-INIT-FORM NOT EQUAL HIGH-VALUES                   EL690
03313              AND                                                  EL690
03314          PI-690-INIT-FORM NOT EQUAL LOW-VALUES                    EL690
03315          IF  PI-690-INIT-FORM NOT EQUAL LA-FORM-A3                EL690
03316              GO TO 3400-BUILD-ARCH5-LIST-BACKWARD.                EL690
03317                                                                   EL690
03318      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.                EL690
03319                                                                   EL690
03320      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.               EL690
03321                                                                   EL690
03322      IF  W-ARCH-NDX LESS THAN +13                                 EL690
03323          GO TO 3400-BUILD-ARCH5-LIST-BACKWARD.                    EL690
03324                                                                   EL690
03325  3400-END.                                                        EL690
03326                                                                   EL690
03327      MOVE -1                     TO CORRSELL.                     EL690
03328      MOVE ZEROS                  TO CORRSELO.                     EL690
03329      PERFORM 3400-END-BROWSE                                      EL690
03330      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
03331                                                                   EL690
03332  3400-END-BROWSE.                                                 EL690
03333                                                                   EL690
03334      MOVE W-ARCH-NDX             TO W-ARCH-NDX3.                  EL690
03335      SUBTRACT +1 FROM W-ARCH-NDX3.                                EL690
03336                                                                   EL690
03337      IF  W-ARCH-NDX3 GREATER THAN +1                              EL690
03338          MOVE +1 TO W-ARCH-NDX2                                   EL690
03339          PERFORM 6500-REVERSE-TABLE-DATA THRU 6500-EXIT           EL690
03340                  UNTIL                                            EL690
03341              W-ARCH-NDX2 NOT LESS W-ARCH-NDX3.                    EL690
03342                                                                   EL690
03343      EXEC CICS ENDBR                                              EL690
03344          DATASET (W-ARCH5-FILE-ID)                                EL690
03345      END-EXEC.                                                    EL690
03346                                                                   EL690
03347  3400-FRONT-OF-FILE.                                              EL690
03348                                                                   EL690
03349      MOVE ER-0131                TO EMI-ERROR.                    EL690
03350      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
03351      MOVE -1                     TO CORRSELL.                     EL690
03352      MOVE ZEROS                  TO CORRSELO.                     EL690
03353      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
03354                                                                   EL690
03355  3400-EXIT.                                                       EL690
03356      EXIT.                                                        EL690
03357                                  EJECT                            EL690
03358  3500-ARCH6-PROCESS.                                              EL690
03359                                                                   EL690
03360      IF  W-CHANGE-REQUESTED                                       EL690
03361          MOVE HIGH-VALUES        TO W-ARCH6-KEY                   EL690
03362          MOVE PI-COMPANY-CD      TO W-ARCH6-COMPANY-CD            EL690
03363          MOVE PI-690-INIT-ENTRY  TO W-ARCH6-ENTRY                 EL690
03364                                                                   EL690
03365                                                                   EL690
03366      ELSE                                                         EL690
03367          IF  W-FIRST-CHANGE-FOUND                                 EL690
03368              MOVE HIGH-VALUES    TO W-ARCH6-KEY                   EL690
03369              MOVE PI-COMPANY-CD  TO W-ARCH6-COMPANY-CD            EL690
03370              MOVE PI-690-LAST-ENTRY                               EL690
03371                                  TO W-ARCH6-ENTRY                 EL690
03372                                                                   EL690
03373          ELSE                                                     EL690
03374              MOVE PI-COMPANY-CD  TO W-ARCH6-COMPANY-CD            EL690
03375              MOVE PI-690-FIRST-ENTRY                              EL690
03376                                  TO W-ARCH6-ENTRY                 EL690
03377              MOVE PI-690-FIRST-ARCHIVE-NO                         EL690
03378                                  TO W-ARCH6-ARCHIVE-NO.           EL690
03379                                                                   EL690
03380      EXEC CICS HANDLE CONDITION                                   EL690
03381          NOTFND  (8080-ARCH-NOT-FOUND)                            EL690
03382          ENDFILE (8080-ARCH-NOT-FOUND)                            EL690
03383          NOTOPEN (8040-ARCH6-NOT-OPEN)                            EL690
03384          END-EXEC.                                                EL690
03385                                                                   EL690
03386      EXEC CICS STARTBR                                            EL690
03387          DATASET  (W-ARCH6-FILE-ID)                               EL690
03388          RIDFLD   (W-ARCH6-KEY)                                   EL690
03389          GTEQ                                                     EL690
03390          END-EXEC.                                                EL690
03391                                                                   EL690
03392      EXEC CICS HANDLE CONDITION                                   EL690
03393          NOTFND  (3500-END-BROWSE)                                EL690
03394          ENDFILE (3500-END-BROWSE)                                EL690
03395          END-EXEC.                                                EL690
03396                                                                   EL690
03397      PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT.            EL690
03398                                                                   EL690
03399      IF  W-CHANGE-REQUESTED                                       EL690
03400              OR                                                   EL690
03401          W-READ-PREV-TWICE                                        EL690
03402          MOVE SPACES            TO W-READ-PREV-TWICE-IND          EL690
03403          PERFORM 7450-READ-PREV-ARCH6 THRU 7450-EXIT.             EL690
03404                                                                   EL690
03405  3500-BUILD-ARCH6-LIST-BACKWARD.                                  EL690
03406                                                                   EL690
03407      PERFORM 7450-READ-PREV-ARCH6 THRU 7450-EXIT.                 EL690
03408                                                                   EL690
03409      IF  LA-COMPANY-CD-A5 NOT EQUAL PI-COMPANY-CD                 EL690
03410          GO TO 3500-END.                                          EL690
03411                                                                   EL690
03412      IF  PI-690-INIT-ENTRY EQUAL HIGH-VALUES OR LOW-VALUES        EL690
03413          GO TO 3500-CONTINUE.                                     EL690
03414                                                                   EL690
03415      IF  LA-ENTRY-A6 NOT EQUAL PI-690-INIT-ENTRY                  EL690
03416          GO TO 3500-END-BROWSE.                                   EL690
03417                                                                   EL690
03418  3500-CONTINUE.                                                   EL690
03419                                                                      CL**3
03420      IF  NOT PI-690-SELECT-ALL                                       CL**3
03421              AND                                                     CL**3
03422          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND             CL**3
03423          GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.                       CL**3
03424                                                                   EL690
03425      IF  PI-690-INIT-FORM NOT EQUAL HIGH-VALUES                   EL690
03426              AND                                                  EL690
03427          PI-690-INIT-FORM NOT EQUAL LOW-VALUES                    EL690
03428          IF  LA-FORM-A3 NOT EQUAL PI-690-INIT-FORM                EL690
03429              GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.                EL690
03430                                                                   EL690
03431      IF  PI-690-INIT-PROCESSOR NOT EQUAL HIGH-VALUES              EL690
03432              AND                                                  EL690
03433          PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES               EL690
03434          IF  LA-PROCESSOR-CD NOT EQUAL PI-690-INIT-PROCESSOR      EL690
03435              GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.                EL690
03436                                                                   EL690
03437      IF  PI-690-INIT-CARRIER NOT EQUAL HIGH-VALUES                EL690
03438              AND                                                  EL690
03439          PI-690-INIT-CARRIER NOT EQUAL LOW-VALUES                 EL690
03440          IF  LA-CARRIER-A2 NOT EQUAL PI-690-INIT-CARRIER          EL690
03441              GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.                EL690
03442                                                                   EL690
03443      IF  PI-690-INIT-GROUPING NOT EQUAL HIGH-VALUES               EL690
03444              AND                                                  EL690
03445          PI-690-INIT-GROUPING NOT EQUAL LOW-VALUES                EL690
03446          IF  LA-GROUPING-A2 NOT EQUAL PI-690-INIT-GROUPING        EL690
03447              GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.                EL690
03448                                                                   EL690
03449      IF  PI-690-INIT-STATE NOT EQUAL HIGH-VALUES                  EL690
03450              AND                                                  EL690
03451          PI-690-INIT-STATE NOT EQUAL LOW-VALUES                   EL690
03452          IF  LA-STATE-A2 NOT EQUAL PI-690-INIT-STATE              EL690
03453              GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.                EL690
03454                                                                   EL690
03455      IF  PI-690-INIT-ACCOUNT NOT EQUAL HIGH-VALUES                EL690
03456              AND                                                  EL690
03457          PI-690-INIT-ACCOUNT NOT EQUAL LOW-VALUES                 EL690
03458          IF  LA-ACCOUNT-A2 NOT EQUAL PI-690-INIT-ACCOUNT          EL690
03459              GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.                EL690
03460                                                                   EL690
03461      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.                EL690
03462                                                                   EL690
03463      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.               EL690
03464                                                                   EL690
03465      IF  W-ARCH-NDX LESS THAN +13                                 EL690
03466          GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.                    EL690
03467                                                                   EL690
03468  3500-END.                                                        EL690
03469                                                                   EL690
03470      MOVE -1                     TO CORRSELL.                     EL690
03471      MOVE ZEROS                  TO CORRSELO.                     EL690
03472      PERFORM 3500-END-BROWSE                                      EL690
03473      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
03474                                                                   EL690
03475  3500-END-BROWSE.                                                 EL690
03476                                                                   EL690
03477      MOVE W-ARCH-NDX             TO W-ARCH-NDX3.                  EL690
03478      SUBTRACT +1 FROM W-ARCH-NDX3.                                EL690
03479                                                                   EL690
03480      IF  W-ARCH-NDX3 GREATER THAN +1                              EL690
03481          MOVE +1 TO W-ARCH-NDX2                                   EL690
03482          PERFORM 6500-REVERSE-TABLE-DATA THRU 6500-EXIT           EL690
03483                  UNTIL                                            EL690
03484              W-ARCH-NDX2 NOT LESS W-ARCH-NDX3.                    EL690
03485                                                                   EL690
03486      EXEC CICS ENDBR                                              EL690
03487          DATASET (W-ARCH6-FILE-ID)                                EL690
03488      END-EXEC.                                                    EL690
03489                                                                   EL690
03490  3500-FRONT-OF-FILE.                                              EL690
03491                                                                   EL690
03492      MOVE ER-0131                TO EMI-ERROR.                    EL690
03493      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
03494      MOVE -1                     TO CORRSELL.                     EL690
03495      MOVE ZEROS                  TO CORRSELO.                     EL690
03496      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
03497                                                                   EL690
03498  3500-EXIT.                                                       EL690
03499      EXIT.                                                        EL690
03500                                  EJECT                            EL690
03501  3600-ARCH-PROCESS.                                               EL690
03502                                                                   EL690
03503      IF  W-CHANGE-REQUESTED                                       EL690
03504          MOVE HIGH-VALUES        TO W-ARCH-KEY                    EL690
03505          MOVE PI-COMPANY-CD      TO W-ARCH-COMPANY-CD             EL690
03506                                                                   EL690
03507      ELSE                                                         EL690
03508          IF  W-FIRST-CHANGE-FOUND                                 EL690
03509              MOVE HIGH-VALUES    TO W-ARCH-KEY                    EL690
03510              MOVE PI-COMPANY-CD  TO W-ARCH-COMPANY-CD             EL690
03511              MOVE PI-690-LAST-ARCHIVE-NO                          EL690
03512                                  TO W-ARCH-ARCHIVE-NO             EL690
03513                                                                   EL690
03514          ELSE                                                     EL690
03515              MOVE PI-COMPANY-CD  TO W-ARCH-COMPANY-CD             EL690
03516              MOVE PI-690-FIRST-ARCHIVE-NO                         EL690
03517                                  TO W-ARCH-ARCHIVE-NO             EL690
03518              MOVE PI-690-FIRST-ARCHIVE-NO                         EL690
03519                                  TO W-ARCH-ARCHIVE-NO.            EL690
03520                                                                   EL690
03521      EXEC CICS HANDLE CONDITION                                   EL690
03522          NOTFND  (8080-ARCH-NOT-FOUND)                            EL690
03523          ENDFILE (8080-ARCH-NOT-FOUND)                            EL690
03524          NOTOPEN (8050-ARCH-NOT-OPEN)                             EL690
03525          END-EXEC.                                                EL690
03526                                                                   EL690
03527      EXEC CICS STARTBR                                            EL690
03528          DATASET  (W-ARCH-FILE-ID)                                EL690
03529          RIDFLD   (W-ARCH-KEY)                                    EL690
03530          GTEQ                                                     EL690
03531          END-EXEC.                                                EL690
03532                                                                   EL690
03533      EXEC CICS HANDLE CONDITION                                   EL690
03534          NOTFND  (3600-END-BROWSE)                                EL690
03535          ENDFILE (3600-END-BROWSE)                                EL690
03536          END-EXEC.                                                EL690
03537                                                                   EL690
03538      PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT.             EL690
03539                                                                   EL690
03540      IF  W-CHANGE-REQUESTED                                       EL690
03541              OR                                                   EL690
03542          W-READ-PREV-TWICE                                        EL690
03543          MOVE SPACES            TO W-READ-PREV-TWICE-IND          EL690
03544          PERFORM 7550-READ-PREV-ARCH THRU 7550-EXIT.              EL690
03545                                                                   EL690
03546  3600-BUILD-ARCH-LIST-BACKWARD.                                   EL690
03547                                                                   EL690
03548      PERFORM 7550-READ-PREV-ARCH THRU 7550-EXIT.                  EL690
03549                                                                   EL690
03550      IF  LA-COMPANY-CD-A5 NOT EQUAL PI-COMPANY-CD                 EL690
03551          GO TO 3600-END.                                          EL690
03552                                                                   EL690
03553      IF  PI-690-INIT-ENTRY NOT EQUAL HIGH-VALUES                  EL690
03554              AND                                                  EL690
03555          PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES                   EL690
03556          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6              EL690
03557              GO TO 3600-BUILD-ARCH-LIST-BACKWARD.                 EL690
03558                                                                   EL690
03559      IF  PI-690-INIT-PROCESSOR NOT EQUAL HIGH-VALUES              EL690
03560              AND                                                  EL690
03561          PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES               EL690
03562          IF  PI-690-INIT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD      EL690
03563              GO TO 3600-BUILD-ARCH-LIST-BACKWARD.                 EL690
03564                                                                   EL690
03565      IF  PI-690-INIT-FORM NOT EQUAL HIGH-VALUES                   EL690
03566              AND                                                  EL690
03567          PI-690-INIT-FORM NOT EQUAL LOW-VALUES                    EL690
03568          IF  PI-690-INIT-FORM NOT EQUAL LA-FORM-A3                EL690
03569              GO TO 3600-BUILD-ARCH-LIST-BACKWARD.                 EL690
03570                                                                   EL690
03571      IF  PI-690-INIT-CARRIER NOT EQUAL HIGH-VALUES                EL690
03572              AND                                                  EL690
03573          PI-690-INIT-CARRIER NOT EQUAL LOW-VALUES                 EL690
03574          IF  LA-CARRIER-A2 NOT EQUAL PI-690-INIT-CARRIER          EL690
03575              GO TO 3600-BUILD-ARCH-LIST-BACKWARD.                 EL690
03576                                                                   EL690
03577      IF  PI-690-INIT-GROUPING NOT EQUAL HIGH-VALUES               EL690
03578              AND                                                  EL690
03579          PI-690-INIT-GROUPING NOT EQUAL LOW-VALUES                EL690
03580          IF  LA-GROUPING-A2 NOT EQUAL PI-690-INIT-GROUPING        EL690
03581              GO TO 3600-BUILD-ARCH-LIST-BACKWARD.                 EL690
03582                                                                   EL690
03583      IF  PI-690-INIT-STATE NOT EQUAL HIGH-VALUES                  EL690
03584              AND                                                  EL690
03585          PI-690-INIT-STATE NOT EQUAL LOW-VALUES                   EL690
03586          IF  LA-STATE-A2 NOT EQUAL PI-690-INIT-STATE              EL690
03587              GO TO 3600-BUILD-ARCH-LIST-BACKWARD.                 EL690
03588                                                                   EL690
03589      IF  PI-690-INIT-ACCOUNT NOT EQUAL HIGH-VALUES                EL690
03590              AND                                                  EL690
03591          PI-690-INIT-ACCOUNT NOT EQUAL LOW-VALUES                 EL690
03592          IF  LA-ACCOUNT-A2 NOT EQUAL PI-690-INIT-ACCOUNT          EL690
03593              GO TO 3600-BUILD-ARCH-LIST-BACKWARD.                 EL690
03594                                                                      CL**3
03595      IF  NOT PI-690-SELECT-ALL                                       CL**3
03596              AND                                                     CL**3
03597          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND             CL**3
03598          GO TO 3600-BUILD-ARCH-LIST-BACKWARD.                        CL**3
03599                                                                   EL690
03600      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.                EL690
03601                                                                   EL690
03602      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.               EL690
03603                                                                   EL690
03604      IF  W-ARCH-NDX LESS THAN +13                                 EL690
03605          GO TO 3600-BUILD-ARCH-LIST-BACKWARD.                     EL690
03606                                                                   EL690
03607  3600-END.                                                        EL690
03608                                                                   EL690
03609      MOVE -1                     TO CORRSELL.                     EL690
03610      MOVE ZEROS                  TO CORRSELO.                     EL690
03611      PERFORM 3600-END-BROWSE                                      EL690
03612      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
03613                                                                   EL690
03614  3600-END-BROWSE.                                                 EL690
03615                                                                   EL690
03616      MOVE W-ARCH-NDX             TO W-ARCH-NDX3.                  EL690
03617      SUBTRACT +1 FROM W-ARCH-NDX3.                                EL690
03618                                                                   EL690
03619      IF  W-ARCH-NDX3 GREATER THAN +1                              EL690
03620          MOVE +1 TO W-ARCH-NDX2                                   EL690
03621          PERFORM 6500-REVERSE-TABLE-DATA THRU 6500-EXIT           EL690
03622                  UNTIL                                            EL690
03623              W-ARCH-NDX2 NOT LESS W-ARCH-NDX3.                    EL690
03624                                                                   EL690
03625      EXEC CICS ENDBR                                              EL690
03626          DATASET (W-ARCH-FILE-ID)                                 EL690
03627      END-EXEC.                                                    EL690
03628                                                                   EL690
03629  3600-FRONT-OF-FILE.                                              EL690
03630                                                                   EL690
03631      MOVE ER-0131                TO EMI-ERROR.                    EL690
03632      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
03633      MOVE -1                     TO CORRSELL.                     EL690
03634      MOVE ZEROS                  TO CORRSELO.                     EL690
03635      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
03636                                                                   EL690
03637  3600-EXIT.                                                       EL690
03638      EXIT.                                                        EL690
03639                                  EJECT                            EL690
03640  6000-SELECT-FOR-OUTPUT.                                          EL690
03641                                                                   EL690
03642      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  EL690
03643              AND                                                  EL690
03644          PI-CARRIER-SECURITY NOT EQUAL LA-CARRIER-A2              EL690
03645                                                                   EL690
03646          IF  EMI-ERROR NOT EQUAL ER-9150                          EL690
03647              MOVE ER-9150        TO EMI-ERROR                     EL690
03648              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL690
03649              GO TO 6000-EXIT                                      EL690
03650                                                                   EL690
03651          ELSE                                                     EL690
03652              GO TO 6000-EXIT.                                     EL690
070711
070711     IF LA-TEMP
070711         GO TO 6000-EXIT
070711     END-IF.
03653                                                                   EL690
03654      PERFORM 6100-TABLE-LINE-CREATE  THRU 6100-EXIT.              EL690
03655                                                                   EL690
03656      MOVE W-ARCH-NDX             TO PI-690-LAST-ARCH-NDX.         EL690
03657      ADD +1                      TO W-ARCH-NDX.                   EL690
03658                                                                   EL690
03659                                                                   EL690
03660      IF  EIBAID EQUAL DFHPF1                                      EL690
03661          MOVE LA-CARRIER-A2      TO PI-690-LAST-CARRIER           EL690
03662          MOVE LA-GROUPING-A2     TO PI-690-LAST-GROUPING          EL690
03663          MOVE LA-STATE-A2        TO PI-690-LAST-STATE             EL690
03664          MOVE LA-ACCOUNT-A2      TO PI-690-LAST-ACCOUNT           EL690
03665          MOVE LA-CERT-PRIME-A2   TO PI-690-LAST-CERT-PRIME        EL690
03666          MOVE LA-CERT-SUFFIX-A2  TO PI-690-LAST-SUFFIX            EL690
03667          MOVE LA-EFFECT-DATE-A2  TO PI-690-LAST-EFFECT-DATE       EL690
03668          MOVE LA-FORM-A3         TO PI-690-LAST-FORM              EL690
03669          MOVE LA-PROCESSOR-CD    TO PI-690-LAST-PROCESSOR         EL690
03670          MOVE LA-ENTRY-A6        TO PI-690-LAST-ENTRY             EL690
03671          MOVE LA-ARCHIVE-NO      TO PI-690-LAST-ARCHIVE-NO        EL690
03672                                                                   EL690
03673          IF  PI-690-FIRST-DATA EQUAL LOW-VALUES                   EL690
03674                  OR                                               EL690
03675              PI-690-FIRST-DATA EQUAL HIGH-VALUES                  EL690
03676              MOVE PI-690-LAST-DATA                                EL690
03677                                  TO PI-690-FIRST-DATA             EL690
03678                                                                   EL690
03679          ELSE                                                     EL690
03680              NEXT SENTENCE                                        EL690
03681                                                                   EL690
03682      ELSE                                                         EL690
03683          MOVE LA-CARRIER-A2      TO PI-690-FIRST-CARRIER          EL690
03684          MOVE LA-GROUPING-A2     TO PI-690-FIRST-GROUPING         EL690
03685          MOVE LA-STATE-A2        TO PI-690-FIRST-STATE            EL690
03686          MOVE LA-ACCOUNT-A2      TO PI-690-FIRST-ACCOUNT          EL690
03687          MOVE LA-CERT-PRIME-A2   TO PI-690-FIRST-CERT-PRIME       EL690
03688          MOVE LA-CERT-SUFFIX-A2  TO PI-690-FIRST-SUFFIX           EL690
03689          MOVE LA-EFFECT-DATE-A2  TO PI-690-FIRST-EFFECT-DATE      EL690
03690          MOVE LA-FORM-A3         TO PI-690-FIRST-FORM             EL690
03691          MOVE LA-PROCESSOR-CD    TO PI-690-FIRST-PROCESSOR        EL690
03692          MOVE LA-ENTRY-A6        TO PI-690-FIRST-ENTRY            EL690
03693          MOVE LA-ARCHIVE-NO      TO PI-690-FIRST-ARCHIVE-NO       EL690
03694                                                                   EL690
03695          IF  PI-690-LAST-DATA EQUAL LOW-VALUES                    EL690
03696                  OR                                               EL690
03697              PI-690-LAST-DATA EQUAL HIGH-VALUES                   EL690
03698              MOVE PI-690-FIRST-DATA                               EL690
03699                                  TO PI-690-LAST-DATA.             EL690
03700                                                                   EL690
03701  6000-EXIT.                                                       EL690
03702      EXIT.                                                        EL690
03703                                  EJECT                            EL690
03704  6100-TABLE-LINE-CREATE.                                          EL690
03705                                                                   EL690
03706      MOVE LA-ARCHIVE-NO                                           EL690
03707          TO W-ARCNOO (W-ARCH-NDX)                                 EL690
03708             PI-690-ARCHIVE-NUM (W-ARCH-NDX).                      EL690
03709                                                                   EL690
070711*    MOVE LA-STATUS              TO W-STATSO (W-ARCH-NDX).        EL690
03711      MOVE LA-FORM-A3             TO W-FORMO (W-ARCH-NDX).         EL690
070711     MOVE LA-PROCESSOR-CD        TO W-USRIDO (W-ARCH-NDX).
03712                                                                   EL690
070711*    IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN                            EL690
070711*        MOVE 'F'                TO W-PRRSTO (W-ARCH-NDX)         EL690
070711*                                                                 EL690
070711*    ELSE                                                         EL690
070711*        IF  LA-PRINT-ONLY-WHEN-PROC-GIVEN                        EL690
070711*            MOVE 'P'            TO W-PRRSTO (W-ARCH-NDX)         EL690
070711*                                                                 EL690
070711*        ELSE                                                     EL690
070711*            IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN                    EL690
070711*                MOVE 'C'        TO W-PRRSTO (W-ARCH-NDX)         EL690
070711*                                                                 EL690
070711*            ELSE                                                 EL690
070711*                MOVE SPACES     TO W-PRRSTO (W-ARCH-NDX).        EL690
03726                                                                   EL690
03727      IF  LA-INITIAL-PRINT-DATE GREATER THAN LOW-VALUES            EL690
070711*        MOVE AL-PABON           TO W-PRINTA (W-ARCH-NDX)         EL690
03729          MOVE LA-INITIAL-PRINT-DATE                               EL690
03730                                  TO DC-BIN-DATE-1                 EL690
03731          MOVE ' '                TO DC-OPTION-CODE                EL690
03732          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT            EL690
03733                                                                   EL690
03734          IF  DC-ERROR-CODE EQUAL SPACES                           EL690
03735              MOVE DC-GREG-DATE-1-EDIT                             EL690
070711                                 TO W-PRINTO (W-ARCH-NDX)         EL690
03737                                                                   EL690
03738          ELSE                                                     EL690
070711             MOVE SPACES         TO W-PRINTO (W-ARCH-NDX)         EL690
070711         END-IF
070711     ELSE
070711         MOVE SPACES             TO W-PRINTO (W-ARCH-NDX)
070711     END-IF.
03740                                                                   EL690
070711*        IF  LA-CREATION-DATE GREATER THAN LOW-VALUES             EL690
070711*            MOVE LA-CREATION-DATE                                EL690
070711*                                TO DC-BIN-DATE-1                 EL690
070711*            MOVE ' '            TO DC-OPTION-CODE                EL690
070711*            PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT        EL690
070711*                                                                 EL690
070711*            IF  DC-ERROR-CODE EQUAL SPACES                       EL690
070711*                MOVE DC-GREG-DATE-1-EDIT                         EL690
070711*                                TO W-CREATO (W-ARCH-NDX)         EL690
070711*                                                                 EL690
070711*            ELSE                                                 EL690
070711*                MOVE SPACES     TO W-CREATO (W-ARCH-NDX)         EL690
070711*                                                                 EL690
070711*        ELSE                                                     EL690
070711*            MOVE SPACES         TO W-CREATO (W-ARCH-NDX).        EL690
03757                                                                   EL690
03758                                                                   EL690
070711*    IF  LA-FOLLOW-UP-DATE GREATER THAN LOW-VALUES                EL690
070711*        MOVE LA-FOLLOW-UP-DATE  TO DC-BIN-DATE-1                 EL690
070711*        MOVE ' '                TO DC-OPTION-CODE                EL690
070711*        PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT            EL690
070711*                                                                 EL690
070711*        IF  DC-ERROR-CODE EQUAL SPACES                           EL690
070711*            MOVE DC-GREG-DATE-1-EDIT                             EL690
070711*                                TO W-FOLUPO (W-ARCH-NDX)         EL690
070711*                                                                 EL690
070711*        ELSE                                                     EL690
070711*            MOVE SPACES         TO W-FOLUPO (W-ARCH-NDX)         EL690
070711*                                                                 EL690
070711*    ELSE                                                         EL690
070711*        MOVE SPACES             TO W-FOLUPO (W-ARCH-NDX).        EL690
070711*                                                                 EL690
03774                                                                   EL690
070711*    IF  LA-SENT-DATE   GREATER THAN LOW-VALUES                   EL690
070711*        MOVE 'C'                TO W-RSSTSO (W-ARCH-NDX)
070711*    ELSE
070711*        MOVE 'A'                TO W-RSSTSO (W-ARCH-NDX)
070711*    END-IF
03787                                                        
03789      IF  LA-RESEND-DATE   GREATER THAN LOW-VALUES      
03790          MOVE LA-RESEND-DATE                           
03791                              TO DC-BIN-DATE-1          
03792          MOVE ' '            TO DC-OPTION-CODE         
03793          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT 
03794                                                        
03795          IF  DC-ERROR-CODE EQUAL SPACES                
03796              MOVE DC-GREG-DATE-1-EDIT                  
070711                             TO W-RSDATO (W-ARCH-NDX)  
03798                                                        
03799          ELSE                                          
070711             MOVE SPACES     TO W-RSDATO (W-ARCH-NDX)  
03801                                                        
03802      ELSE                                              
070711         MOVE SPACES         TO W-RSDATO (W-ARCH-NDX)  
070711                                W-RSFRMO (W-ARCH-NDX)  
03806          GO TO 6100-CHECK-REPLY.                       
03807                                                                   EL690
03808                                                                   EL690
           IF LA-RESEND-LETR NOT = SPACES
070711        MOVE LA-RESEND-LETR      TO W-RSFRMO (W-ARCH-NDX)
           ELSE
070711        MOVE SPACES              TO W-RSFRMO (W-ARCH-NDX)
           END-IF

03809 *    IF  LA-SENT-DATE-2 GREATER THAN LOW-VALUES                   EL690
03810 *        MOVE AL-UABON           TO W-RSFRMA (W-ARCH-NDX)         EL690
03811 *        MOVE LA-SENT-DATE-2     TO DC-BIN-DATE-1                 EL690
03812 *        MOVE ' '                TO DC-OPTION-CODE                EL690
03813 *        PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT            EL690
03814 *                                                                 EL690
03815 *        IF  DC-ERROR-CODE EQUAL SPACES                           EL690
03816 *            MOVE DC-GREG-DATE-1-EDIT                             EL690
03817 *                                TO W-RSFRMO (W-ARCH-NDX)         EL690
03818 *                                                                 EL690
03819 *        ELSE                                                     EL690
03820 *            MOVE SPACES         TO W-RSFRMO (W-ARCH-NDX)         EL690
03821 *                                                                 EL690
03822 *    ELSE                                                         EL690
03823 *        IF  LA-RESEND-DATE-2 GREATER THAN LOW-VALUES             EL690
03824 *            MOVE LA-RESEND-DATE-2                                EL690
03825 *                                TO DC-BIN-DATE-1                 EL690
03826 *            MOVE ' '            TO DC-OPTION-CODE                EL690
03827 *            PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT        EL690
03828 *                                                                 EL690
03829 *            IF  DC-ERROR-CODE EQUAL SPACES                       EL690
03830 *                MOVE DC-GREG-DATE-1-EDIT                         EL690
03831 *                                TO W-RSFRMO (W-ARCH-NDX)         EL690
03832 *                                                                 EL690
03833 *            ELSE                                                 EL690
03834 *                MOVE SPACES     TO W-RSFRMO (W-ARCH-NDX)         EL690
03835 *                                                                 EL690
03836 *        ELSE                                                     EL690
03837 *            MOVE SPACES         TO W-RSFRMO (W-ARCH-NDX)         EL690
03838 *                                   W-RSSTSO (W-ARCH-NDX)         EL690
03839 *            GO TO 6100-CHECK-REPLY.                              EL690
03840 *                                                                 EL690
03841 *                                                                 EL690
03842 *    IF  LA-SENT-DATE-3 GREATER THAN LOW-VALUES                   EL690
03843 *        MOVE AL-UABON           TO W-RSSTSA (W-ARCH-NDX)         EL690
03844 *        MOVE LA-SENT-DATE-3     TO DC-BIN-DATE-1                 EL690
03845 *        MOVE ' '                TO DC-OPTION-CODE                EL690
03846 *        PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT            EL690
03847 *                                                                 EL690
03848 *        IF  DC-ERROR-CODE EQUAL SPACES                           EL690
03849 *            MOVE DC-GREG-DATE-1-EDIT                             EL690
03850 *                                TO W-RSSTSO (W-ARCH-NDX)         EL690
03851 *                                                                 EL690
03852 *        ELSE                                                     EL690
03853 *            MOVE SPACES         TO W-RSSTSO (W-ARCH-NDX)         EL690
03854 *                                                                 EL690
03855 *    ELSE                                                         EL690
03856 *        IF  LA-RESEND-DATE-3 GREATER THAN LOW-VALUES             EL690
03857 *            MOVE LA-RESEND-DATE-3                                EL690
03858 *                                TO DC-BIN-DATE-1                 EL690
03859 *            MOVE ' '            TO DC-OPTION-CODE                EL690
03860 *            PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT        EL690
03861 *                                                                 EL690
03862 *            IF  DC-ERROR-CODE EQUAL SPACES                       EL690
03863 *                MOVE DC-GREG-DATE-1-EDIT                         EL690
03864 *                                TO W-RSSTSO (W-ARCH-NDX)         EL690
03865 *                                                                 EL690
03866 *            ELSE                                                 EL690
03867 *                MOVE SPACES     TO W-RSSTSO (W-ARCH-NDX)         EL690
03868 *                                                                 EL690
03869 *        ELSE                                                     EL690
03870 *            MOVE SPACES         TO W-RSSTSO (W-ARCH-NDX).        EL690

           .
03872  6100-CHECK-REPLY.                                                EL690
03873                                                                   EL690
03874      IF  LA-PURGED-DATE GREATER THAN LOW-VALUES                   EL690
03875              AND                                                  EL690
03876          LA-PURGED-DATE NOT EQUAL SPACES                          EL690
070711*        MOVE LA-PURGED-DATE     TO DC-BIN-DATE-1                 EL690
070711*        MOVE ' '                TO DC-OPTION-CODE                EL690
070711*        PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT            EL690
03880                                                                   EL690
070711*        IF  DC-ERROR-CODE EQUAL SPACES                           EL690
070711*            MOVE DC-GREG-DATE-1-EDIT                             EL690
070711*                                TO W-REPLYO (W-ARCH-NDX)         EL690
070711             MOVE 'PURGED'       TO W-STATUSO (W-ARCH-NDX)
03884                                                                   EL690
070711*        ELSE                                                     EL690
070711*            MOVE SPACES         TO W-REPLYO (W-ARCH-NDX)         EL690
070711*                                   W-STOPO (W-ARCH-NDX)
070711*                                                                 EL690
070711*        END-IF
03888      ELSE                                                         EL690
03889          IF  LA-VOIDED-DATE GREATER THAN LOW-VALUES               EL690
03890                  AND                                              EL690
03891              LA-VOIDED-DATE NOT EQUAL SPACES                      EL690
070711*            MOVE LA-VOIDED-DATE TO DC-BIN-DATE-1                 EL690
070711*            MOVE ' '            TO DC-OPTION-CODE                EL690
070711*            PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT        EL690
03895                                                                   EL690
070711*            IF  DC-ERROR-CODE EQUAL SPACES                       EL690
070711*                MOVE AL-PANOF   TO W-REPLYA (W-ARCH-NDX)         EL690
070711*                MOVE DC-GREG-DATE-1-EDIT                         EL690
070711*                                TO W-REPLYO (W-ARCH-NDX)         EL690
070711                 MOVE 'STOPPED'  TO W-STATUSO (W-ARCH-NDX)
03900                                                                   EL690
070711*            ELSE                                                 EL690
070711*                MOVE AL-PANOF   TO W-REPLYA (W-ARCH-NDX)         EL690
070711*                MOVE SPACES     TO W-REPLYO (W-ARCH-NDX)         EL690
070711*                                   W-STOPO (W-ARCH-NDX)
070711*                                                                 EL690
070711*                                                                 EL690
070711*            END-IF
03906          ELSE                                                     EL690
03907              IF  LA-REPLY-DATE GREATER THAN LOW-VALUES            EL690
03908                      AND                                          EL690
03909                  LA-REPLY-DATE NOT EQUAL SPACES                   EL690
070711*                MOVE LA-REPLY-DATE                               EL690
070711*                                TO DC-BIN-DATE-1                 EL690
070711*                MOVE ' '        TO DC-OPTION-CODE                EL690
070711*                PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT    EL690
070711*                                                                 EL690
070711*                IF  DC-ERROR-CODE EQUAL SPACES                   EL690
070711*                    MOVE DC-GREG-DATE-1-EDIT                     EL690
070711*                                TO W-REPLYO (W-ARCH-NDX)         EL690
070711                     MOVE 'RECEIVED' TO W-STATUSO (W-ARCH-NDX)
03918                                                                   EL690
070711*                ELSE                                             EL690
070711*                    MOVE SPACES TO W-REPLYO (W-ARCH-NDX)         EL690
070711*                                   W-STOPO (W-ARCH-NDX)
070711*                                                                 EL690
070711*                END-IF
03922              ELSE                                                 EL690
070711                 IF LA-STATUS = 'A'
070711                     MOVE 'ACTIVE' TO W-STATUSO (W-ARCH-NDX)
070711                 ELSE 
070711                     IF LA-STATUS = 'C'
070711                        MOVE 'COMPLETE' TO W-STATUSO (W-ARCH-NDX)
070711                     ELSE
070711                        MOVE SPACES     TO W-STATUSO (W-ARCH-NDX)
070711                     END-IF
070711                 END-IF
070711             END-IF
070711         END-IF
070711     END-IF.
03924                                                                   EL690
03932                                                                   EL690
03933  6100-EXIT.                                                       EL690
03934      EXIT.                                                        EL690
03935                                  EJECT                            EL690
03936  6500-REVERSE-TABLE-DATA.                                         EL690
03937                                                                   EL690
03938      MOVE W-ARCHGRP-DATA (W-ARCH-NDX2)                            EL690
03939                                  TO W-HOLD-LINE.                  EL690
03940      MOVE W-ARCHGRP-DATA (W-ARCH-NDX3)                            EL690
03941                                  TO W-ARCHGRP-DATA (W-ARCH-NDX2). EL690
03942      MOVE W-HOLD-LINE            TO W-ARCHGRP-DATA (W-ARCH-NDX3). EL690
03943      MOVE PI-690-ARCHIVE-NUM (W-ARCH-NDX2)                        EL690
03944                                  TO W-HOLD-ARCHIVE.               EL690
03945      MOVE PI-690-ARCHIVE-NUM (W-ARCH-NDX3)                        EL690
03946          TO PI-690-ARCHIVE-NUM (W-ARCH-NDX2).                     EL690
03947      MOVE W-HOLD-ARCHIVE                                          EL690
03948          TO PI-690-ARCHIVE-NUM (W-ARCH-NDX3).                     EL690
03949                                                                   EL690
03950      ADD +1                      TO W-ARCH-NDX2.                  EL690
03951      SUBTRACT +1 FROM W-ARCH-NDX3.                                EL690
03952                                                                   EL690
03953  6500-EXIT.                                                       EL690
03954      EXIT.                                                        EL690
03955                                  EJECT                            EL690
03956  7000-READ-ARCH2-FILE-NEXT.                                       EL690
03957                                                                   EL690
03958      EXEC CICS READNEXT                                           EL690
03959          DATASET  (W-ARCH2-FILE-ID)                               EL690
03960          SET      (ADDRESS OF LETTER-ARCHIVE)                        CL**5
03961          RIDFLD   (W-ARCH2-KEY)                                   EL690
03962          END-EXEC.                                                EL690
03963                                                                   EL690
03964  7000-EXIT.                                                       EL690
03965      EXIT.                                                        EL690
03966                                                                   EL690
03967  7050-READ-PREV-ARCH2.                                            EL690
03968                                                                   EL690
03969      EXEC CICS READPREV                                           EL690
03970          DATASET  (W-ARCH2-FILE-ID)                               EL690
03971          SET      (ADDRESS OF LETTER-ARCHIVE)                        CL**5
03972          RIDFLD   (W-ARCH2-KEY)                                   EL690
03973          END-EXEC.                                                EL690
03974                                                                   EL690
03975  7050-EXIT.                                                       EL690
03976      EXIT.                                                        EL690
03977                                  EJECT                            EL690
03978  7100-READ-ARCH3-FILE-NEXT.                                       EL690
03979                                                                   EL690
03980      EXEC CICS READNEXT                                           EL690
03981          DATASET  (W-ARCH3-FILE-ID)                               EL690
03982          SET      (ADDRESS OF LETTER-ARCHIVE)                        CL**5
03983          RIDFLD   (W-ARCH3-KEY)                                   EL690
03984          END-EXEC.                                                EL690
03985                                                                   EL690
03986  7100-EXIT.                                                       EL690
03987      EXIT.                                                        EL690
03988                                                                   EL690
03989  7150-READ-PREV-ARCH3.                                            EL690
03990                                                                   EL690
03991      EXEC CICS READPREV                                           EL690
03992          DATASET  (W-ARCH3-FILE-ID)                               EL690
03993          SET      (ADDRESS OF LETTER-ARCHIVE)                        CL**5
03994          RIDFLD   (W-ARCH3-KEY)                                   EL690
03995      END-EXEC.                                                    EL690
03996                                                                   EL690
03997  7150-EXIT.                                                       EL690
03998      EXIT.                                                        EL690
03999                                  EJECT                            EL690
04000  7200-READ-ARCH4-FILE-NEXT.                                       EL690
04001                                                                   EL690
04002      EXEC CICS READNEXT                                           EL690
04003          DATASET  (W-ARCH4-FILE-ID)                               EL690
04004          SET      (ADDRESS OF LETTER-ARCHIVE)                        CL**5
04005          RIDFLD   (W-ARCH4-KEY)                                   EL690
04006          END-EXEC.                                                EL690
04007                                                                   EL690
04008  7200-EXIT.                                                       EL690
04009      EXIT.                                                        EL690
04010                                                                   EL690
04011  7250-READ-PREV-ARCH4.                                            EL690
04012                                                                   EL690
04013      EXEC CICS READPREV                                           EL690
04014          DATASET  (W-ARCH4-FILE-ID)                               EL690
04015          SET      (ADDRESS OF LETTER-ARCHIVE)                        CL**5
04016          RIDFLD   (W-ARCH4-KEY)                                   EL690
04017      END-EXEC.                                                    EL690
04018                                                                   EL690
04019  7250-EXIT.                                                       EL690
04020      EXIT.                                                        EL690
04021                                  EJECT                            EL690
04022  7300-READ-ARCH5-FILE-NEXT.                                       EL690
04023                                                                   EL690
04024      EXEC CICS READNEXT                                           EL690
04025          DATASET  (W-ARCH5-FILE-ID)                               EL690
04026          SET      (ADDRESS OF LETTER-ARCHIVE)                        CL**5
04027          RIDFLD   (W-ARCH5-KEY)                                   EL690
04028          END-EXEC.                                                EL690
04029                                                                   EL690
04030  7300-EXIT.                                                       EL690
04031      EXIT.                                                        EL690
04032                                                                   EL690
04033  7350-READ-PREV-ARCH5.                                            EL690
04034                                                                   EL690
04035      EXEC CICS READPREV                                           EL690
04036          DATASET  (W-ARCH5-FILE-ID)                               EL690
04037          SET      (ADDRESS OF LETTER-ARCHIVE)                        CL**5
04038          RIDFLD   (W-ARCH5-KEY)                                   EL690
04039      END-EXEC.                                                    EL690
04040                                                                   EL690
04041  7350-EXIT.                                                       EL690
04042      EXIT.                                                        EL690
04043                                  EJECT                            EL690
04044  7400-READ-ARCH6-FILE-NEXT.                                       EL690
04045                                                                   EL690
04046      EXEC CICS READNEXT                                           EL690
04047          DATASET  (W-ARCH6-FILE-ID)                               EL690
04048          SET      (ADDRESS OF LETTER-ARCHIVE)                        CL**5
04049          RIDFLD   (W-ARCH6-KEY)                                   EL690
04050          END-EXEC.                                                EL690
04051                                                                   EL690
04052  7400-EXIT.                                                       EL690
04053      EXIT.                                                        EL690
04054                                                                   EL690
04055  7450-READ-PREV-ARCH6.                                            EL690
04056                                                                   EL690
04057      EXEC CICS READPREV                                           EL690
04058          DATASET  (W-ARCH6-FILE-ID)                               EL690
04059          SET      (ADDRESS OF LETTER-ARCHIVE)                        CL**5
04060          RIDFLD   (W-ARCH6-KEY)                                   EL690
04061      END-EXEC.                                                    EL690
04062                                                                   EL690
04063  7450-EXIT.                                                       EL690
04064      EXIT.                                                        EL690
04065                                  EJECT                            EL690
04066  7500-READ-ARCH-FILE-NEXT.                                        EL690
04067                                                                   EL690
04068      EXEC CICS READNEXT                                           EL690
04069          DATASET  (W-ARCH-FILE-ID)                                EL690
04070          SET      (ADDRESS OF LETTER-ARCHIVE)                        CL**5
04071          RIDFLD   (W-ARCH-KEY)                                    EL690
04072          END-EXEC.                                                EL690
04073                                                                   EL690
04074  7500-EXIT.                                                       EL690
04075      EXIT.                                                        EL690
04076                                                                   EL690
04077  7550-READ-PREV-ARCH.                                             EL690
04078                                                                   EL690
04079      EXEC CICS READPREV                                           EL690
04080          DATASET  (W-ARCH-FILE-ID)                                EL690
04081          SET      (ADDRESS OF LETTER-ARCHIVE)                        CL**5
04082          RIDFLD   (W-ARCH-KEY)                                    EL690
04083      END-EXEC.                                                    EL690
04084                                                                   EL690
04085  7550-EXIT.                                                       EL690
04086      EXIT.                                                        EL690
04087                                  EJECT                            EL690
04088  7600-READ-ARCH-FILE.                                             EL690
04089                                                                   EL690
04090      EXEC CICS READ                                               EL690
04091          DATASET  (W-ARCH-FILE-ID)                                EL690
04092          SET      (ADDRESS OF LETTER-ARCHIVE)                        CL**5
04093          RIDFLD   (W-ARCH-KEY)                                    EL690
04094          UPDATE                                                   EL690
04095          END-EXEC.                                                EL690
04096                                                                   EL690
04097  7600-EXIT.                                                       EL690
04098      EXIT.                                                        EL690
04099                                  EJECT                            EL690
04100  7650-READ-ARCT-FILE.                                             EL690
04101                                                                   EL690
04102      EXEC CICS READ                                               EL690
04103          DATASET  (W-ARCT-FILE-ID)                                EL690
04104          SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)                   CL**5
04105          RIDFLD   (W-ARCT-KEY)                                    EL690
04106          UPDATE                                                   EL690
04107          END-EXEC.                                                EL690
04108                                                                   EL690
04109  7650-EXIT.                                                       EL690
04110      EXIT.                                                        EL690
04111                                                                   EL690
04112  7750-READ-ARCH-FILE-NUPDT.                                       EL690
04113                                                                   EL690
04114      EXEC CICS READ                                               EL690
04115          DATASET  (W-ARCH-FILE-ID)                                EL690
04116          SET      (ADDRESS OF LETTER-ARCHIVE)                        CL**5
04117          RIDFLD   (W-ARCH-KEY)                                    EL690
04118          END-EXEC.                                                EL690
04119                                                                   EL690
04120  7750-EXIT.                                                       EL690
04121      EXIT.                                                        EL690
04122                                  EJECT                            EL690
04123  7800-DELETE-CYCLE.                                               EL690
04124                                                                   EL690
04125      EXEC CICS HANDLE CONDITION                                   EL690
04126          NOTFND  (7800-EXIT)                                      EL690
04127          ENDFILE (7800-EXIT)                                      EL690
04128          END-EXEC.                                                EL690
04129                                                                   EL690
04130 *    PERFORM 7650-READ-ARCT-FILE THRU 7650-EXIT.                  EL690
04131                                                                   EL690
04132      EXEC CICS DELETE                                             EL690
04133          DATASET   (W-ARCT-FILE-ID)                               EL690
04134          RIDFLD    (W-ARCT-KEY)                                   EL690
04135          KEYLENGTH (5)                                            EL690
04136          GENERIC                                                  EL690
04137      END-EXEC.                                                    EL690
04138                                                                   EL690
04139      EXEC CICS DELETE                                             EL690
04140          DATASET   (W-ARCH-FILE-ID)                               EL690
04141          RIDFLD    (W-ARCH-KEY)                                   EL690
04142      END-EXEC.                                                    EL690
04143                                                                   EL690
04144      MOVE +99999999            TO PI-690-ARCHIVE-NUM (W-ARCH-NDX).EL690
04145      MOVE 1                      TO W-FIRST-CHANGE-IND.           EL690
04146                                                                   EL690
04147  7800-EXIT.                                                       EL690
04148      EXIT.                                                        EL690
04149                                  EJECT                            EL690
04150  8000-UNAUTHORIZED-ACCESS.                                        EL690
04151                                                                   EL690
04152      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL690
04153      GO TO 8300-SEND-TEXT.                                        EL690
04154                                                                   EL690
04155  8000-ARCH2-NOT-OPEN.                                             EL690
04156                                                                   EL690
04157      MOVE ER-7357                TO EMI-ERROR.                    EL690
04158      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
04159      MOVE -1                     TO TYPEBRL.                      EL690
04160                                                                   EL690
04161      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
04162                                                                   EL690
04163  8010-ARCH3-NOT-OPEN.                                             EL690
04164                                                                   EL690
04165      MOVE ER-7366                TO EMI-ERROR.                    EL690
04166      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
04167      MOVE -1                     TO TYPEBRL.                      EL690
04168                                                                   EL690
04169      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
04170                                                                   EL690
04171  8020-ARCH4-NOT-OPEN.                                             EL690
04172                                                                   EL690
04173      MOVE ER-7385                TO EMI-ERROR.                    EL690
04174      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
04175      MOVE -1                     TO TYPEBRL.                      EL690
04176                                                                   EL690
04177      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
04178                                                                   EL690
04179  8030-ARCH5-NOT-OPEN.                                             EL690
04180                                                                   EL690
04181      MOVE ER-7386                TO EMI-ERROR.                    EL690
04182      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
04183      MOVE -1                     TO TYPEBRL.                      EL690
04184                                                                   EL690
04185      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
04186                                                                   EL690
04187  8040-ARCH6-NOT-OPEN.                                             EL690
04188                                                                   EL690
04189      MOVE ER-7387                TO EMI-ERROR.                    EL690
04190      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
04191      MOVE -1                     TO TYPEBRL.                      EL690
04192                                                                   EL690
04193      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
04194                                                                   EL690
04195  8050-ARCH-NOT-OPEN.                                              EL690
04196                                                                   EL690
04197      MOVE ER-7388                TO EMI-ERROR.                    EL690
04198      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
04199      MOVE -1                     TO TYPEBRL.                      EL690
04200                                                                   EL690
04201      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
04202                                                                   EL690
04203  8070-NOTOPEN.                                                    EL690
04204                                                                   EL690
04205      MOVE ER-9282                TO EMI-ERROR.                    EL690
04206      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
04207      MOVE -1                     TO CERTRPL.                      EL690
04208                                                                   EL690
04209      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
04210                                                                   EL690
04211  8080-ARCH-NOT-FOUND.                                             EL690
04212                                                                   EL690
04213      MOVE ER-9010                TO EMI-ERROR.                    EL690
04214      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
04215      MOVE -1                     TO CERTRPL.                      EL690
04216      MOVE AL-UABON               TO CERTRPA.                      EL690
04217      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
04218                                  EJECT                            EL690
04219                                                                   EL690
04220  8090-NOT-FOUND-CNTL.                                             EL690
04221                                                                   EL690
04222      MOVE ER-0190                TO EMI-ERROR.                    EL690
04223      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL690
04224      MOVE -1                     TO MAINTL.                       EL690
04225      MOVE AL-UABON               TO MAINTA.                       EL690
04226      GO TO 8100-SEND-INITIAL-MAP.                                 EL690
04227                                  EJECT                            EL690
04228  8100-SEND-INITIAL-MAP.                                           EL690
04229 ******************************************************************EL690
04230 *                                                                *EL690
04231 *       THIS LOGIC SENDS THE INITIAL MAP.  IT WILL LOOK FOR      *EL690
04232 *       THE MAP DATA UNDER THE NAMES LISTED BELOW AND FOUND      *EL690
04233 *       IN THE WORK AREA SECTION OF WORKING STORAGE.             *EL690
04234 *                                                                *EL690
04235 *       W-MAP          PIC  X(08)                                *EL690
04236 *       W-MAPSET       PIC  X(08)                                *EL690
04237 *       W-MAP-AREA     PIC  X(**)                                *EL690
04238 * ** THIS AREA REDEFINES MAP AREA AND IS MAP DEPENDENT           *EL690
04239 *    FOR SIZE.                                                   *EL690
04240 *                                                                *EL690
04241 ******************************************************************EL690
04242                                                                   EL690
04243      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.                EL690
04244                                                                   EL690
04245      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL690
04246                                                                   EL690
04247      EXEC CICS SEND                                               EL690
04248          MAP    (W-MAP)                                           EL690
04249          MAPSET (W-MAPSET)                                        EL690
04250          FROM   (EL690AI)                                         EL690
04251          ERASE                                                    EL690
04252          FREEKB                                                   EL690
04253          CURSOR                                                   EL690
04254      END-EXEC.                                                    EL690
04255                                                                   EL690
04256      GO TO 9000-RETURN-TRANS.                                     EL690
04257                                                                   EL690
04258  8100-EXIT.                                                       EL690
04259      EXIT.                                                        EL690
04260                                  EJECT                            EL690
04261  8200-SEND-DATAONLY.                                              EL690
04262 ******************************************************************EL690
04263 *                                                                *EL690
04264 *       THIS LOGIC SENDS THE UPDATED VERSION OF THE MAP, USING   *EL690
04265 *       THE FIELDS LISTED BELOW WHICH SHOULD BE FOUND IN THE     *EL690
04266 *       WORK AREA OF WORKING STORAGE.                            *EL690
04267 *                                                                *EL690
04268 *       W-MAP           PIC  X(08)                               *EL690
04269 *       W-MAPSET        PIC  X(08)                               *EL690
04270 *       W-MAP-AREA      PIC  X(****)                             *EL690
04271 *                                                                *EL690
04272 * **** REDEFINES MAP AREA AND IS MAP DEPENDENT.                  *EL690
04273 *                                                                *EL690
04274 ******************************************************************EL690
04275                                                                   EL690
04276      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.                EL690
04277                                                                   EL690
04278      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL690
04279                                                                   EL690
04280      EXEC CICS SEND                                               EL690
04281          MAP    (W-MAP)                                           EL690
04282          MAPSET (W-MAPSET)                                        EL690
04283          FROM   (EL690AI)                                         EL690
04284          DATAONLY                                                 EL690
04285          FREEKB                                                   EL690
04286          CURSOR                                                   EL690
04287      END-EXEC.                                                    EL690
04288                                                                   EL690
04289      GO TO 9000-RETURN-TRANS.                                     EL690
04290                                                                   EL690
04291  8200-EXIT.                                                       EL690
04292      EXIT.                                                        EL690
04293                                  EJECT                            EL690
04294  8300-SEND-TEXT.                                                  EL690
04295 ***************************************************************** EL690
04296 *    THIS PARAGRAPH SENDS THE COMMON LOGOFF MESSAGE.            * EL690
04297 ***************************************************************** EL690
04298                                                                   EL690
04299      EXEC CICS SEND TEXT                                          EL690
04300          FROM    (LOGOFF-TEXT)                                    EL690
04301          LENGTH  (LOGOFF-LENGTH)                                  EL690
04302          ERASE                                                    EL690
04303          FREEKB                                                   EL690
04304      END-EXEC.                                                    EL690
04305                                                                   EL690
04306      GO TO 9000-RETURN-TRANS.                                     EL690
04307                                                                   EL690
04308  8300-EXIT.                                                       EL690
04309      EXIT.                                                        EL690
04310                                  EJECT                            EL690
04311  9000-RETURN-TRANS.                                               EL690
04312 ***************************************************************** EL690
04313 *     THIS PARAGRAPH CAUSES THE PROGRAM TO EXIT TO A            * EL690
04314 *     TRANSACTION.                                              * EL690
04315 *     THE FOLLOWING FIELDS ARE NEEDED IN WORKING-STORAGE        * EL690
04316 *     W-TRANSACTION          PIC  X(04)  VALUE 'XXXX'.          * EL690
04317 ***************************************************************** EL690
04318                                                                   EL690
04319      MOVE EMI-ERROR-NUMBER(1)    TO PI-LAST-ERROR-NO.             EL690
04320                                                                   EL690
04321      EXEC CICS RETURN                                             EL690
04322          TRANSID  (W-TRANSACTION)                                 EL690
04323          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL690
04324          LENGTH   (PI-COMM-LENGTH)                                EL690
04325      END-EXEC.                                                    EL690
04326                                                                   EL690
04327  9000-EXIT.                                                       EL690
04328      EXIT.                                                        EL690
04329                               EJECT                               EL690
04330  9200-DATE-EDIT.                                                  EL690
04331 ***************************************************************** EL690
04332 *     THIS ROUTINE VALIDATES THE DATE PASSED IN THE             * EL690
04333 *     'W-DEEDIT-FIELD' BY CALLING THE 'ELDATECV' PROGRAM.       * EL690
04334 *     AFTER EXITING THIS COPY BOOK, 'DC-ERROR-CODE' CAN BE      * EL690
04335 *     CHECKED FOR SPACES TO DETERMINE IF ERRORS WERE FOUND      * EL690
04336 *     ELSE THE BINARY DATE CAN BE FOUND IN 'DC-BIN-DATE-1'.     * EL690
04337 ***************************************************************** EL690
04338                                                                   EL690
04339      EXEC CICS BIF DEEDIT                                         EL690
04340          FIELD  (W-DEEDIT-FIELD)                                  EL690
04341          LENGTH (15)                                              EL690
04342      END-EXEC.                                                    EL690
04343                                                                   EL690
04344      IF  W-DEEDIT-FIELD-V0 NUMERIC                                EL690
04345          MOVE W-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-EDIT           EL690
04346                                     DC-GREG-DATE-1-MDY            EL690
04347          INSPECT DC-GREG-DATE-1-EDIT CONVERTING ' ' TO '/'           CL**5
04348 *        INSPECT DC-GREG-DATE-1-EDIT REPLACING ALL ' ' BY '/'     EL690
04349          MOVE '4'                TO DC-OPTION-CODE                EL690
04350                                                                   EL690
04351          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT            EL690
04352                                                                   EL690
04353      ELSE                                                         EL690
04354          MOVE HIGH-VALUES        TO  DC-ERROR-CODE.               EL690
04355                                                                   EL690
04356  9200-EXIT.                                                       EL690
04357      EXIT.                                                        EL690
04358                                  EJECT                            EL690
04359  9400-XCTL.                                                       EL690
04360 ***************************************************************** EL690
04361 *    THIS PARAGRAPH TRANSFERS CONTROL TO INDICATED PROGRAM.     * EL690
04362 *    PROGRAM MUST RESIDE IN W-CALL-PGM.                         * EL690
04363 ***************************************************************** EL690
04364                                                                   EL690
04365      EXEC CICS XCTL                                               EL690
04366          PROGRAM  (W-CALL-PGM)                                    EL690
04367          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL690
04368          LENGTH   (PI-COMM-LENGTH)                                EL690
04369      END-EXEC.                                                    EL690
04370                                                                   EL690
04371  9400-EXIT.                                                       EL690
04372      EXIT.                                                        EL690
04373                                  EJECT                            EL690
04374  9500-LINK-DATE-CONVERT.                                          EL690
04375 ***************************************************************** EL690
04376 *    THIS PARAGRAPH 'CALLS' THE UTILITY DATE PROCESSOR.         * EL690
04377 ***************************************************************** EL690
04378                                                                   EL690
04379      EXEC CICS LINK                                               EL690
04380          PROGRAM    ('ELDATCV')                                   EL690
04381          COMMAREA   (DATE-CONVERSION-DATA)                        EL690
04382          LENGTH     (DC-COMM-LENGTH)                              EL690
04383          END-EXEC.                                                EL690
04384                                                                   EL690
04385  9500-EXIT.                                                       EL690
04386      EXIT.                                                        EL690
04387                                  EJECT                            EL690
04388  9600-FORMAT-DATE-TIME.                                           EL690
04389 ***************************************************************** EL690
04390 *     THIS LOGIC UPDATES THE DATE/TIME INFO ON GIVEN MAP        * EL690
04391 ***************************************************************** EL690
04392                                                                   EL690
04394      MOVE W-SAVE-DATE            TO RUNDTEO.                      EL690
04395                                                                   EL690
04396      EXEC CICS ASKTIME                                            EL690
04397      END-EXEC.                                                    EL690
04398                                                                   EL690
04399      MOVE EIBTIME                TO W-TIME-IN.                    EL690
04400      MOVE W-TIME-OUT             TO RUNTIMEO.                     EL690
04393      MOVE PI-COMPANY-ID          TO COMPANYO.                     EL690
101101     MOVE PI-PROCESSOR-ID        TO USERIDO.
04401      MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO.         EL690
04402                                                                   EL690
04403  9600-EXIT.                                                       EL690
04404      EXIT.                                                        EL690
04405                                  EJECT                            EL690
04406  9700-PGMID-ERROR.                                                EL690
04407 ***************************************************************** EL690
04408 *     THIS PARAGRAPH TRANSFERS CONTROL TO EL005 LOGOFF.         * EL690
04409 *     THE FOLLOWING FIELDS ARE NEEDED IN WORKING-STORAGE        * EL690
04410 *     W-CALL-PGM             PIC  X(08)  VALUE 'EL000   '.      * EL690
04411 *     W-THIS-PGM             PIC  X(08).                        * EL690
04412 *     W-XCTL-005             PIC  X(08)  VALUE 'EL005   '.      * EL690
04413 ***************************************************************** EL690
04414                                                                   EL690
04415      EXEC CICS  HANDLE CONDITION                                  EL690
04416          PGMIDERR  (8300-SEND-TEXT)                               EL690
04417          END-EXEC.                                                EL690
04418                                                                   EL690
04419      MOVE W-THIS-PGM             TO PI-CALLING-PROGRAM.           EL690
04420      MOVE ' '                    TO PI-ENTRY-CD-1.                EL690
04421      MOVE W-XCTL-005             TO W-CALL-PGM                    EL690
04422                                     LOGOFF-PGM.                   EL690
04423      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL690
04424                                                                   EL690
04425      PERFORM 9400-XCTL THRU 9400-EXIT.                            EL690
04426                                                                   EL690
04427  9700-EXIT.                                                       EL690
04428      EXIT.                                                        EL690
04429                                  EJECT                            EL690
04430  9800-ABEND.                                                      EL690
04431 ***************************************************************** EL690
04432 *     THIS PARAGRAPH LINKS TO A COMMON ABEND ROUTINE.           * EL690
04433 *     THE FOLLOWING FIELDS ARE NEEDED IN WORKING-STORAGE        * EL690
04434 *     W-LINK-004             PIC  X(08)  VALUE 'EL004   '.      * EL690
04435 ***************************************************************** EL690
04436                                                                   EL690
04437      MOVE W-LINK-004             TO W-CALL-PGM.                   EL690
04438      MOVE DFHEIBLK               TO EMI-LINE1                     EL690
04439                                                                   EL690
04440      EXEC CICS  LINK                                              EL690
04441          PROGRAM   (W-CALL-PGM)                                   EL690
04442          COMMAREA  (EMI-LINE1)                                    EL690
04443          LENGTH    (72)                                           EL690
04444          END-EXEC.                                                EL690
04445                                                                   EL690
04446      GO TO 8200-SEND-DATAONLY.                                    EL690
04447                                                                   EL690
04448  9800-EXIT.                                                       EL690
04449      EXIT.                                                        EL690
04450                                  EJECT                            EL690
04451  9900-ERROR-FORMAT.                                               EL690
04452 ***************************************************************** EL690
04453 *     THIS PARAGRAPH IS THE STANDARDIZED ERROR FORMAT ROUTINE.  * EL690
04454 *     THE FIELDS W-LINK-001 AND W-CALL-PGM MUST BE DEFINED IN   * EL690
04455 *     WORKING STORAGE.                                          * EL690
04456 ***************************************************************** EL690
04457                                                                   EL690
04458      IF  NOT EMI-ERRORS-COMPLETE                                  EL690
04459              AND                                                  EL690
04460          EMI-ERROR NOT EQUAL W-LAST-ERROR                         EL690
04461          MOVE W-LINK-001         TO W-CALL-PGM                    EL690
04462                                                                   EL690
04463          EXEC CICS LINK                                           EL690
04464              PROGRAM    (W-CALL-PGM)                              EL690
04465              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL690
04466              LENGTH     (EMI-COMM-LENGTH)                         EL690
04467          END-EXEC                                                 EL690
04468                                                                   EL690
04469          MOVE EMI-ERROR TO W-LAST-ERROR.                          EL690
04470                                                                   EL690
04471  9900-EXIT.                                                       EL690
04472      EXIT.                                                        EL690
04473                                  EJECT                            EL690
04474  9910-INITIALIZE-SECURITY.                                        EL690
04475 ******************************************************************EL690
04476 *                                                                *EL690
04477 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *EL690
04478 *       USER SECURITY RECORD SET UP BY EL125.  BASED ON THE      *EL690
04479 *       APPLICATION NUMBER FOUND IN WORKING STORAGE UNDER        *EL690
04480 *       W-APPL-SECRTY-NDX (PIC  S9(04) COMP), THIS PROGRAM       *EL690
04481 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *EL690
04482 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *EL690
04483 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *EL690
04484 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *EL690
04485 *                                                                *EL690
04486 *       NOTE:  THE CARRIER/GRP/STATE/PRODUCER SECURITY DATA      *EL690
04487 *       IS ALSO PROVIDED BY THIS LOGIC.                          *EL690
04488 *                                                                *EL690
04489 ******************************************************************EL690
04490                                                                   EL690
04491      IF  PI-PROCESSOR-ID EQUAL 'LGXX'                             EL690
04492          MOVE 'Y'                TO PI-DISPLAY-CAP                EL690
04493                                         PI-MODIFY-CAP             EL690
04494                                                                   EL690
04495      ELSE                                                         EL690
04496 *        MOVE '125E'             TO W-SC-QUID-SYSTEM              EL690
04497 *        MOVE EIBTRMID           TO W-SC-QUID-TERMINAL            EL690
04498                                                                   EL690
04499          EXEC CICS READQ TS                                       EL690
04500              QUEUE  (PI-SECURITY-TEMP-STORE-ID)                   EL690
04501              INTO   (SECURITY-CONTROL)                            EL690
04502              LENGTH (SC-COMM-LENGTH)                              EL690
04503              ITEM   (1)                                           EL690
04504          END-EXEC                                                 EL690
04505                                                                   EL690
04506          MOVE SC-CREDIT-DISPLAY (W-APPL-SCRTY-NDX)                EL690
04507                                  TO PI-DISPLAY-CAP                EL690
04508          MOVE SC-CREDIT-UPDATE (W-APPL-SCRTY-NDX)                 EL690
04509                                  TO PI-MODIFY-CAP                 EL690
04510                                                                   EL690
04511          IF  NOT DISPLAY-CAP                                      EL690
04512              MOVE 'READ'         TO SM-READ                       EL690
04513              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       EL690
04514              MOVE ER-9097        TO EMI-ERROR                     EL690
04515              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL690
04516              GO TO 8100-SEND-INITIAL-MAP.                         EL690
04517                                                                   EL690
04518  9910-EXIT.                                                       EL690
04519      EXIT.                                                        EL690
04520                                  EJECT                            EL690
04521  9995-SECURITY-VIOLATION.                                         EL690
04522                                                                   EL690
04523      MOVE EIBDATE          TO SM-JUL-DATE.                        EL690
04524      MOVE EIBTRMID         TO SM-TERMID.                          EL690
04525      MOVE W-THIS-PGM       TO SM-PGM.                             EL690
04526      MOVE EIBTIME          TO W-TIME-IN.                          EL690
04527      MOVE W-TIME-OUT       TO SM-TIME.                            EL690
04528      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.                    EL690
04529                                                                   EL690
04530      EXEC CICS LINK                                               EL690
04531           PROGRAM  ('EL003')                                      EL690
04532           COMMAREA (SECURITY-MESSAGE)                             EL690
04533           LENGTH   (80)                                           EL690
04534      END-EXEC.                                                    EL690
04535                                                                   EL690
04536  9995-EXIT.                                                       EL690
04537      EXIT.                                                        EL690
04538                                  EJECT                            EL690
04539  9999-GOBACK.                                                     EL690
04540 ******************************************************************EL690
04541 *                                                                *EL690
04542 *       THIS PARAGRAPH FULFILL THE COMPILE REQUIRMENT FOR A      *EL690
04543 *       COBOL RETURN COMMAND.  IN A SEPARATE PARAGRAPH IT        *EL690
04544 *       PREVENTS AN COMPILE ERROR.                               *EL690
04545 *                                                                *EL690
04546 ******************************************************************EL690
04547                                                                   EL690
04548      GOBACK.                                                      EL690
04549                                                                   EL690
04550  9999-EXIT.                                                       EL690
04551      EXIT.                                                        EL690

