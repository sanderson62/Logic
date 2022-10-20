00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL690 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 10:00:53.
00007 *                            VMOD=2.005
00008 *
00008 *
00009 *AUTHOR.        LOGIC,INC.
00010 *               DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023 *
00024 *REMARKS.
00025 *       TRANSACTION - EXM3 - CREDIT SYSTEM CORRESPONDENCE REVIEW
00023 *
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
101512* 101512    2011022800001  AJRA  FIX LIST FOR SAME CERT DIFF EFF D
121112* 121112    2012101700002  AJRA  ADD PF6 TO CERT NOTES
102020* 102020 IR2020101300001   PEMA  Correct billing notes
101101******************************************************************
00026
00027  ENVIRONMENT DIVISION.
00028  DATA DIVISION.
00029                                  EJECT
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER   PIC  X(32) VALUE '*******************************'.
00032  77  FILLER   PIC  X(32) VALUE '*    EL690 WORKING STORAGE    *'.
00033  77  FILLER   PIC  X(32) VALUE '********* VMOD=2.005 **********'.
00034
00035  01  W-PROGRAM-CONSTANTS.
00036      12  FILLER                  PIC  X(18)
00037                                  VALUE 'PROGRAM CONSTANTS:'.
00038      12  W-APPL-SCRTY-NDX        PIC S9(04)  VALUE +3    COMP.
00039
00040      12  W-ARCH-FILE-ID          PIC  X(08)  VALUE 'ERARCH'.
00041      12  W-ARCH2-FILE-ID         PIC  X(08)  VALUE 'ERARCH2'.
00042      12  W-ARCH3-FILE-ID         PIC  X(08)  VALUE 'ERARCH3'.
00043      12  W-ARCH4-FILE-ID         PIC  X(08)  VALUE 'ERARCH4'.
00044      12  W-ARCH5-FILE-ID         PIC  X(08)  VALUE 'ERARCH5'.
00045      12  W-ARCH6-FILE-ID         PIC  X(08)  VALUE 'ERARCH6'.
00046      12  W-ARCT-FILE-ID          PIC  X(08)  VALUE 'ERARCT'.
00047      12  W-CNTL-FILE-ID          PIC  X(08)  VALUE 'ELCNTL'.
00048      12  W-LINK-001              PIC  X(08)  VALUE 'EL001'.
00049      12  W-LINK-004              PIC  X(08)  VALUE 'EL004'.
00050      12  W-MAP                   PIC  X(08)  VALUE 'EL690A'.
00051      12  W-MAP-REDEFINE  REDEFINES   W-MAP.
00052          16  FILLER              PIC  X(02).
00053          16  W-MAP-NUM           PIC  X(06).
00054      12  W-MAPSET                PIC  X(08)  VALUE 'EL690S'.
00055      12  W-THIS-PGM              PIC  X(08)  VALUE 'EL690'.
00056      12  W-TRANSACTION           PIC  X(04)  VALUE 'EXM3'.
00057      12  W-XCTL-005              PIC  X(08)  VALUE 'EL005'.
00058      12  W-XCTL-010              PIC  X(08)  VALUE 'EL010'.
00059      12  W-XCTL-153              PIC  X(08)  VALUE 'EL690'.
00060      12  W-XCTL-626              PIC  X(08)  VALUE 'EL626'.
00061      12  W-XCTL-689              PIC  X(08)  VALUE 'EL689'.
070711     12  W-XCTL-691              PIC  X(08)  VALUE 'EL691'.
121112     12  W-XCTL-1279             PIC  X(08)  VALUE 'EL1279'.
00062
00063  01  W-WORK-AREAS.
00064      12  FILLER                  PIC  X(11)
00065                                       VALUE 'WORK AREAS:'.
00066
00067      12  W-ARCH-NDX              PIC S9(04)  COMP.
00068      12  W-ARCH-NDX2             PIC S9(04)  COMP.
00069      12  W-ARCH-NDX3             PIC S9(04)  COMP.
00070      12  W-ARCH-NUMBER           PIC S9(08)  COMP.
00071      12  W-HOLD-ARCHIVE          PIC S9(08)  COMP.
00072      12  W-KEY-NDX               PIC S9(04)  COMP.
00073      12  W-LAST-ERROR            PIC  9(04)  VALUE 9999.
00074
00075      12  W-CALL-PGM              PIC  X(08)  VALUE SPACES.
00076
00077      12  W-DEEDIT-FIELD          PIC  X(15).
00078      12  W-DEEDIT-FIELD-V0 REDEFINES W-DEEDIT-FIELD
00079                                  PIC S9(15).
00080
00081      12  W-HOLD-LINE             PIC X(100).
00082      12  W-RETURNED-FROM         PIC  X(08)  VALUE SPACES.
00083      12  W-RESEND-DATE-1         PIC  X(02)  VALUE SPACES.
00084      12  W-RESEND-DATE-2         PIC  X(02)  VALUE SPACES.
00085      12  W-RESEND-DATE-3         PIC  X(02)  VALUE SPACES.
00086      12  W-REPLY-DATE            PIC  X(02)  VALUE SPACES.
00087      12  W-SAVE-BIN-DATE         PIC  X(02)  VALUE SPACES.
00088      12  W-SAVE-DATE             PIC  X(08)  VALUE SPACES.
00089      12  W-STATUS                PIC  X(01)  VALUE SPACES.
00090
00091      12  W-TIME-IN               PIC S9(07).
00092      12  FILLER REDEFINES W-TIME-IN.
00093          16  FILLER              PIC  X(01).
00094          16  W-TIME-OUT          PIC  9(02)V9(02).
00095          16  FILLER              PIC  X(02).
00096
00097  01  W-KEY-FIELDS.
00098      12  FILLER                  PIC  X(11)
00099                                       VALUE 'KEY FIELDS:'.
00100      12  W-CNTL-KEY.
00101          16  W-CNTL-COMPANY-ID   PIC  X(03).
00102          16  W-CNTL-RECORD-TYPE  PIC  X(01).
00103          16  FILLER              PIC  X(04).
00104          16  W-CNTL-SEQ          PIC S9(04) COMP.
00105      12  W-QUID-KEY.
00106          16  W-QUID-TERMINAL     PIC  X(04).
00107          16  W-QUID-MAP-NUM      PIC  X(04).
00108 *
00109 *    12  W-SC-QUID-KEY.
00110 *        16  W-SC-QUID-TERMINAL  PIC  X(04).
00111 *        16  W-SC-QUID-SYSTEM    PIC  X(04).
00112
00113      12  W-ARCH-KEY.
00114          16  W-ARCH-COMPANY-CD   PIC  X(01).
00115          16  W-ARCH-ARCHIVE-NO   PIC S9(08) COMP.
00116
00117      12  W-ARCH2-KEY.
00118          16  W-ARCH2-COMPANY-CD  PIC  X(01).
00119          16  W-ARCH2-CSCSAE-KEYLET.
00120              20  W-ARCH2-CERT-PRIME
00121                                  PIC  X(10).
00122              20  W-ARCH2-SUFFIX  PIC  X(01).
00123              20  W-ARCH2-CARRIER PIC  X(01).
00124              20  W-ARCH2-GROUPING
00125                                  PIC  X(06).
00126              20  W-ARCH2-STATE   PIC  X(02).
00127              20  W-ARCH2-ACCOUNT PIC  X(10).
00128              20  W-ARCH2-EFF-DTE PIC  X(02).
00129          16  W-ARCH2-ARCHIVE-NO  PIC S9(08) COMP.
00130
00131      12  W-ARCH3-KEY.
00132          16  W-ARCH3-COMPANY-CD  PIC  X(01).
00133          16  W-ARCH3-FCGSA-KEYLET.
00134              20  W-ARCH3-FORM    PIC  X(04).
00135              20  W-ARCH3-CARRIER PIC  X(01).
00136              20  W-ARCH3-GROUPING
00137                                  PIC  X(06).
00138              20  W-ARCH3-STATE   PIC  X(02).
00139              20  W-ARCH3-ACCOUNT PIC  X(10).
00140          16  W-ARCH3-ARCHIVE-NO  PIC S9(08) COMP.
00141
00142      12  W-ARCH4-KEY.
00143          16  W-ARCH4-COMPANY-CD  PIC  X(01).
00144          16  W-ARCH4-PCGSA-KEYLET.
00145              20  W-ARCH4-PROCESSOR
00146                                  PIC  X(04).
00147              20  W-ARCH4-CARRIER PIC  X(01).
00148              20  W-ARCH4-GROUPING
00149                                  PIC  X(06).
00150              20  W-ARCH4-STATE   PIC  X(02).
00151              20  W-ARCH4-ACCOUNT PIC  X(10).
00152          16  W-ARCH4-ARCHIVE-NO  PIC S9(08) COMP.
00153
00154      12  W-ARCH5-KEY.
00155          16  W-ARCH5-COMPANY-CD  PIC  X(01).
00156          16  W-ARCH5-CGSA-KEYLET.
00157              20  W-ARCH5-CARRIER PIC  X(01).
00158              20  W-ARCH5-GROUPING
00159                                  PIC  X(06).
00160              20  W-ARCH5-STATE   PIC  X(02).
00161              20  W-ARCH5-ACCOUNT PIC  X(10).
00162          16  W-ARCH5-ARCHIVE-NO  PIC S9(08) COMP.
00163
00164      12  W-ARCH6-KEY.
00165          16  W-ARCH6-COMPANY-CD  PIC  X(01).
00166          16  W-ARCH6-ENTRY.
00167              20  W-ARCH6-FILLER  PIC  X(02).
00168              20  W-ARCH6-QUE-CONTROL
00169                                  PIC S9(08) COMP.
00170          16  W-ARCH6-ARCHIVE-NO  PIC S9(08) COMP.
00171
00172      12  W-ARCT-KEY.
00173          16  W-ARCT-COMPANY-CD   PIC  X(01).
00174          16  W-ARCT-ARCHIVE-NO   PIC S9(08) COMP.
00175          16  W-ARCT-RECORD-TYPE  PIC  X(01).
00176              88  W-ARCT-ADDRESS-DATA   VALUE '1'.
00177              88  W-ARCT-TEXT-DATA      VALUE '2'.
00178          16  W-ARCT-LINE-SEQ-NO  PIC S9(04) COMP.
00179
00180      12  FILLER                      PIC  X(28).
00181      12  LT-NUM-LINES-ON-RECORD      PIC S9(04)    COMP.
00182
00183      12  LT-TEXT-RECORD.
00184          16  LT-LETTER-TEXT OCCURS 20 TIMES
00185                             INDEXED BY LT-NDX.
00186              20  LT-TEXT-LINE        PIC  X(70).
00187              20  LT-SKIP-CONTROL     PIC  X(02).
00188                  88  LT-NO-LINES-SKIPPED             VALUE SPACES.
00189                  88  LT-SKIP-TO-NEXT-PAGE            VALUE '99'.
00190              20  FILLER              PIC  X(08).
00191
00192
00193  01  W-PROGRAM-SWITCHES.
00194      12  FILLER                  PIC  X(09)
00195                                       VALUE 'SWITCHES:'.
00196      12  W-CHANGE-REQUESTED-IND  PIC  X(01)  VALUE SPACES.
00197          88  W-CHANGE-REQUESTED       VALUE 'Y'.
00198          88  W-NO-CHANGES-MADE        VALUE SPACES.
00199      12  W-FIRST-CHANGE-IND      PIC  9(02)  VALUE ZEROS.
00200          88  W-FIRST-CHANGE-FOUND     VALUES 01 THRU 12.
00201          88  W-NO-FIRST-CHANGE        VALUE ZEROS.
00202      12  W-READ-PREV-TWICE-IND   PIC  X(01)  VALUE SPACES.
00203          88  W-READ-PREV-TWICE        VALUE 'Y'.
00204      12  W-VALID-STATUS-SW       PIC  X(01).
00205          88  W-ALLOWED-ENTRIES        VALUE 'A' 'H' '+' '#' '@'
00206                                             'X' 'V'.
00207          88  W-VALID-STATUS           VALUE 'A' 'H'
00208                                             'P' 'X' 'V' 'C'.
00209          88  W-ACTIVE                 VALUE 'A'.
00210          88  W-COMPLETED              VALUE 'C'.
00211          88  W-VOIDED                 VALUE 'V'.
00212          88  W-TO-BE-PURGED-STATUS    VALUE 'X'.
00213          88  W-PURGED-STATUS          VALUE 'P'.
00214          88  W-SPECIAL-STATUS         VALUE '+'.
00215          88  W-CLEAR-INITIAL          VALUE '#'.
00216          88  W-CHECK-STATUS           VALUE '@'.
00217                                  EJECT
00218  01  FILLER                      PIC  X(14)
00219                                       VALUE 'PGRM MAP AREA:'.
00220 *    COPY EL690S.
       01  EL690AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDTEL PIC S9(0004) COMP.
           05  RUNDTEF PIC  X(0001).
           05  FILLER REDEFINES RUNDTEF.
               10  RUNDTEA PIC  X(0001).
           05  RUNDTEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  COMPANYL PIC S9(0004) COMP.
           05  COMPANYF PIC  X(0001).
           05  FILLER REDEFINES COMPANYF.
               10  COMPANYA PIC  X(0001).
           05  COMPANYI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  TYPEBRL PIC S9(0004) COMP.
           05  TYPEBRF PIC  X(0001).
           05  FILLER REDEFINES TYPEBRF.
               10  TYPEBRA PIC  X(0001).
           05  TYPEBRI PIC  X(0001).
      *    -------------------------------
           05  CERTRPL PIC S9(0004) COMP.
           05  CERTRPF PIC  X(0001).
           05  FILLER REDEFINES CERTRPF.
               10  CERTRPA PIC  X(0001).
           05  CERTRPI PIC  X(0010).
      *    -------------------------------
           05  SUFFIXL PIC S9(0004) COMP.
           05  SUFFIXF PIC  X(0001).
           05  FILLER REDEFINES SUFFIXF.
               10  SUFFIXA PIC  X(0001).
           05  SUFFIXI PIC  X(0001).
      *    -------------------------------
           05  FORML PIC S9(0004) COMP.
           05  FORMF PIC  X(0001).
           05  FILLER REDEFINES FORMF.
               10  FORMA PIC  X(0001).
           05  FORMI PIC  X(0004).
      *    -------------------------------
           05  ENTRYL PIC S9(0004) COMP.
           05  ENTRYF PIC  X(0001).
           05  FILLER REDEFINES ENTRYF.
               10  ENTRYA PIC  X(0001).
           05  ENTRYI PIC  X(0006).
      *    -------------------------------
           05  CKCNTLL PIC S9(0004) COMP.
           05  CKCNTLF PIC  X(0001).
           05  FILLER REDEFINES CKCNTLF.
               10  CKCNTLA PIC  X(0001).
           05  CKCNTLI PIC  9(8).
      *    -------------------------------
           05  CARRIERL PIC S9(0004) COMP.
           05  CARRIERF PIC  X(0001).
           05  FILLER REDEFINES CARRIERF.
               10  CARRIERA PIC  X(0001).
           05  CARRIERI PIC  X(0001).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  ACCTL PIC S9(0004) COMP.
           05  ACCTF PIC  X(0001).
           05  FILLER REDEFINES ACCTF.
               10  ACCTA PIC  X(0001).
           05  ACCTI PIC  X(0010).
      *    -------------------------------
           05  EFFDTEL PIC S9(0004) COMP.
           05  EFFDTEF PIC  X(0001).
           05  FILLER REDEFINES EFFDTEF.
               10  EFFDTEA PIC  X(0001).
           05  EFFDTEI PIC  X(0008).
      *    -------------------------------
           05  PROCSRL PIC S9(0004) COMP.
           05  PROCSRF PIC  X(0001).
           05  FILLER REDEFINES PROCSRF.
               10  PROCSRA PIC  X(0001).
           05  PROCSRI PIC  X(0004).
      *    -------------------------------
           05  CORRSELL PIC S9(0004) COMP.
           05  CORRSELF PIC  X(0001).
           05  FILLER REDEFINES CORRSELF.
               10  CORRSELA PIC  X(0001).
           05  CORRSELI PIC  X(0002).
      *    -------------------------------
           05  STATSELL PIC S9(0004) COMP.
           05  STATSELF PIC  X(0001).
           05  FILLER REDEFINES STATSELF.
               10  STATSELA PIC  X(0001).
           05  STATSELI PIC  X(0001).
      *    -------------------------------
           05  ARCNO1L PIC S9(0004) COMP.
           05  ARCNO1F PIC  X(0001).
           05  FILLER REDEFINES ARCNO1F.
               10  ARCNO1A PIC  X(0001).
           05  ARCNO1I PIC  X(0008).
      *    -------------------------------
           05  FORM1L PIC S9(0004) COMP.
           05  FORM1F PIC  X(0001).
           05  FILLER REDEFINES FORM1F.
               10  FORM1A PIC  X(0001).
           05  FORM1I PIC  X(0004).
      *    -------------------------------
           05  USRID1L PIC S9(0004) COMP.
           05  USRID1F PIC  X(0001).
           05  FILLER REDEFINES USRID1F.
               10  USRID1A PIC  X(0001).
           05  USRID1I PIC  X(0004).
      *    -------------------------------
           05  PRINT1L PIC S9(0004) COMP.
           05  PRINT1F PIC  X(0001).
           05  FILLER REDEFINES PRINT1F.
               10  PRINT1A PIC  X(0001).
           05  PRINT1I PIC  X(0008).
      *    -------------------------------
           05  STATS1L PIC S9(0004) COMP.
           05  STATS1F PIC  X(0001).
           05  FILLER REDEFINES STATS1F.
               10  STATS1A PIC  X(0001).
           05  STATS1I PIC  X(0008).
      *    -------------------------------
           05  RSFRM1L PIC S9(0004) COMP.
           05  RSFRM1F PIC  X(0001).
           05  FILLER REDEFINES RSFRM1F.
               10  RSFRM1A PIC  X(0001).
           05  RSFRM1I PIC  X(0004).
      *    -------------------------------
           05  RSDAT1L PIC S9(0004) COMP.
           05  RSDAT1F PIC  X(0001).
           05  FILLER REDEFINES RSDAT1F.
               10  RSDAT1A PIC  X(0001).
           05  RSDAT1I PIC  X(0008).
      *    -------------------------------
           05  ARCNO2L PIC S9(0004) COMP.
           05  ARCNO2F PIC  X(0001).
           05  FILLER REDEFINES ARCNO2F.
               10  ARCNO2A PIC  X(0001).
           05  ARCNO2I PIC  X(0008).
      *    -------------------------------
           05  FORM2L PIC S9(0004) COMP.
           05  FORM2F PIC  X(0001).
           05  FILLER REDEFINES FORM2F.
               10  FORM2A PIC  X(0001).
           05  FORM2I PIC  X(0004).
      *    -------------------------------
           05  USRID2L PIC S9(0004) COMP.
           05  USRID2F PIC  X(0001).
           05  FILLER REDEFINES USRID2F.
               10  USRID2A PIC  X(0001).
           05  USRID2I PIC  X(0004).
      *    -------------------------------
           05  PRINT2L PIC S9(0004) COMP.
           05  PRINT2F PIC  X(0001).
           05  FILLER REDEFINES PRINT2F.
               10  PRINT2A PIC  X(0001).
           05  PRINT2I PIC  X(0008).
      *    -------------------------------
           05  STATS2L PIC S9(0004) COMP.
           05  STATS2F PIC  X(0001).
           05  FILLER REDEFINES STATS2F.
               10  STATS2A PIC  X(0001).
           05  STATS2I PIC  X(0008).
      *    -------------------------------
           05  RSFRM2L PIC S9(0004) COMP.
           05  RSFRM2F PIC  X(0001).
           05  FILLER REDEFINES RSFRM2F.
               10  RSFRM2A PIC  X(0001).
           05  RSFRM2I PIC  X(0004).
      *    -------------------------------
           05  RSDAT2L PIC S9(0004) COMP.
           05  RSDAT2F PIC  X(0001).
           05  FILLER REDEFINES RSDAT2F.
               10  RSDAT2A PIC  X(0001).
           05  RSDAT2I PIC  X(0008).
      *    -------------------------------
           05  ARCNO3L PIC S9(0004) COMP.
           05  ARCNO3F PIC  X(0001).
           05  FILLER REDEFINES ARCNO3F.
               10  ARCNO3A PIC  X(0001).
           05  ARCNO3I PIC  X(0008).
      *    -------------------------------
           05  FORM3L PIC S9(0004) COMP.
           05  FORM3F PIC  X(0001).
           05  FILLER REDEFINES FORM3F.
               10  FORM3A PIC  X(0001).
           05  FORM3I PIC  X(0004).
      *    -------------------------------
           05  USRID3L PIC S9(0004) COMP.
           05  USRID3F PIC  X(0001).
           05  FILLER REDEFINES USRID3F.
               10  USRID3A PIC  X(0001).
           05  USRID3I PIC  X(0004).
      *    -------------------------------
           05  PRINT3L PIC S9(0004) COMP.
           05  PRINT3F PIC  X(0001).
           05  FILLER REDEFINES PRINT3F.
               10  PRINT3A PIC  X(0001).
           05  PRINT3I PIC  X(0008).
      *    -------------------------------
           05  STATS3L PIC S9(0004) COMP.
           05  STATS3F PIC  X(0001).
           05  FILLER REDEFINES STATS3F.
               10  STATS3A PIC  X(0001).
           05  STATS3I PIC  X(0008).
      *    -------------------------------
           05  RSFRM3L PIC S9(0004) COMP.
           05  RSFRM3F PIC  X(0001).
           05  FILLER REDEFINES RSFRM3F.
               10  RSFRM3A PIC  X(0001).
           05  RSFRM3I PIC  X(0004).
      *    -------------------------------
           05  RSDAT3L PIC S9(0004) COMP.
           05  RSDAT3F PIC  X(0001).
           05  FILLER REDEFINES RSDAT3F.
               10  RSDAT3A PIC  X(0001).
           05  RSDAT3I PIC  X(0008).
      *    -------------------------------
           05  ARCNO4L PIC S9(0004) COMP.
           05  ARCNO4F PIC  X(0001).
           05  FILLER REDEFINES ARCNO4F.
               10  ARCNO4A PIC  X(0001).
           05  ARCNO4I PIC  X(0008).
      *    -------------------------------
           05  FORM4L PIC S9(0004) COMP.
           05  FORM4F PIC  X(0001).
           05  FILLER REDEFINES FORM4F.
               10  FORM4A PIC  X(0001).
           05  FORM4I PIC  X(0004).
      *    -------------------------------
           05  USRID4L PIC S9(0004) COMP.
           05  USRID4F PIC  X(0001).
           05  FILLER REDEFINES USRID4F.
               10  USRID4A PIC  X(0001).
           05  USRID4I PIC  X(0004).
      *    -------------------------------
           05  PRINT4L PIC S9(0004) COMP.
           05  PRINT4F PIC  X(0001).
           05  FILLER REDEFINES PRINT4F.
               10  PRINT4A PIC  X(0001).
           05  PRINT4I PIC  X(0008).
      *    -------------------------------
           05  STATS4L PIC S9(0004) COMP.
           05  STATS4F PIC  X(0001).
           05  FILLER REDEFINES STATS4F.
               10  STATS4A PIC  X(0001).
           05  STATS4I PIC  X(0008).
      *    -------------------------------
           05  RSFRM4L PIC S9(0004) COMP.
           05  RSFRM4F PIC  X(0001).
           05  FILLER REDEFINES RSFRM4F.
               10  RSFRM4A PIC  X(0001).
           05  RSFRM4I PIC  X(0004).
      *    -------------------------------
           05  RSDAT4L PIC S9(0004) COMP.
           05  RSDAT4F PIC  X(0001).
           05  FILLER REDEFINES RSDAT4F.
               10  RSDAT4A PIC  X(0001).
           05  RSDAT4I PIC  X(0008).
      *    -------------------------------
           05  ARCNO5L PIC S9(0004) COMP.
           05  ARCNO5F PIC  X(0001).
           05  FILLER REDEFINES ARCNO5F.
               10  ARCNO5A PIC  X(0001).
           05  ARCNO5I PIC  X(0008).
      *    -------------------------------
           05  FORM5L PIC S9(0004) COMP.
           05  FORM5F PIC  X(0001).
           05  FILLER REDEFINES FORM5F.
               10  FORM5A PIC  X(0001).
           05  FORM5I PIC  X(0004).
      *    -------------------------------
           05  USRID5L PIC S9(0004) COMP.
           05  USRID5F PIC  X(0001).
           05  FILLER REDEFINES USRID5F.
               10  USRID5A PIC  X(0001).
           05  USRID5I PIC  X(0004).
      *    -------------------------------
           05  PRINT5L PIC S9(0004) COMP.
           05  PRINT5F PIC  X(0001).
           05  FILLER REDEFINES PRINT5F.
               10  PRINT5A PIC  X(0001).
           05  PRINT5I PIC  X(0008).
      *    -------------------------------
           05  STATS5L PIC S9(0004) COMP.
           05  STATS5F PIC  X(0001).
           05  FILLER REDEFINES STATS5F.
               10  STATS5A PIC  X(0001).
           05  STATS5I PIC  X(0008).
      *    -------------------------------
           05  RSFRM5L PIC S9(0004) COMP.
           05  RSFRM5F PIC  X(0001).
           05  FILLER REDEFINES RSFRM5F.
               10  RSFRM5A PIC  X(0001).
           05  RSFRM5I PIC  X(0004).
      *    -------------------------------
           05  RSDAT5L PIC S9(0004) COMP.
           05  RSDAT5F PIC  X(0001).
           05  FILLER REDEFINES RSDAT5F.
               10  RSDAT5A PIC  X(0001).
           05  RSDAT5I PIC  X(0008).
      *    -------------------------------
           05  ARCNO6L PIC S9(0004) COMP.
           05  ARCNO6F PIC  X(0001).
           05  FILLER REDEFINES ARCNO6F.
               10  ARCNO6A PIC  X(0001).
           05  ARCNO6I PIC  X(0008).
      *    -------------------------------
           05  FORM6L PIC S9(0004) COMP.
           05  FORM6F PIC  X(0001).
           05  FILLER REDEFINES FORM6F.
               10  FORM6A PIC  X(0001).
           05  FORM6I PIC  X(0004).
      *    -------------------------------
           05  USRID6L PIC S9(0004) COMP.
           05  USRID6F PIC  X(0001).
           05  FILLER REDEFINES USRID6F.
               10  USRID6A PIC  X(0001).
           05  USRID6I PIC  X(0004).
      *    -------------------------------
           05  PRINT6L PIC S9(0004) COMP.
           05  PRINT6F PIC  X(0001).
           05  FILLER REDEFINES PRINT6F.
               10  PRINT6A PIC  X(0001).
           05  PRINT6I PIC  X(0008).
      *    -------------------------------
           05  STATS6L PIC S9(0004) COMP.
           05  STATS6F PIC  X(0001).
           05  FILLER REDEFINES STATS6F.
               10  STATS6A PIC  X(0001).
           05  STATS6I PIC  X(0008).
      *    -------------------------------
           05  RSFRM6L PIC S9(0004) COMP.
           05  RSFRM6F PIC  X(0001).
           05  FILLER REDEFINES RSFRM6F.
               10  RSFRM6A PIC  X(0001).
           05  RSFRM6I PIC  X(0004).
      *    -------------------------------
           05  RSDAT6L PIC S9(0004) COMP.
           05  RSDAT6F PIC  X(0001).
           05  FILLER REDEFINES RSDAT6F.
               10  RSDAT6A PIC  X(0001).
           05  RSDAT6I PIC  X(0008).
      *    -------------------------------
           05  ARCNO7L PIC S9(0004) COMP.
           05  ARCNO7F PIC  X(0001).
           05  FILLER REDEFINES ARCNO7F.
               10  ARCNO7A PIC  X(0001).
           05  ARCNO7I PIC  X(0008).
      *    -------------------------------
           05  FORM7L PIC S9(0004) COMP.
           05  FORM7F PIC  X(0001).
           05  FILLER REDEFINES FORM7F.
               10  FORM7A PIC  X(0001).
           05  FORM7I PIC  X(0004).
      *    -------------------------------
           05  USRID7L PIC S9(0004) COMP.
           05  USRID7F PIC  X(0001).
           05  FILLER REDEFINES USRID7F.
               10  USRID7A PIC  X(0001).
           05  USRID7I PIC  X(0004).
      *    -------------------------------
           05  PRINT7L PIC S9(0004) COMP.
           05  PRINT7F PIC  X(0001).
           05  FILLER REDEFINES PRINT7F.
               10  PRINT7A PIC  X(0001).
           05  PRINT7I PIC  X(0008).
      *    -------------------------------
           05  STATS7L PIC S9(0004) COMP.
           05  STATS7F PIC  X(0001).
           05  FILLER REDEFINES STATS7F.
               10  STATS7A PIC  X(0001).
           05  STATS7I PIC  X(0008).
      *    -------------------------------
           05  RSFRM7L PIC S9(0004) COMP.
           05  RSFRM7F PIC  X(0001).
           05  FILLER REDEFINES RSFRM7F.
               10  RSFRM7A PIC  X(0001).
           05  RSFRM7I PIC  X(0004).
      *    -------------------------------
           05  RSDAT7L PIC S9(0004) COMP.
           05  RSDAT7F PIC  X(0001).
           05  FILLER REDEFINES RSDAT7F.
               10  RSDAT7A PIC  X(0001).
           05  RSDAT7I PIC  X(0008).
      *    -------------------------------
           05  ARCNO8L PIC S9(0004) COMP.
           05  ARCNO8F PIC  X(0001).
           05  FILLER REDEFINES ARCNO8F.
               10  ARCNO8A PIC  X(0001).
           05  ARCNO8I PIC  X(0008).
      *    -------------------------------
           05  FORM8L PIC S9(0004) COMP.
           05  FORM8F PIC  X(0001).
           05  FILLER REDEFINES FORM8F.
               10  FORM8A PIC  X(0001).
           05  FORM8I PIC  X(0004).
      *    -------------------------------
           05  USRID8L PIC S9(0004) COMP.
           05  USRID8F PIC  X(0001).
           05  FILLER REDEFINES USRID8F.
               10  USRID8A PIC  X(0001).
           05  USRID8I PIC  X(0004).
      *    -------------------------------
           05  PRINT8L PIC S9(0004) COMP.
           05  PRINT8F PIC  X(0001).
           05  FILLER REDEFINES PRINT8F.
               10  PRINT8A PIC  X(0001).
           05  PRINT8I PIC  X(0008).
      *    -------------------------------
           05  STATS8L PIC S9(0004) COMP.
           05  STATS8F PIC  X(0001).
           05  FILLER REDEFINES STATS8F.
               10  STATS8A PIC  X(0001).
           05  STATS8I PIC  X(0008).
      *    -------------------------------
           05  RSFRM8L PIC S9(0004) COMP.
           05  RSFRM8F PIC  X(0001).
           05  FILLER REDEFINES RSFRM8F.
               10  RSFRM8A PIC  X(0001).
           05  RSFRM8I PIC  X(0004).
      *    -------------------------------
           05  RSDAT8L PIC S9(0004) COMP.
           05  RSDAT8F PIC  X(0001).
           05  FILLER REDEFINES RSDAT8F.
               10  RSDAT8A PIC  X(0001).
           05  RSDAT8I PIC  X(0008).
      *    -------------------------------
           05  ARCNO9L PIC S9(0004) COMP.
           05  ARCNO9F PIC  X(0001).
           05  FILLER REDEFINES ARCNO9F.
               10  ARCNO9A PIC  X(0001).
           05  ARCNO9I PIC  X(0008).
      *    -------------------------------
           05  FORM9L PIC S9(0004) COMP.
           05  FORM9F PIC  X(0001).
           05  FILLER REDEFINES FORM9F.
               10  FORM9A PIC  X(0001).
           05  FORM9I PIC  X(0004).
      *    -------------------------------
           05  USRID9L PIC S9(0004) COMP.
           05  USRID9F PIC  X(0001).
           05  FILLER REDEFINES USRID9F.
               10  USRID9A PIC  X(0001).
           05  USRID9I PIC  X(0004).
      *    -------------------------------
           05  PRINT9L PIC S9(0004) COMP.
           05  PRINT9F PIC  X(0001).
           05  FILLER REDEFINES PRINT9F.
               10  PRINT9A PIC  X(0001).
           05  PRINT9I PIC  X(0008).
      *    -------------------------------
           05  STATS9L PIC S9(0004) COMP.
           05  STATS9F PIC  X(0001).
           05  FILLER REDEFINES STATS9F.
               10  STATS9A PIC  X(0001).
           05  STATS9I PIC  X(0008).
      *    -------------------------------
           05  RSFRM9L PIC S9(0004) COMP.
           05  RSFRM9F PIC  X(0001).
           05  FILLER REDEFINES RSFRM9F.
               10  RSFRM9A PIC  X(0001).
           05  RSFRM9I PIC  X(0004).
      *    -------------------------------
           05  RSDAT9L PIC S9(0004) COMP.
           05  RSDAT9F PIC  X(0001).
           05  FILLER REDEFINES RSDAT9F.
               10  RSDAT9A PIC  X(0001).
           05  RSDAT9I PIC  X(0008).
      *    -------------------------------
           05  ARCNO10L PIC S9(0004) COMP.
           05  ARCNO10F PIC  X(0001).
           05  FILLER REDEFINES ARCNO10F.
               10  ARCNO10A PIC  X(0001).
           05  ARCNO10I PIC  X(0008).
      *    -------------------------------
           05  FORM10L PIC S9(0004) COMP.
           05  FORM10F PIC  X(0001).
           05  FILLER REDEFINES FORM10F.
               10  FORM10A PIC  X(0001).
           05  FORM10I PIC  X(0004).
      *    -------------------------------
           05  USRID10L PIC S9(0004) COMP.
           05  USRID10F PIC  X(0001).
           05  FILLER REDEFINES USRID10F.
               10  USRID10A PIC  X(0001).
           05  USRID10I PIC  X(0004).
      *    -------------------------------
           05  PRINT10L PIC S9(0004) COMP.
           05  PRINT10F PIC  X(0001).
           05  FILLER REDEFINES PRINT10F.
               10  PRINT10A PIC  X(0001).
           05  PRINT10I PIC  X(0008).
      *    -------------------------------
           05  STATS10L PIC S9(0004) COMP.
           05  STATS10F PIC  X(0001).
           05  FILLER REDEFINES STATS10F.
               10  STATS10A PIC  X(0001).
           05  STATS10I PIC  X(0008).
      *    -------------------------------
           05  RSFRM10L PIC S9(0004) COMP.
           05  RSFRM10F PIC  X(0001).
           05  FILLER REDEFINES RSFRM10F.
               10  RSFRM10A PIC  X(0001).
           05  RSFRM10I PIC  X(0004).
      *    -------------------------------
           05  RSDAT10L PIC S9(0004) COMP.
           05  RSDAT10F PIC  X(0001).
           05  FILLER REDEFINES RSDAT10F.
               10  RSDAT10A PIC  X(0001).
           05  RSDAT10I PIC  X(0008).
      *    -------------------------------
           05  ARCNO11L PIC S9(0004) COMP.
           05  ARCNO11F PIC  X(0001).
           05  FILLER REDEFINES ARCNO11F.
               10  ARCNO11A PIC  X(0001).
           05  ARCNO11I PIC  X(0008).
      *    -------------------------------
           05  FORM11L PIC S9(0004) COMP.
           05  FORM11F PIC  X(0001).
           05  FILLER REDEFINES FORM11F.
               10  FORM11A PIC  X(0001).
           05  FORM11I PIC  X(0004).
      *    -------------------------------
           05  USRID11L PIC S9(0004) COMP.
           05  USRID11F PIC  X(0001).
           05  FILLER REDEFINES USRID11F.
               10  USRID11A PIC  X(0001).
           05  USRID11I PIC  X(0004).
      *    -------------------------------
           05  PRINT11L PIC S9(0004) COMP.
           05  PRINT11F PIC  X(0001).
           05  FILLER REDEFINES PRINT11F.
               10  PRINT11A PIC  X(0001).
           05  PRINT11I PIC  X(0008).
      *    -------------------------------
           05  STATS11L PIC S9(0004) COMP.
           05  STATS11F PIC  X(0001).
           05  FILLER REDEFINES STATS11F.
               10  STATS11A PIC  X(0001).
           05  STATS11I PIC  X(0008).
      *    -------------------------------
           05  RSFRM11L PIC S9(0004) COMP.
           05  RSFRM11F PIC  X(0001).
           05  FILLER REDEFINES RSFRM11F.
               10  RSFRM11A PIC  X(0001).
           05  RSFRM11I PIC  X(0004).
      *    -------------------------------
           05  RSDAT11L PIC S9(0004) COMP.
           05  RSDAT11F PIC  X(0001).
           05  FILLER REDEFINES RSDAT11F.
               10  RSDAT11A PIC  X(0001).
           05  RSDAT11I PIC  X(0008).
      *    -------------------------------
           05  ARCNO12L PIC S9(0004) COMP.
           05  ARCNO12F PIC  X(0001).
           05  FILLER REDEFINES ARCNO12F.
               10  ARCNO12A PIC  X(0001).
           05  ARCNO12I PIC  X(0008).
      *    -------------------------------
           05  FORM12L PIC S9(0004) COMP.
           05  FORM12F PIC  X(0001).
           05  FILLER REDEFINES FORM12F.
               10  FORM12A PIC  X(0001).
           05  FORM12I PIC  X(0004).
      *    -------------------------------
           05  USRID12L PIC S9(0004) COMP.
           05  USRID12F PIC  X(0001).
           05  FILLER REDEFINES USRID12F.
               10  USRID12A PIC  X(0001).
           05  USRID12I PIC  X(0004).
      *    -------------------------------
           05  PRINT12L PIC S9(0004) COMP.
           05  PRINT12F PIC  X(0001).
           05  FILLER REDEFINES PRINT12F.
               10  PRINT12A PIC  X(0001).
           05  PRINT12I PIC  X(0008).
      *    -------------------------------
           05  STATS12L PIC S9(0004) COMP.
           05  STATS12F PIC  X(0001).
           05  FILLER REDEFINES STATS12F.
               10  STATS12A PIC  X(0001).
           05  STATS12I PIC  X(0008).
      *    -------------------------------
           05  RSFRM12L PIC S9(0004) COMP.
           05  RSFRM12F PIC  X(0001).
           05  FILLER REDEFINES RSFRM12F.
               10  RSFRM12A PIC  X(0001).
           05  RSFRM12I PIC  X(0004).
      *    -------------------------------
           05  RSDAT12L PIC S9(0004) COMP.
           05  RSDAT12F PIC  X(0001).
           05  FILLER REDEFINES RSDAT12F.
               10  RSDAT12A PIC  X(0001).
           05  RSDAT12I PIC  X(0008).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  9(2).
       01  EL690AO REDEFINES EL690AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPANYO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEBRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTRPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUFFIXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORMO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTRYO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKCNTLO PIC  ZZZZZZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROCSRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CORRSELO PIC  ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATSELO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCNO1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRID1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINT1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATS1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSFRM1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSDAT1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCNO2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRID2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINT2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATS2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSFRM2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSDAT2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCNO3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM3O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRID3O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINT3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATS3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSFRM3O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSDAT3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCNO4O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM4O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRID4O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINT4O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATS4O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSFRM4O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSDAT4O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCNO5O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM5O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRID5O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINT5O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATS5O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSFRM5O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSDAT5O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCNO6O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM6O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRID6O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINT6O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATS6O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSFRM6O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSDAT6O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCNO7O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM7O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRID7O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINT7O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATS7O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSFRM7O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSDAT7O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCNO8O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM8O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRID8O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINT8O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATS8O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSFRM8O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSDAT8O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCNO9O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM9O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRID9O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINT9O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATS9O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSFRM9O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSDAT9O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCNO10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM10O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRID10O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINT10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATS10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSFRM10O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSDAT10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCNO11O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM11O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRID11O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINT11O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATS11O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSFRM11O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSDAT11O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCNO12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM12O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRID12O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINT12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATS12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSFRM12O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSDAT12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  X(0002).
      *    -------------------------------
00221  01  FILLER REDEFINES EL690AI.
101101     16  FILLER                  PIC X(154).
00223      16  W-ARCH-GROUPS.
00224          20  W-ARCHGRP-DATA OCCURS 12 TIMES.
00225              24  W-ARCNOL        PIC S9(04) COMP.
00226              24  W-ARCNOA        PIC  X(01).
00227              24  W-ARCNOO        PIC  9(08).
070711             24  W-FORML         PIC S9(04) COMP.
070711             24  W-FORMA         PIC  X(01).
070711             24  W-FORMO         PIC  X(04).
070711             24  W-USRIDL        PIC S9(04) COMP.
070711             24  W-USRIDA        PIC  X(01).
070711             24  W-USRIDO        PIC  X(04).
070711             24  W-PRINTL        PIC S9(04) COMP.
070711             24  W-PRINTA        PIC  X(01).
070711             24  W-PRINTO        PIC  X(08).
00243              24  W-STATUSL       PIC S9(04) COMP.
00244              24  W-STATUSA       PIC  X(01).
00245              24  W-STATUSO       PIC  X(08).
070711             24  W-RSFRML        PIC S9(04) COMP.
070711             24  W-RSFRMA        PIC  X(01).
070711             24  W-RSFRMO        PIC  X(04).
070711             24  W-RSDATL        PIC S9(04) COMP.
070711             24  W-RSDATA        PIC  X(01).
070711             24  W-RSDATO        PIC  X(08).
070711     16  FILLER                  PIC  X(87).
00255                                  EJECT
00256
00257  01  FILLER                      PIC  X(22)
00258                                  VALUE 'PGRM INTERFACE STARTS:'.
00259 *    COPY ELCINTF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
00260      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00261 *    COPY ELC1042.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                           ELC1042                              *
00004 *                            VMOD=2.002                          *
00005 *                                                                *
00006 *    NOTE                                                        *
00007 *        THE WORK AREA IS USED BY EL152, EL1522, EL1042, EL153,  *
00008 *        EM152, EM1522, EL689, EL6892, EL6311, AND EL690.        *
00009 *        THIS COPYBOOK SHOULD NOT BE CHANGED WITHOUT REFERENCE   *
00010 *        TO THESE PROGRAMS.                                      *
00011 *                                                                *
00012 *    NOTE                                                        *
00013 *        THE FILLER AREA AT THE BOTTOM ARE FOR FUTURE EL1042     *
00014 *        USE ONLY!                                               *
00015 *                                                                *
00016 ******************************************************************
00017
00018          16  PI-1042-WA.
00019              20  PI-ACTION       PIC  X(01).
00020                  88 PI-SHOW-MODE           VALUE '1'.
00021                  88 PI-CLEAR-MODE          VALUE '2'.
00022                  88 PI-CREATE-MODE         VALUE '3'.
00023              20  PI-COMM-CONTROL PIC  X(12).
00024              20  PI-CURRENT-LINE PIC S9(03) COMP-3.
00025              20  PI-EOF-SW       PIC  X(01).
00026                  88  PI-FILE-EOF           VALUE 'Y'.
00027              20  PI-FILETYP      PIC  X(01).
00028              20  PI-FORM-SQUEEZE-CONTROL
00029                                  PIC  X(01).
00030                  88  PI-FORM-SQUEEZE-ON     VALUE 'Y'.
00031                  88  PI-FORM-SQUEEZE-OFF    VALUE ' '.
00032              20  PI-LAST-CONTROL PIC  X(12).
00033              20  PI-TEMP-STOR-ITEMS
00034                                  PIC S9(04) COMP.
00035              20  PI-TOTAL-LINES  PIC S9(03) COMP-3.
00036              20  PI-UPDATE-SW    PIC  9(01).
00037                  88 ANY-UPDATES            VALUE 1.
00038              20  PI-104-SCREEN-SENT-IND
00039                                  PIC  X(01).
00040                  88  PI-104-SCREEN-SENT    VALUE 'Y'.
00041                  88  PI-104-SCREEN-NOT-SENT VALUE 'N'.
00042              20  PI-1042-SCREEN-SENT-IND
00043                                  PIC  X(01).
00044                  88  PI-1042-SCREEN-SENT    VALUE 'Y'.
00045                  88  PI-1042-SCREEN-NOT-SENT VALUE 'N'.
00046              20  PI-1042-ARCHIVE-IND
00047                                  PIC  X(01).
00048                  88  PI-1042-ARCHIVE-LETTER VALUE 'Y'.
00049              20  FILLER          PIC  X(29).
00262 *    COPY ELC689PI.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELC689PI                            *
00004 *                            VMOD=2.003                          *
00005 *                                                                *
00006 *    THIS IS THE PI-PROGRAM-WORK-AREA THAT IS USED FOR THE       *
00007 *    CREDIT CORRESPONDENCE SUB-SYSTEM.  ANY CHANGES WILL         *
00008 *    WILL EFFECT THE PROGRAMS OF THAT SUB-SYSTEM.                *
00009 *                                                                *
00010 *    IF THE LENGTH OF THIS PI-AREA CHANGES THE LENGTH MUST       *
00011 *    BE CHANGED FOR THE COMM-AREA WHEN PASSING THIS PI-AREA      *
00012 *    BETWEEN PROGRAMS.                                           *
00013 *                                                                *
00014 *    THE FOLLOWING PROGRAMS USE THIS COPYBOOK:                   *
00015 *                                                                *
00016 *               EL631 - EL689  - EL6891 - EL6892                 *
00017 *                                                                *
00018 ******************************************************************
081004*                   C H A N G E   L O G
081004*
081004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
081004*-----------------------------------------------------------------
081004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
081004* EFFECTIVE    NUMBER
081004*-----------------------------------------------------------------
081004* 081004                   PEMA  CONVERT TO PSUEDO CONVERSATIONAL
100705* 100705  CR2004072800004  PEMA  ADD LETTERS TO BE RESENT
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
081004******************************************************************
00019
00020
00021          16  PI-689-WORK-AREA.
00022              20  PI-689-ALT-PRINTER-ID
00023                                  PIC  X(04).
00024              20  PI-689-ARCHIVE-NUMBER
00025                                  PIC  9(08).
00026              20  PI-689-ARCHIVE-SW
00027                                  PIC  X(01).
00028                  88  PI-689-ARCHIVE-LETTER VALUE 'Y'.
00029              20  PI-689-DATA-SOURCE
00030                                  PIC  X(01).
00031                  88  PI-689-SRC-ACCOUNT        VALUE '1'.
00032                  88  PI-689-SRC-CERTIFICATE    VALUE '2'.
00033                  88  PI-689-SRC-COMPENSATION   VALUE '3'.
00034                  88  PI-689-SRC-PEND-BUSINESS  VALUE '4'.
00035                  88  PI-689-SRC-CHECKS         VALUE '5'.
00036              20  PI-689-ERROR-IND
00037                                  PIC  X(01).
00038                  88  PI-689-ERR-DETECTED-PREV  VALUE 'Y'.
00039              20  PI-689-ERROR    PIC  9(04).
00040                  88  PI-689-NO-ERRORS-DETECTED VALUE 0000.
00041                  88  PI-689-FATAL-ERROR
00042                      VALUES 0004 0006 0008 0013 0023 0029 0033
00043                             0042 0047 0051 0066 0067 0070
00044                             0168 0169 0174 0175 0176 0177 0179
00045                             0180 0181 0182 0184 0185 0189 0190
00046                             0191
00047                             0215 0279 0280
00048                             0412 0413 0454
00049                             0533 0537
00050                             2055 2114 2208 2209 2216 2232 2369
00051                             2398 2433 2908 2999
00052                             3000 3770 3771 3775
00053                             7250 7365 7367 7368 7369 7370 7371
00054                             7272 7373 7374 7376 7377 7378 7379
00055                             7381 7388 7390 7393 7395 7396 7398
00056                             9095 9096 9281 9298 9299 9320 9327
00057                             9426 9427.
00058                  88  PI-689-STOP-ERROR
00059                      VALUES 0004 0008 0013 0023 0029 0033
00060                             0042 0047 0066 0067 0070
00061                             0168 0169 0174 0175 0176 0177
00062                             0181 0182 0184 0185 0189 0190
00063                             0279 0280
00064                             0412 0413 0454
00065                             2055 2208 2209 2216 2232
00066                             2398 2999
00067                             3000 3770 3771 3775
00068                             7250 7365 7369 7370 7371
00069                             7272 7373 7374 7376 7377 7378 7379
00070                             7381 7388 7390 7393 7396 7398
00071                             9095 9096 9299 9320 9426.
00072              20  PI-689-FOLLOW-UP-DATE
00073                                  PIC  X(02).
00074              20  PI-689-FORM-NUMBER
00075                                  PIC  X(04).
00076              20  PI-689-LABEL-SOURCE
00077                                  PIC X(01).
00078                  88  PI-689-SOURCE-ACCOUNT  VALUE '1'.
00079                  88  PI-689-SOURCE-CARRIER  VALUE '2'.
00080                  88  PI-689-SOURCE-COMPANY  VALUE '3'.
00081                  88  PI-689-SOURCE-COMP     VALUE '4'.
00082                  88  PI-689-SOURCE-MAIL     VALUE '5'.
00083                  88  PI-689-SOURCE-CHECK    VALUE '6'.
00084                  88  PI-689-SOURCE-VARIABLE VALUE '7'.
00085              20  PI-689-NUMBER-COPIES
00086                                  PIC  9(01).
00087              20  PI-689-NUMBER-LABEL-LINES
00088                                  PIC  9(01).
00089              20  PI-689-NUMBER-TEXT-RECORDS
00090                                  PIC  9(03).
00091              20  PI-689-PRINT-ORDER-SW
00092                                  PIC  X(01).
00093                  88  PI-689-PRINT-FIRST     VALUE '1'.
00094                  88  PI-689-PRINT-SECOND    VALUE '2'.
00095                  88  PI-689-PRINT-LATER     VALUE '3'.
00096                  88  PI-689-PRINT-ONLY      VALUE '4'.
00097              20  PI-689-PRINT-RESTRICTION
00098                                  PIC  X(01).
00099                  88  PI-689-VALID-RESTRICT     VALUE 'C' 'F'.
00100                  88  PI-689-PRT-ONLY-WITH-CNTL VALUE 'C'.
00101                  88  PI-689-PRT-ONLY-WITH-FORM VALUE 'F'.
00102              20  PI-689-PRINT-SW PIC  X(01).
00103                  88  PI-689-PRINT-PERFORMED VALUE '1'.
00104              20  PI-689-RESEND-DATE-1
00105                                  PIC  X(02).
100705             20  PI-689-RESEND-LETR-1
                                       PIC X(4).
00110              20  PI-689-TEMP-STOR-ID
00111                                  PIC  X(08).
00112              20  PI-689-USE-SCREEN-IND
00113                                  PIC  X(01).
00114                  88  PI-689-CREATE-NO-SCREENS VALUE '1'.
00115              20  PI-689-ARCH-POINTER
00116                                  PIC S9(08) COMP.
00117                  88  PI-689-GET-ARCH-MAIN     VALUE +0.
00118              20  PI-689-ARCT-POINTER
00119                                  PIC S9(08) COMP.
00120                  88  PI-689-GET-ARCT-MAIN     VALUE +0.
00121              20  PI-689-VARIABLE-DATA-GRP.
00122                  24  PI-689-VARIABLE-DATA-1
00123                                  PIC  X(30).
00124                  24  PI-689-VARIABLE-DATA-2
00125                                  PIC  X(30).
00126                  24  PI-689-VARIABLE-DATA-3
00127                                  PIC  X(30).
00128                  24  PI-689-VARIABLE-DATA-4
00129                                  PIC  X(30).
00130
00131          16  PI-689-KEY-DATA-FIELDS.
00132              20  PI-689-ACCOUNT  PIC  X(10).
00133              20  PI-689-CARRIER  PIC  X(01).
00134              20  PI-689-CERT-NO.
00135                  24  PI-689-CERT-PRIME
00136                                  PIC  X(10).
00137                  24  PI-689-CERT-SFX
00138                                  PIC  X(01).
00139              20  PI-689-CHG-SEQ-NO
00140                                  PIC S9(04)    COMP.
00141              20  PI-689-CHG-SEQ-NOX REDEFINES PI-689-CHG-SEQ-NO
00142                                  PIC  X(02).
00143              20  PI-689-ENTRY-BATCH
00144                                  PIC  X(06).
00145              20  PI-689-EFF-DATE PIC  X(02).
00146              20  PI-689-EXP-DATE PIC  X(02).
00147              20  PI-689-GROUPING PIC  X(06).
00148              20  PI-689-RESP-PERSON
00149                                  PIC  X(10).
00150              20  PI-689-SEQ-NO   PIC S9(08)    COMP.
00151              20  PI-689-SEQ-NOX REDEFINES PI-689-SEQ-NO
00152                                  PIC  X(04).
00153              20  PI-689-STATE    PIC  X(02).
00154              20  PI-689-TYPE     PIC  X(01).
00155              20  PI-689-CONTROL  PIC S9(08)    COMP.
00156              20  PI-689-ALT-SEQ-NO
00157                                  PIC S9(04)    COMP.
00158          16  PI-689-DATE-EDIT    PIC  X(08).
00159          16  PI-689-FOLLOW-UP-EDIT
00160                                  PIC  X(08).
00161          16  PI-689-RESEND1-EDIT PIC  X(08).
00164          16  PI-689-SEQ-EDIT     PIC  X(08).
00165          16  PI-689-BCSEQ-EDIT   PIC  X(04).
00166          16  PI-689-LBL-OVERRIDE PIC  X(01).
00167              88  PI-689-LABELS-OVERRIDEN  VALUES 'N'.
081004         16  PI-689-FATAL-CTR    PIC 999     COMP-3.
081004         16  PI-689-FORCABLE-CTR PIC 999     COMP-3.
00263          16  PI-690-WORK-AREA.
00264              20  PI-690-ARCHIVE-TABLE.
00265                  24  PI-690-ARCHIVE-NUM OCCURS 12 TIMES
00266                                  PIC S9(08) COMP.
00267              20  PI-690-CURSOR   PIC S9(04) COMP.
00268              20  PI-690-FIRST-DATA.
00269                  24  PI-690-FIRST-CERT-NO.
00270                      28  PI-690-FIRST-CERT-PRIME
00271                                  PIC  X(10).
00272                      28  PI-690-FIRST-SUFFIX
00273                                  PIC  X(01).
00274                  24  PI-690-FIRST-CARRIER
00275                                  PIC  X(01).
00276                  24  PI-690-FIRST-GROUPING
00277                                  PIC  X(06).
00278                  24  PI-690-FIRST-STATE
00279                                  PIC  X(02).
00280                  24  PI-690-FIRST-ACCOUNT
00281                                  PIC  X(10).
00282                  24  PI-690-FIRST-EFFECT-DATE
00283                                  PIC  X(02).
00284                  24  PI-690-FIRST-ENTRY.
00285                      28  PI-690-FIRST-CONTROL-PREFIX
00286                                  PIC  X(02).
00287                      28  PI-690-FIRST-CONTROL
00288                                  PIC S9(08) COMP.
00289                  24  PI-690-FIRST-FORM
00290                                  PIC  X(04).
00291                  24  PI-690-FIRST-PROCESSOR
00292                                  PIC  X(04).
00293                  24  PI-690-FIRST-ARCHIVE-NO
00294                                  PIC S9(08) COMP.
00295              20  PI-690-INIT-DATA.
00296                  24  PI-690-INIT-CERT-NO.
00297                      28  PI-690-INIT-CERT-PRIME
00298                                  PIC  X(10).
00299                      28  PI-690-INIT-SUFFIX
00300                                  PIC  X(01).
00301                  24  PI-690-INIT-CARRIER
00302                                  PIC  X(01).
00303                  24  PI-690-INIT-GROUPING
00304                                  PIC  X(06).
00305                  24  PI-690-INIT-STATE
00306                                  PIC  X(02).
00307                  24  PI-690-INIT-ACCOUNT
00308                                  PIC  X(10).
00309                  24  PI-690-INIT-EFFECT-DATE
00310                                  PIC  X(02).
00311                  24  PI-690-INIT-EFF-DTE
00312                                  PIC  X(08).
00313                  24  PI-690-INIT-ENTRY.
00314                      28  PI-690-INIT-CONTROL-PREFIX
00315                                  PIC  X(02).
00316                      28  PI-690-INIT-CONTROL
00317                                  PIC S9(08) COMP.
00318                  24  PI-690-INIT-FORM
00319                                  PIC  X(04).
00320                  24  PI-690-INIT-PROCESSOR
00321                                  PIC  X(04).
00322                  24  PI-690-INIT-ARCHIVE-NO
00323                                  PIC S9(08) COMP.
00324              20  PI-690-LAST-DATA.
00325                  24  PI-690-LAST-CERT-NO.
00326                      28  PI-690-LAST-CERT-PRIME
00327                                  PIC  X(10).
00328                      28  PI-690-LAST-SUFFIX
00329                                  PIC  X(01).
00330                  24  PI-690-LAST-CARRIER
00331                                  PIC  X(01).
00332                  24  PI-690-LAST-GROUPING
00333                                  PIC  X(06).
00334                  24  PI-690-LAST-STATE
00335                                  PIC  X(02).
00336                  24  PI-690-LAST-ACCOUNT
00337                                  PIC  X(10).
00338                  24  PI-690-LAST-EFFECT-DATE
00339                                  PIC  X(02).
00340                  24  PI-690-LAST-ENTRY.
00341                      28  PI-690-LAST-CONTROL-PREFIX
00342                                  PIC  X(02).
00343                      28  PI-690-LAST-CONTROL
00344                                  PIC S9(08) COMP.
00345                  24  PI-690-LAST-FORM
00346                                  PIC  X(04).
00347                  24  PI-690-LAST-PROCESSOR
00348                                  PIC  X(04).
00349                  24  PI-690-LAST-ARCHIVE-NO
00350                                  PIC S9(08) COMP.
00351              20  PI-690-LAST-ARCH-NDX
00352                                  PIC S9(04) COMP.
00353              20  PI-690-BRWS-TYPE-IND
00354                                  PIC  9(01).
00355                  88  PI-690-BRWS-CERTRP               VALUE 1.
00356                  88  PI-690-BRWS-FORM                 VALUE 2.
00357                  88  PI-690-BRWS-PROCESSOR            VALUE 3.
00358                  88  PI-690-BRWS-ACCOUNT              VALUE 4.
00359                  88  PI-690-BRWS-ENTRY-CNTL           VALUE 5.
00360                  88  PI-690-BRWS-ARCHIVE              VALUE 6.
00361              20  PI-690-LAST-BROWSE-IND
00362                                  PIC  X(01).
00363                  88  PI-690-LAST-BRWS-FWRD            VALUE '1'.
00364                  88  PI-690-LAST-BRWS-BWRD            VALUE '2'.
00365              20  PI-690-STATUS-SELECTION-IND
00366                                  PIC  X(01).
00367                  88  PI-690-SELECT-ALL       VALUE 'N'.
00368                  88  PI-690-VALID-SELECTION  VALUE 'A' 'C' 'H'
00369                                                'X' 'P' 'V' 'N'.
00370          16  FILLER              PIC X(67).
00371                                  EJECT
00372  01  FILLER                      PIC  X(19)
00373                                  VALUE ':END INTERFACE AREA'.
00374
00375  01  ERROR-MESSAGES.
00376      12  ER-0000                 PIC  X(04)  VALUE '0000'.
00377      12  ER-0004                 PIC  X(04)  VALUE '0004'.
00378      12  ER-0023                 PIC  X(04)  VALUE '0023'.
00379      12  ER-0029                 PIC  X(04)  VALUE '0029'.
00380      12  ER-0050                 PIC  X(04)  VALUE '0050'.
00381      12  ER-0068                 PIC  X(04)  VALUE '0068'.
00382      12  ER-0074                 PIC  X(04)  VALUE '0074'.
00383      12  ER-0085                 PIC  X(04)  VALUE '0085'.
00384      12  ER-0130                 PIC  X(04)  VALUE '0130'.
00385      12  ER-0131                 PIC  X(04)  VALUE '0131'.
00386      12  ER-0185                 PIC  X(04)  VALUE '0185'.
00387      12  ER-0190                 PIC  X(04)  VALUE '0190'.
00388      12  ER-0314                 PIC  X(04)  VALUE '0314'.
00389      12  ER-7008                 PIC  X(04)  VALUE '7008'.
00390      12  ER-7244                 PIC  X(04)  VALUE '7244'.
00391      12  ER-7357                 PIC  X(04)  VALUE '7357'.
00392      12  ER-7358                 PIC  X(04)  VALUE '7358'.
00393      12  ER-7359                 PIC  X(04)  VALUE '7359'.
00394      12  ER-7360                 PIC  X(04)  VALUE '7360'.
00395      12  ER-7361                 PIC  X(04)  VALUE '7361'.
00396      12  ER-7362                 PIC  X(04)  VALUE '7362'.
00397      12  ER-7366                 PIC  X(04)  VALUE '7366'.
00398      12  ER-7384                 PIC  X(04)  VALUE '7384'.
00399      12  ER-7385                 PIC  X(04)  VALUE '7385'.
00400      12  ER-7386                 PIC  X(04)  VALUE '7386'.
00401      12  ER-7387                 PIC  X(04)  VALUE '7387'.
00402      12  ER-7388                 PIC  X(04)  VALUE '7388'.
00403 *    12  ER-7394                 PIC  X(04)  VALUE '7394'.
00404      12  ER-7396                 PIC  X(04)  VALUE '7396'.
00405      12  ER-7397                 PIC  X(04)  VALUE '7397'.
00406      12  ER-7399                 PIC  X(04)  VALUE '7399'.
00407      12  ER-9007                 PIC  X(04)  VALUE '9007'.
00408      12  ER-9008                 PIC  X(04)  VALUE '9008'.
00409      12  ER-9009                 PIC  X(04)  VALUE '9009'.
00410      12  ER-9010                 PIC  X(04)  VALUE '9010'.
00411      12  ER-9011                 PIC  X(04)  VALUE '9011'.
00412      12  ER-9012                 PIC  X(04)  VALUE '9012'.
00413      12  ER-9015                 PIC  X(04)  VALUE '9015'.
00414      12  ER-9016                 PIC  X(04)  VALUE '9016'.
00415      12  ER-9017                 PIC  X(04)  VALUE '9017'.
00416      12  ER-9042                 PIC  X(04)  VALUE '9042'.
00417      12  ER-9080                 PIC  X(04)  VALUE '9080'.
00418      12  ER-9096                 PIC  X(04)  VALUE '9096'.
00419      12  ER-9097                 PIC  X(04)  VALUE '9097'.
00420      12  ER-9129                 PIC  X(04)  VALUE '9129'.
00421      12  ER-9150                 PIC  X(04)  VALUE '9150'.
00422      12  ER-9196                 PIC  X(04)  VALUE '9196'.
00423      12  ER-9245                 PIC  X(04)  VALUE '9245'.
00424      12  ER-9281                 PIC  X(04)  VALUE '9281'.
00425      12  ER-9282                 PIC  X(04)  VALUE '9282'.
00426      12  ER-9293                 PIC  X(04)  VALUE '9293'.
00427      12  ER-9308                 PIC  X(04)  VALUE '9308'.
00428      12  ER-9310                 PIC  X(04)  VALUE '9310'.
00429      12  ER-9312                 PIC  X(04)  VALUE '9312'.
00430      12  ER-9323                 PIC  X(04)  VALUE '9323'.
00431                                  EJECT
00432 *    COPY ELCAID REPLACING '01 DFHAID.' BY '01  DFHAID.'.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
051007*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
00007 ******************************************************************
00008
00009  01  DFHAID.
00010    02  DFHNULL   PIC  X  VALUE  ' '.
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.
00013    02  DFHPEN    PIC  X  VALUE  '='.
00014    02  DFHOPID   PIC  X  VALUE  'W'.
00015    02  DFHPA1    PIC  X  VALUE  '%'.
00016    02  DFHPA2    PIC  X  VALUE  '>'.
00017    02  DFHPA3    PIC  X  VALUE  ','.
00018    02  DFHPF1    PIC  X  VALUE  '1'.
00019    02  DFHPF2    PIC  X  VALUE  '2'.
00020    02  DFHPF3    PIC  X  VALUE  '3'.
00021    02  DFHPF4    PIC  X  VALUE  '4'.
00022    02  DFHPF5    PIC  X  VALUE  '5'.
00023    02  DFHPF6    PIC  X  VALUE  '6'.
00024    02  DFHPF7    PIC  X  VALUE  '7'.
00025    02  DFHPF8    PIC  X  VALUE  '8'.
00026    02  DFHPF9    PIC  X  VALUE  '9'.
00027    02  DFHPF10   PIC  X  VALUE  ':'.
00028    02  DFHPF11   PIC  X  VALUE  '#'.
00029    02  DFHPF12   PIC  X  VALUE  '@'.
00030    02  DFHPF13   PIC  X  VALUE  'A'.
00031    02  DFHPF14   PIC  X  VALUE  'B'.
00032    02  DFHPF15   PIC  X  VALUE  'C'.
00033    02  DFHPF16   PIC  X  VALUE  'D'.
00034    02  DFHPF17   PIC  X  VALUE  'E'.
00035    02  DFHPF18   PIC  X  VALUE  'F'.
00036    02  DFHPF19   PIC  X  VALUE  'G'.
00037    02  DFHPF20   PIC  X  VALUE  'H'.
00038    02  DFHPF21   PIC  X  VALUE  'I'.
051007*00039    02  DFHPF22   PIC  X  VALUE  'Õ'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00433  01  FILLER  REDEFINES  DFHAID.
00434      12  FILLER                  PIC  X(08).
00435      12  PF-VALUES               PIC  X(01)  OCCURS 2.
00436
00437                                  EJECT
00438 *    COPY ELCATTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.
00023      12  AL-PABOF            PIC X       VALUE 'Y'.
00024      12  AL-PABON            PIC X       VALUE 'Z'.
00025      12  AL-PADOF            PIC X       VALUE '%'.
00026      12  AL-PADON            PIC X       VALUE '_'.
00027      12  AL-PANOF            PIC X       VALUE '-'.
00028      12  AL-PANON            PIC X       VALUE '/'.
00029      12  AL-SABOF            PIC X       VALUE '8'.
00030      12  AL-SABON            PIC X       VALUE '9'.
00031      12  AL-SADOF            PIC X       VALUE '@'.
00032      12  AL-SADON            PIC X       VALUE QUOTE.
00033      12  AL-SANOF            PIC X       VALUE '0'.
00034      12  AL-SANON            PIC X       VALUE '1'.
00035      12  AL-UABOF            PIC X       VALUE 'H'.
00036      12  AL-UABON            PIC X       VALUE 'I'.
00037      12  AL-UADOF            PIC X       VALUE '<'.
00038      12  AL-UADON            PIC X       VALUE '('.
00039      12  AL-UANOF            PIC X       VALUE ' '.
00040      12  AL-UANON            PIC X       VALUE 'A'.
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.
00042      12  AL-UNBON            PIC X       VALUE 'R'.
00043      12  AL-UNDOF            PIC X       VALUE '*'.
00044      12  AL-UNDON            PIC X       VALUE ')'.
00045      12  AL-UNNOF            PIC X       VALUE '&'.
00046      12  AL-UNNON            PIC X       VALUE 'J'.
00439                                  EJECT
00440 *    COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
00441                                  EJECT
00442 *    COPY ELCEMIB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.
00020          88  EMI-NO-ERRORS                    VALUE '1'.
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.
00027          88  EMI-AREA1-FULL                   VALUE '2'.
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.
00030          88  EMI-AREA2-FULL                   VALUE '2'.
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.
00037      12  EMI-ERROR-LINES.
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.
00043                  24  EMI-ERR-NUM         PIC X(4).
00044                  24  EMI-FILLER          PIC X.
00045                  24  EMI-SEV             PIC X.
00046                  24  FILLER              PIC X.
00047              20  FILLER                  PIC X(02).
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
00050              20  EMI-ERROR-NUMBER    PIC X(4).
00051              20  EMI-FILL            PIC X.
00052              20  EMI-SEVERITY        PIC X.
00053              20  FILLER              PIC X.
00054              20  EMI-ERROR-TEXT.
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).
00056                  24  FILLER          PIC X(55).
00057      12  EMI-SEVERITY-SAVE           PIC X.
00058          88  EMI-NOTE                    VALUE 'N'.
00059          88  EMI-WARNING                 VALUE 'W'.
00060          88  EMI-FORCABLE                VALUE 'F'.
00061          88  EMI-FATAL                   VALUE 'X'.
00062      12  EMI-MESSAGE-FLAG            PIC X.
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
           12  emi-claim-no                pic x(7).
           12  emi-claim-type              pic x(6).
00070      12  FILLER                      PIC X(124)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00443                                  EJECT
00444 *    COPY ELCLOGOF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00011      12  LOGOFF-TEXT.
00012          16  FILLER          PIC X(5)    VALUE SPACES.
00013          16  LOGOFF-MSG.
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
00015              20  FILLER      PIC X       VALUE SPACES.
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
00017          16  FILLER          PIC X(80)
00018            VALUE '* YOU ARE NOW LOGGED OFF'.
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00020          16  FILLER          PIC X       VALUE QUOTE.
00021          16  LOGOFF-SYS-MSG  PIC X(17)
00022            VALUE 'S CLAS-IC SYSTEM '.
00023      12  TEXT-MESSAGES.
00024          16  UNACCESS-MSG    PIC X(29)
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
00026          16  PGMIDERR-MSG    PIC X(17)
00027              VALUE 'PROGRAM NOT FOUND'.
00445                                  EJECT
00446 *    COPY ELCSCTM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.
00010      12  FILLER                          PIC X(30)
00011             VALUE '** LOGIC SECURITY VIOLATION -'.
00012      12  SM-READ                         PIC X(6).
00013      12  FILLER                          PIC X(5)
00014             VALUE ' PGM='.
00015      12  SM-PGM                          PIC X(6).
00016      12  FILLER                          PIC X(5)
00017             VALUE ' OPR='.
00018      12  SM-PROCESSOR-ID                 PIC X(4).
00019      12  FILLER                          PIC X(6)
00020             VALUE ' TERM='.
00021      12  SM-TERMID                       PIC X(4).
00022      12  FILLER                          PIC XX   VALUE SPACE.
00023      12  SM-JUL-DATE                     PIC 9(5).
00024      12  FILLER                          PIC X    VALUE SPACE.
00025      12  SM-TIME                         PIC 99.99.
00026
00447                                  EJECT
00448 *    COPY ELCSCRTY.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
00013      12  FILLER                       PIC XX    VALUE 'SC'.
00014      12  SC-CREDIT-CODES.
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
00016              20  SC-CREDIT-DISPLAY    PIC X.
00017              20  SC-CREDIT-UPDATE     PIC X.
00018      12  SC-CLAIMS-CODES.
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
00020              20  SC-CLAIMS-DISPLAY    PIC X.
00021              20  SC-CLAIMS-UPDATE     PIC X.
00449                                  EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00451  01  DFHCOMMAREA                 PIC X(1024).
00452
00453 *01 PARMLIST .
00454 *    12  FILLER                  PIC S9(8)   COMP.
00455 *    12  L-ARCH-POINTER          PIC S9(8)   COMP.
00456 *    12  L-ARCT-POINTER          PIC S9(8)   COMP.
00457 *    12  L-CNTL-POINTER          PIC S9(8)   COMP.
00458
00459                                  EJECT
00460 *    COPY ERCARCH.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCARCH.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 250  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERARCH                        RKP=2,LEN=5     *
00013 *     ALTERNATE PATH1 = ERARCH2 (CERT/RESP)      RKP=07,LEN=35   *
00014 *     ALTERNATE PATH2 = ERARCH3 (FORM NUMBER)    RKP=44,LEN=28   *
00015 *     ALTERNATE PATH3 = ERARCH4 (PROCCESSOR ID)  RKP=73,LEN=28   *
00016 *     ALTERNATE PATH4 = ERARCH5 (ACCOUNT KEY)    RKP=100,LEN=24  *
00017 *     ALTERNATE PATH5 = ERARCH6 (BTCH/CHK KEY)   RKP=124,LEN=11  *
00018 *                                                                *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00020 ******************************************************************
031011*                   C H A N G E   L O G
031011*
031011* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031011*-----------------------------------------------------------------
031011*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031011* EFFECTIVE    NUMBER
031011*-----------------------------------------------------------------
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
070711* 070711  CR2011022800001  AJRA  NAPERSOFT CHANGES
110612* 110612    2012101700002  AJRA  ADD NEW FIELDS
102918* 102918  CR2018080300002  PEMA  ADD ONBASE STUFF
031011******************************************************************
00021  01  LETTER-ARCHIVE.
00022      12  LA-RECORD-ID                PIC  X(02).
00023          88  LA-VALID-ID                VALUE 'LA'.
00024
00025      12  LA-CONTROL-PRIMARY.
00026          16  LA-COMPANY-CD           PIC  X(01).
00027          16  LA-ARCHIVE-NO           PIC S9(08)    COMP.
00028
00029      12  LA-CONTROL-BY-CERT-RESP.
00030          16  LA-COMPANY-CD-A2        PIC  X(01).
00031          16  LA-CERT-NO-A2.
00032              20  LA-CERT-PRIME-A2    PIC  X(10).
00033              20  LA-CERT-SUFFIX-A2   PIC  X(01).
00034          16  LA-RSP-PERSON-A2 REDEFINES LA-CERT-NO-A2.
00035              20  LA-RESP-PERSON-A2   PIC  X(10).
00036              20  LA-TYPE-A2          PIC  X(01).
00037          16  LA-CARRIER-A2           PIC  X(01).
00038          16  LA-GROUPING-A2          PIC  X(06).
00039          16  LA-STATE-A2             PIC  X(02).
00040          16  LA-ACCOUNT-A2           PIC  X(10).
00041          16  LA-EFFECT-DATE-A2       PIC  X(02).
00042          16  LA-ARCHIVE-NO-A2        PIC S9(08)    COMP.
00043
00044      12  LA-CONTROL-BY-FORM.
00045          16  LA-COMPANY-CD-A3        PIC  X(01).
00046          16  LA-FORM-A3              PIC  X(04).
00047          16  LA-CARRIER-A3           PIC  X(01).
00048          16  LA-GROUPING-A3          PIC  X(06).
00049          16  LA-STATE-A3             PIC  X(02).
00050          16  LA-ACCOUNT-A3           PIC  X(10).
00051          16  LA-ARCHIVE-NO-A3        PIC S9(08)    COMP.
00052
00053      12  LA-CONTROL-BY-PROCESSOR.
00054          16  LA-COMPANY-CD-A4        PIC  X(01).
00055          16  LA-PROCESSOR-CD         PIC  X(04).
00056          16  LA-CARRIER-A4           PIC  X(01).
00057          16  LA-GROUPING-A4          PIC  X(06).
00058          16  LA-STATE-A4             PIC  X(02).
00059          16  LA-ACCOUNT-A4           PIC  X(10).
00060          16  LA-ARCHIVE-NO-A4        PIC S9(08)    COMP.
00061
00062      12  LA-CONTROL-BY-KEY-FIELDS.
00063          16  LA-COMPANY-CD-A5        PIC  X(01).
00064          16  LA-CARRIER-A5           PIC  X(01).
00065          16  LA-GROUPING-A5          PIC  X(06).
00066          16  LA-STATE-A5             PIC  X(02).
00067          16  LA-ACCOUNT-A5           PIC  X(10).
00068          16  LA-ARCHIVE-NO-A5        PIC S9(08)    COMP.
00069
00070      12  LA-CONTROL-BY-GROUP-CODE.
00071          16  LA-COMPANY-CD-A6        PIC  X(01).
00072          16  LA-ENTRY-A6.
00073              20  LA-FILLER           PIC  X(02).
00074              20  LA-QUE-CONTROL-A6   PIC S9(08)    COMP.
00075          16  LA-ARCHIVE-NO-A6        PIC S9(08)    COMP.
00076
00077      12  FILLER                      PIC  X(09).
00078
00079      12  LA-HEADER-RECORD.
00080          16  LA-NUMBER-LABEL-LINES   PIC S9(04)    COMP.
00081          16  LA-CREATION-DATE        PIC  X(02).
00082          16  LA-FOLLOW-UP-DATE       PIC  X(02).
070711         16  LA-FINAL-ACT-DATE       REDEFINES
070711               LA-FOLLOW-UP-DATE     PIC  X(02).
00083          16  LA-INITIAL-PRINT-DATE   PIC  X(02).
00084          16  LA-NO-OF-COPIES         PIC S9(01).
00085          16  LA-NO-OF-TEXT-RECORDS   PIC S9(04)    COMP.
00086          16  LA-REPLY-DATE           PIC  X(02).
00087          16  LA-RESEND-DATES.
00090              20  LA-RESEND-DATE      PIC  X(02).
00091              20  LA-SENT-DATE        PIC  X(02).
                   20  FILLER              PIC  X(08).
00099          16  LA-SOURCE-INFORMATION.
00100              20  LA-DATA-SOURCE      PIC  X(01).
00101              20  LA-ADDR-SOURCE      PIC  X(01).
00102          16  LA-STATUS               PIC  X(01).
00103              88  LA-STATUS-ACTIVE         VALUE 'A'.
00104              88  LA-STATUS-COMPLETED      VALUE 'C'.
00105              88  LA-STATUS-ON-HOLD        VALUE 'H'.
00106              88  LA-STATUS-TO-BE-PURGED   VALUE 'X'.
00107              88  LA-STATUS-PURGED         VALUE 'P'.
00108              88  LA-STATUS-VOIDED         VALUE 'V'.
00109          16  LA-LAST-RESENT-PRINT-DATE
00110                                      PIC  X(02).
00111          16  LA-PRINT-RESTRICTION    PIC  X(01).
00112              88  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
00113                                           VALUE 'C'.
00114              88  LA-PRINT-ONLY-WHEN-FORM-GIVEN
00115                                           VALUE 'F'.
00116              88  LA-PRINT-ONLY-WHEN-PROC-GIVEN
00117                                           VALUE 'P'.
00118          16  LA-PURGED-DATE          PIC  X(02).
00119          16  LA-VOIDED-DATE          PIC  X(02).
101705         16  LA-RESEND-LETR          PIC  X(4).
102918         16  LA-VOID-ONBASE-YN       PIC  X.
102918         16  LA-ONBASE-UNIQUE-ID     PIC S9(5) COMP-3.
102918         16  FILLER                  PIC  X(04).
101705*        16  LA-RESEND-LETR-2        PIC  X(4).
101705*        16  LA-RESEND-LETR-3        PIC  X(4).
070711*        16  FILLER                  PIC  X(59).
               16  LA-ARCHIVE-STATUS       PIC  X.
                   88  LA-TEMP                VALUE 'T'.
                   88  LA-QWS                 VALUE 'Q'.
                   88  LA-BATCH               VALUE 'B'.
070711         16  LA-FINAL-ACT-IND        PIC  X(1).
070711         16  LA-VA-DISCLOSURE-IND    PIC  X(1).
110612         16  LA-ENDT-ARCH-NO         PIC S9(8) COMP.
110612         16  LA-ENDT-ARCH-NO-X REDEFINES LA-ENDT-ARCH-NO
110612                                     PIC X(4).
110612         16  FILLER                  PIC  X(42).
00120 *        16  FILLER                  PIC  X(71).
070711         16  LA-LAST-MAINT-DATE      PIC  X(2).
070711         16  LA-LAST-MAINT-TIME      PIC S9(6) COMP-3.
070711         16  LA-LAST-MAINT-TIMEX  REDEFINES LA-LAST-MAINT-TIME
070711                                     PIC  X(4).
070711         16  LA-LAST-UPDATED-BY      PIC  X(4).
00121
00122 ******************************************************************
00461                                  EJECT
00462 *    COPY ERCARCT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCARCT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEXT OF ARCHIVED LETTERDS                 *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 1640  RECFORM = FIXED                          *
00011 *                                                                *
00012 *   BASE CLUSTER = ERARCT                        RKP=2,LEN=8     *
00013 *                                                                *
00014 *   LOG = NO                                                     *
00015 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00016 ******************************************************************
070711*                   C H A N G E   L O G
070711*
070711* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
070711*-----------------------------------------------------------------
070711*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
070711* EFFECTIVE    NUMBER
070711*-----------------------------------------------------------------
070711* 070711  CR2011022800001  AJRA  NAPERSOFT CHANGES
070711******************************************************************
00017  01  LETTER-ARCHIVE-TEXT.
00018      12  LT-RECORD-ID                PIC  X(02).
070711         88  LT-VALID-ID                VALUE 'LT'.
00020
00021      12  LT-CONTROL-PRIMARY.
00022          16  LT-COMPANY-CD           PIC  X(01).
00023          16  LT-ARCHIVE-NO           PIC S9(08)    COMP.
00024          16  LT-RECORD-TYPE          PIC  X(01).
00025              88  LT-ADDRESS-DATA        VALUE '1'.
00026              88  LT-TEXT-DATA           VALUE '2'.
070711             88  LT-COMMENT-DATA        VALUE '3'.
00027          16  LT-LINE-SEQ-NO          PIC S9(04)    COMP.
00028
00029      12  FILLER                      PIC  X(28).
00030      12  LT-NUM-LINES-ON-RECORD      PIC S9(04)    COMP.
00031
00032      12  LT-TEXT-RECORD.
00033          16  LT-LETTER-TEXT OCCURS 20 TIMES
00034                             INDEXED BY LT-NDX.
00035              20  LT-TEXT-LINE        PIC  X(70).
00036              20  LT-SKIP-CONTROL     PIC  X(02).
00037                  88  LT-NO-LINES-SKIPPED             VALUE SPACES.
00038                  88  LT-SKIP-TO-NEXT-PAGE            VALUE '99'.
00039              20  FILLER              PIC  X(08).
070711
070711     12  LT-COMMENT-RECORD  REDEFINES LT-TEXT-RECORD.
070711         16  LT-LETTER-COMMENT OCCURS 20 TIMES INDEXED BY LC-NDX.
070711             20  LT-COMMENT-LINE     PIC X(69).
070711             20  LT-COMMENT-CHG-DT   PIC X(02).
070711             20  LT-COMMENT-CHG-BY   PIC X(04).
070711             20  FILLER              PIC X(05).
00040
00463                                  EJECT
00464 *    COPY ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
00465
00466                                  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA LETTER-ARCHIVE
                                LETTER-ARCHIVE-TEXT CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL690' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00468
00469      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00470
00471      
      * EXEC CICS HANDLE CONDITION
00472 *        QIDERR    (0110-PGM-INITIALIZATION)
00473 *        MAPFAIL   (0110-PGM-INITIALIZATION)
00474 *        NOTOPEN   (8070-NOTOPEN)
00475 *        NOTFND    (8080-ARCH-NOT-FOUND)
00476 *        PGMIDERR  (9700-PGMID-ERROR)
00477 *        ERROR     (9800-ABEND)
00478 *        END-EXEC.
      *    MOVE '"$N?JIL.              ! " #00004208' TO DFHEIV0
           MOVE X'22244E3F4A494C2E20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034323038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00479
00480      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00481      MOVE '5'                    TO DC-OPTION-CODE.
00482      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
00483      MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-DATE.
00484      MOVE DC-BIN-DATE-1          TO W-SAVE-BIN-DATE.
00485
00486      MOVE +1                     TO EMI-NUMBER-OF-LINES.
00487      MOVE '2'                    TO EMI-SWITCH2.
00488
00489 ***************************************************************
00490 *       IF ATTEMPTING TO EXECUTE PROGRAM WITHOUT SIGNING ON   *
00491 *       (COMM LENGTH EQUAL ZERO), SEND ERROR MESSAGE.         *
00492 ***************************************************************
00493
00494      IF  EIBCALEN EQUAL 0
00495          GO TO 8000-UNAUTHORIZED-ACCESS.
00496
00497      IF  PI-RETURN-TO-PROGRAM EQUAL W-THIS-PGM
00498          MOVE PI-CALLING-PROGRAM TO W-RETURNED-FROM
00499
00500      ELSE
00501          MOVE SPACES             TO W-RETURNED-FROM.
00502
00503      IF  PI-CALLING-PROGRAM NOT EQUAL W-THIS-PGM
00504
00505          IF  PI-RETURN-TO-PROGRAM NOT EQUAL W-THIS-PGM
00506              MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-6
00507              MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-5
00508              MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-4
00509              MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-3
00510              MOVE PI-SAVED-PROGRAM-1    TO PI-SAVED-PROGRAM-2
00511              MOVE PI-RETURN-TO-PROGRAM  TO PI-SAVED-PROGRAM-1
00512              MOVE PI-CALLING-PROGRAM    TO PI-RETURN-TO-PROGRAM
00513              MOVE W-THIS-PGM            TO PI-CALLING-PROGRAM
00514              MOVE LOW-VALUES            TO PI-690-WORK-AREA
00515              MOVE HIGH-VALUES           TO PI-690-FIRST-DATA
PEMUNI             MOVE ZEROS                 TO PI-690-BRWS-TYPE-IND
00516
00517              IF  PI-RETURN-TO-PROGRAM EQUAL W-XCTL-626
00518                  MOVE LOW-VALUES        TO PI-CARRIER
00519                                            PI-GROUPING
00520                                            PI-STATE
00521                                            PI-ACCOUNT
00522                                            PI-CERT-EFF-DT
00523                                            PI-CERT-PRIME
00524                                            PI-CERT-SFX
00525                                            PI-689-WORK-AREA
00526
00527              ELSE
00528                  PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT
00529                  GO TO 1200-SET-UP-COMPLETE-SCREEN
00530
00531          ELSE
00532              MOVE PI-RETURN-TO-PROGRAM  TO PI-CALLING-PROGRAM
00533              MOVE PI-SAVED-PROGRAM-1    TO PI-RETURN-TO-PROGRAM
00534              MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-1
00535              MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-2
00536              MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-3
00537              MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-4
00538              MOVE PI-SAVED-PROGRAM-6    TO PI-SAVED-PROGRAM-5
00539              MOVE SPACES                TO PI-SAVED-PROGRAM-6.
00540                                  EJECT
00541  0100-ENTRY-LOGIC.
00542
00543      IF  EIBTRNID EQUAL W-TRANSACTION
00544
00545          IF  EIBAID EQUAL DFHCLEAR
00546                  OR
00547              NOT DISPLAY-CAP
00548              MOVE PI-RETURN-TO-PROGRAM
00549                                  TO W-CALL-PGM
00550              PERFORM 9400-XCTL THRU 9400-EXIT
00551
00552          ELSE
00553              GO TO 0200-RECEIVE
00554
00555      ELSE
00556          IF  W-RETURNED-FROM NOT EQUAL SPACES
00557              GO TO 1000-RECOVER-LAST-SCREEN.
00558
00559      MOVE LOW-VALUES             TO EL690AO.
00560
00561  0110-PGM-INITIALIZATION.
00562
00563      PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT.
00564
00565      MOVE LOW-VALUES             TO EL690AO.
00566      MOVE -1                     TO TYPEBRL.
00567      GO TO 8100-SEND-INITIAL-MAP.
00568                                  EJECT
00569  0200-RECEIVE.
00570
00571      IF  EIBAID EQUAL DFHPA1
00572              OR
00573          EIBAID EQUAL DFHPA2
00574              OR
00575          EIBAID EQUAL DFHPA3
00576          MOVE ER-7008            TO EMI-ERROR
00577          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00578          MOVE -1                 TO PFKEYL
00579          GO TO 8200-SEND-DATAONLY.
00580
00581      
      * EXEC CICS RECEIVE
00582 *        MAP     (W-MAP)
00583 *        MAPSET  (W-MAPSET)
00584 *        INTO    (EL690AI)
00585 *        END-EXEC.
           MOVE LENGTH OF
            EL690AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004319' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL690AI, 
                 DFHEIV11, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00586
00587      IF  PFKEYL EQUAL ZERO
00588          GO TO 0300-CHECK-PFKEYS.
00589
00590      IF  EIBAID NOT EQUAL DFHENTER
00591          MOVE ER-0004            TO EMI-ERROR
00592          GO TO 0310-INPUT-ERROR.
00593
00594      IF  PFKEYI GREATER 0 AND LESS 25
00595          MOVE PF-VALUES (PFKEYI) TO EIBAID
00596
00597      ELSE
00598          MOVE ER-0029            TO EMI-ERROR
00599          GO TO 0310-INPUT-ERROR.
00600
00601                                  EJECT
00602  0300-CHECK-PFKEYS.
00603
00604      IF  EIBAID EQUAL DFHPF23
00605          MOVE EIBAID             TO PI-ENTRY-CD-1
00606          MOVE W-XCTL-005         TO W-CALL-PGM
00607          PERFORM 9400-XCTL THRU 9400-EXIT.
00608
00609      IF  EIBAID EQUAL DFHPF24
00610          MOVE W-XCTL-626         TO W-CALL-PGM
00611          PERFORM 9400-XCTL THRU 9400-EXIT.
00612
00613      IF  EIBAID EQUAL DFHPF12
00614          MOVE W-XCTL-010         TO W-CALL-PGM
00615          PERFORM 9400-XCTL THRU 9400-EXIT.
00616
00617      IF  MAINTI EQUAL 'C'
00618              AND
00619          EIBAID EQUAL DFHENTER
00620              AND
00621          CORRSELL GREATER THAN ZEROS
00622          MOVE -1                 TO CORRSELL
00623          MOVE AL-UNBON           TO CORRSELA
00624                                     MAINTA
00625          MOVE ER-9323            TO EMI-ERROR
00626          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00627          GO TO 8200-SEND-DATAONLY.
00628
00629      IF  EIBAID EQUAL DFHPF3
012312*            OR
012312*        (EIBAID EQUAL DFHENTER
012312*            AND
012312*        CORRSELL GREATER THAN ZEROS)
00634          MOVE '3'                TO EIBAID
00635          PERFORM 1500-SELECT-ARCHIVE-REC THRU 1500-EXIT
00636          MOVE W-XCTL-689         TO W-CALL-PGM
00637          GO TO 9400-XCTL.
00638
00639      IF  EIBAID EQUAL DFHPF4
00640          MOVE LOW-VALUES         TO EL690AO
00641                                     PI-690-WORK-AREA
00642          MOVE -1                 TO TYPEBRL
00643          GO TO 8100-SEND-INITIAL-MAP.
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
00644
00645      IF  MAINTI EQUAL 'C'
00646              AND
00647          EIBAID EQUAL DFHENTER
00648          GO TO 0700-PROCESS-CHANGES.
00649
00650      IF  EIBAID EQUAL DFHPF1
00651          PERFORM 0500-EDIT-SCREEN-DATA THRU 0500-EXIT
00652          MOVE LOW-VALUES         TO PI-690-FIRST-DATA
00653                                     W-ARCH-GROUPS
00654          GO TO 2000-PAGE-FORWARD.
00655
00656      IF  EIBAID EQUAL DFHPF2
00657
00658          IF  PI-690-INIT-DATA EQUAL LOW-VALUES
00659              MOVE HIGH-VALUES    TO PI-690-INIT-DATA
00660              PERFORM 0500-EDIT-SCREEN-DATA THRU 0500-EXIT
00661              MOVE HIGH-VALUES    TO PI-690-LAST-DATA
00662              MOVE LOW-VALUES     TO W-ARCH-GROUPS
00663              GO TO 3000-PAGE-BACKWARD
00664
00665          ELSE
00666              PERFORM 0500-EDIT-SCREEN-DATA THRU 0500-EXIT
00667              MOVE HIGH-VALUES    TO PI-690-LAST-DATA
00668              MOVE LOW-VALUES     TO W-ARCH-GROUPS
00669              GO TO 3000-PAGE-BACKWARD.
00670
00671      MOVE ER-0029                TO EMI-ERROR.
00672
00673  0310-INPUT-ERROR.
00674
00675      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00676      MOVE -1                     TO PFKEYL
00677      MOVE AL-UNBON               TO PFKEYA
00678      GO TO 8200-SEND-DATAONLY.
00679                                  EJECT
00680  0500-EDIT-SCREEN-DATA.
00681
00682      IF  TYPEBRL NOT EQUAL ZEROS
00683
00684          IF  TYPEBRI NOT EQUAL PI-690-BRWS-TYPE-IND
00685              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND
00686              MOVE TYPEBRI        TO PI-690-BRWS-TYPE-IND
00687              MOVE AL-UNNON       TO TYPEBRA
00688
00689          ELSE
00690              NEXT SENTENCE
00691
00692      ELSE
00693          IF  PI-690-BRWS-TYPE-IND NOT NUMERIC
00694                  OR
00695              PI-690-BRWS-TYPE-IND LESS THAN 1
00696                  OR
00697              PI-690-BRWS-TYPE-IND GREATER THAN 6
00698              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND
00699              MOVE 6              TO PI-690-BRWS-TYPE-IND
00700              MOVE +1             TO TYPEBRL
00701              MOVE AL-UNNON       TO TYPEBRA
00702
00703          ELSE
00704              MOVE +1             TO TYPEBRL
00705              MOVE AL-UNNON       TO TYPEBRA
00706              MOVE PI-690-BRWS-TYPE-IND
00707                                  TO TYPEBRO.
00708
00709      IF  CERTRPL GREATER THAN ZEROS
00710
00711          IF  CERTRPI NOT EQUAL PI-690-INIT-CERT-PRIME
00712              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND
00713              MOVE CERTRPI        TO PI-690-INIT-CERT-PRIME
00714              MOVE AL-UANON       TO CERTRPA
00715
00716              IF  SUFFIXL GREATER THAN ZEROS
00717
00718                  IF  SUFFIXI NOT EQUAL PI-690-INIT-SUFFIX
00719                      MOVE 'Y'    TO W-CHANGE-REQUESTED-IND
00720
00721                      IF  SUFFIXI NOT GREATER THAN SPACES
00722                          MOVE SPACES
00723                                  TO SUFFIXI
00724                                     PI-690-INIT-SUFFIX
00725                          MOVE AL-UANON
00726                                  TO SUFFIXA
00727
00728                      ELSE
00729                          MOVE AL-UANON
00730                                  TO SUFFIXA
00731                          MOVE SUFFIXI
00732                                  TO PI-690-INIT-SUFFIX
00733
00734                  ELSE
00735                      NEXT SENTENCE
00736
00737              ELSE
00738                  IF  PI-690-INIT-SUFFIX EQUAL LOW-VALUES
00739                          OR
00740                      PI-690-INIT-SUFFIX EQUAL HIGH-VALUES
00741                      MOVE AL-UANON
00742                                  TO SUFFIXA
00743                      MOVE SPACES TO PI-690-INIT-SUFFIX
00744
00745                  ELSE
00746                      NEXT SENTENCE
00747
00748          ELSE
00749              NEXT SENTENCE
00750
00751      ELSE
00752          IF  PI-690-INIT-CERT-PRIME GREATER THAN LOW-VALUES
00753                  AND
00754              PI-690-INIT-CERT-PRIME LESS THAN HIGH-VALUES
00755              MOVE PI-690-INIT-CERT-PRIME
00756                                  TO CERTRPI
00757              MOVE PI-690-INIT-SUFFIX
00758                                  TO SUFFIXI
00759              MOVE AL-UANON       TO CERTRPA
00760                                     SUFFIXA
00761              MOVE +10            TO CERTRPL
00762              MOVE +1             TO SUFFIXL.
00763
00764      IF  FORML GREATER THAN ZEROS
00765
00766          IF  FORMI NOT EQUAL PI-690-INIT-FORM
00767              MOVE AL-UANON       TO FORMA
00768              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND
00769              MOVE FORMI          TO PI-690-INIT-FORM
00770
00771          ELSE
00772              NEXT SENTENCE
00773
00774      ELSE
00775          IF  PI-690-INIT-FORM GREATER THAN LOW-VALUES
00776                  AND
00777              PI-690-INIT-FORM LESS THAN HIGH-VALUES
00778              MOVE AL-UANON       TO FORMA
00779              MOVE +4             TO FORML
00780              MOVE PI-690-INIT-FORM
00781                                  TO FORMI.
00782
00783      IF  ENTRYL GREATER THAN ZEROS
00784              AND
00785          CKCNTLL GREATER THAN ZEROS
00786          MOVE -1                 TO CKCNTLL
00787          MOVE AL-UABON           TO CKCNTLA
00788          MOVE ER-7362            TO EMI-ERROR
00789          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00790          GO TO 8100-SEND-INITIAL-MAP.
00791
00792      IF  ENTRYL GREATER THAN ZEROS
00793
00794          IF  ENTRYI NOT EQUAL PI-690-INIT-ENTRY
00795              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND
00796              MOVE ENTRYI         TO PI-690-INIT-ENTRY
00797
00798          ELSE
00799              NEXT SENTENCE
00800
00801      ELSE
00802          IF  PI-690-INIT-ENTRY GREATER THAN LOW-VALUES
00803                  AND
00804              PI-690-INIT-ENTRY LESS THAN HIGH-VALUES
00805                  AND
00806              PI-690-INIT-CONTROL-PREFIX NOT EQUAL 'CK'
00807              MOVE AL-UANON       TO ENTRYA
00808              MOVE +6             TO ENTRYL
00809              MOVE PI-690-INIT-ENTRY
00810                                  TO ENTRYI.
00811
00812      IF  CKCNTLL GREATER THAN ZEROS
00813
00814          IF  CKCNTLI NUMERIC
00815
00816              IF  CKCNTLI NOT EQUAL PI-690-INIT-CONTROL
00817                  MOVE 'Y'        TO W-CHANGE-REQUESTED-IND
00818                  MOVE 'CK'       TO PI-690-INIT-CONTROL-PREFIX
00819                  MOVE CKCNTLI    TO PI-690-INIT-CONTROL
00820
00821              ELSE
00822                  NEXT SENTENCE
00823
00824          ELSE
00825              MOVE ZEROS          TO PI-690-INIT-CONTROL
00826                                     CKCNTLO
00827
00828      ELSE
00829          IF  PI-690-INIT-CONTROL GREATER THAN ZEROS
00830                  AND
00831              PI-690-INIT-CONTROL LESS THAN 99999999
00832                  AND
00833              PI-690-INIT-CONTROL-PREFIX EQUAL 'CK'
00834              MOVE AL-UANON       TO CKCNTLA
00835              MOVE +8             TO CKCNTLL
00836              MOVE PI-690-INIT-CONTROL
00837                                  TO CKCNTLI.
00838
00839      IF  CARRIERL GREATER THAN ZEROS
00840
00841          IF  CARRIERI NOT EQUAL  TO PI-690-INIT-CARRIER
00842              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND
00843              MOVE CARRIERI       TO PI-690-INIT-CARRIER
00844
00845          ELSE
00846              NEXT SENTENCE
00847
00848      ELSE
00849          IF  PI-690-INIT-CARRIER GREATER THAN LOW-VALUES
00850                  AND
00851              PI-690-INIT-CARRIER LESS THAN HIGH-VALUES
00852              MOVE AL-UANON       TO CARRIERA
00853              MOVE +1             TO CARRIERL
00854              MOVE PI-690-INIT-CARRIER
00855                                  TO CARRIERI.
00856
00857      IF  GROUPL GREATER THAN ZEROS
00858
00859          IF  GROUPI NOT EQUAL    TO PI-690-INIT-GROUPING
00860              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND
00861              MOVE GROUPI         TO PI-690-INIT-GROUPING
00862
00863          ELSE
00864              NEXT SENTENCE
00865
00866      ELSE
00867          IF  PI-690-INIT-GROUPING GREATER THAN LOW-VALUES
00868                  AND
00869              PI-690-INIT-GROUPING LESS THAN HIGH-VALUES
00870              MOVE AL-UANON       TO GROUPA
00871              MOVE +6             TO GROUPL
00872              MOVE PI-690-INIT-GROUPING
00873                                  TO GROUPI.
00874
00875      IF  STATEL GREATER THAN ZEROS
00876
00877          IF  STATEI NOT EQUAL    TO PI-690-INIT-STATE
00878              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND
00879              MOVE STATEI         TO PI-690-INIT-STATE
00880
00881          ELSE
00882              NEXT SENTENCE
00883
00884      ELSE
00885          IF  PI-690-INIT-STATE GREATER THAN LOW-VALUES
00886                  AND
00887              PI-690-INIT-STATE LESS THAN HIGH-VALUES
00888              MOVE AL-UANON       TO STATEA
00889              MOVE +2             TO STATEL
00890              MOVE PI-690-INIT-STATE
00891                                  TO STATEI.
00892
00893      IF  ACCTL GREATER THAN ZEROS
00894
00895          IF  ACCTI NOT EQUAL     TO PI-690-INIT-ACCOUNT
00896              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND
00897              MOVE ACCTI          TO PI-690-INIT-ACCOUNT
00898
00899          ELSE
00900              NEXT SENTENCE
00901
00902      ELSE
00903          IF  PI-690-INIT-ACCOUNT GREATER THAN LOW-VALUES
00904                  AND
00905              PI-690-INIT-ACCOUNT LESS THAN HIGH-VALUES
00906              MOVE AL-UANON       TO ACCTA
00907              MOVE +10            TO ACCTL
00908              MOVE PI-690-INIT-ACCOUNT
00909                                  TO ACCTI.
00910
00911      IF  PROCSRL GREATER THAN ZEROS
00912
00913          IF  PROCSRI NOT EQUAL   TO PI-690-INIT-PROCESSOR
00914              MOVE 'Y'            TO W-CHANGE-REQUESTED-IND
00915              MOVE PROCSRI        TO PI-690-INIT-PROCESSOR
00916
00917          ELSE
00918              NEXT SENTENCE
00919
00920      ELSE
00921          IF  PI-690-INIT-PROCESSOR GREATER THAN LOW-VALUES
00922                  AND
00923              PI-690-INIT-PROCESSOR LESS THAN HIGH-VALUES
00924              MOVE AL-UANON       TO PROCSRA
00925              MOVE +4             TO PROCSRL
00926              MOVE PI-690-INIT-PROCESSOR
00927                                  TO PROCSRI.
00928
00929      IF  EFFDTEL GREATER THAN ZEROS
00930          MOVE EFFDTEI            TO W-DEEDIT-FIELD
00931
00932          PERFORM 9200-DATE-EDIT THRU 9200-EXIT
00933
00934          IF  DC-ERROR-CODE NOT EQUAL SPACES
00935              MOVE -1             TO EFFDTEL
00936              MOVE AL-UABON       TO EFFDTEA
00937              MOVE ER-0314        TO EMI-ERROR
00938              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00939              GO TO 8100-SEND-INITIAL-MAP
00940
00941          ELSE
00942              IF  DC-BIN-DATE-1 NOT = PI-690-INIT-EFFECT-DATE
00943                  MOVE 'Y'        TO W-CHANGE-REQUESTED-IND
00944                  MOVE DC-BIN-DATE-1
00945                                  TO PI-690-INIT-EFFECT-DATE
00946                  MOVE DC-GREG-DATE-1-EDIT
00947                                  TO PI-690-INIT-EFF-DTE
00948
00949              ELSE
00950                  IF  PI-690-INIT-EFFECT-DATE > LOW-VALUES
00952                          AND
00953                      PI-690-INIT-EFFECT-DATE < HIGH-VALUES
00954                      MOVE AL-UANON
00955                                  TO EFFDTEA
00956                      MOVE +8     TO EFFDTEL
00957                      MOVE PI-690-INIT-EFF-DTE
00958                                  TO EFFDTEI.
00959
00960      IF  STATSELL GREATER THAN ZEROS
00961          MOVE STATSELI           TO PI-690-STATUS-SELECTION-IND
00962
00963          IF  NOT PI-690-VALID-SELECTION
00964              MOVE -1             TO STATSELL
00965              MOVE AL-UABON       TO STATSELA
00966              MOVE ER-7244        TO EMI-ERROR
00967              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00968              GO TO 8100-SEND-INITIAL-MAP
00969
00970          ELSE
00971              NEXT SENTENCE
00972
00973      ELSE
00974          IF  PI-690-VALID-SELECTION
00975              MOVE AL-UANON       TO STATSELA
00976              MOVE +1             TO STATSELL
00977              MOVE PI-690-STATUS-SELECTION-IND
00978                                  TO STATSELI
00979
00980          ELSE
00981              MOVE AL-UANON       TO STATSELA
00982              MOVE +1             TO STATSELL
00983              MOVE 'N'            TO PI-690-STATUS-SELECTION-IND
00984                                     STATSELI.
00985
00986  0500-EXIT.
00987      EXIT.
00988                                  EJECT
00989  0700-PROCESS-CHANGES.
00990
00991      IF  NOT MODIFY-CAP
00992          MOVE 'UPDATE'           TO SM-READ
00993          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00994          MOVE ER-9096            TO EMI-ERROR
00995          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00996          MOVE AL-UANON           TO MAINTA
00997          MOVE -1                 TO MAINTL
00998          GO TO 8200-SEND-DATAONLY.
00999
01000      
      * EXEC CICS HANDLE CONDITION
01001 *        NOTFND  (8080-ARCH-NOT-FOUND)
01002 *        ENDFILE (8080-ARCH-NOT-FOUND)
01003 *        END-EXEC.
      *    MOVE '"$I''                  ! # #00004784' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034373834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01004
01005      MOVE ZEROS                  TO W-FIRST-CHANGE-IND.
01006
01007      PERFORM 0720-SEARCH-FOR-CHANGES THRU 0720-EXIT
01008              VARYING
01009          W-ARCH-NDX FROM 1 BY 1
01010              UNTIL
01011          W-ARCH-NDX GREATER THAN 12
01012              OR
01013          PI-690-ARCHIVE-NUM (W-ARCH-NDX) EQUAL ZEROS.
01014
01015      IF  NOT EMI-NO-ERRORS
01016          GO TO 8200-SEND-DATAONLY.
01017
01018      MOVE SPACES                 TO MAINTO
01019      MOVE -1                     TO MAINTL
01020      MOVE ER-9308                TO EMI-ERROR
01021      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01022
01023      MOVE LOW-VALUES             TO EL690AO.
01024
01025      IF  PI-690-LAST-BRWS-FWRD
01026          MOVE '1'                TO EIBAID
01027          GO TO 2000-PAGE-FORWARD.
01028
01029      IF  PI-690-LAST-BRWS-BWRD
01030          MOVE '2'                TO EIBAID
01031          GO TO 3000-PAGE-BACKWARD.
01032
01033  0700-EXIT.
01034      EXIT.
01035
01036  0720-SEARCH-FOR-CHANGES.
01037
01038      MOVE SPACES                 TO W-REPLY-DATE
01039                                     W-RESEND-DATE-1
01040                                     W-RESEND-DATE-2
01041                                     W-RESEND-DATE-3
01042                                     W-STATUS
01043                                     W-VALID-STATUS-SW.
01044
070711*    IF  W-STATSL (W-ARCH-NDX) GREATER THAN ZEROS
070711*        MOVE W-STATSO (W-ARCH-NDX)
070711*                                TO W-VALID-STATUS-SW
070711*
070711*        IF  W-ALLOWED-ENTRIES
070711*            MOVE W-STATSO (W-ARCH-NDX)
070711*                                TO W-STATUS
070711*
070711*        ELSE
070711*            MOVE ER-7358        TO EMI-ERROR
070711*            MOVE AL-UANON       TO W-STATSA (W-ARCH-NDX)
070711*            MOVE -1             TO W-STATSL (W-ARCH-NDX)
070711*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01058
070711     IF  W-RSDATL (W-ARCH-NDX) GREATER THAN ZEROS
01060
070711         IF  W-RSDATO (W-ARCH-NDX) EQUAL SPACES
01062                  OR
070711             W-RSDATO (W-ARCH-NDX) EQUAL ZEROES
01064              MOVE LOW-VALUES     TO W-RESEND-DATE-1
01065
01066          ELSE
070711             MOVE W-RSDATO (W-ARCH-NDX)
01068                                  TO W-DEEDIT-FIELD
01069
01070              PERFORM 9200-DATE-EDIT THRU 9200-EXIT
01071
01072              IF  DC-ERROR-CODE EQUAL SPACES
01073                  MOVE DC-BIN-DATE-1
01074                                  TO W-RESEND-DATE-1
01075                  MOVE DC-GREG-DATE-1-EDIT
070711                                 TO W-RSDATO (W-ARCH-NDX)
01077
01078              ELSE
01079                  MOVE ER-0185    TO EMI-ERROR
070711                 MOVE AL-UANON   TO W-RSDATA (W-ARCH-NDX)
070711                 MOVE -1         TO W-RSDATL (W-ARCH-NDX)
01082                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01083
070711*    IF  W-RSFRML (W-ARCH-NDX) GREATER THAN ZEROS
01085 *
070711*        IF  W-RSFRMO (W-ARCH-NDX) EQUAL SPACES
01087 *                OR
070711*            W-RSFRMO (W-ARCH-NDX) EQUAL ZEROES
01089 *            MOVE LOW-VALUES     TO W-RESEND-DATE-2
01090 *
01091 *        ELSE
070711*            MOVE W-RSFRMO (W-ARCH-NDX)
01093 *                                TO W-DEEDIT-FIELD
01094 *
01095 *            PERFORM 9200-DATE-EDIT THRU 9200-EXIT
01096 *
01097 *            IF  DC-ERROR-CODE EQUAL SPACES
01098 *                MOVE DC-BIN-DATE-1
01099 *                                TO W-RESEND-DATE-2
01100 *                MOVE DC-GREG-DATE-1-EDIT
01101 *                                TO W-RSFRMO (W-ARCH-NDX)
01102 *
01103 *            ELSE
01104 *                MOVE ER-0185    TO EMI-ERROR
01105 *                MOVE AL-UANON   TO W-RSFRMA (W-ARCH-NDX)
01106 *                MOVE -1         TO W-RSFRML (W-ARCH-NDX)
01107 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01108 *
01109 *    IF  W-RSSTSL (W-ARCH-NDX) GREATER THAN ZEROS
01110 *
01111 *        IF  W-RSSTSO (W-ARCH-NDX) EQUAL SPACES
01112 *                OR
01113 *            W-RSSTSO (W-ARCH-NDX) EQUAL ZEROES
01114 *            MOVE LOW-VALUES     TO W-RESEND-DATE-3
01115 *
01116 *        ELSE
01117 *            MOVE W-RSSTSO (W-ARCH-NDX)
01118 *                                TO W-DEEDIT-FIELD
01119 *
01120 *            PERFORM 9200-DATE-EDIT THRU 9200-EXIT
01121 *
01122 *            IF  DC-ERROR-CODE EQUAL SPACES
01123 *                MOVE DC-BIN-DATE-1
01124 *                                TO W-RESEND-DATE-3
01125 *                MOVE DC-GREG-DATE-1-EDIT
01126 *                                TO W-RSSTSO (W-ARCH-NDX)
01127 *
01128 *            ELSE
01129 *                MOVE ER-0185    TO EMI-ERROR
01130 *                MOVE AL-UANON   TO W-RSSTSA (W-ARCH-NDX)
01131 *                MOVE -1         TO W-RSSTSL (W-ARCH-NDX)
01132 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01133
070711*    IF  W-REPLYL (W-ARCH-NDX) GREATER THAN ZEROS
070711*
070711*        IF  W-REPLYO (W-ARCH-NDX) EQUAL SPACES
070711*                OR
070711*            W-REPLYO (W-ARCH-NDX) EQUAL ZEROES
070711*            MOVE LOW-VALUES     TO W-REPLY-DATE
070711*
070711*        ELSE
070711*            MOVE W-REPLYO (W-ARCH-NDX)
070711*                                TO W-DEEDIT-FIELD
070711*
070711*            PERFORM 9200-DATE-EDIT THRU 9200-EXIT
070711*
070711*            IF  DC-ERROR-CODE EQUAL SPACES
070711*                MOVE DC-BIN-DATE-1
070711*                                TO W-REPLY-DATE
070711*                MOVE DC-GREG-DATE-1-EDIT
070711*                                TO W-REPLYO (W-ARCH-NDX)
070711*
070711*            ELSE
070711*                MOVE ER-9245    TO EMI-ERROR
070711*                MOVE AL-UANON   TO W-REPLYA (W-ARCH-NDX)
070711*                MOVE -1         TO W-REPLYL (W-ARCH-NDX)
070711*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01158
070711*    IF  (W-REPLY-DATE NOT EQUAL SPACES
070711*            OR
070711*        W-RESEND-DATE-1 NOT EQUAL SPACES
070711*            OR
070711*        W-RESEND-DATE-2 NOT EQUAL SPACES
070711*            OR
070711*        W-RESEND-DATE-3 NOT EQUAL SPACES
070711*            OR
070711*        W-STATUS GREATER THAN SPACES)
070711*            AND
070711*        PI-690-ARCHIVE-NUM (W-ARCH-NDX) NOT EQUAL ZEROS
070711*        PERFORM 0740-UPDATE-ARCH-RECORD THRU 0740-EXIT.
01171
01172  0720-EXIT.
01173      EXIT.
01174
01175  0740-UPDATE-ARCH-RECORD.
01176
01177      MOVE LOW-VALUES             TO W-ARCT-KEY.
01178      MOVE PI-690-ARCHIVE-NUM (W-ARCH-NDX)
01179                                  TO W-ARCH-ARCHIVE-NO
01180                                     W-ARCT-ARCHIVE-NO
01181                                     W-HOLD-ARCHIVE.
01182      MOVE PI-COMPANY-CD          TO W-ARCH-COMPANY-CD
01183                                     W-ARCT-COMPANY-CD.
01184
01185      IF  W-SPECIAL-STATUS
01186          PERFORM 7800-DELETE-CYCLE THRU 7800-EXIT
01187          GO TO 0740-EXIT.
01188
01189      
      * EXEC CICS HANDLE CONDITION
01190 *        NOTFND  (8080-ARCH-NOT-FOUND)
01191 *        ENDFILE (8080-ARCH-NOT-FOUND)
01192 *        END-EXEC.
      *    MOVE '"$I''                  ! $ #00004973' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034393733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01193
01194      PERFORM 7600-READ-ARCH-FILE THRU 7600-EXIT.
01195
01196      IF  W-REPLY-DATE NOT EQUAL SPACES
01197
01198          IF  W-REPLY-DATE EQUAL LOW-VALUES
01199              MOVE LOW-VALUES     TO LA-REPLY-DATE
01200              MOVE 'A'            TO W-STATUS
01201
01202          ELSE
01203              MOVE W-REPLY-DATE   TO LA-REPLY-DATE
01204              MOVE 'C'            TO W-STATUS.
01205
01206      IF  W-RESEND-DATE-1 NOT EQUAL SPACES
01207              AND
01208          W-RESEND-DATE-1 NOT EQUAL LA-RESEND-DATE
01209
01210          IF  LA-SENT-DATE   GREATER THAN LOW-VALUES
01211              MOVE ER-7396        TO EMI-ERROR
01212              MOVE AL-UANON       TO W-RSDATA (W-ARCH-NDX)
01213              MOVE -1             TO W-RSDATL (W-ARCH-NDX)
01214              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01215
01216          ELSE
01217              MOVE 'A'            TO W-STATUS
01218              MOVE LOW-VALUES     TO LA-SENT-DATE
01219              MOVE W-RESEND-DATE-1
01220                                  TO LA-RESEND-DATE.
01221
01222 *    IF  W-RESEND-DATE-2 NOT EQUAL SPACES
01223 *            AND
01224 *        W-RESEND-DATE-2 NOT EQUAL LA-RESEND-DATE-2
01225 *
01226 *        IF  LA-SENT-DATE-2 GREATER THAN LOW-VALUES
01227 *            MOVE ER-7396        TO EMI-ERROR
01228 *            MOVE AL-UANON       TO W-RSFRMA (W-ARCH-NDX)
01229 *            MOVE -1             TO W-RSFRML (W-ARCH-NDX)
01230 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01231 *
01232 *        ELSE
01233 *            MOVE 'A'            TO W-STATUS
01234 *            MOVE LOW-VALUES     TO LA-SENT-DATE-2
01235 *            MOVE W-RESEND-DATE-2
01236 *                                TO LA-RESEND-DATE-2.
01237
01238 *    IF  W-RESEND-DATE-3 NOT EQUAL SPACES
01239 *            AND
01240 *        W-RESEND-DATE-3 NOT EQUAL LA-RESEND-DATE-3
01241 *
01242 *        IF  LA-SENT-DATE-3 GREATER THAN LOW-VALUES
01243 *
01244 *            IF  PI-COMPANY-ID EQUAL 'LAP' OR 'RMC'
01245 *                MOVE ER-7396    TO EMI-ERROR
01246 *                MOVE AL-UANON   TO W-RSSTSA (W-ARCH-NDX)
01247 *                MOVE -1         TO W-RSSTSL (W-ARCH-NDX)
01248 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01249 *
01250 *            ELSE
01251 *                MOVE LOW-VALUES TO LA-SENT-DATE-3
01252 *                MOVE W-RESEND-DATE-3
01253 *                                TO LA-RESEND-DATE-3
01254 *                MOVE 'A'        TO W-STATUS
01255 *
01256 *        ELSE
01257 *            MOVE 'A'            TO W-STATUS
01258 *            MOVE LOW-VALUES     TO LA-SENT-DATE-3
01259 *            MOVE W-RESEND-DATE-3
01260 *                                TO LA-RESEND-DATE-3.
01261
01262 *    IF  LA-RESEND-DATE-3 GREATER THAN LOW-VALUES
01263 *
01264 *        IF  LA-RESEND-DATE-3 GREATER THAN LA-RESEND-DATE-2
01265 *            NEXT SENTENCE
01266 *
01267 *        ELSE
01268 *            EXEC CICS UNLOCK
01269 *                DATASET (W-ARCH-FILE-ID)
01270 *            END-EXEC
01271 *            MOVE ER-9281        TO EMI-ERROR
01272 *            MOVE AL-UANON       TO W-RSFRMA (W-ARCH-NDX)
01273 *            MOVE -1             TO W-RSFRML (W-ARCH-NDX)
01274 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01275 *            GO TO 0740-EXIT.
01276
01277 *    IF  LA-RESEND-DATE-2 GREATER THAN LOW-VALUES
01278 *
01279 *        IF  LA-RESEND-DATE-2 GREATER THAN LA-RESEND-DATE
01280 *            NEXT SENTENCE
01281 *
01282 *        ELSE
01283 *            EXEC CICS UNLOCK
01284 *                DATASET (W-ARCH-FILE-ID)
01285 *            END-EXEC
01286 *
01287 *            MOVE ER-9281        TO EMI-ERROR
01288 *            MOVE AL-UANON       TO W-RSDATA (W-ARCH-NDX)
01289 *            MOVE -1             TO W-RSDATL (W-ARCH-NDX)
01290 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01291 *            GO TO 0740-EXIT.
01292
01293      IF  LA-STATUS EQUAL 'X'
01294              AND
01295          W-ACTIVE
01296
01297          IF  LA-VOIDED-DATE GREATER THAN LOW-VALUES
01298              MOVE 'V'            TO W-STATUS
01299
01300          ELSE
01301              IF  LA-REPLY-DATE GREATER THAN LOW-VALUES
01302                  MOVE 'C'        TO W-STATUS.
01303
01304      IF  LA-STATUS EQUAL 'V'
01305              AND
01306          W-ACTIVE
01307          MOVE LOW-VALUES         TO LA-VOIDED-DATE.
01308
070711*    IF  LA-STATUS EQUAL 'C'
070711*            AND
070711*        W-ACTIVE
070711*        MOVE ER-7399            TO EMI-ERROR
070711*        MOVE AL-UANON           TO W-STATSA (W-ARCH-NDX)
070711*        MOVE -1                 TO W-STATSL (W-ARCH-NDX)
070711*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
070711*        GO TO 0740-REWRITE.
01317
01318      IF  W-CLEAR-INITIAL
01319          MOVE LOW-VALUES         TO LA-INITIAL-PRINT-DATE
01320          MOVE 'A'                TO W-STATUS.
01321
01322      IF  W-CHECK-STATUS
01323          PERFORM 0750-CHECK-STATUS THRU 0750-EXIT
01324
01325      ELSE
01326          IF  W-VALID-STATUS
01327                  AND
01328              W-STATUS GREATER THAN SPACES
01329              MOVE W-STATUS       TO LA-STATUS.
01330
01331      IF  W-VOIDED
01332          MOVE W-SAVE-BIN-DATE    TO LA-VOIDED-DATE.
01333
01334      IF  LA-REPLY-DATE GREATER THAN LOW-VALUES
01335              AND
01336          LA-STATUS EQUAL 'A'
01337          MOVE 'C'                TO LA-STATUS.
01338
01339  0740-REWRITE.
01340
01341      
      * EXEC CICS REWRITE
01342 *        DATASET (W-ARCH-FILE-ID)
01343 *        FROM    (LETTER-ARCHIVE)
01344 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005125' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01345
01346      IF  W-NO-FIRST-CHANGE
01347          MOVE W-ARCH-NDX         TO W-FIRST-CHANGE-IND.
01348
01349  0740-EXIT.
01350      EXIT.
01351                                  EJECT
01352  0750-CHECK-STATUS.
01353
01354      IF  LA-STATUS EQUAL '#'
01355          MOVE 'A'                TO LA-STATUS
01356          GO TO 0750-EXIT.
01357
01358      IF  LA-STATUS NOT EQUAL 'A' OR 'C'
01359          GO TO 0750-EXIT.
01360
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
01361 *    IF  LA-INITIAL-PRINT-DATE GREATER THAN LOW-VALUES
01362 *        IF  LA-RESEND-DATE GREATER THAN LOW-VALUES
01363 *            IF  LA-SENT-DATE   NOT GREATER THAN LOW-VALUES
01364 *                MOVE 'A'        TO LA-STATUS
01365 *
01366 *            ELSE
01367 *                IF  LA-RESEND-DATE-2 GREATER THAN LOW-VALUES
01368 *
01369 *                    IF  LA-SENT-DATE-2 NOT GREATER THAN
01370 *                            LOW-VALUES
01371 *                        MOVE 'A'        TO LA-STATUS
01372 *
01373 *                    ELSE
01374 *                        IF  LA-RESEND-DATE-3 GREATER THAN
01375 *                                LOW-VALUES
01376 *
01377 *                            IF  LA-SENT-DATE-3 NOT GREATER
01378 *                                    THAN LOW-VALUES
01379 *                                MOVE 'A' TO LA-STATUS
01380 *
01381 *                            ELSE
01382 *                                MOVE 'C' TO LA-STATUS
01383 *
01384 *                        ELSE
01385 *                            MOVE 'C'
01386 *                                TO LA-STATUS
01387 *
01388 *
01389 *                 ELSE
01390 *                     MOVE 'C'   TO LA-STATUS
01391 *
01392 *        ELSE
01393 *            MOVE 'C'            TO LA-STATUS
01394 *
01395 *    ELSE
01396 *        MOVE 'A'                TO LA-STATUS.
01397
           .
01398  0750-EXIT.
01399      EXIT.
01400                                  EJECT
01401  0800-CONVERT-EFFECT-DATE.
01402
01403       IF  PI-CERT-EFF-DT > LOW-VALUES
01404           MOVE PI-CERT-EFF-DT    TO DC-BIN-DATE-1
01405                                     PI-690-INIT-EFFECT-DATE
01406           MOVE ' '               TO DC-OPTION-CODE
01407           PERFORM 9500-LINK-DATE-CONVERT
01408               THRU 9500-EXIT
01409
01410           IF  DC-ERROR-CODE EQUAL SPACES
01411               MOVE DC-GREG-DATE-1-EDIT
01412                                  TO EFFDTEI
01413                                     PI-690-INIT-EFF-DTE
01414               MOVE +8            TO EFFDTEL
01415
01416           ELSE
01417               MOVE LOW-VALUES    TO EFFDTEI
01418               MOVE +8            TO EFFDTEL
01419
01420       ELSE
01421           MOVE LOW-VALUES        TO EFFDTEI
01422           MOVE +8                TO EFFDTEL.
01423
01424  0800-EXIT.
01425      EXIT.
01426                                  EJECT
01427  1000-RECOVER-LAST-SCREEN.
01428
01429      MOVE LOW-VALUES             TO EL690AO.
01430      MOVE 1                      TO W-FIRST-CHANGE-IND.
01431
01432      MOVE PI-690-BRWS-TYPE-IND   TO TYPEBRI.
01433      MOVE +1                     TO TYPEBRL.
01434
01435      IF  PI-690-INIT-CERT-PRIME GREATER THAN LOW-VALUES
01436              AND
01437          PI-690-INIT-CERT-PRIME LESS THAN HIGH-VALUES
01438          MOVE PI-690-INIT-CERT-PRIME
01439                                  TO CERTRPI
01440          MOVE +10                TO CERTRPL
01441          MOVE PI-690-INIT-SUFFIX TO SUFFIXI
01442          MOVE +1                 TO SUFFIXL.
01443
01444      IF  PI-690-INIT-CARRIER GREATER THAN LOW-VALUES
01445              AND
01446          PI-690-INIT-CARRIER LESS THAN HIGH-VALUES
01447          MOVE PI-690-INIT-CARRIER
01448                                  TO CARRIERI
01449          MOVE +1                 TO CARRIERL.
01450
01451      IF  PI-690-INIT-GROUPING GREATER THAN LOW-VALUES
01452              AND
01453          PI-690-INIT-GROUPING LESS THAN HIGH-VALUES
01454          MOVE PI-690-INIT-GROUPING
01455                                  TO GROUPI
01456          MOVE +6                 TO GROUPL.
01457
01458      IF  PI-690-INIT-STATE GREATER THAN LOW-VALUES
01459              AND
01460          PI-690-INIT-STATE LESS THAN HIGH-VALUES
01461          MOVE PI-690-INIT-STATE  TO STATEI
01462          MOVE +2                 TO STATEL.
01463
01464      IF  PI-690-INIT-ACCOUNT GREATER THAN LOW-VALUES
01465              AND
01466          PI-690-INIT-ACCOUNT LESS THAN HIGH-VALUES
01467          MOVE PI-690-INIT-ACCOUNT
01468                                  TO ACCTI
01469          MOVE +10                TO ACCTL.
01470
01471      IF  PI-690-INIT-FORM GREATER THAN LOW-VALUES
01472              AND
01473          PI-690-INIT-FORM LESS THAN HIGH-VALUES
01474          MOVE PI-690-INIT-FORM   TO FORMI
01475          MOVE +04                TO FORML.
01476
01477      IF  PI-690-INIT-PROCESSOR GREATER THAN LOW-VALUES
01478              AND
01479          PI-690-INIT-PROCESSOR LESS THAN HIGH-VALUES
01480          MOVE PI-690-INIT-PROCESSOR
01481                                  TO PROCSRI
01482          MOVE +04                TO PROCSRL.
01483
01484      IF  PI-690-INIT-EFFECT-DATE > LOW-VALUES
01485              AND
01486          PI-690-INIT-EFFECT-DATE < HIGH-VALUES
01487          MOVE PI-690-INIT-EFFECT-DATE
01488                                  TO DC-BIN-DATE-1
01489          MOVE ' '                TO DC-OPTION-CODE
01490          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
01491
01492          IF  DC-ERROR-CODE EQUAL SPACES
01493              MOVE DC-GREG-DATE-1-EDIT
01494                                  TO EFFDTEI
01495              MOVE +8             TO EFFDTEL.
01496
01497      IF  PI-690-INIT-ENTRY GREATER THAN LOW-VALUES
01498              AND
01499          PI-690-INIT-ENTRY LESS THAN HIGH-VALUES
01500              AND
01501          PI-690-INIT-CONTROL-PREFIX NOT EQUAL 'CK'
01502          MOVE PI-690-INIT-ENTRY  TO ENTRYI
01503          MOVE +6                 TO ENTRYL
01504
01505      ELSE
01506          IF  PI-690-INIT-CONTROL GREATER THAN ZEROS
01507                  AND
01508              PI-690-INIT-CONTROL LESS THAN 99999999
01509                  AND
01510              PI-690-INIT-CONTROL-PREFIX EQUAL 'CK'
01511              MOVE PI-690-INIT-CONTROL
01512                                  TO CKCNTLI
01513              MOVE +8             TO CKCNTLL.
01514
01515      IF  PI-690-VALID-SELECTION
01516          MOVE PI-690-STATUS-SELECTION-IND
01517                                  TO STATSELI
01518          MOVE +1                 TO STATSELL
01519
01520      ELSE
01521          MOVE 'N'                TO STATSELI
01522          MOVE +1                 TO STATSELL.
01523
01524      IF  PI-690-LAST-BRWS-FWRD
01525          MOVE '1'                TO EIBAID
01526          GO TO 2000-PAGE-FORWARD.
01527
01528      IF  PI-690-LAST-BRWS-BWRD
01529          MOVE '2'                TO EIBAID
01530          MOVE 'Y'                TO W-READ-PREV-TWICE-IND
01531          GO TO 3000-PAGE-BACKWARD.
01532
01533      MOVE -1                     TO CORRSELL.
01534      GO TO 8100-SEND-INITIAL-MAP.
01535
01536  1000-EXIT.
01537      EXIT.
01538                                  EJECT
01539  1200-SET-UP-COMPLETE-SCREEN.
01540
01541      MOVE LOW-VALUES        TO EL690AO.
01542
01543      IF  PI-RETURN-TO-PROGRAM EQUAL 'EL6311'
01544          GO TO 1220-COMING-FROM-EL6311.
01545
01546      IF  PI-RETURN-TO-PROGRAM EQUAL 'EL652'
01547          GO TO 1240-COMING-FROM-EL652.
01548
01549      IF  PI-RETURN-TO-PROGRAM EQUAL 'EL1273'
01550          MOVE LOW-VALUES         TO PI-689-WORK-AREA
01551          MOVE PI-CERT-PRIME      TO CERTRPI
01552          MOVE +10                TO CERTRPL
01553          MOVE PI-CERT-SFX        TO SUFFIXI
01554          MOVE +1                 TO SUFFIXL
01555          MOVE 1                  TO TYPEBRI
01556          MOVE +1                 TO TYPEBRL
01557
01558      ELSE
01559          IF  PI-RETURN-TO-PROGRAM EQUAL 'EL6501'
01560              MOVE 4              TO TYPEBRI
01561              MOVE +1             TO TYPEBRL.
01562
01563      MOVE PI-CARRIER             TO CARRIERI.
01564      MOVE +1                     TO CARRIERL.
01565      MOVE PI-GROUPING            TO GROUPI.
01566      MOVE +6                     TO GROUPL.
01567      MOVE PI-STATE               TO STATEI.
01568      MOVE +2                     TO STATEL.
01569      MOVE PI-ACCOUNT             TO ACCTI.
01570      MOVE +10                    TO ACCTL.
01571
01572      IF  PI-CERT-EFF-DT NOT EQUAL LOW-VALUES
01573              AND
01574          PI-CERT-EFF-DT NOT EQUAL SPACES
01575          MOVE PI-CERT-EFF-DT     TO DC-BIN-DATE-1
01576          MOVE ' '                TO DC-OPTION-CODE
01577          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
01578
01579          IF  DC-ERROR-CODE EQUAL SPACES
01580              MOVE DC-GREG-DATE-1-EDIT
01581                                  TO EFFDTEI
01582              MOVE +8             TO EFFDTEL.
01583
01584      MOVE '1'                    TO EIBAID.
01585
01586      PERFORM 0500-EDIT-SCREEN-DATA THRU 0500-EXIT.
01587      MOVE ER-7397                TO EMI-ERROR.
01588      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01589      GO TO 3000-PAGE-BACKWARD.
01590 *    GO TO 2000-PAGE-FORWARD.
01591
01592  1200-EXIT.
01593      EXIT.
01594                                  EJECT
01595  1220-COMING-FROM-EL6311.
01596
01597      MOVE 1                      TO TYPEBRI.
01598      MOVE +1                     TO TYPEBRL.
01599 *    MOVE PI-689-ENTRY-BATCH     TO ENTRYI.
01600 *    MOVE +6                     TO ENTRYL.
01601      MOVE PI-689-CERT-PRIME      TO CERTRPI.
01602      MOVE +10                    TO CERTRPL.
01603      MOVE PI-689-CERT-SFX        TO SUFFIXI.
01604      MOVE +1                     TO SUFFIXL.
01605      MOVE PI-689-CARRIER         TO CARRIERI.
01606      MOVE +1                     TO CARRIERL.
01607      MOVE PI-689-GROUPING        TO GROUPI.
01608      MOVE +6                     TO GROUPL.
01609      MOVE PI-689-STATE           TO STATEI.
01610      MOVE +2                     TO STATEL.
01611      MOVE PI-689-ACCOUNT         TO ACCTI.
01612      MOVE +10                    TO ACCTL.
01613
01614      IF  PI-689-EFF-DATE GREATER THAN LOW-VALUES
01615          MOVE PI-689-EFF-DATE    TO DC-BIN-DATE-1
01616          MOVE ' '                TO DC-OPTION-CODE
01617          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
01618
01619          IF  DC-ERROR-CODE EQUAL SPACES
01620              MOVE DC-GREG-DATE-1-EDIT
01621                                  TO EFFDTEI
01622              MOVE +8             TO EFFDTEL
01623
01624          ELSE
01625              MOVE LOW-VALUES     TO EFFDTEI
01626              MOVE +8             TO EFFDTEL
01627
01628      ELSE
01629          MOVE LOW-VALUES         TO EFFDTEI
01630          MOVE +8                 TO EFFDTEL.
01631
01632      MOVE '1'                    TO EIBAID.
01633
01634      PERFORM 0500-EDIT-SCREEN-DATA THRU 0500-EXIT.
01635      MOVE ER-7397                TO EMI-ERROR.
01636      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01637      GO TO 3000-PAGE-BACKWARD.
01638 *    GO TO 2000-PAGE-FORWARD.
01639
01640  1220-EXIT.
01641      EXIT.
01642                                  EJECT
01643  1240-COMING-FROM-EL652.
01644
01645      MOVE 1                      TO TYPEBRI.
01646      MOVE +1                     TO TYPEBRL.
01647      MOVE PI-CR-FIN-RESP         TO CERTRPI.
01648      MOVE +10                    TO CERTRPL.
01649      MOVE PI-CR-TYPE             TO SUFFIXI.
01650      MOVE +1                     TO SUFFIXL.
01651      MOVE PI-CR-CARRIER          TO CARRIERI.
01652      MOVE +1                     TO CARRIERL.
01653      MOVE PI-CR-GROUPING         TO GROUPI.
01654      MOVE +6                     TO GROUPL.
01655      MOVE PI-CR-ACCOUNT          TO ACCTI.
01656      MOVE +10                    TO ACCTL.
01657
01658      MOVE AL-UANON               TO CARRIERA
01659                                     GROUPA
01660                                     ACCTA
01661                                     TYPEBRA
01662                                     CERTRPA.
01663
01664      PERFORM 0500-EDIT-SCREEN-DATA THRU 0500-EXIT.
01665      MOVE ER-7397                TO EMI-ERROR.
01666      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01667      GO TO 3000-PAGE-BACKWARD.
01668 *    GO TO 2000-PAGE-FORWARD.
01669
01670  1240-EXIT.
01671      EXIT.
01672                                  EJECT
01673  1500-SELECT-ARCHIVE-REC.
01674
01675      MOVE ZEROS                  TO PI-689-ARCHIVE-NUMBER.
01676
01677      IF  CORRSELL GREATER THAN +0
01678
01679          
      * EXEC CICS BIF DEEDIT
01680 *            FIELD   (CORRSELI)
01681 *            LENGTH  (2)
01682 *            END-EXEC
           MOVE 2
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005477' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CORRSELI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01683
01684          IF  CORRSELI NUMERIC
01685
01686              IF  CORRSELI NOT LESS THAN 01
01687                      AND
01688                  CORRSELI NOT GREATER THAN 12
01689                  MOVE CORRSELI   TO W-ARCH-NDX
01690
01691                  IF  PI-690-ARCHIVE-NUM (W-ARCH-NDX)
01692                           NOT EQUAL ZEROS
01693                      MOVE PI-690-ARCHIVE-NUM (W-ARCH-NDX)
01694                                  TO PI-689-ARCHIVE-NUMBER
01695                      GO TO 1500-EXIT
01696
01697                  ELSE
01698                      MOVE ER-9293
01699                                  TO EMI-ERROR
01700                      MOVE -1     TO CORRSELL
01701                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01702                      GO TO 8200-SEND-DATAONLY
01703
01704          ELSE
01705              MOVE ER-9293        TO EMI-ERROR
01706              MOVE -1             TO CORRSELL
01707              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01708              GO TO 8200-SEND-DATAONLY.
01709
01710  1500-EXIT.
01711      EXIT.
01712                                  EJECT
01713  2000-PAGE-FORWARD.
01714
01715      MOVE '1'                    TO PI-690-LAST-BROWSE-IND.
01716      MOVE +1                     TO W-ARCH-NDX.
01717      MOVE ZEROS                  TO W-ARCH-NUMBER.
01718
01719      GO TO 2100-ARCH2-PROCESS
01720            2200-ARCH3-PROCESS
01721            2300-ARCH4-PROCESS
01722            2400-ARCH5-PROCESS
01723            2500-ARCH6-PROCESS
01724            2600-ARCH-PROCESS DEPENDING ON PI-690-BRWS-TYPE-IND.
01725
01726      MOVE ER-7384                TO EMI-ERROR.
01727      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01728      MOVE -1                     TO TYPEBRL.
01729      MOVE AL-UABON               TO TYPEBRA.
01730      GO TO 8100-SEND-INITIAL-MAP.
01731
01732  2000-EXIT.
01733      EXIT.
01734                                  EJECT
01735  2100-ARCH2-PROCESS.
01736
01737      IF  W-CHANGE-REQUESTED
01738          MOVE LOW-VALUES         TO W-ARCH2-KEY
01739          MOVE PI-COMPANY-CD      TO W-ARCH2-COMPANY-CD
01740          MOVE PI-690-INIT-CERT-PRIME
01741                                  TO W-ARCH2-CERT-PRIME
01742          MOVE PI-690-INIT-SUFFIX TO W-ARCH2-SUFFIX
01743          MOVE PI-690-INIT-CARRIER
01744                                  TO W-ARCH2-CARRIER
01745          MOVE PI-690-INIT-GROUPING
01746                                  TO W-ARCH2-GROUPING
01747          MOVE PI-690-INIT-STATE  TO W-ARCH2-STATE
01748          MOVE PI-690-INIT-ACCOUNT
01749                                  TO W-ARCH2-ACCOUNT
01750          MOVE PI-690-INIT-EFFECT-DATE
01751                                  TO W-ARCH2-EFF-DTE
01752          MOVE PI-690-INIT-ARCHIVE-NO
01753                                  TO W-ARCH2-ARCHIVE-NO
01754
01755      ELSE
01756          IF  W-FIRST-CHANGE-FOUND
01757              MOVE LOW-VALUES     TO W-ARCH2-KEY
01758              MOVE PI-COMPANY-CD  TO W-ARCH2-COMPANY-CD
01759              MOVE PI-690-FIRST-CERT-PRIME
01760                                  TO W-ARCH2-CERT-PRIME
01761              MOVE PI-690-FIRST-SUFFIX
01762                                  TO W-ARCH2-SUFFIX
01763              MOVE PI-690-FIRST-CARRIER
01764                                  TO W-ARCH2-CARRIER
01765              MOVE PI-690-FIRST-GROUPING
01766                                  TO W-ARCH2-GROUPING
01767              MOVE PI-690-FIRST-STATE
01768                                  TO W-ARCH2-STATE
01769              MOVE PI-690-FIRST-ACCOUNT
01770                                  TO W-ARCH2-ACCOUNT
01771              MOVE PI-690-FIRST-EFFECT-DATE
01772                                  TO W-ARCH2-EFF-DTE
01773              MOVE PI-690-FIRST-ARCHIVE-NO
01774                                  TO W-ARCH2-ARCHIVE-NO
01775
01776          ELSE
01777              MOVE LOW-VALUES     TO W-ARCH2-KEY
01778              MOVE PI-COMPANY-CD  TO W-ARCH2-COMPANY-CD
01779              MOVE PI-690-LAST-CERT-PRIME
01780                                  TO W-ARCH2-CERT-PRIME
01781              MOVE PI-690-LAST-SUFFIX
01782                                  TO W-ARCH2-SUFFIX
01783              MOVE PI-690-LAST-CARRIER
01784                                  TO W-ARCH2-CARRIER
01785              MOVE PI-690-LAST-GROUPING
01786                                  TO W-ARCH2-GROUPING
01787              MOVE PI-690-LAST-STATE
01788                                  TO W-ARCH2-STATE
01789              MOVE PI-690-LAST-ACCOUNT
01790                                  TO W-ARCH2-ACCOUNT
01791              MOVE PI-690-LAST-EFFECT-DATE
01792                                  TO W-ARCH2-EFF-DTE
01793              MOVE PI-690-LAST-ARCHIVE-NO
01794                                  TO W-ARCH2-ARCHIVE-NO.
01795
01796      
      * EXEC CICS HANDLE CONDITION
01797 *        NOTFND  (8080-ARCH-NOT-FOUND)
01798 *        ENDFILE (8080-ARCH-NOT-FOUND)
01799 *        NOTOPEN (8000-ARCH2-NOT-OPEN)
01800 *        END-EXEC.
      *    MOVE '"$I''J                 ! % #00005594' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035353934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01801
01802      
      * EXEC CICS STARTBR
01803 *        DATASET  (W-ARCH2-FILE-ID)
01804 *        RIDFLD   (W-ARCH2-KEY)
01805 *        GTEQ
01806 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005600' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH2-FILE-ID, 
                 W-ARCH2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01807
01808      PERFORM 7000-READ-ARCH2-FILE-NEXT THRU 7000-EXIT.
01809
01810      
      * EXEC CICS HANDLE CONDITION
01811 *        NOTFND  (2100-END-BROWSE)
01812 *        ENDFILE (2100-END-BROWSE)
01813 *        END-EXEC.
      *    MOVE '"$I''                  ! & #00005608' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035363038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01814
01815  2100-BUILD-ARCH2-LIST.
01816
01817      IF  LA-COMPANY-CD-A2 NOT EQUAL PI-COMPANY-CD
01818          GO TO 2100-END-BROWSE.
01819
01820      IF  PI-690-INIT-CERT-NO EQUAL LOW-VALUES OR HIGH-VALUES
01821          GO TO 2100-CONTINUE.
01822
01823      IF  LA-CERT-NO-A2 NOT EQUAL PI-690-INIT-CERT-NO
01824          GO TO 2100-END-BROWSE.
01825
01826      IF  PI-690-INIT-CARRIER EQUAL LOW-VALUES OR HIGH-VALUES
01827          GO TO 2100-CONTINUE.
01828
01829      IF  LA-CARRIER-A2 NOT EQUAL PI-690-INIT-CARRIER
01830          GO TO 2100-END-BROWSE.
01831
01832      IF  PI-690-INIT-GROUPING EQUAL LOW-VALUES OR HIGH-VALUES
01833          GO TO 2100-CONTINUE.
01834
01835      IF  LA-GROUPING-A2 NOT EQUAL PI-690-INIT-GROUPING
01836          GO TO 2100-END-BROWSE.
01837
01838      IF  PI-690-INIT-STATE EQUAL LOW-VALUES OR HIGH-VALUES
01839          GO TO 2100-CONTINUE.
01840
01841      IF  LA-STATE-A2 NOT EQUAL PI-690-INIT-STATE
01842          GO TO 2100-END-BROWSE.
01843
01844      IF  PI-690-INIT-ACCOUNT EQUAL LOW-VALUES OR HIGH-VALUES
01845          GO TO 2100-CONTINUE.
01846
01847      IF  LA-ACCOUNT-A2 NOT EQUAL PI-690-INIT-ACCOUNT
01848          GO TO 2100-END-BROWSE.
01849
01850      IF  PI-690-INIT-EFFECT-DATE = LOW-VALUES OR HIGH-VALUES
01851          GO TO 2100-CONTINUE.
01852
01853      IF  LA-EFFECT-DATE-A2 NOT EQUAL PI-690-INIT-EFFECT-DATE
01854          GO TO 2100-END-BROWSE.
01855
01856  2100-CONTINUE.
01857
01858      IF  NOT PI-690-SELECT-ALL
01859              AND
01860          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND
01861          PERFORM 7000-READ-ARCH2-FILE-NEXT THRU 7000-EXIT
01862          GO TO 2100-BUILD-ARCH2-LIST.
01863
01864      IF  PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES
01865                  AND
01866          PI-690-INIT-ENTRY LESS THAN HIGH-VALUES
01867          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6
01868              PERFORM 7000-READ-ARCH2-FILE-NEXT THRU 7000-EXIT
01869              GO TO 2100-BUILD-ARCH2-LIST.
01870
01871      IF  PI-690-INIT-FORM NOT EQUAL LOW-VALUES
01872                  AND
01873          PI-690-INIT-FORM LESS THAN HIGH-VALUES
01874          IF  PI-690-INIT-FORM NOT EQUAL LA-FORM-A3
01875              PERFORM 7000-READ-ARCH2-FILE-NEXT THRU 7000-EXIT
01876              GO TO 2100-BUILD-ARCH2-LIST.
01877
01878      IF  PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES
01879                  AND
01880          PI-690-INIT-PROCESSOR LESS THAN HIGH-VALUES
01881          IF  PI-690-INIT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
01882              PERFORM 7000-READ-ARCH2-FILE-NEXT THRU 7000-EXIT
01883              GO TO 2100-BUILD-ARCH2-LIST.
01884
01885      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.
01886
01887      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.
01888
01889      IF  W-ARCH-NDX LESS THAN +13
01890          PERFORM 7000-READ-ARCH2-FILE-NEXT THRU 7000-EXIT
01891          GO TO 2100-BUILD-ARCH2-LIST.
01892
01893  2100-END.
01894
01895      MOVE -1                     TO CORRSELL.
01896      MOVE ZEROS                  TO CORRSELO.
01897      PERFORM 2100-END-BROWSE.
01898      GO TO 8100-SEND-INITIAL-MAP.
01899
01900  2100-END-BROWSE.
01901
01902      
      * EXEC CICS ENDBR
01903 *        DATASET (W-ARCH2-FILE-ID)
01904 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005700' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH2-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01905
01906  2100-NOT-FOUND.
01907
01908      MOVE ER-0130                TO EMI-ERROR.
01909      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01910      MOVE -1                     TO CORRSELL.
01911      MOVE ZEROS                  TO CORRSELO.
01912      GO TO 8100-SEND-INITIAL-MAP.
01913
01914  2100-EXIT.
01915      EXIT.
01916                                  EJECT
01917  2200-ARCH3-PROCESS.
01918
01919      IF  W-CHANGE-REQUESTED
01920          MOVE LOW-VALUES         TO W-ARCH3-KEY
01921          MOVE PI-COMPANY-CD      TO W-ARCH3-COMPANY-CD
01922          MOVE PI-690-INIT-FORM   TO W-ARCH3-FORM
01923          MOVE PI-690-INIT-CARRIER
01924                                  TO W-ARCH3-CARRIER
01925          MOVE PI-690-INIT-GROUPING
01926                                  TO W-ARCH3-GROUPING
01927          MOVE PI-690-INIT-STATE  TO W-ARCH3-STATE
01928          MOVE PI-690-INIT-ACCOUNT
01929                                  TO W-ARCH3-ACCOUNT
01930          MOVE PI-690-INIT-ARCHIVE-NO
01931                                  TO W-ARCH3-ARCHIVE-NO
01932
01933      ELSE
01934          IF  W-FIRST-CHANGE-FOUND
01935              MOVE LOW-VALUES     TO W-ARCH3-KEY
01936              MOVE PI-COMPANY-CD  TO W-ARCH3-COMPANY-CD
01937              MOVE PI-690-FIRST-FORM
01938                                  TO W-ARCH3-FORM
01939              MOVE PI-690-FIRST-CARRIER
01940                                  TO W-ARCH3-CARRIER
01941              MOVE PI-690-FIRST-GROUPING
01942                                  TO W-ARCH3-GROUPING
01943              MOVE PI-690-FIRST-STATE
01944                                  TO W-ARCH3-STATE
01945              MOVE PI-690-FIRST-ACCOUNT
01946                                  TO W-ARCH3-ACCOUNT
01947              MOVE PI-690-FIRST-ARCHIVE-NO
01948                                  TO W-ARCH3-ARCHIVE-NO
01949
01950          ELSE
01951              MOVE LOW-VALUES     TO W-ARCH3-KEY
01952              MOVE PI-COMPANY-CD  TO W-ARCH3-COMPANY-CD
01953              MOVE PI-690-LAST-FORM
01954                                  TO W-ARCH3-FORM
01955              MOVE PI-690-LAST-CARRIER
01956                                  TO W-ARCH3-CARRIER
01957              MOVE PI-690-LAST-GROUPING
01958                                  TO W-ARCH3-GROUPING
01959              MOVE PI-690-LAST-STATE
01960                                  TO W-ARCH3-STATE
01961              MOVE PI-690-LAST-ACCOUNT
01962                                  TO W-ARCH3-ACCOUNT
01963              MOVE PI-690-LAST-ARCHIVE-NO
01964                                  TO W-ARCH3-ARCHIVE-NO.
01965
01966      
      * EXEC CICS HANDLE CONDITION
01967 *        NOTFND  (8080-ARCH-NOT-FOUND)
01968 *        ENDFILE (8080-ARCH-NOT-FOUND)
01969 *        NOTOPEN (8010-ARCH3-NOT-OPEN)
01970 *        END-EXEC.
      *    MOVE '"$I''J                 ! '' #00005764' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035373634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01971
01972      
      * EXEC CICS STARTBR
01973 *        DATASET  (W-ARCH3-FILE-ID)
01974 *        RIDFLD   (W-ARCH3-KEY)
01975 *        GTEQ
01976 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005770' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH3-FILE-ID, 
                 W-ARCH3-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01977
01978      PERFORM 7100-READ-ARCH3-FILE-NEXT THRU 7100-EXIT.
01979
01980      
      * EXEC CICS HANDLE CONDITION
01981 *        NOTFND  (2200-END-BROWSE)
01982 *        ENDFILE (2200-END-BROWSE)
01983 *        END-EXEC.
      *    MOVE '"$I''                  ! ( #00005778' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303035373738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01984
01985  2200-BUILD-ARCH3-LIST.
01986
01987      IF  LA-COMPANY-CD-A3 NOT EQUAL PI-COMPANY-CD
01988          GO TO 2200-END-BROWSE.
01989
01990      IF  PI-690-INIT-FORM EQUAL LOW-VALUES OR HIGH-VALUES
01991          GO TO 2200-CONTINUE.
01992
01993      IF  LA-FORM-A3 NOT EQUAL PI-690-INIT-FORM
01994          GO TO 2200-END-BROWSE.
01995
01996      IF  PI-690-INIT-CARRIER EQUAL LOW-VALUES OR HIGH-VALUES
01997          GO TO 2200-CONTINUE.
01998
01999      IF  LA-CARRIER-A3 NOT EQUAL PI-690-INIT-CARRIER
02000          GO TO 2200-END-BROWSE.
02001
02002      IF  PI-690-INIT-GROUPING EQUAL LOW-VALUES OR HIGH-VALUES
02003          GO TO 2200-CONTINUE.
02004
02005      IF  LA-GROUPING-A3 NOT EQUAL PI-690-INIT-GROUPING
02006          GO TO 2200-END-BROWSE.
02007
02008      IF  PI-690-INIT-STATE EQUAL LOW-VALUES OR HIGH-VALUES
02009          GO TO 2200-CONTINUE.
02010
02011      IF  LA-STATE-A3 NOT EQUAL PI-690-INIT-STATE
02012          GO TO 2200-END-BROWSE.
02013
02014      IF  PI-690-INIT-ACCOUNT EQUAL LOW-VALUES OR HIGH-VALUES
02015          GO TO 2200-CONTINUE.
02016
02017      IF  LA-ACCOUNT-A3 NOT EQUAL PI-690-INIT-ACCOUNT
02018          GO TO 2200-END-BROWSE.
02019
02020  2200-CONTINUE.
02021
02022      IF  NOT PI-690-SELECT-ALL
02023              AND
02024          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND
02025          PERFORM 7100-READ-ARCH3-FILE-NEXT THRU 7100-EXIT
02026          GO TO 2200-BUILD-ARCH3-LIST.
02027
02028      IF  PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES
02029                  AND
02030          PI-690-INIT-ENTRY LESS THAN HIGH-VALUES
02031          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6
02032              PERFORM 7100-READ-ARCH3-FILE-NEXT THRU 7100-EXIT
02033              GO TO 2200-BUILD-ARCH3-LIST.
02034
02035      IF  PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES
02036                  AND
02037          PI-690-INIT-PROCESSOR LESS THAN HIGH-VALUES
02038          IF  PI-690-INIT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
02039              PERFORM 7100-READ-ARCH3-FILE-NEXT THRU 7100-EXIT
02040              GO TO 2200-BUILD-ARCH3-LIST.
02041
02042      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.
02043
02044      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.
02045
02046      IF  W-ARCH-NDX LESS THAN +13
02047          PERFORM 7100-READ-ARCH3-FILE-NEXT THRU 7100-EXIT
02048          GO TO 2200-BUILD-ARCH3-LIST.
02049
02050  2200-END.
02051
02052      MOVE -1                     TO CORRSELL.
02053      MOVE ZEROS                  TO CORRSELO.
02054      PERFORM 2200-END-BROWSE.
02055      GO TO 8100-SEND-INITIAL-MAP.
02056
02057  2200-END-BROWSE.
02058
02059      
      * EXEC CICS ENDBR
02060 *        DATASET (W-ARCH3-FILE-ID)
02061 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005857' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH3-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02062
02063  2200-ARCH3-NOT-FOUND.
02064
02065      MOVE ER-0130                TO EMI-ERROR.
02066      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02067      MOVE -1                     TO CORRSELL.
02068      MOVE ZEROS                  TO CORRSELO.
02069      GO TO 8100-SEND-INITIAL-MAP.
02070
02071  2200-EXIT.
02072      EXIT.
02073                                  EJECT
02074  2300-ARCH4-PROCESS.
02075
02076      IF  W-CHANGE-REQUESTED
02077          MOVE LOW-VALUES         TO W-ARCH4-KEY
02078          MOVE PI-COMPANY-CD      TO W-ARCH4-COMPANY-CD
02079          MOVE PI-690-INIT-PROCESSOR
02080                                  TO W-ARCH4-PROCESSOR
02081          MOVE PI-690-INIT-CARRIER
02082                                  TO W-ARCH4-CARRIER
02083          MOVE PI-690-INIT-GROUPING
02084                                  TO W-ARCH4-GROUPING
02085          MOVE PI-690-INIT-STATE  TO W-ARCH4-STATE
02086          MOVE PI-690-INIT-ACCOUNT
02087                                  TO W-ARCH4-ACCOUNT
02088          MOVE PI-690-INIT-ARCHIVE-NO
02089                                  TO W-ARCH4-ARCHIVE-NO
02090
02091      ELSE
02092          IF  W-FIRST-CHANGE-FOUND
02093              MOVE LOW-VALUES     TO W-ARCH4-KEY
02094              MOVE PI-COMPANY-CD  TO W-ARCH4-COMPANY-CD
02095              MOVE PI-690-FIRST-PROCESSOR
02096                                  TO W-ARCH4-PROCESSOR
02097              MOVE PI-690-FIRST-CARRIER
02098                                  TO W-ARCH4-CARRIER
02099              MOVE PI-690-FIRST-GROUPING
02100                                  TO W-ARCH4-GROUPING
02101              MOVE PI-690-FIRST-STATE
02102                                  TO W-ARCH4-STATE
02103              MOVE PI-690-FIRST-ACCOUNT
02104                                  TO W-ARCH4-ACCOUNT
02105              MOVE PI-690-FIRST-ARCHIVE-NO
02106                                  TO W-ARCH4-ARCHIVE-NO
02107
02108          ELSE
02109              MOVE LOW-VALUES     TO W-ARCH4-KEY
02110              MOVE PI-COMPANY-CD  TO W-ARCH4-COMPANY-CD
02111              MOVE PI-690-LAST-PROCESSOR
02112                                  TO W-ARCH4-PROCESSOR
02113              MOVE PI-690-LAST-CARRIER
02114                                  TO W-ARCH4-CARRIER
02115              MOVE PI-690-LAST-GROUPING
02116                                  TO W-ARCH4-GROUPING
02117              MOVE PI-690-LAST-STATE
02118                                  TO W-ARCH4-STATE
02119              MOVE PI-690-LAST-ACCOUNT
02120                                  TO W-ARCH4-ACCOUNT
02121              MOVE PI-690-LAST-ARCHIVE-NO
02122                                  TO W-ARCH4-ARCHIVE-NO.
02123
02124      
      * EXEC CICS HANDLE CONDITION
02125 *        NOTFND  (8080-ARCH-NOT-FOUND)
02126 *        ENDFILE (8080-ARCH-NOT-FOUND)
02127 *        NOTOPEN (8020-ARCH4-NOT-OPEN)
02128 *        END-EXEC.
      *    MOVE '"$I''J                 ! ) #00005922' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035393232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02129
02130      
      * EXEC CICS STARTBR
02131 *        DATASET  (W-ARCH4-FILE-ID)
02132 *        RIDFLD   (W-ARCH4-KEY)
02133 *        GTEQ
02134 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005928' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH4-FILE-ID, 
                 W-ARCH4-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02135
02136      PERFORM 7200-READ-ARCH4-FILE-NEXT THRU 7200-EXIT.
02137
02138      
      * EXEC CICS HANDLE CONDITION
02139 *        NOTFND  (2300-END-BROWSE)
02140 *        ENDFILE (2300-END-BROWSE)
02141 *        END-EXEC.
      *    MOVE '"$I''                  ! * #00005936' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303035393336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02142
02143  2300-BUILD-ARCH4-LIST.
02144
02145      IF  LA-COMPANY-CD-A4 NOT EQUAL PI-COMPANY-CD
02146          GO TO 2300-END-BROWSE.
02147
02148      IF  PI-690-INIT-PROCESSOR EQUAL LOW-VALUES OR HIGH-VALUES
02149          GO TO 2300-CONTINUE.
02150
02151      IF  LA-PROCESSOR-CD NOT EQUAL PI-690-INIT-PROCESSOR
02152          GO TO 2300-END-BROWSE.
02153
02154      IF  PI-690-INIT-CARRIER EQUAL LOW-VALUES OR HIGH-VALUES
02155          GO TO 2300-CONTINUE.
02156
02157      IF  LA-CARRIER-A4 NOT EQUAL PI-690-INIT-CARRIER
02158          GO TO 2300-END-BROWSE.
02159
02160      IF  PI-690-INIT-GROUPING EQUAL LOW-VALUES OR HIGH-VALUES
02161          GO TO 2300-CONTINUE.
02162
02163      IF  LA-GROUPING-A4 NOT EQUAL PI-690-INIT-GROUPING
02164          GO TO 2300-END-BROWSE.
02165
02166      IF  PI-690-INIT-STATE EQUAL LOW-VALUES OR HIGH-VALUES
02167          GO TO 2300-CONTINUE.
02168
02169      IF  LA-STATE-A4 NOT EQUAL PI-690-INIT-STATE
02170          GO TO 2300-END-BROWSE.
02171
02172      IF  PI-690-INIT-ACCOUNT EQUAL LOW-VALUES OR HIGH-VALUES
02173          GO TO 2300-CONTINUE.
02174
02175      IF  LA-ACCOUNT-A4 NOT EQUAL PI-690-INIT-ACCOUNT
02176          GO TO 2300-END-BROWSE.
02177
02178  2300-CONTINUE.
02179
02180      IF  NOT PI-690-SELECT-ALL
02181              AND
02182          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND
02183          PERFORM 7200-READ-ARCH4-FILE-NEXT THRU 7200-EXIT
02184          GO TO 2300-BUILD-ARCH4-LIST.
02185
02186      IF  PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES
02187                  AND
02188          PI-690-INIT-ENTRY LESS THAN HIGH-VALUES
02189          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6
02190              PERFORM 7200-READ-ARCH4-FILE-NEXT THRU 7200-EXIT
02191              GO TO 2300-BUILD-ARCH4-LIST.
02192
02193      IF  PI-690-INIT-FORM NOT EQUAL LOW-VALUES
02194                  AND
02195          PI-690-INIT-FORM LESS THAN HIGH-VALUES
02196          IF  PI-690-INIT-FORM NOT EQUAL LA-FORM-A3
02197              PERFORM 7200-READ-ARCH4-FILE-NEXT THRU 7200-EXIT
02198              GO TO 2300-BUILD-ARCH4-LIST.
02199
02200      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.
02201
02202      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.
02203
02204      IF  W-ARCH-NDX LESS THAN +13
02205          PERFORM 7200-READ-ARCH4-FILE-NEXT THRU 7200-EXIT
02206          GO TO 2300-BUILD-ARCH4-LIST.
02207
02208  2300-END.
02209
02210      MOVE -1                     TO CORRSELL.
02211      MOVE ZEROS                  TO CORRSELO.
02212      PERFORM 2300-END-BROWSE.
02213      GO TO 8100-SEND-INITIAL-MAP.
02214
02215  2300-END-BROWSE.
02216
02217      
      * EXEC CICS ENDBR
02218 *        DATASET (W-ARCH4-FILE-ID)
02219 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006015' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH4-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02220
02221  2300-ARCH4-NOT-FOUND.
02222
02223      MOVE ER-0130                TO EMI-ERROR.
02224      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02225      MOVE -1                     TO CORRSELL.
02226      MOVE ZEROS                  TO CORRSELO.
02227      GO TO 8100-SEND-INITIAL-MAP.
02228
02229  2300-EXIT.
02230      EXIT.
02231                                  EJECT
02232  2400-ARCH5-PROCESS.
02233
02234      IF  W-CHANGE-REQUESTED
02235          MOVE LOW-VALUES         TO W-ARCH5-KEY
02236          MOVE PI-COMPANY-CD      TO W-ARCH5-COMPANY-CD
02237          MOVE PI-690-INIT-CARRIER
02238                                  TO W-ARCH5-CARRIER
02239          MOVE PI-690-INIT-GROUPING
02240                                  TO W-ARCH5-GROUPING
02241          MOVE PI-690-INIT-STATE  TO W-ARCH5-STATE
02242          MOVE PI-690-INIT-ACCOUNT
02243                                  TO W-ARCH5-ACCOUNT
02244          MOVE PI-690-INIT-ARCHIVE-NO
02245                                  TO W-ARCH5-ARCHIVE-NO
02246
02247      ELSE
02248          IF  W-FIRST-CHANGE-FOUND
02249              MOVE LOW-VALUES     TO W-ARCH5-KEY
02250              MOVE PI-COMPANY-CD  TO W-ARCH5-COMPANY-CD
02251              MOVE PI-690-FIRST-CARRIER
02252                                  TO W-ARCH5-CARRIER
02253              MOVE PI-690-FIRST-GROUPING
02254                                  TO W-ARCH5-GROUPING
02255              MOVE PI-690-FIRST-STATE
02256                                  TO W-ARCH5-STATE
02257              MOVE PI-690-FIRST-ACCOUNT
02258                                  TO W-ARCH5-ACCOUNT
02259              MOVE PI-690-FIRST-ARCHIVE-NO
02260                                  TO W-ARCH5-ARCHIVE-NO
02261
02262          ELSE
02263              MOVE LOW-VALUES     TO W-ARCH5-KEY
02264              MOVE PI-COMPANY-CD  TO W-ARCH5-COMPANY-CD
02265              MOVE PI-690-LAST-CARRIER
02266                                  TO W-ARCH5-CARRIER
02267              MOVE PI-690-LAST-GROUPING
02268                                  TO W-ARCH5-GROUPING
02269              MOVE PI-690-LAST-STATE
02270                                  TO W-ARCH5-STATE
02271              MOVE PI-690-LAST-ACCOUNT
02272                                  TO W-ARCH5-ACCOUNT
02273              MOVE PI-690-LAST-ARCHIVE-NO
02274                                  TO W-ARCH5-ARCHIVE-NO.
02275
02276      
      * EXEC CICS HANDLE CONDITION
02277 *        NOTFND  (8080-ARCH-NOT-FOUND)
02278 *        ENDFILE (8080-ARCH-NOT-FOUND)
02279 *        NOTOPEN (8030-ARCH5-NOT-OPEN)
02280 *        END-EXEC.
      *    MOVE '"$I''J                 ! + #00006074' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303036303734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02281
02282      
      * EXEC CICS STARTBR
02283 *        DATASET  (W-ARCH5-FILE-ID)
02284 *        RIDFLD   (W-ARCH5-KEY)
02285 *        GTEQ
02286 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006080' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH5-FILE-ID, 
                 W-ARCH5-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02287
02288      PERFORM 7300-READ-ARCH5-FILE-NEXT THRU 7300-EXIT.
02289
02290      
      * EXEC CICS HANDLE CONDITION
02291 *        NOTFND  (2400-END-BROWSE)
02292 *        ENDFILE (2400-END-BROWSE)
02293 *        END-EXEC.
      *    MOVE '"$I''                  ! , #00006088' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303036303838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02294
02295  2400-BUILD-ARCH5-LIST.
02296
02297      IF  LA-COMPANY-CD-A5 NOT EQUAL PI-COMPANY-CD
02298          GO TO 2400-END-BROWSE.
02299
02300      IF  PI-690-INIT-CARRIER EQUAL LOW-VALUES OR HIGH-VALUES
02301          GO TO 2400-CONTINUE.
02302
02303      IF  LA-CARRIER-A5 NOT EQUAL PI-690-INIT-CARRIER
02304          GO TO 2400-END-BROWSE.
02305
02306      IF  PI-690-INIT-GROUPING EQUAL LOW-VALUES OR HIGH-VALUES
02307          GO TO 2400-CONTINUE.
02308
02309      IF  LA-GROUPING-A5 NOT EQUAL PI-690-INIT-GROUPING
02310          GO TO 2400-END-BROWSE.
02311
02312      IF  PI-690-INIT-STATE EQUAL LOW-VALUES OR HIGH-VALUES
02313          GO TO 2400-CONTINUE.
02314
02315      IF  LA-STATE-A5 NOT EQUAL PI-690-INIT-STATE
02316          GO TO 2400-END-BROWSE.
02317
02318      IF  PI-690-INIT-ACCOUNT EQUAL LOW-VALUES OR HIGH-VALUES
02319          GO TO 2400-CONTINUE.
02320
02321      IF  LA-ACCOUNT-A5 NOT EQUAL PI-690-INIT-ACCOUNT
02322          GO TO 2400-END-BROWSE.
02323
02324  2400-CONTINUE.
02325
02326      IF  NOT PI-690-SELECT-ALL
02327              AND
02328          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND
02329          PERFORM 7300-READ-ARCH5-FILE-NEXT THRU 7300-EXIT
02330          GO TO 2400-BUILD-ARCH5-LIST.
02331
02332      IF  PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES
02333                  AND
02334          PI-690-INIT-ENTRY LESS THAN HIGH-VALUES
02335          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6
02336              PERFORM 7300-READ-ARCH5-FILE-NEXT THRU 7300-EXIT
02337              GO TO 2400-BUILD-ARCH5-LIST.
02338
02339      IF  PI-690-INIT-FORM NOT EQUAL LOW-VALUES
02340                  AND
02341          PI-690-INIT-FORM LESS THAN HIGH-VALUES
02342          IF  PI-690-INIT-FORM NOT EQUAL LA-FORM-A3
02343              PERFORM 7300-READ-ARCH5-FILE-NEXT THRU 7300-EXIT
02344              GO TO 2400-BUILD-ARCH5-LIST.
02345
02346      IF  PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES
02347                  AND
02348          PI-690-INIT-PROCESSOR LESS THAN HIGH-VALUES
02349          IF  PI-690-INIT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
02350              PERFORM 7300-READ-ARCH5-FILE-NEXT THRU 7300-EXIT
02351              GO TO 2400-BUILD-ARCH5-LIST.
02352
02353      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.
02354
02355      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.
02356
02357      IF  W-ARCH-NDX LESS THAN +13
02358          PERFORM 7300-READ-ARCH5-FILE-NEXT THRU 7300-EXIT
02359          GO TO 2400-BUILD-ARCH5-LIST.
02360
02361  2400-END.
02362
02363      MOVE -1                     TO CORRSELL.
02364      MOVE ZEROS                  TO CORRSELO.
02365      PERFORM 2400-END-BROWSE.
02366      GO TO 8100-SEND-INITIAL-MAP.
02367
02368  2400-END-BROWSE.
02369
02370      
      * EXEC CICS ENDBR
02371 *        DATASET (W-ARCH5-FILE-ID)
02372 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006168' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH5-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02373
02374  2400-ARCH5-NOT-FOUND.
02375
02376      MOVE ER-0130                TO EMI-ERROR.
02377      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02378      MOVE -1                     TO CORRSELL.
02379      MOVE ZEROS                  TO CORRSELO.
02380      GO TO 8100-SEND-INITIAL-MAP.
02381
02382  2400-EXIT.
02383      EXIT.
02384                                  EJECT
02385  2500-ARCH6-PROCESS.
02386
02387      IF  W-CHANGE-REQUESTED
02388          MOVE LOW-VALUES         TO W-ARCH6-KEY
02389          MOVE PI-COMPANY-CD      TO W-ARCH6-COMPANY-CD
02390          MOVE PI-690-INIT-ENTRY  TO W-ARCH6-ENTRY
02391          MOVE PI-690-INIT-ARCHIVE-NO
02392                                  TO W-ARCH6-ARCHIVE-NO
02393
02394      ELSE
02395          IF  W-FIRST-CHANGE-FOUND
02396              MOVE LOW-VALUES     TO W-ARCH6-KEY
02397              MOVE PI-COMPANY-CD  TO W-ARCH6-COMPANY-CD
02398              MOVE PI-690-FIRST-ENTRY
02399                                  TO W-ARCH6-ENTRY
02400              MOVE PI-690-FIRST-ARCHIVE-NO
02401                                  TO W-ARCH6-ARCHIVE-NO
02402
02403          ELSE
02404              MOVE LOW-VALUES     TO W-ARCH6-KEY
02405              MOVE PI-COMPANY-CD  TO W-ARCH6-COMPANY-CD
02406              MOVE PI-690-LAST-ENTRY
02407                                  TO W-ARCH6-ENTRY
02408              MOVE PI-690-LAST-ARCHIVE-NO
02409                                  TO W-ARCH6-ARCHIVE-NO.
02410
02411      
      * EXEC CICS HANDLE CONDITION
02412 *        NOTFND  (8080-ARCH-NOT-FOUND)
02413 *        ENDFILE (8080-ARCH-NOT-FOUND)
02414 *        NOTOPEN (8040-ARCH6-NOT-OPEN)
02415 *        END-EXEC.
      *    MOVE '"$I''J                 ! - #00006209' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303036323039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02416
02417      
      * EXEC CICS STARTBR
02418 *        DATASET  (W-ARCH6-FILE-ID)
02419 *        RIDFLD   (W-ARCH6-KEY)
02420 *        GTEQ
02421 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006215' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH6-FILE-ID, 
                 W-ARCH6-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02422
02423      PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT.
02424
02425      
      * EXEC CICS HANDLE CONDITION
02426 *        NOTFND  (2500-END-BROWSE)
02427 *        ENDFILE (2500-END-BROWSE)
02428 *        END-EXEC.
      *    MOVE '"$I''                  ! . #00006223' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303036323233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02429
02430  2500-BUILD-ARCH6-LIST.
02431
02432      IF  LA-COMPANY-CD-A6 NOT EQUAL PI-COMPANY-CD
02433          GO TO 2500-END-BROWSE.
02434
02435      IF  PI-690-INIT-ENTRY EQUAL LOW-VALUES OR HIGH-VALUES
02436          GO TO 2500-CONTINUE.
02437
02438      IF  LA-ENTRY-A6 NOT EQUAL PI-690-INIT-ENTRY
02439          GO TO 2500-END-BROWSE.
02440
02441  2500-CONTINUE.
02442
02443      IF  NOT PI-690-SELECT-ALL
02444              AND
02445          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND
02446          PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT
02447          GO TO 2500-BUILD-ARCH6-LIST.
02448
02449      IF  PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES
02450              AND
02451          PI-690-INIT-PROCESSOR NOT EQUAL HIGH-VALUES
02452          IF  LA-ENTRY-A6 NOT EQUAL PI-690-INIT-PROCESSOR
02453              PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT
02454              GO TO 2500-BUILD-ARCH6-LIST.
02455
02456      IF  PI-690-INIT-FORM NOT EQUAL LOW-VALUES
02457              AND
02458          PI-690-INIT-FORM NOT EQUAL HIGH-VALUES
02459          IF  LA-FORM-A3 NOT EQUAL PI-690-INIT-FORM
02460              PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT
02461              GO TO 2500-BUILD-ARCH6-LIST.
02462
02463      IF  PI-690-INIT-CARRIER NOT EQUAL LOW-VALUES
02464              AND
02465          PI-690-INIT-CARRIER NOT EQUAL HIGH-VALUES
02466          IF  LA-CARRIER-A2 NOT EQUAL PI-690-INIT-CARRIER
02467              PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT
02468              GO TO 2500-BUILD-ARCH6-LIST.
02469
02470      IF  PI-690-INIT-GROUPING NOT EQUAL LOW-VALUES
02471              AND
02472          PI-690-INIT-GROUPING NOT EQUAL HIGH-VALUES
02473          IF  LA-GROUPING-A2 NOT EQUAL PI-690-INIT-GROUPING
02474              PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT
02475              GO TO 2500-BUILD-ARCH6-LIST.
02476
02477      IF  PI-690-INIT-STATE NOT EQUAL LOW-VALUES
02478              AND
02479          PI-690-INIT-STATE NOT EQUAL HIGH-VALUES
02480          IF  LA-STATE-A2 NOT EQUAL PI-690-INIT-STATE
02481              PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT
02482              GO TO 2500-BUILD-ARCH6-LIST.
02483
02484      IF  PI-690-INIT-ACCOUNT NOT EQUAL LOW-VALUES
02485              AND
02486          PI-690-INIT-ACCOUNT NOT EQUAL HIGH-VALUES
02487          IF  LA-ACCOUNT-A2 NOT EQUAL PI-690-INIT-ACCOUNT
02488              PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT
02489              GO TO 2500-BUILD-ARCH6-LIST.
02490
02491      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.
02492
02493      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.
02494
02495      IF  W-ARCH-NDX LESS THAN +13
02496          PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT
02497          GO TO 2500-BUILD-ARCH6-LIST.
02498
02499  2500-END.
02500
02501      MOVE -1                     TO CORRSELL.
02502      MOVE ZEROS                  TO CORRSELO.
02503      PERFORM 2500-END-BROWSE.
02504      GO TO 8100-SEND-INITIAL-MAP.
02505
02506  2500-END-BROWSE.
02507
02508      
      * EXEC CICS ENDBR
02509 *        DATASET (W-ARCH6-FILE-ID)
02510 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006306' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH6-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02511
02512  2500-ARCH6-NOT-FOUND.
02513
02514      MOVE ER-0130                TO EMI-ERROR.
02515      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02516      MOVE -1                     TO CORRSELL.
02517      MOVE ZEROS                  TO CORRSELO.
02518      GO TO 8100-SEND-INITIAL-MAP.
02519
02520  2500-EXIT.
02521      EXIT.
02522                                  EJECT
02523  2600-ARCH-PROCESS.
02524
02525      IF  W-CHANGE-REQUESTED
02526          MOVE LOW-VALUES         TO W-ARCH-KEY
02527          MOVE PI-COMPANY-CD      TO W-ARCH-COMPANY-CD
02528          MOVE PI-690-INIT-ARCHIVE-NO
02529                                  TO W-ARCH-ARCHIVE-NO
02530
02531      ELSE
02532          IF  W-FIRST-CHANGE-FOUND
02533              MOVE LOW-VALUES     TO W-ARCH-KEY
02534              MOVE PI-COMPANY-CD  TO W-ARCH-COMPANY-CD
02535              MOVE PI-690-FIRST-ARCHIVE-NO
02536                                  TO W-ARCH-ARCHIVE-NO
02537
02538          ELSE
02539              MOVE LOW-VALUES     TO W-ARCH-KEY
02540              MOVE PI-COMPANY-CD  TO W-ARCH-COMPANY-CD
02541              MOVE PI-690-LAST-ARCHIVE-NO
02542                                  TO W-ARCH-ARCHIVE-NO.
02543
02544      
      * EXEC CICS HANDLE CONDITION
02545 *        NOTFND  (8080-ARCH-NOT-FOUND)
02546 *        ENDFILE (8080-ARCH-NOT-FOUND)
02547 *        NOTOPEN (8050-ARCH-NOT-OPEN)
02548 *        END-EXEC.
      *    MOVE '"$I''J                 ! / #00006342' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303036333432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02549
02550      
      * EXEC CICS STARTBR
02551 *        DATASET  (W-ARCH-FILE-ID)
02552 *        RIDFLD   (W-ARCH-KEY)
02553 *        GTEQ
02554 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006348' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02555
02556      PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT.
02557
02558      
      * EXEC CICS HANDLE CONDITION
02559 *        NOTFND  (2600-END-BROWSE)
02560 *        ENDFILE (2600-END-BROWSE)
02561 *        END-EXEC.
      *    MOVE '"$I''                  ! 0 #00006356' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303036333536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02562
02563  2600-BUILD-ARCH-LIST.
02564
02565      IF  LA-COMPANY-CD NOT EQUAL PI-COMPANY-CD
02566          GO TO 2600-END-BROWSE.
02567
02568      IF  PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES
02569              AND
02570          PI-690-INIT-PROCESSOR NOT EQUAL HIGH-VALUES
02571          IF  LA-PROCESSOR-CD NOT EQUAL PI-690-INIT-PROCESSOR
02572              PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT
02573              GO TO 2600-BUILD-ARCH-LIST.
02574
02575      IF  PI-690-INIT-FORM NOT EQUAL LOW-VALUES
02576              AND
02577          PI-690-INIT-FORM NOT EQUAL HIGH-VALUES
02578          IF  LA-FORM-A3 NOT EQUAL PI-690-INIT-FORM
02579              PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT
02580              GO TO 2600-BUILD-ARCH-LIST.
02581
02582      IF  PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES
02583              AND
02584          PI-690-INIT-ENTRY NOT EQUAL HIGH-VALUES
02585          IF  LA-ENTRY-A6 NOT EQUAL PI-690-INIT-ENTRY
02586              PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT
02587              GO TO 2600-BUILD-ARCH-LIST.
02588
02589      IF  PI-690-INIT-CARRIER NOT EQUAL LOW-VALUES
02590              AND
02591          PI-690-INIT-CARRIER NOT EQUAL HIGH-VALUES
02592          IF  LA-CARRIER-A2 NOT EQUAL PI-690-INIT-CARRIER
02593              PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT
02594              GO TO 2600-BUILD-ARCH-LIST.
02595
02596      IF  PI-690-INIT-GROUPING NOT EQUAL LOW-VALUES
02597              AND
02598          PI-690-INIT-GROUPING NOT EQUAL HIGH-VALUES
02599          IF  LA-GROUPING-A2 NOT EQUAL PI-690-INIT-GROUPING
02600              PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT
02601              GO TO 2600-BUILD-ARCH-LIST.
02602
02603      IF  PI-690-INIT-STATE NOT EQUAL LOW-VALUES
02604              AND
02605          PI-690-INIT-STATE NOT EQUAL HIGH-VALUES
02606          IF  LA-STATE-A2 NOT EQUAL PI-690-INIT-STATE
02607              PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT
02608              GO TO 2600-BUILD-ARCH-LIST.
02609
02610      IF  PI-690-INIT-ACCOUNT NOT EQUAL LOW-VALUES
02611              AND
02612          PI-690-INIT-ACCOUNT NOT EQUAL HIGH-VALUES
02613          IF  LA-ACCOUNT-A2 NOT EQUAL PI-690-INIT-ACCOUNT
02614              PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT
02615              GO TO 2600-BUILD-ARCH-LIST.
02616
02617  2600-CONTINUE.
02618
02619      IF  NOT PI-690-SELECT-ALL
02620              AND
02621          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND
02622          PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT
02623          GO TO 2600-BUILD-ARCH-LIST.
02624
02625      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.
02626
02627      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.
02628
02629      IF  W-ARCH-NDX LESS THAN +13
02630          PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT
02631          GO TO 2600-BUILD-ARCH-LIST.
02632
02633  2600-END.
02634
02635      MOVE -1                     TO CORRSELL.
02636      MOVE ZEROS                  TO CORRSELO.
02637      PERFORM 2600-END-BROWSE.
02638      GO TO 8100-SEND-INITIAL-MAP.
02639
02640  2600-END-BROWSE.
02641
02642      
      * EXEC CICS ENDBR
02643 *        DATASET (W-ARCH-FILE-ID)
02644 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006440' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02645
02646  2600-ARCH-NOT-FOUND.
02647
02648      MOVE ER-0130                TO EMI-ERROR.
02649      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02650      MOVE -1                     TO CORRSELL.
02651      MOVE ZEROS                  TO CORRSELO.
02652      GO TO 8100-SEND-INITIAL-MAP.
02653
02654  2600-EXIT.
02655      EXIT.
02656                                  EJECT
02657  3000-PAGE-BACKWARD.
02658
02659      MOVE '2'                    TO PI-690-LAST-BROWSE-IND.
02660      MOVE +1                     TO W-ARCH-NDX
02661                                     W-ARCH-NDX2.
02662      MOVE ZEROS                  TO W-ARCH-NUMBER.
02663
02664      GO TO 3100-ARCH2-PROCESS
02665            3200-ARCH3-PROCESS
02666            3300-ARCH4-PROCESS
02667            3400-ARCH5-PROCESS
02668            3500-ARCH6-PROCESS
02669            3600-ARCH-PROCESS DEPENDING ON PI-690-BRWS-TYPE-IND.
02670
02671      MOVE ER-7384                TO EMI-ERROR.
02672      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02673      MOVE -1                     TO TYPEBRL.
02674      MOVE AL-UABON               TO TYPEBRA.
02675      GO TO 8100-SEND-INITIAL-MAP.
02676
02677  3000-EXIT.
02678      EXIT.
02679                                  EJECT
02680  3100-ARCH2-PROCESS.
02681
02682      IF  W-CHANGE-REQUESTED
02683          MOVE HIGH-VALUES        TO W-ARCH2-KEY
02684          MOVE PI-COMPANY-CD      TO W-ARCH2-COMPANY-CD
02685          MOVE PI-690-INIT-CERT-PRIME
02686                                  TO W-ARCH2-CERT-PRIME
02687          MOVE PI-690-INIT-SUFFIX TO W-ARCH2-SUFFIX
02688          MOVE PI-690-INIT-CARRIER
02689                                  TO W-ARCH2-CARRIER
02690          MOVE PI-690-INIT-GROUPING
02691                                  TO W-ARCH2-GROUPING
02692          MOVE PI-690-INIT-STATE  TO W-ARCH2-STATE
02693          MOVE PI-690-INIT-ACCOUNT
02694                                  TO W-ARCH2-ACCOUNT
02695          MOVE PI-690-INIT-EFFECT-DATE
02696                                  TO W-ARCH2-EFF-DTE
02697
02698
02699      ELSE
02700          IF  W-FIRST-CHANGE-FOUND
02701              MOVE HIGH-VALUES    TO W-ARCH2-KEY
02702              MOVE PI-COMPANY-CD  TO W-ARCH2-COMPANY-CD
02703              MOVE PI-690-LAST-CERT-PRIME
02704                                  TO W-ARCH2-CERT-PRIME
02705              MOVE PI-690-LAST-SUFFIX
02706                                  TO W-ARCH2-SUFFIX
02707              MOVE PI-690-LAST-CARRIER
02708                                  TO W-ARCH2-CARRIER
02709              MOVE PI-690-LAST-GROUPING
02710                                  TO W-ARCH2-GROUPING
02711              MOVE PI-690-LAST-STATE
02712                                  TO W-ARCH2-STATE
02713              MOVE PI-690-LAST-ACCOUNT
02714                                  TO W-ARCH2-ACCOUNT
02715              MOVE PI-690-LAST-EFFECT-DATE
02716                                  TO W-ARCH2-EFF-DTE
02717
02718          ELSE
02719              MOVE PI-COMPANY-CD  TO W-ARCH2-COMPANY-CD
02720              MOVE PI-690-FIRST-CERT-PRIME
02721                                  TO W-ARCH2-CERT-PRIME
02722              MOVE PI-690-FIRST-SUFFIX
02723                                  TO W-ARCH2-SUFFIX
02724              MOVE PI-690-FIRST-CARRIER
02725                                  TO W-ARCH2-CARRIER
02726              MOVE PI-690-FIRST-GROUPING
02727                                  TO W-ARCH2-GROUPING
02728              MOVE PI-690-FIRST-STATE
02729                                  TO W-ARCH2-STATE
02730              MOVE PI-690-FIRST-ACCOUNT
02731                                  TO W-ARCH2-ACCOUNT
02732              MOVE PI-690-FIRST-EFFECT-DATE
02733                                  TO W-ARCH2-EFF-DTE
02734              MOVE PI-690-FIRST-ARCHIVE-NO
02735                                  TO W-ARCH2-ARCHIVE-NO.
02736
02737      
      * EXEC CICS HANDLE CONDITION
02738 *        NOTFND  (8080-ARCH-NOT-FOUND)
02739 *        ENDFILE (8080-ARCH-NOT-FOUND)
02740 *        NOTOPEN (8000-ARCH2-NOT-OPEN)
02741 *        END-EXEC.
      *    MOVE '"$I''J                 ! 1 #00006535' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303036353335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02742
02743      
      * EXEC CICS STARTBR
02744 *        DATASET  (W-ARCH2-FILE-ID)
02745 *        RIDFLD   (W-ARCH2-KEY)
02746 *        GTEQ
02747 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006541' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH2-FILE-ID, 
                 W-ARCH2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02748
02749      
      * EXEC CICS HANDLE CONDITION
02750 *        NOTFND  (3100-END-BROWSE)
02751 *        ENDFILE (3100-END-BROWSE)
02752 *        END-EXEC.
      *    MOVE '"$I''                  ! 2 #00006547' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303036353437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02753
02754      PERFORM 7000-READ-ARCH2-FILE-NEXT THRU 7000-EXIT.
02755
02756      IF  W-CHANGE-REQUESTED
02757              OR
02758          W-READ-PREV-TWICE
02759          MOVE SPACES            TO W-READ-PREV-TWICE-IND
02760          PERFORM 7050-READ-PREV-ARCH2 THRU 7050-EXIT.
02761
02762  3100-BUILD-ARCH2-LIST-BACKWARD.
02763
02764      PERFORM 7050-READ-PREV-ARCH2 THRU 7050-EXIT.
02765
02766      IF  LA-COMPANY-CD-A2 NOT EQUAL PI-COMPANY-CD
02767          GO TO 3100-END.
02768
02769      IF  PI-690-INIT-CERT-NO EQUAL HIGH-VALUES OR LOW-VALUES
02770          GO TO 3100-CONTINUE.
02771
02772      IF  LA-CERT-NO-A2 NOT EQUAL PI-690-INIT-CERT-NO
02773          GO TO 3100-END-BROWSE.
02774
02775      IF  PI-690-INIT-CARRIER EQUAL HIGH-VALUES OR LOW-VALUES
02776          GO TO 3100-CONTINUE.
02777
02778      IF  LA-CARRIER-A2 NOT EQUAL PI-690-INIT-CARRIER
02779          GO TO 3100-END-BROWSE.
02780
02781      IF  PI-690-INIT-GROUPING EQUAL HIGH-VALUES OR LOW-VALUES
02782          GO TO 3100-CONTINUE.
02783
02784      IF  LA-GROUPING-A2 NOT EQUAL PI-690-INIT-GROUPING
02785          GO TO 3100-END-BROWSE.
02786
02787      IF  PI-690-INIT-STATE EQUAL HIGH-VALUES OR LOW-VALUES
02788          GO TO 3100-CONTINUE.
02789
02790      IF  LA-STATE-A2 NOT EQUAL PI-690-INIT-STATE
02791          GO TO 3100-END-BROWSE.
02792
02793      IF  PI-690-INIT-ACCOUNT EQUAL HIGH-VALUES OR LOW-VALUES
02794          GO TO 3100-CONTINUE.
02795
02796      IF  LA-ACCOUNT-A2 NOT EQUAL PI-690-INIT-ACCOUNT
02797          GO TO 3100-END-BROWSE.
02798
101512     IF  PI-690-INIT-EFFECT-DATE = LOW-VALUES OR HIGH-VALUES
101512         GO TO 3100-CONTINUE.
101512
101512     IF  LA-EFFECT-DATE-A2 NOT EQUAL PI-690-INIT-EFFECT-DATE
101512         GO TO 3100-END-BROWSE.
101512
02799  3100-CONTINUE.
02800
02801      IF  NOT PI-690-SELECT-ALL
02802              AND
02803          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND
02804          GO TO 3100-BUILD-ARCH2-LIST-BACKWARD.
02805
02806      IF  PI-690-INIT-ENTRY NOT EQUAL HIGH-VALUES
02807              AND
02808          PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES
02809          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6
02810              GO TO 3100-BUILD-ARCH2-LIST-BACKWARD.
02811
02812      IF  PI-690-INIT-FORM NOT EQUAL HIGH-VALUES
02813              AND
02814          PI-690-INIT-FORM NOT EQUAL LOW-VALUES
02815          IF  PI-690-INIT-FORM NOT EQUAL LA-FORM-A3
02816              GO TO 3100-BUILD-ARCH2-LIST-BACKWARD.
02817
02818      IF  PI-690-INIT-PROCESSOR NOT EQUAL HIGH-VALUES
02819              AND
02820          PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES
02821          IF  PI-690-INIT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
02822              GO TO 3100-BUILD-ARCH2-LIST-BACKWARD.
02823
02824      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.
02825
02826      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.
02827
02828      IF  W-ARCH-NDX LESS THAN +13
02829          GO TO 3100-BUILD-ARCH2-LIST-BACKWARD.
02830
02831  3100-END.
02832
02833      MOVE -1                     TO CORRSELL.
02834      MOVE ZEROS                  TO CORRSELO.
02835      PERFORM 3100-END-BROWSE
02836      GO TO 8100-SEND-INITIAL-MAP.
02837
02838  3100-END-BROWSE.
02839
02840      MOVE W-ARCH-NDX             TO W-ARCH-NDX3.
02841      SUBTRACT +1 FROM W-ARCH-NDX3.
02842
02843      IF  W-ARCH-NDX3 GREATER THAN +1
02844          MOVE +1 TO W-ARCH-NDX2
02845          PERFORM 6500-REVERSE-TABLE-DATA THRU 6500-EXIT
02846                  UNTIL
02847              W-ARCH-NDX2 NOT LESS W-ARCH-NDX3.
02848
02849      
      * EXEC CICS ENDBR
02850 *        DATASET (W-ARCH2-FILE-ID)
02851 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006653' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH2-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02852
02853  3100-FRONT-OF-FILE.
02854
02855      MOVE ER-0131                TO EMI-ERROR.
02856      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02857      MOVE -1                     TO CORRSELL.
02858      MOVE ZEROS                  TO CORRSELO.
02859      GO TO 8100-SEND-INITIAL-MAP.
02860
02861  3100-EXIT.
02862      EXIT.
02863                                  EJECT
02864  3200-ARCH3-PROCESS.
02865
02866      IF  W-CHANGE-REQUESTED
02867          MOVE HIGH-VALUES        TO W-ARCH3-KEY
02868          MOVE PI-COMPANY-CD      TO W-ARCH3-COMPANY-CD
02869          MOVE PI-690-INIT-FORM   TO W-ARCH3-FORM
02870          MOVE PI-690-INIT-CARRIER
02871                                  TO W-ARCH3-CARRIER
02872          MOVE PI-690-INIT-GROUPING
02873                                  TO W-ARCH3-GROUPING
02874          MOVE PI-690-INIT-STATE  TO W-ARCH3-STATE
02875          MOVE PI-690-INIT-ACCOUNT
02876                                  TO W-ARCH3-ACCOUNT
02877
02878      ELSE
02879          IF  W-FIRST-CHANGE-FOUND
02880              MOVE HIGH-VALUES    TO W-ARCH3-KEY
02881              MOVE PI-COMPANY-CD  TO W-ARCH3-COMPANY-CD
02882              MOVE PI-690-LAST-FORM
02883                                  TO W-ARCH3-FORM
02884              MOVE PI-690-LAST-CARRIER
02885                                  TO W-ARCH3-CARRIER
02886              MOVE PI-690-LAST-GROUPING
02887                                  TO W-ARCH3-GROUPING
02888              MOVE PI-690-LAST-STATE
02889                                  TO W-ARCH3-STATE
02890              MOVE PI-690-LAST-ACCOUNT
02891                                  TO W-ARCH3-ACCOUNT
02892
02893          ELSE
02894              MOVE PI-COMPANY-CD  TO W-ARCH3-COMPANY-CD
02895              MOVE PI-690-FIRST-FORM
02896                                  TO W-ARCH3-FORM
02897              MOVE PI-690-FIRST-CARRIER
02898                                  TO W-ARCH3-CARRIER
02899              MOVE PI-690-FIRST-GROUPING
02900                                  TO W-ARCH3-GROUPING
02901              MOVE PI-690-FIRST-STATE
02902                                  TO W-ARCH3-STATE
02903              MOVE PI-690-FIRST-ACCOUNT
02904                                  TO W-ARCH3-ACCOUNT
02905              MOVE PI-690-FIRST-ARCHIVE-NO
02906                                  TO W-ARCH3-ARCHIVE-NO.
02907
02908      
      * EXEC CICS HANDLE CONDITION
02909 *        NOTFND  (8080-ARCH-NOT-FOUND)
02910 *        ENDFILE (8080-ARCH-NOT-FOUND)
02911 *        NOTOPEN (8010-ARCH3-NOT-OPEN)
02912 *        END-EXEC.
      *    MOVE '"$I''J                 ! 3 #00006712' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303036373132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02913
02914      
      * EXEC CICS STARTBR
02915 *        DATASET  (W-ARCH3-FILE-ID)
02916 *        RIDFLD   (W-ARCH3-KEY)
02917 *        GTEQ
02918 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006718' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH3-FILE-ID, 
                 W-ARCH3-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02919
02920      
      * EXEC CICS HANDLE CONDITION
02921 *        NOTFND  (3200-END-BROWSE)
02922 *        ENDFILE (3200-END-BROWSE)
02923 *        END-EXEC.
      *    MOVE '"$I''                  ! 4 #00006724' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303036373234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02924
02925      PERFORM 7100-READ-ARCH3-FILE-NEXT THRU 7100-EXIT.
02926
02927      IF  W-CHANGE-REQUESTED
02928              OR
02929          W-READ-PREV-TWICE
02930          MOVE SPACES            TO W-READ-PREV-TWICE-IND
02931          PERFORM 7150-READ-PREV-ARCH3 THRU 7150-EXIT.
02932
02933  3200-BUILD-ARCH3-LIST-BACKWARD.
02934
02935      PERFORM 7150-READ-PREV-ARCH3 THRU 7150-EXIT.
02936
02937      IF  LA-COMPANY-CD-A3 NOT EQUAL PI-COMPANY-CD
02938          GO TO 3200-END.
02939
02940      IF  PI-690-INIT-FORM EQUAL HIGH-VALUES OR LOW-VALUES
02941          GO TO 3200-CONTINUE.
02942
02943      IF  LA-FORM-A3 NOT EQUAL PI-690-INIT-FORM
02944          GO TO 3200-END-BROWSE.
02945
02946      IF  PI-690-INIT-CARRIER EQUAL HIGH-VALUES OR LOW-VALUES
02947          GO TO 3200-CONTINUE.
02948
02949      IF  LA-CARRIER-A3 NOT EQUAL PI-690-INIT-CARRIER
02950          GO TO 3200-END-BROWSE.
02951
02952      IF  PI-690-INIT-GROUPING EQUAL HIGH-VALUES OR LOW-VALUES
02953          GO TO 3200-CONTINUE.
02954
02955      IF  LA-GROUPING-A3 NOT EQUAL PI-690-INIT-GROUPING
02956          GO TO 3200-END-BROWSE.
02957
02958      IF  PI-690-INIT-STATE EQUAL HIGH-VALUES OR LOW-VALUES
02959          GO TO 3200-CONTINUE.
02960
02961      IF  LA-STATE-A3 NOT EQUAL PI-690-INIT-STATE
02962          GO TO 3200-END-BROWSE.
02963
02964      IF  PI-690-INIT-ACCOUNT EQUAL HIGH-VALUES OR LOW-VALUES
02965          GO TO 3200-CONTINUE.
02966
02967      IF  LA-ACCOUNT-A3 NOT EQUAL PI-690-INIT-ACCOUNT
02968          GO TO 3200-END-BROWSE.
02969
02970  3200-CONTINUE.
02971
02972      IF  NOT PI-690-SELECT-ALL
02973              AND
02974          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND
02975          GO TO 3200-BUILD-ARCH3-LIST-BACKWARD.
02976
02977      IF  PI-690-INIT-ENTRY NOT EQUAL HIGH-VALUES
02978              AND
02979          PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES
02980          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6
02981              GO TO 3200-BUILD-ARCH3-LIST-BACKWARD.
02982
02983      IF  PI-690-INIT-PROCESSOR NOT EQUAL HIGH-VALUES
02984              AND
02985          PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES
02986          IF  PI-690-INIT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
02987              GO TO 3200-BUILD-ARCH3-LIST-BACKWARD.
02988
02989      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.
02990
02991      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.
02992
02993      IF  W-ARCH-NDX LESS THAN +13
02994          GO TO 3200-BUILD-ARCH3-LIST-BACKWARD.
02995
02996  3200-END.
02997
02998      MOVE -1                     TO CORRSELL.
02999      MOVE ZEROS                  TO CORRSELO.
03000      PERFORM 3200-END-BROWSE.
03001      GO TO 8100-SEND-INITIAL-MAP.
03002
03003  3200-END-BROWSE.
03004
03005      MOVE W-ARCH-NDX             TO W-ARCH-NDX3.
03006      SUBTRACT +1 FROM W-ARCH-NDX3.
03007
03008      IF  W-ARCH-NDX3 GREATER THAN +1
03009          MOVE +1 TO W-ARCH-NDX2
03010          PERFORM 6500-REVERSE-TABLE-DATA THRU 6500-EXIT
03011                  UNTIL
03012              W-ARCH-NDX2 NOT LESS W-ARCH-NDX3.
03013
03014      
      * EXEC CICS ENDBR
03015 *        DATASET (W-ARCH3-FILE-ID)
03016 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006818' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH3-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03017
03018  3200-FRONT-OF-FILE.
03019
03020      MOVE ER-0131                TO EMI-ERROR.
03021      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03022      MOVE -1                     TO CORRSELL.
03023      MOVE ZEROS                  TO CORRSELO.
03024      GO TO 8100-SEND-INITIAL-MAP.
03025
03026  3200-EXIT.
03027      EXIT.
03028                                  EJECT
03029  3300-ARCH4-PROCESS.
03030
03031      IF  W-CHANGE-REQUESTED
03032          MOVE HIGH-VALUES        TO W-ARCH4-KEY
03033          MOVE PI-COMPANY-CD      TO W-ARCH4-COMPANY-CD
03034          MOVE PI-690-INIT-PROCESSOR
03035                                  TO W-ARCH4-PROCESSOR
03036          MOVE PI-690-INIT-CARRIER
03037                                  TO W-ARCH4-CARRIER
03038          MOVE PI-690-INIT-GROUPING
03039                                  TO W-ARCH4-GROUPING
03040          MOVE PI-690-INIT-STATE  TO W-ARCH4-STATE
03041          MOVE PI-690-INIT-ACCOUNT
03042                                  TO W-ARCH4-ACCOUNT
03043
03044      ELSE
03045          IF  W-FIRST-CHANGE-FOUND
03046              MOVE HIGH-VALUES    TO W-ARCH4-KEY
03047              MOVE PI-COMPANY-CD  TO W-ARCH4-COMPANY-CD
03048              MOVE PI-690-LAST-PROCESSOR
03049                                  TO W-ARCH4-PROCESSOR
03050              MOVE PI-690-LAST-CARRIER
03051                                  TO W-ARCH4-CARRIER
03052              MOVE PI-690-LAST-GROUPING
03053                                  TO W-ARCH4-GROUPING
03054              MOVE PI-690-LAST-STATE
03055                                  TO W-ARCH4-STATE
03056              MOVE PI-690-LAST-ACCOUNT
03057                                  TO W-ARCH4-ACCOUNT
03058
03059          ELSE
03060              MOVE PI-COMPANY-CD  TO W-ARCH4-COMPANY-CD
03061              MOVE PI-690-FIRST-PROCESSOR
03062                                  TO W-ARCH4-PROCESSOR
03063              MOVE PI-690-FIRST-CARRIER
03064                                  TO W-ARCH4-CARRIER
03065              MOVE PI-690-FIRST-GROUPING
03066                                  TO W-ARCH4-GROUPING
03067              MOVE PI-690-FIRST-STATE
03068                                  TO W-ARCH4-STATE
03069              MOVE PI-690-FIRST-ACCOUNT
03070                                  TO W-ARCH4-ACCOUNT
03071              MOVE PI-690-FIRST-ARCHIVE-NO
03072                                  TO W-ARCH4-ARCHIVE-NO.
03073
03074      
      * EXEC CICS HANDLE CONDITION
03075 *        NOTFND  (8080-ARCH-NOT-FOUND)
03076 *        ENDFILE (8080-ARCH-NOT-FOUND)
03077 *        NOTOPEN (8020-ARCH4-NOT-OPEN)
03078 *        END-EXEC.
      *    MOVE '"$I''J                 ! 5 #00006878' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303036383738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03079
03080      
      * EXEC CICS STARTBR
03081 *        DATASET  (W-ARCH4-FILE-ID)
03082 *        RIDFLD   (W-ARCH4-KEY)
03083 *        GTEQ
03084 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006884' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH4-FILE-ID, 
                 W-ARCH4-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03085
03086      
      * EXEC CICS HANDLE CONDITION
03087 *        NOTFND  (3300-END-BROWSE)
03088 *        ENDFILE (3300-END-BROWSE)
03089 *        END-EXEC.
      *    MOVE '"$I''                  ! 6 #00006890' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3620233030303036383930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03090
03091      PERFORM 7200-READ-ARCH4-FILE-NEXT THRU 7200-EXIT.
03092
03093      IF  W-CHANGE-REQUESTED
03094              OR
03095          W-READ-PREV-TWICE
03096          MOVE SPACES            TO W-READ-PREV-TWICE-IND
03097          PERFORM 7250-READ-PREV-ARCH4 THRU 7250-EXIT.
03098
03099  3300-BUILD-ARCH4-LIST-BACKWARD.
03100
03101      PERFORM 7250-READ-PREV-ARCH4 THRU 7250-EXIT.
03102
03103      IF  LA-COMPANY-CD-A3 NOT EQUAL PI-COMPANY-CD
03104          GO TO 3300-END.
03105
03106      IF  PI-690-INIT-PROCESSOR EQUAL HIGH-VALUES OR LOW-VALUES
03107          GO TO 3300-CONTINUE.
03108
03109      IF  LA-PROCESSOR-CD NOT EQUAL PI-690-INIT-PROCESSOR
03110          GO TO 3300-END-BROWSE.
03111
03112      IF  PI-690-INIT-CARRIER EQUAL HIGH-VALUES OR LOW-VALUES
03113          GO TO 3300-CONTINUE.
03114
03115      IF  LA-CARRIER-A4 NOT EQUAL PI-690-INIT-CARRIER
03116          GO TO 3300-END-BROWSE.
03117
03118      IF  PI-690-INIT-GROUPING EQUAL HIGH-VALUES OR LOW-VALUES
03119          GO TO 3300-CONTINUE.
03120
03121      IF  LA-GROUPING-A4 NOT EQUAL PI-690-INIT-GROUPING
03122          GO TO 3300-END-BROWSE.
03123
03124      IF  PI-690-INIT-STATE EQUAL HIGH-VALUES OR LOW-VALUES
03125          GO TO 3300-CONTINUE.
03126
03127      IF  LA-STATE-A4 NOT EQUAL PI-690-INIT-STATE
03128          GO TO 3300-END-BROWSE.
03129
03130      IF  PI-690-INIT-ACCOUNT EQUAL HIGH-VALUES OR LOW-VALUES
03131          GO TO 3300-CONTINUE.
03132
03133      IF  LA-ACCOUNT-A4 NOT EQUAL PI-690-INIT-ACCOUNT
03134          GO TO 3300-END-BROWSE.
03135
03136  3300-CONTINUE.
03137
03138      IF  NOT PI-690-SELECT-ALL
03139              AND
03140          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND
03141          GO TO 3300-BUILD-ARCH4-LIST-BACKWARD.
03142
03143      IF  PI-690-INIT-ENTRY NOT EQUAL HIGH-VALUES
03144              AND
03145          PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES
03146          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6
03147              GO TO 3300-BUILD-ARCH4-LIST-BACKWARD.
03148
03149      IF  PI-690-INIT-FORM NOT EQUAL HIGH-VALUES
03150              AND
03151          PI-690-INIT-FORM NOT EQUAL LOW-VALUES
03152          IF  PI-690-INIT-FORM NOT EQUAL LA-FORM-A3
03153              GO TO 3300-BUILD-ARCH4-LIST-BACKWARD.
03154
03155      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.
03156
03157      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.
03158
03159      IF  W-ARCH-NDX LESS THAN +13
03160          GO TO 3300-BUILD-ARCH4-LIST-BACKWARD.
03161
03162  3300-END.
03163
03164      MOVE -1                     TO CORRSELL.
03165      MOVE ZEROS                  TO CORRSELO.
03166      PERFORM 3300-END-BROWSE
03167      GO TO 8100-SEND-INITIAL-MAP.
03168
03169  3300-END-BROWSE.
03170
03171      MOVE W-ARCH-NDX             TO W-ARCH-NDX3.
03172      SUBTRACT +1 FROM W-ARCH-NDX3.
03173
03174      IF  W-ARCH-NDX3 GREATER THAN +1
03175          MOVE +1 TO W-ARCH-NDX2
03176          PERFORM 6500-REVERSE-TABLE-DATA THRU 6500-EXIT
03177                  UNTIL
03178              W-ARCH-NDX2 NOT LESS W-ARCH-NDX3.
03179
03180      
      * EXEC CICS ENDBR
03181 *        DATASET (W-ARCH4-FILE-ID)
03182 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006984' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH4-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03183
03184  3300-FRONT-OF-FILE.
03185
03186      MOVE ER-0131                TO EMI-ERROR.
03187      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03188      MOVE -1                     TO CORRSELL.
03189      MOVE ZEROS                  TO CORRSELO.
03190      GO TO 8100-SEND-INITIAL-MAP.
03191
03192  3300-EXIT.
03193      EXIT.
03194                                  EJECT
03195  3400-ARCH5-PROCESS.
03196
03197      IF  W-CHANGE-REQUESTED
03198          MOVE HIGH-VALUES        TO W-ARCH5-KEY
03199          MOVE PI-COMPANY-CD      TO W-ARCH5-COMPANY-CD
03200          MOVE PI-690-INIT-CARRIER
03201                                  TO W-ARCH5-CARRIER
03202          MOVE PI-690-INIT-GROUPING
03203                                  TO W-ARCH5-GROUPING
03204          MOVE PI-690-INIT-STATE  TO W-ARCH5-STATE
03205          MOVE PI-690-INIT-ACCOUNT
03206                                  TO W-ARCH5-ACCOUNT
03207
03208      ELSE
03209          IF  W-FIRST-CHANGE-FOUND
03210              MOVE SPACES         TO W-READ-PREV-TWICE-IND
03211              MOVE HIGH-VALUES    TO W-ARCH5-KEY
03212              MOVE PI-COMPANY-CD  TO W-ARCH5-COMPANY-CD
03213              MOVE PI-690-LAST-CARRIER
03214                                  TO W-ARCH5-CARRIER
03215              MOVE PI-690-LAST-GROUPING
03216                                  TO W-ARCH5-GROUPING
03217              MOVE PI-690-LAST-STATE
03218                                  TO W-ARCH5-STATE
03219              MOVE PI-690-LAST-ACCOUNT
03220                                  TO W-ARCH5-ACCOUNT
03221              MOVE PI-690-LAST-ARCHIVE-NO
03222                                  TO W-ARCH5-ARCHIVE-NO
03223
03224          ELSE
03225              MOVE PI-COMPANY-CD  TO W-ARCH5-COMPANY-CD
03226              MOVE PI-690-FIRST-CARRIER
03227                                  TO W-ARCH5-CARRIER
03228              MOVE PI-690-FIRST-GROUPING
03229                                  TO W-ARCH5-GROUPING
03230              MOVE PI-690-FIRST-STATE
03231                                  TO W-ARCH5-STATE
03232              MOVE PI-690-FIRST-ACCOUNT
03233                                  TO W-ARCH5-ACCOUNT
03234              MOVE PI-690-FIRST-ARCHIVE-NO
03235                                  TO W-ARCH5-ARCHIVE-NO.
03236
03237      
      * EXEC CICS HANDLE CONDITION
03238 *        NOTFND  (8080-ARCH-NOT-FOUND)
03239 *        ENDFILE (8080-ARCH-NOT-FOUND)
03240 *        NOTOPEN (8030-ARCH5-NOT-OPEN)
03241 *        END-EXEC.
      *    MOVE '"$I''J                 ! 7 #00007041' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3720233030303037303431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03242
03243      
      * EXEC CICS STARTBR
03244 *        DATASET  (W-ARCH5-FILE-ID)
03245 *        RIDFLD   (W-ARCH5-KEY)
03246 *        GTEQ
03247 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007047' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH5-FILE-ID, 
                 W-ARCH5-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03248
03249      
      * EXEC CICS HANDLE CONDITION
03250 *        NOTFND  (3400-END-BROWSE)
03251 *        ENDFILE (3400-END-BROWSE)
03252 *        END-EXEC.
      *    MOVE '"$I''                  ! 8 #00007053' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3820233030303037303533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03253
03254      PERFORM 7300-READ-ARCH5-FILE-NEXT THRU 7300-EXIT.
03255
03256      IF  W-CHANGE-REQUESTED
03257 *            OR
03258 *        W-READ-PREV-TWICE
03259 *        MOVE SPACES            TO W-READ-PREV-TWICE-IND
03260          PERFORM 7350-READ-PREV-ARCH5 THRU 7350-EXIT.
03261
03262  3400-BUILD-ARCH5-LIST-BACKWARD.
03263
03264      PERFORM 7350-READ-PREV-ARCH5 THRU 7350-EXIT.
03265
03266      IF  LA-COMPANY-CD-A5 NOT EQUAL PI-COMPANY-CD
03267          GO TO 3400-END.
03268
03269      IF  PI-690-INIT-CARRIER EQUAL HIGH-VALUES OR LOW-VALUES
03270          GO TO 3400-CONTINUE.
03271
03272      IF  LA-CARRIER-A5 NOT EQUAL PI-690-INIT-CARRIER
03273          GO TO 3400-END-BROWSE.
03274
03275      IF  PI-690-INIT-GROUPING EQUAL HIGH-VALUES OR LOW-VALUES
03276          GO TO 3400-CONTINUE.
03277
03278      IF  LA-GROUPING-A5 NOT EQUAL PI-690-INIT-GROUPING
03279          GO TO 3400-END-BROWSE.
03280
03281      IF  PI-690-INIT-STATE EQUAL HIGH-VALUES OR LOW-VALUES
03282          GO TO 3400-CONTINUE.
03283
03284      IF  LA-STATE-A5 NOT EQUAL PI-690-INIT-STATE
03285          GO TO 3400-END-BROWSE.
03286
03287      IF  PI-690-INIT-ACCOUNT EQUAL HIGH-VALUES OR LOW-VALUES
03288          GO TO 3400-CONTINUE.
03289
03290      IF  LA-ACCOUNT-A5 NOT EQUAL PI-690-INIT-ACCOUNT
03291          GO TO 3400-END-BROWSE.
03292
03293  3400-CONTINUE.
03294
03295      IF  NOT PI-690-SELECT-ALL
03296              AND
03297          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND
03298          GO TO 3400-BUILD-ARCH5-LIST-BACKWARD.
03299
03300      IF  PI-690-INIT-ENTRY NOT EQUAL HIGH-VALUES
03301              AND
03302          PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES
03303          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6
03304              GO TO 3400-BUILD-ARCH5-LIST-BACKWARD.
03305
03306      IF  PI-690-INIT-PROCESSOR NOT EQUAL HIGH-VALUES
03307              AND
03308          PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES
03309          IF  PI-690-INIT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
03310              GO TO 3400-BUILD-ARCH5-LIST-BACKWARD.
03311
03312      IF  PI-690-INIT-FORM NOT EQUAL HIGH-VALUES
03313              AND
03314          PI-690-INIT-FORM NOT EQUAL LOW-VALUES
03315          IF  PI-690-INIT-FORM NOT EQUAL LA-FORM-A3
03316              GO TO 3400-BUILD-ARCH5-LIST-BACKWARD.
03317
03318      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.
03319
03320      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.
03321
03322      IF  W-ARCH-NDX LESS THAN +13
03323          GO TO 3400-BUILD-ARCH5-LIST-BACKWARD.
03324
03325  3400-END.
03326
03327      MOVE -1                     TO CORRSELL.
03328      MOVE ZEROS                  TO CORRSELO.
03329      PERFORM 3400-END-BROWSE
03330      GO TO 8100-SEND-INITIAL-MAP.
03331
03332  3400-END-BROWSE.
03333
03334      MOVE W-ARCH-NDX             TO W-ARCH-NDX3.
03335      SUBTRACT +1 FROM W-ARCH-NDX3.
03336
03337      IF  W-ARCH-NDX3 GREATER THAN +1
03338          MOVE +1 TO W-ARCH-NDX2
03339          PERFORM 6500-REVERSE-TABLE-DATA THRU 6500-EXIT
03340                  UNTIL
03341              W-ARCH-NDX2 NOT LESS W-ARCH-NDX3.
03342
03343      
      * EXEC CICS ENDBR
03344 *        DATASET (W-ARCH5-FILE-ID)
03345 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007147' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH5-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03346
03347  3400-FRONT-OF-FILE.
03348
03349      MOVE ER-0131                TO EMI-ERROR.
03350      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03351      MOVE -1                     TO CORRSELL.
03352      MOVE ZEROS                  TO CORRSELO.
03353      GO TO 8100-SEND-INITIAL-MAP.
03354
03355  3400-EXIT.
03356      EXIT.
03357                                  EJECT
03358  3500-ARCH6-PROCESS.
03359
03360      IF  W-CHANGE-REQUESTED
03361          MOVE HIGH-VALUES        TO W-ARCH6-KEY
03362          MOVE PI-COMPANY-CD      TO W-ARCH6-COMPANY-CD
03363          MOVE PI-690-INIT-ENTRY  TO W-ARCH6-ENTRY
03364
03365
03366      ELSE
03367          IF  W-FIRST-CHANGE-FOUND
03368              MOVE HIGH-VALUES    TO W-ARCH6-KEY
03369              MOVE PI-COMPANY-CD  TO W-ARCH6-COMPANY-CD
03370              MOVE PI-690-LAST-ENTRY
03371                                  TO W-ARCH6-ENTRY
03372
03373          ELSE
03374              MOVE PI-COMPANY-CD  TO W-ARCH6-COMPANY-CD
03375              MOVE PI-690-FIRST-ENTRY
03376                                  TO W-ARCH6-ENTRY
03377              MOVE PI-690-FIRST-ARCHIVE-NO
03378                                  TO W-ARCH6-ARCHIVE-NO.
03379
03380      
      * EXEC CICS HANDLE CONDITION
03381 *        NOTFND  (8080-ARCH-NOT-FOUND)
03382 *        ENDFILE (8080-ARCH-NOT-FOUND)
03383 *        NOTOPEN (8040-ARCH6-NOT-OPEN)
03384 *        END-EXEC.
      *    MOVE '"$I''J                 ! 9 #00007184' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3920233030303037313834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03385
03386      
      * EXEC CICS STARTBR
03387 *        DATASET  (W-ARCH6-FILE-ID)
03388 *        RIDFLD   (W-ARCH6-KEY)
03389 *        GTEQ
03390 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007190' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH6-FILE-ID, 
                 W-ARCH6-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03391
03392      
      * EXEC CICS HANDLE CONDITION
03393 *        NOTFND  (3500-END-BROWSE)
03394 *        ENDFILE (3500-END-BROWSE)
03395 *        END-EXEC.
      *    MOVE '"$I''                  ! : #00007196' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3A20233030303037313936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03396
03397      PERFORM 7400-READ-ARCH6-FILE-NEXT THRU 7400-EXIT.
03398
03399      IF  W-CHANGE-REQUESTED
03400              OR
03401          W-READ-PREV-TWICE
03402          MOVE SPACES            TO W-READ-PREV-TWICE-IND
03403          PERFORM 7450-READ-PREV-ARCH6 THRU 7450-EXIT.
03404
03405  3500-BUILD-ARCH6-LIST-BACKWARD.
03406
03407      PERFORM 7450-READ-PREV-ARCH6 THRU 7450-EXIT.
03408
03409      IF  LA-COMPANY-CD-A5 NOT EQUAL PI-COMPANY-CD
03410          GO TO 3500-END.
03411
03412      IF  PI-690-INIT-ENTRY EQUAL HIGH-VALUES OR LOW-VALUES
03413          GO TO 3500-CONTINUE.
03414
03415      IF  LA-ENTRY-A6 NOT EQUAL PI-690-INIT-ENTRY
03416          GO TO 3500-END-BROWSE.
03417
03418  3500-CONTINUE.
03419
03420      IF  NOT PI-690-SELECT-ALL
03421              AND
03422          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND
03423          GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.
03424
03425      IF  PI-690-INIT-FORM NOT EQUAL HIGH-VALUES
03426              AND
03427          PI-690-INIT-FORM NOT EQUAL LOW-VALUES
03428          IF  LA-FORM-A3 NOT EQUAL PI-690-INIT-FORM
03429              GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.
03430
03431      IF  PI-690-INIT-PROCESSOR NOT EQUAL HIGH-VALUES
03432              AND
03433          PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES
03434          IF  LA-PROCESSOR-CD NOT EQUAL PI-690-INIT-PROCESSOR
03435              GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.
03436
03437      IF  PI-690-INIT-CARRIER NOT EQUAL HIGH-VALUES
03438              AND
03439          PI-690-INIT-CARRIER NOT EQUAL LOW-VALUES
03440          IF  LA-CARRIER-A2 NOT EQUAL PI-690-INIT-CARRIER
03441              GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.
03442
03443      IF  PI-690-INIT-GROUPING NOT EQUAL HIGH-VALUES
03444              AND
03445          PI-690-INIT-GROUPING NOT EQUAL LOW-VALUES
03446          IF  LA-GROUPING-A2 NOT EQUAL PI-690-INIT-GROUPING
03447              GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.
03448
03449      IF  PI-690-INIT-STATE NOT EQUAL HIGH-VALUES
03450              AND
03451          PI-690-INIT-STATE NOT EQUAL LOW-VALUES
03452          IF  LA-STATE-A2 NOT EQUAL PI-690-INIT-STATE
03453              GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.
03454
03455      IF  PI-690-INIT-ACCOUNT NOT EQUAL HIGH-VALUES
03456              AND
03457          PI-690-INIT-ACCOUNT NOT EQUAL LOW-VALUES
03458          IF  LA-ACCOUNT-A2 NOT EQUAL PI-690-INIT-ACCOUNT
03459              GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.
03460
03461      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.
03462
03463      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.
03464
03465      IF  W-ARCH-NDX LESS THAN +13
03466          GO TO 3500-BUILD-ARCH6-LIST-BACKWARD.
03467
03468  3500-END.
03469
03470      MOVE -1                     TO CORRSELL.
03471      MOVE ZEROS                  TO CORRSELO.
03472      PERFORM 3500-END-BROWSE
03473      GO TO 8100-SEND-INITIAL-MAP.
03474
03475  3500-END-BROWSE.
03476
03477      MOVE W-ARCH-NDX             TO W-ARCH-NDX3.
03478      SUBTRACT +1 FROM W-ARCH-NDX3.
03479
03480      IF  W-ARCH-NDX3 GREATER THAN +1
03481          MOVE +1 TO W-ARCH-NDX2
03482          PERFORM 6500-REVERSE-TABLE-DATA THRU 6500-EXIT
03483                  UNTIL
03484              W-ARCH-NDX2 NOT LESS W-ARCH-NDX3.
03485
03486      
      * EXEC CICS ENDBR
03487 *        DATASET (W-ARCH6-FILE-ID)
03488 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007290' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH6-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03489
03490  3500-FRONT-OF-FILE.
03491
03492      MOVE ER-0131                TO EMI-ERROR.
03493      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03494      MOVE -1                     TO CORRSELL.
03495      MOVE ZEROS                  TO CORRSELO.
03496      GO TO 8100-SEND-INITIAL-MAP.
03497
03498  3500-EXIT.
03499      EXIT.
03500                                  EJECT
03501  3600-ARCH-PROCESS.
03502
03503      IF  W-CHANGE-REQUESTED
03504          MOVE HIGH-VALUES        TO W-ARCH-KEY
03505          MOVE PI-COMPANY-CD      TO W-ARCH-COMPANY-CD
03506
03507      ELSE
03508          IF  W-FIRST-CHANGE-FOUND
03509              MOVE HIGH-VALUES    TO W-ARCH-KEY
03510              MOVE PI-COMPANY-CD  TO W-ARCH-COMPANY-CD
03511              MOVE PI-690-LAST-ARCHIVE-NO
03512                                  TO W-ARCH-ARCHIVE-NO
03513
03514          ELSE
03515              MOVE PI-COMPANY-CD  TO W-ARCH-COMPANY-CD
03516              MOVE PI-690-FIRST-ARCHIVE-NO
03517                                  TO W-ARCH-ARCHIVE-NO
03518              MOVE PI-690-FIRST-ARCHIVE-NO
03519                                  TO W-ARCH-ARCHIVE-NO.
03520
03521      
      * EXEC CICS HANDLE CONDITION
03522 *        NOTFND  (8080-ARCH-NOT-FOUND)
03523 *        ENDFILE (8080-ARCH-NOT-FOUND)
03524 *        NOTOPEN (8050-ARCH-NOT-OPEN)
03525 *        END-EXEC.
      *    MOVE '"$I''J                 ! ; #00007325' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3B20233030303037333235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03526
03527      
      * EXEC CICS STARTBR
03528 *        DATASET  (W-ARCH-FILE-ID)
03529 *        RIDFLD   (W-ARCH-KEY)
03530 *        GTEQ
03531 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007331' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03532
03533      
      * EXEC CICS HANDLE CONDITION
03534 *        NOTFND  (3600-END-BROWSE)
03535 *        ENDFILE (3600-END-BROWSE)
03536 *        END-EXEC.
      *    MOVE '"$I''                  ! < #00007337' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3C20233030303037333337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03537
03538      PERFORM 7500-READ-ARCH-FILE-NEXT THRU 7500-EXIT.
03539
03540      IF  W-CHANGE-REQUESTED
03541              OR
03542          W-READ-PREV-TWICE
03543          MOVE SPACES            TO W-READ-PREV-TWICE-IND
03544          PERFORM 7550-READ-PREV-ARCH THRU 7550-EXIT.
03545
03546  3600-BUILD-ARCH-LIST-BACKWARD.
03547
03548      PERFORM 7550-READ-PREV-ARCH THRU 7550-EXIT.
03549
03550      IF  LA-COMPANY-CD-A5 NOT EQUAL PI-COMPANY-CD
03551          GO TO 3600-END.
03552
03553      IF  PI-690-INIT-ENTRY NOT EQUAL HIGH-VALUES
03554              AND
03555          PI-690-INIT-ENTRY NOT EQUAL LOW-VALUES
03556          IF  PI-690-INIT-ENTRY NOT EQUAL LA-ENTRY-A6
03557              GO TO 3600-BUILD-ARCH-LIST-BACKWARD.
03558
03559      IF  PI-690-INIT-PROCESSOR NOT EQUAL HIGH-VALUES
03560              AND
03561          PI-690-INIT-PROCESSOR NOT EQUAL LOW-VALUES
03562          IF  PI-690-INIT-PROCESSOR NOT EQUAL LA-PROCESSOR-CD
03563              GO TO 3600-BUILD-ARCH-LIST-BACKWARD.
03564
03565      IF  PI-690-INIT-FORM NOT EQUAL HIGH-VALUES
03566              AND
03567          PI-690-INIT-FORM NOT EQUAL LOW-VALUES
03568          IF  PI-690-INIT-FORM NOT EQUAL LA-FORM-A3
03569              GO TO 3600-BUILD-ARCH-LIST-BACKWARD.
03570
03571      IF  PI-690-INIT-CARRIER NOT EQUAL HIGH-VALUES
03572              AND
03573          PI-690-INIT-CARRIER NOT EQUAL LOW-VALUES
03574          IF  LA-CARRIER-A2 NOT EQUAL PI-690-INIT-CARRIER
03575              GO TO 3600-BUILD-ARCH-LIST-BACKWARD.
03576
03577      IF  PI-690-INIT-GROUPING NOT EQUAL HIGH-VALUES
03578              AND
03579          PI-690-INIT-GROUPING NOT EQUAL LOW-VALUES
03580          IF  LA-GROUPING-A2 NOT EQUAL PI-690-INIT-GROUPING
03581              GO TO 3600-BUILD-ARCH-LIST-BACKWARD.
03582
03583      IF  PI-690-INIT-STATE NOT EQUAL HIGH-VALUES
03584              AND
03585          PI-690-INIT-STATE NOT EQUAL LOW-VALUES
03586          IF  LA-STATE-A2 NOT EQUAL PI-690-INIT-STATE
03587              GO TO 3600-BUILD-ARCH-LIST-BACKWARD.
03588
03589      IF  PI-690-INIT-ACCOUNT NOT EQUAL HIGH-VALUES
03590              AND
03591          PI-690-INIT-ACCOUNT NOT EQUAL LOW-VALUES
03592          IF  LA-ACCOUNT-A2 NOT EQUAL PI-690-INIT-ACCOUNT
03593              GO TO 3600-BUILD-ARCH-LIST-BACKWARD.
03594
03595      IF  NOT PI-690-SELECT-ALL
03596              AND
03597          LA-STATUS NOT EQUAL PI-690-STATUS-SELECTION-IND
03598          GO TO 3600-BUILD-ARCH-LIST-BACKWARD.
03599
03600      MOVE LA-ARCHIVE-NO          TO W-ARCH-NUMBER.
03601
03602      PERFORM 6000-SELECT-FOR-OUTPUT THRU 6000-EXIT.
03603
03604      IF  W-ARCH-NDX LESS THAN +13
03605          GO TO 3600-BUILD-ARCH-LIST-BACKWARD.
03606
03607  3600-END.
03608
03609      MOVE -1                     TO CORRSELL.
03610      MOVE ZEROS                  TO CORRSELO.
03611      PERFORM 3600-END-BROWSE
03612      GO TO 8100-SEND-INITIAL-MAP.
03613
03614  3600-END-BROWSE.
03615
03616      MOVE W-ARCH-NDX             TO W-ARCH-NDX3.
03617      SUBTRACT +1 FROM W-ARCH-NDX3.
03618
03619      IF  W-ARCH-NDX3 GREATER THAN +1
03620          MOVE +1 TO W-ARCH-NDX2
03621          PERFORM 6500-REVERSE-TABLE-DATA THRU 6500-EXIT
03622                  UNTIL
03623              W-ARCH-NDX2 NOT LESS W-ARCH-NDX3.
03624
03625      
      * EXEC CICS ENDBR
03626 *        DATASET (W-ARCH-FILE-ID)
03627 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007429' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03628
03629  3600-FRONT-OF-FILE.
03630
03631      MOVE ER-0131                TO EMI-ERROR.
03632      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03633      MOVE -1                     TO CORRSELL.
03634      MOVE ZEROS                  TO CORRSELO.
03635      GO TO 8100-SEND-INITIAL-MAP.
03636
03637  3600-EXIT.
03638      EXIT.
03639                                  EJECT
03640  6000-SELECT-FOR-OUTPUT.
03641
03642      IF  PI-CARRIER-SECURITY GREATER THAN SPACES
03643              AND
03644          PI-CARRIER-SECURITY NOT EQUAL LA-CARRIER-A2
03645
03646          IF  EMI-ERROR NOT EQUAL ER-9150
03647              MOVE ER-9150        TO EMI-ERROR
03648              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03649              GO TO 6000-EXIT
03650
03651          ELSE
03652              GO TO 6000-EXIT.
070711
070711     IF LA-TEMP
070711         GO TO 6000-EXIT
070711     END-IF.
03653
03654      PERFORM 6100-TABLE-LINE-CREATE  THRU 6100-EXIT.
03655
03656      MOVE W-ARCH-NDX             TO PI-690-LAST-ARCH-NDX.
03657      ADD +1                      TO W-ARCH-NDX.
03658
03659
03660      IF  EIBAID EQUAL DFHPF1
03661          MOVE LA-CARRIER-A2      TO PI-690-LAST-CARRIER
03662          MOVE LA-GROUPING-A2     TO PI-690-LAST-GROUPING
03663          MOVE LA-STATE-A2        TO PI-690-LAST-STATE
03664          MOVE LA-ACCOUNT-A2      TO PI-690-LAST-ACCOUNT
03665          MOVE LA-CERT-PRIME-A2   TO PI-690-LAST-CERT-PRIME
03666          MOVE LA-CERT-SUFFIX-A2  TO PI-690-LAST-SUFFIX
03667          MOVE LA-EFFECT-DATE-A2  TO PI-690-LAST-EFFECT-DATE
03668          MOVE LA-FORM-A3         TO PI-690-LAST-FORM
03669          MOVE LA-PROCESSOR-CD    TO PI-690-LAST-PROCESSOR
03670          MOVE LA-ENTRY-A6        TO PI-690-LAST-ENTRY
03671          MOVE LA-ARCHIVE-NO      TO PI-690-LAST-ARCHIVE-NO
03672
03673          IF  PI-690-FIRST-DATA EQUAL LOW-VALUES
03674                  OR
03675              PI-690-FIRST-DATA EQUAL HIGH-VALUES
03676              MOVE PI-690-LAST-DATA
03677                                  TO PI-690-FIRST-DATA
03678
03679          ELSE
03680              NEXT SENTENCE
03681
03682      ELSE
03683          MOVE LA-CARRIER-A2      TO PI-690-FIRST-CARRIER
03684          MOVE LA-GROUPING-A2     TO PI-690-FIRST-GROUPING
03685          MOVE LA-STATE-A2        TO PI-690-FIRST-STATE
03686          MOVE LA-ACCOUNT-A2      TO PI-690-FIRST-ACCOUNT
03687          MOVE LA-CERT-PRIME-A2   TO PI-690-FIRST-CERT-PRIME
03688          MOVE LA-CERT-SUFFIX-A2  TO PI-690-FIRST-SUFFIX
03689          MOVE LA-EFFECT-DATE-A2  TO PI-690-FIRST-EFFECT-DATE
03690          MOVE LA-FORM-A3         TO PI-690-FIRST-FORM
03691          MOVE LA-PROCESSOR-CD    TO PI-690-FIRST-PROCESSOR
03692          MOVE LA-ENTRY-A6        TO PI-690-FIRST-ENTRY
03693          MOVE LA-ARCHIVE-NO      TO PI-690-FIRST-ARCHIVE-NO
03694
03695          IF  PI-690-LAST-DATA EQUAL LOW-VALUES
03696                  OR
03697              PI-690-LAST-DATA EQUAL HIGH-VALUES
03698              MOVE PI-690-FIRST-DATA
03699                                  TO PI-690-LAST-DATA.
03700
03701  6000-EXIT.
03702      EXIT.
03703                                  EJECT
03704  6100-TABLE-LINE-CREATE.
03705
03706      MOVE LA-ARCHIVE-NO
03707          TO W-ARCNOO (W-ARCH-NDX)
03708             PI-690-ARCHIVE-NUM (W-ARCH-NDX).
03709
070711*    MOVE LA-STATUS              TO W-STATSO (W-ARCH-NDX).
03711      MOVE LA-FORM-A3             TO W-FORMO (W-ARCH-NDX).
070711     MOVE LA-PROCESSOR-CD        TO W-USRIDO (W-ARCH-NDX).
03712
070711*    IF  LA-PRINT-ONLY-WHEN-FORM-GIVEN
070711*        MOVE 'F'                TO W-PRRSTO (W-ARCH-NDX)
070711*
070711*    ELSE
070711*        IF  LA-PRINT-ONLY-WHEN-PROC-GIVEN
070711*            MOVE 'P'            TO W-PRRSTO (W-ARCH-NDX)
070711*
070711*        ELSE
070711*            IF  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
070711*                MOVE 'C'        TO W-PRRSTO (W-ARCH-NDX)
070711*
070711*            ELSE
070711*                MOVE SPACES     TO W-PRRSTO (W-ARCH-NDX).
03726
03727      IF  LA-INITIAL-PRINT-DATE GREATER THAN LOW-VALUES
070711*        MOVE AL-PABON           TO W-PRINTA (W-ARCH-NDX)
03729          MOVE LA-INITIAL-PRINT-DATE
03730                                  TO DC-BIN-DATE-1
03731          MOVE ' '                TO DC-OPTION-CODE
03732          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
03733
03734          IF  DC-ERROR-CODE EQUAL SPACES
03735              MOVE DC-GREG-DATE-1-EDIT
070711                                 TO W-PRINTO (W-ARCH-NDX)
03737
03738          ELSE
070711             MOVE SPACES         TO W-PRINTO (W-ARCH-NDX)
070711         END-IF
070711     ELSE
070711         MOVE SPACES             TO W-PRINTO (W-ARCH-NDX)
070711     END-IF.
03740
070711*        IF  LA-CREATION-DATE GREATER THAN LOW-VALUES
070711*            MOVE LA-CREATION-DATE
070711*                                TO DC-BIN-DATE-1
070711*            MOVE ' '            TO DC-OPTION-CODE
070711*            PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
070711*
070711*            IF  DC-ERROR-CODE EQUAL SPACES
070711*                MOVE DC-GREG-DATE-1-EDIT
070711*                                TO W-CREATO (W-ARCH-NDX)
070711*
070711*            ELSE
070711*                MOVE SPACES     TO W-CREATO (W-ARCH-NDX)
070711*
070711*        ELSE
070711*            MOVE SPACES         TO W-CREATO (W-ARCH-NDX).
03757
03758
070711*    IF  LA-FOLLOW-UP-DATE GREATER THAN LOW-VALUES
070711*        MOVE LA-FOLLOW-UP-DATE  TO DC-BIN-DATE-1
070711*        MOVE ' '                TO DC-OPTION-CODE
070711*        PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
070711*
070711*        IF  DC-ERROR-CODE EQUAL SPACES
070711*            MOVE DC-GREG-DATE-1-EDIT
070711*                                TO W-FOLUPO (W-ARCH-NDX)
070711*
070711*        ELSE
070711*            MOVE SPACES         TO W-FOLUPO (W-ARCH-NDX)
070711*
070711*    ELSE
070711*        MOVE SPACES             TO W-FOLUPO (W-ARCH-NDX).
070711*
03774
070711*    IF  LA-SENT-DATE   GREATER THAN LOW-VALUES
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
03807
03808
           IF LA-RESEND-LETR NOT = SPACES
070711        MOVE LA-RESEND-LETR      TO W-RSFRMO (W-ARCH-NDX)
           ELSE
070711        MOVE SPACES              TO W-RSFRMO (W-ARCH-NDX)
           END-IF
03809 *    IF  LA-SENT-DATE-2 GREATER THAN LOW-VALUES
03810 *        MOVE AL-UABON           TO W-RSFRMA (W-ARCH-NDX)
03811 *        MOVE LA-SENT-DATE-2     TO DC-BIN-DATE-1
03812 *        MOVE ' '                TO DC-OPTION-CODE
03813 *        PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
03814 *
03815 *        IF  DC-ERROR-CODE EQUAL SPACES
03816 *            MOVE DC-GREG-DATE-1-EDIT
03817 *                                TO W-RSFRMO (W-ARCH-NDX)
03818 *
03819 *        ELSE
03820 *            MOVE SPACES         TO W-RSFRMO (W-ARCH-NDX)
03821 *
03822 *    ELSE
03823 *        IF  LA-RESEND-DATE-2 GREATER THAN LOW-VALUES
03824 *            MOVE LA-RESEND-DATE-2
03825 *                                TO DC-BIN-DATE-1
03826 *            MOVE ' '            TO DC-OPTION-CODE
03827 *            PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
03828 *
03829 *            IF  DC-ERROR-CODE EQUAL SPACES
03830 *                MOVE DC-GREG-DATE-1-EDIT
03831 *                                TO W-RSFRMO (W-ARCH-NDX)
03832 *
03833 *            ELSE
03834 *                MOVE SPACES     TO W-RSFRMO (W-ARCH-NDX)
03835 *
03836 *        ELSE
03837 *            MOVE SPACES         TO W-RSFRMO (W-ARCH-NDX)
03838 *                                   W-RSSTSO (W-ARCH-NDX)
03839 *            GO TO 6100-CHECK-REPLY.
03840 *
03841 *
03842 *    IF  LA-SENT-DATE-3 GREATER THAN LOW-VALUES
03843 *        MOVE AL-UABON           TO W-RSSTSA (W-ARCH-NDX)
03844 *        MOVE LA-SENT-DATE-3     TO DC-BIN-DATE-1
03845 *        MOVE ' '                TO DC-OPTION-CODE
03846 *        PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
03847 *
03848 *        IF  DC-ERROR-CODE EQUAL SPACES
03849 *            MOVE DC-GREG-DATE-1-EDIT
03850 *                                TO W-RSSTSO (W-ARCH-NDX)
03851 *
03852 *        ELSE
03853 *            MOVE SPACES         TO W-RSSTSO (W-ARCH-NDX)
03854 *
03855 *    ELSE
03856 *        IF  LA-RESEND-DATE-3 GREATER THAN LOW-VALUES
03857 *            MOVE LA-RESEND-DATE-3
03858 *                                TO DC-BIN-DATE-1
03859 *            MOVE ' '            TO DC-OPTION-CODE
03860 *            PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
03861 *
03862 *            IF  DC-ERROR-CODE EQUAL SPACES
03863 *                MOVE DC-GREG-DATE-1-EDIT
03864 *                                TO W-RSSTSO (W-ARCH-NDX)
03865 *
03866 *            ELSE
03867 *                MOVE SPACES     TO W-RSSTSO (W-ARCH-NDX)
03868 *
03869 *        ELSE
03870 *            MOVE SPACES         TO W-RSSTSO (W-ARCH-NDX).
           .
03872  6100-CHECK-REPLY.
03873
03874      IF  LA-PURGED-DATE GREATER THAN LOW-VALUES
03875              AND
03876          LA-PURGED-DATE NOT EQUAL SPACES
070711*        MOVE LA-PURGED-DATE     TO DC-BIN-DATE-1
070711*        MOVE ' '                TO DC-OPTION-CODE
070711*        PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
03880
070711*        IF  DC-ERROR-CODE EQUAL SPACES
070711*            MOVE DC-GREG-DATE-1-EDIT
070711*                                TO W-REPLYO (W-ARCH-NDX)
070711             MOVE 'PURGED'       TO W-STATUSO (W-ARCH-NDX)
03884
070711*        ELSE
070711*            MOVE SPACES         TO W-REPLYO (W-ARCH-NDX)
070711*                                   W-STOPO (W-ARCH-NDX)
070711*
070711*        END-IF
03888      ELSE
03889          IF  LA-VOIDED-DATE GREATER THAN LOW-VALUES
03890                  AND
03891              LA-VOIDED-DATE NOT EQUAL SPACES
070711*            MOVE LA-VOIDED-DATE TO DC-BIN-DATE-1
070711*            MOVE ' '            TO DC-OPTION-CODE
070711*            PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
03895
070711*            IF  DC-ERROR-CODE EQUAL SPACES
070711*                MOVE AL-PANOF   TO W-REPLYA (W-ARCH-NDX)
070711*                MOVE DC-GREG-DATE-1-EDIT
070711*                                TO W-REPLYO (W-ARCH-NDX)
070711                 MOVE 'STOPPED'  TO W-STATUSO (W-ARCH-NDX)
03900
070711*            ELSE
070711*                MOVE AL-PANOF   TO W-REPLYA (W-ARCH-NDX)
070711*                MOVE SPACES     TO W-REPLYO (W-ARCH-NDX)
070711*                                   W-STOPO (W-ARCH-NDX)
070711*
070711*
070711*            END-IF
03906          ELSE
03907              IF  LA-REPLY-DATE GREATER THAN LOW-VALUES
03908                      AND
03909                  LA-REPLY-DATE NOT EQUAL SPACES
070711*                MOVE LA-REPLY-DATE
070711*                                TO DC-BIN-DATE-1
070711*                MOVE ' '        TO DC-OPTION-CODE
070711*                PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
070711*
070711*                IF  DC-ERROR-CODE EQUAL SPACES
070711*                    MOVE DC-GREG-DATE-1-EDIT
070711*                                TO W-REPLYO (W-ARCH-NDX)
070711                     MOVE 'RECEIVED' TO W-STATUSO (W-ARCH-NDX)
03918
070711*                ELSE
070711*                    MOVE SPACES TO W-REPLYO (W-ARCH-NDX)
070711*                                   W-STOPO (W-ARCH-NDX)
070711*
070711*                END-IF
03922              ELSE
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
03924
03932
03933  6100-EXIT.
03934      EXIT.
03935                                  EJECT
03936  6500-REVERSE-TABLE-DATA.
03937
03938      MOVE W-ARCHGRP-DATA (W-ARCH-NDX2)
03939                                  TO W-HOLD-LINE.
03940      MOVE W-ARCHGRP-DATA (W-ARCH-NDX3)
03941                                  TO W-ARCHGRP-DATA (W-ARCH-NDX2).
03942      MOVE W-HOLD-LINE            TO W-ARCHGRP-DATA (W-ARCH-NDX3).
03943      MOVE PI-690-ARCHIVE-NUM (W-ARCH-NDX2)
03944                                  TO W-HOLD-ARCHIVE.
03945      MOVE PI-690-ARCHIVE-NUM (W-ARCH-NDX3)
03946          TO PI-690-ARCHIVE-NUM (W-ARCH-NDX2).
03947      MOVE W-HOLD-ARCHIVE
03948          TO PI-690-ARCHIVE-NUM (W-ARCH-NDX3).
03949
03950      ADD +1                      TO W-ARCH-NDX2.
03951      SUBTRACT +1 FROM W-ARCH-NDX3.
03952
03953  6500-EXIT.
03954      EXIT.
03955                                  EJECT
03956  7000-READ-ARCH2-FILE-NEXT.
03957
03958      
      * EXEC CICS READNEXT
03959 *        DATASET  (W-ARCH2-FILE-ID)
03960 *        SET      (ADDRESS OF LETTER-ARCHIVE)
03961 *        RIDFLD   (W-ARCH2-KEY)
03962 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007779' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH2-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03963
03964  7000-EXIT.
03965      EXIT.
03966
03967  7050-READ-PREV-ARCH2.
03968
03969      
      * EXEC CICS READPREV
03970 *        DATASET  (W-ARCH2-FILE-ID)
03971 *        SET      (ADDRESS OF LETTER-ARCHIVE)
03972 *        RIDFLD   (W-ARCH2-KEY)
03973 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00007790' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH2-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03974
03975  7050-EXIT.
03976      EXIT.
03977                                  EJECT
03978  7100-READ-ARCH3-FILE-NEXT.
03979
03980      
      * EXEC CICS READNEXT
03981 *        DATASET  (W-ARCH3-FILE-ID)
03982 *        SET      (ADDRESS OF LETTER-ARCHIVE)
03983 *        RIDFLD   (W-ARCH3-KEY)
03984 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007801' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH3-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH3-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03985
03986  7100-EXIT.
03987      EXIT.
03988
03989  7150-READ-PREV-ARCH3.
03990
03991      
      * EXEC CICS READPREV
03992 *        DATASET  (W-ARCH3-FILE-ID)
03993 *        SET      (ADDRESS OF LETTER-ARCHIVE)
03994 *        RIDFLD   (W-ARCH3-KEY)
03995 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00007812' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH3-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH3-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03996
03997  7150-EXIT.
03998      EXIT.
03999                                  EJECT
04000  7200-READ-ARCH4-FILE-NEXT.
04001
04002      
      * EXEC CICS READNEXT
04003 *        DATASET  (W-ARCH4-FILE-ID)
04004 *        SET      (ADDRESS OF LETTER-ARCHIVE)
04005 *        RIDFLD   (W-ARCH4-KEY)
04006 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007823' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH4-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH4-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04007
04008  7200-EXIT.
04009      EXIT.
04010
04011  7250-READ-PREV-ARCH4.
04012
04013      
      * EXEC CICS READPREV
04014 *        DATASET  (W-ARCH4-FILE-ID)
04015 *        SET      (ADDRESS OF LETTER-ARCHIVE)
04016 *        RIDFLD   (W-ARCH4-KEY)
04017 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00007834' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH4-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH4-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04018
04019  7250-EXIT.
04020      EXIT.
04021                                  EJECT
04022  7300-READ-ARCH5-FILE-NEXT.
04023
04024      
      * EXEC CICS READNEXT
04025 *        DATASET  (W-ARCH5-FILE-ID)
04026 *        SET      (ADDRESS OF LETTER-ARCHIVE)
04027 *        RIDFLD   (W-ARCH5-KEY)
04028 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007845' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH5-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH5-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04029
04030  7300-EXIT.
04031      EXIT.
04032
04033  7350-READ-PREV-ARCH5.
04034
04035      
      * EXEC CICS READPREV
04036 *        DATASET  (W-ARCH5-FILE-ID)
04037 *        SET      (ADDRESS OF LETTER-ARCHIVE)
04038 *        RIDFLD   (W-ARCH5-KEY)
04039 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00007856' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH5-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH5-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04040
04041  7350-EXIT.
04042      EXIT.
04043                                  EJECT
04044  7400-READ-ARCH6-FILE-NEXT.
04045
04046      
      * EXEC CICS READNEXT
04047 *        DATASET  (W-ARCH6-FILE-ID)
04048 *        SET      (ADDRESS OF LETTER-ARCHIVE)
04049 *        RIDFLD   (W-ARCH6-KEY)
04050 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007867' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH6-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH6-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04051
04052  7400-EXIT.
04053      EXIT.
04054
04055  7450-READ-PREV-ARCH6.
04056
04057      
      * EXEC CICS READPREV
04058 *        DATASET  (W-ARCH6-FILE-ID)
04059 *        SET      (ADDRESS OF LETTER-ARCHIVE)
04060 *        RIDFLD   (W-ARCH6-KEY)
04061 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00007878' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH6-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH6-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04062
04063  7450-EXIT.
04064      EXIT.
04065                                  EJECT
04066  7500-READ-ARCH-FILE-NEXT.
04067
04068      
      * EXEC CICS READNEXT
04069 *        DATASET  (W-ARCH-FILE-ID)
04070 *        SET      (ADDRESS OF LETTER-ARCHIVE)
04071 *        RIDFLD   (W-ARCH-KEY)
04072 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007889' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04073
04074  7500-EXIT.
04075      EXIT.
04076
04077  7550-READ-PREV-ARCH.
04078
04079      
      * EXEC CICS READPREV
04080 *        DATASET  (W-ARCH-FILE-ID)
04081 *        SET      (ADDRESS OF LETTER-ARCHIVE)
04082 *        RIDFLD   (W-ARCH-KEY)
04083 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00007900' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04084
04085  7550-EXIT.
04086      EXIT.
04087                                  EJECT
04088  7600-READ-ARCH-FILE.
04089
04090      
      * EXEC CICS READ
04091 *        DATASET  (W-ARCH-FILE-ID)
04092 *        SET      (ADDRESS OF LETTER-ARCHIVE)
04093 *        RIDFLD   (W-ARCH-KEY)
04094 *        UPDATE
04095 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00007911' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04096
04097  7600-EXIT.
04098      EXIT.
04099                                  EJECT
04100  7650-READ-ARCT-FILE.
04101
04102      
      * EXEC CICS READ
04103 *        DATASET  (W-ARCT-FILE-ID)
04104 *        SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)
04105 *        RIDFLD   (W-ARCT-KEY)
04106 *        UPDATE
04107 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00007923' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE-TEXT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04108
04109  7650-EXIT.
04110      EXIT.
04111
04112  7750-READ-ARCH-FILE-NUPDT.
04113
04114      
      * EXEC CICS READ
04115 *        DATASET  (W-ARCH-FILE-ID)
04116 *        SET      (ADDRESS OF LETTER-ARCHIVE)
04117 *        RIDFLD   (W-ARCH-KEY)
04118 *        END-EXEC.
      *    MOVE '&"S        E          (   #00007935' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04119
04120  7750-EXIT.
04121      EXIT.
04122                                  EJECT
04123  7800-DELETE-CYCLE.
04124
04125      
      * EXEC CICS HANDLE CONDITION
04126 *        NOTFND  (7800-EXIT)
04127 *        ENDFILE (7800-EXIT)
04128 *        END-EXEC.
      *    MOVE '"$I''                  ! = #00007946' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3D20233030303037393436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04129
04130 *    PERFORM 7650-READ-ARCT-FILE THRU 7650-EXIT.
04131
04132      
      * EXEC CICS DELETE
04133 *        DATASET   (W-ARCT-FILE-ID)
04134 *        RIDFLD    (W-ARCT-KEY)
04135 *        KEYLENGTH (5)
04136 *        GENERIC
04137 *    END-EXEC.
           MOVE 5
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00007953' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-FILE-ID, 
                 W-ARCT-KEY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04138
04139      
      * EXEC CICS DELETE
04140 *        DATASET   (W-ARCH-FILE-ID)
04141 *        RIDFLD    (W-ARCH-KEY)
04142 *    END-EXEC.
      *    MOVE '&(  R                 &   #00007960' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04143
04144      MOVE +99999999            TO PI-690-ARCHIVE-NUM (W-ARCH-NDX).
04145      MOVE 1                      TO W-FIRST-CHANGE-IND.
04146
04147  7800-EXIT.
04148      EXIT.
04149                                  EJECT
04150  8000-UNAUTHORIZED-ACCESS.
04151
04152      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
04153      GO TO 8300-SEND-TEXT.
04154
04155  8000-ARCH2-NOT-OPEN.
04156
04157      MOVE ER-7357                TO EMI-ERROR.
04158      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04159      MOVE -1                     TO TYPEBRL.
04160
04161      GO TO 8100-SEND-INITIAL-MAP.
04162
04163  8010-ARCH3-NOT-OPEN.
04164
04165      MOVE ER-7366                TO EMI-ERROR.
04166      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04167      MOVE -1                     TO TYPEBRL.
04168
04169      GO TO 8100-SEND-INITIAL-MAP.
04170
04171  8020-ARCH4-NOT-OPEN.
04172
04173      MOVE ER-7385                TO EMI-ERROR.
04174      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04175      MOVE -1                     TO TYPEBRL.
04176
04177      GO TO 8100-SEND-INITIAL-MAP.
04178
04179  8030-ARCH5-NOT-OPEN.
04180
04181      MOVE ER-7386                TO EMI-ERROR.
04182      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04183      MOVE -1                     TO TYPEBRL.
04184
04185      GO TO 8100-SEND-INITIAL-MAP.
04186
04187  8040-ARCH6-NOT-OPEN.
04188
04189      MOVE ER-7387                TO EMI-ERROR.
04190      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04191      MOVE -1                     TO TYPEBRL.
04192
04193      GO TO 8100-SEND-INITIAL-MAP.
04194
04195  8050-ARCH-NOT-OPEN.
04196
04197      MOVE ER-7388                TO EMI-ERROR.
04198      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04199      MOVE -1                     TO TYPEBRL.
04200
04201      GO TO 8100-SEND-INITIAL-MAP.
04202
04203  8070-NOTOPEN.
04204
04205      MOVE ER-9282                TO EMI-ERROR.
04206      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04207      MOVE -1                     TO CERTRPL.
04208
04209      GO TO 8100-SEND-INITIAL-MAP.
04210
04211  8080-ARCH-NOT-FOUND.
04212
04213      MOVE ER-9010                TO EMI-ERROR.
04214      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04215      MOVE -1                     TO CERTRPL.
04216      MOVE AL-UABON               TO CERTRPA.
04217      GO TO 8100-SEND-INITIAL-MAP.
04218                                  EJECT
04219
04220  8090-NOT-FOUND-CNTL.
04221
04222      MOVE ER-0190                TO EMI-ERROR.
04223      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04224      MOVE -1                     TO MAINTL.
04225      MOVE AL-UABON               TO MAINTA.
04226      GO TO 8100-SEND-INITIAL-MAP.
04227                                  EJECT
04228  8100-SEND-INITIAL-MAP.
04229 ******************************************************************
04230 *                                                                *
04231 *       THIS LOGIC SENDS THE INITIAL MAP.  IT WILL LOOK FOR      *
04232 *       THE MAP DATA UNDER THE NAMES LISTED BELOW AND FOUND      *
04233 *       IN THE WORK AREA SECTION OF WORKING STORAGE.             *
04234 *                                                                *
04235 *       W-MAP          PIC  X(08)                                *
04236 *       W-MAPSET       PIC  X(08)                                *
04237 *       W-MAP-AREA     PIC  X(**)                                *
04238 * ** THIS AREA REDEFINES MAP AREA AND IS MAP DEPENDENT           *
04239 *    FOR SIZE.                                                   *
04240 *                                                                *
04241 ******************************************************************
04242
04243      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
04244
04245      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
04246
04247      
      * EXEC CICS SEND
04248 *        MAP    (W-MAP)
04249 *        MAPSET (W-MAPSET)
04250 *        FROM   (EL690AI)
04251 *        ERASE
04252 *        FREEKB
04253 *        CURSOR
04254 *    END-EXEC.
           MOVE LENGTH OF
            EL690AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E F  H L F ,   #00008068' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2046202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL690AI, 
                 DFHEIV12, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04255
04256      GO TO 9000-RETURN-TRANS.
04257
04258  8100-EXIT.
04259      EXIT.
04260                                  EJECT
04261  8200-SEND-DATAONLY.
04262 ******************************************************************
04263 *                                                                *
04264 *       THIS LOGIC SENDS THE UPDATED VERSION OF THE MAP, USING   *
04265 *       THE FIELDS LISTED BELOW WHICH SHOULD BE FOUND IN THE     *
04266 *       WORK AREA OF WORKING STORAGE.                            *
04267 *                                                                *
04268 *       W-MAP           PIC  X(08)                               *
04269 *       W-MAPSET        PIC  X(08)                               *
04270 *       W-MAP-AREA      PIC  X(****)                             *
04271 *                                                                *
04272 * **** REDEFINES MAP AREA AND IS MAP DEPENDENT.                  *
04273 *                                                                *
04274 ******************************************************************
04275
04276      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
04277
04278      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
04279
04280      
      * EXEC CICS SEND
04281 *        MAP    (W-MAP)
04282 *        MAPSET (W-MAPSET)
04283 *        FROM   (EL690AI)
04284 *        DATAONLY
04285 *        FREEKB
04286 *        CURSOR
04287 *    END-EXEC.
           MOVE LENGTH OF
            EL690AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT    F  H L F ,   #00008101' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2046202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL690AI, 
                 DFHEIV12, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04288
04289      GO TO 9000-RETURN-TRANS.
04290
04291  8200-EXIT.
04292      EXIT.
04293                                  EJECT
04294  8300-SEND-TEXT.
04295 *****************************************************************
04296 *    THIS PARAGRAPH SENDS THE COMMON LOGOFF MESSAGE.            *
04297 *****************************************************************
04298
04299      
      * EXEC CICS SEND TEXT
04300 *        FROM    (LOGOFF-TEXT)
04301 *        LENGTH  (LOGOFF-LENGTH)
04302 *        ERASE
04303 *        FREEKB
04304 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00008120' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOGOFF-TEXT, 
                 LOGOFF-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04305
04306      GO TO 9000-RETURN-TRANS.
04307
04308  8300-EXIT.
04309      EXIT.
04310                                  EJECT
04311  9000-RETURN-TRANS.
04312 *****************************************************************
04313 *     THIS PARAGRAPH CAUSES THE PROGRAM TO EXIT TO A            *
04314 *     TRANSACTION.                                              *
04315 *     THE FOLLOWING FIELDS ARE NEEDED IN WORKING-STORAGE        *
04316 *     W-TRANSACTION          PIC  X(04)  VALUE 'XXXX'.          *
04317 *****************************************************************
04318
04319      MOVE EMI-ERROR-NUMBER(1)    TO PI-LAST-ERROR-NO.
04320
04321      
      * EXEC CICS RETURN
04322 *        TRANSID  (W-TRANSACTION)
04323 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
04324 *        LENGTH   (PI-COMM-LENGTH)
04325 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00008142' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TRANSACTION, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04326
04327  9000-EXIT.
04328      EXIT.
04329                               EJECT
04330  9200-DATE-EDIT.
04331 *****************************************************************
04332 *     THIS ROUTINE VALIDATES THE DATE PASSED IN THE             *
04333 *     'W-DEEDIT-FIELD' BY CALLING THE 'ELDATECV' PROGRAM.       *
04334 *     AFTER EXITING THIS COPY BOOK, 'DC-ERROR-CODE' CAN BE      *
04335 *     CHECKED FOR SPACES TO DETERMINE IF ERRORS WERE FOUND      *
04336 *     ELSE THE BINARY DATE CAN BE FOUND IN 'DC-BIN-DATE-1'.     *
04337 *****************************************************************
04338
04339      
      * EXEC CICS BIF DEEDIT
04340 *        FIELD  (W-DEEDIT-FIELD)
04341 *        LENGTH (15)
04342 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008160' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04343
04344      IF  W-DEEDIT-FIELD-V0 NUMERIC
04345          MOVE W-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-EDIT
04346                                     DC-GREG-DATE-1-MDY
04347          INSPECT DC-GREG-DATE-1-EDIT CONVERTING ' ' TO '/'
04348 *        INSPECT DC-GREG-DATE-1-EDIT REPLACING ALL ' ' BY '/'
04349          MOVE '4'                TO DC-OPTION-CODE
04350
04351          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
04352
04353      ELSE
04354          MOVE HIGH-VALUES        TO  DC-ERROR-CODE.
04355
04356  9200-EXIT.
04357      EXIT.
04358                                  EJECT
04359  9400-XCTL.
04360 *****************************************************************
04361 *    THIS PARAGRAPH TRANSFERS CONTROL TO INDICATED PROGRAM.     *
04362 *    PROGRAM MUST RESIDE IN W-CALL-PGM.                         *
04363 *****************************************************************
04364
04365      
      * EXEC CICS XCTL
04366 *        PROGRAM  (W-CALL-PGM)
04367 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
04368 *        LENGTH   (PI-COMM-LENGTH)
04369 *    END-EXEC.
      *    MOVE '.$C                   %   #00008186' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04370
04371  9400-EXIT.
04372      EXIT.
04373                                  EJECT
04374  9500-LINK-DATE-CONVERT.
04375 *****************************************************************
04376 *    THIS PARAGRAPH 'CALLS' THE UTILITY DATE PROCESSOR.         *
04377 *****************************************************************
04378
04379      
      * EXEC CICS LINK
04380 *        PROGRAM    ('ELDATCV')
04381 *        COMMAREA   (DATE-CONVERSION-DATA)
04382 *        LENGTH     (DC-COMM-LENGTH)
04383 *        END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00008200' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04384
04385  9500-EXIT.
04386      EXIT.
04387                                  EJECT
04388  9600-FORMAT-DATE-TIME.
04389 *****************************************************************
04390 *     THIS LOGIC UPDATES THE DATE/TIME INFO ON GIVEN MAP        *
04391 *****************************************************************
04392
04394      MOVE W-SAVE-DATE            TO RUNDTEO.
04395
04396      
      * EXEC CICS ASKTIME
04397 *    END-EXEC.
      *    MOVE '0"                    "   #00008216' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04398
04399      MOVE EIBTIME                TO W-TIME-IN.
04400      MOVE W-TIME-OUT             TO RUNTIMEO.
04393      MOVE PI-COMPANY-ID          TO COMPANYO.
101101     MOVE PI-PROCESSOR-ID        TO USERIDO.
04401      MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO.
04402
04403  9600-EXIT.
04404      EXIT.
04405                                  EJECT
04406  9700-PGMID-ERROR.
04407 *****************************************************************
04408 *     THIS PARAGRAPH TRANSFERS CONTROL TO EL005 LOGOFF.         *
04409 *     THE FOLLOWING FIELDS ARE NEEDED IN WORKING-STORAGE        *
04410 *     W-CALL-PGM             PIC  X(08)  VALUE 'EL000   '.      *
04411 *     W-THIS-PGM             PIC  X(08).                        *
04412 *     W-XCTL-005             PIC  X(08)  VALUE 'EL005   '.      *
04413 *****************************************************************
04414
04415      
      * EXEC CICS  HANDLE CONDITION
04416 *        PGMIDERR  (8300-SEND-TEXT)
04417 *        END-EXEC.
      *    MOVE '"$L                   ! > #00008237' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3E20233030303038323337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04418
04419      MOVE W-THIS-PGM             TO PI-CALLING-PROGRAM.
04420      MOVE ' '                    TO PI-ENTRY-CD-1.
04421      MOVE W-XCTL-005             TO W-CALL-PGM
04422                                     LOGOFF-PGM.
04423      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
04424
04425      PERFORM 9400-XCTL THRU 9400-EXIT.
04426
04427  9700-EXIT.
04428      EXIT.
04429                                  EJECT
04430  9800-ABEND.
04431 *****************************************************************
04432 *     THIS PARAGRAPH LINKS TO A COMMON ABEND ROUTINE.           *
04433 *     THE FOLLOWING FIELDS ARE NEEDED IN WORKING-STORAGE        *
04434 *     W-LINK-004             PIC  X(08)  VALUE 'EL004   '.      *
04435 *****************************************************************
04436
04437      MOVE W-LINK-004             TO W-CALL-PGM.
04438      MOVE DFHEIBLK               TO EMI-LINE1
04439
04440      
      * EXEC CICS  LINK
04441 *        PROGRAM   (W-CALL-PGM)
04442 *        COMMAREA  (EMI-LINE1)
04443 *        LENGTH    (72)
04444 *        END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00008262' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04445
04446      GO TO 8200-SEND-DATAONLY.
04447
04448  9800-EXIT.
04449      EXIT.
04450                                  EJECT
04451  9900-ERROR-FORMAT.
04452 *****************************************************************
04453 *     THIS PARAGRAPH IS THE STANDARDIZED ERROR FORMAT ROUTINE.  *
04454 *     THE FIELDS W-LINK-001 AND W-CALL-PGM MUST BE DEFINED IN   *
04455 *     WORKING STORAGE.                                          *
04456 *****************************************************************
04457
04458      IF  NOT EMI-ERRORS-COMPLETE
04459              AND
04460          EMI-ERROR NOT EQUAL W-LAST-ERROR
04461          MOVE W-LINK-001         TO W-CALL-PGM
04462
04463          
      * EXEC CICS LINK
04464 *            PROGRAM    (W-CALL-PGM)
04465 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
04466 *            LENGTH     (EMI-COMM-LENGTH)
04467 *        END-EXEC
      *    MOVE '."C                   (   #00008285' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04468
04469          MOVE EMI-ERROR TO W-LAST-ERROR.
04470
04471  9900-EXIT.
04472      EXIT.
04473                                  EJECT
04474  9910-INITIALIZE-SECURITY.
04475 ******************************************************************
04476 *                                                                *
04477 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
04478 *       USER SECURITY RECORD SET UP BY EL125.  BASED ON THE      *
04479 *       APPLICATION NUMBER FOUND IN WORKING STORAGE UNDER        *
04480 *       W-APPL-SECRTY-NDX (PIC  S9(04) COMP), THIS PROGRAM       *
04481 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
04482 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
04483 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
04484 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *
04485 *                                                                *
04486 *       NOTE:  THE CARRIER/GRP/STATE/PRODUCER SECURITY DATA      *
04487 *       IS ALSO PROVIDED BY THIS LOGIC.                          *
04488 *                                                                *
04489 ******************************************************************
04490
04491      IF  PI-PROCESSOR-ID EQUAL 'LGXX'
04492          MOVE 'Y'                TO PI-DISPLAY-CAP
04493                                         PI-MODIFY-CAP
04494
04495      ELSE
04496 *        MOVE '125E'             TO W-SC-QUID-SYSTEM
04497 *        MOVE EIBTRMID           TO W-SC-QUID-TERMINAL
04498
04499          
      * EXEC CICS READQ TS
04500 *            QUEUE  (PI-SECURITY-TEMP-STORE-ID)
04501 *            INTO   (SECURITY-CONTROL)
04502 *            LENGTH (SC-COMM-LENGTH)
04503 *            ITEM   (1)
04504 *        END-EXEC
           MOVE 1
             TO DFHEIV11
      *    MOVE '*$II   L              ''   #00008321' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04505
04506          MOVE SC-CREDIT-DISPLAY (W-APPL-SCRTY-NDX)
04507                                  TO PI-DISPLAY-CAP
04508          MOVE SC-CREDIT-UPDATE (W-APPL-SCRTY-NDX)
04509                                  TO PI-MODIFY-CAP
04510
04511          IF  NOT DISPLAY-CAP
04512              MOVE 'READ'         TO SM-READ
04513              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
04514              MOVE ER-9097        TO EMI-ERROR
04515              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04516              GO TO 8100-SEND-INITIAL-MAP.
04517
04518  9910-EXIT.
04519      EXIT.
04520                                  EJECT
04521  9995-SECURITY-VIOLATION.
04522
04523      MOVE EIBDATE          TO SM-JUL-DATE.
04524      MOVE EIBTRMID         TO SM-TERMID.
04525      MOVE W-THIS-PGM       TO SM-PGM.
04526      MOVE EIBTIME          TO W-TIME-IN.
04527      MOVE W-TIME-OUT       TO SM-TIME.
04528      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
04529
04530      
      * EXEC CICS LINK
04531 *         PROGRAM  ('EL003')
04532 *         COMMAREA (SECURITY-MESSAGE)
04533 *         LENGTH   (80)
04534 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00008352' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04535
04536  9995-EXIT.
04537      EXIT.
04538                                  EJECT
04539  9999-GOBACK.
04540 ******************************************************************
04541 *                                                                *
04542 *       THIS PARAGRAPH FULFILL THE COMPILE REQUIRMENT FOR A      *
04543 *       COBOL RETURN COMMAND.  IN A SEPARATE PARAGRAPH IT        *
04544 *       PREVENTS AN COMPILE ERROR.                               *
04545 *                                                                *
04546 ******************************************************************
04547
04548      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL690' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
04549
04550  9999-EXIT.
04551      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL690' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 0110-PGM-INITIALIZATION,
                     0110-PGM-INITIALIZATION,
                     8070-NOTOPEN,
                     8080-ARCH-NOT-FOUND,
                     9700-PGMID-ERROR,
                     9800-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8080-ARCH-NOT-FOUND,
                     8080-ARCH-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8080-ARCH-NOT-FOUND,
                     8080-ARCH-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8080-ARCH-NOT-FOUND,
                     8080-ARCH-NOT-FOUND,
                     8000-ARCH2-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 2100-END-BROWSE,
                     2100-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8080-ARCH-NOT-FOUND,
                     8080-ARCH-NOT-FOUND,
                     8010-ARCH3-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 2200-END-BROWSE,
                     2200-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8080-ARCH-NOT-FOUND,
                     8080-ARCH-NOT-FOUND,
                     8020-ARCH4-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 2300-END-BROWSE,
                     2300-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 8080-ARCH-NOT-FOUND,
                     8080-ARCH-NOT-FOUND,
                     8030-ARCH5-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 2400-END-BROWSE,
                     2400-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 8080-ARCH-NOT-FOUND,
                     8080-ARCH-NOT-FOUND,
                     8040-ARCH6-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 2500-END-BROWSE,
                     2500-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 8080-ARCH-NOT-FOUND,
                     8080-ARCH-NOT-FOUND,
                     8050-ARCH-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 2600-END-BROWSE,
                     2600-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 8080-ARCH-NOT-FOUND,
                     8080-ARCH-NOT-FOUND,
                     8000-ARCH2-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 3100-END-BROWSE,
                     3100-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 8080-ARCH-NOT-FOUND,
                     8080-ARCH-NOT-FOUND,
                     8010-ARCH3-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 3200-END-BROWSE,
                     3200-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 8080-ARCH-NOT-FOUND,
                     8080-ARCH-NOT-FOUND,
                     8020-ARCH4-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 3300-END-BROWSE,
                     3300-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 8080-ARCH-NOT-FOUND,
                     8080-ARCH-NOT-FOUND,
                     8030-ARCH5-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 3400-END-BROWSE,
                     3400-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 8080-ARCH-NOT-FOUND,
                     8080-ARCH-NOT-FOUND,
                     8040-ARCH6-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 3500-END-BROWSE,
                     3500-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 8080-ARCH-NOT-FOUND,
                     8080-ARCH-NOT-FOUND,
                     8050-ARCH-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 3600-END-BROWSE,
                     3600-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 7800-EXIT,
                     7800-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL690' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
