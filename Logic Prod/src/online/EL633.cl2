00001  IDENTIFICATION DIVISION.                                         00010007
00002                                                                   00020007
00003  PROGRAM-ID.                 EL633.                               00030017
00004 *              PROGRAM CONVERTED BY                               00040007
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   00050007
00006 *              CONVERSION DATE 12/28/94 10:04:35.                 00060007
00007 *                            VMOD=2.021                           00070007
00008 *                                                                 00080007
00009 *AUTHOR.        LOGIC,INC.                                        00090007
00010 *               DALLAS, TEXAS.                                    00100007
00011                                                                   00110007
00012 *DATE-COMPILED.                                                   00120007
00013                                                                   00130007
00014 *SECURITY.   *****************************************************00140007
00015 *            *                                                   *00150007
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *00160007
00017 *            *                                                   *00170007
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *00180007
00019 *                                                                *00190007
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *00200007
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *00210007
00022 *            *                                                   *00220007
00023 *            *****************************************************00230007
00024 *                                                                 00240047
00025 *REMARKS.                                                         00250007
00026 *        TRANSACTION - EXB7 - COMPENSATION PAYMENTS/ADJUSTMENTS.  00260007
00024 *                                                                 00260147
101101******************************************************************00261048
101101*                   C H A N G E   L O G                           00262048
101101*                                                                 00263048
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.                00264048
101101*-----------------------------------------------------------------00265048
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE            00266048
101101* EFFECTIVE    NUMBER                                             00267048
101101*-----------------------------------------------------------------00268048
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID) 00269048
101101*                              ADJUSTED REDEFINES EL633AI FILLER  00269148
010803* 010803                   PEMA  ADD 1825013200 FOR DCC
042303* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
022504* 022504                   PEMA ADD GL CODE FOR DCC
093004* 093004                   PEMA ADD NEW GL CODE FOR DCC
060205* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
061605* 061605    2005051300001  PEMA ADD GL NUM EDIT FOR DCC
122105* 122105    2005033100001  PEMA ADD GL NUM EDIT FOR CSI
032806* 032806                   PEMA ADD MORE GL NUMBERS FOR CSI
071806* 071806    2006012600002  PEMA CHANGE DCC GL NUMBERS
080206* 080206    2006012600002  PEMA ADD GL NUMBER FOR CID & DCC
092506* 092506                   PEMA ADD NEW GL CODE FOR DCC
031909* 031909    2009030300001  AJRA ADD NEW GL CODE FOR CID
040109* 040109    2008050500001  AJRA ADD GL NUMBERS FOR CCC
031710* 031710  CR2009100700001  PEMA CORRECT MATH ON TYPE 'U'
120711* 120711  CR2011120100004  PEMA ADD GL NUMBER FOR CCC
031912* 031912  CR2011120900003  AJRA AHL COMPANY CODE
100713* 100713  CR2013100700001  AJRA ADD 1825091000 FOR CID
081414* 081414    2014012300001  PEMA  ADD PROCESSING FOR CARRIER 7 DCC
110315* 110315  CR2015101400001  PEMA ADD NEW DCC G/L #'S FOR ACH
111715* 111715  IR2015111600001  PEMA  CHG G/L ACCTNO ON CID ACH
021716* 021716  CR2016021000003  PEMA  ADD NEW G/L FOR MACHENS ACCTS
111016* 111016  CR2016110900002  TANA  REPLACE GL NUMBERS FOR CCC
060817* 060817  CR2017060700005  PEMA  ADD NEW G/L FOR VPP
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
101101******************************************************************00269248
                                                                        00269348
00027                                                                   00270007
00028  ENVIRONMENT DIVISION.                                            00280007
00029  DATA DIVISION.                                                   00290007
00030  EJECT                                                            00300007
00031  WORKING-STORAGE SECTION.                                         00310007
00032  77  FILLER  PIC X(32)  VALUE '********************************'. 00320007
00033  77  FILLER  PIC X(32)  VALUE '*    EL633 WORKING STORAGE     *'. 00330007
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.021 *********'. 00340007
00035                                                                   00350007
CIDMOD*77  FILLER  PIC X(32)  VALUE '/////// SAVE COFA MASTER /////'.   00360022
CIDMOD*77  SV-COFA PIC X(42)  VALUE SPACES.                             00370022
CIDMOD*77  K-SPACE PIC X(11)  VALUE SPACES.                             00380022
CIDMOD                                                                  00420022
CIDMOD 01  DATE-WORK-AREAS.                                             00430000
CIDMOD     05  WS-GREG-STORE.                                           00440000
CIDMOD         10  WS-GS-MM  PIC XX.                                    00450000
CIDMOD         10  FILLER    PIC X.                                     00460000
CIDMOD         10  WS-GS-DD  PIC XX.                                    00470000
CIDMOD         10  FILLER    PIC X.                                     00480000
CIDMOD         10  WS-GS-YY  PIC XX.                                    00490000
CIDMOD     05  WS-SDTE-STORE.                                           00500000
CIDMOD         10  WS-SS-MM  PIC XX.                                    00510000
CIDMOD         10  WS-SS-DD  PIC XX.                                    00520000
CIDMOD         10  WS-SS-YY  PIC XX.                                    00530000
00036                              COPY ELCSCTM.                        00540007
00037                              COPY ELCSCRTY.                       00550007
00038                                                                   00560007
00039     EJECT                                                         00570007
00040                                                                   00580007
00041  01  STANDARD-AREAS.                                              00590007
00042      12  SC-ITEM             PIC  S9(4) COMP VALUE +1.            00600007
00043      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.         00610007
00044      12  MAP-NAME            PIC  X(8)       VALUE 'EL633A'.      00620007
00045      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL633S'.      00630007
00046      12  SCREEN-NUMBER       PIC  X(4)       VALUE '633A'.        00640007
00047      12  TRANS-ID            PIC  X(4)       VALUE 'EXB7'.        00650007
00048      12  EL6331-TRANS-ID     PIC  X(4)       VALUE 'EXB8'.        00660007
00049      12  EL640-TRANS-ID      PIC  X(4)       VALUE 'EXC1'.        00670007
00050      12  EL642-TRANS-ID      PIC  X(4)       VALUE 'EXH7'.        00680007
00051      12  EL652-TRANS-ID      PIC  X(4)       VALUE 'EXD4'.        00690007
00052      12  THIS-PGM            PIC  X(8)       VALUE 'EL633'.       00700007
00053      12  PGM-NAME            PIC  X(8).                           00710007
00054      12  TIME-IN             PIC S9(7).                           00720007
00055      12  TIME-OUT-R  REDEFINES  TIME-IN.                          00730007
00056          16  FILLER          PIC  X.                              00740007
00057          16  TIME-OUT        PIC  99V99.                          00750007
00058          16  FILLER          PIC  X(2).                           00760007
00059      12  EL640               PIC  X(8)       VALUE 'EL640'.       00770007
00060      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.       00780007
00061      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.       00790007
00062      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.       00800007
00063      12  XCTL-6331           PIC  X(8)       VALUE 'EL6331'.      00810007
00064      12  XCTL-640            PIC  X(8)       VALUE 'EL640'.       00820007
00065      12  XCTL-642            PIC  X(8)       VALUE 'EL642'.       00830007
00066      12  XCTL-652            PIC  X(8)       VALUE 'EL652'.       00840007
00067      12  LINK-001            PIC  X(8)       VALUE 'EL001'.       00850007
00068      12  LINK-004            PIC  X(8)       VALUE 'EL004'.       00860007
00069      12  RETURNED-FROM       PIC  X(8)       VALUE SPACES.        00870007
00070      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.     00880007
00071      12  PYAJ-FILE-ID        PIC  X(8)       VALUE 'ERPYAJ'.      00890007
00072      12  CHKQ-FILE-ID        PIC  X(8)       VALUE 'ERCHKQ'.      00900021
00073      12  COMP-FILE-ID        PIC  X(8)       VALUE 'ERCOMP'.      00910007
00074      12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.        00922035
00075      12  WS-CURRENT-BIN-DT   PIC  X(2)       VALUE SPACES.        00930007
00076      12  PYAJ-READ-SW        PIC  X          VALUE 'Y'.           00940007
00077          88  PYAJ-1ST-READ                   VALUE 'Y'.           00950007
00078      12  FIRST-ADD-SW        PIC  X          VALUE 'Y'.           00960007
00079          88  FIRST-ADD                       VALUE 'Y'.           00970007
00080      12  ZERO-NDX            PIC  9          VALUE ZERO.          00980007
00081      12  WORK-SEQ-NO         PIC S9(8)       COMP-3 VALUE ZEROS.  00990007
00082      12  WORK-SEQ-TIME       PIC  9(6).                           01000007
00083      12  WORK-SEQ-HHMMSS  REDEFINES  WORK-SEQ-TIME.               01010007
00084          16  WORK-SEQ-HHMMS  PIC  9(5).                           01020007
00085          16  FILLER          PIC  9.                              01030007
00086      12  WORK-DATE-JULIAN.                                        01040007
00087          16  WORK-JULIAN-YR  PIC  99         VALUE ZEROS.         01050007
00088          16  WORK-JULIAN-DD  PIC  999        VALUE ZEROS.         01060007
00089      12  JULIAN-YY-DD        PIC  9(4)       COMP-3 VALUE ZEROS.  01070007
00090      12  CHECK-REC-TYPE      PIC  X          VALUE SPACE.         01080007
00091          88  VALID-REC-TYPE                  VALUE  'R' 'D' 'C'   01090007
00092                                                     'S' 'T' 'U'   01100007
00093                                                     'X' 'Y' 'Z'   01110007
00094                                                     'F'.          01120007
CIDMOD     12  CHECK-CANC-TYPE     PIC  X          VALUE SPACE.         01130022
CIDMOD         88  VALID-CANC-TYPE                 VALUE 'N' 'Y' ' '.   01140022
00095      12  FORCE-SHOW-SW       PIC  X          VALUE SPACE.         01150007
00096          88  FORCE-SHOW                      VALUE  'Y'.          01160007
00097                                                                   01170007
00098      12  WS-EOM-DTS OCCURS 13  TIMES                              01180007
00099                               INDEXED BY PINDEX.                  01190007
00100          16  WS-EOM-DT               PIC XX.                      01200007
CIDMOD*    12  COFA-FILE-ID        PIC  X(8)       VALUE 'COFAXXX '.    01210022
00101                                                                   01220007
00102      EJECT                                                        01230007
00103      12  WS-ERROR-CODES.                                          01240007
00104          16  ER-0000         PIC  X(4)       VALUE '0000'.        01250007
00105          16  ER-0008         PIC  X(4)       VALUE '0008'.        01260007
00106          16  ER-0023         PIC  X(4)       VALUE '0023'.        01270007
00107          16  ER-0029         PIC  X(4)       VALUE '0029'.        01280007
00108          16  ER-0070         PIC  X(4)       VALUE '0070'.        01290007
00109          16  ER-0587         PIC  X(4)       VALUE '0587'.        01300007
00110          16  ER-2056         PIC  X(4)       VALUE '2056'.        01310007
00111          16  ER-2230         PIC  X(4)       VALUE '2230'.        01320007
00112          16  ER-2231         PIC  X(4)       VALUE '2231'.        01330007
00113          16  ER-2232         PIC  X(4)       VALUE '2232'.        01340007
00114          16  ER-2233         PIC  X(4)       VALUE '2233'.        01350007
00115          16  ER-2234         PIC  X(4)       VALUE '2234'.        01360007
00116          16  ER-2235         PIC  X(4)       VALUE '2235'.        01370007
00117          16  ER-2236         PIC  X(4)       VALUE '2236'.        01380007
00118          16  ER-2237         PIC  X(4)       VALUE '2237'.        01390007
00119          16  ER-2238         PIC  X(4)       VALUE '2238'.        01400007
00120          16  ER-2239         PIC  X(4)       VALUE '2239'.        01410007
00121          16  ER-2244         PIC  X(4)       VALUE '2244'.        01420007
00122          16  ER-2245         PIC  X(4)       VALUE '2245'.        01430007
00123          16  ER-2246         PIC  X(4)       VALUE '2246'.        01440007
00124          16  ER-2370         PIC  X(4)       VALUE '2370'.        01450007
00125          16  ER-2432         PIC  X(4)       VALUE '2432'.        01460007
00126          16  ER-2449         PIC  X(4)       VALUE '2449'.        01470007
00127          16  ER-2587         PIC  X(4)       VALUE '2587'.        01480007
00128          16  ER-2588         PIC  X(4)       VALUE '2588'.        01490007
00129          16  ER-2595         PIC  X(4)       VALUE '2595'.        01500007
00130          16  ER-2596         PIC  X(4)       VALUE '2596'.        01510007
00131          16  ER-2763         PIC  X(4)       VALUE '2763'.        01520007
00132          16  ER-2929         PIC  X(4)       VALUE '2929'.        01530007
CIDMOD         16  ER-2957         PIC  X(4)       VALUE '2957'.        01531007
CIDMOD         16  ER-2958         PIC  X(4)       VALUE '2958'.        01532007
CIDMOD*        16  ER-2959         PIC  X(4)       VALUE '2958'.        01532123
CIDMOD         16  ER-2960         PIC  X(4)       VALUE '2960'.        01533024
00133          16  ER-3020         PIC  X(4)       VALUE '3020'.        01540007
00134                                                                   01550007
CIDMOD*************************************************************     01560000
CIDMOD*                      TABLE OF STATE NAMES                       01570000
CIDMOD*************************************************************     01580000
CIDMOD                                                                  01590000
CIDMOD 01  CHECK-STATE-CODE            PIC XX      VALUE SPACE.         01600000
CIDMOD     88  VALID-STATE-CODE    VALUE 'AK' 'AL' 'AR' 'AZ' 'CA'       01610000
CIDMOD                                   'CD' 'CO' 'CT' 'DC' 'DE'       01620000
CIDMOD                                   'FL' 'GA' 'GM' 'HI' 'IA'       01630000
CIDMOD                                   'ID' 'IL' 'IN' 'KS' 'KY'       01640000
CIDMOD                                   'LA' 'MA' 'MD' 'ME' 'MI'       01650000
CIDMOD                                   'MN' 'MO' 'MS' 'MT' 'MX'       01660000
CIDMOD                                   'NC' 'ND' 'NE' 'NH' 'NJ'       01670000
CIDMOD                                   'NM' 'NV' 'NY' 'OF' 'OH'       01680000
CIDMOD                                   'OK' 'OR' 'PA' 'PI' 'PR'       01690000
CIDMOD                                   'RI' 'SC' 'SD' 'TN' 'TX'       01700000
CIDMOD                                   'UT' 'VA' 'VI' 'VT' 'WA'       01710000
CIDMOD                                   'WI' 'WV' 'WY'.                01720000
CIDMOD                                                                  01721024
CIDMOD 01  CHECK-GL-ACCT         PIC X(10)  VALUE SPACE.                01722026
CIDMOD     88  VALID-GL-ACCOUNT  VALUE '1108121010'                     01723026
111715                                 '1108124700'
CIDMOD                                 '1108125100'                     01724026
CIDMOD                                 '1721211400'                     01725026
CIDMOD                                 '1825011200'                     01726026
CIDMOD                                 '1825011300'                     01726126
100713                                 '1825091000'
CIDMOD                                 '1825099050'                     01727026
031909                                 '8505700033'
CIDMOD                                 '8506400030'                     01728026
CIDMOD                                 '8507200020'
080206                                 '8507200010'
021716                                 '2725010160'.

071806     88  VALID-DCC-GL-ACCOUNT  VALUE '2725040300'
071806                                     '2725040320'
071806                                     '2725040330'
071806                                     '2725040310'
071806                                     '8506400030'
080206                                     '8507200010'
092506                                     '1108121250'.
111016*    88  VALID-CSI-GL-ACCOUNT  VALUE '2725040100'
111016*                                    '2725040110'
111016*                                    '2725040120'
111016*                                    '2725040130'
111016*                                    '2725020400'.
111016     88  VALID-CSI-GL-ACCOUNT  VALUE '1108121010'
111016                                     '1825013200'
111016                                     '1825013300'
111016                                     '1825013400'.
040109
040109     88  VALID-CCC-GL-ACCOUNT  VALUE '1825013100'
040109                                     '1825013200'
040109                                     '1825013300'
040109                                     '1825013400'
040109                                     '8506400030'
040109                                     '8507200010'
040109                                     '8507200020'
120711                                     '1108121010'.

           88  VALID-VPP-GL-ACCOUNT  VALUE '2725040510'
                                           '2725040520'
060817                                     '7206100400'
060817                                     '7206104100'.

062121     88  VALID-FNL-GL-ACCOUNT  VALUE '1108121010'
062121                                     '1721211400'
062121                                     '1825011100'
062121                                     '1825011200'
062121                                     '1825011300'
062121                                     '1825099050'
062121                                     '2718000110'
062121                                     '2718000120'
062121                                     '8506400030'
062121                                     '8507200010'.
CIDMOD                                                                  01730024
CIDMOD*01  WS-ACCT-BREAK.                                               01740022
CIDMOD*    05  WS-ACCT-1               PIC X.                           01750022
CIDMOD*    05  FILLER                  PIC X(6).                        01760022
CIDMOD*                                                                 01770022
00135  01  WORK-AREA.                                                   01780007
PEMMOD     12  WS-SAVE-ERPYAJ      PIC X(200)      VALUE SPACES.        01781046
00136      12  QID.                                                     01790007
00137          16  QID-TERM        PIC X(04).                           01800007
00138          16  FILLER          PIC X(04)       VALUE '633A'.        01810007
00139      12  DEEDIT-FIELD        PIC X(11).                           01820007
00140      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(11).       01830007
CIDMOD     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD PIC S9(9)V99.     01840000
00141      12  DATE-TEST-AREA      PIC  9(6).                           01850007
00142      12  DATE-TEST-AREA-R  REDEFINES  DATE-TEST-AREA.             01860007
00143          16  DATE-TEST-MM    PIC  99.                             01870007
00144          16  DATE-TEST-DD    PIC  99.                             01880007
00145          16  DATE-TEST-YY    PIC  99.                             01890007
00146      12  DIVIDE-RESULT       PIC  99.                             01900007
00147      12  DIVIDE-REMAINDER    PIC  9.                              01910007
00148      12  PREV-BIN-MAINT-DT   PIC  XX           VALUE SPACES.      01920007
00149      12  PREV-MAINT-DT       PIC  X(8)         VALUE SPACES.      01930007
00150      12  PREV-BIN-BL-DT      PIC  XX           VALUE SPACES.      01940007
00151      12  PREV-BL-DT          PIC  X(8)         VALUE SPACES.      01950007
00152      12  TOTAL-ACCT-AMT      PIC S9(7)V99      VALUE ZEROS.       01960007
00153      12  TOTAL-ACCT-NET      PIC S9(7)V99      VALUE ZEROS.       01970007
00154      12  WS-SAVE-INDEX-VALUE PIC S9(4) COMP    VALUE ZEROS.       01980007
00155      12  WS-SAVE-NDX-VALUE   PIC S9(4) COMP    VALUE ZEROS.       01990007
00156                                                                   02000007
00157  01  ACCESS-KEYS.                                                 02010007
00158      12  ERPYAJ-KEY.                                              02020007
00159          16  PYAJ-COMP-CD            PIC  X      VALUE SPACE.     02030007
00160          16  PYAJ-CARRIER            PIC  X      VALUE SPACES.    02040007
00161          16  PYAJ-GROUPING           PIC  X(6)   VALUE SPACES.    02050007
00162          16  PYAJ-FIN-RESP           PIC  X(10)  VALUE SPACES.    02060007
00163          16  PYAJ-ACCOUNT            PIC  X(10)  VALUE SPACES.    02070007
00164          16  PYAJ-FILE-SEQ-NO        PIC S9(8)   VALUE +0  COMP.  02080007
00165          16  PYAJ-RECORD-TYPE        PIC  X      VALUE SPACES.    02090007
00166                                                                   02100007
00167      12  ERPYAJ-RECORD-LENGTH        PIC S9(4) COMP VALUE +200.   02110007
00168      12  ERPYAJ-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +223.   02120007
00169                                                                   02130007
00170      12  ERCHKQ-KEY.                                              02140021
00171          16  CHKQ-COMPANY-CD         PIC  X      VALUE SPACE.     02150021
00172          16  CHKQ-CONTROL-NUMBER     PIC S9(8)   VALUE +0  COMP.  02160021
00173          16  CHKQ-SEQUENCE-NUMBER    PIC S9(4)   VALUE +0  COMP.  02170021
00174      12  ERCOMP-KEY.                                              02180007
00175          16  COMP-COMP-CD            PIC  X      VALUE SPACE.     02190007
00176          16  COMP-CARRIER            PIC  X      VALUE SPACES.    02200007
00177          16  COMP-GROUPING           PIC  X(6)   VALUE SPACES.    02210007
00178          16  COMP-FIN-RESP           PIC  X(10)  VALUE SPACES.    02220007
00179          16  COMP-ACCOUNT            PIC  X(10)  VALUE SPACES.    02230007
00180          16  COMP-RECORD-TYPE        PIC  X      VALUE SPACES.    02240007
CIDMOD*    12  COFA-KEY-X.                                              02250022
CIDMOD*        16  COFA-COMPANY-X          PIC  X(4)   VALUE SPACES.    02260022
CIDMOD*        16  COFA-ACCOUNT.                                        02270022
CIDMOD*            20  COFA-FILLER         PIC  X(11)  VALUE SPACES.    02280022
CIDMOD*            20  COFA-MSA-ACCT       PIC  X(07)  VALUE SPACES.    02290022
CIDMOD*                                                                 02300022
00181  EJECT                                                            02310007
00182                                      COPY ELCDATE.                02320007
00183  EJECT                                                            02330007
00184                                      COPY ELCLOGOF.               02340007
00185  EJECT                                                            02350007
00186                                      COPY ELCATTR.                02360007
00187  EJECT                                                            02370007
00188                                      COPY ELCEMIB.                02380007
00189  EJECT                                                            02390007
00190                                      COPY ELCINTF.                02400007
00191      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                 02410007
00192          16  PI-PYAJ-FILE-SW             PIC  X.                  02420007
00193              88  END-OF-ACCT                 VALUE 'A'.           02430007
00194              88  END-OF-ACCT-FULL-PAGE       VALUE 'B'.           02440007
00195              88  END-OF-FILE                 VALUE 'X'.           02450007
00196              88  TOP-OF-FILE                 VALUE 'T'.           02460007
00197              88  PAGE-FULL                   VALUE 'F'.           02470007
00198              88  NO-RECORDS                  VALUE 'Y'.           02480007
00199              88  NOT-OPEN                    VALUE 'Z'.           02490007
00200          16  PI-PREV-FUNCTION            PIC  X.                  02500007
00201          16  PI-SAV-FUNCTION             PIC  X.                  02510007
00202          16  PI-PREV-PFKEY               PIC  X.                  02520007
00203          16  PI-SEQ-NOS.                                          02530007
00204              20  FILLER  OCCURS 13 TIMES                          02540007
00205                              INDEXED BY NDX.                      02550007
00206                  24  PI-REC-TYPE         PIC  X.                  02560007
00207                  24  PI-FILE-SEQ-NO      PIC  S9(8)       COMP.   02570007
00208          16  PI-SAV-ENDING-PYAJ-KEY.                              02580007
00209              20  PI-SAV-COMP-CD          PIC  X.                  02590007
00210              20  PI-SAV-COMP-CONTROL.                             02600007
00211                  24  PI-SAV-CARRIER      PIC  X.                  02610007
00212                  24  PI-SAV-GROUPING     PIC  X(6).               02620007
00213                  24  PI-SAV-FIN-RESP     PIC  X(10).              02630007
00214                  24  PI-SAV-ACCOUNT      PIC  X(10).              02640007
00215                  24  PI-SAV-FILE-SEQ-NO  PIC  S9(8)          COMP.02650007
00216                  24  PI-SAV-RECORD-TYPE  PIC  X.                  02660007
00217          16  PI-START-PYAJ-KEY           PIC  X(33).              02670007
00218          16  PI-SAV-ACCT-AMT             PIC  S9(7)V99.           02680007
00219          16  PI-SAV-ACCT-NET             PIC  S9(7)V99.           02690007
00220          16  PI-SAV-PREV-AMT             PIC  S9(7)V99.           02700007
00221          16  PI-SAV-PREV-NET             PIC  S9(7)V99.           02710007
00222          16  PI-TOTAL-DISPLAYED-SW       PIC  X.                  02720007
00223              88  PI-TOTAL-DISPLAYED               VALUE 'Y'.      02730007
00224          16  PI-FULL-INDX                PIC  S9(4)      COMP.    02740007
00225          16  PI-FRST-FILE-SEQ-NO         PIC  S9(8)          COMP.02750007
00226          16  PI-FRST-RECORD-TYPE         PIC  X.                  02760007
00227          16  PI-PREV-FILE-SEQ-NO         PIC  S9(8)          COMP.02770007
00228          16  PI-PREV-RECORD-TYPE         PIC  X.                  02780007
00229          16  PI-LAST-FILE-SEQ-NO         PIC  S9(8)          COMP.02790007
00230          16  PI-LAST-RECORD-TYPE         PIC  X.                  02800007
00231          16  PI-PAGE-SW                  PIC  X.                  02810007
00232              88  FIRST-PAGE                          VALUE 'Y'.   02820007
00233          16  FILLER                      PIC  X(450).             02830007
00234  EJECT                                                            02840007
00235                              COPY ELCJPFX.                        02850007
00236                              PIC  X(223).                         02860007
00237  EJECT                                                            02870007
00238                              COPY ELCAID.                         02880007
00239                                                                   02890007
00240  01  FILLER  REDEFINES  DFHAID.                                   02900007
00241      12  FILLER              PIC  X(8).                           02910007
00242      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.      02920007
00243  EJECT                                                            02930007
00244                              COPY EL633S.                         02940007
00245                                                                   02950007
00246  01  MAP-EL633A  REDEFINES  EL633AI.                              02960007
101101     12  FILLER                  PIC  X(87).                      02970048
00248      12  DATA-AREA       OCCURS 13 TIMES                          02980007
00249                              INDEXED BY PINDX.                    02990007
CIDMOD         16  GL-ACCT-LEN         PIC S9(4)              COMP.     03030023
CIDMOD         16  GL-ACCT-ATTRB       PIC  X.                          03040023
CIDMOD         16  GL-ACCT             PIC  X(10).                      03050023
CIDMOD         16  WSL-COMM  REDEFINES GL-ACCT.                         03060023
00254              20  WSL-COMM-DTE.                                    03080007
00255                  24  WSL-MO      PIC  X(2).                       03090007
00256                  24  WSL-DA      PIC  X(2).                       03100007
00257                  24  WSL-YR      PIC  X(2).                       03110007
CIDMOD             20  WSL-COMM-REST   PIC  X(4).                       03130022
CIDMOD         16  GL-STATE-LEN        PIC S9(4)              COMP.     03140023
CIDMOD         16  GL-STATE-ATTRB      PIC  X.                          03150023
CIDMOD         16  GL-STATE            PIC  X(02).                      03160023
CIDMOD         16  GL-CANC-LEN         PIC S9(4)              COMP.     03170023
CIDMOD         16  GL-CANC-ATTRB       PIC  X.                          03180023
CIDMOD         16  GL-CANC             PIC  X.                          03190023
CIDMOD         16  GL-COMM-LEN         PIC S9(4)              COMP.     03200023
CIDMOD         16  GL-COMM-ATTRB       PIC  X.                          03210023
CIDMOD         16  GL-COMM             PIC  X(10).                      03220023
00259          16  RTYPE-LEN           PIC S9(4)              COMP.     03230007
00260          16  RTYPE-ATTRB         PIC  X.                          03240007
00261          16  RTYPE               PIC  X.                          03250007
00262          16  AMT-LEN             PIC S9(4)              COMP.     03260007
00263          16  AMT-ATTRB           PIC  X.                          03270007
00264          16  AMT                 PIC S9(9)V9(2).                  03280007
00265          16  AMTO  REDEFINES                                      03290007
00266              AMT                 PIC Z(7).9(2)-.                  03300007
00267          16  VOID-SW-LEN         PIC S9(4)              COMP.     03310007
00268          16  VOID-SW-ATTRB       PIC  X.                          03320007
00269          16  VOID-SW             PIC  X.                          03330007
00270          16  MDTE-LEN            PIC S9(4)              COMP.     03340007
00271          16  MDTE-ATTRB          PIC  X.                          03350007
CIDMOD         16  MDTE                PIC  X(6).                       03360022
00273          16  BDTE-LEN            PIC S9(4)              COMP.     03370007
00274          16  BDTE-ATTRB          PIC  X.                          03380007
CIDMOD         16  BDTE                PIC  X(6).                       03390022
00276          16  SDTE-LEN            PIC S9(4)              COMP.     03400007
00277          16  SDTE-ATTRB          PIC  X.                          03410007
CIDMOD         16  SDTE                PIC  9(6).                       03420014
00279  EJECT                                                            03430007
00280  LINKAGE SECTION.                                                 03440007
00281  01  DFHCOMMAREA             PIC  X(1024).                        03450007
CIDMOD/                                                                 03450120
CIDMOD                             COPY ERCCHKQ.                        03451020
CIDMOD/                                                                 03460020
00284                              COPY ERCPYAJ.                        03480007
CIDMOD/                                                                 03490020
00288                              COPY ERCCOMP.                        03520007
CIDMOD*                                                                 03530022
CIDMOD*                            COPY AIRL0009.                       03550022
CIDMOD/                                                                 03560000
00290  PROCEDURE DIVISION.                                              03570007
00291                                                                   03580007
00292      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     03590007
00293      MOVE EIBTRMID               TO  QID-TERM.                    03600007
00294      MOVE 2                      TO  EMI-NUMBER-OF-LINES.         03610007
00295                                                                   03620007
00296      IF EIBCALEN = ZERO                                           03630007
00297          GO TO 8800-UNAUTHORIZED-ACCESS.                          03640007
00298                                                                   03650007
00299      IF PI-RETURN-TO-PROGRAM = THIS-PGM                           03660007
00300          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM.                03670007
00301                                                                   03680007
00302      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             03690007
00303      MOVE '5'                    TO  DC-OPTION-CODE.              03700007
00304                                                                   03710007
00305      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                  03720007
00306                                                                   03730007
00307      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.           03740007
00308      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.               03750007
00309                                                                   03760007
00310      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         03770007
00311          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   03780007
00312              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6    03790007
00313              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5    03800007
00314              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4    03810007
00315              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3    03820007
00316              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2    03830007
00317              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1    03840007
00318              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM  03850007
00319              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM    03860007
00320          ELSE                                                     03870007
00321              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM    03880007
00322              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM  03890007
00323              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1    03900007
00324              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2    03910007
00325              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3    03920007
00326              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4    03930007
00327              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5    03940007
00328              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.   03950007
00329                                                                   03960007
00330      MOVE LOW-VALUES             TO  EL633AI.                     03970007
00331                                                                   03980007
00332      MOVE DC-JULIAN-YYDDD        TO  WORK-DATE-JULIAN.            03990007
00333      COMPUTE WORK-SEQ-NO = WORK-JULIAN-DD * 100000.               04000007
00334      MOVE EIBTIME                TO  WORK-SEQ-TIME.               04010007
00335      ADD WORK-SEQ-HHMMS TO WORK-SEQ-NO.                           04020007
00336                                                                   04030007
00337      MOVE ZEROS                  TO  TOTAL-ACCT-AMT               04040007
00338                                      TOTAL-ACCT-NET.              04050007
00339                                                                   04060007
00340      IF RETURNED-FROM = XCTL-6331 OR                              04070019
00341                         XCTL-640  OR                              04080019
00342                         XCTL-642  OR                              04090019
00343                         XCTL-652                                  04100019
00344          PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0690-EXIT.        04110007
00345                                                                   04120007
00346      IF EIBTRNID NOT = TRANS-ID                                   04130007
00347          MOVE 'T'                TO  PI-PYAJ-FILE-SW              04140007
00348          MOVE 'Y'                TO  PI-PAGE-SW                   04150007
00349          MOVE ZEROS              TO  PI-SEQ-NOS                   04160007
00350          MOVE ZEROS              TO  PI-SAV-ACCT-AMT              04170007
00351                                      PI-SAV-ACCT-NET              04180007
00352                                      PI-SAV-PREV-AMT              04190007
00353                                      PI-SAV-PREV-NET              04200007
00354          IF (EIBTRNID NOT = EL640-TRANS-ID  AND                   04210007
00355              EIBTRNID NOT = EL642-TRANS-ID  AND                   04220007
CIDMOD             EIBTRNID NOT = EL652-TRANS-ID) OR                    04230000
CIDMOD             PI-CR-FIN-RESP = SPACES                              04240000
00358              GO TO 8100-SEND-INITIAL-MAP                          04250007
00359          ELSE                                                     04260007
00360              IF EIBTRNID = (EL6331-TRANS-ID  OR                   04270007
00361                             EL640-TRANS-ID   OR                   04280007
00362                             EL642-TRANS-ID)  AND                  04290007
00363                 PI-CR-CONTROL-IN-PROGRESS  = SPACES               04300007
00364                 GO TO 8100-SEND-INITIAL-MAP                       04310007
00365              ELSE                                                 04320007
00366                  MOVE DFHENTER   TO  EIBAID                       04330007
00367                  MOVE 'S'        TO  MAINTI                       04340007
00368                  MOVE PI-CR-CARRIER                               04350007
00369                                  TO  CARRIERI                     04360007
00370                  MOVE PI-CR-GROUPING                              04370007
00371                                  TO  GROUPI                       04380007
00372                  MOVE PI-CR-FIN-RESP                              04390007
00373                                  TO  FINRESPI                     04400007
00374                  MOVE PI-CR-ACCOUNT                               04410007
00375                                  TO  ACCTI                        04420007
00376                  MOVE 1          TO  CARRIERL  MAINTA             04430007
00377                  MOVE 3          TO  GROUPL                       04440007
00378                  MOVE 6          TO  FINRESPL  ACCTL              04450007
00379                  GO TO 0400-VALIDATE-KEY-DATA.                    04460007
00380                                                                   04470007
00381      EXEC CICS HANDLE CONDITION                                   04480007
00382          PGMIDERR  (9600-PGMID-ERROR)                             04490007
00383          ERROR     (9990-ABEND)                                   04500007
00384      END-EXEC.                                                    04510007
00385                                                                   04520007
00386      IF EIBAID = DFHCLEAR                                         04530007
00387          GO TO 9400-CLEAR.                                        04540007
00388                                                                   04550007
00389      IF PI-PROCESSOR-ID = 'LGXX'                                  04560007
00390          GO TO 0200-RECEIVE.                                      04570007
00391                                                                   04580007
00392      EXEC CICS READQ TS                                           04590007
00393          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       04600007
00394          INTO   (SECURITY-CONTROL)                                04610007
00395          LENGTH (SC-COMM-LENGTH)                                  04620007
00396          ITEM   (SC-ITEM)                                         04630007
00397      END-EXEC.                                                    04640007
00398                                                                   04650007
00399      MOVE SC-CREDIT-DISPLAY (15)  TO PI-DISPLAY-CAP.              04660007
00400      MOVE SC-CREDIT-UPDATE  (15)  TO PI-MODIFY-CAP.               04670007
00401                                                                   04680007
00402      IF NOT DISPLAY-CAP                                           04690007
00403          MOVE 'READ'          TO SM-READ                          04700007
00404          PERFORM 9995-SECURITY-VIOLATION                          04710007
00405          MOVE ER-0070         TO  EMI-ERROR                       04720007
00406          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 04730007
00407          GO TO 8100-SEND-INITIAL-MAP.                             04740007
00408                                                                   04750007
00409  EJECT                                                            04760007
00410  0200-RECEIVE.                                                    04770007
00411                                                                   04780007
00412      IF EIBAID = DFHPA1  OR  DFHPA2  OR  DFHPA3                   04790007
00413          MOVE ER-0008            TO  EMI-ERROR                    04800007
00414          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               04810007
00415          MOVE -1                 TO  MAINTL                       04820007
00416          GO TO 8200-SEND-DATAONLY.                                04830007
00417                                                                   04840007
00418      EXEC CICS RECEIVE                                            04850007
00419          MAP     (MAP-NAME)                                       04860007
00420          MAPSET  (MAPSET-NAME)                                    04870007
00421          INTO    (EL633AI)                                        04880007
00422      END-EXEC.                                                    04890007
00423                                                                   04900007
00424      IF MAINTI = 'S' AND                                          04910007
00425         EIBAID = DFHENTER                                         04920007
00426          MOVE ZEROS              TO  PI-SAV-ACCT-AMT              04930007
00427                                      PI-SAV-ACCT-NET              04940007
00428                                      PI-SAV-PREV-AMT              04950007
00429                                      PI-SAV-PREV-NET              04960007
00430                                      PI-FRST-FILE-SEQ-NO          04970007
00431          MOVE SPACES             TO  PI-FRST-RECORD-TYPE.         04980007
00432                                                                   04990007
00433      IF PFENTERL = ZERO                                           05000007
00434          GO TO 0300-CHECK-PFKEYS.                                 05010007
00435                                                                   05020007
00436      IF (PFENTERI  IS NUMERIC)                                    05030007
00437        AND (PFENTERI GREATER 0 AND LESS 25)                       05040007
00438          MOVE PF-VALUES (PFENTERI)  TO  EIBAID                    05050007
00439      ELSE                                                         05060007
00440          MOVE ER-0029               TO  EMI-ERROR                 05070007
00441          GO TO 0320-INPUT-ERROR.                                  05080007
00442                                                                   05090007
00443  0300-CHECK-PFKEYS.                                               05100007
00444      IF EIBAID = DFHPF23                                          05110007
00445          GO TO 8810-PF23.                                         05120007
00446                                                                   05130007
00447      IF EIBAID = DFHPF24                                          05140007
00448          GO TO 9200-RETURN-MAIN-MENU.                             05150007
00449                                                                   05160007
00450      IF EIBAID = DFHPF12                                          05170007
00451          GO TO 9500-PF12.                                         05180007
00452                                                                   05190007
00453      IF EIBAID = DFHPF3                                           05200007
00454          IF PI-CR-CONTROL-IN-PROGRESS NOT =      SPACES           05210019
00455              PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT      05220007
00456              MOVE XCTL-652       TO  PGM-NAME                     05230007
CIDMOD             IF ERCOMP-KEY  NOT =      SPACES                     05240019
CIDMOD                 MOVE COMP-CARRIER   TO  PI-CR-CARRIER            05250000
CIDMOD                 MOVE COMP-GROUPING  TO  PI-CR-GROUPING           05260000
CIDMOD                 MOVE COMP-FIN-RESP  TO  PI-CR-FIN-RESP           05270000
CIDMOD                 MOVE COMP-ACCOUNT   TO  PI-CR-ACCOUNT            05280000
CIDMOD             END-IF                                               05290000
CIDMOD                                                                  05300000
00457              IF PI-CR-ACCOUNT = LOW-VALUES                        05310007
00458                  MOVE 'G'        TO  PI-CR-TYPE                   05320007
00459                  GO TO 9300-XCTL                                  05330007
00460              ELSE                                                 05340007
00461                  MOVE 'A'        TO  PI-CR-TYPE                   05350007
00462                  GO TO 9300-XCTL                                  05360007
00463          ELSE                                                     05370007
00464             MOVE ER-3020         TO  EMI-ERROR                    05380007
00465             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT            05390007
00466             GO TO 8100-SEND-INITIAL-MAP.                          05400007
00467                                                                   05410007
00468      IF EIBAID = DFHPF4                                           05420007
00469          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT          05430007
00470          MOVE XCTL-6331          TO  PGM-NAME                     05440007
00471          GO TO 9300-XCTL.                                         05450007
00472                                                                   05460007
00473      IF EIBAID = DFHPF5                                           05470007
00474          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT          05480007
00475          MOVE SPACES TO PI-PROGRAM-WORK-AREA                      05490007
00476          MOVE XCTL-640           TO  PGM-NAME                     05500007
00477          IF PI-CR-ACCOUNT = LOW-VALUES                            05510007
00478              MOVE SPACES     TO  PI-CR-CONTROL-IN-PROGRESS        05520007
CIDMOD             IF ACCTI = LOW-VALUES                                05530000
CIDMOD                MOVE 'G'         TO  PI-CR-TYPE                   05540000
CIDMOD             END-IF                                               05550000
00479              GO TO 9300-XCTL                                      05560007
00480          ELSE                                                     05570007
00481              MOVE 'A'        TO  PI-CR-TYPE                       05580007
00482              GO TO 9300-XCTL.                                     05590007
00483                                                                   05600007
00484      IF EIBAID = DFHPF6                                           05610007
00485         IF PI-GA-BILLING                                          05620007
00486             PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT       05630007
00487             MOVE SPACES TO PI-PROGRAM-WORK-AREA                   05640007
00488             MOVE XCTL-642        TO  PGM-NAME                     05650007
00489             IF PI-CR-ACCOUNT = LOW-VALUES                         05660007
00490                  MOVE 'G'        TO  PI-CR-TYPE                   05670007
00491                  GO TO 9300-XCTL                                  05680007
00492              ELSE                                                 05690007
00493                  MOVE SPACES     TO  PI-CR-CONTROL-IN-PROGRESS    05700007
00494                  GO TO 9300-XCTL                                  05710007
00495         ELSE                                                      05720007
00496             MOVE ER-2929         TO  EMI-ERROR                    05730007
00497             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              05740007
00498             MOVE AL-UNBON        TO  PFENTERA                     05750007
00499             IF PFENTERL = ZERO                                    05760007
00500                 MOVE -1          TO  MAINTL                       05770007
00501                 GO TO 8200-SEND-DATAONLY                          05780007
00502             ELSE                                                  05790007
00503                 MOVE -1          TO  PFENTERL                     05800007
00504                 GO TO 8200-SEND-DATAONLY.                         05810007
00505                                                                   05820007
00506      IF EIBAID = DFHENTER  OR  DFHPF1  OR  DFHPF2                 05830007
00507          GO TO 0400-VALIDATE-KEY-DATA.                            05840007
00508                                                                   05850007
00509  0320-INPUT-ERROR.                                                05860007
00510      MOVE ER-0029                TO  EMI-ERROR.                   05870007
00511                                                                   05880007
00512      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  05890007
00513                                                                   05900007
00514      MOVE AL-UNBON               TO  PFENTERA.                    05910007
00515                                                                   05920007
00516      IF PFENTERL = ZERO                                           05930007
00517          MOVE -1                 TO  MAINTL                       05940007
00518      ELSE                                                         05950007
00519          MOVE -1                 TO  PFENTERL.                    05960007
00520                                                                   05970007
00521      GO TO 8200-SEND-DATAONLY.                                    05980007
00522  EJECT                                                            05990007
00523  0400-VALIDATE-KEY-DATA.                                          06000007
00524      IF MODIFY-CAP  OR (EIBAID = DFHPF1 OR DFHPF2)                06010007
00525          NEXT SENTENCE                                            06020007
00526        ELSE                                                       06030007
00527         IF MAINTI NOT = 'S'                                       06040007
00528          MOVE 'UPDATE'       TO SM-READ                           06050007
00529          PERFORM 9995-SECURITY-VIOLATION                          06060007
00530          MOVE ER-0070        TO EMI-ERROR                         06070007
00531          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 06080007
00532          GO TO 8100-SEND-INITIAL-MAP.                             06090007
00533                                                                   06100007
00534      MOVE PI-COMPANY-CD          TO  PI-SAV-COMP-CD.              06110007
00535                                                                   06120007
00536      IF EIBAID = DFHPF1                                           06130007
00537          GO TO 4000-BROWSE-FRWD.                                  06140007
00538                                                                   06150007
00539      IF EIBAID   = DFHPF1   AND                                   06160007
00540         CARRIERL = ZEROS    AND                                   06170007
00541         GROUPL   = ZEROS    AND                                   06180007
00542         FINRESPL = ZEROS    AND                                   06190007
00543         ACCTL    = ZEROS                                          06200007
00544          GO TO 4000-BROWSE-FRWD.                                  06210007
00545                                                                   06220007
CIDMOD     IF PI-PROCESSOR-ID = 'LGXX'                                  06230019
CIDMOD         IF MAINTI = 'C'                                          06240019
CIDMOD             MOVE AL-UANON       TO MAINTA                        06250000
CIDMOD             MOVE MAINTI         TO PI-SAV-FUNCTION               06260000
CIDMOD             GO TO CSO-BRANCH                                     06270000
CIDMOD         END-IF                                                   06280000
CIDMOD     END-IF.                                                      06290000
CIDMOD                                                                  06300000
CIDMOD     IF MAINTI = 'C' OR  'S'                                      06320013
00547          MOVE AL-UANON           TO  MAINTA                       06330007
00548          MOVE MAINTI             TO  PI-SAV-FUNCTION              06340007
00549      ELSE                                                         06350007
00550          MOVE -1                 TO  MAINTL                       06360007
00551          MOVE ER-0023            TO  EMI-ERROR                    06370007
00552          MOVE AL-UABON           TO  MAINTA                       06380007
00553          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              06390007
00554                                                                   06400007
CIDMOD CSO-BRANCH.                                                      06410000
CIDMOD                                                                  06420000
00555      IF MAINTI = 'C'                                              06430007
00556        AND PI-PREV-FUNCTION NOT = 'S' AND 'C'                     06440007
00557          MOVE -1                 TO  MAINTL                       06450007
00558          MOVE ER-2056            TO  EMI-ERROR                    06460007
00559          MOVE AL-UABON           TO  MAINTA                       06470007
00560          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              06480007
00561                                                                   06490007
00562      IF CARRIERL = ZEROS  AND                                     06500007
00563         GROUPL   = ZEROS  AND                                     06510007
00564         FINRESPL = ZEROS  AND                                     06520007
00565         ACCTL    = ZEROS                                          06530007
00566          MOVE -1                 TO  CARRIERL                     06540007
00567          MOVE ER-2231            TO  EMI-ERROR                    06550007
00568          MOVE AL-UABON           TO  CARRIERA                     06560007
00569                                      GROUPA                       06570007
00570                                      FINRESPA                     06580007
00571                                      ACCTA                        06590007
00572          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               06600007
00573          GO TO 8200-SEND-DATAONLY.                                06610007
00574                                                                   06620007
00575      IF CARRIERL NOT = ZEROS                                      06630007
00576          MOVE AL-UANON           TO  CARRIERA                     06640007
00577          MOVE CARRIERI           TO  COMP-CARRIER                 06650007
00578                                      PI-SAV-CARRIER               06660007
00579                                      PI-CR-CARRIER                06670007
00580          IF CARRIERI NOT = ZEROS                                  06680007
00581            AND (PI-ZERO-CARRIER  OR  PI-ZERO-CAR-GROUP)           06690007
00582              MOVE ER-2587        TO  EMI-ERROR                    06700007
00583              MOVE -1             TO  CARRIERL                     06710007
00584              MOVE AL-UABON       TO  CARRIERA                     06720007
00585              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           06730007
00586          ELSE                                                     06740007
00587              NEXT SENTENCE                                        06750007
00588      ELSE                                                         06760007
00589          MOVE LOW-VALUES          TO  COMP-CARRIER                06770007
00590                                       PI-SAV-CARRIER.             06780007
00591                                                                   06790007
00592      IF CARRIERL NOT = ZEROS                                      06800007
00593          IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES          06810007
00594              IF PI-CARRIER-SECURITY = CARRIERI                    06820007
00595                  NEXT SENTENCE                                    06830007
00596              ELSE                                                 06840007
00597                  MOVE -1         TO  CARRIERL                     06850007
00598                  MOVE ER-2370    TO  EMI-ERROR                    06860007
00599                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.      06870007
00600                                                                   06880007
00601      IF GROUPL NOT = ZEROS                                        06890007
00602          MOVE AL-UANON           TO  GROUPA                       06900007
00603          MOVE GROUPI             TO  COMP-GROUPING                06910007
00604                                      PI-SAV-GROUPING              06920007
00605                                      PI-CR-GROUPING               06930007
00606          IF GROUPI NOT = ZEROS                                    06940007
00607            AND (PI-ZERO-GROUPING  OR  PI-ZERO-CAR-GROUP)          06950007
00608              MOVE ER-2588        TO  EMI-ERROR                    06960007
00609              MOVE -1             TO  GROUPL                       06970007
00610              MOVE AL-UABON       TO  GROUPA                       06980007
00611              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           06990007
00612          ELSE                                                     07000007
00613              NEXT SENTENCE                                        07010007
00614      ELSE                                                         07020007
00615          MOVE LOW-VALUES         TO  COMP-GROUPING                07030007
00616                                      PI-SAV-GROUPING.             07040007
00617                                                                   07050007
00618      IF FINRESPL NOT = ZEROS                                      07060007
00619          MOVE AL-UANON           TO  FINRESPA                     07070007
00620          MOVE FINRESPI           TO  COMP-FIN-RESP                07080007
00621                                      PI-SAV-FIN-RESP              07090007
00622                                      PI-CR-FIN-RESP               07100007
00623      ELSE                                                         07110007
00624          MOVE LOW-VALUES         TO  COMP-FIN-RESP                07120007
00625                                      PI-SAV-FIN-RESP.             07130007
00626                                                                   07140007
00627      IF ACCTL NOT = ZEROS                                         07150007
00628          MOVE AL-UANON           TO  ACCTA                        07160007
00629          MOVE ACCTI              TO  COMP-ACCOUNT                 07170007
00630                                      PI-SAV-ACCOUNT               07180007
00631                                      PI-CR-ACCOUNT                07190007
00632      ELSE                                                         07200007
00633          MOVE LOW-VALUES         TO  COMP-ACCOUNT                 07210007
00634                                      PI-SAV-ACCOUNT               07220007
00635                                      PI-CR-ACCOUNT.               07230007
00636                                                                   07240007
00637      IF EMI-ERROR = ZEROS                                         07250007
00638          NEXT SENTENCE                                            07260007
00639      ELSE                                                         07270007
00640          IF EIBTRNID = EL640-TRANS-ID OR                          07280007
00641             EIBTRNID = EL642-TRANS-ID OR                          07290007
00642             EIBTRNID = EL6331-TRANS-ID                            07300007
00643              GO TO 8100-SEND-INITIAL-MAP                          07310007
00644          ELSE                                                     07320007
00645              GO TO 8200-SEND-DATAONLY.                            07330007
00646                                                                   07340007
00647      MOVE MAINTI                 TO  PI-PREV-FUNCTION.            07350007
00648                                                                   07360007
00649      IF MAINTI = 'S'                                              07370007
00650          IF EIBAID = DFHPF1              OR  DFHENTER             07380007
00651              GO TO 4000-BROWSE-FRWD                               07390007
00652          ELSE                                                     07400007
00653              GO TO 4100-BROWSE-BKWD.                              07410007
00654                                                                   07420007
00655      MOVE SPACES                 TO  COMP-RECORD-TYPE.            07430007
00656      MOVE PI-COMPANY-CD          TO  COMP-COMP-CD.                07440007
00657                                                                   07450007
00658      EXEC CICS HANDLE CONDITION                                   07460007
00659          NOTFND   (0410-NO-COMP-MSTR)                             07470007
00660          NOTOPEN  (7100-COMP-FILE-NOTOPEN)                        07480007
00661          END-EXEC.                                                07490007
00662                                                                   07500007
00663      EXEC CICS READ                                               07510007
00664          DATASET  (COMP-FILE-ID)                                  07520007
00665          SET      (ADDRESS OF COMPENSATION-MASTER)                07530007
00666          RIDFLD   (ERCOMP-KEY)                                    07540007
00667          GTEQ                                                     07550007
00668          END-EXEC.                                                07560007
00669                                                                   07570007
00670      IF PI-COMPANY-CD = CO-COMPANY-CD  AND                        07580007
00671         COMP-CARRIER  = CO-CARRIER     AND                        07590007
00672         COMP-GROUPING = CO-GROUPING    AND                        07600007
00673         COMP-FIN-RESP = CO-RESP-NO    AND                         07610007
00674         COMP-ACCOUNT  = CO-ACCOUNT                                07620007
00675          NEXT SENTENCE                                            07630007
00676      ELSE                                                         07640007
00677          GO TO 0410-NO-COMP-MSTR.                                 07650007
00678                                                                   07660007
00679      IF PI-COMPANY-ID = 'NCL'                                     07670007
00680          IF CO-GA-INACTIVE                                        07680007
00681              GO TO 0420-INACTIVE-COMP.                            07690007
00682                                                                   07700007
00683      IF MAINTI = 'C'                                              07710007
00684          GO TO 1000-EDIT-DATA.                                    07720007
00685                                                                   07730007
00686      IF EIBAID = DFHPF1                                           07740007
00687        OR DFHENTER                                                07750007
00688          GO TO 4000-BROWSE-FRWD.                                  07760007
00689                                                                   07770007
00690      IF EIBAID = DFHPF2                                           07780007
00691          GO TO 4100-BROWSE-BKWD.                                  07790007
00692                                                                   07800007
00693  0410-NO-COMP-MSTR.                                               07810007
00694      MOVE ER-2230                TO  EMI-ERROR.                   07820007
00695      MOVE -1                     TO  CARRIERL.                    07830007
00696      MOVE AL-UABON               TO  CARRIERA                     07840007
00697                                      GROUPA                       07850007
00698                                      FINRESPA                     07860007
00699                                      ACCTA.                       07870007
00700                                                                   07880007
00701      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  07890007
00702                                                                   07900007
00703      GO TO 8200-SEND-DATAONLY.                                    07910007
00704                                                                   07920007
00705  0420-INACTIVE-COMP.                                              07930007
00706      MOVE ER-2763                TO  EMI-ERROR.                   07940007
00707      MOVE -1                     TO  CARRIERL.                    07950007
00708      MOVE AL-UABON               TO  CARRIERA                     07960007
00709                                      GROUPA                       07970007
00710                                      FINRESPA                     07980007
00711                                      ACCTA.                       07990007
00712                                                                   08000007
00713      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  08010007
00714                                                                   08020007
00715      GO TO 8200-SEND-DATAONLY.                                    08030007
00716                                                                   08040007
00717      EJECT                                                        08050007
00718  0500-CREATE-TEMP-STORAGE.                                        08060007
00719                                                                   08070007
00720      PERFORM 0800-DELETE-TS  THRU  0890-EXIT.                     08080007
00721                                                                   08090007
00722      EXEC CICS WRITEQ TS                                          08100007
00723          QUEUE  (QID)                                             08110007
00724          FROM   (PROGRAM-INTERFACE-BLOCK)                         08120007
00725          LENGTH (PI-COMM-LENGTH)                                  08130007
00726      END-EXEC.                                                    08140007
00727                                                                   08150007
00728  0590-EXIT.                                                       08160007
00729       EXIT.                                                       08170007
00730                                                                   08180007
00731  0600-RECOVER-TEMP-STORAGE.                                       08190007
00732      EXEC CICS READQ TS                                           08200007
00733          QUEUE  (QID)                                             08210007
00734          INTO   (PROGRAM-INTERFACE-BLOCK)                         08220007
00735          LENGTH (PI-COMM-LENGTH)                                  08230007
00736      END-EXEC.                                                    08240007
00737                                                                   08250007
00738      PERFORM 0800-DELETE-TS THRU 0890-EXIT.                       08260007
00739                                                                   08270007
00740  0690-EXIT.                                                       08280007
00741       EXIT.                                                       08290007
00742                                                                   08300007
00743  0800-DELETE-TS.                                                  08310007
00744      EXEC CICS HANDLE CONDITION                                   08320007
00745          QIDERR (0890-EXIT)                                       08330007
00746      END-EXEC.                                                    08340007
00747                                                                   08350007
00748      EXEC CICS DELETEQ TS                                         08360007
00749          QUEUE  (QID)                                             08370007
00750      END-EXEC.                                                    08380007
00751                                                                   08390007
00752      EXEC CICS SYNCPOINT                                          08400007
00753      END-EXEC.                                                    08410007
00754                                                                   08420007
00755  0890-EXIT.                                                       08430007
00756       EXIT.                                                       08440007
00757      EJECT                                                        08450007
00758                                                                   08460007
00759  1000-EDIT-DATA.                                                  08470007
00760      SET PINDX                   TO  1.                           08480007
00761                                                                   08490007
           .
00762  1010-EDIT-LOOP.                                                  08500007
00763      SET NDX                     TO  PINDX.                       08510007
CIDMOD     IF GL-ACCT-LEN (PINDX)   = ZEROS  AND                        08520023
CIDMOD        GL-STATE-LEN (PINDX) = ZEROS   AND                        08530023
CIDMOD        GL-CANC-LEN (PINDX)   = ZEROS  AND                        08540023
CIDMOD        GL-COMM-LEN (PINDX)   = ZEROS  AND                        08550023
00765         RTYPE-LEN (PINDX)     = ZEROS  AND                        08560007
00766         AMT-LEN (PINDX)       = ZEROS  AND                        08570007
CIDMOD        VOID-SW-LEN (PINDX)   = ZEROS  AND                        08580007
00768         SDTE-LEN (PINDX)      = ZEROS                             08590007
00769          GO TO 1020-INCREMENT-PINDX.                              08600007
00770                                                                   08610007
CIDMOD     IF GL-ACCT-LEN (PINDX) NOT = ZEROS                           08620023
CIDMOD         MOVE AL-UANON           TO  GL-ACCT-ATTRB (PINDX)        08630023
00773          IF PI-COMPANY-ID NOT = 'WSL'                             08640007
00774              NEXT SENTENCE                                        08650007
00775          ELSE                                                     08660007
00776              MOVE WSL-COMM-DTE (PINDX)  TO  DC-GREG-DATE-1-MDY    08670007
00777              MOVE '4'                   TO  DC-OPTION-CODE        08680007
00778              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT           08690007
00779              IF NO-CONVERSION-ERROR                               08700007
00780                  NEXT SENTENCE                                    08710007
00781              ELSE                                                 08720007
00782                  MOVE ER-2595    TO  EMI-ERROR                    08730007
CIDMOD                 MOVE -1         TO  GL-ACCT-LEN (PINDX)          08740023
CIDMOD                 MOVE AL-UABON   TO  GL-ACCT-ATTRB (PINDX)        08750023
00785                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       08760007
00786      ELSE                                                         08770007
00787          IF PI-COMPANY-ID = 'WSL'                                 08780007
00788            AND PI-FILE-SEQ-NO (NDX) = ZEROS                       08790007
00789              MOVE ER-2596        TO  EMI-ERROR                    08800007
CIDMOD             MOVE -1             TO  GL-ACCT-LEN (PINDX)          08810023
CIDMOD             MOVE AL-UABON       TO  GL-ACCT-ATTRB (PINDX)        08820023
00792              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          08830007
00793                                                                   08840007
CIDMOD                                                                  08850000
CIDMOD     IF GL-ACCT-LEN (PINDX) NOT = ZEROES                          08860023
CIDMOD*        MOVE K-SPACE            TO COFA-FILLER                   08870022
CIDMOD*        MOVE '0001'             TO COFA-COMPANY-X                08880022
CIDMOD*        MOVE MSA-ACCT (PINDX)   TO COFA-MSA-ACCT                 08890022
CIDMOD         MOVE AL-UANON           TO GL-ACCT-ATTRB (PINDX)         08900023
CIDMOD     ELSE                                                         08910000
CIDMOD         IF PI-FILE-SEQ-NO (NDX) = ZEROES                         08920019
CIDMOD             MOVE -1             TO GL-ACCT-LEN (PINDX)           08930023
CIDMOD             MOVE ER-2957        TO EMI-ERROR                     08950007
CIDMOD             MOVE AL-UABON       TO GL-ACCT-ATTRB (PINDX)         08960023
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU                       08970000
CIDMOD                     9900-EXIT                                    08980000
CIDMOD             GO TO 1040-CONTINUE-EDIT                             08990000
PEMMOD         ELSE                                                     08991046
PEMMOD             GO TO 1040-CONTINUE-EDIT                             08992046
CIDMOD         END-IF                                                   09000000
CIDMOD     END-IF.                                                      09010000
CIDMOD                                                                  09020000
021716     IF (PI-COMPANY-ID = 'DCC' or 'VPP')
042303        AND (RTYPE (PINDX) = 'P')
042303        GO TO 1040-CONTINUE-EDIT
042303     END-IF

120711     IF GL-ACCT-LEN (PINDX) NOT = ZEROS
120711        MOVE GL-ACCT (PINDX)     TO CHECK-GL-ACCT
120711     ELSE
120711        MOVE SPACES              TO CHECK-GL-ACCT
120711     END-IF
120711
120711     EVALUATE TRUE
021716        WHEN (PI-COMPANY-ID = 'DCC')
110315           AND (CO-CARRIER = '1' OR '2' or '9')
120711           AND (VALID-DCC-GL-ACCOUNT)
120711           GO TO 1040-CONTINUE-EDIT
021716        WHEN (PI-COMPANY-ID = 'DCC')
120711           AND (CO-CARRIER = '3' OR '4')
120711           AND VALID-CSI-GL-ACCOUNT
120711           GO TO 1040-CONTINUE-EDIT
021716        WHEN (PI-COMPANY-ID = 'DCC')
081414           AND (CO-CARRIER = '5' OR '6' or '7')
120711           AND (VALID-CCC-GL-ACCOUNT)
120711           GO TO 1040-CONTINUE-EDIT
031912        WHEN (PI-COMPANY-ID = 'CID' OR 'AHL')
120711           AND (VALID-GL-ACCOUNT)
120711           GO TO 1040-CONTINUE-EDIT
031912        WHEN (PI-COMPANY-ID = 'VPP')
120711           AND (VALID-VPP-GL-ACCOUNT)
120711           GO TO 1040-CONTINUE-EDIT
062121        WHEN (PI-COMPANY-ID = 'FNL')
062121           AND (VALID-FNL-GL-ACCOUNT)
062121           GO TO 1040-CONTINUE-EDIT
120711     END-EVALUATE

CIDMOD*    EXEC CICS HANDLE CONDITION                                   09030022
CIDMOD*         NOTFND (1040-NO-COFA-MSTR)                              09040022
CIDMOD*         NOTOPEN (7100-COFA-FILE-NOTOPEN)                        09050022
CIDMOD*    END-EXEC.                                                    09060022
CIDMOD*                                                                 09070022
CIDMOD*    EXEC CICS READ                                               09080022
CIDMOD*         DATASET (COFA-FILE-ID)                                  09090022
CIDMOD*         SET     (ADDRESS OF CHART-OF-ACCOUNTS)                  09100022
CIDMOD*         RIDFLD  (COFA-KEY-X)                                    09110022
CIDMOD*    END-EXEC.                                                    09120022
CIDMOD*                                                                 09130022
CIDMOD*    MOVE CHART-OF-ACCOUNTS      TO SV-COFA.                      09140022
CIDMOD*    GO TO 1040-CONTINUE-EDIT.                                    09150022
CIDMOD*                                                                 09160022
CIDMOD*1040-NO-COFA-MSTR.                                               09170024
CIDMOD                                                                  09180024
CIDMOD     MOVE ER-2960                TO EMI-ERROR.                    09190024
CIDMOD     MOVE -1                     TO GL-ACCT-LEN (PINDX).          09200025
CIDMOD     MOVE AL-UABON               TO GL-ACCT-ATTRB (PINDX).        09210025
CIDMOD     PERFORM 9900-ERROR-FORMAT THRU                               09220024
CIDMOD             9900-EXIT.                                           09230024
CIDMOD                                                                  09240024
CIDMOD 1040-CONTINUE-EDIT.                                              09250000
CIDMOD                                                                  09260000
CIDMOD*    MOVE MSA-ACCT (PINDX)       TO WS-ACCT-BREAK.                09270022
CIDMOD     IF GL-STATE-LEN (PINDX) NOT = ZEROS                          09280023
CIDMOD         MOVE GL-STATE (PINDX)   TO CHECK-STATE-CODE              09290023
CIDMOD         IF NOT VALID-STATE-CODE                                  09300000
CIDMOD             MOVE -1             TO GL-STATE-LEN (PINDX)          09310023
CIDMOD             MOVE ER-2957        TO EMI-ERROR                     09320007
CIDMOD             MOVE AL-UABON       TO GL-STATE-ATTRB (PINDX)        09330023
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU                       09340000
CIDMOD                     9900-EXIT                                    09350000
CIDMOD         ELSE                                                     09360000
CIDMOD             MOVE AL-UANON       TO GL-STATE-ATTRB (PINDX)        09370023
CIDMOD         END-IF                                                   09380000
CIDMOD     END-IF.                                                      09390000
CIDMOD                                                                  09400000
CIDMOD 1040-CSO-SKIP.                                                   09410000
CIDMOD                                                                  09420000
CIDMOD     IF GL-CANC (PINDX) = LOW-VALUES                              09430023
CIDMOD         MOVE SPACES             TO GL-CANC (PINDX)               09440023
CIDMOD     END-IF.                                                      09450000
CIDMOD                                                                  09460000
CIDMOD     IF GL-CANC-LEN (PINDX) NOT = ZEROS                           09470023
CIDMOD         MOVE GL-CANC (PINDX)    TO CHECK-CANC-TYPE               09480023
CIDMOD         IF NOT VALID-CANC-TYPE                                   09490000
CIDMOD             MOVE -1             TO GL-CANC-LEN (PINDX)           09500023
CIDMOD             MOVE ER-2958        TO EMI-ERROR                     09510007
CIDMOD             MOVE AL-UABON       TO GL-CANC-ATTRB (PINDX)         09520023
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU                       09530000
CIDMOD                     9900-EXIT                                    09540000
CIDMOD         ELSE                                                     09550000
CIDMOD             MOVE AL-UANON       TO GL-CANC-ATTRB (PINDX)         09560023
CIDMOD         END-IF                                                   09570000
CIDMOD     END-IF.                                                      09580000
CIDMOD                                                                  09590007
00794      IF RTYPE-LEN (PINDX) NOT = ZEROS
00795         MOVE RTYPE (PINDX)       TO  CHECK-REC-TYPE
021716        IF (PI-COMPANY-ID = 'DCC' or 'VPP')
042303           AND (RTYPE (PINDX) = 'P')
042303           SET VALID-REC-TYPE   TO TRUE
042303        END-IF
00796          IF NOT VALID-REC-TYPE                                    09620007
00797              MOVE -1             TO  RTYPE-LEN (PINDX)            09630007
00798              MOVE ER-2234        TO  EMI-ERROR                    09640007
00799              MOVE AL-UABON       TO  RTYPE-ATTRB (PINDX)          09650007
00800              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           09660007
00801          ELSE                                                     09670007
00802              MOVE AL-UANON       TO  RTYPE-ATTRB (PINDX)          09680007
00803      ELSE                                                         09690007
00804          IF PI-FILE-SEQ-NO (NDX) = ZEROS                          09700007
00805              MOVE -1             TO  RTYPE-LEN (PINDX)            09710007
00806              MOVE ER-2235        TO  EMI-ERROR                    09720007
00807              MOVE AL-UABON       TO  RTYPE-ATTRB (PINDX)          09730007
00808              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          09740007
00809                                                                   09750007
00810      IF AMT-LEN (PINDX) NOT = ZEROS                               09760007
00811         EXEC CICS BIF DEEDIT                                      09770007
00812             FIELD (AMT (PINDX))                                   09780018
00813             LENGTH (11)                                           09790018
00814         END-EXEC                                                  09800007
00815          IF AMT (PINDX) = ZEROS                                   09810007
00816              IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS                  09820007
00817                  NEXT SENTENCE                                    09830007
00818              ELSE                                                 09840007
00819                  MOVE ER-2245    TO  EMI-ERROR                    09850007
00820                  MOVE -1         TO  AMT-LEN(PINDX)               09860007
00821                  MOVE AL-UNBON   TO  AMT-ATTRB (PINDX)            09870007
00822                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       09880007
00823          ELSE                                                     09890007
00824              IF AMT (PINDX) NUMERIC                               09900007
00825                  MOVE AMT (PINDX) TO  AMTO (PINDX)                09910007
00826              ELSE                                                 09920007
00827                  MOVE ER-2245    TO  EMI-ERROR                    09930007
00828                  MOVE -1         TO  AMT-LEN(PINDX)               09940007
00829                  MOVE AL-UNBON   TO  AMT-ATTRB (PINDX)            09950007
00830                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       09960007
00831      ELSE                                                         09970007
00832          IF PI-FILE-SEQ-NO (NDX) = ZEROS                          09980007
00833              MOVE -1             TO  AMT-LEN (PINDX)              09990007
00834              MOVE ER-2236        TO  EMI-ERROR                    10000007
00835              MOVE AL-UNBON       TO  AMT-ATTRB (PINDX)            10010007
00836              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          10020007
00837                                                                   10030007
CIDMOD     IF VOID-SW-LEN (PINDX) NOT = ZEROS                           10040007
CIDMOD         IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS                      10050007
CIDMOD           AND (PI-REC-TYPE (NDX) = 'C')                          10060007
CIDMOD             IF VOID-SW (PINDX) = 'V'                             10070007
CIDMOD                 MOVE AL-UANON   TO  VOID-SW-ATTRB (PINDX)        10080007
CIDMOD             ELSE                                                 10090007
CIDMOD                 MOVE ER-2246    TO  EMI-ERROR                    10100007
CIDMOD                 MOVE -1         TO  VOID-SW-LEN (PINDX)          10110007
CIDMOD                 MOVE AL-UABON   TO  VOID-SW-ATTRB (PINDX)        10120007
CIDMOD                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       10130007
CIDMOD         ELSE                                                     10140007
CIDMOD             MOVE ER-2449        TO  EMI-ERROR                    10150007
CIDMOD             MOVE -1             TO  VOID-SW-LEN (PINDX)          10160007
CIDMOD             MOVE AL-UABON       TO  VOID-SW-ATTRB (PINDX)        10170007
CIDMOD             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          10180007
CIDMOD                                                                  10190007
00854      IF SDTE-LEN (PINDX) = ZEROS                                  10200007
00855         GO TO 1020-INCREMENT-PINDX.                               10210007
00856                                                                   10220007
00857      MOVE AL-UNNON               TO  SDTE-ATTRB (PINDX).          10230007
00858                                                                   10240007
00859      MOVE SDTE (PINDX)           TO DEEDIT-FIELD.                 10250007
00860      PERFORM 8600-DEEDIT.                                         10260007
00861                                                                   10270007
00862      IF DEEDIT-FIELD-V0 NOT NUMERIC                               10280007
00863         GO TO 1015-DAY-ERROR.                                     10290007
00864                                                                   10300007
00865      MOVE DEEDIT-FIELD-V0        TO  DC-GREG-DATE-1-MDY.          10310007
00866      MOVE '4'                    TO  DC-OPTION-CODE.              10320007
00867      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                  10330007
00868                                                                   10340007
00869      IF NO-CONVERSION-ERROR                                       10350007
00870         SET PINDEX               TO  PINDX                        10360007
00871         MOVE DC-BIN-DATE-1       TO  WS-EOM-DT (PINDEX)           10370007
00872      ELSE                                                         10380007
00873         GO TO 1015-DAY-ERROR.                                     10390007
00874                                                                   10400007
00875      MOVE DEEDIT-FIELD-V0        TO DATE-TEST-AREA.               10410007
00876                                                                   10420007
00877      IF DATE-TEST-MM = 4 OR  6  OR  9  OR  11                     10430007
00878          IF DATE-TEST-DD  NOT = 30                                10440007
00879              GO TO 1015-DAY-ERROR                                 10450007
00880          ELSE                                                     10460007
00881              GO TO 1020-INCREMENT-PINDX.                          10470007
00882                                                                   10480007
00883      IF DATE-TEST-MM = 1 OR  3  OR  5  OR  7  OR                  10490007
00884                              8  OR  10  OR  12                    10500007
00885          IF DATE-TEST-DD  NOT = 31                                10510007
00886              GO TO 1015-DAY-ERROR                                 10520007
00887          ELSE                                                     10530007
00888              GO TO 1020-INCREMENT-PINDX.                          10540007
00889                                                                   10550007
00890      DIVIDE DATE-TEST-YY  BY  4  GIVING  DIVIDE-RESULT            10560007
00891          REMAINDER  DIVIDE-REMAINDER.                             10570007
00892                                                                   10580007
00893      IF (DATE-TEST-YY = ZERO)                                     10590007
00894        OR (DIVIDE-REMAINDER NOT = ZERO)                           10600007
00895          IF DATE-TEST-DD  NOT = 28                                10610007
00896              GO TO 1015-DAY-ERROR                                 10620007
00897          ELSE                                                     10630007
00898              GO TO 1020-INCREMENT-PINDX                           10640007
00899      ELSE                                                         10650007
00900          IF DATE-TEST-DD = 29                                     10660007
00901              GO TO 1020-INCREMENT-PINDX.                          10670007
00902                                                                   10680007
00903  1015-DAY-ERROR.                                                  10690007
00904      MOVE -1                     TO  SDTE-LEN (PINDX).            10700007
00905      MOVE AL-UNBON               TO  SDTE-ATTRB (PINDX).          10710007
00906      MOVE ER-0587                TO  EMI-ERROR.                   10720007
00907                                                                   10730007
00908      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  10740007
00909                                                                   10750007
00910  1020-INCREMENT-PINDX.                                            10760007
00911      SET PINDX  UP  BY  1.                                        10770007
00912                                                                   10780007
00913      IF PINDX  IS NOT GREATER THAN  13                            10790007
00914          GO TO 1010-EDIT-LOOP.                                    10800007
00915                                                                   10810007
00916      IF EMI-ERROR = ZEROS                                         10820007
00917          NEXT SENTENCE                                            10830007
00918      ELSE                                                         10840007
00919          MOVE 'S'                TO  PI-PREV-FUNCTION             10850007
00920          GO TO 8200-SEND-DATAONLY.                                10860007
00921  EJECT                                                            10870007
00922  2000-UPDATE-THE-FILE.                                            10880007
00923      SET PINDX                   TO  ZERO-NDX.                    10890007
CIDMOD*    SET NDX                     TO  ZERO-NDX.                    10900008
CIDMOD*    SET NDX  UP  BY  1.                                          10910008
00926                                                                   10920007
00927      IF PI-PYAJ-FILE-SW = 'B'                                     10930007
00928          MOVE WORK-SEQ-NO        TO  PI-FRST-FILE-SEQ-NO          10940007
00929          MOVE RTYPE (PINDX)      TO  PI-FRST-RECORD-TYPE          10950007
00930      ELSE                                                         10960007
00931          MOVE PI-LAST-FILE-SEQ-NO                                 10970007
00932                                  TO  PI-SAV-FILE-SEQ-NO           10980007
00933          MOVE PI-LAST-RECORD-TYPE                                 10990007
00934                                  TO  PI-SAV-RECORD-TYPE.          11000007
00935                                                                   11010007
00936  2100-UPDATE-LOOP.                                                11020007
00937      SET PINDX  UP  BY  1.                                        11030007
00938      SET NDX                     TO  PINDX.                       11040007
00939                                                                   11050007
00940      IF PINDX  IS GREATER THAN  13                                11060007
00941          GO TO 2200-UPDATE-COMPLETE.                              11070007
00942                                                                   11080007
CIDMOD     IF GL-ACCT-LEN (PINDX)   = ZEROS AND                         11090023
CIDMOD        GL-STATE-LEN (PINDX) = ZEROS AND                          11100023
CIDMOD        GL-CANC-LEN (PINDX)   = ZEROS AND                         11110023
CIDMOD        GL-COMM-LEN (PINDX)   = ZEROS AND                         11120023
CIDMOD        RTYPE-LEN (PINDX)   = ZEROS  AND                          11130007
CIDMOD        AMT-LEN (PINDX)     = ZEROS  AND                          11140007
CIDMOD        VOID-SW-LEN (PINDX) = ZEROS  AND                          11150007
00947         SDTE-LEN (PINDX)    = ZEROS                               11160007
00948          GO TO 2100-UPDATE-LOOP.                                  11170035
00949                                                                   11180007
00950      EXEC CICS BIF DEEDIT                                         11190007
00951           FIELD (AMT (PINDX))                                     11200007
00952           LENGTH (11)                                             11210007
00953      END-EXEC.                                                    11220007
00954                                                                   11230007
00955      IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS                          11240007
00956          NEXT SENTENCE                                            11250007
00957      ELSE                                                         11260007
00958          GO TO 2110-ADD-RECORD.                                   11270007
00959                                                                   11280007
00960      EXEC CICS HANDLE CONDITION                                   11290007
00961          NOTFND  (2110-ADD-RECORD)                                11300007
00962      END-EXEC.                                                    11310007
00963                                                                   11320007
00964      MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.                 11330007
00965      MOVE PI-FILE-SEQ-NO (NDX)    TO  PYAJ-FILE-SEQ-NO.           11340007
00966      MOVE PI-REC-TYPE (NDX)       TO  PYAJ-RECORD-TYPE.           11350007
00967                                                                   11360007
00968      EXEC CICS READ                                               11370007
00969          SET      (ADDRESS OF PENDING-PAY-ADJ)                    11380007
00970          DATASET  (PYAJ-FILE-ID)                                  11390007
00971          RIDFLD   (ERPYAJ-KEY)                                    11400007
00972          UPDATE                                                   11410007
00973      END-EXEC.                                                    11420007
00974                                                                   11430007
                                                                        11431041
PEMMOD                                                                  11437046
00975      IF RTYPE-LEN (PINDX)  GREATER THAN +0                        11440007
00976         IF RTYPE (PINDX) NOT = PY-RECORD-TYPE                     11450007
PEMMOD            MOVE PENDING-PAY-ADJ TO WS-SAVE-ERPYAJ                11451046
00977             PERFORM 2190-CHANGE-RECORD-TYPE THRU 2190-EXIT        11460007
00978             MOVE RTYPE (PINDX)     TO  PI-FRST-RECORD-TYPE.       11470007
00979                                                                   11480007
00980      IF AMT-LEN (PINDX) NOT = ZEROS                               11490007
00981          IF AMT (PINDX) = ZEROS                                   11500007
CIDMOD             IF PY-CHECK-WRITTEN-DT = LOW-VALUES                  11510007
CIDMOD                 GO TO 2120-DELETE-RECORD                         11520007
CIDMOD             ELSE                                                 11530007
CIDMOD                 MOVE ER-2244       TO  EMI-ERROR                 11540007
CIDMOD                 MOVE -1            TO  AMT-LEN (PINDX)           11550007
CIDMOD                 MOVE AL-UNBON      TO  AMT-ATTRB (PINDX)         11560007
CIDMOD                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       11570007
CIDMOD                 MOVE PY-ENTRY-AMT  TO  AMTO (PINDX)              11580007
CIDMOD                 EXEC CICS UNLOCK                                 11590007
CIDMOD                     DATASET  (PYAJ-FILE-ID)                      11600007
CIDMOD                 END-EXEC                                         11610007
CIDMOD                 GO TO 8200-SEND-DATAONLY.                        11620007
00994                                                                   11630007
00995      MOVE 'B'                    TO  JP-RECORD-TYPE.              11640007
00996      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.                  11650007
00997      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.              11660007
00998                                                                   11670007
00999      PERFORM 8400-LOG-JOURNAL-RECORD.                             11680007
01000                                                                   11690007
01001      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.            11700007
01002      MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.        11710007
01003      MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT.            11720007
01004                                                                   11730007
CIDMOD     IF GL-ACCT-LEN (PINDX) NOT = ZEROS                           11740023
CIDMOD         MOVE GL-ACCT (PINDX)    TO  PY-GL-ACCOUNT.               11750023
CIDMOD     IF GL-STATE-LEN (PINDX) NOT = ZEROS                          11760023
CIDMOD         MOVE GL-STATE (PINDX)   TO  PY-GL-STATE.                 11770023
CIDMOD     IF GL-CANC-LEN (PINDX) NOT = ZEROS                           11780023
CIDMOD         MOVE GL-CANC (PINDX)    TO  PY-GL-CANC-SW.               11790023
CIDMOD     IF GL-COMM-LEN (PINDX) NOT = ZEROS                           11800023
CIDMOD         MOVE GL-COMM (PINDX)    TO  PY-GL-COMMENT.               11810023
01008      IF AMT-LEN (PINDX) NOT = ZEROS                               11820007
01009          MOVE AMT (PINDX)        TO  PY-ENTRY-AMT.                11830007
01010                                                                   11840007
CIDMOD     IF VOID-SW-LEN (PINDX) NOT = ZEROS                           11850007
CIDMOD         MOVE VOID-SW (PINDX)    TO  PY-VOID-SW.                  11860007
01013                                                                   11870007
01014      IF SDTE-LEN (PINDX) NOT = ZEROS                              11880007
01015         SET PINDEX               TO  PINDX                        11890007
01016         MOVE WS-EOM-DT (PINDEX)  TO  PY-CREDIT-SELECT-DT.         11900007
01017                                                                   11910007
01018      MOVE 'C'                    TO  JP-RECORD-TYPE.              11920007
01019      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.                  11930007
01020      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.              11940007
01021                                                                   11950007
01022      EXEC CICS REWRITE                                            11960007
01023          DATASET  (PYAJ-FILE-ID)                                  11970007
01024          FROM     (PENDING-PAY-ADJ)                               11980007
01025      END-EXEC.                                                    11990007
01026                                                                   12000007
01027      PERFORM 8400-LOG-JOURNAL-RECORD.                             12010007
01028                                                                   12020007
01029      GO TO 2100-UPDATE-LOOP.                                      12030007
01030  EJECT                                                            12040007
01031  2110-ADD-RECORD.                                                 12050007
01032      EXEC CICS GETMAIN                                            12060007
01033          SET      (ADDRESS OF PENDING-PAY-ADJ)                    12070007
01034          LENGTH   (ERPYAJ-RECORD-LENGTH)                          12080007
01035          INITIMG  (GETMAIN-SPACE)                                 12090007
01036      END-EXEC.                                                    12100007
01037                                                                   12110007
01038      MOVE 'PY'                   TO  PY-RECORD-ID.                12120007
01039      MOVE PI-COMPANY-CD          TO  PY-COMPANY-CD.               12130007
01040      MOVE PI-SAV-CARRIER         TO  PY-CARRIER.                  12140007
01041      MOVE PI-SAV-GROUPING        TO  PY-GROUPING.                 12150007
01042      MOVE PI-SAV-FIN-RESP        TO  PY-FIN-RESP.                 12160007
01043      MOVE PI-SAV-ACCOUNT         TO  PY-ACCOUNT.                  12170007
01044                                                                   12180007
01045      ADD +1                      TO  WORK-SEQ-NO.                 12190007
01046      MOVE WORK-SEQ-NO            TO  PY-FILE-SEQ-NO.              12200007
01047                                                                   12210007
CIDMOD     MOVE GL-ACCT (PINDX)        TO  PY-GL-ACCOUNT.               12220023
021716     if gl-state-len (pindx) <> zeros
021716        MOVE GL-STATE (PINDX)    TO  PY-GL-STATE
021716     end-if
021716     if gl-canc-len (pindx) <> zeros
021716        MOVE GL-CANC (PINDX)     TO  PY-GL-CANC-SW
021716     end-if
CIDMOD     MOVE GL-COMM (PINDX)        TO  PY-GL-COMMENT.               12250023
CIDMOD                                                                  12260000
01049      MOVE RTYPE (PINDX)          TO  PY-RECORD-TYPE.              12270007
01050      MOVE AMT (PINDX)            TO  PY-ENTRY-AMT.                12280007

060205     IF CO-TYPE = 'B' OR 'A' OR 'G'
060205        MOVE CO-TYPE             TO  PY-ERCOMP-TYPE
060205     END-IF

01051      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.            12290007
01052      MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.        12300007
01053      MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT             12310007
01054                                      PY-INPUT-DT.                 12320007
01055      MOVE ZEROS                  TO  PY-CHECK-QUE-CONTROL         12330007
01056                                      PY-CHECK-QUE-SEQUENCE.       12340007
01057      MOVE LOW-VALUES             TO  PY-CREDIT-ACCEPT-DT          12350007
01058                                      PY-BILLED-DATE               12360007
01059                                      PY-AR-DATE                   12370007
01060                                      PY-REPORTED-DT               12380007
01061                                      PY-CHECK-WRITTEN-DT.         12390007
01062      IF SDTE-LEN (PINDX) = ZEROS                                  12400007
01063          MOVE PI-CR-MONTH-END-DT TO  PY-CREDIT-SELECT-DT          12410007
CIDMOD         GO TO 2115-WRITE-REC                                     12420000
CIDMOD     END-IF.                                                      12430007
CIDMOD                                                                  12440000
01065      IF SDTE (PINDX) IS NUMERIC                                   12450007
01066          MOVE SDTE (PINDX)   TO  DC-GREG-DATE-1-MDY               12460007
01067          MOVE '4'            TO  DC-OPTION-CODE                   12470007
01068          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT               12480007
01069          IF DATE-CONVERSION-ERROR                                 12490007
01070              MOVE PI-CR-MONTH-END-DT                              12500007
01071                              TO  PY-CREDIT-SELECT-DT              12510007
01072          ELSE                                                     12520007
01073              MOVE DC-BIN-DATE-1                                   12530007
01074                              TO  PY-CREDIT-SELECT-DT              12540007
CIDMOD         END-IF                                                   12550007
01075      ELSE                                                         12560007
01076          MOVE PI-CR-MONTH-END-DT                                  12570007
01077                              TO  PY-CREDIT-SELECT-DT              12580007
CIDMOD     END-IF                                                       12590007
01078                                                                   12600007
01079      IF PI-SAV-FILE-SEQ-NO NOT GREATER THAN ZEROS                 12610007
01080          MOVE RTYPE (PINDX)      TO  PI-SAV-RECORD-TYPE           12620007
01081          MOVE WORK-SEQ-NO        TO  PI-SAV-FILE-SEQ-NO.          12630007
01082                                                                   12640007
01083      IF FIRST-ADD AND END-OF-ACCT-FULL-PAGE                       12650007
01084          MOVE RTYPE (PINDX)      TO  PI-SAV-RECORD-TYPE           12660007
01085          MOVE WORK-SEQ-NO        TO  PI-SAV-FILE-SEQ-NO.          12670007
01086                                                                   12680007
01087      MOVE 'N'                    TO  FIRST-ADD-SW.                12690007
01088                                                                   12700007
01089      MOVE 'A'                    TO  JP-RECORD-TYPE.              12710007
01090      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.                  12720007
01091      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.              12730007
01092  2115-WRITE-REC.                                                  12740007
01093      EXEC CICS WRITE                                              12750007
01094          DATASET  (PYAJ-FILE-ID)                                  12760007
01095          FROM     (PENDING-PAY-ADJ)                               12770007
01096          RIDFLD   (PY-CONTROL-PRIMARY)                            12780007
01097      END-EXEC.                                                    12790007
01098                                                                   12800007
01099      PERFORM 8400-LOG-JOURNAL-RECORD.                             12810007
01100                                                                   12820007
01101      GO TO 2100-UPDATE-LOOP.                                      12830007
01102                                                                   12840007
01103  EJECT                                                            12850007
01104  2120-DELETE-RECORD.                                              12860007
CIDMOD     IF PY-RECORD-TYPE NOT = 'C'                                  12870007
CIDMOD         GO TO 2120-DELETE-CONT.                                  12880007
CIDMOD                                                                  12890007
CIDMOD     IF PY-CHECK-QUE-CONTROL = ZEROS                              12900007
CIDMOD       AND PY-CHECK-QUE-SEQUENCE = ZEROS                          12910007
CIDMOD         GO TO 2120-DELETE-CONT.                                  12920007
CIDMOD                                                                  12930007
CIDMOD     EXEC CICS HANDLE CONDITION                                   12940020
CIDMOD         NOTFND  (2120-DELETE-CONT)                               12950020
CIDMOD     END-EXEC.                                                    12960020
CIDMOD                                                                  12970007
CIDMOD     MOVE PY-COMPANY-CD          TO  CHKQ-COMPANY-CD.             12980020
CIDMOD     MOVE PY-CHECK-QUE-CONTROL   TO  CHKQ-CONTROL-NUMBER.         12990020
CIDMOD     MOVE PY-CHECK-QUE-SEQUENCE  TO  CHKQ-SEQUENCE-NUMBER.        13000020
CIDMOD                                                                  13010020
CIDMOD     EXEC CICS READ                                               13020020
CIDMOD         SET      (ADDRESS OF CHECK-QUE)                          13030020
CIDMOD         DATASET  (CHKQ-FILE-ID)                                  13040020
CIDMOD         RIDFLD   (ERCHKQ-KEY)                                    13050020
CIDMOD         UPDATE                                                   13060020
CIDMOD     END-EXEC.                                                    13070020
CIDMOD                                                                  13080007
CIDMOD*    MOVE 'D'                    TO  JP-RECORD-TYPE.              13090020
CIDMOD*    MOVE CHKQ-FILE-ID           TO  JP-FILE-ID.                  13100020
CIDMOD*    MOVE CHECK-QUE              TO  JP-RECORD-AREA.              13110020
CIDMOD*                                                                 13120020
CIDMOD*    PERFORM 8400-LOG-JOURNAL-RECORD.                             13130020
CIDMOD                                                                  13140007
CIDMOD     EXEC CICS DELETE                                             13150020
CIDMOD         DATASET  (CHKQ-FILE-ID)                                  13160020
CIDMOD     END-EXEC.                                                    13170020
01136                                                                   13180007
01137  2120-DELETE-CONT.                                                13190007
CIDMOD*    MOVE 'D'                    TO  JP-RECORD-TYPE.              13200020
CIDMOD*    MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.                  13210020
CIDMOD*    MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.              13220020
CIDMOD*                                                                 13230020
CIDMOD*    PERFORM 8400-LOG-JOURNAL-RECORD.                             13240020
01143                                                                   13250007
01144      IF PI-SAV-ENDING-PYAJ-KEY = ERPYAJ-KEY                       13260007
01145          MOVE ZEROS              TO  PI-FRST-FILE-SEQ-NO          13270007
01146          MOVE SPACE              TO  PI-FRST-RECORD-TYPE          13280007
01147          MOVE ZEROS              TO  PI-SAV-ACCT-AMT              13290007
01148                                      PI-SAV-ACCT-NET              13300007
01149                                      PI-SAV-PREV-AMT              13310007
01150                                      PI-SAV-PREV-NET.             13320007
01151                                                                   13330007
01152      EXEC CICS DELETE                                             13340007
01153          DATASET(PYAJ-FILE-ID)                                    13350007
01154      END-EXEC.                                                    13360007
01155                                                                   13370007
01156      GO TO 2100-UPDATE-LOOP.                                      13380007
01157                                                                   13390007
01158  EJECT                                                            13400007
01159                                                                   13410007
01160  2190-CHANGE-RECORD-TYPE.                                         13420007
01161                                                                   13430007
CIDMOD*    MOVE 'D'                   TO  JP-RECORD-TYPE.               13440020
CIDMOD*    MOVE PYAJ-FILE-ID          TO  JP-FILE-ID.                   13450020
CIDMOD*    MOVE PENDING-PAY-ADJ       TO  JP-RECORD-AREA.               13460020
CIDMOD*                                                                 13470020
CIDMOD*    PERFORM 8400-LOG-JOURNAL-RECORD.                             13480020
01167                                                                   13490007
01168      EXEC CICS DELETE                                             13500007
01169          DATASET(PYAJ-FILE-ID)                                    13510007
01170      END-EXEC.                                                    13520007
01171                                                                   13530007
01172      EXEC CICS GETMAIN                                            13540007
01173          SET      (ADDRESS OF PENDING-PAY-ADJ)                    13550007
01174          LENGTH   (ERPYAJ-RECORD-LENGTH)                          13560007
01175          INITIMG  (GETMAIN-SPACE)                                 13570007
01176      END-EXEC.                                                    13580007
01177                                                                   13590007
CIDMOD*    MOVE JP-RECORD-AREA        TO  PENDING-PAY-ADJ.              13600020
CIDMOD*    MOVE ERPYAJ-KEY            TO  PY-CONTROL-PRIMARY.           13610020
01180                                                                   13620007
PEMMOD     MOVE WS-SAVE-ERPYAJ        TO  PENDING-PAY-ADJ               13621046
01181      MOVE RTYPE (PINDX)         TO  PY-RECORD-TYPE                13630007
01182                                     PYAJ-RECORD-TYPE.             13640007
01183                                                                   13650007
01184      EXEC CICS HANDLE CONDITION                                   13660007
01185          DUPREC (2190-DUP-RECORD)                                 13670007
01186      END-EXEC.                                                    13680007
01187                                                                   13690007
CIDMOD*    MOVE 'A'                   TO  JP-RECORD-TYPE.               13700020
CIDMOD*    MOVE PYAJ-FILE-ID          TO  JP-FILE-ID.                   13710020
CIDMOD*    MOVE PENDING-PAY-ADJ       TO  JP-RECORD-AREA.               13720020
01191                                                                   13730007
01192  2190-RETRY-WRITE.                                                13740007
01193                                                                   13750007
01194      EXEC CICS WRITE                                              13760007
01195          DATASET  (PYAJ-FILE-ID)                                  13770007
01196          FROM     (PENDING-PAY-ADJ)                               13780007
01197          RIDFLD   (PY-CONTROL-PRIMARY)                            13790007
01198      END-EXEC.                                                    13800007
01199                                                                   13810007
CIDMOD*    PERFORM 8400-LOG-JOURNAL-RECORD.                             13820020
01201                                                                   13830007
01202      EXEC CICS READ                                               13840007
01203          SET      (ADDRESS OF PENDING-PAY-ADJ)                    13850007
01204          DATASET  (PYAJ-FILE-ID)                                  13860007
01205          RIDFLD   (ERPYAJ-KEY)                                    13870007
01206          UPDATE                                                   13880007
01207      END-EXEC.                                                    13890007
01208                                                                   13900007
01209      GO TO 2190-EXIT.                                             13910007
01210                                                                   13920007
01211  2190-DUP-RECORD.                                                 13930007
01212                                                                   13940007
01213      COMPUTE PY-FILE-SEQ-NO = PY-FILE-SEQ-NO + 1.                 13950007
01214                                                                   13960007
01215      GO TO 2190-RETRY-WRITE.                                      13970007
01216                                                                   13980007
01217  2190-EXIT.                                                       13990007
01218      EXIT.                                                        14000007
01219  EJECT                                                            14010007
01220                                                                   14020007
01221  2200-UPDATE-COMPLETE.                                            14030007
01222      IF EIBAID = DFHPF1                                           14040007
01223          GO TO 4000-BROWSE-FRWD.                                  14050007
01224                                                                   14060007
01225      IF EIBAID = DFHPF2                                           14070007
01226          GO TO 4100-BROWSE-BKWD.                                  14080007
01227                                                                   14090007
01228      MOVE LOW-VALUES             TO  EL633AI.                     14100007
01229                                                                   14110007
01230      MOVE ER-0000                TO  EMI-ERROR.                   14120007
01231      MOVE -1                     TO  MAINTL.                      14130007
01232                                                                   14140007
01233      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  14150007
01234                                                                   14160007
01235      MOVE PI-FRST-FILE-SEQ-NO    TO  PI-SAV-FILE-SEQ-NO.          14170007
01236      MOVE PI-FRST-RECORD-TYPE    TO  PI-SAV-RECORD-TYPE.          14180007
01237                                                                   14190007
CIDMOD*    GO TO 4000-BROWSE-FRWD.                                      14200044
CIDMOD                                                                  14210000
CIDMOD*    GO TO 8100-SEND-INITIAL-MAP.                                 14220044
01239                                                                   14230007
01240  EJECT                                                            14240007
01241  4000-BROWSE-FRWD.                                                14250007
01242      MOVE '1'                     TO  PI-PREV-PFKEY.              14260007
01243      MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.                 14270007
01244                                                                   14280007
01245      IF EIBAID = DFHPF1                                           14290007
01246          IF PAGE-FULL                                             14300007
01247              MOVE PI-FILE-SEQ-NO (PI-FULL-INDX)                   14310007
01248                                       TO  PYAJ-FILE-SEQ-NO        14320007
01249              MOVE HIGH-VALUES         TO  PYAJ-RECORD-TYPE        14330007
01250              MOVE 'N'                 TO  PYAJ-READ-SW            14340007
01251              MOVE PI-SAV-ACCT-AMT     TO  TOTAL-ACCT-AMT          14350007
01252              MOVE PI-SAV-ACCT-NET     TO  TOTAL-ACCT-NET          14360007
01253              MOVE PI-SAV-ACCT-AMT     TO  PI-SAV-PREV-AMT         14370007
01254              MOVE PI-SAV-ACCT-NET     TO  PI-SAV-PREV-NET         14380007
01255          ELSE                                                     14390007
01256              MOVE ZEROS               TO  PI-SAV-ACCT-AMT         14400007
01257                                           PI-SAV-ACCT-NET         14410007
01258                                           PI-SAV-PREV-AMT         14420007
01259                                           PI-SAV-PREV-NET         14430007
01260              MOVE 'Y'                 TO  PI-PAGE-SW              14440007
CIDMOD             IF NOT END-OF-FILE                                   14450000
CIDMOD                 MOVE 99999999        TO  PYAJ-FILE-SEQ-NO        14460000
CIDMOD                 MOVE HIGH-VALUES     TO  PYAJ-RECORD-TYPE        14470000
CIDMOD             END-IF                                               14480000
01261              IF TOP-OF-FILE                                       14490007
01262                  MOVE SPACES          TO  PI-PYAJ-FILE-SW         14500007
01263                  MOVE ZEROS           TO  PYAJ-FILE-SEQ-NO        14510007
01264                  MOVE SPACES          TO  PYAJ-RECORD-TYPE        14520007
01265              ELSE                                                 14530007
01266                  MOVE -1              TO  PYAJ-FILE-SEQ-NO        14540007
01267                  MOVE HIGH-VALUES     TO  PYAJ-RECORD-TYPE        14550007
01268      ELSE                                                         14560007
01269          MOVE PI-SAV-PREV-AMT         TO  TOTAL-ACCT-AMT          14570007
01270          MOVE PI-SAV-PREV-NET         TO  TOTAL-ACCT-NET          14580007
01271          IF MAINTI = 'S'                                          14590007
01272              MOVE ZEROS               TO  PYAJ-FILE-SEQ-NO        14600007
01273              MOVE SPACES              TO  PYAJ-RECORD-TYPE.       14610007
01274                                                                   14620007
CIDMOD                                                                  14630000
CIDMOD     MOVE SPACE                  TO  PI-PYAJ-FILE-SW.             14640000
CIDMOD                                                                  14650007
01275      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES              14660007
01276          MOVE PI-CARRIER-SECURITY  TO  PYAJ-CARRIER.              14670007
01277                                                                   14680007
01278  4000-BROWSE-FRWD-FOR-PREV.                                       14690007
CIDMOD*    IF END-OF-FILE                                               14700007
CIDMOD*        IF EIBAID = DFHPF1                                       14710007
CIDMOD*           IF PI-TOTAL-DISPLAYED                                 14720007
CIDMOD*              NEXT SENTENCE                                      14730007
CIDMOD*           ELSE                                                  14740007
CIDMOD*              MOVE -1                 TO  MAINTL                 14750007
CIDMOD*              MOVE ER-2237            TO  EMI-ERROR              14760007
CIDMOD*              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT         14770007
CIDMOD*              MOVE SPACE              TO  PI-PYAJ-FILE-SW        14780007
CIDMOD*              SET PINDX               TO  +2                     14790007
CIDMOD*              MOVE 'TOTAL'            TO  COMM (PINDX)           14800007
CIDMOD*              MOVE PI-SAV-ACCT-AMT    TO  AMTO (PINDX)           14810007
CIDMOD*              SET PINDEX UP BY 1                                 14820007
CIDMOD*              MOVE 'NET TOTAL'        TO  COMM (PINDX)           14830007
CIDMOD*              MOVE PI-SAV-ACCT-NET    TO  AMTO (PINDX)           14840007
CIDMOD*              MOVE ZEROS              TO  PI-SAV-ACCT-AMT        14850007
CIDMOD*                                          PI-SAV-ACCT-NET        14860007
CIDMOD*              GO TO 8100-SEND-INITIAL-MAP.                       14870007
CIDMOD*                                                                 14880007
CIDMOD*     IF END-OF-FILE                                              14890007
CIDMOD*        IF EIBAID = DFHPF1                                       14900007
CIDMOD*           MOVE -1             TO  MAINTL                        14910007
CIDMOD*           MOVE ER-2237        TO  EMI-ERROR                     14920007
CIDMOD*           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT            14930007
CIDMOD*           GO TO 8200-SEND-DATAONLY.                             14940007
CIDMOD*                                                                 14950007
01305      PERFORM 5000-START-BROWSE  THRU  5030-EXIT.                  14960007
01306                                                                   14970007
01307      IF NO-RECORDS                                                14980007
01308         MOVE SPACE              TO  PI-PYAJ-FILE-SW               14990007
01309         MOVE LOW-VALUES         TO  EL633AO                       15000007
01310         MOVE ER-2239            TO  EMI-ERROR                     15010007
01311         MOVE -1                 TO  MAINTL                        15020007
01312         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                15030007
01313         GO TO 8100-SEND-INITIAL-MAP.                              15040007
01314                                                                   15050007
01315      IF NOT-OPEN                                                  15060007
01316          GO TO 7000-PYAJ-FILE-NOTOPEN.                            15070007
01317                                                                   15080007
01318      MOVE LOW-VALUES             TO  EL633AO.                     15090007
01319      MOVE ZEROS                  TO  PI-SEQ-NOS.                  15100007
01320                                                                   15110007
CIDMOD     PERFORM 6000-READ-AND-FORMAT-SCREEN  THRU  6200-EXIT         15130000
CIDMOD          VARYING  PINDX  FROM  1  BY  1                          15140000
CIDMOD             UNTIL  END-OF-ACCT                                   15150000
CIDMOD                 OR  END-OF-FILE                                  15160000
CIDMOD                     OR  PAGE-FULL                                15170000
CIDMOD                         OR  NO-RECORDS.                          15180000
01322                                                                   15190007
01323      IF NO-RECORDS                                                15200007
01324          MOVE SPACE              TO  PI-PYAJ-FILE-SW              15210007
CIDMOD         MOVE LOW-VALUES         TO  EL633AO                      15220000
01325          MOVE ER-2239            TO  EMI-ERROR                    15230007
01326          MOVE -1                 TO  MAINTL                       15240007
01327          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               15250007
01328          GO TO 8100-SEND-INITIAL-MAP.                             15260007
01329                                                                   15270007
01330      IF END-OF-FILE                                               15280007
01331          IF EIBAID = DFHPF2                                       15290007
01332              MOVE ER-2238        TO  EMI-ERROR                    15300007
01333              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           15310007
01334              MOVE 'T'            TO  PI-PYAJ-FILE-SW              15320007
01335          ELSE                                                     15330007
01336              MOVE ER-2237        TO  EMI-ERROR                    15340007
01337              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          15350007
01338                                                                   15360007
01339      MOVE 'S'                    TO  PI-PREV-FUNCTION             15370007
01340                                      PI-SAV-FUNCTION.             15380007
01341                                                                   15390007
01342      GO TO 8100-SEND-INITIAL-MAP.                                 15400007
01343  EJECT                                                            15410007
01344  4100-BROWSE-BKWD.                                                15420007
01345      MOVE SPACE                   TO  PI-PYAJ-FILE-SW             15430007
01346                                       PI-TOTAL-DISPLAYED-SW.      15440007
01347      MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.                 15450007
01348      MOVE ZEROS                   TO  PYAJ-FILE-SEQ-NO.           15460007
01349      MOVE ZEROS                   TO  PYAJ-RECORD-TYPE.           15470007
01350      MOVE ZEROS                   TO  PI-SAV-ACCT-AMT             15480007
01351                                       PI-SAV-ACCT-NET             15490007
01352                                       PI-SAV-PREV-AMT             15500007
01353                                       PI-SAV-PREV-NET.            15510007
01354                                                                   15520007
01355      PERFORM 5000-START-BROWSE  THRU  5030-EXIT.                  15530007
01356                                                                   15540007
01357      IF NO-RECORDS                                                15550007
01358          MOVE SPACES             TO  PI-PYAJ-FILE-SW              15560007
01359          MOVE LOW-VALUES         TO  EL633AO                      15570007
01360          MOVE ER-2239            TO  EMI-ERROR                    15580007
01361          MOVE -1                 TO  MAINTL                       15590007
01362          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               15600007
01363          GO TO 8100-SEND-INITIAL-MAP.                             15610007
01364                                                                   15620007
01365      IF NOT-OPEN                                                  15630007
01366          GO TO 7000-PYAJ-FILE-NOTOPEN.                            15640007
01367                                                                   15650007
01368      EXEC CICS READNEXT                                           15660007
01369          SET      (ADDRESS OF PENDING-PAY-ADJ)                    15670007
01370          DATASET  (PYAJ-FILE-ID)                                  15680007
01371          RIDFLD   (ERPYAJ-KEY)                                    15690007
01372      END-EXEC.                                                    15700007
01373                                                                   15710007
01374      IF FIRST-PAGE OR                                             15720007
01375         PI-PREV-PFKEY = '2'                                       15730007
01376          PERFORM 5100-READ-PREVIOUS  THRU  5120-EXIT              15740007
01377          PERFORM 5100-READ-PREVIOUS  THRU  5120-EXIT.             15750007
01378                                                                   15760007
01379      PERFORM 5200-END-BROWSE  THRU  5200-EXIT.                    15770007
01380                                                                   15780007
01381      MOVE ZEROS                  TO  PYAJ-FILE-SEQ-NO.            15790007
01382      MOVE SPACES                 TO  PYAJ-RECORD-TYPE.            15800007
01383      MOVE '2'                     TO  PI-PREV-PFKEY.              15810007
01384                                                                   15820007
01385      GO TO 4000-BROWSE-FRWD-FOR-PREV.                             15830007
01386  EJECT                                                            15840007
01387  5000-START-BROWSE.                                               15850007
01388      EXEC CICS HANDLE CONDITION                                   15860007
01389          NOTOPEN  (5010-NOT-OPEN)                                 15870007
01390          NOTFND   (5020-NO-RECORDS)                               15880007
01391          ENDFILE  (5020-NO-RECORDS)                               15890007
01392      END-EXEC.                                                    15900007
01393                                                                   15910007
01394      EXEC CICS STARTBR                                            15920007
01395          DATASET  (PYAJ-FILE-ID)                                  15930007
01396          RIDFLD   (ERPYAJ-KEY)                                    15940007
01397      END-EXEC.                                                    15950007
01398                                                                   15960007
01399      GO TO 5030-EXIT.                                             15970007
01400                                                                   15980007
01401  5010-NOT-OPEN.                                                   15990007
01402      MOVE 'Z'                    TO  PI-PYAJ-FILE-SW.             16000007
01403                                                                   16010007
01404      GO TO 5030-EXIT.                                             16020007
01405                                                                   16030007
01406  5020-NO-RECORDS.                                                 16040007
01407      MOVE ZEROS                  TO  PI-SEQ-NOS.                  16050007
01408      MOVE 'Y'                    TO  PI-PYAJ-FILE-SW.             16060007
01409                                                                   16070007
01410  5030-EXIT.                                                       16080007
01411      EXIT.                                                        16090007
01412  EJECT                                                            16100007
01413  5100-READ-PREVIOUS.                                              16110007
01414      EXEC CICS HANDLE CONDITION                                   16120007
01415          ENDFILE  (5110-END-OF-FILE)                              16130007
01416          NOTFND   (5110-END-OF-FILE)                              16140007
01417      END-EXEC.                                                    16150007
01418                                                                   16160007
01419      EXEC CICS READPREV                                           16170007
01420          SET      (ADDRESS OF PENDING-PAY-ADJ)                    16180007
01421          DATASET  (PYAJ-FILE-ID)                                  16190007
01422          RIDFLD   (ERPYAJ-KEY)                                    16200007
01423      END-EXEC.                                                    16210007
01424                                                                   16220007
01425      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES              16230007
01426          IF PI-CARRIER-SECURITY = PY-CARRIER                      16240007
01427              NEXT SENTENCE                                        16250007
01428          ELSE                                                     16260007
01429              GO TO 5110-END-OF-FILE.                              16270007
01430                                                                   16280007
01431      GO TO 5120-EXIT.                                             16290007
01432                                                                   16300007
01433  5110-END-OF-FILE.                                                16310007
CIDMOD     MOVE 'X'                    TO  PI-PYAJ-FILE-SW.             16320000
01434      MOVE ER-2238                TO  EMI-ERROR.                   16330007
01435      MOVE -1                     TO  PFENTERL.                    16340007
01436                                                                   16350007
01437      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  16360007
01438                                                                   16370007
01439      GO TO 8200-SEND-DATAONLY.                                    16380007
01440                                                                   16390007
01441  5120-EXIT.                                                       16400007
01442      EXIT.                                                        16410007
01443                                                                   16420007
01444  5200-END-BROWSE.                                                 16430007
01445      EXEC CICS ENDBR                                              16440007
01446          DATASET  (PYAJ-FILE-ID)                                  16450007
01447      END-EXEC.                                                    16460007
01448                                                                   16470007
01449  5200-EXIT.                                                       16480007
01450      EXIT.                                                        16490007
01451  EJECT                                                            16500007
01452  6000-READ-AND-FORMAT-SCREEN.                                     16510007
01453      EXEC CICS HANDLE CONDITION                                   16520007
01454          ENDFILE  (6100-END-OF-FILE)                              16530007
01455          NOTFND   (6100-END-OF-FILE)                              16540007
01456      END-EXEC.                                                    16550007
01457                                                                   16560007
CIDMOD*    MOVE SPACES                 TO  PI-TOTAL-DISPLAYED-SW.       16570037
CIDMOD*    INITIALIZE  PI-SEQ-NOS.                                      16590037
01461                                                                   16600007
01462  6010-READ-NEXT.                                                  16610007
01463                                                                   16620007
01464      EXEC CICS READNEXT                                           16630007
01465          SET      (ADDRESS OF PENDING-PAY-ADJ)                    16640007
01466          DATASET  (PYAJ-FILE-ID)                                  16650007
01467          RIDFLD   (ERPYAJ-KEY)                                    16660007
01468      END-EXEC.                                                    16670007
01469                                                                   16680007
01470      IF PI-COMPANY-CD NOT = PY-COMPANY-CD                         16690007
01471        AND PI-SAV-ENDING-PYAJ-KEY NOT = SPACES                    16700007
01472          GO TO 6100-END-OF-FILE.                                  16710007
01473                                                                   16720007
01474      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES              16730007
01475          IF PY-CARRIER NOT = PI-CARRIER-SECURITY                  16740007
01476              IF PI-SAV-ENDING-PYAJ-KEY NOT = SPACES               16750007
01477                  GO TO 6100-END-OF-FILE.                          16760007
01478                                                                   16770007
01479      IF PYAJ-1ST-READ                                             16780007
01480        AND EIBAID NOT = DFHENTER                                  16790007
01481        AND NOT PAGE-FULL                                          16800007
01482          MOVE PY-CONTROL-PRIMARY TO  PI-SAV-ENDING-PYAJ-KEY       16810007
01483          MOVE PY-CARRIER         TO  PI-CR-CARRIER                16820007
01484          MOVE PY-GROUPING        TO  PI-CR-GROUPING               16830007
01485          MOVE PY-FIN-RESP        TO  PI-CR-FIN-RESP               16840007
01486          MOVE PY-ACCOUNT         TO  PI-CR-ACCOUNT.               16850007
01487                                                                   16860007
01488      IF PYAJ-1ST-READ                                             16870007
01489          MOVE PI-SAV-ENDING-PYAJ-KEY                              16880007
01490                                  TO  PI-START-PYAJ-KEY.           16890007
01491                                                                   16900007
01492      IF PI-COMPANY-CD   = PY-COMPANY-CD  AND                      16910007
01493         PI-SAV-CARRIER  = PY-CARRIER     AND                      16920007
01494         PI-SAV-GROUPING = PY-GROUPING    AND                      16930007
01495         PI-SAV-FIN-RESP = PY-FIN-RESP    AND                      16940007
01496         PI-SAV-ACCOUNT  = PY-ACCOUNT                              16950007
01497          NEXT SENTENCE                                            16960007
01498      ELSE                                                         16970007
01499          IF PYAJ-1ST-READ                                         16980007
01501              MOVE 'Y'            TO  PI-PYAJ-FILE-SW              16990007
01502              MOVE SPACE          TO  PYAJ-READ-SW                 17000007
01503              GO TO 6200-EXIT                                      17010007
01504          ELSE                                                     17020007
CIDMOD             MOVE 'A'            TO  PI-PYAJ-FILE-SW              17030000
CIDMOD             IF PINDX  IS LESS THAN  12                           17040007
01506                  SET PINDX  UP  BY  1                             17050007
CIDMOD                 MOVE 'TOTAL'         TO  GL-ACCT (PINDX)         17060023
CIDMOD                 MOVE TOTAL-ACCT-AMT  TO  AMTO (PINDX)            17070000
CIDMOD                 SET PINDX  UP  BY  1                             17080000
CIDMOD                 MOVE 'NET TOTAL'     TO  GL-ACCT (PINDX)         17090023
CIDMOD                 MOVE TOTAL-ACCT-NET  TO  AMTO (PINDX)            17100000
01520                      GO TO 6200-EXIT                              17110007
01521              ELSE                                                 17120007
CIDMOD                 MOVE 'TOTAL'         TO  GL-ACCT (PINDX)         17130023
CIDMOD                 MOVE TOTAL-ACCT-AMT  TO  AMTO (PINDX)            17140000
CIDMOD                 SET PINDX  UP  BY  1                             17150000
CIDMOD                 MOVE 'NET TOTAL'     TO  GL-ACCT (PINDX)         17160023
CIDMOD                 MOVE TOTAL-ACCT-NET  TO  AMTO (PINDX)            17170000
01537                  GO TO 6200-EXIT.                                 17180007
01538                                                                   17190007
CIDMOD*    MOVE ' '                    TO  PI-PYAJ-FILE-SW.             17200007
01540                                                                   17210007
01541      IF PY-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      17220007
01542          GO TO 6000-READ-AND-FORMAT-SCREEN.                       17230007
01543                                                                   17240007
CIDMOD*    SET PINDX  UP  BY +1.                                        17250012
01545                                                                   17260007
01546      SET WS-SAVE-INDEX-VALUE  TO  PINDX.                          17270007
01547                                                                   17280007
01548      IF PINDX  IS GREATER THAN  13                                17290007
01549          MOVE 'F'                TO  PI-PYAJ-FILE-SW              17300007
01550          MOVE +13                TO  PI-FULL-INDX                 17310007
01551          MOVE PI-FRST-FILE-SEQ-NO                                 17320007
01552                                  TO  PI-LAST-FILE-SEQ-NO          17330007
01553          MOVE PI-FRST-RECORD-TYPE                                 17340007
01554                                  TO  PI-LAST-RECORD-TYPE          17350007
01555          MOVE TOTAL-ACCT-AMT     TO  PI-SAV-ACCT-AMT              17360007
01556          MOVE TOTAL-ACCT-NET     TO  PI-SAV-ACCT-NET              17370007
01557          GO TO 6200-EXIT.                                         17380007
01558                                                                   17390007
01559      MOVE PY-FILE-SEQ-NO         TO  PI-PREV-FILE-SEQ-NO.         17400007
01560      MOVE PY-RECORD-TYPE         TO  PI-PREV-RECORD-TYPE.         17410007
01561                                                                   17420007
01562      IF PYAJ-1ST-READ                                             17430007
01563          MOVE SPACE              TO  PYAJ-READ-SW.                17440007
01564                                                                   17450007
01565      IF PINDX = 1                                                 17460007
01566          MOVE PI-PREV-FILE-SEQ-NO                                 17470007
01567                                  TO  PI-FRST-FILE-SEQ-NO          17480007
01568          MOVE PI-PREV-RECORD-TYPE                                 17490007
01569                                  TO  PI-FRST-RECORD-TYPE          17500007
01570          MOVE PI-PREV-FILE-SEQ-NO                                 17510007
01571                                  TO  PI-LAST-FILE-SEQ-NO          17520007
01572          MOVE PI-PREV-RECORD-TYPE                                 17530007
01573                                  TO  PI-LAST-RECORD-TYPE.         17540007
01574                                                                   17550007
01575      SET NDX                     TO  PINDX.                       17560007
01576      SET WS-SAVE-NDX-VALUE       TO  NDX.                         17570007
01577                                                                   17580007
01578      MOVE PY-FILE-SEQ-NO         TO  PI-FILE-SEQ-NO (NDX).        17590007
PEMMOD                                                                  17593046
CIDMOD     MOVE PY-GL-ACCOUNT          TO  GL-ACCT (PINDX).             17600023
CIDMOD     MOVE PY-GL-STATE            TO  GL-STATE (PINDX).            17610023
CIDMOD     MOVE PY-GL-CANC-SW          TO  GL-CANC (PINDX).             17620023
CIDMOD     MOVE PY-GL-COMMENT          TO  GL-COMM (PINDX).             17630023
01580      MOVE PY-ENTRY-AMT           TO  AMTO (PINDX).                17640007
01581                                                                   17650007
01582      ADD PY-ENTRY-AMT            TO  TOTAL-ACCT-AMT.              17660007
01583                                                                   17670007
031710     IF PY-RECORD-TYPE = 'R' OR 'D' OR 'S' OR 'Z'
01585          ADD PY-ENTRY-AMT        TO  TOTAL-ACCT-NET               17690007
01586      ELSE                                                         17700007
01587          SUBTRACT PY-ENTRY-AMT  FROM  TOTAL-ACCT-NET.             17710007
01588                                                                   17720007
01589      MOVE PY-RECORD-TYPE         TO  RTYPE (PINDX)                17730007
01590                                      PI-REC-TYPE (NDX).           17740007
01591                                                                   17750007
01592      IF PY-VOID-SW NOT = SPACE                                    17760007
01593          MOVE PY-VOID-SW         TO  VOID-SW (PINDX).             17770007
01594                                                                   17780007
01595      IF PY-LAST-MAINT-DT = PREV-BIN-MAINT-DT                      17790007
01596          MOVE PREV-MAINT-DT      TO  MDTE (PINDX)                 17800007
01597      ELSE                                                         17810007
01598          MOVE PY-LAST-MAINT-DT   TO  DC-BIN-DATE-1                17820007
01599                                      PREV-BIN-MAINT-DT            17830007
01600          MOVE SPACE              TO  DC-OPTION-CODE               17840007
01601          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT               17850007
01602** DAN    MOVE DC-GREG-DATE-1-EDIT                                 17860027
CIDMOD         MOVE DC-GREG-DATE-1-MDY                                  17861027
01603                                  TO  MDTE (PINDX)                 17870007
01604                                      PREV-MAINT-DT.               17880007
01605                                                                   17890007
CIDMOD*    IF PY-GL-COMMENT NOT = (LOW-VALUES OR SPACES OR ZEROS)       17900040
PEMMOD     IF PY-GL-COMMENT NOT = (LOW-VALUES AND SPACES AND ZEROS)     17900146
CIDMOD         INSPECT PY-GL-COMMENT CONVERTING LOW-VALUES TO SPACES    17901023
CIDMOD         MOVE PY-GL-COMMENT      TO  GL-COMM (PINDX)              17902023
CIDMOD*        MOVE AL-UANON           TO  GL-COMM-ATTRB (PINDX)        17910042
PEMMOD*        MOVE AL-UANON           TO  GL-COMM-ATTRB (PINDX)        17920046
CIDMOD     END-IF.                                                      17931115
CIDMOD                                                                  17931215
01606      IF PY-BILLED-DATE NOT = LOW-VALUES                           17932015
CIDMOD         MOVE AL-SANOF           TO  GL-ACCT-ATTRB (PINDX)        17933023
CIDMOD                                     AMT-ATTRB (PINDX)            17934015
CIDMOD                                     VOID-SW-ATTRB (PINDX)        17935015
01610          IF PY-BILLED-DATE = PREV-BIN-BL-DT                       17940007
01611              MOVE PREV-BL-DT     TO  BDTE (PINDX)                 17950007
01612          ELSE                                                     17960007
01613              MOVE PY-BILLED-DATE TO  DC-BIN-DATE-1                17970007
01614                                      PREV-BIN-BL-DT               17980007
01615              MOVE SPACE          TO  DC-OPTION-CODE               17990007
01616              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT           18000007
01617** DAN        MOVE DC-GREG-DATE-1-EDIT                             18010028
CIDMOD             MOVE DC-GREG-DATE-1-MDY                              18011028
01618                                  TO  BDTE (PINDX)                 18020007
01619                                      PREV-BL-DT.                  18030007
01620                                                                   18040007
01621      IF (PY-CHECK-ORIGIN-SW = LOW-VALUES OR SPACES OR 'G')        18050019
01622          NEXT SENTENCE                                            18060007
01623      ELSE                                                         18070007
01624          MOVE AL-SANOF           TO  RTYPE-ATTRB   (PINDX)        18080007
01625                                      AMT-ATTRB     (PINDX).       18090007
01626                                                                   18100007
01627      IF PY-CREDIT-SELECT-DT NOT = LOW-VALUES                      18110007
01628          MOVE PY-CREDIT-SELECT-DT                                 18120007
01629                                  TO  DC-BIN-DATE-1                18130007
01630          MOVE SPACE              TO  DC-OPTION-CODE               18140007
01631          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT               18150007
CIDMOD                                                                  18160007
CIDMOD         MOVE DC-GREG-DATE-1-EDIT  TO  WS-GREG-STORE              18170000
CIDMOD         MOVE WS-GS-MM             TO  WS-SS-MM                   18180000
CIDMOD         MOVE WS-GS-DD             TO  WS-SS-DD                   18190000
CIDMOD         MOVE WS-GS-YY             TO  WS-SS-YY                   18200000
CIDMOD         MOVE WS-SDTE-STORE        TO  SDTE (PINDX).              18210000
PEMMOD     IF (PY-INPUT-DT NOT = WS-CURRENT-BIN-DT)
              AND (PI-PROCESSOR-ID NOT = 'PEMA')
PEMMOD         MOVE AL-SANOF           TO RTYPE-ATTRB    (PINDX)        18211146
PEMMOD                                    AMT-ATTRB      (PINDX)        18211246
PEMMOD                                    GL-ACCT-ATTRB  (PINDX)        18211346
PEMMOD                                    GL-COMM-ATTRB  (PINDX)        18211446
PEMMOD                                    VOID-SW-ATTRB  (PINDX)        18211546
PEMMOD                                    GL-STATE-ATTRB (PINDX)        18211646
PEMMOD                                    GL-CANC-ATTRB  (PINDX)        18211746
PEMMOD                                    SDTE-ATTRB     (PINDX)        18211846
PEMMOD                                                                  18212046
PEMMOD     END-IF                                                       18213046
01634                                                                   18220007
CIDMOD*    GO TO 6010-READ-NEXT.                                        18230007
CIDMOD     GO TO 6200-EXIT.                                             18240000
01636                                                                   18250007
01637  6100-END-OF-FILE.                                                18260007
01638      MOVE 'X'                    TO  PI-PYAJ-FILE-SW.             18270007
01639                                                                   18280007
01640      IF PINDX  IS LESS THAN  12                                   18290007
CIDMOD         SET PINDX  UP  BY  1                                     18300000
CIDMOD         MOVE 'TOTAL'            TO  GL-ACCT (PINDX)              18310023
01643          MOVE TOTAL-ACCT-AMT     TO  AMTO (PINDX)                 18320007
01644          SET PINDX  UP  BY  1                                     18330007
CIDMOD         MOVE 'NET TOTAL'        TO  GL-ACCT (PINDX)              18340023
01646          MOVE TOTAL-ACCT-NET     TO  AMTO (PINDX)                 18350007
01647          MOVE 'Y'                TO  PI-TOTAL-DISPLAYED-SW        18360007
01648      ELSE                                                         18370007
CIDMOD         MOVE 'TOTAL'            TO  GL-ACCT (PINDX)              18380023
CIDMOD         MOVE TOTAL-ACCT-AMT     TO  AMTO (PINDX)                 18390000
CIDMOD         SET PINDX  UP  BY  1                                     18400000
CIDMOD         MOVE 'NET TOTAL'        TO  GL-ACCT (PINDX)              18410023
CIDMOD         MOVE TOTAL-ACCT-NET     TO  AMTO (PINDX).                18420000
01651                                                                   18430007
01652  6200-EXIT.                                                       18440007
01653      EXIT.                                                        18450007
01654  EJECT                                                            18460007
01655  7000-PYAJ-FILE-NOTOPEN.                                          18470007
01656      MOVE -1                     TO  MAINTL.                      18480007
01657      MOVE ER-2232                TO  EMI-ERROR.                   18490007
01658                                                                   18500007
01659      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  18510007
01660                                                                   18520007
01661      GO TO 8200-SEND-DATAONLY.                                    18530007
01662                                                                   18540007
01663  7100-COMP-FILE-NOTOPEN.                                          18550007
01664      MOVE -1                     TO  MAINTL.                      18560007
01665      MOVE ER-2233                TO  EMI-ERROR.                   18570007
01666                                                                   18580007
01667      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  18590007
01668                                                                   18600007
01669      GO TO 8200-SEND-DATAONLY.                                    18610007
01670  EJECT                                                            18620007
CIDMOD*                                                                 18630022
CIDMOD*7100-COFA-FILE-NOTOPEN.                                          18640022
CIDMOD*    MOVE -1                     TO PFENTERL.                     18650022
CIDMOD*    MOVE ER-2959                TO EMI-ERROR.                    18660022
CIDMOD*    PERFORM 9900-ERROR-FORMAT THRU                               18670022
CIDMOD*            9900-EXIT.                                           18680022
CIDMOD*    GO TO 8200-SEND-DATAONLY.                                    18690022
CIDMOD*                                                                 18700022
01671  8100-SEND-INITIAL-MAP.                                           18710007
01672      MOVE WS-CURRENT-DT          TO  DATEO.                       18720007
01673      MOVE EIBTIME                TO  TIME-IN.                     18730007
01674      MOVE TIME-OUT               TO  TIMEO.                       18740007
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.                    18741048
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.                     18742048
01675      MOVE -1                     TO  MAINTL.                      18750007
01676      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    18760007
01677      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.                    18770007
01678                                                                   18780007
01679      IF EIBTRNID = TRANS-ID                                       18790007
01680        OR EL640-TRANS-ID                                          18800007
01681        OR EL642-TRANS-ID                                          18810007
01682        OR EL652-TRANS-ID                                          18820007
01683        OR EL6331-TRANS-ID                                         18830007
01684          IF PI-SAV-ENDING-PYAJ-KEY NOT =  SPACES                  18840007
01685              MOVE PI-SAV-FUNCTION  TO  MAINTI                     18850007
01686              MOVE PI-SAV-CARRIER   TO  CARRIERO                   18860007
01687              MOVE PI-SAV-GROUPING  TO  GROUPO                     18870007
01688              MOVE PI-SAV-FIN-RESP  TO  FINRESPO                   18880007
01689              MOVE PI-SAV-ACCOUNT   TO  ACCTO                      18890007
01690              MOVE AL-UANON         TO  MAINTA                     18900007
01691                                        CARRIERA                   18910007
01692                                        GROUPA                     18920007
01693                                        FINRESPA                   18930007
01694                                        ACCTA                      18940007
01695          ELSE                                                     18950007
01696              NEXT SENTENCE.                                       18960007
01697                                                                   18970007
01698      EXEC CICS SEND                                               18980007
01699          MAP     (MAP-NAME)                                       18990007
01700          MAPSET  (MAPSET-NAME)                                    19000007
01701          FROM    (EL633AO)                                        19010007
01702          ERASE                                                    19020007
01703          CURSOR                                                   19030007
01704          END-EXEC.                                                19040007
01705                                                                   19050007
01706      GO TO 9100-RETURN-TRAN.                                      19060007
01707  EJECT                                                            19070007
01708  8200-SEND-DATAONLY.                                              19080007
01709      MOVE WS-CURRENT-DT          TO  DATEO.                       19090007
01710      MOVE EIBTIME                TO  TIME-IN.                     19100007
01711      MOVE TIME-OUT               TO  TIMEO.                       19110007
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.                    19111048
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.                     19112048
01712      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    19120007
01713      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.                    19130007
01714                                                                   19140007
01715      EXEC CICS SEND                                               19150007
01716          MAP     (MAP-NAME)                                       19160007
01717          MAPSET  (MAPSET-NAME)                                    19170007
01718          FROM    (EL633AO)                                        19180007
01719          DATAONLY                                                 19190007
01720          CURSOR                                                   19200007
01721      END-EXEC.                                                    19210007
01722                                                                   19220007
01723      GO TO 9100-RETURN-TRAN.                                      19230007
01724                                                                   19240007
01725  8300-SEND-TEXT.                                                  19250007
01726      EXEC CICS SEND TEXT                                          19260007
01727          FROM    (LOGOFF-TEXT)                                    19270007
01728          LENGTH  (LOGOFF-LENGTH)                                  19280007
01729          ERASE                                                    19290007
01730          FREEKB                                                   19300007
01731      END-EXEC.                                                    19310007
01732                                                                   19320007
01733      EXEC CICS RETURN                                             19330007
01734      END-EXEC.                                                    19340007
01735  EJECT                                                            19350007
01736  8400-LOG-JOURNAL-RECORD.                                         19360007
01737 *    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  19370007
01738 *    MOVE THIS-PGM               TO  JP-PROGRAM-ID.               19380007
01739                                                                   19390007
01740 *    EXEC CICS JOURNAL                                            19400007
01741 *        JFILEID  (PI-JOURNAL-FILE-ID)                            19410007
01742 *        JTYPEID  ('EL')                                          19420007
01743 *        FROM     (JOURNAL-RECORD)                                19430007
01744 *        LENGTH   (223)                                           19440007
01745 *        END-EXEC.                                                19450007
01746                                                                   19460007
01747  8500-DATE-CONVERT.                                               19470007
01748      EXEC CICS LINK                                               19480007
01749          PROGRAM   (LINK-CLDATCV)                                 19490007
01750          COMMAREA  (DATE-CONVERSION-DATA)                         19500007
01751          LENGTH    (DC-COMM-LENGTH)                               19510007
01752      END-EXEC.                                                    19520007
01753                                                                   19530007
01754  8500-EXIT.                                                       19540007
01755      EXIT.                                                        19550007
01756                                                                   19560007
01757  8600-DEEDIT.                                                     19570007
01758      EXEC CICS BIF DEEDIT                                         19580007
01759          FIELD   (DEEDIT-FIELD)                                   19590007
01760          LENGTH  (11)                                             19600007
01761      END-EXEC.                                                    19610007
01762                                                                   19620007
01763  8600-EXIT.                                                       19630007
01764      EXIT.                                                        19640007
01765  EJECT                                                            19650007
01766  8800-UNAUTHORIZED-ACCESS.                                        19660007
01767      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  19670007
01768                                                                   19680007
01769      GO TO 8300-SEND-TEXT.                                        19690007
01770                                                                   19700007
01771  8810-PF23.                                                       19710007
01772      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               19720007
01773      MOVE XCTL-005               TO  PGM-NAME.                    19730007
01774                                                                   19740007
01775      GO TO 9300-XCTL.                                             19750007
01776                                                                   19760007
01777  9100-RETURN-TRAN.                                                19770007
01778      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            19780007
01779      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.        19790007
01780                                                                   19800007
01781      EXEC CICS RETURN                                             19810007
01782          TRANSID   (TRANS-ID)                                     19820007
01783          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      19830007
01784          LENGTH    (PI-COMM-LENGTH)                               19840007
01785      END-EXEC.                                                    19850007
01786                                                                   19860007
01787  9200-RETURN-MAIN-MENU.                                           19870007
01788      MOVE XCTL-626               TO  PGM-NAME.                    19880007
01789                                                                   19890007
01790      GO TO 9300-XCTL.                                             19900007
01791                                                                   19910007
01792  9300-XCTL.                                                       19920007
01793      EXEC CICS XCTL                                               19930007
01794          PROGRAM   (PGM-NAME)                                     19940007
01795          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      19950007
01796          LENGTH    (PI-COMM-LENGTH)                               19960007
01797      END-EXEC.                                                    19970007
01798                                                                   19980007
01799  9400-CLEAR.                                                      19990007
01800      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    20000007
01801                                                                   20010007
01802      IF PI-RETURN-TO-PROGRAM = 'EL626'                            20020007
01803      MOVE SPACES                 TO  PI-CR-CONTROL-IN-PROGRESS    20030007
01804                                      PI-SAV-COMP-CONTROL.         20040007
01805                                                                   20050007
01806      GO TO 9300-XCTL.                                             20060007
01807                                                                   20070007
01808  9500-PF12.                                                       20080007
01809      MOVE XCTL-010               TO  PGM-NAME.                    20090007
01810                                                                   20100007
01811      GO TO 9300-XCTL.                                             20110007
01812                                                                   20120007
01813  9600-PGMID-ERROR.                                                20130007
01814      EXEC CICS HANDLE CONDITION                                   20140007
01815          PGMIDERR  (8300-SEND-TEXT)                               20150007
01816      END-EXEC.                                                    20160007
01817                                                                   20170007
01818      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          20180007
01819      MOVE ' '                    TO  PI-ENTRY-CD-1.               20190007
01820      MOVE XCTL-005               TO  PGM-NAME.                    20200007
01821      MOVE PGM-NAME               TO  LOGOFF-PGM.                  20210007
01822      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 20220007
01823                                                                   20230007
01824      GO TO 9300-XCTL.                                             20240007
01825                                                                   20250007
01826  9900-ERROR-FORMAT.                                               20260007
01827      IF NOT EMI-ERRORS-COMPLETE                                   20270007
01828          MOVE LINK-001           TO  PGM-NAME                     20280007
01829          EXEC CICS LINK                                           20290007
01830              PROGRAM   (PGM-NAME)                                 20300007
01831              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            20310007
01832              LENGTH    (EMI-COMM-LENGTH)                          20320007
01833          END-EXEC.                                                20330007
01834                                                                   20340007
01835  9900-EXIT.                                                       20350007
01836      EXIT.                                                        20360007
01837                                                                   20370007
01838  9990-ABEND.                                                      20380007
01839      MOVE LINK-004               TO  PGM-NAME.                    20390007
01840      MOVE DFHEIBLK               TO  EMI-LINE1.                   20400007
01841                                                                   20410007
01842      EXEC CICS LINK                                               20420007
01843          PROGRAM   (PGM-NAME)                                     20430007
01844          COMMAREA  (EMI-LINE1)                                    20440007
01845          LENGTH    (72)                                           20450007
01846      END-EXEC.                                                    20460007
01847                                                                   20470007
01848      MOVE -1                     TO  PFENTERL.                    20480007
01849                                                                   20490007
01850      GO TO 8200-SEND-DATAONLY.                                    20500007
01851                                                                   20510007
01852  9995-SECURITY-VIOLATION.                                         20520007
01853                              COPY ELCSCTP.                        20530007
01854                                                                   20540007
01855  9995-EXIT.                                                       20550007
01856      EXIT.                                                        20560007
01857                                                                   20570007

