00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL633.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 12/28/94 10:04:35.
00007 *                            VMOD=2.021
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
00019 *                                                                *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024 *
00025 *REMARKS.
00026 *        TRANSACTION - EXB7 - COMPENSATION PAYMENTS/ADJUSTMENTS.
00024 *
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL633AI FILLER
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
101101******************************************************************
00027
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030  EJECT
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*    EL633 WORKING STORAGE     *'.
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.021 *********'.
00035
CIDMOD*77  FILLER  PIC X(32)  VALUE '/////// SAVE COFA MASTER /////'.
CIDMOD*77  SV-COFA PIC X(42)  VALUE SPACES.
CIDMOD*77  K-SPACE PIC X(11)  VALUE SPACES.
CIDMOD
CIDMOD 01  DATE-WORK-AREAS.
CIDMOD     05  WS-GREG-STORE.
CIDMOD         10  WS-GS-MM  PIC XX.
CIDMOD         10  FILLER    PIC X.
CIDMOD         10  WS-GS-DD  PIC XX.
CIDMOD         10  FILLER    PIC X.
CIDMOD         10  WS-GS-YY  PIC XX.
CIDMOD     05  WS-SDTE-STORE.
CIDMOD         10  WS-SS-MM  PIC XX.
CIDMOD         10  WS-SS-DD  PIC XX.
CIDMOD         10  WS-SS-YY  PIC XX.
00036 *                            COPY ELCSCTM.
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
00037 *                            COPY ELCSCRTY.
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
00038
00039     EJECT
00040
00041  01  STANDARD-AREAS.
00042      12  SC-ITEM             PIC  S9(4) COMP VALUE +1.
00043      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.
00044      12  MAP-NAME            PIC  X(8)       VALUE 'EL633A'.
00045      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL633S'.
00046      12  SCREEN-NUMBER       PIC  X(4)       VALUE '633A'.
00047      12  TRANS-ID            PIC  X(4)       VALUE 'EXB7'.
00048      12  EL6331-TRANS-ID     PIC  X(4)       VALUE 'EXB8'.
00049      12  EL640-TRANS-ID      PIC  X(4)       VALUE 'EXC1'.
00050      12  EL642-TRANS-ID      PIC  X(4)       VALUE 'EXH7'.
00051      12  EL652-TRANS-ID      PIC  X(4)       VALUE 'EXD4'.
00052      12  THIS-PGM            PIC  X(8)       VALUE 'EL633'.
00053      12  PGM-NAME            PIC  X(8).
00054      12  TIME-IN             PIC S9(7).
00055      12  TIME-OUT-R  REDEFINES  TIME-IN.
00056          16  FILLER          PIC  X.
00057          16  TIME-OUT        PIC  99V99.
00058          16  FILLER          PIC  X(2).
00059      12  EL640               PIC  X(8)       VALUE 'EL640'.
00060      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.
00061      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.
00062      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.
00063      12  XCTL-6331           PIC  X(8)       VALUE 'EL6331'.
00064      12  XCTL-640            PIC  X(8)       VALUE 'EL640'.
00065      12  XCTL-642            PIC  X(8)       VALUE 'EL642'.
00066      12  XCTL-652            PIC  X(8)       VALUE 'EL652'.
00067      12  LINK-001            PIC  X(8)       VALUE 'EL001'.
00068      12  LINK-004            PIC  X(8)       VALUE 'EL004'.
00069      12  RETURNED-FROM       PIC  X(8)       VALUE SPACES.
00070      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.
00071      12  PYAJ-FILE-ID        PIC  X(8)       VALUE 'ERPYAJ'.
00072      12  CHKQ-FILE-ID        PIC  X(8)       VALUE 'ERCHKQ'.
00073      12  COMP-FILE-ID        PIC  X(8)       VALUE 'ERCOMP'.
00074      12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.
00075      12  WS-CURRENT-BIN-DT   PIC  X(2)       VALUE SPACES.
00076      12  PYAJ-READ-SW        PIC  X          VALUE 'Y'.
00077          88  PYAJ-1ST-READ                   VALUE 'Y'.
00078      12  FIRST-ADD-SW        PIC  X          VALUE 'Y'.
00079          88  FIRST-ADD                       VALUE 'Y'.
00080      12  ZERO-NDX            PIC  9          VALUE ZERO.
00081      12  WORK-SEQ-NO         PIC S9(8)       COMP-3 VALUE ZEROS.
00082      12  WORK-SEQ-TIME       PIC  9(6).
00083      12  WORK-SEQ-HHMMSS  REDEFINES  WORK-SEQ-TIME.
00084          16  WORK-SEQ-HHMMS  PIC  9(5).
00085          16  FILLER          PIC  9.
00086      12  WORK-DATE-JULIAN.
00087          16  WORK-JULIAN-YR  PIC  99         VALUE ZEROS.
00088          16  WORK-JULIAN-DD  PIC  999        VALUE ZEROS.
00089      12  JULIAN-YY-DD        PIC  9(4)       COMP-3 VALUE ZEROS.
00090      12  CHECK-REC-TYPE      PIC  X          VALUE SPACE.
00091          88  VALID-REC-TYPE                  VALUE  'R' 'D' 'C'
00092                                                     'S' 'T' 'U'
00093                                                     'X' 'Y' 'Z'
00094                                                     'F'.
CIDMOD     12  CHECK-CANC-TYPE     PIC  X          VALUE SPACE.
CIDMOD         88  VALID-CANC-TYPE                 VALUE 'N' 'Y' ' '.
00095      12  FORCE-SHOW-SW       PIC  X          VALUE SPACE.
00096          88  FORCE-SHOW                      VALUE  'Y'.
00097
00098      12  WS-EOM-DTS OCCURS 13  TIMES
00099                               INDEXED BY PINDEX.
00100          16  WS-EOM-DT               PIC XX.
CIDMOD*    12  COFA-FILE-ID        PIC  X(8)       VALUE 'COFAXXX '.
00101
00102      EJECT
00103      12  WS-ERROR-CODES.
00104          16  ER-0000         PIC  X(4)       VALUE '0000'.
00105          16  ER-0008         PIC  X(4)       VALUE '0008'.
00106          16  ER-0023         PIC  X(4)       VALUE '0023'.
00107          16  ER-0029         PIC  X(4)       VALUE '0029'.
00108          16  ER-0070         PIC  X(4)       VALUE '0070'.
00109          16  ER-0587         PIC  X(4)       VALUE '0587'.
00110          16  ER-2056         PIC  X(4)       VALUE '2056'.
00111          16  ER-2230         PIC  X(4)       VALUE '2230'.
00112          16  ER-2231         PIC  X(4)       VALUE '2231'.
00113          16  ER-2232         PIC  X(4)       VALUE '2232'.
00114          16  ER-2233         PIC  X(4)       VALUE '2233'.
00115          16  ER-2234         PIC  X(4)       VALUE '2234'.
00116          16  ER-2235         PIC  X(4)       VALUE '2235'.
00117          16  ER-2236         PIC  X(4)       VALUE '2236'.
00118          16  ER-2237         PIC  X(4)       VALUE '2237'.
00119          16  ER-2238         PIC  X(4)       VALUE '2238'.
00120          16  ER-2239         PIC  X(4)       VALUE '2239'.
00121          16  ER-2244         PIC  X(4)       VALUE '2244'.
00122          16  ER-2245         PIC  X(4)       VALUE '2245'.
00123          16  ER-2246         PIC  X(4)       VALUE '2246'.
00124          16  ER-2370         PIC  X(4)       VALUE '2370'.
00125          16  ER-2432         PIC  X(4)       VALUE '2432'.
00126          16  ER-2449         PIC  X(4)       VALUE '2449'.
00127          16  ER-2587         PIC  X(4)       VALUE '2587'.
00128          16  ER-2588         PIC  X(4)       VALUE '2588'.
00129          16  ER-2595         PIC  X(4)       VALUE '2595'.
00130          16  ER-2596         PIC  X(4)       VALUE '2596'.
00131          16  ER-2763         PIC  X(4)       VALUE '2763'.
00132          16  ER-2929         PIC  X(4)       VALUE '2929'.
CIDMOD         16  ER-2957         PIC  X(4)       VALUE '2957'.
CIDMOD         16  ER-2958         PIC  X(4)       VALUE '2958'.
CIDMOD*        16  ER-2959         PIC  X(4)       VALUE '2958'.
CIDMOD         16  ER-2960         PIC  X(4)       VALUE '2960'.
00133          16  ER-3020         PIC  X(4)       VALUE '3020'.
00134
CIDMOD*************************************************************
CIDMOD*                      TABLE OF STATE NAMES
CIDMOD*************************************************************
CIDMOD
CIDMOD 01  CHECK-STATE-CODE            PIC XX      VALUE SPACE.
CIDMOD     88  VALID-STATE-CODE    VALUE 'AK' 'AL' 'AR' 'AZ' 'CA'
CIDMOD                                   'CD' 'CO' 'CT' 'DC' 'DE'
CIDMOD                                   'FL' 'GA' 'GM' 'HI' 'IA'
CIDMOD                                   'ID' 'IL' 'IN' 'KS' 'KY'
CIDMOD                                   'LA' 'MA' 'MD' 'ME' 'MI'
CIDMOD                                   'MN' 'MO' 'MS' 'MT' 'MX'
CIDMOD                                   'NC' 'ND' 'NE' 'NH' 'NJ'
CIDMOD                                   'NM' 'NV' 'NY' 'OF' 'OH'
CIDMOD                                   'OK' 'OR' 'PA' 'PI' 'PR'
CIDMOD                                   'RI' 'SC' 'SD' 'TN' 'TX'
CIDMOD                                   'UT' 'VA' 'VI' 'VT' 'WA'
CIDMOD                                   'WI' 'WV' 'WY'.
CIDMOD
CIDMOD 01  CHECK-GL-ACCT         PIC X(10)  VALUE SPACE.
CIDMOD     88  VALID-GL-ACCOUNT  VALUE '1108121010'
111715                                 '1108124700'
CIDMOD                                 '1108125100'
CIDMOD                                 '1721211400'
CIDMOD                                 '1825011200'
CIDMOD                                 '1825011300'
100713                                 '1825091000'
CIDMOD                                 '1825099050'
031909                                 '8505700033'
CIDMOD                                 '8506400030'
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
CIDMOD
CIDMOD*01  WS-ACCT-BREAK.
CIDMOD*    05  WS-ACCT-1               PIC X.
CIDMOD*    05  FILLER                  PIC X(6).
CIDMOD*
00135  01  WORK-AREA.
PEMMOD     12  WS-SAVE-ERPYAJ      PIC X(200)      VALUE SPACES.
00136      12  QID.
00137          16  QID-TERM        PIC X(04).
00138          16  FILLER          PIC X(04)       VALUE '633A'.
00139      12  DEEDIT-FIELD        PIC X(11).
00140      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(11).
CIDMOD     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD PIC S9(9)V99.
00141      12  DATE-TEST-AREA      PIC  9(6).
00142      12  DATE-TEST-AREA-R  REDEFINES  DATE-TEST-AREA.
00143          16  DATE-TEST-MM    PIC  99.
00144          16  DATE-TEST-DD    PIC  99.
00145          16  DATE-TEST-YY    PIC  99.
00146      12  DIVIDE-RESULT       PIC  99.
00147      12  DIVIDE-REMAINDER    PIC  9.
00148      12  PREV-BIN-MAINT-DT   PIC  XX           VALUE SPACES.
00149      12  PREV-MAINT-DT       PIC  X(8)         VALUE SPACES.
00150      12  PREV-BIN-BL-DT      PIC  XX           VALUE SPACES.
00151      12  PREV-BL-DT          PIC  X(8)         VALUE SPACES.
00152      12  TOTAL-ACCT-AMT      PIC S9(7)V99      VALUE ZEROS.
00153      12  TOTAL-ACCT-NET      PIC S9(7)V99      VALUE ZEROS.
00154      12  WS-SAVE-INDEX-VALUE PIC S9(4) COMP    VALUE ZEROS.
00155      12  WS-SAVE-NDX-VALUE   PIC S9(4) COMP    VALUE ZEROS.
00156
00157  01  ACCESS-KEYS.
00158      12  ERPYAJ-KEY.
00159          16  PYAJ-COMP-CD            PIC  X      VALUE SPACE.
00160          16  PYAJ-CARRIER            PIC  X      VALUE SPACES.
00161          16  PYAJ-GROUPING           PIC  X(6)   VALUE SPACES.
00162          16  PYAJ-FIN-RESP           PIC  X(10)  VALUE SPACES.
00163          16  PYAJ-ACCOUNT            PIC  X(10)  VALUE SPACES.
00164          16  PYAJ-FILE-SEQ-NO        PIC S9(8)   VALUE +0  COMP.
00165          16  PYAJ-RECORD-TYPE        PIC  X      VALUE SPACES.
00166
00167      12  ERPYAJ-RECORD-LENGTH        PIC S9(4) COMP VALUE +200.
00168      12  ERPYAJ-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +223.
00169
00170      12  ERCHKQ-KEY.
00171          16  CHKQ-COMPANY-CD         PIC  X      VALUE SPACE.
00172          16  CHKQ-CONTROL-NUMBER     PIC S9(8)   VALUE +0  COMP.
00173          16  CHKQ-SEQUENCE-NUMBER    PIC S9(4)   VALUE +0  COMP.
00174      12  ERCOMP-KEY.
00175          16  COMP-COMP-CD            PIC  X      VALUE SPACE.
00176          16  COMP-CARRIER            PIC  X      VALUE SPACES.
00177          16  COMP-GROUPING           PIC  X(6)   VALUE SPACES.
00178          16  COMP-FIN-RESP           PIC  X(10)  VALUE SPACES.
00179          16  COMP-ACCOUNT            PIC  X(10)  VALUE SPACES.
00180          16  COMP-RECORD-TYPE        PIC  X      VALUE SPACES.
CIDMOD*    12  COFA-KEY-X.
CIDMOD*        16  COFA-COMPANY-X          PIC  X(4)   VALUE SPACES.
CIDMOD*        16  COFA-ACCOUNT.
CIDMOD*            20  COFA-FILLER         PIC  X(11)  VALUE SPACES.
CIDMOD*            20  COFA-MSA-ACCT       PIC  X(07)  VALUE SPACES.
CIDMOD*
00181  EJECT
00182 *                                    COPY ELCDATE.
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
00183  EJECT
00184 *                                    COPY ELCLOGOF.
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
00185  EJECT
00186 *                                    COPY ELCATTR.
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
00187  EJECT
00188 *                                    COPY ELCEMIB.
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
00189  EJECT
00190 *                                    COPY ELCINTF.
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
00191      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00192          16  PI-PYAJ-FILE-SW             PIC  X.
00193              88  END-OF-ACCT                 VALUE 'A'.
00194              88  END-OF-ACCT-FULL-PAGE       VALUE 'B'.
00195              88  END-OF-FILE                 VALUE 'X'.
00196              88  TOP-OF-FILE                 VALUE 'T'.
00197              88  PAGE-FULL                   VALUE 'F'.
00198              88  NO-RECORDS                  VALUE 'Y'.
00199              88  NOT-OPEN                    VALUE 'Z'.
00200          16  PI-PREV-FUNCTION            PIC  X.
00201          16  PI-SAV-FUNCTION             PIC  X.
00202          16  PI-PREV-PFKEY               PIC  X.
00203          16  PI-SEQ-NOS.
00204              20  FILLER  OCCURS 13 TIMES
00205                              INDEXED BY NDX.
00206                  24  PI-REC-TYPE         PIC  X.
00207                  24  PI-FILE-SEQ-NO      PIC  S9(8)       COMP.
00208          16  PI-SAV-ENDING-PYAJ-KEY.
00209              20  PI-SAV-COMP-CD          PIC  X.
00210              20  PI-SAV-COMP-CONTROL.
00211                  24  PI-SAV-CARRIER      PIC  X.
00212                  24  PI-SAV-GROUPING     PIC  X(6).
00213                  24  PI-SAV-FIN-RESP     PIC  X(10).
00214                  24  PI-SAV-ACCOUNT      PIC  X(10).
00215                  24  PI-SAV-FILE-SEQ-NO  PIC  S9(8)          COMP.
00216                  24  PI-SAV-RECORD-TYPE  PIC  X.
00217          16  PI-START-PYAJ-KEY           PIC  X(33).
00218          16  PI-SAV-ACCT-AMT             PIC  S9(7)V99.
00219          16  PI-SAV-ACCT-NET             PIC  S9(7)V99.
00220          16  PI-SAV-PREV-AMT             PIC  S9(7)V99.
00221          16  PI-SAV-PREV-NET             PIC  S9(7)V99.
00222          16  PI-TOTAL-DISPLAYED-SW       PIC  X.
00223              88  PI-TOTAL-DISPLAYED               VALUE 'Y'.
00224          16  PI-FULL-INDX                PIC  S9(4)      COMP.
00225          16  PI-FRST-FILE-SEQ-NO         PIC  S9(8)          COMP.
00226          16  PI-FRST-RECORD-TYPE         PIC  X.
00227          16  PI-PREV-FILE-SEQ-NO         PIC  S9(8)          COMP.
00228          16  PI-PREV-RECORD-TYPE         PIC  X.
00229          16  PI-LAST-FILE-SEQ-NO         PIC  S9(8)          COMP.
00230          16  PI-LAST-RECORD-TYPE         PIC  X.
00231          16  PI-PAGE-SW                  PIC  X.
00232              88  FIRST-PAGE                          VALUE 'Y'.
00233          16  FILLER                      PIC  X(450).
00234  EJECT
00235 *                            COPY ELCJPFX.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCJPFX.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
00008 *                                                                *
00009 *     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
00010 *     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
00011 *        ELCNTL - CONTROL FILE                                   *
00012 *        ELMSTR - CLAIM MASTERS                                  *
00013 *        ELTRLR - ACTIVITY TRAILERS                              *
00014 *        ELCHKQ - CHECK QUE                                      *
00015 ******************************************************************
00016  01  JOURNAL-RECORD.
           12  jp-date                     pic s9(5) comp-3.
           12  jp-time                     pic s9(7) comp-3.
00017      12  JP-USER-ID                  PIC X(4).
00018      12  JP-FILE-ID                  PIC X(8).
00019      12  JP-PROGRAM-ID               PIC X(8).
00020      12  JP-RECORD-TYPE              PIC X.
00021          88 JP-ADD              VALUE 'A'.
00022          88 JP-BEFORE-CHANGE    VALUE 'B'.
00023          88 JP-AFTER-CHANGE     VALUE 'C'.
00024          88 JP-DELETE           VALUE 'D'.
00025          88 JP-GENERIC-DELETE   VALUE 'G'.
00026          88 JP-KEY-CHG-DELETE   VALUE 'K'.
00027          88 JP-KEY-CHG-ADD      VALUE 'N'.
00028      12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
00029      12  JP-RECORD-AREA
00030
00031
00236                              PIC  X(223).
00237  EJECT
00238 *                            COPY ELCAID.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  '�'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00239
00240  01  FILLER  REDEFINES  DFHAID.
00241      12  FILLER              PIC  X(8).
00242      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.
00243  EJECT
00244 *                            COPY EL633S.
       01  EL633AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  TIMEI PIC  X(0005).
      *    -------------------------------
           05  CMPNYIDL PIC S9(0004) COMP.
           05  CMPNYIDF PIC  X(0001).
           05  FILLER REDEFINES CMPNYIDF.
               10  CMPNYIDA PIC  X(0001).
           05  CMPNYIDI PIC  X(0003).
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
           05  FINRESPL PIC S9(0004) COMP.
           05  FINRESPF PIC  X(0001).
           05  FILLER REDEFINES FINRESPF.
               10  FINRESPA PIC  X(0001).
           05  FINRESPI PIC  X(0010).
      *    -------------------------------
           05  ACCTL PIC S9(0004) COMP.
           05  ACCTF PIC  X(0001).
           05  FILLER REDEFINES ACCTF.
               10  ACCTA PIC  X(0001).
           05  ACCTI PIC  X(0010).
      *    -------------------------------
           05  MSAAC1L PIC S9(0004) COMP.
           05  MSAAC1F PIC  X(0001).
           05  FILLER REDEFINES MSAAC1F.
               10  MSAAC1A PIC  X(0001).
           05  MSAAC1I PIC  X(0010).
      *    -------------------------------
           05  MSAST1L PIC S9(0004) COMP.
           05  MSAST1F PIC  X(0001).
           05  FILLER REDEFINES MSAST1F.
               10  MSAST1A PIC  X(0001).
           05  MSAST1I PIC  X(0002).
      *    -------------------------------
           05  MSACN1L PIC S9(0004) COMP.
           05  MSACN1F PIC  X(0001).
           05  FILLER REDEFINES MSACN1F.
               10  MSACN1A PIC  X(0001).
           05  MSACN1I PIC  X(0001).
      *    -------------------------------
           05  MSACM1L PIC S9(0004) COMP.
           05  MSACM1F PIC  X(0001).
           05  FILLER REDEFINES MSACM1F.
               10  MSACM1A PIC  X(0001).
           05  MSACM1I PIC  X(0010).
      *    -------------------------------
           05  TYPE1L PIC S9(0004) COMP.
           05  TYPE1F PIC  X(0001).
           05  FILLER REDEFINES TYPE1F.
               10  TYPE1A PIC  X(0001).
           05  TYPE1I PIC  X(0001).
      *    -------------------------------
           05  AMT1L PIC S9(0004) COMP.
           05  AMT1F PIC  X(0001).
           05  FILLER REDEFINES AMT1F.
               10  AMT1A PIC  X(0001).
           05  AMT1I PIC  X(0011).
      *    -------------------------------
           05  VOID1L PIC S9(0004) COMP.
           05  VOID1F PIC  X(0001).
           05  FILLER REDEFINES VOID1F.
               10  VOID1A PIC  X(0001).
           05  VOID1I PIC  X(0001).
      *    -------------------------------
           05  MDT1L PIC S9(0004) COMP.
           05  MDT1F PIC  X(0001).
           05  FILLER REDEFINES MDT1F.
               10  MDT1A PIC  X(0001).
           05  MDT1I PIC  X(0006).
      *    -------------------------------
           05  BDT1L PIC S9(0004) COMP.
           05  BDT1F PIC  X(0001).
           05  FILLER REDEFINES BDT1F.
               10  BDT1A PIC  X(0001).
           05  BDT1I PIC  X(0006).
      *    -------------------------------
           05  SDT1L PIC S9(0004) COMP.
           05  SDT1F PIC  X(0001).
           05  FILLER REDEFINES SDT1F.
               10  SDT1A PIC  X(0001).
           05  SDT1I PIC  X(0006).
      *    -------------------------------
           05  MSAAC2L PIC S9(0004) COMP.
           05  MSAAC2F PIC  X(0001).
           05  FILLER REDEFINES MSAAC2F.
               10  MSAAC2A PIC  X(0001).
           05  MSAAC2I PIC  X(0010).
      *    -------------------------------
           05  MSAST2L PIC S9(0004) COMP.
           05  MSAST2F PIC  X(0001).
           05  FILLER REDEFINES MSAST2F.
               10  MSAST2A PIC  X(0001).
           05  MSAST2I PIC  X(0002).
      *    -------------------------------
           05  MSACN2L PIC S9(0004) COMP.
           05  MSACN2F PIC  X(0001).
           05  FILLER REDEFINES MSACN2F.
               10  MSACN2A PIC  X(0001).
           05  MSACN2I PIC  X(0001).
      *    -------------------------------
           05  MSACM2L PIC S9(0004) COMP.
           05  MSACM2F PIC  X(0001).
           05  FILLER REDEFINES MSACM2F.
               10  MSACM2A PIC  X(0001).
           05  MSACM2I PIC  X(0010).
      *    -------------------------------
           05  TYPE2L PIC S9(0004) COMP.
           05  TYPE2F PIC  X(0001).
           05  FILLER REDEFINES TYPE2F.
               10  TYPE2A PIC  X(0001).
           05  TYPE2I PIC  X(0001).
      *    -------------------------------
           05  AMT2L PIC S9(0004) COMP.
           05  AMT2F PIC  X(0001).
           05  FILLER REDEFINES AMT2F.
               10  AMT2A PIC  X(0001).
           05  AMT2I PIC  X(0011).
      *    -------------------------------
           05  VOID2L PIC S9(0004) COMP.
           05  VOID2F PIC  X(0001).
           05  FILLER REDEFINES VOID2F.
               10  VOID2A PIC  X(0001).
           05  VOID2I PIC  X(0001).
      *    -------------------------------
           05  MDT2L PIC S9(0004) COMP.
           05  MDT2F PIC  X(0001).
           05  FILLER REDEFINES MDT2F.
               10  MDT2A PIC  X(0001).
           05  MDT2I PIC  X(0006).
      *    -------------------------------
           05  BDT2L PIC S9(0004) COMP.
           05  BDT2F PIC  X(0001).
           05  FILLER REDEFINES BDT2F.
               10  BDT2A PIC  X(0001).
           05  BDT2I PIC  X(0006).
      *    -------------------------------
           05  SDT2L PIC S9(0004) COMP.
           05  SDT2F PIC  X(0001).
           05  FILLER REDEFINES SDT2F.
               10  SDT2A PIC  X(0001).
           05  SDT2I PIC  X(0006).
      *    -------------------------------
           05  MSAAC3L PIC S9(0004) COMP.
           05  MSAAC3F PIC  X(0001).
           05  FILLER REDEFINES MSAAC3F.
               10  MSAAC3A PIC  X(0001).
           05  MSAAC3I PIC  X(0010).
      *    -------------------------------
           05  MSAST3L PIC S9(0004) COMP.
           05  MSAST3F PIC  X(0001).
           05  FILLER REDEFINES MSAST3F.
               10  MSAST3A PIC  X(0001).
           05  MSAST3I PIC  X(0002).
      *    -------------------------------
           05  MSACN3L PIC S9(0004) COMP.
           05  MSACN3F PIC  X(0001).
           05  FILLER REDEFINES MSACN3F.
               10  MSACN3A PIC  X(0001).
           05  MSACN3I PIC  X(0001).
      *    -------------------------------
           05  MSACM3L PIC S9(0004) COMP.
           05  MSACM3F PIC  X(0001).
           05  FILLER REDEFINES MSACM3F.
               10  MSACM3A PIC  X(0001).
           05  MSACM3I PIC  X(0010).
      *    -------------------------------
           05  TYPE3L PIC S9(0004) COMP.
           05  TYPE3F PIC  X(0001).
           05  FILLER REDEFINES TYPE3F.
               10  TYPE3A PIC  X(0001).
           05  TYPE3I PIC  X(0001).
      *    -------------------------------
           05  AMT3L PIC S9(0004) COMP.
           05  AMT3F PIC  X(0001).
           05  FILLER REDEFINES AMT3F.
               10  AMT3A PIC  X(0001).
           05  AMT3I PIC  X(0011).
      *    -------------------------------
           05  VOID3L PIC S9(0004) COMP.
           05  VOID3F PIC  X(0001).
           05  FILLER REDEFINES VOID3F.
               10  VOID3A PIC  X(0001).
           05  VOID3I PIC  X(0001).
      *    -------------------------------
           05  MDT3L PIC S9(0004) COMP.
           05  MDT3F PIC  X(0001).
           05  FILLER REDEFINES MDT3F.
               10  MDT3A PIC  X(0001).
           05  MDT3I PIC  X(0006).
      *    -------------------------------
           05  BDT3L PIC S9(0004) COMP.
           05  BDT3F PIC  X(0001).
           05  FILLER REDEFINES BDT3F.
               10  BDT3A PIC  X(0001).
           05  BDT3I PIC  X(0006).
      *    -------------------------------
           05  SDT3L PIC S9(0004) COMP.
           05  SDT3F PIC  X(0001).
           05  FILLER REDEFINES SDT3F.
               10  SDT3A PIC  X(0001).
           05  SDT3I PIC  X(0006).
      *    -------------------------------
           05  MSAAC4L PIC S9(0004) COMP.
           05  MSAAC4F PIC  X(0001).
           05  FILLER REDEFINES MSAAC4F.
               10  MSAAC4A PIC  X(0001).
           05  MSAAC4I PIC  X(0010).
      *    -------------------------------
           05  MSAST4L PIC S9(0004) COMP.
           05  MSAST4F PIC  X(0001).
           05  FILLER REDEFINES MSAST4F.
               10  MSAST4A PIC  X(0001).
           05  MSAST4I PIC  X(0002).
      *    -------------------------------
           05  MSACN4L PIC S9(0004) COMP.
           05  MSACN4F PIC  X(0001).
           05  FILLER REDEFINES MSACN4F.
               10  MSACN4A PIC  X(0001).
           05  MSACN4I PIC  X(0001).
      *    -------------------------------
           05  MSACM4L PIC S9(0004) COMP.
           05  MSACM4F PIC  X(0001).
           05  FILLER REDEFINES MSACM4F.
               10  MSACM4A PIC  X(0001).
           05  MSACM4I PIC  X(0010).
      *    -------------------------------
           05  TYPE4L PIC S9(0004) COMP.
           05  TYPE4F PIC  X(0001).
           05  FILLER REDEFINES TYPE4F.
               10  TYPE4A PIC  X(0001).
           05  TYPE4I PIC  X(0001).
      *    -------------------------------
           05  AMT4L PIC S9(0004) COMP.
           05  AMT4F PIC  X(0001).
           05  FILLER REDEFINES AMT4F.
               10  AMT4A PIC  X(0001).
           05  AMT4I PIC  X(0011).
      *    -------------------------------
           05  VOID4L PIC S9(0004) COMP.
           05  VOID4F PIC  X(0001).
           05  FILLER REDEFINES VOID4F.
               10  VOID4A PIC  X(0001).
           05  VOID4I PIC  X(0001).
      *    -------------------------------
           05  MDT4L PIC S9(0004) COMP.
           05  MDT4F PIC  X(0001).
           05  FILLER REDEFINES MDT4F.
               10  MDT4A PIC  X(0001).
           05  MDT4I PIC  X(0006).
      *    -------------------------------
           05  BDT4L PIC S9(0004) COMP.
           05  BDT4F PIC  X(0001).
           05  FILLER REDEFINES BDT4F.
               10  BDT4A PIC  X(0001).
           05  BDT4I PIC  X(0006).
      *    -------------------------------
           05  SDT4L PIC S9(0004) COMP.
           05  SDT4F PIC  X(0001).
           05  FILLER REDEFINES SDT4F.
               10  SDT4A PIC  X(0001).
           05  SDT4I PIC  X(0006).
      *    -------------------------------
           05  MSAAC5L PIC S9(0004) COMP.
           05  MSAAC5F PIC  X(0001).
           05  FILLER REDEFINES MSAAC5F.
               10  MSAAC5A PIC  X(0001).
           05  MSAAC5I PIC  X(0010).
      *    -------------------------------
           05  MSAST5L PIC S9(0004) COMP.
           05  MSAST5F PIC  X(0001).
           05  FILLER REDEFINES MSAST5F.
               10  MSAST5A PIC  X(0001).
           05  MSAST5I PIC  X(0002).
      *    -------------------------------
           05  MSACN5L PIC S9(0004) COMP.
           05  MSACN5F PIC  X(0001).
           05  FILLER REDEFINES MSACN5F.
               10  MSACN5A PIC  X(0001).
           05  MSACN5I PIC  X(0001).
      *    -------------------------------
           05  MSACM5L PIC S9(0004) COMP.
           05  MSACM5F PIC  X(0001).
           05  FILLER REDEFINES MSACM5F.
               10  MSACM5A PIC  X(0001).
           05  MSACM5I PIC  X(0010).
      *    -------------------------------
           05  TYPE5L PIC S9(0004) COMP.
           05  TYPE5F PIC  X(0001).
           05  FILLER REDEFINES TYPE5F.
               10  TYPE5A PIC  X(0001).
           05  TYPE5I PIC  X(0001).
      *    -------------------------------
           05  AMT5L PIC S9(0004) COMP.
           05  AMT5F PIC  X(0001).
           05  FILLER REDEFINES AMT5F.
               10  AMT5A PIC  X(0001).
           05  AMT5I PIC  X(0011).
      *    -------------------------------
           05  VOID5L PIC S9(0004) COMP.
           05  VOID5F PIC  X(0001).
           05  FILLER REDEFINES VOID5F.
               10  VOID5A PIC  X(0001).
           05  VOID5I PIC  X(0001).
      *    -------------------------------
           05  MDT5L PIC S9(0004) COMP.
           05  MDT5F PIC  X(0001).
           05  FILLER REDEFINES MDT5F.
               10  MDT5A PIC  X(0001).
           05  MDT5I PIC  X(0006).
      *    -------------------------------
           05  BDT5L PIC S9(0004) COMP.
           05  BDT5F PIC  X(0001).
           05  FILLER REDEFINES BDT5F.
               10  BDT5A PIC  X(0001).
           05  BDT5I PIC  X(0006).
      *    -------------------------------
           05  SDT5L PIC S9(0004) COMP.
           05  SDT5F PIC  X(0001).
           05  FILLER REDEFINES SDT5F.
               10  SDT5A PIC  X(0001).
           05  SDT5I PIC  X(0006).
      *    -------------------------------
           05  MSAAC6L PIC S9(0004) COMP.
           05  MSAAC6F PIC  X(0001).
           05  FILLER REDEFINES MSAAC6F.
               10  MSAAC6A PIC  X(0001).
           05  MSAAC6I PIC  X(0010).
      *    -------------------------------
           05  MSAST6L PIC S9(0004) COMP.
           05  MSAST6F PIC  X(0001).
           05  FILLER REDEFINES MSAST6F.
               10  MSAST6A PIC  X(0001).
           05  MSAST6I PIC  X(0002).
      *    -------------------------------
           05  MSACN6L PIC S9(0004) COMP.
           05  MSACN6F PIC  X(0001).
           05  FILLER REDEFINES MSACN6F.
               10  MSACN6A PIC  X(0001).
           05  MSACN6I PIC  X(0001).
      *    -------------------------------
           05  MSACM6L PIC S9(0004) COMP.
           05  MSACM6F PIC  X(0001).
           05  FILLER REDEFINES MSACM6F.
               10  MSACM6A PIC  X(0001).
           05  MSACM6I PIC  X(0010).
      *    -------------------------------
           05  TYPE6L PIC S9(0004) COMP.
           05  TYPE6F PIC  X(0001).
           05  FILLER REDEFINES TYPE6F.
               10  TYPE6A PIC  X(0001).
           05  TYPE6I PIC  X(0001).
      *    -------------------------------
           05  AMT6L PIC S9(0004) COMP.
           05  AMT6F PIC  X(0001).
           05  FILLER REDEFINES AMT6F.
               10  AMT6A PIC  X(0001).
           05  AMT6I PIC  X(0011).
      *    -------------------------------
           05  VOID6L PIC S9(0004) COMP.
           05  VOID6F PIC  X(0001).
           05  FILLER REDEFINES VOID6F.
               10  VOID6A PIC  X(0001).
           05  VOID6I PIC  X(0001).
      *    -------------------------------
           05  MDT6L PIC S9(0004) COMP.
           05  MDT6F PIC  X(0001).
           05  FILLER REDEFINES MDT6F.
               10  MDT6A PIC  X(0001).
           05  MDT6I PIC  X(0006).
      *    -------------------------------
           05  BDT6L PIC S9(0004) COMP.
           05  BDT6F PIC  X(0001).
           05  FILLER REDEFINES BDT6F.
               10  BDT6A PIC  X(0001).
           05  BDT6I PIC  X(0006).
      *    -------------------------------
           05  SDT6L PIC S9(0004) COMP.
           05  SDT6F PIC  X(0001).
           05  FILLER REDEFINES SDT6F.
               10  SDT6A PIC  X(0001).
           05  SDT6I PIC  X(0006).
      *    -------------------------------
           05  MSAAC7L PIC S9(0004) COMP.
           05  MSAAC7F PIC  X(0001).
           05  FILLER REDEFINES MSAAC7F.
               10  MSAAC7A PIC  X(0001).
           05  MSAAC7I PIC  X(0010).
      *    -------------------------------
           05  MSAST7L PIC S9(0004) COMP.
           05  MSAST7F PIC  X(0001).
           05  FILLER REDEFINES MSAST7F.
               10  MSAST7A PIC  X(0001).
           05  MSAST7I PIC  X(0002).
      *    -------------------------------
           05  MSACN7L PIC S9(0004) COMP.
           05  MSACN7F PIC  X(0001).
           05  FILLER REDEFINES MSACN7F.
               10  MSACN7A PIC  X(0001).
           05  MSACN7I PIC  X(0001).
      *    -------------------------------
           05  MSACM7L PIC S9(0004) COMP.
           05  MSACM7F PIC  X(0001).
           05  FILLER REDEFINES MSACM7F.
               10  MSACM7A PIC  X(0001).
           05  MSACM7I PIC  X(0010).
      *    -------------------------------
           05  TYPE7L PIC S9(0004) COMP.
           05  TYPE7F PIC  X(0001).
           05  FILLER REDEFINES TYPE7F.
               10  TYPE7A PIC  X(0001).
           05  TYPE7I PIC  X(0001).
      *    -------------------------------
           05  AMT7L PIC S9(0004) COMP.
           05  AMT7F PIC  X(0001).
           05  FILLER REDEFINES AMT7F.
               10  AMT7A PIC  X(0001).
           05  AMT7I PIC  X(0011).
      *    -------------------------------
           05  VOID7L PIC S9(0004) COMP.
           05  VOID7F PIC  X(0001).
           05  FILLER REDEFINES VOID7F.
               10  VOID7A PIC  X(0001).
           05  VOID7I PIC  X(0001).
      *    -------------------------------
           05  MDT7L PIC S9(0004) COMP.
           05  MDT7F PIC  X(0001).
           05  FILLER REDEFINES MDT7F.
               10  MDT7A PIC  X(0001).
           05  MDT7I PIC  X(0006).
      *    -------------------------------
           05  BDT7L PIC S9(0004) COMP.
           05  BDT7F PIC  X(0001).
           05  FILLER REDEFINES BDT7F.
               10  BDT7A PIC  X(0001).
           05  BDT7I PIC  X(0006).
      *    -------------------------------
           05  SDT7L PIC S9(0004) COMP.
           05  SDT7F PIC  X(0001).
           05  FILLER REDEFINES SDT7F.
               10  SDT7A PIC  X(0001).
           05  SDT7I PIC  X(0006).
      *    -------------------------------
           05  MSAAC8L PIC S9(0004) COMP.
           05  MSAAC8F PIC  X(0001).
           05  FILLER REDEFINES MSAAC8F.
               10  MSAAC8A PIC  X(0001).
           05  MSAAC8I PIC  X(0010).
      *    -------------------------------
           05  MSAST8L PIC S9(0004) COMP.
           05  MSAST8F PIC  X(0001).
           05  FILLER REDEFINES MSAST8F.
               10  MSAST8A PIC  X(0001).
           05  MSAST8I PIC  X(0002).
      *    -------------------------------
           05  MSACN8L PIC S9(0004) COMP.
           05  MSACN8F PIC  X(0001).
           05  FILLER REDEFINES MSACN8F.
               10  MSACN8A PIC  X(0001).
           05  MSACN8I PIC  X(0001).
      *    -------------------------------
           05  MSACM8L PIC S9(0004) COMP.
           05  MSACM8F PIC  X(0001).
           05  FILLER REDEFINES MSACM8F.
               10  MSACM8A PIC  X(0001).
           05  MSACM8I PIC  X(0010).
      *    -------------------------------
           05  TYPE8L PIC S9(0004) COMP.
           05  TYPE8F PIC  X(0001).
           05  FILLER REDEFINES TYPE8F.
               10  TYPE8A PIC  X(0001).
           05  TYPE8I PIC  X(0001).
      *    -------------------------------
           05  AMT8L PIC S9(0004) COMP.
           05  AMT8F PIC  X(0001).
           05  FILLER REDEFINES AMT8F.
               10  AMT8A PIC  X(0001).
           05  AMT8I PIC  X(0011).
      *    -------------------------------
           05  VOID8L PIC S9(0004) COMP.
           05  VOID8F PIC  X(0001).
           05  FILLER REDEFINES VOID8F.
               10  VOID8A PIC  X(0001).
           05  VOID8I PIC  X(0001).
      *    -------------------------------
           05  MDT8L PIC S9(0004) COMP.
           05  MDT8F PIC  X(0001).
           05  FILLER REDEFINES MDT8F.
               10  MDT8A PIC  X(0001).
           05  MDT8I PIC  X(0006).
      *    -------------------------------
           05  BDT8L PIC S9(0004) COMP.
           05  BDT8F PIC  X(0001).
           05  FILLER REDEFINES BDT8F.
               10  BDT8A PIC  X(0001).
           05  BDT8I PIC  X(0006).
      *    -------------------------------
           05  SDT8L PIC S9(0004) COMP.
           05  SDT8F PIC  X(0001).
           05  FILLER REDEFINES SDT8F.
               10  SDT8A PIC  X(0001).
           05  SDT8I PIC  X(0006).
      *    -------------------------------
           05  MSAAC9L PIC S9(0004) COMP.
           05  MSAAC9F PIC  X(0001).
           05  FILLER REDEFINES MSAAC9F.
               10  MSAAC9A PIC  X(0001).
           05  MSAAC9I PIC  X(0010).
      *    -------------------------------
           05  MSAST9L PIC S9(0004) COMP.
           05  MSAST9F PIC  X(0001).
           05  FILLER REDEFINES MSAST9F.
               10  MSAST9A PIC  X(0001).
           05  MSAST9I PIC  X(0002).
      *    -------------------------------
           05  MSACN9L PIC S9(0004) COMP.
           05  MSACN9F PIC  X(0001).
           05  FILLER REDEFINES MSACN9F.
               10  MSACN9A PIC  X(0001).
           05  MSACN9I PIC  X(0001).
      *    -------------------------------
           05  MSACM9L PIC S9(0004) COMP.
           05  MSACM9F PIC  X(0001).
           05  FILLER REDEFINES MSACM9F.
               10  MSACM9A PIC  X(0001).
           05  MSACM9I PIC  X(0010).
      *    -------------------------------
           05  TYPE9L PIC S9(0004) COMP.
           05  TYPE9F PIC  X(0001).
           05  FILLER REDEFINES TYPE9F.
               10  TYPE9A PIC  X(0001).
           05  TYPE9I PIC  X(0001).
      *    -------------------------------
           05  AMT9L PIC S9(0004) COMP.
           05  AMT9F PIC  X(0001).
           05  FILLER REDEFINES AMT9F.
               10  AMT9A PIC  X(0001).
           05  AMT9I PIC  X(0011).
      *    -------------------------------
           05  VOID9L PIC S9(0004) COMP.
           05  VOID9F PIC  X(0001).
           05  FILLER REDEFINES VOID9F.
               10  VOID9A PIC  X(0001).
           05  VOID9I PIC  X(0001).
      *    -------------------------------
           05  MDT9L PIC S9(0004) COMP.
           05  MDT9F PIC  X(0001).
           05  FILLER REDEFINES MDT9F.
               10  MDT9A PIC  X(0001).
           05  MDT9I PIC  X(0006).
      *    -------------------------------
           05  BDT9L PIC S9(0004) COMP.
           05  BDT9F PIC  X(0001).
           05  FILLER REDEFINES BDT9F.
               10  BDT9A PIC  X(0001).
           05  BDT9I PIC  X(0006).
      *    -------------------------------
           05  SDT9L PIC S9(0004) COMP.
           05  SDT9F PIC  X(0001).
           05  FILLER REDEFINES SDT9F.
               10  SDT9A PIC  X(0001).
           05  SDT9I PIC  X(0006).
      *    -------------------------------
           05  MSAAC10L PIC S9(0004) COMP.
           05  MSAAC10F PIC  X(0001).
           05  FILLER REDEFINES MSAAC10F.
               10  MSAAC10A PIC  X(0001).
           05  MSAAC10I PIC  X(0010).
      *    -------------------------------
           05  MSAST10L PIC S9(0004) COMP.
           05  MSAST10F PIC  X(0001).
           05  FILLER REDEFINES MSAST10F.
               10  MSAST10A PIC  X(0001).
           05  MSAST10I PIC  X(0002).
      *    -------------------------------
           05  MSACN10L PIC S9(0004) COMP.
           05  MSACN10F PIC  X(0001).
           05  FILLER REDEFINES MSACN10F.
               10  MSACN10A PIC  X(0001).
           05  MSACN10I PIC  X(0001).
      *    -------------------------------
           05  MSACM10L PIC S9(0004) COMP.
           05  MSACM10F PIC  X(0001).
           05  FILLER REDEFINES MSACM10F.
               10  MSACM10A PIC  X(0001).
           05  MSACM10I PIC  X(0010).
      *    -------------------------------
           05  TYPE10L PIC S9(0004) COMP.
           05  TYPE10F PIC  X(0001).
           05  FILLER REDEFINES TYPE10F.
               10  TYPE10A PIC  X(0001).
           05  TYPE10I PIC  X(0001).
      *    -------------------------------
           05  AMT10L PIC S9(0004) COMP.
           05  AMT10F PIC  X(0001).
           05  FILLER REDEFINES AMT10F.
               10  AMT10A PIC  X(0001).
           05  AMT10I PIC  X(0011).
      *    -------------------------------
           05  VOID10L PIC S9(0004) COMP.
           05  VOID10F PIC  X(0001).
           05  FILLER REDEFINES VOID10F.
               10  VOID10A PIC  X(0001).
           05  VOID10I PIC  X(0001).
      *    -------------------------------
           05  MDT10L PIC S9(0004) COMP.
           05  MDT10F PIC  X(0001).
           05  FILLER REDEFINES MDT10F.
               10  MDT10A PIC  X(0001).
           05  MDT10I PIC  X(0006).
      *    -------------------------------
           05  BDT10L PIC S9(0004) COMP.
           05  BDT10F PIC  X(0001).
           05  FILLER REDEFINES BDT10F.
               10  BDT10A PIC  X(0001).
           05  BDT10I PIC  X(0006).
      *    -------------------------------
           05  SDT10L PIC S9(0004) COMP.
           05  SDT10F PIC  X(0001).
           05  FILLER REDEFINES SDT10F.
               10  SDT10A PIC  X(0001).
           05  SDT10I PIC  X(0006).
      *    -------------------------------
           05  MSAAC11L PIC S9(0004) COMP.
           05  MSAAC11F PIC  X(0001).
           05  FILLER REDEFINES MSAAC11F.
               10  MSAAC11A PIC  X(0001).
           05  MSAAC11I PIC  X(0010).
      *    -------------------------------
           05  MSAST11L PIC S9(0004) COMP.
           05  MSAST11F PIC  X(0001).
           05  FILLER REDEFINES MSAST11F.
               10  MSAST11A PIC  X(0001).
           05  MSAST11I PIC  X(0002).
      *    -------------------------------
           05  MSACN11L PIC S9(0004) COMP.
           05  MSACN11F PIC  X(0001).
           05  FILLER REDEFINES MSACN11F.
               10  MSACN11A PIC  X(0001).
           05  MSACN11I PIC  X(0001).
      *    -------------------------------
           05  MSACM11L PIC S9(0004) COMP.
           05  MSACM11F PIC  X(0001).
           05  FILLER REDEFINES MSACM11F.
               10  MSACM11A PIC  X(0001).
           05  MSACM11I PIC  X(0010).
      *    -------------------------------
           05  TYPE11L PIC S9(0004) COMP.
           05  TYPE11F PIC  X(0001).
           05  FILLER REDEFINES TYPE11F.
               10  TYPE11A PIC  X(0001).
           05  TYPE11I PIC  X(0001).
      *    -------------------------------
           05  AMT11L PIC S9(0004) COMP.
           05  AMT11F PIC  X(0001).
           05  FILLER REDEFINES AMT11F.
               10  AMT11A PIC  X(0001).
           05  AMT11I PIC  X(0011).
      *    -------------------------------
           05  VOID11L PIC S9(0004) COMP.
           05  VOID11F PIC  X(0001).
           05  FILLER REDEFINES VOID11F.
               10  VOID11A PIC  X(0001).
           05  VOID11I PIC  X(0001).
      *    -------------------------------
           05  MDT11L PIC S9(0004) COMP.
           05  MDT11F PIC  X(0001).
           05  FILLER REDEFINES MDT11F.
               10  MDT11A PIC  X(0001).
           05  MDT11I PIC  X(0006).
      *    -------------------------------
           05  BDT11L PIC S9(0004) COMP.
           05  BDT11F PIC  X(0001).
           05  FILLER REDEFINES BDT11F.
               10  BDT11A PIC  X(0001).
           05  BDT11I PIC  X(0006).
      *    -------------------------------
           05  SDT11L PIC S9(0004) COMP.
           05  SDT11F PIC  X(0001).
           05  FILLER REDEFINES SDT11F.
               10  SDT11A PIC  X(0001).
           05  SDT11I PIC  X(0006).
      *    -------------------------------
           05  MSAAC12L PIC S9(0004) COMP.
           05  MSAAC12F PIC  X(0001).
           05  FILLER REDEFINES MSAAC12F.
               10  MSAAC12A PIC  X(0001).
           05  MSAAC12I PIC  X(0010).
      *    -------------------------------
           05  MSAST12L PIC S9(0004) COMP.
           05  MSAST12F PIC  X(0001).
           05  FILLER REDEFINES MSAST12F.
               10  MSAST12A PIC  X(0001).
           05  MSAST12I PIC  X(0002).
      *    -------------------------------
           05  MSACN12L PIC S9(0004) COMP.
           05  MSACN12F PIC  X(0001).
           05  FILLER REDEFINES MSACN12F.
               10  MSACN12A PIC  X(0001).
           05  MSACN12I PIC  X(0001).
      *    -------------------------------
           05  MSACM12L PIC S9(0004) COMP.
           05  MSACM12F PIC  X(0001).
           05  FILLER REDEFINES MSACM12F.
               10  MSACM12A PIC  X(0001).
           05  MSACM12I PIC  X(0010).
      *    -------------------------------
           05  TYPE12L PIC S9(0004) COMP.
           05  TYPE12F PIC  X(0001).
           05  FILLER REDEFINES TYPE12F.
               10  TYPE12A PIC  X(0001).
           05  TYPE12I PIC  X(0001).
      *    -------------------------------
           05  AMT12L PIC S9(0004) COMP.
           05  AMT12F PIC  X(0001).
           05  FILLER REDEFINES AMT12F.
               10  AMT12A PIC  X(0001).
           05  AMT12I PIC  X(0011).
      *    -------------------------------
           05  VOID12L PIC S9(0004) COMP.
           05  VOID12F PIC  X(0001).
           05  FILLER REDEFINES VOID12F.
               10  VOID12A PIC  X(0001).
           05  VOID12I PIC  X(0001).
      *    -------------------------------
           05  MDT12L PIC S9(0004) COMP.
           05  MDT12F PIC  X(0001).
           05  FILLER REDEFINES MDT12F.
               10  MDT12A PIC  X(0001).
           05  MDT12I PIC  X(0006).
      *    -------------------------------
           05  BDT12L PIC S9(0004) COMP.
           05  BDT12F PIC  X(0001).
           05  FILLER REDEFINES BDT12F.
               10  BDT12A PIC  X(0001).
           05  BDT12I PIC  X(0006).
      *    -------------------------------
           05  SDT12L PIC S9(0004) COMP.
           05  SDT12F PIC  X(0001).
           05  FILLER REDEFINES SDT12F.
               10  SDT12A PIC  X(0001).
           05  SDT12I PIC  X(0006).
      *    -------------------------------
           05  MSAAC13L PIC S9(0004) COMP.
           05  MSAAC13F PIC  X(0001).
           05  FILLER REDEFINES MSAAC13F.
               10  MSAAC13A PIC  X(0001).
           05  MSAAC13I PIC  X(0010).
      *    -------------------------------
           05  MSAST13L PIC S9(0004) COMP.
           05  MSAST13F PIC  X(0001).
           05  FILLER REDEFINES MSAST13F.
               10  MSAST13A PIC  X(0001).
           05  MSAST13I PIC  X(0002).
      *    -------------------------------
           05  MSACN13L PIC S9(0004) COMP.
           05  MSACN13F PIC  X(0001).
           05  FILLER REDEFINES MSACN13F.
               10  MSACN13A PIC  X(0001).
           05  MSACN13I PIC  X(0001).
      *    -------------------------------
           05  MSACM13L PIC S9(0004) COMP.
           05  MSACM13F PIC  X(0001).
           05  FILLER REDEFINES MSACM13F.
               10  MSACM13A PIC  X(0001).
           05  MSACM13I PIC  X(0010).
      *    -------------------------------
           05  TYPE13L PIC S9(0004) COMP.
           05  TYPE13F PIC  X(0001).
           05  FILLER REDEFINES TYPE13F.
               10  TYPE13A PIC  X(0001).
           05  TYPE13I PIC  X(0001).
      *    -------------------------------
           05  AMT13L PIC S9(0004) COMP.
           05  AMT13F PIC  X(0001).
           05  FILLER REDEFINES AMT13F.
               10  AMT13A PIC  X(0001).
           05  AMT13I PIC  X(0011).
      *    -------------------------------
           05  VOID13L PIC S9(0004) COMP.
           05  VOID13F PIC  X(0001).
           05  FILLER REDEFINES VOID13F.
               10  VOID13A PIC  X(0001).
           05  VOID13I PIC  X(0001).
      *    -------------------------------
           05  MDT13L PIC S9(0004) COMP.
           05  MDT13F PIC  X(0001).
           05  FILLER REDEFINES MDT13F.
               10  MDT13A PIC  X(0001).
           05  MDT13I PIC  X(0006).
      *    -------------------------------
           05  BDT13L PIC S9(0004) COMP.
           05  BDT13F PIC  X(0001).
           05  FILLER REDEFINES BDT13F.
               10  BDT13A PIC  X(0001).
           05  BDT13I PIC  X(0006).
      *    -------------------------------
           05  SDT13L PIC S9(0004) COMP.
           05  SDT13F PIC  X(0001).
           05  FILLER REDEFINES SDT13F.
               10  SDT13A PIC  X(0001).
           05  SDT13I PIC  X(0006).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0079).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
       01  EL633AO REDEFINES EL633AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRESPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT1O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT2O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT3O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT4O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT5O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC6O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM6O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT6O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC7O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM7O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT7O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC8O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM8O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT8O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC9O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST9O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM9O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT9O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT10O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT10O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT10O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT10O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT11O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT11O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT11O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT11O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT12O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT12O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT12O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT12O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT13O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT13O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT13O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT13O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
00245
00246  01  MAP-EL633A  REDEFINES  EL633AI.
101101     12  FILLER                  PIC  X(87).
00248      12  DATA-AREA       OCCURS 13 TIMES
00249                              INDEXED BY PINDX.
CIDMOD         16  GL-ACCT-LEN         PIC S9(4)              COMP.
CIDMOD         16  GL-ACCT-ATTRB       PIC  X.
CIDMOD         16  GL-ACCT             PIC  X(10).
CIDMOD         16  WSL-COMM  REDEFINES GL-ACCT.
00254              20  WSL-COMM-DTE.
00255                  24  WSL-MO      PIC  X(2).
00256                  24  WSL-DA      PIC  X(2).
00257                  24  WSL-YR      PIC  X(2).
CIDMOD             20  WSL-COMM-REST   PIC  X(4).
CIDMOD         16  GL-STATE-LEN        PIC S9(4)              COMP.
CIDMOD         16  GL-STATE-ATTRB      PIC  X.
CIDMOD         16  GL-STATE            PIC  X(02).
CIDMOD         16  GL-CANC-LEN         PIC S9(4)              COMP.
CIDMOD         16  GL-CANC-ATTRB       PIC  X.
CIDMOD         16  GL-CANC             PIC  X.
CIDMOD         16  GL-COMM-LEN         PIC S9(4)              COMP.
CIDMOD         16  GL-COMM-ATTRB       PIC  X.
CIDMOD         16  GL-COMM             PIC  X(10).
00259          16  RTYPE-LEN           PIC S9(4)              COMP.
00260          16  RTYPE-ATTRB         PIC  X.
00261          16  RTYPE               PIC  X.
00262          16  AMT-LEN             PIC S9(4)              COMP.
00263          16  AMT-ATTRB           PIC  X.
00264          16  AMT                 PIC S9(9)V9(2).
00265          16  AMTO  REDEFINES
00266              AMT                 PIC Z(7).9(2)-.
00267          16  VOID-SW-LEN         PIC S9(4)              COMP.
00268          16  VOID-SW-ATTRB       PIC  X.
00269          16  VOID-SW             PIC  X.
00270          16  MDTE-LEN            PIC S9(4)              COMP.
00271          16  MDTE-ATTRB          PIC  X.
CIDMOD         16  MDTE                PIC  X(6).
00273          16  BDTE-LEN            PIC S9(4)              COMP.
00274          16  BDTE-ATTRB          PIC  X.
CIDMOD         16  BDTE                PIC  X(6).
00276          16  SDTE-LEN            PIC S9(4)              COMP.
00277          16  SDTE-ATTRB          PIC  X.
CIDMOD         16  SDTE                PIC  9(6).
00279  EJECT
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
00281  01  DFHCOMMAREA             PIC  X(1024).
CIDMOD/
CIDMOD*                            COPY ERCCHKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCHKQ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE FOR THE CREDIT SYSTEM      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 100  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERCHKQ                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH  = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CHECK-QUE.
00019      12  CQ-RECORD-ID                PIC XX.
00020          88  VALID-CQ-ID                     VALUE 'CQ'.
00021
00022      12  CQ-CONTROL-PRIMARY.
00023          16  CQ-COMPANY-CD           PIC X.
00024          16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00025          16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00026
00027      12  CQ-ENTRY-TYPE               PIC X.
00028              88  CHECK-ON-QUE           VALUE 'Q'.
00029              88  ALIGNMENT-CHECK        VALUE 'A'.
00030              88  MANUAL-CHECK           VALUE 'M'.
00031              88  SPOILED-CHECK          VALUE 'S'.
00032              88  VOIDED-CHECK           VALUE 'V'.
00033              88  PAYMENT-ABORTED        VALUE 'X'.
00034
00035      12  CQ-CREDIT-MASTER-CNTL       PIC X(50).
00036
00037      12  CQ-CREDIT-PYAJ-CNTL         REDEFINES
00038          CQ-CREDIT-MASTER-CNTL.
00039          16  CQ-PYAJ-CARRIER         PIC X.
00040          16  CQ-PYAJ-GROUPING        PIC X(6).
00041          16  CQ-PYAJ-FIN-RESP        PIC X(10).
00042          16  CQ-PYAJ-ACCOUNT         PIC X(10).
00043          16  CQ-PYAJ-SEQ             PIC S9(8)  COMP.
00044          16  CQ-PYAJ-REC-TYPE        PIC X.
00045          16  FILLER                  PIC X(18).
00046
00047      12  CQ-CREDIT-CHEK-CNTL         REDEFINES
00048          CQ-CREDIT-MASTER-CNTL.
00049          16  CQ-CHEK-CARRIER         PIC X.
00050          16  CQ-CHEK-GROUPING        PIC X(6).
00051          16  CQ-CHEK-STATE           PIC XX.
00052          16  CQ-CHEK-ACCOUNT         PIC X(10).
00053          16  CQ-CHEK-CERT-EFF-DT     PIC XX.
00054          16  CQ-CHEK-CERT-NO.
00055              20  CQ-CHEK-CERT-PRIME  PIC X(10).
00056              20  CQ-CHEK-CERT-SFX    PIC X.
00057          16  CQ-CHEK-SEQ-NO          PIC S9(4)       COMP.
00058          16  CQ-CHEK-FIN-RESP        PIC X(10).
00059          16  FILLER                  PIC X(06).
00060
00061      12  CQ-CHECK-NUMBER             PIC X(7).
00062      12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00063      12  CQ-PAYMENT-TYPE             PIC X.
00064              88  CQ-BILLING-CREDIT         VALUE '1'.
00065              88  CQ-REFUND-PMT             VALUE '2'.
00066              88  CQ-CHECK-MAINT-PMT        VALUE '3'.
00067      12  CQ-VOID-INDICATOR           PIC X.
00068              88  CHECK-IS-VOID             VALUE 'V'.
00069      12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
00070      12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00071      12  CQ-CHECK-BY-USER            PIC X(4).
00072      12  CQ-PRE-NUMBERING-SW         PIC X.
00073        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00074        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00075
00076      12  CQ-CHECK-WRITTEN-DT         PIC XX.
00077      12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
00078      12  CQ-ACCOUNT-AGENT            PIC X(10).
00079      12  CQ-CHECK-VOIDED-DT          PIC XX.
00080
00081      12  CQ-LETTERS-IND              PIC X.
00082          88  CQ-LETTERS-REQUIRED           VALUE 'Y'.
00083
00084 ******************************************************************
CIDMOD/
00284 *                            COPY ERCPYAJ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCPYAJ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.015                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING PAYMENT AND ADJUSTMENTS           *
00008 *                                                                *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 200  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERPYAJ                         RKP=2,LEN=33   *
00014 *       ALTERNATE PATHS = NONE                                   *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
042303******************************************************************
042303*                   C H A N G E   L O G
042303*
042303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
042303*-----------------------------------------------------------------
042303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
042303* EFFECTIVE    NUMBER
042303*-----------------------------------------------------------------
042303* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
060205* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
042303******************************************************************
00019
00020  01  PENDING-PAY-ADJ.
00021      12  PY-RECORD-ID                     PIC XX.
00022          88  VALID-PY-ID                        VALUE 'PY'.
00023
00024      12  PY-CONTROL-PRIMARY.
00025          16  PY-COMPANY-CD                PIC X.
00026          16  PY-CARRIER                   PIC X.
00027          16  PY-GROUPING                  PIC X(6).
00028          16  PY-FIN-RESP                  PIC X(10).
00029          16  PY-ACCOUNT                   PIC X(10).
00030          16  PY-PRODUCER REDEFINES PY-ACCOUNT
00031                                           PIC X(10).
00032          16  PY-FILE-SEQ-NO               PIC S9(8)     COMP.
00033          16  PY-RECORD-TYPE               PIC X.
00034              88  PY-REMIT-RECEIVED            VALUE 'R'.
00035              88  PY-DEPOSIT                   VALUE 'D'.
00036              88  PY-CHARGE-TO-AGENT           VALUE 'C'.
00037              88  PY-ADJ-REM-RECEIVED          VALUE 'S'.
00038              88  PY-ADJ-DEPOSIT               VALUE 'T'.
00039              88  PY-ADJ-CHG-TO-AGT            VALUE 'U'.
00040              88  PY-ADD-TO-YTD-COMP           VALUE 'X'.
00041              88  PY-SUBTRACT-YTD-COMP         VALUE 'Y'.
00042              88  PY-ADD-TO-BALANCE            VALUE 'Z'.
00043              88  PY-FICA-ENTRY                VALUE 'F'.
00044              88  PY-REMIT-IND-GROUPING        VALUE 'G'.
00045              88  PY-POLICY-FEE                VALUE 'W'.
042303             88  PY-DUE-PREM-ADJ              VALUE 'P'.
00046
00047      12  PY-PYMT-TYPE                     PIC X.
00048              88  PY-NEW-BUS-PYMT              VALUE 'B'.
00049              88  PY-REINS-PYMT                VALUE 'R'.
00050              88  PY-EXP-PYMT                  VALUE 'E'.
00051
00052      12  PY-BIL-INV                       PIC X(6).
00053      12  PY-REF-NO                        PIC X(12).
00054
00055      12  PY-LAST-MAINT-DT                 PIC XX.
00056      12  PY-LAST-MAINT-BY                 PIC X(4).
00057      12  PY-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00058
00059      12  PY-PYADJ-RECORD.
00060          16  PY-ENTRY-AMT                 PIC S9(7)V99  COMP-3.
00061          16  PY-ENTRY-COMMENT             PIC X(30).
CIDMOD         16  PY-GL-DATA      REDEFINES PY-ENTRY-COMMENT.
CIDMOD             20  PY-GL-ACCOUNT            PIC X(10).
CIDMOD             20  PY-GL-STATE              PIC X(02).
CIDMOD             20  PY-GL-CANC-SW            PIC X(01).
CIDMOD                 88  PY-GL-CANC-SW-ON     VALUE 'Y'.
CIDMOD                 88  PY-GL-CANC-SW-OFF    VALUE 'N'.
CIDMOD             20  PY-GL-COMMENT            PIC X(10).
CIDMOD             20  FILLER      REDEFINES PY-GL-COMMENT.
CIDMOD                 24  PY-GL-CHECK-NO       PIC 9(06).
CIDMOD                 24  FILLER               PIC X(04).
CIDMOD             20  FILLER                   PIC X(07).
00074          16  PY-SAVE-ACCOUNT              PIC X(10).
00075          16  PY-SAVE-TYPE                 PIC X(01).
00076
00077          16  PY-LETTERS.
00078              20  PY-LETTER OCCURS 3 TIMES
00079                            INDEXED BY PY-LET-NDX
00080                                           PIC X(04).
00081
060205         16  PY-ERCOMP-TYPE               PIC X.
060205             88  PY-ACCOUNT-TYPE              VALUE 'A'.
060205             88  PY-GA-TYPE                   VALUE 'G'.
060205             88  PY-BANK-TYPE                 VALUE 'B'.
060205         16  FILLER                       PIC X(05).
00083
00084      12  PY-RECORD-STATUS.
00085          16  PY-CREDIT-SELECT-DT          PIC XX.
00086          16  PY-CREDIT-ACCEPT-DT          PIC XX.
00087          16  PY-BILLED-DATE               PIC XX.
00088          16  PY-REPORTED-DT               PIC XX.
00089          16  PY-PMT-APPLIED               PIC X.
00090              88  PY-ACCOUNT-PMT               VALUE 'A'.
00091              88  PY-GA-PMT                    VALUE 'G'.
00092              88  PY-OVWRITE-PMT               VALUE 'O'.
00093              88  PY-NON-AR-PMT                VALUE 'N'.
00094          16  FILLER                       PIC X(5).
00095          16  PY-INPUT-DT                  PIC XX.
00096          16  PY-CHECK-NUMBER              PIC X(6).
00097          16  PY-VOID-SW                   PIC X.
00098              88  PY-CHECK-VOIDED              VALUE 'V'.
00099          16  PY-CHECK-ORIGIN-SW           PIC X.
00100              88  PY-BILLING-CHECK             VALUE 'B'.
00101              88  PY-REFUND-CHECK              VALUE 'R'.
00102              88  PY-GA-CHECK                  VALUE 'G'.
00103              88  PY-CHECK-WRITTEN             VALUE 'W'.
00104              88  PY-CHECK-REVERSAL            VALUE 'V'.
00105          16  PY-CHECK-WRITTEN-DT          PIC XX.
00106          16  PY-CHECK-QUE-CONTROL         PIC S9(8) COMP.
00107          16  PY-CHECK-QUE-SEQUENCE        PIC S9(4) COMP.
00108          16  PY-BILL-FLAG                 PIC X.
00109              88  PY-BILLED                    VALUE 'B'.
00110          16  PY-AR-FLAG                   PIC X.
00111              88  PY-AR-CYCLE                  VALUE 'C'.
00112              88  PY-AR-MONTH-END              VALUE 'M'.
00113          16  PY-AR-DATE                   PIC XX.
00114
00115      12  PY-GL-CODES.
00116          16  PY-GL-DB                     PIC X(14).
00117          16  PY-GL-CR                     PIC X(14).
00118          16  PY-GL-FLAG                   PIC X.
00119          16  PY-GL-DATE                   PIC XX.
00120
00121      12  PY-CANCEL-FEE-FLAG               PIC X(2).
00122      12  FILLER                           PIC X(3).
00123 ******************************************************************
CIDMOD/
00288 *                            COPY ERCCOMP.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMP                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.019                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER                       *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 700   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
041105* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
092205* 092205    2005050300006  PEMA  ADD LEASE FEE
032406* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
072406* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
071712* 071712  CR2012042700005  PEMA  ADD OVER 120 FOR AHL ONLY
100703******************************************************************
00021
00022  01  COMPENSATION-MASTER.
00023      12  CO-RECORD-ID                          PIC XX.
00024          88  VALID-CO-ID                          VALUE 'CO'.
00025
00026      12  CO-CONTROL-PRIMARY.
00027          16  CO-COMPANY-CD                     PIC X.
00028          16  CO-CONTROL.
00029              20  CO-CTL-1.
00030                  24  CO-CARR-GROUP.
00031                      28  CO-CARRIER            PIC X.
00032                      28  CO-GROUPING.
00033                          32  CO-GROUP-PREFIX   PIC XXX.
00034                          32  CO-GROUP-PRIME    PIC XXX.
00035                  24  CO-RESP-NO.
00036                      28  CO-RESP-PREFIX        PIC X(4).
00037                      28  CO-RESP-PRIME         PIC X(6).
00038              20  CO-CTL-2.
00039                  24  CO-ACCOUNT.
00040                      28  CO-ACCT-PREFIX        PIC X(4).
00041                      28  CO-ACCT-PRIME         PIC X(6).
00042          16  CO-TYPE                           PIC X.
00043              88  CO-COMPANY-TYPE                  VALUE 'C'.
041105             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
00045              88  CO-ACCOUNT-TYPE                  VALUE 'A'.
00046
00047      12  CO-MAINT-INFORMATION.
00048          16  CO-LAST-MAINT-DT                  PIC XX.
00049          16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  CO-LAST-MAINT-USER                PIC X(4).
011410     12  FILLER                                PIC XX.
020210     12  CO-STMT-TYPE                          PIC XXX.
011410     12  CO-COMP-TYPE                          PIC X.
011410         88  CO-COMP-IS-SPPDD                    VALUE '1'.
           12  CO-STMT-OWNER                         PIC X(4).
00053      12  CO-BALANCE-CONTROL                    PIC X.
00054          88  CO-CARRY-BALANCE                     VALUE 'Y'.
00055          88  CO-NO-BALANCE                        VALUE 'N'.
00056
00057      12  CO-INTERNAL-CONTROL-1                 PIC X.
00058          88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
00059          88  CO-AUTO-GENERATED                    VALUE 'Y'.
00060          88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
00061
00062      12  CO-INTERNAL-CONTROL-2                 PIC X.
00063          88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
00064          88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
00065
062907     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
062907     12  CO-GA-DIRECT-DEP                      PIC X.
062907     12  CO-FUTURE-SPACE                       PIC X.
062907         88  CO-FUTURE-NOT-USED                   VALUE ' '.
00068
00069      12  CO-ACCT-NAME                          PIC X(30).
00070      12  CO-MAIL-NAME                          PIC X(30).
00071      12  CO-ADDR-1                             PIC X(30).
00072      12  CO-ADDR-2                             PIC X(30).
CIDMOD     12  CO-ADDR-3.
               16  CO-ADDR-CITY                      PIC X(27).
               16  CO-ADDR-STATE                     PIC XX.
CIDMOD     12  CO-CSO-1099                           PIC X.
00074      12  CO-ZIP.
00075          16  CO-ZIP-PRIME.
00076              20  CO-ZIP-PRI-1ST                PIC X.
00077                  88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  CO-ZIP-PLUS4                      PIC X(4).
00080      12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
00081          16  CO-CAN-POSTAL-1                   PIC XXX.
00082          16  CO-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
00084      12  CO-SOC-SEC                            PIC X(13).
00085      12  CO-TELEPHONE.
00086          16  CO-AREA-CODE                      PIC XXX.
00087          16  CO-PREFIX                         PIC XXX.
00088          16  CO-PHONE                          PIC X(4).
00089
00090      12  CO-ROLADEX-PRINT-DT                   PIC XX.
00091
00092      12  CO-AR-BAL-LEVEL                       PIC X.
00093          88  CO-AR-REF-LVL                        VALUE '1'.
00094          88  CO-AR-BILL-REF-LVL                   VALUE '1'.
00095          88  CO-AR-BILL-LVL                       VALUE '2'.
00096          88  CO-AR-AGT-LVL                        VALUE '3'.
00097          88  CO-AR-FR-LVL                         VALUE '4'.
00098
00099      12  CO-AR-NORMAL-PRINT                    PIC X.
00100          88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
00101          88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
00102
00103      12  CO-AR-SUMMARY-CODE                    PIC X(6).
00104
00105      12  CO-AR-REPORTING                       PIC X.
00106          88  CO-AR-NET-REPORT                     VALUE 'N'.
00107          88  CO-AR-GROSS-REPORT                   VALUE 'G'.
00108
00109      12  CO-AR-PULL-CHECK                      PIC X.
00110          88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
00111          88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
00112
00113      12  CO-AR-BALANCE-PRINT                   PIC X.
00114          88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
00115
00116      12  CO-AR-LAST-RUN-CODE                   PIC X.
00117          88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
00118          88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
00119          88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
00120
00121      12  CO-LAST-EOM-STMT-DT                   PIC XX.
00122
00123      12  CO-USER-CODE                          PIC X.
00124      12  CO-REPORT-GROUP-ID                    PIC X(12).
00125
00126 ******************************************************************
00127 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
00128 *    THE LAST MONTH END RUN.
00129 ******************************************************************
00130
00131      12  CO-LAST-ACTIVITY-DATE.
00132          16  CO-ACT-YEAR                       PIC 99.
00133          16  CO-ACT-MONTH                      PIC 99.
00134          16  CO-ACT-DAY                        PIC 99.
00135
00136      12  CO-LAST-STMT-DT.
00137          16  CO-LAST-STMT-YEAR                 PIC 99.
00138          16  CO-LAST-STMT-MONTH                PIC 99.
00139          16  CO-LAST-STMT-DAY                  PIC 99.
00140
00141      12  CO-MO-END-TOTALS.
00142          16  CO-MONTHLY-TOTALS.
00143              20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
00144              20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
00145              20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
00146              20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
00147              20  CO-END-BAL                PIC S9(7)V99   COMP-3.
00148
00149          16  CO-AGING-TOTALS.
00150              20  CO-CUR                    PIC S9(7)V99   COMP-3.
00151              20  CO-OV30                   PIC S9(7)V99   COMP-3.
00152              20  CO-OV60                   PIC S9(7)V99   COMP-3.
00153              20  CO-OV90                   PIC S9(7)V99   COMP-3.
00154
00155          16  CO-YTD-TOTALS.
00156              20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
00157              20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
00158
00159          16  CO-OVER-UNDER-TOTALS.
00160              20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
00161              20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
00162
00163      12  CO-MISCELLANEOUS-TOTALS.
00164          16  CO-FICA-TOTALS.
00165              20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
00166              20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
00167
00168          16  CO-CLAIM-TOTALS.
00169              20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
00170              20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
00171
00172 ******************************************************************
00173 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
00174 *    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
00175 ******************************************************************
00176
00177      12  CO-CURRENT-TOTALS.
00178          16  CO-CURRENT-LAST-STMT-DT.
00179              20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
00180              20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
00181              20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
00182
00183          16  CO-CURRENT-MONTHLY-TOTALS.
00184              20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
00185              20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
00186              20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
00187              20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
00188              20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
00189
00190          16  CO-CURRENT-AGING-TOTALS.
00191              20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
00192              20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
00193              20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
00194              20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
00195
00196          16  CO-CURRENT-YTD-TOTALS.
00197              20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
00198              20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
00199
00200      12  CO-PAID-COMM-TOTALS.
00201          16  CO-YTD-PAID-COMMS.
00202              20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
00203              20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
00204
00205      12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
00206          88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
00207          88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
00208
00209      12  CO-DELINQUENT-LETTER-CODE         PIC X.
00210          88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
00211          88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
00212          88  CO-AGENT-1ST-LETTER              VALUE 'B'.
00213          88  CO-AGENT-2ND-LETTER              VALUE 'G'.
00214          88  CO-OVERWRITE-LETTER              VALUE 'O'.
00215          88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
00216          88  CO-FINAL-LETTER                  VALUE 'F'.
00217          88  CO-RECONCILING                   VALUE 'R'.
00218          88  CO-PHONE-CALL                    VALUE 'P'.
00219          88  CO-LEGAL                         VALUE 'L'.
00220          88  CO-COLLECTION-AGENCY             VALUE 'C'.
00221          88  CO-WRITE-OFF                     VALUE 'W'.
00222          88  CO-NO-ACTION                     VALUE 'N' ' '.
00223
00224      12  CO-CSR-CODE                       PIC X(4).
00225
00226      12  CO-GA-STATUS-INFO.
00227          16  CO-GA-EFFECTIVE-DT            PIC XX.
00228          16  CO-GA-TERMINATION-DT          PIC XX.
00229          16  CO-GA-STATUS-CODE             PIC X.
00230              88  CO-GA-ACTIVE                 VALUE 'A'.
00231              88  CO-GA-INACTIVE               VALUE 'I'.
00232              88  CO-GA-PENDING                VALUE 'P'.
00233          16  CO-GA-COMMENTS.
00234              20  CO-GA-COMMENT-1           PIC X(40).
00235              20  CO-GA-COMMENT-2           PIC X(40).
00236              20  CO-GA-COMMENT-3           PIC X(40).
00237              20  CO-GA-COMMENT-4           PIC X(40).
00238
00239      12  CO-RPTCD2                         PIC X(10).
071712     12  CO-AHL-OVER120-DATA REDEFINES CO-RPTCD2.
071712         16  CO-OV120                      PIC S9(7)V99   COMP-3.
071712         16  CO-CURRENT-OV120              PIC S9(7)V99   COMP-3.
00240
00241      12  CO-TYPE-AGENT                     PIC X(01).
00242          88  CO-CORPORATION                   VALUE 'C'.
00243          88  CO-PARTNERSHIP                   VALUE 'P'.
00244          88  CO-SOLE-PROPRIETOR               VALUE 'S'.
00245          88  CO-TRUST                         VALUE 'T'.
00246          88  CO-UNKNOWN                       VALUE ' ' 'X'.
00247
00248      12  CO-FAXNO.
00249          16  CO-FAX-AREA-CODE                  PIC XXX.
00250          16  CO-FAX-PREFIX                     PIC XXX.
00251          16  CO-FAX-PHONE                      PIC X(4).
00252
00253      12  CO-BANK-INFORMATION.
00254          16  CO-BANK-TRANSIT-NO                PIC X(8).
00255          16  CO-BANK-TRANSIT-NON REDEFINES
00256              CO-BANK-TRANSIT-NO                PIC 9(8).
00257
00258          16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
           12  CO-MISC-DEDUCT-INFO REDEFINES
                        CO-BANK-INFORMATION.
               16  CO-MD-GL-ACCT                     PIC X(10).
               16  CO-MD-DIV                         PIC XX.
               16  CO-MD-CENTER                      PIC X(4).
               16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
092707         16  CO-CREATE-AP-CHECK                PIC X.
092707         16  CO-DELIVER-CK-TO-MEL              PIC X.
092707         16  FILLER                            PIC XXX.
00259      12  CO-ACH-STATUS                         PIC X.
00260          88  CO-ACH-ACTIVE                         VALUE 'A'.
00261          88  CO-ACH-PENDING                        VALUE 'P'.
00262
CIDMOD     12  CO-BILL-SW                            PIC X.
CIDMOD     12  CO-CONTROL-NAME                       PIC X(30).
092205     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
111504     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
100703     12  CO-CLP-STATE                          PIC XX.
032406     12  CO-FIRST-WRITTEN-DT                   PIC XX.
072406     12  CO-SPP-REFUND-EDIT                    PIC X.
00264
00265 ******************************************************************
CIDMOD*
CIDMOD*                            COPY AIRL0009.
CIDMOD/
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CHECK-QUE
                                PENDING-PAY-ADJ COMPENSATION-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL633' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00291
00292      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00293      MOVE EIBTRMID               TO  QID-TERM.
00294      MOVE 2                      TO  EMI-NUMBER-OF-LINES.
00295
00296      IF EIBCALEN = ZERO
00297          GO TO 8800-UNAUTHORIZED-ACCESS.
00298
00299      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00300          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM.
00301
00302      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00303      MOVE '5'                    TO  DC-OPTION-CODE.
00304
00305      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
00306
00307      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.
00308      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.
00309
00310      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00311          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00312              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00313              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00314              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00315              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00316              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00317              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00318              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00319              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00320          ELSE
00321              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00322              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00323              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00324              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00325              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00326              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00327              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00328              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00329
00330      MOVE LOW-VALUES             TO  EL633AI.
00331
00332      MOVE DC-JULIAN-YYDDD        TO  WORK-DATE-JULIAN.
00333      COMPUTE WORK-SEQ-NO = WORK-JULIAN-DD * 100000.
00334      MOVE EIBTIME                TO  WORK-SEQ-TIME.
00335      ADD WORK-SEQ-HHMMS TO WORK-SEQ-NO.
00336
00337      MOVE ZEROS                  TO  TOTAL-ACCT-AMT
00338                                      TOTAL-ACCT-NET.
00339
00340      IF RETURNED-FROM = XCTL-6331 OR
00341                         XCTL-640  OR
00342                         XCTL-642  OR
00343                         XCTL-652
00344          PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0690-EXIT.
00345
00346      IF EIBTRNID NOT = TRANS-ID
00347          MOVE 'T'                TO  PI-PYAJ-FILE-SW
00348          MOVE 'Y'                TO  PI-PAGE-SW
00349          MOVE ZEROS              TO  PI-SEQ-NOS
00350          MOVE ZEROS              TO  PI-SAV-ACCT-AMT
00351                                      PI-SAV-ACCT-NET
00352                                      PI-SAV-PREV-AMT
00353                                      PI-SAV-PREV-NET
00354          IF (EIBTRNID NOT = EL640-TRANS-ID  AND
00355              EIBTRNID NOT = EL642-TRANS-ID  AND
CIDMOD             EIBTRNID NOT = EL652-TRANS-ID) OR
CIDMOD             PI-CR-FIN-RESP = SPACES
00358              GO TO 8100-SEND-INITIAL-MAP
00359          ELSE
00360              IF EIBTRNID = (EL6331-TRANS-ID  OR
00361                             EL640-TRANS-ID   OR
00362                             EL642-TRANS-ID)  AND
00363                 PI-CR-CONTROL-IN-PROGRESS  = SPACES
00364                 GO TO 8100-SEND-INITIAL-MAP
00365              ELSE
00366                  MOVE DFHENTER   TO  EIBAID
00367                  MOVE 'S'        TO  MAINTI
00368                  MOVE PI-CR-CARRIER
00369                                  TO  CARRIERI
00370                  MOVE PI-CR-GROUPING
00371                                  TO  GROUPI
00372                  MOVE PI-CR-FIN-RESP
00373                                  TO  FINRESPI
00374                  MOVE PI-CR-ACCOUNT
00375                                  TO  ACCTI
00376                  MOVE 1          TO  CARRIERL  MAINTA
00377                  MOVE 3          TO  GROUPL
00378                  MOVE 6          TO  FINRESPL  ACCTL
00379                  GO TO 0400-VALIDATE-KEY-DATA.
00380
00381      
      * EXEC CICS HANDLE CONDITION
00382 *        PGMIDERR  (9600-PGMID-ERROR)
00383 *        ERROR     (9990-ABEND)
00384 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00003211' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033323131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00385
00386      IF EIBAID = DFHCLEAR
00387          GO TO 9400-CLEAR.
00388
00389      IF PI-PROCESSOR-ID = 'LGXX'
00390          GO TO 0200-RECEIVE.
00391
00392      
      * EXEC CICS READQ TS
00393 *        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
00394 *        INTO   (SECURITY-CONTROL)
00395 *        LENGTH (SC-COMM-LENGTH)
00396 *        ITEM   (SC-ITEM)
00397 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00003222' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00398
00399      MOVE SC-CREDIT-DISPLAY (15)  TO PI-DISPLAY-CAP.
00400      MOVE SC-CREDIT-UPDATE  (15)  TO PI-MODIFY-CAP.
00401
00402      IF NOT DISPLAY-CAP
00403          MOVE 'READ'          TO SM-READ
00404          PERFORM 9995-SECURITY-VIOLATION
00405          MOVE ER-0070         TO  EMI-ERROR
00406          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00407          GO TO 8100-SEND-INITIAL-MAP.
00408
00409  EJECT
00410  0200-RECEIVE.
00411
00412      IF EIBAID = DFHPA1  OR  DFHPA2  OR  DFHPA3
00413          MOVE ER-0008            TO  EMI-ERROR
00414          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00415          MOVE -1                 TO  MAINTL
00416          GO TO 8200-SEND-DATAONLY.
00417
00418      
      * EXEC CICS RECEIVE
00419 *        MAP     (MAP-NAME)
00420 *        MAPSET  (MAPSET-NAME)
00421 *        INTO    (EL633AI)
00422 *    END-EXEC.
           MOVE LENGTH OF
            EL633AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003248' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL633AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00423
00424      IF MAINTI = 'S' AND
00425         EIBAID = DFHENTER
00426          MOVE ZEROS              TO  PI-SAV-ACCT-AMT
00427                                      PI-SAV-ACCT-NET
00428                                      PI-SAV-PREV-AMT
00429                                      PI-SAV-PREV-NET
00430                                      PI-FRST-FILE-SEQ-NO
00431          MOVE SPACES             TO  PI-FRST-RECORD-TYPE.
00432
00433      IF PFENTERL = ZERO
00434          GO TO 0300-CHECK-PFKEYS.
00435
00436      IF (PFENTERI  IS NUMERIC)
00437        AND (PFENTERI GREATER 0 AND LESS 25)
00438          MOVE PF-VALUES (PFENTERI)  TO  EIBAID
00439      ELSE
00440          MOVE ER-0029               TO  EMI-ERROR
00441          GO TO 0320-INPUT-ERROR.
00442
00443  0300-CHECK-PFKEYS.
00444      IF EIBAID = DFHPF23
00445          GO TO 8810-PF23.
00446
00447      IF EIBAID = DFHPF24
00448          GO TO 9200-RETURN-MAIN-MENU.
00449
00450      IF EIBAID = DFHPF12
00451          GO TO 9500-PF12.
00452
00453      IF EIBAID = DFHPF3
00454          IF PI-CR-CONTROL-IN-PROGRESS NOT =      SPACES
00455              PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
00456              MOVE XCTL-652       TO  PGM-NAME
CIDMOD             IF ERCOMP-KEY  NOT =      SPACES
CIDMOD                 MOVE COMP-CARRIER   TO  PI-CR-CARRIER
CIDMOD                 MOVE COMP-GROUPING  TO  PI-CR-GROUPING
CIDMOD                 MOVE COMP-FIN-RESP  TO  PI-CR-FIN-RESP
CIDMOD                 MOVE COMP-ACCOUNT   TO  PI-CR-ACCOUNT
CIDMOD             END-IF
CIDMOD
00457              IF PI-CR-ACCOUNT = LOW-VALUES
00458                  MOVE 'G'        TO  PI-CR-TYPE
00459                  GO TO 9300-XCTL
00460              ELSE
00461                  MOVE 'A'        TO  PI-CR-TYPE
00462                  GO TO 9300-XCTL
00463          ELSE
00464             MOVE ER-3020         TO  EMI-ERROR
00465             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00466             GO TO 8100-SEND-INITIAL-MAP.
00467
00468      IF EIBAID = DFHPF4
00469          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
00470          MOVE XCTL-6331          TO  PGM-NAME
00471          GO TO 9300-XCTL.
00472
00473      IF EIBAID = DFHPF5
00474          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
00475          MOVE SPACES TO PI-PROGRAM-WORK-AREA
00476          MOVE XCTL-640           TO  PGM-NAME
00477          IF PI-CR-ACCOUNT = LOW-VALUES
00478              MOVE SPACES     TO  PI-CR-CONTROL-IN-PROGRESS
CIDMOD             IF ACCTI = LOW-VALUES
CIDMOD                MOVE 'G'         TO  PI-CR-TYPE
CIDMOD             END-IF
00479              GO TO 9300-XCTL
00480          ELSE
00481              MOVE 'A'        TO  PI-CR-TYPE
00482              GO TO 9300-XCTL.
00483
00484      IF EIBAID = DFHPF6
00485         IF PI-GA-BILLING
00486             PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
00487             MOVE SPACES TO PI-PROGRAM-WORK-AREA
00488             MOVE XCTL-642        TO  PGM-NAME
00489             IF PI-CR-ACCOUNT = LOW-VALUES
00490                  MOVE 'G'        TO  PI-CR-TYPE
00491                  GO TO 9300-XCTL
00492              ELSE
00493                  MOVE SPACES     TO  PI-CR-CONTROL-IN-PROGRESS
00494                  GO TO 9300-XCTL
00495         ELSE
00496             MOVE ER-2929         TO  EMI-ERROR
00497             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00498             MOVE AL-UNBON        TO  PFENTERA
00499             IF PFENTERL = ZERO
00500                 MOVE -1          TO  MAINTL
00501                 GO TO 8200-SEND-DATAONLY
00502             ELSE
00503                 MOVE -1          TO  PFENTERL
00504                 GO TO 8200-SEND-DATAONLY.
00505
00506      IF EIBAID = DFHENTER  OR  DFHPF1  OR  DFHPF2
00507          GO TO 0400-VALIDATE-KEY-DATA.
00508
00509  0320-INPUT-ERROR.
00510      MOVE ER-0029                TO  EMI-ERROR.
00511
00512      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00513
00514      MOVE AL-UNBON               TO  PFENTERA.
00515
00516      IF PFENTERL = ZERO
00517          MOVE -1                 TO  MAINTL
00518      ELSE
00519          MOVE -1                 TO  PFENTERL.
00520
00521      GO TO 8200-SEND-DATAONLY.
00522  EJECT
00523  0400-VALIDATE-KEY-DATA.
00524      IF MODIFY-CAP  OR (EIBAID = DFHPF1 OR DFHPF2)
00525          NEXT SENTENCE
00526        ELSE
00527         IF MAINTI NOT = 'S'
00528          MOVE 'UPDATE'       TO SM-READ
00529          PERFORM 9995-SECURITY-VIOLATION
00530          MOVE ER-0070        TO EMI-ERROR
00531          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00532          GO TO 8100-SEND-INITIAL-MAP.
00533
00534      MOVE PI-COMPANY-CD          TO  PI-SAV-COMP-CD.
00535
00536      IF EIBAID = DFHPF1
00537          GO TO 4000-BROWSE-FRWD.
00538
00539      IF EIBAID   = DFHPF1   AND
00540         CARRIERL = ZEROS    AND
00541         GROUPL   = ZEROS    AND
00542         FINRESPL = ZEROS    AND
00543         ACCTL    = ZEROS
00544          GO TO 4000-BROWSE-FRWD.
00545
CIDMOD     IF PI-PROCESSOR-ID = 'LGXX'
CIDMOD         IF MAINTI = 'C'
CIDMOD             MOVE AL-UANON       TO MAINTA
CIDMOD             MOVE MAINTI         TO PI-SAV-FUNCTION
CIDMOD             GO TO CSO-BRANCH
CIDMOD         END-IF
CIDMOD     END-IF.
CIDMOD
CIDMOD     IF MAINTI = 'C' OR  'S'
00547          MOVE AL-UANON           TO  MAINTA
00548          MOVE MAINTI             TO  PI-SAV-FUNCTION
00549      ELSE
00550          MOVE -1                 TO  MAINTL
00551          MOVE ER-0023            TO  EMI-ERROR
00552          MOVE AL-UABON           TO  MAINTA
00553          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00554
CIDMOD CSO-BRANCH.
CIDMOD
00555      IF MAINTI = 'C'
00556        AND PI-PREV-FUNCTION NOT = 'S' AND 'C'
00557          MOVE -1                 TO  MAINTL
00558          MOVE ER-2056            TO  EMI-ERROR
00559          MOVE AL-UABON           TO  MAINTA
00560          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00561
00562      IF CARRIERL = ZEROS  AND
00563         GROUPL   = ZEROS  AND
00564         FINRESPL = ZEROS  AND
00565         ACCTL    = ZEROS
00566          MOVE -1                 TO  CARRIERL
00567          MOVE ER-2231            TO  EMI-ERROR
00568          MOVE AL-UABON           TO  CARRIERA
00569                                      GROUPA
00570                                      FINRESPA
00571                                      ACCTA
00572          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00573          GO TO 8200-SEND-DATAONLY.
00574
00575      IF CARRIERL NOT = ZEROS
00576          MOVE AL-UANON           TO  CARRIERA
00577          MOVE CARRIERI           TO  COMP-CARRIER
00578                                      PI-SAV-CARRIER
00579                                      PI-CR-CARRIER
00580          IF CARRIERI NOT = ZEROS
00581            AND (PI-ZERO-CARRIER  OR  PI-ZERO-CAR-GROUP)
00582              MOVE ER-2587        TO  EMI-ERROR
00583              MOVE -1             TO  CARRIERL
00584              MOVE AL-UABON       TO  CARRIERA
00585              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00586          ELSE
00587              NEXT SENTENCE
00588      ELSE
00589          MOVE LOW-VALUES          TO  COMP-CARRIER
00590                                       PI-SAV-CARRIER.
00591
00592      IF CARRIERL NOT = ZEROS
00593          IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
00594              IF PI-CARRIER-SECURITY = CARRIERI
00595                  NEXT SENTENCE
00596              ELSE
00597                  MOVE -1         TO  CARRIERL
00598                  MOVE ER-2370    TO  EMI-ERROR
00599                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00600
00601      IF GROUPL NOT = ZEROS
00602          MOVE AL-UANON           TO  GROUPA
00603          MOVE GROUPI             TO  COMP-GROUPING
00604                                      PI-SAV-GROUPING
00605                                      PI-CR-GROUPING
00606          IF GROUPI NOT = ZEROS
00607            AND (PI-ZERO-GROUPING  OR  PI-ZERO-CAR-GROUP)
00608              MOVE ER-2588        TO  EMI-ERROR
00609              MOVE -1             TO  GROUPL
00610              MOVE AL-UABON       TO  GROUPA
00611              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00612          ELSE
00613              NEXT SENTENCE
00614      ELSE
00615          MOVE LOW-VALUES         TO  COMP-GROUPING
00616                                      PI-SAV-GROUPING.
00617
00618      IF FINRESPL NOT = ZEROS
00619          MOVE AL-UANON           TO  FINRESPA
00620          MOVE FINRESPI           TO  COMP-FIN-RESP
00621                                      PI-SAV-FIN-RESP
00622                                      PI-CR-FIN-RESP
00623      ELSE
00624          MOVE LOW-VALUES         TO  COMP-FIN-RESP
00625                                      PI-SAV-FIN-RESP.
00626
00627      IF ACCTL NOT = ZEROS
00628          MOVE AL-UANON           TO  ACCTA
00629          MOVE ACCTI              TO  COMP-ACCOUNT
00630                                      PI-SAV-ACCOUNT
00631                                      PI-CR-ACCOUNT
00632      ELSE
00633          MOVE LOW-VALUES         TO  COMP-ACCOUNT
00634                                      PI-SAV-ACCOUNT
00635                                      PI-CR-ACCOUNT.
00636
00637      IF EMI-ERROR = ZEROS
00638          NEXT SENTENCE
00639      ELSE
00640          IF EIBTRNID = EL640-TRANS-ID OR
00641             EIBTRNID = EL642-TRANS-ID OR
00642             EIBTRNID = EL6331-TRANS-ID
00643              GO TO 8100-SEND-INITIAL-MAP
00644          ELSE
00645              GO TO 8200-SEND-DATAONLY.
00646
00647      MOVE MAINTI                 TO  PI-PREV-FUNCTION.
00648
00649      IF MAINTI = 'S'
00650          IF EIBAID = DFHPF1              OR  DFHENTER
00651              GO TO 4000-BROWSE-FRWD
00652          ELSE
00653              GO TO 4100-BROWSE-BKWD.
00654
00655      MOVE SPACES                 TO  COMP-RECORD-TYPE.
00656      MOVE PI-COMPANY-CD          TO  COMP-COMP-CD.
00657
00658      
      * EXEC CICS HANDLE CONDITION
00659 *        NOTFND   (0410-NO-COMP-MSTR)
00660 *        NOTOPEN  (7100-COMP-FILE-NOTOPEN)
00661 *        END-EXEC.
      *    MOVE '"$IJ                  ! # #00003508' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033353038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00662
00663      
      * EXEC CICS READ
00664 *        DATASET  (COMP-FILE-ID)
00665 *        SET      (ADDRESS OF COMPENSATION-MASTER)
00666 *        RIDFLD   (ERCOMP-KEY)
00667 *        GTEQ
00668 *        END-EXEC.
      *    MOVE '&"S        G          (   #00003513' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00669
00670      IF PI-COMPANY-CD = CO-COMPANY-CD  AND
00671         COMP-CARRIER  = CO-CARRIER     AND
00672         COMP-GROUPING = CO-GROUPING    AND
00673         COMP-FIN-RESP = CO-RESP-NO    AND
00674         COMP-ACCOUNT  = CO-ACCOUNT
00675          NEXT SENTENCE
00676      ELSE
00677          GO TO 0410-NO-COMP-MSTR.
00678
00679      IF PI-COMPANY-ID = 'NCL'
00680          IF CO-GA-INACTIVE
00681              GO TO 0420-INACTIVE-COMP.
00682
00683      IF MAINTI = 'C'
00684          GO TO 1000-EDIT-DATA.
00685
00686      IF EIBAID = DFHPF1
00687        OR DFHENTER
00688          GO TO 4000-BROWSE-FRWD.
00689
00690      IF EIBAID = DFHPF2
00691          GO TO 4100-BROWSE-BKWD.
00692
00693  0410-NO-COMP-MSTR.
00694      MOVE ER-2230                TO  EMI-ERROR.
00695      MOVE -1                     TO  CARRIERL.
00696      MOVE AL-UABON               TO  CARRIERA
00697                                      GROUPA
00698                                      FINRESPA
00699                                      ACCTA.
00700
00701      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00702
00703      GO TO 8200-SEND-DATAONLY.
00704
00705  0420-INACTIVE-COMP.
00706      MOVE ER-2763                TO  EMI-ERROR.
00707      MOVE -1                     TO  CARRIERL.
00708      MOVE AL-UABON               TO  CARRIERA
00709                                      GROUPA
00710                                      FINRESPA
00711                                      ACCTA.
00712
00713      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00714
00715      GO TO 8200-SEND-DATAONLY.
00716
00717      EJECT
00718  0500-CREATE-TEMP-STORAGE.
00719
00720      PERFORM 0800-DELETE-TS  THRU  0890-EXIT.
00721
00722      
      * EXEC CICS WRITEQ TS
00723 *        QUEUE  (QID)
00724 *        FROM   (PROGRAM-INTERFACE-BLOCK)
00725 *        LENGTH (PI-COMM-LENGTH)
00726 *    END-EXEC.
      *    MOVE '*"     L              ''   #00003572' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00727
00728  0590-EXIT.
00729       EXIT.
00730
00731  0600-RECOVER-TEMP-STORAGE.
00732      
      * EXEC CICS READQ TS
00733 *        QUEUE  (QID)
00734 *        INTO   (PROGRAM-INTERFACE-BLOCK)
00735 *        LENGTH (PI-COMM-LENGTH)
00736 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00003582' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00737
00738      PERFORM 0800-DELETE-TS THRU 0890-EXIT.
00739
00740  0690-EXIT.
00741       EXIT.
00742
00743  0800-DELETE-TS.
00744      
      * EXEC CICS HANDLE CONDITION
00745 *        QIDERR (0890-EXIT)
00746 *    END-EXEC.
      *    MOVE '"$N                   ! $ #00003594' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033353934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00747
00748      
      * EXEC CICS DELETEQ TS
00749 *        QUEUE  (QID)
00750 *    END-EXEC.
      *    MOVE '*&                    #   #00003598' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00751
00752      
      * EXEC CICS SYNCPOINT
00753 *    END-EXEC.
      *    MOVE '6"                    !   #00003602' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00754
00755  0890-EXIT.
00756       EXIT.
00757      EJECT
00758
00759  1000-EDIT-DATA.
00760      SET PINDX                   TO  1.
00761
           .
00762  1010-EDIT-LOOP.
00763      SET NDX                     TO  PINDX.
CIDMOD     IF GL-ACCT-LEN (PINDX)   = ZEROS  AND
CIDMOD        GL-STATE-LEN (PINDX) = ZEROS   AND
CIDMOD        GL-CANC-LEN (PINDX)   = ZEROS  AND
CIDMOD        GL-COMM-LEN (PINDX)   = ZEROS  AND
00765         RTYPE-LEN (PINDX)     = ZEROS  AND
00766         AMT-LEN (PINDX)       = ZEROS  AND
CIDMOD        VOID-SW-LEN (PINDX)   = ZEROS  AND
00768         SDTE-LEN (PINDX)      = ZEROS
00769          GO TO 1020-INCREMENT-PINDX.
00770
CIDMOD     IF GL-ACCT-LEN (PINDX) NOT = ZEROS
CIDMOD         MOVE AL-UANON           TO  GL-ACCT-ATTRB (PINDX)
00773          IF PI-COMPANY-ID NOT = 'WSL'
00774              NEXT SENTENCE
00775          ELSE
00776              MOVE WSL-COMM-DTE (PINDX)  TO  DC-GREG-DATE-1-MDY
00777              MOVE '4'                   TO  DC-OPTION-CODE
00778              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
00779              IF NO-CONVERSION-ERROR
00780                  NEXT SENTENCE
00781              ELSE
00782                  MOVE ER-2595    TO  EMI-ERROR
CIDMOD                 MOVE -1         TO  GL-ACCT-LEN (PINDX)
CIDMOD                 MOVE AL-UABON   TO  GL-ACCT-ATTRB (PINDX)
00785                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00786      ELSE
00787          IF PI-COMPANY-ID = 'WSL'
00788            AND PI-FILE-SEQ-NO (NDX) = ZEROS
00789              MOVE ER-2596        TO  EMI-ERROR
CIDMOD             MOVE -1             TO  GL-ACCT-LEN (PINDX)
CIDMOD             MOVE AL-UABON       TO  GL-ACCT-ATTRB (PINDX)
00792              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00793
CIDMOD
CIDMOD     IF GL-ACCT-LEN (PINDX) NOT = ZEROES
CIDMOD*        MOVE K-SPACE            TO COFA-FILLER
CIDMOD*        MOVE '0001'             TO COFA-COMPANY-X
CIDMOD*        MOVE MSA-ACCT (PINDX)   TO COFA-MSA-ACCT
CIDMOD         MOVE AL-UANON           TO GL-ACCT-ATTRB (PINDX)
CIDMOD     ELSE
CIDMOD         IF PI-FILE-SEQ-NO (NDX) = ZEROES
CIDMOD             MOVE -1             TO GL-ACCT-LEN (PINDX)
CIDMOD             MOVE ER-2957        TO EMI-ERROR
CIDMOD             MOVE AL-UABON       TO GL-ACCT-ATTRB (PINDX)
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU
CIDMOD                     9900-EXIT
CIDMOD             GO TO 1040-CONTINUE-EDIT
PEMMOD         ELSE
PEMMOD             GO TO 1040-CONTINUE-EDIT
CIDMOD         END-IF
CIDMOD     END-IF.
CIDMOD
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
CIDMOD*    EXEC CICS HANDLE CONDITION
CIDMOD*         NOTFND (1040-NO-COFA-MSTR)
CIDMOD*         NOTOPEN (7100-COFA-FILE-NOTOPEN)
CIDMOD*    END-EXEC.
CIDMOD*
CIDMOD*    EXEC CICS READ
CIDMOD*         DATASET (COFA-FILE-ID)
CIDMOD*         SET     (ADDRESS OF CHART-OF-ACCOUNTS)
CIDMOD*         RIDFLD  (COFA-KEY-X)
CIDMOD*    END-EXEC.
CIDMOD*
CIDMOD*    MOVE CHART-OF-ACCOUNTS      TO SV-COFA.
CIDMOD*    GO TO 1040-CONTINUE-EDIT.
CIDMOD*
CIDMOD*1040-NO-COFA-MSTR.
CIDMOD
CIDMOD     MOVE ER-2960                TO EMI-ERROR.
CIDMOD     MOVE -1                     TO GL-ACCT-LEN (PINDX).
CIDMOD     MOVE AL-UABON               TO GL-ACCT-ATTRB (PINDX).
CIDMOD     PERFORM 9900-ERROR-FORMAT THRU
CIDMOD             9900-EXIT.
CIDMOD
CIDMOD 1040-CONTINUE-EDIT.
CIDMOD
CIDMOD*    MOVE MSA-ACCT (PINDX)       TO WS-ACCT-BREAK.
CIDMOD     IF GL-STATE-LEN (PINDX) NOT = ZEROS
CIDMOD         MOVE GL-STATE (PINDX)   TO CHECK-STATE-CODE
CIDMOD         IF NOT VALID-STATE-CODE
CIDMOD             MOVE -1             TO GL-STATE-LEN (PINDX)
CIDMOD             MOVE ER-2957        TO EMI-ERROR
CIDMOD             MOVE AL-UABON       TO GL-STATE-ATTRB (PINDX)
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU
CIDMOD                     9900-EXIT
CIDMOD         ELSE
CIDMOD             MOVE AL-UANON       TO GL-STATE-ATTRB (PINDX)
CIDMOD         END-IF
CIDMOD     END-IF.
CIDMOD
CIDMOD 1040-CSO-SKIP.
CIDMOD
CIDMOD     IF GL-CANC (PINDX) = LOW-VALUES
CIDMOD         MOVE SPACES             TO GL-CANC (PINDX)
CIDMOD     END-IF.
CIDMOD
CIDMOD     IF GL-CANC-LEN (PINDX) NOT = ZEROS
CIDMOD         MOVE GL-CANC (PINDX)    TO CHECK-CANC-TYPE
CIDMOD         IF NOT VALID-CANC-TYPE
CIDMOD             MOVE -1             TO GL-CANC-LEN (PINDX)
CIDMOD             MOVE ER-2958        TO EMI-ERROR
CIDMOD             MOVE AL-UABON       TO GL-CANC-ATTRB (PINDX)
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU
CIDMOD                     9900-EXIT
CIDMOD         ELSE
CIDMOD             MOVE AL-UANON       TO GL-CANC-ATTRB (PINDX)
CIDMOD         END-IF
CIDMOD     END-IF.
CIDMOD
00794      IF RTYPE-LEN (PINDX) NOT = ZEROS
00795         MOVE RTYPE (PINDX)       TO  CHECK-REC-TYPE
021716        IF (PI-COMPANY-ID = 'DCC' or 'VPP')
042303           AND (RTYPE (PINDX) = 'P')
042303           SET VALID-REC-TYPE   TO TRUE
042303        END-IF
00796          IF NOT VALID-REC-TYPE
00797              MOVE -1             TO  RTYPE-LEN (PINDX)
00798              MOVE ER-2234        TO  EMI-ERROR
00799              MOVE AL-UABON       TO  RTYPE-ATTRB (PINDX)
00800              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00801          ELSE
00802              MOVE AL-UANON       TO  RTYPE-ATTRB (PINDX)
00803      ELSE
00804          IF PI-FILE-SEQ-NO (NDX) = ZEROS
00805              MOVE -1             TO  RTYPE-LEN (PINDX)
00806              MOVE ER-2235        TO  EMI-ERROR
00807              MOVE AL-UABON       TO  RTYPE-ATTRB (PINDX)
00808              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00809
00810      IF AMT-LEN (PINDX) NOT = ZEROS
00811         
      * EXEC CICS BIF DEEDIT
00812 *           FIELD (AMT (PINDX))
00813 *           LENGTH (11)
00814 *       END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003778' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AMT(PINDX), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00815          IF AMT (PINDX) = ZEROS
00816              IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS
00817                  NEXT SENTENCE
00818              ELSE
00819                  MOVE ER-2245    TO  EMI-ERROR
00820                  MOVE -1         TO  AMT-LEN(PINDX)
00821                  MOVE AL-UNBON   TO  AMT-ATTRB (PINDX)
00822                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00823          ELSE
00824              IF AMT (PINDX) NUMERIC
00825                  MOVE AMT (PINDX) TO  AMTO (PINDX)
00826              ELSE
00827                  MOVE ER-2245    TO  EMI-ERROR
00828                  MOVE -1         TO  AMT-LEN(PINDX)
00829                  MOVE AL-UNBON   TO  AMT-ATTRB (PINDX)
00830                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00831      ELSE
00832          IF PI-FILE-SEQ-NO (NDX) = ZEROS
00833              MOVE -1             TO  AMT-LEN (PINDX)
00834              MOVE ER-2236        TO  EMI-ERROR
00835              MOVE AL-UNBON       TO  AMT-ATTRB (PINDX)
00836              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00837
CIDMOD     IF VOID-SW-LEN (PINDX) NOT = ZEROS
CIDMOD         IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS
CIDMOD           AND (PI-REC-TYPE (NDX) = 'C')
CIDMOD             IF VOID-SW (PINDX) = 'V'
CIDMOD                 MOVE AL-UANON   TO  VOID-SW-ATTRB (PINDX)
CIDMOD             ELSE
CIDMOD                 MOVE ER-2246    TO  EMI-ERROR
CIDMOD                 MOVE -1         TO  VOID-SW-LEN (PINDX)
CIDMOD                 MOVE AL-UABON   TO  VOID-SW-ATTRB (PINDX)
CIDMOD                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
CIDMOD         ELSE
CIDMOD             MOVE ER-2449        TO  EMI-ERROR
CIDMOD             MOVE -1             TO  VOID-SW-LEN (PINDX)
CIDMOD             MOVE AL-UABON       TO  VOID-SW-ATTRB (PINDX)
CIDMOD             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
CIDMOD
00854      IF SDTE-LEN (PINDX) = ZEROS
00855         GO TO 1020-INCREMENT-PINDX.
00856
00857      MOVE AL-UNNON               TO  SDTE-ATTRB (PINDX).
00858
00859      MOVE SDTE (PINDX)           TO DEEDIT-FIELD.
00860      PERFORM 8600-DEEDIT.
00861
00862      IF DEEDIT-FIELD-V0 NOT NUMERIC
00863         GO TO 1015-DAY-ERROR.
00864
00865      MOVE DEEDIT-FIELD-V0        TO  DC-GREG-DATE-1-MDY.
00866      MOVE '4'                    TO  DC-OPTION-CODE.
00867      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
00868
00869      IF NO-CONVERSION-ERROR
00870         SET PINDEX               TO  PINDX
00871         MOVE DC-BIN-DATE-1       TO  WS-EOM-DT (PINDEX)
00872      ELSE
00873         GO TO 1015-DAY-ERROR.
00874
00875      MOVE DEEDIT-FIELD-V0        TO DATE-TEST-AREA.
00876
00877      IF DATE-TEST-MM = 4 OR  6  OR  9  OR  11
00878          IF DATE-TEST-DD  NOT = 30
00879              GO TO 1015-DAY-ERROR
00880          ELSE
00881              GO TO 1020-INCREMENT-PINDX.
00882
00883      IF DATE-TEST-MM = 1 OR  3  OR  5  OR  7  OR
00884                              8  OR  10  OR  12
00885          IF DATE-TEST-DD  NOT = 31
00886              GO TO 1015-DAY-ERROR
00887          ELSE
00888              GO TO 1020-INCREMENT-PINDX.
00889
00890      DIVIDE DATE-TEST-YY  BY  4  GIVING  DIVIDE-RESULT
00891          REMAINDER  DIVIDE-REMAINDER.
00892
00893      IF (DATE-TEST-YY = ZERO)
00894        OR (DIVIDE-REMAINDER NOT = ZERO)
00895          IF DATE-TEST-DD  NOT = 28
00896              GO TO 1015-DAY-ERROR
00897          ELSE
00898              GO TO 1020-INCREMENT-PINDX
00899      ELSE
00900          IF DATE-TEST-DD = 29
00901              GO TO 1020-INCREMENT-PINDX.
00902
00903  1015-DAY-ERROR.
00904      MOVE -1                     TO  SDTE-LEN (PINDX).
00905      MOVE AL-UNBON               TO  SDTE-ATTRB (PINDX).
00906      MOVE ER-0587                TO  EMI-ERROR.
00907
00908      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00909
00910  1020-INCREMENT-PINDX.
00911      SET PINDX  UP  BY  1.
00912
00913      IF PINDX  IS NOT GREATER THAN  13
00914          GO TO 1010-EDIT-LOOP.
00915
00916      IF EMI-ERROR = ZEROS
00917          NEXT SENTENCE
00918      ELSE
00919          MOVE 'S'                TO  PI-PREV-FUNCTION
00920          GO TO 8200-SEND-DATAONLY.
00921  EJECT
00922  2000-UPDATE-THE-FILE.
00923      SET PINDX                   TO  ZERO-NDX.
CIDMOD*    SET NDX                     TO  ZERO-NDX.
CIDMOD*    SET NDX  UP  BY  1.
00926
00927      IF PI-PYAJ-FILE-SW = 'B'
00928          MOVE WORK-SEQ-NO        TO  PI-FRST-FILE-SEQ-NO
00929          MOVE RTYPE (PINDX)      TO  PI-FRST-RECORD-TYPE
00930      ELSE
00931          MOVE PI-LAST-FILE-SEQ-NO
00932                                  TO  PI-SAV-FILE-SEQ-NO
00933          MOVE PI-LAST-RECORD-TYPE
00934                                  TO  PI-SAV-RECORD-TYPE.
00935
00936  2100-UPDATE-LOOP.
00937      SET PINDX  UP  BY  1.
00938      SET NDX                     TO  PINDX.
00939
00940      IF PINDX  IS GREATER THAN  13
00941          GO TO 2200-UPDATE-COMPLETE.
00942
CIDMOD     IF GL-ACCT-LEN (PINDX)   = ZEROS AND
CIDMOD        GL-STATE-LEN (PINDX) = ZEROS AND
CIDMOD        GL-CANC-LEN (PINDX)   = ZEROS AND
CIDMOD        GL-COMM-LEN (PINDX)   = ZEROS AND
CIDMOD        RTYPE-LEN (PINDX)   = ZEROS  AND
CIDMOD        AMT-LEN (PINDX)     = ZEROS  AND
CIDMOD        VOID-SW-LEN (PINDX) = ZEROS  AND
00947         SDTE-LEN (PINDX)    = ZEROS
00948          GO TO 2100-UPDATE-LOOP.
00949
00950      
      * EXEC CICS BIF DEEDIT
00951 *         FIELD (AMT (PINDX))
00952 *         LENGTH (11)
00953 *    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003920' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AMT(PINDX), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00954
00955      IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS
00956          NEXT SENTENCE
00957      ELSE
00958          GO TO 2110-ADD-RECORD.
00959
00960      
      * EXEC CICS HANDLE CONDITION
00961 *        NOTFND  (2110-ADD-RECORD)
00962 *    END-EXEC.
      *    MOVE '"$I                   ! % #00003930' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033393330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00963
00964      MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.
00965      MOVE PI-FILE-SEQ-NO (NDX)    TO  PYAJ-FILE-SEQ-NO.
00966      MOVE PI-REC-TYPE (NDX)       TO  PYAJ-RECORD-TYPE.
00967
00968      
      * EXEC CICS READ
00969 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
00970 *        DATASET  (PYAJ-FILE-ID)
00971 *        RIDFLD   (ERPYAJ-KEY)
00972 *        UPDATE
00973 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00003938' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00974
PEMMOD
00975      IF RTYPE-LEN (PINDX)  GREATER THAN +0
00976         IF RTYPE (PINDX) NOT = PY-RECORD-TYPE
PEMMOD            MOVE PENDING-PAY-ADJ TO WS-SAVE-ERPYAJ
00977             PERFORM 2190-CHANGE-RECORD-TYPE THRU 2190-EXIT
00978             MOVE RTYPE (PINDX)     TO  PI-FRST-RECORD-TYPE.
00979
00980      IF AMT-LEN (PINDX) NOT = ZEROS
00981          IF AMT (PINDX) = ZEROS
CIDMOD             IF PY-CHECK-WRITTEN-DT = LOW-VALUES
CIDMOD                 GO TO 2120-DELETE-RECORD
CIDMOD             ELSE
CIDMOD                 MOVE ER-2244       TO  EMI-ERROR
CIDMOD                 MOVE -1            TO  AMT-LEN (PINDX)
CIDMOD                 MOVE AL-UNBON      TO  AMT-ATTRB (PINDX)
CIDMOD                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
CIDMOD                 MOVE PY-ENTRY-AMT  TO  AMTO (PINDX)
CIDMOD                 
      * EXEC CICS UNLOCK
CIDMOD*                    DATASET  (PYAJ-FILE-ID)
CIDMOD*                END-EXEC
      *    MOVE '&*                    #   #00003962' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
CIDMOD                 GO TO 8200-SEND-DATAONLY.
00994
00995      MOVE 'B'                    TO  JP-RECORD-TYPE.
00996      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
00997      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
00998
00999      PERFORM 8400-LOG-JOURNAL-RECORD.
01000
01001      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.
01002      MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.
01003      MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT.
01004
CIDMOD     IF GL-ACCT-LEN (PINDX) NOT = ZEROS
CIDMOD         MOVE GL-ACCT (PINDX)    TO  PY-GL-ACCOUNT.
CIDMOD     IF GL-STATE-LEN (PINDX) NOT = ZEROS
CIDMOD         MOVE GL-STATE (PINDX)   TO  PY-GL-STATE.
CIDMOD     IF GL-CANC-LEN (PINDX) NOT = ZEROS
CIDMOD         MOVE GL-CANC (PINDX)    TO  PY-GL-CANC-SW.
CIDMOD     IF GL-COMM-LEN (PINDX) NOT = ZEROS
CIDMOD         MOVE GL-COMM (PINDX)    TO  PY-GL-COMMENT.
01008      IF AMT-LEN (PINDX) NOT = ZEROS
01009          MOVE AMT (PINDX)        TO  PY-ENTRY-AMT.
01010
CIDMOD     IF VOID-SW-LEN (PINDX) NOT = ZEROS
CIDMOD         MOVE VOID-SW (PINDX)    TO  PY-VOID-SW.
01013
01014      IF SDTE-LEN (PINDX) NOT = ZEROS
01015         SET PINDEX               TO  PINDX
01016         MOVE WS-EOM-DT (PINDEX)  TO  PY-CREDIT-SELECT-DT.
01017
01018      MOVE 'C'                    TO  JP-RECORD-TYPE.
01019      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
01020      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
01021
01022      
      * EXEC CICS REWRITE
01023 *        DATASET  (PYAJ-FILE-ID)
01024 *        FROM     (PENDING-PAY-ADJ)
01025 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003999' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01026
01027      PERFORM 8400-LOG-JOURNAL-RECORD.
01028
01029      GO TO 2100-UPDATE-LOOP.
01030  EJECT
01031  2110-ADD-RECORD.
01032      
      * EXEC CICS GETMAIN
01033 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
01034 *        LENGTH   (ERPYAJ-RECORD-LENGTH)
01035 *        INITIMG  (GETMAIN-SPACE)
01036 *    END-EXEC.
      *    MOVE ',"IL                  $   #00004009' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPYAJ-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01037
01038      MOVE 'PY'                   TO  PY-RECORD-ID.
01039      MOVE PI-COMPANY-CD          TO  PY-COMPANY-CD.
01040      MOVE PI-SAV-CARRIER         TO  PY-CARRIER.
01041      MOVE PI-SAV-GROUPING        TO  PY-GROUPING.
01042      MOVE PI-SAV-FIN-RESP        TO  PY-FIN-RESP.
01043      MOVE PI-SAV-ACCOUNT         TO  PY-ACCOUNT.
01044
01045      ADD +1                      TO  WORK-SEQ-NO.
01046      MOVE WORK-SEQ-NO            TO  PY-FILE-SEQ-NO.
01047
CIDMOD     MOVE GL-ACCT (PINDX)        TO  PY-GL-ACCOUNT.
021716     if gl-state-len (pindx) <> zeros
021716        MOVE GL-STATE (PINDX)    TO  PY-GL-STATE
021716     end-if
021716     if gl-canc-len (pindx) <> zeros
021716        MOVE GL-CANC (PINDX)     TO  PY-GL-CANC-SW
021716     end-if
CIDMOD     MOVE GL-COMM (PINDX)        TO  PY-GL-COMMENT.
CIDMOD
01049      MOVE RTYPE (PINDX)          TO  PY-RECORD-TYPE.
01050      MOVE AMT (PINDX)            TO  PY-ENTRY-AMT.
060205     IF CO-TYPE = 'B' OR 'A' OR 'G'
060205        MOVE CO-TYPE             TO  PY-ERCOMP-TYPE
060205     END-IF
01051      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.
01052      MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.
01053      MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT
01054                                      PY-INPUT-DT.
01055      MOVE ZEROS                  TO  PY-CHECK-QUE-CONTROL
01056                                      PY-CHECK-QUE-SEQUENCE.
01057      MOVE LOW-VALUES             TO  PY-CREDIT-ACCEPT-DT
01058                                      PY-BILLED-DATE
01059                                      PY-AR-DATE
01060                                      PY-REPORTED-DT
01061                                      PY-CHECK-WRITTEN-DT.
01062      IF SDTE-LEN (PINDX) = ZEROS
01063          MOVE PI-CR-MONTH-END-DT TO  PY-CREDIT-SELECT-DT
CIDMOD         GO TO 2115-WRITE-REC
CIDMOD     END-IF.
CIDMOD
01065      IF SDTE (PINDX) IS NUMERIC
01066          MOVE SDTE (PINDX)   TO  DC-GREG-DATE-1-MDY
01067          MOVE '4'            TO  DC-OPTION-CODE
01068          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
01069          IF DATE-CONVERSION-ERROR
01070              MOVE PI-CR-MONTH-END-DT
01071                              TO  PY-CREDIT-SELECT-DT
01072          ELSE
01073              MOVE DC-BIN-DATE-1
01074                              TO  PY-CREDIT-SELECT-DT
CIDMOD         END-IF
01075      ELSE
01076          MOVE PI-CR-MONTH-END-DT
01077                              TO  PY-CREDIT-SELECT-DT
CIDMOD     END-IF
01078
01079      IF PI-SAV-FILE-SEQ-NO NOT GREATER THAN ZEROS
01080          MOVE RTYPE (PINDX)      TO  PI-SAV-RECORD-TYPE
01081          MOVE WORK-SEQ-NO        TO  PI-SAV-FILE-SEQ-NO.
01082
01083      IF FIRST-ADD AND END-OF-ACCT-FULL-PAGE
01084          MOVE RTYPE (PINDX)      TO  PI-SAV-RECORD-TYPE
01085          MOVE WORK-SEQ-NO        TO  PI-SAV-FILE-SEQ-NO.
01086
01087      MOVE 'N'                    TO  FIRST-ADD-SW.
01088
01089      MOVE 'A'                    TO  JP-RECORD-TYPE.
01090      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
01091      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
01092  2115-WRITE-REC.
01093      
      * EXEC CICS WRITE
01094 *        DATASET  (PYAJ-FILE-ID)
01095 *        FROM     (PENDING-PAY-ADJ)
01096 *        RIDFLD   (PY-CONTROL-PRIMARY)
01097 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004085' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01098
01099      PERFORM 8400-LOG-JOURNAL-RECORD.
01100
01101      GO TO 2100-UPDATE-LOOP.
01102
01103  EJECT
01104  2120-DELETE-RECORD.
CIDMOD     IF PY-RECORD-TYPE NOT = 'C'
CIDMOD         GO TO 2120-DELETE-CONT.
CIDMOD
CIDMOD     IF PY-CHECK-QUE-CONTROL = ZEROS
CIDMOD       AND PY-CHECK-QUE-SEQUENCE = ZEROS
CIDMOD         GO TO 2120-DELETE-CONT.
CIDMOD
CIDMOD     
      * EXEC CICS HANDLE CONDITION
CIDMOD*        NOTFND  (2120-DELETE-CONT)
CIDMOD*    END-EXEC.
      *    MOVE '"$I                   ! & #00004104' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034313034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD     MOVE PY-COMPANY-CD          TO  CHKQ-COMPANY-CD.
CIDMOD     MOVE PY-CHECK-QUE-CONTROL   TO  CHKQ-CONTROL-NUMBER.
CIDMOD     MOVE PY-CHECK-QUE-SEQUENCE  TO  CHKQ-SEQUENCE-NUMBER.
CIDMOD
CIDMOD     
      * EXEC CICS READ
CIDMOD*        SET      (ADDRESS OF CHECK-QUE)
CIDMOD*        DATASET  (CHKQ-FILE-ID)
CIDMOD*        RIDFLD   (ERCHKQ-KEY)
CIDMOD*        UPDATE
CIDMOD*    END-EXEC.
      *    MOVE '&"S        EU         (   #00004112' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CHKQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCHKQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD*    MOVE 'D'                    TO  JP-RECORD-TYPE.
CIDMOD*    MOVE CHKQ-FILE-ID           TO  JP-FILE-ID.
CIDMOD*    MOVE CHECK-QUE              TO  JP-RECORD-AREA.
CIDMOD*
CIDMOD*    PERFORM 8400-LOG-JOURNAL-RECORD.
CIDMOD
CIDMOD     
      * EXEC CICS DELETE
CIDMOD*        DATASET  (CHKQ-FILE-ID)
CIDMOD*    END-EXEC.
      *    MOVE '&(                    &   #00004125' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CHKQ-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01136
01137  2120-DELETE-CONT.
CIDMOD*    MOVE 'D'                    TO  JP-RECORD-TYPE.
CIDMOD*    MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
CIDMOD*    MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
CIDMOD*
CIDMOD*    PERFORM 8400-LOG-JOURNAL-RECORD.
01143
01144      IF PI-SAV-ENDING-PYAJ-KEY = ERPYAJ-KEY
01145          MOVE ZEROS              TO  PI-FRST-FILE-SEQ-NO
01146          MOVE SPACE              TO  PI-FRST-RECORD-TYPE
01147          MOVE ZEROS              TO  PI-SAV-ACCT-AMT
01148                                      PI-SAV-ACCT-NET
01149                                      PI-SAV-PREV-AMT
01150                                      PI-SAV-PREV-NET.
01151
01152      
      * EXEC CICS DELETE
01153 *        DATASET(PYAJ-FILE-ID)
01154 *    END-EXEC.
      *    MOVE '&(                    &   #00004144' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01155
01156      GO TO 2100-UPDATE-LOOP.
01157
01158  EJECT
01159
01160  2190-CHANGE-RECORD-TYPE.
01161
CIDMOD*    MOVE 'D'                   TO  JP-RECORD-TYPE.
CIDMOD*    MOVE PYAJ-FILE-ID          TO  JP-FILE-ID.
CIDMOD*    MOVE PENDING-PAY-ADJ       TO  JP-RECORD-AREA.
CIDMOD*
CIDMOD*    PERFORM 8400-LOG-JOURNAL-RECORD.
01167
01168      
      * EXEC CICS DELETE
01169 *        DATASET(PYAJ-FILE-ID)
01170 *    END-EXEC.
      *    MOVE '&(                    &   #00004160' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01171
01172      
      * EXEC CICS GETMAIN
01173 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
01174 *        LENGTH   (ERPYAJ-RECORD-LENGTH)
01175 *        INITIMG  (GETMAIN-SPACE)
01176 *    END-EXEC.
      *    MOVE ',"IL                  $   #00004164' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPYAJ-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01177
CIDMOD*    MOVE JP-RECORD-AREA        TO  PENDING-PAY-ADJ.
CIDMOD*    MOVE ERPYAJ-KEY            TO  PY-CONTROL-PRIMARY.
01180
PEMMOD     MOVE WS-SAVE-ERPYAJ        TO  PENDING-PAY-ADJ
01181      MOVE RTYPE (PINDX)         TO  PY-RECORD-TYPE
01182                                     PYAJ-RECORD-TYPE.
01183
01184      
      * EXEC CICS HANDLE CONDITION
01185 *        DUPREC (2190-DUP-RECORD)
01186 *    END-EXEC.
      *    MOVE '"$%                   ! '' #00004177' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034313737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01187
CIDMOD*    MOVE 'A'                   TO  JP-RECORD-TYPE.
CIDMOD*    MOVE PYAJ-FILE-ID          TO  JP-FILE-ID.
CIDMOD*    MOVE PENDING-PAY-ADJ       TO  JP-RECORD-AREA.
01191
01192  2190-RETRY-WRITE.
01193
01194      
      * EXEC CICS WRITE
01195 *        DATASET  (PYAJ-FILE-ID)
01196 *        FROM     (PENDING-PAY-ADJ)
01197 *        RIDFLD   (PY-CONTROL-PRIMARY)
01198 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004187' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01199
CIDMOD*    PERFORM 8400-LOG-JOURNAL-RECORD.
01201
01202      
      * EXEC CICS READ
01203 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
01204 *        DATASET  (PYAJ-FILE-ID)
01205 *        RIDFLD   (ERPYAJ-KEY)
01206 *        UPDATE
01207 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004195' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01208
01209      GO TO 2190-EXIT.
01210
01211  2190-DUP-RECORD.
01212
01213      COMPUTE PY-FILE-SEQ-NO = PY-FILE-SEQ-NO + 1.
01214
01215      GO TO 2190-RETRY-WRITE.
01216
01217  2190-EXIT.
01218      EXIT.
01219  EJECT
01220
01221  2200-UPDATE-COMPLETE.
01222      IF EIBAID = DFHPF1
01223          GO TO 4000-BROWSE-FRWD.
01224
01225      IF EIBAID = DFHPF2
01226          GO TO 4100-BROWSE-BKWD.
01227
01228      MOVE LOW-VALUES             TO  EL633AI.
01229
01230      MOVE ER-0000                TO  EMI-ERROR.
01231      MOVE -1                     TO  MAINTL.
01232
01233      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01234
01235      MOVE PI-FRST-FILE-SEQ-NO    TO  PI-SAV-FILE-SEQ-NO.
01236      MOVE PI-FRST-RECORD-TYPE    TO  PI-SAV-RECORD-TYPE.
01237
CIDMOD*    GO TO 4000-BROWSE-FRWD.
CIDMOD
CIDMOD*    GO TO 8100-SEND-INITIAL-MAP.
01239
01240  EJECT
01241  4000-BROWSE-FRWD.
01242      MOVE '1'                     TO  PI-PREV-PFKEY.
01243      MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.
01244
01245      IF EIBAID = DFHPF1
01246          IF PAGE-FULL
01247              MOVE PI-FILE-SEQ-NO (PI-FULL-INDX)
01248                                       TO  PYAJ-FILE-SEQ-NO
01249              MOVE HIGH-VALUES         TO  PYAJ-RECORD-TYPE
01250              MOVE 'N'                 TO  PYAJ-READ-SW
01251              MOVE PI-SAV-ACCT-AMT     TO  TOTAL-ACCT-AMT
01252              MOVE PI-SAV-ACCT-NET     TO  TOTAL-ACCT-NET
01253              MOVE PI-SAV-ACCT-AMT     TO  PI-SAV-PREV-AMT
01254              MOVE PI-SAV-ACCT-NET     TO  PI-SAV-PREV-NET
01255          ELSE
01256              MOVE ZEROS               TO  PI-SAV-ACCT-AMT
01257                                           PI-SAV-ACCT-NET
01258                                           PI-SAV-PREV-AMT
01259                                           PI-SAV-PREV-NET
01260              MOVE 'Y'                 TO  PI-PAGE-SW
CIDMOD             IF NOT END-OF-FILE
CIDMOD                 MOVE 99999999        TO  PYAJ-FILE-SEQ-NO
CIDMOD                 MOVE HIGH-VALUES     TO  PYAJ-RECORD-TYPE
CIDMOD             END-IF
01261              IF TOP-OF-FILE
01262                  MOVE SPACES          TO  PI-PYAJ-FILE-SW
01263                  MOVE ZEROS           TO  PYAJ-FILE-SEQ-NO
01264                  MOVE SPACES          TO  PYAJ-RECORD-TYPE
01265              ELSE
01266                  MOVE -1              TO  PYAJ-FILE-SEQ-NO
01267                  MOVE HIGH-VALUES     TO  PYAJ-RECORD-TYPE
01268      ELSE
01269          MOVE PI-SAV-PREV-AMT         TO  TOTAL-ACCT-AMT
01270          MOVE PI-SAV-PREV-NET         TO  TOTAL-ACCT-NET
01271          IF MAINTI = 'S'
01272              MOVE ZEROS               TO  PYAJ-FILE-SEQ-NO
01273              MOVE SPACES              TO  PYAJ-RECORD-TYPE.
01274
CIDMOD
CIDMOD     MOVE SPACE                  TO  PI-PYAJ-FILE-SW.
CIDMOD
01275      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
01276          MOVE PI-CARRIER-SECURITY  TO  PYAJ-CARRIER.
01277
01278  4000-BROWSE-FRWD-FOR-PREV.
CIDMOD*    IF END-OF-FILE
CIDMOD*        IF EIBAID = DFHPF1
CIDMOD*           IF PI-TOTAL-DISPLAYED
CIDMOD*              NEXT SENTENCE
CIDMOD*           ELSE
CIDMOD*              MOVE -1                 TO  MAINTL
CIDMOD*              MOVE ER-2237            TO  EMI-ERROR
CIDMOD*              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
CIDMOD*              MOVE SPACE              TO  PI-PYAJ-FILE-SW
CIDMOD*              SET PINDX               TO  +2
CIDMOD*              MOVE 'TOTAL'            TO  COMM (PINDX)
CIDMOD*              MOVE PI-SAV-ACCT-AMT    TO  AMTO (PINDX)
CIDMOD*              SET PINDEX UP BY 1
CIDMOD*              MOVE 'NET TOTAL'        TO  COMM (PINDX)
CIDMOD*              MOVE PI-SAV-ACCT-NET    TO  AMTO (PINDX)
CIDMOD*              MOVE ZEROS              TO  PI-SAV-ACCT-AMT
CIDMOD*                                          PI-SAV-ACCT-NET
CIDMOD*              GO TO 8100-SEND-INITIAL-MAP.
CIDMOD*
CIDMOD*     IF END-OF-FILE
CIDMOD*        IF EIBAID = DFHPF1
CIDMOD*           MOVE -1             TO  MAINTL
CIDMOD*           MOVE ER-2237        TO  EMI-ERROR
CIDMOD*           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
CIDMOD*           GO TO 8200-SEND-DATAONLY.
CIDMOD*
01305      PERFORM 5000-START-BROWSE  THRU  5030-EXIT.
01306
01307      IF NO-RECORDS
01308         MOVE SPACE              TO  PI-PYAJ-FILE-SW
01309         MOVE LOW-VALUES         TO  EL633AO
01310         MOVE ER-2239            TO  EMI-ERROR
01311         MOVE -1                 TO  MAINTL
01312         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01313         GO TO 8100-SEND-INITIAL-MAP.
01314
01315      IF NOT-OPEN
01316          GO TO 7000-PYAJ-FILE-NOTOPEN.
01317
01318      MOVE LOW-VALUES             TO  EL633AO.
01319      MOVE ZEROS                  TO  PI-SEQ-NOS.
01320
CIDMOD     PERFORM 6000-READ-AND-FORMAT-SCREEN  THRU  6200-EXIT
CIDMOD          VARYING  PINDX  FROM  1  BY  1
CIDMOD             UNTIL  END-OF-ACCT
CIDMOD                 OR  END-OF-FILE
CIDMOD                     OR  PAGE-FULL
CIDMOD                         OR  NO-RECORDS.
01322
01323      IF NO-RECORDS
01324          MOVE SPACE              TO  PI-PYAJ-FILE-SW
CIDMOD         MOVE LOW-VALUES         TO  EL633AO
01325          MOVE ER-2239            TO  EMI-ERROR
01326          MOVE -1                 TO  MAINTL
01327          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01328          GO TO 8100-SEND-INITIAL-MAP.
01329
01330      IF END-OF-FILE
01331          IF EIBAID = DFHPF2
01332              MOVE ER-2238        TO  EMI-ERROR
01333              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01334              MOVE 'T'            TO  PI-PYAJ-FILE-SW
01335          ELSE
01336              MOVE ER-2237        TO  EMI-ERROR
01337              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01338
01339      MOVE 'S'                    TO  PI-PREV-FUNCTION
01340                                      PI-SAV-FUNCTION.
01341
01342      GO TO 8100-SEND-INITIAL-MAP.
01343  EJECT
01344  4100-BROWSE-BKWD.
01345      MOVE SPACE                   TO  PI-PYAJ-FILE-SW
01346                                       PI-TOTAL-DISPLAYED-SW.
01347      MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.
01348      MOVE ZEROS                   TO  PYAJ-FILE-SEQ-NO.
01349      MOVE ZEROS                   TO  PYAJ-RECORD-TYPE.
01350      MOVE ZEROS                   TO  PI-SAV-ACCT-AMT
01351                                       PI-SAV-ACCT-NET
01352                                       PI-SAV-PREV-AMT
01353                                       PI-SAV-PREV-NET.
01354
01355      PERFORM 5000-START-BROWSE  THRU  5030-EXIT.
01356
01357      IF NO-RECORDS
01358          MOVE SPACES             TO  PI-PYAJ-FILE-SW
01359          MOVE LOW-VALUES         TO  EL633AO
01360          MOVE ER-2239            TO  EMI-ERROR
01361          MOVE -1                 TO  MAINTL
01362          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01363          GO TO 8100-SEND-INITIAL-MAP.
01364
01365      IF NOT-OPEN
01366          GO TO 7000-PYAJ-FILE-NOTOPEN.
01367
01368      
      * EXEC CICS READNEXT
01369 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
01370 *        DATASET  (PYAJ-FILE-ID)
01371 *        RIDFLD   (ERPYAJ-KEY)
01372 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004376' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01373
01374      IF FIRST-PAGE OR
01375         PI-PREV-PFKEY = '2'
01376          PERFORM 5100-READ-PREVIOUS  THRU  5120-EXIT
01377          PERFORM 5100-READ-PREVIOUS  THRU  5120-EXIT.
01378
01379      PERFORM 5200-END-BROWSE  THRU  5200-EXIT.
01380
01381      MOVE ZEROS                  TO  PYAJ-FILE-SEQ-NO.
01382      MOVE SPACES                 TO  PYAJ-RECORD-TYPE.
01383      MOVE '2'                     TO  PI-PREV-PFKEY.
01384
01385      GO TO 4000-BROWSE-FRWD-FOR-PREV.
01386  EJECT
01387  5000-START-BROWSE.
01388      
      * EXEC CICS HANDLE CONDITION
01389 *        NOTOPEN  (5010-NOT-OPEN)
01390 *        NOTFND   (5020-NO-RECORDS)
01391 *        ENDFILE  (5020-NO-RECORDS)
01392 *    END-EXEC.
      *    MOVE '"$JI''                 ! ( #00004396' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034333936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01393
01394      
      * EXEC CICS STARTBR
01395 *        DATASET  (PYAJ-FILE-ID)
01396 *        RIDFLD   (ERPYAJ-KEY)
01397 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004402' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01398
01399      GO TO 5030-EXIT.
01400
01401  5010-NOT-OPEN.
01402      MOVE 'Z'                    TO  PI-PYAJ-FILE-SW.
01403
01404      GO TO 5030-EXIT.
01405
01406  5020-NO-RECORDS.
01407      MOVE ZEROS                  TO  PI-SEQ-NOS.
01408      MOVE 'Y'                    TO  PI-PYAJ-FILE-SW.
01409
01410  5030-EXIT.
01411      EXIT.
01412  EJECT
01413  5100-READ-PREVIOUS.
01414      
      * EXEC CICS HANDLE CONDITION
01415 *        ENDFILE  (5110-END-OF-FILE)
01416 *        NOTFND   (5110-END-OF-FILE)
01417 *    END-EXEC.
      *    MOVE '"$''I                  ! ) #00004422' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303034343232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01418
01419      
      * EXEC CICS READPREV
01420 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
01421 *        DATASET  (PYAJ-FILE-ID)
01422 *        RIDFLD   (ERPYAJ-KEY)
01423 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004427' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01424
01425      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
01426          IF PI-CARRIER-SECURITY = PY-CARRIER
01427              NEXT SENTENCE
01428          ELSE
01429              GO TO 5110-END-OF-FILE.
01430
01431      GO TO 5120-EXIT.
01432
01433  5110-END-OF-FILE.
CIDMOD     MOVE 'X'                    TO  PI-PYAJ-FILE-SW.
01434      MOVE ER-2238                TO  EMI-ERROR.
01435      MOVE -1                     TO  PFENTERL.
01436
01437      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01438
01439      GO TO 8200-SEND-DATAONLY.
01440
01441  5120-EXIT.
01442      EXIT.
01443
01444  5200-END-BROWSE.
01445      
      * EXEC CICS ENDBR
01446 *        DATASET  (PYAJ-FILE-ID)
01447 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004454' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01448
01449  5200-EXIT.
01450      EXIT.
01451  EJECT
01452  6000-READ-AND-FORMAT-SCREEN.
01453      
      * EXEC CICS HANDLE CONDITION
01454 *        ENDFILE  (6100-END-OF-FILE)
01455 *        NOTFND   (6100-END-OF-FILE)
01456 *    END-EXEC.
      *    MOVE '"$''I                  ! * #00004462' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303034343632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01457
CIDMOD*    MOVE SPACES                 TO  PI-TOTAL-DISPLAYED-SW.
CIDMOD*    INITIALIZE  PI-SEQ-NOS.
01461
01462  6010-READ-NEXT.
01463
01464      
      * EXEC CICS READNEXT
01465 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
01466 *        DATASET  (PYAJ-FILE-ID)
01467 *        RIDFLD   (ERPYAJ-KEY)
01468 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004472' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01469
01470      IF PI-COMPANY-CD NOT = PY-COMPANY-CD
01471        AND PI-SAV-ENDING-PYAJ-KEY NOT = SPACES
01472          GO TO 6100-END-OF-FILE.
01473
01474      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
01475          IF PY-CARRIER NOT = PI-CARRIER-SECURITY
01476              IF PI-SAV-ENDING-PYAJ-KEY NOT = SPACES
01477                  GO TO 6100-END-OF-FILE.
01478
01479      IF PYAJ-1ST-READ
01480        AND EIBAID NOT = DFHENTER
01481        AND NOT PAGE-FULL
01482          MOVE PY-CONTROL-PRIMARY TO  PI-SAV-ENDING-PYAJ-KEY
01483          MOVE PY-CARRIER         TO  PI-CR-CARRIER
01484          MOVE PY-GROUPING        TO  PI-CR-GROUPING
01485          MOVE PY-FIN-RESP        TO  PI-CR-FIN-RESP
01486          MOVE PY-ACCOUNT         TO  PI-CR-ACCOUNT.
01487
01488      IF PYAJ-1ST-READ
01489          MOVE PI-SAV-ENDING-PYAJ-KEY
01490                                  TO  PI-START-PYAJ-KEY.
01491
01492      IF PI-COMPANY-CD   = PY-COMPANY-CD  AND
01493         PI-SAV-CARRIER  = PY-CARRIER     AND
01494         PI-SAV-GROUPING = PY-GROUPING    AND
01495         PI-SAV-FIN-RESP = PY-FIN-RESP    AND
01496         PI-SAV-ACCOUNT  = PY-ACCOUNT
01497          NEXT SENTENCE
01498      ELSE
01499          IF PYAJ-1ST-READ
01501              MOVE 'Y'            TO  PI-PYAJ-FILE-SW
01502              MOVE SPACE          TO  PYAJ-READ-SW
01503              GO TO 6200-EXIT
01504          ELSE
CIDMOD             MOVE 'A'            TO  PI-PYAJ-FILE-SW
CIDMOD             IF PINDX  IS LESS THAN  12
01506                  SET PINDX  UP  BY  1
CIDMOD                 MOVE 'TOTAL'         TO  GL-ACCT (PINDX)
CIDMOD                 MOVE TOTAL-ACCT-AMT  TO  AMTO (PINDX)
CIDMOD                 SET PINDX  UP  BY  1
CIDMOD                 MOVE 'NET TOTAL'     TO  GL-ACCT (PINDX)
CIDMOD                 MOVE TOTAL-ACCT-NET  TO  AMTO (PINDX)
01520                      GO TO 6200-EXIT
01521              ELSE
CIDMOD                 MOVE 'TOTAL'         TO  GL-ACCT (PINDX)
CIDMOD                 MOVE TOTAL-ACCT-AMT  TO  AMTO (PINDX)
CIDMOD                 SET PINDX  UP  BY  1
CIDMOD                 MOVE 'NET TOTAL'     TO  GL-ACCT (PINDX)
CIDMOD                 MOVE TOTAL-ACCT-NET  TO  AMTO (PINDX)
01537                  GO TO 6200-EXIT.
01538
CIDMOD*    MOVE ' '                    TO  PI-PYAJ-FILE-SW.
01540
01541      IF PY-CREDIT-ACCEPT-DT NOT = LOW-VALUES
01542          GO TO 6000-READ-AND-FORMAT-SCREEN.
01543
CIDMOD*    SET PINDX  UP  BY +1.
01545
01546      SET WS-SAVE-INDEX-VALUE  TO  PINDX.
01547
01548      IF PINDX  IS GREATER THAN  13
01549          MOVE 'F'                TO  PI-PYAJ-FILE-SW
01550          MOVE +13                TO  PI-FULL-INDX
01551          MOVE PI-FRST-FILE-SEQ-NO
01552                                  TO  PI-LAST-FILE-SEQ-NO
01553          MOVE PI-FRST-RECORD-TYPE
01554                                  TO  PI-LAST-RECORD-TYPE
01555          MOVE TOTAL-ACCT-AMT     TO  PI-SAV-ACCT-AMT
01556          MOVE TOTAL-ACCT-NET     TO  PI-SAV-ACCT-NET
01557          GO TO 6200-EXIT.
01558
01559      MOVE PY-FILE-SEQ-NO         TO  PI-PREV-FILE-SEQ-NO.
01560      MOVE PY-RECORD-TYPE         TO  PI-PREV-RECORD-TYPE.
01561
01562      IF PYAJ-1ST-READ
01563          MOVE SPACE              TO  PYAJ-READ-SW.
01564
01565      IF PINDX = 1
01566          MOVE PI-PREV-FILE-SEQ-NO
01567                                  TO  PI-FRST-FILE-SEQ-NO
01568          MOVE PI-PREV-RECORD-TYPE
01569                                  TO  PI-FRST-RECORD-TYPE
01570          MOVE PI-PREV-FILE-SEQ-NO
01571                                  TO  PI-LAST-FILE-SEQ-NO
01572          MOVE PI-PREV-RECORD-TYPE
01573                                  TO  PI-LAST-RECORD-TYPE.
01574
01575      SET NDX                     TO  PINDX.
01576      SET WS-SAVE-NDX-VALUE       TO  NDX.
01577
01578      MOVE PY-FILE-SEQ-NO         TO  PI-FILE-SEQ-NO (NDX).
PEMMOD
CIDMOD     MOVE PY-GL-ACCOUNT          TO  GL-ACCT (PINDX).
CIDMOD     MOVE PY-GL-STATE            TO  GL-STATE (PINDX).
CIDMOD     MOVE PY-GL-CANC-SW          TO  GL-CANC (PINDX).
CIDMOD     MOVE PY-GL-COMMENT          TO  GL-COMM (PINDX).
01580      MOVE PY-ENTRY-AMT           TO  AMTO (PINDX).
01581
01582      ADD PY-ENTRY-AMT            TO  TOTAL-ACCT-AMT.
01583
031710     IF PY-RECORD-TYPE = 'R' OR 'D' OR 'S' OR 'Z'
01585          ADD PY-ENTRY-AMT        TO  TOTAL-ACCT-NET
01586      ELSE
01587          SUBTRACT PY-ENTRY-AMT  FROM  TOTAL-ACCT-NET.
01588
01589      MOVE PY-RECORD-TYPE         TO  RTYPE (PINDX)
01590                                      PI-REC-TYPE (NDX).
01591
01592      IF PY-VOID-SW NOT = SPACE
01593          MOVE PY-VOID-SW         TO  VOID-SW (PINDX).
01594
01595      IF PY-LAST-MAINT-DT = PREV-BIN-MAINT-DT
01596          MOVE PREV-MAINT-DT      TO  MDTE (PINDX)
01597      ELSE
01598          MOVE PY-LAST-MAINT-DT   TO  DC-BIN-DATE-1
01599                                      PREV-BIN-MAINT-DT
01600          MOVE SPACE              TO  DC-OPTION-CODE
01601          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
01602** DAN    MOVE DC-GREG-DATE-1-EDIT
CIDMOD         MOVE DC-GREG-DATE-1-MDY
01603                                  TO  MDTE (PINDX)
01604                                      PREV-MAINT-DT.
01605
CIDMOD*    IF PY-GL-COMMENT NOT = (LOW-VALUES OR SPACES OR ZEROS)
PEMMOD     IF PY-GL-COMMENT NOT = (LOW-VALUES AND SPACES AND ZEROS)
CIDMOD         INSPECT PY-GL-COMMENT CONVERTING LOW-VALUES TO SPACES
CIDMOD         MOVE PY-GL-COMMENT      TO  GL-COMM (PINDX)
CIDMOD*        MOVE AL-UANON           TO  GL-COMM-ATTRB (PINDX)
PEMMOD*        MOVE AL-UANON           TO  GL-COMM-ATTRB (PINDX)
CIDMOD     END-IF.
CIDMOD
01606      IF PY-BILLED-DATE NOT = LOW-VALUES
CIDMOD         MOVE AL-SANOF           TO  GL-ACCT-ATTRB (PINDX)
CIDMOD                                     AMT-ATTRB (PINDX)
CIDMOD                                     VOID-SW-ATTRB (PINDX)
01610          IF PY-BILLED-DATE = PREV-BIN-BL-DT
01611              MOVE PREV-BL-DT     TO  BDTE (PINDX)
01612          ELSE
01613              MOVE PY-BILLED-DATE TO  DC-BIN-DATE-1
01614                                      PREV-BIN-BL-DT
01615              MOVE SPACE          TO  DC-OPTION-CODE
01616              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
01617** DAN        MOVE DC-GREG-DATE-1-EDIT
CIDMOD             MOVE DC-GREG-DATE-1-MDY
01618                                  TO  BDTE (PINDX)
01619                                      PREV-BL-DT.
01620
01621      IF (PY-CHECK-ORIGIN-SW = LOW-VALUES OR SPACES OR 'G')
01622          NEXT SENTENCE
01623      ELSE
01624          MOVE AL-SANOF           TO  RTYPE-ATTRB   (PINDX)
01625                                      AMT-ATTRB     (PINDX).
01626
01627      IF PY-CREDIT-SELECT-DT NOT = LOW-VALUES
01628          MOVE PY-CREDIT-SELECT-DT
01629                                  TO  DC-BIN-DATE-1
01630          MOVE SPACE              TO  DC-OPTION-CODE
01631          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
CIDMOD
CIDMOD         MOVE DC-GREG-DATE-1-EDIT  TO  WS-GREG-STORE
CIDMOD         MOVE WS-GS-MM             TO  WS-SS-MM
CIDMOD         MOVE WS-GS-DD             TO  WS-SS-DD
CIDMOD         MOVE WS-GS-YY             TO  WS-SS-YY
CIDMOD         MOVE WS-SDTE-STORE        TO  SDTE (PINDX).
PEMMOD     IF (PY-INPUT-DT NOT = WS-CURRENT-BIN-DT)
              AND (PI-PROCESSOR-ID NOT = 'PEMA')
PEMMOD         MOVE AL-SANOF           TO RTYPE-ATTRB    (PINDX)
PEMMOD                                    AMT-ATTRB      (PINDX)
PEMMOD                                    GL-ACCT-ATTRB  (PINDX)
PEMMOD                                    GL-COMM-ATTRB  (PINDX)
PEMMOD                                    VOID-SW-ATTRB  (PINDX)
PEMMOD                                    GL-STATE-ATTRB (PINDX)
PEMMOD                                    GL-CANC-ATTRB  (PINDX)
PEMMOD                                    SDTE-ATTRB     (PINDX)
PEMMOD
PEMMOD     END-IF
01634
CIDMOD*    GO TO 6010-READ-NEXT.
CIDMOD     GO TO 6200-EXIT.
01636
01637  6100-END-OF-FILE.
01638      MOVE 'X'                    TO  PI-PYAJ-FILE-SW.
01639
01640      IF PINDX  IS LESS THAN  12
CIDMOD         SET PINDX  UP  BY  1
CIDMOD         MOVE 'TOTAL'            TO  GL-ACCT (PINDX)
01643          MOVE TOTAL-ACCT-AMT     TO  AMTO (PINDX)
01644          SET PINDX  UP  BY  1
CIDMOD         MOVE 'NET TOTAL'        TO  GL-ACCT (PINDX)
01646          MOVE TOTAL-ACCT-NET     TO  AMTO (PINDX)
01647          MOVE 'Y'                TO  PI-TOTAL-DISPLAYED-SW
01648      ELSE
CIDMOD         MOVE 'TOTAL'            TO  GL-ACCT (PINDX)
CIDMOD         MOVE TOTAL-ACCT-AMT     TO  AMTO (PINDX)
CIDMOD         SET PINDX  UP  BY  1
CIDMOD         MOVE 'NET TOTAL'        TO  GL-ACCT (PINDX)
CIDMOD         MOVE TOTAL-ACCT-NET     TO  AMTO (PINDX).
01651
01652  6200-EXIT.
01653      EXIT.
01654  EJECT
01655  7000-PYAJ-FILE-NOTOPEN.
01656      MOVE -1                     TO  MAINTL.
01657      MOVE ER-2232                TO  EMI-ERROR.
01658
01659      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01660
01661      GO TO 8200-SEND-DATAONLY.
01662
01663  7100-COMP-FILE-NOTOPEN.
01664      MOVE -1                     TO  MAINTL.
01665      MOVE ER-2233                TO  EMI-ERROR.
01666
01667      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01668
01669      GO TO 8200-SEND-DATAONLY.
01670  EJECT
CIDMOD*
CIDMOD*7100-COFA-FILE-NOTOPEN.
CIDMOD*    MOVE -1                     TO PFENTERL.
CIDMOD*    MOVE ER-2959                TO EMI-ERROR.
CIDMOD*    PERFORM 9900-ERROR-FORMAT THRU
CIDMOD*            9900-EXIT.
CIDMOD*    GO TO 8200-SEND-DATAONLY.
CIDMOD*
01671  8100-SEND-INITIAL-MAP.
01672      MOVE WS-CURRENT-DT          TO  DATEO.
01673      MOVE EIBTIME                TO  TIME-IN.
01674      MOVE TIME-OUT               TO  TIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01675      MOVE -1                     TO  MAINTL.
01676      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
01677      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
01678
01679      IF EIBTRNID = TRANS-ID
01680        OR EL640-TRANS-ID
01681        OR EL642-TRANS-ID
01682        OR EL652-TRANS-ID
01683        OR EL6331-TRANS-ID
01684          IF PI-SAV-ENDING-PYAJ-KEY NOT =  SPACES
01685              MOVE PI-SAV-FUNCTION  TO  MAINTI
01686              MOVE PI-SAV-CARRIER   TO  CARRIERO
01687              MOVE PI-SAV-GROUPING  TO  GROUPO
01688              MOVE PI-SAV-FIN-RESP  TO  FINRESPO
01689              MOVE PI-SAV-ACCOUNT   TO  ACCTO
01690              MOVE AL-UANON         TO  MAINTA
01691                                        CARRIERA
01692                                        GROUPA
01693                                        FINRESPA
01694                                        ACCTA
01695          ELSE
01696              NEXT SENTENCE.
01697
01698      
      * EXEC CICS SEND
01699 *        MAP     (MAP-NAME)
01700 *        MAPSET  (MAPSET-NAME)
01701 *        FROM    (EL633AO)
01702 *        ERASE
01703 *        CURSOR
01704 *        END-EXEC.
           MOVE LENGTH OF
            EL633AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004732' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL633AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01705
01706      GO TO 9100-RETURN-TRAN.
01707  EJECT
01708  8200-SEND-DATAONLY.
01709      MOVE WS-CURRENT-DT          TO  DATEO.
01710      MOVE EIBTIME                TO  TIME-IN.
01711      MOVE TIME-OUT               TO  TIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01712      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
01713      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
01714
01715      
      * EXEC CICS SEND
01716 *        MAP     (MAP-NAME)
01717 *        MAPSET  (MAPSET-NAME)
01718 *        FROM    (EL633AO)
01719 *        DATAONLY
01720 *        CURSOR
01721 *    END-EXEC.
           MOVE LENGTH OF
            EL633AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00004751' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL633AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01722
01723      GO TO 9100-RETURN-TRAN.
01724
01725  8300-SEND-TEXT.
01726      
      * EXEC CICS SEND TEXT
01727 *        FROM    (LOGOFF-TEXT)
01728 *        LENGTH  (LOGOFF-LENGTH)
01729 *        ERASE
01730 *        FREEKB
01731 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004762' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373632' TO DFHEIV0(25:11)
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
           
01732
01733      
      * EXEC CICS RETURN
01734 *    END-EXEC.
      *    MOVE '.(                    ''   #00004769' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01735  EJECT
01736  8400-LOG-JOURNAL-RECORD.
01737 *    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
01738 *    MOVE THIS-PGM               TO  JP-PROGRAM-ID.
01739
01740 *    EXEC CICS JOURNAL
01741 *        JFILEID  (PI-JOURNAL-FILE-ID)
01742 *        JTYPEID  ('EL')
01743 *        FROM     (JOURNAL-RECORD)
01744 *        LENGTH   (223)
01745 *        END-EXEC.
01746
01747  8500-DATE-CONVERT.
01748      
      * EXEC CICS LINK
01749 *        PROGRAM   (LINK-CLDATCV)
01750 *        COMMAREA  (DATE-CONVERSION-DATA)
01751 *        LENGTH    (DC-COMM-LENGTH)
01752 *    END-EXEC.
      *    MOVE '."C                   (   #00004784' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-CLDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01753
01754  8500-EXIT.
01755      EXIT.
01756
01757  8600-DEEDIT.
01758      
      * EXEC CICS BIF DEEDIT
01759 *        FIELD   (DEEDIT-FIELD)
01760 *        LENGTH  (11)
01761 *    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004794' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01762
01763  8600-EXIT.
01764      EXIT.
01765  EJECT
01766  8800-UNAUTHORIZED-ACCESS.
01767      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
01768
01769      GO TO 8300-SEND-TEXT.
01770
01771  8810-PF23.
01772      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01773      MOVE XCTL-005               TO  PGM-NAME.
01774
01775      GO TO 9300-XCTL.
01776
01777  9100-RETURN-TRAN.
01778      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01779      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01780
01781      
      * EXEC CICS RETURN
01782 *        TRANSID   (TRANS-ID)
01783 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
01784 *        LENGTH    (PI-COMM-LENGTH)
01785 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00004817' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01786
01787  9200-RETURN-MAIN-MENU.
01788      MOVE XCTL-626               TO  PGM-NAME.
01789
01790      GO TO 9300-XCTL.
01791
01792  9300-XCTL.
01793      
      * EXEC CICS XCTL
01794 *        PROGRAM   (PGM-NAME)
01795 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
01796 *        LENGTH    (PI-COMM-LENGTH)
01797 *    END-EXEC.
      *    MOVE '.$C                   %   #00004829' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01798
01799  9400-CLEAR.
01800      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
01801
01802      IF PI-RETURN-TO-PROGRAM = 'EL626'
01803      MOVE SPACES                 TO  PI-CR-CONTROL-IN-PROGRESS
01804                                      PI-SAV-COMP-CONTROL.
01805
01806      GO TO 9300-XCTL.
01807
01808  9500-PF12.
01809      MOVE XCTL-010               TO  PGM-NAME.
01810
01811      GO TO 9300-XCTL.
01812
01813  9600-PGMID-ERROR.
01814      
      * EXEC CICS HANDLE CONDITION
01815 *        PGMIDERR  (8300-SEND-TEXT)
01816 *    END-EXEC.
      *    MOVE '"$L                   ! + #00004850' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303034383530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01817
01818      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
01819      MOVE ' '                    TO  PI-ENTRY-CD-1.
01820      MOVE XCTL-005               TO  PGM-NAME.
01821      MOVE PGM-NAME               TO  LOGOFF-PGM.
01822      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01823
01824      GO TO 9300-XCTL.
01825
01826  9900-ERROR-FORMAT.
01827      IF NOT EMI-ERRORS-COMPLETE
01828          MOVE LINK-001           TO  PGM-NAME
01829          
      * EXEC CICS LINK
01830 *            PROGRAM   (PGM-NAME)
01831 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
01832 *            LENGTH    (EMI-COMM-LENGTH)
01833 *        END-EXEC.
      *    MOVE '."C                   (   #00004865' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01834
01835  9900-EXIT.
01836      EXIT.
01837
01838  9990-ABEND.
01839      MOVE LINK-004               TO  PGM-NAME.
01840      MOVE DFHEIBLK               TO  EMI-LINE1.
01841
01842      
      * EXEC CICS LINK
01843 *        PROGRAM   (PGM-NAME)
01844 *        COMMAREA  (EMI-LINE1)
01845 *        LENGTH    (72)
01846 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00004878' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01847
01848      MOVE -1                     TO  PFENTERL.
01849
01850      GO TO 8200-SEND-DATAONLY.
01851
01852  9995-SECURITY-VIOLATION.
01853 *                            COPY ELCSCTP.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008
00008
00009      MOVE EIBDATE          TO SM-JUL-DATE.
00010      MOVE EIBTRMID         TO SM-TERMID.
00011      MOVE THIS-PGM         TO SM-PGM.
00012      MOVE EIBTIME          TO TIME-IN.
00013      MOVE TIME-OUT         TO SM-TIME.
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
00015
00016      
      * EXEC CICS LINK
00017 *         PROGRAM  ('EL003')
00018 *         COMMAREA (SECURITY-MESSAGE)
00019 *         LENGTH   (80)
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00004906' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
01854
01855  9995-EXIT.
01856      EXIT.
01857

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL633' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0410-NO-COMP-MSTR,
                     7100-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0890-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 2110-ADD-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 2120-DELETE-CONT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 2190-DUP-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 5010-NOT-OPEN,
                     5020-NO-RECORDS,
                     5020-NO-RECORDS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 5110-END-OF-FILE,
                     5110-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 6100-END-OF-FILE,
                     6100-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL633' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
