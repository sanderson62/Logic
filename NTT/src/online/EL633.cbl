      *((program: EL633.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL633.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 12/28/94 10:04:35.
000007*                            VMOD=2.021
000008*
000009*AUTHOR.        LOGIC,INC.
000010*               DALLAS, TEXAS.
000011
000012*DATE-COMPILED.
000013
000014*SECURITY.   *****************************************************
000015*            *                                                   *
000016*            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
000017*            *                                                   *
000018*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000019*                                                                *
000020*            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
000021*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
000022*            *                                                   *
000023*            *****************************************************
000024*
000025*REMARKS.
000026*        TRANSACTION - EXB7 - COMPENSATION PAYMENTS/ADJUSTMENTS.
000027*
000028******************************************************************
000029*                   C H A N G E   L O G
000030*
000031* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000032*-----------------------------------------------------------------
000033*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000034* EFFECTIVE    NUMBER
000035*-----------------------------------------------------------------
000036* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
000037*                              ADJUSTED REDEFINES EL633AI FILLER
000038* 010803                   PEMA  ADD 1825013200 FOR DCC
000039* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
000040* 022504                   PEMA ADD GL CODE FOR DCC
000041* 093004                   PEMA ADD NEW GL CODE FOR DCC
000042* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
000043* 061605    2005051300001  PEMA ADD GL NUM EDIT FOR DCC
000044* 122105    2005033100001  PEMA ADD GL NUM EDIT FOR CSI
000045* 032806                   PEMA ADD MORE GL NUMBERS FOR CSI
000046* 071806    2006012600002  PEMA CHANGE DCC GL NUMBERS
000047* 080206    2006012600002  PEMA ADD GL NUMBER FOR CID & DCC
000048* 092506                   PEMA ADD NEW GL CODE FOR DCC
000049* 031909    2009030300001  AJRA ADD NEW GL CODE FOR CID
000050* 040109    2008050500001  AJRA ADD GL NUMBERS FOR CCC
000051* 031710  CR2009100700001  PEMA CORRECT MATH ON TYPE 'U'
000052* 120711  CR2011120100004  PEMA ADD GL NUMBER FOR CCC
000053* 031912  CR2011120900003  AJRA AHL COMPANY CODE
000054* 100713  CR2013100700001  AJRA ADD 1825091000 FOR CID
000055* 081414    2014012300001  PEMA  ADD PROCESSING FOR CARRIER 7 DCC
000056* 110315  CR2015101400001  PEMA ADD NEW DCC G/L #'S FOR ACH
000057* 111715  IR2015111600001  PEMA  CHG G/L ACCTNO ON CID ACH
000058* 021716  CR2016021000003  PEMA  ADD NEW G/L FOR MACHENS ACCTS
000059* 111016  CR2016110900002  TANA  REPLACE GL NUMBERS FOR CCC
000060* 060817  CR2017060700005  PEMA  ADD NEW G/L FOR VPP
000061* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000062******************************************************************
000063
000064
000065 ENVIRONMENT DIVISION.
000066 DATA DIVISION.
000067 EJECT
000068 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000069 77  FILLER  PIC X(32)  VALUE '********************************'.
000070 77  FILLER  PIC X(32)  VALUE '*    EL633 WORKING STORAGE     *'.
000071 77  FILLER  PIC X(32)  VALUE '************ V/M 2.021 *********'.
000072
000073*77  FILLER  PIC X(32)  VALUE '/////// SAVE COFA MASTER /////'.
000074*77  SV-COFA PIC X(42)  VALUE SPACES.
000075*77  K-SPACE PIC X(11)  VALUE SPACES.
000076
000077 01  DATE-WORK-AREAS.
000078     05  WS-GREG-STORE.
000079         10  WS-GS-MM  PIC XX.
000080         10  FILLER    PIC X.
000081         10  WS-GS-DD  PIC XX.
000082         10  FILLER    PIC X.
000083         10  WS-GS-YY  PIC XX.
000084     05  WS-SDTE-STORE.
000085         10  WS-SS-MM  PIC XX.
000086         10  WS-SS-DD  PIC XX.
000087         10  WS-SS-YY  PIC XX.
000088*                            COPY ELCSCTM.
      *>>((file: ELCSCTM))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCSCTM                             *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
000008*                                                                *
000009******************************************************************
000010 01  SECURITY-MESSAGE.
000011     12  FILLER                          PIC X(30)
000012            VALUE '** LOGIC SECURITY VIOLATION -'.
000013     12  SM-READ                         PIC X(6).
000014     12  FILLER                          PIC X(5)
000015            VALUE ' PGM='.
000016     12  SM-PGM                          PIC X(6).
000017     12  FILLER                          PIC X(5)
000018            VALUE ' OPR='.
000019     12  SM-PROCESSOR-ID                 PIC X(4).
000020     12  FILLER                          PIC X(6)
000021            VALUE ' TERM='.
000022     12  SM-TERMID                       PIC X(4).
000023     12  FILLER                          PIC XX   VALUE SPACE.
000024     12  SM-JUL-DATE                     PIC 9(5).
000025     12  FILLER                          PIC X    VALUE SPACE.
000026     12  SM-TIME                         PIC 99.99.
000027
      *<<((file: ELCSCTM))
000089*                            COPY ELCSCRTY.
      *>>((file: ELCSCRTY))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCSCRTY                            *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
000008*        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
000009*        SAVED IN PI-SECURITY-ADDRESS.                           *
000010*                                                                *
000011******************************************************************
000012 01  SECURITY-CONTROL.
000013     12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
000014     12  FILLER                       PIC XX    VALUE 'SC'.
000015     12  SC-CREDIT-CODES.
000016         16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
000017             20  SC-CREDIT-DISPLAY    PIC X.
000018             20  SC-CREDIT-UPDATE     PIC X.
000019     12  SC-CLAIMS-CODES.
000020         16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
000021             20  SC-CLAIMS-DISPLAY    PIC X.
000022             20  SC-CLAIMS-UPDATE     PIC X.
      *<<((file: ELCSCRTY))
000090
000091    EJECT
000092
000093 01  STANDARD-AREAS.
000094     12  SC-ITEM             PIC  S9(4) COMP VALUE +1.
000095     12  GETMAIN-SPACE       PIC  X          VALUE SPACE.
000096     12  MAP-NAME            PIC  X(8)       VALUE 'EL633A'.
000097     12  MAPSET-NAME         PIC  X(8)       VALUE 'EL633S'.
000098     12  SCREEN-NUMBER       PIC  X(4)       VALUE '633A'.
000099     12  TRANS-ID            PIC  X(4)       VALUE 'EXB7'.
000100     12  EL6331-TRANS-ID     PIC  X(4)       VALUE 'EXB8'.
000101     12  EL640-TRANS-ID      PIC  X(4)       VALUE 'EXC1'.
000102     12  EL642-TRANS-ID      PIC  X(4)       VALUE 'EXH7'.
000103     12  EL652-TRANS-ID      PIC  X(4)       VALUE 'EXD4'.
000104     12  THIS-PGM            PIC  X(8)       VALUE 'EL633'.
000105     12  PGM-NAME            PIC  X(8).
000106     12  TIME-IN             PIC S9(7).
000107     12  TIME-OUT-R  REDEFINES  TIME-IN.
000108         16  FILLER          PIC  X.
000109         16  TIME-OUT        PIC  99V99.
000110         16  FILLER          PIC  X(2).
000111     12  EL640               PIC  X(8)       VALUE 'EL640'.
000112     12  XCTL-005            PIC  X(8)       VALUE 'EL005'.
000113     12  XCTL-010            PIC  X(8)       VALUE 'EL010'.
000114     12  XCTL-626            PIC  X(8)       VALUE 'EL626'.
000115     12  XCTL-6331           PIC  X(8)       VALUE 'EL6331'.
000116     12  XCTL-640            PIC  X(8)       VALUE 'EL640'.
000117     12  XCTL-642            PIC  X(8)       VALUE 'EL642'.
000118     12  XCTL-652            PIC  X(8)       VALUE 'EL652'.
000119     12  LINK-001            PIC  X(8)       VALUE 'EL001'.
000120     12  LINK-004            PIC  X(8)       VALUE 'EL004'.
000121     12  RETURNED-FROM       PIC  X(8)       VALUE SPACES.
000122     12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.
000123     12  PYAJ-FILE-ID        PIC  X(8)       VALUE 'ERPYAJ'.
000124     12  CHKQ-FILE-ID        PIC  X(8)       VALUE 'ERCHKQ'.
000125     12  COMP-FILE-ID        PIC  X(8)       VALUE 'ERCOMP'.
000126     12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.
000127     12  WS-CURRENT-BIN-DT   PIC  X(2)       VALUE SPACES.
000128     12  PYAJ-READ-SW        PIC  X          VALUE 'Y'.
000129         88  PYAJ-1ST-READ                   VALUE 'Y'.
000130     12  FIRST-ADD-SW        PIC  X          VALUE 'Y'.
000131         88  FIRST-ADD                       VALUE 'Y'.
000132     12  ZERO-NDX            PIC  9          VALUE ZERO.
000133     12  WORK-SEQ-NO         PIC S9(8)       COMP-3 VALUE ZEROS.
000134     12  WORK-SEQ-TIME       PIC  9(6).
000135     12  WORK-SEQ-HHMMSS  REDEFINES  WORK-SEQ-TIME.
000136         16  WORK-SEQ-HHMMS  PIC  9(5).
000137         16  FILLER          PIC  9.
000138     12  WORK-DATE-JULIAN.
000139         16  WORK-JULIAN-YR  PIC  99         VALUE ZEROS.
000140         16  WORK-JULIAN-DD  PIC  999        VALUE ZEROS.
000141     12  JULIAN-YY-DD        PIC  9(4)       COMP-3 VALUE ZEROS.
000142     12  CHECK-REC-TYPE      PIC  X          VALUE SPACE.
000143         88  VALID-REC-TYPE                  VALUE  'R' 'D' 'C'
000144                                                    'S' 'T' 'U'
000145                                                    'X' 'Y' 'Z'
000146                                                    'F'.
000147     12  CHECK-CANC-TYPE     PIC  X          VALUE SPACE.
000148         88  VALID-CANC-TYPE                 VALUE 'N' 'Y' ' '.
000149     12  FORCE-SHOW-SW       PIC  X          VALUE SPACE.
000150         88  FORCE-SHOW                      VALUE  'Y'.
000151
000152     12  WS-EOM-DTS OCCURS 13  TIMES
000153                              INDEXED BY PINDEX.
000154         16  WS-EOM-DT               PIC XX.
000155*    12  COFA-FILE-ID        PIC  X(8)       VALUE 'COFAXXX '.
000156
000157     EJECT
000158     12  WS-ERROR-CODES.
000159         16  ER-0000         PIC  X(4)       VALUE '0000'.
000160         16  ER-0008         PIC  X(4)       VALUE '0008'.
000161         16  ER-0023         PIC  X(4)       VALUE '0023'.
000162         16  ER-0029         PIC  X(4)       VALUE '0029'.
000163         16  ER-0070         PIC  X(4)       VALUE '0070'.
000164         16  ER-0587         PIC  X(4)       VALUE '0587'.
000165         16  ER-2056         PIC  X(4)       VALUE '2056'.
000166         16  ER-2230         PIC  X(4)       VALUE '2230'.
000167         16  ER-2231         PIC  X(4)       VALUE '2231'.
000168         16  ER-2232         PIC  X(4)       VALUE '2232'.
000169         16  ER-2233         PIC  X(4)       VALUE '2233'.
000170         16  ER-2234         PIC  X(4)       VALUE '2234'.
000171         16  ER-2235         PIC  X(4)       VALUE '2235'.
000172         16  ER-2236         PIC  X(4)       VALUE '2236'.
000173         16  ER-2237         PIC  X(4)       VALUE '2237'.
000174         16  ER-2238         PIC  X(4)       VALUE '2238'.
000175         16  ER-2239         PIC  X(4)       VALUE '2239'.
000176         16  ER-2244         PIC  X(4)       VALUE '2244'.
000177         16  ER-2245         PIC  X(4)       VALUE '2245'.
000178         16  ER-2246         PIC  X(4)       VALUE '2246'.
000179         16  ER-2370         PIC  X(4)       VALUE '2370'.
000180         16  ER-2432         PIC  X(4)       VALUE '2432'.
000181         16  ER-2449         PIC  X(4)       VALUE '2449'.
000182         16  ER-2587         PIC  X(4)       VALUE '2587'.
000183         16  ER-2588         PIC  X(4)       VALUE '2588'.
000184         16  ER-2595         PIC  X(4)       VALUE '2595'.
000185         16  ER-2596         PIC  X(4)       VALUE '2596'.
000186         16  ER-2763         PIC  X(4)       VALUE '2763'.
000187         16  ER-2929         PIC  X(4)       VALUE '2929'.
000188         16  ER-2957         PIC  X(4)       VALUE '2957'.
000189         16  ER-2958         PIC  X(4)       VALUE '2958'.
000190*        16  ER-2959         PIC  X(4)       VALUE '2958'.
000191         16  ER-2960         PIC  X(4)       VALUE '2960'.
000192         16  ER-3020         PIC  X(4)       VALUE '3020'.
000193
000194*************************************************************
000195*                      TABLE OF STATE NAMES
000196*************************************************************
000197
000198 01  CHECK-STATE-CODE            PIC XX      VALUE SPACE.
000199     88  VALID-STATE-CODE    VALUE 'AK' 'AL' 'AR' 'AZ' 'CA'
000200                                   'CD' 'CO' 'CT' 'DC' 'DE'
000201                                   'FL' 'GA' 'GM' 'HI' 'IA'
000202                                   'ID' 'IL' 'IN' 'KS' 'KY'
000203                                   'LA' 'MA' 'MD' 'ME' 'MI'
000204                                   'MN' 'MO' 'MS' 'MT' 'MX'
000205                                   'NC' 'ND' 'NE' 'NH' 'NJ'
000206                                   'NM' 'NV' 'NY' 'OF' 'OH'
000207                                   'OK' 'OR' 'PA' 'PI' 'PR'
000208                                   'RI' 'SC' 'SD' 'TN' 'TX'
000209                                   'UT' 'VA' 'VI' 'VT' 'WA'
000210                                   'WI' 'WV' 'WY'.
000211
000212 01  CHECK-GL-ACCT         PIC X(10)  VALUE SPACE.
000213     88  VALID-GL-ACCOUNT  VALUE '1108121010'
000214                                 '1108124700'
000215                                 '1108125100'
000216                                 '1721211400'
000217                                 '1825011200'
000218                                 '1825011300'
000219                                 '1825091000'
000220                                 '1825099050'
000221                                 '8505700033'
000222                                 '8506400030'
000223                                 '8507200020'
000224                                 '8507200010'
000225                                 '2725010160'.
000226
000227     88  VALID-DCC-GL-ACCOUNT  VALUE '2725040300'
000228                                     '2725040320'
000229                                     '2725040330'
000230                                     '2725040310'
000231                                     '8506400030'
000232                                     '8507200010'
000233                                     '1108121250'.
000234*    88  VALID-CSI-GL-ACCOUNT  VALUE '2725040100'
000235*                                    '2725040110'
000236*                                    '2725040120'
000237*                                    '2725040130'
000238*                                    '2725020400'.
000239     88  VALID-CSI-GL-ACCOUNT  VALUE '1108121010'
000240                                     '1825013200'
000241                                     '1825013300'
000242                                     '1825013400'.
000243
000244     88  VALID-CCC-GL-ACCOUNT  VALUE '1825013100'
000245                                     '1825013200'
000246                                     '1825013300'
000247                                     '1825013400'
000248                                     '8506400030'
000249                                     '8507200010'
000250                                     '8507200020'
000251                                     '1108121010'.
000252
000253     88  VALID-VPP-GL-ACCOUNT  VALUE '2725040510'
000254                                     '2725040520'
000255                                     '7206100400'
000256                                     '7206104100'.
000257
000258     88  VALID-FNL-GL-ACCOUNT  VALUE '1108121010'
000259                                     '1721211400'
000260                                     '1825011100'
000261                                     '1825011200'
000262                                     '1825011300'
000263                                     '1825099050'
000264                                     '2718000110'
000265                                     '2718000120'
000266                                     '8506400030'
000267                                     '8507200010'.
000268
000269*01  WS-ACCT-BREAK.
000270*    05  WS-ACCT-1               PIC X.
000271*    05  FILLER                  PIC X(6).
000272*
000273 01  WORK-AREA.
000274     12  WS-SAVE-ERPYAJ      PIC X(200)      VALUE SPACES.
000275     12  QID.
000276         16  QID-TERM        PIC X(04).
000277         16  FILLER          PIC X(04)       VALUE '633A'.
000278     12  DEEDIT-FIELD        PIC X(11).
000279     12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(11).
000280     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD PIC S9(9)V99.
000281     12  DATE-TEST-AREA      PIC  9(6).
000282     12  DATE-TEST-AREA-R  REDEFINES  DATE-TEST-AREA.
000283         16  DATE-TEST-MM    PIC  99.
000284         16  DATE-TEST-DD    PIC  99.
000285         16  DATE-TEST-YY    PIC  99.
000286     12  DIVIDE-RESULT       PIC  99.
000287     12  DIVIDE-REMAINDER    PIC  9.
000288     12  PREV-BIN-MAINT-DT   PIC  XX           VALUE SPACES.
000289     12  PREV-MAINT-DT       PIC  X(8)         VALUE SPACES.
000290     12  PREV-BIN-BL-DT      PIC  XX           VALUE SPACES.
000291     12  PREV-BL-DT          PIC  X(8)         VALUE SPACES.
000292     12  TOTAL-ACCT-AMT      PIC S9(7)V99      VALUE ZEROS.
000293     12  TOTAL-ACCT-NET      PIC S9(7)V99      VALUE ZEROS.
000294     12  WS-SAVE-INDEX-VALUE PIC S9(4) COMP    VALUE ZEROS.
000295     12  WS-SAVE-NDX-VALUE   PIC S9(4) COMP    VALUE ZEROS.
000296
000297 01  ACCESS-KEYS.
000298     12  ERPYAJ-KEY.
000299         16  PYAJ-COMP-CD            PIC  X      VALUE SPACE.
000300         16  PYAJ-CARRIER            PIC  X      VALUE SPACES.
000301         16  PYAJ-GROUPING           PIC  X(6)   VALUE SPACES.
000302         16  PYAJ-FIN-RESP           PIC  X(10)  VALUE SPACES.
000303         16  PYAJ-ACCOUNT            PIC  X(10)  VALUE SPACES.
000304         16  PYAJ-FILE-SEQ-NO        PIC S9(8)   VALUE +0  COMP.
000305         16  PYAJ-RECORD-TYPE        PIC  X      VALUE SPACES.
000306
000307     12  ERPYAJ-RECORD-LENGTH        PIC S9(4) COMP VALUE +200.
000308     12  ERPYAJ-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +223.
000309
000310     12  ERCHKQ-KEY.
000311         16  CHKQ-COMPANY-CD         PIC  X      VALUE SPACE.
000312         16  CHKQ-CONTROL-NUMBER     PIC S9(8)   VALUE +0  COMP.
000313         16  CHKQ-SEQUENCE-NUMBER    PIC S9(4)   VALUE +0  COMP.
000314     12  ERCOMP-KEY.
000315         16  COMP-COMP-CD            PIC  X      VALUE SPACE.
000316         16  COMP-CARRIER            PIC  X      VALUE SPACES.
000317         16  COMP-GROUPING           PIC  X(6)   VALUE SPACES.
000318         16  COMP-FIN-RESP           PIC  X(10)  VALUE SPACES.
000319         16  COMP-ACCOUNT            PIC  X(10)  VALUE SPACES.
000320         16  COMP-RECORD-TYPE        PIC  X      VALUE SPACES.
000321*    12  COFA-KEY-X.
000322*        16  COFA-COMPANY-X          PIC  X(4)   VALUE SPACES.
000323*        16  COFA-ACCOUNT.
000324*            20  COFA-FILLER         PIC  X(11)  VALUE SPACES.
000325*            20  COFA-MSA-ACCT       PIC  X(07)  VALUE SPACES.
000326*
000327 EJECT
000328*                                    COPY ELCDATE.
      *>>((file: ELCDATE))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCDATE.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003
000007*                                                                *
000008*                                                                *
000009*   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
000010*                 LENGTH = 200                                   *
000011******************************************************************
000012
000013 01  DATE-CONVERSION-DATA.
000014     12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
000015     12  DC-OPTION-CODE                PIC X.
000016         88  BIN-TO-GREG                VALUE ' '.
000017         88  ELAPSED-BETWEEN-BIN        VALUE '1'.
000018         88  EDIT-GREG-TO-BIN           VALUE '2'.
000019         88  YMD-GREG-TO-BIN            VALUE '3'.
000020         88  MDY-GREG-TO-BIN            VALUE '4'.
000021         88  JULIAN-TO-BIN              VALUE '5'.
000022         88  BIN-PLUS-ELAPSED           VALUE '6'.
000023         88  FIND-CENTURY               VALUE '7'.
000024         88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
000025         88  EDIT-GREG-TO-BIN-3         VALUE '9'.
000026         88  YMD-GREG-TO-BIN-3          VALUE 'A'.
000027         88  MDY-GREG-TO-BIN-3          VALUE 'B'.
000028         88  JULIAN-TO-BIN-3            VALUE 'C'.
000029         88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
000030         88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
000031         88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
000032         88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
000033         88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
000034         88  CHECK-LEAP-YEAR            VALUE 'H'.
000035         88  BIN-3-TO-GREG              VALUE 'I'.
000036         88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
000037         88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
000038         88  CYMD-GREG-TO-BIN           VALUE 'L'.
000039         88  MDCY-GREG-TO-BIN           VALUE 'M'.
000040         88  MDY-GREG-TO-JULIAN         VALUE 'N'.
000041         88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
000042         88  YMD-GREG-TO-JULIAN         VALUE 'P'.
000043         88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
000044         88  THREE-CHARACTER-BIN
000045                  VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
000046         88  GREGORIAN-TO-BIN
000047                  VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
000048         88  BIN-TO-GREGORIAN
000049                  VALUES ' ' '1' 'I' '8' 'G'.
000050         88  JULIAN-TO-BINARY
000051                  VALUES '5' 'C' 'E' 'F'.
000052     12  DC-ERROR-CODE                 PIC X.
000053         88  NO-CONVERSION-ERROR        VALUE ' '.
000054         88  DATE-CONVERSION-ERROR
000055                  VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
000056         88  DATE-IS-ZERO               VALUE '1'.
000057         88  DATE-IS-NON-NUMERIC        VALUE '2'.
000058         88  DATE-IS-INVALID            VALUE '3'.
000059         88  DATE1-GREATER-DATE2        VALUE '4'.
000060         88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
000061         88  DATE-INVALID-OPTION        VALUE '9'.
000062         88  INVALID-CENTURY            VALUE 'A'.
000063         88  ONLY-CENTURY               VALUE 'B'.
000064         88  ONLY-LEAP-YEAR             VALUE 'C'.
000065         88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
000066     12  DC-END-OF-MONTH               PIC X.
000067         88  CALCULATE-END-OF-MONTH     VALUE '1'.
000068     12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
000069         88  USE-NORMAL-PROCESS         VALUE ' '.
000070         88  ADJUST-DOWN-100-YRS        VALUE '1'.
000071         88  ADJUST-UP-100-YRS          VALUE '2'.
000072     12  FILLER                        PIC X.
000073     12  DC-CONVERSION-DATES.
000074         16  DC-BIN-DATE-1             PIC XX.
000075         16  DC-BIN-DATE-2             PIC XX.
000076         16  DC-GREG-DATE-1-EDIT       PIC X(08).
000077         16  DC-GREG-DATE-1-EDIT-R REDEFINES
000078                       DC-GREG-DATE-1-EDIT.
000079             20  DC-EDIT1-MONTH        PIC 99.
000080             20  SLASH1-1              PIC X.
000081             20  DC-EDIT1-DAY          PIC 99.
000082             20  SLASH1-2              PIC X.
000083             20  DC-EDIT1-YEAR         PIC 99.
000084         16  DC-GREG-DATE-2-EDIT       PIC X(08).
000085         16  DC-GREG-DATE-2-EDIT-R REDEFINES
000086                     DC-GREG-DATE-2-EDIT.
000087             20  DC-EDIT2-MONTH        PIC 99.
000088             20  SLASH2-1              PIC X.
000089             20  DC-EDIT2-DAY          PIC 99.
000090             20  SLASH2-2              PIC X.
000091             20  DC-EDIT2-YEAR         PIC 99.
000092         16  DC-GREG-DATE-1-YMD        PIC 9(06).
000093         16  DC-GREG-DATE-1-YMD-R  REDEFINES
000094                     DC-GREG-DATE-1-YMD.
000095             20  DC-YMD-YEAR           PIC 99.
000096             20  DC-YMD-MONTH          PIC 99.
000097             20  DC-YMD-DAY            PIC 99.
000098         16  DC-GREG-DATE-1-MDY        PIC 9(06).
000099         16  DC-GREG-DATE-1-MDY-R REDEFINES
000100                      DC-GREG-DATE-1-MDY.
000101             20  DC-MDY-MONTH          PIC 99.
000102             20  DC-MDY-DAY            PIC 99.
000103             20  DC-MDY-YEAR           PIC 99.
000104         16  DC-GREG-DATE-1-ALPHA.
000105             20  DC-ALPHA-MONTH        PIC X(10).
000106             20  DC-ALPHA-DAY          PIC 99.
000107             20  FILLER                PIC XX.
000108             20  DC-ALPHA-CENTURY.
000109                 24 DC-ALPHA-CEN-N     PIC 99.
000110             20  DC-ALPHA-YEAR         PIC 99.
000111         16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
000112         16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
000113         16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
000114         16  DC-JULIAN-DATE            PIC 9(05).
000115         16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
000116                                       PIC 9(05).
000117         16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
000118             20  DC-JULIAN-YEAR        PIC 99.
000119             20  DC-JULIAN-DAYS        PIC 999.
000120         16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
000121         16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
000122         16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
000123     12  DATE-CONVERSION-VARIBLES.
000124         16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
000125         16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
000126             20  FILLER                PIC 9(3).
000127             20  HOLD-CEN-1-CCYY.
000128                 24  HOLD-CEN-1-CC     PIC 99.
000129                 24  HOLD-CEN-1-YY     PIC 99.
000130             20  HOLD-CEN-1-MO         PIC 99.
000131             20  HOLD-CEN-1-DA         PIC 99.
000132         16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
000133             20  HOLD-CEN-1-R-MO       PIC 99.
000134             20  HOLD-CEN-1-R-DA       PIC 99.
000135             20  HOLD-CEN-1-R-CCYY.
000136                 24  HOLD-CEN-1-R-CC   PIC 99.
000137                 24  HOLD-CEN-1-R-YY   PIC 99.
000138             20  FILLER                PIC 9(3).
000139         16  HOLD-CENTURY-1-X.
000140             20  FILLER                PIC X(3)  VALUE SPACES.
000141             20  HOLD-CEN-1-X-CCYY.
000142                 24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
000143                 24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
000144             20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
000145             20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
000146         16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
000147             20  HOLD-CEN-1-R-X-MO     PIC XX.
000148             20  HOLD-CEN-1-R-X-DA     PIC XX.
000149             20  HOLD-CEN-1-R-X-CCYY.
000150                 24  HOLD-CEN-1-R-X-CC PIC XX.
000151                 24  HOLD-CEN-1-R-X-YY PIC XX.
000152             20  FILLER                PIC XXX.
000153         16  DC-BIN-DATE-EXPAND-1      PIC XXX.
000154         16  DC-BIN-DATE-EXPAND-2      PIC XXX.
000155         16  DC-JULIAN-DATE-1          PIC 9(07).
000156         16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
000157             20  DC-JULIAN-1-CCYY.
000158                 24  DC-JULIAN-1-CC    PIC 99.
000159                 24  DC-JULIAN-1-YR    PIC 99.
000160             20  DC-JULIAN-DA-1        PIC 999.
000161         16  DC-JULIAN-DATE-2          PIC 9(07).
000162         16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
000163             20  DC-JULIAN-2-CCYY.
000164                 24  DC-JULIAN-2-CC    PIC 99.
000165                 24  DC-JULIAN-2-YR    PIC 99.
000166             20  DC-JULIAN-DA-2        PIC 999.
000167         16  DC-GREG-DATE-A-EDIT.
000168             20  DC-EDITA-MONTH        PIC 99.
000169             20  SLASHA-1              PIC X VALUE '/'.
000170             20  DC-EDITA-DAY          PIC 99.
000171             20  SLASHA-2              PIC X VALUE '/'.
000172             20  DC-EDITA-CCYY.
000173                 24  DC-EDITA-CENT     PIC 99.
000174                 24  DC-EDITA-YEAR     PIC 99.
000175         16  DC-GREG-DATE-B-EDIT.
000176             20  DC-EDITB-MONTH        PIC 99.
000177             20  SLASHB-1              PIC X VALUE '/'.
000178             20  DC-EDITB-DAY          PIC 99.
000179             20  SLASHB-2              PIC X VALUE '/'.
000180             20  DC-EDITB-CCYY.
000181                 24  DC-EDITB-CENT     PIC 99.
000182                 24  DC-EDITB-YEAR     PIC 99.
000183         16  DC-GREG-DATE-CYMD         PIC 9(08).
000184         16  DC-GREG-DATE-CYMD-R REDEFINES
000185                              DC-GREG-DATE-CYMD.
000186             20  DC-CYMD-CEN           PIC 99.
000187             20  DC-CYMD-YEAR          PIC 99.
000188             20  DC-CYMD-MONTH         PIC 99.
000189             20  DC-CYMD-DAY           PIC 99.
000190         16  DC-GREG-DATE-MDCY         PIC 9(08).
000191         16  DC-GREG-DATE-MDCY-R REDEFINES
000192                              DC-GREG-DATE-MDCY.
000193             20  DC-MDCY-MONTH         PIC 99.
000194             20  DC-MDCY-DAY           PIC 99.
000195             20  DC-MDCY-CEN           PIC 99.
000196             20  DC-MDCY-YEAR          PIC 99.
000197    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
000198        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
000199    12  DC-EL310-DATE                  PIC X(21).
000200    12  FILLER                         PIC X(28).
      *<<((file: ELCDATE))
000329 EJECT
000330*                                    COPY ELCLOGOF.
      *>>((file: ELCLOGOF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCLOGOF.                           *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
000008*                                                                *
000009******************************************************************
000010 01  CLASIC-LOGOFF.
000011     12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
000012     12  LOGOFF-TEXT.
000013         16  FILLER          PIC X(5)    VALUE SPACES.
000014         16  LOGOFF-MSG.
000015             20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
000016             20  FILLER      PIC X       VALUE SPACES.
000017             20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
000018         16  FILLER          PIC X(80)
000019           VALUE '* YOU ARE NOW LOGGED OFF'.
000020         16  FILLER          PIC X(7)    VALUE '* LOGIC'.
000021         16  FILLER          PIC X       VALUE QUOTE.
000022         16  LOGOFF-SYS-MSG  PIC X(17)
000023           VALUE 'S CLAS-IC SYSTEM '.
000024     12  TEXT-MESSAGES.
000025         16  UNACCESS-MSG    PIC X(29)
000026             VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
000027         16  PGMIDERR-MSG    PIC X(17)
000028             VALUE 'PROGRAM NOT FOUND'.
      *<<((file: ELCLOGOF))
000331 EJECT
000332*                                    COPY ELCATTR.
      *>>((file: ELCATTR))
000001******************************************************************
000002*                                                                *
000003*                            ELCATTR.                            *
000004*                            VMOD=2.001                          *
000005*                                                                *
000006*             LIST OF STANDARD ATTRIBUTE VALUES                  *
000007*                                                                *
000008*   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
000009*                                                                *
000010*                   POS 1   P=PROTECTED                          *
000011*                           U=UNPROTECTED                        *
000012*                           S=ASKIP                              *
000013*                   POS 2   A=ALPHA/NUMERIC                      *
000014*                           N=NUMERIC                            *
000015*                   POS 3   N=NORMAL                             *
000016*                           B=BRIGHT                             *
000017*                           D=DARK                               *
000018*                   POS 4-5 ON=MODIFIED DATA TAG ON              *
000019*                           OF=MODIFIED DATA TAG OFF             *
000020*                                                                *
000021*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
000022******************************************************************
000023 01  ATTRIBUTE-LIST.
000024     12  AL-PABOF            PIC X       VALUE 'Y'.
000025     12  AL-PABON            PIC X       VALUE 'Z'.
000026     12  AL-PADOF            PIC X       VALUE '%'.
000027     12  AL-PADON            PIC X       VALUE '_'.
000028     12  AL-PANOF            PIC X       VALUE '-'.
000029     12  AL-PANON            PIC X       VALUE '/'.
000030     12  AL-SABOF            PIC X       VALUE '8'.
000031     12  AL-SABON            PIC X       VALUE '9'.
000032     12  AL-SADOF            PIC X       VALUE '@'.
000033     12  AL-SADON            PIC X       VALUE QUOTE.
000034     12  AL-SANOF            PIC X       VALUE '0'.
000035     12  AL-SANON            PIC X       VALUE '1'.
000036     12  AL-UABOF            PIC X       VALUE 'H'.
000037     12  AL-UABON            PIC X       VALUE 'I'.
000038     12  AL-UADOF            PIC X       VALUE '<'.
000039     12  AL-UADON            PIC X       VALUE '('.
000040     12  AL-UANOF            PIC X       VALUE ' '.
000041     12  AL-UANON            PIC X       VALUE 'A'.
000042     12  AL-UNBOF            PIC X       VALUE 'Q'.
000043     12  AL-UNBON            PIC X       VALUE 'R'.
000044     12  AL-UNDOF            PIC X       VALUE '*'.
000045     12  AL-UNDON            PIC X       VALUE ')'.
000046     12  AL-UNNOF            PIC X       VALUE '&'.
000047     12  AL-UNNON            PIC X       VALUE 'J'.
      *<<((file: ELCATTR))
000333 EJECT
000334*                                    COPY ELCEMIB.
      *>>((file: ELCEMIB))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCEMIB.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.005                          *
000007*                                                                *
000008*    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
000009*                                                                *
000010******************************************************************
000011 01  ERROR-MESSAGE-INTERFACE-BLOCK.
000012     12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
000013     12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
000014     12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
000015     12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
000016     12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
000017     12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
000018     12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
000019     12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
000020     12  EMI-SWITCH1             PIC X        VALUE '1'.
000021         88  EMI-NO-ERRORS                    VALUE '1'.
000022         88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
000023         88  EMI-ERRORS-COMPLETE              VALUE '3'.
000024     12  EMI-SWITCH2             PIC X        VALUE '1'.
000025         88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
000026     12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
000027         88  EMI-AREA1-EMPTY                  VALUE '1'.
000028         88  EMI-AREA1-FULL                   VALUE '2'.
000029     12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
000030         88  EMI-AREA2-EMPTY                  VALUE '1'.
000031         88  EMI-AREA2-FULL                   VALUE '2'.
000032     12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
000033         88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
000034         88  EMI-BYPASS-NOTES                 VALUE 'N'.
000035         88  EMI-BYPASS-WARNINGS              VALUE 'W'.
000036         88  EMI-BYPASS-FORCABLES             VALUE 'F'.
000037         88  EMI-BYPASS-FATALS                VALUE 'X'.
000038     12  EMI-ERROR-LINES.
000039         16  EMI-LINE1           PIC X(72)   VALUE SPACES.
000040         16  EMI-LINE2           PIC X(72)   VALUE SPACES.
000041         16  EMI-LINE3           PIC X(72)   VALUE SPACES.
000042         16  EMI-CODE-LINE REDEFINES EMI-LINE3.
000043             20  EMI-ERR-CODES OCCURS 10 TIMES.
000044                 24  EMI-ERR-NUM         PIC X(4).
000045                 24  EMI-FILLER          PIC X.
000046                 24  EMI-SEV             PIC X.
000047                 24  FILLER              PIC X.
000048             20  FILLER                  PIC X(02).
000049     12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
000050         16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
000051             20  EMI-ERROR-NUMBER    PIC X(4).
000052             20  EMI-FILL            PIC X.
000053             20  EMI-SEVERITY        PIC X.
000054             20  FILLER              PIC X.
000055             20  EMI-ERROR-TEXT.
000056                 24  EMI-TEXT-VARIABLE   PIC X(10).
000057                 24  FILLER          PIC X(55).
000058     12  EMI-SEVERITY-SAVE           PIC X.
000059         88  EMI-NOTE                    VALUE 'N'.
000060         88  EMI-WARNING                 VALUE 'W'.
000061         88  EMI-FORCABLE                VALUE 'F'.
000062         88  EMI-FATAL                   VALUE 'X'.
000063     12  EMI-MESSAGE-FLAG            PIC X.
000064         88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
000065         88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
000066     12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
000067     12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
000068         88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
000069         88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
000070         88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
000071     12  emi-claim-no                pic x(7).
000072     12  emi-claim-type              pic x(6).
000073     12  FILLER                      PIC X(124)  VALUE SPACES.
000074     12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
000075     12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
000076     12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
000077     12  EMI-AH-OVERRIDE-L6          PIC X(6).
      *<<((file: ELCEMIB))
000335 EJECT
000336*                                    COPY ELCINTF.
      *>>((file: ELCINTF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCINTF.                            *
000005*                            VMOD=2.017                          *
000006*                                                                *
000007*   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
000008*                                                                *
000009*       LENGTH = 1024                                            *
000010*                                                                *
000011******************************************************************
000012*                   C H A N G E   L O G
000013*
000014* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000015*-----------------------------------------------------------------
000016*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000017* EFFECTIVE    NUMBER
000018*-----------------------------------------------------------------
000019* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
000020******************************************************************
000021 01  PROGRAM-INTERFACE-BLOCK.
000022     12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
000023     12  PI-CALLING-PROGRAM              PIC X(8).
000024     12  PI-SAVED-PROGRAM-1              PIC X(8).
000025     12  PI-SAVED-PROGRAM-2              PIC X(8).
000026     12  PI-SAVED-PROGRAM-3              PIC X(8).
000027     12  PI-SAVED-PROGRAM-4              PIC X(8).
000028     12  PI-SAVED-PROGRAM-5              PIC X(8).
000029     12  PI-SAVED-PROGRAM-6              PIC X(8).
000030     12  PI-RETURN-TO-PROGRAM            PIC X(8).
000031     12  PI-COMPANY-ID                   PIC XXX.
000032     12  PI-COMPANY-CD                   PIC X.
000033
000034     12  PI-COMPANY-PASSWORD             PIC X(8).
000035
000036     12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
000037
000038     12  PI-CONTROL-IN-PROGRESS.
000039         16  PI-CARRIER                  PIC X.
000040         16  PI-GROUPING                 PIC X(6).
000041         16  PI-STATE                    PIC XX.
000042         16  PI-ACCOUNT                  PIC X(10).
000043         16  PI-PRODUCER REDEFINES PI-ACCOUNT
000044                                         PIC X(10).
000045         16  PI-CLAIM-CERT-GRP.
000046             20  PI-CLAIM-NO             PIC X(7).
000047             20  PI-CERT-NO.
000048                 25  PI-CERT-PRIME       PIC X(10).
000049                 25  PI-CERT-SFX         PIC X.
000050             20  PI-CERT-EFF-DT          PIC XX.
000051         16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
000052             20  PI-PLAN-CODE            PIC X(2).
000053             20  PI-REVISION-NUMBER      PIC X(3).
000054             20  PI-PLAN-EFF-DT          PIC X(2).
000055             20  PI-PLAN-EXP-DT          PIC X(2).
000056             20  FILLER                  PIC X(11).
000057         16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
000058             20  PI-OE-REFERENCE-1.
000059                 25  PI-OE-REF-1-PRIME   PIC X(18).
000060                 25  PI-OE-REF-1-SUFF    PIC XX.
000061
000062     12  PI-SESSION-IN-PROGRESS          PIC X.
000063         88  CLAIM-SESSION                   VALUE '1'.
000064         88  CREDIT-SESSION                  VALUE '2'.
000065         88  WARRANTY-SESSION                VALUE '3'.
000066         88  MORTGAGE-SESSION                VALUE '4'.
000067         88  GENERAL-LEDGER-SESSION          VALUE '5'.
000068
000069
000070*THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
000071
000072     12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
000073     12  PI-ORIGINAL-COMPANY-CD          PIC X.
000074
000075     12  PI-CREDIT-USER                  PIC X.
000076         88  PI-NOT-CREDIT-USER              VALUE 'N'.
000077         88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
000078
000079     12  PI-CLAIM-USER                   PIC X.
000080         88  PI-NOT-CLAIM-USER               VALUE 'N'.
000081         88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
000082
000083     12  PI-PROCESSOR-SYS-ACCESS         PIC X.
000084         88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
000085         88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
000086         88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
000087         88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
000088         88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
000089
000090     12  PI-PROCESSOR-ID                 PIC X(4).
000091
000092     12  PI-PROCESSOR-PASSWORD           PIC X(11).
000093
000094     12  PI-MEMBER-CAPTION               PIC X(10).
000095
000096     12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
000097         88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
000098
000099     12  PI-LIFE-OVERRIDE-L1             PIC X.
000100     12  PI-LIFE-OVERRIDE-L2             PIC XX.
000101     12  PI-LIFE-OVERRIDE-L6             PIC X(6).
000102     12  PI-LIFE-OVERRIDE-L12            PIC X(12).
000103
000104     12  PI-AH-OVERRIDE-L1               PIC X.
000105     12  PI-AH-OVERRIDE-L2               PIC XX.
000106     12  PI-AH-OVERRIDE-L6               PIC X(6).
000107     12  PI-AH-OVERRIDE-L12              PIC X(12).
000108
000109     12  PI-NEW-SYSTEM                   PIC X(2).
000110
000111     12  PI-PRIMARY-CERT-NO              PIC X(11).
000112     12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
000113         88  PI-USES-PAID-TO                 VALUE '1'.
000114     12  PI-CRDTCRD-SYSTEM.
000115         16  PI-CRDTCRD-USER             PIC X.
000116             88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
000117             88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
000118         16  PI-CC-MONTH-END-DT          PIC XX.
000119     12  PI-PROCESSOR-PRINTER            PIC X(4).
000120
000121     12  PI-OE-REFERENCE-2.
000122         16  PI-OE-REF-2-PRIME           PIC X(10).
000123         16  PI-OE-REF-2-SUFF            PIC X.
000124
000125     12  PI-REM-TRM-CALC-OPTION          PIC X.
000126
000127     12  PI-LANGUAGE-TYPE                PIC X.
000128             88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
000129             88  PI-LANGUAGE-IS-FR           VALUE 'F'.
000130             88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
000131
000132     12  PI-POLICY-LINKAGE-IND           PIC X.
000133         88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
000134         88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
000135                                                   LOW-VALUES.
000136
000137     12  PI-ALT-DMD-PRT-ID               PIC X(4).
000138     12  PI-CLAIM-PW-SESSION             PIC X(1).
000139         88  PI-CLAIM-CREDIT                 VALUE '1'.
000140         88  PI-CLAIM-CONVEN                 VALUE '2'.
000141
000142     12  PI-PROCESSOR-CSR-IND            PIC X.
000143         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
000144         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
000145
000146     12  FILLER                          PIC X(3).
000147
000148     12  PI-SYSTEM-LEVEL                 PIC X(145).
000149
000150     12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
000151         PI-SYSTEM-LEVEL.
000152
000153         16  PI-ENTRY-CODES.
000154             20  PI-ENTRY-CD-1           PIC X.
000155             20  PI-ENTRY-CD-2           PIC X.
000156
000157         16  PI-RETURN-CODES.
000158             20  PI-RETURN-CD-1          PIC X.
000159             20  PI-RETURN-CD-2          PIC X.
000160
000161         16  PI-UPDATE-STATUS-SAVE.
000162             20  PI-UPDATE-BY            PIC X(4).
000163             20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
000164
000165         16  PI-LOWER-CASE-LETTERS       PIC X.
000166             88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
000167
000168*        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
000169*            88  CLAIM-NO-UNIQUE             VALUE '1'.
000170*            88  CARRIER-CLM-CNTL            VALUE '2'.
000171
000172         16  PI-CERT-ACCESS-CONTROL      PIC X.
000173             88  ST-ACCNT-CNTL               VALUE ' '.
000174             88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
000175             88  CARR-ST-ACCNT-CNTL          VALUE '2'.
000176             88  ACCNT-CNTL                  VALUE '3'.
000177             88  CARR-ACCNT-CNTL             VALUE '4'.
000178
000179         16  PI-PROCESSOR-CAP-LIST.
000180             20  PI-SYSTEM-CONTROLS.
000181                24 PI-SYSTEM-DISPLAY     PIC X.
000182                 88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
000183                24 PI-SYSTEM-MODIFY      PIC X.
000184                 88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
000185             20  FILLER                  PIC XX.
000186             20  PI-DISPLAY-CAP          PIC X.
000187                 88  DISPLAY-CAP             VALUE 'Y'.
000188             20  PI-MODIFY-CAP           PIC X.
000189                 88  MODIFY-CAP              VALUE 'Y'.
000190             20  PI-MSG-AT-LOGON-CAP     PIC X.
000191                 88  MSG-AT-LOGON-CAP        VALUE 'Y'.
000192             20  PI-FORCE-CAP            PIC X.
000193                 88  FORCE-CAP               VALUE 'Y'.
000194
000195         16  PI-PROGRAM-CONTROLS.
000196             20  PI-PGM-PRINT-OPT        PIC X.
000197             20  PI-PGM-FORMAT-OPT       PIC X.
000198             20  PI-PGM-PROCESS-OPT      PIC X.
000199             20  PI-PGM-TOTALS-OPT       PIC X.
000200
000201         16  PI-HELP-INTERFACE.
000202             20  PI-LAST-ERROR-NO        PIC X(4).
000203             20  PI-CURRENT-SCREEN-NO    PIC X(4).
000204
000205         16  PI-CARRIER-CONTROL-LEVEL    PIC X.
000206             88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
000207
000208         16  PI-CR-CONTROL-IN-PROGRESS.
000209             20  PI-CR-CARRIER           PIC X.
000210             20  PI-CR-GROUPING          PIC X(6).
000211             20  PI-CR-STATE             PIC XX.
000212             20  PI-CR-ACCOUNT           PIC X(10).
000213             20  PI-CR-FIN-RESP          PIC X(10).
000214             20  PI-CR-TYPE              PIC X.
000215
000216         16  PI-CR-BATCH-NUMBER          PIC X(6).
000217
000218         16  PI-CR-MONTH-END-DT          PIC XX.
000219
000220         16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
000221             88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
000222             88  PI-ZERO-CARRIER             VALUE '1'.
000223             88  PI-ZERO-GROUPING            VALUE '2'.
000224             88  PI-ZERO-CAR-GROUP           VALUE '3'.
000225
000226         16  PI-CARRIER-SECURITY         PIC X.
000227             88  PI-NO-CARRIER-SECURITY      VALUE ' '.
000228
000229         16  PI-ACCOUNT-SECURITY         PIC X(10).
000230             88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
000231             88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
000232
000233         16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
000234             20  PI-ACCESS-CODE          OCCURS 10 TIMES
000235                                         INDEXED BY PI-ACCESS-NDX
000236                                         PIC X.
000237
000238         16  PI-GA-BILLING-CONTROL       PIC X.
000239             88  PI-GA-BILLING               VALUE '1'.
000240
000241         16  PI-MAIL-PROCESSING          PIC X.
000242             88  PI-MAIL-YES                 VALUE 'Y'.
000243
000244         16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
000245
000246         16  PI-AR-SYSTEM.
000247             20  PI-AR-PROCESSING-CNTL   PIC X.
000248                 88  PI-AR-PROCESSING        VALUE 'Y'.
000249             20  PI-AR-SUMMARY-CODE      PIC X(6).
000250             20  PI-AR-MONTH-END-DT      PIC XX.
000251
000252         16  PI-MP-SYSTEM.
000253             20  PI-MORTGAGE-USER            PIC X.
000254                 88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
000255                 88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
000256             20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
000257                 88  PI-MP-ST-PROD-CNTL              VALUE ' '.
000258                 88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
000259                 88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
000260                 88  PI-MP-PROD-CNTL                 VALUE '3'.
000261                 88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
000262             20  PI-MP-MONTH-END-DT          PIC XX.
000263             20  PI-MP-REFERENCE-NO.
000264                 24  PI-MP-REFERENCE-PRIME   PIC X(18).
000265                 24  PI-MP-REFERENCE-SFX     PIC XX.
000266
000267         16  PI-LABEL-CONTROL            PIC X(01).
000268             88  PI-CREATE-LABELS                    VALUE 'Y'.
000269             88  PI-BYPASS-LABELS                    VALUE 'N'.
000270
000271         16  PI-BILL-GROUPING-CODE       PIC X(01).
000272             88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
000273
000274         16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
000275             88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
000276             88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
000277
000278         16  FILLER                      PIC X(14).
000279
000280     12  PI-PROGRAM-WORK-AREA            PIC X(640).
000281******************************************************************
      *<<((file: ELCINTF))
000337     12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
000338         16  PI-PYAJ-FILE-SW             PIC  X.
000339             88  END-OF-ACCT                 VALUE 'A'.
000340             88  END-OF-ACCT-FULL-PAGE       VALUE 'B'.
000341             88  END-OF-FILE                 VALUE 'X'.
000342             88  TOP-OF-FILE                 VALUE 'T'.
000343             88  PAGE-FULL                   VALUE 'F'.
000344             88  NO-RECORDS                  VALUE 'Y'.
000345             88  NOT-OPEN                    VALUE 'Z'.
000346         16  PI-PREV-FUNCTION            PIC  X.
000347         16  PI-SAV-FUNCTION             PIC  X.
000348         16  PI-PREV-PFKEY               PIC  X.
000349         16  PI-SEQ-NOS.
000350             20  FILLER  OCCURS 13 TIMES
000351                             INDEXED BY NDX.
000352                 24  PI-REC-TYPE         PIC  X.
000353                 24  PI-FILE-SEQ-NO      PIC  S9(8)       COMP.
000354         16  PI-SAV-ENDING-PYAJ-KEY.
000355             20  PI-SAV-COMP-CD          PIC  X.
000356             20  PI-SAV-COMP-CONTROL.
000357                 24  PI-SAV-CARRIER      PIC  X.
000358                 24  PI-SAV-GROUPING     PIC  X(6).
000359                 24  PI-SAV-FIN-RESP     PIC  X(10).
000360                 24  PI-SAV-ACCOUNT      PIC  X(10).
000361                 24  PI-SAV-FILE-SEQ-NO  PIC  S9(8)          COMP.
000362                 24  PI-SAV-RECORD-TYPE  PIC  X.
000363         16  PI-START-PYAJ-KEY           PIC  X(33).
000364         16  PI-SAV-ACCT-AMT             PIC  S9(7)V99.
000365         16  PI-SAV-ACCT-NET             PIC  S9(7)V99.
000366         16  PI-SAV-PREV-AMT             PIC  S9(7)V99.
000367         16  PI-SAV-PREV-NET             PIC  S9(7)V99.
000368         16  PI-TOTAL-DISPLAYED-SW       PIC  X.
000369             88  PI-TOTAL-DISPLAYED               VALUE 'Y'.
000370         16  PI-FULL-INDX                PIC  S9(4)      COMP.
000371         16  PI-FRST-FILE-SEQ-NO         PIC  S9(8)          COMP.
000372         16  PI-FRST-RECORD-TYPE         PIC  X.
000373         16  PI-PREV-FILE-SEQ-NO         PIC  S9(8)          COMP.
000374         16  PI-PREV-RECORD-TYPE         PIC  X.
000375         16  PI-LAST-FILE-SEQ-NO         PIC  S9(8)          COMP.
000376         16  PI-LAST-RECORD-TYPE         PIC  X.
000377         16  PI-PAGE-SW                  PIC  X.
000378             88  FIRST-PAGE                          VALUE 'Y'.
000379         16  FILLER                      PIC  X(450).
000380 EJECT
000381*                            COPY ELCJPFX.
      *>>((file: ELCJPFX))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCJPFX.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.002                          *
000007*                                                                *
000008*    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
000009*                                                                *
000010*     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
000011*     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
000012*        ELCNTL - CONTROL FILE                                   *
000013*        ELMSTR - CLAIM MASTERS                                  *
000014*        ELTRLR - ACTIVITY TRAILERS                              *
000015*        ELCHKQ - CHECK QUE                                      *
000016******************************************************************
000017 01  JOURNAL-RECORD.
000018     12  jp-date                     pic s9(5) comp-3.
000019     12  jp-time                     pic s9(7) comp-3.
000020     12  JP-USER-ID                  PIC X(4).
000021     12  JP-FILE-ID                  PIC X(8).
000022     12  JP-PROGRAM-ID               PIC X(8).
000023     12  JP-RECORD-TYPE              PIC X.
000024         88 JP-ADD              VALUE 'A'.
000025         88 JP-BEFORE-CHANGE    VALUE 'B'.
000026         88 JP-AFTER-CHANGE     VALUE 'C'.
000027         88 JP-DELETE           VALUE 'D'.
000028         88 JP-GENERIC-DELETE   VALUE 'G'.
000029         88 JP-KEY-CHG-DELETE   VALUE 'K'.
000030         88 JP-KEY-CHG-ADD      VALUE 'N'.
000031     12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
000032     12  JP-RECORD-AREA
000033
000034
      *<<((file: ELCJPFX))
000382                             PIC  X(223).
000383 EJECT
000384*                            COPY ELCAID.
      *>>((file: ELCAID))
000001******************************************************************
000002*                                                                *
000003*                            ELCAID.                             *
000004*                            VMOD=2.001                          *
000005*                                                                *
000006*   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
000007*                                                                *
000008*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
000009*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
000010******************************************************************
000011
000012 01  DFHAID.
000013   02  DFHNULL   PIC  X  VALUE  ' '.
000014   02  DFHENTER  PIC  X  VALUE  QUOTE.
000015   02  DFHCLEAR  PIC  X  VALUE  '_'.
000016   02  DFHPEN    PIC  X  VALUE  '='.
000017   02  DFHOPID   PIC  X  VALUE  'W'.
000018   02  DFHPA1    PIC  X  VALUE  '%'.
000019   02  DFHPA2    PIC  X  VALUE  '>'.
000020   02  DFHPA3    PIC  X  VALUE  ','.
000021   02  DFHPF1    PIC  X  VALUE  '1'.
000022   02  DFHPF2    PIC  X  VALUE  '2'.
000023   02  DFHPF3    PIC  X  VALUE  '3'.
000024   02  DFHPF4    PIC  X  VALUE  '4'.
000025   02  DFHPF5    PIC  X  VALUE  '5'.
000026   02  DFHPF6    PIC  X  VALUE  '6'.
000027   02  DFHPF7    PIC  X  VALUE  '7'.
000028   02  DFHPF8    PIC  X  VALUE  '8'.
000029   02  DFHPF9    PIC  X  VALUE  '9'.
000030   02  DFHPF10   PIC  X  VALUE  ':'.
000031   02  DFHPF11   PIC  X  VALUE  '#'.
000032   02  DFHPF12   PIC  X  VALUE  '@'.
000033   02  DFHPF13   PIC  X  VALUE  'A'.
000034   02  DFHPF14   PIC  X  VALUE  'B'.
000035   02  DFHPF15   PIC  X  VALUE  'C'.
000036   02  DFHPF16   PIC  X  VALUE  'D'.
000037   02  DFHPF17   PIC  X  VALUE  'E'.
000038   02  DFHPF18   PIC  X  VALUE  'F'.
000039   02  DFHPF19   PIC  X  VALUE  'G'.
000040   02  DFHPF20   PIC  X  VALUE  'H'.
000041   02  DFHPF21   PIC  X  VALUE  'I'.
000042*00039    02  DFHPF22   PIC  X  VALUE  'Õ'.
000043   02  DFHPF22   PIC  X  VALUE  '['.
000044   02  DFHPF23   PIC  X  VALUE  '.'.
000045   02  DFHPF24   PIC  X  VALUE  '<'.
000046   02  DFHMSRE   PIC  X  VALUE  'X'.
000047   02  DFHSTRF   PIC  X  VALUE  'h'.
000048   02  DFHTRIG   PIC  X  VALUE  '"'.
      *<<((file: ELCAID))
000385
000386 01  FILLER  REDEFINES  DFHAID.
000387     12  FILLER              PIC  X(8).
000388     12  PF-VALUES           PIC  X          OCCURS 2 TIMES.
000389 EJECT
000390*                            COPY EL633S.
      *>>((file: EL633S))
000001 01  EL633AI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  DATEL PIC S9(0004) COMP.
000005     05  DATEF PIC  X(0001).
000006     05  FILLER REDEFINES DATEF.
000007         10  DATEA PIC  X(0001).
000008     05  DATEI PIC  X(0008).
000009*    -------------------------------
000010     05  TIMEL PIC S9(0004) COMP.
000011     05  TIMEF PIC  X(0001).
000012     05  FILLER REDEFINES TIMEF.
000013         10  TIMEA PIC  X(0001).
000014     05  TIMEI PIC  X(0005).
000015*    -------------------------------
000016     05  CMPNYIDL PIC S9(0004) COMP.
000017     05  CMPNYIDF PIC  X(0001).
000018     05  FILLER REDEFINES CMPNYIDF.
000019         10  CMPNYIDA PIC  X(0001).
000020     05  CMPNYIDI PIC  X(0003).
000021*    -------------------------------
000022     05  USERIDL PIC S9(0004) COMP.
000023     05  USERIDF PIC  X(0001).
000024     05  FILLER REDEFINES USERIDF.
000025         10  USERIDA PIC  X(0001).
000026     05  USERIDI PIC  X(0004).
000027*    -------------------------------
000028     05  MAINTL PIC S9(0004) COMP.
000029     05  MAINTF PIC  X(0001).
000030     05  FILLER REDEFINES MAINTF.
000031         10  MAINTA PIC  X(0001).
000032     05  MAINTI PIC  X(0001).
000033*    -------------------------------
000034     05  CARRIERL PIC S9(0004) COMP.
000035     05  CARRIERF PIC  X(0001).
000036     05  FILLER REDEFINES CARRIERF.
000037         10  CARRIERA PIC  X(0001).
000038     05  CARRIERI PIC  X(0001).
000039*    -------------------------------
000040     05  GROUPL PIC S9(0004) COMP.
000041     05  GROUPF PIC  X(0001).
000042     05  FILLER REDEFINES GROUPF.
000043         10  GROUPA PIC  X(0001).
000044     05  GROUPI PIC  X(0006).
000045*    -------------------------------
000046     05  FINRESPL PIC S9(0004) COMP.
000047     05  FINRESPF PIC  X(0001).
000048     05  FILLER REDEFINES FINRESPF.
000049         10  FINRESPA PIC  X(0001).
000050     05  FINRESPI PIC  X(0010).
000051*    -------------------------------
000052     05  ACCTL PIC S9(0004) COMP.
000053     05  ACCTF PIC  X(0001).
000054     05  FILLER REDEFINES ACCTF.
000055         10  ACCTA PIC  X(0001).
000056     05  ACCTI PIC  X(0010).
000057*    -------------------------------
000058     05  MSAAC1L PIC S9(0004) COMP.
000059     05  MSAAC1F PIC  X(0001).
000060     05  FILLER REDEFINES MSAAC1F.
000061         10  MSAAC1A PIC  X(0001).
000062     05  MSAAC1I PIC  X(0010).
000063*    -------------------------------
000064     05  MSAST1L PIC S9(0004) COMP.
000065     05  MSAST1F PIC  X(0001).
000066     05  FILLER REDEFINES MSAST1F.
000067         10  MSAST1A PIC  X(0001).
000068     05  MSAST1I PIC  X(0002).
000069*    -------------------------------
000070     05  MSACN1L PIC S9(0004) COMP.
000071     05  MSACN1F PIC  X(0001).
000072     05  FILLER REDEFINES MSACN1F.
000073         10  MSACN1A PIC  X(0001).
000074     05  MSACN1I PIC  X(0001).
000075*    -------------------------------
000076     05  MSACM1L PIC S9(0004) COMP.
000077     05  MSACM1F PIC  X(0001).
000078     05  FILLER REDEFINES MSACM1F.
000079         10  MSACM1A PIC  X(0001).
000080     05  MSACM1I PIC  X(0010).
000081*    -------------------------------
000082     05  TYPE1L PIC S9(0004) COMP.
000083     05  TYPE1F PIC  X(0001).
000084     05  FILLER REDEFINES TYPE1F.
000085         10  TYPE1A PIC  X(0001).
000086     05  TYPE1I PIC  X(0001).
000087*    -------------------------------
000088     05  AMT1L PIC S9(0004) COMP.
000089     05  AMT1F PIC  X(0001).
000090     05  FILLER REDEFINES AMT1F.
000091         10  AMT1A PIC  X(0001).
000092     05  AMT1I PIC  X(0011).
000093*    -------------------------------
000094     05  VOID1L PIC S9(0004) COMP.
000095     05  VOID1F PIC  X(0001).
000096     05  FILLER REDEFINES VOID1F.
000097         10  VOID1A PIC  X(0001).
000098     05  VOID1I PIC  X(0001).
000099*    -------------------------------
000100     05  MDT1L PIC S9(0004) COMP.
000101     05  MDT1F PIC  X(0001).
000102     05  FILLER REDEFINES MDT1F.
000103         10  MDT1A PIC  X(0001).
000104     05  MDT1I PIC  X(0006).
000105*    -------------------------------
000106     05  BDT1L PIC S9(0004) COMP.
000107     05  BDT1F PIC  X(0001).
000108     05  FILLER REDEFINES BDT1F.
000109         10  BDT1A PIC  X(0001).
000110     05  BDT1I PIC  X(0006).
000111*    -------------------------------
000112     05  SDT1L PIC S9(0004) COMP.
000113     05  SDT1F PIC  X(0001).
000114     05  FILLER REDEFINES SDT1F.
000115         10  SDT1A PIC  X(0001).
000116     05  SDT1I PIC  X(0006).
000117*    -------------------------------
000118     05  MSAAC2L PIC S9(0004) COMP.
000119     05  MSAAC2F PIC  X(0001).
000120     05  FILLER REDEFINES MSAAC2F.
000121         10  MSAAC2A PIC  X(0001).
000122     05  MSAAC2I PIC  X(0010).
000123*    -------------------------------
000124     05  MSAST2L PIC S9(0004) COMP.
000125     05  MSAST2F PIC  X(0001).
000126     05  FILLER REDEFINES MSAST2F.
000127         10  MSAST2A PIC  X(0001).
000128     05  MSAST2I PIC  X(0002).
000129*    -------------------------------
000130     05  MSACN2L PIC S9(0004) COMP.
000131     05  MSACN2F PIC  X(0001).
000132     05  FILLER REDEFINES MSACN2F.
000133         10  MSACN2A PIC  X(0001).
000134     05  MSACN2I PIC  X(0001).
000135*    -------------------------------
000136     05  MSACM2L PIC S9(0004) COMP.
000137     05  MSACM2F PIC  X(0001).
000138     05  FILLER REDEFINES MSACM2F.
000139         10  MSACM2A PIC  X(0001).
000140     05  MSACM2I PIC  X(0010).
000141*    -------------------------------
000142     05  TYPE2L PIC S9(0004) COMP.
000143     05  TYPE2F PIC  X(0001).
000144     05  FILLER REDEFINES TYPE2F.
000145         10  TYPE2A PIC  X(0001).
000146     05  TYPE2I PIC  X(0001).
000147*    -------------------------------
000148     05  AMT2L PIC S9(0004) COMP.
000149     05  AMT2F PIC  X(0001).
000150     05  FILLER REDEFINES AMT2F.
000151         10  AMT2A PIC  X(0001).
000152     05  AMT2I PIC  X(0011).
000153*    -------------------------------
000154     05  VOID2L PIC S9(0004) COMP.
000155     05  VOID2F PIC  X(0001).
000156     05  FILLER REDEFINES VOID2F.
000157         10  VOID2A PIC  X(0001).
000158     05  VOID2I PIC  X(0001).
000159*    -------------------------------
000160     05  MDT2L PIC S9(0004) COMP.
000161     05  MDT2F PIC  X(0001).
000162     05  FILLER REDEFINES MDT2F.
000163         10  MDT2A PIC  X(0001).
000164     05  MDT2I PIC  X(0006).
000165*    -------------------------------
000166     05  BDT2L PIC S9(0004) COMP.
000167     05  BDT2F PIC  X(0001).
000168     05  FILLER REDEFINES BDT2F.
000169         10  BDT2A PIC  X(0001).
000170     05  BDT2I PIC  X(0006).
000171*    -------------------------------
000172     05  SDT2L PIC S9(0004) COMP.
000173     05  SDT2F PIC  X(0001).
000174     05  FILLER REDEFINES SDT2F.
000175         10  SDT2A PIC  X(0001).
000176     05  SDT2I PIC  X(0006).
000177*    -------------------------------
000178     05  MSAAC3L PIC S9(0004) COMP.
000179     05  MSAAC3F PIC  X(0001).
000180     05  FILLER REDEFINES MSAAC3F.
000181         10  MSAAC3A PIC  X(0001).
000182     05  MSAAC3I PIC  X(0010).
000183*    -------------------------------
000184     05  MSAST3L PIC S9(0004) COMP.
000185     05  MSAST3F PIC  X(0001).
000186     05  FILLER REDEFINES MSAST3F.
000187         10  MSAST3A PIC  X(0001).
000188     05  MSAST3I PIC  X(0002).
000189*    -------------------------------
000190     05  MSACN3L PIC S9(0004) COMP.
000191     05  MSACN3F PIC  X(0001).
000192     05  FILLER REDEFINES MSACN3F.
000193         10  MSACN3A PIC  X(0001).
000194     05  MSACN3I PIC  X(0001).
000195*    -------------------------------
000196     05  MSACM3L PIC S9(0004) COMP.
000197     05  MSACM3F PIC  X(0001).
000198     05  FILLER REDEFINES MSACM3F.
000199         10  MSACM3A PIC  X(0001).
000200     05  MSACM3I PIC  X(0010).
000201*    -------------------------------
000202     05  TYPE3L PIC S9(0004) COMP.
000203     05  TYPE3F PIC  X(0001).
000204     05  FILLER REDEFINES TYPE3F.
000205         10  TYPE3A PIC  X(0001).
000206     05  TYPE3I PIC  X(0001).
000207*    -------------------------------
000208     05  AMT3L PIC S9(0004) COMP.
000209     05  AMT3F PIC  X(0001).
000210     05  FILLER REDEFINES AMT3F.
000211         10  AMT3A PIC  X(0001).
000212     05  AMT3I PIC  X(0011).
000213*    -------------------------------
000214     05  VOID3L PIC S9(0004) COMP.
000215     05  VOID3F PIC  X(0001).
000216     05  FILLER REDEFINES VOID3F.
000217         10  VOID3A PIC  X(0001).
000218     05  VOID3I PIC  X(0001).
000219*    -------------------------------
000220     05  MDT3L PIC S9(0004) COMP.
000221     05  MDT3F PIC  X(0001).
000222     05  FILLER REDEFINES MDT3F.
000223         10  MDT3A PIC  X(0001).
000224     05  MDT3I PIC  X(0006).
000225*    -------------------------------
000226     05  BDT3L PIC S9(0004) COMP.
000227     05  BDT3F PIC  X(0001).
000228     05  FILLER REDEFINES BDT3F.
000229         10  BDT3A PIC  X(0001).
000230     05  BDT3I PIC  X(0006).
000231*    -------------------------------
000232     05  SDT3L PIC S9(0004) COMP.
000233     05  SDT3F PIC  X(0001).
000234     05  FILLER REDEFINES SDT3F.
000235         10  SDT3A PIC  X(0001).
000236     05  SDT3I PIC  X(0006).
000237*    -------------------------------
000238     05  MSAAC4L PIC S9(0004) COMP.
000239     05  MSAAC4F PIC  X(0001).
000240     05  FILLER REDEFINES MSAAC4F.
000241         10  MSAAC4A PIC  X(0001).
000242     05  MSAAC4I PIC  X(0010).
000243*    -------------------------------
000244     05  MSAST4L PIC S9(0004) COMP.
000245     05  MSAST4F PIC  X(0001).
000246     05  FILLER REDEFINES MSAST4F.
000247         10  MSAST4A PIC  X(0001).
000248     05  MSAST4I PIC  X(0002).
000249*    -------------------------------
000250     05  MSACN4L PIC S9(0004) COMP.
000251     05  MSACN4F PIC  X(0001).
000252     05  FILLER REDEFINES MSACN4F.
000253         10  MSACN4A PIC  X(0001).
000254     05  MSACN4I PIC  X(0001).
000255*    -------------------------------
000256     05  MSACM4L PIC S9(0004) COMP.
000257     05  MSACM4F PIC  X(0001).
000258     05  FILLER REDEFINES MSACM4F.
000259         10  MSACM4A PIC  X(0001).
000260     05  MSACM4I PIC  X(0010).
000261*    -------------------------------
000262     05  TYPE4L PIC S9(0004) COMP.
000263     05  TYPE4F PIC  X(0001).
000264     05  FILLER REDEFINES TYPE4F.
000265         10  TYPE4A PIC  X(0001).
000266     05  TYPE4I PIC  X(0001).
000267*    -------------------------------
000268     05  AMT4L PIC S9(0004) COMP.
000269     05  AMT4F PIC  X(0001).
000270     05  FILLER REDEFINES AMT4F.
000271         10  AMT4A PIC  X(0001).
000272     05  AMT4I PIC  X(0011).
000273*    -------------------------------
000274     05  VOID4L PIC S9(0004) COMP.
000275     05  VOID4F PIC  X(0001).
000276     05  FILLER REDEFINES VOID4F.
000277         10  VOID4A PIC  X(0001).
000278     05  VOID4I PIC  X(0001).
000279*    -------------------------------
000280     05  MDT4L PIC S9(0004) COMP.
000281     05  MDT4F PIC  X(0001).
000282     05  FILLER REDEFINES MDT4F.
000283         10  MDT4A PIC  X(0001).
000284     05  MDT4I PIC  X(0006).
000285*    -------------------------------
000286     05  BDT4L PIC S9(0004) COMP.
000287     05  BDT4F PIC  X(0001).
000288     05  FILLER REDEFINES BDT4F.
000289         10  BDT4A PIC  X(0001).
000290     05  BDT4I PIC  X(0006).
000291*    -------------------------------
000292     05  SDT4L PIC S9(0004) COMP.
000293     05  SDT4F PIC  X(0001).
000294     05  FILLER REDEFINES SDT4F.
000295         10  SDT4A PIC  X(0001).
000296     05  SDT4I PIC  X(0006).
000297*    -------------------------------
000298     05  MSAAC5L PIC S9(0004) COMP.
000299     05  MSAAC5F PIC  X(0001).
000300     05  FILLER REDEFINES MSAAC5F.
000301         10  MSAAC5A PIC  X(0001).
000302     05  MSAAC5I PIC  X(0010).
000303*    -------------------------------
000304     05  MSAST5L PIC S9(0004) COMP.
000305     05  MSAST5F PIC  X(0001).
000306     05  FILLER REDEFINES MSAST5F.
000307         10  MSAST5A PIC  X(0001).
000308     05  MSAST5I PIC  X(0002).
000309*    -------------------------------
000310     05  MSACN5L PIC S9(0004) COMP.
000311     05  MSACN5F PIC  X(0001).
000312     05  FILLER REDEFINES MSACN5F.
000313         10  MSACN5A PIC  X(0001).
000314     05  MSACN5I PIC  X(0001).
000315*    -------------------------------
000316     05  MSACM5L PIC S9(0004) COMP.
000317     05  MSACM5F PIC  X(0001).
000318     05  FILLER REDEFINES MSACM5F.
000319         10  MSACM5A PIC  X(0001).
000320     05  MSACM5I PIC  X(0010).
000321*    -------------------------------
000322     05  TYPE5L PIC S9(0004) COMP.
000323     05  TYPE5F PIC  X(0001).
000324     05  FILLER REDEFINES TYPE5F.
000325         10  TYPE5A PIC  X(0001).
000326     05  TYPE5I PIC  X(0001).
000327*    -------------------------------
000328     05  AMT5L PIC S9(0004) COMP.
000329     05  AMT5F PIC  X(0001).
000330     05  FILLER REDEFINES AMT5F.
000331         10  AMT5A PIC  X(0001).
000332     05  AMT5I PIC  X(0011).
000333*    -------------------------------
000334     05  VOID5L PIC S9(0004) COMP.
000335     05  VOID5F PIC  X(0001).
000336     05  FILLER REDEFINES VOID5F.
000337         10  VOID5A PIC  X(0001).
000338     05  VOID5I PIC  X(0001).
000339*    -------------------------------
000340     05  MDT5L PIC S9(0004) COMP.
000341     05  MDT5F PIC  X(0001).
000342     05  FILLER REDEFINES MDT5F.
000343         10  MDT5A PIC  X(0001).
000344     05  MDT5I PIC  X(0006).
000345*    -------------------------------
000346     05  BDT5L PIC S9(0004) COMP.
000347     05  BDT5F PIC  X(0001).
000348     05  FILLER REDEFINES BDT5F.
000349         10  BDT5A PIC  X(0001).
000350     05  BDT5I PIC  X(0006).
000351*    -------------------------------
000352     05  SDT5L PIC S9(0004) COMP.
000353     05  SDT5F PIC  X(0001).
000354     05  FILLER REDEFINES SDT5F.
000355         10  SDT5A PIC  X(0001).
000356     05  SDT5I PIC  X(0006).
000357*    -------------------------------
000358     05  MSAAC6L PIC S9(0004) COMP.
000359     05  MSAAC6F PIC  X(0001).
000360     05  FILLER REDEFINES MSAAC6F.
000361         10  MSAAC6A PIC  X(0001).
000362     05  MSAAC6I PIC  X(0010).
000363*    -------------------------------
000364     05  MSAST6L PIC S9(0004) COMP.
000365     05  MSAST6F PIC  X(0001).
000366     05  FILLER REDEFINES MSAST6F.
000367         10  MSAST6A PIC  X(0001).
000368     05  MSAST6I PIC  X(0002).
000369*    -------------------------------
000370     05  MSACN6L PIC S9(0004) COMP.
000371     05  MSACN6F PIC  X(0001).
000372     05  FILLER REDEFINES MSACN6F.
000373         10  MSACN6A PIC  X(0001).
000374     05  MSACN6I PIC  X(0001).
000375*    -------------------------------
000376     05  MSACM6L PIC S9(0004) COMP.
000377     05  MSACM6F PIC  X(0001).
000378     05  FILLER REDEFINES MSACM6F.
000379         10  MSACM6A PIC  X(0001).
000380     05  MSACM6I PIC  X(0010).
000381*    -------------------------------
000382     05  TYPE6L PIC S9(0004) COMP.
000383     05  TYPE6F PIC  X(0001).
000384     05  FILLER REDEFINES TYPE6F.
000385         10  TYPE6A PIC  X(0001).
000386     05  TYPE6I PIC  X(0001).
000387*    -------------------------------
000388     05  AMT6L PIC S9(0004) COMP.
000389     05  AMT6F PIC  X(0001).
000390     05  FILLER REDEFINES AMT6F.
000391         10  AMT6A PIC  X(0001).
000392     05  AMT6I PIC  X(0011).
000393*    -------------------------------
000394     05  VOID6L PIC S9(0004) COMP.
000395     05  VOID6F PIC  X(0001).
000396     05  FILLER REDEFINES VOID6F.
000397         10  VOID6A PIC  X(0001).
000398     05  VOID6I PIC  X(0001).
000399*    -------------------------------
000400     05  MDT6L PIC S9(0004) COMP.
000401     05  MDT6F PIC  X(0001).
000402     05  FILLER REDEFINES MDT6F.
000403         10  MDT6A PIC  X(0001).
000404     05  MDT6I PIC  X(0006).
000405*    -------------------------------
000406     05  BDT6L PIC S9(0004) COMP.
000407     05  BDT6F PIC  X(0001).
000408     05  FILLER REDEFINES BDT6F.
000409         10  BDT6A PIC  X(0001).
000410     05  BDT6I PIC  X(0006).
000411*    -------------------------------
000412     05  SDT6L PIC S9(0004) COMP.
000413     05  SDT6F PIC  X(0001).
000414     05  FILLER REDEFINES SDT6F.
000415         10  SDT6A PIC  X(0001).
000416     05  SDT6I PIC  X(0006).
000417*    -------------------------------
000418     05  MSAAC7L PIC S9(0004) COMP.
000419     05  MSAAC7F PIC  X(0001).
000420     05  FILLER REDEFINES MSAAC7F.
000421         10  MSAAC7A PIC  X(0001).
000422     05  MSAAC7I PIC  X(0010).
000423*    -------------------------------
000424     05  MSAST7L PIC S9(0004) COMP.
000425     05  MSAST7F PIC  X(0001).
000426     05  FILLER REDEFINES MSAST7F.
000427         10  MSAST7A PIC  X(0001).
000428     05  MSAST7I PIC  X(0002).
000429*    -------------------------------
000430     05  MSACN7L PIC S9(0004) COMP.
000431     05  MSACN7F PIC  X(0001).
000432     05  FILLER REDEFINES MSACN7F.
000433         10  MSACN7A PIC  X(0001).
000434     05  MSACN7I PIC  X(0001).
000435*    -------------------------------
000436     05  MSACM7L PIC S9(0004) COMP.
000437     05  MSACM7F PIC  X(0001).
000438     05  FILLER REDEFINES MSACM7F.
000439         10  MSACM7A PIC  X(0001).
000440     05  MSACM7I PIC  X(0010).
000441*    -------------------------------
000442     05  TYPE7L PIC S9(0004) COMP.
000443     05  TYPE7F PIC  X(0001).
000444     05  FILLER REDEFINES TYPE7F.
000445         10  TYPE7A PIC  X(0001).
000446     05  TYPE7I PIC  X(0001).
000447*    -------------------------------
000448     05  AMT7L PIC S9(0004) COMP.
000449     05  AMT7F PIC  X(0001).
000450     05  FILLER REDEFINES AMT7F.
000451         10  AMT7A PIC  X(0001).
000452     05  AMT7I PIC  X(0011).
000453*    -------------------------------
000454     05  VOID7L PIC S9(0004) COMP.
000455     05  VOID7F PIC  X(0001).
000456     05  FILLER REDEFINES VOID7F.
000457         10  VOID7A PIC  X(0001).
000458     05  VOID7I PIC  X(0001).
000459*    -------------------------------
000460     05  MDT7L PIC S9(0004) COMP.
000461     05  MDT7F PIC  X(0001).
000462     05  FILLER REDEFINES MDT7F.
000463         10  MDT7A PIC  X(0001).
000464     05  MDT7I PIC  X(0006).
000465*    -------------------------------
000466     05  BDT7L PIC S9(0004) COMP.
000467     05  BDT7F PIC  X(0001).
000468     05  FILLER REDEFINES BDT7F.
000469         10  BDT7A PIC  X(0001).
000470     05  BDT7I PIC  X(0006).
000471*    -------------------------------
000472     05  SDT7L PIC S9(0004) COMP.
000473     05  SDT7F PIC  X(0001).
000474     05  FILLER REDEFINES SDT7F.
000475         10  SDT7A PIC  X(0001).
000476     05  SDT7I PIC  X(0006).
000477*    -------------------------------
000478     05  MSAAC8L PIC S9(0004) COMP.
000479     05  MSAAC8F PIC  X(0001).
000480     05  FILLER REDEFINES MSAAC8F.
000481         10  MSAAC8A PIC  X(0001).
000482     05  MSAAC8I PIC  X(0010).
000483*    -------------------------------
000484     05  MSAST8L PIC S9(0004) COMP.
000485     05  MSAST8F PIC  X(0001).
000486     05  FILLER REDEFINES MSAST8F.
000487         10  MSAST8A PIC  X(0001).
000488     05  MSAST8I PIC  X(0002).
000489*    -------------------------------
000490     05  MSACN8L PIC S9(0004) COMP.
000491     05  MSACN8F PIC  X(0001).
000492     05  FILLER REDEFINES MSACN8F.
000493         10  MSACN8A PIC  X(0001).
000494     05  MSACN8I PIC  X(0001).
000495*    -------------------------------
000496     05  MSACM8L PIC S9(0004) COMP.
000497     05  MSACM8F PIC  X(0001).
000498     05  FILLER REDEFINES MSACM8F.
000499         10  MSACM8A PIC  X(0001).
000500     05  MSACM8I PIC  X(0010).
000501*    -------------------------------
000502     05  TYPE8L PIC S9(0004) COMP.
000503     05  TYPE8F PIC  X(0001).
000504     05  FILLER REDEFINES TYPE8F.
000505         10  TYPE8A PIC  X(0001).
000506     05  TYPE8I PIC  X(0001).
000507*    -------------------------------
000508     05  AMT8L PIC S9(0004) COMP.
000509     05  AMT8F PIC  X(0001).
000510     05  FILLER REDEFINES AMT8F.
000511         10  AMT8A PIC  X(0001).
000512     05  AMT8I PIC  X(0011).
000513*    -------------------------------
000514     05  VOID8L PIC S9(0004) COMP.
000515     05  VOID8F PIC  X(0001).
000516     05  FILLER REDEFINES VOID8F.
000517         10  VOID8A PIC  X(0001).
000518     05  VOID8I PIC  X(0001).
000519*    -------------------------------
000520     05  MDT8L PIC S9(0004) COMP.
000521     05  MDT8F PIC  X(0001).
000522     05  FILLER REDEFINES MDT8F.
000523         10  MDT8A PIC  X(0001).
000524     05  MDT8I PIC  X(0006).
000525*    -------------------------------
000526     05  BDT8L PIC S9(0004) COMP.
000527     05  BDT8F PIC  X(0001).
000528     05  FILLER REDEFINES BDT8F.
000529         10  BDT8A PIC  X(0001).
000530     05  BDT8I PIC  X(0006).
000531*    -------------------------------
000532     05  SDT8L PIC S9(0004) COMP.
000533     05  SDT8F PIC  X(0001).
000534     05  FILLER REDEFINES SDT8F.
000535         10  SDT8A PIC  X(0001).
000536     05  SDT8I PIC  X(0006).
000537*    -------------------------------
000538     05  MSAAC9L PIC S9(0004) COMP.
000539     05  MSAAC9F PIC  X(0001).
000540     05  FILLER REDEFINES MSAAC9F.
000541         10  MSAAC9A PIC  X(0001).
000542     05  MSAAC9I PIC  X(0010).
000543*    -------------------------------
000544     05  MSAST9L PIC S9(0004) COMP.
000545     05  MSAST9F PIC  X(0001).
000546     05  FILLER REDEFINES MSAST9F.
000547         10  MSAST9A PIC  X(0001).
000548     05  MSAST9I PIC  X(0002).
000549*    -------------------------------
000550     05  MSACN9L PIC S9(0004) COMP.
000551     05  MSACN9F PIC  X(0001).
000552     05  FILLER REDEFINES MSACN9F.
000553         10  MSACN9A PIC  X(0001).
000554     05  MSACN9I PIC  X(0001).
000555*    -------------------------------
000556     05  MSACM9L PIC S9(0004) COMP.
000557     05  MSACM9F PIC  X(0001).
000558     05  FILLER REDEFINES MSACM9F.
000559         10  MSACM9A PIC  X(0001).
000560     05  MSACM9I PIC  X(0010).
000561*    -------------------------------
000562     05  TYPE9L PIC S9(0004) COMP.
000563     05  TYPE9F PIC  X(0001).
000564     05  FILLER REDEFINES TYPE9F.
000565         10  TYPE9A PIC  X(0001).
000566     05  TYPE9I PIC  X(0001).
000567*    -------------------------------
000568     05  AMT9L PIC S9(0004) COMP.
000569     05  AMT9F PIC  X(0001).
000570     05  FILLER REDEFINES AMT9F.
000571         10  AMT9A PIC  X(0001).
000572     05  AMT9I PIC  X(0011).
000573*    -------------------------------
000574     05  VOID9L PIC S9(0004) COMP.
000575     05  VOID9F PIC  X(0001).
000576     05  FILLER REDEFINES VOID9F.
000577         10  VOID9A PIC  X(0001).
000578     05  VOID9I PIC  X(0001).
000579*    -------------------------------
000580     05  MDT9L PIC S9(0004) COMP.
000581     05  MDT9F PIC  X(0001).
000582     05  FILLER REDEFINES MDT9F.
000583         10  MDT9A PIC  X(0001).
000584     05  MDT9I PIC  X(0006).
000585*    -------------------------------
000586     05  BDT9L PIC S9(0004) COMP.
000587     05  BDT9F PIC  X(0001).
000588     05  FILLER REDEFINES BDT9F.
000589         10  BDT9A PIC  X(0001).
000590     05  BDT9I PIC  X(0006).
000591*    -------------------------------
000592     05  SDT9L PIC S9(0004) COMP.
000593     05  SDT9F PIC  X(0001).
000594     05  FILLER REDEFINES SDT9F.
000595         10  SDT9A PIC  X(0001).
000596     05  SDT9I PIC  X(0006).
000597*    -------------------------------
000598     05  MSAAC10L PIC S9(0004) COMP.
000599     05  MSAAC10F PIC  X(0001).
000600     05  FILLER REDEFINES MSAAC10F.
000601         10  MSAAC10A PIC  X(0001).
000602     05  MSAAC10I PIC  X(0010).
000603*    -------------------------------
000604     05  MSAST10L PIC S9(0004) COMP.
000605     05  MSAST10F PIC  X(0001).
000606     05  FILLER REDEFINES MSAST10F.
000607         10  MSAST10A PIC  X(0001).
000608     05  MSAST10I PIC  X(0002).
000609*    -------------------------------
000610     05  MSACN10L PIC S9(0004) COMP.
000611     05  MSACN10F PIC  X(0001).
000612     05  FILLER REDEFINES MSACN10F.
000613         10  MSACN10A PIC  X(0001).
000614     05  MSACN10I PIC  X(0001).
000615*    -------------------------------
000616     05  MSACM10L PIC S9(0004) COMP.
000617     05  MSACM10F PIC  X(0001).
000618     05  FILLER REDEFINES MSACM10F.
000619         10  MSACM10A PIC  X(0001).
000620     05  MSACM10I PIC  X(0010).
000621*    -------------------------------
000622     05  TYPE10L PIC S9(0004) COMP.
000623     05  TYPE10F PIC  X(0001).
000624     05  FILLER REDEFINES TYPE10F.
000625         10  TYPE10A PIC  X(0001).
000626     05  TYPE10I PIC  X(0001).
000627*    -------------------------------
000628     05  AMT10L PIC S9(0004) COMP.
000629     05  AMT10F PIC  X(0001).
000630     05  FILLER REDEFINES AMT10F.
000631         10  AMT10A PIC  X(0001).
000632     05  AMT10I PIC  X(0011).
000633*    -------------------------------
000634     05  VOID10L PIC S9(0004) COMP.
000635     05  VOID10F PIC  X(0001).
000636     05  FILLER REDEFINES VOID10F.
000637         10  VOID10A PIC  X(0001).
000638     05  VOID10I PIC  X(0001).
000639*    -------------------------------
000640     05  MDT10L PIC S9(0004) COMP.
000641     05  MDT10F PIC  X(0001).
000642     05  FILLER REDEFINES MDT10F.
000643         10  MDT10A PIC  X(0001).
000644     05  MDT10I PIC  X(0006).
000645*    -------------------------------
000646     05  BDT10L PIC S9(0004) COMP.
000647     05  BDT10F PIC  X(0001).
000648     05  FILLER REDEFINES BDT10F.
000649         10  BDT10A PIC  X(0001).
000650     05  BDT10I PIC  X(0006).
000651*    -------------------------------
000652     05  SDT10L PIC S9(0004) COMP.
000653     05  SDT10F PIC  X(0001).
000654     05  FILLER REDEFINES SDT10F.
000655         10  SDT10A PIC  X(0001).
000656     05  SDT10I PIC  X(0006).
000657*    -------------------------------
000658     05  MSAAC11L PIC S9(0004) COMP.
000659     05  MSAAC11F PIC  X(0001).
000660     05  FILLER REDEFINES MSAAC11F.
000661         10  MSAAC11A PIC  X(0001).
000662     05  MSAAC11I PIC  X(0010).
000663*    -------------------------------
000664     05  MSAST11L PIC S9(0004) COMP.
000665     05  MSAST11F PIC  X(0001).
000666     05  FILLER REDEFINES MSAST11F.
000667         10  MSAST11A PIC  X(0001).
000668     05  MSAST11I PIC  X(0002).
000669*    -------------------------------
000670     05  MSACN11L PIC S9(0004) COMP.
000671     05  MSACN11F PIC  X(0001).
000672     05  FILLER REDEFINES MSACN11F.
000673         10  MSACN11A PIC  X(0001).
000674     05  MSACN11I PIC  X(0001).
000675*    -------------------------------
000676     05  MSACM11L PIC S9(0004) COMP.
000677     05  MSACM11F PIC  X(0001).
000678     05  FILLER REDEFINES MSACM11F.
000679         10  MSACM11A PIC  X(0001).
000680     05  MSACM11I PIC  X(0010).
000681*    -------------------------------
000682     05  TYPE11L PIC S9(0004) COMP.
000683     05  TYPE11F PIC  X(0001).
000684     05  FILLER REDEFINES TYPE11F.
000685         10  TYPE11A PIC  X(0001).
000686     05  TYPE11I PIC  X(0001).
000687*    -------------------------------
000688     05  AMT11L PIC S9(0004) COMP.
000689     05  AMT11F PIC  X(0001).
000690     05  FILLER REDEFINES AMT11F.
000691         10  AMT11A PIC  X(0001).
000692     05  AMT11I PIC  X(0011).
000693*    -------------------------------
000694     05  VOID11L PIC S9(0004) COMP.
000695     05  VOID11F PIC  X(0001).
000696     05  FILLER REDEFINES VOID11F.
000697         10  VOID11A PIC  X(0001).
000698     05  VOID11I PIC  X(0001).
000699*    -------------------------------
000700     05  MDT11L PIC S9(0004) COMP.
000701     05  MDT11F PIC  X(0001).
000702     05  FILLER REDEFINES MDT11F.
000703         10  MDT11A PIC  X(0001).
000704     05  MDT11I PIC  X(0006).
000705*    -------------------------------
000706     05  BDT11L PIC S9(0004) COMP.
000707     05  BDT11F PIC  X(0001).
000708     05  FILLER REDEFINES BDT11F.
000709         10  BDT11A PIC  X(0001).
000710     05  BDT11I PIC  X(0006).
000711*    -------------------------------
000712     05  SDT11L PIC S9(0004) COMP.
000713     05  SDT11F PIC  X(0001).
000714     05  FILLER REDEFINES SDT11F.
000715         10  SDT11A PIC  X(0001).
000716     05  SDT11I PIC  X(0006).
000717*    -------------------------------
000718     05  MSAAC12L PIC S9(0004) COMP.
000719     05  MSAAC12F PIC  X(0001).
000720     05  FILLER REDEFINES MSAAC12F.
000721         10  MSAAC12A PIC  X(0001).
000722     05  MSAAC12I PIC  X(0010).
000723*    -------------------------------
000724     05  MSAST12L PIC S9(0004) COMP.
000725     05  MSAST12F PIC  X(0001).
000726     05  FILLER REDEFINES MSAST12F.
000727         10  MSAST12A PIC  X(0001).
000728     05  MSAST12I PIC  X(0002).
000729*    -------------------------------
000730     05  MSACN12L PIC S9(0004) COMP.
000731     05  MSACN12F PIC  X(0001).
000732     05  FILLER REDEFINES MSACN12F.
000733         10  MSACN12A PIC  X(0001).
000734     05  MSACN12I PIC  X(0001).
000735*    -------------------------------
000736     05  MSACM12L PIC S9(0004) COMP.
000737     05  MSACM12F PIC  X(0001).
000738     05  FILLER REDEFINES MSACM12F.
000739         10  MSACM12A PIC  X(0001).
000740     05  MSACM12I PIC  X(0010).
000741*    -------------------------------
000742     05  TYPE12L PIC S9(0004) COMP.
000743     05  TYPE12F PIC  X(0001).
000744     05  FILLER REDEFINES TYPE12F.
000745         10  TYPE12A PIC  X(0001).
000746     05  TYPE12I PIC  X(0001).
000747*    -------------------------------
000748     05  AMT12L PIC S9(0004) COMP.
000749     05  AMT12F PIC  X(0001).
000750     05  FILLER REDEFINES AMT12F.
000751         10  AMT12A PIC  X(0001).
000752     05  AMT12I PIC  X(0011).
000753*    -------------------------------
000754     05  VOID12L PIC S9(0004) COMP.
000755     05  VOID12F PIC  X(0001).
000756     05  FILLER REDEFINES VOID12F.
000757         10  VOID12A PIC  X(0001).
000758     05  VOID12I PIC  X(0001).
000759*    -------------------------------
000760     05  MDT12L PIC S9(0004) COMP.
000761     05  MDT12F PIC  X(0001).
000762     05  FILLER REDEFINES MDT12F.
000763         10  MDT12A PIC  X(0001).
000764     05  MDT12I PIC  X(0006).
000765*    -------------------------------
000766     05  BDT12L PIC S9(0004) COMP.
000767     05  BDT12F PIC  X(0001).
000768     05  FILLER REDEFINES BDT12F.
000769         10  BDT12A PIC  X(0001).
000770     05  BDT12I PIC  X(0006).
000771*    -------------------------------
000772     05  SDT12L PIC S9(0004) COMP.
000773     05  SDT12F PIC  X(0001).
000774     05  FILLER REDEFINES SDT12F.
000775         10  SDT12A PIC  X(0001).
000776     05  SDT12I PIC  X(0006).
000777*    -------------------------------
000778     05  MSAAC13L PIC S9(0004) COMP.
000779     05  MSAAC13F PIC  X(0001).
000780     05  FILLER REDEFINES MSAAC13F.
000781         10  MSAAC13A PIC  X(0001).
000782     05  MSAAC13I PIC  X(0010).
000783*    -------------------------------
000784     05  MSAST13L PIC S9(0004) COMP.
000785     05  MSAST13F PIC  X(0001).
000786     05  FILLER REDEFINES MSAST13F.
000787         10  MSAST13A PIC  X(0001).
000788     05  MSAST13I PIC  X(0002).
000789*    -------------------------------
000790     05  MSACN13L PIC S9(0004) COMP.
000791     05  MSACN13F PIC  X(0001).
000792     05  FILLER REDEFINES MSACN13F.
000793         10  MSACN13A PIC  X(0001).
000794     05  MSACN13I PIC  X(0001).
000795*    -------------------------------
000796     05  MSACM13L PIC S9(0004) COMP.
000797     05  MSACM13F PIC  X(0001).
000798     05  FILLER REDEFINES MSACM13F.
000799         10  MSACM13A PIC  X(0001).
000800     05  MSACM13I PIC  X(0010).
000801*    -------------------------------
000802     05  TYPE13L PIC S9(0004) COMP.
000803     05  TYPE13F PIC  X(0001).
000804     05  FILLER REDEFINES TYPE13F.
000805         10  TYPE13A PIC  X(0001).
000806     05  TYPE13I PIC  X(0001).
000807*    -------------------------------
000808     05  AMT13L PIC S9(0004) COMP.
000809     05  AMT13F PIC  X(0001).
000810     05  FILLER REDEFINES AMT13F.
000811         10  AMT13A PIC  X(0001).
000812     05  AMT13I PIC  X(0011).
000813*    -------------------------------
000814     05  VOID13L PIC S9(0004) COMP.
000815     05  VOID13F PIC  X(0001).
000816     05  FILLER REDEFINES VOID13F.
000817         10  VOID13A PIC  X(0001).
000818     05  VOID13I PIC  X(0001).
000819*    -------------------------------
000820     05  MDT13L PIC S9(0004) COMP.
000821     05  MDT13F PIC  X(0001).
000822     05  FILLER REDEFINES MDT13F.
000823         10  MDT13A PIC  X(0001).
000824     05  MDT13I PIC  X(0006).
000825*    -------------------------------
000826     05  BDT13L PIC S9(0004) COMP.
000827     05  BDT13F PIC  X(0001).
000828     05  FILLER REDEFINES BDT13F.
000829         10  BDT13A PIC  X(0001).
000830     05  BDT13I PIC  X(0006).
000831*    -------------------------------
000832     05  SDT13L PIC S9(0004) COMP.
000833     05  SDT13F PIC  X(0001).
000834     05  FILLER REDEFINES SDT13F.
000835         10  SDT13A PIC  X(0001).
000836     05  SDT13I PIC  X(0006).
000837*    -------------------------------
000838     05  ERRMSG1L PIC S9(0004) COMP.
000839     05  ERRMSG1F PIC  X(0001).
000840     05  FILLER REDEFINES ERRMSG1F.
000841         10  ERRMSG1A PIC  X(0001).
000842     05  ERRMSG1I PIC  X(0079).
000843*    -------------------------------
000844     05  ERRMSG2L PIC S9(0004) COMP.
000845     05  ERRMSG2F PIC  X(0001).
000846     05  FILLER REDEFINES ERRMSG2F.
000847         10  ERRMSG2A PIC  X(0001).
000848     05  ERRMSG2I PIC  X(0079).
000849*    -------------------------------
000850     05  PFENTERL PIC S9(0004) COMP.
000851     05  PFENTERF PIC  X(0001).
000852     05  FILLER REDEFINES PFENTERF.
000853         10  PFENTERA PIC  X(0001).
000854     05  PFENTERI PIC  9(2).
000855 01  EL633AO REDEFINES EL633AI.
000856     05  FILLER            PIC  X(0012).
000857*    -------------------------------
000858     05  FILLER            PIC  X(0003).
000859     05  DATEO PIC  X(0008).
000860*    -------------------------------
000861     05  FILLER            PIC  X(0003).
000862     05  TIMEO PIC  99.99.
000863*    -------------------------------
000864     05  FILLER            PIC  X(0003).
000865     05  CMPNYIDO PIC  X(0003).
000866*    -------------------------------
000867     05  FILLER            PIC  X(0003).
000868     05  USERIDO PIC  X(0004).
000869*    -------------------------------
000870     05  FILLER            PIC  X(0003).
000871     05  MAINTO PIC  X(0001).
000872*    -------------------------------
000873     05  FILLER            PIC  X(0003).
000874     05  CARRIERO PIC  X(0001).
000875*    -------------------------------
000876     05  FILLER            PIC  X(0003).
000877     05  GROUPO PIC  X(0006).
000878*    -------------------------------
000879     05  FILLER            PIC  X(0003).
000880     05  FINRESPO PIC  X(0010).
000881*    -------------------------------
000882     05  FILLER            PIC  X(0003).
000883     05  ACCTO PIC  X(0010).
000884*    -------------------------------
000885     05  FILLER            PIC  X(0003).
000886     05  MSAAC1O PIC  X(0010).
000887*    -------------------------------
000888     05  FILLER            PIC  X(0003).
000889     05  MSAST1O PIC  X(0002).
000890*    -------------------------------
000891     05  FILLER            PIC  X(0003).
000892     05  MSACN1O PIC  X(0001).
000893*    -------------------------------
000894     05  FILLER            PIC  X(0003).
000895     05  MSACM1O PIC  X(0010).
000896*    -------------------------------
000897     05  FILLER            PIC  X(0003).
000898     05  TYPE1O PIC  X(0001).
000899*    -------------------------------
000900     05  FILLER            PIC  X(0003).
000901     05  AMT1O PIC  X(0011).
000902*    -------------------------------
000903     05  FILLER            PIC  X(0003).
000904     05  VOID1O PIC  X(0001).
000905*    -------------------------------
000906     05  FILLER            PIC  X(0003).
000907     05  MDT1O PIC  X(0006).
000908*    -------------------------------
000909     05  FILLER            PIC  X(0003).
000910     05  BDT1O PIC  X(0006).
000911*    -------------------------------
000912     05  FILLER            PIC  X(0003).
000913     05  SDT1O PIC  X(0006).
000914*    -------------------------------
000915     05  FILLER            PIC  X(0003).
000916     05  MSAAC2O PIC  X(0010).
000917*    -------------------------------
000918     05  FILLER            PIC  X(0003).
000919     05  MSAST2O PIC  X(0002).
000920*    -------------------------------
000921     05  FILLER            PIC  X(0003).
000922     05  MSACN2O PIC  X(0001).
000923*    -------------------------------
000924     05  FILLER            PIC  X(0003).
000925     05  MSACM2O PIC  X(0010).
000926*    -------------------------------
000927     05  FILLER            PIC  X(0003).
000928     05  TYPE2O PIC  X(0001).
000929*    -------------------------------
000930     05  FILLER            PIC  X(0003).
000931     05  AMT2O PIC  X(0011).
000932*    -------------------------------
000933     05  FILLER            PIC  X(0003).
000934     05  VOID2O PIC  X(0001).
000935*    -------------------------------
000936     05  FILLER            PIC  X(0003).
000937     05  MDT2O PIC  X(0006).
000938*    -------------------------------
000939     05  FILLER            PIC  X(0003).
000940     05  BDT2O PIC  X(0006).
000941*    -------------------------------
000942     05  FILLER            PIC  X(0003).
000943     05  SDT2O PIC  X(0006).
000944*    -------------------------------
000945     05  FILLER            PIC  X(0003).
000946     05  MSAAC3O PIC  X(0010).
000947*    -------------------------------
000948     05  FILLER            PIC  X(0003).
000949     05  MSAST3O PIC  X(0002).
000950*    -------------------------------
000951     05  FILLER            PIC  X(0003).
000952     05  MSACN3O PIC  X(0001).
000953*    -------------------------------
000954     05  FILLER            PIC  X(0003).
000955     05  MSACM3O PIC  X(0010).
000956*    -------------------------------
000957     05  FILLER            PIC  X(0003).
000958     05  TYPE3O PIC  X(0001).
000959*    -------------------------------
000960     05  FILLER            PIC  X(0003).
000961     05  AMT3O PIC  X(0011).
000962*    -------------------------------
000963     05  FILLER            PIC  X(0003).
000964     05  VOID3O PIC  X(0001).
000965*    -------------------------------
000966     05  FILLER            PIC  X(0003).
000967     05  MDT3O PIC  X(0006).
000968*    -------------------------------
000969     05  FILLER            PIC  X(0003).
000970     05  BDT3O PIC  X(0006).
000971*    -------------------------------
000972     05  FILLER            PIC  X(0003).
000973     05  SDT3O PIC  X(0006).
000974*    -------------------------------
000975     05  FILLER            PIC  X(0003).
000976     05  MSAAC4O PIC  X(0010).
000977*    -------------------------------
000978     05  FILLER            PIC  X(0003).
000979     05  MSAST4O PIC  X(0002).
000980*    -------------------------------
000981     05  FILLER            PIC  X(0003).
000982     05  MSACN4O PIC  X(0001).
000983*    -------------------------------
000984     05  FILLER            PIC  X(0003).
000985     05  MSACM4O PIC  X(0010).
000986*    -------------------------------
000987     05  FILLER            PIC  X(0003).
000988     05  TYPE4O PIC  X(0001).
000989*    -------------------------------
000990     05  FILLER            PIC  X(0003).
000991     05  AMT4O PIC  X(0011).
000992*    -------------------------------
000993     05  FILLER            PIC  X(0003).
000994     05  VOID4O PIC  X(0001).
000995*    -------------------------------
000996     05  FILLER            PIC  X(0003).
000997     05  MDT4O PIC  X(0006).
000998*    -------------------------------
000999     05  FILLER            PIC  X(0003).
001000     05  BDT4O PIC  X(0006).
001001*    -------------------------------
001002     05  FILLER            PIC  X(0003).
001003     05  SDT4O PIC  X(0006).
001004*    -------------------------------
001005     05  FILLER            PIC  X(0003).
001006     05  MSAAC5O PIC  X(0010).
001007*    -------------------------------
001008     05  FILLER            PIC  X(0003).
001009     05  MSAST5O PIC  X(0002).
001010*    -------------------------------
001011     05  FILLER            PIC  X(0003).
001012     05  MSACN5O PIC  X(0001).
001013*    -------------------------------
001014     05  FILLER            PIC  X(0003).
001015     05  MSACM5O PIC  X(0010).
001016*    -------------------------------
001017     05  FILLER            PIC  X(0003).
001018     05  TYPE5O PIC  X(0001).
001019*    -------------------------------
001020     05  FILLER            PIC  X(0003).
001021     05  AMT5O PIC  X(0011).
001022*    -------------------------------
001023     05  FILLER            PIC  X(0003).
001024     05  VOID5O PIC  X(0001).
001025*    -------------------------------
001026     05  FILLER            PIC  X(0003).
001027     05  MDT5O PIC  X(0006).
001028*    -------------------------------
001029     05  FILLER            PIC  X(0003).
001030     05  BDT5O PIC  X(0006).
001031*    -------------------------------
001032     05  FILLER            PIC  X(0003).
001033     05  SDT5O PIC  X(0006).
001034*    -------------------------------
001035     05  FILLER            PIC  X(0003).
001036     05  MSAAC6O PIC  X(0010).
001037*    -------------------------------
001038     05  FILLER            PIC  X(0003).
001039     05  MSAST6O PIC  X(0002).
001040*    -------------------------------
001041     05  FILLER            PIC  X(0003).
001042     05  MSACN6O PIC  X(0001).
001043*    -------------------------------
001044     05  FILLER            PIC  X(0003).
001045     05  MSACM6O PIC  X(0010).
001046*    -------------------------------
001047     05  FILLER            PIC  X(0003).
001048     05  TYPE6O PIC  X(0001).
001049*    -------------------------------
001050     05  FILLER            PIC  X(0003).
001051     05  AMT6O PIC  X(0011).
001052*    -------------------------------
001053     05  FILLER            PIC  X(0003).
001054     05  VOID6O PIC  X(0001).
001055*    -------------------------------
001056     05  FILLER            PIC  X(0003).
001057     05  MDT6O PIC  X(0006).
001058*    -------------------------------
001059     05  FILLER            PIC  X(0003).
001060     05  BDT6O PIC  X(0006).
001061*    -------------------------------
001062     05  FILLER            PIC  X(0003).
001063     05  SDT6O PIC  X(0006).
001064*    -------------------------------
001065     05  FILLER            PIC  X(0003).
001066     05  MSAAC7O PIC  X(0010).
001067*    -------------------------------
001068     05  FILLER            PIC  X(0003).
001069     05  MSAST7O PIC  X(0002).
001070*    -------------------------------
001071     05  FILLER            PIC  X(0003).
001072     05  MSACN7O PIC  X(0001).
001073*    -------------------------------
001074     05  FILLER            PIC  X(0003).
001075     05  MSACM7O PIC  X(0010).
001076*    -------------------------------
001077     05  FILLER            PIC  X(0003).
001078     05  TYPE7O PIC  X(0001).
001079*    -------------------------------
001080     05  FILLER            PIC  X(0003).
001081     05  AMT7O PIC  X(0011).
001082*    -------------------------------
001083     05  FILLER            PIC  X(0003).
001084     05  VOID7O PIC  X(0001).
001085*    -------------------------------
001086     05  FILLER            PIC  X(0003).
001087     05  MDT7O PIC  X(0006).
001088*    -------------------------------
001089     05  FILLER            PIC  X(0003).
001090     05  BDT7O PIC  X(0006).
001091*    -------------------------------
001092     05  FILLER            PIC  X(0003).
001093     05  SDT7O PIC  X(0006).
001094*    -------------------------------
001095     05  FILLER            PIC  X(0003).
001096     05  MSAAC8O PIC  X(0010).
001097*    -------------------------------
001098     05  FILLER            PIC  X(0003).
001099     05  MSAST8O PIC  X(0002).
001100*    -------------------------------
001101     05  FILLER            PIC  X(0003).
001102     05  MSACN8O PIC  X(0001).
001103*    -------------------------------
001104     05  FILLER            PIC  X(0003).
001105     05  MSACM8O PIC  X(0010).
001106*    -------------------------------
001107     05  FILLER            PIC  X(0003).
001108     05  TYPE8O PIC  X(0001).
001109*    -------------------------------
001110     05  FILLER            PIC  X(0003).
001111     05  AMT8O PIC  X(0011).
001112*    -------------------------------
001113     05  FILLER            PIC  X(0003).
001114     05  VOID8O PIC  X(0001).
001115*    -------------------------------
001116     05  FILLER            PIC  X(0003).
001117     05  MDT8O PIC  X(0006).
001118*    -------------------------------
001119     05  FILLER            PIC  X(0003).
001120     05  BDT8O PIC  X(0006).
001121*    -------------------------------
001122     05  FILLER            PIC  X(0003).
001123     05  SDT8O PIC  X(0006).
001124*    -------------------------------
001125     05  FILLER            PIC  X(0003).
001126     05  MSAAC9O PIC  X(0010).
001127*    -------------------------------
001128     05  FILLER            PIC  X(0003).
001129     05  MSAST9O PIC  X(0002).
001130*    -------------------------------
001131     05  FILLER            PIC  X(0003).
001132     05  MSACN9O PIC  X(0001).
001133*    -------------------------------
001134     05  FILLER            PIC  X(0003).
001135     05  MSACM9O PIC  X(0010).
001136*    -------------------------------
001137     05  FILLER            PIC  X(0003).
001138     05  TYPE9O PIC  X(0001).
001139*    -------------------------------
001140     05  FILLER            PIC  X(0003).
001141     05  AMT9O PIC  X(0011).
001142*    -------------------------------
001143     05  FILLER            PIC  X(0003).
001144     05  VOID9O PIC  X(0001).
001145*    -------------------------------
001146     05  FILLER            PIC  X(0003).
001147     05  MDT9O PIC  X(0006).
001148*    -------------------------------
001149     05  FILLER            PIC  X(0003).
001150     05  BDT9O PIC  X(0006).
001151*    -------------------------------
001152     05  FILLER            PIC  X(0003).
001153     05  SDT9O PIC  X(0006).
001154*    -------------------------------
001155     05  FILLER            PIC  X(0003).
001156     05  MSAAC10O PIC  X(0010).
001157*    -------------------------------
001158     05  FILLER            PIC  X(0003).
001159     05  MSAST10O PIC  X(0002).
001160*    -------------------------------
001161     05  FILLER            PIC  X(0003).
001162     05  MSACN10O PIC  X(0001).
001163*    -------------------------------
001164     05  FILLER            PIC  X(0003).
001165     05  MSACM10O PIC  X(0010).
001166*    -------------------------------
001167     05  FILLER            PIC  X(0003).
001168     05  TYPE10O PIC  X(0001).
001169*    -------------------------------
001170     05  FILLER            PIC  X(0003).
001171     05  AMT10O PIC  X(0011).
001172*    -------------------------------
001173     05  FILLER            PIC  X(0003).
001174     05  VOID10O PIC  X(0001).
001175*    -------------------------------
001176     05  FILLER            PIC  X(0003).
001177     05  MDT10O PIC  X(0006).
001178*    -------------------------------
001179     05  FILLER            PIC  X(0003).
001180     05  BDT10O PIC  X(0006).
001181*    -------------------------------
001182     05  FILLER            PIC  X(0003).
001183     05  SDT10O PIC  X(0006).
001184*    -------------------------------
001185     05  FILLER            PIC  X(0003).
001186     05  MSAAC11O PIC  X(0010).
001187*    -------------------------------
001188     05  FILLER            PIC  X(0003).
001189     05  MSAST11O PIC  X(0002).
001190*    -------------------------------
001191     05  FILLER            PIC  X(0003).
001192     05  MSACN11O PIC  X(0001).
001193*    -------------------------------
001194     05  FILLER            PIC  X(0003).
001195     05  MSACM11O PIC  X(0010).
001196*    -------------------------------
001197     05  FILLER            PIC  X(0003).
001198     05  TYPE11O PIC  X(0001).
001199*    -------------------------------
001200     05  FILLER            PIC  X(0003).
001201     05  AMT11O PIC  X(0011).
001202*    -------------------------------
001203     05  FILLER            PIC  X(0003).
001204     05  VOID11O PIC  X(0001).
001205*    -------------------------------
001206     05  FILLER            PIC  X(0003).
001207     05  MDT11O PIC  X(0006).
001208*    -------------------------------
001209     05  FILLER            PIC  X(0003).
001210     05  BDT11O PIC  X(0006).
001211*    -------------------------------
001212     05  FILLER            PIC  X(0003).
001213     05  SDT11O PIC  X(0006).
001214*    -------------------------------
001215     05  FILLER            PIC  X(0003).
001216     05  MSAAC12O PIC  X(0010).
001217*    -------------------------------
001218     05  FILLER            PIC  X(0003).
001219     05  MSAST12O PIC  X(0002).
001220*    -------------------------------
001221     05  FILLER            PIC  X(0003).
001222     05  MSACN12O PIC  X(0001).
001223*    -------------------------------
001224     05  FILLER            PIC  X(0003).
001225     05  MSACM12O PIC  X(0010).
001226*    -------------------------------
001227     05  FILLER            PIC  X(0003).
001228     05  TYPE12O PIC  X(0001).
001229*    -------------------------------
001230     05  FILLER            PIC  X(0003).
001231     05  AMT12O PIC  X(0011).
001232*    -------------------------------
001233     05  FILLER            PIC  X(0003).
001234     05  VOID12O PIC  X(0001).
001235*    -------------------------------
001236     05  FILLER            PIC  X(0003).
001237     05  MDT12O PIC  X(0006).
001238*    -------------------------------
001239     05  FILLER            PIC  X(0003).
001240     05  BDT12O PIC  X(0006).
001241*    -------------------------------
001242     05  FILLER            PIC  X(0003).
001243     05  SDT12O PIC  X(0006).
001244*    -------------------------------
001245     05  FILLER            PIC  X(0003).
001246     05  MSAAC13O PIC  X(0010).
001247*    -------------------------------
001248     05  FILLER            PIC  X(0003).
001249     05  MSAST13O PIC  X(0002).
001250*    -------------------------------
001251     05  FILLER            PIC  X(0003).
001252     05  MSACN13O PIC  X(0001).
001253*    -------------------------------
001254     05  FILLER            PIC  X(0003).
001255     05  MSACM13O PIC  X(0010).
001256*    -------------------------------
001257     05  FILLER            PIC  X(0003).
001258     05  TYPE13O PIC  X(0001).
001259*    -------------------------------
001260     05  FILLER            PIC  X(0003).
001261     05  AMT13O PIC  X(0011).
001262*    -------------------------------
001263     05  FILLER            PIC  X(0003).
001264     05  VOID13O PIC  X(0001).
001265*    -------------------------------
001266     05  FILLER            PIC  X(0003).
001267     05  MDT13O PIC  X(0006).
001268*    -------------------------------
001269     05  FILLER            PIC  X(0003).
001270     05  BDT13O PIC  X(0006).
001271*    -------------------------------
001272     05  FILLER            PIC  X(0003).
001273     05  SDT13O PIC  X(0006).
001274*    -------------------------------
001275     05  FILLER            PIC  X(0003).
001276     05  ERRMSG1O PIC  X(0079).
001277*    -------------------------------
001278     05  FILLER            PIC  X(0003).
001279     05  ERRMSG2O PIC  X(0079).
001280*    -------------------------------
001281     05  FILLER            PIC  X(0003).
001282     05  PFENTERO PIC  X(0002).
001283*    -------------------------------
      *<<((file: EL633S))
000391
000392 01  MAP-EL633A  REDEFINES  EL633AI.
000393     12  FILLER                  PIC  X(87).
000394     12  DATA-AREA       OCCURS 13 TIMES
000395                             INDEXED BY PINDX.
000396         16  GL-ACCT-LEN         PIC S9(4)              COMP.
000397         16  GL-ACCT-ATTRB       PIC  X.
000398         16  GL-ACCT             PIC  X(10).
000399         16  WSL-COMM  REDEFINES GL-ACCT.
000400             20  WSL-COMM-DTE.
000401                 24  WSL-MO      PIC  X(2).
000402                 24  WSL-DA      PIC  X(2).
000403                 24  WSL-YR      PIC  X(2).
000404             20  WSL-COMM-REST   PIC  X(4).
000405         16  GL-STATE-LEN        PIC S9(4)              COMP.
000406         16  GL-STATE-ATTRB      PIC  X.
000407         16  GL-STATE            PIC  X(02).
000408         16  GL-CANC-LEN         PIC S9(4)              COMP.
000409         16  GL-CANC-ATTRB       PIC  X.
000410         16  GL-CANC             PIC  X.
000411         16  GL-COMM-LEN         PIC S9(4)              COMP.
000412         16  GL-COMM-ATTRB       PIC  X.
000413         16  GL-COMM             PIC  X(10).
000414         16  RTYPE-LEN           PIC S9(4)              COMP.
000415         16  RTYPE-ATTRB         PIC  X.
000416         16  RTYPE               PIC  X.
000417         16  AMT-LEN             PIC S9(4)              COMP.
000418         16  AMT-ATTRB           PIC  X.
000419         16  AMT                 PIC S9(9)V9(2).
000420         16  AMTO  REDEFINES
000421             AMT                 PIC Z(7).9(2)-.
000422         16  VOID-SW-LEN         PIC S9(4)              COMP.
000423         16  VOID-SW-ATTRB       PIC  X.
000424         16  VOID-SW             PIC  X.
000425         16  MDTE-LEN            PIC S9(4)              COMP.
000426         16  MDTE-ATTRB          PIC  X.
000427         16  MDTE                PIC  X(6).
000428         16  BDTE-LEN            PIC S9(4)              COMP.
000429         16  BDTE-ATTRB          PIC  X.
000430         16  BDTE                PIC  X(6).
000431         16  SDTE-LEN            PIC S9(4)              COMP.
000432         16  SDTE-ATTRB          PIC  X.
000433         16  SDTE                PIC  9(6).
000434 EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2016-2020 NTT DATA, Inc.                        
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
      * Copyright (c) 2016-2020 NTT DATA, Inc.                        *
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
000436 01  DFHCOMMAREA             PIC  X(1024).
000438*                            COPY ERCCHKQ.
      *>>((file: ERCCHKQ))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCCHKQ                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.005                          *
000007*                                                                *
000008*   FILE DESCRIPTION = CHECK QUE FILE FOR THE CREDIT SYSTEM      *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 100  RECFORM = FIXED                           *
000012*                                                                *
000013*   BASE CLUSTER = ERCHKQ                         RKP=2,LEN=7    *
000014*       ALTERNATE PATH  = NONE                                   *
000015*                                                                *
000016*   LOG = YES                                                    *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019 01  CHECK-QUE.
000020     12  CQ-RECORD-ID                PIC XX.
000021         88  VALID-CQ-ID                     VALUE 'CQ'.
000022
000023     12  CQ-CONTROL-PRIMARY.
000024         16  CQ-COMPANY-CD           PIC X.
000025         16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
000026         16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
000027
000028     12  CQ-ENTRY-TYPE               PIC X.
000029             88  CHECK-ON-QUE           VALUE 'Q'.
000030             88  ALIGNMENT-CHECK        VALUE 'A'.
000031             88  MANUAL-CHECK           VALUE 'M'.
000032             88  SPOILED-CHECK          VALUE 'S'.
000033             88  VOIDED-CHECK           VALUE 'V'.
000034             88  PAYMENT-ABORTED        VALUE 'X'.
000035
000036     12  CQ-CREDIT-MASTER-CNTL       PIC X(50).
000037
000038     12  CQ-CREDIT-PYAJ-CNTL         REDEFINES
000039         CQ-CREDIT-MASTER-CNTL.
000040         16  CQ-PYAJ-CARRIER         PIC X.
000041         16  CQ-PYAJ-GROUPING        PIC X(6).
000042         16  CQ-PYAJ-FIN-RESP        PIC X(10).
000043         16  CQ-PYAJ-ACCOUNT         PIC X(10).
000044         16  CQ-PYAJ-SEQ             PIC S9(8)  COMP.
000045         16  CQ-PYAJ-REC-TYPE        PIC X.
000046         16  FILLER                  PIC X(18).
000047
000048     12  CQ-CREDIT-CHEK-CNTL         REDEFINES
000049         CQ-CREDIT-MASTER-CNTL.
000050         16  CQ-CHEK-CARRIER         PIC X.
000051         16  CQ-CHEK-GROUPING        PIC X(6).
000052         16  CQ-CHEK-STATE           PIC XX.
000053         16  CQ-CHEK-ACCOUNT         PIC X(10).
000054         16  CQ-CHEK-CERT-EFF-DT     PIC XX.
000055         16  CQ-CHEK-CERT-NO.
000056             20  CQ-CHEK-CERT-PRIME  PIC X(10).
000057             20  CQ-CHEK-CERT-SFX    PIC X.
000058         16  CQ-CHEK-SEQ-NO          PIC S9(4)       COMP.
000059         16  CQ-CHEK-FIN-RESP        PIC X(10).
000060         16  FILLER                  PIC X(06).
000061
000062     12  CQ-CHECK-NUMBER             PIC X(7).
000063     12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
000064     12  CQ-PAYMENT-TYPE             PIC X.
000065             88  CQ-BILLING-CREDIT         VALUE '1'.
000066             88  CQ-REFUND-PMT             VALUE '2'.
000067             88  CQ-CHECK-MAINT-PMT        VALUE '3'.
000068     12  CQ-VOID-INDICATOR           PIC X.
000069             88  CHECK-IS-VOID             VALUE 'V'.
000070     12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
000071     12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
000072     12  CQ-CHECK-BY-USER            PIC X(4).
000073     12  CQ-PRE-NUMBERING-SW         PIC X.
000074       88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
000075       88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
000076
000077     12  CQ-CHECK-WRITTEN-DT         PIC XX.
000078     12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
000079     12  CQ-ACCOUNT-AGENT            PIC X(10).
000080     12  CQ-CHECK-VOIDED-DT          PIC XX.
000081
000082     12  CQ-LETTERS-IND              PIC X.
000083         88  CQ-LETTERS-REQUIRED           VALUE 'Y'.
000084
000085******************************************************************
      *<<((file: ERCCHKQ))
000440*                            COPY ERCPYAJ.
      *>>((file: ERCPYAJ))
000001******************************************************************
000002*                                                                *
000003*                            ERCPYAJ                             *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.015                          *
000006*                                                                *
000007*   FILE DESCRIPTION = PENDING PAYMENT AND ADJUSTMENTS           *
000008*                                                                *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 200  RECFORM = FIXED                           *
000012*                                                                *
000013*   BASE CLUSTER = ERPYAJ                         RKP=2,LEN=33   *
000014*       ALTERNATE PATHS = NONE                                   *
000015*                                                                *
000016*   LOG = YES                                                    *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019******************************************************************
000020*                   C H A N G E   L O G
000021*
000022* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000023*-----------------------------------------------------------------
000024*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000025* EFFECTIVE    NUMBER
000026*-----------------------------------------------------------------
000027* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
000028* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
000029******************************************************************
000030
000031 01  PENDING-PAY-ADJ.
000032     12  PY-RECORD-ID                     PIC XX.
000033         88  VALID-PY-ID                        VALUE 'PY'.
000034
000035     12  PY-CONTROL-PRIMARY.
000036         16  PY-COMPANY-CD                PIC X.
000037         16  PY-CARRIER                   PIC X.
000038         16  PY-GROUPING                  PIC X(6).
000039         16  PY-FIN-RESP                  PIC X(10).
000040         16  PY-ACCOUNT                   PIC X(10).
000041         16  PY-PRODUCER REDEFINES PY-ACCOUNT
000042                                          PIC X(10).
000043         16  PY-FILE-SEQ-NO               PIC S9(8)     COMP.
000044         16  PY-RECORD-TYPE               PIC X.
000045             88  PY-REMIT-RECEIVED            VALUE 'R'.
000046             88  PY-DEPOSIT                   VALUE 'D'.
000047             88  PY-CHARGE-TO-AGENT           VALUE 'C'.
000048             88  PY-ADJ-REM-RECEIVED          VALUE 'S'.
000049             88  PY-ADJ-DEPOSIT               VALUE 'T'.
000050             88  PY-ADJ-CHG-TO-AGT            VALUE 'U'.
000051             88  PY-ADD-TO-YTD-COMP           VALUE 'X'.
000052             88  PY-SUBTRACT-YTD-COMP         VALUE 'Y'.
000053             88  PY-ADD-TO-BALANCE            VALUE 'Z'.
000054             88  PY-FICA-ENTRY                VALUE 'F'.
000055             88  PY-REMIT-IND-GROUPING        VALUE 'G'.
000056             88  PY-POLICY-FEE                VALUE 'W'.
000057             88  PY-DUE-PREM-ADJ              VALUE 'P'.
000058
000059     12  PY-PYMT-TYPE                     PIC X.
000060             88  PY-NEW-BUS-PYMT              VALUE 'B'.
000061             88  PY-REINS-PYMT                VALUE 'R'.
000062             88  PY-EXP-PYMT                  VALUE 'E'.
000063
000064     12  PY-BIL-INV                       PIC X(6).
000065     12  PY-REF-NO                        PIC X(12).
000066
000067     12  PY-LAST-MAINT-DT                 PIC XX.
000068     12  PY-LAST-MAINT-BY                 PIC X(4).
000069     12  PY-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
000070
000071     12  PY-PYADJ-RECORD.
000072         16  PY-ENTRY-AMT                 PIC S9(7)V99  COMP-3.
000073         16  PY-ENTRY-COMMENT             PIC X(30).
000074         16  PY-GL-DATA      REDEFINES PY-ENTRY-COMMENT.
000075             20  PY-GL-ACCOUNT            PIC X(10).
000076             20  PY-GL-STATE              PIC X(02).
000077             20  PY-GL-CANC-SW            PIC X(01).
000078                 88  PY-GL-CANC-SW-ON     VALUE 'Y'.
000079                 88  PY-GL-CANC-SW-OFF    VALUE 'N'.
000080             20  PY-GL-COMMENT            PIC X(10).
000081             20  FILLER      REDEFINES PY-GL-COMMENT.
000082                 24  PY-GL-CHECK-NO       PIC 9(06).
000083                 24  FILLER               PIC X(04).
000084             20  FILLER                   PIC X(07).
000085         16  PY-SAVE-ACCOUNT              PIC X(10).
000086         16  PY-SAVE-TYPE                 PIC X(01).
000087
000088         16  PY-LETTERS.
000089             20  PY-LETTER OCCURS 3 TIMES
000090                           INDEXED BY PY-LET-NDX
000091                                          PIC X(04).
000092
000093         16  PY-ERCOMP-TYPE               PIC X.
000094             88  PY-ACCOUNT-TYPE              VALUE 'A'.
000095             88  PY-GA-TYPE                   VALUE 'G'.
000096             88  PY-BANK-TYPE                 VALUE 'B'.
000097         16  FILLER                       PIC X(05).
000098
000099     12  PY-RECORD-STATUS.
000100         16  PY-CREDIT-SELECT-DT          PIC XX.
000101         16  PY-CREDIT-ACCEPT-DT          PIC XX.
000102         16  PY-BILLED-DATE               PIC XX.
000103         16  PY-REPORTED-DT               PIC XX.
000104         16  PY-PMT-APPLIED               PIC X.
000105             88  PY-ACCOUNT-PMT               VALUE 'A'.
000106             88  PY-GA-PMT                    VALUE 'G'.
000107             88  PY-OVWRITE-PMT               VALUE 'O'.
000108             88  PY-NON-AR-PMT                VALUE 'N'.
000109         16  FILLER                       PIC X(5).
000110         16  PY-INPUT-DT                  PIC XX.
000111         16  PY-CHECK-NUMBER              PIC X(6).
000112         16  PY-VOID-SW                   PIC X.
000113             88  PY-CHECK-VOIDED              VALUE 'V'.
000114         16  PY-CHECK-ORIGIN-SW           PIC X.
000115             88  PY-BILLING-CHECK             VALUE 'B'.
000116             88  PY-REFUND-CHECK              VALUE 'R'.
000117             88  PY-GA-CHECK                  VALUE 'G'.
000118             88  PY-CHECK-WRITTEN             VALUE 'W'.
000119             88  PY-CHECK-REVERSAL            VALUE 'V'.
000120         16  PY-CHECK-WRITTEN-DT          PIC XX.
000121         16  PY-CHECK-QUE-CONTROL         PIC S9(8) COMP.
000122         16  PY-CHECK-QUE-SEQUENCE        PIC S9(4) COMP.
000123         16  PY-BILL-FLAG                 PIC X.
000124             88  PY-BILLED                    VALUE 'B'.
000125         16  PY-AR-FLAG                   PIC X.
000126             88  PY-AR-CYCLE                  VALUE 'C'.
000127             88  PY-AR-MONTH-END              VALUE 'M'.
000128         16  PY-AR-DATE                   PIC XX.
000129
000130     12  PY-GL-CODES.
000131         16  PY-GL-DB                     PIC X(14).
000132         16  PY-GL-CR                     PIC X(14).
000133         16  PY-GL-FLAG                   PIC X.
000134         16  PY-GL-DATE                   PIC XX.
000135
000136     12  PY-CANCEL-FEE-FLAG               PIC X(2).
000137     12  FILLER                           PIC X(3).
000138******************************************************************
000139
      *<<((file: ERCPYAJ))
000442*                            COPY ERCCOMP.
      *>>((file: ERCCOMP))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCCOMP                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.019                          *
000007*                                                                *
000008*   ONLINE CREDIT SYSTEM                                         *
000009*                                                                *
000010*   FILE DESCRIPTION = COMPENSATION MASTER                       *
000011*                                                                *
000012*   FILE TYPE = VSAM,KSDS                                        *
000013*   RECORD SIZE = 700   RECFORM = FIXED                          *
000014*                                                                *
000015*   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
000016*       ALTERNATE PATH = NONE                                    *
000017*                                                                *
000018*   LOG = NO                                                     *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020*                                                                *
000021******************************************************************
000022*                   C H A N G E   L O G
000023*
000024* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000025*-----------------------------------------------------------------
000026*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000027* EFFECTIVE    NUMBER
000028*-----------------------------------------------------------------
000029* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
000030* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
000031* 092205    2005050300006  PEMA  ADD LEASE FEE
000032* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
000033* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
000034* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
000035* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000036* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
000037* 071712  CR2012042700005  PEMA  ADD OVER 120 FOR AHL ONLY
000038******************************************************************
000039
000040 01  COMPENSATION-MASTER.
000041     12  CO-RECORD-ID                          PIC XX.
000042         88  VALID-CO-ID                          VALUE 'CO'.
000043
000044     12  CO-CONTROL-PRIMARY.
000045         16  CO-COMPANY-CD                     PIC X.
000046         16  CO-CONTROL.
000047             20  CO-CTL-1.
000048                 24  CO-CARR-GROUP.
000049                     28  CO-CARRIER            PIC X.
000050                     28  CO-GROUPING.
000051                         32  CO-GROUP-PREFIX   PIC XXX.
000052                         32  CO-GROUP-PRIME    PIC XXX.
000053                 24  CO-RESP-NO.
000054                     28  CO-RESP-PREFIX        PIC X(4).
000055                     28  CO-RESP-PRIME         PIC X(6).
000056             20  CO-CTL-2.
000057                 24  CO-ACCOUNT.
000058                     28  CO-ACCT-PREFIX        PIC X(4).
000059                     28  CO-ACCT-PRIME         PIC X(6).
000060         16  CO-TYPE                           PIC X.
000061             88  CO-COMPANY-TYPE                  VALUE 'C'.
000062             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
000063             88  CO-ACCOUNT-TYPE                  VALUE 'A'.
000064
000065     12  CO-MAINT-INFORMATION.
000066         16  CO-LAST-MAINT-DT                  PIC XX.
000067         16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
000068         16  CO-LAST-MAINT-USER                PIC X(4).
000069     12  FILLER                                PIC XX.
000070     12  CO-STMT-TYPE                          PIC XXX.
000071     12  CO-COMP-TYPE                          PIC X.
000072         88  CO-COMP-IS-SPPDD                    VALUE '1'.
000073     12  CO-STMT-OWNER                         PIC X(4).
000074     12  CO-BALANCE-CONTROL                    PIC X.
000075         88  CO-CARRY-BALANCE                     VALUE 'Y'.
000076         88  CO-NO-BALANCE                        VALUE 'N'.
000077
000078     12  CO-INTERNAL-CONTROL-1                 PIC X.
000079         88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
000080         88  CO-AUTO-GENERATED                    VALUE 'Y'.
000081         88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
000082
000083     12  CO-INTERNAL-CONTROL-2                 PIC X.
000084         88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
000085         88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
000086
000087     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
000088     12  CO-GA-DIRECT-DEP                      PIC X.
000089     12  CO-FUTURE-SPACE                       PIC X.
000090         88  CO-FUTURE-NOT-USED                   VALUE ' '.
000091
000092     12  CO-ACCT-NAME                          PIC X(30).
000093     12  CO-MAIL-NAME                          PIC X(30).
000094     12  CO-ADDR-1                             PIC X(30).
000095     12  CO-ADDR-2                             PIC X(30).
000096     12  CO-ADDR-3.
000097         16  CO-ADDR-CITY                      PIC X(27).
000098         16  CO-ADDR-STATE                     PIC XX.
000099     12  CO-CSO-1099                           PIC X.
000100     12  CO-ZIP.
000101         16  CO-ZIP-PRIME.
000102             20  CO-ZIP-PRI-1ST                PIC X.
000103                 88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
000104             20  FILLER                        PIC X(4).
000105         16  CO-ZIP-PLUS4                      PIC X(4).
000106     12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
000107         16  CO-CAN-POSTAL-1                   PIC XXX.
000108         16  CO-CAN-POSTAL-2                   PIC XXX.
000109         16  FILLER                            PIC XXX.
000110     12  CO-SOC-SEC                            PIC X(13).
000111     12  CO-TELEPHONE.
000112         16  CO-AREA-CODE                      PIC XXX.
000113         16  CO-PREFIX                         PIC XXX.
000114         16  CO-PHONE                          PIC X(4).
000115
000116     12  CO-ROLADEX-PRINT-DT                   PIC XX.
000117
000118     12  CO-AR-BAL-LEVEL                       PIC X.
000119         88  CO-AR-REF-LVL                        VALUE '1'.
000120         88  CO-AR-BILL-REF-LVL                   VALUE '1'.
000121         88  CO-AR-BILL-LVL                       VALUE '2'.
000122         88  CO-AR-AGT-LVL                        VALUE '3'.
000123         88  CO-AR-FR-LVL                         VALUE '4'.
000124
000125     12  CO-AR-NORMAL-PRINT                    PIC X.
000126         88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
000127         88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
000128
000129     12  CO-AR-SUMMARY-CODE                    PIC X(6).
000130
000131     12  CO-AR-REPORTING                       PIC X.
000132         88  CO-AR-NET-REPORT                     VALUE 'N'.
000133         88  CO-AR-GROSS-REPORT                   VALUE 'G'.
000134
000135     12  CO-AR-PULL-CHECK                      PIC X.
000136         88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
000137         88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
000138
000139     12  CO-AR-BALANCE-PRINT                   PIC X.
000140         88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
000141
000142     12  CO-AR-LAST-RUN-CODE                   PIC X.
000143         88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
000144         88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
000145         88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
000146
000147     12  CO-LAST-EOM-STMT-DT                   PIC XX.
000148
000149     12  CO-USER-CODE                          PIC X.
000150     12  CO-REPORT-GROUP-ID                    PIC X(12).
000151
000152******************************************************************
000153*    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
000154*    THE LAST MONTH END RUN.
000155******************************************************************
000156
000157     12  CO-LAST-ACTIVITY-DATE.
000158         16  CO-ACT-YEAR                       PIC 99.
000159         16  CO-ACT-MONTH                      PIC 99.
000160         16  CO-ACT-DAY                        PIC 99.
000161
000162     12  CO-LAST-STMT-DT.
000163         16  CO-LAST-STMT-YEAR                 PIC 99.
000164         16  CO-LAST-STMT-MONTH                PIC 99.
000165         16  CO-LAST-STMT-DAY                  PIC 99.
000166
000167     12  CO-MO-END-TOTALS.
000168         16  CO-MONTHLY-TOTALS.
000169             20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
000170             20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
000171             20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
000172             20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
000173             20  CO-END-BAL                PIC S9(7)V99   COMP-3.
000174
000175         16  CO-AGING-TOTALS.
000176             20  CO-CUR                    PIC S9(7)V99   COMP-3.
000177             20  CO-OV30                   PIC S9(7)V99   COMP-3.
000178             20  CO-OV60                   PIC S9(7)V99   COMP-3.
000179             20  CO-OV90                   PIC S9(7)V99   COMP-3.
000180
000181         16  CO-YTD-TOTALS.
000182             20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
000183             20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
000184
000185         16  CO-OVER-UNDER-TOTALS.
000186             20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
000187             20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
000188
000189     12  CO-MISCELLANEOUS-TOTALS.
000190         16  CO-FICA-TOTALS.
000191             20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
000192             20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
000193
000194         16  CO-CLAIM-TOTALS.
000195             20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
000196             20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
000197
000198******************************************************************
000199*    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
000200*    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
000201******************************************************************
000202
000203     12  CO-CURRENT-TOTALS.
000204         16  CO-CURRENT-LAST-STMT-DT.
000205             20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
000206             20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
000207             20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
000208
000209         16  CO-CURRENT-MONTHLY-TOTALS.
000210             20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
000211             20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
000212             20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
000213             20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
000214             20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
000215
000216         16  CO-CURRENT-AGING-TOTALS.
000217             20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
000218             20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
000219             20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
000220             20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
000221
000222         16  CO-CURRENT-YTD-TOTALS.
000223             20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
000224             20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
000225
000226     12  CO-PAID-COMM-TOTALS.
000227         16  CO-YTD-PAID-COMMS.
000228             20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
000229             20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
000230
000231     12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
000232         88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
000233         88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
000234
000235     12  CO-DELINQUENT-LETTER-CODE         PIC X.
000236         88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
000237         88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
000238         88  CO-AGENT-1ST-LETTER              VALUE 'B'.
000239         88  CO-AGENT-2ND-LETTER              VALUE 'G'.
000240         88  CO-OVERWRITE-LETTER              VALUE 'O'.
000241         88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
000242         88  CO-FINAL-LETTER                  VALUE 'F'.
000243         88  CO-RECONCILING                   VALUE 'R'.
000244         88  CO-PHONE-CALL                    VALUE 'P'.
000245         88  CO-LEGAL                         VALUE 'L'.
000246         88  CO-COLLECTION-AGENCY             VALUE 'C'.
000247         88  CO-WRITE-OFF                     VALUE 'W'.
000248         88  CO-NO-ACTION                     VALUE 'N' ' '.
000249
000250     12  CO-CSR-CODE                       PIC X(4).
000251
000252     12  CO-GA-STATUS-INFO.
000253         16  CO-GA-EFFECTIVE-DT            PIC XX.
000254         16  CO-GA-TERMINATION-DT          PIC XX.
000255         16  CO-GA-STATUS-CODE             PIC X.
000256             88  CO-GA-ACTIVE                 VALUE 'A'.
000257             88  CO-GA-INACTIVE               VALUE 'I'.
000258             88  CO-GA-PENDING                VALUE 'P'.
000259         16  CO-GA-COMMENTS.
000260             20  CO-GA-COMMENT-1           PIC X(40).
000261             20  CO-GA-COMMENT-2           PIC X(40).
000262             20  CO-GA-COMMENT-3           PIC X(40).
000263             20  CO-GA-COMMENT-4           PIC X(40).
000264
000265     12  CO-RPTCD2                         PIC X(10).
000266     12  CO-AHL-OVER120-DATA REDEFINES CO-RPTCD2.
000267         16  CO-OV120                      PIC S9(7)V99   COMP-3.
000268         16  CO-CURRENT-OV120              PIC S9(7)V99   COMP-3.
000269
000270     12  CO-TYPE-AGENT                     PIC X(01).
000271         88  CO-CORPORATION                   VALUE 'C'.
000272         88  CO-PARTNERSHIP                   VALUE 'P'.
000273         88  CO-SOLE-PROPRIETOR               VALUE 'S'.
000274         88  CO-TRUST                         VALUE 'T'.
000275         88  CO-UNKNOWN                       VALUE ' ' 'X'.
000276
000277     12  CO-FAXNO.
000278         16  CO-FAX-AREA-CODE                  PIC XXX.
000279         16  CO-FAX-PREFIX                     PIC XXX.
000280         16  CO-FAX-PHONE                      PIC X(4).
000281
000282     12  CO-BANK-INFORMATION.
000283         16  CO-BANK-TRANSIT-NO                PIC X(8).
000284         16  CO-BANK-TRANSIT-NON REDEFINES
000285             CO-BANK-TRANSIT-NO                PIC 9(8).
000286
000287         16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
000288     12  CO-MISC-DEDUCT-INFO REDEFINES
000289                  CO-BANK-INFORMATION.
000290         16  CO-MD-GL-ACCT                     PIC X(10).
000291         16  CO-MD-DIV                         PIC XX.
000292         16  CO-MD-CENTER                      PIC X(4).
000293         16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
000294         16  CO-CREATE-AP-CHECK                PIC X.
000295         16  CO-DELIVER-CK-TO-MEL              PIC X.
000296         16  FILLER                            PIC XXX.
000297     12  CO-ACH-STATUS                         PIC X.
000298         88  CO-ACH-ACTIVE                         VALUE 'A'.
000299         88  CO-ACH-PENDING                        VALUE 'P'.
000300
000301     12  CO-BILL-SW                            PIC X.
000302     12  CO-CONTROL-NAME                       PIC X(30).
000303     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
000304     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
000305     12  CO-CLP-STATE                          PIC XX.
000306     12  CO-FIRST-WRITTEN-DT                   PIC XX.
000307     12  CO-SPP-REFUND-EDIT                    PIC X.
000308
000309******************************************************************
      *<<((file: ERCCOMP))
000443*
000444*                            COPY AIRL0009.
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL633' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000446 VCOBOL-DUMMY-PROCEDURE.
000447
000448     MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
000449     MOVE EIBTRMID               TO  QID-TERM.
000450     MOVE 2                      TO  EMI-NUMBER-OF-LINES.
000451
000452     IF EIBCALEN = ZERO
000453         GO TO 8800-UNAUTHORIZED-ACCESS.
000454
000455     IF PI-RETURN-TO-PROGRAM = THIS-PGM
000456         MOVE PI-CALLING-PROGRAM TO RETURNED-FROM.
000457
000458     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
000459     MOVE '5'                    TO  DC-OPTION-CODE.
000460
000461     PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
000462
000463     MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.
000464     MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.
000465
000466     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000467         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000468             MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
000469             MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
000470             MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
000471             MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
000472             MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
000473             MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
000474             MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
000475             MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
000476         ELSE
000477             MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
000478             MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
000479             MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
000480             MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
000481             MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
000482             MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
000483             MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
000484             MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
000485
000486     MOVE LOW-VALUES             TO  EL633AI.
000487
000488     MOVE DC-JULIAN-YYDDD        TO  WORK-DATE-JULIAN.
000489     COMPUTE WORK-SEQ-NO = WORK-JULIAN-DD * 100000.
000490     MOVE EIBTIME                TO  WORK-SEQ-TIME.
000491     ADD WORK-SEQ-HHMMS TO WORK-SEQ-NO.
000492
000493     MOVE ZEROS                  TO  TOTAL-ACCT-AMT
000494                                     TOTAL-ACCT-NET.
000495
000496     IF RETURNED-FROM = XCTL-6331 OR
000497                        XCTL-640  OR
000498                        XCTL-642  OR
000499                        XCTL-652
000500         PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0690-EXIT.
000501
000502     IF EIBTRNID NOT = TRANS-ID
000503         MOVE 'T'                TO  PI-PYAJ-FILE-SW
000504         MOVE 'Y'                TO  PI-PAGE-SW
000505         MOVE ZEROS              TO  PI-SEQ-NOS
000506         MOVE ZEROS              TO  PI-SAV-ACCT-AMT
000507                                     PI-SAV-ACCT-NET
000508                                     PI-SAV-PREV-AMT
000509                                     PI-SAV-PREV-NET
000510         IF (EIBTRNID NOT = EL640-TRANS-ID  AND
000511             EIBTRNID NOT = EL642-TRANS-ID  AND
000512             EIBTRNID NOT = EL652-TRANS-ID) OR
000513             PI-CR-FIN-RESP = SPACES
000514             GO TO 8100-SEND-INITIAL-MAP
000515         ELSE
000516             IF EIBTRNID = (EL6331-TRANS-ID  OR
000517                            EL640-TRANS-ID   OR
000518                            EL642-TRANS-ID)  AND
000519                PI-CR-CONTROL-IN-PROGRESS  = SPACES
000520                GO TO 8100-SEND-INITIAL-MAP
000521             ELSE
000522                 MOVE DFHENTER   TO  EIBAID
000523                 MOVE 'S'        TO  MAINTI
000524                 MOVE PI-CR-CARRIER
000525                                 TO  CARRIERI
000526                 MOVE PI-CR-GROUPING
000527                                 TO  GROUPI
000528                 MOVE PI-CR-FIN-RESP
000529                                 TO  FINRESPI
000530                 MOVE PI-CR-ACCOUNT
000531                                 TO  ACCTI
000532                 MOVE 1          TO  CARRIERL  MAINTA
000533                 MOVE 3          TO  GROUPL
000534                 MOVE 6          TO  FINRESPL  ACCTL
000535                 GO TO 0400-VALIDATE-KEY-DATA.
000536
000537     
      * EXEC CICS HANDLE CONDITION
000538*        PGMIDERR  (9600-PGMID-ERROR)
000539*        ERROR     (9990-ABEND)
000540*    END-EXEC.
      *    MOVE '"$L.                  ! " #00003239' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303033323339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000541
000542     IF EIBAID = DFHCLEAR
000543         GO TO 9400-CLEAR.
000544
000545     IF PI-PROCESSOR-ID = 'LGXX'
000546         GO TO 0200-RECEIVE.
000547
000548     
      * EXEC CICS READQ TS
000549*        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
000550*        INTO   (SECURITY-CONTROL)
000551*        LENGTH (SC-COMM-LENGTH)
000552*        ITEM   (SC-ITEM)
000553*    END-EXEC.
      *    MOVE '*$II   L              ''   #00003250' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033323530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000554
000555     MOVE SC-CREDIT-DISPLAY (15)  TO PI-DISPLAY-CAP.
000556     MOVE SC-CREDIT-UPDATE  (15)  TO PI-MODIFY-CAP.
000557
000558     IF NOT DISPLAY-CAP
000559         MOVE 'READ'          TO SM-READ
000560         PERFORM 9995-SECURITY-VIOLATION
000561         MOVE ER-0070         TO  EMI-ERROR
000562         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000563         GO TO 8100-SEND-INITIAL-MAP.
000564
000565 EJECT
000566 0200-RECEIVE.
000567
000568     IF EIBAID = DFHPA1  OR  DFHPA2  OR  DFHPA3
000569         MOVE ER-0008            TO  EMI-ERROR
000570         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000571         MOVE -1                 TO  MAINTL
000572         GO TO 8200-SEND-DATAONLY.
000573
000574     
      * EXEC CICS RECEIVE
000575*        MAP     (MAP-NAME)
000576*        MAPSET  (MAPSET-NAME)
000577*        INTO    (EL633AI)
000578*    END-EXEC.
           MOVE LENGTH OF
            EL633AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003276' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033323736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL633AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000579
000580     IF MAINTI = 'S' AND
000581        EIBAID = DFHENTER
000582         MOVE ZEROS              TO  PI-SAV-ACCT-AMT
000583                                     PI-SAV-ACCT-NET
000584                                     PI-SAV-PREV-AMT
000585                                     PI-SAV-PREV-NET
000586                                     PI-FRST-FILE-SEQ-NO
000587         MOVE SPACES             TO  PI-FRST-RECORD-TYPE.
000588
000589     IF PFENTERL = ZERO
000590         GO TO 0300-CHECK-PFKEYS.
000591
000592     IF (PFENTERI  IS NUMERIC)
000593       AND (PFENTERI GREATER 0 AND LESS 25)
000594         MOVE PF-VALUES (PFENTERI)  TO  EIBAID
000595     ELSE
000596         MOVE ER-0029               TO  EMI-ERROR
000597         GO TO 0320-INPUT-ERROR.
000598
000599 0300-CHECK-PFKEYS.
000600     IF EIBAID = DFHPF23
000601         GO TO 8810-PF23.
000602
000603     IF EIBAID = DFHPF24
000604         GO TO 9200-RETURN-MAIN-MENU.
000605
000606     IF EIBAID = DFHPF12
000607         GO TO 9500-PF12.
000608
000609     IF EIBAID = DFHPF3
000610         IF PI-CR-CONTROL-IN-PROGRESS NOT =      SPACES
000611             PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
000612             MOVE XCTL-652       TO  PGM-NAME
000613             IF ERCOMP-KEY  NOT =      SPACES
000614                 MOVE COMP-CARRIER   TO  PI-CR-CARRIER
000615                 MOVE COMP-GROUPING  TO  PI-CR-GROUPING
000616                 MOVE COMP-FIN-RESP  TO  PI-CR-FIN-RESP
000617                 MOVE COMP-ACCOUNT   TO  PI-CR-ACCOUNT
000618             END-IF
000619
000620             IF PI-CR-ACCOUNT = LOW-VALUES
000621                 MOVE 'G'        TO  PI-CR-TYPE
000622                 GO TO 9300-XCTL
000623             ELSE
000624                 MOVE 'A'        TO  PI-CR-TYPE
000625                 GO TO 9300-XCTL
000626         ELSE
000627            MOVE ER-3020         TO  EMI-ERROR
000628            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000629            GO TO 8100-SEND-INITIAL-MAP.
000630
000631     IF EIBAID = DFHPF4
000632         PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
000633         MOVE XCTL-6331          TO  PGM-NAME
000634         GO TO 9300-XCTL.
000635
000636     IF EIBAID = DFHPF5
000637         PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
000638         MOVE SPACES TO PI-PROGRAM-WORK-AREA
000639         MOVE XCTL-640           TO  PGM-NAME
000640         IF PI-CR-ACCOUNT = LOW-VALUES
000641             MOVE SPACES     TO  PI-CR-CONTROL-IN-PROGRESS
000642             IF ACCTI = LOW-VALUES
000643                MOVE 'G'         TO  PI-CR-TYPE
000644             END-IF
000645             GO TO 9300-XCTL
000646         ELSE
000647             MOVE 'A'        TO  PI-CR-TYPE
000648             GO TO 9300-XCTL.
000649
000650     IF EIBAID = DFHPF6
000651        IF PI-GA-BILLING
000652            PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
000653            MOVE SPACES TO PI-PROGRAM-WORK-AREA
000654            MOVE XCTL-642        TO  PGM-NAME
000655            IF PI-CR-ACCOUNT = LOW-VALUES
000656                 MOVE 'G'        TO  PI-CR-TYPE
000657                 GO TO 9300-XCTL
000658             ELSE
000659                 MOVE SPACES     TO  PI-CR-CONTROL-IN-PROGRESS
000660                 GO TO 9300-XCTL
000661        ELSE
000662            MOVE ER-2929         TO  EMI-ERROR
000663            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000664            MOVE AL-UNBON        TO  PFENTERA
000665            IF PFENTERL = ZERO
000666                MOVE -1          TO  MAINTL
000667                GO TO 8200-SEND-DATAONLY
000668            ELSE
000669                MOVE -1          TO  PFENTERL
000670                GO TO 8200-SEND-DATAONLY.
000671
000672     IF EIBAID = DFHENTER  OR  DFHPF1  OR  DFHPF2
000673         GO TO 0400-VALIDATE-KEY-DATA.
000674
000675 0320-INPUT-ERROR.
000676     MOVE ER-0029                TO  EMI-ERROR.
000677
000678     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000679
000680     MOVE AL-UNBON               TO  PFENTERA.
000681
000682     IF PFENTERL = ZERO
000683         MOVE -1                 TO  MAINTL
000684     ELSE
000685         MOVE -1                 TO  PFENTERL.
000686
000687     GO TO 8200-SEND-DATAONLY.
000688 EJECT
000689 0400-VALIDATE-KEY-DATA.
000690     IF MODIFY-CAP  OR (EIBAID = DFHPF1 OR DFHPF2)
000691         NEXT SENTENCE
000692       ELSE
000693        IF MAINTI NOT = 'S'
000694         MOVE 'UPDATE'       TO SM-READ
000695         PERFORM 9995-SECURITY-VIOLATION
000696         MOVE ER-0070        TO EMI-ERROR
000697         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000698         GO TO 8100-SEND-INITIAL-MAP.
000699
000700     MOVE PI-COMPANY-CD          TO  PI-SAV-COMP-CD.
000701
000702     IF EIBAID = DFHPF1
000703         GO TO 4000-BROWSE-FRWD.
000704
000705     IF EIBAID   = DFHPF1   AND
000706        CARRIERL = ZEROS    AND
000707        GROUPL   = ZEROS    AND
000708        FINRESPL = ZEROS    AND
000709        ACCTL    = ZEROS
000710         GO TO 4000-BROWSE-FRWD.
000711
000712     IF PI-PROCESSOR-ID = 'LGXX'
000713         IF MAINTI = 'C'
000714             MOVE AL-UANON       TO MAINTA
000715             MOVE MAINTI         TO PI-SAV-FUNCTION
000716             GO TO CSO-BRANCH
000717         END-IF
000718     END-IF.
000719
000720     IF MAINTI = 'C' OR  'S'
000721         MOVE AL-UANON           TO  MAINTA
000722         MOVE MAINTI             TO  PI-SAV-FUNCTION
000723     ELSE
000724         MOVE -1                 TO  MAINTL
000725         MOVE ER-0023            TO  EMI-ERROR
000726         MOVE AL-UABON           TO  MAINTA
000727         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000728
000729 CSO-BRANCH.
000730
000731     IF MAINTI = 'C'
000732       AND PI-PREV-FUNCTION NOT = 'S' AND 'C'
000733         MOVE -1                 TO  MAINTL
000734         MOVE ER-2056            TO  EMI-ERROR
000735         MOVE AL-UABON           TO  MAINTA
000736         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000737
000738     IF CARRIERL = ZEROS  AND
000739        GROUPL   = ZEROS  AND
000740        FINRESPL = ZEROS  AND
000741        ACCTL    = ZEROS
000742         MOVE -1                 TO  CARRIERL
000743         MOVE ER-2231            TO  EMI-ERROR
000744         MOVE AL-UABON           TO  CARRIERA
000745                                     GROUPA
000746                                     FINRESPA
000747                                     ACCTA
000748         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000749         GO TO 8200-SEND-DATAONLY.
000750
000751     IF CARRIERL NOT = ZEROS
000752         MOVE AL-UANON           TO  CARRIERA
000753         MOVE CARRIERI           TO  COMP-CARRIER
000754                                     PI-SAV-CARRIER
000755                                     PI-CR-CARRIER
000756         IF CARRIERI NOT = ZEROS
000757           AND (PI-ZERO-CARRIER  OR  PI-ZERO-CAR-GROUP)
000758             MOVE ER-2587        TO  EMI-ERROR
000759             MOVE -1             TO  CARRIERL
000760             MOVE AL-UABON       TO  CARRIERA
000761             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000762         ELSE
000763             NEXT SENTENCE
000764     ELSE
000765         MOVE LOW-VALUES          TO  COMP-CARRIER
000766                                      PI-SAV-CARRIER.
000767
000768     IF CARRIERL NOT = ZEROS
000769         IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
000770             IF PI-CARRIER-SECURITY = CARRIERI
000771                 NEXT SENTENCE
000772             ELSE
000773                 MOVE -1         TO  CARRIERL
000774                 MOVE ER-2370    TO  EMI-ERROR
000775                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000776
000777     IF GROUPL NOT = ZEROS
000778         MOVE AL-UANON           TO  GROUPA
000779         MOVE GROUPI             TO  COMP-GROUPING
000780                                     PI-SAV-GROUPING
000781                                     PI-CR-GROUPING
000782         IF GROUPI NOT = ZEROS
000783           AND (PI-ZERO-GROUPING  OR  PI-ZERO-CAR-GROUP)
000784             MOVE ER-2588        TO  EMI-ERROR
000785             MOVE -1             TO  GROUPL
000786             MOVE AL-UABON       TO  GROUPA
000787             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000788         ELSE
000789             NEXT SENTENCE
000790     ELSE
000791         MOVE LOW-VALUES         TO  COMP-GROUPING
000792                                     PI-SAV-GROUPING.
000793
000794     IF FINRESPL NOT = ZEROS
000795         MOVE AL-UANON           TO  FINRESPA
000796         MOVE FINRESPI           TO  COMP-FIN-RESP
000797                                     PI-SAV-FIN-RESP
000798                                     PI-CR-FIN-RESP
000799     ELSE
000800         MOVE LOW-VALUES         TO  COMP-FIN-RESP
000801                                     PI-SAV-FIN-RESP.
000802
000803     IF ACCTL NOT = ZEROS
000804         MOVE AL-UANON           TO  ACCTA
000805         MOVE ACCTI              TO  COMP-ACCOUNT
000806                                     PI-SAV-ACCOUNT
000807                                     PI-CR-ACCOUNT
000808     ELSE
000809         MOVE LOW-VALUES         TO  COMP-ACCOUNT
000810                                     PI-SAV-ACCOUNT
000811                                     PI-CR-ACCOUNT.
000812
000813     IF EMI-ERROR = ZEROS
000814         NEXT SENTENCE
000815     ELSE
000816         IF EIBTRNID = EL640-TRANS-ID OR
000817            EIBTRNID = EL642-TRANS-ID OR
000818            EIBTRNID = EL6331-TRANS-ID
000819             GO TO 8100-SEND-INITIAL-MAP
000820         ELSE
000821             GO TO 8200-SEND-DATAONLY.
000822
000823     MOVE MAINTI                 TO  PI-PREV-FUNCTION.
000824
000825     IF MAINTI = 'S'
000826         IF EIBAID = DFHPF1              OR  DFHENTER
000827             GO TO 4000-BROWSE-FRWD
000828         ELSE
000829             GO TO 4100-BROWSE-BKWD.
000830
000831     MOVE SPACES                 TO  COMP-RECORD-TYPE.
000832     MOVE PI-COMPANY-CD          TO  COMP-COMP-CD.
000833
000834     
      * EXEC CICS HANDLE CONDITION
000835*        NOTFND   (0410-NO-COMP-MSTR)
000836*        NOTOPEN  (7100-COMP-FILE-NOTOPEN)
000837*        END-EXEC.
      *    MOVE '"$IJ                  ! # #00003536' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303033353336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000838
000839     
      * EXEC CICS READ
000840*        DATASET  (COMP-FILE-ID)
000841*        SET      (ADDRESS OF COMPENSATION-MASTER)
000842*        RIDFLD   (ERCOMP-KEY)
000843*        GTEQ
000844*        END-EXEC.
      *    MOVE '&"S        G          (   #00003541' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303033353431' TO DFHEIV0
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
           
000845
000846     IF PI-COMPANY-CD = CO-COMPANY-CD  AND
000847        COMP-CARRIER  = CO-CARRIER     AND
000848        COMP-GROUPING = CO-GROUPING    AND
000849        COMP-FIN-RESP = CO-RESP-NO    AND
000850        COMP-ACCOUNT  = CO-ACCOUNT
000851         NEXT SENTENCE
000852     ELSE
000853         GO TO 0410-NO-COMP-MSTR.
000854
000855     IF PI-COMPANY-ID = 'NCL'
000856         IF CO-GA-INACTIVE
000857             GO TO 0420-INACTIVE-COMP.
000858
000859     IF MAINTI = 'C'
000860         GO TO 1000-EDIT-DATA.
000861
000862     IF EIBAID = DFHPF1
000863       OR DFHENTER
000864         GO TO 4000-BROWSE-FRWD.
000865
000866     IF EIBAID = DFHPF2
000867         GO TO 4100-BROWSE-BKWD.
000868
000869 0410-NO-COMP-MSTR.
000870     MOVE ER-2230                TO  EMI-ERROR.
000871     MOVE -1                     TO  CARRIERL.
000872     MOVE AL-UABON               TO  CARRIERA
000873                                     GROUPA
000874                                     FINRESPA
000875                                     ACCTA.
000876
000877     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000878
000879     GO TO 8200-SEND-DATAONLY.
000880
000881 0420-INACTIVE-COMP.
000882     MOVE ER-2763                TO  EMI-ERROR.
000883     MOVE -1                     TO  CARRIERL.
000884     MOVE AL-UABON               TO  CARRIERA
000885                                     GROUPA
000886                                     FINRESPA
000887                                     ACCTA.
000888
000889     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000890
000891     GO TO 8200-SEND-DATAONLY.
000892
000893     EJECT
000894 0500-CREATE-TEMP-STORAGE.
000895
000896     PERFORM 0800-DELETE-TS  THRU  0890-EXIT.
000897
000898     
      * EXEC CICS WRITEQ TS
000899*        QUEUE  (QID)
000900*        FROM   (PROGRAM-INTERFACE-BLOCK)
000901*        LENGTH (PI-COMM-LENGTH)
000902*    END-EXEC.
      *    MOVE '*"     L              ''   #00003600' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033363030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000903
000904 0590-EXIT.
000905      EXIT.
000906
000907 0600-RECOVER-TEMP-STORAGE.
000908     
      * EXEC CICS READQ TS
000909*        QUEUE  (QID)
000910*        INTO   (PROGRAM-INTERFACE-BLOCK)
000911*        LENGTH (PI-COMM-LENGTH)
000912*    END-EXEC.
      *    MOVE '*$I    L              ''   #00003610' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033363130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000913
000914     PERFORM 0800-DELETE-TS THRU 0890-EXIT.
000915
000916 0690-EXIT.
000917      EXIT.
000918
000919 0800-DELETE-TS.
000920     
      * EXEC CICS HANDLE CONDITION
000921*        QIDERR (0890-EXIT)
000922*    END-EXEC.
      *    MOVE '"$N                   ! $ #00003622' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303033363232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000923
000924     
      * EXEC CICS DELETEQ TS
000925*        QUEUE  (QID)
000926*    END-EXEC.
      *    MOVE '*&                    #   #00003626' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033363236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000927
000928     
      * EXEC CICS SYNCPOINT
000929*    END-EXEC.
      *    MOVE '6"                    !   #00003630' TO DFHEIV0
           MOVE X'362220202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303033363330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000930
000931 0890-EXIT.
000932      EXIT.
000933     EJECT
000934
000935 1000-EDIT-DATA.
000936     SET PINDX                   TO  1.
000937
000938     .
000939 1010-EDIT-LOOP.
000940     SET NDX                     TO  PINDX.
000941     IF GL-ACCT-LEN (PINDX)   = ZEROS  AND
000942        GL-STATE-LEN (PINDX) = ZEROS   AND
000943        GL-CANC-LEN (PINDX)   = ZEROS  AND
000944        GL-COMM-LEN (PINDX)   = ZEROS  AND
000945        RTYPE-LEN (PINDX)     = ZEROS  AND
000946        AMT-LEN (PINDX)       = ZEROS  AND
000947        VOID-SW-LEN (PINDX)   = ZEROS  AND
000948        SDTE-LEN (PINDX)      = ZEROS
000949         GO TO 1020-INCREMENT-PINDX.
000950
000951     IF GL-ACCT-LEN (PINDX) NOT = ZEROS
000952         MOVE AL-UANON           TO  GL-ACCT-ATTRB (PINDX)
000953         IF PI-COMPANY-ID NOT = 'WSL'
000954             NEXT SENTENCE
000955         ELSE
000956             MOVE WSL-COMM-DTE (PINDX)  TO  DC-GREG-DATE-1-MDY
000957             MOVE '4'                   TO  DC-OPTION-CODE
000958             PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
000959             IF NO-CONVERSION-ERROR
000960                 NEXT SENTENCE
000961             ELSE
000962                 MOVE ER-2595    TO  EMI-ERROR
000963                 MOVE -1         TO  GL-ACCT-LEN (PINDX)
000964                 MOVE AL-UABON   TO  GL-ACCT-ATTRB (PINDX)
000965                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000966     ELSE
000967         IF PI-COMPANY-ID = 'WSL'
000968           AND PI-FILE-SEQ-NO (NDX) = ZEROS
000969             MOVE ER-2596        TO  EMI-ERROR
000970             MOVE -1             TO  GL-ACCT-LEN (PINDX)
000971             MOVE AL-UABON       TO  GL-ACCT-ATTRB (PINDX)
000972             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000973
000974
000975     IF GL-ACCT-LEN (PINDX) NOT = ZEROES
000976*        MOVE K-SPACE            TO COFA-FILLER
000977*        MOVE '0001'             TO COFA-COMPANY-X
000978*        MOVE MSA-ACCT (PINDX)   TO COFA-MSA-ACCT
000979         MOVE AL-UANON           TO GL-ACCT-ATTRB (PINDX)
000980     ELSE
000981         IF PI-FILE-SEQ-NO (NDX) = ZEROES
000982             MOVE -1             TO GL-ACCT-LEN (PINDX)
000983             MOVE ER-2957        TO EMI-ERROR
000984             MOVE AL-UABON       TO GL-ACCT-ATTRB (PINDX)
000985             PERFORM 9900-ERROR-FORMAT THRU
000986                     9900-EXIT
000987             GO TO 1040-CONTINUE-EDIT
000988         ELSE
000989             GO TO 1040-CONTINUE-EDIT
000990         END-IF
000991     END-IF.
000992
000993     IF (PI-COMPANY-ID = 'DCC' or 'VPP')
000994        AND (RTYPE (PINDX) = 'P')
000995        GO TO 1040-CONTINUE-EDIT
000996     END-IF
000997
000998     IF GL-ACCT-LEN (PINDX) NOT = ZEROS
000999        MOVE GL-ACCT (PINDX)     TO CHECK-GL-ACCT
001000     ELSE
001001        MOVE SPACES              TO CHECK-GL-ACCT
001002     END-IF
001003
001004     EVALUATE TRUE
001005        WHEN (PI-COMPANY-ID = 'DCC')
001006           AND (CO-CARRIER = '1' OR '2' or '9')
001007           AND (VALID-DCC-GL-ACCOUNT)
001008           GO TO 1040-CONTINUE-EDIT
001009        WHEN (PI-COMPANY-ID = 'DCC')
001010           AND (CO-CARRIER = '3' OR '4')
001011           AND VALID-CSI-GL-ACCOUNT
001012           GO TO 1040-CONTINUE-EDIT
001013        WHEN (PI-COMPANY-ID = 'DCC')
001014           AND (CO-CARRIER = '5' OR '6' or '7')
001015           AND (VALID-CCC-GL-ACCOUNT)
001016           GO TO 1040-CONTINUE-EDIT
001017        WHEN (PI-COMPANY-ID = 'CID' OR 'AHL')
001018           AND (VALID-GL-ACCOUNT)
001019           GO TO 1040-CONTINUE-EDIT
001020        WHEN (PI-COMPANY-ID = 'VPP')
001021           AND (VALID-VPP-GL-ACCOUNT)
001022           GO TO 1040-CONTINUE-EDIT
001023        WHEN (PI-COMPANY-ID = 'FNL')
001024           AND (VALID-FNL-GL-ACCOUNT)
001025           GO TO 1040-CONTINUE-EDIT
001026     END-EVALUATE
001027
001028*    EXEC CICS HANDLE CONDITION
001029*         NOTFND (1040-NO-COFA-MSTR)
001030*         NOTOPEN (7100-COFA-FILE-NOTOPEN)
001031*    END-EXEC.
001032*
001033*    EXEC CICS READ
001034*         DATASET (COFA-FILE-ID)
001035*         SET     (ADDRESS OF CHART-OF-ACCOUNTS)
001036*         RIDFLD  (COFA-KEY-X)
001037*    END-EXEC.
001038*
001039*    MOVE CHART-OF-ACCOUNTS      TO SV-COFA.
001040*    GO TO 1040-CONTINUE-EDIT.
001041*
001042*1040-NO-COFA-MSTR.
001043
001044     MOVE ER-2960                TO EMI-ERROR.
001045     MOVE -1                     TO GL-ACCT-LEN (PINDX).
001046     MOVE AL-UABON               TO GL-ACCT-ATTRB (PINDX).
001047     PERFORM 9900-ERROR-FORMAT THRU
001048             9900-EXIT.
001049
001050 1040-CONTINUE-EDIT.
001051
001052*    MOVE MSA-ACCT (PINDX)       TO WS-ACCT-BREAK.
001053     IF GL-STATE-LEN (PINDX) NOT = ZEROS
001054         MOVE GL-STATE (PINDX)   TO CHECK-STATE-CODE
001055         IF NOT VALID-STATE-CODE
001056             MOVE -1             TO GL-STATE-LEN (PINDX)
001057             MOVE ER-2957        TO EMI-ERROR
001058             MOVE AL-UABON       TO GL-STATE-ATTRB (PINDX)
001059             PERFORM 9900-ERROR-FORMAT THRU
001060                     9900-EXIT
001061         ELSE
001062             MOVE AL-UANON       TO GL-STATE-ATTRB (PINDX)
001063         END-IF
001064     END-IF.
001065
001066 1040-CSO-SKIP.
001067
001068     IF GL-CANC (PINDX) = LOW-VALUES
001069         MOVE SPACES             TO GL-CANC (PINDX)
001070     END-IF.
001071
001072     IF GL-CANC-LEN (PINDX) NOT = ZEROS
001073         MOVE GL-CANC (PINDX)    TO CHECK-CANC-TYPE
001074         IF NOT VALID-CANC-TYPE
001075             MOVE -1             TO GL-CANC-LEN (PINDX)
001076             MOVE ER-2958        TO EMI-ERROR
001077             MOVE AL-UABON       TO GL-CANC-ATTRB (PINDX)
001078             PERFORM 9900-ERROR-FORMAT THRU
001079                     9900-EXIT
001080         ELSE
001081             MOVE AL-UANON       TO GL-CANC-ATTRB (PINDX)
001082         END-IF
001083     END-IF.
001084
001085     IF RTYPE-LEN (PINDX) NOT = ZEROS
001086        MOVE RTYPE (PINDX)       TO  CHECK-REC-TYPE
001087        IF (PI-COMPANY-ID = 'DCC' or 'VPP')
001088           AND (RTYPE (PINDX) = 'P')
001089           SET VALID-REC-TYPE   TO TRUE
001090        END-IF
001091         IF NOT VALID-REC-TYPE
001092             MOVE -1             TO  RTYPE-LEN (PINDX)
001093             MOVE ER-2234        TO  EMI-ERROR
001094             MOVE AL-UABON       TO  RTYPE-ATTRB (PINDX)
001095             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001096         ELSE
001097             MOVE AL-UANON       TO  RTYPE-ATTRB (PINDX)
001098     ELSE
001099         IF PI-FILE-SEQ-NO (NDX) = ZEROS
001100             MOVE -1             TO  RTYPE-LEN (PINDX)
001101             MOVE ER-2235        TO  EMI-ERROR
001102             MOVE AL-UABON       TO  RTYPE-ATTRB (PINDX)
001103             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001104
001105     IF AMT-LEN (PINDX) NOT = ZEROS
001106        
      * EXEC CICS BIF DEEDIT
001107*           FIELD (AMT (PINDX))
001108*           LENGTH (11)
001109*       END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003808' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033383038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AMT(PINDX), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001110         IF AMT (PINDX) = ZEROS
001111             IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS
001112                 NEXT SENTENCE
001113             ELSE
001114                 MOVE ER-2245    TO  EMI-ERROR
001115                 MOVE -1         TO  AMT-LEN(PINDX)
001116                 MOVE AL-UNBON   TO  AMT-ATTRB (PINDX)
001117                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001118         ELSE
001119             IF AMT (PINDX) NUMERIC
001120                 MOVE AMT (PINDX) TO  AMTO (PINDX)
001121             ELSE
001122                 MOVE ER-2245    TO  EMI-ERROR
001123                 MOVE -1         TO  AMT-LEN(PINDX)
001124                 MOVE AL-UNBON   TO  AMT-ATTRB (PINDX)
001125                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001126     ELSE
001127         IF PI-FILE-SEQ-NO (NDX) = ZEROS
001128             MOVE -1             TO  AMT-LEN (PINDX)
001129             MOVE ER-2236        TO  EMI-ERROR
001130             MOVE AL-UNBON       TO  AMT-ATTRB (PINDX)
001131             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001132
001133     IF VOID-SW-LEN (PINDX) NOT = ZEROS
001134         IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS
001135           AND (PI-REC-TYPE (NDX) = 'C')
001136             IF VOID-SW (PINDX) = 'V'
001137                 MOVE AL-UANON   TO  VOID-SW-ATTRB (PINDX)
001138             ELSE
001139                 MOVE ER-2246    TO  EMI-ERROR
001140                 MOVE -1         TO  VOID-SW-LEN (PINDX)
001141                 MOVE AL-UABON   TO  VOID-SW-ATTRB (PINDX)
001142                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001143         ELSE
001144             MOVE ER-2449        TO  EMI-ERROR
001145             MOVE -1             TO  VOID-SW-LEN (PINDX)
001146             MOVE AL-UABON       TO  VOID-SW-ATTRB (PINDX)
001147             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001148
001149     IF SDTE-LEN (PINDX) = ZEROS
001150        GO TO 1020-INCREMENT-PINDX.
001151
001152     MOVE AL-UNNON               TO  SDTE-ATTRB (PINDX).
001153
001154     MOVE SDTE (PINDX)           TO DEEDIT-FIELD.
001155     PERFORM 8600-DEEDIT.
001156
001157     IF DEEDIT-FIELD-V0 NOT NUMERIC
001158        GO TO 1015-DAY-ERROR.
001159
001160     MOVE DEEDIT-FIELD-V0        TO  DC-GREG-DATE-1-MDY.
001161     MOVE '4'                    TO  DC-OPTION-CODE.
001162     PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
001163
001164     IF NO-CONVERSION-ERROR
001165        SET PINDEX               TO  PINDX
001166        MOVE DC-BIN-DATE-1       TO  WS-EOM-DT (PINDEX)
001167     ELSE
001168        GO TO 1015-DAY-ERROR.
001169
001170     MOVE DEEDIT-FIELD-V0        TO DATE-TEST-AREA.
001171
001172     IF DATE-TEST-MM = 4 OR  6  OR  9  OR  11
001173         IF DATE-TEST-DD  NOT = 30
001174             GO TO 1015-DAY-ERROR
001175         ELSE
001176             GO TO 1020-INCREMENT-PINDX.
001177
001178     IF DATE-TEST-MM = 1 OR  3  OR  5  OR  7  OR
001179                             8  OR  10  OR  12
001180         IF DATE-TEST-DD  NOT = 31
001181             GO TO 1015-DAY-ERROR
001182         ELSE
001183             GO TO 1020-INCREMENT-PINDX.
001184
001185     DIVIDE DATE-TEST-YY  BY  4  GIVING  DIVIDE-RESULT
001186         REMAINDER  DIVIDE-REMAINDER.
001187
001188     IF (DATE-TEST-YY = ZERO)
001189       OR (DIVIDE-REMAINDER NOT = ZERO)
001190         IF DATE-TEST-DD  NOT = 28
001191             GO TO 1015-DAY-ERROR
001192         ELSE
001193             GO TO 1020-INCREMENT-PINDX
001194     ELSE
001195         IF DATE-TEST-DD = 29
001196             GO TO 1020-INCREMENT-PINDX.
001197
001198 1015-DAY-ERROR.
001199     MOVE -1                     TO  SDTE-LEN (PINDX).
001200     MOVE AL-UNBON               TO  SDTE-ATTRB (PINDX).
001201     MOVE ER-0587                TO  EMI-ERROR.
001202
001203     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001204
001205 1020-INCREMENT-PINDX.
001206     SET PINDX  UP  BY  1.
001207
001208     IF PINDX  IS NOT GREATER THAN  13
001209         GO TO 1010-EDIT-LOOP.
001210
001211     IF EMI-ERROR = ZEROS
001212         NEXT SENTENCE
001213     ELSE
001214         MOVE 'S'                TO  PI-PREV-FUNCTION
001215         GO TO 8200-SEND-DATAONLY.
001216 EJECT
001217 2000-UPDATE-THE-FILE.
001218     SET PINDX                   TO  ZERO-NDX.
001219*    SET NDX                     TO  ZERO-NDX.
001220*    SET NDX  UP  BY  1.
001221
001222     IF PI-PYAJ-FILE-SW = 'B'
001223         MOVE WORK-SEQ-NO        TO  PI-FRST-FILE-SEQ-NO
001224         MOVE RTYPE (PINDX)      TO  PI-FRST-RECORD-TYPE
001225     ELSE
001226         MOVE PI-LAST-FILE-SEQ-NO
001227                                 TO  PI-SAV-FILE-SEQ-NO
001228         MOVE PI-LAST-RECORD-TYPE
001229                                 TO  PI-SAV-RECORD-TYPE.
001230
001231 2100-UPDATE-LOOP.
001232     SET PINDX  UP  BY  1.
001233     SET NDX                     TO  PINDX.
001234
001235     IF PINDX  IS GREATER THAN  13
001236         GO TO 2200-UPDATE-COMPLETE.
001237
001238     IF GL-ACCT-LEN (PINDX)   = ZEROS AND
001239        GL-STATE-LEN (PINDX) = ZEROS AND
001240        GL-CANC-LEN (PINDX)   = ZEROS AND
001241        GL-COMM-LEN (PINDX)   = ZEROS AND
001242        RTYPE-LEN (PINDX)   = ZEROS  AND
001243        AMT-LEN (PINDX)     = ZEROS  AND
001244        VOID-SW-LEN (PINDX) = ZEROS  AND
001245        SDTE-LEN (PINDX)    = ZEROS
001246         GO TO 2100-UPDATE-LOOP.
001247
001248     
      * EXEC CICS BIF DEEDIT
001249*         FIELD (AMT (PINDX))
001250*         LENGTH (11)
001251*    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003950' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033393530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AMT(PINDX), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001252
001253     IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS
001254         NEXT SENTENCE
001255     ELSE
001256         GO TO 2110-ADD-RECORD.
001257
001258     
      * EXEC CICS HANDLE CONDITION
001259*        NOTFND  (2110-ADD-RECORD)
001260*    END-EXEC.
      *    MOVE '"$I                   ! % #00003960' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303033393630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001261
001262     MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.
001263     MOVE PI-FILE-SEQ-NO (NDX)    TO  PYAJ-FILE-SEQ-NO.
001264     MOVE PI-REC-TYPE (NDX)       TO  PYAJ-RECORD-TYPE.
001265
001266     
      * EXEC CICS READ
001267*        SET      (ADDRESS OF PENDING-PAY-ADJ)
001268*        DATASET  (PYAJ-FILE-ID)
001269*        RIDFLD   (ERPYAJ-KEY)
001270*        UPDATE
001271*    END-EXEC.
      *    MOVE '&"S        EU         (   #00003968' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303033393638' TO DFHEIV0
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
           
001272
001273
001274
001275     IF RTYPE-LEN (PINDX)  GREATER THAN +0
001276        IF RTYPE (PINDX) NOT = PY-RECORD-TYPE
001277            MOVE PENDING-PAY-ADJ TO WS-SAVE-ERPYAJ
001278            PERFORM 2190-CHANGE-RECORD-TYPE THRU 2190-EXIT
001279            MOVE RTYPE (PINDX)     TO  PI-FRST-RECORD-TYPE.
001280
001281     IF AMT-LEN (PINDX) NOT = ZEROS
001282         IF AMT (PINDX) = ZEROS
001283             IF PY-CHECK-WRITTEN-DT = LOW-VALUES
001284                 GO TO 2120-DELETE-RECORD
001285             ELSE
001286                 MOVE ER-2244       TO  EMI-ERROR
001287                 MOVE -1            TO  AMT-LEN (PINDX)
001288                 MOVE AL-UNBON      TO  AMT-ATTRB (PINDX)
001289                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001290                 MOVE PY-ENTRY-AMT  TO  AMTO (PINDX)
001291                 
      * EXEC CICS UNLOCK
001292*                    DATASET  (PYAJ-FILE-ID)
001293*                END-EXEC
      *    MOVE '&*                    #   #00003993' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033393933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001294                 GO TO 8200-SEND-DATAONLY.
001295
001296     MOVE 'B'                    TO  JP-RECORD-TYPE.
001297     MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
001298     MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
001299
001300     PERFORM 8400-LOG-JOURNAL-RECORD.
001301
001302     MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.
001303     MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.
001304     MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT.
001305
001306     IF GL-ACCT-LEN (PINDX) NOT = ZEROS
001307         MOVE GL-ACCT (PINDX)    TO  PY-GL-ACCOUNT.
001308     IF GL-STATE-LEN (PINDX) NOT = ZEROS
001309         MOVE GL-STATE (PINDX)   TO  PY-GL-STATE.
001310     IF GL-CANC-LEN (PINDX) NOT = ZEROS
001311         MOVE GL-CANC (PINDX)    TO  PY-GL-CANC-SW.
001312     IF GL-COMM-LEN (PINDX) NOT = ZEROS
001313         MOVE GL-COMM (PINDX)    TO  PY-GL-COMMENT.
001314     IF AMT-LEN (PINDX) NOT = ZEROS
001315         MOVE AMT (PINDX)        TO  PY-ENTRY-AMT.
001316
001317     IF VOID-SW-LEN (PINDX) NOT = ZEROS
001318         MOVE VOID-SW (PINDX)    TO  PY-VOID-SW.
001319
001320     IF SDTE-LEN (PINDX) NOT = ZEROS
001321        SET PINDEX               TO  PINDX
001322        MOVE WS-EOM-DT (PINDEX)  TO  PY-CREDIT-SELECT-DT.
001323
001324     MOVE 'C'                    TO  JP-RECORD-TYPE.
001325     MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
001326     MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
001327
001328     
      * EXEC CICS REWRITE
001329*        DATASET  (PYAJ-FILE-ID)
001330*        FROM     (PENDING-PAY-ADJ)
001331*    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004030' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034303330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001332
001333     PERFORM 8400-LOG-JOURNAL-RECORD.
001334
001335     GO TO 2100-UPDATE-LOOP.
001336 EJECT
001337 2110-ADD-RECORD.
001338     
      * EXEC CICS GETMAIN
001339*        SET      (ADDRESS OF PENDING-PAY-ADJ)
001340*        LENGTH   (ERPYAJ-RECORD-LENGTH)
001341*        INITIMG  (GETMAIN-SPACE)
001342*    END-EXEC.
      *    MOVE ',"IL                  $   #00004040' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303034303430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPYAJ-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001343
001344     MOVE 'PY'                   TO  PY-RECORD-ID.
001345     MOVE PI-COMPANY-CD          TO  PY-COMPANY-CD.
001346     MOVE PI-SAV-CARRIER         TO  PY-CARRIER.
001347     MOVE PI-SAV-GROUPING        TO  PY-GROUPING.
001348     MOVE PI-SAV-FIN-RESP        TO  PY-FIN-RESP.
001349     MOVE PI-SAV-ACCOUNT         TO  PY-ACCOUNT.
001350
001351     ADD +1                      TO  WORK-SEQ-NO.
001352     MOVE WORK-SEQ-NO            TO  PY-FILE-SEQ-NO.
001353
001354     MOVE GL-ACCT (PINDX)        TO  PY-GL-ACCOUNT.
001355     if gl-state-len (pindx) <> zeros
001356        MOVE GL-STATE (PINDX)    TO  PY-GL-STATE
001357     end-if
001358     if gl-canc-len (pindx) <> zeros
001359        MOVE GL-CANC (PINDX)     TO  PY-GL-CANC-SW
001360     end-if
001361     MOVE GL-COMM (PINDX)        TO  PY-GL-COMMENT.
001362
001363     MOVE RTYPE (PINDX)          TO  PY-RECORD-TYPE.
001364     MOVE AMT (PINDX)            TO  PY-ENTRY-AMT.
001365
001366     IF CO-TYPE = 'B' OR 'A' OR 'G'
001367        MOVE CO-TYPE             TO  PY-ERCOMP-TYPE
001368     END-IF
001369
001370     MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.
001371     MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.
001372     MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT
001373                                     PY-INPUT-DT.
001374     MOVE ZEROS                  TO  PY-CHECK-QUE-CONTROL
001375                                     PY-CHECK-QUE-SEQUENCE.
001376     MOVE LOW-VALUES             TO  PY-CREDIT-ACCEPT-DT
001377                                     PY-BILLED-DATE
001378                                     PY-AR-DATE
001379                                     PY-REPORTED-DT
001380                                     PY-CHECK-WRITTEN-DT.
001381     IF SDTE-LEN (PINDX) = ZEROS
001382         MOVE PI-CR-MONTH-END-DT TO  PY-CREDIT-SELECT-DT
001383         GO TO 2115-WRITE-REC
001384     END-IF.
001385
001386     IF SDTE (PINDX) IS NUMERIC
001387         MOVE SDTE (PINDX)   TO  DC-GREG-DATE-1-MDY
001388         MOVE '4'            TO  DC-OPTION-CODE
001389         PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
001390         IF DATE-CONVERSION-ERROR
001391             MOVE PI-CR-MONTH-END-DT
001392                             TO  PY-CREDIT-SELECT-DT
001393         ELSE
001394             MOVE DC-BIN-DATE-1
001395                             TO  PY-CREDIT-SELECT-DT
001396         END-IF
001397     ELSE
001398         MOVE PI-CR-MONTH-END-DT
001399                             TO  PY-CREDIT-SELECT-DT
001400     END-IF
001401
001402     IF PI-SAV-FILE-SEQ-NO NOT GREATER THAN ZEROS
001403         MOVE RTYPE (PINDX)      TO  PI-SAV-RECORD-TYPE
001404         MOVE WORK-SEQ-NO        TO  PI-SAV-FILE-SEQ-NO.
001405
001406     IF FIRST-ADD AND END-OF-ACCT-FULL-PAGE
001407         MOVE RTYPE (PINDX)      TO  PI-SAV-RECORD-TYPE
001408         MOVE WORK-SEQ-NO        TO  PI-SAV-FILE-SEQ-NO.
001409
001410     MOVE 'N'                    TO  FIRST-ADD-SW.
001411
001412     MOVE 'A'                    TO  JP-RECORD-TYPE.
001413     MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
001414     MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
001415 2115-WRITE-REC.
001416     
      * EXEC CICS WRITE
001417*        DATASET  (PYAJ-FILE-ID)
001418*        FROM     (PENDING-PAY-ADJ)
001419*        RIDFLD   (PY-CONTROL-PRIMARY)
001420*    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004118' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034313138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001421
001422     PERFORM 8400-LOG-JOURNAL-RECORD.
001423
001424     GO TO 2100-UPDATE-LOOP.
001425
001426 EJECT
001427 2120-DELETE-RECORD.
001428     IF PY-RECORD-TYPE NOT = 'C'
001429         GO TO 2120-DELETE-CONT.
001430
001431     IF PY-CHECK-QUE-CONTROL = ZEROS
001432       AND PY-CHECK-QUE-SEQUENCE = ZEROS
001433         GO TO 2120-DELETE-CONT.
001434
001435     
      * EXEC CICS HANDLE CONDITION
001436*        NOTFND  (2120-DELETE-CONT)
001437*    END-EXEC.
      *    MOVE '"$I                   ! & #00004137' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303034313337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001438
001439     MOVE PY-COMPANY-CD          TO  CHKQ-COMPANY-CD.
001440     MOVE PY-CHECK-QUE-CONTROL   TO  CHKQ-CONTROL-NUMBER.
001441     MOVE PY-CHECK-QUE-SEQUENCE  TO  CHKQ-SEQUENCE-NUMBER.
001442
001443     
      * EXEC CICS READ
001444*        SET      (ADDRESS OF CHECK-QUE)
001445*        DATASET  (CHKQ-FILE-ID)
001446*        RIDFLD   (ERCHKQ-KEY)
001447*        UPDATE
001448*    END-EXEC.
      *    MOVE '&"S        EU         (   #00004145' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303034313435' TO DFHEIV0
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
           
001449
001450*    MOVE 'D'                    TO  JP-RECORD-TYPE.
001451*    MOVE CHKQ-FILE-ID           TO  JP-FILE-ID.
001452*    MOVE CHECK-QUE              TO  JP-RECORD-AREA.
001453*
001454*    PERFORM 8400-LOG-JOURNAL-RECORD.
001455
001456     
      * EXEC CICS DELETE
001457*        DATASET  (CHKQ-FILE-ID)
001458*    END-EXEC.
      *    MOVE '&(                    &   #00004158' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303034313538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CHKQ-FILE-ID, 
                 ERCHKQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001459
001460 2120-DELETE-CONT.
001461*    MOVE 'D'                    TO  JP-RECORD-TYPE.
001462*    MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
001463*    MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
001464*
001465*    PERFORM 8400-LOG-JOURNAL-RECORD.
001466
001467     IF PI-SAV-ENDING-PYAJ-KEY = ERPYAJ-KEY
001468         MOVE ZEROS              TO  PI-FRST-FILE-SEQ-NO
001469         MOVE SPACE              TO  PI-FRST-RECORD-TYPE
001470         MOVE ZEROS              TO  PI-SAV-ACCT-AMT
001471                                     PI-SAV-ACCT-NET
001472                                     PI-SAV-PREV-AMT
001473                                     PI-SAV-PREV-NET.
001474
001475     
      * EXEC CICS DELETE
001476*        DATASET(PYAJ-FILE-ID)
001477*    END-EXEC.
      *    MOVE '&(                    &   #00004177' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303034313737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 ERCHKQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001478
001479     GO TO 2100-UPDATE-LOOP.
001480
001481 EJECT
001482
001483 2190-CHANGE-RECORD-TYPE.
001484
001485*    MOVE 'D'                   TO  JP-RECORD-TYPE.
001486*    MOVE PYAJ-FILE-ID          TO  JP-FILE-ID.
001487*    MOVE PENDING-PAY-ADJ       TO  JP-RECORD-AREA.
001488*
001489*    PERFORM 8400-LOG-JOURNAL-RECORD.
001490
001491     
      * EXEC CICS DELETE
001492*        DATASET(PYAJ-FILE-ID)
001493*    END-EXEC.
      *    MOVE '&(                    &   #00004193' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303034313933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 ERCHKQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001494
001495     
      * EXEC CICS GETMAIN
001496*        SET      (ADDRESS OF PENDING-PAY-ADJ)
001497*        LENGTH   (ERPYAJ-RECORD-LENGTH)
001498*        INITIMG  (GETMAIN-SPACE)
001499*    END-EXEC.
      *    MOVE ',"IL                  $   #00004197' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303034313937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPYAJ-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001500
001501*    MOVE JP-RECORD-AREA        TO  PENDING-PAY-ADJ.
001502*    MOVE ERPYAJ-KEY            TO  PY-CONTROL-PRIMARY.
001503
001504     MOVE WS-SAVE-ERPYAJ        TO  PENDING-PAY-ADJ
001505     MOVE RTYPE (PINDX)         TO  PY-RECORD-TYPE
001506                                    PYAJ-RECORD-TYPE.
001507
001508     
      * EXEC CICS HANDLE CONDITION
001509*        DUPREC (2190-DUP-RECORD)
001510*    END-EXEC.
      *    MOVE '"$%                   ! '' #00004210' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303034323130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001511
001512*    MOVE 'A'                   TO  JP-RECORD-TYPE.
001513*    MOVE PYAJ-FILE-ID          TO  JP-FILE-ID.
001514*    MOVE PENDING-PAY-ADJ       TO  JP-RECORD-AREA.
001515
001516 2190-RETRY-WRITE.
001517
001518     
      * EXEC CICS WRITE
001519*        DATASET  (PYAJ-FILE-ID)
001520*        FROM     (PENDING-PAY-ADJ)
001521*        RIDFLD   (PY-CONTROL-PRIMARY)
001522*    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004220' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034323230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001523
001524*    PERFORM 8400-LOG-JOURNAL-RECORD.
001525
001526     
      * EXEC CICS READ
001527*        SET      (ADDRESS OF PENDING-PAY-ADJ)
001528*        DATASET  (PYAJ-FILE-ID)
001529*        RIDFLD   (ERPYAJ-KEY)
001530*        UPDATE
001531*    END-EXEC.
      *    MOVE '&"S        EU         (   #00004228' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303034323238' TO DFHEIV0
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
           
001532
001533     GO TO 2190-EXIT.
001534
001535 2190-DUP-RECORD.
001536
001537     COMPUTE PY-FILE-SEQ-NO = PY-FILE-SEQ-NO + 1.
001538
001539     GO TO 2190-RETRY-WRITE.
001540
001541 2190-EXIT.
001542     EXIT.
001543 EJECT
001544
001545 2200-UPDATE-COMPLETE.
001546     IF EIBAID = DFHPF1
001547         GO TO 4000-BROWSE-FRWD.
001548
001549     IF EIBAID = DFHPF2
001550         GO TO 4100-BROWSE-BKWD.
001551
001552     MOVE LOW-VALUES             TO  EL633AI.
001553
001554     MOVE ER-0000                TO  EMI-ERROR.
001555     MOVE -1                     TO  MAINTL.
001556
001557     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001558
001559     MOVE PI-FRST-FILE-SEQ-NO    TO  PI-SAV-FILE-SEQ-NO.
001560     MOVE PI-FRST-RECORD-TYPE    TO  PI-SAV-RECORD-TYPE.
001561
001562*    GO TO 4000-BROWSE-FRWD.
001563
001564*    GO TO 8100-SEND-INITIAL-MAP.
001565
001566 EJECT
001567 4000-BROWSE-FRWD.
001568     MOVE '1'                     TO  PI-PREV-PFKEY.
001569     MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.
001570
001571     IF EIBAID = DFHPF1
001572         IF PAGE-FULL
001573             MOVE PI-FILE-SEQ-NO (PI-FULL-INDX)
001574                                      TO  PYAJ-FILE-SEQ-NO
001575             MOVE HIGH-VALUES         TO  PYAJ-RECORD-TYPE
001576             MOVE 'N'                 TO  PYAJ-READ-SW
001577             MOVE PI-SAV-ACCT-AMT     TO  TOTAL-ACCT-AMT
001578             MOVE PI-SAV-ACCT-NET     TO  TOTAL-ACCT-NET
001579             MOVE PI-SAV-ACCT-AMT     TO  PI-SAV-PREV-AMT
001580             MOVE PI-SAV-ACCT-NET     TO  PI-SAV-PREV-NET
001581         ELSE
001582             MOVE ZEROS               TO  PI-SAV-ACCT-AMT
001583                                          PI-SAV-ACCT-NET
001584                                          PI-SAV-PREV-AMT
001585                                          PI-SAV-PREV-NET
001586             MOVE 'Y'                 TO  PI-PAGE-SW
001587             IF NOT END-OF-FILE
001588                 MOVE 99999999        TO  PYAJ-FILE-SEQ-NO
001589                 MOVE HIGH-VALUES     TO  PYAJ-RECORD-TYPE
001590             END-IF
001591             IF TOP-OF-FILE
001592                 MOVE SPACES          TO  PI-PYAJ-FILE-SW
001593                 MOVE ZEROS           TO  PYAJ-FILE-SEQ-NO
001594                 MOVE SPACES          TO  PYAJ-RECORD-TYPE
001595             ELSE
001596                 MOVE -1              TO  PYAJ-FILE-SEQ-NO
001597                 MOVE HIGH-VALUES     TO  PYAJ-RECORD-TYPE
001598     ELSE
001599         MOVE PI-SAV-PREV-AMT         TO  TOTAL-ACCT-AMT
001600         MOVE PI-SAV-PREV-NET         TO  TOTAL-ACCT-NET
001601         IF MAINTI = 'S'
001602             MOVE ZEROS               TO  PYAJ-FILE-SEQ-NO
001603             MOVE SPACES              TO  PYAJ-RECORD-TYPE.
001604
001605
001606     MOVE SPACE                  TO  PI-PYAJ-FILE-SW.
001607
001608     IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
001609         MOVE PI-CARRIER-SECURITY  TO  PYAJ-CARRIER.
001610
001611 4000-BROWSE-FRWD-FOR-PREV.
001612*    IF END-OF-FILE
001613*        IF EIBAID = DFHPF1
001614*           IF PI-TOTAL-DISPLAYED
001615*              NEXT SENTENCE
001616*           ELSE
001617*              MOVE -1                 TO  MAINTL
001618*              MOVE ER-2237            TO  EMI-ERROR
001619*              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001620*              MOVE SPACE              TO  PI-PYAJ-FILE-SW
001621*              SET PINDX               TO  +2
001622*              MOVE 'TOTAL'            TO  COMM (PINDX)
001623*              MOVE PI-SAV-ACCT-AMT    TO  AMTO (PINDX)
001624*              SET PINDEX UP BY 1
001625*              MOVE 'NET TOTAL'        TO  COMM (PINDX)
001626*              MOVE PI-SAV-ACCT-NET    TO  AMTO (PINDX)
001627*              MOVE ZEROS              TO  PI-SAV-ACCT-AMT
001628*                                          PI-SAV-ACCT-NET
001629*              GO TO 8100-SEND-INITIAL-MAP.
001630*
001631*     IF END-OF-FILE
001632*        IF EIBAID = DFHPF1
001633*           MOVE -1             TO  MAINTL
001634*           MOVE ER-2237        TO  EMI-ERROR
001635*           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001636*           GO TO 8200-SEND-DATAONLY.
001637*
001638     PERFORM 5000-START-BROWSE  THRU  5030-EXIT.
001639
001640     IF NO-RECORDS
001641        MOVE SPACE              TO  PI-PYAJ-FILE-SW
001642        MOVE LOW-VALUES         TO  EL633AO
001643        MOVE ER-2239            TO  EMI-ERROR
001644        MOVE -1                 TO  MAINTL
001645        PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001646        GO TO 8100-SEND-INITIAL-MAP.
001647
001648     IF NOT-OPEN
001649         GO TO 7000-PYAJ-FILE-NOTOPEN.
001650
001651     MOVE LOW-VALUES             TO  EL633AO.
001652     MOVE ZEROS                  TO  PI-SEQ-NOS.
001653
001654     PERFORM 6000-READ-AND-FORMAT-SCREEN  THRU  6200-EXIT
001655          VARYING  PINDX  FROM  1  BY  1
001656             UNTIL  END-OF-ACCT
001657                 OR  END-OF-FILE
001658                     OR  PAGE-FULL
001659                         OR  NO-RECORDS.
001660
001661     IF NO-RECORDS
001662         MOVE SPACE              TO  PI-PYAJ-FILE-SW
001663         MOVE LOW-VALUES         TO  EL633AO
001664         MOVE ER-2239            TO  EMI-ERROR
001665         MOVE -1                 TO  MAINTL
001666         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001667         GO TO 8100-SEND-INITIAL-MAP.
001668
001669     IF END-OF-FILE
001670         IF EIBAID = DFHPF2
001671             MOVE ER-2238        TO  EMI-ERROR
001672             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001673             MOVE 'T'            TO  PI-PYAJ-FILE-SW
001674         ELSE
001675             MOVE ER-2237        TO  EMI-ERROR
001676             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001677
001678     MOVE 'S'                    TO  PI-PREV-FUNCTION
001679                                     PI-SAV-FUNCTION.
001680
001681     GO TO 8100-SEND-INITIAL-MAP.
001682 EJECT
001683 4100-BROWSE-BKWD.
001684     MOVE SPACE                   TO  PI-PYAJ-FILE-SW
001685                                      PI-TOTAL-DISPLAYED-SW.
001686     MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.
001687     MOVE ZEROS                   TO  PYAJ-FILE-SEQ-NO.
001688     MOVE ZEROS                   TO  PYAJ-RECORD-TYPE.
001689     MOVE ZEROS                   TO  PI-SAV-ACCT-AMT
001690                                      PI-SAV-ACCT-NET
001691                                      PI-SAV-PREV-AMT
001692                                      PI-SAV-PREV-NET.
001693
001694     PERFORM 5000-START-BROWSE  THRU  5030-EXIT.
001695
001696     IF NO-RECORDS
001697         MOVE SPACES             TO  PI-PYAJ-FILE-SW
001698         MOVE LOW-VALUES         TO  EL633AO
001699         MOVE ER-2239            TO  EMI-ERROR
001700         MOVE -1                 TO  MAINTL
001701         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001702         GO TO 8100-SEND-INITIAL-MAP.
001703
001704     IF NOT-OPEN
001705         GO TO 7000-PYAJ-FILE-NOTOPEN.
001706
001707     
      * EXEC CICS READNEXT
001708*        SET      (ADDRESS OF PENDING-PAY-ADJ)
001709*        DATASET  (PYAJ-FILE-ID)
001710*        RIDFLD   (ERPYAJ-KEY)
001711*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004409' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303034343039' TO DFHEIV0
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
           
001712
001713     IF FIRST-PAGE OR
001714        PI-PREV-PFKEY = '2'
001715         PERFORM 5100-READ-PREVIOUS  THRU  5120-EXIT
001716         PERFORM 5100-READ-PREVIOUS  THRU  5120-EXIT.
001717
001718     PERFORM 5200-END-BROWSE  THRU  5200-EXIT.
001719
001720     MOVE ZEROS                  TO  PYAJ-FILE-SEQ-NO.
001721     MOVE SPACES                 TO  PYAJ-RECORD-TYPE.
001722     MOVE '2'                     TO  PI-PREV-PFKEY.
001723
001724     GO TO 4000-BROWSE-FRWD-FOR-PREV.
001725 EJECT
001726 5000-START-BROWSE.
001727     
      * EXEC CICS HANDLE CONDITION
001728*        NOTOPEN  (5010-NOT-OPEN)
001729*        NOTFND   (5020-NO-RECORDS)
001730*        ENDFILE  (5020-NO-RECORDS)
001731*    END-EXEC.
      *    MOVE '"$JI''                 ! ( #00004429' TO DFHEIV0
           MOVE X'22244A492720202020202020' &
                X'202020202020202020202120' &
                X'2820233030303034343239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001732
001733     
      * EXEC CICS STARTBR
001734*        DATASET  (PYAJ-FILE-ID)
001735*        RIDFLD   (ERPYAJ-KEY)
001736*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004435' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303034343335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001737
001738     GO TO 5030-EXIT.
001739
001740 5010-NOT-OPEN.
001741     MOVE 'Z'                    TO  PI-PYAJ-FILE-SW.
001742
001743     GO TO 5030-EXIT.
001744
001745 5020-NO-RECORDS.
001746     MOVE ZEROS                  TO  PI-SEQ-NOS.
001747     MOVE 'Y'                    TO  PI-PYAJ-FILE-SW.
001748
001749 5030-EXIT.
001750     EXIT.
001751 EJECT
001752 5100-READ-PREVIOUS.
001753     
      * EXEC CICS HANDLE CONDITION
001754*        ENDFILE  (5110-END-OF-FILE)
001755*        NOTFND   (5110-END-OF-FILE)
001756*    END-EXEC.
      *    MOVE '"$''I                  ! ) #00004455' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303034343535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001757
001758     
      * EXEC CICS READPREV
001759*        SET      (ADDRESS OF PENDING-PAY-ADJ)
001760*        DATASET  (PYAJ-FILE-ID)
001761*        RIDFLD   (ERPYAJ-KEY)
001762*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004460' TO DFHEIV0
           MOVE X'263053202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303034343630' TO DFHEIV0
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
           
001763
001764     IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
001765         IF PI-CARRIER-SECURITY = PY-CARRIER
001766             NEXT SENTENCE
001767         ELSE
001768             GO TO 5110-END-OF-FILE.
001769
001770     GO TO 5120-EXIT.
001771
001772 5110-END-OF-FILE.
001773     MOVE 'X'                    TO  PI-PYAJ-FILE-SW.
001774     MOVE ER-2238                TO  EMI-ERROR.
001775     MOVE -1                     TO  PFENTERL.
001776
001777     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001778
001779     GO TO 8200-SEND-DATAONLY.
001780
001781 5120-EXIT.
001782     EXIT.
001783
001784 5200-END-BROWSE.
001785     
      * EXEC CICS ENDBR
001786*        DATASET  (PYAJ-FILE-ID)
001787*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004487' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303034343837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001788
001789 5200-EXIT.
001790     EXIT.
001791 EJECT
001792 6000-READ-AND-FORMAT-SCREEN.
001793     
      * EXEC CICS HANDLE CONDITION
001794*        ENDFILE  (6100-END-OF-FILE)
001795*        NOTFND   (6100-END-OF-FILE)
001796*    END-EXEC.
      *    MOVE '"$''I                  ! * #00004495' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303034343935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001797
001798*    MOVE SPACES                 TO  PI-TOTAL-DISPLAYED-SW.
001799*    INITIALIZE  PI-SEQ-NOS.
001800
001801 6010-READ-NEXT.
001802
001803     
      * EXEC CICS READNEXT
001804*        SET      (ADDRESS OF PENDING-PAY-ADJ)
001805*        DATASET  (PYAJ-FILE-ID)
001806*        RIDFLD   (ERPYAJ-KEY)
001807*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004505' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303034353035' TO DFHEIV0
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
           
001808
001809     IF PI-COMPANY-CD NOT = PY-COMPANY-CD
001810       AND PI-SAV-ENDING-PYAJ-KEY NOT = SPACES
001811         GO TO 6100-END-OF-FILE.
001812
001813     IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
001814         IF PY-CARRIER NOT = PI-CARRIER-SECURITY
001815             IF PI-SAV-ENDING-PYAJ-KEY NOT = SPACES
001816                 GO TO 6100-END-OF-FILE.
001817
001818     IF PYAJ-1ST-READ
001819       AND EIBAID NOT = DFHENTER
001820       AND NOT PAGE-FULL
001821         MOVE PY-CONTROL-PRIMARY TO  PI-SAV-ENDING-PYAJ-KEY
001822         MOVE PY-CARRIER         TO  PI-CR-CARRIER
001823         MOVE PY-GROUPING        TO  PI-CR-GROUPING
001824         MOVE PY-FIN-RESP        TO  PI-CR-FIN-RESP
001825         MOVE PY-ACCOUNT         TO  PI-CR-ACCOUNT.
001826
001827     IF PYAJ-1ST-READ
001828         MOVE PI-SAV-ENDING-PYAJ-KEY
001829                                 TO  PI-START-PYAJ-KEY.
001830
001831     IF PI-COMPANY-CD   = PY-COMPANY-CD  AND
001832        PI-SAV-CARRIER  = PY-CARRIER     AND
001833        PI-SAV-GROUPING = PY-GROUPING    AND
001834        PI-SAV-FIN-RESP = PY-FIN-RESP    AND
001835        PI-SAV-ACCOUNT  = PY-ACCOUNT
001836         NEXT SENTENCE
001837     ELSE
001838         IF PYAJ-1ST-READ
001839             MOVE 'Y'            TO  PI-PYAJ-FILE-SW
001840             MOVE SPACE          TO  PYAJ-READ-SW
001841             GO TO 6200-EXIT
001842         ELSE
001843             MOVE 'A'            TO  PI-PYAJ-FILE-SW
001844             IF PINDX  IS LESS THAN  12
001845                 SET PINDX  UP  BY  1
001846                 MOVE 'TOTAL'         TO  GL-ACCT (PINDX)
001847                 MOVE TOTAL-ACCT-AMT  TO  AMTO (PINDX)
001848                 SET PINDX  UP  BY  1
001849                 MOVE 'NET TOTAL'     TO  GL-ACCT (PINDX)
001850                 MOVE TOTAL-ACCT-NET  TO  AMTO (PINDX)
001851                     GO TO 6200-EXIT
001852             ELSE
001853                 MOVE 'TOTAL'         TO  GL-ACCT (PINDX)
001854                 MOVE TOTAL-ACCT-AMT  TO  AMTO (PINDX)
001855                 SET PINDX  UP  BY  1
001856                 MOVE 'NET TOTAL'     TO  GL-ACCT (PINDX)
001857                 MOVE TOTAL-ACCT-NET  TO  AMTO (PINDX)
001858                 GO TO 6200-EXIT.
001859
001860*    MOVE ' '                    TO  PI-PYAJ-FILE-SW.
001861
001862     IF PY-CREDIT-ACCEPT-DT NOT = LOW-VALUES
001863         GO TO 6000-READ-AND-FORMAT-SCREEN.
001864
001865*    SET PINDX  UP  BY +1.
001866
001867     SET WS-SAVE-INDEX-VALUE  TO  PINDX.
001868
001869     IF PINDX  IS GREATER THAN  13
001870         MOVE 'F'                TO  PI-PYAJ-FILE-SW
001871         MOVE +13                TO  PI-FULL-INDX
001872         MOVE PI-FRST-FILE-SEQ-NO
001873                                 TO  PI-LAST-FILE-SEQ-NO
001874         MOVE PI-FRST-RECORD-TYPE
001875                                 TO  PI-LAST-RECORD-TYPE
001876         MOVE TOTAL-ACCT-AMT     TO  PI-SAV-ACCT-AMT
001877         MOVE TOTAL-ACCT-NET     TO  PI-SAV-ACCT-NET
001878         GO TO 6200-EXIT.
001879
001880     MOVE PY-FILE-SEQ-NO         TO  PI-PREV-FILE-SEQ-NO.
001881     MOVE PY-RECORD-TYPE         TO  PI-PREV-RECORD-TYPE.
001882
001883     IF PYAJ-1ST-READ
001884         MOVE SPACE              TO  PYAJ-READ-SW.
001885
001886     IF PINDX = 1
001887         MOVE PI-PREV-FILE-SEQ-NO
001888                                 TO  PI-FRST-FILE-SEQ-NO
001889         MOVE PI-PREV-RECORD-TYPE
001890                                 TO  PI-FRST-RECORD-TYPE
001891         MOVE PI-PREV-FILE-SEQ-NO
001892                                 TO  PI-LAST-FILE-SEQ-NO
001893         MOVE PI-PREV-RECORD-TYPE
001894                                 TO  PI-LAST-RECORD-TYPE.
001895
001896     SET NDX                     TO  PINDX.
001897     SET WS-SAVE-NDX-VALUE       TO  NDX.
001898
001899     MOVE PY-FILE-SEQ-NO         TO  PI-FILE-SEQ-NO (NDX).
001900
001901     MOVE PY-GL-ACCOUNT          TO  GL-ACCT (PINDX).
001902     MOVE PY-GL-STATE            TO  GL-STATE (PINDX).
001903     MOVE PY-GL-CANC-SW          TO  GL-CANC (PINDX).
001904     MOVE PY-GL-COMMENT          TO  GL-COMM (PINDX).
001905     MOVE PY-ENTRY-AMT           TO  AMTO (PINDX).
001906
001907     ADD PY-ENTRY-AMT            TO  TOTAL-ACCT-AMT.
001908
001909     IF PY-RECORD-TYPE = 'R' OR 'D' OR 'S' OR 'Z'
001910         ADD PY-ENTRY-AMT        TO  TOTAL-ACCT-NET
001911     ELSE
001912         SUBTRACT PY-ENTRY-AMT  FROM  TOTAL-ACCT-NET.
001913
001914     MOVE PY-RECORD-TYPE         TO  RTYPE (PINDX)
001915                                     PI-REC-TYPE (NDX).
001916
001917     IF PY-VOID-SW NOT = SPACE
001918         MOVE PY-VOID-SW         TO  VOID-SW (PINDX).
001919
001920     IF PY-LAST-MAINT-DT = PREV-BIN-MAINT-DT
001921         MOVE PREV-MAINT-DT      TO  MDTE (PINDX)
001922     ELSE
001923         MOVE PY-LAST-MAINT-DT   TO  DC-BIN-DATE-1
001924                                     PREV-BIN-MAINT-DT
001925         MOVE SPACE              TO  DC-OPTION-CODE
001926         PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
001927* DAN    MOVE DC-GREG-DATE-1-EDIT
001928         MOVE DC-GREG-DATE-1-MDY
001929                                 TO  MDTE (PINDX)
001930                                     PREV-MAINT-DT.
001931
001932*    IF PY-GL-COMMENT NOT = (LOW-VALUES OR SPACES OR ZEROS)
001933     IF PY-GL-COMMENT NOT = (LOW-VALUES AND SPACES AND ZEROS)
001934         INSPECT PY-GL-COMMENT CONVERTING LOW-VALUES TO SPACES
001935         MOVE PY-GL-COMMENT      TO  GL-COMM (PINDX)
001936*        MOVE AL-UANON           TO  GL-COMM-ATTRB (PINDX)
001937*        MOVE AL-UANON           TO  GL-COMM-ATTRB (PINDX)
001938     END-IF.
001939
001940     IF PY-BILLED-DATE NOT = LOW-VALUES
001941         MOVE AL-SANOF           TO  GL-ACCT-ATTRB (PINDX)
001942                                     AMT-ATTRB (PINDX)
001943                                     VOID-SW-ATTRB (PINDX)
001944         IF PY-BILLED-DATE = PREV-BIN-BL-DT
001945             MOVE PREV-BL-DT     TO  BDTE (PINDX)
001946         ELSE
001947             MOVE PY-BILLED-DATE TO  DC-BIN-DATE-1
001948                                     PREV-BIN-BL-DT
001949             MOVE SPACE          TO  DC-OPTION-CODE
001950             PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
001951* DAN        MOVE DC-GREG-DATE-1-EDIT
001952             MOVE DC-GREG-DATE-1-MDY
001953                                 TO  BDTE (PINDX)
001954                                     PREV-BL-DT.
001955
001956     IF (PY-CHECK-ORIGIN-SW = LOW-VALUES OR SPACES OR 'G')
001957         NEXT SENTENCE
001958     ELSE
001959         MOVE AL-SANOF           TO  RTYPE-ATTRB   (PINDX)
001960                                     AMT-ATTRB     (PINDX).
001961
001962     IF PY-CREDIT-SELECT-DT NOT = LOW-VALUES
001963         MOVE PY-CREDIT-SELECT-DT
001964                                 TO  DC-BIN-DATE-1
001965         MOVE SPACE              TO  DC-OPTION-CODE
001966         PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
001967
001968         MOVE DC-GREG-DATE-1-EDIT  TO  WS-GREG-STORE
001969         MOVE WS-GS-MM             TO  WS-SS-MM
001970         MOVE WS-GS-DD             TO  WS-SS-DD
001971         MOVE WS-GS-YY             TO  WS-SS-YY
001972         MOVE WS-SDTE-STORE        TO  SDTE (PINDX).
001973     IF (PY-INPUT-DT NOT = WS-CURRENT-BIN-DT)
001974        AND (PI-PROCESSOR-ID NOT = 'PEMA')
001975         MOVE AL-SANOF           TO RTYPE-ATTRB    (PINDX)
001976                                    AMT-ATTRB      (PINDX)
001977                                    GL-ACCT-ATTRB  (PINDX)
001978                                    GL-COMM-ATTRB  (PINDX)
001979                                    VOID-SW-ATTRB  (PINDX)
001980                                    GL-STATE-ATTRB (PINDX)
001981                                    GL-CANC-ATTRB  (PINDX)
001982                                    SDTE-ATTRB     (PINDX)
001983
001984     END-IF
001985
001986*    GO TO 6010-READ-NEXT.
001987     GO TO 6200-EXIT.
001988
001989 6100-END-OF-FILE.
001990     MOVE 'X'                    TO  PI-PYAJ-FILE-SW.
001991
001992     IF PINDX  IS LESS THAN  12
001993         SET PINDX  UP  BY  1
001994         MOVE 'TOTAL'            TO  GL-ACCT (PINDX)
001995         MOVE TOTAL-ACCT-AMT     TO  AMTO (PINDX)
001996         SET PINDX  UP  BY  1
001997         MOVE 'NET TOTAL'        TO  GL-ACCT (PINDX)
001998         MOVE TOTAL-ACCT-NET     TO  AMTO (PINDX)
001999         MOVE 'Y'                TO  PI-TOTAL-DISPLAYED-SW
002000     ELSE
002001         MOVE 'TOTAL'            TO  GL-ACCT (PINDX)
002002         MOVE TOTAL-ACCT-AMT     TO  AMTO (PINDX)
002003         SET PINDX  UP  BY  1
002004         MOVE 'NET TOTAL'        TO  GL-ACCT (PINDX)
002005         MOVE TOTAL-ACCT-NET     TO  AMTO (PINDX).
002006
002007 6200-EXIT.
002008     EXIT.
002009 EJECT
002010 7000-PYAJ-FILE-NOTOPEN.
002011     MOVE -1                     TO  MAINTL.
002012     MOVE ER-2232                TO  EMI-ERROR.
002013
002014     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002015
002016     GO TO 8200-SEND-DATAONLY.
002017
002018 7100-COMP-FILE-NOTOPEN.
002019     MOVE -1                     TO  MAINTL.
002020     MOVE ER-2233                TO  EMI-ERROR.
002021
002022     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002023
002024     GO TO 8200-SEND-DATAONLY.
002025 EJECT
002026*
002027*7100-COFA-FILE-NOTOPEN.
002028*    MOVE -1                     TO PFENTERL.
002029*    MOVE ER-2959                TO EMI-ERROR.
002030*    PERFORM 9900-ERROR-FORMAT THRU
002031*            9900-EXIT.
002032*    GO TO 8200-SEND-DATAONLY.
002033*
002034 8100-SEND-INITIAL-MAP.
002035     MOVE WS-CURRENT-DT          TO  DATEO.
002036     MOVE EIBTIME                TO  TIME-IN.
002037     MOVE TIME-OUT               TO  TIMEO.
002038     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
002039     MOVE PI-PROCESSOR-ID        TO  USERIDO.
002040     MOVE -1                     TO  MAINTL.
002041     MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
002042     MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
002043
002044     IF EIBTRNID = TRANS-ID
002045       OR EL640-TRANS-ID
002046       OR EL642-TRANS-ID
002047       OR EL652-TRANS-ID
002048       OR EL6331-TRANS-ID
002049         IF PI-SAV-ENDING-PYAJ-KEY NOT =  SPACES
002050             MOVE PI-SAV-FUNCTION  TO  MAINTI
002051             MOVE PI-SAV-CARRIER   TO  CARRIERO
002052             MOVE PI-SAV-GROUPING  TO  GROUPO
002053             MOVE PI-SAV-FIN-RESP  TO  FINRESPO
002054             MOVE PI-SAV-ACCOUNT   TO  ACCTO
002055             MOVE AL-UANON         TO  MAINTA
002056                                       CARRIERA
002057                                       GROUPA
002058                                       FINRESPA
002059                                       ACCTA
002060         ELSE
002061             NEXT SENTENCE.
002062
002063     
      * EXEC CICS SEND
002064*        MAP     (MAP-NAME)
002065*        MAPSET  (MAPSET-NAME)
002066*        FROM    (EL633AO)
002067*        ERASE
002068*        CURSOR
002069*        END-EXEC.
           MOVE LENGTH OF
            EL633AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004765' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303034373635' TO DFHEIV0
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
           
002070
002071     GO TO 9100-RETURN-TRAN.
002072 EJECT
002073 8200-SEND-DATAONLY.
002074     MOVE WS-CURRENT-DT          TO  DATEO.
002075     MOVE EIBTIME                TO  TIME-IN.
002076     MOVE TIME-OUT               TO  TIMEO.
002077     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
002078     MOVE PI-PROCESSOR-ID        TO  USERIDO.
002079     MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
002080     MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
002081
002082     
      * EXEC CICS SEND
002083*        MAP     (MAP-NAME)
002084*        MAPSET  (MAPSET-NAME)
002085*        FROM    (EL633AO)
002086*        DATAONLY
002087*        CURSOR
002088*    END-EXEC.
           MOVE LENGTH OF
            EL633AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00004784' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303034373834' TO DFHEIV0
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
           
002089
002090     GO TO 9100-RETURN-TRAN.
002091
002092 8300-SEND-TEXT.
002093     
      * EXEC CICS SEND TEXT
002094*        FROM    (LOGOFF-TEXT)
002095*        LENGTH  (LOGOFF-LENGTH)
002096*        ERASE
002097*        FREEKB
002098*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004795' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303034373935' TO DFHEIV0
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
           
002099
002100     
      * EXEC CICS RETURN
002101*    END-EXEC.
      *    MOVE '.(                    ''   #00004802' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034383032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002102 EJECT
002103 8400-LOG-JOURNAL-RECORD.
002104*    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
002105*    MOVE THIS-PGM               TO  JP-PROGRAM-ID.
002106
002107*    EXEC CICS JOURNAL
002108*        JFILEID  (PI-JOURNAL-FILE-ID)
002109*        JTYPEID  ('EL')
002110*        FROM     (JOURNAL-RECORD)
002111*        LENGTH   (223)
002112*        END-EXEC.
002113
002114 8500-DATE-CONVERT.
002115     
      * EXEC CICS LINK
002116*        PROGRAM   (LINK-CLDATCV)
002117*        COMMAREA  (DATE-CONVERSION-DATA)
002118*        LENGTH    (DC-COMM-LENGTH)
002119*    END-EXEC.
      *    MOVE '."C                   (   #00004817' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034383137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-CLDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002120
002121 8500-EXIT.
002122     EXIT.
002123
002124 8600-DEEDIT.
002125     
      * EXEC CICS BIF DEEDIT
002126*        FIELD   (DEEDIT-FIELD)
002127*        LENGTH  (11)
002128*    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004827' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034383237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002129
002130 8600-EXIT.
002131     EXIT.
002132 EJECT
002133 8800-UNAUTHORIZED-ACCESS.
002134     MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
002135
002136     GO TO 8300-SEND-TEXT.
002137
002138 8810-PF23.
002139     MOVE EIBAID                 TO  PI-ENTRY-CD-1.
002140     MOVE XCTL-005               TO  PGM-NAME.
002141
002142     GO TO 9300-XCTL.
002143
002144 9100-RETURN-TRAN.
002145     MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
002146     MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
002147
002148     
      * EXEC CICS RETURN
002149*        TRANSID   (TRANS-ID)
002150*        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
002151*        LENGTH    (PI-COMM-LENGTH)
002152*    END-EXEC.
      *    MOVE '.(CT                  ''   #00004850' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303034383530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002153
002154 9200-RETURN-MAIN-MENU.
002155     MOVE XCTL-626               TO  PGM-NAME.
002156
002157     GO TO 9300-XCTL.
002158
002159 9300-XCTL.
002160     
      * EXEC CICS XCTL
002161*        PROGRAM   (PGM-NAME)
002162*        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
002163*        LENGTH    (PI-COMM-LENGTH)
002164*    END-EXEC.
      *    MOVE '.$C                   %   #00004862' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303034383632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002165
002166 9400-CLEAR.
002167     MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
002168
002169     IF PI-RETURN-TO-PROGRAM = 'EL626'
002170     MOVE SPACES                 TO  PI-CR-CONTROL-IN-PROGRESS
002171                                     PI-SAV-COMP-CONTROL.
002172
002173     GO TO 9300-XCTL.
002174
002175 9500-PF12.
002176     MOVE XCTL-010               TO  PGM-NAME.
002177
002178     GO TO 9300-XCTL.
002179
002180 9600-PGMID-ERROR.
002181     
      * EXEC CICS HANDLE CONDITION
002182*        PGMIDERR  (8300-SEND-TEXT)
002183*    END-EXEC.
      *    MOVE '"$L                   ! + #00004883' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303034383833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002184
002185     MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
002186     MOVE ' '                    TO  PI-ENTRY-CD-1.
002187     MOVE XCTL-005               TO  PGM-NAME.
002188     MOVE PGM-NAME               TO  LOGOFF-PGM.
002189     MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
002190
002191     GO TO 9300-XCTL.
002192
002193 9900-ERROR-FORMAT.
002194     IF NOT EMI-ERRORS-COMPLETE
002195         MOVE LINK-001           TO  PGM-NAME
002196         
      * EXEC CICS LINK
002197*            PROGRAM   (PGM-NAME)
002198*            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
002199*            LENGTH    (EMI-COMM-LENGTH)
002200*        END-EXEC.
      *    MOVE '."C                   (   #00004898' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034383938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002201
002202 9900-EXIT.
002203     EXIT.
002204
002205 9990-ABEND.
002206     MOVE LINK-004               TO  PGM-NAME.
002207     MOVE DFHEIBLK               TO  EMI-LINE1.
002208
002209     
      * EXEC CICS LINK
002210*        PROGRAM   (PGM-NAME)
002211*        COMMAREA  (EMI-LINE1)
002212*        LENGTH    (72)
002213*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00004911' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034393131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002214
002215     MOVE -1                     TO  PFENTERL.
002216
002217     GO TO 8200-SEND-DATAONLY.
002218
002219 9995-SECURITY-VIOLATION.
002220*                            COPY ELCSCTP.
      *>>((file: ELCSCTP))
000001******************************************************************
000002*                                                                *
000003*                            ELCSCTP                             *
000004*                            VMOD=2.001                          *
000005*                                                                *
000006*   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
000007******************************************************************
000008
000009
000010     MOVE EIBDATE          TO SM-JUL-DATE.
000011     MOVE EIBTRMID         TO SM-TERMID.
000012     MOVE THIS-PGM         TO SM-PGM.
000013     MOVE EIBTIME          TO TIME-IN.
000014     MOVE TIME-OUT         TO SM-TIME.
000015     MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
000016
000017     
      * EXEC CICS LINK
000018*         PROGRAM  ('EL003')
000019*         COMMAREA (SECURITY-MESSAGE)
000020*         LENGTH   (80)
000021*    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00004940' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034393430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000022
000023******************************************************************
000024
      *<<((file: ELCSCTP))
002221
002222 9995-EXIT.
002223     EXIT.
002224
002225

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
