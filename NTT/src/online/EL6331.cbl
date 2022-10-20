      *((program: EL6331.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL6331.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 02/12/96 09:49:43.
000007*                            VMOD=2.008
000008*
000009*
000010*AUTHOR.        LOGIC,INC.
000011*               DALLAS, TEXAS.
000012
000013*DATE-COMPILED.
000014
000015*SECURITY.   *****************************************************
000016*            *                                                   *
000017*            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
000018*            *                                                   *
000019*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000020*            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
000021*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
000022*            *                                                   *
000023*            *****************************************************
000024*
000025*REMARKS.
000026*        TRANSACTION - EXB8 - COMPENSATION PAYMENTS/ADJUSTMENTS.
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
000037*                              ADJUSTED REDEFINES EL633BI FILLER
000038* 010803                   PEMA  ADD 1825013200 FOR DCC
000039* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
000040* 022504                   PEMA ADD GL CODE FOR DCC
000041* 093004                   PEMA ADD NEW GL CODE FOR DCC
000042* 120204                   PEMA NOTICED LOW VALS IN BATCH REPORTS
000043* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
000044* 061605    2005051300001  PEMA ADD GL NUM EDIT FOR DCC
000045* 122105    2005033100001  PEMA ADD GL NUM EDIT FOR CSI
000046* 032806                   PEMA ADD MORE GL NUMBERS FOR CSI
000047* 071806    2006012600002  PEMA CHANGE DCC GL NUMBERS
000048* 080206    2006012600002  PEMA ADD GL NUMBER FOR CID & DCC
000049* 092506                   PEMA ADD NEW GL CODE FOR DCC
000050* 031909    2009030300001  AJRA ADD NEW GL CODE FOR CID
000051* 040109    2008050500001  AJRA ADD GL NUMBERS FOR CCC
000052* 031710  CR2009100700001  PEMA ADD RUNNING NET TOTAL
000053* 120711  CR2011120100004  PEMA ADD GL NUMBER FOR CCC
000054* 031912  CR2011120900003  AJRA AHL COMPANY CODE
000055* 100713  CR2013100700001  AJRA ADD 1825091000 FOR CID
000056* 081414    2014012300001  PEMA  ADD PROCESSING FOR CARRIER 7 DCC
000057* 110315  CR2015101400001  PEMA ADD NEW DCC G/L #'S FOR ACH
000058* 111715  IR2015111600001  PEMA  CHG G/L ACCTNO ON CID ACH
000059* 021716  CR2016021000003  PEMA  ADD NEW G/L FOR MACHENS ACCTS
000060* 111016  CR2016110900002  TANA  REPLACE GL NUMBERS FOR CCC
000061* 060817  CR2017060700005  PEMA  ADD NEW G/L FOR VPP
000062* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000063******************************************************************
000064
000065 ENVIRONMENT DIVISION.
000066 DATA DIVISION.
000067 EJECT
000068 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000069 77  FILLER  PIC X(32)  VALUE '********************************'.
000070 77  FILLER  PIC X(32)  VALUE '*    EL6331 WORKING STORAGE    *'.
000071 77  FILLER  PIC X(32)  VALUE '************ V/M 2.008 *********'.
000072
000073 77  HLD-XX  PIC X(32)  VALUE '********************************'.
000074 77  FILLER  PIC X(32)  VALUE '/// SAVE COMPENSATION MASTER ///'.
000075 77  SV-COMP PIC X(400) VALUE SPACE.
000076 77  C1      PIC S999 COMP-3 VALUE +0.
000077*77  FILLER  PIC X(32)  VALUE '/////// SAVE COFA MASTER ///////'.
000078*77  SV-COFA PIC X(42)  VALUE SPACE.
000079*77  K-SPACE PIC X(11)  VALUE SPACES.
000080
000081*                            COPY ELCSCTM.
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
000082*                            COPY ELCSCRTY.
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
000083
000084
000085*01  FORCE-DUMP-X            PIC X(1)        VALUE SPACE.
000086*01  FORCE-DUMP  REDEFINES FORCE-DUMP-X
000087*                            PIC S9.
000088    EJECT
000089
000090 01  STANDARD-AREAS.
000091     12  GETMAIN-SPACE       PIC  X          VALUE SPACE.
000092     12  MAP-NAME            PIC  X(8)       VALUE 'EL633B '.
000093     12  MAPSET-NAME         PIC  X(8)       VALUE 'EL6331S'.
000094     12  SCREEN-NUMBER       PIC  X(4)       VALUE '633B'.
000095     12  TRANS-ID            PIC  X(4)       VALUE 'EXB8'.
000096     12  THIS-PGM            PIC  X(8)       VALUE 'EL6331'.
000097     12  PGM-NAME            PIC  X(8).
000098     12  TIME-IN             PIC S9(7).
000099     12  TIME-OUT-R  REDEFINES  TIME-IN.
000100         16  FILLER          PIC  X.
000101         16  TIME-OUT        PIC  9(2)V9(2).
000102         16  FILLER          PIC  X(2).
000103     12  XCTL-005            PIC  X(8)       VALUE 'EL005'.
000104     12  XCTL-010            PIC  X(8)       VALUE 'EL010'.
000105     12  XCTL-626            PIC  X(8)       VALUE 'EL626'.
000106     12  XCTL-652            PIC  X(8)       VALUE 'EL652'.
000107     12  LINK-001            PIC  X(8)       VALUE 'EL001'.
000108     12  LINK-004            PIC  X(8)       VALUE 'EL004'.
000109     12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.
000110     12  PYAJ-FILE-ID        PIC  X(8)       VALUE 'ERPYAJ'.
000111     12  COMP-FILE-ID        PIC  X(8)       VALUE 'ERCOMP'.
000112     12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.
000113     12  WS-CURRENT-BIN-DT   PIC  X(2)       VALUE SPACES.
000114     12  WORK-SEQ-NO         PIC S9(9)                  COMP-3.
000115     12  TOTAL-AMOUNT        PIC S9(9)V99      VALUE ZEROS.
000116     12  CHECK-REC-TYPE      PIC  X          VALUE SPACE.
000117         88  VALID-REC-TYPE                  VALUE  'R' 'D' 'C'
000118                                                    'S' 'T' 'U'
000119                                                    'X' 'Y' 'Z'
000120                                                    'F'.
000121     12  CHECK-CANC-TYPE         PIC X       VALUE SPACE.
000122         88  VALID-CANC-TYPE                 VALUE 'N' 'Y'.
000123     12  DEEDIT-FIELD            PIC X(11).
000124     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD PIC S9(9)V99.
000125     12  WS-EDITED-AMTS OCCURS 15 TIMES
000126                                INDEXED BY WS-INDX.
000127         16  WS-EDITED-AMT       PIC S9(9)V99.
000128*    12  COFA-FILE-ID            PIC  X(8)   VALUE 'COFAXXX'.
000129*01  FILLER                      PIC  X(15)
000130*                                VALUE '* ACCT BREAK  *'.
000131*01  WS-ACCT-BREAK.
000132*    05  WS-ACCT-1               PIC  X.
000133*    05  FILLER                  PIC  X(5).
000134
000135
000136 01  WS-ERCOMP-TYPE-TYPE.
000137     12  WS-ERCOMP-TYPE OCCURS 15   PIC X.
000138
000139******************************************************************
000140*                      TABLE OF STATE NAMES
000141******************************************************************
000142
000143 01  CHECK-STATE-CODE            PIC  XX     VALUE SPACE.
000144     88  VALID-STATE-CODE        VALUE 'AK' 'AL' 'AR' 'AZ' 'CA'
000145                                       'CD' 'CO' 'CT' 'DC' 'DE'
000146                                       'FL' 'GA' 'GM' 'HI' 'IA'
000147                                       'ID' 'IL' 'IN' 'KS' 'KY'
000148                                       'LA' 'MA' 'MD' 'ME' 'MI'
000149                                       'MN' 'MO' 'MS' 'MT' 'MX'
000150                                       'NC' 'ND' 'NE' 'NH' 'NJ'
000151                                       'NM' 'NV' 'NY' 'OF' 'OH'
000152                                       'OK' 'OR' 'PA' 'PI' 'PR'
000153                                       'RI' 'SC' 'SD' 'TN' 'TX'
000154                                       'UT' 'VA' 'VI' 'VT' 'WA'
000155                                       'WI' 'WV' 'WY'.
000156
000157 01  CHECK-GL-ACCT         PIC X(10)  VALUE SPACE.
000158     88  VALID-GL-ACCOUNT  VALUE '1108121010'
000159                                 '1108124700'
000160                                 '1108125100'
000161                                 '1721211400'
000162                                 '1825011200'
000163                                 '1825011300'
000164                                 '1825091000'
000165                                 '1825099050'
000166                                 '8505700033'
000167                                 '8506400030'
000168                                 '8507200020'
000169                                 '8507200010'
000170                                 '2725010160'.
000171
000172     88  VALID-DCC-GL-ACCOUNT  VALUE '2725040300'
000173                                     '2725040320'
000174                                     '2725040330'
000175                                     '2725040310'
000176                                     '8506400030'
000177                                     '8507200010'
000178                                     '1108121250'.
000179
000180*    88  VALID-CSI-GL-ACCOUNT  VALUE '2725040100'
000181*                                    '2725040110'
000182*                                    '2725040120'
000183*                                    '2725040130'
000184*                                    '2725020400'.
000185     88  VALID-CSI-GL-ACCOUNT  VALUE '1108121010'
000186                                     '1825013200'
000187                                     '1825013300'
000188                                     '1825013400'.
000189
000190
000191     88  VALID-CCC-GL-ACCOUNT  VALUE '1825013100'
000192                                     '1825013200'
000193                                     '1825013300'
000194                                     '1825013400'
000195                                     '8506400030'
000196                                     '8507200010'
000197                                     '8507200020'
000198                                     '1108121010'.
000199
000200     88  VALID-VPP-GL-ACCOUNT  VALUE '2725040510'
000201                                     '2725040520'
000202                                     '7206100400'
000203                                     '7206104100'.
000204
000205     88  VALID-FNL-GL-ACCOUNT  VALUE '1108121010'
000206                                     '1721211400'
000207                                     '1825011100'
000208                                     '1825011200'
000209                                     '1825011300'
000210                                     '1825099050'
000211                                     '2718000110'
000212                                     '2718000120'
000213                                     '8506400030'
000214                                     '8507200010'.
000215
000216
000217 01  ACCESS-KEYS.
000218     12  ERPYAJ-KEY.
000219         16  PYAJ-COMP-CD        PIC  X      VALUE SPACE.
000220         16  PYAJ-CARRIER        PIC  X      VALUE SPACES.
000221         16  PYAJ-GROUPING       PIC  X(6)   VALUE SPACES.
000222         16  PYAJ-FIN-RESP       PIC  X(10)  VALUE SPACES.
000223         16  PYAJ-ACCOUNT        PIC  X(10)  VALUE SPACES.
000224         16  PYAJ-FILE-SEQ-NO    PIC S9(8)   VALUE +0   COMP.
000225         16  PYAJ-RECORD-TYPE    PIC  X      VALUE SPACES.
000226
000227     12  ERPYAJ-RECORD-LENGTH    PIC S9(4)   VALUE +200 COMP.
000228     12  ERPYAJ-JOURNAL-LENGTH   PIC S9(4)   VALUE +223 COMP.
000229
000230     12  ERCOMP-KEY.
000231         16  COMP-COMP-CD        PIC  X      VALUE SPACE.
000232         16  COMP-CARRIER        PIC  X      VALUE SPACES.
000233         16  COMP-GROUPING       PIC  X(6)   VALUE SPACES.
000234         16  COMP-FIN-RESP       PIC  X(10)  VALUE SPACES.
000235         16  COMP-ACCOUNT        PIC  X(10)  VALUE SPACES.
000236         16  COMP-RECORD-TYPE    PIC  X      VALUE SPACES.
000237*    12  COFA-KEY-X.
000238*        16  COFA-COMPANY-X      PIC  X(4)   VALUE SPACES.
000239*        16  COFA-ACCOUNT.
000240*            20  COFA-FILLER     PIC  X(11)  VALUE SPACES.
000241*            20  COFA-MSA-ACCT   PIC  X(07)  VALUE SPACES.
000242*
000243 EJECT
000244 01  ERROR-NUMBERS.
000245     12  ER-0000             PIC  X(4)       VALUE '0000'.
000246     12  ER-0008             PIC  X(4)       VALUE '0008'.
000247     12  ER-0029             PIC  X(4)       VALUE '0029'.
000248     12  ER-0070             PIC  X(4)       VALUE '0070'.
000249     12  ER-0194             PIC  X(4)       VALUE '0194'.
000250     12  ER-0195             PIC  X(4)       VALUE '0195'.
000251     12  ER-0197             PIC  X(4)       VALUE '0197'.
000252     12  ER-2230             PIC  X(4)       VALUE '2230'.
000253     12  ER-2232             PIC  X(4)       VALUE '2232'.
000254     12  ER-2233             PIC  X(4)       VALUE '2233'.
000255     12  ER-2234             PIC  X(4)       VALUE '2234'.
000256     12  ER-2235             PIC  X(4)       VALUE '2235'.
000257     12  ER-2236             PIC  X(4)       VALUE '2236'.
000258     12  ER-2245             PIC  X(4)       VALUE '2245'.
000259     12  ER-2562             PIC  X(4)       VALUE '2562'.
000260     12  ER-2587             PIC  X(4)       VALUE '2587'.
000261     12  ER-2588             PIC  X(4)       VALUE '2588'.
000262     12  ER-2595             PIC  X(4)       VALUE '2595'.
000263     12  ER-2596             PIC  X(4)       VALUE '2596'.
000264     12  ER-2763             PIC  X(4)       VALUE '2763'.
000265     12  ER-2956             PIC  X(4)       VALUE '2956'.
000266     12  ER-2957             PIC  X(4)       VALUE '2957'.
000267     12  ER-2958             PIC  X(4)       VALUE '2958'.
000268     12  ER-2959             PIC  X(4)       VALUE '2959'.
000269     12  ER-2960             PIC  X(4)       VALUE '2960'.
000270     12  ER-2961             PIC  X(4)       VALUE '2961'.
000271     12  ER-9030             PIC  X(4)       VALUE '9030'.
000272
000273 EJECT
000274*                                    COPY ELCDATE.
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
000275 EJECT
000276*                                    COPY ELCLOGOF.
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
000277 EJECT
000278*                                    COPY ELCATTR.
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
000279 EJECT
000280*                                    COPY ELCEMIB.
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
000281 EJECT
000282*                                    COPY ELCINTF.
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
000283     12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
000284         16  PI-PYAJ-FILE-SW         PIC  X.
000285             88  END-OF-ACCT                 VALUE 'A'.
000286             88  END-OF-FILE                 VALUE 'X'.
000287             88  NO-RECORDS                  VALUE 'Y'.
000288             88  NOT-OPEN                    VALUE 'Z'.
000289         16  PI-PREV-FUNCTION        PIC  X.
000290         16  PI-SAV-FUNCTION         PIC  X.
000291         16  PI-SEQ-NOS.
000292             20  FILLER  OCCURS 13 TIMES
000293                             INDEXED BY NDX.
000294                 24  PI-REC-TYPE     PIC  X.
000295                 24  PI-FILE-SEQ-NO  PIC S9(8).
000296         16  PI-SAV-ENDING-PYAJ-KEY.
000297             20  PI-SAV-COMP-CD      PIC  X.
000298             20  PI-SAV-CARRIER      PIC  X.
000299             20  PI-SAV-GROUPING     PIC  X(3).
000300             20  PI-SAV-FIN-RESP     PIC  X(6).
000301             20  PI-SAV-ACCOUNT      PIC  X(6).
000302             20  PI-SAV-FILE-SEQ-NO  PIC S9(8)          COMP.
000303             20  PI-SAV-RECORD-TYPE  PIC  X.
000304         16  FILLER                  PIC  X(498).
000305 EJECT
000306*                            COPY ELCJPFX.
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
000307                             PIC  X(223).
000308 EJECT
000309*                            COPY ELCAID.
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
000310
000311 01  FILLER    REDEFINES DFHAID.
000312     12  FILLER              PIC  X(8).
000313     12  PF-VALUES           PIC  X          OCCURS 2 TIMES.
000314 EJECT
000315*                            COPY EL6331S.
      *>>((file: EL6331S))
000001 01  EL633BI.
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
000028     05  CAR1L PIC S9(0004) COMP.
000029     05  CAR1F PIC  X(0001).
000030     05  FILLER REDEFINES CAR1F.
000031         10  CAR1A PIC  X(0001).
000032     05  CAR1I PIC  X(0001).
000033*    -------------------------------
000034     05  GRP1L PIC S9(0004) COMP.
000035     05  GRP1F PIC  X(0001).
000036     05  FILLER REDEFINES GRP1F.
000037         10  GRP1A PIC  X(0001).
000038     05  GRP1I PIC  X(0006).
000039*    -------------------------------
000040     05  FRESP1L PIC S9(0004) COMP.
000041     05  FRESP1F PIC  X(0001).
000042     05  FILLER REDEFINES FRESP1F.
000043         10  FRESP1A PIC  X(0001).
000044     05  FRESP1I PIC  X(0010).
000045*    -------------------------------
000046     05  ACCT1L PIC S9(0004) COMP.
000047     05  ACCT1F PIC  X(0001).
000048     05  FILLER REDEFINES ACCT1F.
000049         10  ACCT1A PIC  X(0001).
000050     05  ACCT1I PIC  X(0010).
000051*    -------------------------------
000052     05  TYPE1L PIC S9(0004) COMP.
000053     05  TYPE1F PIC  X(0001).
000054     05  FILLER REDEFINES TYPE1F.
000055         10  TYPE1A PIC  X(0001).
000056     05  TYPE1I PIC  X(0001).
000057*    -------------------------------
000058     05  AMT1L PIC S9(0004) COMP.
000059     05  AMT1F PIC  X(0001).
000060     05  FILLER REDEFINES AMT1F.
000061         10  AMT1A PIC  X(0001).
000062     05  AMT1I PIC  X(0011).
000063*    -------------------------------
000064     05  MSAAC1L PIC S9(0004) COMP.
000065     05  MSAAC1F PIC  X(0001).
000066     05  FILLER REDEFINES MSAAC1F.
000067         10  MSAAC1A PIC  X(0001).
000068     05  MSAAC1I PIC  X(0010).
000069*    -------------------------------
000070     05  MSAST1L PIC S9(0004) COMP.
000071     05  MSAST1F PIC  X(0001).
000072     05  FILLER REDEFINES MSAST1F.
000073         10  MSAST1A PIC  X(0001).
000074     05  MSAST1I PIC  X(0002).
000075*    -------------------------------
000076     05  MSACN1L PIC S9(0004) COMP.
000077     05  MSACN1F PIC  X(0001).
000078     05  FILLER REDEFINES MSACN1F.
000079         10  MSACN1A PIC  X(0001).
000080     05  MSACN1I PIC  X(0001).
000081*    -------------------------------
000082     05  MSACM1L PIC S9(0004) COMP.
000083     05  MSACM1F PIC  X(0001).
000084     05  FILLER REDEFINES MSACM1F.
000085         10  MSACM1A PIC  X(0001).
000086     05  MSACM1I PIC  X(0010).
000087*    -------------------------------
000088     05  CAR2L PIC S9(0004) COMP.
000089     05  CAR2F PIC  X(0001).
000090     05  FILLER REDEFINES CAR2F.
000091         10  CAR2A PIC  X(0001).
000092     05  CAR2I PIC  X(0001).
000093*    -------------------------------
000094     05  GRP2L PIC S9(0004) COMP.
000095     05  GRP2F PIC  X(0001).
000096     05  FILLER REDEFINES GRP2F.
000097         10  GRP2A PIC  X(0001).
000098     05  GRP2I PIC  X(0006).
000099*    -------------------------------
000100     05  FRESP2L PIC S9(0004) COMP.
000101     05  FRESP2F PIC  X(0001).
000102     05  FILLER REDEFINES FRESP2F.
000103         10  FRESP2A PIC  X(0001).
000104     05  FRESP2I PIC  X(0010).
000105*    -------------------------------
000106     05  ACCT2L PIC S9(0004) COMP.
000107     05  ACCT2F PIC  X(0001).
000108     05  FILLER REDEFINES ACCT2F.
000109         10  ACCT2A PIC  X(0001).
000110     05  ACCT2I PIC  X(0010).
000111*    -------------------------------
000112     05  TYPE2L PIC S9(0004) COMP.
000113     05  TYPE2F PIC  X(0001).
000114     05  FILLER REDEFINES TYPE2F.
000115         10  TYPE2A PIC  X(0001).
000116     05  TYPE2I PIC  X(0001).
000117*    -------------------------------
000118     05  AMT2L PIC S9(0004) COMP.
000119     05  AMT2F PIC  X(0001).
000120     05  FILLER REDEFINES AMT2F.
000121         10  AMT2A PIC  X(0001).
000122     05  AMT2I PIC  X(0011).
000123*    -------------------------------
000124     05  MSAAC2L PIC S9(0004) COMP.
000125     05  MSAAC2F PIC  X(0001).
000126     05  FILLER REDEFINES MSAAC2F.
000127         10  MSAAC2A PIC  X(0001).
000128     05  MSAAC2I PIC  X(0010).
000129*    -------------------------------
000130     05  MSAST2L PIC S9(0004) COMP.
000131     05  MSAST2F PIC  X(0001).
000132     05  FILLER REDEFINES MSAST2F.
000133         10  MSAST2A PIC  X(0001).
000134     05  MSAST2I PIC  X(0002).
000135*    -------------------------------
000136     05  MSACN2L PIC S9(0004) COMP.
000137     05  MSACN2F PIC  X(0001).
000138     05  FILLER REDEFINES MSACN2F.
000139         10  MSACN2A PIC  X(0001).
000140     05  MSACN2I PIC  X(0001).
000141*    -------------------------------
000142     05  MSACM2L PIC S9(0004) COMP.
000143     05  MSACM2F PIC  X(0001).
000144     05  FILLER REDEFINES MSACM2F.
000145         10  MSACM2A PIC  X(0001).
000146     05  MSACM2I PIC  X(0010).
000147*    -------------------------------
000148     05  CAR3L PIC S9(0004) COMP.
000149     05  CAR3F PIC  X(0001).
000150     05  FILLER REDEFINES CAR3F.
000151         10  CAR3A PIC  X(0001).
000152     05  CAR3I PIC  X(0001).
000153*    -------------------------------
000154     05  GRP3L PIC S9(0004) COMP.
000155     05  GRP3F PIC  X(0001).
000156     05  FILLER REDEFINES GRP3F.
000157         10  GRP3A PIC  X(0001).
000158     05  GRP3I PIC  X(0006).
000159*    -------------------------------
000160     05  FRESP3L PIC S9(0004) COMP.
000161     05  FRESP3F PIC  X(0001).
000162     05  FILLER REDEFINES FRESP3F.
000163         10  FRESP3A PIC  X(0001).
000164     05  FRESP3I PIC  X(0010).
000165*    -------------------------------
000166     05  ACCT3L PIC S9(0004) COMP.
000167     05  ACCT3F PIC  X(0001).
000168     05  FILLER REDEFINES ACCT3F.
000169         10  ACCT3A PIC  X(0001).
000170     05  ACCT3I PIC  X(0010).
000171*    -------------------------------
000172     05  TYPE3L PIC S9(0004) COMP.
000173     05  TYPE3F PIC  X(0001).
000174     05  FILLER REDEFINES TYPE3F.
000175         10  TYPE3A PIC  X(0001).
000176     05  TYPE3I PIC  X(0001).
000177*    -------------------------------
000178     05  AMT3L PIC S9(0004) COMP.
000179     05  AMT3F PIC  X(0001).
000180     05  FILLER REDEFINES AMT3F.
000181         10  AMT3A PIC  X(0001).
000182     05  AMT3I PIC  X(0011).
000183*    -------------------------------
000184     05  MSAAC3L PIC S9(0004) COMP.
000185     05  MSAAC3F PIC  X(0001).
000186     05  FILLER REDEFINES MSAAC3F.
000187         10  MSAAC3A PIC  X(0001).
000188     05  MSAAC3I PIC  X(0010).
000189*    -------------------------------
000190     05  MSAST3L PIC S9(0004) COMP.
000191     05  MSAST3F PIC  X(0001).
000192     05  FILLER REDEFINES MSAST3F.
000193         10  MSAST3A PIC  X(0001).
000194     05  MSAST3I PIC  X(0002).
000195*    -------------------------------
000196     05  MSACN3L PIC S9(0004) COMP.
000197     05  MSACN3F PIC  X(0001).
000198     05  FILLER REDEFINES MSACN3F.
000199         10  MSACN3A PIC  X(0001).
000200     05  MSACN3I PIC  X(0001).
000201*    -------------------------------
000202     05  MSACM3L PIC S9(0004) COMP.
000203     05  MSACM3F PIC  X(0001).
000204     05  FILLER REDEFINES MSACM3F.
000205         10  MSACM3A PIC  X(0001).
000206     05  MSACM3I PIC  X(0010).
000207*    -------------------------------
000208     05  CAR4L PIC S9(0004) COMP.
000209     05  CAR4F PIC  X(0001).
000210     05  FILLER REDEFINES CAR4F.
000211         10  CAR4A PIC  X(0001).
000212     05  CAR4I PIC  X(0001).
000213*    -------------------------------
000214     05  GRP4L PIC S9(0004) COMP.
000215     05  GRP4F PIC  X(0001).
000216     05  FILLER REDEFINES GRP4F.
000217         10  GRP4A PIC  X(0001).
000218     05  GRP4I PIC  X(0006).
000219*    -------------------------------
000220     05  FRESP4L PIC S9(0004) COMP.
000221     05  FRESP4F PIC  X(0001).
000222     05  FILLER REDEFINES FRESP4F.
000223         10  FRESP4A PIC  X(0001).
000224     05  FRESP4I PIC  X(0010).
000225*    -------------------------------
000226     05  ACCT4L PIC S9(0004) COMP.
000227     05  ACCT4F PIC  X(0001).
000228     05  FILLER REDEFINES ACCT4F.
000229         10  ACCT4A PIC  X(0001).
000230     05  ACCT4I PIC  X(0010).
000231*    -------------------------------
000232     05  TYPE4L PIC S9(0004) COMP.
000233     05  TYPE4F PIC  X(0001).
000234     05  FILLER REDEFINES TYPE4F.
000235         10  TYPE4A PIC  X(0001).
000236     05  TYPE4I PIC  X(0001).
000237*    -------------------------------
000238     05  AMT4L PIC S9(0004) COMP.
000239     05  AMT4F PIC  X(0001).
000240     05  FILLER REDEFINES AMT4F.
000241         10  AMT4A PIC  X(0001).
000242     05  AMT4I PIC  X(0011).
000243*    -------------------------------
000244     05  MSAAC4L PIC S9(0004) COMP.
000245     05  MSAAC4F PIC  X(0001).
000246     05  FILLER REDEFINES MSAAC4F.
000247         10  MSAAC4A PIC  X(0001).
000248     05  MSAAC4I PIC  X(0010).
000249*    -------------------------------
000250     05  MSAST4L PIC S9(0004) COMP.
000251     05  MSAST4F PIC  X(0001).
000252     05  FILLER REDEFINES MSAST4F.
000253         10  MSAST4A PIC  X(0001).
000254     05  MSAST4I PIC  X(0002).
000255*    -------------------------------
000256     05  MSACN4L PIC S9(0004) COMP.
000257     05  MSACN4F PIC  X(0001).
000258     05  FILLER REDEFINES MSACN4F.
000259         10  MSACN4A PIC  X(0001).
000260     05  MSACN4I PIC  X(0001).
000261*    -------------------------------
000262     05  MSACM4L PIC S9(0004) COMP.
000263     05  MSACM4F PIC  X(0001).
000264     05  FILLER REDEFINES MSACM4F.
000265         10  MSACM4A PIC  X(0001).
000266     05  MSACM4I PIC  X(0010).
000267*    -------------------------------
000268     05  CAR5L PIC S9(0004) COMP.
000269     05  CAR5F PIC  X(0001).
000270     05  FILLER REDEFINES CAR5F.
000271         10  CAR5A PIC  X(0001).
000272     05  CAR5I PIC  X(0001).
000273*    -------------------------------
000274     05  GRP5L PIC S9(0004) COMP.
000275     05  GRP5F PIC  X(0001).
000276     05  FILLER REDEFINES GRP5F.
000277         10  GRP5A PIC  X(0001).
000278     05  GRP5I PIC  X(0006).
000279*    -------------------------------
000280     05  FRESP5L PIC S9(0004) COMP.
000281     05  FRESP5F PIC  X(0001).
000282     05  FILLER REDEFINES FRESP5F.
000283         10  FRESP5A PIC  X(0001).
000284     05  FRESP5I PIC  X(0010).
000285*    -------------------------------
000286     05  ACCT5L PIC S9(0004) COMP.
000287     05  ACCT5F PIC  X(0001).
000288     05  FILLER REDEFINES ACCT5F.
000289         10  ACCT5A PIC  X(0001).
000290     05  ACCT5I PIC  X(0010).
000291*    -------------------------------
000292     05  TYPE5L PIC S9(0004) COMP.
000293     05  TYPE5F PIC  X(0001).
000294     05  FILLER REDEFINES TYPE5F.
000295         10  TYPE5A PIC  X(0001).
000296     05  TYPE5I PIC  X(0001).
000297*    -------------------------------
000298     05  AMT5L PIC S9(0004) COMP.
000299     05  AMT5F PIC  X(0001).
000300     05  FILLER REDEFINES AMT5F.
000301         10  AMT5A PIC  X(0001).
000302     05  AMT5I PIC  X(0011).
000303*    -------------------------------
000304     05  MSAAC5L PIC S9(0004) COMP.
000305     05  MSAAC5F PIC  X(0001).
000306     05  FILLER REDEFINES MSAAC5F.
000307         10  MSAAC5A PIC  X(0001).
000308     05  MSAAC5I PIC  X(0010).
000309*    -------------------------------
000310     05  MSAST5L PIC S9(0004) COMP.
000311     05  MSAST5F PIC  X(0001).
000312     05  FILLER REDEFINES MSAST5F.
000313         10  MSAST5A PIC  X(0001).
000314     05  MSAST5I PIC  X(0002).
000315*    -------------------------------
000316     05  MSACN5L PIC S9(0004) COMP.
000317     05  MSACN5F PIC  X(0001).
000318     05  FILLER REDEFINES MSACN5F.
000319         10  MSACN5A PIC  X(0001).
000320     05  MSACN5I PIC  X(0001).
000321*    -------------------------------
000322     05  MSACM5L PIC S9(0004) COMP.
000323     05  MSACM5F PIC  X(0001).
000324     05  FILLER REDEFINES MSACM5F.
000325         10  MSACM5A PIC  X(0001).
000326     05  MSACM5I PIC  X(0010).
000327*    -------------------------------
000328     05  CAR6L PIC S9(0004) COMP.
000329     05  CAR6F PIC  X(0001).
000330     05  FILLER REDEFINES CAR6F.
000331         10  CAR6A PIC  X(0001).
000332     05  CAR6I PIC  X(0001).
000333*    -------------------------------
000334     05  GRP6L PIC S9(0004) COMP.
000335     05  GRP6F PIC  X(0001).
000336     05  FILLER REDEFINES GRP6F.
000337         10  GRP6A PIC  X(0001).
000338     05  GRP6I PIC  X(0006).
000339*    -------------------------------
000340     05  FRESP6L PIC S9(0004) COMP.
000341     05  FRESP6F PIC  X(0001).
000342     05  FILLER REDEFINES FRESP6F.
000343         10  FRESP6A PIC  X(0001).
000344     05  FRESP6I PIC  X(0010).
000345*    -------------------------------
000346     05  ACCT6L PIC S9(0004) COMP.
000347     05  ACCT6F PIC  X(0001).
000348     05  FILLER REDEFINES ACCT6F.
000349         10  ACCT6A PIC  X(0001).
000350     05  ACCT6I PIC  X(0010).
000351*    -------------------------------
000352     05  TYPE6L PIC S9(0004) COMP.
000353     05  TYPE6F PIC  X(0001).
000354     05  FILLER REDEFINES TYPE6F.
000355         10  TYPE6A PIC  X(0001).
000356     05  TYPE6I PIC  X(0001).
000357*    -------------------------------
000358     05  AMT6L PIC S9(0004) COMP.
000359     05  AMT6F PIC  X(0001).
000360     05  FILLER REDEFINES AMT6F.
000361         10  AMT6A PIC  X(0001).
000362     05  AMT6I PIC  X(0011).
000363*    -------------------------------
000364     05  MSAAC6L PIC S9(0004) COMP.
000365     05  MSAAC6F PIC  X(0001).
000366     05  FILLER REDEFINES MSAAC6F.
000367         10  MSAAC6A PIC  X(0001).
000368     05  MSAAC6I PIC  X(0010).
000369*    -------------------------------
000370     05  MSAST6L PIC S9(0004) COMP.
000371     05  MSAST6F PIC  X(0001).
000372     05  FILLER REDEFINES MSAST6F.
000373         10  MSAST6A PIC  X(0001).
000374     05  MSAST6I PIC  X(0002).
000375*    -------------------------------
000376     05  MSACN6L PIC S9(0004) COMP.
000377     05  MSACN6F PIC  X(0001).
000378     05  FILLER REDEFINES MSACN6F.
000379         10  MSACN6A PIC  X(0001).
000380     05  MSACN6I PIC  X(0001).
000381*    -------------------------------
000382     05  MSACM6L PIC S9(0004) COMP.
000383     05  MSACM6F PIC  X(0001).
000384     05  FILLER REDEFINES MSACM6F.
000385         10  MSACM6A PIC  X(0001).
000386     05  MSACM6I PIC  X(0010).
000387*    -------------------------------
000388     05  CAR7L PIC S9(0004) COMP.
000389     05  CAR7F PIC  X(0001).
000390     05  FILLER REDEFINES CAR7F.
000391         10  CAR7A PIC  X(0001).
000392     05  CAR7I PIC  X(0001).
000393*    -------------------------------
000394     05  GRP7L PIC S9(0004) COMP.
000395     05  GRP7F PIC  X(0001).
000396     05  FILLER REDEFINES GRP7F.
000397         10  GRP7A PIC  X(0001).
000398     05  GRP7I PIC  X(0006).
000399*    -------------------------------
000400     05  FRESP7L PIC S9(0004) COMP.
000401     05  FRESP7F PIC  X(0001).
000402     05  FILLER REDEFINES FRESP7F.
000403         10  FRESP7A PIC  X(0001).
000404     05  FRESP7I PIC  X(0010).
000405*    -------------------------------
000406     05  ACCT7L PIC S9(0004) COMP.
000407     05  ACCT7F PIC  X(0001).
000408     05  FILLER REDEFINES ACCT7F.
000409         10  ACCT7A PIC  X(0001).
000410     05  ACCT7I PIC  X(0010).
000411*    -------------------------------
000412     05  TYPE7L PIC S9(0004) COMP.
000413     05  TYPE7F PIC  X(0001).
000414     05  FILLER REDEFINES TYPE7F.
000415         10  TYPE7A PIC  X(0001).
000416     05  TYPE7I PIC  X(0001).
000417*    -------------------------------
000418     05  AMT7L PIC S9(0004) COMP.
000419     05  AMT7F PIC  X(0001).
000420     05  FILLER REDEFINES AMT7F.
000421         10  AMT7A PIC  X(0001).
000422     05  AMT7I PIC  X(0011).
000423*    -------------------------------
000424     05  MSAAC7L PIC S9(0004) COMP.
000425     05  MSAAC7F PIC  X(0001).
000426     05  FILLER REDEFINES MSAAC7F.
000427         10  MSAAC7A PIC  X(0001).
000428     05  MSAAC7I PIC  X(0010).
000429*    -------------------------------
000430     05  MSAST7L PIC S9(0004) COMP.
000431     05  MSAST7F PIC  X(0001).
000432     05  FILLER REDEFINES MSAST7F.
000433         10  MSAST7A PIC  X(0001).
000434     05  MSAST7I PIC  X(0002).
000435*    -------------------------------
000436     05  MSACN7L PIC S9(0004) COMP.
000437     05  MSACN7F PIC  X(0001).
000438     05  FILLER REDEFINES MSACN7F.
000439         10  MSACN7A PIC  X(0001).
000440     05  MSACN7I PIC  X(0001).
000441*    -------------------------------
000442     05  MSACM7L PIC S9(0004) COMP.
000443     05  MSACM7F PIC  X(0001).
000444     05  FILLER REDEFINES MSACM7F.
000445         10  MSACM7A PIC  X(0001).
000446     05  MSACM7I PIC  X(0010).
000447*    -------------------------------
000448     05  CAR8L PIC S9(0004) COMP.
000449     05  CAR8F PIC  X(0001).
000450     05  FILLER REDEFINES CAR8F.
000451         10  CAR8A PIC  X(0001).
000452     05  CAR8I PIC  X(0001).
000453*    -------------------------------
000454     05  GRP8L PIC S9(0004) COMP.
000455     05  GRP8F PIC  X(0001).
000456     05  FILLER REDEFINES GRP8F.
000457         10  GRP8A PIC  X(0001).
000458     05  GRP8I PIC  X(0006).
000459*    -------------------------------
000460     05  FRESP8L PIC S9(0004) COMP.
000461     05  FRESP8F PIC  X(0001).
000462     05  FILLER REDEFINES FRESP8F.
000463         10  FRESP8A PIC  X(0001).
000464     05  FRESP8I PIC  X(0010).
000465*    -------------------------------
000466     05  ACCT8L PIC S9(0004) COMP.
000467     05  ACCT8F PIC  X(0001).
000468     05  FILLER REDEFINES ACCT8F.
000469         10  ACCT8A PIC  X(0001).
000470     05  ACCT8I PIC  X(0010).
000471*    -------------------------------
000472     05  TYPE8L PIC S9(0004) COMP.
000473     05  TYPE8F PIC  X(0001).
000474     05  FILLER REDEFINES TYPE8F.
000475         10  TYPE8A PIC  X(0001).
000476     05  TYPE8I PIC  X(0001).
000477*    -------------------------------
000478     05  AMT8L PIC S9(0004) COMP.
000479     05  AMT8F PIC  X(0001).
000480     05  FILLER REDEFINES AMT8F.
000481         10  AMT8A PIC  X(0001).
000482     05  AMT8I PIC  X(0011).
000483*    -------------------------------
000484     05  MSAAC8L PIC S9(0004) COMP.
000485     05  MSAAC8F PIC  X(0001).
000486     05  FILLER REDEFINES MSAAC8F.
000487         10  MSAAC8A PIC  X(0001).
000488     05  MSAAC8I PIC  X(0010).
000489*    -------------------------------
000490     05  MSAST8L PIC S9(0004) COMP.
000491     05  MSAST8F PIC  X(0001).
000492     05  FILLER REDEFINES MSAST8F.
000493         10  MSAST8A PIC  X(0001).
000494     05  MSAST8I PIC  X(0002).
000495*    -------------------------------
000496     05  MSACN8L PIC S9(0004) COMP.
000497     05  MSACN8F PIC  X(0001).
000498     05  FILLER REDEFINES MSACN8F.
000499         10  MSACN8A PIC  X(0001).
000500     05  MSACN8I PIC  X(0001).
000501*    -------------------------------
000502     05  MSACM8L PIC S9(0004) COMP.
000503     05  MSACM8F PIC  X(0001).
000504     05  FILLER REDEFINES MSACM8F.
000505         10  MSACM8A PIC  X(0001).
000506     05  MSACM8I PIC  X(0010).
000507*    -------------------------------
000508     05  CAR9L PIC S9(0004) COMP.
000509     05  CAR9F PIC  X(0001).
000510     05  FILLER REDEFINES CAR9F.
000511         10  CAR9A PIC  X(0001).
000512     05  CAR9I PIC  X(0001).
000513*    -------------------------------
000514     05  GRP9L PIC S9(0004) COMP.
000515     05  GRP9F PIC  X(0001).
000516     05  FILLER REDEFINES GRP9F.
000517         10  GRP9A PIC  X(0001).
000518     05  GRP9I PIC  X(0006).
000519*    -------------------------------
000520     05  FRESP9L PIC S9(0004) COMP.
000521     05  FRESP9F PIC  X(0001).
000522     05  FILLER REDEFINES FRESP9F.
000523         10  FRESP9A PIC  X(0001).
000524     05  FRESP9I PIC  X(0010).
000525*    -------------------------------
000526     05  ACCT9L PIC S9(0004) COMP.
000527     05  ACCT9F PIC  X(0001).
000528     05  FILLER REDEFINES ACCT9F.
000529         10  ACCT9A PIC  X(0001).
000530     05  ACCT9I PIC  X(0010).
000531*    -------------------------------
000532     05  TYPE9L PIC S9(0004) COMP.
000533     05  TYPE9F PIC  X(0001).
000534     05  FILLER REDEFINES TYPE9F.
000535         10  TYPE9A PIC  X(0001).
000536     05  TYPE9I PIC  X(0001).
000537*    -------------------------------
000538     05  AMT9L PIC S9(0004) COMP.
000539     05  AMT9F PIC  X(0001).
000540     05  FILLER REDEFINES AMT9F.
000541         10  AMT9A PIC  X(0001).
000542     05  AMT9I PIC  X(0011).
000543*    -------------------------------
000544     05  MSAAC9L PIC S9(0004) COMP.
000545     05  MSAAC9F PIC  X(0001).
000546     05  FILLER REDEFINES MSAAC9F.
000547         10  MSAAC9A PIC  X(0001).
000548     05  MSAAC9I PIC  X(0010).
000549*    -------------------------------
000550     05  MSAST9L PIC S9(0004) COMP.
000551     05  MSAST9F PIC  X(0001).
000552     05  FILLER REDEFINES MSAST9F.
000553         10  MSAST9A PIC  X(0001).
000554     05  MSAST9I PIC  X(0002).
000555*    -------------------------------
000556     05  MSACN9L PIC S9(0004) COMP.
000557     05  MSACN9F PIC  X(0001).
000558     05  FILLER REDEFINES MSACN9F.
000559         10  MSACN9A PIC  X(0001).
000560     05  MSACN9I PIC  X(0001).
000561*    -------------------------------
000562     05  MSACM9L PIC S9(0004) COMP.
000563     05  MSACM9F PIC  X(0001).
000564     05  FILLER REDEFINES MSACM9F.
000565         10  MSACM9A PIC  X(0001).
000566     05  MSACM9I PIC  X(0010).
000567*    -------------------------------
000568     05  CAR10L PIC S9(0004) COMP.
000569     05  CAR10F PIC  X(0001).
000570     05  FILLER REDEFINES CAR10F.
000571         10  CAR10A PIC  X(0001).
000572     05  CAR10I PIC  X(0001).
000573*    -------------------------------
000574     05  GRP10L PIC S9(0004) COMP.
000575     05  GRP10F PIC  X(0001).
000576     05  FILLER REDEFINES GRP10F.
000577         10  GRP10A PIC  X(0001).
000578     05  GRP10I PIC  X(0006).
000579*    -------------------------------
000580     05  FRESP10L PIC S9(0004) COMP.
000581     05  FRESP10F PIC  X(0001).
000582     05  FILLER REDEFINES FRESP10F.
000583         10  FRESP10A PIC  X(0001).
000584     05  FRESP10I PIC  X(0010).
000585*    -------------------------------
000586     05  ACCT10L PIC S9(0004) COMP.
000587     05  ACCT10F PIC  X(0001).
000588     05  FILLER REDEFINES ACCT10F.
000589         10  ACCT10A PIC  X(0001).
000590     05  ACCT10I PIC  X(0010).
000591*    -------------------------------
000592     05  TYPE10L PIC S9(0004) COMP.
000593     05  TYPE10F PIC  X(0001).
000594     05  FILLER REDEFINES TYPE10F.
000595         10  TYPE10A PIC  X(0001).
000596     05  TYPE10I PIC  X(0001).
000597*    -------------------------------
000598     05  AMT10L PIC S9(0004) COMP.
000599     05  AMT10F PIC  X(0001).
000600     05  FILLER REDEFINES AMT10F.
000601         10  AMT10A PIC  X(0001).
000602     05  AMT10I PIC  X(0011).
000603*    -------------------------------
000604     05  MSAAC10L PIC S9(0004) COMP.
000605     05  MSAAC10F PIC  X(0001).
000606     05  FILLER REDEFINES MSAAC10F.
000607         10  MSAAC10A PIC  X(0001).
000608     05  MSAAC10I PIC  X(0010).
000609*    -------------------------------
000610     05  MSAST10L PIC S9(0004) COMP.
000611     05  MSAST10F PIC  X(0001).
000612     05  FILLER REDEFINES MSAST10F.
000613         10  MSAST10A PIC  X(0001).
000614     05  MSAST10I PIC  X(0002).
000615*    -------------------------------
000616     05  MSACN10L PIC S9(0004) COMP.
000617     05  MSACN10F PIC  X(0001).
000618     05  FILLER REDEFINES MSACN10F.
000619         10  MSACN10A PIC  X(0001).
000620     05  MSACN10I PIC  X(0001).
000621*    -------------------------------
000622     05  MSACM10L PIC S9(0004) COMP.
000623     05  MSACM10F PIC  X(0001).
000624     05  FILLER REDEFINES MSACM10F.
000625         10  MSACM10A PIC  X(0001).
000626     05  MSACM10I PIC  X(0010).
000627*    -------------------------------
000628     05  CAR11L PIC S9(0004) COMP.
000629     05  CAR11F PIC  X(0001).
000630     05  FILLER REDEFINES CAR11F.
000631         10  CAR11A PIC  X(0001).
000632     05  CAR11I PIC  X(0001).
000633*    -------------------------------
000634     05  GRP11L PIC S9(0004) COMP.
000635     05  GRP11F PIC  X(0001).
000636     05  FILLER REDEFINES GRP11F.
000637         10  GRP11A PIC  X(0001).
000638     05  GRP11I PIC  X(0006).
000639*    -------------------------------
000640     05  FRESP11L PIC S9(0004) COMP.
000641     05  FRESP11F PIC  X(0001).
000642     05  FILLER REDEFINES FRESP11F.
000643         10  FRESP11A PIC  X(0001).
000644     05  FRESP11I PIC  X(0010).
000645*    -------------------------------
000646     05  ACCT11L PIC S9(0004) COMP.
000647     05  ACCT11F PIC  X(0001).
000648     05  FILLER REDEFINES ACCT11F.
000649         10  ACCT11A PIC  X(0001).
000650     05  ACCT11I PIC  X(0010).
000651*    -------------------------------
000652     05  TYPE11L PIC S9(0004) COMP.
000653     05  TYPE11F PIC  X(0001).
000654     05  FILLER REDEFINES TYPE11F.
000655         10  TYPE11A PIC  X(0001).
000656     05  TYPE11I PIC  X(0001).
000657*    -------------------------------
000658     05  AMT11L PIC S9(0004) COMP.
000659     05  AMT11F PIC  X(0001).
000660     05  FILLER REDEFINES AMT11F.
000661         10  AMT11A PIC  X(0001).
000662     05  AMT11I PIC  X(0011).
000663*    -------------------------------
000664     05  MSAAC11L PIC S9(0004) COMP.
000665     05  MSAAC11F PIC  X(0001).
000666     05  FILLER REDEFINES MSAAC11F.
000667         10  MSAAC11A PIC  X(0001).
000668     05  MSAAC11I PIC  X(0010).
000669*    -------------------------------
000670     05  MSAST11L PIC S9(0004) COMP.
000671     05  MSAST11F PIC  X(0001).
000672     05  FILLER REDEFINES MSAST11F.
000673         10  MSAST11A PIC  X(0001).
000674     05  MSAST11I PIC  X(0002).
000675*    -------------------------------
000676     05  MSACN11L PIC S9(0004) COMP.
000677     05  MSACN11F PIC  X(0001).
000678     05  FILLER REDEFINES MSACN11F.
000679         10  MSACN11A PIC  X(0001).
000680     05  MSACN11I PIC  X(0001).
000681*    -------------------------------
000682     05  MSACM11L PIC S9(0004) COMP.
000683     05  MSACM11F PIC  X(0001).
000684     05  FILLER REDEFINES MSACM11F.
000685         10  MSACM11A PIC  X(0001).
000686     05  MSACM11I PIC  X(0010).
000687*    -------------------------------
000688     05  CAR12L PIC S9(0004) COMP.
000689     05  CAR12F PIC  X(0001).
000690     05  FILLER REDEFINES CAR12F.
000691         10  CAR12A PIC  X(0001).
000692     05  CAR12I PIC  X(0001).
000693*    -------------------------------
000694     05  GRP12L PIC S9(0004) COMP.
000695     05  GRP12F PIC  X(0001).
000696     05  FILLER REDEFINES GRP12F.
000697         10  GRP12A PIC  X(0001).
000698     05  GRP12I PIC  X(0006).
000699*    -------------------------------
000700     05  FRESP12L PIC S9(0004) COMP.
000701     05  FRESP12F PIC  X(0001).
000702     05  FILLER REDEFINES FRESP12F.
000703         10  FRESP12A PIC  X(0001).
000704     05  FRESP12I PIC  X(0010).
000705*    -------------------------------
000706     05  ACCT12L PIC S9(0004) COMP.
000707     05  ACCT12F PIC  X(0001).
000708     05  FILLER REDEFINES ACCT12F.
000709         10  ACCT12A PIC  X(0001).
000710     05  ACCT12I PIC  X(0010).
000711*    -------------------------------
000712     05  TYPE12L PIC S9(0004) COMP.
000713     05  TYPE12F PIC  X(0001).
000714     05  FILLER REDEFINES TYPE12F.
000715         10  TYPE12A PIC  X(0001).
000716     05  TYPE12I PIC  X(0001).
000717*    -------------------------------
000718     05  AMT12L PIC S9(0004) COMP.
000719     05  AMT12F PIC  X(0001).
000720     05  FILLER REDEFINES AMT12F.
000721         10  AMT12A PIC  X(0001).
000722     05  AMT12I PIC  X(0011).
000723*    -------------------------------
000724     05  MSAAC12L PIC S9(0004) COMP.
000725     05  MSAAC12F PIC  X(0001).
000726     05  FILLER REDEFINES MSAAC12F.
000727         10  MSAAC12A PIC  X(0001).
000728     05  MSAAC12I PIC  X(0010).
000729*    -------------------------------
000730     05  MSAST12L PIC S9(0004) COMP.
000731     05  MSAST12F PIC  X(0001).
000732     05  FILLER REDEFINES MSAST12F.
000733         10  MSAST12A PIC  X(0001).
000734     05  MSAST12I PIC  X(0002).
000735*    -------------------------------
000736     05  MSACN12L PIC S9(0004) COMP.
000737     05  MSACN12F PIC  X(0001).
000738     05  FILLER REDEFINES MSACN12F.
000739         10  MSACN12A PIC  X(0001).
000740     05  MSACN12I PIC  X(0001).
000741*    -------------------------------
000742     05  MSACM12L PIC S9(0004) COMP.
000743     05  MSACM12F PIC  X(0001).
000744     05  FILLER REDEFINES MSACM12F.
000745         10  MSACM12A PIC  X(0001).
000746     05  MSACM12I PIC  X(0010).
000747*    -------------------------------
000748     05  CAR13L PIC S9(0004) COMP.
000749     05  CAR13F PIC  X(0001).
000750     05  FILLER REDEFINES CAR13F.
000751         10  CAR13A PIC  X(0001).
000752     05  CAR13I PIC  X(0001).
000753*    -------------------------------
000754     05  GRP13L PIC S9(0004) COMP.
000755     05  GRP13F PIC  X(0001).
000756     05  FILLER REDEFINES GRP13F.
000757         10  GRP13A PIC  X(0001).
000758     05  GRP13I PIC  X(0006).
000759*    -------------------------------
000760     05  FRESP13L PIC S9(0004) COMP.
000761     05  FRESP13F PIC  X(0001).
000762     05  FILLER REDEFINES FRESP13F.
000763         10  FRESP13A PIC  X(0001).
000764     05  FRESP13I PIC  X(0010).
000765*    -------------------------------
000766     05  ACCT13L PIC S9(0004) COMP.
000767     05  ACCT13F PIC  X(0001).
000768     05  FILLER REDEFINES ACCT13F.
000769         10  ACCT13A PIC  X(0001).
000770     05  ACCT13I PIC  X(0010).
000771*    -------------------------------
000772     05  TYPE13L PIC S9(0004) COMP.
000773     05  TYPE13F PIC  X(0001).
000774     05  FILLER REDEFINES TYPE13F.
000775         10  TYPE13A PIC  X(0001).
000776     05  TYPE13I PIC  X(0001).
000777*    -------------------------------
000778     05  AMT13L PIC S9(0004) COMP.
000779     05  AMT13F PIC  X(0001).
000780     05  FILLER REDEFINES AMT13F.
000781         10  AMT13A PIC  X(0001).
000782     05  AMT13I PIC  X(0011).
000783*    -------------------------------
000784     05  MSAAC13L PIC S9(0004) COMP.
000785     05  MSAAC13F PIC  X(0001).
000786     05  FILLER REDEFINES MSAAC13F.
000787         10  MSAAC13A PIC  X(0001).
000788     05  MSAAC13I PIC  X(0010).
000789*    -------------------------------
000790     05  MSAST13L PIC S9(0004) COMP.
000791     05  MSAST13F PIC  X(0001).
000792     05  FILLER REDEFINES MSAST13F.
000793         10  MSAST13A PIC  X(0001).
000794     05  MSAST13I PIC  X(0002).
000795*    -------------------------------
000796     05  MSACN13L PIC S9(0004) COMP.
000797     05  MSACN13F PIC  X(0001).
000798     05  FILLER REDEFINES MSACN13F.
000799         10  MSACN13A PIC  X(0001).
000800     05  MSACN13I PIC  X(0001).
000801*    -------------------------------
000802     05  MSACM13L PIC S9(0004) COMP.
000803     05  MSACM13F PIC  X(0001).
000804     05  FILLER REDEFINES MSACM13F.
000805         10  MSACM13A PIC  X(0001).
000806     05  MSACM13I PIC  X(0010).
000807*    -------------------------------
000808     05  CAR14L PIC S9(0004) COMP.
000809     05  CAR14F PIC  X(0001).
000810     05  FILLER REDEFINES CAR14F.
000811         10  CAR14A PIC  X(0001).
000812     05  CAR14I PIC  X(0001).
000813*    -------------------------------
000814     05  GRP14L PIC S9(0004) COMP.
000815     05  GRP14F PIC  X(0001).
000816     05  FILLER REDEFINES GRP14F.
000817         10  GRP14A PIC  X(0001).
000818     05  GRP14I PIC  X(0006).
000819*    -------------------------------
000820     05  FRESP14L PIC S9(0004) COMP.
000821     05  FRESP14F PIC  X(0001).
000822     05  FILLER REDEFINES FRESP14F.
000823         10  FRESP14A PIC  X(0001).
000824     05  FRESP14I PIC  X(0010).
000825*    -------------------------------
000826     05  ACCT14L PIC S9(0004) COMP.
000827     05  ACCT14F PIC  X(0001).
000828     05  FILLER REDEFINES ACCT14F.
000829         10  ACCT14A PIC  X(0001).
000830     05  ACCT14I PIC  X(0010).
000831*    -------------------------------
000832     05  TYPE14L PIC S9(0004) COMP.
000833     05  TYPE14F PIC  X(0001).
000834     05  FILLER REDEFINES TYPE14F.
000835         10  TYPE14A PIC  X(0001).
000836     05  TYPE14I PIC  X(0001).
000837*    -------------------------------
000838     05  AMT14L PIC S9(0004) COMP.
000839     05  AMT14F PIC  X(0001).
000840     05  FILLER REDEFINES AMT14F.
000841         10  AMT14A PIC  X(0001).
000842     05  AMT14I PIC  X(0011).
000843*    -------------------------------
000844     05  MSAAC14L PIC S9(0004) COMP.
000845     05  MSAAC14F PIC  X(0001).
000846     05  FILLER REDEFINES MSAAC14F.
000847         10  MSAAC14A PIC  X(0001).
000848     05  MSAAC14I PIC  X(0010).
000849*    -------------------------------
000850     05  MSAST14L PIC S9(0004) COMP.
000851     05  MSAST14F PIC  X(0001).
000852     05  FILLER REDEFINES MSAST14F.
000853         10  MSAST14A PIC  X(0001).
000854     05  MSAST14I PIC  X(0002).
000855*    -------------------------------
000856     05  MSACN14L PIC S9(0004) COMP.
000857     05  MSACN14F PIC  X(0001).
000858     05  FILLER REDEFINES MSACN14F.
000859         10  MSACN14A PIC  X(0001).
000860     05  MSACN14I PIC  X(0001).
000861*    -------------------------------
000862     05  MSACM14L PIC S9(0004) COMP.
000863     05  MSACM14F PIC  X(0001).
000864     05  FILLER REDEFINES MSACM14F.
000865         10  MSACM14A PIC  X(0001).
000866     05  MSACM14I PIC  X(0010).
000867*    -------------------------------
000868     05  CAR15L PIC S9(0004) COMP.
000869     05  CAR15F PIC  X(0001).
000870     05  FILLER REDEFINES CAR15F.
000871         10  CAR15A PIC  X(0001).
000872     05  CAR15I PIC  X(0001).
000873*    -------------------------------
000874     05  GRP15L PIC S9(0004) COMP.
000875     05  GRP15F PIC  X(0001).
000876     05  FILLER REDEFINES GRP15F.
000877         10  GRP15A PIC  X(0001).
000878     05  GRP15I PIC  X(0006).
000879*    -------------------------------
000880     05  FRESP15L PIC S9(0004) COMP.
000881     05  FRESP15F PIC  X(0001).
000882     05  FILLER REDEFINES FRESP15F.
000883         10  FRESP15A PIC  X(0001).
000884     05  FRESP15I PIC  X(0010).
000885*    -------------------------------
000886     05  ACCT15L PIC S9(0004) COMP.
000887     05  ACCT15F PIC  X(0001).
000888     05  FILLER REDEFINES ACCT15F.
000889         10  ACCT15A PIC  X(0001).
000890     05  ACCT15I PIC  X(0010).
000891*    -------------------------------
000892     05  TYPE15L PIC S9(0004) COMP.
000893     05  TYPE15F PIC  X(0001).
000894     05  FILLER REDEFINES TYPE15F.
000895         10  TYPE15A PIC  X(0001).
000896     05  TYPE15I PIC  X(0001).
000897*    -------------------------------
000898     05  AMT15L PIC S9(0004) COMP.
000899     05  AMT15F PIC  X(0001).
000900     05  FILLER REDEFINES AMT15F.
000901         10  AMT15A PIC  X(0001).
000902     05  AMT15I PIC  X(0011).
000903*    -------------------------------
000904     05  MSAAC15L PIC S9(0004) COMP.
000905     05  MSAAC15F PIC  X(0001).
000906     05  FILLER REDEFINES MSAAC15F.
000907         10  MSAAC15A PIC  X(0001).
000908     05  MSAAC15I PIC  X(0010).
000909*    -------------------------------
000910     05  MSAST15L PIC S9(0004) COMP.
000911     05  MSAST15F PIC  X(0001).
000912     05  FILLER REDEFINES MSAST15F.
000913         10  MSAST15A PIC  X(0001).
000914     05  MSAST15I PIC  X(0002).
000915*    -------------------------------
000916     05  MSACN15L PIC S9(0004) COMP.
000917     05  MSACN15F PIC  X(0001).
000918     05  FILLER REDEFINES MSACN15F.
000919         10  MSACN15A PIC  X(0001).
000920     05  MSACN15I PIC  X(0001).
000921*    -------------------------------
000922     05  MSACM15L PIC S9(0004) COMP.
000923     05  MSACM15F PIC  X(0001).
000924     05  FILLER REDEFINES MSACM15F.
000925         10  MSACM15A PIC  X(0001).
000926     05  MSACM15I PIC  X(0010).
000927*    -------------------------------
000928     05  ERRMSG1L PIC S9(0004) COMP.
000929     05  ERRMSG1F PIC  X(0001).
000930     05  FILLER REDEFINES ERRMSG1F.
000931         10  ERRMSG1A PIC  X(0001).
000932     05  ERRMSG1I PIC  X(0079).
000933*    -------------------------------
000934     05  ERRMSG2L PIC S9(0004) COMP.
000935     05  ERRMSG2F PIC  X(0001).
000936     05  FILLER REDEFINES ERRMSG2F.
000937         10  ERRMSG2A PIC  X(0001).
000938     05  ERRMSG2I PIC  X(0079).
000939*    -------------------------------
000940     05  PFENTERL PIC S9(0004) COMP.
000941     05  PFENTERF PIC  X(0001).
000942     05  FILLER REDEFINES PFENTERF.
000943         10  PFENTERA PIC  X(0001).
000944     05  PFENTERI PIC  9(2).
000945*    -------------------------------
000946     05  GAMTL PIC S9(0004) COMP.
000947     05  GAMTF PIC  X(0001).
000948     05  FILLER REDEFINES GAMTF.
000949         10  GAMTA PIC  X(0001).
000950     05  GAMTI PIC  X(0011).
000951 01  EL633BO REDEFINES EL633BI.
000952     05  FILLER            PIC  X(0012).
000953*    -------------------------------
000954     05  FILLER            PIC  X(0003).
000955     05  DATEO PIC  X(0008).
000956*    -------------------------------
000957     05  FILLER            PIC  X(0003).
000958     05  TIMEO PIC  99.99.
000959*    -------------------------------
000960     05  FILLER            PIC  X(0003).
000961     05  CMPNYIDO PIC  X(0003).
000962*    -------------------------------
000963     05  FILLER            PIC  X(0003).
000964     05  USERIDO PIC  X(0004).
000965*    -------------------------------
000966     05  FILLER            PIC  X(0003).
000967     05  CAR1O PIC  X(0001).
000968*    -------------------------------
000969     05  FILLER            PIC  X(0003).
000970     05  GRP1O PIC  X(0006).
000971*    -------------------------------
000972     05  FILLER            PIC  X(0003).
000973     05  FRESP1O PIC  X(0010).
000974*    -------------------------------
000975     05  FILLER            PIC  X(0003).
000976     05  ACCT1O PIC  X(0010).
000977*    -------------------------------
000978     05  FILLER            PIC  X(0003).
000979     05  TYPE1O PIC  X(0001).
000980*    -------------------------------
000981     05  FILLER            PIC  X(0003).
000982     05  AMT1O PIC  X(0011).
000983*    -------------------------------
000984     05  FILLER            PIC  X(0003).
000985     05  MSAAC1O PIC  X(0010).
000986*    -------------------------------
000987     05  FILLER            PIC  X(0003).
000988     05  MSAST1O PIC  X(0002).
000989*    -------------------------------
000990     05  FILLER            PIC  X(0003).
000991     05  MSACN1O PIC  X(0001).
000992*    -------------------------------
000993     05  FILLER            PIC  X(0003).
000994     05  MSACM1O PIC  X(0010).
000995*    -------------------------------
000996     05  FILLER            PIC  X(0003).
000997     05  CAR2O PIC  X(0001).
000998*    -------------------------------
000999     05  FILLER            PIC  X(0003).
001000     05  GRP2O PIC  X(0006).
001001*    -------------------------------
001002     05  FILLER            PIC  X(0003).
001003     05  FRESP2O PIC  X(0010).
001004*    -------------------------------
001005     05  FILLER            PIC  X(0003).
001006     05  ACCT2O PIC  X(0010).
001007*    -------------------------------
001008     05  FILLER            PIC  X(0003).
001009     05  TYPE2O PIC  X(0001).
001010*    -------------------------------
001011     05  FILLER            PIC  X(0003).
001012     05  AMT2O PIC  X(0011).
001013*    -------------------------------
001014     05  FILLER            PIC  X(0003).
001015     05  MSAAC2O PIC  X(0010).
001016*    -------------------------------
001017     05  FILLER            PIC  X(0003).
001018     05  MSAST2O PIC  X(0002).
001019*    -------------------------------
001020     05  FILLER            PIC  X(0003).
001021     05  MSACN2O PIC  X(0001).
001022*    -------------------------------
001023     05  FILLER            PIC  X(0003).
001024     05  MSACM2O PIC  X(0010).
001025*    -------------------------------
001026     05  FILLER            PIC  X(0003).
001027     05  CAR3O PIC  X(0001).
001028*    -------------------------------
001029     05  FILLER            PIC  X(0003).
001030     05  GRP3O PIC  X(0006).
001031*    -------------------------------
001032     05  FILLER            PIC  X(0003).
001033     05  FRESP3O PIC  X(0010).
001034*    -------------------------------
001035     05  FILLER            PIC  X(0003).
001036     05  ACCT3O PIC  X(0010).
001037*    -------------------------------
001038     05  FILLER            PIC  X(0003).
001039     05  TYPE3O PIC  X(0001).
001040*    -------------------------------
001041     05  FILLER            PIC  X(0003).
001042     05  AMT3O PIC  X(0011).
001043*    -------------------------------
001044     05  FILLER            PIC  X(0003).
001045     05  MSAAC3O PIC  X(0010).
001046*    -------------------------------
001047     05  FILLER            PIC  X(0003).
001048     05  MSAST3O PIC  X(0002).
001049*    -------------------------------
001050     05  FILLER            PIC  X(0003).
001051     05  MSACN3O PIC  X(0001).
001052*    -------------------------------
001053     05  FILLER            PIC  X(0003).
001054     05  MSACM3O PIC  X(0010).
001055*    -------------------------------
001056     05  FILLER            PIC  X(0003).
001057     05  CAR4O PIC  X(0001).
001058*    -------------------------------
001059     05  FILLER            PIC  X(0003).
001060     05  GRP4O PIC  X(0006).
001061*    -------------------------------
001062     05  FILLER            PIC  X(0003).
001063     05  FRESP4O PIC  X(0010).
001064*    -------------------------------
001065     05  FILLER            PIC  X(0003).
001066     05  ACCT4O PIC  X(0010).
001067*    -------------------------------
001068     05  FILLER            PIC  X(0003).
001069     05  TYPE4O PIC  X(0001).
001070*    -------------------------------
001071     05  FILLER            PIC  X(0003).
001072     05  AMT4O PIC  X(0011).
001073*    -------------------------------
001074     05  FILLER            PIC  X(0003).
001075     05  MSAAC4O PIC  X(0010).
001076*    -------------------------------
001077     05  FILLER            PIC  X(0003).
001078     05  MSAST4O PIC  X(0002).
001079*    -------------------------------
001080     05  FILLER            PIC  X(0003).
001081     05  MSACN4O PIC  X(0001).
001082*    -------------------------------
001083     05  FILLER            PIC  X(0003).
001084     05  MSACM4O PIC  X(0010).
001085*    -------------------------------
001086     05  FILLER            PIC  X(0003).
001087     05  CAR5O PIC  X(0001).
001088*    -------------------------------
001089     05  FILLER            PIC  X(0003).
001090     05  GRP5O PIC  X(0006).
001091*    -------------------------------
001092     05  FILLER            PIC  X(0003).
001093     05  FRESP5O PIC  X(0010).
001094*    -------------------------------
001095     05  FILLER            PIC  X(0003).
001096     05  ACCT5O PIC  X(0010).
001097*    -------------------------------
001098     05  FILLER            PIC  X(0003).
001099     05  TYPE5O PIC  X(0001).
001100*    -------------------------------
001101     05  FILLER            PIC  X(0003).
001102     05  AMT5O PIC  X(0011).
001103*    -------------------------------
001104     05  FILLER            PIC  X(0003).
001105     05  MSAAC5O PIC  X(0010).
001106*    -------------------------------
001107     05  FILLER            PIC  X(0003).
001108     05  MSAST5O PIC  X(0002).
001109*    -------------------------------
001110     05  FILLER            PIC  X(0003).
001111     05  MSACN5O PIC  X(0001).
001112*    -------------------------------
001113     05  FILLER            PIC  X(0003).
001114     05  MSACM5O PIC  X(0010).
001115*    -------------------------------
001116     05  FILLER            PIC  X(0003).
001117     05  CAR6O PIC  X(0001).
001118*    -------------------------------
001119     05  FILLER            PIC  X(0003).
001120     05  GRP6O PIC  X(0006).
001121*    -------------------------------
001122     05  FILLER            PIC  X(0003).
001123     05  FRESP6O PIC  X(0010).
001124*    -------------------------------
001125     05  FILLER            PIC  X(0003).
001126     05  ACCT6O PIC  X(0010).
001127*    -------------------------------
001128     05  FILLER            PIC  X(0003).
001129     05  TYPE6O PIC  X(0001).
001130*    -------------------------------
001131     05  FILLER            PIC  X(0003).
001132     05  AMT6O PIC  X(0011).
001133*    -------------------------------
001134     05  FILLER            PIC  X(0003).
001135     05  MSAAC6O PIC  X(0010).
001136*    -------------------------------
001137     05  FILLER            PIC  X(0003).
001138     05  MSAST6O PIC  X(0002).
001139*    -------------------------------
001140     05  FILLER            PIC  X(0003).
001141     05  MSACN6O PIC  X(0001).
001142*    -------------------------------
001143     05  FILLER            PIC  X(0003).
001144     05  MSACM6O PIC  X(0010).
001145*    -------------------------------
001146     05  FILLER            PIC  X(0003).
001147     05  CAR7O PIC  X(0001).
001148*    -------------------------------
001149     05  FILLER            PIC  X(0003).
001150     05  GRP7O PIC  X(0006).
001151*    -------------------------------
001152     05  FILLER            PIC  X(0003).
001153     05  FRESP7O PIC  X(0010).
001154*    -------------------------------
001155     05  FILLER            PIC  X(0003).
001156     05  ACCT7O PIC  X(0010).
001157*    -------------------------------
001158     05  FILLER            PIC  X(0003).
001159     05  TYPE7O PIC  X(0001).
001160*    -------------------------------
001161     05  FILLER            PIC  X(0003).
001162     05  AMT7O PIC  X(0011).
001163*    -------------------------------
001164     05  FILLER            PIC  X(0003).
001165     05  MSAAC7O PIC  X(0010).
001166*    -------------------------------
001167     05  FILLER            PIC  X(0003).
001168     05  MSAST7O PIC  X(0002).
001169*    -------------------------------
001170     05  FILLER            PIC  X(0003).
001171     05  MSACN7O PIC  X(0001).
001172*    -------------------------------
001173     05  FILLER            PIC  X(0003).
001174     05  MSACM7O PIC  X(0010).
001175*    -------------------------------
001176     05  FILLER            PIC  X(0003).
001177     05  CAR8O PIC  X(0001).
001178*    -------------------------------
001179     05  FILLER            PIC  X(0003).
001180     05  GRP8O PIC  X(0006).
001181*    -------------------------------
001182     05  FILLER            PIC  X(0003).
001183     05  FRESP8O PIC  X(0010).
001184*    -------------------------------
001185     05  FILLER            PIC  X(0003).
001186     05  ACCT8O PIC  X(0010).
001187*    -------------------------------
001188     05  FILLER            PIC  X(0003).
001189     05  TYPE8O PIC  X(0001).
001190*    -------------------------------
001191     05  FILLER            PIC  X(0003).
001192     05  AMT8O PIC  X(0011).
001193*    -------------------------------
001194     05  FILLER            PIC  X(0003).
001195     05  MSAAC8O PIC  X(0010).
001196*    -------------------------------
001197     05  FILLER            PIC  X(0003).
001198     05  MSAST8O PIC  X(0002).
001199*    -------------------------------
001200     05  FILLER            PIC  X(0003).
001201     05  MSACN8O PIC  X(0001).
001202*    -------------------------------
001203     05  FILLER            PIC  X(0003).
001204     05  MSACM8O PIC  X(0010).
001205*    -------------------------------
001206     05  FILLER            PIC  X(0003).
001207     05  CAR9O PIC  X(0001).
001208*    -------------------------------
001209     05  FILLER            PIC  X(0003).
001210     05  GRP9O PIC  X(0006).
001211*    -------------------------------
001212     05  FILLER            PIC  X(0003).
001213     05  FRESP9O PIC  X(0010).
001214*    -------------------------------
001215     05  FILLER            PIC  X(0003).
001216     05  ACCT9O PIC  X(0010).
001217*    -------------------------------
001218     05  FILLER            PIC  X(0003).
001219     05  TYPE9O PIC  X(0001).
001220*    -------------------------------
001221     05  FILLER            PIC  X(0003).
001222     05  AMT9O PIC  X(0011).
001223*    -------------------------------
001224     05  FILLER            PIC  X(0003).
001225     05  MSAAC9O PIC  X(0010).
001226*    -------------------------------
001227     05  FILLER            PIC  X(0003).
001228     05  MSAST9O PIC  X(0002).
001229*    -------------------------------
001230     05  FILLER            PIC  X(0003).
001231     05  MSACN9O PIC  X(0001).
001232*    -------------------------------
001233     05  FILLER            PIC  X(0003).
001234     05  MSACM9O PIC  X(0010).
001235*    -------------------------------
001236     05  FILLER            PIC  X(0003).
001237     05  CAR10O PIC  X(0001).
001238*    -------------------------------
001239     05  FILLER            PIC  X(0003).
001240     05  GRP10O PIC  X(0006).
001241*    -------------------------------
001242     05  FILLER            PIC  X(0003).
001243     05  FRESP10O PIC  X(0010).
001244*    -------------------------------
001245     05  FILLER            PIC  X(0003).
001246     05  ACCT10O PIC  X(0010).
001247*    -------------------------------
001248     05  FILLER            PIC  X(0003).
001249     05  TYPE10O PIC  X(0001).
001250*    -------------------------------
001251     05  FILLER            PIC  X(0003).
001252     05  AMT10O PIC  X(0011).
001253*    -------------------------------
001254     05  FILLER            PIC  X(0003).
001255     05  MSAAC10O PIC  X(0010).
001256*    -------------------------------
001257     05  FILLER            PIC  X(0003).
001258     05  MSAST10O PIC  X(0002).
001259*    -------------------------------
001260     05  FILLER            PIC  X(0003).
001261     05  MSACN10O PIC  X(0001).
001262*    -------------------------------
001263     05  FILLER            PIC  X(0003).
001264     05  MSACM10O PIC  X(0010).
001265*    -------------------------------
001266     05  FILLER            PIC  X(0003).
001267     05  CAR11O PIC  X(0001).
001268*    -------------------------------
001269     05  FILLER            PIC  X(0003).
001270     05  GRP11O PIC  X(0006).
001271*    -------------------------------
001272     05  FILLER            PIC  X(0003).
001273     05  FRESP11O PIC  X(0010).
001274*    -------------------------------
001275     05  FILLER            PIC  X(0003).
001276     05  ACCT11O PIC  X(0010).
001277*    -------------------------------
001278     05  FILLER            PIC  X(0003).
001279     05  TYPE11O PIC  X(0001).
001280*    -------------------------------
001281     05  FILLER            PIC  X(0003).
001282     05  AMT11O PIC  X(0011).
001283*    -------------------------------
001284     05  FILLER            PIC  X(0003).
001285     05  MSAAC11O PIC  X(0010).
001286*    -------------------------------
001287     05  FILLER            PIC  X(0003).
001288     05  MSAST11O PIC  X(0002).
001289*    -------------------------------
001290     05  FILLER            PIC  X(0003).
001291     05  MSACN11O PIC  X(0001).
001292*    -------------------------------
001293     05  FILLER            PIC  X(0003).
001294     05  MSACM11O PIC  X(0010).
001295*    -------------------------------
001296     05  FILLER            PIC  X(0003).
001297     05  CAR12O PIC  X(0001).
001298*    -------------------------------
001299     05  FILLER            PIC  X(0003).
001300     05  GRP12O PIC  X(0006).
001301*    -------------------------------
001302     05  FILLER            PIC  X(0003).
001303     05  FRESP12O PIC  X(0010).
001304*    -------------------------------
001305     05  FILLER            PIC  X(0003).
001306     05  ACCT12O PIC  X(0010).
001307*    -------------------------------
001308     05  FILLER            PIC  X(0003).
001309     05  TYPE12O PIC  X(0001).
001310*    -------------------------------
001311     05  FILLER            PIC  X(0003).
001312     05  AMT12O PIC  X(0011).
001313*    -------------------------------
001314     05  FILLER            PIC  X(0003).
001315     05  MSAAC12O PIC  X(0010).
001316*    -------------------------------
001317     05  FILLER            PIC  X(0003).
001318     05  MSAST12O PIC  X(0002).
001319*    -------------------------------
001320     05  FILLER            PIC  X(0003).
001321     05  MSACN12O PIC  X(0001).
001322*    -------------------------------
001323     05  FILLER            PIC  X(0003).
001324     05  MSACM12O PIC  X(0010).
001325*    -------------------------------
001326     05  FILLER            PIC  X(0003).
001327     05  CAR13O PIC  X(0001).
001328*    -------------------------------
001329     05  FILLER            PIC  X(0003).
001330     05  GRP13O PIC  X(0006).
001331*    -------------------------------
001332     05  FILLER            PIC  X(0003).
001333     05  FRESP13O PIC  X(0010).
001334*    -------------------------------
001335     05  FILLER            PIC  X(0003).
001336     05  ACCT13O PIC  X(0010).
001337*    -------------------------------
001338     05  FILLER            PIC  X(0003).
001339     05  TYPE13O PIC  X(0001).
001340*    -------------------------------
001341     05  FILLER            PIC  X(0003).
001342     05  AMT13O PIC  X(0011).
001343*    -------------------------------
001344     05  FILLER            PIC  X(0003).
001345     05  MSAAC13O PIC  X(0010).
001346*    -------------------------------
001347     05  FILLER            PIC  X(0003).
001348     05  MSAST13O PIC  X(0002).
001349*    -------------------------------
001350     05  FILLER            PIC  X(0003).
001351     05  MSACN13O PIC  X(0001).
001352*    -------------------------------
001353     05  FILLER            PIC  X(0003).
001354     05  MSACM13O PIC  X(0010).
001355*    -------------------------------
001356     05  FILLER            PIC  X(0003).
001357     05  CAR14O PIC  X(0001).
001358*    -------------------------------
001359     05  FILLER            PIC  X(0003).
001360     05  GRP14O PIC  X(0006).
001361*    -------------------------------
001362     05  FILLER            PIC  X(0003).
001363     05  FRESP14O PIC  X(0010).
001364*    -------------------------------
001365     05  FILLER            PIC  X(0003).
001366     05  ACCT14O PIC  X(0010).
001367*    -------------------------------
001368     05  FILLER            PIC  X(0003).
001369     05  TYPE14O PIC  X(0001).
001370*    -------------------------------
001371     05  FILLER            PIC  X(0003).
001372     05  AMT14O PIC  X(0011).
001373*    -------------------------------
001374     05  FILLER            PIC  X(0003).
001375     05  MSAAC14O PIC  X(0010).
001376*    -------------------------------
001377     05  FILLER            PIC  X(0003).
001378     05  MSAST14O PIC  X(0002).
001379*    -------------------------------
001380     05  FILLER            PIC  X(0003).
001381     05  MSACN14O PIC  X(0001).
001382*    -------------------------------
001383     05  FILLER            PIC  X(0003).
001384     05  MSACM14O PIC  X(0010).
001385*    -------------------------------
001386     05  FILLER            PIC  X(0003).
001387     05  CAR15O PIC  X(0001).
001388*    -------------------------------
001389     05  FILLER            PIC  X(0003).
001390     05  GRP15O PIC  X(0006).
001391*    -------------------------------
001392     05  FILLER            PIC  X(0003).
001393     05  FRESP15O PIC  X(0010).
001394*    -------------------------------
001395     05  FILLER            PIC  X(0003).
001396     05  ACCT15O PIC  X(0010).
001397*    -------------------------------
001398     05  FILLER            PIC  X(0003).
001399     05  TYPE15O PIC  X(0001).
001400*    -------------------------------
001401     05  FILLER            PIC  X(0003).
001402     05  AMT15O PIC  X(0011).
001403*    -------------------------------
001404     05  FILLER            PIC  X(0003).
001405     05  MSAAC15O PIC  X(0010).
001406*    -------------------------------
001407     05  FILLER            PIC  X(0003).
001408     05  MSAST15O PIC  X(0002).
001409*    -------------------------------
001410     05  FILLER            PIC  X(0003).
001411     05  MSACN15O PIC  X(0001).
001412*    -------------------------------
001413     05  FILLER            PIC  X(0003).
001414     05  MSACM15O PIC  X(0010).
001415*    -------------------------------
001416     05  FILLER            PIC  X(0003).
001417     05  ERRMSG1O PIC  X(0079).
001418*    -------------------------------
001419     05  FILLER            PIC  X(0003).
001420     05  ERRMSG2O PIC  X(0079).
001421*    -------------------------------
001422     05  FILLER            PIC  X(0003).
001423     05  PFENTERO PIC  X(0002).
001424*    -------------------------------
001425     05  FILLER            PIC  X(0003).
001426     05  GAMTO PIC  X(0011).
001427*    -------------------------------
      *<<((file: EL6331S))
000316
000317 01  MAP-EL633B   REDEFINES  EL633BI.
000318     12  FILLER                  PIC  X(44).
000319*    12  DATA-AREA       OCCURS  6 TIMES
000320     12  DATA-AREA       OCCURS 15 TIMES
000321                             INDEXED BY INDX.
000322         16  CARR-LEN            PIC S9(4)              COMP.
000323         16  CARR-ATTRB          PIC  X.
000324         16  CARRIER             PIC  X.
000325         16  GRP-LEN             PIC S9(4)              COMP.
000326         16  GRP-ATTRB           PIC  X.
000327         16  GROUPING            PIC  X(6).
000328         16  FIN-LEN             PIC S9(4)              COMP.
000329         16  FIN-ATTRB           PIC  X.
000330         16  FIN-RESP            PIC  X(10).
000331         16  ACCT-LEN            PIC S9(4)              COMP.
000332         16  ACCT-ATTRB          PIC  X.
000333         16  ACCT                PIC  X(10).
000334         16  RTYPE-LEN           PIC S9(4)              COMP.
000335         16  RTYPE-ATTRB         PIC  X.
000336         16  RTYPE               PIC  X.
000337         16  AMT-LEN             PIC S9(4)              COMP.
000338         16  AMT-ATTRB           PIC  X.
000339         16  AMT                 PIC S9(9)V99.
000340         16  AMTO  REDEFINES
000341             AMT                 PIC Z(7).9(2)-.
000342         16  GL-ACCT-LEN         PIC S9(4)              COMP.
000343         16  GL-ACCT-ATTRB       PIC  X.
000344         16  GL-ACCT             PIC  X(10).
000345         16  WSL-COMM  REDEFINES GL-ACCT.
000346             20  WSL-COMM-DTE.
000347                 24  WSL-MO      PIC  X(2).
000348                 24  WSL-DA      PIC  X(2).
000349                 24  WSL-YR      PIC  X(2).
000350             20  FILLER          PIC  X(4).
000351         16  GL-STATE-LEN        PIC S9(4)              COMP.
000352         16  GL-STATE-ATTRB      PIC  X.
000353         16  GL-STATE            PIC  X(02).
000354         16  GL-CANC-LEN         PIC S9(4)              COMP.
000355         16  GL-CANC-ATTRB       PIC  X.
000356         16  GL-CANC             PIC  X(01).
000357         16  GL-COMM-LEN         PIC S9(4)              COMP.
000358         16  GL-COMM-ATTRB       PIC  X.
000359         16  GL-COMM             PIC  X(10).
000360         16  FILLER  REDEFINES GL-COMM.
000361             20  GL-CHECK-NO     PIC  9(06).
000362             20  FILLER          PIC  X(04).
000363     12  MSG1-LEN                PIC S9(4)  COMP.
000364     12  MSG1-ATTRB              PIC X.
000365     12  MSG1                    PIC X(79).
000366     12  MSG2-LEN                PIC S9(4)  COMP.
000367     12  MSG2-ATTRB              PIC X.
000368     12  MSG2                    PIC X(79).
000369     12  FILLER                  PIC X(5).
000370     12  GAMT-LEN                PIC S9(4)  COMP.
000371     12  GAMT-ATTRB              PIC X.
000372     12  GROSS-AMT               PIC S9(9)V99.
000373     12  GROSS-AMTO REDEFINES GROSS-AMT
000374                                 PIC Z(7).9(2)-.
000375
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
000378 01  DFHCOMMAREA             PIC  X(1024).
000379 EJECT
000380*                            COPY ERCPYAJ.
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
000381 EJECT
000382*                            COPY ERCCOMP.
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
000383 EJECT
000384*
000385*                            COPY AIRL0009.
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL6331' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000387 VCOBOL-DUMMY-PROCEDURE.
000388
000389     MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
000390     MOVE 2                      TO  EMI-NUMBER-OF-LINES.
000391
000392     IF EIBCALEN = ZERO
000393         GO TO 8800-UNAUTHORIZED-ACCESS.
000394
000395     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
000396     MOVE '5'                    TO  DC-OPTION-CODE.
000397
000398     PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
000399
000400     MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.
000401     MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.
000402
000403     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000404         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000405             MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
000406             MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
000407             MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
000408             MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
000409             MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
000410             MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
000411             MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
000412             MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
000413         ELSE
000414             MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
000415             MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
000416             MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
000417             MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
000418             MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
000419             MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
000420             MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
000421             MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
000422
000423     MOVE LOW-VALUES             TO  EL633BI.
000424
000425     COMPUTE WORK-SEQ-NO  =  EIBTIME  *  10.
000426
000427     IF EIBTRNID NOT = TRANS-ID
000428         MOVE SPACE              TO  PI-PYAJ-FILE-SW
000429         GO TO 8100-SEND-INITIAL-MAP.
000430
000431     
      * EXEC CICS HANDLE CONDITION
000432*        PGMIDERR  (9600-PGMID-ERROR)
000433*        ERROR     (9990-ABEND)
000434*        END-EXEC.
      *    MOVE '"$L.                  ! " #00003192' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' &
                X'202020202020202020202120' &
                X'2220233030303033313932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000435
000436     IF EIBAID = DFHCLEAR
000437         GO TO 9400-CLEAR.
000438 EJECT
000439 0200-RECEIVE.
000440     IF EIBAID = DFHPA1              OR  DFHPA2  OR  DFHPA3
000441         MOVE ER-0008            TO  EMI-ERROR
000442         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000443         MOVE -1                 TO  PFENTERL
000444         GO TO 8200-SEND-DATAONLY.
000445
000446     
      * EXEC CICS RECEIVE
000447*        MAP     (MAP-NAME)
000448*        MAPSET  (MAPSET-NAME)
000449*        INTO    (EL633BI)
000450*        END-EXEC.
           MOVE LENGTH OF
            EL633BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003207' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303033323037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL633BI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000451
000452     IF PFENTERL = ZERO
000453         GO TO 0300-CHECK-PFKEYS.
000454
000455     IF (PFENTERI  IS NUMERIC)
000456       AND (PFENTERI  IS GREATER THAN  ZERO
000457       AND  IS LESS THAN  25)
000458         MOVE PF-VALUES (PFENTERI)  TO  EIBAID
000459     ELSE
000460         MOVE ER-0029               TO  EMI-ERROR
000461         GO TO 0320-INPUT-ERROR.
000462
000463 0300-CHECK-PFKEYS.
000464     IF EIBAID = DFHPF23
000465         GO TO 8810-PF23.
000466
000467     IF EIBAID = DFHPF24
000468         GO TO 9200-RETURN-MAIN-MENU.
000469
000470     IF EIBAID = DFHPF12
000471         GO TO 9500-PF12.
000472
000473     IF EIBAID = DFHENTER
000474         GO TO 1000-EDIT-DATA.
000475
000476     IF EIBAID = DFHPF1
000477        GO TO 1000-EDIT-DATA.
000478
000479 0320-INPUT-ERROR.
000480     MOVE ER-0029                TO  EMI-ERROR.
000481
000482     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000483
000484     MOVE AL-UNBON               TO  PFENTERA.
000485
000486     IF PFENTERL = ZERO
000487         MOVE -1                 TO  PFENTERL
000488     ELSE
000489         MOVE -1                 TO  PFENTERL.
000490
000491     GO TO 8200-SEND-DATAONLY.
000492 EJECT
000493 1000-EDIT-DATA.
000494     IF NOT MODIFY-CAP
000495         MOVE 'UPDATE'       TO SM-READ
000496         PERFORM 9995-SECURITY-VIOLATION
000497         MOVE ER-0070        TO EMI-ERROR
000498         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000499         GO TO 8100-SEND-INITIAL-MAP.
000500
000501     MOVE PI-COMPANY-CD          TO  PI-SAV-COMP-CD
000502                                     COMP-COMP-CD.
000503
000504     SET INDX                    TO  1.
000505     MOVE +1                     TO C1
000506     MOVE ZEROS                  TO TOTAL-AMOUNT
000507
000508     .
000509 1010-EDIT-LOOP.
000510     IF CARR-LEN (INDX) = ZEROS
000511       AND GRP-LEN (INDX) = ZEROS
000512       AND FIN-LEN (INDX) = ZEROS
000513       AND ACCT-LEN (INDX) = ZEROS
000514       AND GL-ACCT-LEN (INDX) = ZEROS
000515       AND GL-STATE-LEN (INDX) = ZEROS
000516       AND GL-CANC-LEN (INDX) = ZEROS
000517       AND GL-COMM-LEN (INDX) = ZEROS
000518       AND RTYPE-LEN (INDX) = ZEROS
000519       AND AMT-LEN (INDX) = ZEROS
000520         GO TO 1040-INCREMENT-INDX.
000521
000522     IF CARR-LEN (INDX) NOT = ZEROS
000523         MOVE AL-UANON           TO  CARR-ATTRB (INDX)
000524         MOVE CARRIER (INDX)     TO  COMP-CARRIER
000525                                     PI-SAV-CARRIER
000526         IF CARRIER (INDX) NOT = ZEROS
000527           AND (PI-ZERO-CARRIER  OR  PI-ZERO-CAR-GROUP)
000528             MOVE ER-2587        TO  EMI-ERROR
000529             MOVE -1             TO  CARR-LEN (INDX)
000530             MOVE AL-UABON       TO  CARR-ATTRB (INDX)
000531             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000532         ELSE
000533             NEXT SENTENCE
000534     ELSE
000535         MOVE ER-0194            TO  EMI-ERROR
000536         MOVE -1                 TO  CARR-LEN (INDX)
000537         MOVE AL-UABON           TO  CARR-ATTRB (INDX)
000538         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000539
000540     IF GRP-LEN (INDX) NOT = ZEROS
000541         MOVE AL-UANON           TO  GRP-ATTRB (INDX)
000542         MOVE GROUPING (INDX)    TO  COMP-GROUPING
000543                                     PI-SAV-GROUPING
000544         IF GROUPING (INDX) NOT = ZEROS
000545           AND (PI-ZERO-GROUPING  OR  PI-ZERO-CAR-GROUP)
000546             MOVE ER-2588        TO  EMI-ERROR
000547             MOVE -1             TO  GRP-LEN (INDX)
000548             MOVE AL-UABON       TO  GRP-ATTRB (INDX)
000549             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000550         ELSE
000551             NEXT SENTENCE
000552     ELSE
000553         MOVE ER-0195            TO  EMI-ERROR
000554         MOVE -1                 TO  GRP-LEN (INDX)
000555         MOVE AL-UABON           TO  GRP-ATTRB (INDX)
000556         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000557
000558     IF FIN-LEN (INDX) NOT = ZEROS
000559         MOVE AL-UANON           TO  FIN-ATTRB (INDX)
000560         MOVE FIN-RESP (INDX)    TO  COMP-FIN-RESP
000561                                     PI-SAV-FIN-RESP
000562                                     PI-CR-FIN-RESP
000563     ELSE
000564         MOVE ER-2562            TO  EMI-ERROR
000565         MOVE -1                 TO  FIN-LEN (INDX)
000566         MOVE AL-UABON           TO  FIN-ATTRB (INDX)
000567         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000568
000569     IF ACCT-LEN (INDX) NOT = ZEROS
000570         MOVE AL-UANON           TO  ACCT-ATTRB (INDX)
000571         MOVE ACCT (INDX)        TO  COMP-ACCOUNT
000572                                     PI-SAV-ACCOUNT
000573     ELSE
000574         MOVE LOW-VALUES         TO  COMP-ACCOUNT
000575                                     PI-SAV-ACCOUNT.
000576
000577     
      * EXEC CICS HANDLE CONDITION
000578*        NOTFND   (1020-NO-COMP-MSTR)
000579*        NOTOPEN  (7100-COMP-FILE-NOTOPEN)
000580*        END-EXEC.
      *    MOVE '"$IJ                  ! # #00003338' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303033333338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000581
000582     MOVE SPACES                 TO  COMP-RECORD-TYPE.
000583
000584     
      * EXEC CICS READ
000585*        DATASET  (COMP-FILE-ID)
000586*        SET      (ADDRESS OF COMPENSATION-MASTER)
000587*        RIDFLD   (ERCOMP-KEY)
000588*        GTEQ
000589*        END-EXEC.
      *    MOVE '&"S        G          (   #00003345' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303033333435' TO DFHEIV0
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
           
000590
000591     MOVE COMPENSATION-MASTER    TO SV-COMP.
000592     IF PI-COMPANY-CD = CO-COMPANY-CD
000593       AND COMP-CARRIER = CO-CARRIER
000594       AND COMP-GROUPING = CO-GROUPING
000595       AND COMP-FIN-RESP = CO-RESP-NO
000596       AND COMP-ACCOUNT = CO-ACCOUNT
000597         GO TO 1025-CHECK-STATUS.
000598
000599 1020-NO-COMP-MSTR.
000600     MOVE ER-2230                TO  EMI-ERROR.
000601     MOVE -1                     TO  CARR-LEN (INDX)
000602     MOVE AL-UABON               TO  CARR-ATTRB (INDX)
000603                                     GRP-ATTRB (INDX)
000604                                     FIN-ATTRB (INDX)
000605                                     ACCT-ATTRB (INDX)
000606
000607     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000608
000609     GO TO 1030-CONTINUE-EDIT.
000610
000611 1025-CHECK-STATUS.
000612
000613     MOVE CO-TYPE                TO WS-ERCOMP-TYPE (C1)
000614
000615     IF PI-COMPANY-ID = 'NCL'
000616         IF CO-GA-INACTIVE
000617             MOVE ER-2763            TO  EMI-ERROR
000618             MOVE -1                 TO  CARR-LEN   (INDX)
000619             MOVE AL-UABON           TO  CARR-ATTRB (INDX)
000620                                         GRP-ATTRB  (INDX)
000621                                         FIN-ATTRB  (INDX)
000622                                         ACCT-ATTRB (INDX)
000623             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000624
000625 1030-CONTINUE-EDIT.
000626     IF GL-ACCT-LEN (INDX) NOT = ZEROS
000627         MOVE AL-UANON           TO  GL-ACCT-ATTRB (INDX)
000628         IF PI-COMPANY-ID NOT = 'WSL'
000629             NEXT SENTENCE
000630         ELSE
000631             MOVE WSL-COMM-DTE (INDX)  TO  DC-GREG-DATE-1-MDY
000632             MOVE '4'                  TO  DC-OPTION-CODE
000633             PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
000634             IF NO-CONVERSION-ERROR
000635                 NEXT SENTENCE
000636             ELSE
000637                 MOVE ER-2595    TO  EMI-ERROR
000638                 MOVE -1         TO  GL-ACCT-LEN (INDX)
000639                 MOVE AL-UABON   TO  GL-ACCT-ATTRB (INDX)
000640                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000641     ELSE
000642         IF PI-COMPANY-ID = 'WSL'
000643             MOVE ER-2596        TO  EMI-ERROR
000644             MOVE -1             TO  GL-ACCT-LEN (INDX)
000645             MOVE AL-UABON       TO  GL-ACCT-ATTRB (INDX)
000646             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000647
000648     IF RTYPE-LEN (INDX) NOT = ZEROS
000649         MOVE RTYPE (INDX)       TO  CHECK-REC-TYPE
000650         IF (PI-COMPANY-ID = 'DCC' or 'VPP')
000651            AND (RTYPE (INDX) = 'P')
000652            SET VALID-REC-TYPE   TO TRUE
000653         END-IF
000654         IF NOT VALID-REC-TYPE
000655             MOVE -1             TO  RTYPE-LEN (INDX)
000656             MOVE ER-2234        TO  EMI-ERROR
000657             MOVE AL-UABON       TO  RTYPE-ATTRB (INDX)
000658             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000659         ELSE
000660             MOVE AL-UANON       TO  RTYPE-ATTRB (INDX)
000661     ELSE
000662         MOVE -1                 TO  RTYPE-LEN (INDX)
000663         MOVE ER-2235            TO  EMI-ERROR
000664         MOVE AL-UABON           TO  RTYPE-ATTRB (INDX)
000665         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000666
000667     IF AMT-LEN (INDX) NOT = ZEROS
000668         MOVE AL-UNNON           TO  AMT-ATTRB (INDX)
000669         
      * EXEC CICS BIF DEEDIT
000670*             FIELD (AMT (INDX))
000671*             LENGTH (11)
000672*        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003430' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033343330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AMT(INDX), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000673         IF AMT (INDX) = ZEROS
000674             MOVE ER-2245        TO  EMI-ERROR
000675             MOVE -1             TO  AMT-LEN(INDX)
000676             MOVE AL-UNBON       TO  AMT-ATTRB (INDX)
000677             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000678         ELSE
000679             SET WS-INDX          TO INDX
000680             MOVE AMT(INDX)      TO WS-EDITED-AMT (WS-INDX)
000681             IF RTYPE (INDX) = 'R' OR 'D' OR 'S' OR 'Z'
000682                ADD WS-EDITED-AMT (WS-INDX)
000683                                 TO TOTAL-AMOUNT
000684             ELSE
000685                SUBTRACT WS-EDITED-AMT (WS-INDX)
000686                                 FROM TOTAL-AMOUNT
000687             END-IF
000688         END-IF
000689     ELSE
000690         MOVE -1                 TO  AMT-LEN (INDX)
000691         MOVE ER-2236            TO  EMI-ERROR
000692         MOVE AL-UNBON           TO  AMT-ATTRB (INDX)
000693         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000694
000695     IF (PI-COMPANY-ID = 'DCC' or 'VPP')
000696        AND (RTYPE (INDX) = 'P')
000697        CONTINUE
000698     ELSE
000699        IF GL-ACCT-LEN (INDX) = ZEROES
000700           MOVE -1               TO GL-ACCT-LEN (INDX)
000701           MOVE ER-2956          TO EMI-ERROR
000702           MOVE AL-UABON         TO GL-ACCT-ATTRB (INDX)
000703           PERFORM 9900-ERROR-FORMAT
000704                                 THRU 9900-EXIT
000705           GO TO 1040-CONTINUE-EDIT
000706        END-IF
000707     END-IF
000708
000709     IF GL-ACCT-LEN (INDX) NOT = ZEROS
000710        MOVE GL-ACCT (INDX)      TO CHECK-GL-ACCT
000711     ELSE
000712        MOVE SPACES              TO CHECK-GL-ACCT
000713     END-IF
000714
000715     EVALUATE TRUE
000716        WHEN (PI-COMPANY-ID = 'DCC')
000717           AND (CO-CARRIER = '1' OR '2' or '9')
000718           AND (VALID-DCC-GL-ACCOUNT)
000719           GO TO 1040-CONTINUE-EDIT
000720        WHEN (PI-COMPANY-ID = 'DCC')
000721           AND (CO-CARRIER = '3' OR '4')
000722           AND VALID-CSI-GL-ACCOUNT
000723           GO TO 1040-CONTINUE-EDIT
000724        WHEN (PI-COMPANY-ID = 'DCC')
000725           AND (CO-CARRIER = '5' OR '6' or '7')
000726           AND (VALID-CCC-GL-ACCOUNT)
000727           GO TO 1040-CONTINUE-EDIT
000728        WHEN (PI-COMPANY-ID = 'CID' OR 'AHL')
000729           AND (VALID-GL-ACCOUNT)
000730           GO TO 1040-CONTINUE-EDIT
000731        WHEN (PI-COMPANY-ID = 'VPP')
000732           AND (VALID-VPP-GL-ACCOUNT)
000733           GO TO 1040-CONTINUE-EDIT
000734        WHEN (PI-COMPANY-ID = 'FNL')
000735           AND (VALID-FNL-GL-ACCOUNT)
000736           GO TO 1040-CONTINUE-EDIT
000737     END-EVALUATE
000738
000739*    EXEC CICS HANDLE CONDITION
000740*         NOTFND (1040-NO-COFA-MSTR)
000741*         NOTOPEN (7100-COFA-FILE-NOTOPEN)
000742*    END-EXEC.
000743*
000744*    EXEC CICS READ
000745*         DATASET (COFA-FILE-ID)
000746*         SET     (ADDRESS OF CHART-OF-ACCOUNTS)
000747*         RIDFLD  (COFA-KEY-X)
000748*    END-EXEC.
000749*
000750*    MOVE CHART-OF-ACCOUNTS      TO SV-COFA.
000751*    GO TO 1040-CONTINUE-EDIT.
000752
000753     .
000754 1040-NO-COFA-MSTR.
000755
000756     MOVE ER-2960                TO EMI-ERROR.
000757     MOVE -1                     TO GL-ACCT-LEN (INDX).
000758     MOVE AL-UABON               TO GL-ACCT-ATTRB (INDX).
000759     PERFORM 9900-ERROR-FORMAT THRU
000760             9900-EXIT.
000761
000762 1040-CONTINUE-EDIT.
000763
000764*    MOVE MSA-ACCT (INDX)        TO WS-ACCT-BREAK.
000765     IF GL-STATE-LEN (INDX) NOT EQUAL ZEROS
000766         MOVE GL-STATE (INDX)    TO CHECK-STATE-CODE
000767         IF NOT VALID-STATE-CODE
000768             MOVE -1             TO GL-STATE-LEN (INDX)
000769             MOVE ER-2957        TO EMI-ERROR
000770             MOVE AL-UABON       TO GL-STATE-ATTRB (INDX)
000771             PERFORM 9900-ERROR-FORMAT THRU
000772                     9900-EXIT
000773         ELSE
000774             MOVE AL-UANON       TO GL-STATE-ATTRB (INDX)
000775         END-IF
000776     END-IF.
000777
000778 1040-CSO-SKIP.
000779
000780     IF GL-CANC (INDX) = LOW-VALUES
000781         MOVE SPACES             TO GL-CANC (INDX)
000782     END-IF
000783
000784     IF GL-CANC-LEN (INDX) NOT EQUAL ZEROS
000785         MOVE GL-CANC (INDX)     TO CHECK-CANC-TYPE
000786         IF NOT VALID-CANC-TYPE
000787             MOVE -1             TO GL-CANC-LEN (INDX)
000788             MOVE ER-2958        TO EMI-ERROR
000789             MOVE AL-UABON       TO GL-CANC-ATTRB (INDX)
000790             PERFORM 9900-ERROR-FORMAT THRU
000791                     9900-EXIT
000792         ELSE
000793             MOVE AL-UANON       TO GL-CANC-ATTRB (INDX)
000794         END-IF
000795     END-IF.
000796
000797     IF (CARRIER (INDX) EQUAL '6' AND
000798         GL-ACCT (INDX) EQUAL '1082202')
000799         CONTINUE
000800     ELSE
000801         GO TO 1040-INCREMENT-INDX
000802     END-IF.
000803
000804     IF GL-CHECK-NO (INDX) NOT NUMERIC
000805         MOVE -1                 TO GL-COMM-LEN (INDX)
000806         MOVE ER-2961            TO EMI-ERROR
000807         MOVE AL-UABON           TO GL-COMM-ATTRB (INDX)
000808         PERFORM 9900-ERROR-FORMAT THRU
000809                 9900-EXIT
000810     END-IF.
000811
000812 1040-INCREMENT-INDX.
000813     SET INDX  UP  BY  1.
000814
000815     ADD +1                      TO C1
000816
000817
000818     IF INDX  IS NOT GREATER THAN  +15
000819         GO TO 1010-EDIT-LOOP.
000820
000821     IF EMI-ERROR = ZEROS
000822         GO TO 2000-UPDATE-THE-FILE
000823     ELSE
000824         GO TO 8200-SEND-DATAONLY.
000825 EJECT
000826 2000-UPDATE-THE-FILE.
000827
000828     IF EIBAID = DFHPF1
000829        CONTINUE
000830     ELSE
000831        MOVE 'PRESS PF1 TO UPDATE FILE' TO EMI-MESSAGE-AREA (1)
000832        MOVE SPACES TO EMI-MESSAGE-AREA (2)
000833        MOVE -1 TO  CAR1L
000834        MOVE TOTAL-AMOUNT        TO GROSS-AMTO
000835        GO TO 8200-SEND-DATAONLY.
000836
000837     SET INDX                    TO  1.
000838     MOVE +1                     TO C1
000839
000840     .
000841 2100-UPDATE-LOOP.
000842     IF INDX  IS GREATER THAN  +15
000843         GO TO 2200-UPDATE-COMPLETE.
000844
000845     IF CARR-LEN (INDX) = ZEROS
000846         SET INDX  UP  BY  1
000847         ADD +1        TO C1
000848         GO TO 2100-UPDATE-LOOP.
000849
000850     
      * EXEC CICS GETMAIN
000851*        SET      (ADDRESS OF PENDING-PAY-ADJ)
000852*        LENGTH   (ERPYAJ-RECORD-LENGTH)
000853*        INITIMG  (GETMAIN-SPACE)
000854*        END-EXEC.
      *    MOVE ',"IL                  $   #00003611' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303033363131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPYAJ-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000855
000856     MOVE 'PY'                   TO  PY-RECORD-ID.
000857     MOVE PI-COMPANY-CD          TO  PY-COMPANY-CD.
000858     MOVE CARRIER     (INDX)     TO  PY-CARRIER.
000859     MOVE GROUPING    (INDX)     TO  PY-GROUPING.
000860     MOVE FIN-RESP    (INDX)     TO  PY-FIN-RESP.
000861     MOVE ACCT        (INDX)     TO  PY-ACCOUNT.
000862     MOVE RTYPE       (INDX)     TO  PY-RECORD-TYPE.
000863
000864     MOVE WORK-SEQ-NO            TO  PY-FILE-SEQ-NO.
000865
000866     ADD +1                      TO  WORK-SEQ-NO.
000867
000868     IF GL-ACCT-LEN (INDX) NOT = ZEROS
000869        MOVE GL-ACCT (INDX)      TO  PY-GL-ACCOUNT
000870     END-IF
000871
000872     IF GL-STATE-LEN (INDX) NOT = ZEROS
000873        MOVE GL-STATE (INDX)     TO  PY-GL-STATE
000874     END-IF
000875
000876     IF GL-CANC-LEN (INDX) NOT = ZEROS
000877        MOVE GL-CANC (INDX)      TO  PY-GL-CANC-SW
000878     END-IF
000879
000880     IF GL-COMM-LEN (INDX) NOT = ZEROS
000881        MOVE GL-COMM (INDX)      TO  PY-GL-COMMENT
000882     END-IF
000883
000884     MOVE WS-ERCOMP-TYPE (C1)    TO  PY-ERCOMP-TYPE
000885     SET WS-INDX                 TO  INDX.
000886     MOVE WS-EDITED-AMT(WS-INDX) TO  PY-ENTRY-AMT.
000887     MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.
000888     MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.
000889     MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT
000890                                     PY-INPUT-DT.
000891     MOVE ZEROS                  TO  PY-CHECK-QUE-CONTROL
000892                                     PY-CHECK-QUE-SEQUENCE.
000893     MOVE LOW-VALUES             TO  PY-CREDIT-ACCEPT-DT
000894                                     PY-BILLED-DATE
000895                                     PY-AR-DATE
000896                                     PY-REPORTED-DT
000897                                     PY-CHECK-WRITTEN-DT.
000898     MOVE PI-CR-MONTH-END-DT     TO  PY-CREDIT-SELECT-DT.
000899     MOVE 'A'                    TO  JP-RECORD-TYPE.
000900     MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
000901
000902     
      * EXEC CICS WRITE
000903*        DATASET  (PYAJ-FILE-ID)
000904*        FROM     (PENDING-PAY-ADJ)
000905*        RIDFLD   (PY-CONTROL-PRIMARY)
000906*        END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003663' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033363633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000907
000908     PERFORM 8400-LOG-JOURNAL-RECORD.
000909
000910     MOVE LOW-VALUES             TO  DATA-AREA (INDX).
000911
000912     GO TO 2100-UPDATE-LOOP.
000913
000914 2200-UPDATE-COMPLETE.
000915     MOVE LOW-VALUES             TO  EL633BI.
000916     MOVE ER-0000                TO  EMI-ERROR.
000917     MOVE -1                     TO  CAR1L.
000918
000919     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000920
000921     GO TO 8100-SEND-INITIAL-MAP.
000922 EJECT
000923 7000-PYAJ-FILE-NOTOPEN.
000924     MOVE -1                     TO  PFENTERL.
000925     MOVE ER-2232                TO  EMI-ERROR.
000926
000927     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000928
000929     GO TO 8200-SEND-DATAONLY.
000930
000931 7100-COMP-FILE-NOTOPEN.
000932     MOVE -1                     TO  PFENTERL.
000933     MOVE ER-2233                TO  EMI-ERROR.
000934
000935     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000936
000937     GO TO 8200-SEND-DATAONLY.
000939*
000940*7100-COFA-FILE-NOTOPEN.
000941*    MOVE -1                     TO PFENTERL.
000942*    MOVE ER-2959                TO EMI-ERROR.
000943*    PERFORM 9900-ERROR-FORMAT THRU
000944*            9900-EXIT.
000945*    GO TO 8200-SEND-DATAONLY.
000946*
000947 8100-SEND-INITIAL-MAP.
000948     MOVE WS-CURRENT-DT          TO  DATEO.
000949     MOVE EIBTIME                TO  TIME-IN.
000950     MOVE TIME-OUT               TO  TIMEO.
000951     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
000952     MOVE PI-PROCESSOR-ID        TO  USERIDO.
000953     MOVE -1                     TO  CAR1L.
000954     MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
000955     MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
000956
000957     
      * EXEC CICS SEND
000958*        MAP     (MAP-NAME)
000959*        MAPSET  (MAPSET-NAME)
000960*        FROM    (EL633BO)
000961*        ERASE
000962*        CURSOR
000963*        END-EXEC.
           MOVE LENGTH OF
            EL633BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003717' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303033373137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL633BO, 
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
           
000964
000965     GO TO 9100-RETURN-TRAN.
000966 EJECT
000967 8200-SEND-DATAONLY.
000968     MOVE WS-CURRENT-DT          TO  DATEO.
000969     MOVE EIBTIME                TO  TIME-IN.
000970     MOVE TIME-OUT               TO  TIMEO.
000971     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
000972     MOVE PI-PROCESSOR-ID        TO  USERIDO.
000973     MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
000974     MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
000975
000976     
      * EXEC CICS SEND
000977*        MAP     (MAP-NAME)
000978*        MAPSET  (MAPSET-NAME)
000979*        FROM    (EL633BO)
000980*        DATAONLY
000981*        CURSOR
000982*        END-EXEC.
           MOVE LENGTH OF
            EL633BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00003736' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303033373336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL633BO, 
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
           
000983
000984     GO TO 9100-RETURN-TRAN.
000985
000986 8300-SEND-TEXT.
000987     
      * EXEC CICS SEND TEXT
000988*        FROM    (LOGOFF-TEXT)
000989*        LENGTH  (LOGOFF-LENGTH)
000990*        ERASE
000991*        FREEKB
000992*        END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00003747' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303033373437' TO DFHEIV0
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
           
000993
000994     
      * EXEC CICS RETURN
000995*        END-EXEC.
      *    MOVE '.(                    ''   #00003754' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033373534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000996 EJECT
000997 8400-LOG-JOURNAL-RECORD.
000998*    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
000999*    MOVE THIS-PGM               TO  JP-PROGRAM-ID.
001000*    MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
001001*    MOVE ZERO                   TO  JP-GENERIC-KEY-LENGTH.
001002
001003*    EXEC CICS JOURNAL
001004*        JFILEID  (PI-JOURNAL-FILE-ID)
001005*        JTYPEID  ('EL')
001006*        FROM     (JOURNAL-RECORD)
001007*        LENGTH   (223)
001008*        END-EXEC.
001009
001010 8500-DATE-CONVERT.
001011     
      * EXEC CICS LINK
001012*        PROGRAM   (LINK-CLDATCV)
001013*        COMMAREA  (DATE-CONVERSION-DATA)
001014*        LENGTH    (DC-COMM-LENGTH)
001015*        END-EXEC.
      *    MOVE '."C                   (   #00003771' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033373731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-CLDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001016
001017 8500-EXIT.
001018     EXIT.
001019
001020 8600-DEEDIT.
001021     
      * EXEC CICS BIF DEEDIT
001022*        FIELD   (DEEDIT-FIELD)
001023*        LENGTH  (11)
001024*        END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003781' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303033373831' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001025
001026 8600-EXIT.
001027     EXIT.
001028 EJECT
001029 8800-UNAUTHORIZED-ACCESS.
001030     MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
001031
001032     GO TO 8300-SEND-TEXT.
001033
001034 8810-PF23.
001035     MOVE EIBAID                 TO  PI-ENTRY-CD-1.
001036     MOVE XCTL-005               TO  PGM-NAME.
001037
001038     GO TO 9300-XCTL.
001039
001040 9000-RETURN-CICS.
001041     
      * EXEC CICS RETURN
001042*        END-EXEC.
      *    MOVE '.(                    ''   #00003801' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033383031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001043
001044 9100-RETURN-TRAN.
001045     MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
001046     MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
001047
001048     
      * EXEC CICS RETURN
001049*        TRANSID   (TRANS-ID)
001050*        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
001051*        LENGTH    (PI-COMM-LENGTH)
001052*        END-EXEC.
      *    MOVE '.(CT                  ''   #00003808' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303033383038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001053
001054 9200-RETURN-MAIN-MENU.
001055     MOVE XCTL-626               TO  PGM-NAME.
001056
001057     GO TO 9300-XCTL.
001058
001059 9300-XCTL.
001060     
      * EXEC CICS XCTL
001061*        PROGRAM   (PGM-NAME)
001062*        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
001063*        LENGTH    (PI-COMM-LENGTH)
001064*        END-EXEC.
      *    MOVE '.$C                   %   #00003820' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303033383230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001065
001066 9400-CLEAR.
001067     MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
001068
001069     GO TO 9300-XCTL.
001070
001071 9500-PF12.
001072     MOVE XCTL-010               TO  PGM-NAME.
001073
001074     GO TO 9300-XCTL.
001075
001076 9600-PGMID-ERROR.
001077     
      * EXEC CICS HANDLE CONDITION
001078*        PGMIDERR  (8300-SEND-TEXT)
001079*        END-EXEC.
      *    MOVE '"$L                   ! $ #00003837' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303033383337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001080
001081     MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
001082     MOVE ' '                    TO  PI-ENTRY-CD-1.
001083     MOVE XCTL-005               TO  PGM-NAME.
001084     MOVE PGM-NAME               TO  LOGOFF-PGM.
001085     MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
001086
001087     GO TO 9300-XCTL.
001088
001089 9900-ERROR-FORMAT.
001090     IF NOT EMI-ERRORS-COMPLETE
001091         MOVE LINK-001           TO  PGM-NAME
001092         
      * EXEC CICS LINK
001093*            PROGRAM   (PGM-NAME)
001094*            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
001095*            LENGTH    (EMI-COMM-LENGTH)
001096*            END-EXEC.
      *    MOVE '."C                   (   #00003852' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033383532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001097
001098 9900-EXIT.
001099     EXIT.
001100
001101 9990-ABEND.
001102     MOVE LINK-004               TO  PGM-NAME.
001103     MOVE DFHEIBLK               TO  EMI-LINE1.
001104
001105     
      * EXEC CICS LINK
001106*        PROGRAM   (PGM-NAME)
001107*        COMMAREA  (EMI-LINE1)
001108*        LENGTH    (72)
001109*        END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00003865' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033383635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001110
001111     MOVE -1                     TO  PFENTERL.
001112
001113     GO TO 8200-SEND-DATAONLY.
001114
001115     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6331' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
001116
001117 9995-SECURITY-VIOLATION.
001118*                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00003896' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303033383936' TO DFHEIV0
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
001119
001120 9995-EXIT.
001121     EXIT.
001122

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6331' TO DFHEIV1
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
               GO TO 1020-NO-COMP-MSTR,
                     7100-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6331' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
